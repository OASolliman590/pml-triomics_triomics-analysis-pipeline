#!/usr/bin/env python3
"""M7-029 public-synthetic native B/C/D verification and persisted read-back.

Usage: python3 verify_m7_029_resume.py SOURCE FRESH_OUTPUT

SOURCE is read-only and must be the immutable M7-029 candidate commit.
FRESH_OUTPUT must not already contain files. All generated tickets, fixture
copies, subprocess logs, receipts, and native outputs live beneath FRESH_OUTPUT.
"""
from __future__ import annotations

import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

DEFAULT_EXPECTED_COMMIT = "36547f5c132d1a97d7640607688bd4afea2db8f3"
EXPECTED_COMMIT = os.environ.get("TRIOMICS_EXPECTED_COMMIT", DEFAULT_EXPECTED_COMMIT)
MATRIX_PATHS = (
    "inst/examples/data/c04/metadata.tsv",
    "inst/examples/data/independent/proteomics.tsv",
    "inst/examples/data/independent/metabolomics.tsv",
)
EXTRA_INPUT_PATHS = (
    "inst/examples/data/independent/metadata.tsv",
    "inst/examples/data/d-target/external-assays.json",
    "inst/examples/data/d-target/mechanisms.json",
    "inst/examples/data/d-discovery/universe.json",
    "inst/examples/data/d-discovery/protocol.json",
)
ASSAY_KEYS = (
    "evidence_id", "axis", "endpoint", "units", "relation", "source_id",
    "publication_group", "measured_by_pipeline", "value", "target_namespace",
    "target_id", "target_entity_type", "taxon_id", "context_id", "construct",
    "compound_namespace", "compound_id", "assay", "assay_limitations",
)


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path: Path) -> Any:
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def write_receipt(path: Path, value: dict[str, Any]) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", encoding="utf-8", newline="\n") as stream:
        json.dump(value, stream, indent=2, sort_keys=True, ensure_ascii=False)
        stream.write("\n")
        stream.flush()
        os.fsync(stream.fileno())
    temporary.replace(path)


def run_capture(command: list[str], cwd: Path) -> str:
    result = subprocess.run(command, cwd=cwd, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, check=True, text=True)
    return result.stdout.strip()


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def sha_is_hex(value: Any) -> bool:
    return isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value) is not None


def load_commit_and_clean(source: Path) -> str:
    commit = run_capture(["git", "rev-parse", "HEAD"], source)
    require(re.fullmatch(r"[0-9a-f]{40}", commit) is not None,
            "source HEAD is not a full 40-character commit")
    require(commit == EXPECTED_COMMIT,
            f"source commit changed: expected {EXPECTED_COMMIT}, got {commit}")
    dirty = run_capture(["git", "status", "--porcelain", "--untracked-files=all"], source)
    require(not dirty, f"source working tree is not clean: {dirty}")
    return commit


def source_input_map(source: Path, ticket: str) -> list[str]:
    paths = list(MATRIX_PATHS)
    paths.append("inst/examples/data/independent/metadata.tsv")
    paths.append("inst/examples/data/d-target/external-assays.json")
    if ticket in ("D-T10", "D-T11"):
        paths.append("inst/examples/data/d-target/mechanisms.json")
    if ticket == "D-T12":
        paths.extend(("inst/examples/data/d-discovery/universe.json",
                      "inst/examples/data/d-discovery/protocol.json"))
    return paths


def copy_public_inputs(source: Path, case_root: Path, ticket: str) -> dict[str, str]:
    data_root = case_root / "data"
    copied: dict[str, str] = {}
    for relative in source_input_map(source, ticket):
        source_path = source / relative
        require(source_path.is_file() and not source_path.is_symlink(),
                f"required public synthetic fixture missing or symlinked: {relative}")
        data_relative = Path(*Path(relative).parts[3:])
        destination = data_root / data_relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source_path, destination)
        original_hash = sha256(source_path)
        copied_hash = sha256(destination)
        require(original_hash == copied_hash, f"public input copy differs: {relative}")
        copied[relative] = original_hash
    return copied


def matrix_feature_ids(matrix_path: Path) -> list[str]:
    with matrix_path.open(encoding="utf-8", newline="") as stream:
        rows = csv.reader(stream, delimiter="\t")
        header = next(rows)
        require(bool(header) and header[0] == "feature_id",
                f"unexpected public matrix header: {matrix_path}")
        values = [row[0] for row in rows if row]
    require(values and len(values) == len(set(values)),
            f"public matrix feature identifiers are empty/duplicated: {matrix_path}")
    return values


def write_feature_taxon_metadata(case_root: Path, taxon: str) -> dict[str, Any]:
    require(taxon in ("9606", "10090", "unknown"), f"unsupported synthetic taxon control: {taxon}")
    path = case_root / "data" / "d-target" / "feature-taxon-control.tsv"
    metabolomics = case_root / "data" / "independent" / "metabolomics.tsv"
    feature_ids = matrix_feature_ids(metabolomics)
    provenance_ref = f"synthetic://m7-029-resume-control/feature-taxon-{taxon}"
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, delimiter="\t", lineterminator="\n")
        writer.writerow(("feature_id", "taxon_id", "resolution", "capabilities", "provenance_ref"))
        for feature_id in feature_ids:
            writer.writerow((feature_id, taxon, "synthetic_feature", "target_assessment", provenance_ref))
    return {
        "path": str(path),
        "sha256": sha256(path),
        "feature_count": len(feature_ids),
        "taxon_id": taxon,
        "provenance_ref": provenance_ref,
        "origin": "public synthetic control generated under FRESH_OUTPUT; no source fixture changed",
    }


def configure_feature_taxon(ticket_text: str, enabled: bool) -> str:
    block = "  - block_id: block_2\n"
    require(ticket_text.count(block) == 1, "expected exactly one block_2 in public ticket")
    prefix, suffix = ticket_text.split(block, 1)
    if enabled:
        changed, count = re.subn(
            r"(?m)^    feature_metadata: null\s*$",
            "    feature_metadata: ../data/d-target/feature-taxon-control.tsv",
            suffix, count=1,
        )
        require(count == 1, "could not attach controlled feature taxon metadata to block_2")
        suffix = changed
    else:
        # Keep the shipped null declaration byte-for-byte in this copy.
        require(re.search(r"(?m)^    feature_metadata: null\s*$", suffix) is not None,
                "shipped block_2 feature_metadata is not the expected explicit null")
    return prefix + block + suffix


def write_numeric_assays(case_root: Path, *, drop_assay: bool = False,
                         drop_limitations: bool = False) -> dict[str, Any]:
    path = case_root / "data" / "d-target" / "external-assays.json"
    payload = read_json(path)
    require(payload.get("format") == "triomics.external-target-evidence",
            "unexpected external assay fixture format")
    require(len(payload.get("records", [])) == 2,
            "expected the two shipped public synthetic assay controls")
    original = read_json(case_root / "data" / "d-target" / "external-assays.json")
    details: list[dict[str, Any]] = []
    numeric_values = {"SYNTHETIC_TARGET_A": 12.5, "SYNTHETIC_TARGET_B": 25.0}
    for record in payload["records"]:
        target = record["target_id"]
        require(target in numeric_values, f"unexpected external synthetic target: {target}")
        record["value"] = numeric_values[target]
        # Preserve the shipped exact endpoint, unit, relation, source, and assay text.
        if drop_assay:
            record.pop("assay", None)
        if drop_limitations:
            record.pop("assay_limitations", None)
        details.append({
            "evidence_id": record["evidence_id"],
            "target_id": target,
            "value": record["value"],
            "endpoint": record["endpoint"],
            "units": record["units"],
            "source_id": record["source_id"],
            "assay_present": "assay" in record,
            "assay_limitations_present": "assay_limitations" in record,
        })
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        json.dump(payload, stream, indent=2, ensure_ascii=False)
        stream.write("\n")
    return {
        "path": str(path),
        "sha256": sha256(path),
        "records": details,
        "public_synthetic_only": True,
        "adaptation": "Copied public synthetic external assay fixture; numeric values are control values, not biological measurements.",
    }


def prepare_case(source: Path, output: Path, name: str, ticket: str,
                 taxon: str | None = None, numeric_assays: bool = False,
                 drop_assay: bool = False, drop_limitations: bool = False) -> dict[str, Any]:
    case_root = output / "cases" / name
    case_root.mkdir(parents=True, exist_ok=False)
    copied_inputs = copy_public_inputs(source, case_root, ticket)

    ticket_source = source / "inst" / "examples" / "tickets" / f"{ticket}.yaml"
    ticket_source_hash = sha256(ticket_source)
    ticket_text = ticket_source.read_text(encoding="utf-8")
    metadata = None
    if taxon is not None:
        metadata = write_feature_taxon_metadata(case_root, taxon)
        ticket_text = configure_feature_taxon(ticket_text, enabled=True)
    else:
        ticket_text = configure_feature_taxon(ticket_text, enabled=False)
    ticket_path = case_root / "config" / f"{ticket}.yaml"
    ticket_path.parent.mkdir(parents=True, exist_ok=True)
    ticket_path.write_text(ticket_text, encoding="utf-8", newline="\n")

    assays = None
    if numeric_assays or drop_assay or drop_limitations:
        assays = write_numeric_assays(case_root, drop_assay=drop_assay,
                                      drop_limitations=drop_limitations)
    original_hashes = {
        relative: sha256(source / relative)
        for relative in source_input_map(source, ticket)
    }
    # Input hashes here describe the actual external copies consumed by the run.
    copied_hashes = {
        str(path.relative_to(case_root)): sha256(path)
        for path in sorted((case_root / "data").rglob("*")) if path.is_file()
    }
    control = {
        "name": name,
        "ticket": ticket,
        "ticket_path": str(ticket_path),
        "ticket_sha256": sha256(ticket_path),
        "source_ticket_sha256": ticket_source_hash,
        "ticket_adaptation": "block_2 feature_metadata only" if taxon is not None else "none; shipped ticket copied exactly",
        "case_root": str(case_root),
        "native_output": str(case_root / "native-output"),
        "input_hashes_from_source": original_hashes,
        "input_hashes_in_case": copied_hashes,
        "feature_taxon_control": metadata,
        "external_assay_control": assays,
        "public_synthetic_only": True,
    }
    require(ticket_path.resolve().is_relative_to(case_root.resolve()),
            "ticket escaped case root")
    return control


def run_native_case(source: Path, record: dict[str, Any], receipt_path: Path,
                    receipt: dict[str, Any], runtime_environment: dict[str, str]) -> None:
    native_root = Path(record["native_output"])
    require(not native_root.exists(), f"native output is not fresh: {native_root}")
    logs = Path(receipt["output_root"]) / "logs"
    logs.mkdir(parents=True, exist_ok=True)
    for stage in ("B", "C", "D"):
        command = [
            "Rscript", "--vanilla", str(Path("inst") / "exec" / "triomics"),
            "--config", record["ticket_path"], "--stage", stage,
            "--output", str(native_root),
        ]
        log_path = logs / f"{record['name']}-{stage}.log"
        started = utc_now()
        with log_path.open("wb") as log:
            process_environment = os.environ.copy()
            process_environment.update(runtime_environment)
            process = subprocess.Popen(command, cwd=source, stdout=log,
                                       stderr=subprocess.STDOUT, env=process_environment)
            pid = process.pid
            exit_code = process.wait()
            log.flush()
            os.fsync(log.fileno())
        stage_result = {
            "stage": stage,
            "command": command,
            "cwd": str(source),
            "pid": pid,
            "started_utc": started,
            "finished_utc": utc_now(),
            "exit_code": exit_code,
            "log": str(log_path),
            "log_sha256": sha256(log_path),
            "log_bytes": log_path.stat().st_size,
        }
        record["commands"].append(stage_result)
        write_receipt(receipt_path, receipt)
        require(exit_code == 0, f"{record['name']} stage {stage} exited {exit_code}; see {log_path}")
    readback = readback_case(source, record)
    record["readback"] = readback
    record["completed_utc"] = utc_now()
    write_receipt(receipt_path, receipt)


def row_source_path(case_root: Path, source_id: str) -> Path:
    mapping = {
        "block_1": case_root / "data" / "independent" / "proteomics.tsv",
        "block_2": case_root / "data" / "independent" / "metabolomics.tsv",
    }
    require(source_id in mapping, f"unexpected B/C measurement source id: {source_id}")
    return mapping[source_id]


def readback_case(source: Path, record: dict[str, Any]) -> dict[str, Any]:
    native_root = Path(record["native_output"])
    case_root = Path(record["case_root"])
    assertions: list[str] = []

    def check(condition: bool, label: str) -> None:
        require(condition, f"{record['name']}: {label}")
        assertions.append(label)

    check(record["commit"] == EXPECTED_COMMIT, "full immutable source commit recorded")
    source_ticket = source / "inst" / "examples" / "tickets" / f"{record['ticket']}.yaml"
    if record["feature_taxon_control"] is None:
        check(record["ticket_sha256"] == record["source_ticket_sha256"] == sha256(source_ticket),
              "native ticket is an exact external copy of the shipped ticket")
    else:
        check(record["source_ticket_sha256"] == sha256(source_ticket),
              "controlled ticket adaptation is based on the shipped public ticket")
    check(not run_capture(["git", "status", "--porcelain", "--untracked-files=all"], source),
          "source working tree remains clean")
    check(run_capture(["git", "rev-parse", "HEAD"], source) == record["commit"],
          "source commit unchanged after native stages")

    # Source data are byte-identical public synthetic copies. Controlled metadata
    # and assay records live only in this case's external fixture directory.
    for relative, original_hash in record["input_hashes_from_source"].items():
        source_path = source / relative
        if relative.endswith("external-assays.json") and record.get("external_assay_control"):
            continue
        if relative.endswith("feature-taxon-control.tsv"):
            continue
        copy_path = case_root / "data" / Path(*Path(relative).parts[3:])
        if copy_path.exists():
            check(sha256(source_path) == original_hash == sha256(copy_path),
                  f"unchanged public input bytes {relative}")
    matrix_hashes = {}
    for relative in MATRIX_PATHS:
        source_path = source / relative
        copy_path = case_root / "data" / Path(*Path(relative).parts[3:])
        check(sha256(source_path) == sha256(copy_path),
              f"original measurement matrix unchanged {relative}")
        matrix_hashes[relative] = sha256(copy_path)

    pointer_path = native_root / "registries" / "current-D.json"
    pointer = read_json(pointer_path)
    manifest_path = native_root / pointer["generation_manifest"]["path"]
    manifest = read_json(manifest_path)
    manifest_hash = sha256(manifest_path)
    check(manifest_hash == pointer["generation_id"] ==
          pointer["generation_manifest"]["digest"], "full D generation manifest SHA256")
    check(manifest_path.stat().st_size == pointer["generation_manifest"]["size_bytes"],
          "D manifest byte size")
    member_hashes: dict[str, str] = {}
    for member in manifest["members"]:
        member_path = manifest_path.parent / member["path"]
        check(member_path.resolve().is_relative_to(manifest_path.parent.resolve()) and
              not member_path.is_symlink(), f"D member confined to generation: {member['path']}")
        digest = sha256(member_path)
        check(digest == member["digest"] and member_path.stat().st_size == member["size_bytes"],
              f"D member bytes: {member['path']}")
        member_hashes[str(member_path.relative_to(native_root))] = digest

    b_handoffs = list(native_root.glob("B/handoffs/*/stage-handoff-provenance.json"))
    check(len(b_handoffs) == 1, "one native B handoff")
    b_prov = b_handoffs[0]
    b_base = b_prov.parent
    b_files = [
        b_base / "bundle.json",
        b_base / "bundle.envelope.json",
        b_base / "stage-handoff-provenance.json",
        b_base / "stage-handoff-provenance.envelope.json",
    ]
    check(all(path.is_file() and not path.is_symlink() for path in b_files),
          "all four native B predecessor files exist")
    b_hashes = {str(path.relative_to(native_root)): sha256(path) for path in b_files}

    c_prov = native_root / "D" / "stage-handoff-provenance.json"
    c_envelope = native_root / "D" / "stage-handoff-provenance.envelope.json"
    check(c_prov.is_file() and c_envelope.is_file(), "native C-to-D handoff files exist")
    c_hashes = {str(path.relative_to(native_root)): sha256(path)
                for path in (c_prov, c_envelope)}
    c_payload = read_json(c_prov)
    predecessor_digests = {
        entity.get("digest") for entity in c_payload.get("identity", {}).get("entities", [])
    }
    check(all(digest in predecessor_digests for digest in b_hashes.values()),
          "C handoff identity binds all four same-root B predecessor bytes")
    provenance = manifest["provenance"]
    for role, values in (
        ("b", {
            "bundle": (b_files[0], b_files[1]),
            "profile": (b_files[2], b_files[3]),
        }),
        ("c", {
            "profile": (c_prov, c_envelope),
        }),
    ):
        for kind, (payload_path, envelope_path) in values.items():
            expected = provenance[role][kind]
            check(expected["payload_sha256"] == sha256(payload_path),
                  f"D manifest {role} {kind} payload dependency SHA256")
            check(expected["envelope_sha256"] == sha256(envelope_path),
                  f"D manifest {role} {kind} envelope dependency SHA256")

    generation_hashes = {
        "registries/current-D.json": sha256(pointer_path),
        str(manifest_path.relative_to(native_root)): manifest_hash,
        **member_hashes,
    }

    # Read publication semantics from the immutable generation member, never
    # from an unverified convenience mirror.
    # Reconcile persisted B statistics and C loadings to the actual D observations.
    routing_members = [member for member in manifest["members"]
                       if member.get("path") == "evidence-routing.json"]
    check(len(routing_members) == 1, "one canonical evidence-routing generation member")
    routing_member = routing_members[0]
    target_file = manifest_path.parent / routing_member["path"]
    check(sha256(target_file) == routing_member["digest"] and
          target_file.stat().st_size == routing_member["size_bytes"],
          "canonical evidence-routing member matches its manifest digest and size")
    mirror_path = native_root / "D" / "evidence-routing.json"
    if mirror_path.exists():
        check(not mirror_path.is_symlink() and sha256(mirror_path) == sha256(target_file),
              "D convenience mirror equals the canonical generation member")
    payload = read_json(target_file)
    target = payload["target_investigation"]
    profiles = target["profiles"]
    check(bool(profiles), "target profiles persisted")
    check(all(profile.get("selected_target") is None for profile in profiles),
          "no per-profile target winner")
    if len(profiles) > 1:
        alternatives = target["alternatives"]
        check(alternatives.get("selected_target") is None and
              alternatives.get("tie_status") == "unresolved",
              "alternatives have no winner and remain unresolved")
        check(alternatives.get("comparability") == "noncomparable",
              "unresolved coverage prevents canonical comparison")

    b_results = read_json(native_root / "B" / "block-results.json")
    b_rows: dict[str, tuple[str, str, dict[str, Any]]] = {}
    for block_id, block in b_results["blocks"].items():
        for claim_id, claim in block["contrasts"].items():
            for row in claim["rows"]:
                b_rows[row["result_id"]] = (block_id, claim_id, row)

    integration_path = native_root / "D" / "integration-evidence.json"
    integrations_payload = read_json(integration_path)
    integrations = {row["integration_id"]: row for row in integrations_payload["integrations"]}
    nominations_payload = read_json(native_root / "D" / "candidate-nominations.json")
    nominations = {row["nomination_id"]: row for row in nominations_payload["nominations"]}

    case_assays = read_json(case_root / "data" / "d-target" / "external-assays.json")
    raw_assays = {row["evidence_id"]: row for row in case_assays["records"]}
    assay_input_hash = sha256(case_root / "data" / "d-target" / "external-assays.json")
    input_provenance = target.get("input_provenance", {})
    check(input_provenance.get("external_evidence_file", {}).get("sha256") == assay_input_hash,
          "D target record binds exact external assay fixture bytes")
    if (case_root / "data" / "d-target" / "mechanisms.json").exists():
        mechanism_hash = sha256(case_root / "data" / "d-target" / "mechanisms.json")
        check(input_provenance.get("mechanism_file", {}).get("sha256") == mechanism_hash,
              "D target record binds exact mechanism fixture bytes")

    taxon_control = record.get("feature_taxon_control")
    expected_taxon = taxon_control["taxon_id"] if taxon_control else None
    expected_ref = taxon_control["provenance_ref"] if taxon_control else None
    metadata_by_feature: dict[str, dict[str, str]] = {}
    if taxon_control:
        metadata_path = Path(taxon_control["path"])
        with metadata_path.open(encoding="utf-8", newline="") as stream:
            for row in csv.DictReader(stream, delimiter="\t"):
                metadata_by_feature[row["feature_id"]] = row
        check(bool(metadata_by_feature), "controlled feature taxon metadata rows persisted")
    profile_summaries = []
    b_value_signature: dict[str, Any] = {}
    c_value_signature: dict[str, Any] = {}

    for profile in profiles:
        request = profile["request"]
        target_id = request["id"]
        observations = profile["observations"]
        check(profile.get("axes") == [
            "metabolic_multiomics_compatibility", "functional_perturbation",
            "cellular_tissue_engagement", "direct_binding",
        ], f"{target_id}: distinct canonical axes retained")
        check(profile.get("selected_target") is None, f"{target_id}: no target selection")
        by_feature: dict[str, set[str]] = {}
        branches: set[str] = set()

        for observation in observations:
            branch = observation["source_branch"]
            branches.add(branch)
            source_id = observation["source_id"]
            source_path = row_source_path(case_root, source_id)
            source_hash = sha256(source_path)
            check(observation["source_sha256"] == source_hash,
                  f"{target_id}: actual measurement source SHA256")
            check(observation["source_selection_history"] == "full_unselected",
                  f"{target_id}: source selection history retained")
            check(observation["source_selection_evidence"] ==
                  "Public synthetic software fixture; no outcome-based selection.",
                  f"{target_id}: source selection evidence retained")
            with source_path.open(encoding="utf-8", newline="") as matrix_stream:
                measured_ids = [row[0] for row in csv.reader(matrix_stream, delimiter="\t")][1:]
            check(measured_ids.count(observation["feature_id"]) == 1,
                  f"{target_id}: source feature identity occurs exactly once")
            check(observation["feature_namespace"] == "synthetic",
                  f"{target_id}: measured feature namespace retained")
            by_feature.setdefault(observation["feature_key"], set()).add(observation["dependency_group"])
            if branch == "primary_statistics":
                block_id, claim_id, expected_row = b_rows[observation["result_id"]]
                check(block_id == observation["block_id"] and claim_id == observation["claim_id"] and
                      expected_row["feature_key"] == observation["feature_key"],
                      f"{target_id}: actual B result identity")
                check(observation["value"] == expected_row["effect"] == observation["effect"] and
                      observation["effect_unit"] == expected_row["effect_unit"],
                      f"{target_id}: actual B effect/value/unit preserved")
                b_value_signature[observation["result_id"]] = {
                    "block_id": block_id, "feature_key": observation["feature_key"],
                    "effect": expected_row["effect"], "effect_unit": expected_row["effect_unit"],
                    "source_sha256": source_hash,
                }
            elif branch == "integration_selection":
                nomination = nominations[observation["nomination_id"]]
                integration = integrations[observation["integration_id"]]
                selection_value = observation.get("selection_value") or {}
                block_id = observation.get("block_id") or selection_value.get("block_id")
                require(block_id, f"{target_id}: C observation lacks its selected block")
                value = integration["selection"]["selected_values"][block_id][
                    observation["feature_key"]
                ]["component_loadings"][observation["component_id"]]
                check(observation["value"] == value and observation.get("effect") is None and
                      observation["effect_unit"] == "loading",
                      f"{target_id}: actual C loading kept distinct from effect")
                check(observation["selection_rule"] == nomination["selection_rule"] and
                      observation["selection_value"] == nomination["selection_value"],
                      f"{target_id}: actual C selection lineage")
                c_value_signature[observation["nomination_id"] + ":" + observation["component_id"]] = {
                    "block_id": block_id, "feature_key": observation["feature_key"],
                    "component_id": observation["component_id"], "loading": value,
                    "integration_id": observation["integration_id"],
                }
            else:
                raise AssertionError(f"unexpected native target observation branch: {branch}")

            if expected_taxon is None:
                check("feature_taxon_id" in observation and observation["feature_taxon_id"] is None,
                      f"{target_id}: unknown feature taxon is persisted as JSON null")
                check("feature_taxon_declaration" in observation and
                      observation["feature_taxon_declaration"] is None,
                      f"{target_id}: missing taxon declaration origin is persisted as JSON null")
                check("feature_taxon_provenance_ref" in observation and
                      observation["feature_taxon_provenance_ref"] is None,
                      f"{target_id}: missing taxon provenance reference is persisted as JSON null")
                check(observation.get("feature_taxon_status") == "unknown" and
                      observation.get("taxon_applicability") == "unassessable" and
                      observation.get("taxon_reason_code") == "feature_taxon_unknown",
                      f"{target_id}: missing taxon remains unassessable")
                check(observation.get("coverage") == "unassessable",
                      f"{target_id}: unknown taxon coverage persists")
                check(observation.get("status") not in ("support", "conflict"),
                      f"{target_id}: unknown taxon cannot support or conflict")
            else:
                declaration = observation.get("feature_taxon_declaration")
                metadata_row = metadata_by_feature.get(observation["feature_id"])
                check(metadata_row is not None and
                      metadata_row.get("taxon_id") == expected_taxon and
                      metadata_row.get("provenance_ref") == expected_ref,
                      f"{target_id}: measured feature resolves to the external metadata row")
                check(isinstance(declaration, dict) and
                      declaration == {
                          "status": "declared", "value": metadata_row["taxon_id"],
                          "provenance_ref": metadata_row["provenance_ref"],
                      },
                      f"{target_id}: persisted taxon declaration comes from adapted routing metadata")
                check(observation.get("feature_taxon_id") == expected_taxon and
                      observation.get("feature_taxon_status") == "declared" and
                      observation.get("feature_taxon_provenance_ref") == expected_ref,
                      f"{target_id}: persisted feature taxon matches declaration and origin")
                if expected_taxon == "9606":
                    check(observation.get("taxon_applicability") == "matched" and
                          observation.get("taxon_reason_code") is None,
                          f"{target_id}: matching 9606 feature taxon applies")
                    check(observation.get("coverage") in ("measured", "unmeasured") and
                          observation.get("status") not in ("support", "conflict"),
                          f"{target_id}: matched unknown-direction feature remains non-claim")
                elif expected_taxon == "10090":
                    check(observation.get("taxon_applicability") == "inapplicable" and
                          observation.get("taxon_reason_code") == "feature_taxon_mismatch",
                          f"{target_id}: 10090 feature is inapplicable to 9606 target")
                    check(observation.get("coverage") == "inapplicable" and
                          observation.get("status") == "uninformative",
                          f"{target_id}: mismatch retains observation without support/conflict")
                elif expected_taxon == "unknown":
                    check(observation.get("taxon_applicability") == "unassessable" and
                          observation.get("taxon_reason_code") == "feature_taxon_unknown",
                          f"{target_id}: explicit unknown feature taxon remains unassessable")
                    check(observation.get("coverage") == "unassessable" and
                          observation.get("status") not in ("support", "conflict"),
                          f"{target_id}: unknown declaration retains no claim")

        check(bool(observations) and branches == {"primary_statistics", "integration_selection"},
              f"{target_id}: actual B and C producer branches persisted")
        check(all(len(groups) == 1 for groups in by_feature.values()) and
              profile["independent_count"] == len(by_feature),
              f"{target_id}: B/C reuse is not counted as independent confirmation")

        molecular = profile["evidence_axes"]["metabolic_multiomics_compatibility"]
        if expected_taxon is None or expected_taxon in ("10090", "unknown"):
            check(molecular["status"] == "unassessable",
                  f"{target_id}: nonmatching/unknown molecular taxon is unassessable")
        else:
            check(molecular["status"] == "measured",
                  f"{target_id}: declared matching molecular taxon is assessable")
        direct = profile["evidence_axes"]["direct_binding"]
        records = [row for row in profile["external_evidence"] if row["axis"] == "binding"]
        calculated = {
            "expected_records": len(records),
            "measured_records": 0,
            "unmeasured_records": 0,
            "unresolved_records": 0,
            "independent_publication_groups": len({
                row.get("dependency_group") for row in records if row.get("dependency_group")
            }),
        }
        for evidence in records:
            raw = raw_assays[evidence["evidence_id"]]
            if raw.get("target_id") == target_id and raw.get("value") is not None:
                check(evidence.get("attachment") == "request" and
                      evidence.get("availability") == "available",
                      f"{target_id}: exact numeric assay control is attached and available")
                check(evidence.get("axis") == "binding" and
                      evidence.get("endpoint") == "Kd" and evidence.get("units") == "nM",
                      f"{target_id}: exact numeric assay control keeps its binding axis and endpoint")
            for key in ASSAY_KEYS:
                if key in raw:
                    check(evidence.get(key) == raw[key],
                          f"{target_id}: external record preserves exact {key} for {evidence['evidence_id']}")
                else:
                    check(evidence.get(key) is None,
                          f"{target_id}: absent {key} remains absent/null for {evidence['evidence_id']}")
            attached = evidence.get("attachment") == "request"
            qualified = evidence.get("assay_qualification") == "qualified"
            available = evidence.get("availability") == "available"
            applicable = attached and qualified
            if applicable and available:
                calculated["measured_records"] += 1
            elif applicable:
                calculated["unmeasured_records"] += 1
            else:
                calculated["unresolved_records"] += 1
            if not raw.get("assay") and not raw.get("assay_limitations"):
                expected_reason = "assay_description_and_limitations_missing"
            elif not raw.get("assay"):
                expected_reason = "assay_description_missing"
            elif not raw.get("assay_limitations"):
                expected_reason = "assay_limitations_missing"
            else:
                expected_reason = None
            check(evidence.get("assay_qualification") ==
                  ("qualified" if expected_reason is None else "unresolved"),
                  f"{target_id}: record-derived assay qualification")
            check(evidence.get("assay_qualification_reason") == expected_reason,
                  f"{target_id}: record-derived assay qualification reason")
        check(direct.get("evidence") == records,
              f"{target_id}: direct-binding evidence equals its persisted assay records")
        expected_status = (
            "measured" if calculated["measured_records"] else
            "unresolved" if calculated["unresolved_records"] else "unmeasured"
        )
        check(direct["coverage"] == calculated and direct["status"] == expected_status,
              f"{target_id}: direct-binding aggregate derived from persisted records")
        check(direct.get("status") != "unmeasured" or
              (calculated["measured_records"] == 0 and calculated["unresolved_records"] == 0),
              f"{target_id}: direct-binding status is never guessed from a single record")
        profile_summaries.append({
            "target_id": target_id,
            "observation_count": len(observations),
            "molecular_status": molecular["status"],
            "direct_binding_status": direct["status"],
            "direct_binding_coverage": calculated,
            "external_evidence": [
                {
                    "evidence_id": row["evidence_id"],
                    "attachment": row["attachment"],
                    "assay_qualification": row["assay_qualification"],
                    "assay_qualification_reason": row.get("assay_qualification_reason"),
                    "endpoint": row["endpoint"], "value": row["value"],
                    "source_id": row["source_id"],
                }
                for row in records
            ],
        })

    check(bool(b_value_signature) and bool(c_value_signature),
          "actual B effects and C loadings are recorded in read-back")
    return {
        "assertions_passed": len(assertions),
        "assertions": assertions,
        "profiles": profile_summaries,
        "measurement_matrix_sha256": matrix_hashes,
        "actual_B_effects": b_value_signature,
        "actual_C_loadings": c_value_signature,
        "predecessor_sha256": {"B": b_hashes, "C": c_hashes},
        "generation_sha256": generation_hashes,
        "generation_id": pointer["generation_id"],
        "generation_manifest_relative_path": str(manifest_path.relative_to(native_root)),
        "target_routing_sha256": sha256(target_file),
        "external_assay_fixture_sha256": assay_input_hash,
    }


def main() -> int:
    if len(sys.argv) != 3:
        print(f"usage: {Path(sys.argv[0]).name} SOURCE FRESH_OUTPUT", file=sys.stderr)
        return 2
    source = Path(sys.argv[1]).resolve(strict=True)
    raw_output = Path(sys.argv[2]).absolute()
    require(not raw_output.is_symlink(), "FRESH_OUTPUT must not be a symlink")
    output = raw_output.resolve()
    require(re.fullmatch(r"[0-9a-f]{40}", EXPECTED_COMMIT) is not None,
            "TRIOMICS_EXPECTED_COMMIT must be a full 40-character lowercase commit")
    require(source.is_dir(), f"source must be a directory: {source}")
    require(not output.exists() or (output.is_dir() and not any(output.iterdir())),
            f"FRESH_OUTPUT must be absent or empty: {output}")
    require(not output.is_symlink(), "FRESH_OUTPUT must not be a symlink")
    output.mkdir(parents=True, exist_ok=True)
    output = output.resolve(strict=True)
    require(not output.is_relative_to(source), "FRESH_OUTPUT must be outside the source checkout")

    commit = load_commit_and_clean(source)
    proof_root = Path(__file__).resolve().parent.parent
    runtime_environment_path = proof_root / "runtime" / "env.json"
    require(runtime_environment_path.is_file(),
            f"isolated runtime environment receipt is missing: {runtime_environment_path}")
    runtime_environment = read_json(runtime_environment_path)
    required_runtime_keys = (
        "R_LIBS_USER", "TRIOMICS_MOFA_LIBRARY",
        "TRIOMICS_MOFA_PYTHON", "RETICULATE_PYTHON",
    )
    require(all(isinstance(runtime_environment.get(key), str) and
                runtime_environment[key] for key in required_runtime_keys),
            "isolated runtime receipt lacks required R/MOFA/Python settings")
    receipt_path = output / "runner-receipt.json"
    receipt: dict[str, Any] = {
        "schema": "triomics.m7-029-resume-verification-receipt",
        "schema_version": "1.0",
        "started_utc": utc_now(),
        "source": str(source),
        "commit": commit,
        "default_expected_commit": DEFAULT_EXPECTED_COMMIT,
        "expected_commit": EXPECTED_COMMIT,
        "expected_commit_environment_override": os.environ.get("TRIOMICS_EXPECTED_COMMIT"),
        "invocation": [sys.executable, str(Path(__file__).resolve()), str(source), str(output)],
        "source_clean_at_start": True,
        "output_root": str(output),
        "runtime_environment_receipt": str(runtime_environment_path),
        "runtime_environment_receipt_sha256": sha256(runtime_environment_path),
        "runtime_environment_applied_keys": list(required_runtime_keys),
        "fixture_policy": "public synthetic fixture copies only; original source inputs are read-only",
        "control_adaptations": [
            "D10/D11/D12 shipped tickets are copied and run unchanged for native positives.",
            "D11 controlled copies may add feature taxon metadata and/or public synthetic numeric assay controls.",
            "Pass the package entry script relative to source CWD because Rscript rewrites spaces in an absolute --file path.",
            "Assay control values 12.5 and 25.0 nM are synthetic software controls, not biological measurements.",
            "All source measurement matrices are copied byte-for-byte and their SHA256 values are checked.",
            "The prior aggregate binding expectation is derived per record; an unavailable match plus a mismatch yields measured=0, unmeasured=1, unresolved=1.",
            "No B/C bytes are synthesized; all B effects and C loadings are read from native persisted JSON.",
            "D01 regression is outside this runner; its interrupted prior result remains unknown and is not reported as passed.",
        ],
        "cases": [],
        "d01_regression": {
            "status": "not_run",
            "prior_interrupted_result": "unknown",
            "note": "This bounded native D continuation does not infer or report a D01 pass.",
        },
    }
    write_receipt(receipt_path, receipt)

    case_specs = [
        ("native-D10", "D-T10", None, False, False, False),
        ("native-D11", "D-T11", None, False, False, False),
        ("native-D12", "D-T12", None, False, False, False),
        ("control-match-9606", "D-T11", "9606", True, False, False),
        ("control-mismatch-10090", "D-T11", "10090", True, False, False),
        ("control-unknown-taxon", "D-T11", "unknown", True, False, False),
        ("control-missing-feature-taxon", "D-T11", None, True, False, False),
        ("control-missing-assay", "D-T11", "9606", False, True, False),
        ("control-missing-assay-limitations", "D-T11", "9606", False, False, True),
        ("control-both-assay-fields-absent", "D-T11", "9606", False, True, True),
    ]

    for name, ticket, taxon, numeric, drop_assay, drop_limitations in case_specs:
        case = prepare_case(source, output, name, ticket, taxon=taxon,
                            numeric_assays=numeric or drop_assay or drop_limitations,
                            drop_assay=drop_assay, drop_limitations=drop_limitations)
        case["commit"] = commit
        case["commands"] = []
        receipt["cases"].append(case)
        write_receipt(receipt_path, receipt)
        run_native_case(source, case, receipt_path, receipt, runtime_environment)

    native_d11 = next(case for case in receipt["cases"] if case["name"] == "native-D11")
    baseline_b = native_d11["readback"]["actual_B_effects"]
    baseline_c = native_d11["readback"]["actual_C_loadings"]
    for case in receipt["cases"]:
        if case["ticket"] != "D-T11" or case["name"] == "native-D11":
            continue
        require(case["readback"]["actual_B_effects"] == baseline_b,
                f"{case['name']}: controlled D metadata changed native B effect values")
        require(case["readback"]["actual_C_loadings"] == baseline_c,
                f"{case['name']}: controlled D metadata changed native C loading values")
        case["b_c_value_invariance"] = {
            "status": "verified",
            "reference_case": "native-D11",
            "actual_B_effect_count": len(baseline_b),
            "actual_C_loading_count": len(baseline_c),
        }
    require(load_commit_and_clean(source) == commit,
            "source commit or cleanliness changed during verification")
    receipt["finished_utc"] = utc_now()
    receipt["completed"] = True
    receipt["case_count"] = len(receipt["cases"])
    receipt["stage_count"] = sum(len(case["commands"]) for case in receipt["cases"])
    receipt["readback_assertions_passed"] = sum(
        case["readback"]["assertions_passed"] for case in receipt["cases"]
    )
    write_receipt(receipt_path, receipt)
    print(json.dumps({
        "receipt": str(receipt_path),
        "commit": commit,
        "cases": receipt["case_count"],
        "native_stage_commands": receipt["stage_count"],
        "readback_assertions_passed": receipt["readback_assertions_passed"],
        "exit_code": 0,
    }, indent=2))
    return 0


if __name__ == "__main__":
    try:
        exit_code = main()
    except Exception as error:
        # A failed assertion or subprocess is durable evidence, not a repair cue.
        if len(sys.argv) == 3:
            candidate_output = Path(sys.argv[2]).resolve()
            receipt_path = candidate_output / "runner-receipt.json"
            if receipt_path.exists():
                try:
                    failure_receipt = read_json(receipt_path)
                    failure_receipt["completed"] = False
                    failure_receipt["failure_utc"] = utc_now()
                    failure_receipt["failure_type"] = type(error).__name__
                    failure_receipt["failure"] = str(error)
                    write_receipt(receipt_path, failure_receipt)
                except Exception:
                    pass
        print(f"verification stopped: {type(error).__name__}: {error}", file=sys.stderr)
        raise
    raise SystemExit(exit_code)

