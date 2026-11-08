# RDY-001 exact-hash read-back

Run from the canonical Git repository. This read-only procedure verifies the immutable reviewed commit and all177 bound blobs, then the current92-file scientific content. It rejects unknown drift. It does not run the old delivery validator or relabel its recorded failure. The manifest itself is read from the reviewed commit, not from a mutable working copy. Changing review-context records after acceptance does not retroactively alter their reviewed versions.

```sh
python3 - <<'PYCODE'
import hashlib, json, pathlib, subprocess
revision = "6e77395360955d18258a29cd055038fcf7feb0bd"
expected_context = "b34bbfec9ef403065dee9399c9befbe7895e37a4e60131c855ae3187f3395b0c"
expected_content = "461f3d14a32a3b84cc765f44d6ef76f8073aa64c147f34d75c7ff38c6158a4b8"
def blob(path):
    return subprocess.check_output(["git", "show", revision + ":" + path])
def digest(value):
    return hashlib.sha256(value).hexdigest()
manifest = json.loads(blob("docs/implementation/readiness/RDY-001-review-manifest.json"))
entries = manifest["reviewed_files"]
assert len(entries) == 177
assert digest(json.dumps(entries, sort_keys=True, separators=(",", ":")).encode()) == expected_context
for entry in entries:
    assert digest(blob(entry["path"])) == entry["sha256"], entry["path"]
freeze = json.loads(blob("discovery/spec-v2/freeze.json"))
current = [{"path": x["path"], "sha256": digest(pathlib.Path(x["path"]).read_bytes())}
           for x in freeze["content_files"]]
assert len(current) == 92
assert digest(json.dumps(current, sort_keys=True, separators=(",", ":")).encode()) == expected_content
print("RDY-001 READ-BACK: PASS (177 immutable blobs; 92 current content files)")
PYCODE
```

Sol executed these same assertions before recording acceptance; the local rdy001-post-audit-readback.json records the result. This is a current exact-byte approval of the reviewed specification/context, not future amendment approval or biological validation.
