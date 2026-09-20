#!/usr/bin/env python3
"""Validate the delivered triomics specification, not the unimplemented R pipeline.

Requires jsonschema and PyYAML. No network calls, data uploads, or pipeline/Git mutations. Optional report output only.
Run from any directory. --verify-delivery additionally checks every delivered file.
"""
from __future__ import annotations
import argparse, copy, csv, hashlib, json, re, sys
from pathlib import Path
from itertools import combinations
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
SPEC = ROOT / 'discovery' / 'spec-v2'

def load(path: Path) -> Any:
    return json.loads(path.read_text(encoding='utf-8'))

def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()

def demand(value: bool, message: str) -> None:
    if not value:
        raise ValueError(message)

def topological(nodes: dict[str, set[str]]) -> list[list[str]]:
    pending={k:set(v) for k,v in nodes.items()}; done:set[str]=set(); waves=[]
    for n,deps in pending.items():
        demand(deps <= pending.keys(), f'Unknown dependency for {n}: {deps-pending.keys()}')
    while pending:
        ready=sorted(n for n,d in pending.items() if d <= done)
        demand(bool(ready), 'Dependency/review-gate cycle detected')
        waves.append(ready); done.update(ready)
        for n in ready: pending.pop(n)
    return waves

def semantic(config: dict[str, Any]) -> None:
    blocks=config['blocks']; ids=[b['block_id'] for b in blocks]
    demand(len(ids)==len(set(ids)), 'Duplicate block ID')
    srcs=[s['source_id'] for s in config['sources']]
    demand(len(srcs)==len(set(srcs)), 'Duplicate source ID')
    for b in blocks:
        demand(set(b['source_refs']) <= set(srcs), 'Unresolved block source ref')
        pp=b['preprocessing']; sc=b['measurement']['scale']
        demand(not(sc in ['log2','ln','clr_ln','clr_log2'] and pp['transform']['method']=='log2'), 'Double log')
        demand(not(pp['normalisation']['method']=='vsn' and pp['transform']['method']!='none'), 'VSN double transform')
        if pp['normalisation']['method']=='pqn':
            demand(sc=='linear', 'PQN requires declared linear input')
        if pp['imputation']['method'] in ['minprob','qrilc']:
            demand(b['measurement']['missingness_assumption'] in ['mnar_left_censored','mixed'], 'Censoring assumption unqualified')
            demand(bool(pp['imputation']['qualification_ref']), 'Missing imputation qualification')
    aid=[a['analysis_id'] for a in config['analyses']]
    demand(len(aid)==len(set(aid)), 'Duplicate analysis ID')
    for a in config['analyses']:
        demand(set(a['block_ids']) <= set(ids), 'Unknown analysis block')
        demand(len(a['block_ids'])==len(set(a['block_ids'])), 'Repeated block in analysis')
        d=a['design']
        if d['repeated']:
            demand(bool(d['subject_id']), 'Repeated design has no subject')
        if d['representation']=='delta':
            demand(bool(d['visit_column']) and bool(d['reference_visit']) and bool(d['target_visit']), 'Delta missing visit semantics')
            demand(d['target_visit']!=d['reference_visit'], 'Identical reference/target visit')
        if d['representation']=='mean':
            demand(bool(d.get('mean_weights')), 'Mean representation has no weights')
        for it in a['integration']:
            demand(len(a['block_ids'])>=2, 'Integration requires two blocks')
            if it['method'] in ['mixomics_diablo','mixomics_block_spls']:
                mat=it['design_matrix']; expected=set(a['block_ids'])
                demand(isinstance(mat,dict) and set(mat)==expected, 'Integration design names missing/mismatched')
                for i in expected:
                    demand(set(mat[i])==expected, 'Integration design row names mismatched')
                    demand(mat[i][i]==0, 'Nonzero integration design diagonal')
                    for j in expected: demand(mat[i][j]==mat[j][i], 'Asymmetric integration design')
                demand(it['score_definition']=='consensus_mean', 'Missing consensus policy')
            if it['evaluation']['mode']=='nested_cv':
                demand(it['evaluation']['primary_metric']!='none', 'CV without a metric')
                if d['repeated']: demand(it['evaluation']['generalization_unit']=='subject', 'Repeated CV leaks subjects')
    if config['clinical']['enabled']:
        demand(all(config['clinical'][k] for k in ['dictionary_file','observations_file','links_file']), 'Missing clinical schema sources')
    t=config['target_investigation']
    if t['enabled']:
        demand(bool(t['context_ref']) and bool(t['protocol_ref']), 'Missing target context/protocol')
        if t['mode']=='discover': demand(bool(t['universe_ref']), 'Missing target universe')
    if config['data_class']=='real':
        demand(config['execution']['audience']!='synthetic_demo', 'Real data labelled synthetic audience')

def validate(verify_delivery: bool=False) -> dict[str, Any]:
    try:
        import yaml
        from jsonschema import Draft202012Validator
    except ImportError as exc:
        raise RuntimeError('Specification checks require jsonschema and PyYAML; no automatic installation was attempted.') from exc
    checks=[]
    # Parse every candidate JSON, including role requests and schemas.
    all_json=list(SPEC.rglob('*.json'))
    for f in all_json: load(f)
    checks.append({'check':'candidate_json_parse','files':len(all_json),'status':'pass'})
    ps=load(SPEC/'packets.json')['packets']; byid={p['id']:p for p in ps}
    expected={f'A-T{i:02d}' for i in range(1,10)} | {f'B-T{i:02d}' for i in range(1,10)} | {f'C-{i:02d}' for i in [1,2,3,4,5,6,8,9]} | {f'D-T{i:02d}' for i in range(1,16)}
    demand(len(ps)==41 and set(byid)==expected, 'Historical 41-ticket crosswalk mismatch')
    checks.append({'check':'historical_ticket_identities','count':41,'status':'pass'})
    waves=topological({p['id']:set(p['dependencies']) for p in ps})
    ms=load(SPEC/'milestones.json')['milestones']
    docs=load(SPEC/'documentation-packets.json')['packets']
    graph={'G0':set()}
    for p in ps:graph[p['id']]=set(p['dependencies'])|set(p['requires_review_gates'])|{'G0'}
    for d in docs:graph[d['id']]=set(d['finalize_dependencies'])|{d['draft_requires_gate']}
    for m in ms:graph[m['gate_id']]=set(m['packets'])|{m['required_documentation']}
    allwaves=topological(graph)
    demand(len(ms)==9 and len(docs)==9,'Nine milestones/documentation packets required')
    for m in ms:
        demand(m['reviewer_role']=='astra' and m['requested_effort']=='low','Wrong milestone reviewer')
        demand(set(m['packets'])=={p['id'] for p in ps if p['milestone']==m['id']},'Milestone packet mismatch')
    checks.append({'check':'implementation_and_review_dags','packet_waves':len(waves),'combined_nodes':len(graph),'combined_waves':len(allwaves),'status':'pass'})
    ownership={}
    for p in ps:
        demand(p['files_allowed'] and p['acceptance'] and p['tests'],f'Incomplete card {p["id"]}')
        for path in p['files_allowed']:
            demand(path not in ownership,f'Shared worker write ownership: {path}')
            ownership[path]=p['id']
        demand((SPEC/'packets'/f'{p["id"]}.md').exists(),'Missing packet card')
        for a in p['acceptance']:
            demand(a['id'] in (SPEC/'packets'/f'{p["id"]}.md').read_text(), 'Acceptance/card drift')
    for d in docs:
        for path in d['files_allowed']:
            demand(path not in ownership, 'Code/documentation write collision'); ownership[path]=d['id']
    checks.append({'check':'exclusive_code_doc_ownership','paths':len(ownership),'status':'pass'})
    reqs=load(SPEC/'requirements.json')['requirements']; validreq={r['id'] for r in reqs}
    for r in reqs:
        demand(bool(r['packets']),f'Uncovered requirement {r["id"]}')
        demand(set(r['packets'])=={p['id'] for p in ps if r['id'] in p['requirements']},'Requirement/card trace drift')
    for p in ps: demand(set(p['requirements'])<=validreq,'Unknown requirement')
    checks.append({'check':'requirement_traceability','count':len(reqs),'status':'pass'})
    schema=load(SPEC/'schemas/config.schema.json'); Draft202012Validator.check_schema(schema)
    Draft202012Validator.check_schema(load(SPEC/'schemas/artifact-envelope.schema.json'))
    validator=Draft202012Validator(schema)
    examples=[]
    for path in sorted((SPEC/'examples').glob('*/config.json')):
        conf=load(path); validator.validate(conf); semantic(conf)
        demand(yaml.safe_load(path.with_suffix('.yaml').read_text())==conf, 'JSON/YAML config mismatch')
        for source in conf['sources']:
            f=path.parent/source['file']; demand(f.is_file(),f'Missing fixture {f}')
            demand(digest(f)==source['sha256'], 'Fixture source hash mismatch')
        with (path.parent/conf['metadata']['file']).open() as f:
            metadata=list(csv.DictReader(f,delimiter='\t'))
        sample_ids=[r[conf['metadata']['sample_id']] for r in metadata]
        demand(len(sample_ids)==len(set(sample_ids)), 'Duplicate synthetic sample IDs')
        for b in conf['blocks']:
            with (path.parent/b['file']).open() as f: rows=list(csv.reader(f,delimiter='\t'))
            demand(rows[0][1:]==sample_ids,'Fixture sample map mismatch')
            demand(all(len(r)==len(rows[0]) for r in rows),'Ragged fixture')
            demand(len({r[0] for r in rows[1:]})==len(rows)-1,'Duplicate fixture features')
            demand(all(float(v)>0 for row in rows[1:] for v in row[1:]),'Fixture has unexpected nonpositive value')
        examples.append({'kind':path.parent.name,'samples':len(sample_ids),'blocks':len(conf['blocks']),'status':'pass'})
    demand(len(examples)==2,'Exactly two delivered synthetic fixture studies expected')
    checks.append({'check':'schemas_synthetic_configs_sources','examples':examples,'status':'pass'})
    # Negative controls for the validator itself, not R statistical tests.
    original=load(SPEC/'examples/paired/config.json')
    mutations=[]
    bad=copy.deepcopy(original);bad['unknown_key']=1;mutations.append(('unknown_key',bad))
    bad=copy.deepcopy(original);bad['blocks'][0]['profile']='invented';mutations.append(('unknown_profile',bad))
    bad=copy.deepcopy(original);bad['blocks'][0]['measurement']['scale']='log2';mutations.append(('double_log',bad))
    bad=copy.deepcopy(original);bad['analyses'][0]['design']['subject_id']=None;mutations.append(('repeated_no_subject',bad))
    bad=copy.deepcopy(original);bad['analyses'][0]['design']['target_visit']='baseline';mutations.append(('same_visits',bad))
    bad=copy.deepcopy(original);bad['analyses'][0]['block_ids']=['block_1'];mutations.append(('one_block_integration',bad))
    bad=copy.deepcopy(original);bad['analyses'][0]['integration'][0]['method']='mixomics_diablo';mutations.append(('missing_design_matrix',bad))
    bad=copy.deepcopy(original);bad['clinical']['enabled']=True;mutations.append(('missing_clinical_sources',bad))
    bad=copy.deepcopy(original);bad['target_investigation']['enabled']=True;mutations.append(('missing_target_context',bad))
    for name, conf in mutations:
        rejected=False
        try:validator.validate(conf);semantic(conf)
        except Exception:rejected=True
        demand(rejected,f'Negative validator control accepted: {name}')
    checks.append({'check':'negative_spec_controls','cases':[x[0] for x in mutations],'status':'pass'})
    # Check links in new normative markdown; historical paths outside overlay are not invented as local copies.
    broken=[]; checked=0
    for f in SPEC.rglob('*.md'):
        for target in re.findall(r'\]\(([^)]+)\)',f.read_text()):
            if '://' in target or target.startswith('#'):continue
            target=target.split('#',1)[0]
            if not target:continue
            dest=(f.parent/target).resolve()
            # Canonical in-kit links must exist; historical references are separately documented.
            if SPEC==dest or SPEC in dest.parents or dest==ROOT/'docs/adr/0002-spec-v2-hardening.md':
                checked+=1
                if not dest.exists():broken.append(str(f.relative_to(ROOT))+':'+target)
    demand(not broken,'Broken canonical links: '+', '.join(broken))
    checks.append({'check':'canonical_markdown_links','count':checked,'status':'pass'})
    fr=SPEC/'freeze.json'
    if fr.exists():
        freeze=load(fr)
        entries=[]
        for item in freeze['content_files']:
            p=ROOT/item['path']; demand(p.is_file(),'Frozen content file absent')
            actual=digest(p); demand(actual==item['sha256'],f'Frozen content changed: {item["path"]}')
            entries.append(item)
        actualhash=hashlib.sha256(json.dumps(entries,sort_keys=True,separators=(',',':')).encode()).hexdigest()
        demand(actualhash==freeze['content_sha256'],'Content-set hash mismatch')
        demand(freeze['independent_audit_status']=='pending','Delivery may not invent independent review')
        checks.append({'check':'frozen_candidate_hashes','files':len(entries),'status':'pass'})
    if verify_delivery:
        manifest=load(ROOT/'FILE_MANIFEST.json')
        for entry in manifest['files']:
            demand(digest(ROOT/entry['path'])==entry['sha256'],'Delivery checksum mismatch: '+entry['path'])
        checks.append({'check':'full_delivery_manifest','files':len(manifest['files']),'status':'pass'})
    return {'status':'pass','scope':'specification_consistency_only','checks':checks,'implementation_packets':41,
            'required_packets':sum(p['scope_type']=='required' for p in ps),'conditional_packets':[p['id'] for p in ps if p['scope_type']=='conditional'],
            'pipeline_R_tests_run':False,'real_data_validated':False,'independent_spec_audit_performed':False,'GitHub_push_performed':False}

def main() -> int:
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--verify-delivery',action='store_true')
    parser.add_argument('--json-out',type=Path)
    args=parser.parse_args()
    try:result=validate(args.verify_delivery)
    except Exception as exc:
        print(f'SPEC VALIDATION FAILED: {exc}',file=sys.stderr);return 1
    text=json.dumps(result,indent=2)
    print(text)
    if args.json_out:
        args.json_out.parent.mkdir(parents=True,exist_ok=True);args.json_out.write_text(text+'\n')
    return 0
if __name__=='__main__':raise SystemExit(main())
