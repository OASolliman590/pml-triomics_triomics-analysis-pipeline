from pathlib import Path
import hashlib,json,sys
r=Path(sys.argv[1]).resolve();assert r.is_dir()
h=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
def read(p):return json.loads(p.read_text())
def safe(rel):
 p=r/rel
 assert not Path(rel).is_absolute() and '..' not in Path(rel).parts
 for q in [p,*p.parents]:
  if q==r.parent:break
  assert not q.is_symlink(),str(q)
 assert p.is_file(),str(p)
 return p
pp=safe('registries/current-D.json');p=read(pp)
assert set(p)=={'schema_version','study_id','run_id','generation_id','generation_manifest'}
mref=p['generation_manifest'];mp=safe(mref['path']);m=read(mp)
assert h(mp)==p['generation_id']==mref['digest'];assert mp.stat().st_size==mref['size_bytes']
assert set(m)=={'profile','profile_version','study_id','run_id','producer','status','limitations','members','provenance'}
assert m['profile']=='triomics.output-generation-manifest' and m['profile_version']=='1.0' and m['status']=='complete'
assert p['study_id']==m['study_id'] and p['run_id']==m['run_id']
assert mp.parent.name==p['generation_id']
paths=[]
for a in m['members']:
 assert set(a) <= {'path','scientific_role','artifact_type','media_type','status','size_bytes','digest_algorithm','digest','envelope_for_artifact_id'}
 q=safe(str(mp.parent.relative_to(r)/a['path']));paths.append(a['path'])
 assert h(q)==a['digest'] and q.stat().st_size==a['size_bytes'] and a['digest_algorithm']=='sha256'
actual=sorted(str(q.relative_to(mp.parent)) for q in mp.parent.rglob('*') if q.is_file())
assert sorted(paths+['generation-manifest.json'])==actual
assert paths==sorted(set(paths))
y=read(mp.parent/'evidence-routing.json')
controls=y['inherited_evidence']['evaluation_evidence']['controls']
assert len(controls)==4 and all(c['status']=='not_run' for c in controls)
assert y['inherited_evidence']['stage_status']['clinical_prediction']['completion_policy']=='conditional'
assert y['routes'] and all(v['status']=='not_requested' and v['reason_code']=='D_ROUTE_DISABLED' for v in y['routes'].values())
bp=list((r/'B').rglob('stage-handoff-provenance.json'));assert len(bp)==1
cp=safe('D/stage-handoff-provenance.json');ce=read(cp)['identity']['entities']
bfiles=[bp[0].parent/'bundle.json',bp[0].parent/'bundle.envelope.json',bp[0],bp[0].with_name('stage-handoff-provenance.envelope.json')]
pred=[e['digest'] for e in ce if e['scientific_role'] in ('predecessor_payload','predecessor_envelope')]
assert all(h(f) in pred for f in bfiles)
files=bfiles+[cp,cp.with_name('stage-handoff-provenance.envelope.json'),mp,pp,mp.parent/'evidence-routing.json',mp.parent/'evidence-routing.envelope.json']
result={'root':str(r),'generation_id':p['generation_id'],'files':{str(f.relative_to(r)):h(f) for f in files},'routes':list(y['routes']),'controls_not_run':len(controls)}
Path(sys.argv[2]).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
