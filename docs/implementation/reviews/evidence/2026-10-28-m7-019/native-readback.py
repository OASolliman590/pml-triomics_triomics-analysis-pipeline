import json,hashlib
from pathlib import Path
b=Path('/tmp/triomics-m7-019');ev=json.loads((b/'native-evidence.json').read_text());r=Path(ev['output_root'])
def read(p):return json.loads(p.read_text())
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def values(x):return list(x.values()) if isinstance(x,dict) else x
bp=list(r.glob('B/handoffs/*/stage-handoff-provenance.json'));assert len(bp)==1
bd=bp[0].parent
bf=[bd/n for n in ['bundle.json','bundle.envelope.json','stage-handoff-provenance.json','stage-handoff-provenance.envelope.json']]
cp=r/'D/stage-handoff-provenance.json';ce=r/'D/stage-handoff-provenance.envelope.json'
entities=read(cp)['identity']['entities'];digests={e['digest'] for e in entities}
assert all(sha(f) in digests for f in bf)
ptr=r/'registries/current-D.json';p=read(ptr);mp=r/p['generation_manifest']['path'];m=read(mp)
assert sha(mp)==p['generation_id']==p['generation_manifest']['digest']
assert mp.stat().st_size==p['generation_manifest']['size_bytes']
assert m['provenance']['b']['profile']['payload_sha256']==sha(bp[0])
assert m['provenance']['b']['profile']['envelope_sha256']==sha(bf[3])
assert m['provenance']['b']['bundle']['payload_sha256']==sha(bf[0])
assert m['provenance']['b']['bundle']['envelope_sha256']==sha(bf[1])
assert m['provenance']['c']['profile']['payload_sha256']==sha(cp)
assert m['provenance']['c']['profile']['envelope_sha256']==sha(ce)
members=[]
for member in m['members']:
 f=mp.parent/member['path'];assert f.resolve().is_relative_to(mp.parent.resolve());assert sha(f)==member['digest'];assert f.stat().st_size==member['size_bytes'];members.append(f)
ep=r/'C/score-endpoints/integration-01.json';endpoints=values(read(ep)['endpoints']);assert endpoints
for e in endpoints:
 rows=[u for u in e['unit_map'] if u['unit_id']=='SYN_12'];assert len(rows)==1
 assert 'time' in rows[0] and rows[0]['time'] is None and rows[0]['eligibility']=='excluded'
 assert not any(s.get('sample_id')=='SYN_12' for s in e['samples'])
 assert 'SYN_12' not in e['training_unit_ids']
files=bf+[cp,ce,mp,ptr,ep]+members
result={'root':str(r),'generation_id':p['generation_id'],'persisted_time':None,'excluded_source':'SYN_12','four_b_predecessors_verified':True,'files':{str(f.relative_to(r)):sha(f) for f in files}}
(b/'native-independent-readback.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
