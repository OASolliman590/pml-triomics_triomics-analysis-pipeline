"""Independent persisted ORA count and byte-binding proof; run after native-packet-proof."""
import hashlib,json,math,sys
from pathlib import Path
source=Path(sys.argv[1]);root=Path(sys.argv[2]);outfile=Path(sys.argv[3])
def read(p):return json.loads(p.read_text())
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def members(x):return [x] if isinstance(x,str) else x
p=read(root/'registries/current-D.json');d=root/'D/generations'/p['generation_id'];x=read(d/'evidence-routing.json')
assert x['routes']['ora']['status']=='complete'
claims=x['routes']['ora']['claims'];assert claims
results=[]
for c in claims:
 assert c['inventory']['status']=='full_unselected'
 u=c['measured_universe'];ids=u['ids'];assert len(ids)==len(set(ids))==10
 assert len(u['included'])==10 and len(u['excluded'])==0
 keys=[v['feature_key'] for v in u['included']];assert len(set(keys))==10
 assert set(c['selected_entities']['feature_keys']).issubset(keys)
 f=c['set_family'];assert f['declared_count']==3 and f['result']['full_family_size']==3
 assert len(f['declared'])==len(f['result']['sets'])==3
 for declared,result in zip(f['declared'],f['result']['sets']):
  assert result['set_id']==declared['set_id']
  N,K,n,k=(result[z] for z in ('N','K','n','k'))
  assert N==len(ids) and K==len(set(members(declared['members']))&set(ids))
  assert n==len(c['selected_entities']['entity_ids'])
  assert k==len(set(members(declared['members']))&set(c['selected_entities']['entity_ids']))
  if result['status']=='estimable':
   hand=sum(math.comb(K,j)*math.comb(N-K,n-j) for j in range(k,min(K,n)+1) if 0<=n-j<=N-K)/math.comb(N,n)
   assert abs(hand-result['p'])<1e-12
  else:assert result['p'] is None and result['q'] is None
 first=f['result']['sets'][0]
 if c['identity']['source_branch']=='integration_selection':
  assert [first[z] for z in ('N','K','n','k')]==[10,3,1,1]
  assert abs(first['p']-.3)<1e-12 and abs(first['q']-.3)<1e-12
 assert c['bindings']['protocol_sha256']==sha(source/'inst/examples/data/d-ora/ora-protocol.json')
 assert c['bindings']['mapping_sha256']==sha(source/'inst/examples/data/d-ora/gene-mapping.tsv')
 results.append({'identity':c['identity'],'first_set':first,'universe_count':len(ids),'original_features':len(keys),'bindings':c['bindings']})
outfile.write_text(json.dumps({'generation_id':p['generation_id'],'claim_readback':results,'interpretation':x['interpretation']},indent=2)+'\n')
print(json.dumps({'verified_claims':len(results),'generation_id':p['generation_id']}))
