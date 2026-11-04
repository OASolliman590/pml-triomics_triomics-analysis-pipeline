"""Independent Sol readback of canonical native applicability outputs; no fits."""
import json,hashlib,sys
from pathlib import Path
root=Path(sys.argv[1]).resolve();out=Path(sys.argv[2]).resolve();receipt=json.loads((root/'runner-receipt.json').read_text())
assert receipt['completed'] and receipt['case_count']==10 and receipt['stage_count']==30
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
checks=[];summaries=[];signatures={}
def ck(value,description):
 assert value,description
 checks.append(description)
for case in receipt['cases']:
 name=case['name'];r=Path(case['native_output']);ptr=json.loads((r/'registries/current-D.json').read_text());m=r/ptr['generation_manifest']['path'];ck(sha(m)==ptr['generation_manifest']['digest'],name+': manifest raw digest')
 manifest=json.loads(m.read_text());member=next(x for x in manifest['members'] if x['path']=='evidence-routing.json');f=m.parent/member['path'];ck(sha(f)==member['digest'] and f.stat().st_size==member['size_bytes'],name+': canonical routing bytes')
 t=json.loads(f.read_text())['target_investigation'];profiles=t['profiles'];ck(len(profiles)>0,name+': profiles present')
 control=case.get('feature_taxon_control');taxon=control['taxon_id'] if control else None
 expected='matched' if taxon=='9606' else 'inapplicable' if taxon=='10090' else 'unassessable'
 sig=[];details=[]
 raw=json.loads((Path(case['case_root'])/'data/d-target/external-assays.json').read_text())['records'];raw_by_id={x['evidence_id']:x for x in raw}
 for profile in profiles:
  target=profile['request']['id'];obs=profile['observations'];ck(bool(obs),name+': observations present '+target)
  for o in obs:
   ck(o['taxon_applicability']==expected,name+': applicability '+target)
   if taxon is None:
    ck(all(k in o and o[k] is None for k in ['feature_taxon_id','feature_taxon_declaration','feature_taxon_provenance_ref']),name+': named nulls '+target)
   else:
    ck(o['feature_taxon_id']==taxon and o['feature_taxon_declaration']==dict(status='declared',value=taxon,provenance_ref=control['provenance_ref']),name+': exact declaration origin '+target)
   if expected!='matched':ck(o['status'] not in ['support','conflict'],name+': no inapplicable claim '+target)
   sig.append([target,o['source_branch'],o.get('result_id'),o.get('nomination_id'),o.get('component_id'),o['value'],o.get('effect'),o['effect_unit'],o['feature_key'],o['source_id'],o['source_sha256'],o['dependency_group']])
  molecular=profile['evidence_axes']['metabolic_multiomics_compatibility'];ck(molecular['status']==('measured' if expected=='matched' else 'unassessable'),name+': molecular axis qualification '+target)
  records=profile['external_evidence'];binding=[e for e in records if e['axis']=='binding'];nmeasured=nunmeasured=nunresolved=0
  for e in records:
   original=raw_by_id[e['evidence_id']];ck(all(k in e and e[k]==v for k,v in original.items()),name+': original external evidence preserved '+e['evidence_id'])
   qualified=bool(original.get('assay')) and bool(original.get('assay_limitations'));ck(e['assay_qualification']==('qualified' if qualified else 'unresolved'),name+': assay completeness '+e['evidence_id'])
   if e['axis']=='binding':
    applicable=e['attachment']=='request' and qualified
    nmeasured+=int(applicable and original['value'] is not None);nunmeasured+=int(applicable and original['value'] is None);nunresolved+=int(not applicable)
  axis=profile['evidence_axes']['direct_binding'];c=axis['coverage'];ck([c['measured_records'],c['unmeasured_records'],c['unresolved_records']]==[nmeasured,nunmeasured,nunresolved],name+': exact binding coverage '+target)
  if 'missing-assay' in name or 'both-assay-fields' in name:ck(axis['status']=='unresolved' and nmeasured==0,name+': incomplete assay not qualified '+target)
  details.append(dict(target=target,molecular_status=molecular['status'],binding_status=axis['status'],binding_coverage=c))
 signatures[name]=sorted(sig,key=lambda v:json.dumps(v))
 summaries.append(dict(case=name,routing_sha256=sha(f),manifest_sha256=sha(m),taxon=taxon,expected_applicability=expected,profiles=details))
base=signatures['native-D11']
for name,sig in signatures.items():
 if name.startswith('control-'):ck(sig==base,name+': all effects, components, source and dependency identities unchanged from native D11')
out.write_text(json.dumps(dict(commit=receipt['expected_commit'],assertions_passed=len(checks),checks=checks,cases=summaries,acceptance=False),indent=2)+'\n');print('Sol canonical applicability readback passed',len(checks))
