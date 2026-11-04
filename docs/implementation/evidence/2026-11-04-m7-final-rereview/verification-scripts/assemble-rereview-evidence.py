"""Sol evidence assembler; requires finished gates and never grants acceptance."""
import json,re,hashlib,collections,shutil,sys
from pathlib import Path
root=Path(sys.argv[1]).resolve(); repo=Path(sys.argv[2]).resolve()
dest=repo/'docs/implementation/evidence/2026-11-04-m7-final-rereview'
assert not dest.exists(), 'Do not overwrite prior evidence'
state=json.loads((root/'gate-state.json').read_text())
assert state['phase']=='gates_complete_pending_Sol_warning_review_Terra_Astra'
s=json.loads((root/'source/source-receipt.json').read_text())
i=json.loads((root/'package/warning-inventory.json').read_text())
ar=json.loads((root/'archive-receipt.json').read_text())
assert [l for l in (root/'package/00check.log').read_text().splitlines() if l.startswith('Status:')]==['Status: OK']
text=(root/'package/testthat.Rout').read_text()
counts=re.findall(r'FAIL\s+(\d+)\s*\|\s*WARN\s+(\d+)\s*\|\s*SKIP\s+(\d+)\s*\|\s*PASS\s+(\d+)',text)
assert counts, 'Cannot identify installed testthat summary'
f,w,k,p=map(int,counts[-1]); assert f==k==0
source_w=[c for c in s['conditions'] if c['type']=='expectation_warning']
source_names=dict(collections.Counter(c['test'] for c in source_w)); installed_names=dict(collections.Counter(c['test'] for c in i['warnings']))
assert source_names==installed_names and sum(installed_names.values())==w
summary={'product_commit':state['product_commit'],'source_counts':s['counts'],'package_counts':dict(pass_=p,fail=f,warn=w,skip=k),'package_status':'Status: OK','archive_tar_sha256':ar['tar_sha256'],'archive_file_count':ar['file_count'],'source_started':s['started'],'source_finished':s['finished'],'source_warning_names':source_names,'package_warning_names':installed_names,'acceptance':'pending Terra DOC-M7, focused final Astra and Sol decision','external_evidence_root':str(root),'source_installed_pass_delta':s['counts']['pass']-p,'log_sha256':{}}
summary['package_counts']['pass']=summary['package_counts'].pop('pass_')
for rel in ['source/full-source.log','source/testthat-results.rds','package/testthat.Rout','package/00check.log','package/check.log','package/build.log']:
 q=root/rel;assert q.exists(),str(q);summary['log_sha256'][rel]=hashlib.sha256(q.read_bytes()).hexdigest()
for key,items in [('source_warning_messages',source_w),('package_warning_messages',i['warnings'])]:
 c=collections.Counter((x['test'],x['message']) for x in items);summary[key]=[dict(test=t,message=m,count=n) for (t,m),n in c.items()]
native=json.loads((root/'native/native-receipt.json').read_text());summary['native_commands']=sum(len(x['commands']) for x in native)
a=json.loads((root/'applicability-native/runner-receipt.json').read_text());assert a['completed'];summary['applicability_cases']=a['case_count'];summary['applicability_native_commands']=a['stage_count'];summary['applicability_readback_assertions']=a['readback_assertions_passed']
dest.mkdir(parents=True)
rels=['gate-state.json','runtime.json','archive-receipt.json','archive-generated-sidecars.json','packet-receipts.json','source/source-receipt.json','package/warning-inventory.json','package/00check.log','native/native-receipt.json','semantic-readback-commands.json','target-readback.json','discovery-readback.json','ora-readback.json','generation-readback.json','resource-readback.json','sol-applicability-readback.json','applicability-native/runner-receipt.json']
for rel in rels:
 q=dest/rel;q.parent.mkdir(parents=True,exist_ok=True);shutil.copyfile(root/rel,q)
(dest/'gate-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
for q in Path(__file__).parent.iterdir():
 if q.is_file() and q.suffix in ('.py','.R') and 'executed-apfs-failed' not in q.name:
  v=dest/'verification-scripts'/q.name;v.parent.mkdir(exist_ok=True);shutil.copyfile(q,v)
print(json.dumps(summary,indent=2))
