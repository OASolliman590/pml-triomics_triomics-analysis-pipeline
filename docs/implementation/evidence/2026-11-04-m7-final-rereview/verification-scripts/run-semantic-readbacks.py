"""Read public final native results independently; does not fit or alter producer artifacts."""
import json,subprocess,sys,hashlib
from pathlib import Path
source=Path(sys.argv[1]).resolve();root=Path(sys.argv[2]).resolve();native=root/'native/native-receipt.json'
rows=json.loads(native.read_text());by={Path(x['ticket']).stem:x for x in rows}
assert set(by)=={'D-T01','D-T02','D-T03','D-T04','D-T05','D-T10','D-T11','D-T12'}
for name,selected in [('targets',['D-T10','D-T11']),('discovery',['D-T12'])]:
 (root/(name+'-native-subset.json')).write_text(json.dumps([by[x] for x in selected],indent=2)+'\n')
commands=[
 ['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/sol-target-readback.py',str(source),str(root/'targets-native-subset.json'),str(root/'target-readback.json')],
 ['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/sol-discovery-readback.py',str(source),str(root/'discovery-native-subset.json'),str(root/'discovery-readback.json')],
 ['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/sol-ora-readback.py',str(source),by['D-T04']['root'],str(root/'ora-readback.json')],
 ['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/sol-native-readback.py',str(source),str(native),str(root/'generation-readback.json')]
]
for command in commands:subprocess.run(command,check=True)
# Resource-status publication and native source inventory are explicit on D05.
r=Path(by['D-T05']['root']);ptr=json.loads((r/'registries/current-D.json').read_text());m=r/ptr['generation_manifest']['path'];g=json.loads(m.read_text());routing=json.loads((m.parent/'evidence-routing.json').read_text())
status={'commit':by['D-T05']['commit'],'generation_id':ptr['generation_id'],'routing_sha256':hashlib.sha256((m.parent/'evidence-routing.json').read_bytes()).hexdigest(),'routes':{k:{'status':v.get('status'),'reason_code':v.get('reason_code'),'required':v.get('required')} for k,v in routing['routes'].items()}}
assert g['status']=='complete' and len(status['routes'])==5
assert status['routes']['gene_reaction']['status']=='complete'
assert status['routes']['lipid_ontology']['status']=='complete'
assert status['routes']['ora']['status']=='unavailable' and status['routes']['ora']['reason_code']=='D_RESOURCE_RIGHTS'
for route in ['mofa_native','biopan']:
 assert status['routes'][route]['status']=='not_requested' and status['routes'][route]['reason_code']=='D_ROUTE_DISABLED'
status['interpretation']='Required local mapping complete; optional ORA unavailable due unverified rights; conditional native routes disabled without completion credit.'
(root/'resource-readback.json').write_text(json.dumps(status,indent=2)+'\n')
(root/'semantic-readback-commands.json').write_text(json.dumps({'commands':commands,'exit_codes':[0]*len(commands)},indent=2)+'\n')
print('All final semantic readbacks completed.')
