"""Read-only Sol validation of exact native publication bytes and qualifications."""
import json,hashlib,subprocess,sys
from pathlib import Path
source=Path(sys.argv[1]);receipt=Path(sys.argv[2]);out=Path(sys.argv[3]);records=json.loads(receipt.read_text())
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=source,text=True).strip()
assert not subprocess.check_output(['git','status','--porcelain'],cwd=source).strip()
result=[]
for record in records:
 assert record['commit']==commit
 assert [x['exit_code'] for x in record['commands']]==[0,0,0]
 root=Path(record['root']);checks=[]
 for relative,expected in record['files'].items():
  path=root/relative;assert path.resolve().is_relative_to(root.resolve()) and not path.is_symlink()
  assert hashlib.sha256(path.read_bytes()).hexdigest()==expected,relative
  checks.append(relative)
 pointer=json.loads((root/'registries/current-D.json').read_text());manifest=root/pointer['generation_manifest']['path'];value=json.loads(manifest.read_text())
 assert hashlib.sha256(manifest.read_bytes()).hexdigest()==pointer['generation_id']==record['generation_id']
 assert 'This publication is not a C-06, M7, or G7 acceptance receipt.' in value['limitations']
 assert 'C-06 remains unaccepted.' not in value['limitations']
 assert 'C-07 controls remain not_run.' in value['limitations']
 assert 'C-09 remains unqualified and conditional.' in value['limitations']
 result.append({'commit':commit,'commands':record['commands'],'generation_id':pointer['generation_id'],'limitations':value['limitations'],'file_hash_checks':len(checks),'sha256':record['files']})
out.write_text(json.dumps(result,indent=2)+'\n');print('Verified',len(result),'native generation(s); exact wording and all recorded file hashes match.')
