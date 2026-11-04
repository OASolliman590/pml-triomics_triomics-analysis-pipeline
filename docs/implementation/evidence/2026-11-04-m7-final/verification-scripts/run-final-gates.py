"""Sol sequential gate driver. Verification only; never accepts packets or milestones."""
import subprocess,json,hashlib,tarfile,sys,os,datetime,traceback
from pathlib import Path
source=Path(sys.argv[1]).resolve();out=Path(sys.argv[2]).resolve();expected=sys.argv[3]
assert not out.exists(),'Use a fresh evidence directory; never overwrite an earlier gate.'
out.mkdir(parents=True);env=os.environ.copy();env.update(R_LIBS_USER='/tmp/triomics-runtime-20261103/R:/Users/megastore/Library/R/x86_64/4.6/library',TRIOMICS_MOFA_LIBRARY='/tmp/triomics-runtime-20261103/R',TRIOMICS_MOFA_PYTHON='/tmp/triomics-runtime-20261103/python/bin/python',RETICULATE_PYTHON='/tmp/triomics-runtime-20261103/python/bin/python')
state={'product_commit':expected,'acceptance':False,'M7':'closed','G7':'closed','commands':[]}
def now():return datetime.datetime.now(datetime.timezone.utc).isoformat()
def save(phase):
 state.update(phase=phase,updated=now());(out/'gate-state.json').write_text(json.dumps(state,indent=2)+'\n');print(phase,flush=True)
def git(*args):return subprocess.check_output(['git',*args],cwd=source,text=True).strip()
def clean():assert git('rev-parse','HEAD')==expected and not git('status','--porcelain')
def run(phase,command,cwd,log):
 save(phase)
 with log.open('w') as f:r=subprocess.run(command,cwd=cwd,env=env,stdout=f,stderr=subprocess.STDOUT)
 state['commands'].append({'phase':phase,'command':command,'working_directory':str(cwd),'exit_code':r.returncode,'log':str(log)})
 save(phase+'_finished');assert r.returncode==0,(phase,r.returncode,str(log))
try:
 clean();(out/'product-commit.txt').write_text(expected+'\n');(out/'source').mkdir()
 run('full_source',['Rscript','--vanilla','/tmp/triomics-stage-d-evidence/final/run-source.R',str(source),str(out/'source')],source,out/'source/full-source.log')
 s=json.loads((out/'source/source-receipt.json').read_text());assert s['commit']==expected and s['exit_code']==0
 assert all(s['counts'][k]==0 for k in ['fail','error','skip'])
 known={'C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes','C-05 real MOFA2 fits and extracts a missing sample-view capability cell','D-T01 refuses stale paired signs and preserves the old pointer'}
 warnings=[c for c in s['conditions'] if c['type']=='expectation_warning'];assert len(warnings)==s['counts']['warn'];assert all(c['test'] in known for c in warnings),'New source warning needs Sol disposition'
 clean();tickets=['inst/examples/tickets/'+n+'.yaml' for n in ['D-T01','D-T02','D-T03','D-T04','D-T05','D-T10','D-T11','D-T12']]
 run('native_packets',['python3','/tmp/triomics-stage-d-evidence/native-packet-proof.py',str(source),str(out/'native'),*tickets],source,out/'native-packets.log')
 run('semantic_readbacks',['python3','/tmp/triomics-stage-d-evidence/final/run-semantic-readbacks.py',str(source),str(out)],source,out/'semantic-readbacks.log')
 run('packet_receipts',['python3','/tmp/triomics-stage-d-evidence/final/build-packet-receipts.py',str(out)],source,out/'packet-receipts.log')
 clean();save('archive_preparation');tar=out/'source.tar';archive_command=['git','archive','--format=tar','--output='+str(tar),expected];subprocess.run(archive_command,cwd=source,check=True)
 archive=out/'archive';archive.mkdir()
 with tarfile.open(tar) as t:t.extractall(archive,filter='data')
 assert (archive/'inst/triomics-source-revision').read_text().strip()==expected and not (archive/'.git').exists()
 files={str(f.relative_to(archive)):hashlib.sha256(f.read_bytes()).hexdigest() for f in archive.rglob('*') if f.is_file()};assert not any(Path(n).name.startswith('._') for n in files)
 ar={'archive_commit':expected,'command':archive_command,'tar_sha256':hashlib.sha256(tar.read_bytes()).hexdigest(),'source_stamp':expected,'file_count':len(files),'files':files};(out/'archive-receipt.json').write_text(json.dumps(ar,indent=2)+'\n')
 run('package_check',['Rscript','--vanilla','tools/check-package.R','--root',str(archive),'--output',str(out/'package')],archive,out/'package-driver.log')
 statuses=[x for x in (out/'package/00check.log').read_text().splitlines() if x.startswith('Status:')];assert statuses==['Status: OK'],statuses
 inventory=json.loads((out/'package/warning-inventory.json').read_text());assert all(w.get('test') and w['test']!='<unknown test>' for w in inventory['warnings'])
 after={str(f.relative_to(archive)):hashlib.sha256(f.read_bytes()).hexdigest() for f in archive.rglob('*') if f.is_file()};assert after==files,'Archive source changed during checker'
 clean();state['source_counts']=s['counts'];state['package_status']=statuses[0];state['package_warning_records']=len(inventory['warnings']);save('gates_complete_pending_Sol_warning_review_Terra_Astra')
except BaseException as e:
 state['failure']=str(e);save('failed');traceback.print_exc();sys.exit(1)
