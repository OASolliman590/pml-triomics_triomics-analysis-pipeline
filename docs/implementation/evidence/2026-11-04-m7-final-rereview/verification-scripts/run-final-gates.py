"""Sol sequential gate driver. Verification only; never accepts packets or milestones."""
import subprocess,json,hashlib,tarfile,sys,os,datetime,traceback
from pathlib import Path
source=Path(sys.argv[1]).resolve();out=Path(sys.argv[2]).resolve();expected=sys.argv[3]
assert not out.exists(),'Use a fresh evidence directory; never overwrite an earlier gate.'
scripts=Path(__file__).resolve().parent;proof=scripts.parent
out.mkdir(parents=True);env=os.environ.copy();env.update(json.loads((proof/'runtime/env.json').read_text()));env['TRIOMICS_EXPECTED_COMMIT']=expected
state={'product_commit':expected,'acceptance':False,'M7':'unreleased','G7':'unreleased','commands':[]}
def now():return datetime.datetime.now(datetime.timezone.utc).isoformat()
def save(phase):
 state.update(phase=phase,updated=now());(out/'gate-state.json').write_text(json.dumps(state,indent=2)+'\n');print(phase,flush=True)
def git(*args):return subprocess.check_output(['git',*args],cwd=source,text=True).strip()
def clean():assert git('rev-parse','HEAD')==expected and not git('status','--porcelain')
def run(phase,command,cwd,log):
 save(phase)
 started=now()
 with log.open('w') as f:
  process=subprocess.Popen(command,cwd=cwd,env=env,stdout=f,stderr=subprocess.STDOUT)
  state['active_process']={'phase':phase,'pid':process.pid,'command':command,'working_directory':str(cwd),'log':str(log),'started':started};save(phase+'_running')
  code=process.wait()
 state['commands'].append({'phase':phase,'pid':process.pid,'command':command,'working_directory':str(cwd),'exit_code':code,'log':str(log),'log_sha256':hashlib.sha256(log.read_bytes()).hexdigest(),'started':started,'finished':now()})
 state['active_process']=None
 save(phase+'_finished');assert code==0,(phase,code,str(log))
try:
 clean();(out/'product-commit.txt').write_text(expected+'\n');(out/'source').mkdir()
 runtime=json.loads(subprocess.check_output(['Rscript','--vanilla','-e','pkgs <- c("MOFA2","DEqMS","testthat","mixOmics","limma","jsonlite","reticulate");cat(jsonlite::toJSON(list(R=R.version.string,platform=R.version$platform,packages=lapply(pkgs,function(p)list(package=p,version=as.character(packageVersion(p))))),auto_unbox=TRUE))'],cwd=source,env=env,text=True))
 runtime['python']=json.loads(subprocess.check_output([env['TRIOMICS_MOFA_PYTHON'],'-c','import sys,json,importlib.metadata; print(json.dumps(dict(executable=sys.executable,version=sys.version,mofapy2=importlib.metadata.version("mofapy2"))))'],text=True))
 (out/'runtime.json').write_text(json.dumps(runtime,indent=2)+'\n')
 run('full_source',['Rscript','--vanilla','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/run-source.R',str(source),str(out/'source')],source,out/'source/full-source.log')
 s=json.loads((out/'source/source-receipt.json').read_text());assert s['commit']==expected and s['exit_code']==0
 assert all(s['counts'][k]==0 for k in ['fail','error','skip'])
 known={'C-05 real MOFA2 fit preserves scattered missingness and reports seed sensitivity and enrichment hashes','C-05 real MOFA2 fits and extracts a missing sample-view capability cell','D-T01 refuses stale paired signs and preserves the old pointer'}
 warnings=[c for c in s['conditions'] if c['type']=='expectation_warning'];assert len(warnings)==s['counts']['warn'];assert all(c['test'] in known for c in warnings),'New source warning needs Sol disposition'
 clean();tickets=['inst/examples/tickets/'+n+'.yaml' for n in ['D-T01','D-T02','D-T03','D-T04','D-T05','D-T10','D-T11','D-T12']]
 run('native_packets',['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/native-packet-proof.py',str(source),str(out/'native'),*tickets],source,out/'native-packets.log')
 run('semantic_readbacks',['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/run-semantic-readbacks.py',str(source),str(out)],source,out/'semantic-readbacks.log')
 run('native_applicability',['python3',str(scripts/'verify_m7_029_resume.py'),str(source),str(out/'applicability-native')],source,out/'applicability-native.log')
 run('Sol_applicability_readback',['python3',str(scripts/'sol-applicability-readback.py'),str(out/'applicability-native'),str(out/'sol-applicability-readback.json')],source,out/'sol-applicability-readback.log')
 run('packet_receipts',['python3','/Users/megastore/.triomics-verification/m7-resume-20261104-01/scripts/build-packet-receipts.py',str(out)],source,out/'packet-receipts.log')
 clean();save('archive_preparation');tar=out/'source.tar';archive_command=['git','archive','--format=tar','--output='+str(tar),expected];subprocess.run(archive_command,cwd=source,check=True)
 archive=out/'archive';archive.mkdir()
 with tarfile.open(tar) as t:
  tar_files={m.name for m in t.getmembers() if m.isfile()};assert not any(Path(n).name.startswith('._') for n in tar_files);t.extractall(archive,filter='data')
 generated_sidecars=[]
 for f in archive.rglob('._*'):
  if f.is_file() and f.read_bytes()[:4]==b'\x00\x05\x16\x07':generated_sidecars.append(str(f.relative_to(archive)));f.unlink()
 assert {str(f.relative_to(archive)) for f in archive.rglob('*') if f.is_file()}==tar_files
 (out/'archive-generated-sidecars.json').write_text(json.dumps({'removed_runtime_metadata_only':generated_sidecars},indent=2)+'\n')
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
