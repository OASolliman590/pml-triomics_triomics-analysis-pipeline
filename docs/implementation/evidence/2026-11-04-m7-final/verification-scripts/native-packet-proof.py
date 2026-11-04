"""Sol independent public-synthetic native packet runner and immutable-byte readback."""
import hashlib,json,os,subprocess,sys
from pathlib import Path
source=Path(sys.argv[1]).resolve();evidence=Path(sys.argv[2]).resolve();tickets=sys.argv[3:]
evidence.mkdir(parents=True,exist_ok=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=source).strip(), 'source is dirty'
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=source,text=True).strip()
def read(p):return json.loads(p.read_text())
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
results=[]
for ticket in tickets:
    name=Path(ticket).stem;root=evidence/name
    assert not root.exists(), 'refusing prior output root'
    root.mkdir();commands=[]
    result={'ticket':ticket,'commit':commit,'root':str(root),'commands':commands}
    results.append(result)
    for stage in ('B','C','D'):
        command=['Rscript','--vanilla','inst/exec/triomics','--config',ticket,'--stage',stage,'--output',str(root)]
        with (evidence/(name+'-'+stage+'.log')).open('w') as f:
            run=subprocess.run(command,cwd=source,stdout=f,stderr=subprocess.STDOUT)
        commands.append({'command':command,'exit_code':run.returncode})
        (evidence/'native-receipt.json').write_text(json.dumps(results,indent=2)+'\n')
        assert run.returncode==0,(ticket,stage,run.returncode)
    bp=list(root.glob('B/handoffs/*/stage-handoff-provenance.json'));assert len(bp)==1
    bd=bp[0].parent
    bf=[bd/n for n in ['bundle.json','bundle.envelope.json','stage-handoff-provenance.json','stage-handoff-provenance.envelope.json']]
    cp=root/'D/stage-handoff-provenance.json';ce=root/'D/stage-handoff-provenance.envelope.json'
    digests={e['digest'] for e in read(cp)['identity']['entities']}
    assert all(sha(f) in digests for f in bf)
    ptr=root/'registries/current-D.json';p=read(ptr);mp=root/p['generation_manifest']['path'];m=read(mp)
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
        f=mp.parent/member['path'];assert f.resolve().is_relative_to(mp.parent.resolve())
        assert not f.is_symlink();assert sha(f)==member['digest'];assert f.stat().st_size==member['size_bytes'];members.append(f)
    payload=read(mp.parent/'evidence-routing.json')
    if name in ('D-T02','D-T03'):
        route='lipid_ontology' if name=='D-T02' else 'gene_reaction'
        mappings=payload['routes'][route]['mappings'];assert len(mappings)==1
        mapping=next(iter(mappings.values()));assert mapping['status']=='available'
        assert mapping['observations'] and mapping['edges']
        obs=mapping['observations'];assert len({o['feature_key'] for o in obs})==len(obs)
        assert len(obs)==len({n['feature_key'] for n in payload['routes'][route]['nominations']})
        assert mapping['resource']['network_attempted'] is False
        assert mapping['resource']['study_upload'] is False
        assert sha(Path(mapping['resource']['local_cache']))==mapping['resource']['content_sha256']
        if name=='D-T02':
            pc=[o for o in obs if o['source_id']=='PC(34:1)'];assert len(pc)==1
            assert pc[0]['resolution']=='sum_composition'
            assert all(pc[0][f] is None for f in ('chains','sn_positions','isomer'))
            edges=[e for e in mapping['edges'] if e['feature_key']==pc[0]['feature_key']]
            assert len(edges)==2 and edges[0]['ambiguity_group']==edges[1]['ambiguity_group']
        else:
            gene=[o for o in obs if o['source_id']=='SYN-GENE-001'];assert len(gene)==1
            assert {n['source_branch'] for n in gene[0]['nominations']}=={'primary_statistics','integration_selection'}
            assert gene[0]['independent_corroboration'] is False
        result.update(route=route,observation_count=len(obs),edge_count=len(mapping['edges']))
    files=bf+[cp,ce,mp,ptr]+members
    result.update(generation_id=p['generation_id'],four_b_predecessors_verified=True,files={str(f.relative_to(root)):sha(f) for f in files})
    (evidence/'native-receipt.json').write_text(json.dumps(results,indent=2)+'\n')
    print(json.dumps({k:v for k,v in result.items() if k!='files'}),flush=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=source).strip(), 'source changed during proof'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=source,text=True).strip()==commit
