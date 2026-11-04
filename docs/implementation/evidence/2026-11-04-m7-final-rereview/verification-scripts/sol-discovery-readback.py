"""Independent Sol read-back of public synthetic target producer artifacts, no fitting."""
import csv,hashlib,json,subprocess,sys
from pathlib import Path
source=Path(sys.argv[1]).resolve(); receipts=Path(sys.argv[2]); out=Path(sys.argv[3])
def read(p):return json.loads(p.read_text())
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=source,text=True).strip()
assert not subprocess.check_output(['git','status','--porcelain'],cwd=source).strip()
axes=['metabolic_multiomics_compatibility','functional_perturbation','cellular_tissue_engagement','direct_binding']
reports=[]
for native in read(receipts):
    assert native['commit']==commit
    assert [x['exit_code'] for x in native['commands']]==[0,0,0]
    root=Path(native['root']).resolve();ticket=source/native['ticket']; checks=[]
    def check(condition,name):
        assert condition,name
        checks.append(name)
    cfg=json.loads(subprocess.check_output(['Rscript','--vanilla','-e',
        'cat(jsonlite::toJSON(yaml::read_yaml(commandArgs(TRUE)[[1]]),auto_unbox=TRUE,null="null"))',str(ticket)],text=True))
    def fixture(relative):
        p=(ticket.parent/relative).resolve()
        assert p.is_relative_to(source/'inst/examples/data'),'only public synthetic fixture paths allowed'
        return p
    ptr=root/'registries/current-D.json';pointer=read(ptr);mp=root/pointer['generation_manifest']['path'];manifest=read(mp)
    check(sha(mp)==pointer['generation_id']==pointer['generation_manifest']['digest'],'D generation full SHA256')
    check(mp.stat().st_size==pointer['generation_manifest']['size_bytes'],'D manifest byte size')
    for member in manifest['members']:
        p=mp.parent/member['path'];check(p.resolve().is_relative_to(mp.parent.resolve()) and not p.is_symlink(),'D member stays in generation')
        check(sha(p)==member['digest'] and p.stat().st_size==member['size_bytes'],'D member bytes '+member['path'])
    bps=list(root.glob('B/handoffs/*/stage-handoff-provenance.json'));check(len(bps)==1,'one native B frame')
    bp=bps[0];bf=[bp.parent/x for x in ['bundle.json','bundle.envelope.json','stage-handoff-provenance.json','stage-handoff-provenance.envelope.json']]
    cp=root/'D/stage-handoff-provenance.json';ce=root/'D/stage-handoff-provenance.envelope.json'
    cdigests={x['digest'] for x in read(cp)['identity']['entities']}
    check(all(sha(p) in cdigests for p in bf),'C binds all four same-root B predecessor bytes')
    for role,paths in [('b',bf),('c',[None,None,cp,ce])]:
        for kind,parts in [('bundle',paths[:2]),('profile',paths[2:])]:
            if parts[0] is None:continue
            check(manifest['provenance'][role][kind]['payload_sha256']==sha(parts[0]),'D '+role+' '+kind+' payload')
            check(manifest['provenance'][role][kind]['envelope_sha256']==sha(parts[1]),'D '+role+' '+kind+' envelope')
    routing=read(mp.parent/'evidence-routing.json');t=routing['target_investigation'];profiles=t['profiles']
    check(t['mode']=='discover' and t['discovery']['enumeration']=='unranked','explicit unranked discovery mode')
    inv=cfg['target_investigation'];universe_path=fixture(inv['universe_ref']);protocol_path=fixture(inv['protocol_ref']);assay_path=fixture(inv['external_evidence_file'])
    universe=read(universe_path);protocol=read(protocol_path);assays={x['evidence_id']:x for x in read(assay_path)['records']}
    included=[m for m in universe['members'] if m['disposition']=='included']
    check(len(profiles)==len(included),'one profile per included member')
    check(len(t['discovery']['members'])==len(universe['members']),'every declared member accounted')
    check(protocol['universe_sha256']==sha(universe_path)==t['discovery']['universe']['sha256'],'protocol binds full universe bytes')
    check(t['discovery']['protocol']['sha256']==sha(protocol_path),'protocol bytes bound')
    check(t['discovery']['protocol']['enumeration_rule']==protocol['enumeration_rule'],'exact unranked enumeration rule')
    for raw,member in zip(universe['members'],t['discovery']['members']):
        check(raw['member_id']==member['member_id'] and raw['disposition']==member['inclusion_status'] and raw['reason']==member['reason'],'member identity disposition reason preserved')
        check(raw['selection_history']==member['prior_selection_history'],'prior selection history retained')
    check(t['alternatives']['profiles']==profiles,'both published profile copies agree')
    check(t.get('selected_target') is None and t['alternatives'].get('selected_target') is None,'discovery has no selected winner')
    for key,path in [('universe_ref',universe_path),('protocol_ref',protocol_path),('external_evidence_file',assay_path)]:
        check(t['input_provenance'][key]['sha256']==sha(path),'consumed '+key+' actual bytes')
    blocks={x['block_id']:x for x in cfg['blocks']};sources={x['source_id']:x for x in cfg['sources']}
    b=read(root/'B/block-results.json');brows={}
    for block_id,block in b['blocks'].items():
        for claim_id,claim in block['contrasts'].items():
            for row in claim['rows']:brows[row['result_id']]=(block_id,claim_id,row)
    c=read(root/'D/integration-evidence.json');integrations={x['integration_id']:x for x in c['integrations']}
    nominations={x['nomination_id']:x for x in read(root/'D/candidate-nominations.json')['nominations']}
    profile_summary=[]
    for profile in profiles:
        request=profile['request'];tid=request['id'];obs=profile['observations']
        declaration=next(x for x in included if x['identity']['id']==tid)
        check(profile['links']==declaration['links'],tid+' declared paths retained')
        check(profile['axes']==axes and list(profile['evidence_axes'])==axes,tid+' four distinct canonical axes')
        check(profile.get('selected_target') is None,tid+' no selected winner')
        check(all(s['hypothesis_status']=='exploratory' and s['independent_confirmation'] is False for s in request['selection']),tid+' selection remains exploratory')
        for key in ['universe_ref','protocol_ref','external_evidence_file']:
            check(profile['input_provenance'][key]==t['input_provenance'][key],tid+' profile '+key+' binding')
        by_feature={}
        for row in obs:
            check(row['source_branch'] in ('primary_statistics','integration_selection'),tid+' native observation source')
            source_record=sources[row['source_id']];source_path=fixture(source_record['file'])
            check(row['source_sha256']==sha(source_path),tid+' source measurement hash')
            check(row['source_selection_history']==source_record['selection_history'] and row['source_selection_evidence']==source_record['selection_evidence'],tid+' source selection history')
            block_id=row.get('block_id',row.get('selection_value',{}).get('block_id'));block=blocks[block_id]
            feature_rows=list(csv.DictReader(source_path.open(),delimiter='\t'))
            check(sum(x[block['feature_id_column']]==row['feature_id'] for x in feature_rows)==1,tid+' original measured feature identity')
            check(row['feature_namespace']==block['id_namespace'],tid+' measured namespace')
            by_feature.setdefault(row['feature_key'],set()).add(row['dependency_group'])
            if row['source_branch']=='primary_statistics':
                bid,cid,expected=brows[row['result_id']]
                check(bid==block_id and cid==row['claim_id'] and expected['feature_key']==row['feature_key'],tid+' actual B result identity')
                check(row['value']==row['effect']==expected['effect'] and row['effect_unit']==expected['effect_unit'],tid+' actual B effect and unit')
            else:
                nomination=nominations[row['nomination_id']];integration=integrations[row['integration_id']]
                value=integration['selection']['selected_values'][block_id][row['feature_key']]['component_loadings'][row['component_id']]
                check(row['value']==value and row['effect'] is None and row['effect_unit']=='loading',tid+' actual C loading kept distinct from effect')
                check(row['selection_rule']==nomination['selection_rule'] and row['selection_value']==nomination['selection_value'],tid+' actual C selection lineage')
        check(bool(obs) and {x['source_branch'] for x in obs}=={'primary_statistics','integration_selection'},tid+' both real producer branches present')
        check(all(len(groups)==1 for groups in by_feature.values()) and profile['independent_count']==len(by_feature),tid+' B/C reuse does not add independent confirmation')
        for record in profile['external_evidence']:
            raw=assays[record['evidence_id']]
            check(all(record.get(k)==v for k,v in raw.items()),tid+' external assay original values retained')
            matched=raw['target_id']==tid
            check(record['attachment']==('request' if matched else 'target_mismatch'),tid+' exact target assay attachment')
            check(record['value'] is None and record['availability']=='unavailable' and record['measured_by_pipeline'] is False and record['independent_replication'] is False,tid+' absent fixture affinity remains unavailable/external')
        binding=profile['evidence_axes']['direct_binding'];records=profile['external_evidence']
        applicable=[v for v in records if v['attachment']=='request' and v['assay_qualification']=='qualified']
        unresolved=len(records)-len(applicable)
        check(binding['coverage']['measured_records']==0 and binding['coverage']['unmeasured_records']==len(applicable) and binding['coverage']['unresolved_records']==unresolved and binding['coverage']['expected_records']==len(records),tid+' exact unavailable/unresolved assay coverage')
        check(binding['status']==('unresolved' if unresolved else 'unmeasured'),tid+' no binding measurement invented; mixed unresolved identity retained')
        if unresolved:check(binding['reason_code']=='D_TARGET_IDENTITY_UNRESOLVED',tid+' exact mismatch qualification reason')
        profile_summary.append({'target':tid,'observation_count':len(obs),'molecular_groups':len(by_feature),'axis_status':{k:v['status'] for k,v in profile['evidence_axes'].items()}})
    if len(profiles)>1:
        check(t['alternatives']['comparability']=='noncomparable','unresolved external identity coverage is noncomparable')
        check(t['alternatives'].get('selected_target') is None and t['alternatives']['tie_status']=='unresolved','alternatives remain unranked/no winner')
    reports.append({'commit':commit,'ticket':native['ticket'],'root':str(root),'commands':native['commands'],'assertions':len(checks),'checks':checks,'profiles':profile_summary,'generation_id':pointer['generation_id'],'sha256':{str(p.relative_to(root)):sha(p) for p in bf+[cp,ce,mp,mp.parent/'evidence-routing.json']},'external_file_sha256':{'universe':sha(universe_path),'protocol':sha(protocol_path),'assays':sha(assay_path)}})
    out.write_text(json.dumps(reports,indent=2)+'\n')
    print(native['ticket'],len(checks),'read-back assertions passed',flush=True)
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=source,text=True).strip()==commit
assert not subprocess.check_output(['git','status','--porcelain'],cwd=source).strip()
