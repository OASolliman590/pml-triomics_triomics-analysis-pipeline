"""Assemble same-snapshot records from completed proofs; never grants acceptance."""
import json,hashlib,sys
from pathlib import Path
root=Path(sys.argv[1]);source=json.loads((root/'source/source-receipt.json').read_text())
natives=json.loads((root/'native/native-receipt.json').read_text())
assert source['exit_code']==0 and all(source['counts'][x]==0 for x in ['fail','error','skip'])
commit=source['commit'];required=['D-T01','D-T02','D-T03','D-T04','D-T05','D-T10','D-T11','D-T12']
by_native={Path(n['ticket']).stem:n for n in natives};receipts={}
for packet in ['B-T07','C-06']+required:
    matching={name:value for name,value in source['by_file'].items() if packet in name}
    assert matching
    counts={key:sum(value[key] for value in matching.values()) for key in ['pass','fail','error','warn','skip','blocks']}
    assert counts['pass']>0 and all(counts[x]==0 for x in ['fail','error','skip'])
    conditions=[x for x in source['conditions'] if x['file'] in matching]
    record={'packet':packet,'tested_commit':commit,'status':'verified_pending_Sol_acceptance',
      'test_command':source['command'],'test_command_exit_code':source['exit_code'],
      'count_attribution':'files matched by packet ID within the uninterrupted complete source suite',
      'matching_test_files':matching,
      'test_counts':counts,'conditions':conditions,'source_receipt_sha256':hashlib.sha256((root/'source/source-receipt.json').read_bytes()).hexdigest()}
    native=by_native[packet if packet in required else 'D-T01']
    assert native['commit']==commit and all(x['exit_code']==0 for x in native['commands'])
    record.update(native_commands=native['commands'],native_root=native['root'],native_generation=native['generation_id'],artifact_sha256=native['files'])
    if packet not in required:record['native_attribution']='same-root native predecessor exercised by D-T01 synthetic B/C/D publication'
    receipts[packet]=record
out=root/'packet-receipts.json';out.write_text(json.dumps({'commit':commit,'required_D_packets':required,'receipts':receipts,'acceptance':'pending'},indent=2)+'\n')
print(out)
