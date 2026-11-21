from pathlib import Path
import zipfile,io,json
r=Path(__file__).resolve().parents[1];d=r/'build/bulk-053'
with zipfile.ZipFile(d/'all-runs.zip') as bundle:
 assert bundle.testzip() is None
 names=bundle.namelist();assert len(names)==32 and len(set(names))==32
 for i,name in enumerate(names,1):
  assert name==f'MISSY_native_run_{i}.rdmlt'
  with zipfile.ZipFile(io.BytesIO(bundle.read(name))) as run:
   assert run.testzip() is None
   record=run.read('native-run.txt').decode().splitlines();head=list(map(int,record[1].split()))
   prov=json.loads(run.read('source/fault-provenance.json'))
   assert prov['status']==head[2]
   if i==32:assert head[2]==2 and head[1]<45
   else:assert head[1]==45 and head[2]==1
 assert bundle.read(names[-1])==(d/'selected.rdmlt').read_bytes()
report={'passed':True,'runs':32,'checks':['outer ZIP CRC','all 32 nested RDMLt ZIP CRCs','31 complete runs and 1 partial run preserved','selected RDMLt byte-for-byte identical to bundle member']}
(d/'integrity.json').write_text(json.dumps(report,indent=2));print(json.dumps(report))
