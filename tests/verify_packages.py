from pathlib import Path
import json,zipfile,xml.etree.ElementTree as E,hashlib
root=Path(__file__).resolve().parents[1]
ns={'r':'http://www.rdml.org'};results=[]
for folder in ['build/scenarios','build/chrome-052']:
 for p in sorted((root/folder).glob('case-*.rdmlt')):
  with zipfile.ZipFile(p) as z:
   assert z.testzip() is None,p
   lines=z.read('native-run.txt').decode().splitlines();assert lines[0]=='MISSY-NATIVE-2'
   h=list(map(int,lines[1].split()));sc=list(map(int,lines[2].split()));present=list(map(int,lines[3].split()))
   nw,ncy,status,seed,ne,nr,copies,thr,fault,nt,elapsed,gap=h
   assert len(lines)==4+2*ncy+nt
   prov=json.loads(z.read('source/fault-provenance.json'))
   assert prov['scenarioCode']==sc[0] and prov['seed']==seed and prov['extractions']==ne
   assert prov['replicates']==nr and prov['netThreshold']==thr
   if sc[0]:assert [round(v['value']*1000) for v in prov['parameters']]==sc[2:8]
   xml=E.fromstring(z.read('rdml_data.xml'));E.fromstring(z.read('rdmlt.xml'))
   reacts=xml.findall('.//r:react',ns);assert len(reacts)==sum(present)
   ids=[int(a.attrib['id']) for a in reacts];assert len(ids)==len(set(ids)) and min(ids)>=1 and max(ids)<=384
   for react in reacts:
    w=int(react.find('r:sample',ns).attrib['id'].split('-')[1])-1
    data=react.findall('r:data',ns);assert len(data)==2
    for channel in [0,1]:
     measured=[list(map(int,lines[4+2*i+channel].split()))[w] for i in range(ncy)]
     points=data[channel].findall('r:adp',ns)
     actual=[(int(a.find('r:cyc',ns).text),int(a.find('r:fluor',ns).text)) for a in points]
     expected=[(i+1,v) for i,v in enumerate(measured) if v>=0]
     assert actual==expected,(p,w,channel)
     assert all(0<=v<=65535 and 1<=i<=45 for i,v in actual)
   if sc[0]==19:assert len(reacts[0].findall('r:data',ns)[0].findall('r:adp',ns))==42
   if sc[0]==37:assert ids[0]==384
   if sc[0]==38:assert len(reacts)==nw-2
   if sc[0]==22:assert ncy==30 and status==5
   results.append({'file':str(p.relative_to(root)),'case':sc[0],'reacts':len(reacts),'cyclesReached':ncy,'sha256':hashlib.sha256(p.read_bytes()).hexdigest()})
assert len(results)==102,len(results)
(root/'build/package-verification.json').write_text(json.dumps({'passed':True,'packages':len(results),'checks':['all ZIP entry CRCs','XML well positions and control presence','both channel acquisitions exactly match native record','integer cycle/count serialization','scenario parameter snapshots','dropout and crash records'], 'results':results},indent=2))
print('PASS',len(results),'independent ZIP/XML/provenance comparisons')
