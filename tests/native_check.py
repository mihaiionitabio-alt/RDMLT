import ctypes as C,json,zipfile,io,time
from pathlib import Path
r=Path(__file__).resolve().parents[1];k=C.CDLL(str(r/'build/libr2.so'))
k.native_render.restype=C.c_void_p;k.native_tick.argtypes=[C.c_double]
k.native_event.argtypes=[C.c_int,C.c_double,C.c_double,C.c_int]
k.native_output.restype=C.c_void_p;k.native_output_size.restype=C.c_int
k.native_file.argtypes=[C.c_void_p,C.c_int]
def status():
 a=(C.c_int*12)();k.native_status(a);return list(a)
def render():assert k.native_render()
def click(x,y):k.native_event(1,x*2,y*2,0)
def profile(**kwargs):
 p=dict(format='missy-native-profile/1',runs=1,extractions=1,replicates=2,copies=30000,threshold=150,seed=11,fault=0,speed=5000);p.update(kwargs)
 b=json.dumps(p,separators=(',',':')).encode();a=C.create_string_buffer(b);k.native_file(a,len(b));return p
k.native_event(4,0,0,27)
print('RENDER',flush=True);render();print('PROFILE',flush=True);profile();assert status()[7:10]==[1,2,1],status()
# Failed profile is transactional.
profile(extractions=9);assert status()[7]==1
print('START',flush=True);profile();click(100,350);assert status()[5]==1,status()
t=time.time()
for i in range(160):
 changed=k.native_tick(1000)
 if not status()[5] and status()[6]==0:assert changed==1
 if i%10==0:print('TICK',i,status(),flush=True)
 if not status()[5] and status()[6]==0:break
print('FINISHED',status(),flush=True);st=status();assert st[1]==1 and st[3]==45 and st[4]==1,st
# Navigate to results using native hit-testing, then export.
print('RENDER LIVE',flush=True);render();print('RESULT CLICK',flush=True);click(550,k.native_height()-110);render();assert status()[0]==2,status()
print('EXPORT CLICK',flush=True);click(k.native_width()-200,150);print('EXPORTED',flush=True)
b=C.string_at(k.native_output(),k.native_output_size());assert b[:4]==b'PK\x03\x04'
(r/'build/sample.rdmlt').write_bytes(b)
with zipfile.ZipFile(io.BytesIO(b)) as z:
 assert z.testzip() is None
 record=z.read('native-run.txt').decode().splitlines();head=list(map(int,record[1].split()));assert head[1]==45
 assert len(record)==2*head[1]+head[9]+4
 assert b'<cyc>45</cyc>' in z.read('rdml_data.xml')
 import xml.etree.ElementTree as ET
 xml=ET.fromstring(z.read('rdml_data.xml'));ns={'r':'http://www.rdml.org'}
 for react in xml.findall('.//r:react',ns):
  for data in react.findall('r:data',ns):
   points=data.findall('r:adp',ns);assert len(points)==45
   assert [int(p.find('r:cyc',ns).text) for p in points]==list(range(1,46))
 # Native import round trip and CRC rejection.
 a=C.create_string_buffer(b);k.native_file(a,len(b));assert status()[1]==2,status()
 corrupt=bytearray(b);start=b.index(b'MISSY-NATIVE-2');corrupt[start+1]^=1
 a=C.create_string_buffer(bytes(corrupt));k.native_file(a,len(corrupt));assert status()[1]==2
 forged=bytearray(b);forged[18:22]=(2147483647).to_bytes(4,'little');a=C.create_string_buffer(bytes(forged));k.native_file(a,len(forged));assert status()[1]==2
profile(fault=1);k.native_event(4,0,0,27);render();click(100,350)
for i in range(12):
 k.native_tick(1000)
 if not status()[5]:break
assert status()[4]==3 and status()[1]==3,status()
# Twenty compatible runs exercise native batch orchestration and baseline history.
profile(runs=20,replicates=1);k.native_event(4,0,0,27);render();click(100,350)
for i in range(5000):
 k.native_tick(1000)
 if not status()[5] and status()[6]==0:break
assert status()[1]==23 and status()[3]==45,status()
k.native_event(4,0,0,27);render();click(550,350);render()
assert status()[0]==2,status()
# Operator stop files the current partial record and cancels the batch.
profile(runs=1,replicates=1);k.native_event(4,0,0,27);render();click(100,350);k.native_tick(1000);k.native_event(4,0,0,32)
assert status()[4]==2 and status()[1]==24 and status()[6]==0,status()
k.native_event(4,0,0,27);render();click(550,350);render()
report=dict(passed=True,checks=['transactional native JSON profile','native button hit-testing','complete 45-cycle kinetic PCR','native RDMLt CRC-valid ZIP export','native file import round trip','corrupt input rejected','independent secondary-bias stop','20-run native batch','operator stop files partial run'],seconds=round(time.time()-t,2),status=status(),sampleBytes=len(b))
(r/'build/native-verification.json').write_text(json.dumps(report,indent=2));print(json.dumps(report))
