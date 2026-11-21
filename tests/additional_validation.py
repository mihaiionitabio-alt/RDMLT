import ctypes as C,json,zipfile,io
from pathlib import Path
r=Path(__file__).resolve().parents[1];k=C.CDLL(str(r/'build/libr2.so'))
k.native_render.restype=C.c_void_p;k.native_tick.argtypes=[C.c_double]
k.native_event.argtypes=[C.c_int,C.c_double,C.c_double,C.c_int]
k.native_file.argtypes=[C.c_void_p,C.c_int];k.native_output.restype=C.c_void_p
base=dict(format='missy-native-profile/2',runs=1,extractions=1,replicates=2,copies=30000,threshold=150,seed=11,fault=0,speed=5000,scenario=0)
def st():
 a=(C.c_int*12)();k.native_status(a);return list(a)
def diag():
 a=(C.c_double*24)();k.native_scenario_review(a);return list(a)
def load(b):
 if isinstance(b,dict):b=json.dumps(b).encode()
 a=C.create_string_buffer(b);k.native_file(a,len(b))
def run(p):
 load(base|p);k.native_event(4,0,0,27);k.native_render();k.native_event(1,200,700,0)
 for i in range(200):
  k.native_tick(1000)
  if not st()[5] and not st()[6]:break
 k.native_event(4,0,0,27);k.native_render();k.native_event(1,1000,700,0);k.native_render();k.native_event(1,2160,300,0)
 return C.string_at(k.native_output(),k.native_output_size())
checks=[]
b=run(dict(scenario=6,parameters=[.2,0,0,0,1,0]));assert diag()[7]==6
with zipfile.ZipFile(io.BytesIO(b)) as z:
 rec=z.read('native-run.txt').decode().splitlines()
 vals=list(map(int,rec[4+2*44].split()))
 assert vals[0]<.3*vals[1],vals
checks.append('custom well mask affects only requested sample')
old=st()[1]
for token in [b'missy-native-provenance/1',b'http://www.rdml.org']:
 corrupt=bytearray(b);corrupt[b.index(token)]^=1;load(bytes(corrupt));assert st()[1]==old
checks.append('CRC rejects corrupted provenance and core XML')
load(base|dict(scenario=19,parameters=[18,3,0,0,0,0]));oldparams=diag()[:7]
load(base|dict(scenario=38,parameters=[.5,1,0,0,0,0]));assert diag()[:7]==oldparams
load(base|dict(scenario=19,parameters=[18.2,3,0,0,0,0]));assert diag()[:7]==oldparams
checks.append('fractional omission switches and cycle indices rejected transactionally')
b=run(dict(scenario=37,parameters=[90,0,0,0,0,0]));assert st()[4]==6 and st()[3]==0
checks.append('90 degree plate orientation safely rejected before acquisition')
legacy=r.parent/'MISSYR2-native-0.5.0-preview/build/sample.rdmlt'
old=st()[1];load(legacy.read_bytes());assert st()[1]==old+1 and diag()[7]==0
checks.append('older native record /1 imports with no invented YY readings')
load((r/'build/chrome-051/all-settings.json').read_bytes());assert diag()[0]==51
checks.append('saved 306-value bank loads with selected configuration')
(r/'build/additional-verification.json').write_text(json.dumps(dict(passed=True,checks=checks),indent=2))
print(json.dumps(dict(passed=True,checks=checks)))
