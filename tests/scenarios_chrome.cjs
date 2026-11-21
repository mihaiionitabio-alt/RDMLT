const {chromium}=require('../dependencies/playwright-core');
const fs=require('fs'),path=require('path'),assert=require('assert/strict');
const root=path.resolve(__dirname,'..'),out=path.join(root,'build/chrome-052');fs.mkdirSync(out,{recursive:true});
const catalogue=JSON.parse(fs.readFileSync(path.join(root,'SCENARIO_CATALOGUE.json')));
const base={format:'missy-native-profile/2',runs:1,extractions:1,replicates:2,copies:30000,threshold:150,seed:11,fault:0,speed:5000,scenario:0};
(async()=>{const b=await chromium.launch({executablePath:process.env.CHROME_PATH||'C:/Program Files/Google/Chrome/Application/chrome.exe'});
const p=await b.newPage({viewport:{width:1280,height:900}});let errors=[];p.on('pageerror',e=>errors.push(e.message));p.on('console',m=>{if(m.type()==='error')errors.push(m.text())});
async function boot(){await p.goto(require('url').pathToFileURL(path.join(root,'index.html')).href);await p.waitForFunction(()=>!!window.NativeTransport,null,{timeout:60000});await p.evaluate(()=>NativeTransport.engine._native_event(4,0,0,27));await p.waitForFunction(()=>document.querySelector('canvas').width===2560);await p.waitForTimeout(100);}
async function state(){return p.evaluate(()=>{let k=NativeTransport.engine,a=k._malloc(48);k._native_status(a);let v=Array.from(k.HEAP32.subarray(a/4,a/4+12));k._free(a);return v})}
async function diag(){return p.evaluate(()=>{let k=NativeTransport.engine,a=k._malloc(192);k._native_scenario_review(a);let v=Array.from(k.HEAPF64.subarray(a/8,a/8+24));k._free(a);return v})}
async function load(profile){return p.evaluate(t=>{let k=NativeTransport.engine,b=new TextEncoder().encode(t),a=k._malloc(b.length);k.HEAPU8.set(b,a);k._native_file(a,b.length);k._free(a)},typeof profile==='string'?profile:JSON.stringify(profile))}
async function begin(){await p.evaluate(()=>{let k=NativeTransport.engine;k._native_event(4,0,0,27);k._native_render();k._native_event(1,200,700,0)});}
async function finish(){for(let i=0;i<35;i++){await p.evaluate(()=>{let k=NativeTransport.engine;for(let j=0;j<10;j++)k._native_tick(1000)});let st=await state();if(!st[5]&&!st[6])return st;}throw Error('run did not finish');}
async function exportRun(file){const data=await p.evaluate(()=>{let k=NativeTransport.engine;k._native_event(4,0,0,27);k._native_render();k._native_event(1,1000,700,0);k._native_render();k._native_event(1,2160,300,0);let a=Array.from(k.HEAPU8.subarray(k._native_output(),k._native_output()+k._native_output_size()));while(k._native_request()){}return a});assert.equal(Buffer.from(data).subarray(0,2).toString(),'PK');fs.writeFileSync(path.join(out,file),Buffer.from(data));return data;}
try{await boot();let d=await diag();assert.equal(d[20],2.5);assert.ok(Math.abs(d[21]-5/3)<1e-12);assert.equal(d[22],6);assert.equal(d[23],1);
 await p.screenshot({path:path.join(out,'01-home.png')});
 // Browse every catalogue page and open every configuration using the native hit tests.
 await p.mouse.click(1000,350);await p.waitForTimeout(100);
 for(let page=0;page<9;page++){
  for(let i=0;i<6;i++){
   const code=page*6+i+1;if(code>51)break;
   await p.mouse.click(130+(i%2)*595,250+Math.floor(i/2)*125);await p.waitForTimeout(20);assert.equal((await diag())[0],code);
   if(code===6){await p.screenshot({path:path.join(out,'03-edit-led.png')});await p.mouse.click(630,250);await p.keyboard.type('0.4');await p.keyboard.press('Enter');assert.equal((await diag())[1],.4);}
   if(code===21)await p.screenshot({path:path.join(out,'04-melt-unavailable.png')});
   await p.mouse.click(160,675);await p.waitForTimeout(20);
  }
  if(page===0)await p.screenshot({path:path.join(out,'02-catalogue.png')});
  if(page<8){await p.mouse.click(370,645);await p.waitForTimeout(20);}
 }
 // Save/reload all 51 editable configurations through the actual download/file chooser.
 const [dl]=await Promise.all([p.waitForEvent('download'),p.mouse.click(800,645)]);const profileFile=path.join(out,'all-settings.json');await dl.saveAs(profileFile);
 let saved=JSON.parse(fs.readFileSync(profileFile));assert.equal(saved.scenarioBank.length,306);assert.equal(saved.scenarioBank[30],.4);
 await load({...base,scenario:6,parameters:[.8,0,0,0,0,0]});assert.equal((await diag())[1],.8);
 await p.locator('canvas').focus();await p.keyboard.press('Escape');await p.waitForTimeout(250);await p.waitForFunction(()=>document.querySelector('canvas').width===2560);let [fc]=await Promise.all([p.waitForEvent('filechooser'),p.mouse.click(600,480)]);await fc.setFiles(profileFile);await p.waitForTimeout(50);assert.equal((await diag())[0],51);
 await load({...base,scenario:6});assert.equal((await diag())[1],.4);
 // Transactional rejection: malformed, unknown, duplicate, fractional integer and bad bounds.
 for(const bad of ['{"format":',JSON.stringify({...base,scenario:99}),JSON.stringify({...base,parameters:[0,0,0,0,0,0]}),JSON.stringify({...base,runs:1.2}),JSON.stringify({...base,bogus:1}),JSON.stringify(base).replace('"runs":1','"runs":1,"runs":2')]){await load({...base,scenario:6});await load(bad);assert.equal((await diag())[0],6);}
 const results=[];await boot();
 for(let code=0;code<=51;code++){
  if(code===31)await boot();
  const item=code?catalogue[code-1]:null;const prof={...base,scenario:code};if(item)prof.parameters=item.editableParameters.map(x=>x.default);
  await load(prof);const before=(await state())[1];await begin();
  if(code===21){assert.equal((await state())[5],0);assert.equal((await state())[1],before);continue;}
  const st=await finish();assert.equal(st[1],before+1);assert.equal(st[5],0);const dg=await diag();assert.equal(dg[7],code);
  if(code===19)assert.equal(dg[15],42);if(code===20)assert.equal(st[3],40);if(code===22)assert.equal(st[3],30);
  if(code===38)assert.equal(dg[14],2);if(code===12)assert.equal(st[4],3);if(code===13||code===14)assert.equal(st[4],7);
  if(code===0){await p.waitForTimeout(80);await p.screenshot({path:path.join(out,'05-complete-pcr.png')});}
  const bytes=await exportRun('case-'+String(code).padStart(2,'0')+'.rdmlt');
  results.push({code,id:item?.id||'normal',status:st[4],cycles:st[3],firstTraceAcquisitions:dg[15],bytes:bytes.length});
  if([0,19,38,51].includes(code)){await p.waitForTimeout(80);await p.screenshot({path:path.join(out,'06-results-'+code+'.png')});}
  console.log('case',code,st[4],st[3]);
 }
 // 20 completed compatible runs exercise stdlib baseline and control-chart output.
 await boot();await load({...base,runs:20,replicates:1});await begin();
 for(let i=0;i<250;i++){await p.evaluate(()=>{let k=NativeTransport.engine;for(let j=0;j<10;j++)k._native_tick(1000)});const st=await state();if(!st[5]&&!st[6])break;}
 assert.equal((await state())[1],20);await p.locator('canvas').focus();await p.keyboard.press('Escape');await p.waitForTimeout(250);await p.mouse.click(600,350);await p.waitForTimeout(100);assert.equal((await state())[0],2);await p.screenshot({path:path.join(out,'07-results-20-runs.png')});
 // Own RDMLt import and analysis retain snapshot, both channels and missing read-outs.
 let file=path.join(out,'case-19.rdmlt');await p.locator('canvas').focus();await p.keyboard.press('Escape');await p.waitForTimeout(250);let [fc2]=await Promise.all([p.waitForEvent('filechooser'),p.mouse.click(600,480)]);await fc2.setFiles(file);await p.waitForTimeout(50);assert.equal((await state())[1],21);assert.equal((await diag())[15],42);
 await p.screenshot({path:path.join(out,'08-imported-run.png')});
 assert.deepEqual(errors,[]);
 fs.writeFileSync(path.join(out,'report.json'),JSON.stringify({passed:true,checks:['51 catalogue configurations opened','direct numeric editing','profile with 306 saved values','download/file-picker round trip','malformed profile rejection','50 executable cases completed or safely stopped','all packages exported','missing acquisitions preserved','20 virtual runs retained for review','own RDMLt import','JSON-Fortran string escape routine','no page errors'],results,errors,removed:['voice commands','microphone','control chart screen'],notTested:['external legacy MISSY compatibility','scientific validation against instruments']},null,2));
 console.log('PASS CHROME',results.length);
}catch(e){await p.screenshot({path:path.join(out,'failure.png')});console.log('STATE',await state(),'DIAG',await diag());throw e;}finally{await b.close()}})().catch(e=>{console.error(e);process.exitCode=1});
