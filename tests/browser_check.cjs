const {chromium}=require('../dependencies/playwright-core');
const fs=require('fs'),path=require('path'),assert=require('assert/strict');
const root=path.resolve(__dirname,'..'),out=path.join(root,'build/browser');fs.mkdirSync(out,{recursive:true});
(async()=>{
 const b=await chromium.launch({executablePath:process.env.CHROME_PATH||'C:/Program Files/Google/Chrome/Application/chrome.exe'});
 const p=await b.newPage({viewport:{width:1280,height:900}}),errors=[];p.on('pageerror',e=>errors.push(e.message));
 const state=()=>p.evaluate(()=>{const k=NativeTransport.engine,a=k._malloc(48);k._native_status(a);const v=Array.from(new Int32Array(k.HEAPU8.buffer,a,12));k._free(a);return v;});
 const profile={format:'missy-native-profile/1',runs:1,extractions:2,replicates:2,copies:30000,threshold:150,seed:17,fault:0,speed:5000};
 try{
  await p.goto(require('url').pathToFileURL(path.join(root,'index.html')).href);
  await p.waitForFunction(()=>!!window.NativeTransport,null,{timeout:60000});await p.evaluate(()=>NativeTransport.engine._native_event(4,0,0,27));await p.waitForTimeout(150);await p.screenshot({path:path.join(out,'01-home.png')});
  fs.writeFileSync(path.join(out,'profile.json'),JSON.stringify(profile));
  const [fc]=await Promise.all([p.waitForEvent('filechooser'),p.mouse.click(600,480)]);await fc.setFiles(path.join(out,'profile.json'));
  await p.waitForTimeout(50);assert.deepEqual((await state()).slice(7,10),[2,2,1]);
  await p.mouse.click(100,350);
  await p.waitForFunction(()=>{const k=NativeTransport.engine,a=k._malloc(48);k._native_status(a);const v=new Int32Array(k.HEAPU8.buffer,a,12);const ok=v[1]===1&&v[3]===45&&v[4]===1;k._free(a);return ok;},null,{timeout:120000});
  await p.waitForTimeout(100);await p.screenshot({path:path.join(out,'02-fluorescence.png')});
  const nativePixels=await p.locator('canvas').evaluate(c=>({w:c.width,h:c.height,png:c.toDataURL('image/png').split(',')[1]}));
  assert.deepEqual([nativePixels.w,nativePixels.h],[2560,1800]);fs.writeFileSync(path.join(out,'02-fluorescence-full-resolution.png'),Buffer.from(nativePixels.png,'base64'));
  await p.mouse.click(180,790);await p.waitForTimeout(100);await p.screenshot({path:path.join(out,'03-temperature.png')});
  await p.mouse.click(550,790);await p.waitForTimeout(50);assert.equal((await state())[0],2);await p.screenshot({path:path.join(out,'04-results.png')});
  const review=await p.evaluate(()=>{const k=NativeTransport.engine,a=k._malloc(160);k._native_review(a);const v=Array.from(new Float64Array(k.HEAPU8.buffer,a,20));k._free(a);return v;});
  assert.equal(review[1],9);assert.ok(review[4]>0);assert.equal(review[4+5],0);assert.equal(review[4+6],0);
  const [dl]=await Promise.all([p.waitForEvent('download'),p.mouse.click(1100,150)]);await dl.saveAs(path.join(out,'sample.rdmlt'));
  const before=(await state())[1];const [fc2]=await Promise.all([p.waitForEvent('filechooser'),(async()=>{await p.keyboard.press('Escape');await p.waitForTimeout(50);await p.mouse.click(600,480);})()]);
  await fc2.setFiles(path.join(out,'sample.rdmlt'));await p.waitForTimeout(100);assert.equal((await state())[1],before+1);
  await p.keyboard.press('Escape');await p.waitForTimeout(100);await p.mouse.click(100,480);
  assert.equal((await state())[0],4);await p.waitForTimeout(100);await p.screenshot({path:path.join(out,'05-profile.png')});
  await p.keyboard.press('Escape');await p.waitForTimeout(50);await p.mouse.click(950,480);await p.waitForTimeout(50);assert.equal((await state())[0],5);await p.screenshot({path:path.join(out,'06-limits.png')});
  const audit={scriptFiles:await p.locator('script[src]').evaluateAll(ns=>ns.map(n=>n.getAttribute('src'))),htmlButtons:await p.locator('button').count(),hostLines:fs.readFileSync(path.join(root,'browser-host.js'),'utf8').split('\n').length};
  assert.equal(audit.htmlButtons,0);assert.deepEqual(audit.scriptFiles,['native-engine.js','browser-host.js']);assert.deepEqual(errors,[]);
  const report={passed:true,framebuffer:[2560,1800],pointsPerTrace:45,state:await state(),review,audit,errors,checks:['browser native UI boot','native profile file chooser','2x2 extraction/replicate layout','45-cycle Wasm run','Fortran results and control identities','native chart switch','native RDMLt download/import','native profile navigation'],removed:['voice commands','microphone','control charts']};
  fs.writeFileSync(path.join(out,'report.json'),JSON.stringify(report,null,2));console.log(JSON.stringify(report));
 }catch(e){await p.screenshot({path:path.join(out,'failure.png')}).catch(()=>{});throw e;}finally{await b.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
