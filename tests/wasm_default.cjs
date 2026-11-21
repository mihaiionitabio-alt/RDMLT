const fs=require('fs'),assert=require('assert/strict');
const k=require('../native-engine.js')({print(){},printErr(){}});
function status(){const p=k._malloc(48);k._native_status(p);const a=Array.from(new Int32Array(k.HEAPU8.buffer,p,12));k._free(p);return a;}
k._native_event(4,0,0,27);k._native_render();k._native_event(1,200,700,0);
let last=0;for(let i=0;i<150;i++){last=k._native_tick(1000);if(!status()[5]&&!status()[6])break;}
assert.equal(status()[3],45);assert.equal(last,1,'Completion must request a final redraw');
const p=k._malloc(160);k._native_review(p);const review=Array.from(new Float64Array(k.HEAPU8.buffer,p,20));k._free(p);
assert.equal(review[1],14);assert.equal(review[4+10],0);assert.equal(review[4+11],0);
k._native_render();k._native_event(1,1100,1420,0);k._native_render();k._native_event(1,2200,300,0);
const addr=k._native_output(),n=k._native_output_size();fs.writeFileSync('build/default-3x3.rdmlt',k.HEAPU8.slice(addr,addr+n));
const host=fs.readFileSync('browser-host.js','utf8');assert.equal((host.match(/\.putImageData\(/g)||[]).length,1);
assert.ok(!/JSON\.parse|\.fillRect\(|\.fillText\(|\.lineTo\(|Math\.(exp|log|sqrt|sin|cos)/.test(host));
const report={passed:true,checks:['maximum 3x3 design with 14 wells','45 acquisitions','Fortran blank/NTC decisions','final-frame invalidation','single native framebuffer blit','no JSON parser/scientific maths/vector drawing in handwritten host'],review,hostLines:host.split('\n').length};
fs.writeFileSync('build/wasm-default-verification.json',JSON.stringify(report,null,2));console.log(JSON.stringify(report));
