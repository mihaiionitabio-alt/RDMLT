/* SPDX-License-Identifier: GPL-3.0-only
 * Platform transport only. No JSON parsing, scientific calculations, menus,
 * layout rules, SOP evaluation, chart specifications or command interpretation.
 */
(function () {
  "use strict";
  const canvas=document.getElementById('display'),status=document.getElementById('platform-status');
  let engine,last=performance.now();
  const decoder=new TextDecoder();
  function input(bytes,entry){
    const p=engine._malloc(Math.max(1,bytes.length));
    try{engine.HEAPU8.set(bytes,p);engine[entry](p,bytes.length);}finally{engine._free(p);}
  }
  function bytes(){return engine.HEAPU8.slice(engine._native_output(),engine._native_output()+engine._native_output_size());}
  function requests(){
    for(let i=0;i<8;i++){
      const request=engine._native_request();if(!request)break;
      if(request===1)document.getElementById('platform-file').click();
      if(request===2){
        const p=engine._native_filename(),n=engine._native_filename_size();
        const name=decoder.decode(engine.HEAPU8.subarray(p,p+n));
        const url=URL.createObjectURL(new Blob([bytes()],{type:'application/octet-stream'}));
        const link=document.createElement('a');link.href=url;link.download=name;link.click();
        setTimeout(()=>URL.revokeObjectURL(url),4000);
      }
    }
  }
  function event(kind,e,value){
    const rect=canvas.getBoundingClientRect();
    const scale=Math.min(rect.width/canvas.width,rect.height/canvas.height);
    const left=rect.left+(rect.width-canvas.width*scale)/2,top=rect.top+(rect.height-canvas.height*scale)/2;
    engine._native_event(kind,(e.clientX-left)/scale,(e.clientY-top)/scale,value||0);
    requests();
  }
  function frame(t){
    const changed=engine._native_tick(t-last);last=t;
    if(changed){
      const p=engine._native_render(),w=engine._native_frame_width(),h=engine._native_frame_height();
      if(canvas.width!==w)canvas.width=w;if(canvas.height!==h)canvas.height=h;
      const data=new Uint8ClampedArray(engine.HEAPU8.buffer,p,w*h*4);
      canvas.getContext('2d').putImageData(new ImageData(data,w,h),0,0);
    }
    requests();requestAnimationFrame(frame);
  }
  try{
    engine=R2FortranModule({print(){},printErr(s){console.warn(s);}});
    // Exposed for diagnostics only; all UI decisions are made by exported Fortran procedures.
    window.NativeTransport={engine};
    status.remove();
    canvas.addEventListener('pointerdown',e=>{canvas.focus();canvas.setPointerCapture(e.pointerId);event(1,e);});
    canvas.addEventListener('pointermove',e=>event(2,e));
    canvas.addEventListener('pointerup',e=>{event(3,e);if(canvas.hasPointerCapture(e.pointerId))canvas.releasePointerCapture(e.pointerId);});
    canvas.addEventListener('wheel',e=>{e.preventDefault();event(6,e,e.deltaY);},{passive:false});
    canvas.addEventListener('keydown',e=>{e.preventDefault();engine._native_event(4,0,0,e.keyCode);requests();});
    const resize=()=>engine._native_event(5,window.innerWidth,window.innerHeight,0);
    window.addEventListener('resize',resize);resize();
    document.getElementById('platform-file').addEventListener('change',async e=>{
      for(const file of e.target.files)input(new Uint8Array(await file.arrayBuffer()),'_native_file');
      e.target.value='';requests();
    });
    requestAnimationFrame(frame);
  }catch(e){status.textContent='The native module could not start: '+e.message;console.error(e);}
})();
