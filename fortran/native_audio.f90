! SPDX-License-Identifier: GPL-3.0-only
! MISSY R2 Fortran: interface sounds and speech.
! Every cue of the MISSY R2 sound catalogue (native_audio_data: 507 cues) is rendered here to
! 16-bit PCM at 22 050 Hz with the same synthesis as the original Web Audio code (oscillators
! with linear frequency ramps, xorshift noise, biquad filters, attack / hold / release
! envelope); the two sampled cues are taken from their stored PCM bytes. The result is written
! as a WAV file into a byte array, encoded as base64 and handed to the browser, which turns it
! into a temporary Blob file and plays it. Speech uses the browser's speech synthesis.
module native_audio
 use iso_c_binding
 use native_audio_data
 use native_sys
 implicit none
 private
 public :: au_on, voice_on, play_cue, play_tone, speak, say_calm, load_calm, cue_index, cue_phrase, last_cue, cues_played
 logical,save :: au_on=.true.,voice_on=.true.
 integer,save :: last_cue=0,cues_played=0
 integer,parameter :: srate=22050,maxs=40000
 real(c_double),parameter :: volume=.55d0,pi=3.141592653589793d0
 real(c_double),save :: mixb(maxs)
 integer,save :: wavb(44+2*maxs)
 character(len=4*((44+2*maxs+2)/3)+600),save :: js
contains
! @nativefn NR2-AUD-F0001
 integer function cue_index(id) result(k)
  character(len=*),intent(in)::id
  integer::i
  k=0
  do i=1,au_n
   if(trim(au_id(i))==trim(id))then
    k=i;return
   end if
  end do
 end function
! @nativefn NR2-AUD-F0002
 function cue_phrase(id) result(t)
  character(len=*),intent(in)::id
  character(len=64)::t
  integer::k
  k=cue_index(id);t=''
  if(k>0)t=au_en(k)
 end function
! @nativefn NR2-AUD-F0003
 subroutine play_cue(id)
  character(len=*),intent(in)::id
  integer::k,n
  logical::said
  if(.not.au_on)return
  k=cue_index(id);if(k==0)return
  call render(k,n)
  if(n<=0)return
  call to_wav(n)
  call send_wav(44+2*n)
  last_cue=k;cues_played=cues_played+1
  ! as in MISSY: the cue tone, then its phrase in the Calm v1 voice once the tone has ended
  if(k<=99)said=say_calm(trim(au_en(k)),nint(1000d0*n/srate),.true.)   ! cues 1-99 have a phrase
 end subroutine
! @nativefn NR2-AUD-F0003B
 subroutine play_tone(id)
  ! The cue tone alone (used when a longer description is spoken right after it).
  character(len=*),intent(in)::id
  integer::k,n
  if(.not.au_on)return
  k=cue_index(id);if(k==0)return
  call render(k,n)
  if(n<=0)return
  call to_wav(n)
  call send_wav(44+2*n)
  last_cue=k;cues_played=cues_played+1
 end subroutine
! @nativefn NR2-AUD-F0003C
 subroutine load_calm()
  ! Loads the MISSY Calm v1 phrases (missy-calm-v1.txt beside the page: phrase, tab, base64 MP3)
  ! once at start-up, in the background.
  call sys_script("(function(){if(window.__calmLoading)return;window.__calmLoading=1;"// &
   "fetch('missy-calm-v1.txt').then(function(r){return r.ok?r.text():''}).then(function(t){var m={};"// &
   "t.split('\n').forEach(function(l){var i=l.indexOf('\t');if(i>0&&l.charAt(0)!=='#')m[l.slice(0,i)]=l.slice(i+1)});"// &
   "window.__calm=m}).catch(function(){})})()")
 end subroutine
! @nativefn NR2-AUD-F0003D
 logical function say_calm(text,delay_ms,interrupt) result(done)
  ! MISSY voice: the recorded Calm v1 phrase when the text is one of them, otherwise the browser's
  ! local English voice chosen as in MISSY (Microsoft Zira when installed, else the first local
  ! English voice), rate 0.9, pitch 1, volume 0.8, "Cq" read as "threshold cycle".
  ! interrupt=.true. replaces anything being said (after delay_ms); otherwise the phrase is
  ! dropped while another is still being spoken.
  character(len=*),intent(in)::text
  integer,intent(in)::delay_ms
  logical,intent(in)::interrupt
  character(len=400)::t
  character(len=2400)::cmd
  integer::i,n
  done=.false.
  if(.not.voice_on.or.len_trim(text)==0)return
  done=.true.
  t='';n=0
  do i=1,min(len_trim(text),390)
   if(text(i:i)=="'".or.text(i:i)=='\'.or.iachar(text(i:i))<32)then
    n=n+1;t(n:n)=' '
   else
    n=n+1;t(n:n)=text(i:i)
   end if
  end do
  cmd="(function(){try{var T='"//t(1:n)//"',S=window.speechSynthesis;"// &
   "function busy(){var c=window.__mcalm;return (c&&!c.paused&&!c.ended)||(S&&(S.speaking||S.pending))}"// &
   "function go(){var d=window.__calm&&window.__calm[T];"// &
   "if(d){var a=new Audio('data:audio/mpeg;base64,'+d);a.volume=.8;window.__mcalm=a;a.play().catch(function(){});return}"// &
   "if(!S)return;var vs=S.getVoices().filter(function(v){return v.localService&&v.lang.toLowerCase().indexOf('en')===0});"// &
   "var v=vs.filter(function(v){return /zira/i.test(v.name||v.voiceURI)})[0]||vs[0];"// &
   "var m=new SpeechSynthesisUtterance(T.replace(/\bCq\b/gi,'threshold cycle'));"// &
   "if(v){m.voice=v;m.lang=v.lang}else{m.lang='en-US'}m.rate=.9;m.pitch=1;m.volume=.8;S.speak(m)}"
  if(interrupt)then
   cmd=trim(cmd)//"var g=window.__mcg=(window.__mcg||0)+1;setTimeout(function(){if(g!==window.__mcg)return;"// &
    "if(window.__mcalm)window.__mcalm.pause();if(S)S.cancel();go()},"//trim(itxt(delay_ms))//")}catch(e){}})()"
  else
   cmd=trim(cmd)//"if(busy())return;go()}catch(e){}})()"
  end if
  call sys_script(trim(cmd))
 end function
 function itxt(v) result(t)
  integer,intent(in)::v
  character(len=12)::t
  write(t,'(i0)')max(0,v)
 end function
! @nativefn NR2-AUD-F0004
 subroutine render(k,n)
  ! Mixes the notes of cue k into mixb(1:n) (floating point, -1..1).
  integer,intent(in)::k
  integer,intent(out)::n
  integer::j,i,i0,i1,s,b0
  real(c_double)::tend,g,st,ln,att,rel,t,f,ph,v,env,y,x1,x2,y1,y2,c0,c1,c2,d1,d2,w0,al,q,cw
  integer(8)::seed
  integer(8),parameter::m32=4294967295_8
  mixb=0;n=0
  if(au_smp(k)>0)then
   s=au_smp(k);b0=au_sfirst(s)
   n=min(maxs,(au_slast(s)-b0+1)/2)
   do i=1,n
    v=au_bytes(b0+2*(i-1))+256*au_bytes(b0+2*(i-1)+1)
    if(v>=32768)v=v-65536
    mixb(i)=v/32768d0*volume*au_vol(k)/1000d0
   end do
   return
  end if
  tend=0
  do j=au_first(k),au_last(k)
   tend=max(tend,(au_t(j)+au_l(j))/1d4)
  end do
  n=min(maxs,int((tend+.06d0)*srate))
  g=merge(.009d0,.055d0,au_bg(k)==1)*volume*au_vol(k)/1000d0
  do j=au_first(k),au_last(k)
   st=.01d0+au_t(j)/1d4;ln=au_l(j)/1d4
   att=min(.015d0,ln/4);rel=max(att,ln-.04d0)
   i0=int(st*srate)+1;i1=min(n,int((st+ln)*srate))
   ph=0;seed=123456789_8;x1=0;x2=0;y1=0;y2=0
   c0=1;c1=0;c2=0;d1=0;d2=0
   if(au_ft(j)>0)then
    w0=2*pi*(au_fc(j)/100d0)/srate;cw=cos(w0);q=au_q(j)/1000d0
    if(au_ft(j)/=1)q=10**(q/20)            ! low / high pass: Q in dB (Web Audio)
    al=sin(w0)/(2*max(1d-3,q))
    select case(au_ft(j))
    case(1)
     c0=al;c1=0;c2=-al
    case(2)
     c0=(1+cw)/2;c1=-(1+cw);c2=(1+cw)/2
    case default
     c0=(1-cw)/2;c1=1-cw;c2=(1-cw)/2
    end select
    c0=c0/(1+al);c1=c1/(1+al);c2=c2/(1+al);d1=-2*cw/(1+al);d2=(1-al)/(1+al)
   end if
   do i=i0,i1
    t=(i-1)/real(srate,c_double)-st
    if(t<0)cycle
    f=au_f(j)/100d0
    if(au_e(j)>0)f=f+(au_e(j)/100d0-f)*min(1d0,t/max(1d-6,ln))
    select case(au_w(j))
    case(5)
     seed=ieor(seed,iand(ishft(seed,13),m32));seed=ieor(seed,ishft(seed,-17));seed=ieor(seed,iand(ishft(seed,5),m32))
     v=real(seed,c_double)/2147483648d0-1
    case(2)
     v=1-4*abs(ph-.5d0)
    case(3)
     v=merge(1d0,-1d0,ph<.5d0)
    case(4)
     v=2*ph-1
    case default
     v=sin(2*pi*ph)
    end select
    ph=ph+f/srate;ph=ph-floor(ph)
    if(au_ft(j)>0)then
     y=c0*v+c1*x1+c2*x2-d1*y1-d2*y2;x2=x1;x1=v;y2=y1;y1=y;v=y
    end if
    if(t<att)then
     env=t/att
    else if(t<rel)then
     env=1
    else
     env=max(0d0,(ln-t)/max(1d-6,ln-rel))
    end if
    mixb(i)=mixb(i)+g*env*v
   end do
  end do
 end subroutine
! @nativefn NR2-AUD-F0005
 subroutine to_wav(n)
  ! RIFF / WAVE header (PCM, mono, 16 bit, 22 050 Hz) and the samples, as bytes 0..255.
  integer,intent(in)::n
  integer::i,v
  call put4(1,int(z'46464952'));call put4(5,36+2*n);call put4(9,int(z'45564157'));call put4(13,int(z'20746d66'))
  call put4(17,16);call put2(21,1);call put2(23,1);call put4(25,srate);call put4(29,2*srate);call put2(33,2);call put2(35,16)
  call put4(37,int(z'61746164'));call put4(41,2*n)
  do i=1,n
   v=nint(max(-1d0,min(1d0,mixb(i)))*32767);if(v<0)v=v+65536
   wavb(44+2*i-1)=iand(v,255);wavb(44+2*i)=iand(ishft(v,-8),255)
  end do
 contains
  subroutine put4(p,x)
   integer,intent(in)::p,x
   integer(8)::u
   u=x;if(u<0)u=u+4294967296_8
   wavb(p)=int(iand(u,255_8));wavb(p+1)=int(iand(ishft(u,-8),255_8))
   wavb(p+2)=int(iand(ishft(u,-16),255_8));wavb(p+3)=int(iand(ishft(u,-24),255_8))
  end subroutine
  subroutine put2(p,x)
   integer,intent(in)::p,x
   wavb(p)=iand(x,255);wavb(p+1)=iand(ishft(x,-8),255)
  end subroutine
 end subroutine
! @nativefn NR2-AUD-F0006
 subroutine send_wav(nbytes)
  ! Base64 of the WAV bytes inside a short browser expression that writes them to a temporary
  ! Blob file, plays it and releases the file when playback ends.
  integer,intent(in)::nbytes
  character(len=64),parameter::b64='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  character(len=*),parameter::head="(function(){try{var b=atob('"
  character(len=*),parameter::tail="');var a=new Uint8Array(b.length);for(var i=0;i<b.length;i++)a[i]=b.charCodeAt(i);"// &
   "var u=URL.createObjectURL(new Blob([a],{type:'audio/wav'}));var s=new Audio(u);"// &
   "s.onended=function(){URL.revokeObjectURL(u)};s.play().catch(function(){URL.revokeObjectURL(u)})}catch(e){}})()"
  integer::i,p,x,b1,b2,b3
  js(1:len(head))=head;p=len(head)
  do i=1,nbytes,3
   b1=wavb(i);b2=0;b3=0
   if(i+1<=nbytes)b2=wavb(i+1)
   if(i+2<=nbytes)b3=wavb(i+2)
   x=b1*65536+b2*256+b3
   js(p+1:p+1)=b64(ishft(x,-18)+1:ishft(x,-18)+1)
   js(p+2:p+2)=b64(iand(ishft(x,-12),63)+1:iand(ishft(x,-12),63)+1)
   if(i+1<=nbytes)then
    js(p+3:p+3)=b64(iand(ishft(x,-6),63)+1:iand(ishft(x,-6),63)+1)
   else
    js(p+3:p+3)='='
   end if
   if(i+2<=nbytes)then
    js(p+4:p+4)=b64(iand(x,63)+1:iand(x,63)+1)
   else
    js(p+4:p+4)='='
   end if
   p=p+4
  end do
  js(p+1:p+len(tail))=tail;p=p+len(tail)
  call sys_script(js(1:p))
 end subroutine
! @nativefn NR2-AUD-F0007
 subroutine speak(text,interrupt)
  ! Spoken text in the MISSY voice (see say_calm).
  character(len=*),intent(in)::text
  logical,intent(in)::interrupt
  logical::said
  said=say_calm(text,0,interrupt)
 end subroutine
end module native_audio
