! SPDX-License-Identifier: GPL-3.0-only
! Teaching animations drawn by the Fortran rasteriser: the home-page double helix and one scene
! per qPCR scenario. Four scene families:
!  instrument - cross-section of a real-time cycler (LED, excitation filter, dichroic mirror,
!               heated lid, plate, block, Peltier zones, heat sink, fan, emission filter, camera),
!               adapted from the optical/thermal layout of a plate-imaging real-time cycler;
!  molecule   - one PCR cycle with a hydrolysis probe (denature, anneal, extend, cleave), with the
!               polymerase, primers, probe reporter/quencher and dNTPs drawn as shaded spheres;
!  wells      - template copies, volumes and controls in a strip of wells / plate map;
!  data       - instrument to computer data path and the analysis view.
! Scenes are illustrations of the mechanism, not simulations; the curves next to them come
! from the full numerical model.
module native_anim
 use iso_c_binding
 use r2_framebuffer
 use r2_font
 use native_structure
 use r2_oligo, only: r2_tm_nn
 implicit none
 private
 public :: anim_helix_banner, draw_fault_scene, scene_family, draw_structure_scene, draw_cycler
 public :: mix, tcol, box, disc, glow, ball, seg, dash, arrow, frame_rect, txt, num, set_clip, example_tm
 type, public :: cycler_t
  real(c_double) :: tz(2)=25,tsens(2)=25,tlid=25,tsink=25,duty(2)=0,lidduty=0,edge=0
  real(c_double) :: led=1,glow(16)=0,liquid(16)=1,lift=0,lidopen=0,misalign=0,seal=1,autofl=0,xtalk=0
  real(c_double) :: sat=0,noise=0,dirt=0,condense=0,reading_level=0
  integer :: bubbles=0,droplets=0,nospin=0,mux=1,reading=0,fanstall=0
 end type
 real(c_double),save :: cb(4)=[0d0,0d0,1d4,1d4]
 real(c_double),parameter :: pi=3.141592653589793d0
 integer(c_int32_t),parameter :: c_bg=int(z'FF10090D',c_int32_t),c_orange=int(z'FF649BFF',c_int32_t)
 integer(c_int32_t),parameter :: c_purple=int(z'FFC99ACB',c_int32_t),c_ink=int(z'FFF3E0EA',c_int32_t)
 integer(c_int32_t),parameter :: c_panel=int(z'FF281723',c_int32_t),c_cyan=int(z'FFDCDC60',c_int32_t)
contains
! ------------------------------------------------------------------ primitives
! @nativefn NR2-ANI-F0001
 function mix(a,b,t) result(c)
  integer(c_int32_t),intent(in)::a,b
  real(c_double),intent(in)::t
  integer(c_int32_t)::c
  integer::k,v(3)
  real(c_double)::u
  u=max(0d0,min(1d0,t))
  do k=1,3
   v(k)=nint(ibits(a,8*(k-1),8)*(1-u)+ibits(b,8*(k-1),8)*u)
  end do
  c=rgba(v(1),v(2),v(3),255)
 end function
! @nativefn NR2-ANI-F0002
 function tcol(t) result(c)
  ! Temperature colour: 25 C blue, 60 C amber, 95 C red.
  real(c_double),intent(in)::t
  integer(c_int32_t)::c
  if(t<60)then
   c=mix(rgba(70,120,230,255),rgba(240,170,60,255),(t-25)/35)
  else
   c=mix(rgba(240,170,60,255),rgba(235,60,50,255),(t-60)/35)
  end if
 end function
! @nativefn NR2-ANI-F0003
 subroutine box(x,y,w,h,c)
  real(c_double),intent(in)::x,y,w,h
  integer(c_int32_t),intent(in)::c
  integer::i,j,i0,i1,j0,j1
  i0=max(nint(x*render_scale),nint(cb(1)*render_scale));i1=min(nint((x+w)*render_scale),nint(cb(3)*render_scale))-1
  j0=max(nint(y*render_scale),nint(cb(2)*render_scale));j1=min(nint((y+h)*render_scale),nint(cb(4)*render_scale))-1
  do j=j0,j1
   do i=i0,i1
    call pixel_raw(i,j,c,1d0)
   end do
  end do
 end subroutine
! @nativefn NR2-ANI-F0004
 subroutine disc(x,y,r,c,alpha)
  real(c_double),intent(in)::x,y,r,alpha
  integer(c_int32_t),intent(in)::c
  integer::i,j
  real(c_double)::d,cov
  do j=floor((max(y-r,cb(2)))*render_scale),ceiling((min(y+r,cb(4)))*render_scale)
   do i=floor((max(x-r,cb(1)))*render_scale),ceiling((min(x+r,cb(3)))*render_scale)
    d=sqrt((i+.5d0-x*render_scale)**2+(j+.5d0-y*render_scale)**2)
    cov=max(0d0,min(1d0,r*render_scale+.5d0-d))*alpha
    if(cov>0)call pixel_raw(i,j,c,cov)
   end do
  end do
 end subroutine
! @nativefn NR2-ANI-F0005
 subroutine glow(x,y,r,c,alpha)
  real(c_double),intent(in)::x,y,r,alpha
  integer(c_int32_t),intent(in)::c
  integer::i,j
  real(c_double)::d,cov
  if(alpha<=0)return
  do j=floor((max(y-r,cb(2)))*render_scale),ceiling((min(y+r,cb(4)))*render_scale)
   do i=floor((max(x-r,cb(1)))*render_scale),ceiling((min(x+r,cb(3)))*render_scale)
    d=sqrt((i+.5d0-x*render_scale)**2+(j+.5d0-y*render_scale)**2)/(r*render_scale)
    if(d>=1)cycle
    cov=alpha*(1-d)**2
    call pixel_raw(i,j,c,cov)
   end do
  end do
 end subroutine
! @nativefn NR2-ANI-F0006
 subroutine ball(x,y,r,c)
  ! Shaded sphere impostor (Lambert + specular), light from the upper left.
  real(c_double),intent(in)::x,y,r
  integer(c_int32_t),intent(in)::c
  integer::i,j,k,v(3)
  real(c_double)::dx,dy,d2,nz,diff,spec,inten,e,cov,sr
  sr=r*render_scale
  do j=floor((max(y-r,cb(2)))*render_scale),ceiling((min(y+r,cb(4)))*render_scale)
   do i=floor((max(x-r,cb(1)))*render_scale),ceiling((min(x+r,cb(3)))*render_scale)
    dx=(i+.5d0-x*render_scale)/sr;dy=(y*render_scale-(j+.5d0))/sr;d2=dx*dx+dy*dy
    if(d2>=1)cycle
    nz=sqrt(1-d2);diff=max(0d0,-.45d0*dx+.55d0*dy+.70d0*nz)
    spec=max(0d0,-.25d0*dx+.30d0*dy+.92d0*nz)**24
    inten=.32d0+.68d0*diff;e=min(1d0,spec*1.1d0)
    do k=1,3
     v(k)=nint(min(255d0,ibits(c,8*(k-1),8)*inten*(1-e)+255*e))
    end do
    cov=min(1d0,(1-sqrt(d2))*sr*1.5d0+.35d0)
    call pixel_raw(i,j,rgba(v(1),v(2),v(3),255),cov)
   end do
  end do
 end subroutine
! @nativefn NR2-ANI-F0007
 subroutine seg(x0,y0,x1,y1,c,wd)
  real(c_double),intent(in)::x0,y0,x1,y1,wd
  integer(c_int32_t),intent(in)::c
  call aa_line(x0,y0,x1,y1,c,wd,.false.,cb)
 end subroutine
! @nativefn NR2-ANI-F0008
 subroutine dash(x0,y0,x1,y1,c,wd,phase)
  ! Moving dashes along a ray (photons / data packets).
  real(c_double),intent(in)::x0,y0,x1,y1,wd,phase
  integer(c_int32_t),intent(in)::c
  real(c_double)::l,s,a,b
  integer::k,n
  l=sqrt((x1-x0)**2+(y1-y0)**2);if(l<1)return
  n=int(l/14)+1
  do k=0,n
   s=(k*14+mod(phase,14d0))/l;a=max(0d0,s);b=min(1d0,s+7/l)
   if(a>=1.or.b<=0)cycle
   call seg(x0+(x1-x0)*a,y0+(y1-y0)*a,x0+(x1-x0)*b,y0+(y1-y0)*b,c,wd)
  end do
 end subroutine
! @nativefn NR2-ANI-F0009
 subroutine arrow(x0,y0,x1,y1,c,wd)
  real(c_double),intent(in)::x0,y0,x1,y1,wd
  integer(c_int32_t),intent(in)::c
  real(c_double)::a
  call seg(x0,y0,x1,y1,c,wd);a=atan2(y1-y0,x1-x0)
  call seg(x1,y1,x1-8*cos(a-.45d0),y1-8*sin(a-.45d0),c,wd)
  call seg(x1,y1,x1-8*cos(a+.45d0),y1-8*sin(a+.45d0),c,wd)
 end subroutine
! @nativefn NR2-ANI-F0010
 subroutine frame_rect(x,y,w,h,c,wd)
  real(c_double),intent(in)::x,y,w,h,wd
  integer(c_int32_t),intent(in)::c
  call seg(x,y,x+w,y,c,wd);call seg(x+w,y,x+w,y+h,c,wd);call seg(x+w,y+h,x,y+h,c,wd);call seg(x,y+h,x,y,c,wd)
 end subroutine
! @nativefn NR2-ANI-F0011
 subroutine txt(s,x,y,c)
  character(len=*),intent(in)::s
  real(c_double),intent(in)::x,y
  integer(c_int32_t),intent(in)::c
  integer::i,g,xx,yy,advance,level,px,py
  advance=0
  do i=1,len_trim(s)
   g=glyph(iachar(s(i:i)))
   do yy=0,font_height-1
    do xx=0,font_width-1
     level=int(iand(shiftr(glyph_rows((g-1)*font_height+yy+1),2*xx),3_c_int64_t))
     if(level==0)cycle
     px=nint(x*render_scale)+advance+xx;py=nint(y*render_scale)+yy
     if(px<cb(1)*render_scale.or.px>cb(3)*render_scale.or.py<cb(2)*render_scale.or.py>cb(4)*render_scale)cycle
     call pixel_raw(px,py,c,level/3d0)
    end do
   end do
   advance=advance+glyph_advance(g)
  end do
 end subroutine
! @nativefn NR2-ANI-F0012
 function num(v,d) result(s)
  real(c_double),intent(in)::v
  integer,intent(in)::d
  character(len=16)::s
  integer::q,sc,k
  character(len=16)::a
  sc=10**d;q=nint(abs(v)*sc);write(a,'(i0)')q/sc;s=trim(a)
  if(d>0)then
   write(a,'(i0)')mod(q,sc);k=len_trim(a)
   s=trim(s)//'.'//repeat('0',d-k)//trim(a)
  end if
  if(v<0.and.q>0)s='-'//trim(s)
 end function
! @nativefn NR2-ANI-F0013
 subroutine set_clip(x0,y0,x1,y1)
  real(c_double),intent(in)::x0,y0,x1,y1
  cb=[x0,y0,x1,y1]
 end subroutine
! ------------------------------------------------------------------ home banner
! @nativefn NR2-ANI-F0020
 subroutine anim_helix_banner(x0,x1,ym,amp,t)
  ! Rotating double helix seen from the side: the strand in front is drawn last, wider and
  ! brighter; base-pair rungs fade with depth. t in seconds.
  real(c_double),intent(in)::x0,x1,ym,amp,t
  integer::i,n,pass
  real(c_double)::xa,xb,th0,th1,ya0,ya1,yb0,yb1,za,zb,ph
  integer(c_int32_t)::ca,cbb,cr
  cb=[x0-5,ym-amp-12,x1+5,ym+amp+12];n=int((x1-x0)/6);ph=t*1.6d0
  do pass=0,1
   do i=0,n-1
    xa=x0+i*6d0;xb=xa+6
    th0=(xa-x0)*.0311d0+ph;th1=(xb-x0)*.0311d0+ph
    ya0=ym+amp*sin(th0);ya1=ym+amp*sin(th1);yb0=ym-amp*sin(th0);yb1=ym-amp*sin(th1)
    za=cos(th0);zb=-za
    if(pass==0.and.mod(i,4)==0)then
     cr=mix(c_bg,c_cyan,.35d0+.45d0*abs(sin(th0)))
     call seg(xa,ya0,xa,yb0,cr,1d0)
    end if
    ca=mix(c_bg,c_purple,.45d0+.55d0*(za+1)/2);cbb=mix(c_bg,c_orange,.45d0+.55d0*(zb+1)/2)
    if((pass==0.and.za<0).or.(pass==1.and.za>=0))call seg(xa,ya0,xb,ya1,ca,2d0+1.5d0*max(0d0,za))
    if((pass==0.and.zb<0).or.(pass==1.and.zb>=0))call seg(xa,yb0,xb,yb1,cbb,2d0+1.5d0*max(0d0,zb))
   end do
  end do
  cb=[0d0,0d0,1d4,1d4]
 end subroutine
! ------------------------------------------------------------------ scene dispatch
! @nativefn NR2-ANI-F0030
 integer function scene_family(code) result(f)
  ! 1 instrument, 2 molecule, 3 wells, 4 data.
  integer,intent(in)::code
  select case(code)
  case(1:14,28:30,39,41,45,46)
   f=1
  case(23:27,31:35,48)
   f=2
  case(15,36:38,40,42:44,49:51)
   f=3
  case default
   f=4
  end select
 end function
! @nativefn NR2-ANI-F0031
 subroutine draw_fault_scene(code,p,x,y,w,h,t,fault_on)
  ! code 0 draws the normal instrument. fault_on=.false. shows the same scene without the fault.
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  logical,intent(in)::fault_on
  integer::c,f
  c=code;if(.not.fault_on)c=0
  f=scene_family(code);if(code==0)f=1
  cb=[x,y,x+w,y+h];call box(x,y,w,h,rgba(22,14,20,255))
  select case(f)
  case(1)
   call scene_instrument(c,p,x,y,w,h,t)
  case(2)
   call scene_molecule(c,p,x,y,w,h,t)
  case(3)
   call scene_wells(c,p,x,y,w,h,t)
  case default
   call scene_data(c,p,x,y,w,h,t)
  end select
  cb=[x,y,x+w,y+h];call frame_rect(x+.5d0,y+.5d0,w-1,h-1,c_purple,1d0)
  cb=[0d0,0d0,1d4,1d4]
 end subroutine
! ------------------------------------------------------------------ instrument
! @nativefn NR2-ANI-F0040
 subroutine scene_instrument(code,p,x,y,w,h,t)
  ! Fault scenes on the generic 16-tube cycler. One 6 s loop: 0-2 s denaturation at 95 C,
  ! 2-6 s annealing at 60 C with the per-tube LED read (photodiodes sample LED on / LED off).
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  type(cycler_t)::s
  real(c_double)::ph,tb,cyc,g
  integer::i
  ph=mod(t,6d0);cyc=floor(t/6d0)
  if(ph<2)then
   tb=60+35*min(1d0,ph/.6d0)
  else
   tb=95-35*min(1d0,(ph-2)/.8d0)
  end if
  s%tz=tb;s%tsens=tb;s%tlid=105;s%tsink=32;s%duty=0;s%lidduty=.3d0
  if(ph<2)then
   s%duty=.8d0
  else if(ph<2.8d0)then
   s%duty=-.8d0
  else
   s%duty=.15d0
  end if
  s%reading=0;if(ph>3.2d0.and.ph<5.6d0)s%reading=1
  s%mux=1+mod(int((ph-3.2d0)/2.4d0*16*3),16)
  g=min(1d0,.12d0+mod(cyc,8d0)/8)
  s%glow=g;s%reading_level=g
  select case(code)
  case(1)
   s%edge=p(1)
  case(2)
   s%duty(1)=s%duty(1)*p(1);s%tz(1)=tb-(1-p(1))*abs(tb-60)*.8d0
  case(3)
   s%tlid=p(1);s%condense=1
  case(4)
   s%misalign=1-p(1)
  case(5,17)
   s%xtalk=p(1)
  case(6)
   s%led=p(1)
  case(7)
   s%led=p(2)+(p(1)-p(2))*mod(t/30d0,1d0)
  case(8)
   s%sat=1
  case(9)
   s%noise=p(1)
  case(10)
   s%dirt=1-p(1)
  case(11,28)
   s%lift=6
  case(12)
   s%tsens=tb-min(p(2),p(1)*mod(t,24d0))
  case(13)
   s%fanstall=1;s%tsink=60+10*sin(t)
  case(14)
   s%lidopen=min(1d0,max(0d0,(mod(t,12d0)-4)/1.5d0))
  case(29)
   s%seal=p(1)
  case(30)
   s%autofl=1
  case(39)
   s%liquid(1)=max(.35d0,1-p(1)*8*mod(t/18d0,1d0));s%liquid(8)=s%liquid(1)
  case(41)
   s%bubbles=max(1,nint(p(1)))
  case(45)
   s%droplets=1
  case(46)
   s%nospin=1
  end select
  if(code==10)then
   do i=4,5
    s%glow(i)=s%glow(i)*p(1)
   end do
  end if
  call draw_cycler(s,x,y,w,h,t)
  call txt(trim(fault_caption(code,p)),x+8,y+6,c_orange)
  call txt(phase_text(ph),x+8,y+h-20,c_cyan)
 end subroutine
! @nativefn NR2-ANI-F0042
 function fault_caption(code,p) result(s)
  integer,intent(in)::code
  real(c_double),intent(in)::p(6)
  character(len=96)::s
  select case(code)
  case(0)
   s='Generic 16-tube cycler: one LED and photodiode channel per tube'
  case(1)
   s='end tubes run '//trim(num(p(1),1))//' C colder than the centre of the block'
  case(2)
   s='Peltier zone 1 pumps '//trim(num(100*p(1),0))//'% of nominal: slow ramps on the left half'
  case(3)
   s='lid at '//trim(num(p(1),0))//' C: condensation forms under the caps'
  case(4)
   s='lid height wrong: LED spots miss the tube centres, far tubes read low'
  case(5,17)
   s='emission filter leaks: FAM light reaches the second channel'
  case(6)
   s='LED output '//trim(num(100*p(1),0))//'%: lower counts in every tube'
  case(7)
   s='LED output drifts during the run'
  case(8)
   s='detector gain too high: readings clip at ADC full scale'
  case(9)
   s='detector noise x'//trim(num(p(1),0))
  case(10)
   s='spill on the excitation filter: tubes 4-5 dim and noisy'
  case(11,28)
   s='tubes not seated: air gap above the block, slow heat transfer'
  case(12)
   s='thermistor drifts: the controller follows a wrong reading'
  case(13)
   s='fan stalled: heat sink overheats, interlock stops the run'
  case(14)
   s='lid opened during the run: heating stops, interlock'
  case(29)
   s='caps not optically clear: most light is blocked'
  case(30)
   s='tube plastic autofluoresces: raised baseline'
  case(39)
   s='end tubes evaporate through a poor seal'
  case(41)
   s='bubbles in the tubes scatter the light'
  case(45)
   s='a droplet on the tube wall joins the reaction late'
  case(46)
   s='liquid left on the walls: tubes not spun down'
  case default
   s=''
  end select
 end function
! @nativefn NR2-ANI-F0043
 subroutine draw_cycler(s,x,y,w,h,t)
  ! Generic layout (not any specific product): long view of one 8-tube strip with the lid
  ! stack (LED board, excitation filter, heated lid), sample block with two thermistors, two
  ! Peltier zones, copper spreader, finned heat sink and fan; section A-A through a tube pair
  ! showing the optical path to the side photodiode boards; detector timing strip.
  type(cycler_t),intent(in)::s
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::lw,x0,pw,wx,yled,yfil,ylid,ycap,ytube,yblk,ypel,ycu,yfin,yfe,cx,tw,ly,liq,lo,ang,sx0,sw,bx,by,v,dx
  real(c_double)::lidlift,detx,dety,detw,deth,ux,vv
  integer::i,k,n
  integer(c_int32_t)::c,ledc,emc
  lw=w*.64d0;x0=x+84;pw=lw-112;wx=pw/8
  lidlift=s%lidopen*46
  yled=y+30-lidlift;yfil=yled+10;ylid=yfil+8;ycap=y+68;ytube=ycap+7
  yblk=y+86;ypel=yblk+48;ycu=ypel+13;yfin=ycu+5;yfe=yfin+34
  ! ---- heat sink, copper spreader, Peltier zones, block
  call box(x0-8,ycu,pw+16,5d0,rgba(205,120,70,255))
  call txt('Cu',x0-30,ycu-7,c_ink)
  n=int((pw+16)/8)
  do i=0,n
   c=rgba(150,150,165,255)
   if(s%tsink>45)c=mix(c,rgba(230,70,50,255),min(1d0,(s%tsink-45)/20))
   call box(x0-8+i*8d0,yfin,4d0,yfe-yfin,c)
  end do
  call txt('heat sink',x0-76,yfin+8,c_ink)
  ang=t*14;if(s%fanstall>0)ang=0
  call disc(x+lw-14,yfin+17,15d0,rgba(55,48,58,255),1d0)
  do k=0,2
   call seg(x+lw-14,yfin+17,x+lw-14+13*cos(ang+k*2*pi/3),yfin+17+13*sin(ang+k*2*pi/3),c_ink,3d0)
  end do
  if(s%fanstall==0)then
   do k=0,2
    call dash(x+lw-34,yfin+6+k*11d0,x0,yfin+6+k*11d0,rgba(120,170,230,255),1d0,t*60)
   end do
  end if
  call txt('fan',x+lw-24,yfe+2,c_ink)
  do k=1,2
   v=s%duty(k)
   c=mix(rgba(60,60,72,255),rgba(235,90,60,255),max(0d0,v))
   if(v<0)c=mix(rgba(60,60,72,255),rgba(80,150,255,255),-v)
   call box(x0+(k-1)*pw/2+2,ypel,pw/2-4,11d0,c)
   call txt('Peltier '//achar(48+k)//'   T'//achar(48+k)//' '//trim(num(s%tsens(k),1))//' C',x0+(k-1)*pw/2+8,ypel-1,c_bg)
   if(abs(v)>.05d0)then
    if(v>0)then
     call arrow(x0+(k-.5d0)*pw/2,ypel+6,x0+(k-.5d0)*pw/2,ypel-8-10*v,rgba(255,120,80,255),1.5d0)
    else
     call arrow(x0+(k-.5d0)*pw/2,ypel-4,x0+(k-.5d0)*pw/2,ypel+14-8*v,rgba(110,170,255,255),1.5d0)
    end if
   end if
  end do
  do i=0,7
   tw=s%tz(1+i/4)
   if(i==0.or.i==7)tw=tw-s%edge
   call box(x0+i*wx,yblk,wx+.5d0,46d0,tcol(tw))
  end do
  call txt('block',x0-50,yblk+18,c_ink)
  ! two thermistors, one per Peltier zone, between tubes 2-3 and 6-7
  do k=1,2
   bx=x0+(4*k-2)*wx;by=yblk+34
   call disc(bx,by,3.5d0,c_ink,1d0)
  end do
  ! ---- tubes: cap, conical body, liquid with fluorescence
  do i=0,7
   cx=x0+i*wx+wx/2
   liq=s%liquid(i+1)
   c=rgba(215,215,228,255);if(s%seal<1)c=mix(rgba(120,120,125,255),c,s%seal)
   call box(cx-wx*.34d0,ycap-s%lift,wx*.68d0,6d0,c)
   call seg(cx-wx*.3d0,ytube-s%lift,cx-wx*.12d0,yblk+40-s%lift,rgba(200,200,215,255),1.5d0)
   call seg(cx+wx*.3d0,ytube-s%lift,cx+wx*.12d0,yblk+40-s%lift,rgba(200,200,215,255),1.5d0)
   call seg(cx-wx*.12d0,yblk+40-s%lift,cx+wx*.12d0,yblk+40-s%lift,rgba(200,200,215,255),1.5d0)
   v=s%glow(i+1)*s%led*s%seal*(1-s%misalign*i/7d0)*(.35d0+.65d0*s%reading)
   emc=mix(rgba(40,90,60,255),rgba(110,255,140,255),v)
   call box(cx-wx*.16d0,yblk+38-s%lift-24*liq,wx*.32d0,24*liq,emc)
   if(v>0)call glow(cx,yblk+28-s%lift,wx*.45d0,rgba(110,255,140,255),.5d0*v)
   if(s%autofl>0)call glow(cx,ytube+6-s%lift,wx*.4d0,rgba(255,240,140,255),.3d0)
   if(s%condense>0)call disc(cx+2*sin(t+i),ycap+8+mod(t*6+i*5,10d0),2.2d0,rgba(150,210,255,255),.9d0)
   if(s%bubbles>0.and.(i==2.or.i==5))then
    do k=1,s%bubbles
     call disc(cx-3+5*(k-1),yblk+34-s%lift-mod(t*12+k*9,22d0),3d0,c_ink,.8d0)
    end do
   end if
   if(s%droplets>0.and.(i==1.or.i==6))call disc(cx+wx*.22d0,ytube+4+min(28d0,max(0d0,(mod(t,12d0)-6)*7)),2.6d0, &
    rgba(110,200,255,255),1d0)
   if(s%nospin>0)then
    call disc(cx-wx*.2d0,ytube+6,2.6d0,rgba(110,200,255,255),1d0);call disc(cx+wx*.18d0,ytube+10,2.2d0,rgba(110,200,255,255),1d0)
   end if
  end do
  call txt('tubes',x0-50,ytube-4-s%lift,c_ink)
  ! ---- lid stack: heated lid, excitation filter, LED board (lifts when opened)
  call box(x0-6,ylid,pw+12,12d0,tcol(s%tlid))
  call txt('lid',x0-50,ylid-3,c_ink)
  c=rgba(90,140,255,255);if(s%dirt>0)c=mix(c,rgba(120,90,60,255),.5d0)
  call box(x0-6,yfil,pw+12,4d0,c);call txt('Ex filter',x0-76,yfil-7,c_ink)
  if(s%dirt>0)call disc(x0+4*wx,yfil+2,wx*.9d0,rgba(120,90,60,255),.6d0)
  call box(x0-6,yled-4,pw+12,6d0,rgba(40,120,70,255));call txt('LEDs',x0-50,yled-8,c_ink)
  do i=0,7
   cx=x0+i*wx+wx/2+s%misalign*wx*.35d0
   ledc=mix(rgba(50,60,90,255),rgba(140,200,255,255),s%led*s%reading)
   call ball(cx,yled+5,4d0,ledc)
   if(s%reading>0.and.s%lidopen<=0)then
    call glow(cx,yled+5,10d0,rgba(140,200,255,255),.6d0*s%led)
    call dash(cx,yled+10,cx,ycap-s%lift,rgba(120,180,255,255),1.3d0*s%led,t*70)
   end if
  end do
  call box(x0+pw/2-64,ylid+1,128d0,10d0,tcol(s%tlid))
  call txt('heated lid '//trim(num(s%tlid,0))//' C',x0+pw/2-56,ylid-1,c_bg)
  ! ---- section A-A: tube pair with side photodiode boards
  sx0=x+lw+14;sw=w-lw-22
  call box(sx0+18,yblk,sw-36,46d0,tcol(s%tz(1)))
  call box(sx0+18,ylid,sw-36,12d0,tcol(s%tlid))
  call box(sx0+18,yfil,sw-36,4d0,rgba(90,140,255,255))
  call box(sx0+18,yled-4,sw-36,6d0,rgba(40,120,70,255))
  do k=0,1
   cx=sx0+sw*(.36d0+.28d0*k)
   call box(cx-10,ycap-s%lift,20d0,6d0,rgba(215,215,228,255))
   call seg(cx-9,ytube-s%lift,cx-4,yblk+40-s%lift,rgba(200,200,215,255),1.5d0)
   call seg(cx+9,ytube-s%lift,cx+4,yblk+40-s%lift,rgba(200,200,215,255),1.5d0)
   v=s%reading_level*s%led*s%seal*(.35d0+.65d0*s%reading)
   call box(cx-5,yblk+16-s%lift,10d0,22d0,mix(rgba(40,90,60,255),rgba(110,255,140,255),v))
   call ball(cx+s%misalign*8,yled+5,4d0,mix(rgba(50,60,90,255),rgba(140,200,255,255),s%led*s%reading))
   if(s%reading>0.and.s%lidopen<=0)then
    call dash(cx+s%misalign*8,yled+10,cx+s%misalign*8,yblk+20,rgba(120,180,255,255),1.3d0,t*70)
    dx=-1;if(k==1)dx=1
    call dash(cx,yblk+26,cx+dx*(sw*.36d0-8),yblk+26,rgba(110,255,140,255),1.3d0*v+.2d0,t*70)
    if(s%xtalk>0)call dash(cx,yblk+32,cx+dx*(sw*.36d0-8),yblk+32,rgba(255,220,70,255),1.3d0*s%xtalk*2,t*70)
   end if
  end do
  do k=0,1
   bx=sx0+4+k*(sw-12)
   call box(bx,yblk-8,6d0,58d0,rgba(40,120,70,255))
   c=rgba(110,255,140,255);if(s%xtalk>0)c=mix(c,rgba(255,220,70,255),.5d0)
   call box(bx+6-k*9,yblk+8,3d0,30d0,c)
  end do
  call txt('section A-A',sx0+22,yblk+28,c_bg)
  call txt('photodiode',sx0,yblk+56,c_ink);call txt('boards + Em filters',sx0,yblk+72,c_ink)
  ! ---- detector strip: LED on / LED off samples of the tube being read
  detx=sx0;dety=y+h-92;detw=sw;deth=58
  call box(detx,dety,detw,deth,rgba(14,10,16,255));call frame_rect(detx,dety,detw,deth,c_purple,1d0)
  call txt('photodiode ADC  tube '//trim(num(real(s%mux,c_double),0)),detx+4,dety+2,c_cyan)
  ly=dety+deth-8
  do i=0,63
   ux=detx+4+i*(detw-8)/64d0
   vv=0
   if(s%reading>0.and.mod(i/8,2)==0)vv=s%reading_level*s%led*s%seal*.8d0+.08d0
   if(s%noise>0)vv=vv+.05d0*s%noise*sin(i*12.9898d0+t*41)
   lo=min(.92d0,max(0d0,vv));if(s%sat>0.and.vv>0)lo=.92d0
   call seg(ux,ly-lo*(deth-22),ux+(detw-8)/64d0,ly-lo*(deth-22),rgba(110,255,140,255),1.5d0)
  end do
  if(s%sat>0)call seg(detx+4,ly-.92d0*(deth-22),detx+detw-4,ly-.92d0*(deth-22),rgba(255,80,60,255),1d0)
  call txt('LED on / off',detx+4,dety+deth+2,c_ink)
 end subroutine
! @nativefn NR2-ANI-F0041
 function phase_text(ph) result(s)
  real(c_double),intent(in)::ph
  character(len=40)::s
  if(ph<2)then
   s='Denaturation 95 C'
  else if(ph<3.2d0)then
   s='Cooling to annealing 60 C'
  else if(ph<5.6d0)then
   s='Annealing 60 C - LED read'
  else
   s='Next cycle'
  end if
 end function
! ------------------------------------------------------------------ molecule
! @nativefn NR2-ANI-F0050
 subroutine scene_molecule(code,p,x,y,w,h,t)
  ! One cycle in 8 s: 0-1.5 denature, 1.5-3.5 primers and probe anneal, 3.5-7 extension with
  ! probe hydrolysis (reporter released and fluorescent), 7-8 next cycle.
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  real(c_double)::ph,sep,ytop,ybot,x0,x1,ext,pp,ann,fade,speed,xr,yr,bob,xq,stall,dnt,glowr
  integer::i,n,np,k
  integer(c_int32_t)::cpol,crep,cpr
  logical::probe_binds,primer_binds
  ph=mod(t,8d0);x0=x+40;x1=x+w-40;n=int((x1-x0)/9)
  sep=0;if(ph<1.5d0)sep=ph/1.5d0
  if(ph>=1.5d0)sep=1
  if(ph>7)sep=1-(ph-7)
  ytop=y+h*.38d0-28*sep;ybot=y+h*.38d0+10+28*sep
  ann=0;if(ph>1.5d0)ann=min(1d0,(ph-1.5d0)/1.5d0)
  if(ph>7)ann=0
  speed=1;stall=1d9
  select case(code)
  case(23)
   speed=p(2);stall=.6d0
  case(27)
   speed=p(2)*.8d0;stall=.55d0
  case(26)
   stall=p(1)
  end select
  ext=0;if(ph>3.5d0)ext=min(1d0,(ph-3.5d0)/3.2d0*speed)
  if(ph>7)ext=0
  ext=min(ext,stall)
  probe_binds=.true.;primer_binds=.true.
  if(code==34)primer_binds=.false.
  if(code==34)probe_binds=.false.
  if(code==33)probe_binds=mod(t,2d0)<1
  ! template strands (backbone spheres + base-pair rungs while hybridised)
  do i=0,n
   if(sep<.2d0.and.mod(i,2)==0)call seg(x0+i*9d0,ytop+4,x0+i*9d0,ybot-4,mix(c_bg,c_cyan,.6d0),1d0)
  end do
  do i=0,n
   call ball(x0+i*9d0,ytop+2*sin(i*.6d0+t),3.6d0,rgba(200,150,210,255))
   call ball(x0+i*9d0,ybot+2*sin(i*.6d0+t+1),3.6d0,rgba(90,200,200,255))
  end do
  call txt('template',x0,ytop-26,c_ink)
  ! forward primer on the lower strand (left) with polymerase extending to the right
  np=6;pp=ann
  cpr=c_orange
  if(code==31)cpr=mix(c_orange,c_bg,1-p(1))
  if(primer_binds)then
   do i=0,np-1
    call ball(x0+i*9d0,ybot-16*pp-(1-pp)*40,3.4d0,cpr)
   end do
   k=nint(ext*(n-np))
   do i=np,np+k-1
    call ball(x0+i*9d0,ybot-16,3.4d0,rgba(255,200,120,255))
   end do
  else
   bob=mod(t*1.5d0,2d0);if(bob>1)bob=2-bob
   do i=0,np-1
    call ball(x0+30+i*9d0,ybot-22-30*bob,3.4d0,cpr)
   end do
   call txt('primer cannot hold at '//trim(num(p(1),0))//' C',x0+90,ybot-50,c_orange)
  end if
  if(code==35)then
   do i=0,np-1
    call ball(x0+(n-14+i)*9d0,ytop+16*ann,3.4d0,c_orange)
   end do
   call txt('off-target binding at '//trim(num(p(1),0))//' C',x0+(n-30)*9d0,ytop-26,c_orange)
  end if
  ! polymerase
  cpol=rgba(80,140,255,255)
  if(code==23)cpol=rgba(120,120,130,255)
  if(primer_binds.and.ph>3.2d0.and.ph<7)then
   xr=x0+(np+ext*(n-np))*9d0
   call ball(xr,ybot-26,14d0,cpol);call txt('Pol',xr-10,ybot-32,c_bg)
   if(code==27)then
    do k=0,2
     call ball(xr-14+k*14,ybot-46,5d0,rgba(140,100,60,255))
    end do
    call txt('inhibitor',xr+18,ybot-58,c_orange)
   end if
   if(code==23)call txt('inactive enzyme',xr+18,ybot-58,c_orange)
  end if
  ! hydrolysis probe on the lower strand: reporter (R) and quencher (Q)
  xr=x0+(n*.62d0)*9d0;xq=xr+8*9d0
  crep=rgba(110,255,140,255);if(code==24)crep=rgba(120,120,120,255)
  fade=ann;if(.not.probe_binds)fade=0
  if(primer_binds.and.ext*(n-np)+np>=n*.62d0.and.code/=34)then
   ! cleaved: reporter drifts free and glows
   yr=ybot-40-20*min(1d0,(ph-3.5d0-(n*.62d0-np)/(n-np)*3.2d0/speed))
   glowr=1;if(code==24)glowr=0
   call glow(xr-10,yr,26d0,rgba(110,255,140,255),.7d0*glowr)
   call ball(xr-10,yr,6d0,crep);call ball(xq+12,ybot-34,6d0,rgba(60,50,70,255))
   call txt('R free: fluorescence',xr-60,yr-24,c_cyan)
  else
   do i=0,8
    call ball(xr+i*9d0,ybot-16*fade-(1-fade)*48,3.2d0,rgba(255,240,150,255))
   end do
   call ball(xr,ybot-16*fade-(1-fade)*48-8,6d0,crep);call ball(xq,ybot-16*fade-(1-fade)*48-8,6d0,rgba(60,50,70,255))
   call txt('R',xr-4,ybot-16*fade-(1-fade)*48-30,c_ink);call txt('Q',xq-4,ybot-16*fade-(1-fade)*48-30,c_ink)
   if(code==33)then
    call disc(xr+4*9d0,ybot-16*fade-(1-fade)*48,4d0,rgba(255,70,70,255),1d0)
    call txt('mismatch',xr+20,ybot-70,c_orange)
   end if
  end if
  ! free probes in solution: degraded probes (case 25) leave unquenched reporters glowing
  do k=1,6
   xr=x0+mod(k*97+t*9,x1-x0);yr=y+h*.78d0+12*sin(t+k)
   if(code==25)then
    call glow(xr,yr,16d0,rgba(110,255,140,255),.6d0);call ball(xr,yr,5d0,rgba(110,255,140,255))
    call ball(xr+40,yr+8,5d0,rgba(60,50,70,255))
   else
    call ball(xr,yr,5d0,crep);call ball(xr+12,yr,5d0,rgba(60,50,70,255))
    call seg(xr,yr,xr+12,yr,rgba(255,240,150,255),2d0)
   end if
  end do
  if(code==25)call txt('quencher separated: reporters glow without amplification',x0,y+h-42,c_orange)
  if(code==24)then
   do k=1,3
    call arrow(x0+100*k,y+8,x0+100*k-10,y+28,rgba(255,240,120,255),2d0)
   end do
   call txt('photobleached reporter',x0+330,y+12,c_orange)
  end if
  ! dNTP pool
  dnt=1;if(code==26)dnt=max(0d0,1-mod(t/40d0,1d0))
  do k=1,nint(18*dnt)
   call ball(x0+mod(k*53+t*20,x1-x0),y+h*.66d0+8*sin(k+t*2),2.2d0,rgba(255,170,90,255))
  end do
  call txt('dNTP',x1-40,y+h*.66d0-18,c_ink)
  if(code==26)call txt('dNTPs hydrolysed: extension stops',x0+190,y+h*.66d0-18,c_orange)
  if(code==32)then
   do k=0,1
    do i=0,5
     call ball(x0+40+k*200+i*8d0,y+h*.88d0,3d0,c_orange)
     call ball(x0+80+k*200-i*8d0,y+h*.88d0+8,3d0,c_orange)
    end do
   end do
   call txt('primer-dimers formed on the warm bench',x0+20,y+h*.88d0-24,c_orange)
  end if
  if(code==31)call txt('new lot: fewer primers / probes',x0+200,ytop-26,c_orange)
  if(code==34.or.code==35)call txt('example primer: nearest-neighbour Tm '//trim(num(example_tm(),1))// &
   ' C; annealing set to '//trim(num(p(1),0))//' C',x0,y+h-42,c_ink)
  if(code==33)call txt('probe Tm 70 C in the model; mismatch lowers it to '//trim(num(70-p(1),0))// &
   ' C, below the 60 C read: the probe falls off',x0,y+h-42,c_ink)
  if(code==48)then
   call txt('target assay '//trim(num(100*p(1),0))//'% of reference efficiency: fewer copies per cycle',x0,y+h-40,c_orange)
  end if
  call txt(mol_phase(ph),x+8,y+h-20,c_cyan)
 end subroutine
! @nativefn NR2-ANI-F0052
 real(c_double) function example_tm() result(v)
  ! SantaLucia (1998) unified nearest-neighbour Tm of an illustrative 20-mer, computed by r2_tm_nn.
  character(len=20),parameter::seq='ACGTTGCAGGTCAGTCAAGC'
  integer(c_int8_t)::b(20)
  integer::i
  do i=1,20
   b(i)=int(iachar(seq(i:i)),c_int8_t)
  end do
  v=r2_tm_nn(b,20_c_int,.05d0,.003d0,300d-9)      ! molar: 50 mM Na+, 3 mM Mg2+, 300 nM primer
 end function
! @nativefn NR2-ANI-F0051
 function mol_phase(ph) result(s)
  real(c_double),intent(in)::ph
  character(len=48)::s
  if(ph<1.5d0)then
   s='Denaturation: strands separate'
  else if(ph<3.5d0)then
   s='Annealing: primers and probe bind'
  else if(ph<7)then
   s='Extension: polymerase cleaves the probe'
  else
   s='Next cycle'
  end if
 end function
! ------------------------------------------------------------------ wells / plate
! @nativefn NR2-ANI-F0060
 subroutine scene_wells(code,p,x,y,w,h,t)
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  character(len=6),parameter::names(7)=[character(len=6)::'S1','S2','S3','PC','NTC','Blank','LOD']
  real(c_double)::wx,cx,yb,lev,copies(7),vol(7),a,s,dx,dy,rot,col,gl
  integer::i,k,m,r,c
  integer(c_int32_t)::dye
  copies=[30d0,30d0,30d0,20d0,0d0,0d0,3d0];vol=1
  select case(code)
  case(15)
   copies(1:3)=60
  case(40,49)
   copies(1:3)=max(1d0,30/p(1)*3)
  case(42)
   vol(1:3)=[1-p(1)/100,1d0,1+p(1)/100]
  case(44)
   copies(1:3)=[30d0,30*(1+p(1))/2,30*p(1)]
  case(43)
   copies(5)=min(8d0,mod(t,10d0))
  case(50)
   copies(6)=min(8d0,mod(t,10d0))
  case(38)
   copies(5:6)=-1
  end select
  if(code==37)then
   ! plate map rotating 180 degrees: A1 ends at the bottom right
   rot=min(pi*p(1)/180,max(0d0,mod(t,10d0)-3)*pi/2)
   do r=0,3
    do c=0,7
     dx=(c-3.5d0)*24;dy=(r-1.5d0)*24
     a=x+w/2+dx*cos(rot)-dy*sin(rot);s=y+h/2-10+dx*sin(rot)+dy*cos(rot)
     col=real(c,c_double)/7
     call disc(a,s,9d0,mix(rgba(110,255,140,255),rgba(255,140,90,255),col),1d0)
     if(r==0.and.c==0)call txt('A1',a-8,s-8,c_bg)
    end do
   end do
   call txt('plate loaded rotated: wells map to the wrong samples',x+12,y+12,c_orange)
   return
  end if
  wx=(w-60)/7;yb=y+h-70
  do i=1,7
   cx=x+30+(i-.5d0)*wx
   lev=46*vol(i)
   call seg(cx-16,yb-60,cx-10,yb,c_ink,1.5d0);call seg(cx+16,yb-60,cx+10,yb,c_ink,1.5d0);call seg(cx-10,yb,cx+10,yb,c_ink,1.5d0)
   if(copies(i)<0)then
    call seg(cx-14,yb-50,cx+14,yb-4,c_orange,3d0);call seg(cx+14,yb-50,cx-14,yb-4,c_orange,3d0)
   else
    dye=rgba(70,110,170,255)
    if(code==44.and.i<=3)dye=mix(rgba(30,40,60,255),rgba(70,110,170,255),copies(i)/30)
    call box(cx-11-2*vol(i),yb-lev,22+4*vol(i),lev,dye)
    m=nint(copies(i));gl=min(1d0,m/30d0)
    do k=1,m
     call disc(cx-9+18*mod(k*.618034d0,1d0),yb-lev+4+(lev-8)*mod(k*.414214d0+t*.05d0,1d0),1.8d0,rgba(110,255,140,255),1d0)
    end do
   end if
   call txt(names(i),cx-14,yb+6,c_ink)
  end do
  if(code==43)then
   do k=1,5
    a=mod(t*.5d0+k*.2d0,1d0)
    call disc(x+30+3.5d0*wx+a*wx,yb-70-40*sin(a*pi),2.5d0,rgba(110,255,140,255),1d0)
   end do
   call txt('aerosol: amplicon carried into the NTC',x+12,y+12,c_orange)
  end if
  if(code==50)call txt('extraction blank contaminated',x+12,y+12,c_orange)
  if(code==38)call txt('NTC and extraction blank omitted',x+12,y+12,c_orange)
  if(code==15)call txt('very high template: amplification inside the baseline window',x+12,y+12,c_orange)
  if(code==40.or.code==49)call txt('template '//trim(num(p(1),0))//' x lower: later Cq',x+12,y+12,c_orange)
  if(code==42)then
   call txt('pipetting CV '//trim(num(p(1),0))//'%: unequal volumes',x+12,y+12,c_orange)
   a=x+30+.5d0*wx+mod(t,3d0)/3*2*wx
   call seg(a,y+30,a,yb-80,c_ink,4d0);call box(a-5,y+24,10d0,8d0,c_purple)
  end if
  if(code==44)call txt('master mix not homogenised: gradient across columns',x+12,y+12,c_orange)
  if(code==51)then
   do i=1,3
    cx=x+30+(i-.5d0)*wx
    call txt('ref '//trim(num(p(1)*sin(t+i*2.1d0),1)),cx-20,yb-80,c_cyan)
   end do
   call txt('reference gene varies by extract',x+12,y+12,c_orange)
  end if
  if(code==36)then
   call txt('probe dye read on the wrong channel',x+12,y+12,c_orange)
   call box(x+w-170,y+40,60d0,18d0,rgba(110,255,140,255));call txt('FAM',x+w-160,y+41,c_bg)
   call box(x+w-90,y+40,60d0,18d0,rgba(255,220,70,255));call txt('YY',x+w-80,y+41,c_bg)
   a=mod(t,3d0)/3
   call arrow(x+w-140,y+64,x+w-140+80*a,y+64,c_orange,2d0)
  end if
 end subroutine
! ------------------------------------------------------------------ data path / analysis
! @nativefn NR2-ANI-F0070
 subroutine scene_data(code,p,x,y,w,h,t)
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  real(c_double)::ix,iy,pcx,pcy,gx,gy,gw,gh,a,v,v0,thr,k
  integer::i,cyc,lost0,lost1
  integer(c_int32_t)::c
  ix=x+20;iy=y+40;pcx=x+w-250;pcy=y+30
  call box(ix,iy,120d0,80d0,rgba(60,50,60,255));call txt('cycler',ix+30,iy+30,c_ink)
  call box(pcx,pcy,230d0,150d0,rgba(40,40,55,255));call box(pcx+8,pcy+8,214d0,118d0,rgba(15,15,22,255))
  call txt('analysis software',pcx+50,pcy+130,c_ink)
  cyc=int(mod(t*3,45d0))+1;lost0=nint(p(1));lost1=lost0+nint(p(2))-1
  ! data packets on the cable
  call seg(ix+120,iy+40,pcx,iy+40,c_purple,2d0)
  do i=0,4
   a=mod(t*.8d0+i*.2d0,1d0)
   c=c_cyan
   if(code==19.and.cyc>=lost0.and.cyc<=lost1)c=rgba(255,70,70,255)
   call box(ix+120+a*(pcx-ix-120)-5,iy+35,10d0,10d0,c)
  end do
  if(code==19.and.cyc>=lost0.and.cyc<=lost1)call txt('packets lost: cycles missing',ix+130,iy+56,c_orange)
  ! curve on the screen
  gx=pcx+14;gy=pcy+14;gw=200;gh=104
  thr=gy+gh-22
  if(code==16)thr=gy+gh-90*p(1)/2000-10
  do i=1,cyc-1
   v0=sig(i);v=sig(i+1)
   if(code==19.and.((i>=lost0.and.i<=lost1).or.(i+1>=lost0.and.i+1<=lost1)))cycle
   if(code==18)then
    v0=v0*(1+.5d0*sin(i*1.7d0));v=v*(1+.5d0*sin((i+1)*1.7d0))
   end if
   call seg(gx+(i-1)*gw/44,gy+gh-v0*gh*.85d0,gx+i*gw/44,gy+gh-v*gh*.85d0,rgba(110,255,140,255),1.5d0)
   if(code==47)then
    call seg(gx+(i-1)*gw/44,gy+gh-min(.9d0,max(0d0,(i-p(2))*p(1)/400))*gh,gx+i*gw/44, &
     gy+gh-min(.9d0,max(0d0,(i+1-p(2))*p(1)/400))*gh,c_orange,1.5d0)
   end if
   if(code==17)then
    call seg(gx+(i-1)*gw/44,gy+gh-v0*gh*.85d0*p(1),gx+i*gw/44,gy+gh-v*gh*.85d0*p(1),rgba(255,220,70,255),1.2d0)
   end if
  end do
  call seg(gx,thr,gx+gw,thr,c_orange,1d0)
  if(code==16)call txt('threshold '//trim(num(p(1),0)),gx+4,thr-16,c_orange)
  if(code==18)call txt('divided by an absent ROX signal',x+12,y+h-44,c_orange)
  if(code==17)call txt('no colour compensation: FAM appears in YY',x+12,y+h-44,c_orange)
  if(code==47)call txt('negative creeps upward: not a sigmoid',x+12,y+h-44,c_orange)
  if(code==20)then
   call box(ix,iy+100,150d0,60d0,rgba(80,70,40,255))
   call txt('old template',ix+8,iy+104,c_ink)
   call txt(trim(num(p(1),0))//' C / '//trim(num(p(2),0))//' cycles',ix+8,iy+124,c_orange)
   a=mod(t,3d0)/3;call arrow(ix+60,iy+100,ix+60,iy+82-10*a,c_orange,2d0)
  end if
  if(code==21)then
   call txt('melt derivative needs an intercalating dye',x+12,y+h-44,c_orange)
   do i=1,40
    k=60+i*.9d0;v=exp(-((k-p(1))/2.2d0)**2)+.15d0*sin(i*2.3d0)
    call seg(gx+(i-1)*5d0,gy+gh-10-v*60,gx+i*5d0,gy+gh-10-(exp(-((k+.9d0-p(1))/2.2d0)**2)+.15d0*sin((i+1)*2.3d0))*60, &
     c_cyan,1.2d0)
   end do
  end if
  if(code==22.and.cyc>=nint(p(1)))then
   call box(pcx+8,pcy+8,214d0,118d0,rgba(140,30,30,255))
   call txt('software crash',pcx+50,pcy+50,c_ink);call txt('unsaved cycles lost',pcx+40,pcy+72,c_ink)
  end if
  call txt('cycle '//trim(num(real(cyc,c_double),0)),ix,iy+90,c_cyan)
 contains
  real(c_double) function sig(k)
   integer,intent(in)::k
   sig=.05d0+.9d0/(1+exp(-(k-24d0)/2.2d0))
  end function
 end subroutine
! ------------------------------------------------------------------ structure (PDB 8UCU)
! @nativefn NR2-ANI-F0080
 subroutine draw_structure_scene(code,p,x,y,w,h,t,fault_on,sidx)
  ! Rotating polymerase-DNA complex drawn back to front (64 depth bins). Polymerase as its
  ! C-alpha trace, nucleic acids and ligands as heavy atoms.
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  logical,intent(in)::fault_on
  integer,intent(in)::sidx
  integer,parameter::nbin=64
  real(c_double)::a,tilt,ca,sa,ct,st,px,py,pz,sc,cx,cy,r,rx,ry,rz,zmin,zmax,jit,ix,iy,iz,ang
  real(c_double),allocatable::sx(:),sy(:),sz(:)
  real(c_double),save::gs(3,8)=0,rs(8)=-1
  real(c_double)::g(3),rmax
  integer::i0,i1,ns,si
  integer,allocatable::order(:),cnt(:)
  integer::i,k,b,c,nin,j
  integer(c_int32_t)::col
  logical::on
  on=fault_on;c=code;if(.not.on)c=0
  si=max(1,min(st_count,sidx));i0=st_first(si);i1=st_last(si);ns=i1-i0+1
  if(si>1)c=0
  cb=[x,y,x+w,y+h];call box(x,y,w,h,rgba(18,12,18,255))
  allocate(sx(ns),sy(ns),sz(ns),order(ns),cnt(0:nbin))
  if(rs(si)<0)then
   do k=1,3
    gs(k,si)=sum(st_xyz(k,i0:i1))/(10d0*ns)
   end do
   rs(si)=0
   do i=i0,i1
    rs(si)=max(rs(si),sqrt(sum((st_xyz(:,i)/10d0-gs(:,si))**2)))
   end do
  end if
  g=gs(:,si);rmax=rs(si)
  a=t*.35d0;tilt=.30d0;ca=cos(a);sa=sin(a);ct=cos(tilt);st=sin(tilt)
  sc=.92d0*(min(w,h-60)/2)/rmax;cx=x+w/2;cy=y+h/2+8
  jit=0;if(c==23)jit=4*(1-p(1))
  do j=1,ns
   i=i0+j-1
   px=st_xyz(1,i)/10d0-g(1);py=st_xyz(2,i)/10d0-g(2);pz=st_xyz(3,i)/10d0-g(3)
   if(jit>0.and.(st_kind(i)<=2.or.st_kind(i)==8))then
    px=px+jit*sin(i*1.7d0+t*3);py=py+jit*sin(i*2.3d0+t*2.6d0);pz=pz+jit*sin(i*3.1d0+t*3.3d0)
   end if
   rx=ca*px+sa*pz;rz=-sa*px+ca*pz;ry=ct*py-st*rz;rz=st*py+ct*rz
   sx(j)=cx+sc*rx;sy(j)=cy-sc*ry;sz(j)=rz
  end do
  zmin=minval(sz);zmax=maxval(sz)+1d-6
  ! counting sort by depth, far (small z) first
  cnt=0
  do i=1,ns
   b=min(nbin-1,int((sz(i)-zmin)/(zmax-zmin)*nbin));cnt(b+1)=cnt(b+1)+1
  end do
  do b=1,nbin
   cnt(b)=cnt(b)+cnt(b-1)
  end do
  do i=1,ns
   b=min(nbin-1,int((sz(i)-zmin)/(zmax-zmin)*nbin));cnt(b)=cnt(b)+1;order(cnt(b))=i
  end do
  do k=1,ns
   j=order(k);i=i0+j-1
   select case(st_kind(i))
   case(1)
    r=1.9d0;col=mix(rgba(150,110,200,255),rgba(90,150,255,255),real(i,c_double)/870)
    if(c==23)col=rgba(125,120,130,255)
   case(2)
    r=2.1d0;col=c_orange
    if(c==23)col=rgba(150,130,120,255)
   case(3)
    r=1.05d0;col=c_cyan
   case(4)
    r=1.05d0;col=rgba(255,150,200,255)
   case(5)
    r=1.15d0;col=rgba(110,255,140,255)
    if(c==26.and.mod(t,6d0)>2)cycle
   case(6)
    r=1.15d0;col=rgba(255,220,70,255)
   case(8)
    r=1.9d0;col=mix(rgba(70,170,150,255),rgba(140,210,120,255),real(j,c_double)/ns)
   case(9)
    r=1.9d0;col=rgba(235,235,240,255)
   case default
    r=1.5d0;col=rgba(255,80,255,255)
   end select
   col=mix(rgba(18,12,18,255),col,.55d0+.45d0*(sz(j)-zmin)/(zmax-zmin))
   call ball(sx(j),sy(j),max(1d0,r*sc),col)
  end do
  if(c==27)then
   ! inhibitor molecules (humic acid / ethanol carry-over) crowding the DNA-binding surface
   nin=8
   do k=1,nin
    ang=k*2*pi/nin+t*.6d0
    ix=18*cos(ang);iy=10*sin(k*1.3d0);iz=18*sin(ang)
    rx=ca*ix+sa*iz;rz=-sa*ix+ca*iz;ry=ct*iy-st*rz
    call ball(cx+sc*rx,cy-sc*ry,3.2d0*sc,rgba(140,100,60,255))
   end do
  end if
  select case(si)
  case(1)
   call txt('PDB 8UCU: DNA polymerase alpha (C-alpha trace), template (cyan), primer (pink),',x+8,y+6,c_ink)
   call txt('incoming dGTP (green), terminal ddC (yellow), Mg2+ (magenta), DNA-contact residues (orange)',x+8,y+24,c_ink)
  case(2)
   call txt('PDB 1RFF: tyrosyl-DNA phosphodiesterase 1 (DNA repair) dimer, DNA (cyan/pink),',x+8,y+6,c_ink)
   call txt('topoisomerase I peptides (white), vanadate (yellow), DNA-contact residues (orange)',x+8,y+24,c_ink)
  case(3)
   call txt('PDB 1A3Q: NF-kappaB p52 homodimer (transcription factor) gripping its DNA site,',x+8,y+6,c_ink)
   call txt('monomers purple / green, DNA strands cyan / pink, DNA-contact residues (orange)',x+8,y+24,c_ink)
  case default
   call txt('PDB 1RM1: TATA-binding protein (purple) and TFIIA (green) on TATA-box DNA,',x+8,y+6,c_ink)
   call txt('DNA strands cyan / pink (bent by TBP), DNA-contact residues (orange)',x+8,y+24,c_ink)
  end select
  if(si>1)call txt('General protein-DNA teaching scene: not a PCR enzyme',x+8,y+h-42,c_orange)
  if(c==23)call txt('heat-inactivated enzyme: the fold is lost, no productive binding',x+8,y+h-42,c_orange)
  if(c==27)call txt('inhibitors bind the enzyme and the DNA: fewer productive complexes',x+8,y+h-42,c_orange)
  if(c==26)call txt('no dNTP at the active site for most of the time: extension stalls',x+8,y+h-42,c_orange)
  select case(si)
  case(1)
   call txt('Real structure, rotating. Mullins et al. 2024 (cryo-EM)',x+8,y+h-20,c_cyan)
  case(2)
   call txt('Real structure, rotating. Davies et al. 2004 (X-ray)',x+8,y+h-20,c_cyan)
  case(3)
   call txt('Real structure, rotating. Cramer et al. 1997 (X-ray)',x+8,y+h-20,c_cyan)
  case default
   call txt('Real structure, rotating. Jin, Gewirth and Geiger (X-ray)',x+8,y+h-20,c_cyan)
  end select
  cb=[x,y,x+w,y+h];call frame_rect(x+.5d0,y+.5d0,w-1,h-1,c_purple,1d0)
  cb=[0d0,0d0,1d4,1d4]
 end subroutine
end module
