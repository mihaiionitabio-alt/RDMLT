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
 public :: anim_helix_banner, draw_fault_scene, scene_family, draw_structure_scene
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
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  integer,parameter::nwell=8
  real(c_double)::ph,tblock,tsens,lidt,led,cyc,read,px0,pw,wx,wt,yl,yp,yb,camx,camy,mx0,my0,fan,lift,liq,gl,ang
  real(c_double)::trans,zone(2),lidopen,fx,fy,mfx
  integer::i,k
  integer(c_int32_t)::lc,glc
  ! 6 s loop: 0-2 s denaturation at 95 C, 2-6 s annealing at 60 C with the camera read.
  ph=mod(t,6d0);cyc=floor(t/6d0)
  if(ph<2)then
   tblock=60+35*min(1d0,ph/.6d0)
  else
   tblock=95-35*min(1d0,(ph-2)/.8d0)
  end if
  read=0;if(ph>3.2d0.and.ph<5.6d0)read=1
  zone=tblock;lidt=105;led=1;lift=0;lidopen=0;trans=1;tsens=tblock
  select case(code)
  case(2)
   zone(1)=tblock-(1-p(1))*abs(tblock-60)*.8d0
  case(3)
   lidt=p(1)
  case(6)
   led=p(1)
  case(7)
   led=p(2)+(p(1)-p(2))*mod(t/30d0,1d0)
  case(11,28)
   lift=6
  case(12)
   tsens=tblock-min(p(2),p(1)*mod(t,24d0))
  case(14)
   lidopen=min(1d0,max(0d0,(mod(t,12d0)-4)/1.5d0))
  case(29)
   trans=p(1)
  end select
  px0=x+70;pw=w-140;wx=pw/nwell;yl=y+102;yp=y+120;yb=y+168
  camx=x+w*.70d0;camy=y+14;mx0=x+w/2-95;my0=y+70
  ! heat sink, Peltier zones, block
  call box(px0-10,yb+34,pw+20,6d0,rgba(120,120,135,255))
  do i=0,int((pw+20)/9)
   call box(px0-10+i*9d0,yb+40,4d0,26d0,rgba(150,150,165,255))
  end do
  if(code==13)then
   do i=0,int((pw+20)/9)
    call box(px0-10+i*9d0,yb+40,4d0,26d0,mix(rgba(150,150,165,255),rgba(230,70,50,255),.3d0+.3d0*sin(t*3)))
   end do
  end if
  do k=1,2
   gl=1;if(code==2.and.k==1)gl=p(1)
   call box(px0+(k-1)*pw/2+2,yb+22,pw/2-4,11d0,mix(rgba(60,60,70,255),rgba(90,170,255,255),gl))
   call txt('Peltier '//achar(48+k),px0+(k-1)*pw/2+8,yb+21,c_bg)
   if(ph<2)then
    call arrow(px0+(k-.5d0)*pw/2,yb+30,px0+(k-.5d0)*pw/2,yb+20-10*gl,rgba(235,90,60,255),1.5d0)
   else
    call arrow(px0+(k-.5d0)*pw/2,yb+18,px0+(k-.5d0)*pw/2,yb+30+6*gl,rgba(90,150,255,255),1.5d0)
   end if
  end do
  if(code==2)call txt('70%',px0+pw/4-12,yb+46,c_ink)
  do i=0,nwell-1
   wt=zone(1+min(1,(2*i)/nwell))
   if(code==1.and.(i==0.or.i==nwell-1))wt=wt-p(1)
   call box(px0+i*wx,yb,wx+.5d0,20d0,tcol(wt))
  end do
  call txt('Block '//trim(num(tblock,0))//' C',px0+4,yb+3,c_bg)
  if(code==1)then
   call txt('edge -'//trim(num(p(1),1))//' C',px0+pw-wx-30,yb+3,c_bg)
   call txt('edge wells run colder than the centre',x+8,y+6,c_orange)
  end if
  if(code==2)call txt('zone 1 Peltier weak: slow ramps',x+8,y+6,c_orange)
  if(code==12)call txt('controller follows a drifting sensor',x+8,y+6,c_orange)
  ! temperature sensor
  call disc(px0+pw-14,yb+10,4d0,c_ink,1d0)
  call txt('sensor',x+8,yb+2,c_ink);call txt(trim(num(tsens,1))//' C',x+8,yb+20,c_ink)
  if(code==12)call txt('true '//trim(num(tblock,1))//' C',x+8,yb+38,c_orange)
  ! fan
  fan=t*12;if(code==13.and.mod(t,10d0)>3)fan=fan*0
  call disc(x+w-34,yb+52,15d0,rgba(60,50,60,255),1d0)
  do k=0,2
   ang=fan+k*2*pi/3
   call seg(x+w-34,yb+52,x+w-34+13*cos(ang),yb+52+13*sin(ang),c_ink,3d0)
  end do
  if(code==13)then
   call txt('fan STALLED',x+w-100,yb+70,c_orange)
  else
   call txt('fan',x+w-44,yb+70,c_ink)
  end if
  ! plate (raised when not seated / wrong plastic)
  do i=0,nwell-1
   fx=px0+i*wx+wx/2;liq=1
   if(code==39.and.(i==0.or.i==nwell-1))liq=max(.35d0,1-p(1)*8*mod(t/18d0,1d0))
   call seg(fx-wx*.32d0,yp-lift,fx-wx*.18d0,yb-4-lift,rgba(200,200,215,255),1.5d0)
   call seg(fx+wx*.32d0,yp-lift,fx+wx*.18d0,yb-4-lift,rgba(200,200,215,255),1.5d0)
   call seg(fx-wx*.18d0,yb-4-lift,fx+wx*.18d0,yb-4-lift,rgba(200,200,215,255),1.5d0)
   ! reaction liquid and its fluorescence (grows cycle by cycle)
   gl=read*min(1d0,.15d0+mod(cyc,8d0)/8)*led*trans
   if(code==4)gl=gl*(1-(1-p(1))*real(i,c_double)/(nwell-1))
   if(code==10.and.(i==3.or.i==4))gl=gl*p(1)
   glc=mix(rgba(40,90,60,255),rgba(110,255,140,255),gl)
   call box(fx-wx*.2d0,yb-4-lift-26*liq,wx*.4d0,26*liq,glc)
   if(gl>0)call glow(fx,yb-16-lift,wx*.45d0,rgba(110,255,140,255),.5d0*gl)
   if(code==30)call glow(fx,yb-12-lift,wx*.5d0,rgba(255,240,140,255),.35d0)
   if(code==41.and.(i==2.or.i==5))then
    k=nint(p(1))
    do k=1,max(1,nint(p(1)))
     call disc(fx-4+6*k-6,yb-10-lift-mod(t*12+k*9,22d0),3.5d0,c_ink,.8d0)
    end do
   end if
   if(code==45.and.(i==1.or.i==6))then
    call disc(fx+wx*.24d0,yp+6-lift+min(30d0,max(0d0,(mod(t,12d0)-6)*8)),3d0,rgba(110,200,255,255),1d0)
   end if
   if(code==46)then
    call disc(fx-wx*.22d0,yp+8-lift,3d0,rgba(110,200,255,255),1d0)
    call disc(fx+wx*.2d0,yp+12-lift,2.5d0,rgba(110,200,255,255),1d0)
   end if
  end do
  call txt('Plate',px0-48,yp+14-lift,c_ink)
  if(lift>0)call txt('plate not seated: gap above the block, slow heat transfer',x+8,y+6,c_orange)
  ! sealing film
  lc=rgba(220,220,230,255);if(code==29)lc=rgba(140,140,140,255)
  call box(px0,yp-3-lift,pw,3d0,lc)
  if(code==39)then
   call glow(px0+wx/2,yp-8,14d0,c_ink,.4d0+.3d0*sin(t*4));call glow(px0+pw-wx/2,yp-8,14d0,c_ink,.4d0+.3d0*sin(t*4))
  end if
  ! heated lid (swings open in case 14)
  if(lidopen>0)then
   call seg(px0,yl,px0+pw*cos(lidopen*1.1d0),yl-pw*sin(lidopen*1.1d0),tcol(lidt),7d0)
   call txt('lid opened during the run: heating stops, interlock',x+8,y+6,c_orange)
  else
   call box(px0,yl,pw,10d0,tcol(lidt))
   call txt('Heated lid '//trim(num(lidt,0))//' C',px0+pw/2-55,yl-1,c_bg)
  end if
  if(code==3)then
   do i=0,nwell-1
    call disc(px0+i*wx+wx/2+3*sin(t+i),yp+4+mod(t*6+i*5,12d0),2.5d0,rgba(150,210,255,255),.9d0)
   end do
   call txt('lid too cool: condensation drips into the wells',x+8,y+6,c_orange)
  end if
  ! optics: LED -> excitation filter -> dichroic -> plate; plate -> emission filter -> camera
  call glow(x+28,my0,26*max(.1d0,led),rgba(130,200,255,255),.8d0*min(1d0,led))
  call ball(x+28,my0,9d0,mix(rgba(60,60,80,255),rgba(140,210,255,255),min(1d0,led)))
  call txt('LED',x+14,my0+14,c_ink)
  if(code==6.or.code==7)then
   call txt(trim(num(100*led,0))//'%',x+14,my0+28,c_orange)
   call txt('weaker excitation: lower counts in every well',x+8,y+6,c_orange)
  end if
  if(code==29)call txt('non-optical seal: most light is blocked',x+8,y+6,c_orange)
  if(code==13)call txt('fan stalled: heat sink overheats, interlock stops the run',x+8,y+6,c_orange)
  if(code==14.and.lidopen<=0)call txt('lid will be opened during the run',x+8,y+6,c_orange)

  call box(x+56,my0-12,5d0,24d0,rgba(90,140,255,255));call txt('Ex',x+50,my0+14,c_ink)
  mfx=0;if(code==4)mfx=18
  call seg(mx0-14,my0+14,mx0+14,my0-14,rgba(200,230,255,255),3d0)
  call txt('dichroic',mx0-26,my0-30,c_ink)
  if(read>0)then
   call dash(x+40,my0,mx0,my0,rgba(120,180,255,255),2d0*min(1d0,led),t*80)
   do i=0,nwell-1,2
    call dash(mx0,my0,px0+i*wx+wx/2+mfx,yp-6,rgba(120,180,255,255),1.2d0*min(1d0,led),t*80)
   end do
   do i=1,nwell-1,2
    gl=min(1d0,.15d0+mod(cyc,8d0)/8)*led*trans
    call dash(px0+i*wx+wx/2,yp-6,camx,camy+46,rgba(110,255,140,255),1.2d0*gl+.2d0,t*80)
   end do
  end if
  ! emission filter wheel and camera
  call disc(camx,camy+50,11d0,rgba(70,70,90,255),1d0)
  call disc(camx,camy+50,6d0,rgba(110,255,140,255),1d0)
  if(code==5.or.code==17)call disc(camx+5,camy+50,6d0,rgba(255,220,70,255),.8d0)
  call txt('Em',camx+14,camy+44,c_ink)
  call box(camx-36,camy,72d0,30d0,rgba(50,50,65,255));call txt('CCD camera',camx-32,camy+8,c_ink)
  if(read>0)call box(camx-30,camy+24,60d0,3d0,rgba(110,255,140,255))
  if(code==8)then
   call box(camx+40,camy,10d0,30d0,rgba(255,80,60,255));call txt('gain too high: counts clip at the ADC ceiling',x+8,y+6,c_orange)
  end if
  if(code==9)then
   call txt('camera read noise x'//trim(num(p(1),0)),x+8,y+6,c_orange)
   do k=1,30
    call disc(camx-34+mod(k*37+t*500,68d0),camy+2+mod(k*13+t*331,26d0),1d0,c_ink,.9d0)
   end do
  end if
  if(code==10)then
   call disc(px0+3.5d0*wx+wx/2,yp-5,wx*1.1d0,rgba(120,90,60,255),.55d0);call txt('spill on the optical path: patch wells dim and noisy',x+8,y+6,c_orange)
  end if
  if(code==5.or.code==17)call txt('filter shifted: FAM leaks into YY',x+8,y+6,c_orange)
  if(code==4)call txt('beam off-axis: far wells dim',x+8,y+6,c_orange)
  if(code==30)call txt('plastic fluoresces',x+8,y+6,c_orange)
  if(code==41)call txt('bubbles scatter light',x+8,y+6,c_orange)
  if(code==45)call txt('droplet on the wall joins late',x+8,y+6,c_orange)
  if(code==46)call txt('liquid on walls: not spun',x+8,y+6,c_orange)
  if(code==39)call txt('edge wells evaporate',x+8,y+6,c_orange)
  call txt(phase_text(ph),x+8,y+h-20,c_cyan)
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
   s='Annealing 60 C - camera read'
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
 subroutine draw_structure_scene(code,p,x,y,w,h,t,fault_on)
  ! Rotating polymerase-DNA complex drawn back to front (64 depth bins). Polymerase as its
  ! C-alpha trace, nucleic acids and ligands as heavy atoms.
  integer,intent(in)::code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  logical,intent(in)::fault_on
  integer,parameter::nbin=64
  real(c_double)::a,tilt,ca,sa,ct,st,px,py,pz,sc,cx,cy,r,rx,ry,rz,zmin,zmax,jit,ix,iy,iz,ang
  real(c_double),allocatable::sx(:),sy(:),sz(:)
  real(c_double),save::g(3)=0,rmax=-1
  integer,allocatable::order(:),cnt(:)
  integer::i,k,b,c,nin
  integer(c_int32_t)::col
  logical::on
  on=fault_on;c=code;if(.not.on)c=0
  cb=[x,y,x+w,y+h];call box(x,y,w,h,rgba(18,12,18,255))
  allocate(sx(st_n),sy(st_n),sz(st_n),order(st_n),cnt(0:nbin))
  if(rmax<0)then
   do k=1,3
    g(k)=sum(st_xyz(k,:))/(10d0*st_n)
   end do
   rmax=0
   do i=1,st_n
    rmax=max(rmax,sqrt(sum((st_xyz(:,i)/10d0-g)**2)))
   end do
  end if
  a=t*.35d0;tilt=.30d0;ca=cos(a);sa=sin(a);ct=cos(tilt);st=sin(tilt)
  sc=.92d0*(min(w,h-60)/2)/rmax;cx=x+w/2;cy=y+h/2+8
  jit=0;if(c==23)jit=4*(1-p(1))
  do i=1,st_n
   px=st_xyz(1,i)/10d0-g(1);py=st_xyz(2,i)/10d0-g(2);pz=st_xyz(3,i)/10d0-g(3)
   if(jit>0.and.st_kind(i)<=2)then
    px=px+jit*sin(i*1.7d0+t*3);py=py+jit*sin(i*2.3d0+t*2.6d0);pz=pz+jit*sin(i*3.1d0+t*3.3d0)
   end if
   rx=ca*px+sa*pz;rz=-sa*px+ca*pz;ry=ct*py-st*rz;rz=st*py+ct*rz
   sx(i)=cx+sc*rx;sy(i)=cy-sc*ry;sz(i)=rz
  end do
  zmin=minval(sz);zmax=maxval(sz)+1d-6
  ! counting sort by depth, far (small z) first
  cnt=0
  do i=1,st_n
   b=min(nbin-1,int((sz(i)-zmin)/(zmax-zmin)*nbin));cnt(b+1)=cnt(b+1)+1
  end do
  do b=1,nbin
   cnt(b)=cnt(b)+cnt(b-1)
  end do
  do i=1,st_n
   b=min(nbin-1,int((sz(i)-zmin)/(zmax-zmin)*nbin));cnt(b)=cnt(b)+1;order(cnt(b))=i
  end do
  do k=1,st_n
   i=order(k)
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
   case default
    r=1.5d0;col=rgba(255,80,255,255)
   end select
   col=mix(rgba(18,12,18,255),col,.55d0+.45d0*(sz(i)-zmin)/(zmax-zmin))
   call ball(sx(i),sy(i),max(1d0,r*sc),col)
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
  call txt('PDB 8UCU: DNA polymerase alpha (C-alpha trace), template (cyan), primer (pink),',x+8,y+6,c_ink)
  call txt('incoming dGTP (green), terminal ddC (yellow), Mg2+ (magenta), DNA-contact residues (orange)',x+8,y+24,c_ink)
  if(c==23)call txt('heat-inactivated enzyme: the fold is lost, no productive binding',x+8,y+h-42,c_orange)
  if(c==27)call txt('inhibitors bind the enzyme and the DNA: fewer productive complexes',x+8,y+h-42,c_orange)
  if(c==26)call txt('no dNTP at the active site for most of the time: extension stalls',x+8,y+h-42,c_orange)
  call txt('Real structure, rotating. Mullins et al. 2024 (cryo-EM)',x+8,y+h-20,c_cyan)
  cb=[x,y,x+w,y+h];call frame_rect(x+.5d0,y+.5d0,w-1,h-1,c_purple,1d0)
  cb=[0d0,0d0,1d4,1d4]
 end subroutine
end module
