! SPDX-License-Identifier: GPL-3.0-only
! Animation theatre: full-screen teaching animations drawn by the Fortran rasteriser.
!  1 3-D instrument (main scene)          5 optics: ray tracing and spectra
!  2 3-D DNA and molecular dynamics       6 fluid: SPH simulation of the liquid in a tube
!  3 electrical schematics and timing     7 enzyme kinetics and inhibition (particles)
!  4 heat equation in the sample block    8 3-D amplification surface from the model
! Methods (re-implemented here, no code copied): quaternion rotation and depth-sorted 3-D
! rendering (3Drendering, FortranGL), velocity-Verlet molecular dynamics (molecular-dynamics-
! FORTRAN), smoothed-particle hydrodynamics with poly6 / spiky / viscosity kernels
! (sph-fluidsim-fortran; Muller et al. 2003), Monte Carlo ray tracing with Snell refraction
! (RayTran), finite differences (Incompact3d), particle tracking and ray-transfer matrices
! (CAMPA accelerator simulation codes database). Illustrations of mechanisms only.
module native_theatre
 use iso_c_binding
 use r2_framebuffer
 use native_anim
 use native_model
 use native_structure
 implicit none
 private
 public :: nscene, theatre_title, draw_theatre_scene, theatre_reset, show_tags, theatre_warm, molecule_banner, molecule_icon, molecule_stage, case_scene_3d, &
  mv_si, mv_yaw, mv_pitch, mv_dist, mv_show, mv_col, mv_spin, mv_cut, mv_clip, mv_open, mv_draw, mv_pick, mv_clear, mv_tick, &
  mv_show_name, mv_col_name
 logical,save :: show_tags=.false.
 integer,parameter :: nscene=9
 real(c_double),parameter :: pi=3.141592653589793d0
 integer(c_int32_t),parameter :: c_ink=int(z'FFF3E0EA',c_int32_t),c_orange=int(z'FF649BFF',c_int32_t)
 integer(c_int32_t),parameter :: c_purple=int(z'FFC99ACB',c_int32_t),c_cyan=int(z'FFDCDC60',c_int32_t)
 integer(c_int32_t),parameter :: c_green=int(z'FF8CFF6E',c_int32_t),c_yellow=int(z'FF46D2FF',c_int32_t)
 integer(c_int32_t),parameter :: c_dark=int(z'FF140C12',c_int32_t),c_wire=int(z'FFDCC8D2',c_int32_t)
 ! ---- 3-D engine state: viewport, z-buffer (device pixels), camera
 real(c_double),save :: vx,vy,vw,vh,fcam,dcam,qcam(4)=[1d0,0d0,0d0,0d0],lit(3),woff(3)=0,wsc=1
 real(4),allocatable,save :: zb(:,:)
 integer,save :: zx0,zy0,zw=0,zh=0
 ! ---- simulation states
 logical,save :: md_init=.false.,sph_init=.false.,heat_init=.false.,enz_init=.false.,opt_init=.false.
 logical,save :: dna_ready=.false.
 real(c_double),save :: th_side=300
 integer,save :: th_fault=0
 real(c_double),save,public :: th_zoom=1   ! <1 brings the 3D camera closer (full-screen views)
 ! ---- molecule viewer (home banner -> full screen)
 integer,save :: mv_si=1,mv_show=1,mv_col=1,mv_np=0,mv_pk(2)=0,mv_dsi=0
 real(c_double),save :: mv_yaw=.6d0,mv_pitch=-.25d0,mv_dist=150,mv_cut=8,mv_clip=0
 logical,save :: mv_spin=.true.
 real(c_double),allocatable,save :: mv_sx(:),mv_sy(:),mv_sz(:),mv_dna(:)
 real(c_double),save :: mv_g(3)=0,mv_rg=0,mv_rmax=0
 real(c_double),save :: dna_c(3)=0,dna_dir(3)=0,dna_perp(3)=0
 integer,parameter :: nmd=110
 real(c_double),save :: mdx(3,nmd),mdv(3,nmd),mdf(3,nmd)
 integer,parameter :: nsph=360
 real(c_double),save :: spx(2,nsph),spv(2,nsph),spf(2,nsph),sprho(nsph),spp(nsph),spt(nsph)
 integer,parameter :: hx=150,hy=46
 real(c_double),save :: ht(hx,hy),hk(hx,hy),hc(hx,hy),hist(3,600),hint=0
 integer,save :: hn=0
 integer,parameter :: nen=2,ne=12,ns=150,nin=40
 real(c_double),save :: ex(2,ne,nen),ev(2,ne,nen),sx(2,ns,nen),sv(2,ns,nen),ix(2,nin),iv(2,nin)
 integer,save :: estate(ne,nen),etimer(ne,nen),sstate(ns,nen),istate(nin),eprod(nen),ehist(2,400),nehist=0
 integer,parameter :: nray=260
 real(c_double),save :: rpts(2,8,nray),rlen(nray),rcol(nray)
 integer,save :: rnp(nray),rnext=1,ndet=0,nemit=0,nexc=0
 integer(c_int64_t),save :: rng=88172645463325252_c_int64_t
contains
! ======================================================================= utilities
! @nativefn NR2-THE-F0001
 function theatre_title(k) result(s)
  integer,intent(in)::k
  character(len=80)::s
  select case(k)
  case(1)
   s='3-D instrument: lid, LEDs, tubes, block, Peltier, heat sink, fan'
  case(2)
   s='3-D DNA: denaturation, annealing, extension and thermal motion'
  case(3)
   s='Electrical: LED driver, photodiode front end, ADC, Peltier H-bridge'
  case(4)
   s='Heat: 2-D heat equation in the sample block, solved live'
  case(5)
   s='Optics: ray tracing from LED to photodiode, spectra and filters'
  case(6)
   s='Fluid: SPH simulation of the reaction liquid in a tube'
  case(7)
   s='Enzyme kinetics and inhibition: particles and Michaelis-Menten'
  case(8)
   s='3-D amplification surface: fluorescence by cycle and well'
  case default
   s='Deep zoom: tube -> liquid -> polymerase on DNA -> active site (PDB 8UCU)'
  end select
 end function
! @nativefn NR2-THE-F0002
 subroutine theatre_reset()
  md_init=.false.;sph_init=.false.;heat_init=.false.;enz_init=.false.;opt_init=.false.
 end subroutine
! @nativefn NR2-THE-F0003
 real(c_double) function urand() result(u)
  ! xorshift64*, deterministic animation noise
  rng=ieor(rng,shiftl(rng,13));rng=ieor(rng,shiftr(rng,7));rng=ieor(rng,shiftl(rng,17))
  u=real(iand(shiftr(rng,11),int(z'FFFFFFFFFFFFF',c_int64_t)),c_double)/4503599627370496d0
 end function
! @nativefn NR2-THE-F0004
 real(c_double) function gauss() result(g)
  real(c_double)::a,b
  a=max(1d-12,urand());b=urand();g=sqrt(-2*log(a))*cos(2*pi*b)
 end function
! @nativefn NR2-THE-F0005
 subroutine panel(x,y,w,h)
  real(c_double),intent(in)::x,y,w,h
  call set_clip(x,y,x+w,y+h);call box(x,y,w,h,rgba(30,18,28,255));call frame_rect(x+.5d0,y+.5d0,w-1,h-1,c_purple,1d0)
 end subroutine
! @nativefn NR2-THE-F0006
 function cmap(v) result(c)
  ! perceptual-style dark-blue -> magenta -> orange -> yellow ramp for 0..1
  real(c_double),intent(in)::v
  integer(c_int32_t)::c
  real(c_double)::u
  u=max(0d0,min(1d0,v))
  if(u<.33d0)then
   c=mix(rgba(25,30,90,255),rgba(140,50,150,255),u/.33d0)
  else if(u<.66d0)then
   c=mix(rgba(140,50,150,255),rgba(240,120,60,255),(u-.33d0)/.33d0)
  else
   c=mix(rgba(240,120,60,255),rgba(255,235,120,255),(u-.66d0)/.34d0)
  end if
 end function
! ======================================================================= 3-D engine
! @nativefn NR2-THE-F0010
 function qmul(a,b) result(c)
  real(c_double),intent(in)::a(4),b(4)
  real(c_double)::c(4)
  c(1)=a(1)*b(1)-a(2)*b(2)-a(3)*b(3)-a(4)*b(4)
  c(2)=a(1)*b(2)+a(2)*b(1)+a(3)*b(4)-a(4)*b(3)
  c(3)=a(1)*b(3)-a(2)*b(4)+a(3)*b(1)+a(4)*b(2)
  c(4)=a(1)*b(4)+a(2)*b(3)-a(3)*b(2)+a(4)*b(1)
 end function
! @nativefn NR2-THE-F0011
 function qaxis(ax,ang) result(q)
  real(c_double),intent(in)::ax(3),ang
  real(c_double)::q(4)
  q(1)=cos(ang/2);q(2:4)=ax/sqrt(sum(ax*ax))*sin(ang/2)
 end function
! @nativefn NR2-THE-F0012
 function qrot(q,v) result(r)
  ! rotate v by unit quaternion q: r = q v q*
  real(c_double),intent(in)::q(4),v(3)
  real(c_double)::r(3),t(3)
  t=2*[q(3)*v(3)-q(4)*v(2),q(4)*v(1)-q(2)*v(3),q(2)*v(2)-q(3)*v(1)]
  r=v+q(1)*t+[q(3)*t(3)-q(4)*t(2),q(4)*t(1)-q(2)*t(3),q(2)*t(2)-q(3)*t(1)]
 end function
! @nativefn NR2-THE-F0013
 subroutine view_begin(x,y,w,h,yaw,pitch,dist,fov)
  ! Viewport, orbit camera (yaw about y, then pitch about x) and an empty depth buffer.
  real(c_double),intent(in)::x,y,w,h,yaw,pitch,dist,fov
  integer::nw,nh
  vx=x;vy=y;vw=w;vh=h;dcam=dist;fcam=min(w,h)*fov
  qcam=qmul(qaxis([1d0,0d0,0d0],pitch),qaxis([0d0,1d0,0d0],yaw))
  lit=[-.45d0,.75d0,-.5d0];lit=lit/sqrt(sum(lit*lit))
  nw=nint(w*render_scale);nh=nint(h*render_scale)
  if(nw/=zw.or.nh/=zh)then
   if(allocated(zb))deallocate(zb)
   allocate(zb(nw,nh));zw=nw;zh=nh
  end if
  zx0=nint(x*render_scale);zy0=nint(y*render_scale);zb=1e30;woff=0;wsc=1
  call set_clip(x,y,x+w,y+h)
 end subroutine
! @nativefn NR2-THE-F0014
 subroutine to_cam(p,c)
  real(c_double),intent(in)::p(3)
  real(c_double),intent(out)::c(3)
  c=qrot(qcam,woff+wsc*p);c(3)=c(3)+dcam
 end subroutine
! @nativefn NR2-THE-F0015
 subroutine project(p,sx,sy,sz)
  ! world -> device pixel coordinates and depth
  real(c_double),intent(in)::p(3)
  real(c_double),intent(out)::sx,sy,sz
  real(c_double)::c(3)
  call to_cam(p,c);sz=max(1d-3,c(3))
  sx=(vx+vw/2+fcam*c(1)/sz)*render_scale;sy=(vy+vh/2-fcam*c(2)/sz)*render_scale
 end subroutine
! @nativefn NR2-THE-F0016
 subroutine screen_of(p,sx,sy)
  ! world -> logical coordinates (for labels)
  real(c_double),intent(in)::p(3)
  real(c_double),intent(out)::sx,sy
  real(c_double)::z
  call project(p,sx,sy,z);sx=sx/render_scale;sy=sy/render_scale
 end subroutine
! @nativefn NR2-THE-F0017
 subroutine tri(a,b,c,col,emit)
  ! Flat-shaded, depth-tested triangle (two-sided Lambert + Blinn specular).
  real(c_double),intent(in)::a(3),b(3),c(3)
  integer(c_int32_t),intent(in)::col
  real(c_double),intent(in),optional::emit
  real(c_double)::ca(3),cbv(3),cc(3),n(3),ln,d,s,hv(3),sh,x(3),y(3),z(3),area,w0,w1,w2,zz,e
  integer::i,j,i0,i1,j0,j1,k,v(3)
  integer(c_int32_t)::shade
  call to_cam(a,ca);call to_cam(b,cbv);call to_cam(c,cc)
  if(min(ca(3),cbv(3),cc(3))<1)return
  n=[(cbv(2)-ca(2))*(cc(3)-ca(3))-(cbv(3)-ca(3))*(cc(2)-ca(2)), &
     (cbv(3)-ca(3))*(cc(1)-ca(1))-(cbv(1)-ca(1))*(cc(3)-ca(3)), &
     (cbv(1)-ca(1))*(cc(2)-ca(2))-(cbv(2)-ca(2))*(cc(1)-ca(1))]
  ln=sqrt(sum(n*n));if(ln<1d-12)return
  n=n/ln;if(n(3)>0)n=-n
  d=max(0d0,sum(n*[lit(1),lit(2),-lit(3)]))
  hv=[lit(1),lit(2),-lit(3)-1];hv=hv/sqrt(sum(hv*hv));s=max(0d0,sum(n*hv))**28
  e=0;if(present(emit))e=emit
  sh=min(1.25d0,.28d0+.72d0*d+e)
  do k=1,3
   v(k)=nint(min(255d0,ibits(col,8*(k-1),8)*sh+190*s))
  end do
  shade=rgba(v(1),v(2),v(3),255)
  x=[ca(1),cbv(1),cc(1)];y=[ca(2),cbv(2),cc(2)];z=[ca(3),cbv(3),cc(3)]
  do k=1,3
   x(k)=(vx+vw/2+fcam*x(k)/z(k))*render_scale;y(k)=(vy+vh/2-fcam*y(k)/z(k))*render_scale
  end do
  area=(x(2)-x(1))*(y(3)-y(1))-(x(3)-x(1))*(y(2)-y(1));if(abs(area)<1d-9)return
  i0=max(zx0,floor(minval(x)));i1=min(zx0+zw-1,ceiling(maxval(x)))
  j0=max(zy0,floor(minval(y)));j1=min(zy0+zh-1,ceiling(maxval(y)))
  ! incremental edge functions along each row; depth-passing pixels are written directly
  i0=max(i0,0);i1=min(i1,fw-1);j0=max(j0,0);j1=min(j1,fh-1)
  do j=j0,j1
   w0=((x(2)-(i0+.5d0))*(y(3)-(j+.5d0))-(x(3)-(i0+.5d0))*(y(2)-(j+.5d0)))/area
   w1=((x(3)-(i0+.5d0))*(y(1)-(j+.5d0))-(x(1)-(i0+.5d0))*(y(3)-(j+.5d0)))/area
   do i=i0,i1
    w2=1-w0-w1
    if(w0>=-1d-9.and.w1>=-1d-9.and.w2>=-1d-9)then
     zz=w0*z(1)+w1*z(2)+w2*z(3)
     if(zz<zb(i-zx0+1,j-zy0+1))then
      zb(i-zx0+1,j-zy0+1)=real(zz,4);pixels(j*fw+i+1)=shade
     end if
    end if
    w0=w0+(y(2)-y(3))/area;w1=w1+(y(3)-y(1))/area
   end do
  end do
 end subroutine
! @nativefn NR2-THE-F0018
 subroutine quad(a,b,c,d,col,emit)
  real(c_double),intent(in)::a(3),b(3),c(3),d(3)
  integer(c_int32_t),intent(in)::col
  real(c_double),intent(in),optional::emit
  real(c_double)::e
  e=0;if(present(emit))e=emit
  call tri(a,b,c,col,e);call tri(a,c,d,col,e)
 end subroutine
! @nativefn NR2-THE-F0019
 subroutine cuboid(c,sz,col,q,pivot,emit)
  ! Box centred at c with size sz; optional rotation q about pivot.
  real(c_double),intent(in)::c(3),sz(3)
  integer(c_int32_t),intent(in)::col
  real(c_double),intent(in),optional::q(4),pivot(3),emit
  real(c_double)::p(3,8),e
  integer::k
  integer,parameter::f(4,6)=reshape([1,2,4,3, 5,7,8,6, 1,5,6,2, 3,4,8,7, 1,3,7,5, 2,6,8,4],[4,6])
  e=0;if(present(emit))e=emit
  do k=1,8
   p(1,k)=c(1)+sz(1)/2*merge(-1,1,mod(k-1,2)==0)
   p(2,k)=c(2)+sz(2)/2*merge(-1,1,mod((k-1)/2,2)==0)
   p(3,k)=c(3)+sz(3)/2*merge(-1,1,(k-1)/4==0)
   if(present(q))p(:,k)=pivot+qrot(q,p(:,k)-pivot)
  end do
  do k=1,6
   call quad(p(:,f(1,k)),p(:,f(2,k)),p(:,f(3,k)),p(:,f(4,k)),col,e)
  end do
 end subroutine
! @nativefn NR2-THE-F0020
 subroutine frustum(p0,p1,r0,r1,nseg,col,caps,emit)
  ! Cylinder / cone frustum between p0 (radius r0) and p1 (radius r1).
  real(c_double),intent(in)::p0(3),p1(3),r0,r1
  integer,intent(in)::nseg
  integer(c_int32_t),intent(in)::col
  logical,intent(in)::caps
  real(c_double),intent(in),optional::emit
  real(c_double)::ax(3),u(3),w(3),a0,a1,q00(3),q01(3),q10(3),q11(3),e
  integer::k
  e=0;if(present(emit))e=emit
  ax=p1-p0;ax=ax/sqrt(sum(ax*ax))
  u=[1d0,0d0,0d0];if(abs(ax(1))>.9d0)u=[0d0,0d0,1d0]
  u=u-sum(u*ax)*ax;u=u/sqrt(sum(u*u))
  w=[ax(2)*u(3)-ax(3)*u(2),ax(3)*u(1)-ax(1)*u(3),ax(1)*u(2)-ax(2)*u(1)]
  do k=0,nseg-1
   a0=2*pi*k/nseg;a1=2*pi*(k+1)/nseg
   q00=p0+r0*(cos(a0)*u+sin(a0)*w);q01=p0+r0*(cos(a1)*u+sin(a1)*w)
   q10=p1+r1*(cos(a0)*u+sin(a0)*w);q11=p1+r1*(cos(a1)*u+sin(a1)*w)
   call quad(q00,q01,q11,q10,col,e)
   if(caps)then
    if(r0>0)call tri(p0,q01,q00,col,e)
    if(r1>0)call tri(p1,q10,q11,col,e)
   end if
  end do
 end subroutine
! @nativefn NR2-THE-F0021
 subroutine zsphere(p,r,col,emit)
  ! Depth-correct shaded sphere impostor (radius r in world units).
  real(c_double),intent(in)::p(3),r
  integer(c_int32_t),intent(in)::col
  real(c_double),intent(in),optional::emit
  real(c_double)::c(3),sx,sy,sr,dx,dy,d2,nz,diff,spec,inten,zz,e
  integer::i,j,k,v(3)
  call to_cam(p,c);if(c(3)<r*wsc+1)return
  sx=(vx+vw/2+fcam*c(1)/c(3))*render_scale;sy=(vy+vh/2-fcam*c(2)/c(3))*render_scale
  sr=fcam*r*wsc/c(3)*render_scale
  e=0;if(present(emit))e=emit
  do j=max(zy0,floor(sy-sr)),min(zy0+zh-1,ceiling(sy+sr))
   do i=max(zx0,floor(sx-sr)),min(zx0+zw-1,ceiling(sx+sr))
    dx=(i+.5d0-sx)/sr;dy=(sy-(j+.5d0))/sr;d2=dx*dx+dy*dy
    if(d2>=1)cycle
    nz=sqrt(1-d2);zz=c(3)-r*wsc*nz
    if(zz>=zb(i-zx0+1,j-zy0+1))cycle
    zb(i-zx0+1,j-zy0+1)=real(zz,4)
    diff=max(0d0,lit(1)*dx+lit(2)*dy+(-lit(3))*nz);spec=max(0d0,-.25d0*dx+.35d0*dy+.9d0*nz)**30
    inten=min(1.3d0,.3d0+.7d0*diff+e)
    do k=1,3
     v(k)=nint(min(255d0,ibits(col,8*(k-1),8)*inten+200*spec))
    end do
    call pixel_raw(i,j,rgba(v(1),v(2),v(3),255),1d0)
   end do
  end do
 end subroutine
! @nativefn NR2-THE-F0022
 subroutine zline(a,b,col,wd)
  ! Depth-tested 3-D line (bond, wire) drawn as small discs along its length.
  real(c_double),intent(in)::a(3),b(3),wd
  integer(c_int32_t),intent(in)::col
  real(c_double)::ax,ay,az,bx,by,bz,l,t,px,py,pz,r
  integer::k,n,i,j
  call project(a,ax,ay,az);call project(b,bx,by,bz)
  l=sqrt((bx-ax)**2+(by-ay)**2);n=max(1,int(l/1.5d0));r=wd*render_scale/2
  do k=0,n
   t=real(k,c_double)/n;px=ax+(bx-ax)*t;py=ay+(by-ay)*t;pz=az+(bz-az)*t
   do j=max(zy0,floor(py-r)),min(zy0+zh-1,ceiling(py+r))
    do i=max(zx0,floor(px-r)),min(zx0+zw-1,ceiling(px+r))
     if((i+.5d0-px)**2+(j+.5d0-py)**2>r*r)cycle
     if(pz-.5d0>=zb(i-zx0+1,j-zy0+1))cycle
     zb(i-zx0+1,j-zy0+1)=real(pz,4);call pixel_raw(i,j,col,1d0)
    end do
   end do
  end do
 end subroutine
! @nativefn NR2-THE-F0023
 subroutine tag(p,s,dx,dy)
  ! Label with a leader line from a 3-D anchor.
  real(c_double),intent(in)::p(3),dx,dy
  character(len=*),intent(in)::s
  real(c_double)::sx,sy
  if(.not.show_tags)return
  call screen_of(p,sx,sy)
  call disc(sx,sy,2.5d0,c_ink,1d0);call seg(sx,sy,sx+dx,sy+dy,rgba(220,210,220,255),1d0)
  call txt(s,sx+dx+merge(4d0,-7d0*len_trim(s)-4,dx>=0),sy+dy-8,c_ink)
 end subroutine
! @nativefn NR2-THE-F0024
 subroutine glow3(p,r,col,a)
  real(c_double),intent(in)::p(3),r,a
  integer(c_int32_t),intent(in)::col
  real(c_double)::sx,sy,c(3)
  call to_cam(p,c);if(c(3)<1)return
  call screen_of(p,sx,sy);call glow(sx,sy,fcam*r*wsc/c(3),col,a)
 end subroutine
! ======================================================================= 1 3-D instrument
! @nativefn NR2-THE-F0040
 subroutine scene_3d_instrument(x,y,w,h,t)
  ! 40 s loop: lid open and tubes lowered (0-6 s), lid closes (6-9 s), then cycling:
  ! 4 s per cycle, denaturation 95 C, annealing 60 C with the LED read (fluorescence grows).
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::tl,ang,yaw,lidang,drop,ph,tb,tlid,cyc,flu,read,fan,q(4),piv(3),tx,tz,gl,px,pw,yy
  integer::i,r,k
  integer(c_int32_t)::alu,cu,cer,pcb,dark,tube
  character(len=40)::stage
  tl=max(0d0,mod(t,52d0)-12);yaw=.6d0+t*.18d0
  lidang=0;drop=0
  if(tl<6)then
   lidang=1.15d0;drop=60*max(0d0,1-tl/5)
  else if(tl<9)then
   lidang=1.15d0*(1-(tl-6)/3)
  end if
  ph=0;cyc=0;read=0;tb=25;tlid=25+min(1d0,tl/8)*80;stage='Loading tubes'
  if(tl>=9)then
   cyc=floor((tl-9)/4)+1;ph=mod(tl-9,4d0)
   if(ph<1.6d0)then
    tb=60+35*min(1d0,ph/.5d0);stage='Denaturation 95 C'
   else
    tb=95-35*min(1d0,(ph-1.6d0)/.6d0);stage='Annealing 60 C'
    if(ph>2.5d0.and.ph<3.6d0)then
     read=1;stage='LED read, photodiodes sample'
    end if
   end if
  else if(tl>=6)then
   stage='Closing the heated lid'
  end if
  flu=1/(1+exp(-(cyc-5.5d0)/1.1d0))
  call panel(x,y,w,h)
  call box(x+1,y+1,w-2,h-2,rgba(16,11,18,255))
  ! Open qPCR-style exterior (dark wedge chassis, sloped touchscreen, raised lid housing with
  ! knob and handle) for 12 s, then the housing lifts and the chassis turns to a cut-away.
  if(mod(t,52d0)<12)then
   call view_begin(x,y,w-th_side,h,pi-.55d0+.35d0*sin(t*.35d0),-.36d0,620d0,1.15d0)
   woff=[0d0,-95d0,0d0]
  else
   call view_begin(x,y,w-th_side,h,yaw,-.42d0,merge(230d0*th_zoom,330d0,th_side<=0),1.15d0)
   wsc=.62d0;woff=-wsc*[0d0,96d0,-70d0]
  end if
  call chassis(min(1d0,max(0d0,(mod(t,52d0)-10)/3)),mod(t,52d0)>=12,tb,tlid)
  if(mod(t,52d0)<12)then
   call tag([0d0,150d0,-40d0],'lid housing (knob, handle)',90d0,-50d0)
   call tag([0d0,95d0,95d0],'touchscreen',-90d0,40d0)
   call tag([-150d0,60d0,-80d0],'rear: fan grille, power inlet, switch',-60d0,50d0)
   call side_panel_info(x,y,w,h,'Exterior of a 16-well dual-channel cycler','(Open qPCR-style chassis)',tb,tlid,cyc,flu)
   call cad_credit(x,y,w-th_side,h)
   return
  end if
  ! to scale (1.94 units per mm, 9 mm well pitch = 17.5 units): heat sink 100 mm long, 70 mm
  ! wide, 6 mm base + 74 mm fins under the copper spreader; 60 mm fan on its end face
  wsc=.33d0;woff=wsc*[0d0,58d0,0d0]
  alu=rgba(170,172,185,255);cu=rgba(205,120,70,255);cer=rgba(235,232,225,255);pcb=rgba(40,120,70,255)
  dark=rgba(95,86,102,255);tube=rgba(225,228,235,255)
  ! housing and heat sink
  call cuboid([0d0,-176d0,0d0],[260d0,10d0,170d0],dark)
  call cuboid([0d0,-18d0,0d0],[194d0,12d0,136d0],alu)
  do i=0,16
   call cuboid([0d0,-96d0,-64d0+i*8d0],[194d0,144d0,2.6d0],mix(alu,rgba(150,150,165,255),.5d0))
  end do
  ! 60 mm fan on the end face, rotating blades, blowing along the fins
  fan=t*9
  call frustum([98d0,-96d0,0d0],[122d0,-96d0,0d0],56d0,56d0,32,dark,.true.)
  do k=0,4
   q=qaxis([1d0,0d0,0d0],fan+k*2*pi/5)
   call cuboid([123d0,-96d0,0d0]+qrot(q,[0d0,26d0,0d0]),[2d0,44d0,16d0],rgba(150,150,165,255),q,[123d0,-96d0,0d0])
  end do
  ! copper spreader, Peltier modules (red = heating, blue = cooling)
  call cuboid([0d0,-10d0,0d0],[150d0,4d0,84d0],cu)
  do k=0,1
   gl=0;if(tl>=9)gl=merge(1d0,-1d0,ph<1.6d0)
   call cuboid([-37d0+74d0*k,-5d0,0d0],[62d0,6d0,62d0],mix(cer,merge(rgba(240,90,60,255),rgba(80,150,255,255),gl>=0),.35d0*abs(gl)))
  end do
  ! sample block (temperature colour) with two thermistors
  call cuboid([0d0,8d0,0d0],[146d0,20d0,74d0],mix(alu,tcol(tb),.55d0))
  call zsphere([-35d0,8d0,37.5d0],2d0,c_ink);call zsphere([35d0,8d0,37.5d0],2d0,c_ink)
  ! photodiode boards and emission filters along both long sides
  do k=-1,1,2
   call cuboid([0d0,10d0,k*44d0],[146d0,26d0,2d0],pcb,emit=.35d0*read)
   call cuboid([0d0,12d0,k*41.5d0],[140d0,6d0,1d0],rgba(110,230,140,255),emit=.5d0*read)
  end do
  ! 2 x 8 tubes with liquid; they drop in at the start
  do r=0,1
   do i=0,7
    tx=-61.25d0+i*17.5d0;tz=-15d0+30d0*r
    call frustum([tx,2d0+drop,tz],[tx,24d0+drop,tz],2.6d0,4.6d0,12,tube,.true.)
    call frustum([tx,24d0+drop,tz],[tx,28d0+drop,tz],5.2d0,5.2d0,12,tube,.true.)
    gl=flu*(.25d0+.75d0*read)*(.85d0+.15d0*sin(i*1.7d0+r))
    if(i==7.and.r==1)gl=0
    call zsphere([tx,10d0+drop,tz],3.0d0,mix(rgba(50,90,70,255),c_green,gl),emit=.6d0*gl)
    if(gl>.2d0)call glow3([tx,10d0+drop,tz],9d0,c_green,.45d0*gl)
   end do
  end do
  ! heated lid with LED board, hinged on the back edge
  piv=[0d0,30d0,-42d0];q=qaxis([1d0,0d0,0d0],-lidang)
  call cuboid([0d0,36d0,0d0],[152d0,12d0,84d0],mix(rgba(120,110,120,255),tcol(tlid),.45d0),q,piv)
  call cuboid([0d0,43.5d0,0d0],[152d0,3d0,84d0],pcb,q,piv)
  do r=0,1
   do i=0,7
    tx=-61.25d0+i*17.5d0;tz=-15d0+30d0*r
    call zsphere(piv+qrot(q,[tx,46.5d0,tz]-piv),2.2d0,mix(rgba(60,70,110,255),rgba(140,200,255,255),read),emit=read)
    if(read>0)call glow3(piv+qrot(q,[tx,46.5d0,tz]-piv),8d0,rgba(140,200,255,255),.55d0)
   end do
  end do
  ! labels
  call tag(piv+qrot(q,[76d0,45d0,30d0]-piv),'LED board + heated lid',40d0,-30d0)
  call tag([61d0,26d0+drop,15d0],'tubes (2 x 8)',50d0,-20d0)
  call tag([73d0,10d0,30d0],'sample block',60d0,0d0)
  call tag([0d0,10d0,45d0],'photodiode board',-60d0,40d0)
  call tag([60d0,-5d0,31d0],'Peltier modules',60d0,22d0)
  call tag([75d0,-10d0,42d0],'copper spreader',60d0,44d0)
  call tag([-80d0,-96d0,68d0],'heat sink 80 mm',-60d0,30d0)
  call tag([122d0,-96d0,40d0],'fan 60 mm',40d0,30d0)
  call tag([0d0,12d0,-44d0],'optics modules: 520/24 + 585/24 filters',-120d0,-60d0)
  ! callout: what is inside one tube (real structure, PDB 8UCU)
  call screen_of([-61.25d0+17.5d0*2,26d0+drop,15d0],tx,tz)
  woff=0;wsc=1
  if(show_tags)call seg(tx,tz,x+28+170d0,y+h-290d0,c_orange,1.2d0)
  if(th_side<=0)then
   call set_clip(0d0,0d0,1d4,1d4);return   ! compact (previews, full-screen views): sources are listed by the page
  end if
  call molecule_inset(x+20,y+h-300,340d0,240d0,1,t,'inside the tube: polymerase on DNA (PDB 8UCU)',.false.)
  ! side panel
  px=x+w-290;pw=280
  call set_clip(0d0,0d0,1d4,1d4)
  call box(px,y+10,pw,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,pw,h-20,c_purple,1d0)
  call txt('Generic 16-tube real-time cycler',px+12,y+22,c_orange)
  call txt(trim(stage),px+12,y+50,c_cyan)
  call txt('Cycle   '//trim(num(cyc,0))//' / 8 (demo loop)',px+12,y+78,c_ink)
  call txt('Block   '//trim(num(tb,1))//' C',px+12,y+102,c_ink)
  call txt('Lid     '//trim(num(tlid,0))//' C',px+12,y+126,c_ink)
  call txt('Signal  '//trim(num(100*flu,0))//' % of plateau',px+12,y+150,c_ink)
  yy=y+190
  call txt('Heat path: block -> Peltier ->',px+12,yy,c_purple);call txt('copper spreader -> fins -> fan air',px+12,yy+20,c_purple)
  call txt('Light path: LED -> excitation filter ->',px+12,yy+52,c_purple);call txt('tube -> side emission filter ->',px+12,yy+72,c_purple)
  call txt('photodiode -> transimpedance -> ADC',px+12,yy+92,c_purple)
  call txt('Software 3-D renderer: quaternion',px+12,y+h-80,c_ink);call txt('camera, depth buffer, flat shading.',px+12,y+h-60,c_ink)
  call txt('Layout generic, not a specific product.',px+12,y+h-36,c_orange)
  call cad_credit(x,y,w-th_side,h)
 end subroutine
! @nativefn NR2-THE-F0041
 subroutine chassis(lift,cutaway,tb,tlid)
  ! Wedge chassis in mm (x width, y up, z front): cross-section extruded along x. In cut-away
  ! mode only its edges are drawn so the internal assembly stays visible.
  real(c_double),intent(in)::lift,tb,tlid
  logical,intent(in)::cutaway
  real(c_double),parameter::hw=135
  real(c_double)::pz(5),py(5),a(3),b(3),q(4),piv(3),sx,sy,c1(3),c2(3),c3(3),c4(3)
  integer::k,m
  integer(c_int32_t)::body,dk
  pz=[-150d0,-150d0,20d0,150d0,150d0];py=[0d0,140d0,140d0,55d0,0d0]
  body=rgba(92,96,102,255);dk=rgba(70,73,78,255)
  if(cutaway)then
   do k=1,5
    m=1+mod(k,5)
    call zline([-hw,py(k),pz(k)],[-hw,py(m),pz(m)],rgba(120,125,130,255),1.2d0)
    call zline([hw,py(k),pz(k)],[hw,py(m),pz(m)],rgba(120,125,130,255),1.2d0)
    call zline([-hw,py(k),pz(k)],[hw,py(k),pz(k)],rgba(120,125,130,255),1.2d0)
   end do
  else
   do k=1,5
    m=1+mod(k,5)
    call quad([-hw,py(k),pz(k)],[hw,py(k),pz(k)],[hw,py(m),pz(m)],[-hw,py(m),pz(m)],body)
   end do
   do k=2,4
    call tri([-hw,py(1),pz(1)],[-hw,py(k),pz(k)],[-hw,py(k+1),pz(k+1)],dk)
    call tri([hw,py(1),pz(1)],[hw,py(k),pz(k)],[hw,py(k+1),pz(k+1)],dk)
   end do
   ! touchscreen on the slope
   c1=[-60d0,128d0,40d0];c2=[60d0,128d0,40d0];c3=[60d0,72d0,128d0];c4=[-60d0,72d0,128d0]
   call quad(c1+[0d0,.6d0,.4d0],c2+[0d0,.6d0,.4d0],c3+[0d0,.6d0,.4d0],c4+[0d0,.6d0,.4d0],rgba(70,110,150,255),.5d0)
   call screen_of((c1+c2+c3+c4)/4,sx,sy)
   call txt('Run in progress',sx-50,sy-30,c_ink);call txt('Block '//trim(num(tb,1))//' C',sx-50,sy-10,c_ink)
   call txt('Lid '//trim(num(tlid,0))//' C',sx-50,sy+8,c_ink)
   ! rear grille
   do k=0,5
    do m=0,3
     call quad([-151d0,30d0+m*20d0,-60d0+k*20d0],[-151d0,30d0+m*20d0,-48d0+k*20d0],[-151d0,42d0+m*20d0,-48d0+k*20d0], &
      [-151d0,42d0+m*20d0,-60d0+k*20d0],rgba(25,25,28,255))
    end do
   end do
  end if
  ! lid housing (trapezoid prism) with knob and handle, hinged at the back; lifts open
  piv=[0d0,140d0,-140d0];q=qaxis([1d0,0d0,0d0],-1.2d0*lift)
  if(lift<1)then
   call hous(q,piv,body)
  else
   call hous(q,piv,rgba(70,72,76,255))
  end if
  call frustum(piv+qrot(q,[0d0,214d0,-70d0]-piv),piv+qrot(q,[0d0,228d0,-70d0]-piv),16d0,16d0,20,rgba(30,30,32,255),.true.)
  a=piv+qrot(q,[-40d0,170d0,-28d0]-piv);b=piv+qrot(q,[40d0,170d0,-28d0]-piv)
  call zline(a,b,rgba(235,235,240,255),5d0)
  call zline(a,piv+qrot(q,[-40d0,180d0,-36d0]-piv),rgba(235,235,240,255),5d0)
  call zline(b,piv+qrot(q,[40d0,180d0,-36d0]-piv),rgba(235,235,240,255),5d0)
 contains
  subroutine hous(q,piv,col)
   real(c_double),intent(in)::q(4),piv(3)
   integer(c_int32_t),intent(in)::col
   real(c_double)::v(3,8)
   integer::i
   v(:,1)=[-85d0,140d0,-130d0];v(:,2)=[85d0,140d0,-130d0];v(:,3)=[85d0,140d0,-30d0];v(:,4)=[-85d0,140d0,-30d0]
   v(:,5)=[-72d0,214d0,-120d0];v(:,6)=[72d0,214d0,-120d0];v(:,7)=[72d0,214d0,-38d0];v(:,8)=[-72d0,214d0,-38d0]
   do i=1,8
    v(:,i)=piv+qrot(q,v(:,i)-piv)
   end do
   call quad(v(:,5),v(:,6),v(:,7),v(:,8),col);call quad(v(:,4),v(:,3),v(:,7),v(:,8),col)
   call quad(v(:,1),v(:,2),v(:,6),v(:,5),col);call quad(v(:,1),v(:,4),v(:,8),v(:,5),col)
   call quad(v(:,2),v(:,3),v(:,7),v(:,6),col)
  end subroutine
 end subroutine
! @nativefn NR2-THE-F0042
 subroutine side_panel_info(x,y,w,h,l1,l2,tb,tlid,cyc,flu)
  real(c_double),intent(in)::x,y,w,h,tb,tlid,cyc,flu
  character(len=*),intent(in)::l1,l2
  real(c_double)::px
  px=x+w-290;call set_clip(0d0,0d0,1d4,1d4)
  call box(px,y+10,280d0,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,280d0,h-20,c_purple,1d0)
  call txt(l1,px+12,y+22,c_orange);call txt(l2,px+12,y+42,c_purple)
  call txt('Block   '//trim(num(tb,1))//' C',px+12,y+80,c_ink);call txt('Lid     '//trim(num(tlid,0))//' C',px+12,y+104,c_ink)
  call txt('Next: the lid housing opens and the',px+12,y+150,c_cyan);call txt('chassis becomes a cut-away showing',px+12,y+170,c_cyan)
  call txt('block, Peltiers, spreader, heat sink,',px+12,y+190,c_cyan);call txt('fan, optics modules and LED board.',px+12,y+210,c_cyan)
  if(cyc+flu<0)continue
 end subroutine
! ======================================================================= 2 3-D DNA + MD
! @nativefn NR2-THE-F0050
 subroutine scene_3d_dna(x,y,w,h,t,dt)
  ! 20 bp B-DNA (rise 3.4 A, 36 degrees per base pair) melting with temperature (two-state model,
  ! Tm from the nearest-neighbour calculation), primers annealing, polymerase extending, and
  ! dNTPs in thermal motion: Langevin velocity-Verlet MD with a soft repulsion, thermostat set by T.
  real(c_double),intent(in)::x,y,w,h,t,dt
  integer,parameter::nbp=20
  character(len=nbp),parameter::sq='ACGTTGCAGGTCAGTCAAGC'
  real(c_double)::tl,tc,fm,sep,ext,ann,tm,kin,px,yy,amp,p0(3),rr
  integer::i,k
  integer(c_int32_t)::cb1
  character(len=32)::stage
  tl=mod(t,24d0)
  if(tl<4)then
   tc=60;stage='Annealed template at 60 C'
  else if(tl<9)then
   tc=60+35*min(1d0,(tl-4)/1.5d0);stage='Heating to 95 C: denaturation'
  else if(tl<13)then
   tc=95-37*min(1d0,(tl-9)/1.5d0);stage='Cooling: primers anneal'
  else
   tc=72;stage='72 C: polymerase extends'
  end if
  tm=example_tm()
  fm=1/(1+exp((tm-tc)/1.8d0))           ! melted fraction, two-state
  sep=fm;if(tl>=13)sep=0   ! at 72 C the primer is held by the polymerase while it extends
  ann=0;if(tl>=10)ann=min(1d0,(tl-10)/2)
  ext=0;if(tl>=13)ext=min(1d0,(tl-13)/9)
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(14,10,18,255))
  call view_begin(x,y,w-th_side,h,.5d0+t*.25d0,-.15d0,merge(140d0,95d0,tl>=13)*merge(th_zoom,1d0,th_side<=0),1.0d0)
  ! MD solvent particles (dNTPs) first so the DNA is drawn in front where closer
  call md_step(tc,dt)
  do i=1,nmd
   call zsphere(mdx(:,i),.8d0,rgba(200,130,80,255))
  end do
  ! Real coordinates (PDB 8UCU): template strand (kind 3), primer strand with its terminal ddC
  ! (kinds 4, 6), incoming dGTP (5), Mg2+ (7) and the polymerase C-alpha trace (1, 2).
  ! Melting moves the primer strand away from the template along the line joining the two
  ! strand centres (melted fraction from the two-state model); every atom also jiggles with
  ! an amplitude that grows with temperature. At 72 C the polymerase is shown in its crystal
  ! position and the dGTP diffuses into the active site.
  if(.not.dna_ready)call dna_frame()
  amp=.25d0*sqrt((tc+273.15d0)/333.15d0)*(1+4*fm)
  do i=st_first(1),st_last(1)
   k=st_kind(i)
   p0=st_xyz(:,i)/10d0-dna_c
   select case(k)
   case(3)
    p0=p0-8*sep*dna_dir;rr=1.25d0;cb1=rgba(90,200,210,255)
   case(4,6)
    p0=p0+30*sep*dna_dir;rr=1.25d0;cb1=rgba(255,150,200,255);if(k==6)cb1=c_yellow
   case(5)
    if(tl<13.or.th_fault==26)cycle          ! case 26: the dNTP never reaches the active site
    p0=p0+(1-ext)*(36*dna_perp+10*dna_dir);rr=1.35d0;cb1=c_green
   case(7)
    if(tl<13)cycle
    rr=1.6d0;cb1=rgba(255,80,255,255)
   case default
    if(tl<13)cycle
    rr=1.9d0;cb1=mix(rgba(150,110,200,255),rgba(90,150,255,255),real(i-st_first(1),c_double)/(st_last(1)-st_first(1)))
    if(k==2)cb1=c_orange
   end select
   if(th_fault==23.and.(k==1.or.k==2))then  ! case 23: the polymerase loses its fold
    p0=p0+9*[sin(i*.71d0+t*.9d0),sin(i*1.13d0+t*.7d0),sin(i*.53d0+t*1.1d0)]
   end if
   p0=p0+amp*[sin(i*1.73d0+t*9.1d0),sin(i*2.31d0+t*8.3d0),sin(i*3.07d0+t*7.7d0)]
   call zsphere(p0,rr,cb1)
  end do
  if(th_fault==27.and.tl>=13)then             ! case 27: inhibitor molecules crowd the active site
   do i=1,14
    p0=kind_centre(1,5)-dna_c+7*[sin(i*2.1d0+t),cos(i*1.7d0+t*.8d0),sin(i*1.3d0-t*.6d0)]
    call zsphere(p0,2.2d0,rgba(235,60,60,255))
   end do
  end if
  ! panel
  kin=0
  do i=1,nmd
   kin=kin+.5d0*sum(mdv(:,i)**2)
  end do
  px=x+w-290;call set_clip(0d0,0d0,1d4,1d4)
  if(th_side<=0)then
   call set_clip(x,y,x+w,y+h)               ! stay inside the card
   call txt(trim(num(tc,0))//' C   melted '//trim(num(100*fm,0))//' %',x+10,y+h-24,c_cyan)
   call set_clip(0d0,0d0,1d4,1d4);return
  end if
  call box(px,y+10,280d0,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,280d0,h-20,c_purple,1d0)
  call txt('Primer / template DNA, PDB 8UCU',px+12,y+22,c_orange)
  call txt(trim(stage),px+12,y+50,c_cyan)
  call txt('Temperature '//trim(num(tc,1))//' C',px+12,y+78,c_ink)
  call txt('Example Tm  '//trim(num(tm,1))//' C (nearest-neighbour)',px+12,y+102,c_ink)
  call txt('Melted      '//trim(num(100*fm,0))//' % (two-state)',px+12,y+126,c_ink)
  call txt('dNTP kinetic energy '//trim(num(kin/nmd,2)),px+12,y+150,c_ink)
  call txt('(scales with absolute temperature)',px+12,y+170,c_purple)
  yy=y+210
  call txt('Atoms from the crystal structure:',px+12,yy,c_ink);call txt('template cyan, primer pink, ddC yellow,',px+12,yy+20,c_ink)
  call txt('dGTP green, Mg2+ magenta, polymerase',px+12,yy+40,c_ink);call txt('C-alpha violet / blue (interface orange).',px+12,yy+60,c_ink)
  call txt('MD: velocity Verlet, soft repulsion,',px+12,y+h-80,c_ink);call txt('Langevin thermostat at the set T.',px+12,y+h-60,c_ink)
  call txt('Illustration, not to scale in time.',px+12,y+h-36,c_orange)
 end subroutine
! @nativefn NR2-THE-F0143
 subroutine dna_frame()
  ! Centre of the 8UCU nucleic acids, unit vector from the template to the primer centre
  ! (direction in which the strands separate) and a perpendicular for the dGTP approach.
  real(c_double)::c3(3),c4(3),cn(3),v(3)
  integer::i,n3,n4,nn
  c3=0;c4=0;cn=0;n3=0;n4=0;nn=0
  do i=st_first(1),st_last(1)
   select case(st_kind(i))
   case(3)
    c3=c3+st_xyz(:,i)/10d0;n3=n3+1
   case(4,6)
    c4=c4+st_xyz(:,i)/10d0;n4=n4+1
   end select
   if(st_kind(i)>=3.and.st_kind(i)<=7)then
    cn=cn+st_xyz(:,i)/10d0;nn=nn+1
   end if
  end do
  c3=c3/max(1,n3);c4=c4/max(1,n4);dna_c=cn/max(1,nn)
  v=c4-c3;dna_dir=v/max(1d-9,sqrt(sum(v*v)))
  v=[dna_dir(2),-dna_dir(1),0d0];if(sum(v*v)<1d-6)v=[0d0,dna_dir(3),-dna_dir(2)]
  dna_perp=v/sqrt(sum(v*v));dna_ready=.true.
 end subroutine
! @nativefn NR2-THE-F0051
 subroutine md_step(tc,dt)
  ! Langevin dynamics in a 60 A box: soft pair repulsion, friction gamma, random kicks with
  ! variance 2*gamma*kT*dt (kT proportional to absolute temperature), periodic boundaries.
  real(c_double),intent(in)::tc,dt
  real(c_double)::kt,gam,h,d(3),r2,f,sig
  integer::i,j,s,nsub
  real(c_double),parameter::lbox=60
  if(.not.md_init)then
   do i=1,nmd
    mdx(:,i)=[urand(),urand(),urand()]*lbox-lbox/2;mdv(:,i)=[gauss(),gauss(),gauss()]
   end do
   md_init=.true.
  end if
  kt=(tc+273.15d0)/333.15d0;gam=1;h=.02d0;nsub=max(1,min(8,int(dt/.012d0)))
  do s=1,nsub
   mdf=0
   do i=1,nmd-1
    do j=i+1,nmd
     d=mdx(:,i)-mdx(:,j);d=d-lbox*nint(d/lbox);r2=sum(d*d)
     if(r2<36)then
      f=8*(1-sqrt(r2)/6)/max(.3d0,sqrt(r2));mdf(:,i)=mdf(:,i)+f*d;mdf(:,j)=mdf(:,j)-f*d
     end if
    end do
   end do
   sig=sqrt(2*gam*kt/h)
   do i=1,nmd
    mdv(:,i)=mdv(:,i)+h*(mdf(:,i)-gam*mdv(:,i)+sig*[gauss(),gauss(),gauss()])
    mdx(:,i)=mdx(:,i)+h*mdv(:,i)*6
    mdx(:,i)=mdx(:,i)-lbox*nint(mdx(:,i)/lbox)
   end do
  end do
 end subroutine
! ======================================================================= 3 electrical
! @nativefn NR2-THE-F0060
 subroutine wire(x0,y0,x1,y1)
  real(c_double),intent(in)::x0,y0,x1,y1
  call seg(x0,y0,x1,y1,c_wire,1.6d0)
 end subroutine
! @nativefn NR2-THE-F0061
 subroutine node(x0,y0)
  real(c_double),intent(in)::x0,y0
  call disc(x0,y0,3.2d0,c_wire,1d0)
 end subroutine
! @nativefn NR2-THE-F0062
 subroutine resistor(x0,y0,x1,y1,lab)
  ! IEEE zig-zag resistor between two points (horizontal or vertical), with leads.
  real(c_double),intent(in)::x0,y0,x1,y1
  character(len=*),intent(in)::lab
  real(c_double)::l,ux,uy,nx,ny,a,b,p0x,p0y,p1x,p1y
  integer::k
  l=sqrt((x1-x0)**2+(y1-y0)**2);ux=(x1-x0)/l;uy=(y1-y0)/l;nx=-uy;ny=ux
  a=l*.25d0;b=l*.75d0
  call wire(x0,y0,x0+ux*a,y0+uy*a);call wire(x0+ux*b,y0+uy*b,x1,y1)
  p0x=x0+ux*a;p0y=y0+uy*a
  do k=0,5
   p1x=x0+ux*(a+(b-a)*(k+.5d0)/6)+nx*7*merge(1,-1,mod(k,2)==0);p1y=y0+uy*(a+(b-a)*(k+.5d0)/6)+ny*7*merge(1,-1,mod(k,2)==0)
   call seg(p0x,p0y,p1x,p1y,c_wire,1.6d0);p0x=p1x;p0y=p1y
  end do
  call seg(p0x,p0y,x0+ux*b,y0+uy*b,c_wire,1.6d0)
  call txt(lab,(x0+x1)/2+nx*14-merge(0d0,4d0*len_trim(lab),abs(ux)<.5d0),(y0+y1)/2+ny*14-8,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0063
 subroutine capacitor(x0,y0,x1,y1,lab)
  real(c_double),intent(in)::x0,y0,x1,y1
  character(len=*),intent(in)::lab
  real(c_double)::l,ux,uy,nx,ny,mx,my
  l=sqrt((x1-x0)**2+(y1-y0)**2);ux=(x1-x0)/l;uy=(y1-y0)/l;nx=-uy;ny=ux;mx=(x0+x1)/2;my=(y0+y1)/2
  call wire(x0,y0,mx-ux*4,my-uy*4);call wire(mx+ux*4,my+uy*4,x1,y1)
  call seg(mx-ux*4-nx*11,my-uy*4-ny*11,mx-ux*4+nx*11,my-uy*4+ny*11,c_wire,2.2d0)
  call seg(mx+ux*4-nx*11,my+uy*4-ny*11,mx+ux*4+nx*11,my+uy*4+ny*11,c_wire,2.2d0)
  call txt(lab,mx+nx*16,my+ny*16-8,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0064
 subroutine ground(x0,y0)
  real(c_double),intent(in)::x0,y0
  call wire(x0,y0,x0,y0+8);call seg(x0-11,y0+8,x0+11,y0+8,c_wire,1.8d0)
  call seg(x0-7,y0+12,x0+7,y0+12,c_wire,1.6d0);call seg(x0-3,y0+16,x0+3,y0+16,c_wire,1.4d0)
 end subroutine
! @nativefn NR2-THE-F0065
 subroutine rail(x0,y0,lab)
  real(c_double),intent(in)::x0,y0
  character(len=*),intent(in)::lab
  call wire(x0,y0,x0,y0-10);call seg(x0-8,y0-10,x0+8,y0-10,c_wire,1.8d0);call txt(lab,x0-4*len_trim(lab),y0-30,c_orange)
 end subroutine
! @nativefn NR2-THE-F0066
 subroutine opamp(x0,y0,lab)
  ! Triangle pointing right; inputs at (x0, y0-12) inverting and (x0, y0+12) non-inverting,
  ! output at (x0+60, y0).
  real(c_double),intent(in)::x0,y0
  character(len=*),intent(in)::lab
  call seg(x0,y0-30,x0,y0+30,c_wire,1.8d0);call seg(x0,y0-30,x0+60,y0,c_wire,1.8d0);call seg(x0,y0+30,x0+60,y0,c_wire,1.8d0)
  call txt('-',x0+5,y0-21,c_ink);call txt('+',x0+5,y0+3,c_ink);call txt(lab,x0+12,y0+30,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0067
 subroutine diode(x0,y0,y1,lab,mode,lit)
  ! Vertical diode, anode at y0 (top), cathode at y1. mode 1 LED (arrows out), 2 photodiode (in).
  real(c_double),intent(in)::x0,y0,y1,lit
  character(len=*),intent(in)::lab
  integer,intent(in)::mode
  real(c_double)::m
  integer::k
  m=(y0+y1)/2
  call wire(x0,y0,x0,m-9);call wire(x0,m+9,x0,y1)
  call seg(x0-11,m-9,x0+11,m-9,c_wire,1.8d0);call seg(x0-11,m-9,x0,m+9,c_wire,1.8d0);call seg(x0+11,m-9,x0,m+9,c_wire,1.8d0)
  call seg(x0-11,m+9,x0+11,m+9,c_wire,2d0)
  do k=0,1
   if(mode==1)then
    call arrow(x0+14,m-4+k*9d0,x0+32,m+6+k*9d0,mix(c_wire,rgba(140,200,255,255),lit),1.4d0)
   else
    call arrow(x0+34,m-12+k*9d0,x0+16,m-2+k*9d0,mix(c_wire,c_green,lit),1.4d0)
   end if
  end do
  if(mode==1.and.lit>0)call glow(x0,m,26d0,rgba(140,200,255,255),.5d0*lit)
  call txt(lab,x0-14-7d0*len_trim(lab),m-8,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0068
 subroutine nmos(x0,y0,lab,on)
  ! N-channel MOSFET; gate lead at (x0-24,y0), drain at (x0+10,y0-26), source at (x0+10,y0+26).
  real(c_double),intent(in)::x0,y0
  character(len=*),intent(in)::lab
  logical,intent(in)::on
  integer(c_int32_t)::c
  c=c_wire;if(on)c=c_orange
  call wire(x0-24,y0,x0-6,y0);call seg(x0-6,y0-14,x0-6,y0+14,c,2d0);call seg(x0,y0-16,x0,y0+16,c,2.4d0)
  call wire(x0,y0-12,x0+10,y0-12);call wire(x0+10,y0-12,x0+10,y0-26)
  call wire(x0,y0+12,x0+10,y0+12);call wire(x0+10,y0+12,x0+10,y0+26)
  call arrow(x0+10,y0,x0+2,y0,c_wire,1.4d0);call wire(x0+10,y0,x0+10,y0+12)
  call txt(lab,x0+16,y0-8,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0069
 subroutine block(x0,y0,ww,hh,l1,l2)
  real(c_double),intent(in)::x0,y0,ww,hh
  character(len=*),intent(in)::l1,l2
  call box(x0,y0,ww,hh,rgba(38,26,40,255));call frame_rect(x0,y0,ww,hh,c_wire,1.6d0)
  call txt(l1,x0+8,y0+8,c_ink);call txt(l2,x0+8,y0+28,c_purple)
 end subroutine
! @nativefn NR2-THE-F0070
 subroutine flow(px,py,n,phase,c)
  ! Current shown as dots moving along a polyline (conventional current direction).
  integer,intent(in)::n
  real(c_double),intent(in)::px(n),py(n),phase
  integer(c_int32_t),intent(in)::c
  real(c_double)::tot,s,l,a
  integer::k
  tot=0
  do k=1,n-1
   tot=tot+sqrt((px(k+1)-px(k))**2+(py(k+1)-py(k))**2)
  end do
  s=mod(phase,18d0)
  do while(s<tot)
   a=s
   do k=1,n-1
    l=sqrt((px(k+1)-px(k))**2+(py(k+1)-py(k))**2)
    if(a<=l)then
     call disc(px(k)+(px(k+1)-px(k))*a/l,py(k)+(py(k+1)-py(k))*a/l,3.2d0,c,1d0);exit
    end if
    a=a-l
   end do
   s=s+18
  end do
 end subroutine
! @nativefn NR2-THE-F0071
 subroutine scene_circuit(x,y,w,h,t)
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::ox,oy,qx,qy,ph,led,heat,cool,lidp,sy0,sw,tt,v,fl
  integer::k,mux
  logical::rd
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(14,12,20,255))
  ! timing: 4 s thermal cycle; reading 2.5-3.6 s with LED chopped 8 on / 8 off samples
  ph=mod(t,4d0);rd=ph>2.5d0.and.ph<3.6d0
  led=0;if(rd.and.mod(int(t*20),2)==0)led=1
  mux=1+mod(int(t*12),16)
  heat=0;cool=0
  if(ph<1.6d0)then
   heat=1
  else if(ph<2.2d0)then
   cool=1
  else
   heat=.25d0
  end if
  lidp=0;if(mod(t,1d0)<.35d0)lidp=1
  fl=.5d0+.5d0*sin(t*.3d0)
  ! ---------------- sheet A: optical front end
  ox=x+30;oy=y+40
  call txt('A  Optical front end (one of 16 channels shown)',ox,oy-30,c_orange)
  call rail(ox+190,oy+30,'V_LED')
  call diode(ox+190,oy+30,oy+110,'D1 LED',1,led)
  call nmos(ox+180,oy+150,'Q1',led>0)
  call wire(ox+190,oy+110,ox+190,oy+124)
  call resistor(ox+190,oy+176,ox+190,oy+250,'Rs')
  call ground(ox+190,oy+250)
  call opamp(ox+60,oy+162,'U1 current regulator')
  call wire(ox+120,oy+162,ox+140,oy+162);call wire(ox+140,oy+162,ox+140,oy+150);call wire(ox+140,oy+150,ox+156,oy+150)
  call wire(ox+60,oy+174,ox+20,oy+174);call txt('LED_SET',ox-26,oy+180,c_purple)
  call wire(ox+60,oy+150,ox+40,oy+150);call wire(ox+40,oy+150,ox+40,oy+212);call wire(ox+40,oy+212,ox+190,oy+212)
  call node(ox+190,oy+212)
  if(led>0)call flow([ox+190,ox+190,ox+190,ox+190],[oy+20,oy+110,oy+176,oy+250],4,t*60,c_yellow)
  ! light path to the tube and photodiode
  call dash(ox+226,oy+80,ox+300,oy+80,mix(rgba(60,60,90,255),rgba(140,200,255,255),led),2d0,t*80)
  call frustum2d(ox+300,oy+60,oy+120)
  call dash(ox+330,oy+100,ox+372,oy+130,mix(rgba(40,70,50,255),c_green,led*fl),2d0,t*80)
  ! photodiode into the transimpedance amplifier
  call diode(ox+390,oy+210,oy+130,'PD1',2,led*fl)
  call ground(ox+390,oy+210)
  call wire(ox+390,oy+130,ox+430,oy+130);call node(ox+430,oy+130)
  call opamp(ox+450,oy+142,'U2 TIA')
  call wire(ox+430,oy+130,ox+450,oy+130)
  call wire(ox+450,oy+154,ox+440,oy+154);call wire(ox+440,oy+154,ox+440,oy+170);call ground(ox+440,oy+170)
  call wire(ox+430,oy+130,ox+430,oy+86);call resistor(ox+430,oy+86,ox+530,oy+86,'Rf')
  call wire(ox+430,oy+86,ox+430,oy+56);call capacitor(ox+430,oy+56,ox+530,oy+56,'Cf')
  call wire(ox+530,oy+56,ox+530,oy+142);call node(ox+530,oy+142);call node(ox+530,oy+86)
  call wire(ox+510,oy+142,ox+560,oy+142)
  call txt('TP2',ox+535,oy+148,c_yellow)
  if(led>0)call flow([ox+530,ox+530,ox+430,ox+430,ox+390],[oy+142,oy+86,oy+86,oy+130,oy+130],5,t*50,c_green)
  ! multiplexer, ADC, controller
  call block(ox+560,oy+100,70d0,150d0,'U3','MUX 16:1')
  do k=0,3
   call wire(ox+540,oy+180+k*16d0,ox+560,oy+180+k*16d0);call txt('ch'//trim(num(real(k*5+2,c_double),0)),ox+508,oy+172+k*16d0,c_purple)
  end do
  call txt('sel '//trim(num(real(mux,c_double),0)),ox+570,oy+226,c_yellow)
  call wire(ox+630,oy+150,ox+650,oy+150)
  call block(ox+650,oy+110,110d0,80d0,'U4 ADC','delta-sigma 24 b')
  call txt('(LTC2444-class)',ox+652,oy+196,c_purple)
  call block(ox+600,oy+300,160d0,70d0,'U5 MCU','LED / MUX / SPI')
  call wire(ox+705,oy+190,ox+705,oy+300);call txt('SPI',ox+712,oy+240,c_cyan)
  call wire(ox+620,oy+300,ox+620,oy+250);call txt('S0..S3',ox+560,oy+272,c_cyan)
  call wire(ox+600,oy+340,ox+20,oy+340);call wire(ox+20,oy+340,ox+20,oy+174)
  if(rd)then
   call flow([ox+560d0,ox+650d0],[oy+150,oy+150],2,t*70,c_yellow)
   call flow([ox+705d0,ox+705d0],[oy+190,oy+300],2,t*70,c_cyan)
  end if
  ! ---------------- sheet B: thermal power stage
  qx=x+w/2+60;qy=y+40
  call txt('B  Thermal power: H-bridge, Peltier, sensing, protection',qx-30,qy-30,c_orange)
  call rail(qx+40,qy+30,'V_PELT')
  call block(qx+20,qy+30,40d0,22d0,'F1','')
  call txt('fuse + thermal cut-out',qx+66,qy+34,c_purple)
  call wire(qx+40,qy+52,qx+40,qy+70);call wire(qx+40,qy+70,qx+260,qy+70)
  call nmos(qx+70,qy+110,'Q2',heat>0)
  call nmos(qx+230,qy+110,'Q3',cool>0)
  call nmos(qx+70,qy+230,'Q4',cool>0)
  call nmos(qx+230,qy+230,'Q5',heat>0)
  call wire(qx+80,qy+70,qx+80,qy+84);call wire(qx+240,qy+70,qx+240,qy+84)
  call wire(qx+80,qy+136,qx+80,qy+204);call wire(qx+240,qy+136,qx+240,qy+204)
  call wire(qx+80,qy+256,qx+80,qy+280);call wire(qx+240,qy+256,qx+240,qy+280);call wire(qx+80,qy+280,qx+240,qy+280)
  call ground(qx+160,qy+280)
  call node(qx+80,qy+170);call node(qx+240,qy+170)
  call wire(qx+80,qy+170,qx+125,qy+170);call wire(qx+195,qy+170,qx+240,qy+170)
  call box(qx+125,qy+156,70d0,28d0,mix(rgba(60,60,72,255),merge(rgba(240,90,60,255),rgba(80,150,255,255),heat>=cool),.6d0))
  call frame_rect(qx+125,qy+156,70d0,28d0,c_wire,1.6d0);call txt('TE1',qx+146,qy+162,c_ink)
  call txt('+',qx+116,qy+152,c_ink);call txt('-',qx+198,qy+152,c_ink)
  if(heat>0)then
   call flow([qx+40,qx+80,qx+80,qx+240,qx+240,qx+160],[qy+70,qy+70,qy+170,qy+170,qy+280,qy+280],6,t*55*heat,rgba(255,120,80,255))
   call txt('heating: Q2 + Q5 on',qx+100,qy+205,rgba(255,140,100,255))
  else if(cool>0)then
   call flow([qx+40,qx+240,qx+240,qx+80,qx+80,qx+160],[qy+70,qy+70,qy+170,qy+170,qy+280,qy+280],6,t*55,rgba(120,170,255,255))
   call txt('cooling: Q3 + Q4 on',qx+100,qy+205,rgba(140,180,255,255))
  end if
  ! thermistor divider into the ADC and the PID controller
  call rail(qx+380,qy+60,'V_REF')
  call resistor(qx+380,qy+60,qx+380,qy+140,'Rref')
  call node(qx+380,qy+140);call wire(qx+380,qy+140,qx+430,qy+140);call txt('to ADC',qx+436,qy+132,c_purple)
  call resistor(qx+380,qy+140,qx+380,qy+220,'RT1 NTC')
  call seg(qx+366,qy+196,qx+394,qy+164,c_wire,1.4d0)
  call ground(qx+380,qy+220)
  ! lid heater and fan, low-side switches
  call rail(qx+500,qy+60,'V_LID')
  call resistor(qx+500,qy+60,qx+500,qy+150,'R_lid')
  call nmos(qx+490,qy+180,'Q6',lidp>0);call wire(qx+500,qy+150,qx+500,qy+154);call ground(qx+500,qy+206)
  if(lidp>0)call flow([qx+500d0,qx+500d0],[qy+50,qy+206],2,t*55,c_yellow)
  call disc(qx+590,qy+110,18d0,rgba(38,26,40,255),1d0);call txt('M',qx+585,qy+102,c_ink);call txt('fan',qx+575,qy+132,c_purple)
  call block(qx+260,qy+320,260d0,60d0,'U6 PID controller','PWM to Q2..Q6, reads RT1')
  call wire(qx+46,qy+110,qx+30,qy+110);call wire(qx+30,qy+110,qx+30,qy+350);call wire(qx+30,qy+350,qx+260,qy+350)
  ! ---------------- oscilloscope traces
  sy0=y+h-200;sw=w-60
  call box(x+30,sy0,sw,185d0,rgba(10,16,12,255));call frame_rect(x+30,sy0,sw,185d0,c_green,1d0)
  do k=1,9
   call seg(x+30+k*sw/10,sy0,x+30+k*sw/10,sy0+185,rgba(25,45,30,255),1d0)
  end do
  call txt('TP1 LED current',x+38,sy0+4,c_yellow);call txt('TP2 TIA output',x+38,sy0+34,c_green)
  call txt('MUX select',x+38,sy0+64,c_cyan);call txt('ADC convert',x+38,sy0+94,c_purple)
  call txt('TE1 current (+heat / -cool)',x+38,sy0+124,rgba(255,140,100,255));call txt('Lid PWM',x+38,sy0+154,c_ink)
  do k=0,int(sw)-180,2
   tt=t-(sw-180-k)/(sw-180)*4
   call trace(x+210+k,sy0+24,merge(1d0,0d0,mod(tt,4d0)>2.5d0.and.mod(tt,4d0)<3.6d0.and.mod(int(tt*20),2)==0),c_yellow)
   v=0;if(mod(tt,4d0)>2.5d0.and.mod(tt,4d0)<3.6d0.and.mod(int(tt*20),2)==0)v=fl
   call trace(x+210+k,sy0+54,v,c_green)
   call trace(x+210+k,sy0+84,mod(int(tt*12),16)/15d0,c_cyan)
   call trace(x+210+k,sy0+114,merge(1d0,0d0,mod(tt*12,1d0)<.3d0),c_purple)
   v=.5d0
   if(mod(tt,4d0)<1.6d0)then
    v=1
   else if(mod(tt,4d0)<2.2d0)then
    v=0
   else
    v=.62d0
   end if
   call trace(x+210+k,sy0+144,v,rgba(255,140,100,255))
   call trace(x+210+k,sy0+174,merge(1d0,0d0,mod(tt,1d0)<.35d0),c_ink)
  end do
  call txt('Symbols and topology are generic teaching circuits; component values are symbolic.',x+w-640,y+h-224,c_orange)
 contains
  subroutine trace(px,base,lev,c)
   real(c_double),intent(in)::px,base,lev
   integer(c_int32_t),intent(in)::c
   call box(px,base-18*lev-1,2d0,2d0,c)
  end subroutine
  subroutine frustum2d(cx,ytop,ybot)
   real(c_double),intent(in)::cx,ytop,ybot
   call seg(cx-14,ytop,cx-6,ybot,c_wire,1.6d0);call seg(cx+14,ytop,cx+6,ybot,c_wire,1.6d0)
   call seg(cx-6,ybot,cx+6,ybot,c_wire,1.6d0);call box(cx-10,ytop+30,20d0,24d0,mix(rgba(40,90,60,255),c_green,led*fl))
   call txt('tube',cx-14,ybot+6,c_purple)
  end subroutine
 end subroutine
! ======================================================================= 4 heat equation
! @nativefn NR2-THE-F0080
 subroutine heat_setup()
  ! Cross-section along one strip. Rows from the bottom: heat sink (1-4, fixed 30 C at row 1),
  ! copper spreader (5-6), Peltier ceramic (7-8, source term), aluminium block (9-26) with 8
  ! liquid-filled wells (rows 14-34), air (27-40), heated lid (41-46, fixed 105 C).
  integer::i,j,wl
  hk=.02d0;hc=.5d0
  do j=1,hy
   do i=1,hx
    if(j<=4)then
     hk(i,j)=1.2d0;hc(i,j)=1
    else if(j<=6)then
     hk(i,j)=1.6d0;hc(i,j)=1
    else if(j<=8)then
     hk(i,j)=.25d0;hc(i,j)=1
    else if(j<=26)then
     hk(i,j)=1;hc(i,j)=1
    else if(j>=41)then
     hk(i,j)=1;hc(i,j)=1
    end if
    wl=(i-1)/18
    if(j>=14.and.j<=34.and.mod(i-1,18)>=5.and.mod(i-1,18)<=12.and.wl<8)then
     hk(i,j)=.12d0;hc(i,j)=1.7d0
     if(j>26)then
      hk(i,j)=.1d0
     end if
    end if
   end do
  end do
  ht=30;ht(:,41:hy)=105;hint=0;hn=0;heat_init=.true.
 end subroutine
! @nativefn NR2-THE-F0081
 subroutine scene_heat(x,y,w,h,t,dt)
  real(c_double),intent(in)::x,y,w,h,t,dt
  real(c_double)::sp,e,u(2),q,tn(hx,hy),dtau,cw,ch,gx,gy,tsens(2),px,py,pw,ph2,tl
  integer::i,j,s,k,zone
  if(.not.heat_init)call heat_setup()
  tl=mod(t,24d0)
  sp=60;if(mod(tl,12d0)<5)sp=95
  dtau=.12d0
  do s=1,120
   tsens(1)=ht(38,12);tsens(2)=ht(113,12)
   do k=1,2
    e=sp-tsens(k);u(k)=max(-1d0,min(1d0,.25d0*e+.004d0*hint))
   end do
   hint=max(-300d0,min(300d0,hint+(sp-.5d0*(tsens(1)+tsens(2)))*dtau))
   tn=ht
   do j=2,hy-1
    if(j>=41)cycle
    do i=1,hx
     q=0
     if(j==7.or.j==8)then
      zone=1;if(i>hx/2)zone=2
      q=u(zone)*14d0;if(j==7)q=-q
      q=q+abs(u(zone))*2d0
     end if
     tn(i,j)=ht(i,j)+dtau/hc(i,j)*( &
      kf(i,j,i+1,j)*(t_at(i+1,j)-ht(i,j))+kf(i,j,i-1,j)*(t_at(i-1,j)-ht(i,j))+ &
      kf(i,j,i,j+1)*(ht(i,j+1)-ht(i,j))+kf(i,j,i,j-1)*(ht(i,j-1)-ht(i,j))+q)
    end do
   end do
   tn(:,1)=30;ht=tn
  end do
  hn=min(600,hn+1)
  if(hn==600)hist(:,1:599)=hist(:,2:600)
  hist(:,hn)=[sp,ht(9*18/2+1,20),ht(4*18-9,20)]
  ! draw field
  call panel(x,y,w,h)
  gx=x+20;gy=y+40;cw=(w-40)/hx;ch=(h-260)/hy
  call txt('Temperature field (25 - 105 C), set point '//trim(num(sp,0))//' C; isotherms at 60 and 95 C',gx,y+12,c_orange)
  do j=1,hy
   do i=1,hx
    call box(gx+(i-1)*cw,gy+(hy-j)*ch,cw+.6d0,ch+.6d0,cmap((ht(i,j)-25)/80))
   end do
  end do
  do j=1,hy-1
   do i=1,hx-1
    if((ht(i,j)-60)*(ht(i+1,j)-60)<0.or.(ht(i,j)-60)*(ht(i,j+1)-60)<0)call disc(gx+(i-.5d0)*cw,gy+(hy-j+.5d0)*ch,1.2d0,c_ink,1d0)
    if((ht(i,j)-95)*(ht(i+1,j)-95)<0.or.(ht(i,j)-95)*(ht(i,j+1)-95)<0)call disc(gx+(i-.5d0)*cw,gy+(hy-j+.5d0)*ch,1.2d0,c_cyan,1d0)
   end do
  end do
  call txt('heated lid 105 C',gx+8,gy+2,c_dark);call txt('air',gx+8,gy+(hy-38)*ch,c_ink)
  call txt('wells (liquid)',gx+8*18*cw/2,gy+(hy-34)*ch-2,c_ink)
  call txt('aluminium block',gx+8,gy+(hy-20)*ch,c_dark);call txt('Peltier zone 1',gx+8,gy+(hy-8)*ch,c_ink)
  call txt('Peltier zone 2',gx+hx/2*cw+8,gy+(hy-8)*ch,c_ink);call txt('copper',gx+8,gy+(hy-6)*ch+2,c_dark)
  call txt('heat sink 30 C',gx+8,gy+(hy-3)*ch,c_dark)
  call disc(gx+37.5d0*cw,gy+(hy-12+.5d0)*ch,4d0,c_ink,1d0);call disc(gx+112.5d0*cw,gy+(hy-12+.5d0)*ch,4d0,c_ink,1d0)
  call txt('T1',gx+37.5d0*cw+6,gy+(hy-12)*ch-6,c_ink);call txt('T2',gx+112.5d0*cw+6,gy+(hy-12)*ch-6,c_ink)
  ! history plot
  px=x+20;py=y+h-200;pw=w-340;ph2=180
  call box(px,py,pw,ph2,rgba(16,11,18,255));call frame_rect(px,py,pw,ph2,c_purple,1d0)
  do k=2,hn
   call seg(px+(k-2)*pw/599,py+ph2-(hist(1,k-1)-20)/90*ph2,px+(k-1)*pw/599,py+ph2-(hist(1,k)-20)/90*ph2,c_ink,1d0)
   call seg(px+(k-2)*pw/599,py+ph2-(hist(2,k-1)-20)/90*ph2,px+(k-1)*pw/599,py+ph2-(hist(2,k)-20)/90*ph2,c_orange,1.6d0)
   call seg(px+(k-2)*pw/599,py+ph2-(hist(3,k-1)-20)/90*ph2,px+(k-1)*pw/599,py+ph2-(hist(3,k)-20)/90*ph2,c_cyan,1.6d0)
  end do
  call txt('set point',px+8,py+6,c_ink);call txt('centre well liquid',px+90,py+6,c_orange);call txt('end well liquid',px+250,py+6,c_cyan)
  px=x+w-310;call box(px,py,290d0,ph2,rgba(24,15,23,255));call frame_rect(px,py,290d0,ph2,c_purple,1d0)
  call txt('Explicit finite differences:',px+10,py+10,c_orange)
  call txt('c dT/dt = div(k grad T) + q',px+10,py+32,c_ink)
  call txt('Peltier: +q on the block side,',px+10,py+56,c_ink);call txt('-q on the sink side, plus Joule',px+10,py+76,c_ink)
  call txt('PI control on T1 / T2 per zone.',px+10,py+100,c_ink)
  call txt('Liquid lags the block: low k, high c.',px+10,py+124,c_cyan)
  call txt('Dimensionless teaching model.',px+10,py+152,c_orange)
 contains
  real(c_double) function t_at(i,j)
   integer,intent(in)::i,j
   t_at=ht(max(1,min(hx,i)),j)
  end function
  real(c_double) function kf(i,j,i2,j2)
   integer,intent(in)::i,j,i2,j2
   integer::a
   a=max(1,min(hx,i2))
   kf=2*hk(i,j)*hk(a,j2)/(hk(i,j)+hk(a,j2))
  end function
 end subroutine
! ======================================================================= 5 optics
! @nativefn NR2-THE-F0090
 real(c_double) function n_at(p,blue) result(n)
  ! Refractive index map of the section through one tube (units mm): LED epoxy dome, excitation
  ! filter slab, tube cap, tube walls (polypropylene), reaction liquid, emission filters.
  real(c_double),intent(in)::p(2)
  logical,intent(in)::blue
  real(c_double)::xl,xr,yy
  n=1
  if(sqrt(p(1)**2+(p(2)-40)**2)<3)n=1.5d0
  if(p(2)>=26.and.p(2)<=27.and.abs(p(1))<9)n=1.52d0
  if(p(2)>=21.and.p(2)<=22.5d0.and.abs(p(1))<6)n=1.49d0
  yy=p(2)
  if(yy>=3.and.yy<=21)then
   xl=-(2.5d0+2.5d0*(yy-3)/18);xr=-xl
   if(abs(p(1))<=xr+.8d0.and.abs(p(1))>=xr)n=1.49d0
   if(abs(p(1))<xr.and.yy<=15)n=1.33d0
  end if
  if(abs(abs(p(1))-9.5d0)<=.5d0.and.p(2)>=6.and.p(2)<=15)n=1.52d0
  if(.not.blue)continue
 end function
! @nativefn NR2-THE-F0091
 recursive subroutine trace_ray(p0,d0,blue,k)
  ! March the ray in small steps; refract (Snell) or reflect (total internal reflection)
  ! where the index changes; blue rays absorbed in the liquid spawn an isotropic green ray.
  real(c_double),intent(in)::p0(2),d0(2)
  logical,intent(in)::blue
  integer,intent(in)::k
  real(c_double)::p(2),d(2),n1,n2,nrm(2),ci,ct2,st,ds,tlen
  integer::s,np,k2
  p=p0;d=d0;np=1;rpts(:,1,k)=p;tlen=0;ds=.05d0
  if(blue)then
   rcol(k)=0;nexc=nexc+1
  else
   rcol(k)=1;nemit=nemit+1
  end if
  do s=1,1400
   n1=n_at(p,blue);p=p+ds*d;tlen=tlen+ds;n2=n_at(p,blue)
   if(abs(n2-n1)>1d-6)then
    nrm=grad_n(p,blue);ci=-sum(nrm*d)
    if(ci<0)then
     nrm=-nrm;ci=-ci
    end if
    st=n1/n2;ct2=1-st*st*(1-ci*ci)
    if(ct2<0)then
     d=d+2*ci*nrm;p=p+ds*d
    else
     d=st*d+(st*ci-sqrt(ct2))*nrm
    end if
    d=d/sqrt(sum(d*d))
    if(np<8)then
     np=np+1;rpts(:,np,k)=p
    end if
   end if
   ! filters: excitation passes blue only, emission filters pass green only
   if(p(2)>=26.and.p(2)<=27.and.abs(p(1))<9.and..not.blue)exit
   if(abs(abs(p(1))-9.5d0)<=.5d0.and.p(2)>=6.and.p(2)<=15.and.blue)exit
   if(blue.and.n2==1.33d0.and.urand()<.012d0)then
    if(np<8)np=np+1
    rpts(:,np,k)=p;rnp(k)=np;rlen(k)=tlen
    k2=1+mod(k,nray);call trace_ray(p,unit_dir(),.false.,k2);rnext=1+mod(k2,nray);return
   end if
   if(.not.blue.and.abs(p(1))>=10.and.p(2)>=6.and.p(2)<=15)then
    ndet=ndet+1;exit
   end if
   if(abs(p(1))>14.or.p(2)<-2.or.p(2)>44)exit
  end do
  if(np<8)np=np+1
  rpts(:,np,k)=p;rnp(k)=np;rlen(k)=tlen;rnext=1+mod(k,nray)
 end subroutine
! @nativefn NR2-THE-F0092
 function grad_n(p,blue) result(g)
  real(c_double),intent(in)::p(2)
  logical,intent(in)::blue
  real(c_double)::g(2),e
  e=.06d0
  g=[n_at(p+[e,0d0],blue)-n_at(p-[e,0d0],blue),n_at(p+[0d0,e],blue)-n_at(p-[0d0,e],blue)]
  if(sum(g*g)<1d-12)g=[0d0,1d0]
  g=g/sqrt(sum(g*g))
 end function
! @nativefn NR2-THE-F0093
 function unit_dir() result(d)
  real(c_double)::d(2),a
  a=2*pi*urand();d=[cos(a),sin(a)]
 end function
! @nativefn NR2-THE-F0094
 subroutine scene_optics(x,y,w,h,t)
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::s,ox,oy,a,px,py,pw,ph2,l,xl,xr,yy,ab,em,ld,fx
  integer::k,i,j,kk
  integer(c_int32_t)::c
  if(.not.opt_init)then
   rnp=0;rnext=1;ndet=0;nemit=0;nexc=0;opt_init=.true.
  end if
  do kk=1,4
   a=-pi/2+(urand()-.5d0)*1.0d0;k=rnext
   call trace_ray([ (urand()-.5d0)*1.2d0,39d0],[cos(a),sin(a)],.true.,k)
  end do
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(12,10,18,255))
  s=min((w*.5d0)/30,(h-60)/46);ox=x+20+15*s;oy=y+40
  call txt('Section through one tube (mm scale): LED, excitation filter, cap, wall, liquid, emission filters, photodiodes',x+16,y+12,c_orange)
  ! geometry
  call disc(ox,oy+(44-40)*s,3*s,rgba(70,90,140,255),.6d0);call txt('LED dome',ox+3.5d0*s,oy+3*s,c_ink)
  call box(ox-9*s,oy+(44-27)*s,18*s,1*s,rgba(70,110,255,255));call txt('Ex filter (blue)',ox+9.5d0*s,oy+16.5d0*s,c_ink)
  call box(ox-6*s,oy+(44-22.5d0)*s,12*s,1.5d0*s,rgba(200,200,215,255));call txt('cap',ox+6.5d0*s,oy+21.5d0*s,c_ink)
  do i=0,180
   yy=3+18*i/180d0;xr=2.5d0+2.5d0*(yy-3)/18
   call box(ox+xr*s,oy+(44-yy)*s,.8d0*s,18*s/180+.6d0,rgba(160,160,175,255))
   call box(ox-(xr+.8d0)*s,oy+(44-yy)*s,.8d0*s,18*s/180+.6d0,rgba(160,160,175,255))
   if(yy<=15)call box(ox-xr*s,oy+(44-yy)*s,2*xr*s,18*s/180+.6d0,rgba(30,60,50,255))
  end do
  call txt('liquid with FAM probe',ox-2.5d0*s,oy+30.5d0*s,c_green)
  do k=-1,1,2
   call box(ox+k*9.5d0*s-.5d0*s,oy+(44-15)*s,1*s,9*s,rgba(90,220,120,255))
   call box(ox+k*10.5d0*s-.5d0*s,oy+(44-15)*s,1*s,9*s,rgba(40,120,70,255))
  end do
  call txt('Em filter + photodiode',ox+11.2d0*s,oy+33*s,c_ink)
  ! rays
  do k=1,nray
   if(rnp(k)<2)cycle
   if(rcol(k)<.5d0)then
    c=rgba(120,170,255,255)
   else
    c=c_green
   end if
   do j=1,rnp(k)-1
    call seg(ox+rpts(1,j,k)*s,oy+(44-rpts(2,j,k))*s,ox+rpts(1,j+1,k)*s,oy+(44-rpts(2,j+1,k))*s,c,1d0)
   end do
  end do
  ! spectra panel
  px=x+w*.56d0;py=y+40;pw=w*.42d0;ph2=(h-80)*.48d0
  call box(px,py,pw,ph2,rgba(18,12,20,255));call frame_rect(px,py,pw,ph2,c_purple,1d0)
  call txt('Spectra 420-620 nm (approximate shapes for teaching)',px+8,py+6,c_orange)
  call box(px+(460-420)/200d0*pw,py+24,30/200d0*pw,ph2-44,rgba(30,40,90,255))
  call box(px+(510-420)/200d0*pw,py+24,30/200d0*pw,ph2-44,rgba(25,70,40,255))
  do i=0,199
   l=420+i
   ld=exp(-((l-470)/12)**2);ab=exp(-((l-494)/20)**2);em=exp(-((l-520)/16)**2)
   if(l>520)em=exp(-((l-520)/28)**2)
   fx=px+i/200d0*pw
   call disc(fx,py+ph2-20-ld*(ph2-50),1.4d0,rgba(120,170,255,255),1d0)
   call disc(fx,py+ph2-20-ab*(ph2-50),1.2d0,c_yellow,1d0)
   call disc(fx,py+ph2-20-em*(ph2-50),1.4d0,c_green,1d0)
  end do
  call txt('LED',px+(462-420)/200d0*pw,py+30,rgba(120,170,255,255));call txt('FAM absorbs',px+(488-420)/200d0*pw,py+48,c_yellow)
  call txt('FAM emits',px+(530-420)/200d0*pw,py+66,c_green)
  call txt('Ex band',px+(460-420)/200d0*pw,py+ph2-18,c_ink);call txt('Em band',px+(510-420)/200d0*pw,py+ph2-18,c_ink)
  ! FDTD wave panel
  call fdtd_panel(px,py+ph2+12,pw,(h-80)*.52d0-12,t)
  ! counters
  call txt('Excitation rays '//trim(num(real(nexc,c_double),0))//'   emitted '//trim(num(real(nemit,c_double),0))// &
   '   detected '//trim(num(real(ndet,c_double),0)),x+16,y+h-24,c_cyan)
 end subroutine
! @nativefn NR2-THE-F0095
 subroutine fdtd_panel(px,py,pw,ph2,t)
  ! 2-D TE wave (Ez, Hx, Hy) on a Yee grid: a 470 nm-like source in air meets the tube wall
  ! (n = 1.49) and the liquid (n = 1.33): refraction and partial reflection, live.
  real(c_double),intent(in)::px,py,pw,ph2,t
  integer,parameter::gx=120,gy=70
  real(c_double),save::ez(gx,gy)=0,hxf(gx,gy)=0,hyf(gx,gy)=0,eps(gx,gy)=1,tt=0
  logical,save::init=.false.
  integer::i,j,s
  real(c_double)::cw,ch
  if(.not.init)then
   do j=1,gy
    do i=1,gx
     if(i>55.and.i<=65)eps(i,j)=1.49d0**2
     if(i>65)eps(i,j)=1.33d0**2
    end do
   end do
   init=.true.
  end if
  do s=1,6
   tt=tt+1
   do j=1,gy-1
    do i=1,gx-1
     hxf(i,j)=hxf(i,j)-.5d0*(ez(i,j+1)-ez(i,j))
     hyf(i,j)=hyf(i,j)+.5d0*(ez(i+1,j)-ez(i,j))
    end do
   end do
   do j=2,gy-1
    do i=2,gx-1
     ez(i,j)=ez(i,j)+.5d0/eps(i,j)*((hyf(i,j)-hyf(i-1,j))-(hxf(i,j)-hxf(i,j-1)))
    end do
   end do
   do j=20,50
    ez(8,j)=sin(tt*.22d0)*exp(-((j-35)/10d0)**2)
   end do
   ez(1,:)=ez(2,:)*.5d0;ez(gx,:)=ez(gx-1,:)*.5d0;ez(:,1)=ez(:,2)*.5d0;ez(:,gy)=ez(:,gy-1)*.5d0
  end do
  call box(px,py,pw,ph2,rgba(10,10,16,255))
  cw=pw/gx;ch=(ph2-24)/gy
  do j=1,gy
   do i=1,gx
    if(abs(ez(i,j))>.02d0)then
     if(ez(i,j)>0)then
      call box(px+(i-1)*cw,py+22+(j-1)*ch,cw+.5d0,ch+.5d0,mix(rgba(10,10,16,255),rgba(120,170,255,255),min(1d0,ez(i,j))))
     else
      call box(px+(i-1)*cw,py+22+(j-1)*ch,cw+.5d0,ch+.5d0,mix(rgba(10,10,16,255),rgba(240,120,60,255),min(1d0,-ez(i,j))))
     end if
    end if
   end do
  end do
  call seg(px+55*cw,py+22,px+55*cw,py+ph2,c_ink,1d0);call seg(px+65*cw,py+22,px+65*cw,py+ph2,c_ink,1d0)
  call frame_rect(px,py,pw,ph2,c_purple,1d0)
  call txt('Wave view (FDTD, Yee grid): air | tube wall n 1.49 | liquid n 1.33',px+8,py+4,c_orange)
  if(t<0)continue
 end subroutine
! ======================================================================= 6 SPH fluid
! @nativefn NR2-THE-F0100
 subroutine wall_dist(p,dmin,nrm)
  ! Distance to the tube wall (V-shaped tube, mm) and the inward normal.
  real(c_double),intent(in)::p(2)
  real(c_double),intent(out)::dmin,nrm(2)
  real(c_double)::a(2),b(2),ab(2),tt,q(2),d,cands(2,2,3)
  integer::k
  cands(:,1,1)=[-5d0,20d0];cands(:,2,1)=[-2d0,2d0]
  cands(:,1,2)=[-2d0,2d0];cands(:,2,2)=[2d0,2d0]
  cands(:,1,3)=[2d0,2d0];cands(:,2,3)=[5d0,20d0]
  dmin=1d9;nrm=[0d0,1d0]
  do k=1,3
   a=cands(:,1,k);b=cands(:,2,k);ab=b-a
   tt=max(0d0,min(1d0,sum((p-a)*ab)/sum(ab*ab)));q=a+tt*ab;d=sqrt(sum((p-q)**2))
   if(d<dmin)then
    dmin=d;nrm=(p-q)/max(1d-9,d)
   end if
  end do
 end subroutine
! @nativefn NR2-THE-F0101
 subroutine sph_init_state()
  integer::i,k
  k=0
  do i=1,nsph-50
   k=k+1;spx(:,k)=[-1.8d0+mod(i-1,12)*.3d0+.05d0*urand(),2.6d0+((i-1)/12)*.3d0]
  end do
  do i=1,50
   k=k+1;spx(:,k)=[-4.0d0+mod(i-1,5)*.26d0,14.5d0+((i-1)/5)*.26d0]
  end do
  do i=1,nsph
   spv(:,i)=0;spt(i)=25
  end do
  sph_init=.true.
 end subroutine
! @nativefn NR2-THE-F0102
 subroutine sph_step(gmul,heat,dt)
  ! Weakly compressible SPH (Muller et al. 2003): poly6 density, spiky pressure gradient,
  ! viscosity Laplacian, gravity, Boussinesq buoyancy from the particle temperature, wall
  ! repulsion; heat conducts from the warm wall into the liquid.
  real(c_double),intent(in)::gmul,heat,dt
  real(c_double),parameter::hs=.55d0,mass=1,k0=40,rho0=14,mu=1.2d0
  real(c_double)::d(2),r,r2,w6,dw,lv,a,dm,nrm(2),tm
  integer::i,j,s
  real(c_double)::cpoly,cspik,cvisc,tnew(nsph)
  cpoly=4/(pi*hs**8);cspik=-30/(pi*hs**5);cvisc=40/(pi*hs**5)
  do s=1,4
   sprho=0
   do i=1,nsph
    do j=i,nsph
     d=spx(:,i)-spx(:,j);r2=sum(d*d)
     if(r2<hs*hs)then
      w6=mass*cpoly*(hs*hs-r2)**3;sprho(i)=sprho(i)+w6;if(j/=i)sprho(j)=sprho(j)+w6
     end if
    end do
   end do
   spp=k0*(sprho-rho0)
   tm=sum(spt)/nsph;spf=0;tnew=spt
   do i=1,nsph-1
    do j=i+1,nsph
     d=spx(:,i)-spx(:,j);r2=sum(d*d)
     if(r2<hs*hs.and.r2>1d-12)then
      r=sqrt(r2)
      dw=cspik*(hs-r)**2
      a=-mass*(spp(i)+spp(j))/(2*max(1d0,sprho(j)))*dw
      spf(:,i)=spf(:,i)+a*d/r;spf(:,j)=spf(:,j)-a*d/r
      lv=cvisc*(hs-r)*mu*mass
      spf(:,i)=spf(:,i)+lv*(spv(:,j)-spv(:,i))/max(1d0,sprho(j))
      spf(:,j)=spf(:,j)+lv*(spv(:,i)-spv(:,j))/max(1d0,sprho(i))
      tnew(i)=tnew(i)+.02d0*(spt(j)-spt(i));tnew(j)=tnew(j)+.02d0*(spt(i)-spt(j))
     end if
    end do
   end do
   spt=tnew
   do i=1,nsph
    a=max(1d0,sprho(i))
    spf(:,i)=spf(:,i)/a+[0d0,-9.8d0*gmul]+[0d0,.35d0*(spt(i)-tm)*heat]
    call wall_dist(spx(:,i),dm,nrm)
    if(dm<.25d0)then
     spf(:,i)=spf(:,i)+nrm*(.25d0-dm)*900
     spt(i)=spt(i)+(25+70*heat-spt(i))*.05d0
    end if
    spv(:,i)=spv(:,i)+dt*spf(:,i);spv(:,i)=spv(:,i)*.995d0
    spx(:,i)=spx(:,i)+dt*spv(:,i)
    call wall_dist(spx(:,i),dm,nrm)
    if(dm<.08d0.or.(spx(2,i)<2.and.abs(spx(1,i))<2))then
     spx(:,i)=spx(:,i)+nrm*(.08d0-dm);if(sum(spv(:,i)*nrm)<0)spv(:,i)=spv(:,i)-1.5d0*sum(spv(:,i)*nrm)*nrm
    end if
    if(abs(spx(1,i))>2+3*(spx(2,i)-2)/18+.01d0.and.spx(2,i)>2)then
     spx(1,i)=sign(2+3*(spx(2,i)-2)/18,spx(1,i));spv(1,i)=-.3d0*spv(1,i)
    end if
    if(spx(2,i)<2.1d0)then
     spx(2,i)=2.1d0;spv(2,i)=abs(spv(2,i))*.3d0
    end if
   end do
  end do
 end subroutine
! @nativefn NR2-THE-F0103
 subroutine scene_sph(x,y,w,h,t,dt)
  real(c_double),intent(in)::x,y,w,h,t,dt
  real(c_double)::tl,gm,ht2,s,ox,oy,vmag,px,adh
  integer::i
  character(len=60)::stage
  real(c_double),save::tlast=-1
  if(.not.sph_init)call sph_init_state()
  tl=mod(t,36d0)
  if(tl<tlast)call sph_init_state()
  tlast=tl
  gm=1;ht2=0;stage='After pipetting: a droplet clings to the wall'
  if(tl>=6.and.tl<12)then
   gm=6;stage='Spin-down: the droplet slides and merges'
  else if(tl>=12)then
   ht2=1;stage='Heating from the walls: convection mixes the liquid'
  end if
  adh=0;if(tl<6)adh=1
  if(adh>0)then
   do i=nsph-49,nsph
    spv(:,i)=spv(:,i)*.6d0
   end do
  end if
  call sph_step(gm,ht2,min(.004d0,dt/8))
  if(adh>0)then
   do i=nsph-49,nsph
    spv(2,i)=max(spv(2,i),-.05d0)
   end do
  end if
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(12,10,18,255))
  s=min((w-360)/12,(h-80)/22);ox=x+(w-320)/2;oy=y+40
  call seg(ox-5*s,oy+(22-20)*s,ox-2*s,oy+(22-2)*s,c_wire,3d0);call seg(ox-2*s,oy+(22-2)*s,ox+2*s,oy+(22-2)*s,c_wire,3d0)
  call seg(ox+2*s,oy+(22-2)*s,ox+5*s,oy+(22-20)*s,c_wire,3d0)
  do i=1,nsph
   vmag=sqrt(sum(spv(:,i)**2))
   call disc(ox+spx(1,i)*s,oy+(22-spx(2,i))*s,.2d0*s,mix(rgba(60,120,220,255),rgba(255,120,60,255),(spt(i)-25)/70),1d0)
   if(vmag>.6d0)call glow(ox+spx(1,i)*s,oy+(22-spx(2,i))*s,.35d0*s,c_ink,min(.4d0,vmag/10))
  end do
  px=x+w-310;call set_clip(0d0,0d0,1d4,1d4)
  call box(px,y+10,300d0,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,300d0,h-20,c_purple,1d0)
  call txt('Smoothed-particle hydrodynamics',px+12,y+22,c_orange)
  call txt(trim(stage),px+12,y+50,c_cyan)
  call txt(trim(num(real(nsph,c_double),0))//' particles, poly6 / spiky /',px+12,y+86,c_ink)
  call txt('viscosity kernels; walls repel.',px+12,y+106,c_ink)
  call txt('Colour: blue cool -> orange warm.',px+12,y+136,c_ink)
  call txt('Gravity x'//trim(num(gm,0))//' during the spin.',px+12,y+160,c_ink)
  call txt('Buoyancy (Boussinesq) drives',px+12,y+190,c_ink);call txt('convection when the walls heat.',px+12,y+210,c_ink)
  call txt('Why it matters: droplets left on',px+12,y+250,c_purple);call txt('the wall join late (case 45);',px+12,y+270,c_purple)
  call txt('unspun tubes give spikes (case 46).',px+12,y+290,c_purple)
  call txt('Teaching model, not to scale.',px+12,y+h-36,c_orange)
 end subroutine
! ======================================================================= 7 enzyme kinetics
! @nativefn NR2-THE-F0110
 subroutine enz_setup()
  integer::b,i
  do b=1,nen
   do i=1,ne
    ex(:,i,b)=[urand(),urand()];ev(:,i,b)=0;estate(i,b)=0;etimer(i,b)=0
   end do
   do i=1,ns
    sx(:,i,b)=[urand(),urand()];sstate(i,b)=0
   end do
   eprod(b)=0
  end do
  do i=1,nin
   ix(:,i)=[urand(),urand()];istate(i)=0
  end do
  nehist=0;enz_init=.true.
 end subroutine
! @nativefn NR2-THE-F0111
 subroutine scene_enzyme(x,y,w,h,t)
  ! Two identical reaction boxes: E + S <-> ES -> E + P; the right box also contains a
  ! competitive inhibitor I (E + I <-> EI). Brownian motion; binding on contact.
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::bx,by,bw,bh,d2,px,py,pw,ph2,km,ki,vmax,ss,v0,v1,ii
  integer::b,i,j,k
  real(c_double),save::tlast=-1
  if(.not.enz_init)call enz_setup()
  if(mod(t,40d0)<tlast)call enz_setup()
  tlast=mod(t,40d0)
  do b=1,nen
   do i=1,ne
    ex(:,i,b)=modulo(ex(:,i,b)+.004d0*[gauss(),gauss()],1d0)
    if(estate(i,b)==1)then
     etimer(i,b)=etimer(i,b)-1
     if(etimer(i,b)<=0)then
      estate(i,b)=0;eprod(b)=eprod(b)+1
     end if
    else if(estate(i,b)==2)then
     if(urand()<.01d0)then
      estate(i,b)=0
      do k=1,nin
       if(istate(k)==i)then
        istate(k)=0;exit
       end if
      end do
     end if
    end if
   end do
   do j=1,ns
    if(sstate(j,b)/=0)cycle
    sx(:,j,b)=modulo(sx(:,j,b)+.012d0*[gauss(),gauss()],1d0)
    do i=1,ne
     if(estate(i,b)/=0)cycle
     d2=sum((sx(:,j,b)-ex(:,i,b))**2)
     if(d2<.035d0**2.and.urand()<.5d0)then
      estate(i,b)=1;etimer(i,b)=25;sstate(j,b)=i;exit
     end if
    end do
   end do
   do j=1,ns
    if(sstate(j,b)>0)then
     if(estate(sstate(j,b),b)==0)then
      sstate(j,b)=-1
     else
      sx(:,j,b)=ex(:,sstate(j,b),b)
     end if
    else if(sstate(j,b)<0)then
     sx(:,j,b)=modulo(sx(:,j,b)+.012d0*[gauss(),gauss()],1d0)
    end if
   end do
  end do
  do k=1,nin
   if(istate(k)/=0)then
    ix(:,k)=ex(:,istate(k),2);cycle
   end if
   ix(:,k)=modulo(ix(:,k)+.012d0*[gauss(),gauss()],1d0)
   do i=1,ne
    if(estate(i,2)/=0)cycle
    if(sum((ix(:,k)-ex(:,i,2))**2)<.035d0**2.and.urand()<.5d0)then
     estate(i,2)=2;istate(k)=i;exit
    end if
   end do
  end do
  if(nehist<400)then
   nehist=nehist+1;ehist(:,nehist)=eprod
  end if
  call panel(x,y,w,h)
  bw=min((w-60)/2*.62d0,h-260);bh=bw
  do b=1,nen
   bx=x+20+(b-1)*(bw+30);by=y+50
   call box(bx,by,bw,bh,rgba(16,12,20,255));call frame_rect(bx,by,bw,bh,c_purple,1d0)
   if(b==1)then
    call txt('No inhibitor',bx+8,by-26,c_orange)
   else
    call txt('With competitive inhibitor (e.g. humic acid in a PCR)',bx+8,by-26,c_orange)
   end if
   do j=1,ns
    if(sstate(j,b)>0)cycle
    if(sstate(j,b)<0)then
     call disc(bx+sx(1,j,b)*bw,by+sx(2,j,b)*bh,3d0,c_green,1d0)
    else
     call disc(bx+sx(1,j,b)*bw,by+sx(2,j,b)*bh,3d0,c_yellow,1d0)
    end if
   end do
   if(b==2)then
    do k=1,nin
     call disc(bx+ix(1,k)*bw,by+ix(2,k)*bh,3.6d0,rgba(150,100,60,255),1d0)
    end do
   end if
   do i=1,ne
    if(estate(i,b)==1)then
     call ball(bx+ex(1,i,b)*bw,by+ex(2,i,b)*bh,10d0,c_orange)
    else if(estate(i,b)==2)then
     call ball(bx+ex(1,i,b)*bw,by+ex(2,i,b)*bh,10d0,rgba(120,100,110,255))
    else
     call ball(bx+ex(1,i,b)*bw,by+ex(2,i,b)*bh,10d0,rgba(90,140,255,255))
    end if
   end do
   call txt('product '//trim(num(real(eprod(b),c_double),0)),bx+8,by+bh+8,c_green)
  end do
  call txt('Enzyme free (blue), with substrate ES (orange), blocked EI (grey); substrate yellow, product green, inhibitor brown.', &
   x+20,y+60+bh+30,c_ink)
  ! product curves and Michaelis-Menten
  px=x+20;py=y+h-180;pw=(w-60)/2;ph2=160
  call box(px,py,pw,ph2,rgba(16,11,18,255));call frame_rect(px,py,pw,ph2,c_purple,1d0)
  call txt('Product formed over time',px+8,py+6,c_orange)
  do k=2,nehist
   call seg(px+(k-2)*pw/400,py+ph2-ehist(1,k-1)*(ph2-30)/ns,px+(k-1)*pw/400,py+ph2-ehist(1,k)*(ph2-30)/ns,c_green,1.6d0)
   call seg(px+(k-2)*pw/400,py+ph2-ehist(2,k-1)*(ph2-30)/ns,px+(k-1)*pw/400,py+ph2-ehist(2,k)*(ph2-30)/ns,rgba(150,100,60,255),1.6d0)
  end do
  px=x+40+pw;call box(px,py,pw,ph2,rgba(16,11,18,255));call frame_rect(px,py,pw,ph2,c_purple,1d0)
  call txt('v = Vmax [S] / (Km (1 + [I]/Ki) + [S])',px+8,py+6,c_orange)
  km=1;ki=.5d0;vmax=1;ii=1
  do k=0,199
   ss=k/20d0
   v0=vmax*ss/(km+ss);v1=vmax*ss/(km*(1+ii/ki)+ss)
   call disc(px+10+k*(pw-20)/200,py+ph2-10-v0*(ph2-40),1.4d0,c_green,1d0)
   call disc(px+10+k*(pw-20)/200,py+ph2-10-v1*(ph2-40),1.4d0,rgba(150,100,60,255),1d0)
  end do
  call txt('[S]',px+pw-30,py+ph2-24,c_ink);call txt('same Vmax, higher apparent Km',px+pw/2-90,py+ph2-44,c_cyan)
  call box(x+w-300,y+50,280d0,260d0,rgba(24,15,23,255));call frame_rect(x+w-300,y+50,280d0,260d0,c_purple,1d0)
  call txt('Why it matters in PCR:',x+w-290,y+60,c_orange)
  call txt('inhibitors carried over from the',x+w-290,y+86,c_ink);call txt('extraction occupy the polymerase,',x+w-290,y+106,c_ink)
  call txt('fewer copies per cycle, later Cq',x+w-290,y+126,c_ink);call txt('(scenario 27).',x+w-290,y+146,c_ink)
  call txt('Competitive: more substrate',x+w-290,y+180,c_cyan);call txt('restores the rate (same Vmax).',x+w-290,y+200,c_cyan)
  call txt('Stochastic particle model.',x+w-290,y+280,c_orange)
 end subroutine
! ======================================================================= 8 3-D surface
! @nativefn NR2-THE-F0120
 subroutine scene_surface(x,y,w,h,t)
  ! Fluorescence (height) by cycle and well, from the last preview / selected record / run.
  real(c_double),intent(in)::x,y,w,h,t
  type(curve_t)::r
  real(c_double)::lo,hi,z(45,24),a(3),b(3),c(3),d(3),xs,zs,ys,px,v
  integer::i,j,nw,nc,k
  character(len=60)::src
  r=curve_t()
  if(pv_state==3)then
   r=pv_fault;src='last case preview (fault run)'
  else if(nh>0)then
   r=curves_of(history(nh));src='last filed run'
  else if(running)then
   r=live_curves(current);src='run in progress'
  end if
  nw=0;nc=r%ncy
  do j=1,r%nw
   if(r%present(j)>0)nw=nw+1
  end do
  if(nc<2.or.nw<2)then
   src='idealised curves (no run yet)';nc=45;nw=14
   do j=1,nw
    do i=1,nc
     z(i,j)=1/(1+exp(-(i-(22d0+j*.9d0))/1.6d0))
     if(j==11.or.j==12)z(i,j)=0
    end do
   end do
  else
   lo=1d9;hi=-1d9;k=0
   do j=1,r%nw
    if(r%present(j)==0)cycle
    k=k+1
    do i=1,nc
     lo=min(lo,r%y(i,j));hi=max(hi,r%y(i,j))
    end do
   end do
   k=0
   do j=1,r%nw
    if(r%present(j)==0)cycle
    k=k+1
    do i=1,nc
     z(i,k)=(r%y(i,j)-lo)/max(1d0,hi-lo)
    end do
   end do
  end if
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(14,10,18,255))
  call view_begin(x,y,w-300,h,.7d0+.35d0*sin(t*.25d0),-.75d0,430d0,1.05d0)
  xs=200d0/(nc-1);zs=120d0/max(1,nw-1);ys=70
  do j=1,nw-1
   do i=1,nc-1
    a=[-100+(i-1)*xs,ys*z(i,j),-60+(j-1)*zs];b=[-100+i*xs,ys*z(i+1,j),-60+(j-1)*zs]
    c=[-100+i*xs,ys*z(i+1,j+1),-60+j*zs];d=[-100+(i-1)*xs,ys*z(i,j+1),-60+j*zs]
    v=(z(i,j)+z(i+1,j)+z(i+1,j+1)+z(i,j+1))/4
    call quad(a,b,c,d,cmap(v))
   end do
  end do
  call zline([-100d0,0d0,64d0],[104d0,0d0,64d0],c_ink,1.2d0)
  call zline([-104d0,0d0,-60d0],[-104d0,0d0,64d0],c_ink,1.2d0)
  call zline([-104d0,0d0,64d0],[-104d0,75d0,64d0],c_ink,1.2d0)
  call tag([104d0,0d0,64d0],'cycle 45',30d0,20d0)
  call tag([-100d0,0d0,64d0],'cycle 1',-30d0,24d0)
  call tag([-104d0,0d0,-60d0],'well 1',-40d0,-10d0)
  call tag([-104d0,75d0,64d0],'fluorescence',-30d0,-20d0)
  call molecule_inset(x+20,y+h-250,300d0,230d0,1,t,'amplicon duplex: each cycle copies it',.true.)
  px=x+w-290;call set_clip(0d0,0d0,1d4,1d4)
  call box(px,y+10,280d0,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,280d0,h-20,c_purple,1d0)
  call txt('Amplification surface',px+12,y+22,c_orange)
  call txt('Data: '//trim(src),px+12,y+50,c_cyan)
  call txt('Height and colour = normalised',px+12,y+78,c_ink);call txt('fluorescence; rows = wells.',px+12,y+98,c_ink)
  call txt('Flat rows: negative controls.',px+12,y+126,c_ink)
  call txt('Run a case or a batch, then',px+12,y+160,c_purple);call txt('come back to see its surface.',px+12,y+180,c_purple)
 end subroutine
! ======================================================================= molecules in 3-D scenes
! @nativefn NR2-THE-F0130
 subroutine draw_molecule(si,c0,sc,qm,only_na,emit_iface,stride)
  ! Places PDB structure si (native_structure) in the current 3-D view: centre c0, scale sc
  ! (world units per angstrom), rotation qm. only_na draws just the nucleic acids and ligands.
  integer,intent(in)::si
  real(c_double),intent(in)::c0(3),sc,qm(4)
  logical,intent(in)::only_na
  real(c_double),intent(in)::emit_iface
  integer,intent(in),optional::stride
  real(c_double),save::gc(3,8,2)=0
  logical,save::have(8,2)=.false.
  real(c_double)::p(3),r,acc(3)
  integer::i,k,kd,m,cnt,st
  integer(c_int32_t)::col
  m=1;if(only_na)m=2
  if(.not.have(si,m))then
   acc=0;cnt=0
   do i=st_first(si),st_last(si)
    kd=st_kind(i)
    if(only_na.and.(kd==1.or.kd==2.or.kd==8.or.kd==9))cycle
    acc=acc+st_xyz(:,i)/10d0;cnt=cnt+1
   end do
   gc(:,si,m)=acc/max(1,cnt);have(si,m)=.true.
  end if
  st=1;if(present(stride))st=stride
  do i=st_first(si),st_last(si),st
   kd=st_kind(i)
   if(only_na.and.(kd==1.or.kd==2.or.kd==8.or.kd==9))cycle
   select case(kd)
   case(1)
    r=1.9d0;col=mix(rgba(150,110,200,255),rgba(90,150,255,255),real(i-st_first(si),c_double)/max(1,st_last(si)-st_first(si)))
   case(2)
    r=2.1d0;col=c_orange
   case(3)
    r=1.05d0;col=c_cyan
   case(4)
    r=1.05d0;col=rgba(255,150,200,255)
   case(5)
    r=1.2d0;col=c_green
   case(6)
    r=1.2d0;col=c_yellow
   case(8)
    r=1.9d0;col=rgba(90,190,150,255)
   case(9)
    r=1.9d0;col=rgba(235,235,240,255)
   case default
    r=1.6d0;col=rgba(255,80,255,255)
   end select
   p=c0+sc*qrot(qm,st_xyz(:,i)/10d0-gc(:,si,m))
   if(kd==2)then
    call zsphere(p,r*sc,col,emit=emit_iface)
   else
    call zsphere(p,r*sc,col)
   end if
  end do
 end subroutine
! @nativefn NR2-THE-F0131
 subroutine molecule_inset(x,y,w,h,si,t,title,only_na)
  ! A magnified, rotating molecular view drawn in its own viewport (call after the main scene).
  real(c_double),intent(in)::x,y,w,h,t
  integer,intent(in)::si
  character(len=*),intent(in)::title
  logical,intent(in)::only_na
  real(c_double)::dd
  real(c_double)::u,g(3),act(3),tg(3)
  integer::k
  call set_clip(x,y,x+w,y+h);call box(x,y,w,h,rgba(18,12,22,255))
  u=.5d0-.5d0*cos(2*pi*mod(t,16d0)/16)
  dd=160;if(only_na)dd=90
  do k=1,3
   g(k)=sum(st_xyz(k,st_first(si):st_last(si)))/(10d0*(st_last(si)-st_first(si)+1))
  end do
  act=kind_centre(si,5);if(si>1)act=g
  tg=(act-g)*u;dd=dd*(1-.7d0*u)
  call view_begin(x,y+20,w,h-20,t*.4d0,-.25d0,dd,1.0d0)
  if(only_na)then
   call draw_molecule(si,[0d0,0d0,0d0],1d0,[1d0,0d0,0d0,0d0],only_na,.25d0)
  else
   call draw_molecule(si,-tg,1d0,[1d0,0d0,0d0,0d0],only_na,.25d0)
   if(u>.6d0)then
    call tag(act-g-tg,'dGTP',30d0,-24d0);call tag(kind_centre(si,7)-g-tg,'Mg2+',30d0,22d0)
   else
    call tag(kind_centre(si,3)-g-tg,'template',-40d0,30d0);call tag(kind_centre(si,1)-g-tg,'polymerase',40d0,-30d0)
   end if
  end if
  call set_clip(0d0,0d0,1d4,1d4);call frame_rect(x,y,w,h,c_orange,1.4d0);call txt(title,x+8,y+4,c_orange)
 end subroutine
! @nativefn NR2-THE-F0132
 function kind_centre(si,kd) result(c)
  ! Mean position (angstrom) of the atoms of one kind in structure si.
  integer,intent(in)::si,kd
  real(c_double)::c(3)
  real(c_double),save::kc(3,9,8)=0
  logical,save::kok(9,8)=.false.
  integer::i,n
  if(kok(kd,si))then
   c=kc(:,kd,si);return
  end if
  c=0;n=0
  do i=st_first(si),st_last(si)
   if(st_kind(i)==kd.or.(kd==1.and.st_kind(i)==2))then
    c=c+st_xyz(:,i)/10d0;n=n+1
   end if
  end do
  c=c/max(1,n);kc(:,kd,si)=c;kok(kd,si)=.true.
 end function
! @nativefn NR2-THE-F0140
 subroutine molecule_banner(x,y,w,h,t)
  ! Home-page banner: the four PDB structures carried by native_structure (real atomic
  ! coordinates, not a drawn helix), each turning slowly in its own depth-buffered view.
  ! Protein as C-alpha spheres, DNA strands as heavy atoms; PDB code in small print below.
  real(c_double),intent(in)::x,y,w,h,t
  character(len=4),parameter::ids(4)=['8UCU','1RFF','1A3Q','1RM1']
  real(c_double),save::rad(4)=0
  real(c_double)::cw,cx,g(3),d,ah
  integer::si,i,n
  if(rad(1)<=0)then
   do si=1,st_count
    g=0;n=0
    do i=st_first(si),st_last(si)
     g=g+st_xyz(:,i)/10d0;n=n+1
    end do
    g=g/max(1,n);d=0
    do i=st_first(si),st_last(si)
     d=max(d,sqrt(sum((st_xyz(:,i)/10d0-g)**2)))
    end do
    rad(si)=d
   end do
  end if
  cw=w/st_count;ah=h-18
  do si=1,st_count
   cx=x+(si-1)*cw
   call view_begin(cx+4,y,cw-8,ah,.35d0*t+1.6d0*si,-.18d0+.08d0*sin(.3d0*t+si),rad(si)/.53d0,1.0d0)
   call draw_molecule(si,[0d0,0d0,0d0],1d0,[1d0,0d0,0d0,0d0],.false.,0d0)
   call set_clip(0d0,0d0,1d4,1d4)
   call txt(ids(si),cx+cw/2-14,y+ah+2,rgba(150,135,170,255))
  end do
 end subroutine
! @nativefn NR2-THE-F0141
 subroutine molecule_icon(cx,cy,r,t)
  ! Logo mark: the template / primer DNA of PDB 8UCU with its incoming dGTP and Mg2+
  ! (real coordinates), turning inside a square of half-width r.
  real(c_double),intent(in)::cx,cy,r,t
  call view_begin(cx-r,cy-r,2*r,2*r,.6d0*t,-.2d0,62d0,1.0d0)
  call draw_molecule(1,[0d0,0d0,0d0],1d0,[1d0,0d0,0d0,0d0],.true.,0d0)
  call set_clip(0d0,0d0,1d4,1d4)
 end subroutine
! @nativefn NR2-THE-F0142
 subroutine molecule_stage(x,y,w,h,t)
  ! Start-up screen: PDB 8UCU (DNA polymerase on primer / template DNA with the incoming dGTP
  ! and Mg2+) filling the whole panel. The camera turns round the complex and moves in on the
  ! active site and back over a 20 s loop. Nothing is drawn over the molecule.
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::u,g(3),act(3),tg(3),dd
  integer::k
  call set_clip(x,y,x+w,y+h);call box(x,y,w,h,rgba(12,8,16,255))
  u=.5d0-.5d0*cos(2*pi*mod(t,20d0)/20)
  do k=1,3
   g(k)=sum(st_xyz(k,st_first(1):st_last(1)))/(10d0*(st_last(1)-st_first(1)+1))
  end do
  act=kind_centre(1,5);tg=(act-g)*u;dd=108*(1-.45d0*u)
  call view_begin(x,y,w,h,t*.25d0,-.22d0,dd,1.0d0)
  call draw_molecule(1,-tg,1d0,[1d0,0d0,0d0,0d0],.false.,.35d0)
  call set_clip(0d0,0d0,1d4,1d4)
 end subroutine
! ======================================================================= molecule viewer
! Full-screen viewer for the four PDB structures (home banner). Drag to rotate, wheel or the
! buttons to zoom, click two atoms to measure their distance. Analyses follow the protein-DNA
! interface approach of ProteinDNAInterfaceAnalysis (M. Joshi, MIT): Naccess interface
! residues from its output, plus contacts counted here as protein C-alpha atoms within a
! distance cut-off of any nucleic-acid atom, radius of gyration and composition.
! @nativefn NR2-THE-F0150
 subroutine mv_open(si)
  integer,intent(in)::si
  mv_si=max(1,min(st_count,si));mv_np=0;mv_pk=0
  call mv_prepare()
  mv_dist=mv_rmax*2.0d0;mv_yaw=.6d0;mv_pitch=-.25d0;mv_clip=0
 end subroutine
! @nativefn NR2-THE-F0151
 subroutine mv_prepare()
  ! Centre, radius of gyration, largest radius and, for every protein atom, the distance to
  ! the nearest nucleic-acid atom (angstrom).
  integer::i,j,n
  real(c_double)::d,q(3)
  if(mv_dsi==mv_si)return
  if(.not.allocated(mv_sx))allocate(mv_sx(st_n),mv_sy(st_n),mv_sz(st_n),mv_dna(st_n))
  mv_g=0;n=0
  do i=st_first(mv_si),st_last(mv_si)
   mv_g=mv_g+st_xyz(:,i)/10d0;n=n+1
  end do
  mv_g=mv_g/max(1,n);mv_rg=0;mv_rmax=0
  do i=st_first(mv_si),st_last(mv_si)
   d=sum((st_xyz(:,i)/10d0-mv_g)**2);mv_rg=mv_rg+d;mv_rmax=max(mv_rmax,sqrt(d))
  end do
  mv_rg=sqrt(mv_rg/max(1,n))
  do i=st_first(mv_si),st_last(mv_si)
   mv_dna(i)=1d9
   if(.not.is_prot(st_kind(i)))cycle
   q=st_xyz(:,i)/10d0
   do j=st_first(mv_si),st_last(mv_si)
    if(st_kind(j)/=3.and.st_kind(j)/=4)cycle
    d=sum((st_xyz(:,j)/10d0-q)**2)
    if(d<mv_dna(i))mv_dna(i)=d
   end do
   mv_dna(i)=sqrt(mv_dna(i))
  end do
  mv_dsi=mv_si
 end subroutine
! @nativefn NR2-THE-F0152
 logical function is_prot(k)
  integer,intent(in)::k
  is_prot=k==1.or.k==2.or.k==8.or.k==9
 end function
! @nativefn NR2-THE-F0153
 subroutine mv_tick(dt)
  real(c_double),intent(in)::dt
  if(mv_spin)mv_yaw=mv_yaw+.25d0*dt
 end subroutine
! @nativefn NR2-THE-F0154
 function mv_show_name() result(t)
  character(len=24)::t
  select case(mv_show)
  case(2)
   t='Show: protein'
  case(3)
   t='Show: DNA + ligands'
  case(4)
   t='Show: interface'
  case default
   t='Show: all'
  end select
 end function
! @nativefn NR2-THE-F0155
 function mv_col_name() result(t)
  character(len=24)::t
  select case(mv_col)
  case(2)
   t='Colour: chain order'
  case(3)
   t='Colour: DNA distance'
  case(4)
   t='Colour: contacts'
  case default
   t='Colour: components'
  end select
 end function
! @nativefn NR2-THE-F0156
 subroutine mv_draw(x,y,w,h)
  real(c_double),intent(in)::x,y,w,h
  character(len=4),parameter::ids(4)=['8UCU','1RFF','1A3Q','1RM1']
  character(len=72),parameter::names(4)=[character(len=72):: &
   'DNA polymerase alpha, primer / template DNA', &
   'Tyrosyl-DNA phosphodiesterase 1 with DNA', &
   'NF-kappaB p52 homodimer bound to DNA', &
   'TBP / TFIIA on TATA-box DNA']
  integer::i,k,n(9),ncon,nif,yy,j
  real(c_double)::p(3),r,sx,sy,d,px,f
  integer(c_int32_t)::col
  logical::shown
  call mv_prepare()
  call box(x,y,w,h,rgba(10,7,13,255))
  call view_begin(x,y,w-330,h,mv_yaw,mv_pitch,mv_dist,1.0d0)
  n=0;ncon=0;nif=0
  do i=st_first(mv_si),st_last(mv_si)
   k=st_kind(i);n(k)=n(k)+1
   if(is_prot(k).and.mv_dna(i)<=mv_cut)ncon=ncon+1
   if(k==2)nif=nif+1
   mv_sz(i)=-1
   select case(mv_show)
   case(2)
    shown=is_prot(k)
   case(3)
    shown=.not.is_prot(k)
   case(4)
    shown=.not.is_prot(k).or.mv_dna(i)<=mv_cut.or.k==2
   case default
    shown=.true.
   end select
   if(.not.shown)cycle
   if(is_prot(k))then
    r=1.9d0
   else if(k==7)then
    r=1.6d0
   else
    r=1.15d0
   end if
   col=comp_col(i,k)
   f=real(i-st_first(mv_si),c_double)/max(1,st_last(mv_si)-st_first(mv_si))
   select case(mv_col)
   case(2)
    if(is_prot(k))col=rainbow(f)
   case(3)
    if(is_prot(k))col=mix(rgba(255,90,70,255),rgba(70,110,230,255),min(1d0,max(0d0,(mv_dna(i)-4)/26)))
    if(.not.is_prot(k))col=rgba(235,235,240,255)
   case(4)
    if(is_prot(k))then
     col=rgba(95,90,110,255);if(mv_dna(i)<=mv_cut)col=c_orange
    end if
   end select
   if(any(mv_pk(1:mv_np)==i))col=rgba(255,255,120,255)
   p=st_xyz(:,i)/10d0-mv_g
   if(mv_clip>0)then                       ! slice: front clipping plane through the molecule
    call to_depth(p,d)
    if(d<mv_dist-mv_rmax+2*mv_rmax*mv_clip)cycle
   end if
   call zsphere(p,r,col)
   call screen_of(p,sx,sy);mv_sx(i)=sx;mv_sy(i)=sy
   call to_depth(p,mv_sz(i))
  end do
  if(mv_np==2)call zline(st_xyz(:,mv_pk(1))/10d0-mv_g,st_xyz(:,mv_pk(2))/10d0-mv_g,rgba(255,255,120,255),1.6d0)
  call set_clip(0d0,0d0,1d4,1d4)
  ! analysis panel
  px=x+w-320
  call box(px,y+8,312d0,h-16,rgba(24,15,23,255));call frame_rect(px,y+8,312d0,h-16,c_purple,1d0)
  call txt('PDB '//ids(mv_si),px+12,y+20,c_orange)
  call txt(trim(names(mv_si)),px+12,y+44,c_ink)
  yy=nint(y)+80
  call txt('Composition (atoms shown as spheres)',px+12,yy*1d0,c_cyan);yy=yy+24
  call txt('protein C-alpha  '//trim(inum(n(1)+n(2)+n(8)+n(9))),px+12,yy*1d0,c_ink);yy=yy+20
  call txt('nucleic-acid atoms '//trim(inum(n(3)+n(4))),px+12,yy*1d0,c_ink);yy=yy+20
  if(n(5)+n(6)+n(7)>0)then
   call txt('ligand / ion atoms '//trim(inum(n(5)+n(6)+n(7))),px+12,yy*1d0,c_ink);yy=yy+20
  end if
  yy=yy+10
  call txt('Shape',px+12,yy*1d0,c_cyan);yy=yy+24
  call txt('radius of gyration '//trim(num(mv_rg,1))//' A',px+12,yy*1d0,c_ink);yy=yy+20
  call txt('largest radius     '//trim(num(mv_rmax,1))//' A',px+12,yy*1d0,c_ink);yy=yy+30
  call txt('Protein-DNA interface',px+12,yy*1d0,c_cyan);yy=yy+24
  call txt('Naccess interface residues '//trim(inum(nif)),px+12,yy*1d0,c_ink);yy=yy+20
  call txt('C-alpha within '//trim(num(mv_cut,0))//' A of DNA: '//trim(inum(ncon)),px+12,yy*1d0,c_ink);yy=yy+20
  call txt('(cut-off with - / + buttons)',px+12,yy*1d0,c_purple);yy=yy+30
  call txt('Measure',px+12,yy*1d0,c_cyan);yy=yy+24
  if(mv_np==0)then
   call txt('Click an atom to select it.',px+12,yy*1d0,c_ink);yy=yy+20
  end if
  do j=1,mv_np
   call txt(trim(atom_name(mv_pk(j))),px+12,yy*1d0,rgba(255,255,120,255));yy=yy+20
  end do
  if(mv_np==2)then
   d=sqrt(sum((real(st_xyz(:,mv_pk(1)),c_double)-st_xyz(:,mv_pk(2)))**2))/10d0
   call txt('distance '//trim(num(d,1))//' A',px+12,yy*1d0,c_orange);yy=yy+20
  else if(mv_np==1)then
   if(is_prot(st_kind(mv_pk(1))))call txt('nearest DNA atom '//trim(num(mv_dna(mv_pk(1)),1))//' A',px+12,yy*1d0,c_ink)
   yy=yy+20
  end if
  if(mv_clip>0)call txt('Slice: front '//trim(num(100*mv_clip,0))//' % removed',px+12,y+h-144,c_orange)
  call txt('Drag: rotate   Wheel: zoom',px+12,y+h-124,c_purple)
  call txt('Click: select atom (two = distance)',px+12,y+h-104,c_purple)
  call txt('wwPDB data, CC0 1.0',px+12,y+h-84,c_purple)
 contains
  function inum(m) result(t)
   integer,intent(in)::m
   character(len=12)::t
   write(t,'(i0)')m
  end function
  function atom_name(i) result(t)
   integer,intent(in)::i
   character(len=60)::t
   select case(st_kind(i))
   case(1,2,8,9)
    t='protein C-alpha #'//trim(inum(i-st_first(mv_si)+1))
    if(st_kind(i)==2)t=trim(t)//' (interface)'
   case(3)
    t='DNA strand 1 atom #'//trim(inum(i-st_first(mv_si)+1))
   case(4)
    t='DNA strand 2 atom #'//trim(inum(i-st_first(mv_si)+1))
   case(5)
    t='dGTP atom'
   case(6)
    t='ddC / vanadate atom'
   case default
    t='Mg2+ ion'
   end select
  end function
  integer(c_int32_t) function rainbow(f)
   real(c_double),intent(in)::f
   if(f<.5d0)then
    rainbow=mix(rgba(70,110,230,255),rgba(80,210,120,255),f*2)
   else
    rainbow=mix(rgba(80,210,120,255),rgba(255,90,70,255),f*2-1)
   end if
  end function
  integer(c_int32_t) function comp_col(i,k)
   integer,intent(in)::i,k
   select case(k)
   case(1)
    comp_col=mix(rgba(150,110,200,255),rgba(90,150,255,255),real(i-st_first(mv_si),c_double)/max(1,st_last(mv_si)-st_first(mv_si)))
   case(2)
    comp_col=c_orange
   case(3)
    comp_col=c_cyan
   case(4)
    comp_col=rgba(255,150,200,255)
   case(5)
    comp_col=c_green
   case(6)
    comp_col=c_yellow
   case(8)
    comp_col=rgba(90,190,150,255)
   case(9)
    comp_col=rgba(235,235,240,255)
   case default
    comp_col=rgba(255,80,255,255)
   end select
  end function
 end subroutine
! @nativefn NR2-THE-F0157
 subroutine to_depth(p,z)
  real(c_double),intent(in)::p(3)
  real(c_double),intent(out)::z
  real(c_double)::c(3)
  call to_cam(p,c);z=c(3)
 end subroutine
! @nativefn NR2-THE-F0158
 subroutine mv_pick(x,y)
  ! Nearest drawn atom to the pointer (within 9 px, front-most wins); two picks give a distance.
  real(c_double),intent(in)::x,y
  integer::i,best
  real(c_double)::bz,d
  if(.not.allocated(mv_sx))return
  best=0;bz=1d30
  do i=st_first(mv_si),st_last(mv_si)
   if(mv_sz(i)<=0)cycle
   d=(mv_sx(i)-x)**2+(mv_sy(i)-y)**2
   if(d<81.and.mv_sz(i)<bz)then
    bz=mv_sz(i);best=i
   end if
  end do
  if(best==0)return
  if(mv_np>=2)mv_np=0
  mv_np=mv_np+1;mv_pk(mv_np)=best
 end subroutine
! @nativefn NR2-THE-F0159
 subroutine mv_clear()
  mv_np=0;mv_pk=0
 end subroutine
! @nativefn NR2-THE-F0160
 subroutine case_scene_3d(code,fam,caption,fault_on,x,y,w,h,t)
  ! 3-D view for the case page: the cut-away instrument (hardware, wells and data cases) or the
  ! real 8UCU primer / template DNA with the polymerase (reagent and molecular cases), drawn
  ! in the panel without the theatre side panels; the case caption is written on top.
  integer,intent(in)::code,fam
  character(len=*),intent(in)::caption
  logical,intent(in)::fault_on
  real(c_double),intent(in)::x,y,w,h,t
  th_side=0;th_fault=0;if(fault_on)th_fault=code
  if(fam==2)then
   call scene_3d_dna(x,y,w,h,t,.033d0)
  else
   call scene_3d_instrument(x,y,w,h,12+mod(t,40d0))
  end if
  th_side=300;th_fault=0
  call set_clip(x,y,x+w,y+h)                 ! captions stay inside the panel
  if(fault_on.and.code>0)then
   call txt(trim(caption),x+10,y+8,c_orange)
  else if(code>0)then
   call txt('Normal run (no fault)',x+10,y+8,c_cyan)
  end if
  call set_clip(0d0,0d0,1d4,1d4)
  call frame_rect(x,y,w,h,c_purple,1d0)
 end subroutine
! @nativefn NR2-THE-F0161
 subroutine cad_credit(x,y,w,h)
  ! Source of the instrument form, shown on every 3D instrument view.
  real(c_double),intent(in)::x,y,w,h
  call set_clip(0d0,0d0,1d4,1d4)
  call txt('Source: Chai Open qPCR [CAD model], GrabCAD Community Library',x+10,y+h-40,rgba(185,170,200,255))
  call txt('https://grabcad.com/library/chai-open-qpcr-1',x+10,y+h-22,rgba(185,170,200,255))
 end subroutine
! @nativefn NR2-THE-F0134
 subroutine theatre_warm()
  ! Start-up: structure centres for every structure and kind, and the simulation states.
  integer::si,kd
  real(c_double)::c(3)
  do si=1,st_count
   do kd=1,9
    c=kind_centre(si,kd)
   end do
  end do
  if(.not.md_init)call md_step(60d0,0d0)
  if(.not.heat_init)call heat_setup()
  if(.not.sph_init)call sph_init_state()
  if(.not.enz_init)call enz_setup()
  if(c(1)>1d30)continue
 end subroutine
! @nativefn NR2-THE-F0133
 subroutine scene_zoom(x,y,w,h,t)
  ! One continuous zoom (36 s): the tube (millimetres), the liquid crowded with complexes, one
  ! polymerase-DNA complex (nanometres), then the active site (angstroms) with every atom of the
  ! incoming dGTP, the ddC-terminated primer end, the template and the Mg2+ ion. Labelled.
  real(c_double),intent(in)::x,y,w,h,t
  real(c_double)::tl,u,dd,tgt(3),g(3),q(4),c0(3),pa(3),pb(3),a(3),b(3),rr,yaw,px,act(3)
  real(c_double),allocatable::sel(:,:)
  integer::i,j,k,n,kd
  integer(c_int32_t)::col
  character(len=48)::stage,scl
  tl=mod(t,36d0);yaw=.4d0+t*.12d0
  call panel(x,y,w,h);call box(x+1,y+1,w-2,h-2,rgba(12,9,16,255))
  q=[1d0,0d0,0d0,0d0]
  if(tl<6)then
   ! level 1: the tube, camera moving in
   u=tl/6;dd=260*(1-u)+70*u
   call view_begin(x,y,w-300,h,yaw,-.35d0,dd,1.1d0)
   call frustum([0d0,-40d0,0d0],[0d0,30d0,0d0],8d0,16d0,32,rgba(220,225,235,255),.false.)
   call frustum([0d0,30d0,0d0],[0d0,40d0,0d0],17d0,17d0,32,rgba(230,232,238,255),.true.)
   call frustum([0d0,-38d0,0d0],[0d0,-10d0,0d0],7.5d0,11d0,32,rgba(60,170,110,255),.true.,.3d0)
   call tag([16d0,30d0,0d0],'PCR tube',50d0,-30d0)
   call tag([11d0,-12d0,0d0],'reaction liquid (about 20 microlitres)',60d0,20d0)
   stage='1  The tube';scl='scale: millimetres'
  else if(tl<11)then
   ! level 2: inside the liquid, many complexes and DNA fragments in thermal motion
   u=(tl-6)/5;dd=200*(1-u)+120*u
   call view_begin(x,y,w-300,h,yaw,-.2d0,dd,1.0d0)
   do k=1,7
    c0=[70*sin(k*2.1d0+t*.05d0),45*cos(k*1.3d0+t*.04d0),60*sin(k*3.7d0)]
    call draw_molecule(1,c0,.35d0,qaxis([sin(k*1d0),cos(k*1d0),.3d0],t*.2d0+k),k>4,.2d0,2)
   end do
   call tag([70*sin(2.1d0+t*.05d0),45*cos(1.3d0+t*.04d0)+16,60*sin(3.7d0)],'polymerase bound to DNA',60d0,-40d0)
   call tag([70*sin(5*2.1d0+t*.05d0),45*cos(5*1.3d0+t*.04d0),60*sin(5*3.7d0)],'free DNA duplex',-60d0,40d0)
   stage='2  In the liquid';scl='scale: tens of nanometres'
  else
   ! levels 3-4: one complex, then the active site
   u=min(1d0,(tl-11)/16);u=u*u*(3-2*u)
   g=kind_centre(1,3)*0;act=kind_centre(1,5)
   do k=1,3
    g(k)=sum(st_xyz(k,st_first(1):st_last(1)))/(10d0*(st_last(1)-st_first(1)+1))
   end do
   tgt=g+(act-g)*u;dd=exp(log(190d0)*(1-u)+log(30d0)*u)
   call view_begin(x,y,w-300,h,yaw*.6d0,-.25d0,dd,1.0d0)
   c0=-qrot(q,tgt-g)
   if(u<.75d0)then
    call draw_molecule(1,c0,1d0,q,.false.,.3d0)
   else
    ! active-site close-up: atoms within 14 A of the dGTP, enlarged, with bonds
    n=0
    allocate(sel(4,st_last(1)-st_first(1)+1))
    do i=st_first(1),st_last(1)
     if(sqrt(sum((st_xyz(:,i)/10d0-act)**2))<14)then
      n=n+1;sel(1:3,n)=st_xyz(:,i)/10d0-g;sel(4,n)=st_kind(i)
     end if
    end do
    do i=1,n
     kd=nint(sel(4,i))
     do j=i+1,n
      if(nint(sel(4,j))/=kd.or.kd<3)cycle
      rr=sqrt(sum((sel(1:3,i)-sel(1:3,j))**2))
      if(rr<1.75d0)then
       a=c0+sel(1:3,i);b=c0+sel(1:3,j);call zline(a,b,rgba(200,200,210,255),1.6d0)
      end if
     end do
    end do
    do i=1,n
     kd=nint(sel(4,i));pa=c0+sel(1:3,i)
     select case(kd)
     case(1,2)
      col=merge(c_orange,rgba(120,140,230,255),kd==2);rr=1.1d0
     case(3)
      col=c_cyan;rr=.55d0
     case(4)
      col=rgba(255,150,200,255);rr=.55d0
     case(5)
      col=c_green;rr=.6d0
     case(6)
      col=c_yellow;rr=.6d0
     case default
      col=rgba(255,80,255,255);rr=.9d0
     end select
     call zsphere(pa,rr,col)
    end do
   end if
   ! labels
   if(u<.75d0)then
    call tag(c0+(kind_centre(1,1)-g),'DNA polymerase (C-alpha trace)',90d0,-60d0)
    call tag(c0+(kind_centre(1,2)-g),'residues touching DNA',-80d0,-50d0)
    call tag(c0+(kind_centre(1,3)-g),'template strand',-90d0,50d0)
    call tag(c0+(kind_centre(1,4)-g),'primer strand',90d0,60d0)
    call tag(c0+(act-g),'active site',70d0,-20d0)
    stage='3  One polymerase-DNA complex';scl='scale: nanometres (structure ~10 nm)'
   else
    call tag(c0+(act-g),'incoming dGTP',80d0,-50d0)
    call tag(c0+(kind_centre(1,6)-g),'primer 3-prime end (ddC)',-110d0,-40d0)
    call tag(c0+(kind_centre(1,7)-g),'Mg2+ ion',90d0,40d0)
    call tag(c0+(kind_centre(1,3)-g),'template base pairing',-100d0,60d0)
    stage='4  The active site';scl='scale: angstroms (atoms ~0.1 nm)'
   end if
  end if
  px=x+w-290;call set_clip(0d0,0d0,1d4,1d4)
  call box(px,y+10,280d0,h-20,rgba(24,15,23,255));call frame_rect(px,y+10,280d0,h-20,c_purple,1d0)
  call txt('Deep zoom',px+12,y+22,c_orange)
  call txt(trim(stage),px+12,y+50,c_cyan);call txt(trim(scl),px+12,y+74,c_ink)
  call txt('Real coordinates: PDB 8UCU',px+12,y+110,c_ink);call txt('(DNA polymerase alpha, cryo-EM).',px+12,y+130,c_ink)
  call txt('Every polymerase residue is one',px+12,y+160,c_ink);call txt('sphere (C-alpha); nucleic acids and',px+12,y+180,c_ink)
  call txt('ligands show every heavy atom.',px+12,y+200,c_ink)
  call txt('In the close-up: dGTP green,',px+12,y+236,c_purple);call txt('ddC yellow, template cyan,',px+12,y+256,c_purple)
  call txt('primer pink, Mg2+ magenta,',px+12,y+276,c_purple);call txt('DNA-contact residues orange.',px+12,y+296,c_purple)
  call txt('The PCR enzyme (Taq) differs;',px+12,y+h-60,c_orange);call txt('the chemistry of the site is shared.',px+12,y+h-40,c_orange)
 end subroutine
! ======================================================================= dispatcher
! @nativefn NR2-THE-F0030
 subroutine draw_theatre_scene(k,x,y,w,h,t,dt)
  integer,intent(in)::k
  real(c_double),intent(in)::x,y,w,h,t,dt
  select case(k)
  case(1)
   call scene_3d_instrument(x,y,w,h,t)
  case(2)
   call scene_3d_dna(x,y,w,h,t,dt)
  case(3)
   call scene_circuit(x,y,w,h,t)
  case(4)
   call scene_heat(x,y,w,h,t,dt)
  case(5)
   call scene_optics(x,y,w,h,t)
  case(6)
   call scene_sph(x,y,w,h,t,dt)
  case(7)
   call scene_enzyme(x,y,w,h,t)
  case(8)
   call scene_surface(x,y,w,h,t)
  case default
   call scene_zoom(x,y,w,h,t)
  end select
  call set_clip(0d0,0d0,1d4,1d4)
 end subroutine
end module
