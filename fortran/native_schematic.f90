! SPDX-License-Identifier: GPL-3.0-only
! MISSY R2 Fortran: electrical schematics of the cycler, animated with the values computed by the
! MNA circuit simulator (native_circuit). Two drawings per hardware case:
!  part 1 - thermal electronics: supply, H-bridge with flyback diodes and Peltier module, shunt
!           and current-sense amplifier, lid heater, fan driver, thermistor dividers, MCU (PI);
!  part 2 - optical electronics: LED constant-current driver, light path through the tube and
!           filters, photodiode transimpedance amplifier, gain stage, anti-alias filter, ADC.
! The component most likely at fault is outlined and probed with a meter that shows its live
! readings; a diagnosis panel lists the suspected part, probable causes and the measurements,
! and a scope trace shows the recent history. The drawing is laid out on a 1600 x 900 grid and
! scaled into the panel (full-screen view: 3840 x 2160 device pixels).
module native_schematic
 use iso_c_binding
 use r2_framebuffer
 use native_anim, only: box, disc, glow, seg, dash, arrow, frame_rect, txt, num, set_clip
 use native_circuit
 implicit none
 private
 public :: draw_schematic, schematic_part_title
 real(c_double),save :: ox=0,oy=0,ss=1,tt=0
 integer,save :: hot=0,cur_part=1
 integer(c_int32_t),save :: cw_,cl_,cv_,ck_,cr_,cg_,cp_,cb_
contains
 function schematic_part_title(part) result(s)
  integer,intent(in)::part
  character(len=60)::s
  if(part==1)then
   s='Thermal electronics schematic'
  else
   s='Optical electronics schematic'
  end if
 end function
! ------------------------------------------------------------------ scaled primitives
 real(c_double) function px(u)
  real(c_double),intent(in)::u
  px=ox+u*ss
 end function
 real(c_double) function py(v)
  real(c_double),intent(in)::v
  py=oy+v*ss
 end function
 subroutine ln(x0,y0,x1,y1,c)
  real(c_double),intent(in)::x0,y0,x1,y1
  integer(c_int32_t),intent(in)::c
  call seg(px(x0),py(y0),px(x1),py(y1),c,max(1d0,1.7d0*ss))
 end subroutine
 subroutine lnw(x0,y0,x1,y1,c,w)
  real(c_double),intent(in)::x0,y0,x1,y1,w
  integer(c_int32_t),intent(in)::c
  call seg(px(x0),py(y0),px(x1),py(y1),c,max(1d0,w*ss))
 end subroutine
 subroutine flow(x0,y0,x1,y1,amp)
  ! moving current dashes, speed and brightness from the current
  real(c_double),intent(in)::x0,y0,x1,y1,amp
  real(c_double)::a
  a=min(1d0,abs(amp))
  if(a<.02d0)return
  if(amp>0)then
   call dash(px(x0),py(y0),px(x1),py(y1),cg_,max(1.5d0,3*ss),tt*60*(.3d0+a))
  else
   call dash(px(x1),py(y1),px(x0),py(y0),cg_,max(1.5d0,3*ss),tt*60*(.3d0+a))
  end if
 end subroutine
 subroutine lbl(s,u,v,c)
  character(len=*),intent(in)::s
  real(c_double),intent(in)::u,v
  integer(c_int32_t),intent(in)::c
  if(ss<.55d0)return
  call txt(s,px(u),py(v),c)
 end subroutine
 subroutine tag(s,u,v,c)
  ! live value on a small dark plate
  character(len=*),intent(in)::s
  real(c_double),intent(in)::u,v
  integer(c_int32_t),intent(in)::c
  if(ss<.55d0)return
  call box(px(u)-3,py(v)-2,10.5d0*len_trim(s)+6,22d0,rgba(12,8,14,235))
  call txt(s,px(u),py(v),c)
 end subroutine
 subroutine dot(u,v)
  real(c_double),intent(in)::u,v
  call disc(px(u),py(v),max(1.5d0,4*ss),cw_,1d0)
 end subroutine
 subroutine res(x0,y0,x1,y1,c)
  ! zigzag resistor between two points (straight leads, six peaks in the middle half)
  real(c_double),intent(in)::x0,y0,x1,y1
  integer(c_int32_t),intent(in)::c
  real(c_double)::l,ux,uy,nx,ny,a,b,s0,s1
  integer::k
  l=sqrt((x1-x0)**2+(y1-y0)**2);ux=(x1-x0)/l;uy=(y1-y0)/l;nx=-uy;ny=ux
  a=.25d0*l;b=.75d0*l
  call ln(x0,y0,x0+ux*a,y0+uy*a,c);call ln(x0+ux*b,y0+uy*b,x1,y1,c)
  do k=0,5
   s0=a+(b-a)*k/6d0;s1=a+(b-a)*(k+1)/6d0
   call ln(x0+ux*s0+nx*merge(0d0,merge(10d0,-10d0,mod(k,2)==1),k==0),y0+uy*s0+ny*merge(0d0,merge(10d0,-10d0,mod(k,2)==1),k==0), &
    x0+ux*s1+nx*merge(0d0,merge(10d0,-10d0,mod(k,2)==0),k==5),y0+uy*s1+ny*merge(0d0,merge(10d0,-10d0,mod(k,2)==0),k==5),c)
  end do
 end subroutine
 subroutine cap(x0,y0,x1,y1,c)
  real(c_double),intent(in)::x0,y0,x1,y1
  integer(c_int32_t),intent(in)::c
  real(c_double)::l,ux,uy,nx,ny,m
  l=sqrt((x1-x0)**2+(y1-y0)**2);ux=(x1-x0)/l;uy=(y1-y0)/l;nx=-uy;ny=ux;m=l/2
  call ln(x0,y0,x0+ux*(m-5),y0+uy*(m-5),c);call ln(x0+ux*(m+5),y0+uy*(m+5),x1,y1,c)
  call lnw(x0+ux*(m-5)-nx*16,y0+uy*(m-5)-ny*16,x0+ux*(m-5)+nx*16,y0+uy*(m-5)+ny*16,c,2.6d0)
  call lnw(x0+ux*(m+5)-nx*16,y0+uy*(m+5)-ny*16,x0+ux*(m+5)+nx*16,y0+uy*(m+5)+ny*16,c,2.6d0)
 end subroutine
 subroutine dio(x0,y0,x1,y1,c,kind)
  ! diode anode (x0,y0) -> cathode (x1,y1); kind 0 plain, 1 LED (light out), 2 photodiode (in)
  real(c_double),intent(in)::x0,y0,x1,y1
  integer(c_int32_t),intent(in)::c
  integer,intent(in)::kind
  real(c_double)::l,ux,uy,nx,ny,m,h
  integer::k
  l=sqrt((x1-x0)**2+(y1-y0)**2);ux=(x1-x0)/l;uy=(y1-y0)/l;nx=-uy;ny=ux;m=l/2;h=12
  call ln(x0,y0,x0+ux*(m-h),y0+uy*(m-h),c);call ln(x0+ux*(m+h),y0+uy*(m+h),x1,y1,c)
  call ln(x0+ux*(m-h)+nx*h,y0+uy*(m-h)+ny*h,x0+ux*(m-h)-nx*h,y0+uy*(m-h)-ny*h,c)
  call ln(x0+ux*(m-h)+nx*h,y0+uy*(m-h)+ny*h,x0+ux*(m+h),y0+uy*(m+h),c)
  call ln(x0+ux*(m-h)-nx*h,y0+uy*(m-h)-ny*h,x0+ux*(m+h),y0+uy*(m+h),c)
  call lnw(x0+ux*(m+h)+nx*h,y0+uy*(m+h)+ny*h,x0+ux*(m+h)-nx*h,y0+uy*(m+h)-ny*h,c,2.4d0)
  do k=0,1
   if(kind==1)call arrow(px(x0+ux*(m-4+8*k)+nx*16),py(y0+uy*(m-4+8*k)+ny*16), &
    px(x0+ux*(m-4+8*k)+nx*16+(nx+ux)*16),py(y0+uy*(m-4+8*k)+ny*16+(ny+uy)*16),c,max(1d0,1.5d0*ss))
   if(kind==2)call arrow(px(x0+ux*(m-4+8*k)+nx*34+(nx+ux)*0),py(y0+uy*(m-4+8*k)+ny*34), &
    px(x0+ux*(m-4+8*k)+nx*16),py(y0+uy*(m-4+8*k)+ny*16),c,max(1d0,1.5d0*ss))
  end do
 end subroutine
 subroutine gnd(u,v)
  real(c_double),intent(in)::u,v
  call ln(u,v,u,v+10,cw_);call ln(u-14,v+10,u+14,v+10,cw_);call ln(u-9,v+15,u+9,v+15,cw_);call ln(u-4,v+20,u+4,v+20,cw_)
 end subroutine
 subroutine mos(u,v,on,name,flip)
  ! n-channel MOSFET, drain (u,v-40), source (u,v+40), gate lead to the left (or right if flip)
  real(c_double),intent(in)::u,v
  logical,intent(in)::on,flip
  character(len=*),intent(in)::name
  real(c_double)::sg
  integer(c_int32_t)::c
  sg=merge(-1d0,1d0,flip);c=merge(cg_,cl_,on)
  call ln(u,v-40,u,v-18,cw_);call ln(u,v-18,u-8*sg,v-18,cw_)
  call ln(u,v+40,u,v+18,cw_);call ln(u,v+18,u-8*sg,v+18,cw_)
  call lnw(u-8*sg,v-26,u-8*sg,v+26,c,3d0)
  call ln(u-16*sg,v-20,u-16*sg,v+20,cw_);call ln(u-16*sg,v,u-40*sg,v,cw_)
  call arrow(px(u),py(v+18),px(u-7*sg),py(v+18),cw_,max(1d0,1.4d0*ss))
  call lbl(name,u+8*sg-merge(34d0,0d0,flip),v-12,cl_)
  call disc(px(u-40*sg),py(v),max(2d0,5*ss),merge(cg_,cp_,on),1d0)
 end subroutine
 subroutine npn(u,v,name)
  ! bipolar NPN: base (u-40,v), collector (u+20,v-40), emitter (u+20,v+40)
  real(c_double),intent(in)::u,v
  character(len=*),intent(in)::name
  call frame_circle(u,v,30d0,cw_)
  call ln(u-40,v,u-8,v,cw_);call lnw(u-8,v-18,u-8,v+18,cw_,3d0)
  call ln(u-8,v-8,u+20,v-26,cw_);call ln(u+20,v-26,u+20,v-40,cw_)
  call ln(u-8,v+8,u+20,v+26,cw_);call ln(u+20,v+26,u+20,v+40,cw_)
  call arrow(px(u+4),py(v+16),px(u+18),py(v+25),cw_,max(1d0,1.4d0*ss))
  call lbl(name,u+30,v-10,cl_)
 end subroutine
 subroutine frame_circle(u,v,r,c)
  real(c_double),intent(in)::u,v,r
  integer(c_int32_t),intent(in)::c
  integer::k
  real(c_double)::a0,a1
  do k=0,23
   a0=k*2*3.141592653589793d0/24;a1=(k+1)*2*3.141592653589793d0/24
   call ln(u+r*cos(a0),v+r*sin(a0),u+r*cos(a1),v+r*sin(a1),c)
  end do
 end subroutine
 subroutine opamp(u,v,w,name,sat,plus_top)
  ! inputs at (u, v-w/4) and (u, v+w/4): inverting on top unless plus_top; output (u+w, v)
  real(c_double),intent(in)::u,v,w
  character(len=*),intent(in)::name
  logical,intent(in)::sat,plus_top
  integer(c_int32_t)::c
  c=merge(cr_,cw_,sat)
  call ln(u,v-w/2,u,v+w/2,c);call ln(u,v-w/2,u+w,v,c);call ln(u,v+w/2,u+w,v,c)
  call lbl(merge('+','-',plus_top),u+6,v-w/4-10,cl_);call lbl(merge('-','+',plus_top),u+6,v+w/4-10,cl_)
  call lbl(name,u+w*.28d0,v-w/2-22,cl_)
 end subroutine
 subroutine blk(u,v,w,h,title,c)
  real(c_double),intent(in)::u,v,w,h
  character(len=*),intent(in)::title
  integer(c_int32_t),intent(in)::c
  call box(px(u),py(v),w*ss,h*ss,rgba(30,20,34,255))
  call frame_rect(px(u),py(v),w*ss,h*ss,c,max(1d0,1.6d0*ss))
  call lbl(title,u+10,v+8,c)
 end subroutine
 subroutine highlight(u,v,w,h)
  ! suspected component: pulsing red outline and glow
  real(c_double),intent(in)::u,v,w,h
  real(c_double)::a
  a=.5d0+.5d0*sin(tt*5)
  call glow(px(u+w/2),py(v+h/2),max(w,h)*ss*.75d0,cr_,.18d0+.12d0*a)
  call frame_rect(px(u),py(v),w*ss,h*ss,cr_,max(1.5d0,(2+2*a)*ss))
  call lbl('SUSPECT',u,v-24,cr_)
 end subroutine
 subroutine meter(u0,v0,l1,l2,l3,pu1,pv1,pu2,pv2)
  ! hand-held meter with two probe leads to (pu1,pv1) red and (pu2,pv2) black; it sits in the
  ! free column on the right of the sheet (u0, v0 kept for reference only)
  real(c_double),intent(in)::u0,v0,pu1,pv1,pu2,pv2
  character(len=*),intent(in)::l1,l2,l3
  real(c_double)::u,v
  if(ss<.55d0)return
  u=1310;v=merge(330d0,60d0,cur_part==1)
  if(.false.)u=u0+v0
  call lnw(u+40,v+110,pu1,pv1,cr_,2.2d0);call lnw(u+120,v+110,pu2,pv2,rgba(90,90,90,255),2.2d0)
  call disc(px(pu1),py(pv1),5*ss,cr_,1d0);call disc(px(pu2),py(pv2),5*ss,rgba(150,150,150,255),1d0)
  call box(px(u),py(v),230*ss,110*ss,rgba(46,40,22,255))
  call frame_rect(px(u),py(v),230*ss,110*ss,rgba(230,200,90,255),max(1d0,2*ss))
  call box(px(u+8),py(v+8),214*ss,94*ss,rgba(150,170,120,255))
  call txt(trim(l1),px(u+14),py(v+12),rgba(20,26,16,255))
  call txt(trim(l2),px(u+14),py(v+40),rgba(20,26,16,255))
  call txt(trim(l3),px(u+14),py(v+68),rgba(20,26,16,255))
 end subroutine
 subroutine scope(u,v,w,h,k1,k2,lo,hi,title,c1,c2)
  ! scope trace of the recorded history (60 s loop), up to two channels
  real(c_double),intent(in)::u,v,w,h,lo,hi
  integer,intent(in)::k1,k2
  character(len=*),intent(in)::title
  integer(c_int32_t),intent(in)::c1,c2
  integer::i,n,j
  real(c_double)::x0,x1,y0,y1
  call box(px(u),py(v),w*ss,h*ss,rgba(8,16,12,255))
  call frame_rect(px(u),py(v),w*ss,h*ss,cl_,max(1d0,1.2d0*ss))
  do j=1,3
   call ln(u,v+h*j/4,u+w,v+h*j/4,rgba(30,52,40,255))
   call ln(u+w*j/4,v,u+w*j/4,v+h,rgba(30,52,40,255))
  end do
  n=min(nhist,int(ckt_tm/.2d0))
  do i=2,n
   x0=u+w*(i-2)/(nhist-1d0);x1=u+w*(i-1)/(nhist-1d0)
   y0=v+h-h*max(0d0,min(1d0,(hist(k1,i-1)-lo)/(hi-lo)));y1=v+h-h*max(0d0,min(1d0,(hist(k1,i)-lo)/(hi-lo)))
   call ln(x0,y0,x1,y1,c1)
   if(k2>0)then
    y0=v+h-h*max(0d0,min(1d0,(hist(k2,i-1)-lo)/(hi-lo)));y1=v+h-h*max(0d0,min(1d0,(hist(k2,i)-lo)/(hi-lo)))
    call ln(x0,y0,x1,y1,c2)
   end if
  end do
  call lbl(title,u+8,v+6,cl_)
 end subroutine
! ------------------------------------------------------------------ main entry
 subroutine draw_schematic(part,code,p,fault_on,x,y,w,h,t)
  integer,intent(in)::part,code
  real(c_double),intent(in)::p(6),x,y,w,h,t
  logical,intent(in)::fault_on
  integer::fp
  cw_=rgba(214,200,214,255);cl_=rgba(176,150,190,255);cv_=rgba(96,220,220,255);ck_=rgba(20,12,18,255)
  cr_=rgba(255,70,70,255);cg_=rgba(110,255,160,255);cp_=rgba(130,110,140,255);cb_=rgba(255,155,100,255)
  tt=t
  call ckt_advance(code,p,fault_on,t)
  call box(x,y,w,h,rgba(16,9,13,255))
  call set_clip(x,y,x+w,y+h)
  ss=min(w/1600d0,h/900d0);ox=x+(w-1600*ss)/2;oy=y+(h-900*ss)/2
  fp=ckt_fault_part(code);cur_part=part
  hot=0
  if(fault_on.and.ckt_short==0)hot=suspect(code)
  if(part==1)then
   call thermal_sheet()
  else
   call optical_sheet()
  end if
  call diagnosis(part,code,fault_on,fp)
  call set_clip(0d0,0d0,1d4,1d4)
 end subroutine
 function suspect_name(k) result(s)
  integer,intent(in)::k
  character(len=24)::s
  select case(k)
  case(1)
   s='TEC1 Peltier'
  case(2)
   s='NTC1 thermistor'
  case(3)
   s='lid heater'
  case(4)
   s='fan M1'
  case(5)
   s='lid switch S1'
  case(6)
   s='tube contact'
  case(7)
   s='block edge'
  case(11)
   s='LED driver'
  case(12)
   s='gain stage U3'
  case(14)
   s='optical path'
  case(15)
   s='PD1 / TIA'
  case(16)
   s='emission filter'
  case default
   s=''
  end select
 end function
 integer function suspect(code) result(k)
  ! 1 TEC1, 2 NTC1, 3 lid heater, 4 fan, 5 lid switch, 6 tube contact, 7 block edge,
  ! 11 LED driver, 12 gain stage, 14 optical path, 15 photodiode / TIA, 16 emission filter
  integer,intent(in)::code
  select case(code)
  case(1);k=7
  case(2);k=1
  case(3);k=3
  case(11,28);k=6
  case(12);k=2
  case(13);k=4
  case(14);k=5
  case(4,10,29,30);k=14
  case(5,17);k=16
  case(6,7);k=11
  case(8);k=12
  case(9);k=15
  case default;k=0
  end select
 end function
! ------------------------------------------------------------------ thermal sheet
 subroutine thermal_sheet()
  real(c_double)::ia,iled,dir,rn
  logical::q(6)
  integer::i
  do i=1,6
   q(i)=tr(i)==1
  end do
  ia=tq(20)/6;dir=0
  call lbl('THERMAL ELECTRONICS  -  H-bridge Peltier drive, sensing and PI control (simulated)',20d0,10d0,cb_)
  ! supply, fuse, bulk capacitor, 12 V bus
  call ln(60d0,110d0,60d0,160d0,cw_);call lnw(40d0,160d0,80d0,160d0,cw_,3d0);call ln(48d0,172d0,72d0,172d0,cw_);call ln(60d0,172d0,60d0,230d0,cw_);call gnd(60d0,230d0)
  call lbl('12 V',86d0,150d0,cl_);call lbl('PSU',86d0,172d0,cl_)
  call ln(60d0,110d0,110d0,110d0,cw_);call box(px(110d0),py(103d0),40*ss,14*ss,cp_);call lbl('F1',112d0,82d0,cl_);call ln(150d0,110d0,840d0,110d0,cw_)
  call dot(200d0,110d0);call cap(200d0,110d0,200d0,220d0,cw_);call gnd(200d0,220d0);call lbl('C1 470u',212d0,150d0,cl_)
  call tag('Vbus '//trim(num(tq(22),2))//' V',170d0,60d0,cv_)
  ! H-bridge legs (x=320 left, x=600 right)
  call dot(320d0,110d0);call dot(600d0,110d0)
  call ln(320d0,110d0,320d0,160d0,cw_);call mos(320d0,200d0,q(1),'Q1',.false.)
  call ln(320d0,240d0,320d0,330d0,cw_);call dot(320d0,290d0)
  call mos(320d0,370d0,q(3),'Q3',.false.);call ln(320d0,410d0,320d0,460d0,cw_)
  call ln(600d0,110d0,600d0,160d0,cw_);call mos(600d0,200d0,q(2),'Q2',.true.)
  call ln(600d0,240d0,600d0,330d0,cw_);call dot(600d0,290d0)
  call mos(600d0,370d0,q(4),'Q4',.true.);call ln(600d0,410d0,600d0,460d0,cw_)
  ! flyback diodes across each switch
  call ln(320d0,150d0,370d0,150d0,cw_);call ln(320d0,250d0,370d0,250d0,cw_);call dio(370d0,250d0,370d0,150d0,cl_,0)
  call ln(600d0,150d0,550d0,150d0,cw_);call ln(600d0,250d0,550d0,250d0,cw_);call dio(550d0,250d0,550d0,150d0,cl_,0)
  call ln(320d0,330d0,370d0,330d0,cw_);call ln(320d0,430d0,370d0,430d0,cw_);call dio(370d0,430d0,370d0,330d0,cl_,0)
  call ln(600d0,330d0,550d0,330d0,cw_);call ln(600d0,430d0,550d0,430d0,cw_);call dio(550d0,430d0,550d0,330d0,cl_,0)
  ! Peltier module between the bridge midpoints
  call ln(320d0,290d0,400d0,290d0,cw_);call ln(520d0,290d0,600d0,290d0,cw_)
  call box(px(400d0),py(268d0),120*ss,44*ss,rgba(60,40,70,255))
  do i=0,5
   call ln(410+i*18d0,270d0,422+i*18d0,310d0,merge(cb_,cv_,tq(20)>=0))
  end do
  call frame_rect(px(400d0),py(268d0),120*ss,44*ss,cw_,max(1d0,1.6d0*ss))
  call lbl('TEC1 Peltier',404d0,318d0,cl_)
  if(hot==1)call highlight(392d0,260d0,136d0,60d0)
  call tag(trim(num(tq(9),2))//' A  '//trim(num(tq(10),1))//' V',398d0,236d0,cv_)
  ! low side: shunt, ground, current-sense amplifier
  call ln(320d0,460d0,600d0,460d0,cw_);call dot(460d0,460d0);call res(460d0,460d0,460d0,540d0,cw_);call gnd(460d0,540d0)
  call lbl('Rsh 10m',472d0,486d0,cl_)
  call ln(460d0,460d0,460d0,470d0,cw_);call ln(460d0,470d0,640d0,470d0,cw_);call opamp(640d0,490d0,70d0,'U4 x50',.false.,.true.)
  call ln(640d0,507d0,630d0,507d0,cw_);call ln(630d0,507d0,630d0,560d0,cw_);call gnd(630d0,560d0)
  call ln(710d0,490d0,750d0,490d0,cw_);call res(750d0,490d0,820d0,490d0,cw_);call dot(820d0,490d0)
  call cap(820d0,490d0,820d0,560d0,cw_);call gnd(820d0,560d0);call ln(820d0,490d0,860d0,490d0,cw_)
  call tag('I '//trim(num(abs(tq(9)),2))//' A avg',720d0,530d0,cv_)
  ! current flow through the bridge
  if(q(1).and.q(4))then
   call flow(320d0,110d0,320d0,290d0,ia);call flow(320d0,290d0,600d0,290d0,ia);call flow(600d0,290d0,600d0,460d0,ia)
   call flow(600d0,460d0,460d0,460d0,ia)
  else if(q(2).and.q(3))then
   call flow(600d0,110d0,600d0,290d0,-ia);call flow(600d0,290d0,320d0,290d0,-ia);call flow(320d0,290d0,320d0,460d0,-ia)
   call flow(320d0,460d0,460d0,460d0,-ia)
  end if
  call lbl(merge('HEAT ','     ',tq(20)>.05d0)//merge('COOL','    ',tq(20)<-.05d0),420d0,206d0,merge(cb_,cv_,tq(20)>=0))
  ! gate driver
  call blk(80d0,300d0,150d0,130d0,'Gate driver',cl_)
  call lbl('PWM '//trim(num(100*abs(tq(8)),0))//'%',92d0,336d0,cv_)
  call lbl(merge('Q1+Q4','Q2+Q3',tq(8)>=0),92d0,362d0,cl_)
  call ln(230d0,330d0,280d0,330d0,cp_);call ln(280d0,330d0,280d0,200d0,cp_);call ln(230d0,370d0,280d0,370d0,cp_)
  call ln(230d0,400d0,250d0,400d0,cp_);call ln(250d0,400d0,250d0,600d0,cp_);call ln(250d0,600d0,640d0,600d0,cp_);call ln(640d0,600d0,640d0,200d0,cp_)
  ! lid heater (high-side switch Q5) and lid switch S1
  call dot(840d0,110d0);call ln(840d0,110d0,900d0,110d0,cw_)
  call ln(900d0,110d0,900d0,130d0,cw_)
  if(hot==5.and.ckt_tm>10)then
   call ln(900d0,130d0,926d0,102d0,cr_)
  else
   call ln(900d0,130d0,930d0,130d0,cw_)
  end if
  call ln(930d0,130d0,930d0,160d0,cw_);call lbl('S1 lid',940d0,112d0,cl_)
  if(hot==5)call highlight(888d0,100d0,60d0,70d0)
  call mos(930d0,200d0,q(5),'Q5',.true.);call ln(930d0,240d0,930d0,260d0,cw_)
  call res(930d0,260d0,930d0,360d0,cb_);call gnd(930d0,360d0);call lbl('Lid heater 8R',946d0,296d0,cl_)
  if(hot==3)call highlight(900d0,250d0,60d0,120d0)
  call tag(trim(num(tq(17),1))//' W',946d0,322d0,cv_)
  if(q(5).and..not.fuse_blown)call flow(930d0,240d0,930d0,360d0,.8d0)
  ! fan: inductance + motor + flyback diode + low-side switch Q6
  call ln(1040d0,110d0,1080d0,110d0,cw_);call ln(1000d0,110d0,1040d0,110d0,cw_)
  call ln(1080d0,110d0,1080d0,140d0,cw_);call frame_circle(1080d0,170d0,26d0,cw_);call lbl('M',1072d0,160d0,cw_)
  call ln(1080d0,196d0,1080d0,250d0,cw_);call ln(1080d0,140d0,1130d0,140d0,cw_);call ln(1080d0,250d0,1130d0,250d0,cw_)
  call dio(1130d0,250d0,1130d0,140d0,cl_,0)
  call mos(1080d0,290d0,q(6),'Q6',.true.);call ln(1080d0,330d0,1080d0,350d0,cw_);call gnd(1080d0,350d0)
  call lbl('Fan M1',1000d0,148d0,cl_)
  if(hot==4)call highlight(1040d0,130d0,110d0,140d0)
  call tag(trim(num(tq(14),0))//' rpm',990d0,380d0,merge(cr_,cv_,tq(14)<100))
  call tag(trim(num(1000*tq(15),0))//' mA',990d0,406d0,cv_)
  call flow(1080d0,110d0,1080d0,330d0,tq(15)*4)
  call ln(1000d0,110d0,900d0,110d0,cw_)
  ! thermistor dividers and ADC filters
  call lbl('3.3 V ref',40d0,640d0,cl_);call ln(60d0,665d0,60d0,690d0,cw_);call ln(60d0,690d0,360d0,690d0,cw_)
  call dot(120d0,690d0);call res(120d0,690d0,120d0,760d0,cw_);call dot(120d0,760d0)
  call res(120d0,760d0,120d0,840d0,cv_);call ln(100d0,820d0,140d0,780d0,cv_);call gnd(120d0,840d0)
  call lbl('R1 10k',132d0,700d0,cl_);call lbl('NTC1 block',132d0,804d0,cv_)
  if(hot==2)call highlight(92d0,750d0,140d0,100d0)
  call ln(120d0,760d0,170d0,760d0,cw_);call res(170d0,760d0,230d0,760d0,cw_);call dot(230d0,760d0);call cap(230d0,760d0,230d0,830d0,cw_)
  call gnd(230d0,830d0);call ln(230d0,760d0,560d0,760d0,cw_)
  rn=1d4*vt(10)/max(1d-6,3.3d0-vt(10))
  call tag(trim(num(vt(11),3))//' V',250d0,724d0,cv_)
  call dot(360d0,690d0);call res(360d0,690d0,360d0,760d0,cw_);call res(360d0,780d0,360d0,840d0,cv_)
  call ln(360d0,760d0,360d0,780d0,cw_);call ln(340d0,830d0,380d0,790d0,cv_);call gnd(360d0,840d0);call lbl('NTC2 lid',372d0,808d0,cv_)
  call ln(360d0,770d0,560d0,770d0,cw_)
  call ln(860d0,490d0,880d0,490d0,cw_);call ln(880d0,490d0,880d0,700d0,cw_);call ln(880d0,700d0,700d0,700d0,cw_)
  ! MCU
  call blk(560d0,640d0,330d0,240d0,'MCU   ADC 12 bit + PI',cv_)
  call lbl('ch0 NTC1 '//trim(num(tq(12),0)),576d0,676d0,cw_)
  call lbl('ch1 NTC2 '//trim(num(tq(24),0)),576d0,700d0,cw_)
  call lbl('ch2 Itec '//trim(num(tq(13),0)),576d0,724d0,cw_)
  call lbl('Tset  '//trim(num(tq(1),1))//' C',576d0,756d0,cb_)
  call lbl('Tmeas '//trim(num(tq(3),1))//' C',576d0,780d0,cb_)
  call lbl('duty  '//trim(num(100*tq(8),0))//' %',576d0,804d0,cb_)
  if(tq(18)>.5d0)then
   call lbl('INTERLOCK: bridge off',576d0,836d0,cr_)
  else
   call lbl('loop running',576d0,836d0,cg_)
  end if
  ! heat path: block, tubes, heat sink (labels with live temperatures)
  call blk(930d0,430d0,300d0,170d0,'Heat path',cl_)
  call lbl('Block  '//trim(num(tq(2),1))//' C',944d0,462d0,cw_)
  call lbl('Tube   '//trim(num(tq(6),1))//' C',944d0,488d0,cw_)
  call lbl('Edge   '//trim(num(tq(7),1))//' C',944d0,514d0,cw_)
  call lbl('Sink   '//trim(num(tq(4),1))//' C',944d0,540d0,merge(cr_,cw_,tq(4)>50))
  call lbl('Lid    '//trim(num(tq(5),1))//' C',944d0,566d0,cw_)
  if(hot==6.or.hot==7)call highlight(930d0,452d0,300d0,90d0)
  call scope(930d0,640d0,300d0,240d0,1,2,20d0,110d0,'Scope: Tset / Tblock',cb_,cg_)
  iled=0;dir=iled
  call thermal_extras()
 end subroutine
! ------------------------------------------------------------------ optical sheet
 subroutine optical_sheet()
  real(c_double)::a,sig
  logical::clip
  clip=oq(16)>.5d0;sig=min(1d0,oq(8))
  call lbl('OPTICAL ELECTRONICS  -  LED current driver, light path, photodiode TIA and ADC (simulated)',20d0,10d0,cb_)
  ! +5 V rail, LED, series resistor, NPN, sense resistor
  call lbl('+5 V',40d0,70d0,cl_);call ln(60d0,100d0,420d0,100d0,cw_)
  call dot(360d0,100d0);call res(360d0,100d0,360d0,170d0,cw_);call lbl('R 47',372d0,124d0,cl_)
  call dio(360d0,170d0,360d0,270d0,cb_,1);call lbl('D1 LED 470 nm',380d0,200d0,cb_)
  call glow(px(400d0),py(220d0),46*ss*min(1d0,oq(1)/.02d0),cv_,.35d0)
  call npn(340d0,320d0,'Q1');call ln(360d0,270d0,360d0,280d0,cw_)
  call dot(360d0,360d0);call res(360d0,370d0,360d0,450d0,cw_);call ln(360d0,360d0,360d0,370d0,cw_);call gnd(360d0,450d0)
  call lbl('Re 10R',372d0,396d0,cl_)
  call flow(360d0,100d0,360d0,360d0,oq(1)/.02d0)
  call tag(trim(num(1000*oq(1),1))//' mA',200d0,230d0,cv_)
  ! DAC and U1 (loop holds V(emitter) = V(DAC))
  call blk(40d0,270d0,90d0,60d0,'DAC',cl_);call lbl(trim(num(oq(2),3))//' V',48d0,300d0,cv_)
  call ln(130d0,310d0,180d0,310d0,cw_);call opamp(180d0,327d0,70d0,'U1',.false.,.true.)
  call ln(180d0,345d0,165d0,345d0,cw_);call ln(165d0,345d0,165d0,400d0,cw_)
  call ln(165d0,400d0,360d0,400d0,cw_);call dot(360d0,400d0)
  call ln(250d0,327d0,270d0,327d0,cw_);call res(270d0,320d0,300d0,320d0,cw_);call ln(250d0,327d0,270d0,320d0,cw_)
  call lbl('Rb 1k',262d0,290d0,cl_)
  if(hot==11)call highlight(30d0,160d0,420d0,300d0)
  ! light path: LED -> excitation filter -> tube -> emission filter -> photodiode
  call lnw(430d0,220d0,520d0,220d0,cv_,4d0*min(1d0,oq(1)/.02d0))
  call box(px(520d0),py(180d0),14*ss,80*ss,rgba(60,120,200,255));call lbl('Ex',514d0,266d0,cl_)
  call lnw(534d0,220d0,600d0,220d0,cv_,4d0*min(1d0,oq(1)/.02d0))
  call frame_rect(px(600d0),py(150d0),80*ss,150*ss,cw_,max(1d0,1.6d0*ss))
  a=sig
  call box(px(606d0),py(210d0),68*ss,84*ss,rgba(40+nint(80*a),120+nint(120*a),60+nint(40*a),255))
  call lbl('Tube',606d0,306d0,cl_);call lbl('sample',598d0,330d0,cl_)
  call lnw(680d0,240d0,760d0,240d0,cg_,1+4d0*sig)
  call box(px(760d0),py(200d0),14*ss,80*ss,rgba(60,180,90,255));call lbl('Em',754d0,286d0,cl_)
  call lnw(774d0,240d0,830d0,240d0,cg_,1+4d0*sig)
  if(hot==14)call highlight(510d0,140d0,280d0,210d0)
  if(hot==16)call highlight(746d0,190d0,44d0,104d0)
  ! photodiode + TIA (U2, Rf || Cf)
  call ln(850d0,260d0,850d0,300d0,cw_);call dio(850d0,420d0,850d0,300d0,cw_,2);call gnd(850d0,440d0);call ln(850d0,420d0,850d0,440d0,cw_)
  call lbl('PD1',870d0,400d0,cl_)
  call ln(850d0,300d0,950d0,300d0,cw_);call dot(900d0,300d0)
  call opamp(950d0,317d0,80d0,'U2 TIA',.false.,.false.);call ln(950d0,337d0,935d0,337d0,cw_);call ln(935d0,337d0,935d0,360d0,cw_);call gnd(935d0,360d0)
  call ln(900d0,300d0,900d0,200d0,cw_);call ln(900d0,200d0,940d0,200d0,cw_);call res(940d0,200d0,1020d0,200d0,cw_);call ln(1020d0,200d0,1060d0,200d0,cw_)
  call lbl('Rf 10M',948d0,168d0,cl_)
  call ln(900d0,200d0,900d0,140d0,cw_);call ln(900d0,140d0,940d0,140d0,cw_);call cap(940d0,140d0,1020d0,140d0,cw_);call ln(1020d0,140d0,1060d0,140d0,cw_)
  call lbl('Cf 10p',948d0,108d0,cl_)
  call ln(1060d0,140d0,1060d0,317d0,cw_);call ln(1030d0,317d0,1080d0,317d0,cw_);call dot(1060d0,317d0)
  if(hot==15)call highlight(830d0,100d0,250d0,350d0)
  call tag(trim(num(oq(3),1))//' nA',800d0,460d0,cv_)
  call tag(trim(num(oq(4),3))//' V',1040d0,350d0,cv_)
  ! gain stage U3 (non-inverting), anti-alias RC, ADC
  call ln(1080d0,317d0,1080d0,383d0,cw_);call ln(1080d0,383d0,1110d0,383d0,cw_)
  call opamp(1110d0,403d0,80d0,'U3',clip,.true.)
  call ln(1110d0,423d0,1100d0,423d0,cw_);call ln(1100d0,423d0,1100d0,500d0,cw_);call dot(1100d0,500d0)
  call res(1100d0,500d0,1100d0,570d0,cw_);call gnd(1100d0,570d0);call lbl('R1 10k',1112d0,520d0,cl_)
  call ln(1100d0,500d0,1210d0,500d0,cw_);call res(1210d0,500d0,1210d0,410d0,cw_);call lbl('R2 '//trim(num((3*oq(9)/3-1)*10,0))//'k',1222d0,452d0,cl_)
  call ln(1190d0,403d0,1240d0,403d0,cw_);call dot(1210d0,403d0)
  call res(1240d0,403d0,1310d0,403d0,cw_);call dot(1310d0,403d0);call cap(1310d0,403d0,1310d0,480d0,cw_);call gnd(1310d0,480d0)
  call ln(1310d0,403d0,1350d0,403d0,cw_)
  if(hot==12)call highlight(1090d0,360d0,160d0,230d0)
  call tag('x'//trim(num(oq(9),1))//'  '//trim(num(oq(5),2))//' V',1150d0,330d0,merge(cr_,cv_,clip))
  ! MCU / ADC
  call blk(1350d0,330d0,230d0,250d0,'ADC 12 bit / MCU',cv_)
  call lbl('FAM '//trim(num(oq(7),0)),1364d0,370d0,merge(cr_,cw_,clip))
  call lbl('YY  '//trim(num(oq(10),0)),1364d0,396d0,cw_)
  call lbl('cycle '//trim(num(oq(15),0)),1364d0,422d0,cw_)
  if(clip)call lbl('CLIPPED',1364d0,460d0,cr_)
  call scope(560d0,560d0,560d0,300d0,5,0,0d0,4200d0,'Scope: ADC counts (FAM)',cg_,cg_)
  call ttag(11,420d0,150d0);call ttag(12,270d0,350d0)
  call lbl('Enclosure air '//trim(num(tenv,1))//' C',40d0,520d0,cl_)
  if(burnt(11))call burn(360d0,220d0)
  if(burnt(12))call burn(350d0,320d0)
  if(ckt_short==4)call short_mark(360d0,165d0,360d0,275d0)
  call blk(40d0,560d0,480d0,300d0,'LED driver readings',cl_)
  call lbl('V(DAC)  '//trim(num(oq(2),3))//' V',56d0,600d0,cw_)
  call lbl('I(LED)  '//trim(num(1000*oq(1),2))//' mA',56d0,626d0,cw_)
  call lbl('P(opt)  '//trim(num(oq(11)*1d0,2))//' mW',56d0,652d0,cw_)
  call lbl('Vbe '//trim(num(oq(13),3))//' V  Vce '//trim(num(oq(12),2))//' V',56d0,678d0,cw_)
  call lbl('Iph '//trim(num(oq(3),1))//' nA  noise '//trim(num(oq(14),2))//' nA',56d0,704d0,cw_)
  call lbl('signal '//trim(num(oq(8),2))//'  (simulated curve)',56d0,730d0,cw_)
 end subroutine
! ------------------------------------------------------------------ temperatures, shorts, burns
 function tcolor(t) result(c)
  ! component temperature colour: < 45 C cool, 45-90 C warm, > 90 C hot
  real(c_double),intent(in)::t
  integer(c_int32_t)::c
  if(t<45)then
   c=cv_
  else if(t<90)then
   c=cb_
  else
   c=cr_
  end if
 end function
 subroutine ttag(k,u,v)
  integer,intent(in)::k
  real(c_double),intent(in)::u,v
  call tag(trim(num(comp_t(k),0))//' C',u,v,tcolor(comp_t(k)))
 end subroutine
 subroutine burn(u,v)
  ! burn mark: charred patch with soot rays and rising smoke
  real(c_double),intent(in)::u,v
  integer::k
  real(c_double)::a,r,sm
  do k=0,11
   a=k*.5236d0+.3d0*sin(k*2.1d0);r=18+10*sin(k*1.7d0)
   call ln(u,v,u+r*1.8d0*cos(a),v+r*1.8d0*sin(a),rgba(40,26,18,255))
  end do
  call disc(px(u),py(v),30*ss,rgba(52,34,22,255),.85d0)
  call disc(px(u+8),py(v-6),20*ss,rgba(28,18,12,255),.9d0)
  call disc(px(u-10),py(v+8),14*ss,rgba(18,12,8,255),.9d0)
  call disc(px(u+3),py(v+2),7*ss,rgba(255,120,40,255),.25d0+.25d0*sin(tt*9))
  do k=0,2
   sm=mod(tt*.7d0+k/3d0,1d0)
   call glow(px(u+10*sin(tt+k)),py(v-20-90*sm),(14+30*sm)*ss,rgba(150,150,150,255),.35d0*(1-sm))
  end do
  call lbl('BURNT',u-26,v+34,cr_)
 end subroutine
 subroutine short_mark(u0,v0,u1,v1)
  ! applied short: thick red link with sparks while current flows
  real(c_double),intent(in)::u0,v0,u1,v1
  real(c_double)::mx_,my_
  integer::k
  call lnw(u0,v0,u1,v1,cr_,6d0)
  mx_=(u0+u1)/2;my_=(v0+v1)/2
  if(.not.fuse_blown.or.ckt_short==4)then
   do k=0,5
    call ln(mx_,my_,mx_+22*cos(k+tt*13),my_+22*sin(k*1.3d0+tt*11),rgba(255,240,120,255))
   end do
  end if
  call tag('SHORT',mx_+14,my_-30,cr_)
 end subroutine
 subroutine thermal_extras()
  integer::k
  real(c_double)::a
  ! temperatures
  call ttag(1,236d0,226d0);call ttag(2,640d0,160d0);call ttag(3,230d0,400d0);call ttag(4,640d0,330d0)
  call ttag(5,400d0,520d0);call ttag(6,540d0,318d0);call ttag(7,960d0,160d0);call ttag(8,946d0,348d0)
  call ttag(9,1140d0,190d0);call ttag(10,1110d0,300d0);call ttag(13,110d0,64d0);call ttag(14,140d0,196d0)
  call tag('Ambient '//trim(num(tenv,1))//' C',1040d0,40d0,tcolor(tenv))
  ! fan: spinning blades at the simulated speed
  a=tt*tq(14)/60*2*3.141592653589793d0*.05d0
  do k=0,2
   call ln(1080d0,170d0,1080+20*cos(a+k*2.094d0),170+20*sin(a+k*2.094d0),merge(cr_,cg_,tq(14)<100))
  end do
  call tag(trim(num(tq(14),0))//' RPM',1140d0,230d0,merge(cr_,cg_,tq(14)<1000))
  ! blown fuse
  if(fuse_blown)then
   call box(px(110d0),py(100d0),40*ss,20*ss,rgba(16,9,13,255))
   call ln(110d0,110d0,124d0,104d0,cr_);call ln(136d0,116d0,150d0,110d0,cr_)
   call tag('F1 BLOWN',96d0,130d0,cr_)
  end if
  ! applied shorts
  select case(ckt_short)
  case(1)
   call short_mark(340d0,332d0,340d0,408d0)
  case(2)
   call short_mark(392d0,256d0,528d0,256d0)
  case(3)
   call short_mark(1110d0,142d0,1110d0,198d0)
  end select
  ! burn marks
  if(burnt(1))call burn(320d0,200d0)
  if(burnt(2))call burn(600d0,200d0)
  if(burnt(3))call burn(320d0,370d0)
  if(burnt(4))call burn(600d0,370d0)
  if(burnt(5))call burn(460d0,500d0)
  if(burnt(6))call burn(460d0,290d0)
  if(burnt(9))call burn(1080d0,170d0)
  if(burnt(10))call burn(1080d0,290d0)
  if(burnt(13))call burn(130d0,110d0)
 end subroutine
! ------------------------------------------------------------------ diagnosis panel
 subroutine diagnosis(part,code,fault_on,fp)
  integer,intent(in)::part,code,fp
  logical,intent(in)::fault_on
  character(len=60)::comp,c1,c2,c3,m1,m2,m3,m4
  real(c_double)::u,v,rexp,rmeas
  logical::here
  if(ss<.55d0)then
   ! preview: just mark the suspect part
   if(ckt_short>0)then
    call box(px(1150d0),py(820d0),440*ss,70*ss,rgba(36,16,22,250))
    call txt('SHORT CIRCUIT',px(1170d0),py(834d0),cr_)
   else if(fault_on.and.hot>0.and.((part==1.and.hot<10).or.(part==2.and.hot>10)))then
    call box(px(1150d0),py(820d0),440*ss,70*ss,rgba(36,16,22,250))
    call txt('Suspect: '//trim(suspect_name(hot)),px(1170d0),py(834d0),cr_)
   end if
   return
  end if
  u=1250;v=40
  if(part==2)then
   u=1250;v=600
  end if
  here=fault_on.and.((part==1.and.fp==1).or.(part==2.and.fp==2))
  comp='';c1='';c2='';c3='';m1='';m2='';m3='';m4=''
  if(ckt_short>0)then
   here=(part==1.and.ckt_short<4).or.(part==2.and.ckt_short==4)
   comp='SHORT: '//trim(short_name(ckt_short))
   select case(ckt_short)
   case(1)
    c1='- failed MOSFET (drain-source)';c2='- solder bridge on the Q3 pads';c3='- gate driver dead-time error'
    m1='I peak  '//trim(num(ipk,1))//' A';m2='T Q1/Q3 '//trim(num(comp_t(1),0))//'/'//trim(num(comp_t(3),0))//' C'
    if(here)call meter(20d0,470d0,'R(DS) 0.02 ohm','CONTINUITY','beep',320d0,334d0,320d0,406d0)
   case(2)
    c1='- TEC lead insulation damaged';c2='- solder bridge at TEC connector';c3='- condensation in the module'
    m1='I peak  '//trim(num(ipk,1))//' A';m2='T Q1/Q4 '//trim(num(comp_t(1),0))//'/'//trim(num(comp_t(4),0))//' C'
    if(here)call meter(650d0,140d0,'R(TEC) 0.01 ohm','expected 1.6','CONTINUITY',400d0,290d0,520d0,290d0)
   case(3)
    c1='- winding insulation breakdown';c2='- overheated / seized bearing';c3='- moisture in the motor'
    m1='I fan   '//trim(num(abs(tq(15)),1))//' A';m2='T M1    '//trim(num(comp_t(9),0))//' C'
    if(here)call meter(1170d0,310d0,'I '//trim(num(abs(tq(15)),1))//' A','R 0.5 ohm','exp 24 ohm',1080d0,144d0,1080d0,250d0)
   case(4)
    c1='- LED die failed short';c2='- solder bridge on LED pads';c3='- ESD damage'
    m1='I(LED)  '//trim(num(1000*oq(1),1))//' mA (limited)';m2='Vce Q1  '//trim(num(oq(12),2))//' V'
    m3='light   none: ADC baseline'
    if(here)call meter(100d0,450d0,'V(D1) '//trim(num(vo(7)-vo(6),3))//' V','I '//trim(num(1000*oq(1),1))//' mA', &
     'no light',360d0,172d0,360d0,268d0)
   end select
   if(ckt_short<4)m3='F1 fuse '//merge('BLOWN','ok   ',fuse_blown)
   if(.not.here)then
    comp='Short applied on the '//merge('thermal sheet','optical sheet',ckt_short<4)
    c1='This circuit: readings nominal';c2='';c3='';m1='';m2='';m3=''
   end if
  else if(.not.fault_on)then
   comp='Normal run: no suspect part'
   m1='All readings within nominal range'
  else if(fp==3)then
   comp='Electronics nominal'
   c1='Fault is in the sample or tube,'
   c2='not in a circuit: '//trim(ckt_note(1:30))
   m1='Readings within nominal range'
  else if(.not.here)then
   comp='This circuit: no fault found'
   c1='Readings nominal here; the suspect'
   c2='part is on the '//merge('optical sheet','thermal sheet',fp==2)
   m1='Ruled out by measurement'
  else
   select case(code)
   case(1)
    comp='Block edge zone (Peltier edge)'
    c1='- edge Peltier rows weaker'
    c2='- dry thermal paste at the edges'
    c3='- heat loss at the block rim'
    m1='T centre '//trim(num(tq(2),1))//' C'
    m2='T edge   '//trim(num(tq(7),1))//' C'
    m3='dT       '//trim(num(tq(2)-tq(7),1))//' C'
   case(2)
    comp='TEC1 Peltier module (zone 1)'
    c1='- module ageing: Seebeck drop'
    c2='- cracked solder raises R'
    c3='- dry thermal interface'
    m1='V(TEC) '//trim(num(tq(10),2))//' V'
    m2='I(TEC) '//trim(num(tq(9),2))//' A'
    m3='P      '//trim(num(tq(11),1))//' W'
    m4='Seebeck '//trim(num(tq(23),2))//' V'
    call meter(650d0,140d0,'V '//trim(num(tq(10),2))//' V','I '//trim(num(tq(9),2))//' A', &
     'R '//trim(num(abs(tq(10))/max(.05d0,abs(tq(9))),2))//' ohm',400d0,290d0,520d0,290d0)
   case(3)
    comp='Lid heater controller / Q5'
    c1='- wrong lid set-point (firmware)'
    c2='- heater element partly open'
    c3='- NTC2 lid sensor mis-reads'
    m1='T lid   '//trim(num(tq(5),1))//' C'
    m2='P lid   '//trim(num(tq(17),1))//' W'
    m3='ADC ch1 '//trim(num(tq(24),0))
    call meter(660d0,170d0,'T '//trim(num(tq(5),1))//' C','P '//trim(num(tq(17),1))//' W','V '//trim(num(vt(13),2))//' V', &
     930d0,262d0,930d0,358d0)
   case(11,28)
    comp='Tube to block contact'
    c1='- tubes not pushed into wells'
    c2='- wrong tube / plate type'
    c3='- debris in the wells'
    m1='T block '//trim(num(tq(2),1))//' C'
    m2='T tube  '//trim(num(tq(6),1))//' C'
    m3='lag     '//trim(num(tq(2)-tq(6),1))//' C'
   case(12)
    rmeas=1d4*vt(10)/max(1d-6,3.3d0-vt(10));rexp=rntc(tq(2))
    comp='NTC1 block thermistor'
    c1='- thermistor ageing (R drift)'
    c2='- loose connector adds R'
    c3='- R1 divider resistor drift'
    m1='R meas  '//trim(num(rmeas/1d3,2))//' k'
    m2='R true  '//trim(num(rexp/1d3,2))//' k'
    m3='T read  '//trim(num(tq(3),1))//' C'
    m4='T true  '//trim(num(tq(2),1))//' C'
    call meter(10d0,460d0,'R '//trim(num(rmeas/1d3,2))//' k','V '//trim(num(vt(10),3))//' V', &
     'exp '//trim(num(rexp/1d3,2))//' k',120d0,762d0,120d0,838d0)
   case(13)
    comp='Fan M1 (rotor locked)'
    c1='- fan bearing seized'
    c2='- fan blocked by dust / cable'
    c3='- Q6 driver or tach fault'
    m1='fan     '//trim(num(tq(14),0))//' rpm'
    m2='I fan   '//trim(num(1000*tq(15),0))//' mA'
    m3='T sink  '//trim(num(tq(4),1))//' C'
    m4=merge('INTERLOCK tripped','interlock armed  ',tq(18)>.5d0)
    call meter(1170d0,310d0,'I '//trim(num(1000*tq(15),0))//' mA',trim(num(tq(14),0))//' rpm', &
     'V '//trim(num(vt(14)-vt(16),2))//' V',1080d0,144d0,1080d0,250d0)
   case(14)
    comp='S1 lid interlock switch'
    c1='- lid opened during the run'
    c2='- worn interlock switch'
    c3='- broken lid latch'
    m1='S1      '//merge('OPEN  ','closed',ckt_tm>10)
    m2='P lid   '//trim(num(tq(17),1))//' W'
    m3='bridge  '//merge('disabled','enabled ',tq(18)>.5d0)
   case(4,10,29,30)
    comp='Optical path (lid / filter / caps)'
    select case(code)
    case(4)
     c1='- lid height / plate pressure'
     c2='- LED spots miss far column'
    case(10)
     c1='- spill on excitation filter'
     c2='- scratched filter coating'
    case(29)
     c1='- fogged or opaque caps'
     c2='- marker ink on the caps'
    case default
     c1='- autofluorescent tube plastic'
     c2='- wrong consumable lot'
    end select
    c3='- electronics read normal'
    m1='I(LED)  '//trim(num(1000*oq(1),1))//' mA (ok)'
    m2='I(ph)   '//trim(num(oq(3),1))//' nA'
    m3='ADC     '//trim(num(oq(7),0))
    call meter(560d0,380d0,'Iph '//trim(num(oq(3),1))//' nA','V '//trim(num(oq(4),3))//' V','ADC '//trim(num(oq(7),0)), &
     900d0,300d0,850d0,420d0)
   case(5,17)
    comp='Emission filter (FAM -> YY leak)'
    c1='- degraded / wrong filter'
    c2='- crosstalk calibration old'
    c3='- filter wheel position error'
    m1='FAM ADC '//trim(num(oq(7),0))
    m2='YY ADC  '//trim(num(oq(10),0))
    m3='leak    '//trim(num(100*oq(10)/max(1d0,oq(7)),0))//' %'
   case(6,7)
    comp='D1 LED / driver (U1, Q1, Re)'
    c1='- LED ageing (dim emitter)'
    c2='- DAC reference low / drifting'
    c3='- sense resistor Re drifted'
    m1='V(DAC)  '//trim(num(oq(2),3))//' V'
    m2='I(LED)  '//trim(num(1000*oq(1),2))//' mA'
    m3='Vce     '//trim(num(oq(12),2))//' V'
    call meter(100d0,450d0,'I '//trim(num(1000*oq(1),2))//' mA','Ve '//trim(num(vo(5),3))//' V', &
     'Vce '//trim(num(oq(12),2))//' V',360d0,365d0,360d0,445d0)
   case(8)
    comp='Gain stage U3 (R2) / ADC range'
    c1='- wrong gain resistor R2'
    c2='- gain set too high in firmware'
    c3='- ADC reference low'
    m1='gain    x'//trim(num(oq(9),1))
    m2='V(U3)   '//trim(num(oq(5),2))//' V'
    m3='ADC     '//trim(num(oq(7),0))//merge(' CLIP','     ',oq(16)>.5d0)
    call meter(1290d0,140d0,'Vo '//trim(num(oq(5),2))//' V','ADC '//trim(num(oq(7),0)),merge('CLIPPED  ','in range ',oq(16)>.5d0), &
     1210d0,403d0,1100d0,568d0)
   case(9)
    comp='PD1 / TIA U2 (noise)'
    c1='- photodiode dark current'
    c2='- Cf open: TIA bandwidth high'
    c3='- ground loop / EMI pickup'
    m1='I(ph)   '//trim(num(oq(3),1))//' nA'
    m2='noise   '//trim(num(oq(14),2))//' nA rms'
    m3='V(TIA)  '//trim(num(oq(4),3))//' V'
    call meter(1130d0,40d0,'V '//trim(num(oq(4),3))//' V','n '//trim(num(oq(14),2))//' nA',' ',1060d0,317d0,935d0,365d0)
   end select
  end if
  call box(px(u),py(v),330*ss,250*ss,rgba(36,16,22,250))
  call frame_rect(px(u),py(v),330*ss,250*ss,merge(cr_,cg_,here),max(1d0,2*ss))
  call txt('DIAGNOSIS',px(u+10),py(v+8),merge(cr_,cg_,here))
  call txt(trim(comp),px(u+10),py(v+34),cw_)
  call txt(trim(c1),px(u+10),py(v+62),cl_);call txt(trim(c2),px(u+10),py(v+86),cl_);call txt(trim(c3),px(u+10),py(v+110),cl_)
  call txt('Measured:',px(u+10),py(v+140),cv_)
  call txt(trim(m1),px(u+10),py(v+164),cw_);call txt(trim(m2),px(u+10),py(v+186),cw_)
  call txt(trim(m3),px(u+10),py(v+208),cw_);call txt(trim(m4),px(u+170),py(v+140),cr_)
 end subroutine
end module native_schematic
