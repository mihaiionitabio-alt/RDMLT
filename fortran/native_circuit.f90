! SPDX-License-Identifier: GPL-3.0-only
! MISSY R2 Fortran: electrical simulation of the cycler electronics shown on the schematics.
! A small SPICE-style solver: modified nodal analysis (MNA) with companion models — backward
! Euler for capacitors and inductors, Newton-Raphson for diodes, bipolar transistors
! (Ebers-Moll) and op-amps with soft output rails (tanh) — solved by Gaussian elimination with
! partial pivoting every 2 ms of model time. Two circuits are simulated:
!  thermal - 12 V supply, bulk capacitor, MOSFET H-bridge with flyback diodes driving the Peltier
!            module (resistance + Seebeck back-EMF), low-side shunt and current-sense amplifier,
!            NTC thermistor dividers with RC filters into the ADC, lid heater switch and the fan
!            motor (inductance, resistance, back-EMF, flyback diode); a PI controller closes the
!            loop on the ADC reading and lumped heat balances move the block, heat sink, lid and
!            tube temperatures;
!  optical - LED constant-current driver (DAC, op-amp, NPN transistor, emitter sense resistor),
!            photodiode with junction capacitance into a transimpedance amplifier (Rf || Cf),
!            non-inverting gain stage, anti-alias RC filter and a 12-bit ADC.
! The case fault is applied to the parts it acts on. Values are simulated (illustrative part
! values; PWM slowed to 25 Hz and fault onsets moved into a 60 s demonstration loop).
module native_circuit
 use iso_c_binding
 implicit none
 private
 public :: ckt_advance, ckt_reset, vt, vo, tq, tr, oq, orr, ckt_note, ckt_tm, ckt_fault_part, rntc, hist, nhist, &
  ckt_short, short_name, comp_t, burnt, fuse_blown, ipk, tenv, ncomp, comp_name
 integer,parameter :: dp=c_double, mxn=28
 real(dp),parameter :: dt=2d-3, vtherm=.025852d0, loop_s=60d0, onset=10d0
 ! solver work arrays
 real(dp),save :: ga(mxn,mxn),za(mxn),xa(mxn)
 integer,save :: nnode,nun
 ! thermal circuit: 16 nodes + 6 branch rows; optical: 13 nodes + 5 branch rows
 real(dp),save :: vt(22)=0,vo(18)=0,xt_prev(22)=0,xo_prev(18)=0
 ! tq: thermal readouts, tr: switch states; oq: optical readouts
 !  tq: 1 Tset 2 Tblock 3 Tmeas 4 Tsink 5 Tlid 6 Ttube 7 Tedge 8 duty 9 I_tec(avg) 10 V_tec(avg)
 !      11 P_tec 12 ADC block code 13 ADC current code 14 fan rpm 15 I_fan 16 lid duty 17 P_lid
 !      18 interlock(0/1) 19 sensor bias 20 I_tec(inst) 21 I_shunt 22 V_bus 23 Seebeck V 24 ADC lid
 !  tr: 1..4 bridge Q1..Q4 on, 5 lid switch, 6 fan switch
 !  oq: 1 I_led 2 V_dac 3 I_ph (nA) 4 V_tia 5 V_gain 6 V_adc 7 ADC code 8 signal 9 gain 10 YY code
 !      11 P_led (mW) 12 V_ce 13 V_be 14 noise (nA) 15 cycle 16 clipped(0/1)
 real(dp),save :: tq(24)=0,oq(16)=0
 integer,save :: tr(6)=0,osat(8)=0
 real(dp),save :: orr=0
 integer,save :: oqn=0
 real(dp),save :: ckt_tm=-1
 character(len=120),save :: ckt_note=''
 ! state
 real(dp),save :: tb=60,ts=30,tl=105,tt=60,om=1000,ilm=0,integ=0,ilid=0,sim_t=-1,iavg=0,vavg=0,pavg=0
 real(dp),save :: lidint=0,ufilt=0,cbulk_v=12,cadc=0
 integer,save :: last_code=-999,seed=12345,nsub=0
 real(dp),save :: last_t=-1d9
 ! scope traces, one sample per 0.2 s over the 60 s loop: Tset, Tblock, I_tec, Tsink, ADC optical
 integer,parameter :: nhist=300
 ! user-applied short circuit: 0 none, 1 Q3 drain-source short (shoot-through), 2 TEC1 terminals
 ! shorted, 3 fan winding short, 4 LED D1 shorted
 integer,save :: ckt_short=0
 ! component temperatures (junction / body, first-order thermal RC to the enclosure air):
 !  1-4 Q1-Q4, 5 Rsh, 6 TEC1, 7 Q5, 8 lid heater, 9 fan winding, 10 Q6, 11 LED D1, 12 Q1 (LED
 !  driver), 13 F1 fuse, 14 C1; parts above 150 C are marked burnt until the simulation restarts
 integer,parameter :: ncomp=14
 real(dp),save :: comp_t(ncomp)=25,tenv=25,ipk=0,fuse_th=0
 logical,save :: burnt(ncomp)=.false.,fuse_blown=.false.
 real(dp),save :: hist(5,nhist)=0
contains
! ------------------------------------------------------------------ MNA stamps
 subroutine clear(nn,nb)
  integer,intent(in)::nn,nb
  nnode=nn;nun=nn+nb
  ga(1:nun,1:nun)=0;za(1:nun)=0
 end subroutine
 subroutine st_g(a,b,g)
  integer,intent(in)::a,b
  real(dp),intent(in)::g
  if(a>0)ga(a,a)=ga(a,a)+g
  if(b>0)ga(b,b)=ga(b,b)+g
  if(a>0.and.b>0)then
   ga(a,b)=ga(a,b)-g;ga(b,a)=ga(b,a)-g
  end if
 end subroutine
 subroutine st_r(a,b,r)
  integer,intent(in)::a,b
  real(dp),intent(in)::r
  call st_g(a,b,1/r)
 end subroutine
 subroutine st_i(a,b,i)
  ! current i flowing from node a through the source to node b
  integer,intent(in)::a,b
  real(dp),intent(in)::i
  if(a>0)za(a)=za(a)-i
  if(b>0)za(b)=za(b)+i
 end subroutine
 subroutine st_v(k,a,b,v)
  ! ideal source V(a)-V(b)=v on branch row k
  integer,intent(in)::k,a,b
  real(dp),intent(in)::v
  integer::m
  m=nnode+k
  if(a>0)then
   ga(a,m)=ga(a,m)+1;ga(m,a)=ga(m,a)+1
  end if
  if(b>0)then
   ga(b,m)=ga(b,m)-1;ga(m,b)=ga(m,b)-1
  end if
  za(m)=v
 end subroutine
 subroutine st_cap(a,b,c,vprev)
  integer,intent(in)::a,b
  real(dp),intent(in)::c,vprev
  call st_g(a,b,c/dt);call st_i(b,a,c/dt*vprev)
 end subroutine
 subroutine st_ind(k,a,b,l,iprev)
  ! V(a)-V(b) = L/dt (i - iprev), branch current i on row k
  integer,intent(in)::k,a,b
  real(dp),intent(in)::l,iprev
  call st_v(k,a,b,-l/dt*iprev)
  ga(nnode+k,nnode+k)=ga(nnode+k,nnode+k)-l/dt
 end subroutine
 subroutine st_vccs(a,b,c,d,gm)
  ! current gm*(V(c)-V(d)) from a to b
  integer,intent(in)::a,b,c,d
  real(dp),intent(in)::gm
  if(a>0.and.c>0)ga(a,c)=ga(a,c)+gm
  if(a>0.and.d>0)ga(a,d)=ga(a,d)-gm
  if(b>0.and.c>0)ga(b,c)=ga(b,c)-gm
  if(b>0.and.d>0)ga(b,d)=ga(b,d)+gm
 end subroutine
 subroutine st_diode(a,b,is,n,x)
  ! Shockley diode anode a, cathode b, linearised at the present iterate x
  integer,intent(in)::a,b
  real(dp),intent(in)::is,n,x(:)
  real(dp)::v,e,gd,id
  real(dp)::de,um
  v=nv(x,a)-nv(x,b);um=log(1/is)            ! exponential up to 1 A, linear beyond
  call sexp(v/(n*vtherm),um,e,de)
  id=is*(e-1);gd=max(1d-12,is*de/(n*vtherm))
  call st_g(a,b,gd);call st_i(a,b,id-gd*v)
 end subroutine
 subroutine st_npn(c,b,e,is,bf,br,x)
  ! Ebers-Moll transport model, linearised (collector c, base b, emitter e)
  integer,intent(in)::c,b,e
  real(dp),intent(in)::is,bf,br,x(:)
  real(dp)::vbe,vbc,ef,er,gf,gr,it
  real(dp)::df,dr,um
  vbe=nv(x,b)-nv(x,e);vbc=nv(x,b)-nv(x,c);um=log(1/is)
  call sexp(vbe/vtherm,um,ef,df);call sexp(vbc/vtherm,um,er,dr);gf=is*df/vtherm;gr=is*dr/vtherm
  call st_g(b,e,gf/bf+1d-12);call st_i(b,e,is/bf*(ef-1)-gf/bf*vbe)
  call st_g(b,c,gr/br+1d-12);call st_i(b,c,is/br*(er-1)-gr/br*vbc)
  it=is*(ef-er)
  call st_vccs(c,e,b,e,gf);call st_vccs(c,e,b,c,-gr)
  call st_i(c,e,it-gf*vbe+gr*vbc)
 end subroutine
 subroutine st_opamp(k,o,p,n,a,vr,x)
  ! op-amp with gain a and output rails +/- vr: linear (V(o) = a (V(p)-V(n))) or, once the
  ! output would pass a rail, a fixed source at that rail; the state is re-checked every
  ! Newton iteration (rail_check).
  integer,intent(in)::k,o,p,n
  real(dp),intent(in)::a,vr,x(:)
  integer::m
  m=nnode+k
  if(osat(k)==0)then
   call st_v(k,o,0,0d0)
   if(p>0)ga(m,p)=ga(m,p)-a
   if(n>0)ga(m,n)=ga(m,n)+a
  else
   call st_v(k,o,0,osat(k)*vr)
  end if
  if(.false.)print*,x(1)
 end subroutine
 subroutine rail_check(k,o,p,n,a,vr,x,changed)
  integer,intent(in)::k,o,p,n
  real(dp),intent(in)::a,vr,x(:)
  logical,intent(inout)::changed
  real(dp)::lin
  lin=a*(nv(x,p)-nv(x,n))
  if(osat(k)==0)then
   if(abs(nv(x,o))>vr)then
    osat(k)=nint(sign(1d0,nv(x,o)));changed=.true.
   end if
  else if(abs(lin)<vr.or.lin*osat(k)<0)then
   osat(k)=0;changed=.true.
  end if
 end subroutine
 subroutine sexp(u,um,e,de)
  ! exp(u) continued linearly above um (keeps Newton iterations bounded)
  real(dp),intent(in)::u,um
  real(dp),intent(out)::e,de
  if(u<=um)then
   e=exp(u);de=e
  else
   de=exp(um);e=de*(1+u-um)
  end if
 end subroutine
 real(dp) function nv(x,a)
  real(dp),intent(in)::x(:)
  integer,intent(in)::a
  nv=0;if(a>0)nv=x(a)
 end function
 subroutine solve(ok)
  logical,intent(out)::ok
  integer::i,j,k,p
  real(dp)::f,t,row(mxn)
  do i=1,nnode
   ga(i,i)=ga(i,i)+1d-12
  end do
  ok=.true.
  do k=1,nun
   p=k
   do i=k+1,nun
    if(abs(ga(i,k))>abs(ga(p,k)))p=i
   end do
   if(abs(ga(p,k))<1d-30)then
    ok=.false.;return
   end if
   if(p/=k)then
    row(1:nun)=ga(k,1:nun);ga(k,1:nun)=ga(p,1:nun);ga(p,1:nun)=row(1:nun)
    t=za(k);za(k)=za(p);za(p)=t
   end if
   do i=k+1,nun
    f=ga(i,k)/ga(k,k)
    if(f/=0)then
     ga(i,k+1:nun)=ga(i,k+1:nun)-f*ga(k,k+1:nun);za(i)=za(i)-f*za(k)
    end if
   end do
  end do
  do i=nun,1,-1
   t=za(i)
   do j=i+1,nun
    t=t-ga(i,j)*xa(j)
   end do
   xa(i)=t/ga(i,i)
  end do
 end subroutine
! ------------------------------------------------------------------ helpers
 real(dp) function rand1()
  seed=mod(seed*1103515245+12345,2147483647);if(seed<0)seed=-seed
  rand1=mod(seed,100000)/1d5
 end function
 real(dp) function gauss()
  gauss=rand1()+rand1()+rand1()+rand1()+rand1()+rand1()-3
 end function
 real(dp) function rntc(t)
  ! NTC thermistor, 10 kohm at 25 C, B = 3950 K
  real(dp),intent(in)::t
  rntc=1d4*exp(3950d0*(1/(t+273.15d0)-1/298.15d0))
 end function
 real(dp) function t_of_adc(code)
  real(dp),intent(in)::code
  real(dp)::v,r
  v=max(1d-3,min(3.299d0,code/4095d0*3.3d0));r=1d4*v/(3.3d0-v)
  t_of_adc=1/(log(r/1d4)/3950d0+1/298.15d0)-273.15d0
 end function
 real(dp) function setpoint(tm)
  ! two-step-plus-extension profile: 95 C 1.6 s, 60 C 2.2 s, 72 C 2.2 s (demo timing)
  real(dp),intent(in)::tm
  real(dp)::c
  c=mod(tm,6d0)
  if(c<1.6d0)then
   setpoint=95
  else if(c<3.8d0)then
   setpoint=60
  else
   setpoint=72
  end if
 end function
 integer function ckt_fault_part(code) result(k)
  ! 0 none, 1 thermal electronics, 2 optical electronics, 3 both (sample / tube faults)
  integer,intent(in)::code
  select case(code)
  case(1,2,3,11,12,13,14,28)
   k=1
  case(4,5,6,7,8,9,10,17,29,30)
   k=2
  case(39,41,45,46)
   k=3
  case default
   k=0
  end select
 end function
 subroutine ckt_reset()
  tb=60;ts=30;tl=105;tt=60;om=1000;ilm=.083d0;integ=0;sim_t=0;iavg=0;vavg=0;pavg=0;lidint=0
  comp_t=25;burnt=.false.;fuse_blown=.false.;ipk=0;fuse_th=0;tenv=25
  xt_prev=0;xo_prev=0;vt=0;vo=0;xt_prev(1:2)=12;seed=12345;ufilt=0;tq=0;oq=0;tr=0;hist=0;osat=0
 end subroutine
! ------------------------------------------------------------------ public driver
 subroutine ckt_advance(code,p,fault_on,t)
  ! Advances both circuits to animation time t (seconds); restarts at the loop boundary or
  ! when the case / fault setting changes.
  integer,intent(in)::code
  real(dp),intent(in)::p(6),t
  logical,intent(in)::fault_on
  real(dp)::tm
  integer::key,n,k
  key=(code*2+merge(1,0,fault_on))*8+ckt_short
  tm=mod(max(0d0,t),loop_s)
  if(key/=last_code.or.tm<sim_t-1d-9.or.t<last_t-1d-9)then
   call ckt_reset();last_code=key;sim_t=0
  end if
  last_t=t
  if(tm-sim_t>3d0)sim_t=tm-3d0             ! catch up at most 3 s per frame
  n=0
  do while(sim_t+dt<=tm.and.n<1600)
   sim_t=sim_t+dt;n=n+1
   call step_thermal(code,p,fault_on.and.ckt_fault_part(code)>0,sim_t)
   if(mod(n,2)==0)call step_optical(code,p,fault_on.and.ckt_fault_part(code)>0,sim_t)
   if(mod(nint(sim_t/dt),100)==0)then
    k=min(nhist,1+int(sim_t/.2d0+1d-6))
    hist(:,k)=[tq(1),tq(2),tq(9),tq(4),oq(7)]
   end if
  end do
  ckt_tm=sim_t
  call note(code,p,fault_on)
 end subroutine
 subroutine note(code,p,f)
  integer,intent(in)::code
  real(dp),intent(in)::p(6)
  logical,intent(in)::f
  ckt_note=''
  if(ckt_short>0)then
   ckt_note='Short circuit: '//trim(short_name(ckt_short))
   if(fuse_blown)ckt_note=trim(ckt_note)//'; F1 fuse blown'
   return
  end if
  if(.not.f)return
  select case(code)
  case(1);ckt_note='Edge tubes '//trim(rt(p(1)))//' C below the block centre (edge sensor)'
  case(2);ckt_note='Peltier zone 1 Seebeck coefficient x'//trim(rt(p(1)))//': less heat per ampere'
  case(3);ckt_note='Lid controller target '//trim(rt(p(1)))//' C instead of 105 C'
  case(4);ckt_note='Far column optical transmission x'//trim(rt(p(1)))//' at the photodiode'
  case(5,17);ckt_note='FAM leaks into the YY channel: x'//trim(rt(p(1)))
  case(6);ckt_note='LED DAC setpoint x'//trim(rt(p(1)))
  case(7);ckt_note='LED output drifts to x'//trim(rt(p(1)))//' over the demo loop'
  case(8);ckt_note='Gain stage x'//trim(rt(p(1)))//', ADC ceiling '//trim(rt(p(2)))//' counts'
  case(9);ckt_note='Read noise x'//trim(rt(p(1)))
  case(10);ckt_note='Filter patch: transmission x'//trim(rt(p(1)))//', noise x'//trim(rt(p(2)))
  case(11,28);ckt_note='Tube contact lag '//trim(rt(p(1)))//' s (shown x0.1 in demo time)'
  case(12);ckt_note='Thermistor bias ramps to '//trim(rt(p(2)))//' C from 10 s'
  case(13);ckt_note='Fan rotor locks at 10 s: sink overheats, interlock at 60 C'
  case(14);ckt_note='Lid switch opens at 10 s: heater and bridge disabled'
  case(29);ckt_note='Caps transmit x'//trim(rt(p(1)))//' of the light'
  case(30);ckt_note='Tube autofluorescence adds '//trim(rt(p(1)))//' counts'
  case(39);ckt_note='Evaporation: signal falls with lost volume'
  case(41);ckt_note='Bubbles: random light scatter at the photodiode'
  case(45);ckt_note='Droplet joins late: signal step'
  case(46);ckt_note='Liquid on walls: lower signal until it drains'
  end select
 contains
  function rt(v) result(s)
   real(dp),intent(in)::v
   character(len=16)::s
   integer::q
   q=nint(v*100)
   if(mod(q,100)==0)then
    write(s,'(i0)')q/100
   else
    write(s,'(f0.2)')v
    if(s(1:1)=='.')s='0'//s
   end if
  end function
 end subroutine
! ------------------------------------------------------------------ thermal circuit
 subroutine step_thermal(code,p,f,tm)
  integer,intent(in)::code
  real(dp),intent(in)::p(6),tm
  logical,intent(in)::f
  real(dp),parameter::ron=.02d0,roff=1d6,rtec0=1.6d0,alpha0=.05d0,kt0=.35d0,cb=2.5d0,cs=15d0
  real(dp)::alpha,rtec,kth,bias,tmeas,err,u,ph,code_adc,rsink,itec,vtec,qb,qs,ke,im,tset,lidset
  real(dp)::x(22),xold(22),rn,rl,vbe,tedge,taut,dl
  logical::q(6),ok,lock,lidopen,interlock
  integer::it
  alpha=alpha0;rtec=rtec0;kth=kt0;bias=0;lock=.false.;lidopen=.false.;lidset=105;taut=.3d0
  if(f)then
   select case(code)
   case(2);alpha=alpha0*p(1);kth=kt0*(2-p(1))
   case(3);lidset=p(1)
   case(11,28);taut=max(.05d0,.1d0*p(1))
   case(12);bias=p(2)*min(1d0,max(0d0,(tm-onset)/20))
   case(13);lock=tm>onset
   case(14);lidopen=tm>onset
   end select
  end if
  interlock=ts>60.or.lidopen.or.tq(18)>.5d0.and.ts>45.or.fuse_blown
  ! controller: PI on the filtered ADC reading of the block thermistor
  tset=setpoint(tm);tmeas=t_of_adc(4095*nv(xt_prev,11)/3.3d0)
  if(tm<.01d0)tmeas=tb
  err=tset-tmeas;integ=max(-.6d0,min(.6d0,integ+err*dt*1.2d0))
  u=max(-1d0,min(1d0,.22d0*err+integ));if(interlock)u=0
  ph=mod(tm*25,1d0)
  q=.false.
  if(u>0.and.ph<u)then
   q(1)=.true.;q(4)=.true.
  else if(u<0.and.ph<-u)then
   q(2)=.true.;q(3)=.true.
  end if
  ! lid heater: PI on the lid temperature, slow PWM
  dl=lidset-tl;lidint=max(0d0,min(1d0,lidint+dl*dt*.02d0))
  q(5)=.not.lidopen.and.mod(tm*5,1d0)<max(0d0,min(1d0,.08d0*dl+lidint))
  q(6)=.not.(lock.and.tm>onset+3)          ! driver turns the fan off 3 s after the stall
  ke=.01d0;if(ckt_short==3)ke=.002d0     ! shorted turns: less back-EMF and torque
  rn=rntc(tb+bias);rl=rntc(tl)
  x=xt_prev
  do it=1,30
   xold=x
   call clear(16,6)
   call st_v(1,1,0,12d0)
   call st_r(1,2,merge(1d6,.05d0,fuse_blown));call st_cap(2,0,470d-6,xt_prev(2))
   call st_r(2,3,merge(ron,roff,q(1)));call st_r(2,4,merge(ron,roff,q(2)))
   call st_r(3,5,merge(ron,roff,q(3).or.ckt_short==1));call st_r(4,5,merge(ron,roff,q(4)))
   call st_r(5,0,.01d0)
   call st_r(3,6,rtec);call st_v(2,6,4,alpha*(tb-ts))
   if(ckt_short==2)call st_r(3,4,5d-3)
   call st_diode(3,2,1d-9,1.5d0,x);call st_diode(4,2,1d-9,1.5d0,x)
   call st_diode(5,3,1d-9,1.5d0,x);call st_diode(5,4,1d-9,1.5d0,x)
   call st_v(3,7,0,0d0);ga(nnode+3,5)=ga(nnode+3,5)-50       ! current-sense amplifier, gain 50
   call st_r(7,8,1d3);call st_cap(8,0,10d-9,xt_prev(8))
   call st_v(4,9,0,3.3d0)
   call st_r(9,10,1d4);call st_r(10,0,rn);call st_r(10,11,1d3);call st_cap(11,0,100d-9,xt_prev(11))
   call st_r(9,12,1d4);call st_r(12,0,rl)
   call st_r(2,13,merge(ron,roff,q(5)));call st_r(13,0,8d0)
   call st_ind(5,2,14,1d-3,ilm);call st_r(14,15,merge(.5d0,24d0,ckt_short==3));call st_v(6,15,16,ke*om)
   call st_r(16,0,merge(ron,roff,q(6)));call st_diode(16,2,1d-9,1.5d0,x)
   call solve(ok);if(.not.ok)exit
   x=xa(1:22)
   if(maxval(abs(x-xold))<1d-6)exit
  end do
  if(.not.ok)return
  xt_prev=x;vt=x
  itec=(x(3)-x(6))/rtec;vtec=x(3)-x(4);im=x(16+5)
  call heat_parts(x,q)
  ilm=im
  ! heat balances
  qb=alpha*itec*(tb+273.15d0)+.5d0*itec**2*rtec-kth*(tb-ts)-.02d0*(tb-25)
  qs=-alpha*itec*(ts+273.15d0)+.5d0*itec**2*rtec+kth*(tb-ts)
  rsink=.12d0/(.04d0+.96d0*min(1d0,max(0d0,om/1000)))
  tb=tb+dt*qb/cb;ts=ts+dt*(qs-(ts-25)/rsink)/cs
  tl=tl+dt*((x(13)**2/8d0)*merge(1d0,0d0,q(5))-(tl-25)*.15d0-merge(.6d0*(tl-25),0d0,lidopen))/10
  tedge=tb;if(f.and.code==1)tedge=tb-p(1)*max(0d0,tb-25)/70
  tt=tt+dt*(tedge-tt)/taut
  if(lock)then
   om=0
  else if(ckt_short==3)then
   om=max(0d0,om*(1-dt/.6d0))          ! shorted turns brake the rotor
  else
   om=max(0d0,om+dt*(ke*im-8.3d-7*om)/1d-6)
  end if
  iavg=iavg+(itec-iavg)*dt/.08d0;vavg=vavg+(vtec-vavg)*dt/.08d0;pavg=pavg+(itec*vtec-pavg)*dt/.08d0
  ufilt=ufilt+(u-ufilt)*dt/.05d0
  tq(1)=tset;tq(2)=tb;tq(3)=tmeas;tq(4)=ts;tq(5)=tl;tq(6)=tt;tq(7)=tedge;tq(8)=ufilt;tq(9)=iavg;tq(10)=vavg
  tq(11)=pavg;tq(12)=nint(4095*x(11)/3.3d0);tq(13)=nint(min(4095d0,4095*x(8)/3.3d0));tq(14)=om*60/(2*3.141592653589793d0)
  tq(15)=im;tq(16)=merge(1d0,0d0,q(5));tq(17)=x(13)**2/8;tq(18)=merge(1d0,0d0,interlock);tq(19)=bias
  tq(20)=itec;tq(21)=x(5)/.01d0;tq(22)=x(2);tq(23)=alpha*(tb-ts);tq(24)=nint(4095*x(12)/3.3d0)
  tr=0
  do it=1,6
   if(q(it))tr(it)=1
  end do
  vbe=0
 end subroutine
! @nativefn NR2-CKT-F0001
 subroutine heat_parts(x,q)
  ! dissipation of each part from the solved currents, first-order junction temperatures,
  ! slow-blow fuse I^2 t (10 A rating, 2 s) and burn marks above 150 C
  real(dp),intent(in)::x(22)
  logical,intent(in)::q(6)
  real(dp)::pw(ncomp),ib,rq3
  integer::k
  real(dp),parameter::rth(ncomp)=[40d0,40d0,40d0,40d0,60d0,1d0,40d0,1d0,30d0,40d0,250d0,200d0,80d0,20d0]
  real(dp),parameter::cth(ncomp)=[.05d0,.05d0,.05d0,.05d0,.02d0,1d0,.05d0,1d0,.5d0,.05d0,.01d0,.01d0,.005d0,.5d0]
  pw=0
  rq3=merge(.02d0,1d6,q(3).or.ckt_short==1)
  pw(1)=merge((x(2)-x(3))**2/.02d0,0d0,q(1));pw(2)=merge((x(2)-x(4))**2/.02d0,0d0,q(2))
  pw(3)=(x(3)-x(5))**2/rq3;pw(4)=merge((x(4)-x(5))**2/.02d0,0d0,q(4))
  pw(5)=x(5)**2/.01d0;pw(7)=merge((x(2)-x(13))**2/.02d0,0d0,q(5));pw(9)=x(21)**2*merge(.5d0,24d0,ckt_short==3)
  pw(10)=merge(x(16)**2/.02d0,0d0,q(6))
  if(ckt_short==2)pw(6)=(x(3)-x(4))**2/5d-3
  pw(11)=max(0d0,(vo(7)-vo(6))*vo(5)/10);pw(12)=max(0d0,(vo(6)-vo(5))*vo(5)/10)
  if(ckt_short==4)pw(11)=(vo(7)-vo(6))**2/.01d0
  ib=(x(1)-x(2))/merge(1d6,.05d0,fuse_blown);ipk=max(ipk,abs(ib))
  pw(13)=ib**2*.01d0;pw(14)=((x(2)-xt_prev(2))*470d-6/dt)**2*.05d0
  tenv=25+.05d0*(ts-25)
  do k=1,ncomp
   comp_t(k)=comp_t(k)+dt*(pw(k)-(comp_t(k)-tenv)/rth(k))/cth(k)
  end do
  comp_t(6)=max(comp_t(6),.5d0*(tb+ts));comp_t(8)=max(comp_t(8),tl)
  fuse_th=fuse_th+dt*((ib/10)**2-fuse_th/2d0)
  if(fuse_th>4.5d0.and..not.fuse_blown)then
   fuse_blown=.true.;burnt(13)=.true.
  end if
  do k=1,ncomp
   if(comp_t(k)>150)burnt(k)=.true.
  end do
 end subroutine
! @nativefn NR2-CKT-F0002
 function short_name(k) result(s)
  integer,intent(in)::k
  character(len=40)::s
  select case(k)
  case(1)
   s='Q3 drain-source short'
  case(2)
   s='TEC1 terminals shorted'
  case(3)
   s='Fan winding short'
  case(4)
   s='LED D1 shorted'
  case default
   s='off'
  end select
 end function
! @nativefn NR2-CKT-F0003
 function comp_name(k) result(s)
  integer,intent(in)::k
  character(len=12)::s
  character(len=12),parameter::nm(ncomp)=[character(len=12)::'Q1','Q2','Q3','Q4','Rsh','TEC1','Q5','Lid heater', &
   'Fan M1','Q6','LED D1','Q1 (LED)','F1 fuse','C1']
  s=nm(k)
 end function
! ------------------------------------------------------------------ optical circuit
 subroutine step_optical(code,p,f,tm)
  integer,intent(in)::code
  real(dp),intent(in)::p(6),tm
  logical,intent(in)::f
  real(dp)::x(18),xold(18),vset,trans,gain,ceil,sig,cyc,iph,noise,base,r2,leak,codev,dts,auto,fr,dmax
  logical::chg
  logical::ok
  integer::it
  vset=.2d0;trans=1;gain=1;ceil=4095;noise=.25d0;leak=.02d0;auto=0
  cyc=1+39*tm/loop_s
  sig=.06d0+1/(1+exp(-(cyc-22)/1.8d0))
  if(f)then
   select case(code)
   case(4);trans=p(1)
   case(5,17);leak=leak+p(1)
   case(6);vset=vset*p(1)
   case(7);fr=min(1d0,tm/loop_s);vset=vset*(p(2)+(p(1)-p(2))*fr)
   case(8);gain=p(1);ceil=p(2)
   case(9);noise=noise*p(1)
   case(10);trans=p(1);noise=noise*p(2)
   case(29);trans=p(1)
   case(30);auto=p(1)
   case(39);sig=sig*(1-min(.9d0,p(1)*cyc))
   case(41);trans=1-.35d0*rand1()*merge(1d0,.3d0,mod(tm,3d0)<1)
   case(45);if(cyc<p(1)*4)sig=sig*.7d0
   case(46);sig=sig*min(1d0,.6d0+.4d0*tm/loop_s)
   end select
  end if
  ! photocurrent: LED optical power (0.3 W/A) x collection x signal x responsivity (0.4 A/W)
  iph=.4d0*.3d0*max(0d0,tq_led())*4.2d-5*sig*trans
  iph=iph+noise*1d-9*gauss()
  r2=2d4*(3*gain-1)/2
  x=xo_prev
  do it=1,80
   xold=x
   call clear(13,5)
   call st_v(1,1,0,5d0)                          ! +5 V
   call st_v(2,2,0,vset)                         ! DAC setpoint
   call st_opamp(3,3,2,5,2d3,4.8d0,x)            ! U1 drives the transistor base
   call st_r(3,4,1d3)                            ! base resistor
   call st_npn(6,4,5,1d-14,150d0,1d0,x)          ! Q1
   call st_r(5,0,10d0)                           ! emitter sense resistor: I_led = Vset/10 ohm
   call st_r(1,7,47d0)                           ! LED series resistor
   call st_diode(7,6,1d-20,1.9d0,x)              ! LED (anode 7, cathode 6)
   if(ckt_short==4)call st_r(7,6,1d-2)
   call st_r(6,5,1d7)
   call st_i(8,0,iph)                            ! photodiode current out of the TIA input
   call st_cap(8,0,20d-12,xo_prev(8));call st_r(8,0,1d9)
   call st_opamp(4,9,0,8,1d5,4.8d0,x)            ! U2 transimpedance amplifier
   call st_r(9,8,1d7);call st_cap(9,8,10d-12,xo_prev(9)-xo_prev(8))
   call st_opamp(5,10,9,11,1d5,4.8d0,x)          ! U3 non-inverting gain stage
   call st_r(11,0,1d4);call st_r(10,11,r2)
   call st_r(10,12,1d3);call st_cap(12,0,1d-6,xo_prev(12))   ! anti-alias filter
   call st_r(12,13,1d2);call st_r(13,0,1d7)                  ! ADC input
   call solve(ok);if(.not.ok)exit
   x=xa(1:18)
   dmax=maxval(abs(x(1:13)-xold(1:13)))                          ! damped Newton step
   if(dmax>1d0)x=xold+(x-xold)/dmax
   chg=.false.
   call rail_check(3,3,2,5,2d3,4.8d0,x,chg);call rail_check(4,9,0,8,1d5,4.8d0,x,chg)
   call rail_check(5,10,9,11,1d5,4.8d0,x,chg)
   if(.not.chg.and.maxval(abs(x-xold))<1d-7)exit
  end do
  if(.not.ok)return
  xo_prev=x;vo=x
  codev=max(0d0,x(13))/3.3d0*4095+auto
  oq(16)=0
  if(codev>=ceil)then
   codev=ceil;oq(16)=1
  end if
  oq(1)=x(5)/10;oq(2)=vset;oq(3)=iph*1d9;oq(4)=x(9);oq(5)=x(10);oq(6)=x(13);oq(7)=nint(codev);oq(8)=sig
  oq(9)=gain*3;oq(10)=nint(min(4095d0,leak*codev+60));oq(11)=(x(5)/10)*300;oq(12)=x(6)-x(5);oq(13)=x(4)-x(5)
  oq(14)=noise;oq(15)=cyc
 contains
  real(dp) function tq_led()
   tq_led=xo_prev(5)/10
   if(tq_led<=0)tq_led=vset/10
  end function
 end subroutine
end module native_circuit
