! SPDX-License-Identifier: GPL-3.0-only
! Reduced native application model. All protocol, batch, analysis and review decisions are Fortran.
module native_model
 use iso_c_binding
 use, intrinsic :: ieee_arithmetic
 use native_scenarios
 use r2_math
 use stdlib_browser_stats, only:stdlib_mean
 use r2_chemistry
 use r2_thermal
 use r2_monitor
 use r2_optics
 implicit none
 integer, parameter :: maxruns=32,maxw=14,maxtrace=8000
 type run_t
  integer :: serial=0,nw=0,ncy=0,status=0,seed=1,ne=3,nr=3,copies=30000,threshold=150,fault=0,nt=0,scenario=0,planned_cycles=45,present(maxw)=1
  real(c_double) :: y(45,maxw)=0,y2(45,maxw)=0,cq(maxw)=0,cq2(maxw)=0,params(6)=0,trace(6,maxtrace)=0,elapsed=0,maxgap=0
 end type
 type(run_t), save :: current,history(maxruns)
 integer, save :: nh=0,selected=0,phase=0,remaining=0
 integer, save :: p_runs=1,p_ext=3,p_rep=3,p_copies=30000,p_threshold=150,p_seed=11,p_fault=0,p_speed=1000
 logical, save :: running=.false.
 real(c_double),save :: plant(19),pid(4,2),chem(34,maxw),assay(14,maxw),probe(6,maxw),camera(9,maxw),chem2(34,maxw),assay2(14,maxw),probe2(6,maxw),camera2(9,maxw)
 real(c_double),save :: random=1,independent=2,reference_random=3,scenario_random=4,contact(maxw)=25,measured(2)=25,monitor(6)=0,hold=0,target=95,budget=0
 character(len=160),save :: message='Ready. Native Fortran application.'
contains
! @nativefn NR2-M01-F0001
 function integer_text(v) result(s)
  integer, intent(in) :: v
  character(len=32) :: s
  character(len=32) :: rev
  integer :: q,n,k,d
  q=abs(v);n=0;s='';rev=''
  do
   n=n+1;d=mod(q,10);rev(n:n)=achar(48+d);q=q/10;if(q==0)exit
  end do
  k=1;if(v<0)then
   s(1:1)='-';k=2
  end if
  do q=n,1,-1
   s(k:k)=rev(q:q);k=k+1
  end do
 end function
! @nativefn NR2-M01-F0002
 function real_text(v,decimals) result(s)
  real(c_double),intent(in)::v
  integer,intent(in)::decimals
  character(len=40)::s
  character(len=32)::a,b
  integer::q,scale,j,rem
  s='NA';if(.not.ieee_is_finite(v))return
  scale=10**decimals;q=nint(abs(v)*scale);a=integer_text(q/scale);s=trim(a)
  if(decimals>0)then
   b='';rem=mod(q,scale)
   do j=decimals,1,-1
    b(j:j)=achar(48+mod(rem,10));rem=rem/10
   end do
   s=trim(a)//'.'//b(1:decimals)
  end if
  if(v<0)s='-'//trim(s)
 end function
! @nativefn NR2-M01-F0003
 subroutine analyse_curve(y,n,threshold,cq)
  real(c_double),intent(in)::y(45),threshold
  integer,intent(in)::n
  real(c_double),intent(out)::cq
  real(c_double)::base,d,bvals(5)
  integer::k,b,cnt,j
  cq=0;if(n<5)return
  cnt=count(y(1:5)>=0);if(cnt<3)return
  j=0
  do k=1,5
   if(y(k)>=0)then
    j=j+1;bvals(j)=y(k)
   end if
  end do
  base=stdlib_mean(bvals(1:cnt))
  do k=6,n
   if(y(k)<0.or.y(k-1)<0)cycle
   d=y(k)-y(k-1);b=min(n,k+2)
   if(y(k-1)-base<threshold.and.y(k)-base>=threshold.and.d>0)then
    if(any(y(k:b)<0).or.minval(y(k:b)-base)<threshold)cycle
    cq=k-1+(threshold-(y(k-1)-base))/d;return
   end if
  end do
 end subroutine
! @nativefn NR2-M01-F0011
 subroutine analyse(r)
  type(run_t),intent(inout)::r
  real(c_double)::temp(45),thr
  integer::w
  r%cq=0;r%cq2=0;thr=real(r%threshold,c_double)
  if(r%scenario==16)thr=r%params(1)
  do w=1,r%nw
   if(r%present(w)==0)cycle
   temp=r%y(:,w)
   if(r%scenario==18)then
    where(r%y(:,w)>=0.and.r%y2(:,w)>0)temp=r%params(1)*r%y(:,w)/max(1d0,r%y2(:,w))
   end if
   call analyse_curve(temp,r%ncy,thr,r%cq(w))
   call analyse_curve(r%y2(:,w),r%ncy,real(r%threshold,c_double),r%cq2(w))
  end do
 end subroutine
! @nativefn NR2-M01-F0012
 integer function verdict(r) result(v)
  type(run_t),intent(in)::r
  integer::s
  s=r%ne*r%nr;v=3
  if(r%status/=1.or.r%ncy/=45)return
  v=2
  if(any(r%present(s+1:s+5)==0))return
  if(r%scenario==37.and.nint(r%params(1))/=0)return
  if(r%cq(s+1)<=0.or.r%cq(s+2)>0.or.r%cq(s+3)>0)return
  if(r%cq(s+4)<=0.and.r%cq(s+5)<=0)return
  v=1
  if(r%scenario/=0)v=4
 end function
! @nativefn NR2-M01-F0005
 subroutine begin_run()
  integer::w,s,i,ex
  real(c_double)::p(6),shifts(3),f,t
  current=run_t();current%serial=nh+1;current%nw=p_ext*p_rep+5
  current%ne=p_ext;current%nr=p_rep;current%copies=p_copies;current%threshold=p_threshold
  current%seed=p_seed+nh*7919;current%fault=p_fault;current%scenario=p_scenario
  if(p_scenario>0)current%params=scenario_bank(:,p_scenario)
  p=current%params
  if(p_scenario==20)current%planned_cycles=nint(p(2))
  plant=0;plant(1:7)=25;plant(11)=.5d0;plant(12)=1
  pid=0;monitor=0;monitor(2)=-1;phase=0;hold=0;budget=0;target=95
  random=real(current%seed,c_double)+p(6);independent=random+17431;reference_random=random+27183
  scenario_random=random+42443+p(6);camera=0;camera2=0;contact=25;measured=25
  do i=1,3
   shifts(i)=rng_uniform(scenario_random)*2-1
  end do
  s=p_ext*p_rep
  do w=1,current%nw
   assay(:,w)=[1d0,real(p_copies,c_double),120d0,84d0,91d0,62d0,300d0,.95d0,.3d0,50d0,76d0,1d0,1d0,0d0]
   if(w==s+1)assay(2,w)=10000
   if(w==s+2.or.w==s+3)assay(2,w)=0
   if(w>=s+4)assay(2,w)=15
   assay2(:,w)=assay(:,w);assay2(2,w)=0
   if(p_scenario==48.or.p_scenario==49.or.p_scenario==51)then
    if(w<=s.or.w==s+1)assay2(2,w)=10000
   end if
   if(p_scenario>0.and.affects(p_scenario,p,w,p_ext,p_rep))then
    select case(p_scenario)
    case(15)
     assay(2,w)=assay(2,w)*p(1)
    case(23,27)
     assay(13,w)=p(1);assay(8,w)=assay(8,w)*p(2)
    case(31)
     assay(7,w)=assay(7,w)*p(1)
    case(32)
     assay(12,w)=0
    case(40,49)
     assay(2,w)=assay(2,w)/p(1)
    case(42)
     f=sqrt(log(1+(p(1)/100)**2));f=exp(f*rng_gauss(scenario_random)-f*f/2)
     assay(2,w)=assay(2,w)*f;assay2(2,w)=assay2(2,w)*f
    case(43)
     assay(2,w)=assay(2,w)+p(1)
    case(44)
     i=mod(well_position(w,p_ext,p_rep),24)
     f=1-(1-p(1))*min(1d0,real(i,c_double)/max(1,p_rep-1));assay(2,w)=assay(2,w)*f
    case(48)
     assay(8,w)=assay(8,w)*p(1)
    case(50)
     if(w==s+3)assay(2,w)=assay(2,w)+p(1)
    case(51)
     ex=min(3,max(1,(w-1)/p_rep+1));assay2(2,w)=assay2(2,w)*2**(p(1)*shifts(ex))
    end select
   end if
   call r2_chem_init(chem(:,w),assay(:,w),random)
   call r2_chem_init(chem2(:,w),assay2(:,w),reference_random)
   f=200
   if(p_scenario==31.and.affects(p_scenario,p,w,p_ext,p_rep))f=200*p(2)
   if(p_scenario==25.and.affects(p_scenario,p,w,p_ext,p_rep))f=200-p(1)
   call r2_probe_init(probe(:,w),f,0d0);call r2_probe_init(probe2(:,w),200d0,0d0)
   if(p_scenario==32.and.affects(p_scenario,p,w,p_ext,p_rep))then
    do i=1,nint(p(1)*3600)
     call r2_chem_step(chem(:,w),assay(:,w),p(2),1d0,scenario_random)
    end do
   end if
  end do
  if(p_scenario==38)then
   if(nint(p(1))==1)current%present(s+2)=0
   if(nint(p(2))==1)current%present(s+3)=0
  end if
  running=.true.;message='Running: native thermal / chemistry / detector model.'
  if(p_scenario==37.and.mod(nint(p(1)),180)/=0)call finish_run(6)
 end subroutine
! @nativefn NR2-M01-F0006
 subroutine start_batch()
  if(running)then
   message='A batch is already running.';return
  end if
  if(nh+p_runs>maxruns)then
   message='History capacity is 32 runs. Export and restart the application.';return
  end if
  if(.not.scenario_available(p_scenario))then
   message='Melt derivative scenario needs a SYBR/melt workflow; its settings remain editable.';return
  end if
  remaining=p_runs;call begin_run()
 end subroutine
! @nativefn NR2-M01-F0007
 subroutine finish_run(status)
  integer,intent(in)::status
  plant(8:10)=0;plant(12)=0;current%status=status;running=.false.
  call apply_record_scenario(current)
  call analyse(current)
  if(nh<maxruns)then
   nh=nh+1;history(nh)=current;selected=nh
  end if
  remaining=remaining-1
  if(status/=1)remaining=0
  if(status==1)message='Virtual run filed. Open MISSY Results.'
  if(status==2)message='Stopped by operator. Partial run filed; batch cancelled.'
  if(status==3)message='Independent monitor stopped the run. Heating inhibited.'
  if(status==4)message='Protocol time limit reached. Partial run filed.'
  if(status==5)message='Injected software crash: partial file retained; no run-complete event.'
  if(status==6)message='Invalid physical setup or unsupported plate orientation; run rejected.'
  if(status==7)message='Hardware interlock stopped the run; heating inhibited.'
 end subroutine
! @nativefn NR2-M01-F0008
 subroutine acquire()
  integer::w,k
  real(c_double)::cut,sig,sig2,late(2),p(6),base,light,noise,bright,dark,free,refcut
  logical::hit
  k=current%ncy+1;late=[.025d0,.025d0]
  p=current%params
  do w=1,current%nw
   if(current%present(w)==0)then
    current%y(k,w)=-1;current%y2(k,w)=-1;cycle
   end if
   hit=.false.;if(current%scenario>0)hit=affects(current%scenario,p,w,current%ne,current%nr)
   camera(1,w)=probe(1,w);camera(2,w)=probe(2,w);camera(3,w)=k
   cut=r2_camera_cleavage(camera(:,w),late,2,.61d0,0)
   camera2(1,w)=probe2(1,w);camera2(2,w)=probe2(2,w);camera2(3,w)=k
   refcut=r2_camera_cleavage(camera2(:,w),late,2,.61d0,0)
   bright=probe(1,w)/1.2044d10;dark=0;free=0;light=1;noise=1
   if(hit)then
    select case(current%scenario)
    case(4)
     light=1-(1-p(1))*mod(well_position(w,current%ne,current%nr),24)/23
    case(6)
     light=p(1)
    case(7)
     light=p(2)+(p(1)-p(2))*clamp((k-p(3))/max(1d0,45-p(3)),0d0,1d0)
    case(8)
     light=p(1)
    case(9)
     noise=p(1)
    case(10)
     light=p(1);noise=p(2)
    case(24)
     dark=bright*(1-p(1));bright=bright*p(1)
    case(25)
     free=p(1)
    case(29)
     light=p(1)
    end select
   end if
   sig=r2_probe_counts(probe(1,w),cut,bright,dark,free,.96d0,real(k,c_double),0d0,60d0)
   sig2=r2_probe_counts(probe2(1,w),refcut,200d0,0d0,0d0,.96d0,real(k,c_double),0d0,60d0)
   sig2=sig2+.08d0*max(0d0,sig-360)
   if(hit.and.current%scenario==30)sig=sig+p(1)
   sig=sig*light+2*noise*rng_gauss(random);sig2=sig2*light+2*noise*rng_gauss(reference_random)
   if(hit.and.current%scenario==8)then
    sig=min(p(2),sig);sig2=min(p(2),sig2)
   end if
   current%y(k,w)=real(max(0,min(65535,nint(sig))),c_double)
   current%y2(k,w)=real(max(0,min(65535,nint(sig2))),c_double)
  end do
  current%ncy=k
  if(current%scenario==22.and.k>=nint(p(1)))then
   call finish_run(5)
  else if(k==current%planned_cycles)then
   call finish_run(1)
  else
   phase=1;hold=0;target=95
  end if
 end subroutine
! @nativefn NR2-M01-F0009
 subroutine advance_one()
  integer::i,w,z,lastsec,nowsec
  real(c_double)::obs(6),cfg(4),mcfg(5),out(4),primary(2),secondary(2),seconds,dt,p(6),lid,ann,t,tm
  integer::code
  logical::hit
  dt=.05d0;code=current%scenario;p=current%params;ann=60;lid=105
  if(code==3)lid=p(1)
  if(code==20.or.code==34.or.code==35)ann=p(1)
  if(phase==2)target=ann
  cfg=[.55d0,.25d0,.06d0,.08d0]
  do z=1,2
   plant(7+z)=r2_pid_step(cfg,pid(:,z),target,measured(z),dt,.0045d0)
  end do
  if(code==2)plant(8)=plant(8)*p(1)
  plant(10)=clamp(.2d0*(lid-plant(5)),0d0,1d0);plant(11)=clamp(.3d0+(plant(6)-35)/20,.3d0,1d0)
  if(code==13.and.current%elapsed>=p(1))plant(13)=1
  if(code==13.and.current%elapsed>=p(1)+p(2))then
   call finish_run(7);return
  end if
  if(code==14.and.current%elapsed>=p(1))then
   call finish_run(7);return
  end if
  do i=1,50
   call r2_thermal_step(plant)
  end do
  current%elapsed=current%elapsed+dt
  do w=1,current%nw
   if(current%present(w)==0)cycle
   t=plant(3+mod(mod(well_position(w,current%ne,current%nr),24),2));tm=70
   hit=.false.;if(code>0)hit=affects(code,p,w,current%ne,current%nr)
   if(hit)then
    if(code==1)t=t-p(1)*max(0d0,min(1d0,(t-60)/35))
    if(code==11.or.code==28)then
     contact(w)=contact(w)+(t-contact(w))*(1-exp(-dt/p(1)));t=contact(w)
    end if
    if(code==33)tm=tm-p(1)
   end if
   call r2_chem_step(chem(:,w),assay(:,w),t,dt,random)
   call r2_probe_step(probe(:,w),chem(33,w),t,200d0,0d0,30d0,assay(7,w),tm,1.5d0)
   call r2_chem_step(chem2(:,w),assay2(:,w),t,dt,reference_random)
   call r2_probe_step(probe2(:,w),chem2(33,w),t,200d0,0d0,30d0,300d0,70d0,1.5d0)
  end do
  primary=plant(1:2);secondary=plant(1:2)
  do z=1,2
   primary(z)=primary(z)+.01d0*rng_gauss(random)
   secondary(z)=secondary(z)+.01d0*rng_gauss(independent)
  end do
  if(current%fault==1.and.current%elapsed>=60)secondary(1)=secondary(1)+5
  if(code==12.and.current%elapsed>=p(3))primary(1)=primary(1)-min(p(2),p(1)*(current%elapsed-p(3)))
  measured=primary
  seconds=current%elapsed*1000;obs=[primary,secondary,seconds,seconds]
  mcfg=[2d0,2000d0,1000d0,-5d0,103d0]
  call r2_monitor_step(monitor,obs,seconds,mcfg,out)
  current%maxgap=max(current%maxgap,out(3))
  nowsec=int(current%elapsed)
  if(nowsec>current%nt.and.current%nt<maxtrace)then
   current%nt=current%nt+1
   current%trace(:,current%nt)=[current%elapsed,primary,secondary(1),secondary(2),target]
  end if
  if(out(4)/=0)then
   call finish_run(3);return
  end if
  if(current%elapsed>7900)then
   call finish_run(4);return
  end if
  if(abs((plant(1)+plant(2))/2-target)<.5d0)then
   hold=hold+dt
  else
   hold=0
  end if
  if(phase==0.and.hold>=120)then
   phase=1;hold=0
  else if(phase==1.and.hold>=10)then
   phase=2;hold=0;target=ann
  else if(phase==2.and.hold>=30)then
   call acquire()
  end if
 end subroutine
! @nativefn NR2-M01-F0010
 subroutine apply_record_scenario(r)
  type(run_t),intent(inout)::r
  real(c_double)::p(6),base,plateau,old(45),old2(45),v,noise,rr
  integer::code,w,k,j,n,src,first,last
  code=r%scenario;if(code==0)return
  p=r%params;n=r%ncy;rr=real(r%seed,c_double)+42443+p(6)
  do w=1,r%nw
   if(.not.affects(code,p,w,r%ne,r%nr).or.r%present(w)==0)cycle
   old=r%y(:,w);old2=r%y2(:,w);base=old(1)
   select case(code)
   case(3)
    do k=max(1,nint(p(3))),n
     r%y(k,w)=max(0d0,real(nint(old(k)*(1-p(2)*(k-p(3)))*(1+.03d0*rng_gauss(rr))),c_double))
    end do
   case(5,17)
    do k=1,n
     r%y2(k,w)=real(nint(old2(k)+p(1)*max(0d0,old(k)-base)),c_double)
    end do
   case(19)
    first=nint(p(1));last=min(n,first+nint(p(2))-1)
    if(first<=n)then
     r%y(first:last,w)=-1;r%y2(first:last,w)=-1
    end if
   case(26)
    if(n<5)cycle
    base=sum(old(1:5))/5;plateau=p(1)*(maxval(old(1:n))-base)
    if(plateau<=0)cycle
    do k=1,n
     r%y(k,w)=real(max(0,nint(base+plateau*tanh((old(k)-base)/plateau))),c_double)
    end do
   case(36)
    do k=1,n
     r%y(k,w)=real(nint((1-p(1))*old(k)+p(1)*old2(k)),c_double)
     r%y2(k,w)=real(nint((1-p(1))*old2(k)+p(1)*old(k)),c_double)
    end do
   case(39)
    do k=max(1,nint(p(2))),n
     r%y(k,w)=real(max(0,nint(old(k)*(1-p(1)*(k-p(2))))),c_double)
    end do
   case(41)
    do j=1,nint(p(1))
     k=min(n-1,nint(p(3))+3*(j-1));if(k<1)cycle
     r%y(k,w)=real(max(0,nint(base+(old(k)-base)*(1-p(2))-.25d0*base)),c_double)
     r%y(k+1,w)=real(nint(old(k+1)*1.18d0),c_double)
    end do
   case(45,46)
    j=nint(p(1));if(code==46)j=nint(p(2))
    do k=1,n
     src=max(1,k-j);r%y(k,w)=old(src);r%y2(k,w)=old2(src)
     if(code==46)then
      r%y(k,w)=r%y(k,w)+p(1)
      if(k==5)r%y(k,w)=r%y(k,w)+p(3)
     end if
    end do
   case(47)
    do k=max(1,nint(p(2))),n
     r%y(k,w)=old(k)+p(1)*(k-p(2))
    end do
   end select
  end do
  where(r%y>=0)r%y=min(65535d0,anint(r%y))
  where(r%y2>=0)r%y2=min(65535d0,anint(r%y2))
 end subroutine
! @nativefn NR2-M01-F0013
 subroutine tick_model(wall_ms)
  real(c_double),intent(in)::wall_ms
  integer::i,steps
  if(.not.running)then
   if(remaining>0)call begin_run()
   return
  end if
  budget=budget+min(1000d0,max(0d0,wall_ms))*p_speed/1000
  steps=min(2000,int(budget/.05d0));budget=budget-steps*.05d0
  do i=1,steps
   if(.not.running)exit
   call advance_one()
  end do
 end subroutine
end module
