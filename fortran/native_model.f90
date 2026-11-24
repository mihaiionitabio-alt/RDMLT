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
 ! Instrument record (exported as RDMLt telemetry, events, optical reads and chemistry snapshots).
 integer, parameter :: ntel=28,maxev=1200,snap_every=10,nsnapf=16,nhf=20,nsg=20
 type run_t
  integer :: serial=0,nw=0,ncy=0,status=0,seed=1,ne=3,nr=3,copies=30000,threshold=150,fault=0,nt=0,scenario=0,planned_cycles=45,present(maxw)=1
  real(c_double) :: y(45,maxw)=0,y2(45,maxw)=0,cq(maxw)=0,cq2(maxw)=0,params(6)=0,trace(6,maxtrace)=0,elapsed=0,maxgap=0
  integer :: ntel_rows=0,nev=0,nsnap=0,lid_ready=0,recorded=0
  character(len=29) :: started='',finished=''
  real(c_float) :: tele(ntel,maxtrace)=0
  real(c_double) :: ev(4,maxev)=0,acq_t(45)=0,acq_zone(2,45)=0
  real(c_float) :: rawsig(45,maxw,2)=0,cutf(45,maxw,2)=0,lightf(45,maxw)=1
  integer :: prog=1,nhold=2,hf_slot=-1
  real(c_double) :: hold_t(2)=[50d0,95d0],hold_s(2)=[120d0,600d0],den_s=15,ann_hold=60,ann_t=60,lid_t=105
  real(c_double) :: assay_s(14,maxw)=0,assay2_s(14,maxw)=0,probe_f(maxw)=200,probe_tm(maxw)=70
 end type
 ! High-rate record of a run, allocated on demand: 10 Hz telemetry (v), 10 s snapshots of the
 ! controller/plant (g) and of every well (c). Slot 0 is the running run; finished runs move here.
 type hf_t
  real(c_float),allocatable :: v(:,:),g(:,:),c(:,:,:)
  integer :: n=0,ns=0
 end type
 type(hf_t),save :: hfr(0:maxruns)
 ! Event codes (names in event_name): stable numeric identifiers inside the run record.
 integer, parameter :: ev_boot=1,ev_run_start=2,ev_selftest_ok=3,ev_lid_ready=4,ev_stage_start=5,ev_step_start=6, &
  ev_target_reached=7,ev_step_end=8,ev_read_start=9,ev_read_end=10,ev_run_complete=11,ev_run_aborted=12, &
  ev_monitor_stop=13,ev_interlock=14,ev_time_limit=15,ev_host_crash=16,ev_setup_rejected=17
 type(run_t), save :: current,history(maxruns)
 integer, save :: nh=0,selected=0,phase=0,remaining=0
 integer, save :: p_prog=1,p_runs=1,p_ext=3,p_rep=3,p_copies=30000,p_threshold=150,p_seed=11,p_fault=0,p_speed=168
 logical, save :: running=.false.
 real(c_double),save :: plant(19),pid(4,2),chem(34,maxw),assay(14,maxw),probe(6,maxw),camera(9,maxw),chem2(34,maxw),assay2(14,maxw),probe2(6,maxw),camera2(9,maxw)
 integer, save :: hf_tick=0
 real(c_double),save :: random=1,independent=2,reference_random=3,scenario_random=4,contact(maxw)=25,measured(2)=25,monitor(6)=0,hold=0,target=95,budget=0
 character(len=160),save :: message='Ready. Native Fortran application.'
 ! Scenario preview: a normal reference run and a fault run simulated with the full model
 ! (same seed, telemetry logging skipped) and kept out of the run history.
 logical,save :: preview_mode=.false.
 integer,save :: bg_code=-1,bg_phase=0
 integer,save :: pv_state=0,pv_code=-1,pv_key(8)=-1,pv_saved_scenario=0,pv_steps=3000
 ! Curves and calls of a preview run (the full record is not kept).
 type curve_t
  integer :: nw=0,ncy=0,ne=3,nr=3,status=0,present(maxw)=1
  real(c_double) :: y(45,maxw)=0,y2(45,maxw)=0,cq(maxw)=0,cq2(maxw)=0,params(6)=0
 end type
 type(curve_t),save :: pv_norm,pv_fault
 ! Start-up / background cache of preview curves: index 0 = normal reference, 1..51 = cases.
 ! Valid for one profile key; a cached case is replayed at the real-run pace (pv_state 4).
 type(curve_t),allocatable,save :: pv_cache(:)
 logical,save :: pv_cached(0:51)=.false.,pv_bg=.false.,warm_on=.false.,warm_hold=.false.,pv_force=.false.
 integer,save :: cache_key(8)=-1,warm_next=1
 real(c_double),save :: replay_t=0
 ! Multi-case selection: number of runs requested per scenario code (0 = normal run).
 integer,save :: sel_n(0:51)=0,sel_last(0:51)=1,qn=0,qpos=0,qcode(52)=0,qreps(52)=0,q_saved=0
 logical,save :: queue_active=.false.
 ! Real (wall-clock) seconds of the acquisition in progress and the instrument time of the last
 ! completed run, used to show real versus simulated time (about 30 s per run at the default 168x).
 real(c_double),save :: wall_run=0,last_full_elapsed=5040,pv_budget=0
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
! @nativefn NR2-M01-F0040
 logical function ui_busy() result(b)
  ! A real acquisition is in progress (a background scenario preview does not count).
  b=running.and..not.preview_mode
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
! @nativefn NR2-M01-F0019
 function iso_now() result(s)
  ! Wall-clock ISO 8601 time with UTC offset (browser playback time, not instrument time).
  character(len=29)::s
  integer::v(8),off
  character(len=1)::sg
  call date_and_time(values=v)
  s=''
  if(v(1)<1970)return
  off=v(4);sg='+';if(off<0)sg='-'
  off=abs(off)
  s=trim(integer_text(v(1)))//'-'//d2(v(2))//'-'//d2(v(3))//'T'//d2(v(5))//':'//d2(v(6))//':'//d2(v(7))//'.'// &
   d3(v(8))//sg//d2(off/60)//':'//d2(mod(off,60))
 contains
  function d2(i) result(t)
   integer,intent(in)::i
   character(len=2)::t
   t=achar(48+mod(i/10,10))//achar(48+mod(i,10))
  end function
  function d3(i) result(t)
   integer,intent(in)::i
   character(len=3)::t
   t=achar(48+mod(i/100,10))//achar(48+mod(i/10,10))//achar(48+mod(i,10))
  end function
 end function
! @nativefn NR2-M01-F0014
 function event_name(code) result(s)
  integer,intent(in)::code
  character(len=20)::s
  select case(code)
  case(ev_boot);s='BOOT'
  case(ev_run_start);s='RUN_START'
  case(ev_selftest_ok);s='SELFTEST_OK'
  case(ev_lid_ready);s='LID_READY'
  case(ev_stage_start);s='STAGE_START'
  case(ev_step_start);s='STEP_START'
  case(ev_target_reached);s='TARGET_REACHED'
  case(ev_step_end);s='STEP_END'
  case(ev_read_start);s='READ_START'
  case(ev_read_end);s='READ_END'
  case(ev_run_complete);s='RUN_COMPLETE'
  case(ev_run_aborted);s='RUN_ABORTED'
  case(ev_monitor_stop);s='MONITOR_STOP'
  case(ev_interlock);s='INTERLOCK_STOP'
  case(ev_time_limit);s='TIME_LIMIT'
  case(ev_host_crash);s='HOST_CRASH'
  case(ev_setup_rejected);s='SETUP_REJECTED'
  case default;s='UNKNOWN'
  end select
 end function
! @nativefn NR2-M01-F0015
 subroutine log_event(code,a,b)
  ! Appends one firmware-style event at the current simulated time. Never alters model state.
  integer,intent(in)::code,a,b
  if(current%nev>=maxev)return
  current%nev=current%nev+1
  current%ev(:,current%nev)=[current%elapsed*1000,real(code,c_double),real(a,c_double),real(b,c_double)]
 end subroutine
! @nativefn NR2-M01-F0016
 subroutine stage_step(ph,stage,step)
  ! Native protocol mapping: phase 0 = stage 0 (initial denaturation); phases 1/2 = stage 1 steps 0/1.
  integer,intent(in)::ph
  integer,intent(out)::stage,step
  stage=min(ph,current%nhold);step=0
  if(ph==current%nhold+1)step=1
 end subroutine
! @nativefn NR2-M01-F0017
 subroutine log_telemetry(primary,secondary,gap)
  ! One row per simulated second, logged at the same instant as the existing trace.
  real(c_double),intent(in)::primary(2),secondary(2),gap
  integer::stage,step,state,cyc,busy
  if(preview_mode)return
  if(current%ntel_rows>=maxtrace)return
  call stage_step(phase,stage,step)
  state=2;if(hold>0)state=3
  cyc=0;if(stage==current%nhold)cyc=current%ncy+1
  busy=0
  if(current%ncy>0)then
   if(current%elapsed-current%acq_t(current%ncy)<1.5d0)busy=1
  end if
  current%ntel_rows=current%ntel_rows+1
  current%tele(:,current%ntel_rows)=real([current%elapsed,target,primary(1),primary(2),plant(5),plant(6), &
   (secondary(1)+secondary(2))/2,plant(8),plant(9),plant(10),plant(11),plant(16),plant(17),plant(19),plant(12), &
   real(state,c_double),real(stage,c_double),real(step,c_double),real(cyc,c_double),plant(3),plant(4),plant(1),plant(2), &
   plant(7),plant(18),gap,real(busy,c_double),plant(13)],c_float)
 end subroutine
! @nativefn NR2-M01-F0018
 subroutine log_chemistry()
  ! 10 s snapshot: controller and plant state (g) and the reaction state of every well (c).
  integer::w,k
  real(c_double)::tw
  if(preview_mode)return
  if(.not.allocated(hfr(0)%g))return
  if(hfr(0)%ns>=size(hfr(0)%g,2))return
  hfr(0)%ns=hfr(0)%ns+1;k=hfr(0)%ns
  hfr(0)%g(:,k)=real([current%elapsed,plant(3),plant(4),target,pid(1,1),pid(1,2),pid(2,1),pid(2,2),plant(1),plant(2), &
   plant(5),plant(6),plant(7),plant(16),plant(17),plant(18),plant(12),plant(13),real(current%ncy,c_double),0d0],c_float)
  do w=1,current%nw
   tw=plant(3+mod(mod(well_position(w,current%ne,current%nr),24),2))
   hfr(0)%c(:,w,k)=real([chem(27,w),chem(1,w),chem(2,w),sum(chem(3:12,w)),chem(13,w),chem(14,w),chem(28,w), &
    chem(29,w),chem(30,w),chem(31,w),chem(32,w),chem(33,w),probe(2,w)/max(1d0,probe(1,w)),probe(1,w), &
    sigmoid((current%probe_tm(w)-tw)/1.5d0),current%probe_tm(w)],c_float)
  end do
 end subroutine
! @nativefn NR2-M01-F0020
 subroutine log_highrate(primary,secondary)
  ! 10 Hz telemetry row (every second 50 ms model step), same quantities as the R2 record.
  real(c_double),intent(in)::primary(2),secondary(2)
  integer::stage,step,state,cyc,busy,k
  if(preview_mode)return
  if(.not.allocated(hfr(0)%v))return
  if(hfr(0)%n>=size(hfr(0)%v,2))return
  call stage_step(phase,stage,step)
  state=2;if(hold>0)state=3
  cyc=0;if(stage==current%nhold)cyc=current%ncy+1
  busy=0
  if(current%ncy>0)then
   if(current%elapsed-current%acq_t(current%ncy)<0.2d0*current%nw+0.05d0)busy=1
  end if
  hfr(0)%n=hfr(0)%n+1;k=hfr(0)%n
  hfr(0)%v(:,k)=real([current%elapsed,target,primary(1),primary(2),plant(5),plant(6),(secondary(1)+secondary(2))/2, &
   plant(8),plant(9),plant(10),plant(11),plant(16),plant(17),plant(19),plant(12),real(state,c_double), &
   real(stage,c_double),real(step,c_double),real(cyc,c_double),real(busy,c_double)],c_float)
 end subroutine
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
  if(preview_mode)current%seed=p_seed
  if(.not.preview_mode)wall_run=0
  if(p_scenario>0)current%params=scenario_bank(:,p_scenario)
  p=current%params
  if(p_scenario==20)current%planned_cycles=nint(p(2))
  plant=0;plant(1:7)=25;plant(11)=.5d0;plant(12)=1
  current%prog=p_prog
  if(p_prog==1)then
   current%nhold=2;current%hold_t=[50d0,95d0];current%hold_s=[120d0,600d0];current%den_s=15;current%ann_hold=60
  else
   current%nhold=1;current%hold_t=[95d0,95d0];current%hold_s=[120d0,0d0];current%den_s=10;current%ann_hold=30
  end if
  current%ann_t=60;if(p_scenario==20.or.p_scenario==34.or.p_scenario==35)current%ann_t=p(1)
  current%lid_t=105;if(p_scenario==3)current%lid_t=p(1)
  pid=0;monitor=0;monitor(2)=-1;phase=0;hold=0;budget=0;target=current%hold_t(1);hf_tick=0
  if(allocated(hfr(0)%v))deallocate(hfr(0)%v)
  if(allocated(hfr(0)%g))deallocate(hfr(0)%g)
  if(allocated(hfr(0)%c))deallocate(hfr(0)%c)
  allocate(hfr(0)%v(nhf,80000),hfr(0)%g(nsg,800),hfr(0)%c(nsnapf,current%nw,800));hfr(0)%n=0;hfr(0)%ns=0
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
   current%assay_s(:,w)=assay(:,w);current%assay2_s(:,w)=assay2(:,w);current%probe_f(w)=f
   current%probe_tm(w)=70;if(p_scenario==33.and.affects(p_scenario,p,w,p_ext,p_rep))current%probe_tm(w)=70-p(1)
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
  current%recorded=1;current%started=iso_now()
  call log_event(ev_boot,0,0);call log_event(ev_run_start,0,0);call log_event(ev_selftest_ok,0,0)
  call log_event(ev_stage_start,0,0);call log_event(ev_step_start,0,0)
  if(p_scenario==37.and.mod(nint(p(1)),180)/=0)then
   call log_event(ev_setup_rejected,nint(p(1)),0);call finish_run(6)
  end if
 end subroutine
! @nativefn NR2-M01-F0006
 subroutine start_batch()
  if(preview_mode)call cancel_preview()
  if(running)then
   message='A batch is already running.';return
  end if
  queue_active=.false.;qn=0
  if(nh+p_runs>maxruns)then
   message='History capacity is 32 runs. Export and restart the application.';return
  end if
  if(.not.scenario_available(p_scenario))then
   message='Melt derivative scenario needs a SYBR/melt workflow; its settings remain editable.';return
  end if
  remaining=p_runs;call begin_run()
 end subroutine
! @nativefn NR2-M01-F0021
 subroutine file_highrate(slot)
  ! Moves the running record into a history slot, trimmed to the rows actually written.
  integer,intent(in)::slot
  real(c_float),allocatable::t2(:,:),t3(:,:,:)
  if(.not.allocated(hfr(0)%v))return
  allocate(t2(nhf,max(1,hfr(0)%n)));t2=0;t2(:,1:hfr(0)%n)=hfr(0)%v(:,1:hfr(0)%n);call move_alloc(t2,hfr(slot)%v)
  allocate(t2(nsg,max(1,hfr(0)%ns)));t2=0;t2(:,1:hfr(0)%ns)=hfr(0)%g(:,1:hfr(0)%ns);call move_alloc(t2,hfr(slot)%g)
  allocate(t3(nsnapf,size(hfr(0)%c,2),max(1,hfr(0)%ns)));t3=0;t3(:,:,1:hfr(0)%ns)=hfr(0)%c(:,:,1:hfr(0)%ns)
  call move_alloc(t3,hfr(slot)%c)
  hfr(slot)%n=hfr(0)%n;hfr(slot)%ns=hfr(0)%ns
  deallocate(hfr(0)%v,hfr(0)%g,hfr(0)%c);hfr(0)%n=0;hfr(0)%ns=0
 end subroutine
! @nativefn NR2-M01-F0007
 subroutine finish_run(status)
  integer,intent(in)::status
  plant(8:10)=0;plant(12)=0;current%status=status;running=.false.;current%finished=iso_now()
  if(preview_mode)then
   call apply_record_scenario(current);call analyse(current);call preview_filed();return
  end if
  if(status==1.and.current%ncy>=45)last_full_elapsed=current%elapsed
  if(status==1)then
   call log_event(ev_run_complete,0,0)
  else if(status==5)then
   call log_event(ev_host_crash,current%ncy,0)
  else
   call log_event(ev_run_aborted,status,current%ncy)
  end if
  call apply_record_scenario(current)
  call analyse(current)
  if(nh<maxruns)then
   nh=nh+1;call file_highrate(nh);current%hf_slot=nh;history(nh)=current;selected=nh
  end if
  remaining=remaining-1
  if(status==2)then
   if(queue_active)p_scenario=q_saved
   remaining=0;queue_active=.false.;qn=0
  else if(status/=1.and..not.queue_active)then
   remaining=0
  end if
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
  call log_event(ev_read_start,k,0)
  current%acq_t(k)=current%elapsed;current%acq_zone(:,k)=measured
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
   current%rawsig(k,w,:)=real([sig,sig2],c_float);current%lightf(k,w)=real(light,c_float)
   current%cutf(k,w,:)=real([cut/max(1d0,probe(1,w)),refcut/max(1d0,probe2(1,w))],c_float)
  end do
  call log_event(ev_read_end,k,0)
  current%ncy=k
  if(current%scenario==22.and.k>=nint(p(1)))then
   call finish_run(5)
  else if(k==current%planned_cycles)then
   call finish_run(1)
  else
   call log_event(ev_step_end,current%nhold,1)
   phase=current%nhold;hold=0;target=95
   call log_event(ev_step_start,current%nhold,0)
  end if
 end subroutine
! @nativefn NR2-M01-F0009
 subroutine advance_one()
  integer::i,w,z,lastsec,nowsec
  real(c_double)::obs(6),cfg(4),mcfg(5),out(4),primary(2),secondary(2),seconds,dt,p(6),lid,ann,t,tm
  integer::code,stg,stp
  logical::hit
  dt=.05d0;code=current%scenario;p=current%params;ann=current%ann_t;lid=current%lid_t
  ann=current%ann_t
  if(phase==current%nhold+1)target=ann
  cfg=[.55d0,.25d0,.06d0,.08d0]
  do z=1,2
   plant(7+z)=r2_pid_step(cfg,pid(:,z),target,measured(z),dt,.0045d0)
  end do
  if(code==2)plant(8)=plant(8)*p(1)
  plant(10)=clamp(.2d0*(lid-plant(5)),0d0,1d0);plant(11)=clamp(.3d0+(plant(6)-35)/20,.3d0,1d0)
  if(code==13.and.current%elapsed>=p(1))plant(13)=1
  if(code==13.and.current%elapsed>=p(1)+p(2))then
   call log_event(ev_interlock,13,0)
   call finish_run(7);return
  end if
  if(code==14.and.current%elapsed>=p(1))then
   call log_event(ev_interlock,14,0)
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
   call log_telemetry(primary,secondary,out(3))
   if(mod(nowsec,snap_every)==0)call log_chemistry()
  end if
  hf_tick=hf_tick+1
  if(mod(hf_tick,2)==0)call log_highrate(primary,secondary)
  if(current%lid_ready==0.and.plant(5)>=lid-2)then
   current%lid_ready=1;call log_event(ev_lid_ready,nint(plant(5)*1000),0)
  end if
  if(out(4)/=0)then
   call log_event(ev_monitor_stop,nint(out(3)*1000),0)
   call finish_run(3);return
  end if
  if(current%elapsed>7900)then
   call log_event(ev_time_limit,0,0)
   call finish_run(4);return
  end if
  if(abs((plant(1)+plant(2))/2-target)<.5d0)then
   if(hold==0)then
    call stage_step(phase,stg,stp);call log_event(ev_target_reached,stg,stp)
   end if
   hold=hold+dt
  else
   hold=0
  end if
  if(phase<current%nhold)then
   if(hold>=current%hold_s(phase+1))then
    call log_event(ev_step_end,phase,0)
    phase=phase+1;hold=0
    if(phase<current%nhold)then
     target=current%hold_t(phase+1)
    else
     target=95
    end if
    call log_event(ev_stage_start,phase,0);call log_event(ev_step_start,phase,0)
   end if
  else if(phase==current%nhold.and.hold>=current%den_s)then
   call log_event(ev_step_end,phase,0)
   phase=phase+1;hold=0;target=ann
   call log_event(ev_step_start,current%nhold,1)
  else if(phase==current%nhold+1.and.hold>=current%ann_hold)then
   call acquire()
  end if
 end subroutine
! @nativefn NR2-M01-F0010
 subroutine apply_record_scenario(r)
  type(run_t),intent(inout)::r
  if(r%scenario==0)return
  call scenario_effects(r%scenario,r%params,r%seed,r%ncy,r%nw,r%ne,r%nr,r%present,r%y,r%y2)
 end subroutine
! @nativefn NR2-M01-F0055
 function live_curves(r) result(c)
  ! Display-only view of a run in progress: the record-level fault effects that finish_run
  ! applies to the whole file are applied to the cycles read so far, so the live curve
  ! already shows the fault instead of following the normal path and jumping at the end.
  ! Unread cycles are padded with the last reading and the run is treated as full length,
  ! so the noise sequence (and therefore every plotted point) matches the filed record.
  type(run_t),intent(in)::r
  type(curve_t)::c
  integer::n,w,k
  c=curves_of(r)
  if(r%scenario==0.or.r%ncy<1.or..not.running)return   ! filed records already carry the effects
  n=r%ncy
  do w=1,c%nw
   do k=n+1,45
    c%y(k,w)=c%y(n,w);c%y2(k,w)=c%y2(n,w)
   end do
  end do
  call scenario_effects(r%scenario,r%params,r%seed,max(n,min(45,r%planned_cycles)),c%nw,c%ne,c%nr,c%present,c%y,c%y2)
  do w=1,c%nw
   do k=n+1,45
    c%y(k,w)=0;c%y2(k,w)=0
   end do
  end do
 end function
! @nativefn NR2-M01-F0056
 subroutine scenario_effects(code,pin,seed,n,nw,ne,nr,present,y,y2)
  integer,intent(in)::code,seed,n,nw,ne,nr,present(:)
  real(c_double),intent(in)::pin(6)
  real(c_double),intent(inout)::y(:,:),y2(:,:)
  real(c_double)::p(6),base,plateau,old(45),old2(45),rr
  integer::w,k,j,src,first,last
  p=pin;rr=real(seed,c_double)+42443+p(6)
  do w=1,nw
   if(.not.affects(code,p,w,ne,nr).or.present(w)==0)cycle
   old=y(:,w);old2=y2(:,w);base=old(1)
   select case(code)
   case(3)
    do k=max(1,nint(p(3))),n
     y(k,w)=max(0d0,real(nint(old(k)*(1-p(2)*(k-p(3)))*(1+.03d0*rng_gauss(rr))),c_double))
    end do
   case(5,17)
    do k=1,n
     y2(k,w)=real(nint(old2(k)+p(1)*max(0d0,old(k)-base)),c_double)
    end do
   case(19)
    first=nint(p(1));last=min(n,first+nint(p(2))-1)
    if(first<=n)then
     y(first:last,w)=-1;y2(first:last,w)=-1
    end if
   case(26)
    if(n<5)cycle
    base=sum(old(1:5))/5;plateau=p(1)*(maxval(old(1:n))-base)
    if(plateau<=0)cycle
    do k=1,n
     y(k,w)=real(max(0,nint(base+plateau*tanh((old(k)-base)/plateau))),c_double)
    end do
   case(36)
    do k=1,n
     y(k,w)=real(nint((1-p(1))*old(k)+p(1)*old2(k)),c_double)
     y2(k,w)=real(nint((1-p(1))*old2(k)+p(1)*old(k)),c_double)
    end do
   case(39)
    do k=max(1,nint(p(2))),n
     y(k,w)=real(max(0,nint(old(k)*(1-p(1)*(k-p(2))))),c_double)
    end do
   case(41)
    do j=1,nint(p(1))
     k=min(n-1,nint(p(3))+3*(j-1));if(k<1)cycle
     y(k,w)=real(max(0,nint(base+(old(k)-base)*(1-p(2))-.25d0*base)),c_double)
     y(k+1,w)=real(nint(old(k+1)*1.18d0),c_double)
    end do
   case(45,46)
    j=nint(p(1));if(code==46)j=nint(p(2))
    do k=1,n
     src=max(1,k-j);y(k,w)=old(src);y2(k,w)=old2(src)
     if(code==46)then
      y(k,w)=y(k,w)+p(1)
      if(k==5)y(k,w)=y(k,w)+p(3)
     end if
    end do
   case(47)
    do k=max(1,nint(p(2))),n
     y(k,w)=old(k)+p(1)*(k-p(2))
    end do
   end select
  end do
  where(y>=0)y=min(65535d0,anint(y))
  where(y2>=0)y2=min(65535d0,anint(y2))
 end subroutine
! @nativefn NR2-M01-F0013
 subroutine tick_model(wall_ms)
  real(c_double),intent(in)::wall_ms
  integer::i,steps
  if(preview_mode.and.pv_bg.and.warm_hold)then
   call cancel_preview();return
  end if
  if(preview_mode)then
   ! The normal reference is computed quickly; the fault run is paced like a real run
   ! (about 30 s at the default acceleration) so its curves grow in front of the reference.
   if(.not.running)then
    if(pv_bg)then
     call bg_begin()
    else
     call preview_begin()
    end if
   end if
   if(pv_state==1.or.pv_bg)then
    steps=pv_steps;if(pv_bg)steps=1500
   else
    wall_run=wall_run+min(1000d0,max(0d0,wall_ms))/1000
    pv_budget=pv_budget+min(1000d0,max(0d0,wall_ms))*p_speed/1000
    steps=min(2000,int(pv_budget/.05d0));pv_budget=pv_budget-steps*.05d0
   end if
   do i=1,steps
    if(.not.running)exit
    call advance_one()
   end do
   return
  end if
  if(.not.running.and.remaining<=0.and..not.queue_active)then
   call warm_tick()
   if(preview_mode)return
  end if
  if(.not.running)then
   if(remaining<=0.and.queue_active)then
    if(qpos<qn)then
     qpos=qpos+1;p_scenario=qcode(qpos);remaining=qreps(qpos)
    else
     queue_active=.false.;qn=0;p_scenario=q_saved
     message='Selected cases completed and filed. Open MISSY Results.'
    end if
   end if
   if(remaining>0)call begin_run()
   return
  end if
  wall_run=wall_run+min(1000d0,max(0d0,wall_ms))/1000
  budget=budget+min(1000d0,max(0d0,wall_ms))*p_speed/1000
  steps=min(2000,int(budget/.05d0));budget=budget-steps*.05d0
  do i=1,steps
   if(.not.running)exit
   call advance_one()
  end do
 end subroutine
! @nativefn NR2-M01-F0048
 function curves_of(r) result(c)
  type(run_t),intent(in)::r
  type(curve_t)::c
  c%nw=r%nw;c%ncy=r%ncy;c%ne=r%ne;c%nr=r%nr;c%status=r%status;c%present=r%present
  c%y=r%y;c%y2=r%y2;c%cq=r%cq;c%cq2=r%cq2;c%params=r%params
 end function
! @nativefn NR2-M01-F0041
 subroutine start_preview(code)
  ! Simulates the normal reference (cached per profile) and then the selected fault case.
  integer,intent(in)::code
  integer::key(8)
  if(running.and..not.preview_mode)return
  if(preview_mode)call cancel_preview()
  if(.not.scenario_available(code))then
   pv_state=0;pv_code=code;return
  end if
  key=[p_ext,p_rep,p_copies,p_threshold,p_seed,p_prog,p_fault,1]
  call cache_check(key)
  if(.not.pv_force.and..not.pv_bg.and.code>0)then
   if(pv_cached(0).and.pv_cached(code))then
    if(all(abs(pv_cache(code)%params-scenario_bank(:,code))<1d-12))then
     ! precomputed at start-up / in the background: replay at the pace of a real run
     pv_norm=pv_cache(0);pv_fault=pv_cache(code);pv_code=code;pv_state=4;replay_t=0;return
    end if
   end if
  end if
  pv_force=.false.
  pv_saved_scenario=p_scenario;pv_code=code;preview_mode=.true.
  if(all(key==pv_key))then
   pv_state=2
  else if(pv_cached(0))then
   pv_norm=pv_cache(0);pv_key=key;pv_state=2     ! normal reference from the cache
  else
   pv_key=key;pv_state=1
  end if
  call preview_begin()
 end subroutine
! @nativefn NR2-M01-F0042
 subroutine preview_begin()
  ! begin_run reads the case from p_scenario; the selection shown in the interface is kept.
  character(len=160)::keep
  integer::shown
  keep=message;shown=p_scenario;pv_budget=0;wall_run=0
  p_scenario=pv_code;if(pv_state==1)p_scenario=0
  call begin_run();message=keep;p_scenario=shown
 end subroutine
! @nativefn NR2-M01-F0043
 subroutine preview_filed()
  ! Called by finish_run while previewing: keeps the record out of the history.
  if(pv_bg)then
   if(bg_phase==1)then
    call cache_store(0,curves_of(current))
   else
    call cache_store(bg_code,curves_of(current))
   end if
   call bg_end();return
  end if
  if(pv_state==1)then
   pv_norm=curves_of(current);pv_state=2   ! the fault run starts on the next tick
   call cache_store(0,pv_norm)
   if(pv_code==0)then
    pv_fault=pv_norm;call preview_end()
   end if
  else
   pv_fault=curves_of(current);call cache_store(pv_code,pv_fault);call preview_end()
  end if
 end subroutine
! @nativefn NR2-M01-F0044
 subroutine preview_end()
  ! A background cache run never owns the selected scenario, so it is not restored from it.
  pv_state=3;preview_mode=.false.;running=.false.;current=run_t()
  if(.not.pv_bg)p_scenario=pv_saved_scenario
  if(pv_bg)then
   pv_bg=.false.;pv_state=0;pv_code=-1
  end if
  if(allocated(hfr(0)%v))deallocate(hfr(0)%v)
  if(allocated(hfr(0)%g))deallocate(hfr(0)%g)
  if(allocated(hfr(0)%c))deallocate(hfr(0)%c)
  hfr(0)%n=0;hfr(0)%ns=0
 end subroutine
! @nativefn NR2-M01-F0045
 subroutine cancel_preview()
  if(.not.preview_mode)return
  if(pv_bg)then
   call bg_end();return
  end if
  if(pv_state==1)pv_key=-1
  call preview_end();pv_state=0
 end subroutine
! @nativefn NR2-M01-F0049
 subroutine cache_check(key)
  integer,intent(in)::key(8)
  if(.not.allocated(pv_cache))allocate(pv_cache(0:51))
  if(any(key/=cache_key))then
   cache_key=key;pv_cached=.false.;warm_next=1
  end if
 end subroutine
! @nativefn NR2-M01-F0050
 subroutine cache_store(code,c)
  integer,intent(in)::code
  type(curve_t),intent(in)::c
  integer::key(8)
  key=[p_ext,p_rep,p_copies,p_threshold,p_seed,p_prog,p_fault,1]
  call cache_check(key)
  if(code>=0.and.code<=51)then
   pv_cache(code)=c;pv_cached(code)=.true.
  end if
 end subroutine
! @nativefn NR2-M01-F0051
 subroutine start_background(code)
  ! Full-speed preview of one case for the cache (no pacing, interface not marked busy).
  integer,intent(in)::code
  integer::key(8)
  if(running.or.preview_mode)return
  key=[p_ext,p_rep,p_copies,p_threshold,p_seed,p_prog,p_fault,1]
  call cache_check(key)
  ! The background run has its own code and phase and never touches the case shown on the
  ! case page (pv_state, pv_code, pv_norm, pv_fault), so it can run on every page.
  pv_bg=.true.;bg_code=code;preview_mode=.true.
  if(code==0.or..not.pv_cached(0))then
   bg_phase=1;bg_code=0
  else
   bg_phase=2
  end if
  call bg_begin()
 end subroutine
! @nativefn NR2-M01-F0054
 subroutine bg_begin()
  character(len=160)::keep
  integer::shown
  keep=message;shown=p_scenario;pv_budget=0;wall_run=0
  p_scenario=bg_code;if(bg_phase==1)p_scenario=0
  call begin_run();message=keep;p_scenario=shown
 end subroutine
! @nativefn NR2-M01-F0055
 subroutine bg_end()
  preview_mode=.false.;running=.false.;current=run_t();pv_bg=.false.;bg_phase=0;bg_code=-1
  if(allocated(hfr(0)%v))deallocate(hfr(0)%v)
  if(allocated(hfr(0)%g))deallocate(hfr(0)%g)
  if(allocated(hfr(0)%c))deallocate(hfr(0)%c)
  hfr(0)%n=0;hfr(0)%ns=0
 end subroutine
! @nativefn NR2-M01-F0052
 subroutine warm_tick()
  ! Background warm-up: compute the next uncached case while the interface is idle.
  ! Runs on every page; a real run pauses it. When the run profile changes, the cache is
  ! invalidated and filled again in the background.
  integer::c,key(8)
  if(warm_hold.and.pv_bg.and.preview_mode)then
   call cancel_preview();return
  end if
  if(warm_hold.or.preview_mode.or.running)return
  key=[p_ext,p_rep,p_copies,p_threshold,p_seed,p_prog,p_fault,1]
  call cache_check(key)
  if(.not.pv_cached(0))then
   warm_on=.true.;call start_background(0);return
  end if
  do c=warm_next,51
   if(.not.scenario_available(c))cycle
   if(pv_cached(c))cycle
   warm_on=.true.;warm_next=c;call start_background(c);return
  end do
  warm_on=.false.
 end subroutine
! @nativefn NR2-M01-F0053
 integer function cached_cases() result(n)
  integer::c
  n=0
  do c=1,51
   if(scenario_available(c).and.pv_cached(c))n=n+1
  end do
 end function
! @nativefn NR2-M01-F0046
 integer function selection_runs() result(n)
  integer::c
  n=0
  do c=0,51
   if(sel_n(c)>0.and.scenario_available(c))n=n+sel_n(c)
  end do
 end function
! @nativefn NR2-M01-F0047
 subroutine start_selection()
  ! Runs every selected case the requested number of times, one after another.
  integer::c,n
  if(preview_mode)call cancel_preview()
  if(running)then
   message='A batch is already running.';return
  end if
  n=selection_runs()
  if(n==0)then
   message='Select at least one case (or the normal run) first.';return
  end if
  if(nh+n>maxruns)then
   message='History capacity is 32 runs: reduce the selection or export and restart.';return
  end if
  qn=0
  do c=0,51
   if(sel_n(c)>0.and.scenario_available(c))then
    qn=qn+1;qcode(qn)=c;qreps(qn)=sel_n(c)
   end if
  end do
  q_saved=p_scenario;queue_active=.true.;qpos=1;p_scenario=qcode(1);remaining=qreps(1)
  call begin_run()
 end subroutine
end module
