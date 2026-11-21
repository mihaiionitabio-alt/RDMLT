! SPDX-License-Identifier: GPL-3.0-only
module r2_thermal
  use iso_c_binding, only: c_double,c_int
  use r2_math
  use, intrinsic :: ieee_arithmetic, only: ieee_value,ieee_quiet_nan,ieee_negative_inf
  implicit none
  private
  public :: r2_thermal_step,r2_pid_step,r2_ohm_to_c,r2_c_to_ohm,r2_e96,r2_trip_divider
  public :: r2_step_temp,r2_step_hold,r2_well_temp
  public :: r2_plant_span
  public :: r2_sensor_bundle
contains
! @r2fn R2-C0121-F00011
  subroutine r2_sensor_bundle(v,rng,mode,out) bind(c)
    real(c_double), intent(in) :: v(7)
    real(c_double), intent(inout) :: rng
    integer(c_int), value :: mode
    real(c_double), intent(out) :: out(7)
    real(c_double) :: noise(5)
    integer :: i
    do i=1,5
      noise(i)=0.01_c_double*rng_gauss(rng)
    end do
    out(1:5)=[v(1)+noise(1),v(2)+noise(2),v(3)+noise(3),v(4)+noise(4),(v(1)+v(2))/2+noise(5)]
    if(mode==1) out(1)=ieee_value(0.0_c_double,ieee_quiet_nan)
    if(mode==2) out(1)=ieee_value(0.0_c_double,ieee_negative_inf)
    if(mode==3) out(1)=v(1)-min(12.0_c_double,v(7)*0.001_c_double*0.5_c_double)+0.01_c_double*rng_gauss(rng)
    out(6)=v(5)*(1+0.01_c_double*rng_gauss(rng))
    out(7)=v(6)*(1+0.01_c_double*rng_gauss(rng))
  end subroutine
  ! The 10 ms host boundary contains 1 ms physical/interlock updates in Fortran.
  ! x extends the thermal ABI with clock, firmware enable, watchdog kick, trip,
  ! watchdog status, lid, rails, proof test and trip limits. Chemistry runs at
  ! the same 50 ms boundaries in the host, preserving every random draw.
! @r2fn R2-C0121-F00010
  subroutine r2_plant_span(x,n,fault,at_ms,end_ms) bind(c)
    real(c_double), intent(inout) :: x(29)
    integer(c_int), value :: n,fault
    real(c_double), value :: at_ms,end_ms
    integer :: i
    logical :: active,en
    do i=1,n
      x(20)=x(20)+1
      active=x(20)>=at_ms .and. (end_ms==0 .or. x(20)<end_ms)
      if(fault==1) then
        x(25)=1; if(active) x(25)=0
      end if
      if(fault==2 .and. active) x(26)=0
      x(24)=0; if(x(20)-x(22)<=200) x(24)=1
      if(max(x(1),x(2))>x(28) .or. x(5)>x(29) .or. x(27)/=0) x(23)=1
      if(fault==3 .and. active) x(23)=0
      en=x(21)/=0 .and. x(23)==0 .and. x(24)/=0 .and. x(25)/=0 .and. x(26)/=0
      x(12)=0; if(en) x(12)=1
      x(13:15)=0
      if(active) then
        if(fault==4) x(14)=1
        if(fault==5) x(15)=1
        if(fault==6) x(13)=1
      end if
      call r2_thermal_step(x)
    end do
  end subroutine
  ! x: block[2], liquid[2], lid, sink, ambient, duty[2], lid duty,
  ! fan duty, enabled?, fan stalled?, bridge stuck?, TEC open?, currents[2], lid current, fan RPM
! @r2fn R2-C0121-F00001
  subroutine r2_thermal_step(x) bind(c)
    real(c_double), intent(inout) :: x(19)
    real(c_double) :: d,q,cond,db,ds,qhs,fan,lid
    integer :: z,other
    qhs=0
    do z=1,2
      other=3-z; d=x(7+z)
      if(z==1 .and. x(14)/=0) d=1
      if(z==1 .and. x(15)/=0) d=0
      if(x(12)==0) d=0
      x(15+z)=abs(d)*6000
      if(d>0) then
        q=d*90
      else
        q=d*60
      end if
      cond=0.35_c_double*(x(z)-x(6))
      db=(q-cond-0.27_c_double*(x(z)-x(z+2))-0.5_c_double*(x(z)-x(other)) &
        -0.05_c_double*(x(z)-x(7)))/21
      ds=(0.27_c_double*(x(z)-x(z+2))+0.006_c_double*(x(5)-x(z+2)))/0.67_c_double
      ! Preserve the original sequential update of zones (not a simultaneous Euler step).
      x(z)=x(z)+db*0.001_c_double; x(z+2)=x(z+2)+ds*0.001_c_double
      qhs=qhs-q+abs(d)*40+cond
    end do
    fan=x(11); if(x(13)/=0) fan=0
    x(19)=floor(4200*fan)*1.0_c_double
    x(6)=x(6)+(qhs-(1.5_c_double+8*fan)*(x(6)-x(7)))/200*0.001_c_double
    lid=0; if(x(12)/=0) lid=x(10)
    x(18)=lid*2000
    x(5)=x(5)+(40*lid-0.12_c_double*(x(5)-x(7)))/25*0.001_c_double
  end subroutine
! @r2fn R2-C0121-F00002
  function r2_pid_step(cfg,s,sp,meas,dt,ff) result(u) bind(c)
    real(c_double), intent(in) :: cfg(4)
    real(c_double), intent(inout) :: s(4)
    real(c_double), value :: sp,meas,dt,ff
    real(c_double) :: e,dr,un,u
    if(s(4)==0) then
      s(3)=meas; s(4)=1
    end if
    e=sp-meas; dr=(meas-s(3))/dt; s(3)=meas
    s(2)=s(2)+(dr-s(2))*dt/(cfg(4)+dt)
    un=ff+cfg(1)*e+s(1)-cfg(3)*s(2); u=clamp(un,-1.0_c_double,1.0_c_double)
    if(u==un .or. (un>1 .and. e<0) .or. (un< -1 .and. e>0)) s(1)=s(1)+cfg(2)*e*dt
  end function
! @r2fn R2-C0121-F00003
  function r2_step_temp(temp,delta,start,cycle) result(v) bind(c)
    real(c_double), value :: temp,delta,start,cycle
    real(c_double) :: v
    v=clamp(temp+delta*max(0.0_c_double,cycle-start),4.0_c_double,100.0_c_double)
  end function
! @r2fn R2-C0121-F00004
  function r2_step_hold(hold,delta,start,cycle) result(v) bind(c)
    real(c_double), value :: hold,delta,start,cycle
    real(c_double) :: v
    v=max(0.0_c_double,hold+delta*max(0.0_c_double,cycle-start))
  end function
! @r2fn R2-C0121-F00005
  function r2_well_temp(liquid,ambient,edge,camera) result(v) bind(c)
    real(c_double), value :: liquid,ambient
    integer(c_int), value :: edge,camera
    real(c_double) :: v,loss
    v=liquid; if(edge==0) return
    loss=0.008_c_double; if(camera/=0) loss=0.0015_c_double
    v=liquid-loss*(liquid-ambient)
  end function
! @r2fn R2-C0121-F00006
  function r2_ohm_to_c(cv,r) result(v) bind(c)
    real(c_double), intent(in) :: cv(3)
    real(c_double), value :: r
    real(c_double) :: v,l
    l=log(r); v=1/(cv(1)+cv(2)*l+cv(3)*l*l*l)-273.15_c_double
  end function
! @r2fn R2-C0121-F00007
  function r2_c_to_ohm(cv,t) result(v) bind(c)
    real(c_double), intent(in) :: cv(3)
    real(c_double), value :: t
    real(c_double) :: v,lo,hi,mid
    integer :: i
    lo=1; hi=5e6_c_double
    do i=1,80
      mid=sqrt(lo*hi)
      if(r2_ohm_to_c(cv,mid)>t) then
        lo=mid
      else
        hi=mid
      end if
    end do
    v=sqrt(lo*hi)
  end function
! @r2fn R2-C0121-F00008
  function r2_e96(r) result(v) bind(c)
    real(c_double), value :: r
    real(c_double) :: v,d,best,err,x
    integer :: i
    d=10.0_c_double**floor(log10(r)); best=1; err=1e9_c_double
    do i=0,95
      x=floor(10.0_c_double**(real(i,c_double)/96)*100+0.5_c_double)/100.0_c_double
      if(abs(x*d-r)<err) then
        err=abs(x*d-r); best=x
      end if
    end do
    v=best*d
  end function
! @r2fn R2-C0121-F00009
  subroutine r2_trip_divider(cv,t,rpu,bottom,out) bind(c)
    real(c_double), intent(in) :: cv(3)
    real(c_double), value :: t,rpu,bottom
    real(c_double), intent(out) :: out(6)
    real(c_double) :: rn,ratio,top,got
    rn=r2_c_to_ohm(cv,t); ratio=rn/(rn+rpu); top=r2_e96(bottom*(1-ratio)/ratio)
    got=bottom/(bottom+top)
    out=[rn,ratio,top,bottom,got,r2_ohm_to_c(cv,got*rpu/(1-got))]
  end subroutine
end module
