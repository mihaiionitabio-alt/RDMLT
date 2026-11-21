! SPDX-License-Identifier: GPL-3.0-only
! Independent monitor: it has no access to controller PID/configuration/state.
! Two disagreeing sensors cannot establish which one is correct. Stop, never vote.
module r2_monitor
  use iso_c_binding, only: c_double,c_int
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: r2_monitor_step,r2_sensor_value,r2_sensor_valid,r2_sensor_gap
contains
! @r2fn R2-C0118-F00001
  function r2_sensor_valid(v,lo,hi) result(ok) bind(c)
    real(c_double), value :: v,lo,hi
    integer(c_int) :: ok
    ok=0
    if(ieee_is_finite(v)) then
      if(v>=lo .and. v<=hi) ok=1
    end if
  end function
! @r2fn R2-C0118-F00002
  function r2_sensor_gap(a,b) result(gap) bind(c)
    real(c_double), value :: a,b
    real(c_double) :: gap
    gap=abs(a-b)
  end function
! @r2fn R2-C0118-F00003
  function r2_sensor_value(truth,noise,bias,drift,elapsed) result(v) bind(c)
    real(c_double), value :: truth,noise,bias,drift,elapsed
    real(c_double) :: v
    v=truth+noise+bias+drift*elapsed/1000.0_c_double
  end function
  ! state: state code (0 normal/1 suspect/2 stop latched), suspect start,
  ! last sample time, maximum gap, observed samples, current reason.
  ! cfg: gap limit C, persistence ms, freshness ms, min C, max C.
  ! obs: primary[2], independent[2], primary timestamp, independent timestamp.
  ! out: state, reason (1 disagreement/2 invalid primary/3 invalid secondary/
  ! 4 invalid both/5 stale), maximum finite gap, stop requested.
! @r2fn R2-C0118-F00004
  subroutine r2_monitor_step(s,obs,now,cfg,out) bind(c)
    real(c_double), intent(inout) :: s(6)
    real(c_double), intent(in) :: obs(6),cfg(5)
    real(c_double), value :: now
    real(c_double), intent(out) :: out(4)
    integer :: z,reason,badp,bads
    real(c_double) :: gap
    gap=0; badp=0; bads=0; reason=0
    do z=1,2
      if(r2_sensor_valid(obs(z),cfg(4),cfg(5))==0) badp=badp+1
      if(r2_sensor_valid(obs(z+2),cfg(4),cfg(5))==0) bads=bads+1
      if(ieee_is_finite(obs(z)) .and. ieee_is_finite(obs(z+2))) gap=max(gap,abs(obs(z)-obs(z+2)))
    end do
    if(gap>cfg(1)) reason=1
    if(badp>0) reason=2
    if(bads>0) reason=3
    if(badp>0 .and. bads>0) reason=4
    if(now-obs(5)>cfg(3) .or. now-obs(6)>cfg(3) .or. obs(5)>now .or. obs(6)>now) reason=5
    s(4)=max(s(4),gap); s(5)=s(5)+1; s(3)=now
    if(s(1)/=2) then
      if(reason==0) then
        s(1)=0; s(2)=-1; s(6)=0
      else
        if(s(1)==0 .or. s(6)/=reason) s(2)=now
        s(1)=1; s(6)=reason
        if(now-s(2)>=cfg(2)) s(1)=2
      end if
    end if
    out=[s(1),s(6),gap,0.0_c_double]
    if(s(1)==2) out(4)=1
  end subroutine
end module
