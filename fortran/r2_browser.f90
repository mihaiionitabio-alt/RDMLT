! SPDX-License-Identifier: GPL-3.0-only
! Browser boundaries accept numeric command IDs and data buffers, never microphone
! recordings, personal text, DOM references or long-running animation loops.
module r2_browser
  use iso_c_binding, only: c_double,c_int
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  public :: r2_voice_transition,r2_plot_xy,r2_axis_ticks,r2_plot_cursor
contains
  ! id: 1 wake, 2 card, 3 back, 4 undo, 5 repeat, 6 review, 7 confirm,
  ! 8 launch, 9 stop, 10 navigation, 11 structured answer.
  ! phase: 0 interview, 1 review, 2 armed, 3 generating, 4 complete.
  ! s: accepted count, wake count, card count, last ID, last argument,
  !    armed, stop requested, last phase. out: status, action, argument, armed.
  ! Actions 1=acknowledge, 2=apply, 3=launch, 4=stop/file current.
! @r2fn R2-C0135-F00001
  subroutine r2_voice_transition(s,id,arg,phase,permitted,out) bind(c)
    real(c_double), intent(inout) :: s(8)
    integer(c_int), value :: id,arg,phase,permitted
    real(c_double), intent(out) :: out(4)
    out=[0.0_c_double,2.0_c_double,real(arg,c_double),s(6)]
    if(permitted==0 .or. id<1 .or. id>11) then
      out(1)=1; out(2)=0; return
    end if
    if(id==2 .and. (arg<1 .or. arg>15)) then
      out(1)=2; out(2)=0; return
    end if
    if((id==8 .and. phase/=2) .or. (id==7 .and. phase/=1)) then
      out(1)=3; out(2)=0; return
    end if
    s(1)=s(1)+1; s(4)=id; s(5)=arg; s(8)=phase
    if(id==1) then
      s(2)=s(2)+1; out(2)=1
    end if
    if(id==2) s(3)=s(3)+1
    if(id==7) s(6)=1
    if(id==8) then
      out(2)=3; s(6)=0
    end if
    if(id==9) then
      s(6)=0; s(7)=1; out(2)=4
    else
      s(7)=0
    end if
    out(4)=s(6)
  end subroutine
  ! Produce interleaved pixel coordinates and validity flags in shared WASM memory.
  ! JS paints these vertices. Numerical axes/data-to-pixel transforms live here.
! @r2fn R2-C0135-F00002
  subroutine r2_plot_xy(x,y,n,limits,rect,points,valid) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n),y(n),limits(4),rect(4)
    real(c_double), intent(out) :: points(2*n)
    integer(c_int), intent(out) :: valid(n)
    real(c_double) :: dx,dy
    integer :: i
    dx=limits(2)-limits(1); dy=limits(4)-limits(3)
    points=0; valid=0; if(dx<=0 .or. dy<=0) return
    do i=1,n
      if(.not.ieee_is_finite(x(i)) .or. .not.ieee_is_finite(y(i))) cycle
      points(2*i-1)=rect(1)+(x(i)-limits(1))/dx*rect(3)
      points(2*i)=rect(2)+rect(4)-(y(i)-limits(3))/dy*rect(4)
      if(ieee_is_finite(points(2*i-1)) .and. ieee_is_finite(points(2*i))) valid(i)=1
    end do
  end subroutine
! @r2fn R2-C0135-F00003
  subroutine r2_axis_ticks(lo,hi,target,ticks,capacity,count) bind(c)
    real(c_double), value :: lo,hi
    integer(c_int), value :: target,capacity
    real(c_double), intent(out) :: ticks(capacity)
    integer(c_int), intent(out) :: count
    real(c_double) :: span,step0,err,step,v
    integer :: i,start
    count=0; if(hi<lo .or. target<=0 .or. capacity<=0) return
    span=hi-lo; if(span==0) span=1
    step0=10.0_c_double**floor(log10(span/target)); err=span/target/step0
    step=step0
    if(err>=1.5_c_double) step=2*step0
    if(err>=3.5_c_double) step=5*step0
    if(err>=7.5_c_double) step=10*step0
    start=ceiling(lo/step)
    do i=0,capacity-1
      v=(start+i)*step; if(v>hi+step*1e-9_c_double) exit
      count=count+1; ticks(count)=v
    end do
  end subroutine
! @r2fn R2-C0135-F00004
  function r2_plot_cursor(x,lo,hi,left,width) result(pixel) bind(c)
    real(c_double), value :: x,lo,hi,left,width
    real(c_double) :: pixel
    pixel=left
    if(hi>lo) pixel=left+(x-lo)/(hi-lo)*width
  end function
end module
