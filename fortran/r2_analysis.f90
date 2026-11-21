! SPDX-License-Identifier: GPL-3.0-only
! Numerical primitives only; SOP interpretation and UI remain in the host.
module r2_analysis
  use iso_c_binding, only: c_double,c_int
  use, intrinsic :: ieee_arithmetic, only: ieee_value,ieee_quiet_nan,ieee_is_finite
  use r2_math, only: clamp
  implicit none
  private
  public :: r2_mean,r2_sd,r2_median,r2_line,r2_cq_threshold,r2_sigmoid_value,r2_solve4
  public :: r2_net_signal,r2_round_up,r2_salt_eq,r2_tm_amplicon,r2_efficiency
  public :: r2_baseline,r2_standard_curve,r2_rms,r2_peak_rate
contains
! @r2fn R2-C0115-F00001
  function r2_mean(x,n) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n)
    real(c_double) :: v
    integer :: i
    v=ieee_value(0.0_c_double,ieee_quiet_nan); if(n<=0) return
    v=0
    do i=1,n
      v=v+x(i)
    end do
    v=v/n
  end function
! @r2fn R2-C0115-F00002
  function r2_sd(x,n) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n)
    real(c_double) :: v,m
    integer :: i
    v=ieee_value(0.0_c_double,ieee_quiet_nan); if(n<2) return
    m=r2_mean(x,n); v=0
    do i=1,n
      v=v+(x(i)-m)**2
    end do
    v=sqrt(v/(n-1))
  end function
! @r2fn R2-C0115-F00003
  function r2_median(x,n) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n)
    real(c_double) :: v,b(n),tmp
    integer :: i,j
    v=ieee_value(0.0_c_double,ieee_quiet_nan); if(n<=0) return
    b=x
    do i=2,n
      tmp=b(i); j=i-1
      do while(j>=1)
        if(b(j)<=tmp) exit
        b(j+1)=b(j); j=j-1
      end do
      b(j+1)=tmp
    end do
    if(mod(n,2)==1) then
      v=b((n+1)/2)
    else
      v=(b(n/2)+b(n/2+1))/2
    end if
  end function
! @r2fn R2-C0115-F00004
  subroutine r2_line(x,y,n,out) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n),y(n)
    real(c_double), intent(out) :: out(3)
    real(c_double) :: mx,my,sxx,sxy,b,a,ss
    integer :: i
    out=ieee_value(0.0_c_double,ieee_quiet_nan); if(n<=0) return
    mx=r2_mean(x,n); my=r2_mean(y,n); sxx=0; sxy=0
    do i=1,n
      sxx=sxx+(x(i)-mx)**2; sxy=sxy+(x(i)-mx)*(y(i)-my)
    end do
    b=0; if(sxx/=0) b=sxy/sxx
    a=my-b*mx; ss=0
    do i=1,n
      ss=ss+(y(i)-a-b*x(i))**2
    end do
    out=[a,b,sqrt(ss/max(1,n-2))]
  end subroutine
  ! status: 0 crossing; 1 no amplification; 2 above at start; 3 no crossing; 4 bad input.
! @r2fn R2-C0115-F00005
  subroutine r2_cq_threshold(x,d,n,thr,start,margin,ratio,out) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n),d(n)
    real(c_double), value :: thr,start,margin,ratio
    real(c_double), intent(out) :: out(2)
    integer :: i,j
    logical :: ok
    out=[ieee_value(0.0_c_double,ieee_quiet_nan),4.0_c_double]
    if(n<1 .or. thr<=0) return
    out(2)=1; if(.not.(d(n)>ratio*thr)) return
    do i=2,n
      if(x(i)<=start-margin) cycle
      if(d(i-1)<thr .and. d(i)>=thr) then
        ok=.true.
        do j=i,min(n,i+2)
          if(d(j)<thr) ok=.false.
        end do
        if(.not.ok) cycle
        if(d(i-1)>0) then
          out(1)=x(i-1)+(log(thr)-log(d(i-1)))/(log(d(i))-log(d(i-1)))*(x(i)-x(i-1))
        else
          out(1)=x(i-1)+(thr-d(i-1))/(d(i)-d(i-1))*(x(i)-x(i-1))
        end if
        out(2)=0; return
      end if
    end do
    out(2)=3; if(d(1)>=thr) out(2)=2
  end subroutine
! @r2fn R2-C0115-F00006
  function r2_sigmoid_value(x,theta) result(v) bind(c)
    real(c_double), value :: x
    real(c_double), intent(in) :: theta(4)
    real(c_double) :: v
    v=theta(2)+(theta(3)-theta(2))/(1+exp(clamp(theta(1)*(log(x)-theta(4)),-700.0_c_double,700.0_c_double)))
  end function
  ! Row-major ABI, unlike Fortran's native column-major matrices.
! @r2fn R2-C0115-F00007
  subroutine r2_solve4(a,g,out,ok) bind(c)
    real(c_double), intent(in) :: a(16),g(4)
    real(c_double), intent(out) :: out(4)
    integer(c_int), intent(out) :: ok
    real(c_double) :: m(4,5),tmp(5),f
    integer :: i,j,r,p
    do i=1,4
      do j=1,4
        m(i,j)=a((i-1)*4+j)
      end do
      m(i,5)=g(i)
    end do
    ok=0; out=0
    do i=1,4
      p=i
      do r=i+1,4
        if(abs(m(r,i))>abs(m(p,i))) p=r
      end do
      if(abs(m(p,i))<1e-300_c_double) return
      tmp=m(i,:); m(i,:)=m(p,:); m(p,:)=tmp
      do r=1,4
        if(r==i) cycle
        f=m(r,i)/m(i,i)
        do j=i,5
          m(r,j)=m(r,j)-f*m(i,j)
        end do
      end do
    end do
    do i=1,4
      out(i)=m(i,5)/m(i,i)
    end do
    ok=1
  end subroutine
! @r2fn R2-C0115-F00008
  function r2_net_signal(on,off,periods) result(v) bind(c)
    real(c_double), value :: on,off,periods
    real(c_double) :: v
    v=(on-off)/(8*periods)
  end function
! @r2fn R2-C0115-F00009
  function r2_round_up(v,step) result(out) bind(c)
    real(c_double), value :: v,step
    real(c_double) :: out
    out=ceiling(v/step-1e-9_c_double)*step
  end function
! @r2fn R2-C0115-F00010
  function r2_salt_eq(na,mg,dntp) result(v) bind(c)
    real(c_double), value :: na,mg,dntp
    real(c_double) :: v
    v=(na+120*sqrt(max(0.0_c_double,mg-dntp)))/1000
  end function
! @r2fn R2-C0115-F00011
  function r2_tm_amplicon(n,gc,salt) result(v) bind(c)
    real(c_double), value :: n,gc,salt
    real(c_double) :: v
    v=81.5_c_double+16.6_c_double*log10(salt)+0.41_c_double*gc-675/n
  end function
! @r2fn R2-C0115-F00012
  function r2_efficiency(slope) result(v) bind(c)
    real(c_double), value :: slope
    real(c_double) :: v
    v=10.0_c_double**(-1/slope)-1
  end function
! @r2fn R2-C0115-F00013
  subroutine r2_baseline(x,y,n,first,last,out,corrected) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n),y(n)
    real(c_double), value :: first,last
    real(c_double), intent(out) :: out(3),corrected(n)
    real(c_double) :: bx(n),by(n)
    integer :: i,k
    k=0
    do i=1,n
      if(x(i)>=first .and. x(i)<=last .and. ieee_is_finite(y(i))) then
        k=k+1; bx(k)=x(i); by(k)=y(i)
      end if
    end do
    call r2_line(bx(1:k),by(1:k),k,out)
    do i=1,n
      corrected(i)=y(i)-(out(1)+out(2)*x(i))
    end do
  end subroutine
! @r2fn R2-C0115-F00014
  subroutine r2_standard_curve(log_copies,cq,n,out) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: log_copies(n),cq(n)
    real(c_double), intent(out) :: out(5)
    real(c_double) :: fit(3),sst,sse,m
    integer :: i
    call r2_line(log_copies,cq,n,fit)
    m=r2_mean(cq,n); sst=0; sse=0
    do i=1,n
      sst=sst+(cq(i)-m)**2; sse=sse+(cq(i)-fit(1)-fit(2)*log_copies(i))**2
    end do
    out(1:3)=fit; out(4:5)=ieee_value(0.0_c_double,ieee_quiet_nan)
    if(sst>0) out(4)=1-sse/sst
    if(fit(2)/=0) out(5)=r2_efficiency(fit(2))
  end subroutine
! @r2fn R2-C0115-F00015
  function r2_rms(x,n) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: x(n)
    real(c_double) :: v
    integer :: i
    v=0
    do i=1,n
      v=v+x(i)*x(i)
    end do
    if(n>0) v=sqrt(v/n)
  end function
! @r2fn R2-C0115-F00016
  function r2_peak_rate(t,y,n) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: t(n),y(n)
    real(c_double) :: v
    integer :: i
    v=0
    do i=2,n
      if(t(i)>t(i-1)) v=max(v,abs((y(i)-y(i-1))/(t(i)-t(i-1))))
    end do
  end function
end module
