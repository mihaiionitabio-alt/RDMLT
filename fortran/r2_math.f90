! SPDX-License-Identifier: GPL-3.0-only
! Numerical port of js/core/util.js. No browser or file-system dependencies.
module r2_math
  use iso_c_binding, only: c_double, c_int, c_int64_t
  implicit none
  private
  public :: clamp, sigmoid, fraction, mul32, rng_uniform, rng_gauss, rng_poisson
  public :: r2_clamp, r2_sigmoid, r2_rng_uniform, r2_rng_gauss, r2_rng_poisson
  integer(c_int64_t), parameter :: mask32 = 4294967295_c_int64_t
contains
! @r2fn R2-C0117-F00001
  pure real(c_double) function clamp(x, lo, hi)
    real(c_double), intent(in) :: x, lo, hi
    clamp = min(hi, max(lo, x))
  end function
! @r2fn R2-C0117-F00002
  pure real(c_double) function sigmoid(x)
    real(c_double), intent(in) :: x
    sigmoid = 1.0_c_double / (1.0_c_double + exp(-x))
  end function
! @r2fn R2-C0117-F00003
  pure real(c_double) function fraction(rate, dt)
    real(c_double), intent(in) :: rate, dt
    fraction = 1.0_c_double - exp(-rate * dt)
  end function
  ! Explicit modulo-2**32 arithmetic: never rely on signed integer overflow.
! @r2fn R2-C0117-F00004
  pure integer(c_int64_t) function mul32(a,b) result(v)
    integer(c_int64_t), intent(in) :: a,b
    integer(c_int64_t) :: al,bl,ah,bh
    al=iand(a,65535_c_int64_t); bl=iand(b,65535_c_int64_t)
    ah=shiftr(a,16); bh=shiftr(b,16)
    v=iand(al*bl+shiftl(iand(ah*bl+al*bh,65535_c_int64_t),16),mask32)
  end function
! @r2fn R2-C0117-F00005
  real(c_double) function rng_uniform(state) result(v)
    real(c_double), intent(inout) :: state
    integer(c_int64_t) :: t,s
    s=iand(int(state,c_int64_t)+1831565813_c_int64_t,mask32); state=real(s,c_double)
    t=mul32(ieor(s,shiftr(s,15)),ior(s,1_c_int64_t))
    t=ieor(t,iand(t+mul32(ieor(t,shiftr(t,7)),ior(t,61_c_int64_t)),mask32))
    v=real(ieor(t,shiftr(t,14)),c_double)/4294967296.0_c_double
  end function
! @r2fn R2-C0117-F00006
  real(c_double) function rng_gauss(state) result(v)
    real(c_double), intent(inout) :: state
    real(c_double) :: u,w
    u=rng_uniform(state)+1.0e-12_c_double; w=rng_uniform(state)
    v=sqrt(-2.0_c_double*log(u))*cos(6.283185307179586_c_double*w)
  end function
! @r2fn R2-C0117-F00007
  real(c_double) function rng_poisson(state,mean) result(v)
    real(c_double), intent(inout) :: state
    real(c_double), intent(in) :: mean
    real(c_double) :: p,l
    integer :: k
    v=0; if(mean<=0) return
    if(mean>50) then
      v=max(0.0_c_double,floor(mean+sqrt(mean)*rng_gauss(state)+0.5_c_double,kind=c_int64_t)*1.0_c_double)
      return
    end if
    l=exp(-mean); p=1; k=0
    do
      k=k+1; p=p*rng_uniform(state)
      if(p<=l) exit
    end do
    v=real(k-1,c_double)
  end function
! @r2fn R2-C0117-F00008
  function r2_clamp(x,lo,hi) result(v) bind(c)
    real(c_double), value :: x,lo,hi
    real(c_double) :: v
    v=clamp(x,lo,hi)
  end function
! @r2fn R2-C0117-F00009
  function r2_sigmoid(x) result(v) bind(c)
    real(c_double), value :: x
    real(c_double) :: v
    v=sigmoid(x)
  end function
! @r2fn R2-C0117-F00010
  function r2_rng_uniform(s) result(v) bind(c)
    real(c_double), intent(inout) :: s
    real(c_double) :: v
    v=rng_uniform(s)
  end function
! @r2fn R2-C0117-F00011
  function r2_rng_gauss(s) result(v) bind(c)
    real(c_double), intent(inout) :: s
    real(c_double) :: v
    v=rng_gauss(s)
  end function
! @r2fn R2-C0117-F00012
  function r2_rng_poisson(s,mean) result(v) bind(c)
    real(c_double), intent(inout) :: s
    real(c_double), value :: mean
    real(c_double) :: v
    v=rng_poisson(s,mean)
  end function
end module
