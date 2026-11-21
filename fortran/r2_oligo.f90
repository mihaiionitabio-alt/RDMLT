! SPDX-License-Identifier: GPL-3.0-only
! DNA uses ASCII bytes; caller applies the same A/C/G/T cleaning as js/core/oligo.js.
module r2_oligo
  use iso_c_binding, only: c_double,c_int,c_int8_t
  use, intrinsic :: ieee_arithmetic, only: ieee_value,ieee_quiet_nan
  implicit none
  private
  public :: base_index,complement,r2_gc,r2_revcomp,r2_tm_nn,r2_complexity,r2_dimer3,r2_sites
contains
! @r2fn R2-C0119-F00001
  pure integer function base_index(b) result(i)
    integer(c_int8_t), intent(in) :: b
    i=index('ACGT',achar(int(b)))
  end function
! @r2fn R2-C0119-F00002
  pure integer(c_int8_t) function complement(b) result(c)
    integer(c_int8_t), intent(in) :: b
    integer :: i
    i=base_index(b); c=int(iachar('N'),c_int8_t)
    if(i>0) c=int(iachar('TGCA'(i:i)),c_int8_t)
  end function
! @r2fn R2-C0119-F00003
  function r2_gc(s,n) result(v) bind(c)
    integer(c_int), value :: n
    integer(c_int8_t), intent(in) :: s(n)
    real(c_double) :: v
    integer :: i,k
    k=0
    do i=1,n
      if(s(i)==71 .or. s(i)==67) k=k+1
    end do
    v=0; if(n>0) v=100.0_c_double*k/n
  end function
! @r2fn R2-C0119-F00004
  subroutine r2_revcomp(s,n,out) bind(c)
    integer(c_int), value :: n
    integer(c_int8_t), intent(in) :: s(n)
    integer(c_int8_t), intent(out) :: out(n)
    integer :: i
    do i=1,n
      out(i)=complement(s(n+1-i))
    end do
  end subroutine
! @r2fn R2-C0119-F00005
  function r2_tm_nn(s,n,mono,mg,oligo) result(v) bind(c)
    integer(c_int), value :: n
    integer(c_int8_t), intent(in) :: s(n)
    real(c_double), value :: mono,mg,oligo
    real(c_double) :: v,dh,ds,inv,f,mon,lm,ratio,lg,a,d,g,h(4,4),entropy(4,4)
    integer :: i,j,k
    logical :: symmetric
    ! Columns are second base, rows first base, in A/C/G/T order.
    h=reshape([-7.9_c_double,-8.5_c_double,-8.2_c_double,-7.2_c_double, &
      -8.4_c_double,-8.0_c_double,-9.8_c_double,-8.2_c_double, &
      -7.8_c_double,-10.6_c_double,-8.0_c_double,-8.5_c_double, &
      -7.2_c_double,-7.8_c_double,-8.4_c_double,-7.9_c_double],[4,4])
    entropy=reshape([-22.2_c_double,-22.7_c_double,-22.2_c_double,-21.3_c_double, &
      -22.4_c_double,-19.9_c_double,-24.4_c_double,-22.2_c_double, &
      -21.0_c_double,-27.2_c_double,-19.9_c_double,-22.7_c_double, &
      -20.4_c_double,-21.0_c_double,-22.4_c_double,-22.2_c_double],[4,4])
    v=ieee_value(0.0_c_double,ieee_quiet_nan); if(n<8) return
    dh=0; ds=0
    do i=1,n-1
      j=base_index(s(i)); k=base_index(s(i+1)); if(j==0 .or. k==0) return
      dh=dh+h(j,k); ds=ds+entropy(j,k)
    end do
    do i=1,n,n-1
      if(s(i)==71 .or. s(i)==67) then
        dh=dh+0.1_c_double; ds=ds-2.8_c_double
      else
        dh=dh+2.3_c_double; ds=ds+4.1_c_double
      end if
    end do
    symmetric=.true.
    do i=1,n
      if(s(i)/=complement(s(n+1-i))) symmetric=.false.
    end do
    if(symmetric) ds=ds-1.4_c_double
    inv=(ds+1.9872_c_double*log(oligo))/(dh*1000)
    f=r2_gc(s,n)/100; mon=max(mono,1e-6_c_double); lm=log(mon); ratio=0
    if(mg>0) ratio=sqrt(mg)/mon
    if(ratio<0.22_c_double) then
      inv=inv+(4.29_c_double*f-3.95_c_double)*1e-5_c_double*lm+9.40e-6_c_double*lm*lm
    else
      lg=log(mg); a=3.92e-5_c_double; d=1.42e-5_c_double; g=8.31e-5_c_double
      if(ratio<6) then
        a=a*(0.843_c_double-0.352_c_double*sqrt(mon)*lm)
        d=d*(1.279_c_double-4.03e-3_c_double*lm-8.03e-3_c_double*lm*lm)
        g=g*(0.486_c_double-0.258_c_double*lm+5.25e-3_c_double*lm*lm*lm)
      end if
      inv=inv+a-9.11e-6_c_double*lg+f*(6.26e-5_c_double+d*lg) &
        +1.0_c_double/(2*(n-1))*(-4.82e-4_c_double+5.25e-4_c_double*lg+g*lg*lg)
    end if
    v=1/inv-273.15_c_double
  end function
! @r2fn R2-C0119-F00006
  function r2_complexity(s,n) result(v) bind(c)
    integer(c_int), value :: n
    integer(c_int8_t), intent(in) :: s(n)
    integer(c_int) :: v
    integer :: seen(256),i,k,j,key,obs
    v=100; if(n<6) return
    obs=0
    do k=1,4
      seen=0
      do i=1,n-k+1
        key=0
        do j=0,k-1
          key=key*4+base_index(s(i+j))-1
        end do
        if(key>=0 .and. key<256) seen(key+1)=1
      end do
      obs=obs+sum(seen)
    end do
    v=100*obs/(4+min(n-1,16)+min(n-2,64)+min(n-3,256))
  end function
! @r2fn R2-C0119-F00007
  function r2_dimer3(a,na,b,nb) result(best) bind(c)
    integer(c_int), value :: na,nb
    integer(c_int8_t), intent(in) :: a(na),b(nb)
    integer(c_int) :: best
    integer :: off,n
    best=0
    do off=1,nb
      n=0
      do while(n<na .and. off-n>=1)
        if(a(na-n)/=complement(b(nb+1-off+n))) exit
        n=n+1
      end do
      best=max(best,n)
    end do
  end function
  ! Output zero-based positions; capacity is explicit, never write beyond caller memory.
! @r2fn R2-C0119-F00008
  subroutine r2_sites(t,nt,p,np,maxmis,positions,capacity,count) bind(c)
    integer(c_int), value :: nt,np,maxmis,capacity
    integer(c_int8_t), intent(in) :: t(nt),p(np)
    integer(c_int), intent(out) :: positions(capacity),count
    integer :: i,k,mis
    count=0; if(np<8) return
    do i=1,nt-np+1
      mis=0
      do k=1,np
        if(t(i+k-1)/=p(k)) then
          mis=mis+1; if(k>=np-2) mis=99
        end if
        if(mis>maxmis) exit
      end do
      if(mis<=maxmis) then
        count=count+1; if(count<=capacity) positions(count)=i-1
      end if
    end do
  end subroutine
end module
