! SPDX-License-Identifier: GPL-3.0-only
module r2_optics
  use iso_c_binding, only: c_double,c_int
  use r2_math
  implicit none
  private
  public :: r2_smoothstep,r2_late_reporter,r2_probe_init,r2_probe_step,r2_probe_counts
  public :: r2_signal_mix,r2_half_life,r2_intact,r2_mgb_bonus,r2_detector_sample
  public :: r2_camera_cleavage
  public :: r2_optics_period,r2_optics_reference
contains
! @r2fn R2-C0120-F00012
  subroutine r2_optics_period(signal,rng,out) bind(c)
    real(c_double), intent(in) :: signal(2)
    real(c_double), intent(inout) :: rng
    real(c_double), intent(out) :: out(6)
    real(c_double) :: v
    integer :: c,k
    out=0
    do c=1,2
      do k=1,8
        v=200+signal(c)+(2+0.15_c_double*sqrt(signal(c)))*rng_gauss(rng)
        if(v>=4095) then
          v=4095; out(c+4)=out(c+4)+1
        end if
        out(c)=out(c)+floor(v+0.5_c_double)
        out(c+2)=out(c+2)+floor(200+2*rng_gauss(rng)+0.5_c_double)
      end do
    end do
  end subroutine
! @r2fn R2-C0120-F00013
  subroutine r2_optics_reference(led,valid,rng,out) bind(c)
    real(c_double), value :: led
    integer(c_int), value :: valid
    real(c_double), intent(inout) :: rng
    real(c_double), intent(out) :: out(2)
    integer :: k
    out=0
    do k=1,8
      out(1)=out(1)+floor(200+3000*led+3*rng_gauss(rng)+0.5_c_double)
      out(2)=out(2)+floor(200+2*rng_gauss(rng)+0.5_c_double)
    end do
    if(valid==0) out(1)=out(2)
  end subroutine
  ! p: total, cut, cycles, onset?, onset cycle/logit, late onset?, late cycle/fraction.
! @r2fn R2-C0120-F00001
  function r2_camera_cleavage(p,profile,n,rate,late_method) result(v) bind(c)
    integer(c_int), value :: n,late_method
    real(c_double), intent(inout) :: p(9)
    real(c_double), intent(in) :: profile(n)
    real(c_double), value :: rate
    real(c_double) :: v,limit,fraction,start,response,elapsed,weight,fitted,offset
    limit=p(1)*0.115_c_double; v=0; if(limit<=0) return
    fraction=p(2)/limit
    if(late_method/=0 .and. p(4)==0 .and. p(7)==0 .and. p(3)>=34 .and. fraction>=0.025_c_double) then
      p(7)=1; p(8)=p(3); p(9)=fraction
    end if
    if(p(4)==0 .and. fraction>=0.05_c_double) then
      start=clamp(fraction,0.05_c_double,0.8_c_double); p(4)=1; p(5)=p(3); p(6)=log(start/(1-start))
    end if
    response=fraction
    if(p(4)/=0) then
      elapsed=p(3)-p(5)
      response=response+r2_smoothstep(elapsed/2)*(sigmoid(p(6)+rate*elapsed)-response)
    end if
    if(p(7)/=0) then
      elapsed=p(3)-p(8); weight=r2_smoothstep((p(8)-34)/3)
      fitted=profile(1)+1.11_c_double*(r2_late_reporter(profile,n,elapsed*1.34_c_double)-profile(1))
      offset=(p(9)-0.025_c_double)*exp(-elapsed)
      response=response+weight*r2_smoothstep(elapsed/2)*(fitted+offset-response)
    end if
    v=limit*clamp(response,0.0_c_double,1.65_c_double)
  end function
! @r2fn R2-C0120-F00002
  function r2_smoothstep(x) result(v) bind(c)
    real(c_double), value :: x
    real(c_double) :: v,u
    u=clamp(x,0.0_c_double,1.0_c_double); v=u*u*(3-2*u)
  end function
! @r2fn R2-C0120-F00003
  function r2_late_reporter(p,n,elapsed) result(v) bind(c)
    integer(c_int), value :: n
    real(c_double), intent(in) :: p(n)
    real(c_double), value :: elapsed
    real(c_double) :: v,increment
    integer :: i
    i=floor(elapsed)
    if(i<n-1) then
      v=p(i+1)+(elapsed-i)*(p(i+2)-p(i+1))
    else
      increment=max(0.0_c_double,p(n)-p(max(1,n-2)))/2
      v=p(n)+increment*(1-exp(-0.45_c_double*(elapsed-(n-1))))/0.45_c_double
    end if
  end function
  ! probe state: total, cut, last made, minimum T, hot?, cycles
! @r2fn R2-C0120-F00004
  subroutine r2_probe_init(p,bright,dark) bind(c)
    real(c_double), intent(out) :: p(6)
    real(c_double), value :: bright,dark
    p=[(bright+dark)*1.2044e10_c_double,0.0_c_double,0.0_c_double,200.0_c_double,0.0_c_double,0.0_c_double]
  end subroutine
! @r2fn R2-C0120-F00005
  subroutine r2_probe_step(p,made_total,t,probe_nm,dark_nm,ann_s,primer_nm,tm,width) bind(c)
    real(c_double), intent(inout) :: p(6)
    real(c_double), value :: made_total,t,probe_nm,dark_nm,ann_s,primer_nm,tm,width
    real(c_double) :: made,hyb,full,delta
    if(t>85) then
      p(4)=200
      if(p(5)==0) then
        p(5)=1; p(6)=p(6)+1
      end if
    else
      if(t<p(4)) p(4)=t
      if(t<75) p(5)=0
    end if
    made=made_total-p(3); p(3)=made_total; if(made<=0) return
    hyb=1-exp(-2e-3_c_double*(probe_nm+dark_nm)*ann_s)
    full=2*primer_nm*1.2044e10_c_double*0.8_c_double
    delta=p(1)*0.15_c_double*(made/full)*sigmoid((tm-p(4))/width)*hyb/0.95_c_double
    p(2)=min(p(1)*0.15_c_double*1.5_c_double,p(2)+delta)
  end subroutine
! @r2fn R2-C0120-F00006
  function r2_probe_counts(total,cut,bright_nm,dark_nm,free_nm,q,cycles,drift,t) result(v) bind(c)
    real(c_double), value :: total,cut,bright_nm,dark_nm,free_nm,q,cycles,drift,t
    real(c_double) :: v,bright,cutnm,intactnm
    bright=bright_nm/max(1e-9_c_double,bright_nm+dark_nm)
    cutnm=cut/1.2044e10_c_double*bright; intactnm=(total-cut)/1.2044e10_c_double*bright
    v=45*(cutnm+(intactnm*(1-q)+free_nm)*(1+0.03_c_double*exp(-cycles/2) &
      -(0.0015_c_double+drift)*cycles))*(1-0.004_c_double*(t-60))
  end function
! @r2fn R2-C0120-F00007
  subroutine r2_signal_mix(a,b,factor,boost,out) bind(c)
    real(c_double), value :: a,b,factor,boost
    real(c_double), intent(out) :: out(2)
    out=[factor*(a+0.04_c_double*b+60)*boost,factor*(0.08_c_double*a+b+50)]
  end subroutine
! @r2fn R2-C0120-F00008
  function r2_half_life(t) result(v) bind(c)
    real(c_double), value :: t
    real(c_double) :: v,table(5,2)
    integer :: i
    table(:,1)=[-80.0_c_double,-20.0_c_double,4.0_c_double,22.0_c_double,37.0_c_double]
    table(:,2)=[73000.0_c_double,14600.0_c_double,2920.0_c_double,730.0_c_double,180.0_c_double]
    if(t<=table(1,1)) then
      v=table(1,2); return
    end if
    do i=2,5
      if(t<=table(i,1)) then
        v=(t-table(i-1,1))/(table(i,1)-table(i-1,1))
        v=exp(log(table(i-1,2))*(1-v)+log(table(i,2))*v); return
      end if
    end do
    v=table(5,2)*0.5_c_double**((t-37)/8)
  end function
! @r2fn R2-C0120-F00009
  function r2_intact(t,months,hl_factor) result(v) bind(c)
    real(c_double), value :: t,months,hl_factor
    real(c_double) :: v
    v=0.5_c_double**(months*30.4_c_double/(r2_half_life(t)*hl_factor))
  end function
! @r2fn R2-C0120-F00010
  function r2_mgb_bonus(n) result(v) bind(c)
    real(c_double), value :: n
    real(c_double) :: v
    v=clamp(30-0.8_c_double*n,10.0_c_double,20.0_c_double)
  end function
! @r2fn R2-C0120-F00011
  function r2_detector_sample(signal,noise) result(v) bind(c)
    real(c_double), value :: signal,noise
    real(c_double) :: v
    v=floor(min(4095.0_c_double,200+signal+(2+0.15_c_double*sqrt(signal))*noise)+0.5_c_double)*1.0_c_double
  end function
end module
