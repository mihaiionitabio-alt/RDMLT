! SPDX-License-Identifier: GPL-3.0-only
! js/core/chemistry.js: same 10-bin kinetic model and random draw order.
module r2_chemistry
  use iso_c_binding, only: c_double, c_int
  use r2_math
  implicit none
  private
  public :: frac_open,primer_bound,extension,enzyme_half_life,primed,product_step
  public :: r2_chem_init,r2_chem_step,r2_ds_base_pairs,r2_chem_snapshot
  public :: r2_frac_open,r2_primer_bound,r2_extension,r2_enzyme_half_life,r2_primed
  integer, parameter :: nb=10
  real(c_double), parameter :: nm=1.2044e10_c_double,enz=5e10_c_double,dntp0=9.6e15_c_double
  ! c: target(ds,ss,ext[10],stalled), dimer(same), template, primers, dntp,
  !    active enzyme, blocked enzyme, realized copies, made target, made dimer.
  ! a: reaction?, copies, amplicon bp/Tm, template Tm, primer Tm/nM,
  !    efficiency, dimer rate/bp/Tm, hot start?, enzyme fraction, initial dimers.
contains
! @r2fn R2-C0116-F00001
  function r2_frac_open(t,tm,w) result(v) bind(c)
    real(c_double), value :: t,tm,w
    real(c_double) :: v
    v=frac_open(t,tm,w)
  end function
! @r2fn R2-C0116-F00002
  function r2_primer_bound(t,tm) result(v) bind(c)
    real(c_double), value :: t,tm
    real(c_double) :: v
    v=primer_bound(t,tm)
  end function
! @r2fn R2-C0116-F00003
  function r2_extension(t) result(v) bind(c)
    real(c_double), value :: t
    real(c_double) :: v
    v=extension(t)
  end function
! @r2fn R2-C0116-F00004
  function r2_enzyme_half_life(t) result(v) bind(c)
    real(c_double), value :: t
    real(c_double) :: v
    v=enzyme_half_life(t)
  end function
! @r2fn R2-C0116-F00005
  function r2_primed(p) result(v) bind(c)
    real(c_double), intent(in) :: p(13)
    real(c_double) :: v
    v=primed(p)
  end function
! @r2fn R2-C0116-F00006
  pure real(c_double) function frac_open(t,tm,w)
    real(c_double), intent(in) :: t,tm,w
    frac_open=sigmoid((t-tm)/w)
  end function
! @r2fn R2-C0116-F00007
  pure real(c_double) function primer_bound(t,tm)
    real(c_double), intent(in) :: t,tm
    primer_bound=sigmoid((tm-t)/1.5_c_double)
  end function
! @r2fn R2-C0116-F00008
  pure real(c_double) function extension(t) result(v)
    real(c_double), intent(in) :: t
    real(c_double) :: d
    d=(t-72.0_c_double)/14.0_c_double
    v=0; if(t<=85) v=60.0_c_double*exp(-d*d)
  end function
! @r2fn R2-C0116-F00009
  pure real(c_double) function enzyme_half_life(t)
    real(c_double), intent(in) :: t
    enzyme_half_life=2400.0_c_double*exp(-0.5_c_double*(t-95.0_c_double))
  end function
! @r2fn R2-C0116-F00010
  pure real(c_double) function primed(p) result(v)
    real(c_double), intent(in) :: p(13)
    integer :: b
    v=0
    do b=3,12
      v=v+p(b)
    end do
  end function
! @r2fn R2-C0116-F00011
  real(c_double) function product_step(c,base,len,tm,ptm,eff,served,t,dt) result(made)
    real(c_double), intent(inout) :: c(34)
    integer, intent(in) :: base
    real(c_double), intent(in) :: len,tm,ptm,eff,served,t,dt
    real(c_double) :: m,x,offhot,bound,young,closed,rp,rr,tot,leave,pr,re,v,adv,done
    integer :: b
    m=c(base+1)*fraction(5.0_c_double*frac_open(t,tm+2,0.5_c_double),dt)
    c(base+1)=c(base+1)-m; c(base+2)=c(base+2)+2*m
    x=c(base+13)*fraction(5.0_c_double*frac_open(t,tm-2,0.7_c_double),dt)
    c(base+13)=c(base+13)-x; c(base+2)=c(base+2)+x
    offhot=fraction(5.0_c_double*frac_open(t,tm-4,1.0_c_double),dt)
    do b=3,12
      x=c(base+b)*offhot; c(base+b)=c(base+b)-x; c(base+2)=c(base+2)+x
    end do
    bound=primer_bound(t,ptm); young=min(1.0_c_double,15.0_c_double/(len/nb))
    x=c(base+3)*fraction(2*(1-bound)*young,dt); c(base+3)=c(base+3)-x; c(base+2)=c(base+2)+x; c(28)=c(28)+x
    closed=1-frac_open(t,tm,0.7_c_double)
    rp=1e-3_c_double*(c(28)/2/nm)*bound; rr=2.5e-3_c_double*(c(base+2)/2/nm)*closed; tot=rp+rr
    leave=c(base+2)*fraction(tot,dt); pr=0
    if(tot>0) pr=leave*rp/tot
    re=leave-pr; pr=min(pr,c(28))
    c(base+2)=c(base+2)-pr-re; c(base+3)=c(base+3)+pr; c(28)=c(28)-pr; c(base+1)=c(base+1)+re/2
    v=0; if(c(29)>0) v=extension(t)*served
    adv=min(1.0_c_double,v*dt*nb/len); done=c(base+12)*adv; c(base+12)=c(base+12)-done
    do b=11,3,-1
      x=c(base+b)*adv; c(base+b)=c(base+b)-x; c(base+b+1)=c(base+b+1)+x
    end do
    made=done*eff; c(base+1)=c(base+1)+made; c(base+13)=c(base+13)+done-made
    c(29)=max(0.0_c_double,c(29)-made*len)
  end function
! @r2fn R2-C0116-F00012
  subroutine r2_chem_init(c,a,rng) bind(c)
    real(c_double), intent(out) :: c(34)
    real(c_double), intent(in) :: a(14)
    real(c_double), intent(inout) :: rng
    real(c_double) :: copies,e,n,draw
    c=0; if(a(1)/=1) return
    draw=rng_gauss(rng); copies=rng_poisson(rng,a(2)*(1+0.02_c_double*draw))
    c(32)=copies; c(27)=copies; c(28)=2*a(7)*nm; c(29)=dntp0
    e=enz*a(13)
    if(a(12)/=0) then
      c(31)=e
    else
      c(30)=e
    end if
    if(a(14)>0) then
      n=min(rng_poisson(rng,a(14)),c(28)/2); c(14)=c(14)+n; c(28)=c(28)-2*n
    end if
  end subroutine
! @r2fn R2-C0116-F00013
  subroutine r2_chem_step(c,a,t,dt,rng) bind(c)
    real(c_double), intent(inout) :: c(34),rng
    real(c_double), intent(in) :: a(14)
    real(c_double), value :: t,dt
    real(c_double) :: act,g,need,served,pf,ex,lam,nd,made,needed_target,needed_dimer
    integer :: b
    if(a(1)/=1) return
    act=c(31)*fraction(log(2.0_c_double)/30*sigmoid((t-90)/1.5_c_double),dt)
    c(31)=c(31)-act; c(30)=c(30)+act
    c(30)=c(30)*exp(-log(2.0_c_double)/enzyme_half_life(t)*dt)
    g=c(27)*fraction(5.0_c_double*frac_open(t,a(5)+2,0.7_c_double),dt)
    c(27)=c(27)-g; c(2)=c(2)+2*g
    needed_target=0; needed_dimer=0
    do b=3,12
      needed_target=needed_target+c(b); needed_dimer=needed_dimer+c(b+13)
    end do
    need=needed_target+needed_dimer; served=1
    if(need>c(30)) served=c(30)/need
    made=product_step(c,0,a(3),a(4),a(6),a(8),served,t,dt); c(33)=c(33)+made
    made=product_step(c,13,a(10),a(11),a(6),0.85_c_double,served,t,dt); c(34)=c(34)+made
    pf=0; if(a(7)>0) pf=c(28)/(2*a(7)*nm)
    ex=extension(t); g=1; if(ex<=0.05_c_double) g=ex/0.05_c_double
    lam=a(9)*pf*pf*sigmoid(((a(6)-12)-t)/2.5_c_double)*(c(30)/enz)*g*dt
    nd=rng_poisson(rng,lam)
    if(nd>0 .and. c(28)>=2*nd) then
      c(14)=c(14)+nd; c(28)=c(28)-2*nd
    end if
  end subroutine
! @r2fn R2-C0116-F00014
  function r2_ds_base_pairs(c,a,t) result(bp) bind(c)
    real(c_double), intent(in) :: c(34),a(14)
    real(c_double), value :: t
    real(c_double) :: bp,len,tm
    integer :: j,k,b
    bp=0; if(a(1)/=1) return
    do j=1,2
      k=(j-1)*13; len=a(3); tm=a(4)
      if(j==2) then
        len=a(10); tm=a(11)
      end if
      bp=bp+c(k+1)*len*(1-frac_open(t,tm,0.7_c_double))
      do b=0,9
        bp=bp+c(k+3+b)*len*(b+0.5_c_double)/nb*(1-frac_open(t,tm-4,1.5_c_double))
      end do
    end do
  end function
! @r2fn R2-C0116-F00015
  subroutine r2_chem_snapshot(c,a,out) bind(c)
    real(c_double), intent(in) :: c(34),a(14)
    real(c_double), intent(out) :: out(11)
    out=[c(27),c(1),c(2),primed(c(1:13)),c(13),c(14)+c(15)/2, &
      0.0_c_double,c(30)/enz,c(31)/enz,c(32),c(29)]
    if(a(7)>0) out(7)=c(28)/(2*a(7)*nm)
  end subroutine
end module
