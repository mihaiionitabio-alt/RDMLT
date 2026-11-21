! MIT: rank-one REAL64 specializations from stdlib 0.8.1 stats mean/var templates.
! Adaptation: module procedures in a standalone module, optional-mask helper expanded;
! explicit short-input guard added. Full upstream archive and license bundled.
module stdlib_browser_stats
 use iso_c_binding
 use, intrinsic::ieee_arithmetic
 implicit none
contains
! @nativefn NR2-D01-F0001
 function stdlib_mean(x) result(res)
 real(c_double),intent(in)::x(:)
 real(c_double)::res
 if(size(x)==0)then
 res=ieee_value(1d0,ieee_quiet_nan);return
 end if
 res=sum(x)/real(size(x,kind=c_int64_t),c_double)
 end function
! @nativefn NR2-D01-F0002
 function stdlib_var(x) result(res)
 real(c_double),intent(in)::x(:)
 real(c_double)::res,n,mean
 if(size(x)<2)then
 res=ieee_value(1d0,ieee_quiet_nan);return
 end if
 n=real(size(x,kind=c_int64_t),c_double);mean=sum(x)/n
 res=sum((x-mean)**2)/(n-1)
 end function
end module
