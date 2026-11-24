! MISSY R2 Fortran: memory statistics stub for the native (gfortran) test build, which has no
! browser. The page build uses native_sys_wasm.f90 instead.
module native_sys
 implicit none
 private
 public :: sys_sample, sys_heap_mb, sys_js_used_mb, sys_js_limit_mb, sys_device_mb, sys_native, sys_script, sys_script_int, sys_last_len
 logical,parameter :: sys_native=.true.
 integer,save :: sys_last_len=0
 integer,save :: sys_heap_mb=-1,sys_js_used_mb=-1,sys_js_limit_mb=-1,sys_device_mb=-1
contains
! @nativefn NR2-SYS-F0002
 subroutine sys_script(js)
  character(len=*),intent(in)::js
  sys_last_len=len(js)   ! no browser in the test build; the length is kept for checks
 end subroutine
! @nativefn NR2-SYS-F0003
 integer function sys_script_int(js) result(v)
  character(len=*),intent(in)::js
  v=0;if(len(js)<0)v=-1
 end function
! @nativefn NR2-SYS-F0001
 subroutine sys_sample()
 end subroutine
end module
