! MISSY R2 Fortran: memory statistics stub for the native (gfortran) test build, which has no
! browser. The page build uses native_sys_wasm.f90 instead.
module native_sys
 implicit none
 private
 public :: sys_sample, sys_heap_mb, sys_js_used_mb, sys_js_limit_mb, sys_device_mb, sys_native
 logical,parameter :: sys_native=.true.
 integer,save :: sys_heap_mb=-1,sys_js_used_mb=-1,sys_js_limit_mb=-1,sys_device_mb=-1
contains
! @nativefn NR2-SYS-F0001
 subroutine sys_sample()
 end subroutine
end module
