! MISSY R2 Fortran: memory statistics of the computer running the page (browser build).
! The Emscripten C runtime is called directly from Fortran through bind(c); the short
! expressions passed to emscripten_run_script_int are evaluated by the browser and return
! whole numbers (MiB). Browsers that do not expose a figure return -1 and it is shown as n/a.
module native_sys
 use iso_c_binding
 implicit none
 private
 public :: sys_sample, sys_heap_mb, sys_js_used_mb, sys_js_limit_mb, sys_device_mb, sys_native
 logical,parameter :: sys_native=.false.
 integer,save :: sys_heap_mb=-1,sys_js_used_mb=-1,sys_js_limit_mb=-1,sys_device_mb=-1
 interface
  function emscripten_get_heap_size() bind(c,name='emscripten_get_heap_size') result(n)
   import :: c_size_t
   integer(c_size_t) :: n
  end function
  function emscripten_run_script_int(script) bind(c,name='emscripten_run_script_int') result(n)
   import :: c_char,c_int
   character(kind=c_char),dimension(*),intent(in) :: script
   integer(c_int) :: n
  end function
 end interface
contains
! @nativefn NR2-SYS-F0001
 subroutine sys_sample()
  integer(c_size_t)::h
  h=emscripten_get_heap_size()
  sys_heap_mb=int(h/1048576_c_size_t)
  sys_js_used_mb=emscripten_run_script_int( &
   'typeof performance!=="undefined"&&performance.memory?Math.round(performance.memory.usedJSHeapSize/1048576):-1'//c_null_char)
  sys_js_limit_mb=emscripten_run_script_int( &
   'typeof performance!=="undefined"&&performance.memory?Math.round(performance.memory.jsHeapSizeLimit/1048576):-1'//c_null_char)
  sys_device_mb=emscripten_run_script_int( &
   'typeof navigator!=="undefined"&&navigator.deviceMemory?Math.round(navigator.deviceMemory*1024):-1'//c_null_char)
 end subroutine
end module
