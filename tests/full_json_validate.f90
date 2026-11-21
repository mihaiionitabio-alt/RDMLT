program full_json_validate
use json_module
implicit none
type(json_file)::j
character(len=512)::name
integer::sc,n
logical::found
real(json_RK),allocatable::bank(:)
call get_command_argument(1,name)
call j%initialize(allow_duplicate_keys=.false.)
call j%load_file(trim(name))
if(j%failed())stop 1
call j%get('scenario',sc,found)
if(.not.found)stop 2
call j%get('scenarioBank',bank,found)
if(.not.found)stop 3
if(size(bank)/=306.or.sc<0.or.sc>51)stop 4
call j%destroy()
print *,'PASS full JSON-Fortran 9.0.5 native parser: scenario ',sc,', bank size ',size(bank)
end program
