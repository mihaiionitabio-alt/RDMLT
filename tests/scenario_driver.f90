program scenario_driver
use native_model
use native_io
use native_rdmlt
use stdlib_browser_stats
implicit none
integer::code,i,j,u,steps,oldnh
real(c_double)::normal(45,maxw),normal2(45,maxw),saved(6),v
character(len=120)::name
logical::ok
p_speed=5000;p_ext=1;p_rep=2;p_runs=1;p_seed=11
if(abs(stdlib_mean([1d0,2d0,3d0,4d0])-2.5d0)>1d-12)stop 1
if(abs(stdlib_var([1d0,2d0,3d0,4d0])-5d0/3)>1d-12)stop 2
! Every case is editable, including the unsupported acquisition workflow.
do code=1,51
 saved=scenario_bank(:,code)
 call edit_parameter(code,1,1)
 if(.not.parameters_valid(code,scenario_bank(:,code)))stop 16
 scenario_bank(:,code)=saved
 call edit_parameter(code,6,1)
 if(scenario_bank(6,code)/=saved(6)+1)stop 3
 scenario_bank(:,code)=saved
end do
call export_profile()
call parse_profile(work,used)
if(index(message,'validated')==0)stop 4
open(newunit=u,file='scenarios/results.csv',status='replace')
write(u,'(a)')'code,status,cycles,validFirstTrace,verdict,maxSensorGapC'
do code=0,51
 nh=0;selected=0;remaining=0;running=.false.;p_scenario=code
 call start_batch()
 if(code==21)then
  if(running.or.nh/=0)stop 5
  cycle
 end if
 do steps=1,300
  call tick_model(1000d0)
  if(.not.running.and.remaining==0)exit
 end do
 if(nh/=1.or.running)stop 6
 if(code==0)then
  normal=history(1)%y;normal2=history(1)%y2
 end if
 if(code==16.or.code==18)then
  if(any(normal/=history(1)%y))stop 7
 end if
 if(code==19.and.count(history(1)%y(1:45,1)>=0)/=42)stop 8
 if(code==20.and.history(1)%ncy/=40)stop 9
 if(code==22.and.history(1)%ncy/=30)stop 10
 if(code==38.and.count(history(1)%present(1:7)==0)/=2)stop 11
 if(code==12.and.history(1)%status/=3)stop 12
 if((code==13.or.code==14).and.history(1)%status/=7)stop 13
 write(u,'(i0,a,i0,a,i0,a,i0,a,i0,a,f8.3)')code,',',history(1)%status,',',history(1)%ncy,',', &
 count(history(1)%y(1:history(1)%ncy,1)>=0),',',verdict(history(1)),',',history(1)%maxgap
 call export_run(history(1))
 write(name,'(a,i2.2,a)')'scenarios/case-',code,'.rdmlt'
 open(newunit=j,file=trim(name),access='stream',form='unformatted',status='replace')
 write(j)output(1:output_size);close(j)
 ! Freshly export/import through actual archive reader; it must preserve the snapshot.
 call load_file(output,output_size)
 if(nh/=2)then
  print *,code,trim(message);stop 14
 end if
 if(history(2)%scenario/=code.or.any(abs(history(2)%params-history(1)%params)>1d-10))then
 print *,code,history(2)%scenario,history(1)%params,history(2)%params;stop 15
 end if
end do
close(u)
print *,'PASS: 51 editable, 50 executable, unsupported melt rejected, all run packages round-trip.'
end program
