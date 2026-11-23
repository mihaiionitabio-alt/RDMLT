! SPDX-License-Identifier: GPL-3.0-only
! Native profile parsing, ZIP-store/CRC, RDML serialization and native-package import.
module native_io
 use iso_c_binding
 use native_model
 use json_fortran_browser_strings, only:escape_string,lowercase_string
 implicit none
 integer,parameter::cap=33554432,input_cap=67108864,max_output=1073741824
 ! Package bytes for the host; grows on demand (the host reads the pointer after each export).
 integer(c_int8_t),allocatable,target,save::output(:)
 integer,save::output_size=0,name_size=0
 character(c_char),target,save::output_name(80)
 ! Export text buffer, allocated once at startup (a static buffer would be embedded in the module as blanks).
 integer,parameter::work_cap=50331648
 character(len=:),allocatable,save::work
 integer,save::used=0
 integer(c_int64_t),save::crc_table(0:255)=0
 logical,save::crc_ready=.false.
contains
! @nativefn NR2-M02-F0030
 subroutine crc_init()
  ! Standard reflected CRC-32 table (polynomial EDB88320); results equal the bitwise loop.
  integer(c_int64_t)::c
  integer::n,k
  do n=0,255
   c=int(n,c_int64_t)
   do k=1,8
    if(iand(c,1_c_int64_t)/=0)then
     c=ieor(shiftr(c,1),3988292384_c_int64_t)
    else
     c=shiftr(c,1)
    end if
   end do
   crc_table(n)=c
  end do
  crc_ready=.true.
 end subroutine
! @nativefn NR2-M02-F0032
 subroutine ensure_output(n)
  ! Capacity of at least n bytes, doubling, keeping the bytes already written.
  integer,intent(in)::n
  integer(c_int8_t),allocatable::t(:)
  integer::m
  if(.not.allocated(output))allocate(output(cap))
  if(size(output)>=n)return
  m=size(output)
  do while(m<n.and.m<max_output)
   m=min(max_output,2*m)
  end do
  if(m<n)return
  allocate(t(m));t(1:output_size)=output(1:output_size);call move_alloc(t,output)
 end subroutine
! @nativefn NR2-M02-F0031
 subroutine ensure_work()
  if(.not.allocated(work))allocate(character(len=work_cap)::work)
 end subroutine
! @nativefn NR2-M02-F0001
 subroutine add(s)
  character(len=*),intent(in)::s
  integer::n
  call ensure_work()
  n=len_trim(s)
  if(used+n>len(work))then
   message='Export exceeds the native buffer limit.';return
  end if
  work(used+1:used+n)=s(1:n);used=used+n
 end subroutine
! @nativefn NR2-M02-F0002
 subroutine newline()
  call add(achar(10))
 end subroutine
! @nativefn NR2-M02-F0003
 subroutine set_name(s)
  character(len=*),intent(in)::s
  integer::i
  name_size=min(80,len_trim(s));output_name=c_null_char
  do i=1,name_size
   output_name(i)=s(i:i)
  end do
 end subroutine
! @nativefn NR2-M02-F0004
 subroutine put_byte(v)
  integer,intent(in)::v
  integer::b
  if(.not.allocated(output))call ensure_output(cap)
  if(output_size>=size(output))call ensure_output(output_size+1)
  if(output_size>=size(output))return
  b=iand(v,255);if(b>127)b=b-256
  output_size=output_size+1;output(output_size)=int(b,c_int8_t)
 end subroutine
! @nativefn NR2-M02-F0005
 subroutine put_int(v,n)
  integer(c_int64_t),intent(in)::v
  integer,intent(in)::n
  integer::i
  do i=0,n-1
   call put_byte(int(iand(shiftr(v,8*i),255_c_int64_t)))
  end do
 end subroutine
! @nativefn NR2-M02-F0006
 integer(c_int64_t) function crc_text(s,n) result(crc)
  integer,intent(in)::n
  character(len=*),intent(in)::s
  integer(c_int64_t)::c
  integer::i,j
  c=4294967295_c_int64_t
  if(crc_ready)then
   do i=1,n
    c=ieor(crc_table(iand(ieor(c,int(iachar(s(i:i)),c_int64_t)),255_c_int64_t)),shiftr(c,8))
   end do
   crc=ieor(c,4294967295_c_int64_t);return
  end if
  do i=1,n
   c=ieor(c,int(iachar(s(i:i)),c_int64_t))
   do j=1,8
    if(iand(c,1_c_int64_t)/=0)then
     c=ieor(shiftr(c,1),3988292384_c_int64_t)
    else
     c=shiftr(c,1)
    end if
   end do
  end do
  crc=ieor(c,4294967295_c_int64_t)
 end function
! @nativefn NR2-M02-F0007
 subroutine zip_entry(name,offset,crc,sz)
  character(len=*),intent(in)::name
  integer,intent(out)::offset,sz
  integer(c_int64_t),intent(out)::crc
  integer::i
  offset=output_size;sz=used;crc=crc_text(work,used);
  call put_int(int(z'04034B50',c_int64_t),4);call put_int(20_c_int64_t,2)
  call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(2162688_c_int64_t,4)
  call put_int(crc,4);call put_int(int(sz,c_int64_t),4);call put_int(int(sz,c_int64_t),4)
  call put_int(int(len_trim(name),c_int64_t),2);call put_int(0_c_int64_t,2)
  
  do i=1,len_trim(name)
   call put_byte(iachar(name(i:i)))
  end do
  
  do i=1,used
   call put_byte(iachar(work(i:i)))
  end do
  
 end subroutine
! @nativefn NR2-M02-F0008
 subroutine zip_directory(names,offsets,crcs,sizes,n)
  integer,intent(in)::n,offsets(n),sizes(n)
  character(len=*),intent(in)::names(n)
  integer(c_int64_t),intent(in)::crcs(n)
  integer::start,i,j,sz
  start=output_size
  do i=1,n
   call put_int(int(z'02014B50',c_int64_t),4);call put_int(20_c_int64_t,2);call put_int(20_c_int64_t,2)
   call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(2162688_c_int64_t,4)
   call put_int(crcs(i),4);call put_int(int(sizes(i),c_int64_t),4);call put_int(int(sizes(i),c_int64_t),4)
   call put_int(int(len_trim(names(i)),c_int64_t),2)
   call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2)
   call put_int(0_c_int64_t,2);call put_int(2162688_c_int64_t,4);call put_int(int(offsets(i),c_int64_t),4)
   do j=1,len_trim(names(i))
    call put_byte(iachar(names(i)(j:j)))
   end do
  end do
  sz=output_size-start;call put_int(int(z'06054B50',c_int64_t),4)
  call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(int(n,c_int64_t),2)
  call put_int(int(n,c_int64_t),2);call put_int(int(sz,c_int64_t),4);call put_int(int(start,c_int64_t),4)
  call put_int(0_c_int64_t,2)
 end subroutine
! @nativefn NR2-M02-F0009
 subroutine write_record(r)
  type(run_t),intent(in)::r
  integer::i,j
  used=0;call add('MISSY-NATIVE-2');call newline()
  call add(trim(integer_text(r%nw))//' '//trim(integer_text(r%ncy))//' '//trim(integer_text(r%status))//' ' &
   //trim(integer_text(r%seed))//' '//trim(integer_text(r%ne))//' '//trim(integer_text(r%nr))//' ' &
   //trim(integer_text(r%copies))//' '//trim(integer_text(r%threshold))//' '//trim(integer_text(r%fault))//' ' &
   //trim(integer_text(r%nt))//' '//trim(integer_text(nint(r%elapsed*1000)))//' '//trim(integer_text(nint(r%maxgap*1000))))
  call newline()
  call add(trim(integer_text(r%scenario))//' '//trim(integer_text(r%planned_cycles)))
  do j=1,6
   used=used+1;work(used:used)=' ';call add(trim(integer_text(nint(r%params(j)*1000))))
  end do
  call newline()
  do j=1,r%nw
   call add(trim(integer_text(r%present(j))))
   if(j<r%nw)then
    used=used+1;work(used:used)=' '
   end if
  end do
  call newline()
  do i=1,r%ncy
   do j=1,r%nw
    call add(trim(integer_text(nint(r%y(i,j))))//' ')
    ! Explicit delimiter because add trims trailing whitespace.
    if(j<r%nw)then
     used=used+1;work(used:used)=' '
    end if
   end do
   call newline()
   do j=1,r%nw
    call add(trim(integer_text(nint(r%y2(i,j)))))
    if(j<r%nw)then
     used=used+1;work(used:used)=' '
    end if
   end do
   call newline()
  end do
  do i=1,r%nt
   do j=1,6
    call add(trim(integer_text(nint(r%trace(j,i)*1000))))
    if(j<6)then
     used=used+1;work(used:used)=' '
    end if
   end do
   call newline()
  end do
 end subroutine
! @nativefn NR2-M02-F0010
 subroutine write_rdml(r)
  type(run_t),intent(in)::r
  integer::i,w,s,channel,pos
  character(len=3)::role
  used=0;s=r%ne*r%nr
  call add('<?xml version="1.0" encoding="UTF-8"?><rdml xmlns="http://www.rdml.org" version="1.3">')
  call add('<id><publisher>MISSY native preview</publisher><serialNumber>NATIVE-'//trim(integer_text(r%serial))// &
   '</serialNumber></id><dye id="FAM"><description>Simulated probe fluorescence</description></dye>')
  call add('<dye id="YY"><description>Simulated second detection channel</description></dye>')
  call add('<target id="DNA-target"><type>toi</type><dyeId id="FAM"/></target>')
  call add('<target id="YY-target"><type>ref</type><dyeId id="YY"/></target>')
  do w=1,r%nw
   role='unk';if(w==s+1.or.w>=s+4)role='pos'
   if(w==s+2)role='ntc'
   if(w==s+3)role='nac'
   call add('<sample id="well-'//trim(integer_text(w))//'"><type>'//trim(role)//'</type></sample>')
  end do
  call add('<experiment id="Native-'//trim(integer_text(r%serial))//'"><run id="Native-'// &
   trim(integer_text(r%serial))//'"><pcrFormat><rows>16</rows><columns>24</columns><rowLabel>ABC</rowLabel>'// &
   '<columnLabel>123</columnLabel></pcrFormat>')
  do w=1,r%nw
   if(r%present(w)==0)cycle
   pos=recorded_position(r%scenario,r%params,w,r%ne,r%nr)
   call add('<react id="'//trim(integer_text(pos+1))//'"><sample id="well-'//trim(integer_text(w))//'"/><vol>20</vol>')
   do channel=1,2
    if(channel==1)then
     call add('<data><tar id="DNA-target"/>')
     if(r%cq(w)>0)call add('<cq>'//trim(real_text(r%cq(w),4))//'</cq>')
    else
     call add('<data><tar id="YY-target"/>')
     if(r%cq2(w)>0)call add('<cq>'//trim(real_text(r%cq2(w),4))//'</cq>')
    end if
    do i=1,r%ncy
     if(channel==1)then
      if(r%y(i,w)<0)cycle
      call add('<adp><cyc>'//trim(integer_text(i))//'</cyc><fluor>'//trim(integer_text(nint(r%y(i,w))))//'</fluor></adp>')
     else
      if(r%y2(i,w)<0)cycle
      call add('<adp><cyc>'//trim(integer_text(i))//'</cyc><fluor>'//trim(integer_text(nint(r%y2(i,w))))//'</fluor></adp>')
     end if
    end do
    call add('</data>')
   end do
   call add('</react>')
  end do
  call add('</run></experiment></rdml>')
 end subroutine
! @nativefn NR2-M02-F0011
 subroutine export_run_reduced(r)
  type(run_t),intent(in)::r
  character(len=40)::names(4)
  integer::offsets(4),sizes(4),i,j
  character(len=:),allocatable::escaped
  integer(c_int64_t)::crcs(4)
  names=[character(len=40)::'rdml_data.xml','native-run.txt','source/fault-provenance.json','rdmlt.xml']
  output_size=0;call write_rdml(r);call zip_entry(trim(names(1)),offsets(1),crcs(1),sizes(1))
  call write_record(r);call zip_entry(trim(names(2)),offsets(2),crcs(2),sizes(2))
  used=0;call add('{"format":"missy-native-provenance/1","engine":"fortran","simulated":true,'// &
   '"monitorGapLimitC":2,"monitorPersistenceMs":2000,"injectedSecondaryBiasC":')
  call add(trim(integer_text(r%fault*5))//',"maxSensorGapC":'//trim(real_text(r%maxgap,3))// &
   ',"status":'//trim(integer_text(r%status))//',"seed":'//trim(integer_text(r%seed))// &
   ',"extractions":'//trim(integer_text(r%ne))//',"replicates":'//trim(integer_text(r%nr))// &
   ',"startingCopies":'//trim(integer_text(r%copies))//',"netThreshold":'//trim(integer_text(r%threshold))// &
   ',"plannedCycles":'//trim(integer_text(r%planned_cycles))//',"acquisitionCyclesReached":'//trim(integer_text(r%ncy))// &
   ',"elapsedSeconds":'//trim(real_text(r%elapsed,3))// &
   ',"commonCause":"shared virtual plant, browser, computer","checkpointRecovery":false,"scenarioCode":'// &
   trim(integer_text(r%scenario))//',"deliberateScenario":')
  if(r%scenario==0)then
   call add('false')
  else
   call add('true,"scenarioId":"'//trim(scenario_ids(r%scenario))//'","kind":"'//trim(scenario_kinds(r%scenario))//'","title":"')
   call escape_string(trim(scenario_titles(r%scenario)),escaped,.false.);call add(escaped//'","parameters":[')
   do j=1,6
    call escape_string(trim(parameter_names(j,r%scenario)),escaped,.false.)
    call add('{"name":"'//escaped//'","value":'//trim(real_text(r%params(j),3))//'}')
    if(j<6)call add(',')
   end do
   call add(']')
  end if
  call add(',"limitations":"Reduced generic DNA probe workflow; experimental RDMLt; no claim of legacy SOP equivalence"}')
  call zip_entry(trim(names(3)),offsets(3),crcs(3),sizes(3))
  used=0;call add('<?xml version="1.0"?><rdmlt xmlns="urn:rdmlt:1" version="1.0-draft" baseVersion="1.3" revision="2">')
  do i=1,3
   call add('<file id="file-'//trim(integer_text(i))//'"><filename>'//trim(names(i))// &
    '</filename><byteCount>'//trim(integer_text(sizes(i)))//'</byteCount></file>')
  end do
  call add('<core><file id="file-1"/></core><description>Experimental reduced native edition. '// &
   'ZIP entries use CRC-32, not authenticated signatures. Native run telemetry is in native-run.txt.</description></rdmlt>')
  call zip_entry(trim(names(4)),offsets(4),crcs(4),sizes(4));call zip_directory(names,offsets,crcs,sizes,4)
  call set_name('MISSY_native_run_'//trim(integer_text(r%serial))//'.rdmlt')
 end subroutine
! @nativefn NR2-M02-F0021
 subroutine export_all_runs_reduced()
  integer(c_int8_t),allocatable::bundle(:)
  character(len=40)::names(maxruns)
  integer::offsets(maxruns),sizes(maxruns),i,j,n,outer_size
  integer(c_int64_t)::crcs(maxruns)
  if(nh==0)then
   output_size=0;message='No stored virtual runs to export.';return
  end if
  call ensure_output(cap);allocate(bundle(cap));outer_size=0
  do i=1,nh
   call export_run_reduced(history(i));n=output_size
   if(n>len(work).or.outer_size+n+200*(nh+1)>cap)then
    output_size=0;message='Run archive exceeds the export buffer. Export runs individually.'
    deallocate(bundle);return
   end if
   do j=1,n
    work(j:j)=achar(iand(int(output(j)),255))
   end do
   used=n
   output(1:outer_size)=bundle(1:outer_size);output_size=outer_size
   names(i)='MISSY_native_run_'//trim(integer_text(i))//'.rdmlt'
   call zip_entry(trim(names(i)),offsets(i),crcs(i),sizes(i))
   outer_size=output_size;bundle(1:outer_size)=output(1:outer_size)
  end do
  call zip_directory(names(1:nh),offsets(1:nh),crcs(1:nh),sizes(1:nh),nh)
  call set_name('MISSY_all_virtual_runs.rdmlt.zip')
  message='All '//trim(integer_text(nh))//' stored virtual runs packaged as individual RDMLt files.'
  deallocate(bundle)
 end subroutine
! @nativefn NR2-M02-F0012
 subroutine export_profile()
  integer::i
  used=0;call add('{"format":"missy-native-profile/2","runs":'//trim(integer_text(p_runs))// &
   ',"extractions":'//trim(integer_text(p_ext))//',"replicates":'//trim(integer_text(p_rep))// &
   ',"copies":'//trim(integer_text(p_copies))//',"threshold":'//trim(integer_text(p_threshold))// &
   ',"seed":'//trim(integer_text(p_seed))//',"fault":'//trim(integer_text(p_fault))// &
   ',"speed":'//trim(integer_text(p_speed))//',"scenario":'//trim(integer_text(p_scenario))//',"scenarioBank":[')
  do i=1,306
   call add(trim(real_text(scenario_bank(mod(i-1,6)+1,(i-1)/6+1),3)))
   if(i<306)call add(',')
  end do
  call add(']}')
  output_size=0
  do i=1,used
   call put_byte(iachar(work(i:i)))
  end do
  call set_name('MISSY_native_profile.json')
 end subroutine
! @nativefn NR2-M02-F0013
 integer function byte_at(a,i)result(v)
  integer(c_int8_t),intent(in)::a(*)
  integer,intent(in)::i
  v=iand(int(a(i)),255)
 end function
! @nativefn NR2-M02-F0014
 integer(c_int64_t) function read_int(a,i,n)result(v)
  integer(c_int8_t),intent(in)::a(*)
  integer,intent(in)::i,n
  integer::j
  v=0
  do j=0,n-1
   v=ior(v,shiftl(int(byte_at(a,i+j),c_int64_t),j*8))
  end do
 end function
! @nativefn NR2-M02-F0015
 subroutine skip_space(s,n,i)
  character(len=*),intent(in)::s
  integer,intent(in)::n
  integer,intent(inout)::i
  do while(i<=n)
   if(index(' '//achar(9)//achar(10)//achar(13),s(i:i))==0)exit
   i=i+1
  end do
 end subroutine
! @nativefn NR2-M02-F0019
 subroutine json_number(s,n,i,v,ok)
  character(len=*),intent(in)::s
  integer,intent(in)::n
  integer,intent(inout)::i
  real(c_double),intent(out)::v
  logical,intent(out)::ok
  integer::sg,es,expn,digits,frac,c,start
  v=0;ok=.false.;sg=1;es=1;expn=0;frac=0;digits=0
  if(i>n)return
  if(s(i:i)=='-')then
   sg=-1;i=i+1
  end if
  if(i>n)return
  start=i
  do while(i<=n)
   c=iachar(s(i:i))-48;if(c<0.or.c>9)exit
   if(i>start.and.s(start:start)=='0')return
   digits=digits+1;if(digits>12)return
   v=v*10+c;i=i+1
  end do
  if(digits==0)return
  if(i<=n)then
   if(s(i:i)=='.')then
    i=i+1;start=i
    do while(i<=n)
     c=iachar(s(i:i))-48;if(c<0.or.c>9)exit
     frac=frac+1;if(frac>9)return
     v=v+c*10d0**(-frac);i=i+1
    end do
    if(i==start)return
   end if
  end if
  if(i<=n)then
   if(s(i:i)=='e'.or.s(i:i)=='E')then
    i=i+1;if(i>n)return
    if(s(i:i)=='+'.or.s(i:i)=='-')then
     if(s(i:i)=='-')es=-1
     i=i+1
    end if
    start=i
    do while(i<=n)
     c=iachar(s(i:i))-48;if(c<0.or.c>9)exit
     expn=expn*10+c;if(expn>30)return
     i=i+1
    end do
    if(i==start)return
   end if
  end if
  v=sg*v*10d0**(es*expn);ok=ieee_is_finite(v)
 end subroutine
! @nativefn NR2-M02-F0020
 subroutine parse_profile(s,n)
  integer,intent(in)::n
  character(len=*),intent(in)::s
  character(len=32)::key,fmt
  character(len=16),parameter::keys(8)=[character(len=16)::'runs','extractions','replicates','copies','threshold','seed','fault','speed']
  integer::i,j,k,idx,values(8),seen(12),sc,countv
  real(c_double)::v,pars(6),bank(6,51),arr(306)
  logical::ok,done
  values=[p_runs,p_ext,p_rep,p_copies,p_threshold,p_seed,p_fault,p_speed]
  seen=0;sc=p_scenario;bank=scenario_bank;pars=0;fmt='';i=1
  message='Invalid profile JSON or parameter bounds. Nothing changed.'
  call skip_space(s,n,i);if(i>n)return
  if(s(i:i)/='{')return
  i=i+1
  do
   call skip_space(s,n,i);if(i>n)return
   if(s(i:i)/='"')return
   j=index(s(i+1:n),'"');if(j<1.or.j>32)return
   key=s(i+1:i+j-1);i=i+j+1
   call skip_space(s,n,i);if(i>n)return
   if(s(i:i)/=':')return
   i=i+1;call skip_space(s,n,i);if(i>n)return
   idx=0
   do k=1,8
    if(trim(key)==trim(keys(k)))idx=k
   end do
   if(key=='format')idx=9
   if(key=='scenario')idx=10
   if(key=='parameters')idx=11
   if(key=='scenarioBank')idx=12
   if(idx==0)return
   if(seen(idx)/=0)return
   seen(idx)=1
   if(idx==9)then
    if(s(i:i)/='"')return
    j=index(s(i+1:n),'"');if(j<1.or.j>32)return
    fmt=s(i+1:i+j-1);i=i+j+1
   else if(idx==11.or.idx==12)then
    if(s(i:i)/='[')return
    i=i+1;countv=6;if(idx==12)countv=306
    do k=1,countv
     call skip_space(s,n,i);call json_number(s,n,i,v,ok);if(.not.ok)return
     arr(k)=v;call skip_space(s,n,i);if(i>n)return
     if(k<countv)then
      if(s(i:i)/=',')return
      i=i+1
     end if
    end do
    if(s(i:i)/=']')return
    i=i+1
    if(idx==11)pars=arr(1:6)
    if(idx==12)bank=reshape(arr,[6,51])
   else
    call json_number(s,n,i,v,ok);if(.not.ok)return
    if(v<0.or.v>10000000.or.v/=anint(v))return
    if(idx<=8)values(idx)=nint(v)
    if(idx==10)sc=nint(v)
   end if
   call skip_space(s,n,i);if(i>n)return
   if(s(i:i)=='}')exit
   if(s(i:i)/=',')return
   i=i+1
  end do
  i=i+1;call skip_space(s,n,i);if(i<=n)return
  if(any(seen(1:9)==0))return
  if(fmt/='missy-native-profile/1'.and.fmt/='missy-native-profile/2')return
  if(fmt=='missy-native-profile/2'.and.seen(10)==0)return
  if(fmt=='missy-native-profile/1')then
   if(any(seen(10:12)/=0))return
   sc=0
  end if
  if(sc<0.or.sc>51)return
  do k=1,51
   if(.not.parameters_valid(k,bank(:,k)))return
  end do
  if(seen(11)==1)then
   if(sc==0)return
   if(.not.parameters_valid(sc,pars))return
   bank(:,sc)=pars
  end if
  if(values(1)<1.or.values(1)>20.or.values(2)<1.or.values(2)>3.or.values(3)<1.or.values(3)>3)return
  if(values(4)<1.or.values(4)>10000000.or.values(5)<20.or.values(5)>2000)return
  if(values(6)<1.or.values(6)>999999.or.values(7)>1.or.values(8)<100.or.values(8)>5000)return
  p_runs=values(1);p_ext=values(2);p_rep=values(3);p_copies=values(4);p_threshold=values(5)
  p_seed=values(6);p_fault=values(7);p_speed=values(8);p_scenario=sc;scenario_bank=bank
  message='Profile and scenario bank validated and loaded in Fortran.'
 end subroutine
! @nativefn NR2-M02-F0016
 subroutine parse_numbers(s,out,n,ios)
  character(len=*),intent(in)::s
  integer,intent(in)::n
  integer,intent(out)::out(n),ios
  integer::i,k,sgn,digits,c
  integer(c_int64_t)::v
  out=0;ios=1;i=1
  do k=1,n
   do while(i<=len(s))
    if(index(' '//achar(9)//achar(13),s(i:i))==0)exit
    i=i+1
   end do
   if(i>len(s))return
   sgn=1;if(s(i:i)=='-')then
    sgn=-1;i=i+1
   end if
   v=0;digits=0
   do while(i<=len(s))
    c=iachar(s(i:i))-48;if(c<0.or.c>9)exit
    digits=digits+1;v=v*10+c;if(v>2147483647_c_int64_t)return
    i=i+1
   end do
   if(digits==0)return
   if(i<=len(s))then
    if(index(' '//achar(9)//achar(13),s(i:i))==0)return
   end if
   out(k)=int(v)*sgn
  end do
  if(i<=len_trim(s))return
  ios=0
 end subroutine
! @nativefn NR2-M02-F0017
 subroutine import_record(s,n)
  integer,intent(in)::n
  character(len=*),intent(in)::s
  type(run_t)::r
  integer::pos,j,k,i,w,ios,tm,gap,nums(14),version
  message='Invalid native run record; no history changed.'
  if(nh>=maxruns)return
  pos=index(s(1:n),achar(10));if(pos==0)return
  version=0
  if(s(1:pos-1)=='MISSY-NATIVE-1')version=1
  if(s(1:pos-1)=='MISSY-NATIVE-2')version=2
  if(version==0)return
  k=pos+1;j=index(s(k:n),achar(10));if(j==0)return
  call parse_numbers(s(k:k+j-2),nums,12,ios);if(ios/=0)return
  r%nw=nums(1);r%ncy=nums(2);r%status=nums(3);r%seed=nums(4);r%ne=nums(5);r%nr=nums(6)
  r%copies=nums(7);r%threshold=nums(8);r%fault=nums(9);r%nt=nums(10);tm=nums(11);gap=nums(12)
  if(r%ne<1.or.r%ne>3.or.r%nr<1.or.r%nr>3.or.r%nw/=r%ne*r%nr+5)return
  if(r%ncy<0.or.r%ncy>45.or.r%nt<0.or.r%nt>maxtrace.or.r%status<1.or.r%status>7)return
  if(r%threshold<20.or.r%threshold>2000.or.tm<0.or.gap<0)return
  if(r%copies<1.or.r%copies>10000000.or.r%fault<0.or.r%fault>1.or.r%seed<1)return
  r%elapsed=tm/1000d0;r%maxgap=gap/1000d0;pos=k+j
  if(version==2)then
   j=index(s(pos:n),achar(10));if(j==0)return
   call parse_numbers(s(pos:pos+j-2),nums,8,ios);if(ios/=0)return
   r%scenario=nums(1);r%planned_cycles=nums(2);r%params=real(nums(3:8),c_double)/1000;pos=pos+j
   if(r%scenario<0.or.r%scenario>51.or.r%planned_cycles<1.or.r%planned_cycles>45)return
   if(r%scenario>0)then
    if(.not.parameters_valid(r%scenario,r%params))return
   end if
   j=index(s(pos:n),achar(10));if(j==0)return
   call parse_numbers(s(pos:pos+j-2),nums,r%nw,ios);if(ios/=0)return
   if(any(nums(1:r%nw)<0).or.any(nums(1:r%nw)>1))return
   r%present(1:r%nw)=nums(1:r%nw);pos=pos+j
  else
   r%y2=-1
  end if
  do i=1,r%ncy
   j=index(s(pos:n),achar(10));if(j==0)return
   call parse_numbers(s(pos:pos+j-2),nums,r%nw,ios);if(ios/=0)return
   if(any(nums(1:r%nw)<-1).or.any(nums(1:r%nw)>65535))return
   r%y(i,1:r%nw)=real(nums(1:r%nw),c_double);pos=pos+j
   if(version==2)then
    j=index(s(pos:n),achar(10));if(j==0)return
    call parse_numbers(s(pos:pos+j-2),nums,r%nw,ios);if(ios/=0)return
    if(any(nums(1:r%nw)<-1).or.any(nums(1:r%nw)>65535))return
    r%y2(i,1:r%nw)=real(nums(1:r%nw),c_double);pos=pos+j
   end if
  end do
  do i=1,r%nt
   j=index(s(pos:n),achar(10));if(j==0)return
   call parse_numbers(s(pos:pos+j-2),nums,6,ios);if(ios/=0)return
   if(nums(1)<0.or.nums(1)>tm+50)return
   if(any(nums(2:5)<-5000).or.any(nums(2:5)>125000))return
   if(nums(6)<4000.or.nums(6)>100000)return
   if(i>1)then
    if(nums(1)<=nint(r%trace(1,i-1)*1000))return
   end if
   r%trace(:,i)=real(nums(1:6),c_double)/1000;pos=pos+j
  end do
  if(pos<=n)then
   if(len_trim(s(pos:n))>0)return
  end if
  r%serial=nh+1;call analyse(r);nh=nh+1;history(nh)=r;selected=nh
  message='Native RDMLt run loaded and analysed in Fortran.'
 end subroutine
! @nativefn NR2-M02-F0018
 subroutine load_file(a,n)
  integer,intent(in)::n
  integer(c_int8_t),intent(in)::a(n)
  integer::i,j,p,namelen,extra,sz,method,flags,start,lead,record_start,record_size,entries
  integer(c_int64_t)::crc
  character(len=100)::name
  if(preview_mode)call cancel_preview()
  if(running)then
   message='Stop generation before importing files.';return
  end if
  if(n<2.or.n>input_cap)then
   message='File empty or larger than the 64 MiB native input limit.';return
  end if
  call ensure_work()
  lead=1
  do while(lead<n)
   if(index(' '//achar(9)//achar(10)//achar(13),achar(byte_at(a,lead)))==0)exit
   lead=lead+1
  end do
  if(byte_at(a,lead)==123)then
   if(n>65536)then
    message='Profile exceeds 64 KiB.';return
   end if
   do i=1,n
    work(i:i)=achar(byte_at(a,i))
   end do
   call parse_profile(work,n);return
  end if
  message='This edition imports only its own native RDMLt packages (native-run.txt entry).'
  p=1;record_start=0;record_size=0;entries=0
  do while(p+29<=n)
   if(read_int(a,p,4)/=int(z'04034B50',c_int64_t))exit
   flags=int(read_int(a,p+6,2));method=int(read_int(a,p+8,2));crc=read_int(a,p+14,4)
   sz=int(read_int(a,p+18,4));namelen=int(read_int(a,p+26,2));extra=int(read_int(a,p+28,2));start=p+30+namelen+extra
   if(flags/=0.or.(method/=0.and.method/=8).or.sz<0.or.namelen>100.or.start<1)return
   if(int(start,c_int64_t)+int(sz,c_int64_t)>int(n,c_int64_t)+1)return
   if(method==8)then
    ! Compressed companion entries are not needed to restore a run; native-run.txt is always stored.
    entries=entries+1;p=start+sz;cycle
   end if
   if(read_int(a,p+22,4)/=int(sz,c_int64_t))return
   name=''
   do j=1,namelen
    name(j:j)=achar(byte_at(a,p+29+j))
   end do
   if(sz>len(work))return
   do j=1,sz
    work(j:j)=achar(byte_at(a,start+j-1))
   end do
   if(crc_text(work,sz)/=crc)then
    message='CRC mismatch: corrupted package entry rejected.';return
   end if
   entries=entries+1
   if(trim(name)=='native-run.txt')then
    if(record_start/=0)return
    record_start=start;record_size=sz
   end if
   p=start+sz
  end do
  if(entries<2.or.record_start==0)return
  if(p+3>n)return
  if(read_int(a,p,4)/=int(z'02014B50',c_int64_t))return
  do j=1,record_size
   work(j:j)=achar(byte_at(a,record_start+j-1))
  end do
  call import_record(work,record_size)
 end subroutine
end module
