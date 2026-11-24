! SPDX-License-Identifier: GPL-3.0-only
! Full RDMLt package writer for the native Fortran edition. Produces the same package layout as the
! MISSY R2 workbench: RDML 1.3 core, rdmlt_data.xml companion (RDMLt 1.0-draft), telemetry TSVs,
! per-scan acquisition TSVs, simulator record, run input, chemistry snapshots and analysis files.
! Every manifest entry carries SHA-256 and byte count, as MISSY verifies them on import.
! All data are simulated. The detector integration statistics are derived from the recorded
! per-read counts with an independent random stream; they do not change any recorded value.
module native_rdmlt
 use iso_c_binding
 use, intrinsic :: ieee_arithmetic
 use r2_math, only: rng_gauss
 use stdlib_browser_stats, only: stdlib_mean, stdlib_var
 use native_scenarios
 use native_model
 use native_io
 use native_codec
 implicit none
 private
 public :: export_run, export_all_runs, sample_id, well_name, run_tag
 integer, parameter :: maxf = 200, periods = 100, spp = 8
 character(len=1), parameter :: tb = achar(9)
 character(len=40), save :: fname(maxf), fid(maxf), fmedia(maxf)
 character(len=64), save :: fsha(maxf)
 integer, save :: nf = 0, foff(maxf), fusz(maxf), fcsz(maxf), fmeth(maxf)
 integer(c_int64_t), save :: fcrc(maxf)
 logical, save :: overflow = .false.
 ! Derived detector integration per read: on, off, sumsq, min, max, saturated, ref on, ref off.
 integer(c_int64_t), save :: rs(8,45,maxw,2)
 logical, save :: rok(45,maxw,2)
 integer, save :: rseq(45,maxw,2)
 real(c_double), save :: rt0(45,maxw,2)
 character(len=*),parameter::sp1= &
   "{""assay"":""DNA-target (generic hydrolysis probe) / YY-target (second channel)"",""auth" // &
   "or"":"""",""controls"":[{""cqHi"":"""",""cqLo"":"""",""expect"":""negative"",""id"":""DN" // &
   "A-target:NTC"",""match"":""/-NTC$/i"",""matchBy"":""name"",""minPerRun"":1,""minReplicat" // &
   "es"":1,""name"":""NTC"",""onFail"":""reject"",""targets"":""/^(?:DNA-target)$/i""},{""cq" // &
   "Hi"":"""",""cqLo"":"""",""expect"":""negative"",""id"":""DNA-target:NAC"",""match"":""/-" // &
   "NAC$/i"",""matchBy"":""name"",""minPerRun"":1,""minReplicates"":1,""name"":""NAC"",""onF" // &
   "ail"":""reject"",""targets"":""/^(?:DNA-target)$/i""},{""cqHi"":"""",""cqLo"":"""",""exp" // &
   "ect"":""positive"",""id"":""DNA-target:PC"",""match"":""/-PC$/i"",""matchBy"":""name"",""m" // &
   "inPerRun"":1,""minReplicates"":1,""name"":""PC"",""onFail"":""reject"",""targets"":""/^(" // &
   "?:DNA-target)$/i""},{""cqHi"":"""",""cqLo"":"""",""expect"":""positive"",""id"":""DNA-ta" // &
   "rget:LOD"",""match"":""/-PC-LOD-\\d+$/i"",""matchBy"":""name"",""minPerRun"":1,""minRepl" // &
   "icates"":1,""name"":""PC at LOD"",""onFail"":""review"",""targets"":""/^(?:DNA-target)$/" // &
   "i""}],""effective"":"""",""evaluationMode"":""automatic-with-evidence"",""fileStem"":"""
 character(len=*),parameter::sp2= &
   "_MISSY"",""graphs"":{""showCutoffs"":true,""showLate"":true,""showSdLimit"":true},""ic"":" // &
   "{""maxShift"":"""",""missingOutcome"":""Not interpreted"",""referenceCq"":"""",""shiftOu" // &
   "tcome"":""Inconclusive""},""lab"":{""analyst"":"""",""name"":""""},""name"":""Native gen" // &
   "eric probe detection (MISSY Fortran edition)"",""notes"":""Simulated run. Native Fortran" // &
   " edition procedure "
 character(len=*),parameter::sp3= &
   ". Teaching profile; not validated.\n"",""outcomes"":{""Control fail"":{""colour"":""#e11" // &
   "d48"",""label"":""Control failed"",""text"":""{sample} / {target}: control FAILED " // &
   achar(226)//achar(128)//achar(148) // &
   " {reason}""},""Control pass"":{""colour"":""#16a34a"",""label"":""Control accepted"",""t" // &
   "ext"":""{sample} / {target}: control accepted""},""Inconclusive"":{""colour"":""#d97706""," // &
   """label"":""Inconclusive"",""text"":""{target} inconclusive: {reason}""},""Invalid"":{""c" // &
   "olour"":""#111827"",""label"":""Invalid result"",""text"":""{target}: invalid " // &
   achar(226)//achar(128)//achar(148) // &
   " {reason}""},""Invalid run"":{""colour"":""#9f1239"",""label"":""Invalid run"",""text"":" // &
   """Run not accepted " // &
   achar(226)//achar(128)//achar(148) // &
   " {reason}""},""Negative"":{""colour"":""#64748b"",""label"":""Not detected"",""text"":""{" // &
   "target} not detected ({n} replicates; Cq cut-off {cutoff})""},""Not interpreted"":{""col" // &
   "our"":""#cbd5e1"",""label"":""Not interpreted"",""text"":""{target}: not interpreted""}," // &
   """Positive"":{""colour"":""#dc2626"",""label"":""Detected"",""text"":""{target} detected" // &
   " (mean Cq {cq}; {det}/{n} replicates)""},""Repeat"":{""colour"":""#7c3aed"",""label"":""R" // &
   "epeat analysis"",""text"":""{target}: repeat " // &
   achar(226)//achar(128)//achar(148) // &
   " {reason}""},""Standard"":{""colour"":""#0891b2"",""label"":""Standard"",""text"":""{sam" // &
   "ple} / {target}: standard""}},""profileFamily"":""native-fortran"",""replicates"":{""max" // &
   "Sd"":"""",""min"":1,""partialOutcome"":""Repeat"",""positiveMin"":1,""sdOutcome"":""Repe" // &
   "at""},""reviewOnly"":true,""run"":{""calibration"":""off"",""colourCompensation"":""off""," // &
   """controlsFail"":""reject"",""controlsMissing"":""reject"",""editAction"":""review"",""e" // &
   "ditDelayHours"":72,""effMax"":"""",""effMin"":"""",""integrity"":""reject"",""invalidRun" // &
   "Overrides"":true,""maxZoneSpread"":"""",""minLogs"":"""",""minR2"":"""",""ntcGap"":""""," // &
   """ntcGapAction"":""review"",""referencePolicy"":""none"",""runState"":""reject"",""stdCu" // &
   "rve"":""off"",""zoneAction"":""review""},""sample_id"":{""flags"":""i"",""pattern"":""""}" // &
   ",""schema"":""qpcr-sop-profile/1"",""sourceWorkbenchSop"":{""format"":""missy-native-sop" // &
   "/1"",""id"":"""
 character(len=*),parameter::sp4= &
   """},""targets"":[{""cqLate"":"""",""cqMax"":40,""kind"":""target"",""lateOutcome"":""Inc" // &
   "onclusive"",""match"":""/^DNA-target$/i"",""quantMin"":""""},{""cqLate"":"""",""cqMax"":" // &
   "40,""kind"":""target"",""lateOutcome"":""Inconclusive"",""match"":""/^YY-target$/i"",""q" // &
   "uantMin"":""""}],""version"":""1.2""}"
contains
! ---------------------------------------------------------------- text helpers
! @nativefn NR2-M06-F0001
 function i8t(v) result(s)
  integer(c_int64_t),intent(in)::v
  character(len=24)::s
  character(len=24)::rev
  integer(c_int64_t)::q
  integer::n,k
  q=abs(v);n=0;rev=''
  do
   n=n+1;rev(n:n)=achar(48+int(mod(q,10_c_int64_t)));q=q/10;if(q==0)exit
  end do
  s='';k=1
  if(v<0)then
   s(1:1)='-';k=2
  end if
  do while(n>0)
   s(k:k)=rev(n:n);k=k+1;n=n-1
  end do
 end function
! @nativefn NR2-M06-F0002
 function it(v) result(s)
  integer,intent(in)::v
  character(len=24)::s
  s=integer_text(v)
 end function
! @nativefn NR2-M06-F0003
 function gn(v) result(s)
  ! Six significant digits, plain or exponent notation; valid in JSON, TSV and xs:double.
  real(c_double),intent(in)::v
  character(len=40)::s
  real(c_double)::a,m
  integer::e,dec,k
  integer(c_int64_t)::q
  character(len=6)::digs
  s='NA';if(.not.ieee_is_finite(v))return
  if(v==0)then
   s='0';return
  end if
  a=abs(v);e=floor(log10(a))
  if(e>=-3.and.e<=8)then
   dec=max(0,min(9,5-e));s=real_text(v,dec)
   if(dec>0)then
    k=len_trim(s)
    do while(s(k:k)=='0')
     s(k:k)=' ';k=k-1
    end do
    if(s(k:k)=='.')s(k:k)=' '
   end if
   return
  end if
  m=a/10d0**e;q=nint(m*1d5,c_int64_t)
  if(q>=1000000_c_int64_t)then
   q=q/10;e=e+1
  end if
  digs=i8t(q)
  s=digs(1:1)//'.'//digs(2:6)//'e'//trim(it(e))
  if(v<0)s='-'//trim(s)
 end function
! @nativefn NR2-M06-F0004
 function ft(v,d) result(s)
  real(c_double),intent(in)::v
  integer,intent(in)::d
  character(len=40)::s
  s='NA';if(.not.ieee_is_finite(v))return
  if(abs(v)*10d0**d<2d9)then
   s=real_text(v,d)
  else
   s=gn(v)
  end if
 end function
! @nativefn NR2-M06-F0005
 function jn(v,d) result(s)
  real(c_double),intent(in)::v
  integer,intent(in)::d
  character(len=40)::s
  s='null';if(ieee_is_finite(v))s=ft(v,d)
 end function
! @nativefn NR2-M06-F0006
 function tf(x) result(s)
  real(c_float),intent(in)::x
  character(len=40)::s
  s=gn(real(x,c_double))
 end function
! @nativefn NR2-M06-F0007
 function lg(b) result(s)
  logical,intent(in)::b
  character(len=5)::s
  s='false';if(b)s='true'
 end function
! @nativefn NR2-M06-F0008
 function esc(s) result(o)
  ! XML and JSON-safe text: escapes the five XML specials and quotes/backslashes as entities.
  character(len=*),intent(in)::s
  character(len=:),allocatable::o
  integer::i
  o=''
  do i=1,len_trim(s)
   select case(s(i:i))
   case('&');o=o//'&amp;'
   case('<');o=o//'&lt;'
   case('>');o=o//'&gt;'
   case('"');o=o//'&quot;'
   case('\');o=o//'/'
   case default;o=o//s(i:i)
   end select
  end do
 end function
! ---------------------------------------------------------------- run facts
! @nativefn NR2-M06-F0010
 function role_of(r,w) result(s)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=8)::s
  integer::n
  n=r%ne*r%nr;s='unknown'
  if(w==n+1.or.w>=n+4)s='positive'
  if(w==n+2)s='ntc'
  if(w==n+3)s='nac'
 end function
! @nativefn NR2-M06-F0011
 function rdml_type(r,w) result(s)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=4)::s
  select case(trim(role_of(r,w)))
  case('unknown');s='unkn'
  case('positive');s='pos'
  case('ntc');s='ntc'
  case default;s='nac'
  end select
 end function
! @nativefn NR2-M06-F0012
 function run_tag(r) result(s)
  ! Run identifier that stays unique across sessions: start date and time plus the run number,
  ! e.g. RUN20261124-231005-02 (falls back to RUN02 when the start time is unknown).
  type(run_t),intent(in)::r
  character(len=32)::s
  character(len=2)::nn
  nn=achar(48+mod(r%serial/10,10))//achar(48+mod(r%serial,10))
  if(len_trim(r%started)>=19)then
   s='RUN'//r%started(1:4)//r%started(6:7)//r%started(9:10)//'-'//r%started(12:13)//r%started(15:16)//r%started(18:19)//'-'//nn
  else
   s='RUN'//nn
  end if
 end function
 function sample_id(r,w) result(s)
  ! Sample names start with a common root per type so they can be grouped across runs:
  ! S1-E<extraction>_<run>, PC_<run>, NTC_<run>, EB_<run> (extraction blank), LOD<n>_<run>.
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=48)::s
  integer::n
  n=r%ne*r%nr
  if(w<=n)then
   s='S1-E'//trim(it((w-1)/r%nr+1))//'_'//trim(run_tag(r))
  else if(w==n+1)then
   s='PC_'//trim(run_tag(r))
  else if(w==n+2)then
   s='NTC_'//trim(run_tag(r))
  else if(w==n+3)then
   s='EB_'//trim(run_tag(r))
  else
   s='LOD'//trim(it(w-n-3))//'_'//trim(run_tag(r))
  end if
 end function
 function well_name(r,w) result(s)
  ! Unique name of one reaction: the sample name with the PCR replicate for samples
  ! (S1-E<e>-P<p>_<run>); controls are one reaction each and keep their sample name.
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=52)::s
  integer::n
  n=r%ne*r%nr
  if(w<=n)then
   s='S1-E'//trim(it((w-1)/r%nr+1))//'-P'//trim(it(mod(w-1,r%nr)+1))//'_'//trim(run_tag(r))
  else
   s=sample_id(r,w)
  end if
 end function
! @nativefn NR2-M06-F0013
 function sample_text(r,w) result(s)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=200)::s
  integer::n
  n=r%ne*r%nr
  if(w<=n)then
   s='Simulated generic DNA sample 1, extraction '//trim(it((w-1)/r%nr+1))//', nominal '// &
    trim(it(r%copies))//' starting copies per reaction'
  else if(w==n+1)then
   s='Positive control, nominal 10000 copies'
  else if(w==n+2)then
   s='No-template control'
  else if(w==n+3)then
   s='No-amplification control (template-free reaction)'
  else
   s='Positive control at the limit of detection, nominal 15 copies'
  end if
 end function
! @nativefn NR2-M06-F0014
 real(c_double) function nominal_copies(r,w,c) result(q)
  type(run_t),intent(in)::r
  integer,intent(in)::w,c
  integer::n
  n=r%ne*r%nr;q=real(r%copies,c_double)
  if(w==n+1)q=10000
  if(w==n+2.or.w==n+3)q=0
  if(w>=n+4)q=15
  if(c==2)then
   q=0
   if(r%scenario==48.or.r%scenario==49.or.r%scenario==51)then
    if(w<=n+1)q=10000
   end if
  end if
 end function
! @nativefn NR2-M06-F0015
 integer function pos_of(r,w) result(p)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  p=recorded_position(r%scenario,r%params,w,r%ne,r%nr)
 end function
! @nativefn NR2-M06-F0016
 function pos_label(p) result(s)
  integer,intent(in)::p
  character(len=8)::s
  s=achar(65+p/24)//trim(it(mod(p,24)+1))
 end function
! @nativefn NR2-M06-F0017
 real(c_double) function ann_of(r) result(v)
  type(run_t),intent(in)::r
  v=r%ann_t
 end function
! @nativefn NR2-M06-F0018
 real(c_double) function lid_of(r) result(v)
  type(run_t),intent(in)::r
  v=r%lid_t
 end function
! @nativefn NR2-M06-F0019
 real(c_double) function thr_of(r) result(v)
  type(run_t),intent(in)::r
  v=real(r%threshold,c_double);if(r%scenario==16)v=r%params(1)
 end function
! @nativefn NR2-M06-F0020
 function outcome(r) result(s)
  type(run_t),intent(in)::r
  character(len=24)::s
  select case(r%status)
  case(1);s='completed'
  case(2);s='stopped by operator'
  case(3);s='monitor stop'
  case(4);s='time limit'
  case(5);s='host crash'
  case(6);s='setup rejected'
  case(7);s='interlock stop'
  case default;s='incomplete'
  end select
 end function
! @nativefn NR2-M06-F0021
 real(c_double) function baseline(r,w,c) result(b)
  type(run_t),intent(in)::r
  integer,intent(in)::w,c
  real(c_double)::y(45),acc
  integer::k,n
  if(c==1)then
   y=r%y(:,w)
  else
   y=r%y2(:,w)
  end if
  acc=0;n=0
  do k=1,min(5,r%ncy)
   if(y(k)>=0)then
    acc=acc+y(k);n=n+1
   end if
  end do
  b=ieee_value(b,ieee_quiet_nan);if(n>0)b=acc/n
 end function
! @nativefn NR2-M06-F0022
 real(c_double) function yv(r,k,w,c) result(v)
  type(run_t),intent(in)::r
  integer,intent(in)::k,w,c
  if(c==1)then
   v=r%y(k,w)
  else
   v=r%y2(k,w)
  end if
 end function
! @nativefn NR2-M06-F0023
 real(c_double) function cqv(r,w,c) result(v)
  type(run_t),intent(in)::r
  integer,intent(in)::w,c
  if(c==1)then
   v=r%cq(w)
  else
   v=r%cq2(w)
  end if
 end function
! @nativefn NR2-M06-F0024
 real(c_double) function tel(r,col,i) result(v)
  type(run_t),intent(in)::r
  integer,intent(in)::col,i
  v=real(r%tele(col,i),c_double)
 end function
! @nativefn NR2-M06-F0026B
 integer function hslot(r) result(k)
  ! History slot holding the 10 Hz record of r, or 0 (imported runs keep only the 1 Hz trace).
  type(run_t),intent(in)::r
  k=0
  if(r%hf_slot>=1.and.r%hf_slot<=maxruns)then
   if(allocated(hfr(r%hf_slot)%v))k=r%hf_slot
  end if
 end function
! @nativefn NR2-M06-F0026C
 integer function hn(r) result(n)
  type(run_t),intent(in)::r
  integer::k
  k=hslot(r);n=r%ntel_rows
  if(k>0)n=hfr(k)%n
 end function
! @nativefn NR2-M06-F0026D
 real(c_double) function hv(r,col,i) result(v)
  ! 10 Hz value (columns 1-19 match the 1 Hz trace; 20 = detector busy).
  type(run_t),intent(in)::r
  integer,intent(in)::col,i
  integer::k,c
  k=hslot(r)
  if(k>0)then
   v=real(hfr(k)%v(col,i),c_double)
  else
   c=col;if(col==20)c=27
   v=real(r%tele(c,i),c_double)
  end if
 end function
! @nativefn NR2-M06-F0026E
 subroutine prog_step(r,k,stage,step,temp,dur,rd)
  ! Programmed step k (1-based) of the run's protocol: holds first, then the two cycling steps.
  type(run_t),intent(in)::r
  integer,intent(in)::k
  integer,intent(out)::stage,step
  real(c_double),intent(out)::temp,dur
  logical,intent(out)::rd
  rd=.false.;step=0
  if(k<=r%nhold)then
   stage=k-1;temp=r%hold_t(k);dur=r%hold_s(k)
  else if(k==r%nhold+1)then
   stage=r%nhold;temp=95;dur=r%den_s
  else
   stage=r%nhold;step=1;temp=r%ann_t;dur=r%ann_hold;rd=.true.
  end if
 end subroutine
! @nativefn NR2-M06-F0026F
 function run_date(r) result(s)
  type(run_t),intent(in)::r
  character(len=10)::s
  s='';if(len_trim(r%started)>=10)s=r%started(1:10)
 end function
! @nativefn NR2-M06-F0026G
 function jstr(s) result(o)
  ! JSON string or null.
  character(len=*),intent(in)::s
  character(len=:),allocatable::o
  if(len_trim(s)==0)then
   o='null'
  else
   o='"'//esc(s)//'"'
  end if
 end function
! @nativefn NR2-M06-F0026H
 subroutine base_fit(r,w,c,b0,b1,sd)
  ! Least-squares line over the native baseline window (cycles 1-5) and its residual SD.
  type(run_t),intent(in)::r
  integer,intent(in)::w,c
  real(c_double),intent(out)::b0,b1,sd
  real(c_double)::x(5),y(5),mx,my,sxx,sxy
  integer::k,n
  n=0
  do k=1,min(5,r%ncy)
   if(yv(r,k,w,c)>=0)then
    n=n+1;x(n)=k;y(n)=yv(r,k,w,c)
   end if
  end do
  b0=ieee_value(1d0,ieee_quiet_nan);b1=b0;sd=b0;if(n<2)return
  mx=sum(x(1:n))/n;my=sum(y(1:n))/n;sxx=sum((x(1:n)-mx)**2);sxy=sum((x(1:n)-mx)*(y(1:n)-my))
  b1=0;if(sxx>0)b1=sxy/sxx
  b0=my-b1*mx;sd=sqrt(sum((y(1:n)-b0-b1*x(1:n))**2)/max(1,n-2))
 end subroutine
! @nativefn NR2-M06-F0026I
 integer(c_int64_t) function crc_bytes(a,n) result(crc)
  integer(c_int8_t),intent(in)::a(*)
  integer,intent(in)::n
  integer(c_int64_t)::c
  integer::i,j
  c=4294967295_c_int64_t
  do i=1,n
   if(crc_ready)then
    c=ieor(crc_table(iand(ieor(c,int(iand(int(a(i)),255),c_int64_t)),255_c_int64_t)),shiftr(c,8))
   else
    c=ieor(c,int(iand(int(a(i)),255),c_int64_t))
    do j=1,8
     if(iand(c,1_c_int64_t)/=0)then
      c=ieor(shiftr(c,1),3988292384_c_int64_t)
     else
      c=shiftr(c,1)
     end if
    end do
   end if
  end do
  crc=ieor(c,4294967295_c_int64_t)
 end function
! @nativefn NR2-M06-F0025
 function state_name(code) result(s)
  integer,intent(in)::code
  character(len=12)::s
  select case(code)
  case(0);s='SELFTEST'
  case(1);s='LID_PREHEAT'
  case(2);s='RAMP'
  case(3);s='HOLD'
  case(4);s='DONE'
  case default;s='IDLE'
  end select
 end function
! @nativefn NR2-M06-F0026
 integer function severity_code(code) result(v)
  integer,intent(in)::code
  v=0
  if(code==ev_time_limit)v=1
  if(code==ev_monitor_stop.or.code==ev_interlock.or.code==ev_host_crash.or.code==ev_run_aborted.or.code==ev_setup_rejected)v=2
 end function
! ---------------------------------------------------------------- detector integration statistics
! @nativefn NR2-M06-F0030
 subroutine compute_reads(r)
  ! Chopped-illumination record of each read: periods x 8 LED-on and LED-off detector samples.
  ! Mean(on - off) equals the recorded count exactly; the stream is seeded per read and is
  ! independent of every model stream, so no recorded value changes.
  type(run_t),intent(in)::r
  integer::k,w,c,i,np,j,seq
  real(c_double)::y,st,e(periods*spp),o,net,soff,sq,mn,mx,light,ron,roff,emean
  rok=.false.;rs=0;rseq=0;rt0=0;seq=100
  do k=1,r%ncy
   np=count(r%present(1:r%nw)/=0)
   do c=1,2
    j=0
    do w=1,r%nw
     if(r%present(w)==0)cycle
     j=j+1;seq=seq+2
     rt0(k,w,c)=r%acq_t(k)+0.02d0+0.1d0*real((c-1)*np+j-1,c_double);rseq(k,w,c)=seq
     y=yv(r,k,w,c);if(y<0)cycle
     st=real(r%seed,c_double)*7919d0+real(k,c_double)*104729d0+real(w,c_double)*1299709d0+real(c,c_double)*15485863d0
     do i=1,periods*spp
      e(i)=1.5d0*rng_gauss(st)
     end do
     emean=sum(e)/size(e);e=e-emean
     soff=0;sq=0;mn=huge(1d0);mx=-huge(1d0)
     do i=1,periods*spp
      o=200+1.2d0*rng_gauss(st);net=y+e(i)
      soff=soff+o;sq=sq+net*net;mn=min(mn,net);mx=max(mx,net)
     end do
     light=real(r%lightf(k,w),c_double)
     ron=(200+3000*light)*periods*spp+30*rng_gauss(st);roff=200d0*periods*spp+30*rng_gauss(st)
     rs(2,k,w,c)=nint(soff,c_int64_t);rs(1,k,w,c)=rs(2,k,w,c)+nint(y,c_int64_t)*periods*spp
     rs(3,k,w,c)=nint(sq,c_int64_t);rs(4,k,w,c)=nint(mn,c_int64_t);rs(5,k,w,c)=nint(mx,c_int64_t)
     rs(6,k,w,c)=0;if(y>=65535)rs(6,k,w,c)=periods*spp
     rs(7,k,w,c)=nint(ron,c_int64_t);rs(8,k,w,c)=nint(roff,c_int64_t)
     rok(k,w,c)=.true.
    end do
   end do
  end do
 end subroutine
! @nativefn NR2-M06-F0031
 real(c_double) function zone_temp(r,k,w) result(v)
  type(run_t),intent(in)::r
  integer,intent(in)::k,w
  v=r%acq_zone(1+mod(mod(pos_of(r,w),24),2),k)
 end function
! ---------------------------------------------------------------- ZIP with DEFLATE
! @nativefn NR2-M06-F0040
 subroutine put_entry(name,id,media,compress)
  character(len=*),intent(in)::name,id,media
  logical,intent(in)::compress
  integer::i,csz,meth
  logical::ok
  if(overflow)return
  if(nf>=maxf)then
   overflow=.true.;message='Too many package entries.';return
  end if
  nf=nf+1;fname(nf)=name;fid(nf)=id;fmedia(nf)=media
  fsha(nf)=sha256_hex(work,used);fcrc(nf)=crc_text(work,used);fusz(nf)=used
  meth=0;csz=used
  if(compress.and.used>64)then
   call deflate_fixed(work,used,ok)
   if(ok.and.zlen<used)then
    meth=8;csz=zlen
   end if
  end if
  call ensure_output(output_size+csz+len_trim(name)+200)
  if(output_size+csz+len_trim(name)+200>size(output))then
   overflow=.true.;message='Export exceeds the native output buffer.';return
  end if
  fmeth(nf)=meth;fcsz(nf)=csz;foff(nf)=output_size
  call put_int(int(z'04034B50',c_int64_t),4);call put_int(20_c_int64_t,2);call put_int(0_c_int64_t,2)
  call put_int(int(meth,c_int64_t),2);call put_int(2162688_c_int64_t,4);call put_int(fcrc(nf),4)
  call put_int(int(csz,c_int64_t),4);call put_int(int(used,c_int64_t),4)
  call put_int(int(len_trim(name),c_int64_t),2);call put_int(0_c_int64_t,2)
  do i=1,len_trim(name)
   call put_byte(iachar(name(i:i)))
  end do
  if(meth==8)then
   do i=1,zlen
    call put_byte(int(zbuf(i)))
   end do
  else
   do i=1,used
    call put_byte(iachar(work(i:i)))
   end do
  end if
 end subroutine
! @nativefn NR2-M06-F0041
 subroutine put_directory()
  integer::start,i,j,sz
  start=output_size
  do i=1,nf
   call put_int(int(z'02014B50',c_int64_t),4);call put_int(20_c_int64_t,2);call put_int(20_c_int64_t,2)
   call put_int(0_c_int64_t,2);call put_int(int(fmeth(i),c_int64_t),2);call put_int(2162688_c_int64_t,4)
   call put_int(fcrc(i),4);call put_int(int(fcsz(i),c_int64_t),4);call put_int(int(fusz(i),c_int64_t),4)
   call put_int(int(len_trim(fname(i)),c_int64_t),2)
   call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2)
   call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,4);call put_int(int(foff(i),c_int64_t),4)
   do j=1,len_trim(fname(i))
    call put_byte(iachar(fname(i)(j:j)))
   end do
  end do
  sz=output_size-start;call put_int(int(z'06054B50',c_int64_t),4)
  call put_int(0_c_int64_t,2);call put_int(0_c_int64_t,2);call put_int(int(nf,c_int64_t),2)
  call put_int(int(nf,c_int64_t),2);call put_int(int(sz,c_int64_t),4);call put_int(int(start,c_int64_t),4)
  call put_int(0_c_int64_t,2)
 end subroutine
! ---------------------------------------------------------------- RDML 1.3 core
! @nativefn NR2-M06-F0050
 subroutine write_core(r)
  type(run_t),intent(in)::r
  integer::w,c,k,n,e,stg,stp
  real(c_double)::temp,dur
  logical::rd
  character(len=10)::tar
  used=0;n=r%ne*r%nr
  call add('<?xml version="1.0" encoding="UTF-8"?>');call newline()
  call add('<rdml version="1.3" xmlns:rdml="http://www.rdml.org" xmlns="http://www.rdml.org">');call newline()
  if(len_trim(r%started)>0)then
   call add('<dateMade>'//trim(r%started)//'</dateMade>');call newline()
  end if
  call add('<id><publisher>MISSY native Fortran edition (simulated instrument)</publisher><serialNumber>NATIVE-'// &
   trim(it(r%serial))//'</serialNumber></id>');call newline()
  call add('<documentation id="origin"><text>Simulated data from the MISSY native Fortran engine, seed '//trim(it(r%seed))// &
   '. Teaching model; research use only; not from a real instrument. Fluorescence = mean LED-on minus LED-off detector'// &
   ' counts, no baseline subtraction, no normalisation. Generic DNA hydrolysis-probe workflow with a second detection'// &
   ' channel; no legacy SOP equivalence is claimed.</text></documentation>');call newline()
  call add('<dye id="FAM"><description>FAM reporter of a hydrolysis probe, detection channel 1 (simulated)</description></dye>')
  call newline()
  call add('<dye id="YY"><description>Yakima Yellow reporter, detection channel 2 (simulated)</description></dye>');call newline()
  do e=1,r%ne
   w=(e-1)*r%nr+1
   call add('<sample id="'//trim(sample_id(r,w))//'"><description>'//trim(sample_text(r,w))//'</description><type>unkn</type></sample>')
   call newline()
  end do
  do w=n+1,r%nw
   call add('<sample id="'//trim(sample_id(r,w))//'"><description>'//trim(sample_text(r,w))//'</description><type>'// &
    trim(rdml_type(r,w))//'</type></sample>');call newline()
  end do
  call add('<target id="DNA-target"><description>Generic DNA target, hydrolysis probe, channel 1 (native model)</description>'// &
   '<type>toi</type><dyeId id="FAM"/></target>');call newline()
  call add('<target id="YY-target"><description>Second detection channel, reference assay (native model)</description>'// &
   '<type>ref</type><dyeId id="YY"/></target>');call newline()
  call add('<thermalCyclingConditions id="protocol"><description>'//trim(prog_name(r))//'</description><lidTemperature>'// &
   trim(gn(lid_of(r)))//'</lidTemperature>');call newline()
  do k=1,r%nhold+2
   call prog_step(r,k,stg,stp,temp,dur,rd)
   if(rd)then
    call add('<step><nr>'//trim(it(k))//'</nr><description>fluorescence read at the end of the hold</description><temperature>'// &
     '<temperature>'//trim(gn(temp))//'</temperature><duration>'//trim(gn(dur))//'</duration></temperature></step>')
   else
    call add('<step><nr>'//trim(it(k))//'</nr><temperature><temperature>'//trim(gn(temp))//'</temperature><duration>'// &
     trim(gn(dur))//'</duration></temperature></step>')
   end if
   call newline()
  end do
  call add('<step><nr>'//trim(it(r%nhold+3))//'</nr><loop><goto>'//trim(it(r%nhold+1))//'</goto><repeat>'// &
   trim(it(r%planned_cycles-1))//'</repeat></loop></step>');call newline()
  call add('</thermalCyclingConditions>');call newline()
  call add('<experiment id="Native-'//trim(it(r%serial))//'"><description>Simulated run, outcome: '//trim(outcome(r))// &
   '</description>');call newline()
  call add('<run id="Native-'//trim(it(r%serial))//'"><description>Cq settings: DNA-target and YY-target: threshold '// &
   trim(gn(thr_of(r)))//' counts above the mean of cycles 1 to 5</description><instrument>MISSY R2 simulated instrument'// &
   ' (native Fortran edition), serial NATIVE-'//trim(it(r%serial))//', 384 wells</instrument>');call newline()
  call add('<dataCollectionSoftware><name>MISSY native Fortran edition</name><version>RDMLt export 3</version>'// &
   '</dataCollectionSoftware>');call newline()
  call add('<backgroundDeterminationMethod>other</backgroundDeterminationMethod>');call newline()
  call add('<cqDetectionMethod>other</cqDetectionMethod>');call newline()
  call add('<thermalCyclingConditions id="protocol"/><pcrFormat><rows>16</rows><columns>24</columns><rowLabel>ABC</rowLabel>'// &
   '<columnLabel>123</columnLabel></pcrFormat>')
  if(len_trim(r%started)>0)call add('<runDate>'//trim(r%started)//'</runDate>')
  call newline()
  do w=1,r%nw
   if(r%present(w)==0)cycle
   call add('<react id="'//trim(it(pos_of(r,w)+1))//'"><sample id="'//trim(sample_id(r,w))//'"/>')
   do c=1,2
    tar='DNA-target';if(c==2)tar='YY-target'
    call add('<data><tar id="'//trim(tar)//'"/>')
    if(cqv(r,w,c)>0)call add('<cq>'//trim(real_text(cqv(r,w,c),4))//'</cq>')
    do k=1,r%ncy
     if(yv(r,k,w,c)<0)cycle
     call add('<adp><cyc>'//trim(it(k))//'</cyc><fluor>'//trim(it(nint(yv(r,k,w,c))))//'</fluor></adp>')
    end do
    call add('</data>')
   end do
   call add('</react>');call newline()
  end do
  call add('</run></experiment></rdml>');call newline()
 end subroutine
! ---------------------------------------------------------------- provenance (unchanged content)
! @nativefn NR2-M06-F0051
 subroutine write_provenance(r)
  type(run_t),intent(in)::r
  integer::j
  used=0;call add('{"format":"missy-native-provenance/1","engine":"fortran","simulated":true,'// &
   '"monitorGapLimitC":2,"monitorPersistenceMs":2000,"injectedSecondaryBiasC":')
  call add(trim(it(r%fault*5))//',"maxSensorGapC":'//trim(real_text(r%maxgap,3))// &
   ',"status":'//trim(it(r%status))//',"seed":'//trim(it(r%seed))// &
   ',"extractions":'//trim(it(r%ne))//',"replicates":'//trim(it(r%nr))// &
   ',"startingCopies":'//trim(it(r%copies))//',"netThreshold":'//trim(it(r%threshold))// &
   ',"plannedCycles":'//trim(it(r%planned_cycles))//',"acquisitionCyclesReached":'//trim(it(r%ncy))// &
   ',"elapsedSeconds":'//trim(real_text(r%elapsed,3))// &
   ',"commonCause":"shared virtual plant, browser, computer","checkpointRecovery":false,"scenarioCode":'// &
   trim(it(r%scenario))//',"deliberateScenario":')
  if(r%scenario==0)then
   call add('false')
  else
   call add('true,"scenarioId":"'//trim(scenario_ids(r%scenario))//'","kind":"'//trim(scenario_kinds(r%scenario))// &
    '","title":"'//esc(scenario_titles(r%scenario))//'","parameters":[')
   do j=1,6
    call add('{"name":"'//esc(parameter_names(j,r%scenario))//'","value":'//trim(real_text(r%params(j),3))//'}')
    if(j<6)call add(',')
   end do
   call add(']')
  end if
  call add(',"limitations":"Generic DNA probe workflow; experimental RDMLt; no claim of legacy SOP equivalence"}')
 end subroutine
! ---------------------------------------------------------------- source/run-input.json
! @nativefn NR2-M06-F0052
 subroutine write_run_input(r)
  ! R2 run-input layout: identity, outcome, scenario, plate (384 positions), protocol, controller,
  ! simulation setup. Values come from the native run; keys the native model has no value for are null.
  type(run_t),intent(in)::r
  integer::p,w,n,j,wp(0:383),k,stage,step
  real(c_double)::temp,dur,a(14)
  logical::rd
  used=0;n=r%ne*r%nr;wp=0
  do w=1,r%nw
   if(r%present(w)/=0)wp(pos_of(r,w))=w
  end do
  call add('{"format":"missy-native-run-input/2","runId":"Native-'//trim(it(r%serial))//'","serial":"NATIVE-'// &
   trim(it(r%serial))//'","startedAt":'//jstr(r%started)//',"finishedAt":'//jstr(r%finished)//',')
  call add('"outcome":"'//trim(outcome(r))//'","numericalEngine":"fortran","preRunTask":null,"injectedFault":')
  if(r%scenario==0)then
   call add('null')
  else
   call add('{"id":"'//trim(scenario_ids(r%scenario))//'","kind":"'//trim(scenario_kinds(r%scenario))//'","title":"'// &
    esc(scenario_titles(r%scenario))//'","note":"Deliberate native training scenario '//trim(it(r%scenario))// &
    '; parameters recorded in source/fault-provenance.json","code":'//trim(it(r%scenario))//',"parameters":[')
   do j=1,6
    call add('{"name":"'//esc(parameter_names(j,r%scenario))//'","value":'//trim(gn(r%params(j)))//'}')
    if(j<6)call add(',')
   end do
   call add(']}')
  end if
  call add(',"batch":null,"sampleIdentity":{"id":"RUN-NATIVE-'//trim(it(r%serial))//'","name":"NATIVE-'//trim(it(r%serial))// &
   '-S1","prefix":"NATIVE-'//trim(it(r%serial))//'","createdAt":'//jstr(r%started)//',"mode":"native",'// &
   '"samples":[{"no":1,"name":"NATIVE-'//trim(it(r%serial))//'-S1","extractionDate":'//jstr(run_date(r))// &
   ',"label":"","preset":"Generic DNA template (native model)"}]},"plate":[')
  do p=0,383
   if(p>0)call add(',')
   w=wp(p)
   if(w==0)then
    call add('{"role":"empty","t":[]}');cycle
   end if
   call add('{"role":"'//trim(role_of(r,w))//'","ctl":"'//trim(ctl_of(r,w))//'","sample":"'//trim(sample_id(r,w))// &
    '","sampleName":"'//trim(sample_id(r,w))//'","label":"","extractionDate":'//jstr(run_date(r)))
   if(w<=n)call add(',"sampleNo":1,"ext":'//trim(it((w-1)/r%nr+1))//',"rep":'//trim(it(mod(w-1,r%nr)+1)))
   call add(',"sampleId":"'//trim(sample_id(r,w))//'","parentSampleId":"RUN-NATIVE-'//trim(it(r%serial))//'","material":"'// &
    trim(sample_text(r,w))//'","stocks":[],"dilution":null,"pos":'//trim(it(p))//',"well":"'//trim(pos_label(p))// &
    '","t":[{"m":"DNA-target","q":'//trim(gn(nominal_copies(r,w,1)))//',"material":"'//trim(sample_text(r,w))// &
    '","expectPct":null},{"m":"YY-target","q":'//trim(gn(nominal_copies(r,w,2)))//',"material":"'//trim(sample_text(r,w))// &
    '","expectPct":null}]}')
  end do
  call add('],"sopPlate":null,"protocol":{"name":"'//trim(prog_name(r))//'","lid_c":'//trim(gn(r%lid_t))//',"stages":[')
  do k=1,r%nhold
   call prog_step(r,k,stage,step,temp,dur,rd)
   call add('{"type":"hold","repeats":1,"steps":[{"temp_c":'//trim(gn(temp))//',"hold_s":'//trim(gn(dur))//'}]},')
  end do
  call prog_step(r,r%nhold+1,stage,step,temp,dur,rd)
  call add('{"type":"cycle","repeats":'//trim(it(r%planned_cycles))//',"steps":[{"temp_c":'//trim(gn(temp))//',"hold_s":'// &
   trim(gn(dur))//'},')
  call prog_step(r,r%nhold+2,stage,step,temp,dur,rd)
  call add('{"temp_c":'//trim(gn(temp))//',"hold_s":'//trim(gn(dur))//',"read":true}]}]},')
  call add('"seed":'//trim(it(r%seed))//',"operator":null,"gmo":null,"design":{"extractions":'//trim(it(r%ne))// &
   ',"replicates":'//trim(it(r%nr))//',"startingCopies":'//trim(it(r%copies))//',"netThreshold":'//trim(it(r%threshold))// &
   ',"plannedCycles":'//trim(it(r%planned_cycles))//',"wells":'//trim(it(r%nw))//',"programme":'//trim(it(r%prog))//'},')
  call add('"firmwareConfiguration":{"kp":0.55,"ki":0.25,"kd":0.06,"dTau":0.08,"ffHeat":0.0045,"ffCool":null,"band":0.5,'// &
   '"dwellMs":null,"rampTimeoutMs":null,"lidKp":0.2,"lidKi":0,"lidBand":2,"lidTimeoutMs":null,"fanMin":0.3,"fanLo":35,'// &
   '"fanHi":55,"blockMax":103,"blockMin":-5,"lidMax":null,"hsMax":null,"limitN":null,"zoneMis":null,"zoneMisMs":null,'// &
   '"refMis":2,"refMisMs":2000,"iMin":null,"iMax":6000,"iMs":null,"rpmMin":null,"stallMs":null,"maxRamp":null,'// &
   '"telemetryMs":100,"ledCode":null,"wells":384,"optics":"sequential well scan, nominal 100 ms per well and channel",'// &
   '"opticsSeed":'//trim(it(r%seed))//',"cameraReporter":{"reachableFraction":0.115,"ratePerCycle":0.61,'// &
   '"transitionCycles":2,"lateFraction":0.025,"onsetFraction":null,"lateP35S":null,'// &
   '"opticalVariation":null,"rateByMethod":null},"controlStepS":0.05,"plantStepMs":1,"monitorGapLimitC":2,'// &
   '"monitorPersistenceMs":2000,"monitorDelayMs":1000,"injectedSecondaryBiasC":'//trim(it(r%fault*5))//'},')
  call add('"simulationSetup":{"wells":[')
  do p=0,383
   if(p>0)call add(',')
   w=wp(p)
   if(w==0)then
    call add('{"kind":"empty"}');cycle
   end if
   a=r%assay_s(:,w)
   call add('{"kind":"reaction","copies":'//trim(gn(a(2)))//',"amplicon_bp":'//trim(gn(a(3)))//',"amplicon_tm":'// &
    trim(gn(a(4)))//',"template_tm":'//trim(gn(a(5)))//',"primer_tm":'//trim(gn(a(6)))//',"primer_nM":'//trim(gn(a(7)))// &
    ',"primer_ref_nM":'//trim(gn(r%assay2_s(7,w)))//',"dimer_rate":'//trim(gn(a(9)))//',"dimer_bp":'//trim(gn(a(10)))// &
    ',"dimer_tm":'//trim(gn(a(11)))//',"hot_start":'//trim(lg(a(12)/=0))//',"enz":'//trim(gn(a(13)))//',"eff":'// &
    trim(gn(a(8)))//',"noRef":'//trim(lg(r%assay2_s(2,w)<=0))//',"probe":{"methodId":"DNA-target","nM":'// &
    trim(gn(r%probe_f(w)))//',"darkNM":0,"freeNM":0,"tm":'//trim(gn(r%probe_tm(w)))//',"width":1.5,"q":0.96,'// &
    '"channel":0,"annS":'//trim(gn(r%ann_hold))//'}}')
  end do
  w=1
  call add('],"info":{"DNA-target":{"F":'//trim(gn(r%assay_s(7,w)))//',"R":'//trim(gn(r%assay_s(7,w)))//',"tmF":'// &
   trim(gn(r%assay_s(6,w)))//',"tmR":'//trim(gn(r%assay_s(6,w)))//',"eff":'//trim(gn(r%assay_s(8,w)))// &
   ',"ampliconTm":'//trim(gn(r%assay_s(4,w)))//',"dye":"FAM","quencher":"dark quencher (nominal)","probeNominal":200,'// &
   '"probeTm":70,"probeTm0":null,"q":0.96,"mgb":false,"mix":1,"primerIntact":1,"probeIntact":1,"bleached":0,'// &
   '"imperfect":false,"inhibitor":0,"dimer3":null,"baseline":null,"plateau":null,"bound":null},"YY-target":{"F":'//trim(gn(r%assay2_s(7,w)))//',"R":'//trim(gn(r%assay2_s(7,w)))// &
   ',"eff":'//trim(gn(r%assay2_s(8,w)))//',"dye":"Yakima Yellow","probeNominal":200,"probeTm":70,"channel":1}},')
  call add('"variation":{"natural":true,"seed":'//trim(it(r%seed))//',"runIndex":'//trim(it(r%serial))// &
   ',"streams":["model","independent monitor","reference channel","scenario"],"source":"native seeded random streams",'// &
   '"extractions":[],"extractionSd":null,"gmCv":null,"tubeSd":null,"refTubeSd":null,"runSd":null,"runShiftCycles":null,'// &
   '"tubeShiftCycles":null},'// &
   '"telemetryCadenceS":0.1,"chemistrySnapshotS":'//trim(it(snap_every))//',"detectorPeriods":'//trim(it(periods))// &
   ',"samplesPerPeriod":'//trim(it(spp))//',"detectorStatistics":"derived from recorded counts with an independent seeded'// &
   ' stream","nominalCopies":"before scenario modifications"}}')
 end subroutine
! @nativefn NR2-M06-F0052B
 function ctl_of(r,w) result(s)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=4)::s
  integer::n
  n=r%ne*r%nr;s='S'
  if(w==n+1)s='PC'
  if(w==n+2)s='NTC'
  if(w==n+3)s='NAC'
  if(w>=n+4)s='LOD'
 end function
! @nativefn NR2-M06-F0052C
 function prog_name(r) result(s)
  type(run_t),intent(in)::r
  character(len=80)::s
  if(r%prog==1)then
   s='Standard probe programme: UNG 50 C 2 min, activation 95 C 10 min, 45 x (95 C 15 s, anneal 60 s)'
  else
   s='Native short programme: 95 C 2 min, 45 x (95 C 10 s, anneal 30 s)'
  end if
 end function
! ---------------------------------------------------------------- source/simulator-record.json
! @nativefn NR2-M06-F0053
 subroutine write_record_json(r)
  type(run_t),intent(in)::r
  integer::i,k,w,c,code,first,nr
  used=0;nr=hn(r)
  call add('{"format":"missy-native-simulator-record/2","engine":"fortran","telemetry":[')
  do i=1,nr
   if(i>1)call add(',')
   call add('{"seq":'//trim(it(2*i))//',"t_ms":'//trim(it(nint(hv(r,1,i)*1000)))//',"state":"'// &
    trim(state_name(nint(hv(r,16,i))))//'","stage":'//trim(it(nint(hv(r,17,i))))//',"step":'//trim(it(nint(hv(r,18,i))))// &
    ',"cycle":'//trim(it(nint(hv(r,19,i))))//',"setpoint_c":'//trim(ft(hv(r,2,i),3))//',"zone1_c":'//trim(ft(hv(r,3,i),4))// &
    ',"zone2_c":'//trim(ft(hv(r,4,i),4))//',"lid_c":'//trim(ft(hv(r,5,i),4))//',"heatsink_c":'//trim(ft(hv(r,6,i),4)))
   call add(',"ref_c":'//trim(ft(hv(r,7,i),4))//',"duty1":'//trim(ft(hv(r,8,i),4))//',"duty2":'//trim(ft(hv(r,9,i),4))// &
    ',"lid_duty":'//trim(ft(hv(r,10,i),4))//',"fan_duty":'//trim(ft(hv(r,11,i),4))//',"i_tec1_mA":'// &
    trim(ft(hv(r,12,i),1))//',"i_tec2_mA":'//trim(ft(hv(r,13,i),1))//',"fan_rpm":'//trim(it(nint(hv(r,14,i))))// &
    ',"enable":'//trim(it(nint(hv(r,15,i))))//',"lid_closed":1,"hw_trip":0,"rails_ok":1,"optics_busy":'// &
    trim(it(nint(hv(r,20,i))))//'}')
  end do
  call add('],"optical":[');first=1
  do k=1,r%ncy
   do c=1,2
    do w=1,r%nw
     if(.not.rok(k,w,c))cycle
     if(first==0)call add(',')
     first=0
     call add('{"seq":'//trim(it(rseq(k,w,c)))//',"t_start_ms":'//trim(it(nint(rt0(k,w,c)*1000)))//',"t_end_ms":'// &
      trim(it(nint(rt0(k,w,c)*1000)+100))//',"kind":"A","acq":'//trim(it(k))//',"stage":'//trim(it(r%nhold))// &
      ',"step":1,"cycle":'//trim(it(k))//',"well":'//trim(it(pos_of(r,w)))//',"channel":'//trim(it(c-1))//',"periods":'// &
      trim(it(periods))//',"sum_on":'//trim(i8t(rs(1,k,w,c)))//',"sum_off":'//trim(i8t(rs(2,k,w,c)))//',"sumsq_net":'// &
      trim(i8t(rs(3,k,w,c))))
     call add(',"min_net":'//trim(i8t(rs(4,k,w,c)))//',"max_net":'//trim(i8t(rs(5,k,w,c)))//',"sat_samples":'// &
      trim(i8t(rs(6,k,w,c)))//',"ref_sum_on":'//trim(i8t(rs(7,k,w,c)))//',"ref_sum_off":'//trim(i8t(rs(8,k,w,c)))// &
      ',"tzone_start_mC":'//trim(it(nint(zone_temp(r,k,w)*1000)))//',"tzone_end_mC":'// &
      trim(it(nint(zone_temp(r,k,w)*1000)))//',"cleaved_fraction":'//trim(tf(r%cutf(k,w,c)))//',"light_factor":'// &
      trim(tf(r%lightf(k,w)))//',"model_counts":'//trim(tf(r%rawsig(k,w,c)))//'}')
    end do
   end do
  end do
  call add('],"events":[')
  do i=1,r%nev
   if(i>1)call add(',')
   code=nint(r%ev(2,i))
   call add('{"seq":'//trim(it(i-1))//',"t_ms":'//trim(it(nint(r%ev(1,i))))//',"code":"'//trim(event_name(code))// &
    '","severity":'//trim(it(severity_code(code)))//',"a":'//trim(it(nint(r%ev(3,i))))//',"b":'//trim(it(nint(r%ev(4,i))))//'}')
  end do
  call add('],"sensorObservations":[')
  do i=1,r%ntel_rows
   if(i>1)call add(',')
   call add('{"t_ms":'//trim(it(nint(tel(r,1,i)*1000)))//',"gap_c":'//trim(ft(tel(r,26,i),4))//'}')
  end do
  call add(']}')
 end subroutine
! ---------------------------------------------------------------- source/simulator-chemistry.json
! @nativefn NR2-M06-F0054
 subroutine write_chemistry(r)
  ! R2 snapshot layout: liquid temperatures, controller, plant and per-well reaction state every 10 s.
  type(run_t),intent(in)::r
  integer::k,w,first,s,last
  real(c_float)::g(nsg)
  used=0;call add('[');s=hslot(r)
  if(s>0)then
   last=hfr(s)%ns
   do k=1,last
    g=hfr(s)%g(:,k)
    if(k>1)call add(',')
    call add('{"t_ms":'//trim(it(nint(g(1)*1000)))//',"liquid_c":['//trim(tf(g(2)))//','//trim(tf(g(3)))//'],'// &
     '"control":{"destination_c":'//trim(tf(g(4)))//',"setpoint_c":'//trim(tf(g(4)))//',"maximum_ramp_c_per_s":null,'// &
     '"at_target":'//trim(lg(abs((g(9)+g(10))/2-g(4))<0.5))//',"lid_integral":0,"zone_integrals":['//trim(tf(g(5)))//','//trim(tf(g(6)))//'],"zone_filtered_derivatives":['// &
     trim(tf(g(7)))//','//trim(tf(g(8)))//']},"plant":{"block_c":['//trim(tf(g(9)))//','//trim(tf(g(10)))//'],"lid_c":'// &
     trim(tf(g(11)))//',"sink_c":'//trim(tf(g(12)))//',"ambient_c":'//trim(tf(g(13)))//',"tec_mA":['//trim(tf(g(14)))// &
     ','//trim(tf(g(15)))//'],"lid_mA":'//trim(tf(g(16)))//',"watchdog_ok":null,"watchdog_age_ms":null,"enable_request":'// &
     trim(lg(g(17)>0))//',"overtemperature_test":false,"hardware_trip":'//trim(lg(g(18)>0))// &
     ',"selected_well":-1,"led_drive":null},"wells":[')
    first=1
    do w=1,r%nw
     if(r%present(w)==0)cycle
     if(first==0)call add(',')
     first=0
     call add('{"template":'//trim(tf(hfr(s)%c(1,w,k)))//',"ds":'//trim(tf(hfr(s)%c(2,w,k)))//',"ss":'// &
      trim(tf(hfr(s)%c(3,w,k)))//',"copying":'//trim(tf(hfr(s)%c(4,w,k)))//',"stalled":'//trim(tf(hfr(s)%c(5,w,k)))// &
      ',"dimer":'//trim(tf(hfr(s)%c(6,w,k)))//',"primers":'//trim(tf(hfr(s)%c(7,w,k)))//',"enzyme":'// &
      trim(tf(hfr(s)%c(9,w,k)))//',"enzymeBlocked":'//trim(tf(hfr(s)%c(10,w,k)))//',"copiesReal":'// &
      trim(tf(hfr(s)%c(11,w,k)))//',"well":'//trim(it(pos_of(r,w))))
     call add(',"probe":{"cleaved":'//trim(tf(hfr(s)%c(13,w,k)))//',"bound":'//trim(tf(hfr(s)%c(15,w,k)))//',"tm":'// &
      trim(tf(hfr(s)%c(16,w,k)))//',"total":'//trim(tf(hfr(s)%c(14,w,k)))//'},"dntp":'//trim(tf(hfr(s)%c(8,w,k)))// &
      ',"madeTarget":'//trim(tf(hfr(s)%c(12,w,k)))//'}')
    end do
    call add(']}')
   end do
  end if
  call add(']')
 end subroutine
! ---------------------------------------------------------------- analysis files
! @nativefn NR2-M06-F0055
 subroutine assay_wells_json(r,c)
  type(run_t),intent(in)::r
  integer,intent(in)::c
  integer::w,first,sat,k
  real(c_double)::b0,b1,sd
  call add('"wells":[');first=1
  do w=1,r%nw
   if(r%present(w)==0)cycle
   if(first==0)call add(',')
   first=0;sat=0
   do k=1,r%ncy
    if(rok(k,w,c).and.rs(6,k,w,c)>0)sat=sat+int(rs(6,k,w,c))
   end do
   call base_fit(r,w,c,b0,b1,sd)
   call add('{"well":'//trim(it(pos_of(r,w)))//',"position":"'//trim(pos_label(pos_of(r,w)))//'","role":"'// &
    trim(role_of(r,w))//'","sample":"'//trim(sample_id(r,w))//'","cq":')
   if(cqv(r,w,c)>0)then
    call add(trim(ft(cqv(r,w,c),4))//',"cq_reason_if_none":null,"cq_threshold":'//trim(ft(cqv(r,w,c),4)))
   else
    call add('null,"cq_reason_if_none":"no threshold crossing","cq_threshold":null')
   end if
   call add(',"cq_cy0":null,"copies_from_calibration":null,"baseline":{"window":[1,5],"intercept":'//trim(jn(b0,3))// &
    ',"slope":'//trim(jn(b1,4))//',"residual_sd":'//trim(jn(sd,4))//',"mean":'//trim(jn(baseline(r,w,c),3))// &
    '},"saturated_samples":'//trim(it(sat))//',"tm_c":null,"end_point":')
   if(r%ncy>0)then
    if(yv(r,r%ncy,w,c)>=0)then
     call add(trim(it(nint(yv(r,r%ncy,w,c))))//'}')
    else
     call add('null}')
    end if
   else
    call add('null}')
   end if
  end do
  call add(']')
 end subroutine
! @nativefn NR2-M06-F0056
 subroutine cq_list(r,c,want)
  type(run_t),intent(in)::r
  integer,intent(in)::c
  character(len=*),intent(in)::want
  integer::w,first
  first=1
  do w=1,r%nw
   if(r%present(w)==0.or.trim(role_of(r,w))/=want)cycle
   if(cqv(r,w,c)<=0)cycle
   if(first==0)call add(',')
   first=0;call add(trim(ft(cqv(r,w,c),4)))
  end do
 end subroutine
! @nativefn NR2-M06-F0056B
 subroutine ext_stats(r,c,e,tubes,amp,meancq,sdcq)
  ! Replicate tubes of extraction e: number, number amplified, mean and SD of Cq.
  type(run_t),intent(in)::r
  integer,intent(in)::c,e
  integer,intent(out)::tubes,amp
  real(c_double),intent(out)::meancq,sdcq
  integer::w
  real(c_double)::s1,s2
  tubes=0;amp=0;s1=0;s2=0
  do w=(e-1)*r%nr+1,e*r%nr
   if(r%present(w)==0)cycle
   tubes=tubes+1
   if(cqv(r,w,c)>0)then
    amp=amp+1;s1=s1+cqv(r,w,c);s2=s2+cqv(r,w,c)**2
   end if
  end do
  meancq=ieee_value(1d0,ieee_quiet_nan);sdcq=meancq
  if(amp>0)meancq=s1/amp
  if(amp>1)sdcq=sqrt(max(0d0,(s2-amp*meancq**2)/(amp-1)))
 end subroutine
! @nativefn NR2-M06-F0057
 subroutine write_evaluation(r,stats,sop_id)
  type(run_t),intent(in)::r
  real(c_double),intent(in)::stats(4)
  character(len=*),intent(in)::sop_id
  integer::c,v,n,e,tubes,amp,ne_ok,first
  real(c_double)::m,sd,em(3),mm,ss,m2,sd2
  character(len=60)::vt
  character(len=20)::res
  used=0;v=verdict(r);n=r%ne*r%nr
  select case(v)
  case(1);vt='valid run, controls passed'
  case(2);vt='invalid: control acceptance failed'
  case(4);vt='controls passed; deliberate training scenario'
  case default;vt='incomplete run, not interpretable'
  end select
  res='not detected'
  do e=1,r%ne
   call ext_stats(r,1,e,tubes,amp,m,sd);if(amp>0)res='detected'
  end do
  if(v==2.or.v==3)res='invalid'
  call add('{"format":"missy-native-analysis/2","notice":"Research use only. Not validated. Simulated data.","run_id":"Native-'// &
   trim(it(r%serial))//'","run_package_manifest_sha256":null,"sop_id":"'//trim(sop_id)//'","sample_identity":{"id":'// &
   '"RUN-NATIVE-'//trim(it(r%serial))//'","name":"NATIVE-'//trim(it(r%serial))//'-S1","prefix":"NATIVE-'//trim(it(r%serial))// &
   '","createdAt":'//jstr(r%started)//',"mode":"native","samples":[{"no":1,"name":"NATIVE-'//trim(it(r%serial))// &
   '-S1","extractionDate":'//jstr(run_date(r))//',"label":"","preset":"Generic DNA template (native model)"}]},')
  call add('"step":"detection","species":null,"sample":"NATIVE-'//trim(it(r%serial))//'-S1","dna_ng_per_tube":null,'// &
   '"dna_template":{"copiesPerReaction":'//trim(it(r%copies))//',"concNgPerUl":null,"volUl":null},'// &
   '"natural_variation":{"natural":true,"seed":'//trim(it(r%seed))//',"runIndex":'//trim(it(r%serial))// &
   ',"source":"native seeded random streams (model, monitor, reference channel, scenario)","extractions":[],'// &
   '"extractionSd":null,"gmCv":null,"tubeSd":null,"refTubeSd":null,"runSd":null,"runShiftCycles":null,'// &
   '"tubeShiftCycles":null},"template":null,')
  call add('"reagents_and_handling":{"primerF":'//trim(gn(r%assay_s(7,1)))//',"primerR":'//trim(gn(r%assay_s(7,1)))// &
   ',"probe":'//trim(gn(r%probe_f(1)))//',"quencher":"method","hotStart":'//trim(lg(r%assay_s(12,1)/=0))// &
   ',"ungActive":'//trim(lg(r%prog==1))//',"inhibitor":0,"efficiency":'//trim(gn(r%assay_s(8,1)))// &
   ',"ambientC":25,"benchMin":null,"carryover":0,"freezeThaw":null,"mixMonths":null,"mixStoreC":null,"pipetCV":null,'// &
   '"primerMonths":null,"primerStoreC":null,"probeLightH":null,"probeMismatch":false,"probeMonths":null,"probeStoreC":null},'// &
   '"methods_source":"native generic hydrolysis-probe model; no reference-method database","mara":null,')
  call add('"verdict_code":'//trim(it(v))//',"verdict":"'//trim(vt)//'","threshold":'//trim(gn(thr_of(r)))//',"assays":[')
  do c=1,2
   if(c==2)call add(',')
   if(c==1)then
    call add('{"method":"DNA-target","target":"DNA-target","kind":"target","channel":1,')
   else
    call add('{"method":"YY-target","target":"YY-target","kind":"reference","channel":2,')
   end if
   call add('"threshold":'//trim(gn(thr_of(r)))//',"threshold_source":"fixed net threshold of the native profile",'// &
    '"cq_method":"threshold","baseline_window":[1,5],"controls":{')
   if(c==1.and.r%nw>=n+5)then
    call add('"no_template_present":'//trim(lg(r%present(n+2)/=0))//',"no_template_clean":'//trim(lg(r%cq(n+2)<=0))// &
     ',"positive_amplified":'//trim(lg(r%cq(n+1)>0))//',"valid":'//trim(lg(v==1.or.v==4))//',"no_amplification_clean":'// &
     trim(lg(r%cq(n+3)<=0))//',"lod_detected":'//trim(lg(r%cq(n+4)>0.or.r%cq(n+5)>0)))
   else
    call add('"no_template_present":null,"no_template_clean":null,"positive_amplified":null,"valid":null')
   end if
   call add('},"sample_result":')
   if(c==1)then
    call add('"'//trim(res)//'"')
   else
    call add('null')
   end if
   call add(',"positive_cq":[');call cq_list(r,c,'positive');call add('],"sample_cq":[');call cq_list(r,c,'unknown')
   call add('],"calibration":{"slope":null,"intercept":null,"r2":null,"efficiency":null,"within_acceptance":null},')
   call assay_wells_json(r,c);call add('}')
  end do
  call add('],"findings":[')
  first=1
  if(r%nw>=n+5)then
   if(r%cq(n+1)<=0)then
    call add('"positive control did not amplify"');first=0
   end if
   if(r%cq(n+2)>0)then
    if(first==0)call add(',')
    call add('"no-template control amplified"');first=0
   end if
   if(r%cq(n+3)>0)then
    if(first==0)call add(',')
    call add('"no-amplification control amplified"');first=0
   end if
   if(r%cq(n+4)<=0.and.r%cq(n+5)<=0)then
    if(first==0)call add(',')
    call add('"neither LOD control detected"');first=0
   end if
  end if
  if(r%status/=1)then
   if(first==0)call add(',')
   call add('"run not completed: '//trim(outcome(r))//'"')
  end if
  call add('],"screening":null,"quantification":{"status":"not applicable","reason":"the native design has no calibrant'// &
   ' series or reference-gene ratio","acceptance":null},"samples":[{"no":1,"name":"NATIVE-'//trim(it(r%serial))// &
   '-S1","result":"'//trim(res)//'","gm_percent_copies":null,"by_extraction":[')
  ne_ok=0
  do e=1,r%ne
   call ext_stats(r,1,e,tubes,amp,m,sd)
   if(e>1)call add(',')
   call ext_stats(r,2,e,tubes,amp,m2,sd2);call ext_stats(r,1,e,tubes,amp,m,sd)
   call add('{"ext":'//trim(it(e))//',"gmPct":null,"eventCq":'//trim(jn(m,4))//',"taxonCq":'//trim(jn(m2,4))//',"meanCq":'// &
    trim(jn(m,4))//',"sdCq":'//trim(jn(sd,4))//',"tubes":'//trim(it(tubes))//',"amplified":'//trim(it(amp))//'}')
   if(amp>0)then
    ne_ok=ne_ok+1;em(ne_ok)=m
   end if
  end do
  mm=ieee_value(1d0,ieee_quiet_nan);ss=mm
  if(ne_ok>0)mm=sum(em(1:ne_ok))/ne_ok
  if(ne_ok>1)ss=sqrt(sum((em(1:ne_ok)-mm)**2)/(ne_ok-1))
  call add('],"extraction_statistics":{"n":'//trim(it(ne_ok))//',"mean":'//trim(jn(mm,4))//',"sd":'//trim(jn(ss,4))// &
   ',"rsdPct":')
  if(ne_ok>1)then
   call add(trim(jn(100*ss/mm,3)))
  else
   call add('null')
  end if
  call add(',"quantity":"mean Cq of the extraction means"},"laboratory":null,"mara":null,"assays":[')
  do c=1,2
   if(c==2)call add(',')
   if(c==1)then
    call add('{"method":"DNA-target","sample_result":"'//trim(res)//'","by_extraction":[')
   else
    call add('{"method":"YY-target","sample_result":null,"by_extraction":[')
   end if
   do e=1,r%ne
    call ext_stats(r,c,e,tubes,amp,m,sd)
    if(e>1)call add(',')
    call add('{"extraction":'//trim(it(e))//',"tubes":'//trim(it(tubes))//',"amplified":'//trim(it(amp))//',"mean_cq":'// &
     trim(jn(m,4))//'}')
   end do
   call add(']}')
  end do
  call add('],"findings":[]}],"plate_format":{"wells":384,"rows":16,"columns":24,"extractions_per_sample":'//trim(it(r%ne))// &
   ',"pcr_replicates_per_extraction":'//trim(it(r%nr))//'},"run_prefix":"NATIVE-'//trim(it(r%serial))//'","thermal":{'// &
   '"holdErrMax":'//trim(jn(stats(1),4))//',"holdErrRms":'//trim(jn(stats(2),4))//',"zoneDiffMax":'//trim(jn(stats(3),4))// &
   ',"blockMax":'//trim(jn(stats(4),4))//'},"software":"MISSY native Fortran edition, RDMLt export 3"}')
 end subroutine
! @nativefn NR2-M06-F0058
 subroutine write_assay_detail(r,stats)
  ! R2 per-assay diagnostics: tables (per plate position, both channels), amp (per well baseline
  ! and corrected curve), sc, melt, thermal, flags, options.
  type(run_t),intent(in)::r
  real(c_double),intent(in)::stats(4)
  integer::c,w,k,first,cc,tb,p,wp(0:383)
  real(c_double)::b0,b1,sd
  character(len=10)::tar
  wp=0
  do w=1,r%nw
   if(r%present(w)/=0)wp(pos_of(r,w))=w
  end do
  used=0;call add('{')
  do c=1,2
   tar='DNA-target';if(c==2)tar='YY-target'
   if(c==2)call add(',')
   call add('"'//trim(tar)//'":{"tables":{"nw":384,"cycles":[')
   do k=1,r%ncy
    if(k>1)call add(',')
    call add(trim(it(k)))
   end do
   call add(']')
   do tb=1,2
    if(tb==1)then
     call add(',"amp":[')
    else
     call add(',"ampSat":[')
    end if
    do cc=1,2
     if(cc==2)call add(',')
     call add('[')
     do p=0,383
      if(p>0)call add(',')
      w=wp(p)
      if(w==0)then
       call add('null');cycle
      end if
      call add('[')
      do k=1,r%ncy
       if(k>1)call add(',')
       if(tb==1)then
        if(yv(r,k,w,cc)<0)then
         call add('null')
        else
         call add(trim(it(nint(yv(r,k,w,cc)))))
        end if
       else
        call add(trim(lg(rok(k,w,cc).and.rs(6,k,w,cc)>0)))
       end if
      end do
      call add(']')
     end do
     call add(']')
    end do
    call add(']')
   end do
   call add(',"ampRef":[null,null],"melt":[null,null],"meltT":[null,null]},"amp":{"cycles":[')
   do k=1,r%ncy
    if(k>1)call add(',')
    call add(trim(it(k)))
   end do
   call add('],"wells":[');first=1
   do w=1,r%nw
    if(r%present(w)==0)cycle
    if(first==0)call add(',')
    first=0;call base_fit(r,w,c,b0,b1,sd)
    call add('{"well":'//trim(it(pos_of(r,w)))//',"role":"'//trim(role_of(r,w))//'","sample":"'//trim(sample_id(r,w))// &
     '","baseline":{"window":[1,5],"intercept":'//trim(jn(b0,3))//',"slope":'//trim(jn(b1,4))//',"residualSd":'// &
     trim(jn(sd,4))//',"takeoff":')
    if(cqv(r,w,c)>0)then
     call add(trim(it(int(cqv(r,w,c)))))
    else
     call add('null')
    end if
    call add(',"corrected":[')
    do k=1,r%ncy
     if(k>1)call add(',')
     if(yv(r,k,w,c)<0)then
      call add('null')
     else
      call add(trim(jn(yv(r,k,w,c)-baseline(r,w,c),3)))
     end if
    end do
    call add(']},"cq":'//trim(jn(merge(cqv(r,w,c),ieee_value(1d0,ieee_quiet_nan),cqv(r,w,c)>0),4))//'}')
   end do
   call add('],"threshold":'//trim(gn(thr_of(r)))//',"thresholdSource":"fixed net threshold of the native profile",'// &
    '"cqMethod":"threshold","unit":"counts (LED on - LED off)"},"sc":{"available":false,"slope":null,"intercept":null,'// &
    '"r2":null,"efficiency":null,"points":[],"range":null,"reason":"no calibrant series in the native design"},'// &
    '"melt":null,"thermal":{"holdErrMax":'//trim(jn(stats(1),4))//',"holdErrRms":'//trim(jn(stats(2),4))// &
    ',"zoneDiffMax":'//trim(jn(stats(3),4))//',"blockMax":'//trim(jn(stats(4),4))//'},"flags":[],"options":{"target":'// &
    trim(it(c-1))//',"passiveRef":null,"cqMethod":"threshold","threshold":'//trim(gn(thr_of(r)))//',"wells":{}}}')
  end do
  call add('}')
 end subroutine
! @nativefn NR2-M06-F0059
 subroutine write_temperature_statistics(r,stats)
  ! Per programmed step and per cycle, from the 10 Hz record (stdlib mean and variance), with the
  ! R2 keys: holds, mean, sd, min, max, maxErr, overshoot, dir, rampRate, rampS, holdS, perCycle.
  type(run_t),intent(in)::r
  real(c_double),intent(out)::stats(4)
  real(c_double),allocatable::buf(:)
  real(c_double)::mean,sd,errmax,acc2,t,temp,dur,ts0,rsum,rate,tstart,dir,ov,lsum,lsum2
  integer::i,n,k,stg,stp,first,cyc,nall,nc,nr,holds,j,nramp
  logical::rd
  nr=hn(r);allocate(buf(max(1,nr)))
  used=0;stats=ieee_value(1d0,ieee_quiet_nan);errmax=0;acc2=0;nall=0
  call add('{"cadenceS":0.1,"steps":[')
  do k=1,r%nhold+2
   call prog_step(r,k,stg,stp,temp,dur,rd)
   if(k>1)call add(',')
   n=0;ov=0
   do i=1,nr
    if(nint(hv(r,17,i))/=stg.or.nint(hv(r,18,i))/=stp.or.nint(hv(r,16,i))/=3)cycle
    n=n+1;buf(n)=(hv(r,3,i)+hv(r,4,i))/2
    t=abs(buf(n)-temp);errmax=max(errmax,t);acc2=acc2+t*t;nall=nall+1
   end do
   ! ramps: STEP_START to TARGET_REACHED of this step
   holds=0;rsum=0;rate=0;nramp=0;dir=0;tstart=-1
   do j=1,r%nev
    if(nint(r%ev(3,j))/=stg.or.nint(r%ev(4,j))/=stp)cycle
    if(nint(r%ev(2,j))==ev_step_start)tstart=r%ev(1,j)
    if(nint(r%ev(2,j))==ev_target_reached.and.tstart>=0)then
     holds=holds+1;ts0=(r%ev(1,j)-tstart)/1000;rsum=rsum+ts0
     t=start_temp(r,tstart/1000,nr)
     if(dir==0)dir=sign(1d0,temp-t)
     if(ts0>0)then
      rate=rate+abs(temp-t)/ts0;nramp=nramp+1
     end if
     tstart=-1
    end if
   end do
   if(n>0)ov=max(0d0,maxval(dir*(buf(1:n)-temp)))
   call add('{"stage":'//trim(it(stg))//',"step":'//trim(it(stp))//',"type":"')
   if(k<=r%nhold)then
    call add('hold"')
   else
    call add('cycle"')
   end if
   call add(',"target":'//trim(gn(temp))//',"holds":'//trim(it(holds))//',"n":'//trim(it(n)))
   if(n>1)then
    mean=stdlib_mean(buf(1:n));sd=sqrt(stdlib_var(buf(1:n)))
    call add(',"mean":'//trim(ft(mean,4))//',"sd":'//trim(ft(sd,4))//',"min":'//trim(ft(minval(buf(1:n)),4))// &
     ',"max":'//trim(ft(maxval(buf(1:n)),4))//',"maxErr":'//trim(ft(maxval(abs(buf(1:n)-temp)),4)))
   end if
   call add(',"overshoot":'//trim(ft(ov,4))//',"dir":'//trim(it(nint(dir)))//',"rampRate":')
   if(nramp>0)then
    call add(trim(ft(rate/nramp,4)))
   else
    call add('null')
   end if
   call add(',"rampS":')
   if(holds>0)then
    call add(trim(ft(rsum/holds,3)))
   else
    call add('null')
   end if
   call add(',"holdS":'//trim(gn(dur))//',"perCycle":[')
   first=1
   do cyc=1,max(1,r%ncy+1)
    nc=0;mean=0
    do i=1,nr
     if(nint(hv(r,17,i))/=stg.or.nint(hv(r,18,i))/=stp.or.nint(hv(r,16,i))/=3)cycle
     if(k>r%nhold.and.nint(hv(r,19,i))/=cyc)cycle
     nc=nc+1;mean=mean+(hv(r,3,i)+hv(r,4,i))/2
    end do
    if(nc==0)cycle
    if(first==0)call add(',')
    first=0;call add('{"cycle":'//trim(it(cyc))//',"mean":'//trim(ft(mean/nc,4))//'}')
    if(k<=r%nhold)exit
   end do
   call add(']}')
  end do
  call add(']')
  if(nr>0)then
   stats(1)=errmax;if(nall>0)stats(2)=sqrt(acc2/nall)
   stats(3)=0;stats(4)=-huge(1d0);lsum=0;lsum2=0
   do i=1,nr
    stats(3)=max(stats(3),abs(hv(r,3,i)-hv(r,4,i)));stats(4)=max(stats(4),hv(r,3,i),hv(r,4,i))
    buf(i)=hv(r,5,i);lsum=lsum+buf(i);lsum2=lsum2+buf(i)**2
   end do
   call add(',"zoneDiffMax":'//trim(ft(stats(3),4))//',"blockMax":'//trim(ft(stats(4),4))//',"hsMax":')
   mean=-huge(1d0)
   do i=1,nr
    mean=max(mean,hv(r,6,i))
   end do
   call add(trim(ft(mean,4))//',"lid":{"n":'//trim(it(nr))//',"s":'//trim(gn(lsum))//',"s2":'//trim(gn(lsum2))// &
    '},"lidMean":'//trim(ft(stdlib_mean(buf(1:nr)),4)))
   if(nr>1)call add(',"lidSd":'//trim(ft(sqrt(stdlib_var(buf(1:nr))),4)))
   call add(',"holdErrMax":'//trim(ft(stats(1),4))//',"holdErrRms":'//trim(jn(stats(2),4)))
  end if
  call add(',"cycleMs":[')
  do k=2,r%ncy
   if(k>2)call add(',')
   call add(trim(it(nint((r%acq_t(k)-r%acq_t(k-1))*1000))))
  end do
  call add(']')
  if(r%ncy>1)call add(',"cycleS":'//trim(ft((r%acq_t(r%ncy)-r%acq_t(1))/(r%ncy-1),3)))
  call add('}')
 end subroutine
! @nativefn NR2-M06-F0059B
 real(c_double) function start_temp(r,t,nr) result(v)
  ! Mean block temperature at the first 10 Hz row at or after time t (s).
  type(run_t),intent(in)::r
  real(c_double),intent(in)::t
  integer,intent(in)::nr
  integer::lo,hi,m
  v=25;if(nr<1)return
  lo=1;hi=nr
  do while(lo<hi)
   m=(lo+hi)/2
   if(hv(r,1,m)<t)then
    lo=m+1
   else
    hi=m
   end if
  end do
  v=(hv(r,3,lo)+hv(r,4,lo))/2
 end function
! ---------------------------------------------------------------- TSV payloads
! @nativefn NR2-M06-F0060
 integer function rows_every(r,every) result(n)
  type(run_t),intent(in)::r
  integer,intent(in)::every
  integer::i
  n=0
  do i=1,r%ntel_rows
   if(mod(nint(tel(r,1,i)),every)==0)n=n+1
  end do
 end function
! @nativefn NR2-M06-F0061
 subroutine tsv_value(r,col,scale,dec)
  ! 10 Hz series (falls back to the 1 Hz trace for imported runs).
  type(run_t),intent(in)::r
  integer,intent(in)::col,dec
  real(c_double),intent(in)::scale
  integer::i
  used=0;call add('elapsed'//tb//'value');call newline()
  do i=1,hn(r)
   call add(trim(ft(hv(r,1,i),2))//tb//trim(ft(hv(r,col,i)*scale,dec)));call newline()
  end do
 end subroutine
! @nativefn NR2-M06-F0061B
 subroutine tsv_liquid(r,col)
  type(run_t),intent(in)::r
  integer,intent(in)::col
  integer::i
  used=0;call add('elapsed'//tb//'value');call newline()
  do i=1,r%ntel_rows
   if(mod(nint(tel(r,1,i)),10)/=0)cycle
   call add(trim(ft(tel(r,1,i),2))//tb//trim(ft(tel(r,col,i),4)));call newline()
  end do
 end subroutine
! @nativefn NR2-M06-F0062
 subroutine tsv_firmware(r)
  type(run_t),intent(in)::r
  integer::i,trip,nr
  used=0;nr=hn(r)
  call add('elapsed'//tb//'sequence'//tb//'state'//tb//'stage'//tb//'step'//tb//'cycle'//tb//'enable'//tb//'lid-closed'// &
   tb//'hardware-trip'//tb//'rails-ok'//tb//'detector-busy');call newline()
  do i=1,nr
   trip=0;if(r%status==7.and.i==nr)trip=1
   call add(trim(ft(hv(r,1,i),2))//tb//trim(it(2*i))//tb//trim(it(nint(hv(r,16,i))))//tb//trim(it(nint(hv(r,17,i))))//tb// &
    trim(it(nint(hv(r,18,i))))//tb//trim(it(nint(hv(r,19,i))))//tb//trim(it(nint(hv(r,15,i))))//tb//'1'//tb// &
    trim(it(trip))//tb//'1'//tb//trim(it(nint(hv(r,20,i)))));call newline()
  end do
 end subroutine
! @nativefn NR2-M06-F0063
 subroutine tsv_plant(r)
  ! R2 plant-model columns every 10 s; quantities the native plant does not model are NA.
  type(run_t),intent(in)::r
  integer::i
  used=0
  call add('elapsed'//tb//'block-1'//tb//'block-2'//tb//'lid'//tb//'heat-sink'//tb//'ambient'//tb//'current-1'//tb// &
   'current-2'//tb//'lid-current'//tb//'watchdog-age'//tb//'watchdog-ok'//tb//'enable-request'//tb//'trip-test'//tb// &
   'led-drive'//tb//'destination'//tb//'setpoint'//tb//'ramp');call newline()
  do i=1,r%ntel_rows
   if(mod(nint(tel(r,1,i)),10)/=0)cycle
   call add(trim(ft(tel(r,1,i),2))//tb//trim(ft(tel(r,22,i),4))//tb//trim(ft(tel(r,23,i),4))//tb//trim(ft(tel(r,5,i),4))// &
    tb//trim(ft(tel(r,6,i),4))//tb//trim(ft(tel(r,24,i),3))//tb//trim(ft(tel(r,12,i)/1000,4))//tb// &
    trim(ft(tel(r,13,i)/1000,4))//tb//trim(ft(tel(r,25,i)/1000,4))//tb//'NA'//tb//'NA'//tb//trim(it(nint(tel(r,15,i))))// &
    tb//'NA'//tb//'NA'//tb//trim(ft(tel(r,2,i),3))//tb//trim(ft(tel(r,2,i),3))//tb//'NA')
   call newline()
  end do
 end subroutine
! @nativefn NR2-M06-F0064
 subroutine tsv_read(r,k,c)
  type(run_t),intent(in)::r
  integer,intent(in)::k,c
  integer::w
  real(c_double)::z
  used=0
  call add('reaction'//tb//'sequence'//tb//'start'//tb//'end'//tb//'integration'//tb//'periods'//tb//'led-on'//tb//'led-off'// &
   tb//'mean-net'//tb//'squared-net'//tb//'min-net'//tb//'max-net'//tb//'saturation'//tb//'reference-on'//tb// &
   'reference-off'//tb//'zone-start'//tb//'zone-end');call newline()
  do w=1,r%nw
   if(r%present(w)==0)cycle
   z=zone_temp(r,k,w)
   call add(trim(it(pos_of(r,w)+1))//tb//trim(it(rseq(k,w,c)))//tb//trim(ft(rt0(k,w,c),2))//tb//trim(ft(rt0(k,w,c)+0.1d0,2))// &
    tb//'100'//tb//trim(it(periods))//tb)
   if(rok(k,w,c))then
    call add(trim(i8t(rs(1,k,w,c)))//tb//trim(i8t(rs(2,k,w,c)))//tb// &
     trim(gn(real(rs(1,k,w,c)-rs(2,k,w,c),c_double)/(periods*spp)))//tb//trim(i8t(rs(3,k,w,c)))//tb// &
     trim(i8t(rs(4,k,w,c)))//tb//trim(i8t(rs(5,k,w,c)))//tb//trim(i8t(rs(6,k,w,c)))//tb//trim(i8t(rs(7,k,w,c)))//tb// &
     trim(i8t(rs(8,k,w,c))))
   else
    call add('NA'//tb//'NA'//tb//'NA'//tb//'NA'//tb//'NA'//tb//'NA'//tb//'NA'//tb//'NA'//tb//'NA')
   end if
   call add(tb//trim(ft(z,3))//tb//trim(ft(z,3)));call newline()
  end do
 end subroutine
! ---------------------------------------------------------------- rdmlt_data.xml
! @nativefn NR2-M06-F0070
 subroutine x_origin()
  call add('<timeOrigin><elapsed unit="s" origin="reported" termId="elapsed-time">0</elapsed><clock>elapsed</clock>'// &
   '<referenceEvent id="clock-origin"/></timeOrigin>')
 end subroutine
! @nativefn NR2-M06-F0071
 subroutine x_col(id,quantity,unit,origin)
  character(len=*),intent(in)::id,quantity,unit,origin
  call add('<column id="'//id//'"><quantity>'//quantity//'</quantity><unit>'//unit//'</unit><dataType>float64</dataType>'// &
   '<missingValue>NA</missingValue><origin>'//origin//'</origin></column>')
 end subroutine
! @nativefn NR2-M06-F0072
 subroutine x_table_open(fileid,rows)
  character(len=*),intent(in)::fileid
  integer,intent(in)::rows
  call add('<table><file id="'//fileid//'"/><storage>TSV</storage><rowCount>'//trim(it(rows))//'</rowCount>')
  call x_col('elapsed','Elapsed instrument time','s','reported')
 end subroutine
! @nativefn NR2-M06-F0073
 subroutine x_log(r,id,quantity_rows)
  type(run_t),intent(in)::r
  character(len=*),intent(in)::id
  integer,intent(in)::quantity_rows
  call add('<temperatureLog id="log-'//id//'"><sensor id="'//id//'"/>');call x_origin()
  call x_table_open(id,quantity_rows)
  call x_col('value','Temperature','Cel','measured')
  call add('<documentation id="mapping"/></table></temperatureLog>');call newline()
 end subroutine
! @nativefn NR2-M06-F0074
 subroutine x_series(r,id,quantity,unit,origin)
  type(run_t),intent(in)::r
  character(len=*),intent(in)::id,quantity,unit,origin
  call add('<series id="series-'//id//'"><quantity>'//quantity//'</quantity><unit>'//unit//'</unit><origin>'//origin// &
   '</origin>');call x_origin();call x_table_open(id,hn(r))
  call x_col('value',quantity,unit,origin)
  call add('<documentation id="mapping"/></table></series>');call newline()
 end subroutine
! @nativefn NR2-M06-F0075
 subroutine x_sensor(id,desc,loc)
  character(len=*),intent(in)::id,desc,loc
  call add('<sensor id="'//id//'" termId="temperature"><description>'//desc//'</description><location>'//loc// &
   '</location><quantity>temperature</quantity><unit>Cel</unit></sensor>')
 end subroutine
! @nativefn NR2-M06-F0076
 subroutine x_metric(prop,val,unit,origin)
  character(len=*),intent(in)::prop,val,unit,origin
  call add('<metric><property>'//prop//'</property><value unit="'//unit//'" origin="'//origin//'">'//val//'</value></metric>')
 end subroutine
! @nativefn NR2-M06-F0077
 subroutine x_event(r,i)
  type(run_t),intent(in)::r
  integer,intent(in)::i
  integer::code,a,b,k
  character(len=12)::sev
  code=nint(r%ev(2,i));a=nint(r%ev(3,i));b=nint(r%ev(4,i))
  sev='information';if(severity_code(code)==1)sev='warning'
  if(severity_code(code)==2)sev='error'
  call add('<event id="event-'//trim(it(i-1))//'"><time><elapsed unit="s" origin="reported" termId="elapsed-time">'// &
   trim(gn(r%ev(1,i)/1000))//'</elapsed><clock>elapsed</clock></time><severity>'//trim(sev)//'</severity><type>')
  select case(code)
  case(ev_boot);call add('firmware start')
  case(ev_run_start);call add('run start')
  case(ev_selftest_ok);call add('instrument self-test passed')
  case(ev_lid_ready);call add('heated lid ready')
  case(ev_stage_start);call add('thermal stage start')
  case(ev_step_start);call add('thermal step start')
  case(ev_target_reached);call add('thermal target reached')
  case(ev_step_end);call add('thermal step end')
  case(ev_read_start);call add('optical acquisition start')
  case(ev_read_end);call add('optical acquisition end')
  case(ev_run_complete);call add('run completion')
  case(ev_run_aborted);call add('run aborted')
  case(ev_monitor_stop);call add('independent temperature monitor stop')
  case(ev_interlock);call add('hardware interlock stop')
  case(ev_time_limit);call add('protocol time limit reached')
  case(ev_host_crash);call add('host software crash')
  case default;call add('setup rejected')
  end select
  call add('</type><description>'//trim(event_name(code))//'</description>')
  select case(code)
  case(ev_lid_ready)
   call x_metric('Lid temperature at readiness',trim(ft(a/1000d0,3)),'Cel','measured')
  case(ev_stage_start)
   call x_metric('Programmed stage index (zero-based)',trim(it(a)),'1','reported')
  case(ev_step_start,ev_step_end)
   call add('<step id="step-'//trim(it(a))//'-'//trim(it(b))//'"/>')
  case(ev_target_reached)
   do k=i-1,1,-1
    if(nint(r%ev(2,k))==ev_step_start)exit
   end do
   if(k>=1)call add('<duration unit="s" origin="reported">'//trim(ft((r%ev(1,i)-r%ev(1,k))/1000,2))//'</duration>')
   if(a<r%nhold)then
    call x_metric('Programmed thermal target',trim(gn(r%hold_t(a+1))),'Cel','programmed')
   else if(b==1)then
    call x_metric('Programmed thermal target',trim(gn(ann_of(r))),'Cel','programmed')
   else
    call x_metric('Programmed thermal target','95','Cel','programmed')
   end if
  case(ev_read_start)
   call add('<cyc>'//trim(it(a))//'</cyc>');call x_metric('Plate scan identifier',trim(it(a)),'1','reported')
  case(ev_read_end)
   call x_metric('Plate scan identifier',trim(it(a)),'1','reported')
  case(ev_run_complete)
   call x_metric('Completed plate scans',trim(it(r%ncy)),'1','reported')
  case(ev_run_aborted)
   call x_metric('Native run status code',trim(it(a)),'1','reported')
   call x_metric('Completed plate scans',trim(it(b)),'1','reported')
  case(ev_monitor_stop)
   call x_metric('Primary-secondary sensor gap',trim(ft(a/1000d0,3)),'Cel','derived')
  case(ev_host_crash)
   call x_metric('Completed plate scans',trim(it(a)),'1','reported')
  end select
  call add('</event>');call newline()
 end subroutine
! @nativefn NR2-M06-F0078
 subroutine write_manifest(r)
  type(run_t),intent(in)::r
  integer::i,k,c,w,nfiles,np,stg,stp
  real(c_double)::b,y,temp,dur
  logical::rd
  character(len=10)::tar
  character(len=29)::created
  character(len=12)::avail
  nfiles=nf;used=0;np=count(r%present(1:r%nw)/=0)
  created=iso_now();if(len_trim(created)==0)created=r%finished
  if(len_trim(created)==0)created='1970-01-01T00:00:00Z'
  avail='present';if(r%recorded==0)avail='absent'
  call add('<?xml version="1.0" encoding="UTF-8"?>');call newline()
  call add('<rdmlt xmlns="urn:rdmlt:1" xmlns:rdml="http://www.rdml.org" version="1.0-draft" baseVersion="1.3" revision="2">')
  call newline()
  call add('<description>MISSY native Fortran edition simulated instrument record with telemetry</description>');call newline()
  do i=1,nfiles
   call add('<file id="'//trim(fid(i))//'"><filename>'//trim(fname(i))//'</filename><mediaType>'//trim(fmedia(i))// &
    '</mediaType><sha256>'//fsha(i)//'</sha256><byteCount>'//trim(it(fusz(i)))//'</byteCount></file>');call newline()
  end do
  call add('<core><file id="core-file"/></core>');call newline()
  call add('<documentation id="mapping"><description>All data are simulated by the native Fortran engine. Telemetry rows are'// &
   ' written every 0.1 s of simulated time (plant-model and liquid estimates every 10 s); elapsed seconds use the simulated'// &
   ' instrument clock with zero at run start. Temperature logs use the virtual block zone sensors (primary channel),'// &
   ' lid, heat sink, the mean of the independent secondary sensors (reference) and the modeled liquid temperatures.'// &
   ' The setpoint is the programmed controller target. The firmware table supplies an explicit state code map. Each'// &
   ' optical acquisition is one channel in one sequential plate scan with nominal 100 ms integrations per well. The'// &
   ' native detector model yields one count per read; the LED-on, LED-off, squared, minimum, maximum and reference'// &
   ' sums are derived from that count with an independent seeded stream so that mean LED-on minus LED-off equals the'// &
   ' recorded count exactly. Reads clipped at 65535 are reported with all samples saturated. Cq values in the core'// &
   ' are per-well observations; use the evaluation file for run validity.</description></documentation>');call newline()
  call add('<documentation id="evaluation"><description>Native run validity, control checks, thresholds and per-well'// &
   ' observations.</description><file id="analysis-file"/></documentation>');call newline()
  call add('<documentation id="assay-diagnostics"><description>Per-assay raw and baseline-corrected amplification tables'// &
   ' and settings.</description><file id="assay-detail"/></documentation>');call newline()
  call add('<documentation id="inputs"><description>Sample and control identity, plate, protocol, controller and'// &
   ' scenario inputs.</description><file id="run-input"/></documentation>');call newline()
  call add('<documentation id="scenario"><description>Deliberate training scenario provenance and model limits.'// &
   '</description><file id="fault-provenance"/></documentation>');call newline()
  call add('<documentation id="chemistry"><description>Virtual reaction-state snapshots every '//trim(it(snap_every))// &
   ' s for every occupied well (channel-1 chemistry and probe) with modeled liquid temperatures. These values come'// &
   ' from model state.</description><file id="chemistry-file"/></documentation>');call newline()
  call add('<documentation id="temperature-statistics"><description>Derived whole-run, step and per-cycle temperature'// &
   ' statistics.</description><file id="thermal-statistics"/></documentation>');call newline()
  call add('<documentation id="sop"><description>Teaching procedure record of the native analysis (one SOP per analysis'// &
   ' definition).</description><file id="sop-file"/></documentation>');call newline()
  call add('<documentation id="missy-sop"><description>MISSY SOP profile of the native analysis, with its SHA-256.'// &
   '</description><file id="missy-sop-profile"/></documentation>');call newline()
  call add('<documentation id="native-record-text"><description>Compact native record used by the native edition to'// &
   ' re-import this run.</description><file id="native-text"/></documentation>');call newline()
  call add('<software id="native-fortran"><name>MISSY native Fortran edition</name><version>RDMLt export 3</version>'// &
   '</software>');call newline()
  call add('<provenance><source id="native-record"/><software id="native-fortran"/><created>'//trim(created)// &
   '</created><documentation id="mapping"/></provenance>');call newline()
  call add('<dictionary id="scientific-quantities" version="1.0-r2"><authority>RDMLt proposal</authority>');call newline()
  call add('<term id="elapsed-time" kind="coordinate"><name>Elapsed time</name><definition>Time relative to an explicitly'// &
   ' declared origin.</definition><dimension>time</dimension><canonicalUnit>s</canonicalUnit></term>');call newline()
  call add('<term id="temperature" kind="quantity"><name>Temperature</name><definition>Observed temperature at an identified'// &
   ' sensor.</definition><dimension>temperature</dimension><canonicalUnit>Cel</canonicalUnit></term>');call newline()
  call add('</dictionary>');call newline()
  call add('<instrument id="simulated-native"><description>Simulated PCR instrument (native Fortran edition)</description>'// &
   '<model>MISSY R2 native</model><serialNumber>NATIVE-'//trim(it(r%serial))//'</serialNumber>')
  call x_sensor('block-1','Simulated Block zone 1 sensor','Block zone 1')
  call x_sensor('block-2','Simulated Block zone 2 sensor','Block zone 2')
  call x_sensor('lid','Simulated Heated lid sensor','Heated lid')
  call x_sensor('heat-sink','Simulated Heat sink sensor','Heat sink')
  call x_sensor('reference','Mean of the independent secondary block sensors','Reference sensor')
  call x_sensor('liquid-estimate-1','Simulation estimate of reaction liquid temperature','Virtual reaction liquid zone 1')
  call x_sensor('liquid-estimate-2','Simulation estimate of reaction liquid temperature','Virtual reaction liquid zone 2')
  call add('<channel id="channel-1"><description>FAM</description><active>true</active></channel><channel id="channel-2">'// &
   '<description>Yakima Yellow</description><active>true</active></channel></instrument>');call newline()
  call add('<experiment id="Native-'//trim(it(r%serial))//'"><run id="Native-'//trim(it(r%serial))//'"><description>'// &
   trim(outcome(r))//'</description><documentation id="inputs"/><documentation id="sop"/><documentation id="scenario"/><documentation id="chemistry"/>'// &
   '<documentation id="temperature-statistics"/><instrument id="simulated-native"/><dataCollectionSoftware id="native-fortran"/>')
  call newline()
  if(len_trim(r%started)>0)then
   call add('<runDate>'//trim(r%started)//'</runDate>');call newline()
  end if
  if(len_trim(r%finished)>0)then
   call add('<endDate>'//trim(r%finished)//'</endDate>');call newline()
  end if
  if(r%status==1)then
   call add('<state>completed</state>')
  else
   call add('<state>aborted</state>')
  end if
  call newline();call x_origin();call newline()
  call add('<conditions><description>Programmed stages in order. Repeat applies to all steps of the named stage. Hold times'// &
   ' start when both block zones are within 0.5 C of the target.</description><thermalCyclingConditions id="protocol"/>'// &
   '<coverTemperature unit="Cel" origin="programmed">'//trim(gn(lid_of(r)))//'</coverTemperature>')
  do k=1,r%nhold+2
   call prog_step(r,k,stg,stp,temp,dur,rd)
   if(k<=r%nhold)then
    call add('<step id="step-'//trim(it(stg))//'-'//trim(it(stp))//'"><description>hold stage '//trim(it(stg+1))//', step 1'// &
     '</description>')
   else
    call add('<step id="step-'//trim(it(stg))//'-'//trim(it(stp))//'"><description>cycle stage '//trim(it(stg+1))//', step '// &
     trim(it(stp+1))//'</description>')
   end if
   call add('<temperature unit="Cel" origin="programmed">'//trim(gn(temp))//'</temperature><duration unit="s" origin='// &
    '"programmed">'//trim(gn(dur))//'</duration><acquisition>'//trim(lg(rd))//'</acquisition>')
   if(rd)call add('<repeat>'//trim(it(r%planned_cycles))//'</repeat>')
   call add('</step>')
  end do
  call add('<documentation id="inputs"/></conditions>');call newline()
  call add('<availability section="acquisition" status="'//trim(avail)//'"/>');call newline()
  call add('<availability section="react" status="present"/>');call newline()
  call add('<availability section="temperatureLog" status="'//trim(avail)//'"/>');call newline()
  call add('<availability section="series" status="'//trim(avail)//'"/>');call newline()
  if(r%ntel_rows>0)then
   call x_log(r,'block-1',hn(r));call x_log(r,'block-2',hn(r));call x_log(r,'lid',hn(r))
   call x_log(r,'heat-sink',hn(r));call x_log(r,'reference',hn(r))
   call x_log(r,'liquid-estimate-1',rows_every(r,10));call x_log(r,'liquid-estimate-2',rows_every(r,10))
   call x_series(r,'setpoint','Block temperature setpoint','Cel','programmed')
   call x_series(r,'tec-current-1','Zone 1 thermoelectric current','A','measured')
   call x_series(r,'tec-current-2','Zone 2 thermoelectric current','A','measured')
   call x_series(r,'tec-duty-1','Zone 1 signed drive fraction','1','reported')
   call x_series(r,'tec-duty-2','Zone 2 signed drive fraction','1','reported')
   call x_series(r,'lid-duty','Lid drive fraction','1','reported')
   call x_series(r,'fan-duty','Fan drive fraction','1','reported')
   call x_series(r,'fan-speed','Fan rotational speed','1/min','measured')
   call add('<series id="firmware-state"><quantity>Firmware state and interlocks</quantity><unit>1</unit><origin>reported'// &
    '</origin>');call x_origin();call x_table_open('firmware',hn(r))
   call x_col('sequence','Firmware record sequence','1','reported');call x_col('state','Firmware state code','1','reported')
   call x_col('stage','Programmed stage index (zero-based)','1','reported')
   call x_col('step','Programmed step index (zero-based)','1','reported')
   call x_col('cycle','Firmware cycle index','1','reported');call x_col('enable','Drive enabled','1','reported')
   call x_col('lid-closed','Lid closed','1','reported');call x_col('hardware-trip','Hardware trip asserted','1','reported')
   call x_col('rails-ok','Power rails within limits','1','reported')
   call x_col('detector-busy','Optical acquisition active','1','reported')
   call add('<documentation id="mapping"/></table><annotation><rdml:property>firmware state code map</rdml:property>'// &
    '<rdml:value>0=SELFTEST, 1=LID_PREHEAT, 2=RAMP, 3=HOLD, 4=DONE, 5=IDLE</rdml:value></annotation></series>');call newline()
   call add('<series id="plant-model-state"><description>Per-second internal simulation values before sensor conversion.'// &
    '</description><quantity>Virtual plant and controller state</quantity><unit>1</unit><origin>derived</origin>')
   call x_origin();call x_table_open('plant-model',rows_every(r,10))
   call x_col('block-1','Modeled block zone 1 temperature','Cel','derived')
   call x_col('block-2','Modeled block zone 2 temperature','Cel','derived')
   call x_col('lid','Modeled lid temperature','Cel','derived');call x_col('heat-sink','Modeled heat sink temperature','Cel','derived')
   call x_col('ambient','Modeled ambient temperature','Cel','derived')
   call x_col('current-1','Modeled zone 1 TEC current','A','derived');call x_col('current-2','Modeled zone 2 TEC current','A','derived')
   call x_col('lid-current','Modeled lid heater current','A','derived')
   call x_col('watchdog-age','Time since watchdog kick (not modelled: NA)','ms','derived')
   call x_col('watchdog-ok','Watchdog permits operation (not modelled: NA)','1','derived')
   call x_col('enable-request','Firmware requests drive enable','1','derived')
   call x_col('trip-test','Overtemperature proof test asserted (not modelled: NA)','1','derived')
   call x_col('led-drive','Selected optical LED drive ratio (not modelled: NA)','1','derived')
   call x_col('destination','Controller target temperature','Cel','programmed')
   call x_col('setpoint','Current controller setpoint','Cel','programmed')
   call x_col('ramp','Controller ramp rate limit (not modelled: NA)','Cel/s','derived')
   call add('<documentation id="mapping"/></table></series>');call newline()
  end if
  do k=1,r%ncy
   do c=1,2
    call add('<acquisition id="read-'//trim(it(k))//'-'//trim(it(c))//'"><cyc>'//trim(it(k))//'</cyc><step id="step-'//trim(it(r%nhold))//'-1"/>'// &
     '<channel id="channel-'//trim(it(c))//'"/><time><elapsed unit="s" origin="reported" termId="elapsed-time">'// &
     trim(ft(r%acq_t(k)+0.02d0+0.1d0*(c-1)*np,2))//'</elapsed><clock>elapsed</clock><referenceEvent id="clock-origin"/></time>')
    call add('<table><file id="read-'//trim(it(k))//'-'//trim(it(c))//'"/><storage>TSV</storage><rowCount>'//trim(it(np))// &
     '</rowCount>')
    call x_col('reaction','Reaction position (row-major, one-based)','1','reported')
    call x_col('sequence','Firmware record sequence','1','reported')
    call x_col('start','Integration start elapsed time','s','reported')
    call x_col('end','Integration end elapsed time','s','reported')
    call x_col('integration','Integration duration','ms','reported')
    call x_col('periods','Chopped illumination periods','1','reported')
    call x_col('led-on','Sum of illuminated detector counts','{instrument_unit}','reported')
    call x_col('led-off','Sum of dark detector counts','{instrument_unit}','reported')
    call x_col('mean-net','Mean background-subtracted detector counts','{instrument_unit}','derived')
    call x_col('squared-net','Sum of squared net detector counts','{instrument_unit}2','reported')
    call x_col('min-net','Minimum net detector count','{instrument_unit}','reported')
    call x_col('max-net','Maximum net detector count','{instrument_unit}','reported')
    call x_col('saturation','Saturated detector samples','1','reported')
    call x_col('reference-on','Illuminated instrument reference sum','{instrument_unit}','reported')
    call x_col('reference-off','Dark instrument reference sum','{instrument_unit}','reported')
    call x_col('zone-start','Block zone temperature at integration start','Cel','reported')
    call x_col('zone-end','Block zone temperature at integration end','Cel','reported')
    call add('<documentation id="mapping"/></table><annotation><rdml:property>acquisition mode</rdml:property><rdml:value>'// &
     'amplification</rdml:value></annotation><annotation><rdml:property>plate scan identifier</rdml:property><rdml:value>'// &
     trim(it(k))//'</rdml:value></annotation></acquisition>');call newline()
   end do
  end do
  call add('<event id="clock-origin"><time><elapsed unit="s" origin="reported" termId="elapsed-time">0</elapsed><clock>elapsed'// &
   '</clock></time><severity>information</severity><type>Instrument clock origin</type></event>');call newline()
  do i=1,r%nev
   call x_event(r,i)
  end do
  do c=1,2
   tar='DNA-target';if(c==2)tar='YY-target'
   call add('<analysis id="analysis-'//trim(tar)//'"><name>'//trim(tar)//'</name><software id="native-fortran"/><origin>stored'// &
    ' analysis</origin><documentation id="evaluation"/><documentation id="assay-diagnostics"/></analysis>');call newline()
  end do
  do w=1,r%nw
   if(r%present(w)==0)cycle
   call add('<react id="'//trim(it(pos_of(r,w)+1))//'"><description>'//trim(well_name(r,w))//'</description><position>'// &
    trim(pos_label(pos_of(r,w)))//'</position><sample id="'//trim(sample_id(r,w))//'"/><volume unit="uL" origin="programmed">20'// &
    '</volume>')
   do c=1,2
    tar='DNA-target';if(c==2)tar='YY-target'
    call add('<data><tar id="'//trim(tar)//'"/>')
    do i=1,2
     if(i==1)then
      call add('<signal id="signal-'//trim(it(pos_of(r,w)+1))//'-'//trim(tar)//'-amp"><type>background corrected</type>')
     else
      call add('<signal id="signal-'//trim(it(pos_of(r,w)+1))//'-'//trim(tar)//'-baseline"><type>baseline corrected</type>')
     end if
     call add('<channel id="channel-'//trim(it(c))//'"/><unit>{instrument_unit}</unit><origin>derived</origin>')
     b=baseline(r,w,c)
     do k=1,r%ncy
      y=yv(r,k,w,c)
      if(y<0)then
       call add('<adp><cyc>'//trim(it(k))//'</cyc><valid>false</valid></adp>');cycle
      end if
      if(i==2)y=y-b
      call add('<adp><cyc>'//trim(it(k))//'</cyc><fluor unit="{instrument_unit}" origin="derived">'//trim(gn(y))// &
       '</fluor><elapsed unit="s" origin="reported">'//trim(ft(rt0(k,w,c)+0.1d0,2))//'</elapsed><acquisition id="read-'// &
       trim(it(k))//'-'//trim(it(c))//'"/>')
      if(rok(k,w,c).and.rs(6,k,w,c)>0)call add('<valid>false</valid>')
      call add('</adp>')
     end do
     call add('<documentation id="mapping"/></signal>')
    end do
    call add('<analysis id="analysis-'//trim(tar)//'"/><analysisSettings><description>threshold, fixed '//trim(gn(thr_of(r)))// &
     ' counts above the mean of cycles 1 to 5</description><cqDetectionMethod>other</cqDetectionMethod><threshold'// &
     ' unit="{instrument_unit}" origin="reported">'//trim(gn(thr_of(r)))//'</threshold><baseline><automatic>false</automatic>'// &
     '<startCycle>1</startCycle><endCycle>5</endCycle><method>Mean of cycles 1 to 5</method></baseline><documentation'// &
     ' id="evaluation"/></analysisSettings><result>')
    if(cqv(r,w,c)>0)then
     call add('<state>determined</state><cq unit="1" origin="stored analysis">'//trim(ft(cqv(r,w,c),4))//'</cq>')
    else
     call add('<state>undetermined</state>')
    end if
    if(r%ncy>0)then
     if(yv(r,r%ncy,w,c)>=0)call add('<endPt unit="{instrument_unit}" origin="stored analysis">'// &
      trim(gn(yv(r,r%ncy,w,c)))//'</endPt>')
    end if
    if(cqv(r,w,c)>0)then
     call add('<call>detected</call>')
    else
     call add('<call>not detected</call>')
    end if
    call add('</result></data>')
   end do
   call add('</react>');call newline()
  end do
  call add('</run></experiment>');call newline()
  call add('</rdmlt>');call newline()
 end subroutine
! @nativefn NR2-M06-F0080
 function sop_profile(id) result(o)
  ! Canonical MISSY SOP profile (keys sorted, generated with MISSY's sopNormalise/sopCanonical);
  ! the procedure id is inserted at three places before hashing.
  character(len=*),intent(in)::id
  character(len=:),allocatable::o
  o=sp1//trim(id)//sp2//trim(id)//sp3//trim(id)//sp4
 end function
! @nativefn NR2-M06-F0081
 function sop_definition(r) result(o)
  ! Canonical JSON (sorted keys, no spaces) of the analysis definition; its SHA-256 names the SOP.
  type(run_t),intent(in)::r
  character(len=:),allocatable::o
  integer::k,stg,stp
  real(c_double)::temp,dur
  logical::rd
  o='{"design":{"extractions":'//trim(it(r%ne))//',"replicates":'//trim(it(r%nr))//',"starting_copies":'// &
   trim(it(r%copies))//'},"kind":"native-generic-probe-run","layout":[{"control":"PC","role":"positive","wells":1},'// &
   '{"control":"NTC","role":"ntc","wells":1},{"control":"NAC","role":"nac","wells":1},{"control":"LOD","role":"positive",'// &
   '"wells":2}],"lid_c":'//trim(gn(r%lid_t))//',"procedure":"MISSY native Fortran edition","programme":['
  do k=1,r%nhold
   call prog_step(r,k,stg,stp,temp,dur,rd)
   o=o//'{"repeats":1,"steps":[{"hold_s":'//trim(gn(dur))//',"temp_c":'//trim(gn(temp))//'}],"type":"hold"},'
  end do
  call prog_step(r,r%nhold+1,stg,stp,temp,dur,rd)
  o=o//'{"repeats":'//trim(it(r%planned_cycles))//',"steps":[{"hold_s":'//trim(gn(dur))//',"temp_c":'//trim(gn(temp))//'},'
  call prog_step(r,r%nhold+2,stg,stp,temp,dur,rd)
  o=o//'{"hold_s":'//trim(gn(dur))//',"read":true,"temp_c":'//trim(gn(temp))//'}],"type":"cycle"}],'// &
   '"reaction_mixes":["DNA-target","YY-target"],"rules":{"baseline_window":[1,5],"cq_method":"threshold","threshold":'// &
   trim(gn(thr_of(r)))//'},"stage":"detection"}'
 end function
! @nativefn NR2-M06-F0082
 function sop_identity(r) result(id)
  type(run_t),intent(in)::r
  character(len=16)::id
  character(len=:),allocatable::d
  character(len=64)::h
  integer::i
  d=sop_definition(r);h=sha256_hex(d,len(d))
  do i=1,12
   if(h(i:i)>='a'.and.h(i:i)<='f')h(i:i)=achar(iachar(h(i:i))-32)
  end do
  id='SOP-'//h(1:12)
 end function
! @nativefn NR2-M06-F0083
 subroutine write_sop(r,id)
  ! Teaching procedure record (R2 analysis/sop.json layout) for the native generic probe run.
  type(run_t),intent(in)::r
  character(len=*),intent(in)::id
  integer::k,stg,stp
  real(c_double)::temp,dur,total
  logical::rd
  used=0
  call add('{"format":"missy-native-sop/1","experimentalDesign":null,"sop_id":"'//trim(id)//'","notice":"Teaching document'// &
   ' generated from the simulated native Fortran edition. Not a validated method.","identity_basis":"SHA-256 of the canonical'// &
   ' JSON in ''definition''. Run names, times, sample names and seeds are not part of it, so every run of the same analysis'// &
   ' carries this SOP.","title":"Detection of a generic DNA target with a hydrolysis probe (native model)","purpose":'// &
   '"Teaching run: detect the target in the extracts of one sample with positive, no-template, no-amplification and LOD'// &
   ' controls.",')
  call add('"scope":{"species":null,"copies_per_reaction":'//trim(it(r%copies))//',"template":"generic DNA, nominal copies"},'// &
   '"methods":[{"id":"DNA-target","title":"Generic DNA target (native model)","role":"target","target":"generic DNA target",'// &
   '"dye":"FAM","channel":1,"source":"native generic hydrolysis-probe model","amplicon_bp":'//trim(gn(r%assay_s(3,1)))// &
   ',"detection":"hydrolysis probe, FAM","reaction_volume_ul":20,"final_concentrations_nmol_per_l":{"forward":'// &
   trim(gn(r%assay_s(7,1)))//',"reverse":'//trim(gn(r%assay_s(7,1)))//',"probe":'//trim(gn(r%probe_f(1)))//'},'// &
   '"oligonucleotides":null,"reference_material":null,"reported_efficiency_percent":'//trim(gn(100*r%assay_s(8,1)))// &
   ',"reported_limits":null,"cycling_in_record":{"ung_50C_s":'//trim(gn(merge(r%hold_s(1),0d0,r%prog==1)))// &
   ',"activation_95C_s":'//trim(gn(merge(r%hold_s(2),r%hold_s(1),r%prog==1)))//',"denaturation_95C_s":'// &
   trim(gn(r%den_s))//',"annealing_C":'//trim(gn(r%ann_t))//',"annealing_s":'//trim(gn(r%ann_hold))//',"cycles":'// &
   trim(it(r%planned_cycles))//'}},{"id":"YY-target","title":"Second detection channel","role":"reference","target":'// &
   '"second detection channel","dye":"Yakima Yellow","channel":2}],"reaction_mixes_per_reaction":[{"mix":"DNA-target",'// &
   '"primer_nM":'//trim(gn(r%assay_s(7,1)))//',"probe_nM":'//trim(gn(r%probe_f(1)))//',"hot_start":'// &
   trim(lg(r%assay_s(12,1)/=0))//',"volume_ul":20}],"dna_solutions":[{"name":"PC","copies":10000},{"name":"PC at LOD",'// &
   '"copies":15},{"name":"NTC","copies":0},{"name":"NAC","copies":0}],"calibrant_series":[],')
  call add('"layout":[{"mix":"DNA-target","per_sample":{"extractions":'//trim(it(r%ne))//',"pcr_replicates":'//trim(it(r%nr))// &
   '},"run_controls":[{"role":"positive","control":"PC","wells":1},{"role":"ntc","control":"NTC","wells":1},{"role":"nac",'// &
   '"control":"NAC","wells":1},{"role":"positive","control":"LOD","wells":2}]}],"plate_format":"384 wells, 16 rows x 24'// &
   ' columns; reactions in plate order","programme":{"lid_C":'//trim(gn(r%lid_t))//',"stages":[')
  total=0
  do k=1,r%nhold
   call prog_step(r,k,stg,stp,temp,dur,rd);total=total+dur
   call add('{"type":"hold","repeats":1,"steps":[{"temp_c":'//trim(gn(temp))//',"hold_s":'//trim(gn(dur))//'}]},')
  end do
  call prog_step(r,r%nhold+1,stg,stp,temp,dur,rd);total=total+dur*r%planned_cycles
  call add('{"type":"cycle","repeats":'//trim(it(r%planned_cycles))//',"steps":[{"temp_c":'//trim(gn(temp))//',"hold_s":'// &
   trim(gn(dur))//'},')
  call prog_step(r,r%nhold+2,stg,stp,temp,dur,rd);total=total+dur*r%planned_cycles
  call add('{"temp_c":'//trim(gn(temp))//',"hold_s":'//trim(gn(dur))//',"read":true}]}],"estimated_hold_time_s":'// &
   trim(gn(total))//',"estimated_instrument_time_s":'//trim(it(nint(r%elapsed)))//'},')
  call add('"procedure":["Prepare the reaction mix without template.","Dispense the mix; add water to the no-template'// &
   ' control first, then the sample extracts, then the positive and LOD controls.","Run the programme; read fluorescence at'// &
   ' the end of each annealing hold.","Analyse with a fixed net threshold above the mean of cycles 1 to 5."],"acceptance":'// &
   '["The no-template and no-amplification controls show no amplification.","The positive control amplifies.","At least one'// &
   ' LOD control is detected."],"evaluation":"Target detected when at least one replicate of an extract crosses the threshold;'// &
   ' controls decide run validity.","definition":'//sop_definition(r)//',"methods_source":"native generic hydrolysis-probe'// &
   ' model","software":"MISSY native Fortran edition"}')
 end subroutine
! @nativefn NR2-M06-F0084
 subroutine write_missy_sop(id)
  character(len=*),intent(in)::id
  character(len=:),allocatable::c
  c=sop_profile(id)
  used=0;call add(c(1:len(c)-1)//',"locked":false,"sha256":"'//sha256_hex(c,len(c))//'"}')
 end subroutine
! ---------------------------------------------------------------- package
! @nativefn NR2-M06-F0090
 subroutine export_run(r)
  type(run_t),intent(in)::r
  integer::k,c
  real(c_double)::stats(4)
  character(len=40)::nm
  character(len=16)::sid
  character(len=24),parameter::tn(15)=[character(len=24)::'block-1','block-2','lid','heat-sink','reference','setpoint', &
   'tec-duty-1','tec-duty-2','tec-current-1','tec-current-2','lid-duty','fan-duty','fan-speed','liquid-estimate-1', &
   'liquid-estimate-2']
  integer,parameter::tcol(15)=[3,4,5,6,7,2,8,9,12,13,10,11,14,20,21],tdec(15)=[4,4,4,4,4,3,4,4,4,4,4,4,0,4,4]
  real(c_double),parameter::tsc(15)=[1d0,1d0,1d0,1d0,1d0,1d0,1d0,1d0,1d-3,1d-3,1d0,1d0,1d0,1d0,1d0]
  output_size=0;nf=0;overflow=.false.
  call compute_reads(r)
  call write_core(r);call put_entry('rdml_data.xml','core-file','application/xml',.true.)
  call write_record(r);call put_entry('native-run.txt','native-text','text/plain',.false.)
  call write_provenance(r);call put_entry('source/fault-provenance.json','fault-provenance','application/json',.true.)
  call write_run_input(r);call put_entry('source/run-input.json','run-input','application/json',.true.)
  call write_record_json(r);call put_entry('source/simulator-record.json','native-record','application/json',.true.)
  call write_chemistry(r);call put_entry('source/simulator-chemistry.json','chemistry-file','application/json',.true.)
  call write_temperature_statistics(r,stats)
  call put_entry('analysis/temperature-statistics.json','thermal-statistics','application/json',.true.)
  sid=sop_identity(r)
  call write_evaluation(r,stats,sid);call put_entry('analysis/evaluation.json','analysis-file','application/json',.true.)
  call write_assay_detail(r,stats);call put_entry('analysis/assay-detail.json','assay-detail','application/json',.true.)
  call write_sop(r,sid);call put_entry('analysis/sop.json','sop-file','application/json',.true.)
  call write_missy_sop(sid);call put_entry('analysis/missy.sop.json','missy-sop-profile','application/json',.true.)
  if(hn(r)>0)then
   do k=1,15
    if(k>=14)then
     call tsv_liquid(r,tcol(k))
    else
     call tsv_value(r,tcol(k),tsc(k),tdec(k))
    end if
    call put_entry('telemetry/'//trim(tn(k))//'.tsv',trim(tn(k)),'text/tab-separated-values',.true.)
   end do
   call tsv_firmware(r);call put_entry('telemetry/firmware.tsv','firmware','text/tab-separated-values',.true.)
   call tsv_plant(r);call put_entry('telemetry/plant-model.tsv','plant-model','text/tab-separated-values',.true.)
  end if
  do k=1,r%ncy
   do c=1,2
    nm='read-'//trim(it(k))//'-'//trim(it(c))
    call tsv_read(r,k,c);call put_entry('acquisitions/'//trim(nm)//'.tsv',trim(nm),'text/tab-separated-values',.true.)
   end do
  end do
  call write_manifest(r);call put_entry('rdmlt_data.xml','companion','application/xml',.true.)
  if(overflow)then
   output_size=0;return
  end if
  call put_directory()
  call set_name('MISSY_native_run_'//trim(it(r%serial))//'.rdmlt')
 end subroutine
! @nativefn NR2-M06-F0091
 subroutine export_all_runs()
  ! Outer ZIP (stored) of individually complete RDMLt packages; buffers grow with the number of runs.
  integer(c_int8_t),allocatable::acc(:),pkg(:),tmp(:)
  character(len=40)::names(maxruns)
  integer::offsets(maxruns),sizes(maxruns),i,j,n,m,nl
  integer(c_int64_t)::crcs(maxruns)
  if(nh==0)then
   output_size=0;message='No stored virtual runs to export.';return
  end if
  allocate(acc(cap));m=0
  do i=1,nh
   call export_run(history(i));n=output_size
   if(n==0)then
    output_size=0;message='A run could not be packaged. Export runs individually.';return
   end if
   allocate(pkg(n));pkg=output(1:n)
   names(i)='MISSY_native_run_'//trim(it(i))//'.rdmlt';nl=len_trim(names(i))
   crcs(i)=crc_bytes(pkg,n);sizes(i)=n;offsets(i)=m
   do while(m+n+nl+64>size(acc))
    allocate(tmp(2*size(acc)));tmp(1:m)=acc(1:m);call move_alloc(tmp,acc)
   end do
   call put_local(acc,m,names(i),crcs(i),n)
   acc(m+1:m+n)=pkg;m=m+n;deallocate(pkg)
  end do
  output_size=0;call ensure_output(m+200*(nh+1));output(1:m)=acc(1:m);output_size=m
  call zip_directory(names(1:nh),offsets(1:nh),crcs(1:nh),sizes(1:nh),nh)
  call set_name('MISSY_all_virtual_runs.rdmlt.zip')
  message='All '//trim(it(nh))//' stored virtual runs packaged as individual RDMLt files.'
 contains
  subroutine put_local(a,m,name,crc,n)
   integer(c_int8_t),intent(inout)::a(:)
   integer,intent(inout)::m
   character(len=*),intent(in)::name
   integer(c_int64_t),intent(in)::crc
   integer,intent(in)::n
   integer::k
   call le(a,m,int(z'04034B50',c_int64_t),4);call le(a,m,20_c_int64_t,2);call le(a,m,0_c_int64_t,2);call le(a,m,0_c_int64_t,2)
   call le(a,m,2162688_c_int64_t,4);call le(a,m,crc,4);call le(a,m,int(n,c_int64_t),4);call le(a,m,int(n,c_int64_t),4)
   call le(a,m,int(len_trim(name),c_int64_t),2);call le(a,m,0_c_int64_t,2)
   do k=1,len_trim(name)
    m=m+1;a(m)=int(iachar(name(k:k))-merge(256,0,iachar(name(k:k))>127),c_int8_t)
   end do
  end subroutine
  subroutine le(a,m,v,nb)
   integer(c_int8_t),intent(inout)::a(:)
   integer,intent(inout)::m
   integer(c_int64_t),intent(in)::v
   integer,intent(in)::nb
   integer::k,b
   do k=0,nb-1
    b=int(iand(shiftr(v,8*k),255_c_int64_t));if(b>127)b=b-256
    m=m+1;a(m)=int(b,c_int8_t)
   end do
  end subroutine
 end subroutine
end module
