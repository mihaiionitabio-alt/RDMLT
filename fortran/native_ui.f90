! SPDX-License-Identifier: GPL-3.0-only
! Immediate-mode native application UI. Browser knows no screens, fields, buttons or chart rules.
module native_ui
 use iso_c_binding
 use native_model
 use native_io
 use r2_framebuffer
 use r2_font
 use stdlib_browser_stats
 implicit none
 integer,save::width=1280,height=820,page=0,graphmode=0,catalog_page=0,edit_param=0,nb=0,buttons(7,80)=0,request=0
 character(len=24),save::edit_buffer=''
 logical,save::show_group(5,maxruns)=.true.,show_name(maxw,maxruns)=.true.
 logical,save::dirty=.true.,dragging=.false.
 real(c_double),save::mx=-1,my=-1,zoom=1,pan=0,lastx=0
 integer(c_int32_t),parameter::bg=int(z'FF10090D',c_int32_t),orange=int(z'FF649BFF',c_int32_t)
 integer(c_int32_t),parameter::purple=int(z'FFC99ACB',c_int32_t),ink=int(z'FFF3E0EA',c_int32_t)
 integer(c_int32_t),parameter::panel=int(z'FF281723',c_int32_t),cyan=int(z'FFDCDC60',c_int32_t)
contains
! @nativefn NR2-M03-F0001
 subroutine label(s,x,y,color,scale)
  character(len=*),intent(in)::s
  integer,intent(in)::x,y,scale
  integer(c_int32_t),intent(in)::color
  integer::i,g,xx,yy,k,l,advance,level,copies
  advance=0;copies=scale*render_scale/2
  do i=1,len_trim(s)
   g=glyph(iachar(s(i:i)))
   do yy=0,font_height-1
    do xx=0,font_width-1
     level=int(iand(shiftr(glyph_rows((g-1)*font_height+yy+1),2*xx),3_c_int64_t))
     if(level==0)cycle
     do k=0,copies-1
      do l=0,copies-1
       call pixel_raw(x*render_scale+advance+xx*copies+k,y*render_scale+yy*copies+l,color,level/3d0)
      end do
     end do
    end do
   end do
   advance=advance+glyph_advance(g)*copies
  end do
 end subroutine
! @nativefn NR2-M03-F0041
 subroutine soft_panel(x,y,w,h,r,c)
  integer,intent(in)::x,y,w,h,r
  integer(c_int32_t),intent(in)::c
  integer::i,pad
  real(c_double)::dy
  do i=0,h-1
   pad=0
   if(i<r.or.i>=h-r)then
    dy=real(r,c_double)-.5d0-min(i,h-i-1)
    pad=ceiling(r-sqrt(max(0d0,r*r-dy*dy)))
   end if
   call rectangle(x+pad,y+i,max(0,w-2*pad),1,c)
  end do
 end subroutine
! @nativefn NR2-M03-F0042
 subroutine home_card(id,title,detail,number,x,y,w,enabled,accent)
  integer,intent(in)::id,number,x,y,w
  character(len=*),intent(in)::title,detail
  logical,intent(in)::enabled
  integer(c_int32_t),intent(in)::accent
  integer(c_int32_t)::edge
  edge=accent;if(.not.enabled)edge=purple
  call button(id,0,'',x,y,w,110,enabled)
  call soft_panel(x,y,w,110,20,edge)
  call soft_panel(x+2,y+10,w-4,98,18,panel)
  call label('0'//trim(integer_text(number)),x+18,y+25,edge,2)
  call label(title,x+75,y+25,ink,2)
  call label(detail,x+20,y+76,purple,1)
 end subroutine
! @nativefn NR2-M03-F0002
 subroutine button(id,arg,s,x,y,w,h,enabled)
  integer,intent(in)::id,arg,x,y,w,h
  logical,intent(in)::enabled
  character(len=*),intent(in)::s
  integer(c_int32_t)::c
  nb=nb+1;buttons(:,nb)=[id,arg,x,y,w,h,0];if(enabled)buttons(7,nb)=1
  c=panel;if(enabled)c=purple
  if(id==2.and.enabled)c=orange
  call soft_panel(x,y,w,h,min(20,h/2),c)
  if(enabled)then
   call label(s,x+12,y+max(0,(h-18)/2),bg,1)
  else
   call label(s,x+12,y+max(0,(h-18)/2),ink,1)
  end if
 end subroutine
! @nativefn NR2-M03-F0003
 subroutine draw_chart(r)
  type(run_t),intent(in)::r
  real(c_double)::cfg(10),x(30000),y(30000),sty(40),mouse(3),sp(1),lo,hi,span,x0,x1,v
  integer(c_int32_t)::colors(26)
  integer(c_int)::starts(21),lab(64)
  integer::n,nc,w,i,k,nl
  type(c_ptr)::p
  colors=orange;colors(1:6)=[bg,int(z'FF392A40',c_int32_t),purple,ink,cyan,panel]
  starts=0;sty=0;sty(1:39:2)=1.4d0;mouse=[mx,my,-1d100];sp=0;n=0;lab=0
  if(graphmode/=1)then
   nc=r%nw;sty(2:40:2)=-1
   do w=1,nc
    starts(w)=n
    if(.not.running.and.selected>0)then
     if(.not.result_visible(r,w))cycle
    end if
    do i=1,r%ncy
     n=n+1;x(n)=i;y(n)=r%y(i,w)
     if(graphmode==2)y(n)=r%y2(i,w)
     if(y(n)<0.or.r%present(w)==0)y(n)=ieee_value(1d0,ieee_quiet_nan)
    end do
    colors(w+6)=orange
    if(w==r%ne*r%nr+1)colors(w+6)=purple
    if(w==r%ne*r%nr+2)colors(w+6)=rgba(75,220,135,255)
    if(w==r%ne*r%nr+3)colors(w+6)=cyan
    if(w>=r%ne*r%nr+4)colors(w+6)=rgba(255,210,70,255)
   end do
   starts(nc+1)=n;x0=1;x1=45;lo=200;hi=1600
   if(n>0)then
    lo=200;hi=1600
    do i=1,n
     if(.not.ieee_is_finite(y(i)))cycle
     lo=min(lo,max(0d0,y(i)-30));hi=max(hi,y(i)+50)
    end do
   end if
   call codes('Cycle',lab,nl);nl=nl+1;lab(nl)=0
   call more_codes('Raw counts',lab,nl)
  else
   nc=3
   do w=1,nc
    starts(w)=n
    do i=1,r%nt
     n=n+1;x(n)=r%trace(1,i)/60
     k=2;if(w==2)k=4
     if(w==3)k=6
     y(n)=r%trace(k,i)
    end do
   end do
   starts(4)=n;colors(7:9)=[orange,cyan,purple]
   x0=0;x1=max(1d0,r%elapsed/60);lo=20;hi=110
   call codes('Instrument minutes',lab,nl);nl=nl+1;lab(nl)=0
   call more_codes('Block C',lab,nl)
  end if
  span=(x1-x0)/zoom;v=(x0+x1)/2+pan*(x1-x0);x0=v-span/2;x1=v+span/2
  cfg=[real(width,c_double),real(height,c_double),x0,x1,lo,hi,80d0,170d0,real(width-110,c_double),real(height-360,c_double)]
  p=r2_render_chart(cfg,x(1:n),y(1:n),starts(1:nc+1),nc,n,colors(1:nc+6),sty(1:2*nc),lab(1:nl),nl,mouse,sp,0)
 end subroutine
! @nativefn NR2-M03-F0004
 subroutine codes(s,a,n)
  character(len=*),intent(in)::s
  integer(c_int),intent(out)::a(*)
  integer,intent(out)::n
  integer::i
  n=len_trim(s)
  do i=1,n
   a(i)=iachar(s(i:i))
  end do
 end subroutine
! @nativefn NR2-M03-F0005
 subroutine more_codes(s,a,n)
  character(len=*),intent(in)::s
  integer(c_int),intent(inout)::a(*)
  integer,intent(inout)::n
  integer::i
  do i=1,len_trim(s)
   n=n+1;a(n)=iachar(s(i:i))
  end do
 end subroutine
! @nativefn NR2-M03-F0043
 integer function result_group(r,w)result(g)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  integer::s
  s=r%ne*r%nr;g=1
  if(w==s+1)g=2
  if(w==s+2)g=3
  if(w==s+3)g=4
  if(w>=s+4)g=5
 end function
! @nativefn NR2-M03-F0044
 function result_name(r,w)result(name)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  character(len=50)::name
  integer::s,e,rep
  s=r%ne*r%nr
  if(w<=s)then
   e=(w-1)/r%nr+1;rep=mod(w-1,r%nr)+1
   name='Sample 1 / E'//trim(integer_text(e))//' / R'//trim(integer_text(rep))
  else if(w==s+1)then
   name='Positive control'
  else if(w==s+2)then
   name='NTC'
  else if(w==s+3)then
   name='Extraction blank'
  else
   name='LOD '//trim(integer_text(w-s-3))
  end if
 end function
! @nativefn NR2-M03-F0045
 logical function result_visible(r,w)result(v)
  type(run_t),intent(in)::r
  integer,intent(in)::w
  v=.true.;if(selected<1)return
  v=show_group(result_group(r,w),selected).and.show_name(w,selected)
 end function
! @nativefn NR2-M03-F0046
 subroutine check_button(id,arg,s,x,y,w,checked)
  integer,intent(in)::id,arg,x,y,w
  character(len=*),intent(in)::s
  logical,intent(in)::checked
  real(c_double)::clip(4)
  call button(id,arg,'',x,y,w,42,.true.)
  call rectangle(x+12,y+10,22,22,panel)
  if(checked)then
   clip=[real(x,c_double),real(y,c_double),real(x+w,c_double),real(y+42,c_double)]
   call aa_line(real(x+16,c_double),real(y+20,c_double),real(x+22,c_double),real(y+26,c_double),cyan,2d0,.false.,clip)
   call aa_line(real(x+22,c_double),real(y+26,c_double),real(x+31,c_double),real(y+14,c_double),cyan,2d0,.false.,clip)
  end if
  call label(s,x+45,y+13,bg,1)
 end subroutine
! @nativefn NR2-M03-F0047
 subroutine draw_result_selection(r)
  type(run_t),intent(in)::r
  character(len=20),parameter::groups(5)=[character(len=20)::'Samples','Positive controls','NTC','Extraction blanks','LOD controls']
  integer::i,x,y,cw
  call label('Displayed groups and names',60,158,orange,2)
  call label('Both the group and the individual name must be checked. Display only; stored data and analysis stay unchanged.',60,200,ink,1)
  cw=(width-160)/5
  do i=1,5
   call check_button(34,i,trim(groups(i)),60+(i-1)*(cw+10),240,cw,show_group(i,selected))
  end do
  cw=(width-150)/2
  do i=1,r%nw
   x=60+mod(i-1,2)*(cw+30);y=315+((i-1)/2)*47
   call check_button(36,i,trim(result_name(r,i)),x,y,cw,show_name(i,selected))
  end do
  call button(37,0,'Show all',60,height-122,180,44,.true.)
  call button(38,0,'Clear all',260,height-122,180,44,.true.)
  call button(39,0,'Back to Results',460,height-122,220,44,.true.)
 end subroutine
! @nativefn NR2-M03-F0007
 subroutine draw_results(r)
  type(run_t),intent(in)::r
  integer::w,y,s,e,rep,v,pos,row
  character(len=50)::role
  character(len=80)::status
  s=r%ne*r%nr;v=verdict(r)
  status='Partial / stopped: not accepted'
  if(v==1)status='Accepted by reduced teaching controls'
  if(v==2)status='Control check failed: reject / investigate'
  if(v==4)status='Deliberate scenario: review required; controls passed'
  call label(trim(status),60,195,orange,2)
  call label('Position   Identity / role                Extraction     Replicate       FAM Cq          YY Cq',60,250,purple,1)
  row=0
  do w=1,r%nw
   if(.not.result_visible(r,w))cycle
   y=275+row*25;row=row+1;role='Sample';e=0;rep=0
   if(w<=s)then
    e=(w-1)/r%nr+1;rep=mod(w-1,r%nr)+1
   end if
   if(w==s+1)role='Positive control'
   if(w==s+2)role='NTC'
   if(w==s+3)role='Extraction blank'
   if(w>=s+4)role='LOD control'
   role=result_name(r,w)
   pos=recorded_position(r%scenario,r%params,w,r%ne,r%nr)
   call label(achar(65+pos/24)//trim(integer_text(mod(pos,24)+1)),60,y,ink,1);call label(role,150,y,ink,1)
   if(e>0)call label(trim(integer_text(e)),420,y,ink,1)
   if(rep>0)call label(trim(integer_text(rep)),540,y,ink,1)
   status='Not detected';if(r%cq(w)>0)status=real_text(r%cq(w),2)
   if(r%present(w)==0)status='Missing control'
   call label(trim(status),660,y,ink,1)
   status='Not detected';if(r%cq2(w)>0)status=real_text(r%cq2(w),2)
   if(r%present(w)==0)status='Missing control'
   call label(trim(status),860,y,ink,1)
  end do
  if(row==0)call label('No names selected. Open Display selection to restore the view.',60,300,cyan,1)
 end subroutine
! @nativefn NR2-M03-F0008
 subroutine draw_profile()
  character(len=36),parameter::names(8)=[character(len=36)::'Number of runs','DNA extractions','PCR replicates per extraction', &
   'Starting DNA copies','Net threshold counts','Seed','Sensor fault: 0 normal / 1 bias','Simulation acceleration']
  integer::i,y,vals(8)
  vals=[p_runs,p_ext,p_rep,p_copies,p_threshold,p_seed,p_fault,p_speed]
  do i=1,8
   y=165+(i-1)*52
   call label(trim(names(i)),60,y+12,ink,1)
   call label(trim(integer_text(vals(i))),470,y+8,orange,2)
   call button(14,i,'-',650,y,64,42,.not.running)
   call button(15,i,'+',730,y,64,42,.not.running)
  end do
  call label('Fixed generic probe assay: positive, NTC, extraction blank, and 2 LOD wells.',60,605,purple,1)
  call button(12,0,'Download profile',60,655,180,46,.true.)
  call button(13,0,'Load profile / native RDMLt',260,655,250,46,.not.running)
  call button(2,0,'Start batch',530,655,180,46,.not.running.and.scenario_available(p_scenario))
  call button(20,0,'qPCR error scenarios',730,655,240,46,.not.running)
 end subroutine
! @nativefn NR2-M03-F0026
 subroutine wrapped(s,x,y,chars,color)
  character(len=*),intent(in)::s
  integer,intent(in)::x,y,chars
  integer(c_int32_t),intent(in)::color
  integer::a,b,k,row
  a=1;row=0
  do while(a<=len_trim(s))
   b=min(len_trim(s),a+chars-1)
   if(b<len_trim(s))then
    do k=b,a+1,-1
     if(s(k:k)==' ')then
      b=k;exit
     end if
    end do
   end if
   call label(s(a:b),x,y+row*22,color,1);a=b+1;row=row+1
  end do
 end subroutine
! @nativefn NR2-M03-F0027
 subroutine draw_catalogue()
  integer::i,code,x,y,col,row,cw
  call label('qPCR error scenarios - editable Fortran configurations',60,128,orange,2)
  call label('48 fault cases + 2 analysis cases. Melt case is editable but cannot run on this probe model.',60,173,ink,1)
  cw=(width-150)/2
  do i=1,6
   code=catalog_page*6+i;if(code>51)exit
   col=mod(i-1,2);row=(i-1)/2;x=60+col*(cw+30);y=220+row*125
   call button(21,code,'',x,y,cw,110,.not.running)
   call label(trim(integer_text(code))//' | '//trim(scenario_ids(code))//' | '//trim(scenario_kinds(code)),x+12,y+8,bg,1)
   call wrapped(trim(scenario_titles(code)),x+12,y+37,max(25,(cw-30)/9),bg)
  end do
  call button(22,0,'Previous',60,625,140,44,catalog_page>0)
  call label(trim(integer_text(catalog_page+1))//' / 9',220,635,ink,1)
  call button(23,0,'Next',320,625,140,44,catalog_page<8)
  call button(26,0,'Normal run',480,625,180,44,.not.running)
  call button(12,0,'Save profile + all 51 settings',680,625,310,44,.true.)
  call label('Select a card to edit. Settings apply to subsequent runs; previous records keep their own snapshot.',60,695,purple,1)
 end subroutine
! @nativefn NR2-M03-F0028
 subroutine draw_scenario()
  integer::i,y,row
  row=0
  if(p_scenario<1)return
  call label(trim(integer_text(p_scenario))//' | '//trim(scenario_ids(p_scenario))//' | '//trim(scenario_kinds(p_scenario)),60,127,cyan,1)
  call wrapped(trim(scenario_titles(p_scenario)),60,158,max(40,(width-130)/13),orange)
  do i=1,6
   if(len_trim(parameter_names(i,p_scenario))==0)cycle
   y=230+row*56;row=row+1
   call label(trim(parameter_names(i,p_scenario)),60,y+10,ink,1)
   call button(30,i,trim(real_text(scenario_bank(i,p_scenario),3)),550,y,210,42,.not.running.and.scenario_step(i,p_scenario)>0)
   if(edit_param==i)call label(trim(edit_buffer)//'_',560,y+11,cyan,1)
   call button(24,i,'-',780,y,64,42,.not.running.and.scenario_step(i,p_scenario)>0)
   call button(25,i,'+',860,y,64,42,.not.running.and.scenario_step(i,p_scenario)>0)
   call label(trim(real_text(scenario_min(i,p_scenario),3))//' .. '//trim(real_text(scenario_max(i,p_scenario),3)),950,y+10,purple,1)
  end do
  call label('Click a value to type; Enter applies, Escape cancels. Or use - / +.',60,575,cyan,1)
  call label('Scope: 0 default, 1 all, 2 samples, 3 controls, 4 edge, 5 patch, 6 NTC + blank.',60,600,ink,1)
  call label('Mask overrides scope; controls follow samples. Precision: 0.001.',60,625,ink,1)
  call button(20,0,'Back to catalogue',60,655,220,44,.true.)
  call button(27,0,'Reset this case',300,655,180,44,.not.running)
  call button(12,0,'Save all settings',500,655,180,44,.true.)
  call button(2,0,'Run selected case',700,655,220,44,.not.running.and.scenario_available(p_scenario))
  if(p_scenario==21)call label('Not reproducible: requires an intercalating-dye melt acquisition workflow.',60,710,orange,1)
 end subroutine
! @nativefn NR2-M03-F0009
 function native_render() result(ptr) bind(c)
  type(c_ptr)::ptr
  integer::v,cw,shown_cycle,i
  real(c_double)::a0,a1,b0,b1,clipmark(4)
  nb=0
  shown_cycle=current%ncy
  if(.not.running.and.selected>0)shown_cycle=history(selected)%ncy
  if(page==1.and.(running.or.selected==0))then
   call draw_chart(current)
  else if(page==1.and.selected>0)then
   call draw_chart(history(selected))
  else
   call allocate_frame(width,height,bg)
  end if
  call rectangle(0,0,width,16,orange)
  call soft_panel(0,16,38,130,19,orange)
  call rectangle(0,155,30,130,purple);call rectangle(0,295,30,110,rgba(155,145,255,255))
  call rectangle(0,415,30,100,orange);call rectangle(0,525,30,max(0,height-610),purple)
  call rectangle(48,108,width-280,4,purple)
  call label('MISSY R2',65,24,orange,3)
  call label('F O R T R A N',68,77,purple,1)
  call rectangle(310,32,70,10,purple);call rectangle(390,32,100,10,orange)
  call rectangle(500,32,40,10,cyan)
  call button(1,0,'Home / Go back',width-200,32,170,42,.true.)
  if(page==0)then
   clipmark=[60d0,145d0,real(width-60,c_double),260d0]
   do i=0,58
    a0=60+i*9d0;a1=a0+9;b0=205+25*sin(i*.28d0);b1=205+25*sin((i+1)*.28d0)
    call aa_line(a0,b0,a1,b1,purple,3d0,.false.,clipmark)
    call aa_line(a0,410-b0,a1,410-b1,orange,3d0,.false.,clipmark)
    if(mod(i,3)==0)call aa_line(a0,b0,a0,410-b0,cyan,1d0,.false.,clipmark)
   end do
   call soft_panel(width-400,180,340,18,9,purple)
   call soft_panel(width-310,210,250,18,9,orange)
   cw=(width-160)/3
   call home_card(2,'Virtual PCR','Start the selected virtual experiment',1,60,300,cw,.not.running.and.scenario_available(p_scenario),orange)
   call home_card(3,'Results','Review recorded curves and controls',2,80+cw,300,cw,nh>0,purple)
   call home_card(20,'qPCR scenarios','Edit parameters and explore virtual faults',3,100+2*cw,300,cw,.not.running,cyan)
   call home_card(5,'Configure profile','Set batches, DNA design and analysis',4,60,435,cw,.not.running,purple)
   call home_card(13,'Load files','Open a profile or native RDMLt record',5,80+cw,435,cw,.not.running,orange)
   call home_card(6,'About / help','Controls, capabilities and limitations',6,100+2*cw,435,cw,.true.,purple)
   if(p_scenario==0)call label('Scenario: normal run',60,655,cyan,1)
   if(p_scenario>0)call label('Scenario: '//trim(scenario_ids(p_scenario)),60,655,cyan,1)
   if(.not.scenario_available(p_scenario))call label('Melt acquisition is unavailable. Select another scenario or Normal virtual PCR.',60,700,orange,1)
  else if(page==1)then
   call label('PCR acquisition '//trim(integer_text(shown_cycle))//' / 45 | '//trim(integer_text(nh))//' run(s) filed',60,120,ink,1)
   if(graphmode/=1)then
    call label('Sample',80,142,orange,1);call label('Positive control',190,142,purple,1)
    call label('NTC',370,142,rgba(75,220,135,255),1);call label('Blank',450,142,cyan,1)
    call label('LOD controls',540,142,rgba(255,210,70,255),1)
   end if
   if(graphmode/=1)call label(trim(integer_text(shown_cycle))//' cycles reached; missing readings omitted; integer-cycle markers only', &
    80,height-158,ink,1)
   if(graphmode==1)call label('Instrument minutes / block C | primary, secondary, setpoint',80,height-158,ink,1)
   call button(10,0,'Temperature / Fluorescence',60,height-132,245,44,.true.)
   call button(18,0,'Reset chart',325,height-132,145,44,.true.)
   call button(3,0,'Results',490,height-132,140,44,nh>0)
   call button(7,0,'Stop and file current run',650,height-132,245,44,running)
   call button(28,0,'FAM / YY channel',915,height-132,210,44,graphmode/=1)
   if(graphmode==2)call label('YY channel',740,142,cyan,1)
   if(graphmode==0)call label('FAM channel',740,142,cyan,1)
  else if(page==2)then
   if(selected>0)call draw_results(history(selected))
   call button(33,0,'Display selection',60,height-122,240,44,selected>0)
   call button(35,0,'View selected curves',320,height-122,240,44,selected>0)
  else if(page==9)then
   if(selected>0)call draw_result_selection(history(selected))
  else if(page==6)then
   call draw_catalogue()
  else if(page==7)then
   call draw_scenario()
  else if(page==4)then
   call draw_profile()
  else if(page==5)then
   call label('Retained: native generic DNA probe assay, 45 cycles, 1-3 extractions and replicates.',60,165,ink,1)
   call label('Native kinetics, thermal plant / PID, dual sensor monitor, raw integer fluorescence.',60,205,ink,1)
   call label('Native batch 1-20, seeded variation and teaching control review.',60,245,ink,1)
   call label('Native profile JSON and experimental uncompressed RDMLt package round trip.',60,285,ink,1)
   call label('Removed: legacy format readers, MARA / advanced SOP editor, quizzes and 3 builders.',60,345,orange,1)
   call label('Removed: checkpoint recovery and bilingual interface.',60,385,orange,1)
   call label('Imports: this edition native packages only. CRC-32 detects corruption; no authentication.',60,425,purple,1)
   call label('Mouse and keyboard controls. Escape: Home. Space: stop and file the current run.',60,485,ink,1)
   call label('Numeric fields: click a value, type, then Enter to apply. Escape cancels an edit.',60,525,ink,1)
   call label('Pointer: wheel zoom, drag pan; keyboard Escape returns Home. All decisions are native.',60,585,ink,1)
  end if
  if(page==2)then
   call button(8,0,'Previous run',60,132,150,36,selected>1)
   call button(9,0,'Next run',225,132,150,36,selected<nh)
   call label('Run '//trim(integer_text(selected))//' of '//trim(integer_text(nh)),405,143,cyan,1)
   call button(40,0,'Download all RDMLt (ZIP)',width-600,132,280,36,nh>0.and..not.running.and.remaining==0)
   call button(11,0,'Download selected RDMLt',width-300,132,260,36,selected>0)
  end if
  ptr=c_loc(pixels(1));dirty=.false.
 end function
! @nativefn NR2-M03-F0010
 subroutine change_field(i,d)
  integer,intent(in)::i,d
  if(running)return
  select case(i)
  case(1)
   p_runs=max(1,min(20,p_runs+d))
  case(2)
   p_ext=max(1,min(3,p_ext+d))
  case(3)
   p_rep=max(1,min(3,p_rep+d))
  case(4)
   p_copies=max(1,min(10000000,p_copies+d*10000))
  case(5)
   p_threshold=max(20,min(2000,p_threshold+d*25))
  case(6)
   p_seed=max(1,min(999999,p_seed+d))
  case(7)
   p_fault=1-p_fault
  case(8)
   p_speed=max(100,min(5000,p_speed+d*500))
  end select
  message='Native profile updated. Save the profile to reuse these settings.'
 end subroutine
! @nativefn NR2-M03-F0011
 subroutine action(id,arg)
  integer,intent(in)::id,arg
  select case(id)
  case(1)
   page=0;edit_param=0
  case(2)
   call start_batch();if(running)page=1
  case(3)
   if(nh>0)page=2
  case(5)
   if(.not.running)page=4
  case(6)
   page=5
  case(7)
   if(running)call finish_run(2)
  case(8)
   selected=max(1,selected-1)
  case(9)
   selected=min(nh,selected+1)
  case(10)
   if(graphmode==1)then
    graphmode=0
   else
    graphmode=1
   end if
   zoom=1;pan=0
  case(11)
   if(selected>0)then
    call export_run(history(selected));request=2
   end if
  case(12)
   call export_profile();request=2
  case(13)
   if(.not.running)request=1
  case(14)
   call change_field(arg,-1)
  case(15)
   call change_field(arg,1)
  case(18)
   zoom=1;pan=0
  case(20)
   if(.not.running)page=6
  case(21)
   if(.not.running)then
    p_scenario=arg;page=7
   end if
  case(22)
   catalog_page=max(0,catalog_page-1)
  case(23)
   catalog_page=min(8,catalog_page+1)
  case(24)
   if(.not.running)call edit_parameter(p_scenario,arg,-1)
  case(25)
   if(.not.running)call edit_parameter(p_scenario,arg,1)
  case(26)
   if(.not.running)then
    p_scenario=0;page=6;message='Normal run selected. Saved fault settings retained.'
   end if
  case(27)
   if(p_scenario>0.and..not.running)scenario_bank(:,p_scenario)=scenario_defaults(:,p_scenario)
  case(40)
   if(nh>0.and..not.running.and.remaining==0)then
    call export_all_runs()
    if(output_size>0)request=2
   end if
  case(33)
   if(selected>0)page=9
  case(34)
   if(selected>0)show_group(arg,selected)=.not.show_group(arg,selected)
  case(36)
   if(selected>0)show_name(arg,selected)=.not.show_name(arg,selected)
  case(37)
   if(selected>0)then
    show_group(:,selected)=.true.;show_name(:,selected)=.true.
   end if
  case(38)
   if(selected>0)then
    show_group(:,selected)=.false.;show_name(:,selected)=.false.
   end if
  case(39)
   if(selected>0)page=2
  case(35)
   if(selected>0)then
    page=1;graphmode=0;zoom=1;pan=0
   end if
  case(30)
   edit_param=arg;edit_buffer=''
  case(28)
   if(graphmode==0)then
    graphmode=2
   else if(graphmode==2)then
    graphmode=0
   end if
  end select
  dirty=.true.
 end subroutine
! @nativefn NR2-M03-F0012
 subroutine native_event(kind,x,y,value) bind(c)
  integer(c_int),value::kind,value
  real(c_double),value::x,y
  integer::i,n,pos
  real(c_double)::v,p(6)
  logical::ok
  type(c_ptr)::refreshed
  if(kind==1.or.kind==2.or.kind==3.or.kind==6)then
   mx=x/render_scale;my=y/render_scale
  else
   mx=x;my=y
  end if
  select case(kind)
  case(1)
   if(dirty)then
    refreshed=native_render();dirty=.true.
   end if
   do i=nb,1,-1
    if(buttons(7,i)==0)cycle
    if(mx>=buttons(3,i).and.mx<=buttons(3,i)+buttons(5,i).and.my>=buttons(4,i).and.my<=buttons(4,i)+buttons(6,i))then
     call action(buttons(1,i),buttons(2,i));return
    end if
   end do
   if(page==1.and.my>=170.and.my<=height-190)then
    dragging=.true.;lastx=mx
   end if
  case(2)
   if(dragging)then
    pan=pan-(mx-lastx)/max(1,width-110)/zoom;lastx=mx
   end if
  case(3)
   dragging=.false.
  case(4)
   if(edit_param>0.and.page==7)then
    n=len_trim(edit_buffer)
    if(value>=48.and.value<=57.and.n<20)edit_buffer(n+1:n+1)=achar(value)
    if(value>=96.and.value<=105.and.n<20)edit_buffer(n+1:n+1)=achar(value-48)
    if((value==190.or.value==110).and.n<20)edit_buffer(n+1:n+1)='.'
    if((value==189.or.value==109).and.n==0)edit_buffer(1:1)='-'
    if(value==8.and.n>0)edit_buffer(n:n)=' '
    if(value==27)edit_param=0
    if(value==13)then
     pos=1;call json_number(edit_buffer,len_trim(edit_buffer),pos,v,ok)
     if(ok.and.pos>len_trim(edit_buffer))then
      p=scenario_bank(:,p_scenario);p(edit_param)=v
      if(parameters_valid(p_scenario,p))then
       scenario_bank(:,p_scenario)=p;edit_param=0;message='Scenario value applied.'
      else
       message='Value outside bounds, precision or integer requirements. No setting changed.'
      end if
     else
      message='Enter a valid decimal number.'
     end if
    end if
    dirty=.true.;return
   end if
   if(value==27)page=0
   if(value==32.and.running)call finish_run(2)
  case(5)
   width=max(1280,min(1920,int(x)));height=max(800,min(1200,int(y)))
  case(6)
   if(page==1)then
    if(value<0)zoom=min(20d0,zoom*1.2d0)
    if(value>0)zoom=max(1d0,zoom/1.2d0)
   end if
  end select
  dirty=.true.
 end subroutine
! @nativefn NR2-M03-F0013
 subroutine native_file(a,n) bind(c)
  integer(c_int),value::n
  integer(c_int8_t),intent(in)::a(n)
  integer::before
  before=nh
  call load_file(a,n);dirty=.true.
  if(nh>before)page=2
  if(page==7.and.p_scenario==0)page=4
 end subroutine
! @nativefn NR2-M03-F0015
 integer(c_int) function native_tick(ms) result(changed) bind(c)
  real(c_double),value::ms
  logical::was_running
  was_running=running
  call tick_model(ms)
  changed=0;if(dirty.or.was_running.or.running.or.remaining>0)changed=1
 end function
! @nativefn NR2-M03-F0016
 integer(c_int) function native_frame_width() result(v) bind(c)
  v=fw
 end function
! @nativefn NR2-M03-F0029
 integer(c_int) function native_frame_height() result(v) bind(c)
  v=fh
 end function
! @nativefn NR2-M03-F0030
 integer(c_int) function native_width() result(v) bind(c)
  v=width
 end function
! @nativefn NR2-M03-F0017
 integer(c_int) function native_height() result(v) bind(c)
  v=height
 end function
! @nativefn NR2-M03-F0018
 integer(c_int) function native_request()result(v) bind(c)
  v=request;request=0
 end function
! @nativefn NR2-M03-F0020
 function native_output()result(p) bind(c)
  type(c_ptr)::p
  p=c_loc(output(1))
 end function
! @nativefn NR2-M03-F0021
 integer(c_int) function native_output_size()result(n) bind(c)
  n=output_size
 end function
! @nativefn NR2-M03-F0022
 function native_filename()result(p) bind(c)
  type(c_ptr)::p
  p=c_loc(output_name(1))
 end function
! @nativefn NR2-M03-F0023
 integer(c_int) function native_filename_size()result(n) bind(c)
  n=name_size
 end function
! @nativefn NR2-M03-F0024
 subroutine native_review(out) bind(c)
  real(c_double),intent(out)::out(20)
  out=0
  if(selected<=0)return
  out(1)=verdict(history(selected));out(2)=history(selected)%nw;out(3)=history(selected)%ncy
  out(4)=history(selected)%maxgap;out(5:18)=history(selected)%cq
 end subroutine
 ! Machine-readable diagnostics for testing; no host code uses these to make application decisions.
! @nativefn NR2-M03-F0025
 subroutine native_status(out) bind(c)
  integer(c_int),intent(out)::out(12)
  out=[page,nh,selected,current%ncy,current%status,0,remaining,p_ext,p_rep,p_runs,p_fault,p_copies]
  if(running)out(6)=1
 end subroutine
! @nativefn NR2-M03-F0031
subroutine native_scenario_review(a) bind(c)
 real(c_double),intent(out)::a(24)
 integer::i
 character(len=:),allocatable::escaped
 a=0;a(1)=p_scenario
 if(p_scenario>0)a(2:7)=scenario_bank(:,p_scenario)
 if(selected>0)then
  a(8)=history(selected)%scenario;a(9:14)=history(selected)%params
  a(15)=count(history(selected)%present(1:history(selected)%nw)==0)
  a(16)=count(history(selected)%y(1:history(selected)%ncy,1)>=0)
  a(17)=count(history(selected)%y2(1:history(selected)%ncy,1)>=0)
  a(18:20)=history(selected)%cq2(1:3)
 end if
 a(21)=stdlib_mean([1d0,2d0,3d0,4d0]);a(22)=stdlib_var([1d0,2d0,3d0,4d0])
 call escape_string(achar(34)//achar(92)//achar(10),escaped,.false.)
 a(23)=len(escaped)
 if(escaped==achar(92)//achar(34)//achar(92)//achar(92)//achar(92)//'n')a(24)=1
end subroutine
! @nativefn NR2-M03-F0048
 subroutine native_visibility(a) bind(c)
  integer(c_int),intent(out)::a(4)
  integer::w,mask
  a=0;a(1)=page;if(selected<1)return
  mask=0
  do w=1,history(selected)%nw
   if(result_visible(history(selected),w))mask=ibset(mask,w-1)
  end do
  a(2)=mask;a(3)=popcnt(mask);a(4)=history(selected)%nw
 end subroutine
end module
