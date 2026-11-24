! SPDX-License-Identifier: GPL-3.0-only
! Immediate-mode native application UI. Browser knows no screens, fields, buttons or chart rules.
module native_ui
 use iso_c_binding
 use native_model
 use native_io
 use native_rdmlt
 use r2_framebuffer
 use r2_font
 use stdlib_browser_stats
 use native_codec, only: sha256_hex, deflate_fixed, zlen
 use native_references
 use native_anim
use native_sys
use native_audio
 use native_theatre
 use native_schematic
 use native_circuit, only: ckt_short, short_name
 use native_structure, only: st_n,st_kind,st_count
 use native_scenarios
 use r2_math, only: rng_uniform, rng_gauss
 use r2_thermal
 use r2_chemistry
 use r2_monitor
 use r2_optics
 implicit none
 character(len=60),save::blabel(200)=''
 integer,save::nar_cycle=0,nar_amp=0
 logical,save::nar_live=.false.
 integer,save::width=1280,height=820,page=-2,graphmode=0,catalog_page=0,edit_param=0,nb=0,buttons(7,200)=0,request=0
 character(len=24),save::edit_buffer=''
 logical,save::show_group(5,maxruns)=.true.,show_name(maxw,maxruns)=.true.
 ! Voice commands as in MISSY: wake word "Computer", then a command from a short grammar.
 logical,save::vc_on=.false.,vc_alert=.false.,help_on=.false.
 character(len=200),save::vc_buf='',vc_heard='',vc_reply=''
 integer,save::vc_n=0,vc_pack=0     ! English pack: 0 not checked 1 checking 2 downloading 3 ready 4 not offered 5 failed
 logical,save::dirty=.true.,dragging=.false.,mv_drag=.false.
 real(c_double),save::mx=-1,my=-1,zoom=1,pan=0,lastx=0,mv_lx=0,mv_ly=0,mv_px=0,mv_py=0
 ! Startup screen: one embedded component per frame, entry only after all are ready.
 integer,parameter::nboot=25
 integer,parameter::boot_last(5)=[3,10,14,20,25]
 character(len=56),parameter::boot_label(nboot)=[character(len=56):: 'Display surface and framebuffer', &
  'Bitmap font atlas (1531 glyphs)','Interface palette, layout and button tables','Seeded random number streams', &
  'Statistics library (stdlib mean and variance)','Thermal plant model and PID controller', &
  'Independent dual-sensor temperature monitor','Reaction kinetics (10-bin chemistry model)', &
  'Hydrolysis probe and detector optics','Cq threshold analysis','Scenario identifiers, titles and kinds (51)', &
  'Scenario parameter labels and bounds','Scenario default parameter bank (51 x 6)','Native profile defaults', &
  'Run history store (32 runs)','CRC-32 lookup table','SHA-256 message digest','DEFLATE compressor (fixed Huffman, LZ77)', &
  'ZIP and RDMLt export buffers','References and edition notes (157 entries)','Home and navigation', &
  'Virtual PCR acquisition view and charts','Results and curve selection','Scenario catalogue, animations, 4 structures', &
  'References page']
 integer,save::boot_next=1,boot_pending=0,boot_failed=0,ref_page=0,boot_phase=0
 real(c_double),save::boot_ready_ms=-1,boot_data_ms=0,boot_dt=0,mem_t=0,pre_ms=0,pre_done_ms=0
 integer,save::pre_phase=0
 real(c_double),save::boot_anim=0,boot_ms=0,boot_el(0:nboot)=0,host_ms=-1,host_mb=-1,boot_done_ms=0
 logical,save::booted=.false.
 real(c_double),save::anim_t=0,anim_acc=0
 integer,save::pv_channel=1
 logical,save::show_fault=.true.,show_structure=.false.,show_3d=.true.,view_mv=.false.
 integer,save::card_page=1
 integer,save::view_k=1,view_group=0,view_part=1
 integer,save::struct_idx=1
 logical,save::pv_show_norm=.true.,pv_show_fault=.true.
 integer,save::theatre_k=1
 real(c_double),save::theatre_t=0,theatre_dt=0
 logical,save::theatre_pause=.false.
 integer(c_int32_t),parameter::bg=int(z'FF10090D',c_int32_t),orange=int(z'FF649BFF',c_int32_t)
 integer(c_int32_t),parameter::purple=int(z'FFC99ACB',c_int32_t),ink=int(z'FFF3E0EA',c_int32_t)
 integer(c_int32_t),parameter::panel=int(z'FF281723',c_int32_t),cyan=int(z'FFDCDC60',c_int32_t)
contains
! @nativefn NR2-M03-F0001
 subroutine label(s,x,y,color,scale)
  character(len=*),intent(in)::s
  integer,intent(in)::x,y,scale
  integer(c_int32_t),intent(in)::color
  integer::i,g,xx,yy,k,l,advance,level,copies,code,step
  advance=0;copies=scale*render_scale/2;i=1
  do while(i<=len_trim(s))
   call utf8_code(s,i,code,step);i=i+step
   g=glyph(code)
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
! @nativefn NR2-M03-F0122
 integer function label_width(s,scale) result(wpx)
  ! Width in logical pixels of label(s,...,scale).
  character(len=*),intent(in)::s
  integer,intent(in)::scale
  integer::i,code,step,adv,copies
  adv=0;copies=scale*render_scale/2;i=1
  do while(i<=len_trim(s))
   call utf8_code(s,i,code,step);i=i+step
   adv=adv+glyph_advance(glyph(code))*copies
  end do
  wpx=adv/render_scale
 end function
! @nativefn NR2-M03-F0040
 subroutine utf8_code(s,i,code,step)
  ! Decodes one UTF-8 sequence at s(i:); malformed bytes are shown as single characters.
  character(len=*),intent(in)::s
  integer,intent(in)::i
  integer,intent(out)::code,step
  integer::b,n
  n=len_trim(s);b=iachar(s(i:i));code=b;step=1
  if(b>=240.and.i+3<=n)then
   code=iand(b,7)*262144+iand(iachar(s(i+1:i+1)),63)*4096+iand(iachar(s(i+2:i+2)),63)*64+iand(iachar(s(i+3:i+3)),63);step=4
  else if(b>=224.and.i+2<=n)then
   code=iand(b,15)*4096+iand(iachar(s(i+1:i+1)),63)*64+iand(iachar(s(i+2:i+2)),63);step=3
  else if(b>=192.and.i+1<=n)then
   code=iand(b,31)*64+iand(iachar(s(i+1:i+1)),63);step=2
  end if
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
  call button(id,0,'',x,y,w,110,enabled);blabel(nb)=title
  call soft_panel(x,y,w,110,20,edge)
  call soft_panel(x+2,y+10,w-4,98,18,panel)
  call label('0'//trim(integer_text(number)),x+18,y+25,edge,2)
  call label(title,x+75,y+25,ink,2)
  call label(detail,x+20,y+76,purple,1)
 end subroutine
! @nativefn NR2-M03-F0002
 subroutine hotspot(id,arg,x,y,w,h)
  ! Clickable region with nothing drawn (used over the molecule banner).
  integer,intent(in)::id,arg,x,y,w,h
  if(nb>=size(buttons,2))return
  nb=nb+1;buttons(:,nb)=[id,arg,x,y,w,h,1];blabel(nb)=''
 end subroutine
! @nativefn NR2-M03-F0132
 subroutine mv_toolbar(x0,y)
  ! Molecule tools: structure, what to show, colouring, contact cut-off, zoom, slice (front
  ! clipping plane, to look inside), spin, reset and clearing the selected atoms.
  integer,intent(inout)::x0
  integer,intent(in)::y
  call button(80,0,'<',x0,y,44,42,.true.);x0=x0+48
  call button(81,0,'>',x0,y,44,42,.true.);x0=x0+48
  call button(82,0,trim(mv_show_name()),x0,y,130,42,.true.);x0=x0+134
  call button(83,0,trim(mv_col_name()),x0,y,160,42,.true.);x0=x0+164
  call button(84,0,'Cut-off -',x0,y,80,42,.true.);x0=x0+84
  call button(85,0,'+',x0,y,40,42,.true.);x0=x0+44
  call button(86,0,'Zoom +',x0,y,64,42,.true.);x0=x0+68
  call button(87,0,'Zoom -',x0,y,64,42,.true.);x0=x0+68
  call button(100,0,'Slice -',x0,y,64,42,.true.);x0=x0+68
  call button(101,0,'Slice +',x0,y,64,42,.true.);x0=x0+68
  if(mv_spin)then
   call button(88,0,'Spin: on',x0,y,80,42,.true.)
  else
   call button(88,0,'Spin: off',x0,y,80,42,.true.)
  end if
  x0=x0+84
  call button(89,0,'Reset',x0,y,56,42,.true.);x0=x0+60
  call button(91,0,'Clear',x0,y,56,42,.true.);x0=x0+60
 end subroutine
! @nativefn NR2-M03-F0133
 subroutine view_sources(k,n,l)
  ! One verified source for the picture, listed under the full-screen view (not on previews).
  integer,intent(in)::k
  integer,intent(out)::n
  character(len=220),intent(out)::l(12)
  integer::f
  f=scene_family(p_scenario);n=0;l=''
  if(k==7)then
   call add('MISSY R2 numerical model (this project): full-model simulation of the case and of the normal run; all values simulated.')
   return
  end if
  if(k==5.or.k==6)then
   call add('Method: modified nodal analysis as in SPICE (Nagel, L.W., 1975, SPICE2, Memo ERL-M520, Univ. of California, '// &
    'Berkeley); circuit values simulated by MISSY R2 (this project) with illustrative part values.')
   return
  end if
  if(k==4.and.f/=2)then
   call add('Mullins, E.A., Salay, L.E., Durie, C.L., Bradley, N.P., Jackman, J.E., Ohi, M.D., Chazin, W.J. and Eichman, B.F. '// &
    '(2024). A mechanistic model of primer synthesis from catalytic structures of DNA polymerase alpha-primase. '// &
    'Nat. Struct. Mol. Biol., 31, 777. doi:10.1038/s41594-024-01227-4')
   return
  end if
  if(k==4)then
   select case(max(1,struct_idx))
   case(2)
    call add('Davies, D.R., Interthal, H., Champoux, J.J. and Hol, W.G. (2004). Explorations of peptide and oligonucleotide '// &
     'binding sites of tyrosyl-DNA phosphodiesterase using vanadate complexes. J. Med. Chem., 47, 829-837. doi:10.1021/jm030487x')
   case(3)
    call add('Cramer, P., Larson, C.J., Verdine, G.L. and Muller, C.W. (1997). Structure of the human NF-kappaB p52 homodimer-DNA '// &
     'complex at 2.1 A resolution. EMBO J., 16, 7078-7090. doi:10.1093/emboj/16.23.7078')
   case(4)
    call add('Jin, X., Gewirth, D.T. and Geiger, J.H. High resolution structure of a yeast TFIIA/TBP/TATA-box DNA complex. '// &
     'PDB entry 1RM1, to be published. doi:10.2210/pdb1RM1/pdb')
   case default
    call add('Mullins, E.A., Salay, L.E., Durie, C.L., Bradley, N.P., Jackman, J.E., Ohi, M.D., Chazin, W.J. and Eichman, B.F. '// &
     '(2024). A mechanistic model of primer synthesis from catalytic structures of DNA polymerase alpha-primase. '// &
     'Nat. Struct. Mol. Biol., 31, 777. doi:10.1038/s41594-024-01227-4')
   end select
  else if(f==2.and.k==3)then
   call add('Mullins, E.A., Salay, L.E., Durie, C.L., Bradley, N.P., Jackman, J.E., Ohi, M.D., Chazin, W.J. and Eichman, B.F. '// &
    '(2024). A mechanistic model of primer synthesis from catalytic structures of DNA polymerase alpha-primase. '// &
    'Nat. Struct. Mol. Biol., 31, 777. doi:10.1038/s41594-024-01227-4')
  else if(f==2)then
   call add('SantaLucia, J. Jr. (1998). A unified view of polymer, dumbbell, and oligonucleotide DNA nearest-neighbor '// &
    'thermodynamics. Proc. Natl. Acad. Sci. USA, 95(4), 1460-1465. doi:10.1073/pnas.95.4.1460')
  else
   call add('Chai Open qPCR [CAD model]. GrabCAD Community Library. https://grabcad.com/library/chai-open-qpcr-1')
  end if
 contains
  subroutine add(t)
   character(len=*),intent(in)::t
   if(n<12)then
    n=n+1;l(n)=t
   end if
  end subroutine
 end subroutine
! @nativefn NR2-M03-F0135
 subroutine view_card(k,cx,cy,cw,ch)
  ! One picture card: live preview, 2D / 3D by what it shows, a description; click: full screen.
  integer,intent(in)::k,cx,cy,cw,ch
  integer(c_int32_t)::c
  call case_view(k,real(cx,c_double),real(cy,c_double),real(cw,c_double),real(ch-74,c_double),anim_t)
  call soft_panel(cx,cy+ch-72,cw,72,8,rgba(28,18,30,255))
  c=merge(cyan,orange,view_kind(k)=='3D')
  call label(trim(view_kind(k)),cx+10,cy+ch-62,c,2)
  if(cw>=380)then
   call wrapped_px(trim(view_desc_card(k)),cx+56,cy+ch-66,cw-64,ink)
  else
   call wrapped_px(trim(view_short(k)),cx+56,cy+ch-60,cw-64,ink)
  end if
  call hotspot(97,k,cx,cy,cw,ch)
  if(mx>=cx.and.mx<=cx+cw.and.my>=cy.and.my<=cy+ch)then
   call card_frame(cx-4,cy-4,cw+8,ch+8,orange,3)
  else
   call card_frame(cx-4,cy-4,cw+8,ch+8,c,2)
  end if
 end subroutine
! @nativefn NR2-M03-F0146
 subroutine group_card(g,cx,cy,cw,ch)
  ! A group of similar views as one card: live preview of its first view, name and count;
  ! click: the group's own page with its views (or full screen when it has one view).
  integer,intent(in)::g,cx,cy,cw,ch
  integer(c_int32_t)::c
  integer::n,v(4)
  call group_views(g,n,v)
  call case_view(v(1),real(cx,c_double),real(cy,c_double),real(cw,c_double),real(ch-74,c_double),anim_t)
  if(n>1)then
   call soft_panel(cx+cw-118,cy+8,110,30,8,rgba(24,15,23,235))
   call label(trim(integer_text(n))//' views',cx+cw-104,cy+12,cyan,1)
  end if
  call soft_panel(cx,cy+ch-72,cw,72,8,rgba(28,18,30,255))
  c=merge(cyan,orange,g==2)
  call label(trim(group_name(g)),cx+12,cy+ch-68,c,1)
  if(cw>=380)then
   call wrapped_px(trim(group_text(g)),cx+12,cy+ch-44,cw-24,ink)
  end if
  call hotspot(106,g,cx,cy,cw,ch)
  if(mx>=cx.and.mx<=cx+cw.and.my>=cy.and.my<=cy+ch)then
   call card_frame(cx-4,cy-4,cw+8,ch+8,orange,3)
  else
   call card_frame(cx-4,cy-4,cw+8,ch+8,c,2)
  end if
 end subroutine
! @nativefn NR2-M03-F0138
 subroutine card_frame(x,y,w,h,c,t)
  integer,intent(in)::x,y,w,h,t
  integer(c_int32_t),intent(in)::c
  call rectangle(x,y,w,t,c);call rectangle(x,y+h-t,w,t,c)
  call rectangle(x,y,t,h,c);call rectangle(x+w-t,y,t,h,c)
 end subroutine
! @nativefn NR2-M03-F0139
 subroutine settings_card(cx,cy,cw,ch)
  ! The case's settings inside a card: name, editable parameters, curve channel, recompute.
  integer,intent(in)::cx,cy,cw,ch
  integer::i,y,row,np,rh,bh,vx
  call soft_panel(cx,cy,cw,ch,10,rgba(24,15,23,255))
  call card_frame(cx-4,cy-4,cw+8,ch+8,purple,2)
  call label(trim(integer_text(p_scenario))//' | '//trim(scenario_ids(p_scenario))//' | '//trim(scenario_kinds(p_scenario))// &
   ' | '//trim(category_name(p_scenario)),cx+14,cy+10,cyan,1)
  call wrapped_px(trim(scenario_titles(p_scenario)),cx+14,cy+32,cw-28,orange)
  np=count(len_trim(parameter_names(:,p_scenario))>0)
  rh=max(34,min(54,(ch-84-104)/max(1,np)));bh=min(42,rh-6);vx=cx+cw-260
  row=0
  do i=1,6
   if(len_trim(parameter_names(i,p_scenario))==0)cycle
   y=cy+82+row*rh;row=row+1
   call label(trim(parameter_names(i,p_scenario)),cx+14,y+2,ink,1)
   if(rh>=50)call label(trim(real_text(scenario_min(i,p_scenario),3))//' .. '//trim(real_text(scenario_max(i,p_scenario),3)), &
    cx+14,y+24,purple,1)
   call button(30,i,trim(real_text(scenario_bank(i,p_scenario),3)),vx,y,130,bh,.not.ui_busy().and.scenario_step(i,p_scenario)>0)
   if(edit_param==i)call label(trim(edit_buffer)//'_',vx+10,y+bh/2-10,cyan,1)
   call button(24,i,'-',vx+138,y,52,bh,.not.ui_busy().and.scenario_step(i,p_scenario)>0)
   call button(25,i,'+',vx+196,y,52,bh,.not.ui_busy().and.scenario_step(i,p_scenario)>0)
  end do
  if(cy+82+row*rh+30<cy+ch-126)call wrapped_px('Click a value to type; Enter applies, Escape cancels; or use - / +.', &
   cx+14,cy+82+row*rh+10,cw-28,purple)
  ! settings as JSON: the Fortran profile writer / parser (all 51 cases and the run profile)
  call button(12,0,'Save settings (JSON)',cx+14,cy+ch-94,190,38,.true.)
  call button(13,0,'Load settings (JSON)',cx+214,cy+ch-94,190,38,.not.ui_busy())
  call button(27,0,'Defaults',cx+414,cy+ch-94,min(110,cw-428),38,.not.ui_busy())
  if(p_scenario==21)call label('Not reproducible: needs a dye melt workflow.',cx+14,cy+ch-74,orange,1)
  if(pv_channel==1)then
   call button(58,0,'Curves: target',cx+14,cy+ch-48,150,38,.true.)
  else
   call button(58,0,'Curves: endogene',cx+14,cy+ch-48,150,38,.true.)
  end if
  call button(57,0,'Recompute curves',cx+174,cy+ch-48,180,38,.not.ui_busy().and.scenario_available(p_scenario))
 end subroutine
! @nativefn NR2-M03-F0137
 function view_desc_card(k) result(t)
  ! Card text: short name and what the picture shows (the full-screen view adds the source).
  integer,intent(in)::k
  character(len=240)::t
  t=trim(view_short(k))//'. '
  select case(k)
  case(1)
   t=trim(t)//' The case fault, drawn and animated.'
  case(2)
   t=trim(t)//' Same drawing, normal run, for comparison.'
  case(3)
   if(scene_family(p_scenario)==2)then
    t=trim(t)//' Atoms from the crystal structure.'
   else
    t=trim(t)//' Block, Peltiers, heat sink, fan, lid, optics.'
   end if
  case(4)
   if(scene_family(p_scenario)==2)then
    t=trim(t)//' Rotating atom model, fault on the enzyme.'
   else
    t=trim(t)//' What happens in each tube, atom by atom.'
   end if
  case(5)
   t=trim(t)//' H-bridge, Peltier, sensors, fan, lid; live simulated values.'
  case(6)
   t=trim(t)//' LED driver, photodiode amplifier, ADC; live simulated values.'
  case default
   t=trim(t)//' Fault (colours) against normal (grey dashed). Click for full size.'
  end select
 end function
! @nativefn NR2-M03-F0136
 function view_short(k) result(t)
  integer,intent(in)::k
  character(len=60)::t
  select case(k)
  case(1)
   t='Fault drawing'
  case(2)
   t='Normal drawing (no fault)'
  case(3)
   if(scene_family(p_scenario)==2)then
    t='Real 8UCU DNA and polymerase'
   else
    t='Rotating instrument cut-away'
   end if
  case(4)
   if(scene_family(p_scenario)==2)then
    t='Protein-DNA structures (PDB)'
   else
    t='Inside the tube: 8UCU DNA'
   end if
  case(5)
   t='Thermal electronics schematic'
  case(6)
   t='Optical electronics schematic'
  case default
   t='Amplification curves'
  end select
 end function
! @nativefn NR2-M03-F0134
 subroutine click_frame(x,y,w,h)
  ! Visible frame around a clickable panel, with a hint; orange under the pointer.
  integer,intent(in)::x,y,w,h
  integer(c_int32_t)::c
  integer::t
  c=cyan;t=2
  if(mx>=x.and.mx<=x+w.and.my>=y.and.my<=y+h)then
   c=orange;t=3
  end if
  call rectangle(x-4,y-4,w+8,t,c);call rectangle(x-4,y+h+4-t,w+8,t,c)
  call rectangle(x-4,y-4,t,h+8,c);call rectangle(x+w+4-t,y-4,t,h+8,c)
  call soft_panel(x+w-150,y+h-30,146,26,8,rgba(24,15,23,255))
  call label('Click: full screen',x+w-140,y+h-26,c,1)
 end subroutine
! @nativefn NR2-M03-F0140
 subroutine enter_view()
  ! 3D molecule views open with the molecule controls; other views open as animations.
  view_mv=(view_k==3.and.scene_family(p_scenario)==2).or.(view_k==4)
  if(view_mv)then
   if(view_k==4.and.scene_family(p_scenario)==2)then
    call mv_open(max(1,struct_idx))
   else
    call mv_open(1)
   end if
   mv_spin=.true.
  end if
 end subroutine
! @nativefn NR2-M03-F0127
 integer function nviews() result(n)
  ! 1 fault drawing, 2 normal drawing, 3-4 3D views, 5-6 electrical schematics (hardware
  ! cases only, see view_ok), 7 amplification curves
  n=7
 end function
! @nativefn NR2-M03-F0128
 function view_kind(k) result(t)
  ! What the picture is, not which mode button produced it: the structure view is a rotating
  ! 3-D atom model even though it sits with the drawings.
  integer,intent(in)::k
  character(len=4)::t
  select case(k)
  case(3,4)
   t='3D'
  case default
   t='2D'
  end select
 end function
! @nativefn NR2-M03-F0141
 logical function view_ok(k)
  ! electrical schematics exist for the hardware (instrument) cases
  integer,intent(in)::k
  view_ok=k>=1.and.k<=nviews()
  if(k==5.or.k==6)view_ok=scene_family(p_scenario)==1
 end function
! @nativefn NR2-M03-F0142
 subroutine group_views(g,n,v)
  ! card groups: 1 technical drawings, 2 3D views, 3 electrical schematics, 4 curves
  integer,intent(in)::g
  integer,intent(out)::n,v(4)
  v=7
  select case(g)
  case(1)
   n=2;v(1:2)=[1,2]
  case(2)
   n=2;v(1:2)=[3,4]
  case(3)
   n=2;v(1:2)=[5,6]
  case default
   n=1
  end select
 end subroutine
! @nativefn NR2-M03-F0143
 integer function group_of(k) result(g)
  integer,intent(in)::k
  g=min(4,(k+1)/2)
 end function
! @nativefn NR2-M03-F0144
 function group_name(g) result(t)
  integer,intent(in)::g
  character(len=40)::t
  select case(g)
  case(1)
   t='Technical drawings'
  case(2)
   t='3D views'
  case(3)
   t='Electrical schematics'
  case default
   t='Amplification curves'
  end select
 end function
! @nativefn NR2-M03-F0145
 function group_text(g) result(t)
  integer,intent(in)::g
  character(len=200)::t
  select case(g)
  case(1)
   t='2 views: the fault drawing and the same drawing without the fault. Click to choose.'
  case(2)
   if(scene_family(p_scenario)==2)then
    t='2 views: real 8UCU DNA with polymerase, and the protein-DNA structures. Click to choose.'
   else
    t='2 views: rotating instrument cut-away, and inside the tube (8UCU DNA). Click to choose.'
   end if
  case(3)
   t='2 views: thermal and optical electronics, simulated live, with the suspect part probed.'
  case default
   t='Fault (colours) against normal (grey dashed). Click for full size.'
  end select
 end function
! @nativefn NR2-M03-F0129
 function view_desc(k) result(t)
  integer,intent(in)::k
  character(len=200)::t
  integer::f
  f=scene_family(p_scenario)
  if(k==7)then
   t='Amplification curves of this case: fault (colours) against the normal run (grey dashed)';return
  end if
  select case(k)
  case(1)
   select case(f)
   case(1)
    t='Technical drawing of the 16-tube cycler with the fault'
   case(2)
    t='Schematic of one PCR cycle (primers, probe, polymerase) with the fault'
   case(3)
    t='Sample wells and template copies with the fault'
   case default
    t='Signal and data path with the fault'
   end select
   if(len_trim(fault_caption(p_scenario,scenario_bank(:,p_scenario)))>0)then
    t=trim(t)//': '//trim(fault_caption(p_scenario,scenario_bank(:,p_scenario)))
   else
    t=trim(t)//': '//trim(scenario_titles(p_scenario))
   end if
  case(2)
   t='The same drawing without the fault (normal reference)'
  case(3)
   if(f==2)then
    t='Real PDB 8UCU primer / template DNA and polymerase, rotating, melting, annealing and extending'
   else
    t='Rotating cut-away of the cycler: block, Peltier modules, heat sink, fan, heated lid and optics'
   end if
  case(5)
   t='Thermal electronics: H-bridge Peltier drive, current sense, thermistors, lid heater, fan; MNA simulation, suspect part probed'
  case(6)
   t='Optical electronics: LED current driver, light path, photodiode TIA, gain stage, ADC; MNA simulation, suspect part probed'
  case default
   if(f==2)then
    t='Rotating atom model of the protein-DNA structures (PDB), with the fault shown on the enzyme'
   else
    t='Inside the tube: real PDB 8UCU primer / template DNA and polymerase, melting, annealing and extending'
   end if
  end select
 end function
! @nativefn NR2-M03-F0130
 subroutine case_view(k,x,y,w,h,t)
  integer,intent(in)::k
  real(c_double),intent(in)::x,y,w,h,t
  if(k==7)then
   call plot_preview(nint(x)+10,nint(y)+10,nint(w)-20,nint(h)-20);return
  end if
  if(page==12.and.k<=2.and.scene_family(p_scenario)==1)then
   cyc_part=view_part
   call draw_instrument_stacked(p_scenario,scenario_bank(:,p_scenario),x,y,w,h,t,k==1)
   cyc_part=0;return
  end if
  if(page==12.and.k<=2)then        ! other drawings: twice as large, still into the 4K frame
   render_scale=4
   call draw_fault_scene(p_scenario,scenario_bank(:,p_scenario),x/2,y/2,w/2,h/2,t,k==1)
   render_scale=2;return
  end if
  if(k==5.or.k==6)then
   call draw_schematic(k-4,p_scenario,scenario_bank(:,p_scenario),.true.,x,y,w,h,t);return
  end if
  select case(k)
  case(1)
   call draw_fault_scene(p_scenario,scenario_bank(:,p_scenario),x,y,w,h,t,.true.)
  case(2)
   call draw_fault_scene(p_scenario,scenario_bank(:,p_scenario),x,y,w,h,t,.false.)
  case(3)
   call case_scene_3d(p_scenario,scene_family(p_scenario),fault_caption(p_scenario,scenario_bank(:,p_scenario)),.true.,x,y,w,h,t)
  case default
   if(scene_family(p_scenario)==2)then
    call draw_structure_scene(p_scenario,scenario_bank(:,p_scenario),x,y,w,h,t,.true.,struct_idx)
   else
    call case_scene_3d(0,2,'Inside the tube: PDB 8UCU',.false.,x,y,w,h,t)
   end if
  end select
 end subroutine
! @nativefn NR2-M03-F0131
 subroutine draw_view_menu()
  ! Full-screen menu: every view of the selected case as a live preview; click one to open it.
  integer::n,k,tw,th,tx,ty,cols,i,vl(7)
  if(view_group>0)then
   call group_views(view_group,n,vl(1:4))
  else
   n=0
   do i=1,nviews()
    if(view_ok(i))then
     n=n+1;vl(n)=i
    end if
   end do
  end if
  cols=min(n,2)
  call label('Case '//trim(scenario_ids(p_scenario))//'   '//trim(scenario_titles(p_scenario)),24,14,orange,2)
  if(view_group>0)then
   call label(trim(group_name(view_group))//': choose a view to open it full screen',24,50,ink,1)
  else
   call label('Choose a view to open it full screen',24,50,ink,1)
  end if
  call button(1,0,'Back',width-170,14,150,42,.true.)
  call mic_button(width-330,14,140)
  tw=(width-60-(cols-1)*28)/cols;th=(height-122-((n+cols-1)/cols-1)*28)/((n+cols-1)/cols)
  do i=1,n
   k=vl(i)
   tx=30+mod(i-1,cols)*(tw+28);ty=92+((i-1)/cols)*(th+28)
   call case_view(k,real(tx,c_double),real(ty,c_double),real(tw,c_double),real(th-64,c_double),anim_t)
   call soft_panel(tx,ty+th-60,tw,58,10,rgba(28,18,30,255))
   call label(trim(view_kind(k)),tx+14,ty+th-52,merge(cyan,orange,view_kind(k)=='3D'),2)
   call wrapped_px(trim(view_desc(k)),tx+70,ty+th-54,tw-84,ink)
   ! frame around the whole tile (preview and caption); brighter under the pointer
   if(mx>=tx.and.mx<=tx+tw.and.my>=ty.and.my<=ty+th)then
    call tile_frame(tx-6,ty-6,tw+12,th+12,orange,3)
   else
    call tile_frame(tx-6,ty-6,tw+12,th+12,merge(cyan,purple,view_kind(k)=='3D'),2)
   end if
   call hotspot(97,k,tx,ty,tw,th)
  end do
 contains
  subroutine tile_frame(x,y,w,h,c,t)
   integer,intent(in)::x,y,w,h,t
   integer(c_int32_t),intent(in)::c
   call rectangle(x,y,w,t,c);call rectangle(x,y+h-t,w,t,c)
   call rectangle(x,y,t,h,c);call rectangle(x+w-t,y,t,h,c)
  end subroutine
 end subroutine
! @nativefn NR2-M03-F0125V
 subroutine voice_toggle()
  ! Starts or stops browser speech recognition. The recogniser hands each final phrase back to
  ! native_event as characters (kind 8) and an end mark (kind 9); kind 10 reports its state.
  ! Phrases heard while MISSY itself is speaking are ignored so it never answers itself.
  vc_on=.not.vc_on
  if(vc_on)then
   vc_heard='';vc_reply=''
   if(vc_pack==5)call voice_pack()
   call sys_script("(function(){try{var R=window.SpeechRecognition||window.webkitSpeechRecognition;"// &
    "var E=NativeTransport.engine;if(!R){E._native_event(10,0,0,4);return}"// &
    "if(window.__mvr){try{window.__mvr.abort()}catch(e){}}"// &
    "var r=new R();r.lang='en-US';r.continuous=true;r.interimResults=false;r.maxAlternatives=1;"// &
    "if(window.__mvpack&&('processLocally' in r))r.processLocally=true;"// &
    "window.__mvon=true;window.__mvr=r;"// &
    "r.onresult=function(ev){var c=window.__mcalm;if((c&&!c.paused&&!c.ended)||"// &
    "(window.speechSynthesis&&speechSynthesis.speaking))return;"// &
    "for(var i=ev.resultIndex;i<ev.results.length;i++){if(!ev.results[i].isFinal)continue;"// &
    "var t=ev.results[i][0].transcript.trim().slice(0,190);"// &
    "for(var j=0;j<t.length;j++){var k=t.charCodeAt(j);E._native_event(8,0,0,k<128?k:32)}"// &
    "E._native_event(9,0,0,0)}};"// &
    "r.onerror=function(e){if(e.error==='not-allowed'||e.error==='service-not-allowed'){window.__mvon=false;"// &
    "E._native_event(10,0,0,3)}else if(e.error==='audio-capture'||e.error==='language-not-supported'){"// &
    "window.__mvon=false;E._native_event(10,0,0,4)}};"// &
    "r.onend=function(){if(window.__mvon){try{r.start()}catch(e){}}};"// &
    "r.start()}catch(e){try{NativeTransport.engine._native_event(10,0,0,4)}catch(e2){}}})()")
   call play_cue('voiceStart')
  else
   call sys_script("(function(){window.__mvon=false;try{window.__mvr&&window.__mvr.abort()}catch(e){}})()")
   call play_cue('voiceStop')
  end if
 end subroutine
! @nativefn NR2-M03-F0125H
 subroutine draw_overlays()
  ! The "?" help switch in the bottom-right corner of every page and, when open, the panel with
  ! every voice command, key and mouse action.
  if(page<0)return
  if(help_on)call draw_help()
  call button(122,0,'?',width-60,height-60,48,48,.true.)
 end subroutine
 subroutine draw_help()
  character(len=90),parameter::vcmd(26)=[character(len=90):: &
   'Computer                      wake word: "Computer standing by"', &
   'Computer help                 this list (also spoken)', &
   'Computer close help           close this panel', &
   'Computer status report        run, cycle, block and lid, runs filed', &
   'Computer start run            start the virtual PCR (profile or selection)', &
   'Computer show live run        open the run that is acquiring', &
   'Computer stop run             stop and file the current run', &
   'Computer show results         results table of the filed runs', &
   'Computer next run             next filed run', &
   'Computer previous run         previous filed run', &
   'Computer check the controls   PC, NTC, extraction blank, verdict', &
   'Computer target channel       target gene curves (FAM)', &
   'Computer endogenous channel   endogenous gene curves (YY)', &
   'Computer temperature          block temperature chart', &
   'Computer zoom in / zoom out   chart magnification', &
   'Computer reset view           chart zoom and position', &
   'Computer scenarios            qPCR scenario catalogue', &
   'Computer references           sources and references', &
   'Computer animation theatre    full-screen scenes', &
   'Computer profile              configure profile', &
   'Computer download             RDMLt file of the selected run', &
   'Computer go home              home page', &
   'Computer go back              one level back', &
   'Computer sound on / sound off interface sounds', &
   'Computer red alert / cancel red alert', &
   'Computer stop listening       microphone off']
  character(len=90),parameter::keys(9)=[character(len=90):: &
   'Mic / Voice commands button   microphone on or off (every page)', &
   '? button, F1 or ?             open / close this help', &
   'Esc                           close help; otherwise home', &
   'Enter                         start screen: enter the app', &
   'Space                         stop the run that is acquiring', &
   'Left / Right arrows           previous / next theatre scene', &
   'Mouse wheel                   zoom live chart and molecules', &
   'Drag                          pan the chart, rotate molecules', &
   'Click a card or view          open it; Back goes one level up']
  integer::x,y,w,h,i
  character(len=60)::pk
  w=min(width-80,1500);h=min(height-90,880);x=(width-w)/2;y=40
  call soft_panel(x,y,w,h,14,rgba(16,10,20,248))
  call card_frame(x,y,w,h,orange,3)
  call hotspot(122,0,x,y,w,h)                 ! click anywhere on the panel to close it
  call label('Help: commands',x+24,y+16,orange,2)
  call label('Voice: switch the microphone on, then say "Computer" followed by the command. Click the panel or press Esc to close.', &
   x+24,y+56,ink,1)
  call label('Voice commands',x+24,y+92,cyan,1)
  do i=1,size(vcmd)
   call label(trim(vcmd(i)),x+24+merge(0,w/2,i<=13),y+120+mod(i-1,13)*26,ink,1)
  end do
  call label('Keys, buttons and mouse',x+24,y+480,cyan,1)
  do i=1,size(keys)
   call label(trim(keys(i)),x+24+merge(0,w/2,i<=5),y+508+mod(i-1,5)*26,ink,1)
  end do
  pk=pack_text()
  if(len_trim(pk)>0)call label(trim(pk),x+24,y+h-60,purple,1)
  call label('Answers use the MISSY Calm v1 voice; other replies use a local English voice.',x+24,y+h-34,purple,1)
 end subroutine
! @nativefn NR2-M03-F0125P
 subroutine voice_pack()
  ! As in MISSY: check the browser's on-device English speech pack (en-US, processed locally) and
  ! download it when the browser offers it, at start-up; recognition then runs offline. Browsers
  ! without on-device recognition keep their normal recogniser.
  vc_pack=1
  call sys_script("(function(){try{var C=window.SpeechRecognition||window.webkitSpeechRecognition;"// &
   "var E=NativeTransport.engine;if(!C||typeof C.available!=='function'){E._native_event(10,0,0,13);return}"// &
   "var o={langs:['en-US'],processLocally:true};C.available(o).then(function(st){"// &
   "if(st==='available'){window.__mvpack=true;E._native_event(10,0,0,11);return}"// &
   "if((st==='downloadable'||st==='downloading')&&typeof C.install==='function'){E._native_event(10,0,0,12);"// &
   "C.install(o).then(function(ok){window.__mvpack=!!ok;E._native_event(10,0,0,ok?11:14)})"// &
   ".catch(function(){E._native_event(10,0,0,14)});return}"// &
   "E._native_event(10,0,0,13)}).catch(function(){E._native_event(10,0,0,13)})}"// &
   "catch(e){try{NativeTransport.engine._native_event(10,0,0,13)}catch(e2){}}})()")
 end subroutine
 function pack_text() result(t)
  character(len=60)::t
  select case(vc_pack)
  case(1)
   t='English voice pack: checking'
  case(2)
   t='English voice pack: downloading'
  case(3)
   t='English voice pack: ready (offline)'
  case(4)
   t='English voice pack: not offered (browser recogniser)'
  case(5)
   t='English voice pack: download failed (browser recogniser)'
  case default
   t=''
  end select
 end function
 subroutine mic_button(x,y,w)
  ! Voice-command (microphone) switch, present on every page.
  integer,intent(in)::x,y,w
  if(vc_on)then
   call button(120,0,'Mic: on',x,y,w,42,.true.)
  else
   call button(120,0,'Mic: off',x,y,w,42,.true.)
  end if
 end subroutine
! @nativefn NR2-M03-F0125W
 subroutine voice_event(kind,value)
  integer,intent(in)::kind,value
  select case(kind)
  case(8)
   if(value>=32.and.value<127.and.vc_n<199)then
    vc_n=vc_n+1;vc_buf(vc_n:vc_n)=achar(value)
   end if
  case(9)
   if(vc_n>0)call voice_command(vc_buf(1:vc_n))
   vc_buf='';vc_n=0
  case(10)
   select case(value)
   case(3)
    vc_on=.false.;call play_cue('voicePermission')
   case(4)
    vc_on=.false.;call play_cue('voiceUnavailable')
   case(11)
    vc_pack=3
   case(12)
    vc_pack=2
   case(13)
    vc_pack=4
   case(14)
    vc_pack=5
   end select
  end select
 end subroutine
! @nativefn NR2-M03-F0125X
 subroutine voice_command(heard)
  ! Grammar of the MISSY voice console: "Computer" (wake word) followed by a command. Answers
  ! use the MISSY cues and Calm voice; unknown commands get the "not in grammar" tone.
  character(len=*),intent(in)::heard
  character(len=200)::t,c
  integer::i,k
  t=' '//heard
  do i=1,len_trim(t)
   if(t(i:i)>='A'.and.t(i:i)<='Z')t(i:i)=achar(iachar(t(i:i))+32)
   if(index('.,;:!?"',t(i:i))>0)t(i:i)=' '
  end do
  k=index(t,'computer',back=.true.)
  if(k==0)return                                   ! no wake word: not addressed to MISSY
  c=adjustl(t(k+8:))
  vc_heard='Computer '//trim(c)
  if(len_trim(c)==0)then
   call play_cue('enter');vc_reply='Computer standing by.';return
  end if
  if(has('cancel red alert').or.has('cancel alert'))then
   vc_alert=.false.;call play_tone('map003');call speak('Manual red alert stopped.',.true.)
  else if(has('red alert'))then
   vc_alert=.true.;call play_cue('redalert')
  else if(has('stop listening').or.has('voice off').or.has('voice control off').or.has('voice commands off'))then
   call voice_toggle()
  else if(has('close help').or.has('hide help'))then
   help_on=.false.;call play_cue('commandClose')
  else if(has('help').or.has('what can i say').or.has('commands'))then
   help_on=.true.;call play_tone('map003')
   call speak('Say Computer, then: status report; start run; show live run; stop run; show results; '// &
    'next run; previous run; check the controls; target channel; endogenous channel; temperature; '// &
    'zoom in; zoom out; reset view; scenarios; references; animation theatre; go home; go back; '// &
    'sound on or off; red alert; stop listening.',.true.)
  else if(has('status'))then
   call play_tone('map003');call speak(voice_status_text(),.true.)
  else if(has('control'))then
   call play_tone('map003');call speak(controls_text(),.true.)
  else if(has('stop').or.has('abort'))then
   if(ui_busy())then
    call action(7,0);call play_cue('cancel')
   else
    call play_tone('map005');call speak('No run in progress.',.true.)
   end if
  else if(has('live').or.has('show run').or.has('watch').or.has('current run'))then
   if(ui_busy())then
    call action(55,0);call play_cue('reviewCurves')
   else
    call play_tone('map005');call speak('No run in progress.',.true.)
   end if
  else if(has('start').or.has('begin').or.has('run pcr').or.has('run the pcr'))then
   if(ui_busy())then
    call play_tone('map005');call speak('A run is already in progress. '//trim(live_run_text())//'.',.true.)
   else
    call action(55,0);call play_cue('working')
   end if
  else if(has('result'))then
   if(nh>0)then
    call action(3,0);call play_cue('resultsPage')
   else
    call play_tone('map005');call speak('No runs filed yet.',.true.)
   end if
  else if(has('next'))then
   call action(9,0);call play_cue('next')
  else if(has('previous').or.has('last run'))then
   call action(8,0);call play_cue('previous')
  else if(has('endogen').or.has('reference gene').or.has('second channel').or.has('yellow'))then
   if(page/=1)page=merge(1,page,ui_busy().or.nh>0)
   graphmode=2;call play_cue('signalSelected')
  else if(has('target').or.has('fam').or.has('fluorescence'))then
   if(page/=1)page=merge(1,page,ui_busy().or.nh>0)
   graphmode=0;call play_cue('signalSelected')
  else if(has('temperature'))then
   if(page/=1)page=merge(1,page,ui_busy().or.nh>0)
   graphmode=1;zoom=1;pan=0;call play_cue('graphSelected')
  else if(has('zoom in').or.has('magnif'))then
   zoom=min(20d0,zoom*1.2d0);call play_cue('zoomIn')
  else if(has('zoom out'))then
   zoom=max(1d0,zoom/1.2d0);call play_cue('zoomOut')
  else if(has('reset'))then
   call action(18,0);call play_cue('zoomReset')
  else if(has('scenario').or.has('fault'))then
   call action(20,0);call play_cue('sopPage')
  else if(has('reference').or.has('source'))then
   call action(6,0);call play_cue('license')
  else if(has('theatre').or.has('theater').or.has('animation'))then
   call action(73,0);call play_cue('graphsPage')
  else if(has('profile').or.has('configure'))then
   call action(5,0);call play_cue('profileSelect')
  else if(has('home').or.has('entry screen'))then
   page=0;view_mv=.false.;call play_cue('workspaceBack')
  else if(has('back').or.has('return'))then
   call action(1,0);call play_cue('consoleClose')
  else if(has('sound off').or.has('mute'))then
   call play_cue('affirmative');au_on=.false.
  else if(has('sound on'))then
   au_on=.true.;call play_cue('affirmative')
  else if(has('download').or.has('export'))then
   if(selected>0.and..not.ui_busy())then
    call action(11,0);call play_cue('download')
   else
    call play_tone('map005');call speak('Select a filed run first.',.true.)
   end if
  else
   call play_tone('map004')
   call speak('Unable to comply. Say Computer help for the command list.',.true.)
  end if
 contains
  logical function has(w)
   character(len=*),intent(in)::w
   has=index(' '//trim(c)//' ',' '//w)>0
  end function
 end subroutine
! @nativefn NR2-M03-F0125Y
 function voice_status_text() result(t)
  character(len=300)::t
  if(ui_busy())then
   t='Status report. '//trim(live_run_text())//'. Block '//trim(real_text(.5d0*(plant(3)+plant(4)),1))// &
    ' degrees. Lid '//trim(real_text(plant(5),0))//' degrees. '//trim(integer_text(nh))//' runs filed.'
  else
   t='Status report. No run in progress. '//trim(integer_text(nh))//' runs filed.'
  end if
 end function
! @nativefn NR2-M03-F0125Z
 function controls_text() result(t)
  ! Spoken check of the run controls of the selected (or latest) filed run.
  character(len=400)::t
  integer::s,k
  type(run_t)::rr
  k=selected;if(k<1)k=nh
  if(k<1)then
   t='No runs filed yet.';return
  end if
  rr=history(k);s=rr%ne*rr%nr
  t='Run '//trim(integer_text(k))//'. '
  if(rr%cq(s+1)>0)then
   t=trim(t)//' Positive control target Cq '//trim(real_text(rr%cq(s+1),1))//', endogene Cq '//trim(real_text(rr%cq2(s+1),1))//'.'
  else
   t=trim(t)//' Positive control target not detected.'
  end if
  t=trim(t)//merge(' NTC clean.             ',' NTC amplified.         ',rr%cq(s+2)<=0.and.rr%cq2(s+2)<=0)
  t=trim(t)//merge(' Extraction blank clean.    ',' Extraction blank amplified.',rr%cq(s+3)<=0.and.rr%cq2(s+3)<=0)
  select case(verdict(rr))
  case(1)
   t=trim(t)//' Controls passed.'
  case(4)
   t=trim(t)//' Controls passed. Deliberate scenario: review required.'
  case(2)
   t=trim(t)//' Control check failed. Investigate.'
  case default
   t=trim(t)//' Run partial or stopped. Not accepted.'
  end select
 end function
! @nativefn NR2-M03-F0125C
 function cue_for(id,l) result(c)
  ! MISSY cue (tone + phrase) for an action: the same station names as the MISSY workbench.
  integer,intent(in)::id
  character(len=*),intent(in)::l
  character(len=20)::c
  c='affirmative'
  select case(id)
  case(3)
   c='resultsPage'
  case(5)
   c='profileSelect'
  case(6)
   c='license'
  case(7)
   c='cancel'
  case(8)
   c='previous'
  case(9)
   c='next'
  case(10,28,58)
   c='signalSelected'
  case(11,12,40)
   c='download'
  case(13)
   c='loadPage'
  case(18)
   c='zoomReset'
  case(20)
   c='sopPage'
  case(55)
   c='working';if(ui_busy())c='reviewCurves'
  case(57,2)
   c='working'
  case(62)
   c='reviewInstrument'
  case(73,76)
   c='graphsPage'
  case(90)
   c='enter'
  case(122)
   c='commandOpen';if(help_on)c='commandClose'
  case(1,74)
   c='consoleClose';if(page==2.or.page==4.or.page==5.or.page==6.or.page==1)c='workspaceBack'
  case(106,97,99)
   c='graphSelected'
  end select
  if(index(l,'download')>0.or.index(l,'save')>0)c='download'
 end function
! @nativefn NR2-M03-F0125
 subroutine select_feedback(id,arg,lab)
  ! Action sound (MISSY R2 cue chosen as in the original: download, return, start, enter,
  ! confirm) before the action, then the spoken name of what was selected.
  integer,intent(in)::id,arg
  character(len=*),intent(in)::lab
  character(len=60)::l
  character(len=200)::say
  character(len=20)::cue
  integer::i
  l=lab
  do i=1,len_trim(l)
   if(l(i:i)>='A'.and.l(i:i)<='Z')l(i:i)=achar(iachar(l(i:i))+32)
  end do
  ! MISSY behaviour: the cue tone followed by the cue's Calm v1 phrase ("Result records
  ! accessed.", "Stop requested." ...). Views, scenes and molecules have their own description,
  ! which is spoken instead of the phrase (tone only).
  cue=cue_for(id,l)
  if(id==120)then
   call action(id,arg);return                  ! voice_toggle plays its own cue
  end if
  call action(id,arg)
  say=''
  select case(id)
  case(70,71,73)
   say=theatre_title(theatre_k)
  case(76,80,81,89)
   say=trim(mv_title())
  case(97,99,103,104)
   say=trim(view_kind(view_k))//' view. '//trim(view_desc(view_k))
  case(106)
   say=trim(group_name(arg))
   if(page==12)say=trim(view_kind(view_k))//' view. '//trim(view_desc(view_k))
  case(93)
   say='';cue='affirmative';if(.not.au_on)cue=''
  case(94)
   say='';cue='voiceStart';if(.not.voice_on)cue=''
  end select
  if(len_trim(say)>=2)then
   call play_tone(trim(cue));call speak(trim(say),.true.)
  else if(len_trim(cue)>0)then
   call play_cue(trim(cue))
  end if
 contains
  function mv_title() result(t)
   character(len=120)::t
   select case(mv_si)
   case(1)
    t='PDB 8 U C U. DNA polymerase alpha on primer and template DNA.'
   case(2)
    t='PDB 1 R F F. Tyrosyl DNA phosphodiesterase 1 with DNA.'
   case(3)
    t='PDB 1 A 3 Q. NF kappa B p52 homodimer bound to DNA.'
   case default
    t='PDB 1 R M 1. TBP and TFIIA on TATA box DNA.'
   end select
  end function
 end subroutine
! @nativefn NR2-M03-F0124
 subroutine button(id,arg,s,x,y,w,h,enabled)
  integer,intent(in)::id,arg,x,y,w,h
  logical,intent(in)::enabled
  character(len=*),intent(in)::s
  integer(c_int32_t)::c
  if(nb>=size(buttons,2))return
  nb=nb+1;buttons(:,nb)=[id,arg,x,y,w,h,0];if(enabled)buttons(7,nb)=1;blabel(nb)=s
  c=panel;if(enabled)c=purple
  if((id==2.or.id==53).and.enabled)c=orange
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
  type(curve_t)::lv
  real(c_double)::cfg(10),x(30000),y(30000),sty(40),mouse(3),sp(1),lo,hi,span,x0,x1,v
  integer(c_int32_t)::colors(26)
  integer(c_int)::starts(21),lab(64)
  integer::n,nc,w,i,k,nl
  type(c_ptr)::p
  colors=orange;colors(1:6)=[bg,int(z'FF392A40',c_int32_t),purple,ink,cyan,panel]
  starts=0;sty=0;sty(1:39:2)=1.4d0;mouse=[mx,my,-1d100];sp=0;n=0;lab=0
  lv=curves_of(r);if(running.and.r%serial==current%serial)lv=live_curves(r)
  if(graphmode/=1)then
   nc=r%nw;sty(2:40:2)=-1
   do w=1,nc
    starts(w)=n
    if(.not.ui_busy().and.selected>0)then
     if(.not.result_visible(r,w))cycle
    end if
    do i=1,r%ncy
     n=n+1;x(n)=i;y(n)=lv%y(i,w)
     if(graphmode==2)y(n)=lv%y2(i,w)
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
  character(len=52)::name
  integer::s,e,rep
  ! Unique reaction name with a common root per type (S1-E<e>-P<p>, PC, NTC, EB, LOD<n>) and the
  ! run identifier (start date-time and run number), as written to the exported file.
  s=r%ne*r%nr;e=0;rep=0
  name=well_name(r,w)
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
  call label('Position',60,250,purple,1);call label('Sample name (unique)',150,250,purple,1)
  call label('Role',470,250,purple,1);call label('Extraction',650,250,purple,1);call label('Replicate',770,250,purple,1)
  call label('Target (FAM) Cq',890,250,purple,1);call label('Endogene (YY) Cq',1040,250,purple,1)
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
   if(w==s+4)role='LOD control 1'
   if(w>s+4)role='LOD control '//trim(integer_text(w-s-3))
   pos=recorded_position(r%scenario,r%params,w,r%ne,r%nr)
   call label(achar(65+pos/24)//trim(integer_text(mod(pos,24)+1)),60,y,ink,1)
   call label(trim(result_name(r,w)),150,y,ink,1);call label(trim(role),470,y,ink,1)
   if(e>0)call label(trim(integer_text(e)),650,y,ink,1)
   if(rep>0)call label(trim(integer_text(rep)),770,y,ink,1)
   status='Not detected';if(r%cq(w)>0)status=real_text(r%cq(w),2)
   if(r%present(w)==0)status='Missing control'
   call label(trim(status),890,y,ink,1)
   status='Not detected';if(r%cq2(w)>0)status=real_text(r%cq2(w),2)
   if(r%present(w)==0)status='Missing control'
   call label(trim(status),1040,y,ink,1)
  end do
  if(row==0)call label('No names selected. Open Display selection to restore the view.',60,300,cyan,1)
 end subroutine
! @nativefn NR2-M03-F0008
 subroutine draw_profile()
  character(len=36),parameter::names(8)=[character(len=36)::'Number of runs','DNA extractions','PCR replicates per extraction', &
   'Starting DNA copies','Net threshold counts','Seed','Sensor fault: 0 normal / 1 bias','Acceleration (168 = 30 s run)']
  integer::i,y,vals(8)
  vals=[p_runs,p_ext,p_rep,p_copies,p_threshold,p_seed,p_fault,p_speed]
  do i=1,8
   y=165+(i-1)*52
   call label(trim(names(i)),60,y+12,ink,1)
   call label(trim(integer_text(vals(i))),470,y+8,orange,2)
   call button(14,i,'-',650,y,64,42,.not.ui_busy())
   call button(15,i,'+',730,y,64,42,.not.ui_busy())
  end do
  call label('Fixed generic probe assay: positive, NTC, extraction blank, and 2 LOD wells.',60,605,purple,1)
  call button(12,0,'Download profile',60,655,180,46,.true.)
  call button(13,0,'Load profile / native RDMLt',260,655,250,46,.not.ui_busy())
  call button(2,0,'Start batch',530,655,180,46,.not.ui_busy().and.scenario_available(p_scenario))
  call button(20,0,'qPCR error scenarios',730,655,240,46,.not.ui_busy())
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
  ! Cards: open (click), select (tick box) and repeat count (- / +). Selected cases run in
  ! catalogue order, each the requested number of times.
  integer::i,code,x,y,col,row,cw,free,ncase,nrun,c
  integer(c_int32_t)::edge
  character(len=200)::info
  call label('qPCR error scenarios - editable Fortran configurations',60,128,orange,2)
  call label('Tick cards to queue them and set how many runs each. Click a card to see its animation, '// &
   'fault-versus-normal curves and settings.',60,173,ink,1)
  cw=(width-150)/2
  do i=1,6
   code=catalog_page*6+i;if(code>51)exit
   col=mod(i-1,2);row=(i-1)/2;x=60+col*(cw+30);y=208+row*142
   call button(21,code,'',x,y,cw,134,.not.ui_busy())
   edge=purple;if(sel_n(code)>0)edge=orange
   call soft_panel(x,y,cw,134,20,edge)
   call soft_panel(x+4,y+4,cw-8,126,17,purple)
   call label(trim(integer_text(code))//' | '//trim(scenario_ids(code))//' | '//trim(scenario_kinds(code)),x+14,y+9,bg,1)
   call label(trim(category_name(code)),x+cw-14-text_px(trim(category_name(code)),1),y+9,panel,1)
   call wrapped_px(trim(scenario_titles(code)),x+14,y+34,cw-36,bg)
   call label('Scene: '//trim(scene_name(code))//'  |  curves: full model vs normal',x+14,y+60+22*(min(2,wrap_rows(trim(scenario_titles(code)),cw-36))-1),panel,1)
   info=param_summary(code)
   call label(info(1:max(1,min(len_trim(info),(cw-240)/7))),x+14,y+104,panel,1)
   call check_box(50,code,x+cw-208,y+96,sel_n(code)>0,.not.ui_busy().and.scenario_available(code))
   call button(51,code,'-',x+cw-168,y+96,38,30,.not.ui_busy())
   call label(achar(195)//achar(151)//trim(integer_text(sel_n(code))),x+cw-122,y+102,bg,1)
   call button(52,code,'+',x+cw-70,y+96,38,30,.not.ui_busy().and.scenario_available(code))
  end do
  y=640
  call button(22,0,'Previous',60,y,130,42,catalog_page>0)
  call label(trim(integer_text(catalog_page+1))//' / 9',205,y+12,ink,1)
  call button(23,0,'Next',265,y,130,42,catalog_page<8)
  call label('Normal run',470,y+12,ink,1)
  call check_box(50,0,425,y+6,sel_n(0)>0,.not.ui_busy())
  call button(51,0,'-',570,y+6,38,30,.not.ui_busy())
  call label(achar(195)//achar(151)//trim(integer_text(sel_n(0))),616,y+12,ink,1)
  call button(52,0,'+',664,y+6,38,30,.not.ui_busy())
  ncase=0
  do c=0,51
   if(sel_n(c)>0.and.scenario_available(c))ncase=ncase+1
  end do
  nrun=selection_runs();free=maxruns-nh
  call button(53,0,'Run selection: '//trim(integer_text(ncase))//' case(s), '//trim(integer_text(nrun))//' run(s)', &
   730,y,330,42,.not.ui_busy().and.nrun>0.and.nrun<=free)
  call button(54,0,'Clear',1075,y,110,42,.not.ui_busy().and.nrun>0)
  call button(12,0,'Save profile + all 51 settings',60,y+58,310,42,.true.)
  call button(5,0,'Configure profile',390,y+58,210,42,.not.ui_busy())
  call label('History: '//trim(integer_text(nh))//' filed, '//trim(integer_text(free))// &
   ' free of 32. Each run uses the profile on the Configure page.',620,y+70,purple,1)
  call label(trim(message),60,y+122,cyan,1)
 end subroutine
! @nativefn NR2-M03-F0101
 subroutine check_box(id,arg,x,y,checked,enabled)
  integer,intent(in)::id,arg,x,y
  logical,intent(in)::checked,enabled
  call button(id,arg,'',x,y,30,30,enabled)
  if(checked)then
   call soft_panel(x+4,y+4,22,22,6,orange)
   call aa_line(x+9d0,y+15d0,x+14d0,y+21d0,bg,2.5d0,.false.,[0d0,0d0,1d4,1d4])
   call aa_line(x+14d0,y+21d0,x+22d0,y+9d0,bg,2.5d0,.false.,[0d0,0d0,1d4,1d4])
  else
   call soft_panel(x+5,y+5,20,20,5,panel)
   call soft_panel(x+8,y+8,14,14,3,purple)
  end if
 end subroutine
! @nativefn NR2-M03-F0102
 function category_name(code) result(s)
  integer,intent(in)::code
  character(len=24)::s
  select case(code)
  case(1:14)
   s='Hardware'
  case(15:22)
   s='Software'
  case(23:33)
   s='Reagents'
  case(34:40)
   s='Human: set-up'
  case(41:46)
   s='Human: handling'
  case default
   s='Interpretation'
  end select
 end function
! @nativefn NR2-M03-F0110
 subroutine draw_live()
  ! Live instrument view of the acquisition: the generic cycler driven by the thermal plant,
  ! controller outputs and detector readings of the run in progress (or the selected record).
  type(cycler_t)::s
  type(curve_t)::r
  real(c_double)::lo,hi,v,bw,bx,by,bh,pk
  integer::w,n,i,x0
  logical::live
  live=ui_busy()
  if(live)then
   r=live_curves(current)
  else if(selected>0)then
   r=curves_of(history(selected))
  else
   r=curves_of(current)
  end if
  if(live)then
   s%tz=plant(3:4);s%tsens=measured;s%tlid=plant(5);s%tsink=plant(6)
   s%duty=max(-1d0,min(1d0,plant(8:9)));s%lidduty=plant(10)
   s%fanstall=0;if(plant(13)>.5d0)s%fanstall=1
   s%reading=0
   if(current%ncy>0)then
    if(current%elapsed-current%acq_t(current%ncy)<1.5d0)s%reading=1
   end if
  else
   s%tz=25;s%tsens=25;s%tlid=25;s%tsink=25;s%reading=1
  end if
  s%mux=1+mod(int(anim_t*16),16)
  n=r%ncy;lo=1d9;hi=-1d9
  do w=1,min(16,r%nw)
   if(r%present(w)==0.or.n<1)cycle
   lo=min(lo,r%y(1,w));hi=max(hi,maxval(r%y(1:n,w)))
  end do
  if(hi<=lo)hi=lo+1
  do w=1,min(16,r%nw)
   if(r%present(w)==0.or.n<1)cycle
   s%glow(w)=max(0d0,min(1d0,(r%y(n,w)-lo)/max(400d0,hi-lo)))
  end do
  s%reading_level=maxval(s%glow)
  call draw_cycler(s,60d0,150d0,820d0,300d0,anim_t)
  if(live)then
   call label('Live: thermal plant, controller and detector of the run in progress',68,156,cyan,1)
  else
   call label('Idle instrument with the readings of the selected record',68,156,cyan,1)
  end if
  ! numbers
  x0=905
  call soft_panel(x0,150,width-x0-60,300,12,panel)
  call label('Set point  '//trim(real_text(target,1))//' C',x0+16,166,ink,1)
  call label('Zone 1     '//trim(real_text(s%tz(1),2))//' C',x0+16,192,orange,1)
  call label('Zone 2     '//trim(real_text(s%tz(2),2))//' C',x0+16,218,orange,1)
  call label('Thermistors '//trim(real_text(s%tsens(1),2))//' / '//trim(real_text(s%tsens(2),2)),x0+16,244,ink,1)
  call label('Lid        '//trim(real_text(s%tlid,1))//' C',x0+16,270,ink,1)
  call label('Heat sink  '//trim(real_text(s%tsink,1))//' C',x0+16,296,ink,1)
  call label('Peltier    '//trim(real_text(100*s%duty(1),0))//'% / '//trim(real_text(100*s%duty(2),0))//'%',x0+16,322,ink,1)
  call label('Cycle      '//trim(integer_text(n))//' / 45',x0+16,348,cyan,1)
  if(live)then
   if(s%reading>0)then
    call label('Reading: LED on / off per tube',x0+16,374,cyan,1)
   else if(target>=90)then
    call label('Denaturation',x0+16,374,orange,1)
   else
    call label('Annealing / extension',x0+16,374,purple,1)
   end if
  end if
  call label('Real time  '//trim(clock_text(wall_run,.false.))//' of ~'//trim(clock_text(last_full_elapsed/p_speed,.false.)),x0+16,400,ink,1)
  call label('Simulated  '//trim(clock_text(current%elapsed,.true.))//' of ~'//trim(clock_text(last_full_elapsed,.true.)),x0+16,426,orange,1)
  ! photodiode readings of the last cycle, one bar per tube
  bx=60;by=470;bw=(width-120)/16d0;bh=height-170-by-30
  call soft_panel(60,nint(by)-6,width-120,nint(bh)+40,12,panel)
  call label('Photodiode readings, cycle '//trim(integer_text(n))//' (net counts above cycle 1)',72,nint(by),ink,1)
  pk=0
  do w=1,min(16,r%nw)
   if(r%present(w)==0.or.n<1)cycle
   pk=max(pk,r%y(n,w)-r%y(1,w))
  end do
  pk=max(pk,400d0)
  do w=1,min(16,r%nw)
   if(r%present(w)==0.or.n<1)cycle
   v=max(0d0,(r%y(n,w)-r%y(1,w))/pk)
   i=nint(v*(bh-30))
   call soft_panel(nint(bx+(w-1)*bw+6),nint(by+bh+4)-i,nint(bw-12),max(4,i),4,well_colour(r,w))
   call label(trim(integer_text(w)),nint(bx+(w-1)*bw+bw/2-6),nint(by+bh+10),ink,1)
  end do
 end subroutine
! @nativefn NR2-M03-F0111
 integer(c_int32_t) function well_colour(r,w) result(c)
  type(curve_t),intent(in)::r
  integer,intent(in)::w
  integer::ss
  ss=r%ne*r%nr;c=orange
  if(w==ss+1)c=rgba(155,145,255,255)
  if(w==ss+2)c=rgba(75,220,135,255)
  if(w==ss+3)c=cyan
  if(w>=ss+4)c=rgba(255,210,70,255)
 end function
! @nativefn NR2-M03-F0114
 function run_status_line(shown_cycle) result(s)
  ! Run status in the wording of common instrument software: run of batch, stage / step, block and lid.
  integer,intent(in)::shown_cycle
  character(len=160)::s
  character(len=80)::rt
  integer::stg,stp
  if(ui_busy())then
   rt=live_run_text();rt=rt(1:index(rt,', cycle')-1)
   call stage_step(phase,stg,stp)
   if(stg<current%nhold)then
    s=trim(rt)//' | STAGE 1, STEP '//trim(integer_text(stg+1))
   else
    s=trim(rt)//' | STAGE 2, STEP '//trim(integer_text(stp+1))
   end if
   s=trim(s)//' | cycle '//trim(integer_text(shown_cycle))//' / 45 | Block Temp: '//trim(real_text(.5d0*(plant(3)+plant(4)),1))// &
    ' C | Lid Temp: '//trim(real_text(plant(5),0))//' C'
  else
   s='No run in progress | '//trim(integer_text(nh))//' run(s) filed | '//trim(integer_text(shown_cycle))//' / 45 cycles shown'
  end if
 end function
! @nativefn NR2-M03-F0114B
 function live_run_text() result(s)
  ! Which run of the batch / selection is acquiring, its cycle, and how many are still queued.
  character(len=80)::s
  integer::left,tot
  left=max(0,remaining-1)
  if(queue_active.and.qpos<qn)left=left+sum(qreps(qpos+1:qn))
  tot=current%serial+left
  s='run '//trim(integer_text(current%serial))//' of '//trim(integer_text(tot))//', cycle '// &
   trim(integer_text(current%ncy))//' / 45'
 end function
! @nativefn NR2-M03-F0115
 subroutine draw_protocol_strip(x,y,w,h)
  ! Protocol overview: HOLDING and CYCLING stages, one column per step with its temperature and
  ! hold time, the temperature profile drawn across the columns, the read (gather data) marker
  ! and the current step highlighted.
  integer,intent(in)::x,y,w,h
  real(c_double)::tt(4),dd(4),cw,yl,y0,y1
  integer::k,n,cur,stg,stp
  character(len=16)::tm
  n=2
  if(p_prog==1)then
   tt=[50d0,95d0,95d0,60d0];dd=[120d0,600d0,15d0,60d0];n=4
  else
   tt=[95d0,95d0,60d0,0d0];dd=[120d0,10d0,30d0,0d0];n=3
  end if
  if(ui_busy())then
   tt(n)=current%ann_t
  end if
  cur=0
  if(ui_busy())then
   call stage_step(phase,stg,stp)
   if(stg<current%nhold)then
    cur=stg+1
   else
    cur=current%nhold+1+stp
   end if
  end if
  cw=real(w,c_double)/n
  call soft_panel(x,y,w,h,8,panel)
  do k=1,n
   if(k==cur)call rectangle(nint(x+(k-1)*cw)+2,y+2,nint(cw)-4,h-4,rgba(70,45,60,255))
   if(k>1)call rectangle(nint(x+(k-1)*cw),y+16,1,h-18,purple)
  end do
  call label('HOLDING',x+8,y+2,purple,1)
  call label('CYCLING x45',nint(x+(n-2)*cw)+8,y+2,orange,1)
  do k=1,n
   yl=y+h-24-(tt(k)-40)/60*(h-46)
   call aa_line(x+(k-1)*cw+6,yl,x+k*cw-6,yl,cyan,2d0,.false.,[0d0,0d0,1d4,1d4])
   if(k<n)then
    y1=y+h-24-(tt(k+1)-40)/60*(h-46)
    call aa_line(x+k*cw-6,yl,x+k*cw+6,y1,cyan,1.4d0,.false.,[0d0,0d0,1d4,1d4])
   end if
   write(tm,'(i0,a,i2.2)')int(dd(k))/60,':',mod(int(dd(k)),60)
   call label(trim(real_text(tt(k),1))//' C  '//trim(tm),nint(x+(k-1)*cw)+8,y+h-20,ink,1)
  end do
  call disc_mark(nint(x+n*cw)-14,y+12)
 contains
  subroutine disc_mark(px,py)
   integer,intent(in)::px,py
   call soft_panel(px-7,py-7,14,14,7,orange)
  end subroutine
 end subroutine
! @nativefn NR2-M03-F0112
 function clock_text(sec,hours) result(s)
  real(c_double),intent(in)::sec
  logical,intent(in)::hours
  character(len=16)::s
  integer::t,hh,mm,ss
  character(len=2)::a,b
  t=max(0,nint(sec));hh=t/3600;mm=mod(t/60,60);ss=mod(t,60)
  write(a,'(i2.2)')mm;write(b,'(i2.2)')ss
  if(hours)then
   s=trim(integer_text(hh))//':'//a//':'//b
  else
   s=trim(integer_text(t/60))//':'//b
  end if
 end function
! @nativefn NR2-M03-F0113
 function time_line() result(s)
  ! Real (wall-clock) time of the run next to the instrument time it simulates.
  character(len=120)::s
  real(c_double)::sim
  if(ui_busy())then
   s='Real '//trim(clock_text(wall_run,.false.))//' of ~'//trim(clock_text(last_full_elapsed/p_speed,.false.))// &
    ' | real cycler '//trim(clock_text(current%elapsed,.true.))//' of ~'//trim(clock_text(last_full_elapsed,.true.))
  else if(selected>0)then
   sim=history(selected)%elapsed
   s='Recorded run: real cycler time '//trim(clock_text(sim,.true.))
  else
   s='A run takes ~'//trim(clock_text(last_full_elapsed/p_speed,.false.))//' here for ~'// &
    trim(clock_text(last_full_elapsed,.true.))//' on a real cycler'
  end if
 end function
! @nativefn NR2-M03-F0109
 function scene_name(code) result(s)
  integer,intent(in)::code
  character(len=32)::s
  select case(scene_family(code))
  case(1)
   s='instrument cross-section'
  case(2)
   s='molecules in the well'
  case(3)
   s='wells and plate map'
  case default
   s='data path and analysis'
  end select
 end function
! @nativefn NR2-M03-F0103
 function param_summary(code) result(s)
  ! First editable settings of a case, e.g. "Edge temperature deficit (C) = 7".
  integer,intent(in)::code
  character(len=200)::s
  integer::i,k
  s='';k=0
  do i=1,6
   if(len_trim(parameter_names(i,code))==0)cycle
   if(index(parameter_names(i,code),'scope')>0.or.index(parameter_names(i,code),'mask')>0)cycle
   if(index(parameter_names(i,code),'seed')>0)cycle
   if(k>0)then
    s=trim(s)//';  '//trim(parameter_names(i,code))//' = '//trim(real_text(scenario_bank(i,code),3))
   else
    s=trim(parameter_names(i,code))//' = '//trim(real_text(scenario_bank(i,code),3))
   end if
   k=k+1;if(k==2)exit
  end do
  if(k==0)s='No editable settings'
 end function
! @nativefn NR2-M03-F0028
 subroutine draw_scenario()
  ! Case page as cards: settings card plus the five picture cards (fault drawing, normal
  ! drawing, 3D view, second 3D view, curves). Cards keep a minimum size; when they do not all
  ! fit, arrows page through them. Picture cards open full screen when clicked.
  integer::i,gx,gy,gw,gh,cols,rows,per,pages,cw,ch,k,slot,cx,cy,y,ncard,gl(4)
  if(p_scenario<1)return
  ncard=1
  do i=1,4
   if(i==3.and.scene_family(p_scenario)/=1)cycle
   ncard=ncard+1;gl(ncard-1)=i
  end do
  gx=40;gy=122;gw=width-80;gh=height-122-150
  cols=max(1,min(3,(gw+18)/(480+18)));rows=max(1,min(2,(gh+18)/(240+18)))
  if(ncard==5.and.cols==3.and.rows==2)then
   cols=3                        ! 5 cards: 3 + 2
  end if
  per=cols*rows;pages=(ncard+per-1)/per
  card_page=max(1,min(pages,card_page))
  cw=(gw-(cols-1)*18)/cols;ch=(gh-(rows-1)*18)/rows
  do k=1,ncard
   slot=k-(card_page-1)*per
   if(slot<1.or.slot>per)cycle
   cx=gx+mod(slot-1,cols)*(cw+18);cy=gy+((slot-1)/cols)*(ch+18)
   if(k==1)then
    call settings_card(cx,cy,cw,ch)
   else
    call group_card(gl(k-1),cx,cy,cw,ch)
   end if
  end do
  if(pages>1)then
   call button(105,-1,'<',width-640,height-84,56,40,card_page>1)
   call label('More cards: '//trim(integer_text(card_page))//' / '//trim(integer_text(pages)),width-570,height-72,ink,1)
   call button(105,1,'>',width-400,height-84,56,40,card_page<pages)
  end if
  y=height-130
  call button(20,0,'Back to catalogue',60,y,210,42,.true.)
  call button(27,0,'Reset this case',285,y,175,42,.not.ui_busy())
  call button(12,0,'Save all settings',475,y,175,42,.true.)
  call button(2,0,'Run this case now',665,y,210,42,.not.ui_busy().and.scenario_available(p_scenario))
  call label('In selection',895,y+12,ink,1)
  call check_box(50,p_scenario,1010,y+6,sel_n(p_scenario)>0,.not.ui_busy().and.scenario_available(p_scenario))
  call button(51,p_scenario,'-',1050,y+6,38,30,.not.ui_busy())
  call label(achar(195)//achar(151)//trim(integer_text(sel_n(p_scenario))),1096,y+12,ink,1)
  call button(52,p_scenario,'+',1144,y+6,38,30,.not.ui_busy().and.scenario_available(p_scenario))
  call label(trim(message),60,y+60,cyan,1)
 end subroutine
! @nativefn NR2-M03-F0147
 function end_text(status,ncy) result(t)
  ! Why a fault run ended early (shown on the curves card and the full-screen curves).
  integer,intent(in)::status,ncy
  character(len=160)::t
  select case(status)
  case(3)
   t='Fault run stopped by the independent monitor'
  case(4)
   t='Fault run hit the protocol time limit: the block could not reach its set temperatures'
  case(5)
   t='Fault run: software crash, partial data only'
  case(6)
   t='Fault run rejected: invalid physical setup'
  case(7)
   t='Fault run stopped by the hardware interlock'
  case default
   t='Fault run ended early'
  end select
  t=trim(t)//' ('//trim(integer_text(ncy))//' of 45 cycles).'
 end function
! @nativefn NR2-M03-F0104
 subroutine plot_preview(x,y,w,h)
  ! Fault (coloured by well role) against the normal reference (purple), both computed by the
  ! full thermal / chemistry / detector model with the same seed and profile.
  integer,intent(in)::x,y,w,h
  logical::cmp
  real(c_double)::lo,hi,gx,gy,gw,gh,clip(4),v,v1,cqn,cqf
  integer::k,wl,s,nshow
  integer(c_int32_t)::c
  logical::have_n,have_f,stale
  character(len=160)::st
  call soft_panel(x,y,w,h,12,panel)
  gx=x+56;gy=y+34;gw=w-76;gh=h-96;clip=[gx,gy-2,gx+gw,gy+gh+2]
  have_n=pv_code==p_scenario.and.pv_state>=2
  have_f=pv_code==p_scenario.and.(pv_state==3.or.pv_state==4.or.(pv_state==2.and.preview_mode))
  stale=.false.
  if(pv_code==p_scenario.and.pv_state==3)stale=any(abs(pv_fault%params-scenario_bank(:,p_scenario))>1d-9)
  if(.not.scenario_available(p_scenario))then
   st='No curves: this case needs a melt workflow the probe model does not have.'
  else if(pv_code/=p_scenario.or.pv_state==0)then
   st='Press Recompute curves to simulate this case.'
  else if(pv_state==1)then
   st='Normal reference: cycle '//trim(integer_text(current%ncy))//' / 45'
  else if(pv_state==2)then
   st='Fault: cycle '//trim(integer_text(current%ncy))//' / 45, real '//trim(clock_text(wall_run,.false.))// &
    ' (cycler '//trim(clock_text(current%elapsed,.true.))//')'
  else if(pv_state==4)then
   st='Fault: cycle '//trim(integer_text(replay_cycles()))//' / '//trim(integer_text(pv_fault%ncy))//', real '// &
    trim(clock_text(replay_t,.false.))//' (precomputed at start-up)'
  else if(stale)then
   st='Settings changed: press Recompute curves.'
  else if(pv_code==p_scenario.and.pv_state==3.and.pv_fault%status/=1)then
   st=end_text(pv_fault%status,pv_fault%ncy)
  else
   st='Full model, same seed and profile.'
  end if
  cmp=w<700          ! small card: axes and curves only
  if(.not.cmp)call label(trim(st),x+14,y+8,cyan,1)
  if(cmp.and.pv_code==p_scenario.and.(pv_state==1.or.pv_state==2))then
   call label('Computing: cycle '//trim(integer_text(current%ncy))//' / 45',x+14,y+8,cyan,1)
  else if(cmp.and.pv_code==p_scenario.and.pv_state==3.and.pv_fault%status/=1)then
   call label('Run ended early: '//trim(integer_text(pv_fault%ncy))//' / 45 cycles (open for details)',x+14,y+8,orange,1)
  end if
  if(cmp)then
  else if(pv_show_norm)then
   call button(63,0,'Normal: shown',x+w-290,y+4,140,26,.true.)
  else
   call button(63,0,'Normal: hidden',x+w-290,y+4,140,26,.true.)
  end if
  if(cmp)then
  else if(pv_show_fault)then
   call button(64,0,'Fault: shown',x+w-142,y+4,130,26,.true.)
  else
   call button(64,0,'Fault: hidden',x+w-142,y+4,130,26,.true.)
  end if
  lo=1d9;hi=-1d9
  call range_of(pv_norm,have_n);call range_of_f()
  if(pv_code==p_scenario.and.pv_state==1.and.preview_mode)call range_of(live_curves(current),.true.)
  if(hi<lo)then
   lo=0;hi=1000
  end if
  if(hi-lo<200)hi=lo+200
  call aa_line(gx,gy+gh,gx+gw,gy+gh,purple,1d0,.false.,[0d0,0d0,1d4,1d4])
  call aa_line(gx,gy,gx,gy+gh,purple,1d0,.false.,[0d0,0d0,1d4,1d4])
  do k=0,45,5
   call aa_line(gx+gw*(k-1)/44d0,gy+gh,gx+gw*(k-1)/44d0,gy+gh+4,purple,1d0,.false.,[0d0,0d0,1d4,1d4])
   if(k>0.and.(.not.cmp.or.mod(k,10)==0))call label(trim(integer_text(k)),nint(gx+gw*(k-1)/44d0)-6,nint(gy+gh)+6,ink,1)
  end do
  call label(trim(integer_text(nint(hi))),x+6,nint(gy)-6,ink,1)
  call label(trim(integer_text(nint(lo))),x+6,nint(gy+gh)-14,ink,1)
  if(.not.cmp)call label('cycle',nint(gx+gw)-40,nint(gy+gh)+24,purple,1)
  if(have_n.and.pv_show_norm)call curves(pv_norm,.true.)
  if(pv_code==p_scenario.and.pv_state==1.and.preview_mode.and.pv_show_norm)call curves(live_curves(current),.true.)
  if(have_f.and.pv_show_fault)then
   if(pv_state==3)then
    call curves(pv_fault,.false.)
   else if(pv_state==4)then
    call curves(replayed(),.false.)
   else
    call curves(live_curves(current),.false.)
   end if
  end if
  ! Cq summary
  s=pv_norm%ne*pv_norm%nr
  if(cmp)then
  else if(have_n.and.(pv_state==3.or.pv_state==4))then
   st='Mean sample Cq  normal '//trim(cq_text(mean_cq(pv_norm,1,s)))//'  fault '//trim(cq_text(mean_cq(pv_fault,1,s)))// &
    '   PC '//trim(cq_text(cqc(pv_norm,s+1)))//' / '//trim(cq_text(cqc(pv_fault,s+1)))// &
    '   NTC '//trim(cq_text(cqc(pv_norm,s+2)))//' / '//trim(cq_text(cqc(pv_fault,s+2)))// &
    '   Blank '//trim(cq_text(cqc(pv_norm,s+3)))//' / '//trim(cq_text(cqc(pv_fault,s+3)))
   if(pv_channel==2)st='YY channel: '//trim(st)
   call label(trim(st),x+14,y+h-40,ink,1)
   call label('Fault run: '//trim(status_text(pv_fault%status))//'; '//trim(integer_text(pv_fault%ncy))// &
    ' cycles.   Normal = grey dashed, fault = colours.',x+14,y+h-20,orange,1)
  else
   if(w>=720)then
    call label('Normal: grey dashed.  Fault: orange samples, violet PC, green NTC, cyan blank, yellow LOD',x+14,y+h-30,ink,1)
   else
    call label('Normal grey dashed, fault in colours',x+14,y+h-30,ink,1)
   end if
  end if
 contains
  subroutine range_of(r,ok)
   type(curve_t),intent(in)::r
   logical,intent(in)::ok
   integer::i,j
   if(.not.ok)return
   do j=1,r%nw
    if(r%present(j)==0)cycle
    do i=1,r%ncy
     v=r%y(i,j);if(pv_channel==2)v=r%y2(i,j)
     if(v<0)cycle
     lo=min(lo,v);hi=max(hi,v)
    end do
   end do
  end subroutine
  subroutine range_of_f()
   if(.not.have_f)return
   if(pv_state==3.or.pv_state==4)then
    call range_of(pv_fault,.true.)
   else
    call range_of(live_curves(current),.true.)
   end if
  end subroutine
  subroutine curves(r,normal)
   type(curve_t),intent(in)::r
   logical,intent(in)::normal
   integer::i,j,ss
   real(c_double)::x0,y0,x1,y1
   ss=r%ne*r%nr
   do j=1,r%nw
    if(r%present(j)==0)cycle
    c=orange
    if(j==ss+1)c=rgba(155,145,255,255)
    if(j==ss+2)c=rgba(75,220,135,255)
    if(j==ss+3)c=cyan
    if(j>=ss+4)c=rgba(255,210,70,255)
    if(normal)c=rgba(185,185,195,255)
    do i=1,r%ncy-1
     v=r%y(i,j);v1=r%y(i+1,j)
     if(pv_channel==2)then
      v=r%y2(i,j);v1=r%y2(i+1,j)
     end if
     if(v<0.or.v1<0)cycle
     x0=gx+gw*(i-1)/44d0;x1=gx+gw*i/44d0
     y0=gy+gh-(v-lo)/(hi-lo)*gh;y1=gy+gh-(v1-lo)/(hi-lo)*gh
     if(normal)then
      call aa_line(x0,y0,x0+(x1-x0)*.55d0,y0+(y1-y0)*.55d0,c,1.4d0,.false.,clip)
     else
      call aa_line(x0,y0,x1,y1,c,1.6d0,.false.,clip)
     end if
    end do
   end do
  end subroutine
 end subroutine
! @nativefn NR2-M03-F0116
 integer function replay_cycles() result(n)
  ! Cycles of a cached fault run revealed so far, at the pace of a real run.
  n=min(pv_fault%ncy,max(1,ceiling(pv_fault%ncy*replay_t/max(1d0,last_full_elapsed/p_speed))))
 end function
! @nativefn NR2-M03-F0117
 function replayed() result(c)
  type(curve_t)::c
  c=pv_fault;c%ncy=replay_cycles()
 end function
! @nativefn NR2-M03-F0105
 real(c_double) function mean_cq(r,a,b) result(m)
  type(curve_t),intent(in)::r
  integer,intent(in)::a,b
  integer::j,n
  m=0;n=0
  do j=a,b
   if(r%present(j)==0)cycle
   if(pv_channel==1.and.r%cq(j)>0)then
    m=m+r%cq(j);n=n+1
   else if(pv_channel==2.and.r%cq2(j)>0)then
    m=m+r%cq2(j);n=n+1
   end if
  end do
  if(n>0)then
   m=m/n
  else
   m=0
  end if
 end function
! @nativefn NR2-M03-F0108
 real(c_double) function cqc(r,j) result(v)
  type(curve_t),intent(in)::r
  integer,intent(in)::j
  v=r%cq(j);if(pv_channel==2)v=r%cq2(j)
  if(r%present(j)==0)v=0
 end function
! @nativefn NR2-M03-F0106
 function cq_text(v) result(s)
  real(c_double),intent(in)::v
  character(len=16)::s
  if(v>0)then
   s=real_text(v,1)
  else
   s='neg'
  end if
 end function
! @nativefn NR2-M03-F0107
 function status_text(st) result(s)
  integer,intent(in)::st
  character(len=48)::s
  select case(st)
  case(1)
   s='completed'
  case(3)
   s='stopped by the independent monitor'
  case(4)
   s='protocol time limit'
  case(5)
   s='software crash, partial file'
  case(6)
   s='set-up rejected'
  case(7)
   s='hardware interlock'
  case default
   s='stopped'
  end select
 end function
! ---------------------------------------------------------------- startup screen
! @nativefn NR2-M03-F0050
 subroutine boot_step(k)
  ! One embedded component per frame. Each step loads or prepares real data; three steps
  ! carry known-answer checks (CRC-32, SHA-256, DEFLATE) and stop startup if they fail.
  integer,intent(in)::k
  real(c_double)::st,v(4096),x(19),cfg(4),s(4),c(34),a(14),pr(6),y(45),cq,mon(6),obs(6),mcfg(5),out(4),q
  integer::i,n,code
  character(len=2000)::sample
  logical::ok
  select case(k)
  case(1)
   call allocate_frame(width,height,bg)
  case(2)
   n=0
   do i=1,font_count
    if(any(glyph_rows((i-1)*font_height+1:i*font_height)/=0))n=n+1
   end do
   if(n<95)boot_failed=k
  case(3)
   buttons=0;nb=0
  case(4)
   st=real(p_seed,c_double)
   do i=1,size(v)
    v(i)=rng_uniform(st)
   end do
   if(minval(v)<0.or.maxval(v)>=1)boot_failed=k
  case(5)
   st=17
   do i=1,size(v)
    v(i)=rng_gauss(st)
   end do
   q=stdlib_mean(v);if(abs(q)>0.1d0.or.abs(stdlib_var(v)-1)>0.1d0)boot_failed=k
  case(6)
   x=0;x(1:7)=25;x(11)=.5d0;x(12)=1;cfg=[.55d0,.25d0,.06d0,.08d0];s=0
   do i=1,2000
    x(8)=r2_pid_step(cfg,s,95d0,x(1),.05d0,.0045d0);x(9)=x(8);call r2_thermal_step(x)
   end do
  case(7)
   mon=0;mon(2)=-1;mcfg=[2d0,2000d0,1000d0,-5d0,103d0]
   do i=1,200
    obs=[25d0,25d0,25d0,25d0,i*50d0,i*50d0];call r2_monitor_step(mon,obs,i*50d0,mcfg,out)
   end do
   if(out(4)/=0)boot_failed=k
  case(8)
   st=11;a=[1d0,30000d0,120d0,84d0,91d0,62d0,300d0,.95d0,.3d0,50d0,76d0,1d0,1d0,0d0]
   call r2_chem_init(c,a,st)
   do i=1,200
    call r2_chem_step(c,a,95d0,.05d0,st)
   end do
  case(9)
   call r2_probe_init(pr,200d0,0d0)
   do i=1,200
    call r2_probe_step(pr,1d6,60d0,200d0,0d0,30d0,300d0,70d0,1.5d0)
   end do
   q=r2_probe_counts(pr(1),pr(2),200d0,0d0,0d0,.96d0,10d0,0d0,60d0)
  case(10)
   do i=1,45
    y(i)=360+1200/(1+exp(-(i-25d0)/1.6d0))
   end do
   call analyse_curve(y,45,150d0,cq);if(cq<20.or.cq>30)boot_failed=k
  case(11)
   n=0
   do code=1,51
    if(len_trim(scenario_ids(code))>0.and.len_trim(scenario_titles(code))>0)n=n+1
   end do
   if(n/=51)boot_failed=k
  case(12)
   n=count(len_trim(parameter_names)>0);if(n==0)boot_failed=k
  case(13)
   n=0
   do code=1,51
    if(parameters_valid(code,scenario_defaults(:,code)))n=n+1
   end do
   if(n/=51)boot_failed=k
  case(14)
   if(p_ext<1.or.p_rep<1.or.p_runs<1)boot_failed=k
  case(15)
   if(nh==0)then
    do i=1,maxruns
     history(i)=run_t()
    end do
   end if
  case(16)
   call crc_init();if(crc_text('123456789',9)/=3421780262_c_int64_t)boot_failed=k
  case(17)
   if(sha256_hex('abc',3)/='ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad')boot_failed=k
  case(18)
   do i=1,len(sample)
    sample(i:i)=achar(48+mod(i*7,10))
   end do
   call deflate_fixed(sample,len(sample),ok);if(.not.ok.or.zlen<=0.or.zlen>=len(sample))boot_failed=k
  case(19)
   call ensure_work();work(1:1)=' ';call ensure_output(cap);output(1:cap)=0;output_size=0;used=0
  case(20)
   n=count(len_trim(ref_text)>0);if(n/=nref)boot_failed=k
  case(24)
   if(st_count/=4.or.st_n/=4511.or.count(st_kind==5)/=31.or.count(st_kind==7)/=1)boot_failed=k
  case default
   continue
  end select
 end subroutine
! @nativefn NR2-M03-F0051
 function boot_line(k) result(s)
  integer,intent(in)::k
  character(len=200)::s
  character(len=8)::tag
  tag='[LOADED]';if(k>20)tag='[READY] '
  if(boot_failed==k)tag='[FAILED]'
  s=trim(tag)//' '//d2(k)//'/'//d2(nboot)//' '//achar(194)//achar(183)//' '//trim(boot_label(k))//' '//achar(194)//achar(183)//' '// &
   trim(real_text(boot_el(k),1))//' ms elapsed (+'//trim(real_text(boot_el(k)-boot_el(k-1),1))//' ms)'
 contains
  function d2(i) result(t)
   integer,intent(in)::i
   character(len=2)::t
   t=achar(48+i/10)//achar(48+mod(i,10))
  end function
 end function
! @nativefn NR2-M03-F0052
 subroutine ring_segment(cx,cy,r0,r1,a0,a1,color,alpha)
  ! Filled annulus sector with anti-aliased edges, drawn at device resolution.
  real(c_double),intent(in)::cx,cy,r0,r1,a0,a1,alpha
  integer(c_int32_t),intent(in)::color
  integer::i,j,k
  real(c_double)::px,py,r,ang,cov,ex,ey,dr,mid,half,bx0,bx1,by0,by1,t
  mid=(a0+a1)/2;half=(a1-a0)/2
  ! Bounding box of this sector only (sampled along both arcs), so each segment scans its own area.
  bx0=huge(1d0);bx1=-huge(1d0);by0=huge(1d0);by1=-huge(1d0)
  do k=0,32
   t=(a0-2+(a1-a0+4)*k/32d0)/57.29577951308232d0
   bx0=min(bx0,cx+r0*cos(t),cx+r1*cos(t));bx1=max(bx1,cx+r0*cos(t),cx+r1*cos(t))
   by0=min(by0,cy+r0*sin(t),cy+r1*sin(t));by1=max(by1,cy+r0*sin(t),cy+r1*sin(t))
  end do
  do j=floor((by0-1)*render_scale),ceiling((by1+1)*render_scale)
   do i=floor((bx0-1)*render_scale),ceiling((bx1+1)*render_scale)
    px=(i+.5d0)/render_scale-cx;py=(j+.5d0)/render_scale-cy;r=sqrt(px*px+py*py)
    if(r<r0-1.or.r>r1+1)cycle
    ang=atan2(py,px)*57.29577951308232d0-mid
    ang=modulo(ang+180,360d0)-180
    if(abs(ang)>half+2)cycle
    dr=min(r-r0,r1-r)*render_scale
    ex=(half-abs(ang))/57.29577951308232d0*r*render_scale
    cov=max(0d0,min(1d0,dr+.5d0))*max(0d0,min(1d0,ex+.5d0))
    if(cov>0)call pixel_raw(i,j,color,cov*alpha)
   end do
  end do
 end subroutine
! @nativefn NR2-M03-F0056
 subroutine logo_dna(cx,cy,hr,angle)
  ! Rotating double helix inside the logo hexagon. Port of the dna_denature.f90 renderer:
  ! sphere impostors, Z-buffer, Lambert + specular lighting and depth cueing, clipped to the
  ! hexagon (circumradius hr, pointy top) and drawn at device resolution.
  real(c_double),intent(in)::cx,cy,hr,angle
  integer,parameter::nbp=10,ns=96
  character(len=nbp),parameter::seq='ATGCGATATA'
  real(c_double)::zb(ns,ns),pa(3,nbp),pb(3,nbp),q(3),d(3),th,zc,s,t,blen
  real(c_double),parameter::lx=-.45d0,ly=.55d0,lz=.70d0
  integer(c_int32_t)::mat(0:5)
  integer::i,k,ma,mb,ox,oy
  mat=[rgba(201,155,208,255),rgba(95,200,200,255),rgba(255,90,90,255),rgba(255,210,74,255),rgba(95,211,138,255), &
   rgba(90,169,255,255)]
  zb=huge(1d0);s=1.45d0*render_scale              ! device pixels per angstrom
  ox=nint(cx*render_scale)-ns/2;oy=nint(cy*render_scale)-ns/2
  do i=1,nbp
   th=angle/57.29577951308232d0+(i-1)*36d0/57.29577951308232d0;zc=(i-(nbp+1)/2d0)*3.4d0
   pa(:,i)=[9d0*cos(th),zc,9d0*sin(th)]
   pb(:,i)=[9d0*cos(th+155d0/57.29577951308232d0),zc,9d0*sin(th+155d0/57.29577951308232d0)]
  end do
  do i=1,nbp
   call ball(pa(:,i),2.1d0,0);call ball(pb(:,i),2.1d0,1)
   if(i<nbp)then
    do k=1,3
     t=k/4d0;q=pa(:,i)+t*(pa(:,i+1)-pa(:,i));call ball(q,1.25d0,0)
     q=pb(:,i)+t*(pb(:,i+1)-pb(:,i));call ball(q,1.25d0,1)
    end do
   end if
   select case(seq(i:i))
   case('A');ma=2;mb=3
   case('T');ma=3;mb=2
   case('G');ma=4;mb=5
   case default;ma=5;mb=4
   end select
   d=pb(:,i)-pa(:,i);blen=sqrt(sum(d*d))/2;d=d/(2*blen)
   do k=1,5
    t=blen*k/5d0;q=pa(:,i)+t*d;call ball(q,1.1d0,ma);q=pb(:,i)-t*d;call ball(q,1.1d0,mb)
   end do
  end do
 contains
  subroutine ball(p,r,m)
   real(c_double),intent(in)::p(3),r
   integer,intent(in)::m
   real(c_double)::sx,sy,sr,dx,dy,d2,nz,depth,diff,spec,inten,cue,hx,hy,cov,e
   integer::i0,i1,j0,j1,i,j,c(3),k
   sx=ns/2d0+s*p(1);sy=ns/2d0-s*p(2);sr=s*r
   i0=max(1,floor(sx-sr));i1=min(ns,ceiling(sx+sr));j0=max(1,floor(sy-sr));j1=min(ns,ceiling(sy+sr))
   cue=1-.35d0*(p(3)+11)/22
   do j=j0,j1
    do i=i0,i1
     dx=(i-.5d0-sx)/sr;dy=(sy-(j-.5d0))/sr;d2=dx*dx+dy*dy
     if(d2>=1)cycle
     hx=abs((ox+i-.5d0)/render_scale-cx);hy=abs((oy+j-.5d0)/render_scale-cy)
     if(hx>hr*.866d0-2.5d0.or.hx*.57735d0+hy>hr-2.5d0)cycle
     nz=sqrt(1-d2);depth=-p(3)-r*nz
     if(depth>=zb(i,j))cycle
     zb(i,j)=depth
     diff=max(0d0,dx*lx+dy*ly+nz*lz);spec=max(0d0,dx*(-.25d0)+dy*.30d0+nz*.92d0)**28
     inten=(.30d0+.70d0*diff)*cue;e=min(1d0,spec*1.2d0)
     do k=1,3
      c(k)=nint(min(255d0,ibits(mat(m),8*(k-1),8)*inten*(1-e)+255*e))
     end do
     cov=min(1d0,(1-sqrt(d2))*sr*1.5d0+.35d0)
     call pixel_raw(ox+i-1,oy+j-1,rgba(c(1),c(2),c(3),255),cov)
    end do
   end do
  end subroutine
 end subroutine
! @nativefn NR2-M03-F0053
 subroutine centred(s,cx,y,color,scale)
  character(len=*),intent(in)::s
  integer,intent(in)::cx,y,scale
  integer(c_int32_t),intent(in)::color
  call label(s,cx-text_px(s,scale)/2,y,color,scale)
 end subroutine
! @nativefn NR2-M03-F0054
 integer function text_px(s,scale) result(w)
  ! Width in logical pixels of UTF-8 text at the label scale.
  character(len=*),intent(in)::s
  integer,intent(in)::scale
  integer::i,code,step,adv
  adv=0;i=1
  do while(i<=len_trim(s))
   call utf8_code(s,i,code,step);adv=adv+glyph_advance(glyph(code))*scale*render_scale/2;i=i+step
  end do
  w=adv/render_scale
 end function
! @nativefn NR2-M03-F0055
 subroutine draw_boot()
  integer::x0,y0,ww,wh,k,stages,i,first,lines,lx,ly,lw,cx,cy,done,xr,wr,lg,stg
  integer::nl,j
  character(len=200)::rows(0:nboot+1)
  real(c_double)::sc,a0
  integer(c_int32_t)::seg(5),edge
  character(len=200)::txt
  seg=[rgba(245,155,72,255),rgba(217,137,92,255),rgba(214,162,211,255),rgba(171,127,192,255),rgba(224,165,96,255)]
  ww=min(width-60,1180);wh=height-40;x0=(width-ww)/2;y0=20
  call soft_panel(x0,y0,ww,wh,28,rgba(101,71,86,255));call soft_panel(x0+2,y0+2,ww-4,wh-4,26,rgba(5,5,7,255))
  call soft_panel(x0,y0,ww,64,28,rgba(240,160,94,255));call rectangle(x0,y0+32,ww,32,rgba(240,160,94,255))
  call rectangle(x0,y0+64,ww,8,rgba(5,5,7,255));call rectangle(x0,y0+40,24,wh-80,rgba(197,155,207,255))
  call label('MISSY / SYSTEM INITIALIZATION',x0+44,y0+22,rgba(21,13,18,255),1)
  done=0
  do k=1,nboot
   if(boot_el(k)>0.or.(booted.and.boot_failed==0))done=done+1
  end do
  if(boot_failed>0)done=boot_failed-1
  stages=0
  do stg=1,5
   if(done>=boot_last(stg))stages=stages+1
  end do
  ! ring of five startup stages (geometry of the MISSY R2 startup dial)
  sc=1.08d0;cx=x0+200;cy=y0+215
  do k=1,5
   a0=-86+72d0*(k-1)
   if(k<=stages)then
    call ring_segment(real(cx,c_double),real(cy,c_double),79*sc,108*sc,a0,a0+64,seg(k),1d0)
   else
    call ring_segment(real(cx,c_double),real(cy,c_double),79*sc,108*sc,a0,a0+64,seg(k),.25d0)
   end if
   call centred('0'//achar(48+k),cx+nint(93.5d0*sc*cos((a0+32)/57.29577951308232d0)), &
    cy+nint(93.5d0*sc*sin((a0+32)/57.29577951308232d0))-9,rgba(9,9,9,255),1)
  end do
  call ring_segment(real(cx,c_double),real(cy,c_double),64*sc-.6d0,64*sc+.6d0,-180d0,180d0,rgba(104,76,112,255),1d0)
  call centred('S T A R T U P',cx,cy-34,rgba(211,160,215,255),1)
  call centred(achar(48+stages)//' / 5',cx,cy-4,rgba(245,163,85,255),2)
  ! instrument card: the MISSY logo is the entry control
  xr=x0+400;wr=ww-440;lx=xr+(wr-320)/2;ly=y0+96
  edge=rgba(103,209,216,255);if(booted.and.boot_failed==0)edge=rgba(240,160,94,255)
  if(booted.and.boot_failed==0)call button(90,0,'',lx,ly,320,112,.true.)
  call soft_panel(lx,ly,320,112,16,edge);call soft_panel(lx+3,ly+3,314,106,14,rgba(249,251,252,255))
  call molecule_icon(lx+58d0,ly+56d0,30d0,boot_anim/1000)
  do i=0,5
   a0=(60*i-90)/57.29577951308232d0
   call aa_line(lx+58+38*cos(a0),ly+56+38*sin(a0),lx+58+38*cos(a0+1.0471975511965976d0),ly+56+38*sin(a0+1.0471975511965976d0), &
    rgba(23,50,77,255),2.5d0,.false.,[0d0,0d0,1d5,1d5])
  end do
  call label('M I S S Y',lx+112,ly+16,rgba(23,50,77,255),2)
  call label('FORTRAN',lx+114,ly+58,rgba(31,138,138,255),1)
  call label('NOT VALIDATED',lx+114,ly+84,rgba(23,50,77,255),1)
  if(boot_failed>0)then
   txt='Startup stopped: '//trim(boot_label(boot_failed))//' failed. Reload the page.'
  else if(booted)then
   txt='Ready '//achar(226)//achar(128)//achar(148)//' click the MISSY logo or press Enter to enter.'
  else
   txt='Loading the Fortran edition '//achar(194)//achar(183)//' component '//trim(integer_text(min(nboot,done+1)))//' of '// &
    trim(integer_text(nboot))
  end if
  call label(txt,xr,y0+232,ink,1)
  call rectangle(xr,y0+268,wr,6,rgba(42,31,49,255))
  lg=nint(wr*real(done,c_double)/nboot)
  do i=0,lg-1
   call rectangle(xr+i,y0+268,1,6,rgba(240-nint(39d0*i/max(1,wr)),160-nint(6d0*i/max(1,wr)),94+nint(113d0*i/max(1,wr)),255))
  end do
  call label('Embedded Fortran components: display, numerical models, scenario catalogue, records and export, interface.', &
   xr,y0+296,purple,1)
  if(host_ms>=0)call label('WebAssembly module compiled and instantiated by the browser in '//trim(real_text(host_ms,1))//' ms.',xr,y0+326,purple,1)
  ! boot log
  ! Boot log starts below the dial and the status block, whichever is lower (no overlap at any size).
  lx=x0+50;lw=ww-90;ly=max(y0+410,cy+nint(108*sc)+78)
  call label('B O O T   L O G   /   E M B E D D E D   C O M P O N E N T S',lx,ly-46,rgba(199,154,199,255),1)
  call label('Component loading '//achar(226)//achar(128)//achar(148)//' three known-answer checks (CRC-32, SHA-256, DEFLATE). '// &
   'Times: elapsed startup (+ since previous component).',lx,ly-22,rgba(214,190,226,255),1)
  lines=max(6,(wh-(ly-y0)-150)/22)
  call soft_panel(lx,ly,lw,lines*22+20,12,rgba(104,76,112,255));call soft_panel(lx+2,ly+2,lw-4,lines*22+16,10,rgba(11,9,15,255))
   nl=0
   if(host_ms>=0)then
    rows(nl)='[LOADED] 00/'//trim(integer_text(nboot))//' '//achar(194)//achar(183)//' WebAssembly module (browser) '// &
     achar(194)//achar(183)//' '//trim(real_text(host_ms,1))//' ms';nl=nl+1
   end if
   if(pre_done_ms>0)then
    rows(nl)='[LOADED] '//achar(194)//achar(183)//' Preloaded during the start animation: theatre caches, normal reference run '// &
     '(full model, 45 cycles) '//achar(194)//achar(183)//' '//trim(real_text(pre_done_ms,0))//' ms';nl=nl+1
   end if
   do k=1,nboot
    if(boot_el(k)<=0.and.boot_failed/=k)cycle
    rows(nl)=boot_line(k);nl=nl+1
   end do
   if(booted.and.boot_failed==0)then
    rows(nl)='[READY] Initialization complete. No experiment files read during startup. Case curves continue in the background.';nl=nl+1
   end if
   first=max(0,nl-lines)
   do j=first,nl-1
    if(index(rows(j),'[FAILED]')==1)then
     call label(rows(j),lx+16,ly+10+(j-first)*22,orange,1)
    else
     call label(rows(j),lx+16,ly+10+(j-first)*22,rgba(214,190,226,255),1)
    end if
   end do
  ly=ly+lines*22+34
  do i=0,6
   call rectangle(lx+i*(lw/7)+3,ly,lw/7-6,5,cal(i+1))
  end do
  call label('Version: native Fortran edition '//achar(194)//achar(183)//' RDMLt export 2',lx,ly+22,rgba(212,176,224,255),1)
  txt=trim(integer_text(done))//' of '//trim(integer_text(nboot))//' Fortran components loaded '//achar(194)//achar(183)//' '// &
   trim(real_text(merge(boot_done_ms,boot_ms,booted),0))//' ms'
  if(host_mb>0)txt=trim(txt)//' '//achar(194)//achar(183)//' '//trim(integer_text(nint(host_mb)))//' MB WebAssembly memory'
  call label(txt,lx+lw-text_px(txt,1),ly+22,rgba(200,172,216,255),1)
 contains
  integer(c_int32_t) function cal(i)
   integer,intent(in)::i
   integer(c_int32_t),parameter::sw(7)=[int(z'FFDFD6CB',c_int32_t),int(z'FF4CC6D0',c_int32_t),int(z'FFCEC948',c_int32_t), &
    int(z'FF79B653',c_int32_t),int(z'FFBA6BB6',c_int32_t),int(z'FF6E6ED5',c_int32_t),int(z'FFC67D62',c_int32_t)]
   cal=sw(i)
  end function
 end subroutine
! @nativefn NR2-M03-F0123
 subroutine draw_preload()
  ! Preloading screen: theatre scene 2 (DNA from PDB 8UCU melting, annealing and extending)
  ! fills the window while the data are prepared; one loading bar along the bottom.
  real(c_double)::f
  integer::bw,i,lg
  character(len=160)::st
  call draw_theatre_scene(2,0d0,0d0,real(width,c_double),real(height-70,c_double),pre_ms/1000,boot_dt)
  f=0;st='Preparing: structures and simulation states'
  if(pre_phase>=1)then
   f=.1d0;st='Preparing: normal reference run (full model)'
   if(preview_mode.and.pv_bg.and.bg_phase==1)then
    f=.1d0+.9d0*current%ncy/45d0
    st='Preparing: normal reference run (full model), cycle '//trim(integer_text(current%ncy))//' / 45'
   end if
  end if
  if(pv_cached(0))then
   f=1;st='Prepared. Starting the Fortran components.'
  end if
  bw=width-480
  call label(trim(st),24,height-60,ink,1)
  call rectangle(24,height-30,bw,6,rgba(42,31,49,255))
  lg=nint(bw*f)
  do i=0,lg-1
   call rectangle(24+i,height-30,1,6,rgba(240-nint(39d0*i/max(1,bw)),160-nint(6d0*i/max(1,bw)),94+nint(113d0*i/max(1,bw)),255))
  end do
 end subroutine
! ---------------------------------------------------------------- references
! @nativefn NR2-M03-F0060
 integer function line_end(s,a,maxpx) result(b)
  ! Last byte of the longest run of whole words from s(a:) that fits in maxpx logical pixels.
  character(len=*),intent(in)::s
  integer,intent(in)::a,maxpx
  integer::k,n,lastspace
  n=len_trim(s);b=n;lastspace=0
  do k=a,n
   if(s(k:k)==' ')lastspace=k
   if(text_px(s(a:k),1)>maxpx)then
    b=k-1;if(lastspace>a)b=lastspace
    return
   end if
  end do
 end function
! @nativefn NR2-M03-F0062
 integer function wrap_rows(s,maxpx) result(rows)
  character(len=*),intent(in)::s
  integer,intent(in)::maxpx
  integer::a
  a=1;rows=0
  do while(a<=len_trim(s))
   a=max(a+1,line_end(s,a,maxpx)+1);rows=rows+1
   do while(a<=len_trim(s))
    if(s(a:a)/=' ')exit
    a=a+1
   end do
  end do
 end function
! @nativefn NR2-M03-F0063
 subroutine wrapped_px(s,x,y,maxpx,color)
  character(len=*),intent(in)::s
  integer,intent(in)::x,y,maxpx
  integer(c_int32_t),intent(in)::color
  integer::a,b,row
  a=1;row=0
  do while(a<=len_trim(s))
   b=max(a,line_end(s,a,maxpx));call label(s(a:b),x,y+row*22,color,1);a=b+1;row=row+1
   do while(a<=len_trim(s))
    if(s(a:a)/=' ')exit
    a=a+1
   end do
  end do
 end subroutine
! @nativefn NR2-M03-F0061
 subroutine draw_references()
  integer::i,y,chars,ystart,ybottom,pg,k,shown,last_section
  integer::starts(nref+1),npages
  logical::cont
  character(len=12)::num
  chars=width-112-80;ystart=200;ybottom=height-150
  ! page boundaries from the same wrapping rule used for drawing
  npages=1;starts(1)=1;y=ystart;last_section=0
  do i=1,nref
   k=wrap_rows(ref_text(i),chars)*22+12
   if(ref_section(i)/=last_section)k=k+34
   if(y+k>ybottom.and.y>ystart)then
    npages=npages+1;starts(npages)=i;y=ystart+34
   end if
   y=y+k;last_section=ref_section(i)
  end do
  starts(npages+1)=nref+1
  ref_page=max(0,min(npages-1,ref_page));pg=ref_page+1
  call label('References',60,128,orange,2)
  call label('Sources of the methods, standards, data formats and software used by this edition. Page '// &
   trim(integer_text(pg))//' of '//trim(integer_text(npages))//'.',60,168,ink,1)
  y=ystart;shown=0;last_section=0
  if(starts(pg)>1)last_section=-1
  do i=starts(pg),starts(pg+1)-1
   if(ref_section(i)/=last_section)then
    cont=.false.
    if(last_section==-1)cont=ref_section(i)==ref_section(starts(pg)-1)
    if(cont)then
     call label(trim(section_title(ref_section(i)))//' (continued)',60,y,cyan,1)
    else
     call label(trim(section_title(ref_section(i))),60,y,cyan,1)
    end if
    y=y+34;last_section=ref_section(i)
   end if
   num='['//trim(integer_text(i))//']'
   if(ref_section(i)==3)num=achar(194)//achar(183)
   call label(num,60,y,purple,1)
   call wrapped_px(trim(ref_text(i)),112,y,chars,ink)
   y=y+wrap_rows(ref_text(i),chars)*22+12
  end do
  call button(41,0,'Previous',60,height-122,140,44,pg>1)
  call label(trim(integer_text(pg))//' / '//trim(integer_text(npages)),220,height-110,ink,1)
  call button(42,0,'Next',320,height-122,140,44,pg<npages)
 end subroutine
! @nativefn NR2-M03-F0009
 function native_render() result(ptr) bind(c)
  type(c_ptr)::ptr
  integer::v,cw,shown_cycle,i,x0,nsrc
  character(len=220)::srcs(12)
  real(c_double)::a0,a1,b0,b1,clipmark(4)
  nb=0
  if(vc_pack==0)then                       ! from start-up: English speech pack and MISSY voice
   call voice_pack();call load_calm()
  end if
  if(page/=7.and.page/=11.and.page/=12.and.preview_mode.and..not.pv_bg)call cancel_preview()   ! background cache runs continue
  if(page==-2)then
   call allocate_frame(width,height,rgba(10,7,12,255));call draw_preload();call draw_memory_stats()
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  if(page==-1)then
   call allocate_frame(width,height,bg);call draw_boot();call draw_memory_stats()
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  if(page==9)then
   call allocate_frame(width,height,rgba(10,7,12,255))
   call label('Animation theatre  '//trim(integer_text(theatre_k))//' / '//trim(integer_text(nscene)),24,14,orange,2)
   call label(trim(theatre_title(theatre_k)),24,52,ink,1)
   if(show_tags)then
    call button(75,0,'Labels: on',width-790,16,160,42,.true.)
   else
    call button(75,0,'Labels: off',width-790,16,160,42,.true.)
   end if
   call button(70,0,'Previous',width-620,16,130,42,.true.)
   if(theatre_pause)then
    call button(72,0,'Play',width-480,16,110,42,.true.)
   else
    call button(72,0,'Pause',width-480,16,110,42,.true.)
   end if
   call button(71,0,'Next',width-360,16,130,42,.true.)
   call button(74,0,'Home',width-220,16,190,42,.true.)
   call mic_button(width-940,16,140)
   call draw_theatre_scene(theatre_k,16d0,78d0,real(width-32,c_double),real(height-90,c_double),theatre_t,theatre_dt)
   call draw_memory_stats()
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  ! Two device pixels per logical pixel: a full-screen window 1920 wide renders 3840 x 2160 (4K).
  ! (A higher scale on small windows was too slow for the 2-D drawings in the browser.)
  render_scale=2
  if(page==11)then
   call allocate_frame(width,height,rgba(10,7,13,255))
   call draw_view_menu()
   call draw_memory_stats()
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  if(page==12.and.view_mv)then
   ! molecule tools on a full-screen 3D molecule view
   call allocate_frame(width,height,rgba(10,7,13,255))
   call view_sources(view_k,nsrc,srcs)
   nsrc=nsrc+1;srcs(nsrc)=stats_one_line()
   call mv_draw(0d0,62d0,real(width,c_double),real(height-62-(22*nsrc+40),c_double))
   call soft_panel(10,height-(22*nsrc+34),width-20,22*nsrc+28,10,rgba(24,15,23,255))
   call label('Sources',24,height-(22*nsrc+28),cyan,1)
   do i=1,nsrc
    call label(trim(srcs(i)),110,height-(22*nsrc+28)+22*(i-1),ink,1)
   end do
   x0=16
   call mv_toolbar(x0,10)
   call button(102,0,'Animation',x0,10,110,42,.true.);x0=x0+115
   call button(99,-1,'View <',x0,10,90,42,.true.);x0=x0+95
   call button(99,1,'View >',x0,10,90,42,.true.);x0=x0+95
   call button(1,0,'Back',x0,10,90,42,.true.);x0=x0+95
   call mic_button(x0,10,120)
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  if(page==12)then
   call allocate_frame(width,height,rgba(10,7,13,255))
   call view_sources(view_k,nsrc,srcs)
   nsrc=nsrc+1;srcs(nsrc)=stats_one_line()
   th_zoom=.6d0                ! 3D views fill the full-screen page
   call case_view(view_k,0d0,62d0,real(width,c_double),real(height-62-(22*nsrc+40),c_double),anim_t)
   th_zoom=1
   call soft_panel(10,height-(22*nsrc+34),width-20,22*nsrc+28,10,rgba(24,15,23,255))
   call label('Sources',24,height-(22*nsrc+28),cyan,1)
   do i=1,nsrc
    call label(trim(srcs(i)),110,height-(22*nsrc+28)+22*(i-1),ink,1)
   end do
   call label(trim(view_kind(view_k))//'   '//trim(view_desc(view_k)),24,18,orange,1)
   if((view_k==3.and.scene_family(p_scenario)==2).or.view_k==4)call button(102,0,'Molecule tools',width-600,10,170,42,.true.)
   if(view_k<=2.and.scene_family(p_scenario)==1)then
    call button(108,1,'Front view',width-1060,10,140,42,view_part/=1)
    call button(108,2,'Section A-A',width-910,10,150,42,view_part/=2)
   end if
   if(view_k==5.or.view_k==6)call button(107,0,'Short circuit: '//trim(short_name(ckt_short)),width-760,10,330,42,.true.)
   call button(99,-1,'<',width-420,10,50,42,.true.)
   call button(99,1,'>',width-365,10,50,42,.true.)
   call button(1,0,'Back',width-310,10,130,42,.true.)
   call mic_button(width-170,10,150)
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  if(page==10)then
   call allocate_frame(width,height,rgba(10,7,13,255))
   x0=16
   call mv_toolbar(x0,16)
   call button(1,0,'Home',x0,16,75,42,.true.);x0=x0+80
   call mic_button(x0,16,120)
   call mv_draw(16d0,70d0,real(width-32,c_double),real(height-80,c_double))
   call draw_memory_stats()
   call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.;return
  end if
  shown_cycle=current%ncy
  if(.not.ui_busy().and.selected>0)shown_cycle=history(selected)%ncy
  if(page==1.and.graphmode==3)then
   call allocate_frame(width,height,bg)
  else if(page==1.and.(ui_busy().or.selected==0))then
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
  if(page==7)then
   call button(1,0,'Go back',width-200,32,170,42,.true.)
  else
   call button(1,0,'Home / Go back',width-200,32,170,42,.true.)
  end if
  if(vc_on)then
   call button(120,0,'Voice commands: on',700,32,280,42,.true.)
   if(len_trim(vc_heard)>0)then
    call label('Heard: '//trim(vc_heard),700,80,cyan,1)
   else
    call label('Say "Computer" then a command ("Computer help")',700,80,cyan,1)
   end if
  else
   call button(120,0,'Voice commands: off',700,32,280,42,.true.)
   if(vc_pack>0)call label(trim(pack_text()),700,80,purple,1)
  end if
  if(ui_busy().and.page/=1)call button(55,0,'Live run: '//trim(live_run_text()),width-900,32,340,42,.true.)
  if(au_on)then
   call button(93,0,'Sounds: on',width-540,32,160,42,.true.)
  else
   call button(93,0,'Sounds: off',width-540,32,160,42,.true.)
  end if
  if(voice_on)then
   call button(94,0,'Voice: on',width-370,32,160,42,.true.)
  else
   call button(94,0,'Voice: off',width-370,32,160,42,.true.)
  end if
  if(page==0)then
   call molecule_banner(60d0,122d0,real(width-520,c_double),172d0,anim_t)
   do i=1,4
    call hotspot(76,i,60+(i-1)*(width-520)/4,122,(width-520)/4,172)
   end do
   call soft_panel(width-400,180,340,18,9,purple)
   call soft_panel(width-310,210,250,18,9,orange)
   cw=(width-160)/3
   if(ui_busy())then
    call home_card(55,'Virtual PCR','Running: '//trim(live_run_text())//'. Click: live view / stop',1,60,300,cw,.true.,orange)
   else if(selection_runs()>0)then
    call home_card(55,'Virtual PCR','Run the selection: '//trim(integer_text(selection_runs()))//' run(s)',1,60,300,cw, &
     .not.ui_busy().and.selection_runs()<=maxruns-nh,orange)
   else
    call home_card(55,'Virtual PCR','Start the selected virtual experiment',1,60,300,cw,.not.ui_busy().and.scenario_available(p_scenario),orange)
   end if
   call home_card(3,'Results','Review recorded curves and controls',2,80+cw,300,cw,nh>0,purple)
   call home_card(20,'qPCR scenarios','Edit parameters and explore virtual faults',3,100+2*cw,300,cw,.not.ui_busy(),cyan)
   call home_card(5,'Configure profile','Set batches, DNA design and analysis',4,60,435,cw,.not.ui_busy(),purple)
   call home_card(13,'Load files','Open a profile or native RDMLt record',5,80+cw,435,cw,.not.ui_busy(),orange)
   call home_card(6,'References','Sources, standards, libraries and notes',6,100+2*cw,435,cw,.true.,purple)
   call home_card(73,'Animation theatre','Nine full-screen scenes: 3-D instrument, DNA, circuits, heat, optics, fluid, '// &
    'enzymes, amplification surface, molecular zoom',7,60,570,3*cw+40,.true.,orange)
   if(selection_runs()>0)then
    call label('Selection: '//trim(integer_text(selection_runs()))//' run(s) from the qPCR scenarios page (card 03)',60,700,cyan,1)
   else if(p_scenario==0)then
    call label('Scenario: normal run. Tick cards on the qPCR scenarios page to queue several cases.',60,700,cyan,1)
   else
    call label('Scenario: '//trim(scenario_ids(p_scenario)),60,700,cyan,1)
   end if
   if(warm_on.or.pv_bg)then
    call label('Preparing case curves in the background: '//trim(integer_text(cached_cases()))//' / 50',60,770,purple,1)
   else if(pv_cached(0))then
    call label('Case curves ready: '//trim(integer_text(cached_cases()))//' / 50 precomputed',60,770,purple,1)
   end if
   if(.not.scenario_available(p_scenario))call label('Melt acquisition is unavailable. Select another scenario or Normal virtual PCR.',60,735,orange,1)
  else if(page==1)then
   if(graphmode==3)call draw_live()
   call draw_protocol_strip(1140,height-140,max(240,width-1290),80)   ! bottom right, beside the run controls
   call label(trim(run_status_line(shown_cycle)),60,120,ink,1)
   call label(trim(time_line()),720,120,cyan,1)
   if(graphmode==0.or.graphmode==2)then
    call label('Sample',80,142,orange,1);call label('Positive control',190,142,purple,1)
    call label('NTC',370,142,rgba(75,220,135,255),1);call label('Blank',450,142,cyan,1)
    call label('LOD controls',540,142,rgba(255,210,70,255),1)
   end if
   if(graphmode==0.or.graphmode==2)call label(trim(integer_text(shown_cycle))//' cycles reached; missing readings omitted; integer-cycle markers only', &
    80,height-158,ink,1)
   if(graphmode==1)call label('Instrument minutes / block C | primary, secondary, setpoint',80,height-158,ink,1)
   call button(10,0,'Temperature / Fluorescence',60,height-132,245,44,.true.)
   call button(62,0,'Instrument',width-130,height-132,100,44,.true.)
   call button(18,0,'Reset chart',325,height-132,145,44,graphmode/=3)
   call button(3,0,'Results',490,height-132,140,44,nh>0)
   call button(7,0,'Stop and file current run',650,height-132,245,44,ui_busy())
   call button(28,0,'Target / endogene',915,height-132,210,44,graphmode==0.or.graphmode==2)
   if(graphmode==2)call label('Endogenous gene (YY channel)',740,142,cyan,1)
   if(graphmode==0)call label('Target gene (FAM channel)',740,142,cyan,1)
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
   call draw_references()
  end if
  if(page==2)then
   call button(8,0,'Previous run',60,132,150,36,selected>1)
   call button(9,0,'Next run',225,132,150,36,selected<nh)
   call label('Run '//trim(integer_text(selected))//' of '//trim(integer_text(nh)),405,143,cyan,1)
   call button(40,0,'Download all RDMLt (ZIP)',width-600,132,280,36,nh>0.and..not.ui_busy().and.remaining==0)
   call button(11,0,'Download selected RDMLt',width-300,132,260,36,selected>0)
  end if
  call draw_memory_stats()
  call draw_overlays();ptr=c_loc(pixels(1));dirty=.false.
 end function
! @nativefn NR2-M03-F0120
 function stats_lines(n) result(l)
  ! Memory figures and background case-curve progress (1 to 3 lines).
  integer,intent(out)::n
  character(len=120)::l(3)
  n=0;l=''
  if(page>=0.and.(warm_on.or.pv_bg))then
   n=n+1;l(n)='Case curves (background) '//trim(integer_text(cached_cases()))//' / 50'
  end if
  if(sys_native)then
   n=n+1;l(n)='Memory: native test build (no browser figures)'
  else
   n=n+1;l(n)='Memory  app heap '//trim(mb_text(sys_heap_mb))//'   JS heap '//trim(mb_text(sys_js_used_mb))
   if(sys_js_limit_mb>0)l(n)=trim(l(n))//' / '//trim(mb_text(sys_js_limit_mb))
   n=n+1;l(n)='Device RAM (browser estimate) '//trim(mb_text(sys_device_mb))
  end if
 end function
 function stats_one_line() result(t)
  ! The same figures on one line, for the Sources bar of full-screen views.
  character(len=220)::t
  character(len=120)::l(3)
  integer::n,i
  l=stats_lines(n);t='Status: '//trim(l(1))
  do i=2,n
   t=trim(t)//'  |  '//trim(l(i))
  end do
 end function
! @nativefn NR2-M03-F0120B
 subroutine draw_memory_stats()
  ! Memory figures and background progress, placed where they never cover content: in the page
  ! header beside the colour bars; on full-screen pages in the free strip under the top buttons;
  ! on start-up pages bottom right. (Full-screen views show them in their Sources bar instead.)
  character(len=120)::l(3)
  integer::n,i,bw,bx,by
  l=stats_lines(n)
  bw=0
  do i=1,n
   bw=max(bw,label_width(trim(l(i)),1))
  end do
  if(page<0)then
   bx=width-bw-34;by=height-12-22*n
   call soft_panel(bx,by-6,bw+24,22*n+10,8,rgba(24,16,30,235))
   bx=bx+12
  else if(page==9.or.page==11)then
   bx=width-bw-30;by=58;if(page==11)by=58
   if(page==9)then
    call label(trim(stats_one_line()),width-label_width(trim(stats_one_line()),1)-30,58,purple,1)
    return
   end if
   call label(trim(stats_one_line()),width-label_width(trim(stats_one_line()),1)-30,56,purple,1)
   return
  else if(page==10)then
   call label(trim(stats_one_line()),width-label_width(trim(stats_one_line()),1)-30,26,purple,1)
   return
  else
   bx=310;by=50
  end if
  do i=1,n
   if(index(l(i),'Case curves')==1)then
    call label(trim(l(i)),bx,by+(i-1)*20,orange,1)
   else
    call label(trim(l(i)),bx,by+(i-1)*20,merge(cyan,ink,i<n.or.n==1),1)
   end if
  end do
 end subroutine
! @nativefn NR2-M03-F0121
 function mb_text(m) result(t)
  integer,intent(in)::m
  character(len=24)::t
  if(m<0)then
   t='n/a'
  else if(m>=10240)then
   t=trim(integer_text(nint(m/1024d0)))//' GB'
  else if(m>=1024)then
   t=trim(integer_text(m/1024))//'.'//trim(integer_text(mod(m*10/1024,10)))//' GB'
  else
   t=trim(integer_text(m))//' MB'
  end if
 end function
! @nativefn NR2-M03-F0010
 subroutine change_field(i,d)
  integer,intent(in)::i,d
  if(ui_busy())return
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
   if(p_speed+d*24<=500)then
    p_speed=max(24,p_speed+d*24)
   else
    p_speed=max(24,min(5000,p_speed+d*500))
   end if
  end select
  message='Native profile updated. Save the profile to reuse these settings.'
 end subroutine
! @nativefn NR2-M03-F0011
 subroutine action(id,arg)
  integer,intent(in)::id,arg
  integer::nbk,vbk(4)
  select case(id)
  case(1)
   ! Go back one level: case page -> catalogue (same catalogue page), view -> view menu ->
   ! case page; everything else -> home.
   select case(page)
   case(7)
    page=6
   case(12)
    page=7      ! the case page holds the view cards
    if(view_group>0)then
     call group_views(view_group,nbk,vbk)
     if(nbk>1)page=11
    end if
   case(11)
    page=7
    if(pv_state==0.and.p_scenario>0)call start_preview(p_scenario)
   case default
    page=0
   end select
   edit_param=0;view_mv=.false.
  case(2)
   call start_batch();if(ui_busy())page=1
  case(3)
   if(nh>0)page=2
  case(5)
   if(.not.ui_busy())page=4
  case(6)
   page=5;ref_page=0
  case(41)
   ref_page=max(0,ref_page-1)
  case(42)
   ref_page=ref_page+1
  case(90)
   if(booted.and.boot_failed==0)page=0
  case(7)
   if(ui_busy())call finish_run(2)
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
   if(.not.ui_busy())request=1
  case(14)
   call change_field(arg,-1)
  case(15)
   call change_field(arg,1)
  case(18)
   zoom=1;pan=0
  case(120)
   call voice_toggle()
  case(122)
   help_on=.not.help_on
  case(20)
   if(.not.ui_busy())page=6
  case(21)
   if(.not.ui_busy())then
    call cancel_preview();p_scenario=arg;page=7;card_page=1;show_fault=.true.;edit_param=0;call start_preview(arg)
   end if
  case(50)
   if(.not.ui_busy().and.scenario_available(arg))then
    if(sel_n(arg)>0)then
     sel_last(arg)=sel_n(arg);sel_n(arg)=0
    else if(selection_runs()+max(1,sel_last(arg))<=maxruns-nh)then
     sel_n(arg)=max(1,sel_last(arg))
    else
     message='History capacity reached (32 runs per session).'
    end if
   end if
  case(51)
   if(.not.ui_busy().and.sel_n(arg)>0)then
    sel_n(arg)=sel_n(arg)-1;if(sel_n(arg)>0)sel_last(arg)=sel_n(arg)
   end if
  case(52)
   if(.not.ui_busy().and.scenario_available(arg))then
    if(selection_runs()<maxruns-nh)then
     sel_n(arg)=sel_n(arg)+1;sel_last(arg)=sel_n(arg)
    else
     message='History capacity reached (32 runs per session).'
    end if
   end if
  case(53)
   call start_selection();if(ui_busy())page=1
  case(55)
   if(ui_busy())then
    page=1;graphmode=0        ! a run is going on: open its live view (Stop is there)
   else
    if(selection_runs()>0)then
     call start_selection()
    else
     call start_batch()
    end if
    if(ui_busy())page=1
   end if
  case(54)
   if(.not.ui_busy())sel_n=0
  case(57)
   if(.not.ui_busy().and.p_scenario>0)then
    pv_force=.true.;call start_preview(p_scenario)
   end if
  case(58)
   pv_channel=3-pv_channel
  case(59)
   show_fault=.not.show_fault
  case(60)
   show_structure=.not.show_structure
  case(73)
   page=9;theatre_t=0
  case(70)
   theatre_k=1+mod(theatre_k-2+nscene,nscene);theatre_t=0;call theatre_reset()
  case(71)
   theatre_k=1+mod(theatre_k,nscene);theatre_t=0;call theatre_reset()
  case(72)
   theatre_pause=.not.theatre_pause
  case(74)
   page=0
  case(95)
   show_3d=.not.show_3d
  case(96)
   page=11;view_mv=.false.
  case(105)
   card_page=card_page+arg
  case(103)
   view_mv=.false.;page=12
   if(show_3d)then
    view_k=3
   else if(show_structure.and.scene_family(p_scenario)==2)then
    view_k=4
   else if(show_fault)then
    view_k=1
   else
    view_k=2
   end if
  case(104)
   view_mv=.false.;page=12;view_k=7;view_group=4
  case(100)
   mv_clip=max(0d0,mv_clip-.05d0)
  case(101)
   mv_clip=min(.95d0,mv_clip+.05d0)
  case(102)
   view_mv=.not.view_mv
   if(view_mv)then
    call mv_open(max(1,struct_idx));mv_spin=.true.
   end if
  case(97)
   view_k=arg;view_group=group_of(arg);page=12;view_part=1;call enter_view()
  case(108)
   view_part=arg
  case(107)
   ckt_short=mod(ckt_short+1,5)    ! cycle through the simulated shorts (restarts the circuit run)
  case(106)
   view_group=arg;view_mv=.false.
   call group_views(arg,nbk,vbk)
   if(nbk>1)then
    page=11
   else
    view_k=vbk(1);page=12;call enter_view()
   end if
  case(98)
   page=7;view_mv=.false.
   if(pv_state==0.and.p_scenario>0)call start_preview(p_scenario)
  case(99)
   ! a to-scale instrument drawing is two full screens (front view, section A-A): step
   ! through them before moving to the next view
   if(view_k<=2.and.scene_family(p_scenario)==1.and.((arg>0.and.view_part==1).or.(arg<0.and.view_part==2)))then
    view_part=view_part+arg
   else
    view_part=1
    view_k=1+mod(view_k-1+arg+nviews(),nviews())
    if(.not.view_ok(view_k))view_k=1+mod(view_k-1+arg+nviews(),nviews())
    if(.not.view_ok(view_k))view_k=1+mod(view_k-1+arg+nviews(),nviews())
    if(arg<0.and.view_k<=2.and.scene_family(p_scenario)==1)view_part=2
    view_group=group_of(view_k);page=12;call enter_view()
   end if
  case(93)
   au_on=.not.au_on
  case(94)
   voice_on=.not.voice_on
  case(76)
   call mv_open(arg);mv_spin=.true.;page=10
  case(80)
   call mv_open(1+mod(mv_si-2+st_count,st_count))
  case(81)
   call mv_open(1+mod(mv_si,st_count))
  case(82)
   mv_show=1+mod(mv_show,4)
  case(83)
   mv_col=1+mod(mv_col,4)
  case(84)
   mv_cut=max(3d0,mv_cut-1)
  case(85)
   mv_cut=min(20d0,mv_cut+1)
  case(86)
   mv_dist=max(15d0,mv_dist/1.25d0)
  case(87)
   mv_dist=min(800d0,mv_dist*1.25d0)
  case(88)
   mv_spin=.not.mv_spin
  case(89)
   call mv_open(mv_si)
  case(91)
   call mv_clear()
  case(75)
   show_tags=.not.show_tags
  case(63)
   pv_show_norm=.not.pv_show_norm
  case(64)
   pv_show_fault=.not.pv_show_fault
  case(62)
   if(graphmode==3)then
    graphmode=0
   else
    graphmode=3
   end if
  case(61)
   struct_idx=1+mod(struct_idx,st_count)
  case(22)
   catalog_page=max(0,catalog_page-1)
  case(23)
   catalog_page=min(8,catalog_page+1)
  case(24)
   if(.not.ui_busy())call edit_parameter(p_scenario,arg,-1)
  case(25)
   if(.not.ui_busy())call edit_parameter(p_scenario,arg,1)
  case(26)
   if(.not.ui_busy())then
    p_scenario=0;page=6;message='Normal run selected. Saved fault settings retained.'
   end if
  case(27)
   if(p_scenario>0.and..not.ui_busy())then
    scenario_bank(:,p_scenario)=scenario_defaults(:,p_scenario);call start_preview(p_scenario)
   end if
  case(40)
   if(nh>0.and..not.ui_busy().and.remaining==0)then
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
  if(kind>=8.and.kind<=10)then          ! speech recognition results (see voice_toggle)
   call voice_event(kind,value);dirty=.true.;return
  end if
  if(kind==7)then
   mx=mx
  else if(kind==1.or.kind==2.or.kind==3.or.kind==6)then
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
     call select_feedback(buttons(1,i),buttons(2,i),blabel(i));return
    end if
   end do
   if(page==1.and.my>=170.and.my<=height-190)then
    dragging=.true.;lastx=mx
   end if
   if((page==10.or.(page==12.and.view_mv)).and.my>=62.and.mx<width-340)then
    mv_drag=.true.;mv_lx=mx;mv_ly=my;mv_px=mx;mv_py=my
   end if
  case(2)
   if(mv_drag.and.(page==10.or.page==12))then
    mv_yaw=mv_yaw+(mx-mv_lx)*.01d0;mv_pitch=max(-1.5d0,min(1.5d0,mv_pitch+(my-mv_ly)*.01d0))
    if(abs(mx-mv_px)+abs(my-mv_py)>4)mv_spin=.false.
    mv_lx=mx;mv_ly=my
   end if
   if(dragging)then
    pan=pan-(mx-lastx)/max(1,width-110)/zoom;lastx=mx
   end if
  case(3)
   dragging=.false.
   if(mv_drag.and.(page==10.or.page==12).and.abs(mx-mv_px)+abs(my-mv_py)<=4)call mv_pick(mx,my)
   mv_drag=.false.
  case(7)
   host_ms=x;host_mb=y;boot_ms=max(boot_ms,x)
  case(4)
   if(page<0)then
    if(value==13.and.booted.and.boot_failed==0)page=0
    dirty=.true.;return
   end if
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
   if(page==9.and.value==37)call action(70,0)
   if(page==9.and.value==39)call action(71,0)
   if(help_on.and.(value==27.or.value==112.or.value==191))then
    help_on=.false.;dirty=.true.;return
   end if
   if(value==112.or.value==191)then
    help_on=.true.;dirty=.true.;return
   end if
   if(value==27)page=0
   if(value==32.and.ui_busy())call finish_run(2)
  case(5)
   width=max(1280,min(1920,int(x)));height=max(800,min(1200,int(y)))
  case(6)
   if(page==10.or.(page==12.and.view_mv))then
    if(value<0)mv_dist=max(15d0,mv_dist/1.12d0)
    if(value>0)mv_dist=min(800d0,mv_dist*1.12d0)
   end if
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
  if(page<0)return
  before=nh
  call load_file(a,n);dirty=.true.
  if(page==7.and.p_scenario>0.and..not.ui_busy())call start_preview(p_scenario)   ! loaded settings: new curves
  if(nh>before)page=2
  if(page==7.and.p_scenario==0)page=4
 end subroutine
! @nativefn NR2-M03-F0015
 integer(c_int) function native_tick(ms) result(changed) bind(c)
  real(c_double),value::ms
  logical::was_running
  was_running=running
  mem_t=mem_t+max(0d0,min(10000d0,ms))
  if(mem_t>=1000.or.sys_heap_mb<0)then
   mem_t=0;call sys_sample();dirty=.true.
  end if
  if(page==-2)then
   ! Preloading: scene 2 plays full screen while the structures, simulation states and the
   ! normal reference run (full model) are prepared; then the component start-up begins.
   pre_ms=pre_ms+max(0d0,min(100d0,ms));boot_dt=max(0d0,min(100d0,ms))/1000;dirty=.true.
   if(pre_phase==0)then
    call theatre_warm();call start_background(0);pre_phase=1
   else if(pv_cached(0))then
    pre_done_ms=pre_ms;page=-1;warm_on=.true.
   else if(.not.preview_mode)then
    call start_background(0)
   end if
  else if(page==-1)then
   ! Component start-up (25 Fortran components); the user enters with the MISSY logo.
   boot_anim=boot_anim+max(0d0,min(100d0,ms));dirty=.true.
   boot_ms=boot_ms+max(0d0,min(10000d0,ms))
   if(boot_pending>0)then
    boot_el(boot_pending)=boot_ms;boot_pending=0;dirty=.true.
   end if
   if(boot_failed==0.and.boot_next<=nboot)then
    call boot_step(boot_next)
    if(boot_failed==0)then
     boot_pending=boot_next
    else
     boot_el(boot_next)=boot_ms
    end if
    boot_next=boot_next+1;dirty=.true.
   else if(boot_failed==0.and..not.booted.and.boot_pending==0)then
    booted=.true.;boot_done_ms=boot_ms;dirty=.true.
   end if
   boot_dt=max(0d0,min(100d0,ms))/1000
  end if
  anim_t=anim_t+max(0d0,min(200d0,ms))/1000
  warm_hold=.false.        ! the background cache runs on every page (a real run pauses it)
  if(pv_state==4)then        ! replay keeps going on the views and full-screen pages
   replay_t=replay_t+max(0d0,min(200d0,ms))/1000
   if(replay_t>=last_full_elapsed/p_speed)pv_state=3
   dirty=.true.
  end if
  theatre_dt=0
  if(page==9.and..not.theatre_pause)then
   theatre_dt=max(0d0,min(100d0,ms))/1000;theatre_t=theatre_t+theatre_dt
  end if
  if(page==10.or.(page==12.and.view_mv))call mv_tick(max(0d0,min(100d0,ms))/1000)
  if(page==0.or.page==7.or.page==9.or.page==10.or.page==11.or.page==12.or.(page==1.and.graphmode==3))then
   anim_acc=anim_acc+max(0d0,ms)
   if(anim_acc>=33)then
    anim_acc=0;dirty=.true.
   end if
  end if
  call tick_model(ms)
  call narrate(was_running)
  changed=0;if(dirty.or.was_running.or.running.or.remaining>0.or.queue_active)changed=1
 end function
! @nativefn NR2-M03-F0126
 subroutine narrate(was_running)
  ! Spoken run report for a virtual PCR (not the case previews): start, the current cycle with
  ! block temperature and mean sample signal whenever the voice is free, the first cycle with
  ! clear amplification, and the filed result with the mean sample Cq.
  logical,intent(in)::was_running
  integer::k,s,w,n
  real(c_double)::m,b,cq
  character(len=240)::t
  if(preview_mode.or.was_running.and..false.)return
  if(running.and..not.nar_live)then
   nar_live=.true.;nar_cycle=0;nar_amp=0
   call play_cue('working')
   t='Virtual PCR started.'
   if(current%scenario>0)then
    t=trim(t)//' Case '//trim(scenario_ids(current%scenario))//'.'
   else
    t=trim(t)//' Normal run.'
   end if
   t=trim(t)//' '//trim(integer_text(current%planned_cycles))//' cycles, about '// &
    trim(integer_text(nint(last_full_elapsed/p_speed)))//' seconds here, '// &
    trim(integer_text(nint(last_full_elapsed/60)))//' minutes on a real cycler.'
   call speak(trim(t),.true.)
   return
  end if
  if(running)then
   k=current%ncy
   if(k>nar_cycle.and.k>0)then
    nar_cycle=k;s=current%ne*current%nr
    m=0;n=0
    do w=1,s
     if(current%present(w)==1.and.current%y(k,w)>=0)then
      m=m+current%y(k,w);n=n+1
     end if
    end do
    m=m/max(1,n)
    if(nar_amp==0.and.k>=8)then
     b=0;n=0
     do w=1,s
      if(current%present(w)==1.and.all(current%y(1:5,w)>=0))then
       b=b+sum(current%y(1:5,w))/5;n=n+1
      end if
     end do
     b=b/max(1,n)
     if(b>0.and.m>1.5d0*b)then
      nar_amp=k;call play_cue('received')
      call speak('Amplification detected at cycle '//trim(integer_text(k))//'.',.true.)
      return
     end if
    end if
    t='Cycle '//trim(integer_text(k))//' of '//trim(integer_text(current%planned_cycles))//'. Block '// &
     trim(integer_text(nint(measured(1))))//' degrees. Mean sample signal '//trim(integer_text(nint(m)))//'.'
    call speak(trim(t),.false.)
   end if
   return
  end if
  if(nar_live.and..not.running)then
   nar_live=.false.
   if(nh<=0)return
   s=history(nh)%ne*history(nh)%nr;cq=0;n=0
   do w=1,s
    if(history(nh)%cq(w)>0)then
     cq=cq+history(nh)%cq(w);n=n+1
    end if
   end do
   if(history(nh)%status==1)then
    call play_cue('loaded')
   else
    call play_cue('error')
   end if
   t=trim(message)
   if(n>0)then
    t=trim(t)//' Mean sample C q '//trim(real_text(cq/n,1))//'.'
   else
    t=trim(t)//' No sample crossed the threshold.'
   end if
   call speak(trim(t),.true.)
  end if
 end subroutine
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
  if(ui_busy())out(6)=1
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
