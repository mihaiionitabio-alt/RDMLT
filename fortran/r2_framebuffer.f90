! SPDX-License-Identifier: GPL-3.0-only
! Native software rasterizer. Packed 32-bit RGBA pixels; JS only blits the frame.
module r2_framebuffer
  use iso_c_binding, only: c_int,c_int8_t,c_int32_t,c_double,c_ptr,c_loc,c_null_ptr,c_int64_t
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  use r2_font
  use r2_browser, only: r2_axis_ticks
  implicit none
  private
  public :: r2_render_chart,r2_frame_heatmap,r2_pick_cell,r2_hsl_rgba
  public :: r2_frame_bmp,r2_frame_bmp_size
  public :: allocate_frame,pixel,pixel_raw,aa_line,glyph,text_width,pixels,fw,fh,rectangle,draw_text,rgba,number_text
  public :: render_scale
  integer, parameter :: render_scale=2,font_divisor=2
  integer(c_int32_t), allocatable, target, save :: pixels(:)
  integer(c_int8_t), allocatable, target, save :: bmp(:)
  integer, save :: fw=0,fh=0
contains
! @r2fn R2-C0137-F00001
  subroutine little_endian(offset,value,n)
    integer, intent(in) :: offset,n
    integer(c_int64_t), intent(in) :: value
    integer :: j,v
    do j=0,n-1
      v=int(iand(shiftr(value,8*j),255_c_int64_t)); if(v>127) v=v-256
      bmp(offset+j)=int(v,c_int8_t)
    end do
  end subroutine
! @r2fn R2-C0137-F00002
  function r2_frame_bmp_size() result(n) bind(c)
    integer(c_int) :: n
    n=0; if(fw>0 .and. fh>0) n=54+((fw*3+3)/4)*4*fh
  end function
! @r2fn R2-C0137-F00003
  function r2_frame_bmp() result(ptr) bind(c)
    type(c_ptr) :: ptr
    integer :: n,stride,x,y,i,offset,j,v
    integer(c_int32_t) :: color
    ptr=c_null_ptr; if(.not.allocated(pixels)) return
    n=r2_frame_bmp_size(); stride=((fw*3+3)/4)*4
    if(allocated(bmp)) then
      if(size(bmp)/=n) deallocate(bmp)
    end if
    if(.not.allocated(bmp)) allocate(bmp(n))
    bmp=0; bmp(1)=66; bmp(2)=77
    call little_endian(3,int(n,c_int64_t),4)
    call little_endian(11,54_c_int64_t,4); call little_endian(15,40_c_int64_t,4)
    call little_endian(19,int(fw,c_int64_t),4); call little_endian(23,int(fh,c_int64_t),4)
    call little_endian(27,1_c_int64_t,2); call little_endian(29,24_c_int64_t,2)
    call little_endian(35,int(stride*fh,c_int64_t),4)
    do y=0,fh-1
      do x=0,fw-1
        color=pixels((fh-1-y)*fw+x+1); offset=55+y*stride+x*3
        do j=0,2
          v=ibits(color,(2-j)*8,8); if(v>127) v=v-256
          bmp(offset+j)=int(v,c_int8_t)
        end do
      end do
    end do
    ptr=c_loc(bmp(1))
  end function
! @r2fn R2-C0137-F00004
  subroutine allocate_frame(w,h,bg)
    integer, intent(in) :: w,h
    integer(c_int32_t), intent(in) :: bg
    if(allocated(pixels)) then
      if(size(pixels)/=w*h*render_scale**2) deallocate(pixels)
    end if
    if(.not.allocated(pixels)) allocate(pixels(w*h*render_scale**2))
    fw=w*render_scale; fh=h*render_scale; pixels=bg
  end subroutine
! @r2fn R2-C0137-F00005
  function rgba(r,g,b,a) result(v)
    integer, intent(in) :: r,g,b,a
    integer(c_int32_t) :: v
    v=ior(ior(int(r,c_int32_t),shiftl(int(g,c_int32_t),8)), &
          ior(shiftl(int(b,c_int32_t),16),shiftl(int(a,c_int32_t),24)))
  end function
! @r2fn R2-C0137-F00006
  subroutine pixel_raw(x,y,color,coverage)
    integer, intent(in) :: x,y
    integer(c_int32_t), intent(in) :: color
    real(c_double), intent(in) :: coverage
    integer :: i,k,out(4),src,dst
    real(c_double) :: a
    if(x<0 .or. x>=fw .or. y<0 .or. y>=fh) return
    i=y*fw+x+1
    ! Opaque colour at full coverage: the blend below would return the colour itself.
    if(coverage>=1.0_c_double .and. ibits(color,24,8)==255) then
      pixels(i)=color; return
    end if
    a=max(0.0_c_double,min(1.0_c_double,coverage))*ibits(color,24,8)/255.0_c_double
    do k=0,2
      src=ibits(color,k*8,8); dst=ibits(pixels(i),k*8,8)
      out(k+1)=nint(src*a+dst*(1-a))
    end do
    pixels(i)=rgba(out(1),out(2),out(3),255)
  end subroutine
! @nativefn NR2-GFX-F00002
  subroutine pixel(x,y,color,coverage)
    integer,intent(in)::x,y
    integer(c_int32_t),intent(in)::color
    real(c_double),intent(in)::coverage
    integer::k,l
    do k=0,render_scale-1
      do l=0,render_scale-1
        call pixel_raw(x*render_scale+k,y*render_scale+l,color,coverage)
      end do
    end do
  end subroutine
! @r2fn R2-C0137-F00007
  subroutine rectangle(x,y,w,h,color)
    integer, intent(in) :: x,y,w,h
    integer(c_int32_t), intent(in) :: color
    integer :: i,j
    do j=max(0,y),min(fh-1,y+h-1)
      do i=max(0,x),min(fw-1,x+w-1)
        call pixel(i,j,color,1.0_c_double)
      end do
    end do
  end subroutine
! @r2fn R2-C0137-F00008
  subroutine aa_line(x0,y0,x1,y1,color,width,dashed,clip)
    real(c_double), intent(in) :: x0,y0,x1,y1,width,clip(4)
    integer(c_int32_t), intent(in) :: color
    logical, intent(in) :: dashed
    real(c_double) :: dx,dy,x,y,fx,fy,weight,offset,radius,cx,cy,length
    integer :: n,i,j,px,py,passes
    dx=(x1-x0)*render_scale; dy=(y1-y0)*render_scale; length=sqrt(dx*dx+dy*dy)
    if(length>100000) return
    n=max(1,ceiling(max(abs(dx),abs(dy)))); passes=max(1,ceiling(width*render_scale))
    do j=1,passes
      offset=real(j-1,c_double)-(passes-1)*0.5_c_double
      weight=min(1.0_c_double,width*render_scale/passes)
      do i=0,n
        if(dashed .and. mod(int(real(i,c_double)/n*length),7*render_scale)>=4*render_scale) cycle
        x=x0*render_scale+dx*i/n; y=y0*render_scale+dy*i/n
        if(abs(dx)>=abs(dy)) then
          y=y+offset
        else
          x=x+offset
        end if
        if(x<clip(1)*render_scale .or. x>clip(3)*render_scale .or. y<clip(2)*render_scale .or. y>clip(4)*render_scale) cycle
        px=floor(x); py=floor(y); fx=x-px; fy=y-py
        call pixel_raw(px,py,color,(1-fx)*(1-fy)*weight)
        call pixel_raw(px+1,py,color,fx*(1-fy)*weight)
        call pixel_raw(px,py+1,color,(1-fx)*fy*weight)
        call pixel_raw(px+1,py+1,color,fx*fy*weight)
      end do
    end do
  end subroutine
! @nativefn NR2-GFX-F00001
  subroutine acquisition_marker(x,y,r,color,clip)
    real(c_double),intent(in)::x,y,r,clip(4)
    integer(c_int32_t),intent(in)::color
    integer::i,j
    real(c_double)::d,cov
    if(x<clip(1).or.x>clip(3).or.y<clip(2).or.y>clip(4))return
    do j=floor((y-r)*render_scale),ceiling((y+r)*render_scale)
      do i=floor((x-r)*render_scale),ceiling((x+r)*render_scale)
        d=sqrt((i+.5_c_double-x*render_scale)**2+(j+.5_c_double-y*render_scale)**2)
        cov=max(0.0_c_double,min(1.0_c_double,r*render_scale+.5_c_double-d))
        if(cov>0)call pixel_raw(i,j,color,cov)
      end do
    end do
  end subroutine
! @r2fn R2-C0137-F00009
  integer function glyph(code) result(g)
    integer, intent(in) :: code
    integer :: lo,hi,m
    if(code>=32 .and. code<=126) then
      g=code-31; return
    end if
    lo=96; hi=font_count; g=32
    do while(lo<=hi)
      m=(lo+hi)/2
      if(glyph_codes(m)==code) then
        g=m; return
      else if(glyph_codes(m)<code) then
        lo=m+1
      else
        hi=m-1
      end if
    end do
  end function
! @r2fn R2-C0137-F00010
  integer function text_width(text,n) result(w)
    integer, intent(in) :: n,text(n)
    integer :: i
    w=0
    do i=1,n
      w=w+glyph_advance(glyph(text(i)))
    end do
    w=w/font_divisor
  end function
! @r2fn R2-C0137-F00011
  subroutine draw_text(text,n,x,y,color,rotate)
    integer, intent(in) :: n,text(n),x,y,rotate
    integer(c_int32_t), intent(in) :: color
    integer :: i,g,xx,yy,advance,level
    advance=0
    do i=1,n
      g=glyph(text(i))
      do yy=0,font_height-1
        do xx=0,font_width-1
          level=int(iand(shiftr(glyph_rows((g-1)*font_height+yy+1),2*xx),3_c_int64_t))
          if(level==0) cycle
          if(rotate==0) then
            call pixel_raw(x*render_scale+(advance+xx)*render_scale/font_divisor, &
              y*render_scale+yy*render_scale/font_divisor,color,level/3.0_c_double)
          else
            call pixel_raw(x*render_scale+yy*render_scale/font_divisor, &
              y*render_scale-(advance+xx)*render_scale/font_divisor,color,level/3.0_c_double)
          end if
        end do
      end do
      advance=advance+glyph_advance(g)
    end do
  end subroutine
! @r2fn R2-C0137-F00012
  subroutine number_text(value,text,n)
    real(c_double), intent(in) :: value
    integer, intent(out) :: text(32),n
    integer :: tmp(24),k,j,q,decimals,whole,frac
    real(c_double) :: v
    text=32; n=0; if(.not.ieee_is_finite(value)) return
    v=abs(value); if(value<0) then
      n=1; text(n)=45
    end if
    decimals=0
    if(abs(v-anint(v))>1e-7_c_double) decimals=1
    whole=int(v+1e-9_c_double); frac=nint((v-whole)*10)
    if(frac==10) then
      whole=whole+1; frac=0
    end if
    k=0; q=whole
    do
      k=k+1; tmp(k)=48+mod(q,10); q=q/10; if(q==0 .or. k>=20) exit
    end do
    do j=k,1,-1
      n=n+1; text(n)=tmp(j)
    end do
    if(decimals/=0) then
      n=n+1; text(n)=46; n=n+1; text(n)=48+frac
    end if
  end subroutine
! @r2fn R2-C0137-F00013
  function r2_hsl_rgba(h,s,l) result(color) bind(c)
    real(c_double), value :: h,s,l
    integer(c_int32_t) :: color
    real(c_double) :: c,x,m,a,r,g,b
    a=modulo(h,360.0_c_double)/60; c=(1-abs(2*l-1))*s; x=c*(1-abs(modulo(a,2.0_c_double)-1)); m=l-c/2
    r=0; g=0; b=0
    select case(int(a))
    case(0); r=c; g=x
    case(1); r=x; g=c
    case(2); g=c; b=x
    case(3); g=x; b=c
    case(4); r=x; b=c
    case default; r=c; b=x
    end select
    color=rgba(nint((r+m)*255),nint((g+m)*255),nint((b+m)*255),255)
  end function
! @r2fn R2-C0137-F00014
  function r2_render_chart(cfg,x,y,st,nc,nt,col,sty,lab,nl,mouse,sp,ns) result(ptr) bind(c)
    integer(c_int), value :: nc,nt,nl,ns
    real(c_double), intent(in) :: cfg(10),x(nt),y(nt),sty(2*nc),mouse(3),sp(2*ns)
    integer(c_int), intent(in) :: st(nc+1),lab(nl)
    integer(c_int32_t), intent(in) :: col(nc+6)
    type(c_ptr) :: ptr
    real(c_double) :: clip(4),px,py,lx,ly,dx,dy,ticks(64),nearest,bestdist,d,rx,ry
    integer :: w,h,i,j,k,n,count,text(32),tw,split,idx,best,series
    w=int(cfg(1)); h=int(cfg(2)); ptr=c_null_ptr
    if(w<80 .or. h<60 .or. w>4096 .or. h>4096 .or. nc<0 .or. nt<0) return
    call allocate_frame(w,h,col(1)); ptr=c_loc(pixels(1))
    clip=[cfg(7),cfg(8),cfg(7)+cfg(9),cfg(8)+cfg(10)]
    dx=cfg(4)-cfg(3); dy=cfg(6)-cfg(5); if(dx<=0 .or. dy<=0) return
    do i=1,ns
      px=max(clip(1),cfg(7)+(sp(2*i-1)-cfg(3))/dx*cfg(9))
      py=min(clip(3),cfg(7)+(sp(2*i)-cfg(3))/dx*cfg(9))
      if(py>px) call rectangle(nint(px),nint(clip(2)),nint(py-px),nint(cfg(10)),col(6))
    end do
    call r2_axis_ticks(cfg(3),cfg(4),max(3,nint(cfg(9)/90)),ticks,64,count)
    do i=1,count
      px=cfg(7)+(ticks(i)-cfg(3))/dx*cfg(9)
      call aa_line(px,clip(2),px,clip(4),col(2),1.0_c_double,.false.,clip)
      call number_text(ticks(i),text,n); tw=text_width(text,n)
      call draw_text(text,n,nint(px)-tw/2,nint(clip(4))+3,col(4),0)
    end do
    call r2_axis_ticks(cfg(5),cfg(6),5,ticks,64,count)
    do i=1,count
      py=cfg(8)+cfg(10)-(ticks(i)-cfg(5))/dy*cfg(10)
      call aa_line(clip(1),py,clip(3),py,col(2),1.0_c_double,.false.,clip)
      call number_text(ticks(i),text,n); tw=text_width(text,n)
      call draw_text(text,n,nint(clip(1))-tw-6,nint(py)-8,col(4),0)
    end do
    call aa_line(clip(1),clip(2),clip(3),clip(2),col(3),1.0_c_double,.false.,clip)
    call aa_line(clip(1),clip(4),clip(3),clip(4),col(3),1.0_c_double,.false.,clip)
    call aa_line(clip(1),clip(2),clip(1),clip(4),col(3),1.0_c_double,.false.,clip)
    call aa_line(clip(3),clip(2),clip(3),clip(4),col(3),1.0_c_double,.false.,clip)
    best=0; bestdist=100
    do series=1,nc
      idx=0
      do j=st(series)+1,st(series+1)
        if(.not.ieee_is_finite(x(j)) .or. .not.ieee_is_finite(y(j))) then
          idx=0; cycle
        end if
        px=cfg(7)+(x(j)-cfg(3))/dx*cfg(9); py=cfg(8)+cfg(10)-(y(j)-cfg(5))/dy*cfg(10)
        if(idx/=0) call aa_line(lx,ly,px,py,col(series+6),sty(2*series-1),sty(2*series)>0,clip)
        if(sty(2*series)<0) call acquisition_marker(px,py,2.2_c_double,col(series+6),clip)
        lx=px; ly=py; idx=j
        if(mouse(1)>=clip(1) .and. mouse(1)<=clip(3) .and. mouse(2)>=clip(2) .and. mouse(2)<=clip(4)) then
          d=(px-mouse(1))**2+(py-mouse(2))**2
          if(d<bestdist) then
            best=j; bestdist=d; rx=px; ry=py
          end if
        end if
      end do
    end do
    if(ieee_is_finite(mouse(3))) then
      px=cfg(7)+(mouse(3)-cfg(3))/dx*cfg(9)
      call aa_line(px,clip(2),px,clip(4),col(5),1.0_c_double,.false.,clip)
    end if
    split=0
    do i=1,nl
      if(lab(i)==0) then
        split=i; exit
      end if
    end do
    if(split>0) then
      tw=text_width(lab(1:split-1),split-1)
      call draw_text(lab(1:split-1),split-1,max(0,nint(cfg(7)+cfg(9)/2)-tw/2),h-19,col(4),0)
      tw=text_width(lab(split+1:nl),nl-split)
      call draw_text(lab(split+1:nl),nl-split,0,nint(cfg(8)+cfg(10)/2)+tw/2,col(4),1)
    end if
    if(best>0) then
      call aa_line(rx,clip(2),rx,clip(4),col(5),1.0_c_double,.false.,clip)
      call rectangle(w-148,15,140,38,col(1))
      call number_text(x(best),text,n); call draw_text(text,n,w-140,16,col(4),0)
      call number_text(y(best),text,n); call draw_text(text,n,w-140,34,col(4),0)
    end if
  end function
! @r2fn R2-C0137-F00015
  function r2_frame_heatmap(values,valid,n,w,h,cols,rows) result(ptr) bind(c)
    integer(c_int), value :: n,w,h,cols,rows
    real(c_double), intent(in) :: values(n)
    integer(c_int), intent(in) :: valid(n)
    type(c_ptr) :: ptr
    real(c_double) :: lo,hi,v
    integer :: i,x,y,cw,ch
    ptr=c_null_ptr
    if(w<=0 .or. h<=0 .or. w>4096 .or. h>4096 .or. cols<=0 .or. rows<=0) return
    call allocate_frame(w,h,rgba(14,10,22,255)); ptr=c_loc(pixels(1))
    lo=huge(lo); hi=-huge(hi)
    do i=1,n
      if(valid(i)==0 .or. .not.ieee_is_finite(values(i))) cycle
      lo=min(lo,values(i)); hi=max(hi,values(i))
    end do
    cw=max(1,w/cols); ch=max(1,h/rows)
    do i=1,min(n,cols*rows)
      x=mod(i-1,cols)*cw; y=(i-1)/cols*ch
      if(valid(i)==0) cycle
      v=0.5_c_double; if(hi>lo) v=(values(i)-lo)/(hi-lo)
      call rectangle(x+1,y+1,cw-2,ch-2,r2_hsl_rgba(270-240*v,0.8_c_double,0.3_c_double+0.3_c_double*v))
    end do
  end function
! @r2fn R2-C0137-F00016
  function r2_pick_cell(mx,my,w,h,cols,rows) result(cell) bind(c)
    real(c_double), value :: mx,my
    integer(c_int), value :: w,h,cols,rows
    integer(c_int) :: cell,cw,ch
    cell=-1; if(w<=0 .or. h<=0 .or. cols<=0 .or. rows<=0) return
    if(mx<0 .or. mx>=w .or. my<0 .or. my>=h) return
    cw=max(1,w/cols); ch=max(1,h/rows)
    if(int(mx)/cw>=cols .or. int(my)/ch>=rows) return
    cell=(int(my)/ch)*cols+int(mx)/cw
  end function
end module
