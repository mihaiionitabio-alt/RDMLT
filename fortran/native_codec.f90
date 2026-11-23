! SPDX-License-Identifier: GPL-3.0-only
! Native package codecs: SHA-256 (FIPS 180-4) for the RDMLt manifest and raw DEFLATE
! (RFC 1951, fixed Huffman codes, LZ77 hash chains) for ZIP method 8 entries.
! Integer-only arithmetic on 64-bit integers masked to 32 bits, so gfortran and LFortran agree.
module native_codec
 use iso_c_binding
 implicit none
 private
 public :: sha256_hex, deflate_fixed, zbuf, zlen
 integer(c_int64_t), parameter :: m32 = 4294967295_c_int64_t
 ! Output of deflate_fixed; grown on demand to the worst case of the current input.
 integer(c_int8_t), allocatable, target, save :: zbuf(:)
 integer, save :: zlen = 0
 integer(c_int64_t), save :: bitacc = 0
 integer, save :: bitcnt = 0
 logical, save :: zover = .false.
 integer, parameter :: wsize = 32768, hsize = 32768, maxchain = 48
 integer, save :: head(0:hsize-1), prev(0:wsize-1)
 integer, parameter :: lbase(29) = [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258]
 integer, parameter :: lextra(29) = [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
 integer, parameter :: dbase(30) = [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073, &
  4097,6145,8193,12289,16385,24577]
 integer, parameter :: dextra(30) = [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]
 integer(c_int64_t), parameter :: kk(64) = [ &
  1116352408_c_int64_t,1899447441_c_int64_t,3049323471_c_int64_t,3921009573_c_int64_t,961987163_c_int64_t, &
  1508970993_c_int64_t,2453635748_c_int64_t,2870763221_c_int64_t,3624381080_c_int64_t,310598401_c_int64_t, &
  607225278_c_int64_t,1426881987_c_int64_t,1925078388_c_int64_t,2162078206_c_int64_t,2614888103_c_int64_t, &
  3248222580_c_int64_t,3835390401_c_int64_t,4022224774_c_int64_t,264347078_c_int64_t,604807628_c_int64_t, &
  770255983_c_int64_t,1249150122_c_int64_t,1555081692_c_int64_t,1996064986_c_int64_t,2554220882_c_int64_t, &
  2821834349_c_int64_t,2952996808_c_int64_t,3210313671_c_int64_t,3336571891_c_int64_t,3584528711_c_int64_t, &
  113926993_c_int64_t,338241895_c_int64_t,666307205_c_int64_t,773529912_c_int64_t,1294757372_c_int64_t, &
  1396182291_c_int64_t,1695183700_c_int64_t,1986661051_c_int64_t,2177026350_c_int64_t,2456956037_c_int64_t, &
  2730485921_c_int64_t,2820302411_c_int64_t,3259730800_c_int64_t,3345764771_c_int64_t,3516065817_c_int64_t, &
  3600352804_c_int64_t,4094571909_c_int64_t,275423344_c_int64_t,430227734_c_int64_t,506948616_c_int64_t, &
  659060556_c_int64_t,883997877_c_int64_t,958139571_c_int64_t,1322822218_c_int64_t,1537002063_c_int64_t, &
  1747873779_c_int64_t,1955562222_c_int64_t,2024104815_c_int64_t,2227730452_c_int64_t,2361852424_c_int64_t, &
  2428436474_c_int64_t,2756734187_c_int64_t,3204031479_c_int64_t,3329325298_c_int64_t]
contains
! @nativefn NR2-M05-F0001
 integer(c_int64_t) function rotr(x,n) result(v)
  integer(c_int64_t),intent(in)::x
  integer,intent(in)::n
  v=iand(ior(shiftr(x,n),shiftl(x,32-n)),m32)
 end function
! @nativefn NR2-M05-F0002
 subroutine sha_block(w0,h)
  integer(c_int64_t),intent(in)::w0(16)
  integer(c_int64_t),intent(inout)::h(8)
  integer(c_int64_t)::w(64),a,b,c,d,e,f,g,hh,s0,s1,t1,t2,ch,mj
  integer::i
  w(1:16)=w0
  do i=17,64
   s0=ieor(ieor(rotr(w(i-15),7),rotr(w(i-15),18)),shiftr(w(i-15),3))
   s1=ieor(ieor(rotr(w(i-2),17),rotr(w(i-2),19)),shiftr(w(i-2),10))
   w(i)=iand(w(i-16)+s0+w(i-7)+s1,m32)
  end do
  a=h(1);b=h(2);c=h(3);d=h(4);e=h(5);f=h(6);g=h(7);hh=h(8)
  do i=1,64
   s1=ieor(ieor(rotr(e,6),rotr(e,11)),rotr(e,25))
   ch=ieor(iand(e,f),iand(ieor(e,m32),g))
   t1=iand(hh+s1+ch+kk(i)+w(i),m32)
   s0=ieor(ieor(rotr(a,2),rotr(a,13)),rotr(a,22))
   mj=ieor(ieor(iand(a,b),iand(a,c)),iand(b,c))
   t2=iand(s0+mj,m32)
   hh=g;g=f;f=e;e=iand(d+t1,m32);d=c;c=b;b=a;a=iand(t1+t2,m32)
  end do
  h(1)=iand(h(1)+a,m32);h(2)=iand(h(2)+b,m32);h(3)=iand(h(3)+c,m32);h(4)=iand(h(4)+d,m32)
  h(5)=iand(h(5)+e,m32);h(6)=iand(h(6)+f,m32);h(7)=iand(h(7)+g,m32);h(8)=iand(h(8)+hh,m32)
 end subroutine
! @nativefn NR2-M05-F0003
 function sha256_hex(s,n) result(hex)
  character(len=*),intent(in)::s
  integer,intent(in)::n
  character(len=64)::hex
  character(len=16),parameter::digits='0123456789abcdef'
  integer(c_int64_t)::h(8),w(16),bits
  integer(c_int64_t)::tail(128)
  integer::i,j,k,full,rest,nt,byte
  h=[1779033703_c_int64_t,3144134277_c_int64_t,1013904242_c_int64_t,2773480762_c_int64_t, &
     1359893119_c_int64_t,2600822924_c_int64_t,528734635_c_int64_t,1541459225_c_int64_t]
  full=n/64;rest=n-full*64
  do i=0,full-1
   do j=1,16
    k=i*64+(j-1)*4
    w(j)=ior(ior(shiftl(int(iachar(s(k+1:k+1)),c_int64_t),24),shiftl(int(iachar(s(k+2:k+2)),c_int64_t),16)), &
      ior(shiftl(int(iachar(s(k+3:k+3)),c_int64_t),8),int(iachar(s(k+4:k+4)),c_int64_t)))
   end do
   call sha_block(w,h)
  end do
  tail=0
  do i=1,rest
   tail(i)=iachar(s(full*64+i:full*64+i))
  end do
  tail(rest+1)=128
  nt=64;if(rest>=56)nt=128
  bits=int(n,c_int64_t)*8
  do i=0,7
   tail(nt-i)=iand(shiftr(bits,8*i),255_c_int64_t)
  end do
  do i=0,nt/64-1
   do j=1,16
    k=i*64+(j-1)*4
    w(j)=ior(ior(shiftl(tail(k+1),24),shiftl(tail(k+2),16)),ior(shiftl(tail(k+3),8),tail(k+4)))
   end do
   call sha_block(w,h)
  end do
  do i=1,8
   do j=0,7
    byte=int(iand(shiftr(h(i),28-4*j),15_c_int64_t))
    hex((i-1)*8+j+1:(i-1)*8+j+1)=digits(byte+1:byte+1)
   end do
  end do
 end function
! @nativefn NR2-M05-F0004
 subroutine zput(v)
  integer,intent(in)::v
  integer::b
  if(zlen>=size(zbuf))then
   zover=.true.;return
  end if
  b=iand(v,255);if(b>127)b=b-256
  zlen=zlen+1;zbuf(zlen)=int(b,c_int8_t)
 end subroutine
! @nativefn NR2-M05-F0005
 subroutine putbits(v,n)
  integer,intent(in)::v,n
  bitacc=ior(bitacc,shiftl(int(v,c_int64_t),bitcnt));bitcnt=bitcnt+n
  do while(bitcnt>=8)
   call zput(int(iand(bitacc,255_c_int64_t)));bitacc=shiftr(bitacc,8);bitcnt=bitcnt-8
  end do
 end subroutine
! @nativefn NR2-M05-F0006
 integer function reverse_bits(code,n) result(r)
  integer,intent(in)::code,n
  integer::i
  r=0
  do i=0,n-1
   if(btest(code,i))r=ibset(r,n-1-i)
  end do
 end function
! @nativefn NR2-M05-F0007
 subroutine put_symbol(sym)
  integer,intent(in)::sym
  if(sym<=143)then
   call putbits(reverse_bits(48+sym,8),8)
  else if(sym<=255)then
   call putbits(reverse_bits(400+sym-144,9),9)
  else if(sym<=279)then
   call putbits(reverse_bits(sym-256,7),7)
  else
   call putbits(reverse_bits(192+sym-280,8),8)
  end if
 end subroutine
! @nativefn NR2-M05-F0008
 subroutine put_match(length,dist)
  integer,intent(in)::length,dist
  integer::i,li,di
  li=1
  do i=29,1,-1
   if(length>=lbase(i))then
    li=i;exit
   end if
  end do
  call put_symbol(256+li)
  if(lextra(li)>0)call putbits(length-lbase(li),lextra(li))
  di=1
  do i=30,1,-1
   if(dist>=dbase(i))then
    di=i;exit
   end if
  end do
  call putbits(reverse_bits(di-1,5),5)
  if(dextra(di)>0)call putbits(dist-dbase(di),dextra(di))
 end subroutine
! @nativefn NR2-M05-F0009
 integer function hash3(s,p) result(h)
  character(len=*),intent(in)::s
  integer,intent(in)::p
  h=iand(ieor(ieor(shiftl(iachar(s(p:p)),10),shiftl(iachar(s(p+1:p+1)),5)),iachar(s(p+2:p+2))),hsize-1)
 end function
! @nativefn NR2-M05-F0010
 subroutine deflate_fixed(s,n,ok)
  ! Compresses s(1:n) into zbuf(1:zlen) as one final fixed-Huffman block.
  character(len=*),intent(in)::s
  integer,intent(in)::n
  logical,intent(out)::ok
  integer::p,h,cand,chain,best,bestd,l,lim,k
  if(allocated(zbuf))then
   if(size(zbuf)<n+n/7+1024)deallocate(zbuf)
  end if
  if(.not.allocated(zbuf))allocate(zbuf(n+n/7+1024))
  zlen=0;bitacc=0;bitcnt=0;zover=.false.;head=-1;prev=-1
  call putbits(1,1);call putbits(1,2)
  p=1
  do while(p<=n)
   best=0;bestd=0
   if(p+2<=n)then
    h=hash3(s,p);cand=head(h);chain=0;lim=min(258,n-p+1)
    do while(cand>=1.and.chain<maxchain)
     if(p-cand>wsize)exit
     if(s(cand+best:cand+best)==s(p+best:p+best))then
      l=0
      do while(l<lim)
       if(s(cand+l:cand+l)/=s(p+l:p+l))exit
       l=l+1
      end do
      if(l>best)then
       best=l;bestd=p-cand
       if(l>=lim)exit
      end if
     end if
     cand=prev(mod(cand,wsize));chain=chain+1
    end do
    prev(mod(p,wsize))=head(h);head(h)=p
   end if
   if(best>=3)then
    call put_match(best,bestd)
    do k=p+1,p+best-1
     if(k+2<=n)then
      h=hash3(s,k);prev(mod(k,wsize))=head(h);head(h)=k
     end if
    end do
    p=p+best
   else
    call put_symbol(iachar(s(p:p)));p=p+1
   end if
   if(zover)exit
  end do
  call put_symbol(256)
  if(bitcnt>0)call putbits(0,8-bitcnt)
  ok=.not.zover
 end subroutine
end module
