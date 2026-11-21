! BSD-3-Clause: selected routines from JSON-Fortran 9.0.5, Jacob Williams.
! Source and full license bundled in dependencies/json-fortran. No claim of full DOM parser Wasm support.
module json_fortran_browser_strings
 use iso_c_binding
 implicit none
 integer,parameter::CK=kind('a'),IK=c_int,LK=kind(.true.),chunk_size=256
 character(len=1),parameter::space=' ',quotation_mark='"',backslash=achar(92),slash='/', &
  bspace=achar(8),formfeed=achar(12),newline=achar(10),carriage_return=achar(13),horizontal_tab=achar(9)
 character(len=256),parameter::blank_chunk=repeat(' ',256)
 character(len=26),parameter::upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ',lower='abcdefghijklmnopqrstuvwxyz'
contains
! @nativefn NR2-D02-F0001
    subroutine escape_string(str_in, str_out, escape_solidus)

    implicit none

    character(kind=CK,len=*),intent(in)              :: str_in
    character(kind=CK,len=:),allocatable,intent(out) :: str_out
    logical(LK),intent(in) :: escape_solidus  !! if the solidus (forward slash)
                                              !! is also to be escaped

    integer(IK) :: i    !! counter
    integer(IK) :: ipos !! accumulated string size
                        !! (so we can allocate it in chunks for
                        !! greater runtime efficiency)
    character(kind=CK,len=1) :: c  !! for reading `str_in` one character at a time.
    logical :: to_be_escaped !! if there are characters to be escaped

    character(kind=CK,len=*),parameter :: specials_no_slash = quotation_mark//&
                                                     backslash//&
                                                     bspace//&
                                                     formfeed//&
                                                     newline//&
                                                     carriage_return//&
                                                     horizontal_tab

    character(kind=CK,len=*),parameter :: specials = specials_no_slash//slash

    !Do a quick scan for the special characters,
    ! if any are present, then process the string,
    ! otherwise, return the string as is.
    if (escape_solidus) then
        to_be_escaped = scan(str_in,specials)>0
    else
        to_be_escaped = scan(str_in,specials_no_slash)>0
    end if

    if (to_be_escaped) then

        str_out = repeat(space,chunk_size)
        ipos = 1

        !go through the string and look for special characters:
        do i=1,len(str_in)

            c = str_in(i:i)    !get next character in the input string

            !if the string is not big enough, then add another chunk:
            if (ipos+3>len(str_out)) str_out = str_out // blank_chunk

            select case(c)
            case(backslash)

                !test for unicode sequence: '\uXXXX'
                ![don't add an extra '\' for those]
                if (i+5<=len(str_in)) then
                    if (str_in(i+1:i+1)==CK_'u' .and. &
                        valid_json_hex(str_in(i+2:i+5))) then
                        str_out(ipos:ipos) = c
                        ipos = ipos + 1
                        cycle
                    end if
                end if

                str_out(ipos:ipos+1) = backslash//c
                ipos = ipos + 2

            case(quotation_mark)
                str_out(ipos:ipos+1) = backslash//c
                ipos = ipos + 2
            case(slash)
                if (escape_solidus) then
                    str_out(ipos:ipos+1) = backslash//c
                    ipos = ipos + 2
                else
                    str_out(ipos:ipos) = c
                    ipos = ipos + 1
                end if
            case(bspace)
                str_out(ipos:ipos+1) = '\b'
                ipos = ipos + 2
            case(formfeed)
                str_out(ipos:ipos+1) = '\f'
                ipos = ipos + 2
            case(newline)
                str_out(ipos:ipos+1) = '\n'
                ipos = ipos + 2
            case(carriage_return)
                str_out(ipos:ipos+1) = '\r'
                ipos = ipos + 2
            case(horizontal_tab)
                str_out(ipos:ipos+1) = '\t'
                ipos = ipos + 2
            case default
                str_out(ipos:ipos) = c
                ipos = ipos + 1
            end select

        end do

        !trim the string if necessary:
        if (ipos<len(str_out)+1) then
            if (ipos==1) then
                str_out = CK_''
            else
                str_out = str_out(1:ipos-1)  !original
            end if
        end if

    else

        str_out = str_in

    end if

    end subroutine escape_string
! @nativefn NR2-D02-F0002
    pure function valid_json_hex(str) result(valid)

    implicit none

    logical(LK)                         :: valid  !! is str a value 4-digit hex string
    character(kind=CK,len=*),intent(in) :: str    !! the string to check.

    integer(IK) :: n  !! length of `str`
    integer(IK) :: i  !! counter

    !> an array of the valid hex characters
    character(kind=CK,len=1),dimension(22),parameter :: valid_chars = &
        [ (achar(i),i=48,57), & ! decimal digits
          (achar(i),i=65,70), & ! capital A-F
          (achar(i),i=97,102) ] ! lowercase a-f

    !initialize
    valid = .false.

    !check all the characters in the string:
    n = len(str)
    if (n==4) then
        do i=1,n
            if (.not. any(str(i:i)==valid_chars)) return
        end do
        valid = .true.    !all are in the set, so it is OK
    end if

    end function valid_json_hex
! @nativefn NR2-D02-F0003
    pure function lowercase_string(str) result(s_lower)

    implicit none

    character(kind=CK,len=*),intent(in) :: str      !! input string
    character(kind=CK,len=(len(str)))   :: s_lower  !! lowercase version of the string

    integer :: i  !! counter
    integer :: j  !! index of uppercase character

    s_lower = str

    do i = 1, len_trim(str)
        j = index(upper,s_lower(i:i))
        if (j>0) s_lower(i:i) = lower(j:j)
    end do

    end function lowercase_string
end module
