
! This file is part of FORTAX verify;

! FORTAX is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! FORTAX is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with FORTAX.  If not, see <http://www.gnu.org/licenses/>.

! usage: usage: ./fortax_verify_check number_families system_directory file_name

program fortax_verify_check

    use fortax_realtype
    use fortax_type
    use fortax_compare

    implicit none

    integer :: nfam
    integer :: ios
    type(fam_t), allocatable :: fam(:)
    type(net_t), allocatable :: net(:)
    character(len=40), allocatable :: sysname(:)
    character(len=255) :: carg, sysdir, fname
    integer :: narg
    integer :: maxdiff

    ! define grid for equiprobable intervals

    narg = command_argument_count()

    if (narg<2 .or. narg>3) then
        stop 'usage: ./fortax_verify_check file_name system_directory [number_detailed_differences]'
    end if

    call get_command_argument(1,fname)
    call get_command_argument(2,sysdir)

    if (narg==3) then
        call get_command_argument(3,carg)
        read (carg,*,iostat=ios) maxdiff
        if (ios.ne.0) then
            stop 'third argument (number_detailed_differences) must be a number'
        else
            if (maxdiff<0) then
                stop 'third argument (number_detailed_differences) must be at least zero'
            end if
        end if
    else
        maxdiff=20
    end if
    
    call readFamCompareDatabase(fname,fam,net,sysname,nfam)
    call compareFamDatabase(nfam,fam,sysdir,sysname,net,maxdiff)

end program fortax_verify_check