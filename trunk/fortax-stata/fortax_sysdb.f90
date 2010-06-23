! This file is part of FORTAX for Stata.  
! (c) 2009 Andrew Shephard; andrubuntu@gmail.com
! 
! FORTAX for Stata is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! FORTAX for Stata is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with FORTAX for Stata.  If not, see <http://www.gnu.org/licenses/>. 

!module that contains a database of system files
module fortax_sysdb

    implicit none

contains

    !extract system from database. files generated using sys_saveF90
    subroutine get_sysdb(sys,sysname,ifail)

        use fortax_type, only : sys_t, sys_init
        use fortax_util, only : lower
        use fortax_realtype, only : dp

        implicit none

        type(sys_t), intent(out) :: sys
        character(len=*), intent(in) :: sysname
        integer, intent(out) :: ifail

        ifail = 0
        select case(trim(adjustl(lower(sysname))))
        case('april91')
#           include 'systemsf90/April91.f90'
        case('april92')
#           include 'systemsf90/April92.f90'
        case('april93')
#           include 'systemsf90/April93.f90'
        case('april94')
#           include 'systemsf90/April94.f90'
        case('april95')
#           include 'systemsf90/April95.f90'
        case('april96')
#           include 'systemsf90/April96.f90'
        case('april97')
#           include 'systemsf90/April97.f90'
        case('april98')
#           include 'systemsf90/April98.f90'
        case('april99')
#           include 'systemsf90/April99.f90'
        case('april00')
#           include 'systemsf90/April00.f90'
        case('april01')
#           include 'systemsf90/April01.f90'
        case('april02')
#           include 'systemsf90/April02.f90'
        case('april03')
#           include 'systemsf90/April03.f90'
        case('april04')
#           include 'systemsf90/April04.f90'
        case('april05')
#           include 'systemsf90/April05.f90'
        case('april06')
#           include 'systemsf90/April06.f90'
        case('april07')
#           include 'systemsf90/April07.f90'
        case('april08')
#           include 'systemsf90/April08.f90'
        case('april09')
#           include 'systemsf90/April09.f90'
        case('autumn99')
#           include 'systemsf90/Autumn99.f90'
        case('autumn00')
#           include 'systemsf90/Autumn00.f90'
        case('autumn01')
#           include 'systemsf90/Autumn01.f90'
        case('autumn02')
#           include 'systemsf90/Autumn02.f90'
        case('autumn03')
#           include 'systemsf90/Autumn03.f90'
        case('autumn04')
#           include 'systemsf90/Autumn04.f90'
        case('autumn05')
#           include 'systemsf90/Autumn05.f90'
        case('autumn06')
#           include 'systemsf90/Autumn06.f90'
        case('autumn07')
#           include 'systemsf90/Autumn07.f90'
        case('autumn08')
#           include 'systemsf90/Autumn08.f90'
        case default
            ifail = 1 !system not in database
        end select

    end subroutine get_sysdb

end module fortax_sysdb
