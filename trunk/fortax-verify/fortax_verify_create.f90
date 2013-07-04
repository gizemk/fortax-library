
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

! usage: usage: ./fortax_verify_create number_families system_directory file_name

program fortax_verify_create

    use fortax_realtype
    use fortax_type
    use fortax_compare

    implicit none

    integer :: ix, ixKid
    integer :: m
    integer :: nfam
    integer :: ios
    real(dp), allocatable :: rnd(:)
    type(fam_t), allocatable :: fam(:)
    character(len=40), allocatable :: system(:)
    character(len=255) :: carg, sysdir, fname
    integer :: narg

    ! define grid for equiprobable intervals

    ! adult
    real(dp), parameter :: hrs_grid(17) = (/  0.0_dp,  2.0_dp,  5.0_dp, 10.0_dp, 15.0_dp, 16.0_dp, 17.0_dp, 20.0_dp, &
                                        29.0_dp, 30.0_dp, 31.0_dp, 35.0_dp, 40.0_dp, 50.0_dp, 60.0_dp, 80.0_dp, 100.0_dp/)

    real(dp), parameter :: earn_grid(14) = (/    1.0_dp,  10.0_dp,  50.0_dp, 100.0_dp, 150.0_dp, 200.0_dp, 300.0_dp, 400.0_dp, &
                                          500.0_dp, 600.0_dp, 700.0_dp, 1000.0_dp, 2000.0_dp, 5000.0_dp /)

    integer, parameter :: age_grid(7) = (/16, 18, 25, 30, 40, 55, 60/)

    logical, parameter :: selfemp_grid(5) = (/.false.,.false.,.false.,.false.,.true./)

    ! tu
    logical, parameter :: couple_grid(2)  = (/.true.,.false./)
    logical, parameter :: married_grid(2) = (/.true.,.false./)

    integer, parameter :: nkids_grid(1+maxkids) = (/ (ix, ix = 0, maxkids) /)
    integer, parameter :: kidage_grid(19) = (/ (ix, ix = 0, 18) /)
    integer, parameter :: nothads_grid(3) = (/ (ix, ix = 0, 2)  /)

    real(dp), parameter :: ccexp_grid(9) = (/0.0_dp, 0.0_dp, 0.0_dp, 10.0_dp, 50.0_dp, 100.0_dp, 200.0_dp, 500.0_dp, 1000.0_dp/)
    real(dp), parameter :: maint_grid(8) = (/0.0_dp, 0.0_dp, 0.0_dp, 10.0_dp, 90.0_dp, 180.0_dp, 370.0_dp, 800.0_dp/)

    integer,  parameter :: tenure_grid(7)     = (/ (ix, ix = 1, 7) /)
    real(dp), parameter :: rent_grid(51)          = (/ ( real(ix,dp), ix = 0,1000,20) /)
    real(dp), parameter :: rentcap_grid(51)       = (/ ( real(ix,dp), ix = 0,1000,20) /)
    integer,  parameter :: ctband_grid(8)     = (/ ( ix, ix = 1, 8 ) /)
    real(dp), parameter :: banddratio_grid(20) = (/ ( 0.1_dp*real(ix,dp), ix = 1, 20 ) /)

    real(dp), allocatable           :: fam_real(:)
    logical, allocatable            :: fam_logical(:)
    integer, allocatable            :: fam_integer(:)
    character(len=40), allocatable  :: fam_char(:)

    character(len=40) :: system_grid(24) = (/ character(len=40) :: 'April90','April91','April92','April93','April94','April95',&
        'April96','April97','April98','April99','April00','April01','April02','April03','April04','April05',&
        'April06','Autumn99','Autumn00','Autumn01','Autumn02','Autumn03','Autumn04','Autumn05' /)

    integer, parameter :: seed(4) = (/ 13912, 9041278, 840832, 780529 /)


    narg = command_argument_count()

    if (narg.ne.3) then
        stop 'usage: ./fortax_verify_create number_families system_directory file_name'
    end if

    call get_command_argument(1,carg)
    read (carg,*,iostat=ios) nfam

    if (ios.ne.0) then
        stop 'first argument (number_families) must be a number'
    else
        if (nfam<=0) then
            stop 'first argument (number_families) must be greater than zero'
        end if
    end if

    allocate(rnd(nfam))
    allocate(fam(nfam))
    allocate(system(nfam))

    allocate(fam_real(nfam))
    allocate(fam_logical(nfam))
    allocate(fam_integer(nfam))
    allocate(fam_char(nfam))

    call random_seed( size = m )
    call random_seed( put = seed(1:m) )

    call fam_init(fam)
    
    call get_command_argument(2,sysdir)
    call get_command_argument(3,fname)

    ! couple
    call random_number( rnd )
    call getRnd_Logical( rnd, couple_grid, fam_logical )
    forall (ix=1:nfam) fam(ix)%couple=fam_logical(ix)

    ! married
    call random_number( rnd )
    call getRnd_Logical( rnd, married_grid, fam_logical )
    forall (ix=1:nfam) fam(ix)%married=merge(fam_logical(ix),.false.,fam(ix)%couple)

    ! number of kids
    call random_number( rnd )
    call getRnd_Int( rnd, nkids_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%nkids=fam_integer(ix)

    ! age of kids
    do ixKid = 1, maxkids
        call random_number( rnd )
        call getRnd_Int( rnd, kidage_grid, fam_integer )
        forall (ix=1:nfam) fam(ix)%kidage(ixKid) = fam_integer(ix)
    end do

    ! fix kid ages, youngest kid
    do ix = 1, nfam
        if (fam(ix)%nkids<maxkids) then
            fam(ix)%kidage(fam(ix)%nkids+1:maxkids) = 0
        end if
        if (fam(ix)%nkids>0) then
            fam(ix)%yngkid = minval( fam(ix)%kidage(1:fam(ix)%nkids) )
        else
            fam(ix)%yngkid = 0
        end if
    end do

    ! other adults
    call random_number( rnd )
    call getRnd_Int( rnd, nothads_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%nothads=fam_integer(ix)

    ! tenure
    call random_number( rnd )
    call getRnd_Int( rnd, tenure_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%tenure=fam_integer(ix)

    ! ctband
    call random_number( rnd )
    call getRnd_Int( rnd, ctband_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%ctband=fam_integer(ix)

    ! ccexp
    call random_number( rnd )
    call getRnd_Real( rnd, ccexp_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%ccexp=merge(fam_real(ix),0.0_dp,fam(ix)%nkids>0)

    ! maint
    call random_number( rnd )
    call getRnd_Real( rnd, maint_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%maint=merge(fam_real(ix),0.0_dp,fam(ix)%nkids>0)

    ! banddratio
    call random_number( rnd )
    call getRnd_Real( rnd, banddratio_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%banddratio= fam_real(ix)

    ! rent 
    call random_number( rnd )
    call getRnd_Real( rnd, rent_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%rent = fam_real(ix)

    ! rent cap (only implemented for private rents)
    call random_number( rnd )
    call getRnd_Real( rnd, rentcap_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%rentcap = merge(fam_real(ix),0.0_dp,fam(ix)%tenure==5)

    ! ----------------------- ad1 -------------------- !

    ! hours
    call random_number( rnd )
    call getRnd_Real( rnd, hrs_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%ad(1)%hrs = fam_real(ix)

    ! earnings
    call random_number( rnd )
    call getRnd_Real( rnd, earn_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%ad(1)%earn = fam_real(ix)

    ! age
    call random_number( rnd )
    call getRnd_Int( rnd, age_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%ad(1)%age=fam_integer(ix)

    ! selfemp
    call random_number( rnd )
    call getRnd_Logical( rnd, selfemp_grid, fam_logical )
    forall (ix=1:nfam) fam(ix)%ad(1)%selfemp=fam_logical(ix)

    ! ----------------------- ad2 -------------------- !

    ! hours
    call random_number( rnd )
    call getRnd_Real( rnd, hrs_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%ad(2)%hrs = fam_real(ix)

    ! earnings
    call random_number( rnd )
    call getRnd_Real( rnd, earn_grid, fam_real )
    forall (ix=1:nfam) fam(ix)%ad(2)%earn = fam_real(ix)

    ! age
    call random_number( rnd )
    call getRnd_Int( rnd, age_grid, fam_integer )
    forall (ix=1:nfam) fam(ix)%ad(2)%age=fam_integer(ix)

    ! selfemp
    call random_number( rnd )
    call getRnd_Logical( rnd, selfemp_grid, fam_logical )
    forall (ix=1:nfam) fam(ix)%ad(2)%selfemp=fam_logical(ix)

    do ix=1, nfam
        if (.not. fam(ix)%couple) then
            fam(ix)%ad(2)%hrs=0.0_dp
            fam(ix)%ad(2)%earn=0.0_dp
            fam(ix)%ad(2)%age=25
            fam(ix)%ad(2)%selfemp=.false.
        end if
    end do

    ! ----------------------- sys -------------------- !

    ! system
    call random_number( rnd )
    call getRnd_Char( rnd, system_grid, fam_char )
    forall (ix=1:nfam) system(ix) = fam_char(ix)

    call writeFamCompareDatabase(nfam,fam,system,trim(sysdir),fname)

end program fortax_verify_create