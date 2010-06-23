
! This file is part of FORTAX Calculator;
! (c) 2009 Andrew Shephard; andrubuntu@gmail.com
!
! FORTAX Calculator is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or (at your
! option) any later version.
!
! FORTAX Calculator is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
! Public License for more details.
!
! You should have received a copy of the GNU General Public License along
! with FORTAX Calculator.  If not, see <http://www.gnu.org/licenses/>.


subroutine returnbudcon(wageOrHours,range0,range1,doHours,sysdir,numsys,sysnames,&
    & taxout,taxlevel,adult,couple,married,&
    & age1,wage1,hours1,selfemp1,age2,wage2,hours2,selfemp2, &
    & numkids,kidage,tenure,rent,ctband,banddratio,&
    & childcare,maintenance,setprice,pricetarget,pricedate,priceindex,pricenum,&
    & kinks_hrs,kinks_earn,kinks_net,kinks_mtr,kinks_num, &
    & netoutLevel,netoutName,netoutAmt,netoutNum)

    !use fortax_realtype, only : dp
    use fortax_type,     only : fam_t, sys_t, net_t, fam_gen, fam_desc, net_init
    use fortax_read
    use fortax_write
    use fortax_calc
    use fortax_prices
    use fortax_kinks
    use fortax_extra

    implicit none

    !real(dp), parameter :: dp = 8, maxkinks = 200
    real(8), intent(in)  :: wageOrHours,range0,range1
    logical, intent(in) :: doHours
    character(*), intent(in) :: sysdir
    integer, intent(in)      :: numsys
    character(*), intent(in) :: sysnames
    integer,  intent(in)  :: adult
    logical,  intent(in)  :: couple, married
    real(8), intent(in)  :: wage1,hours1,wage2,hours2
    logical,  intent(in)  :: selfemp1, selfemp2
    integer,    intent(in) :: numkids
    integer,  intent(in)  :: kidage(max(1,numkids))
    integer, intent(in) :: tenure,ctband
    real(8), intent(in) :: rent,banddratio,childcare,maintenance
    logical, intent(in) :: setprice
    integer, intent(in) :: pricetarget
    integer, intent(in) :: pricenum
    integer, intent(in) :: pricedate(pricenum)
    real(8), intent(in) :: priceindex(pricenum)
    integer,  intent(in)  :: age1, age2
    character(*), intent(in) :: taxlevel
    character(*), intent(in) :: taxout
    integer,  intent(out) :: kinks_num(numsys)
    real(8),  intent(out), dimension(200,numsys) :: kinks_hrs,kinks_earn,kinks_net,kinks_mtr

    character*16, intent(out) :: netoutLevel(100),netoutName(100)
	real(8), intent(out) :: netoutAmt(100,numsys)
    integer, intent(out) :: netoutNum

	integer::i,i0,i1,s
    real(8) :: factor
    character(255) :: sysname(numsys)

    type(fam_t)   :: fam
    type(sys_t)   :: sys(numsys)
    type(net_t)   :: net
    type(bcout_t) :: bcout

    !extract systems names from csv list
    i0 = 0
    s  = 0    
    do        
        s = s+1
        i1 = index(sysnames(i0+1:),',')
        if (i1>0) then
            sysname(s) = sysnames(i0+1:i0+i1-1)
            i0 = i0+i1
        else
            exit
        end if
    end do
    sysname(s)=sysnames(i0+1:)
    if (s.ne.numsys) then
        print *,'s.ne.numsys'
        stop
    end if

    do s = 1,numsys
        call readTaxParams(sys(s),sysdir//'/'//trim(sysname(s)),'fortax')
    end do

    if (setprice) then
        call setindex(pricedate,priceindex,pricenum)
        do s = 1, numsys
            factor = upratefactor(sys(s)%extra%prices,pricetarget)
            call upratesys(sys(s),factor)
        end do
    end if
    
    if (couple) then
        if (numkids>0) then
            fam = fam_gen(couple=.true.,married=married,age1=age1,hrs1=hours1,earn1=wage1*hours1,selfemp1=selfemp1, &
                age2=age2,hrs2=hours2,earn2=wage2*hours2,selfemp2=selfemp2,kidage=kidage,&
                tenure=tenure,rent=rent,ctband=ctband,banddratio=banddratio,ccexp=childcare,maint=maintenance)
        else
            fam = fam_gen(couple=.true.,married=married,age1=age1,hrs1=hours1,earn1=wage1*hours1,selfemp1=selfemp1, &
                age2=age2,hrs2=hours2,earn2=wage2*hours2,selfemp2=selfemp2,&
                tenure=tenure,rent=rent,ctband=ctband,banddratio=banddratio,maint=maintenance)
        end if
    else
        if (numkids>0) then
            fam = fam_gen(couple=.false.,age1=age1,hrs1=hours1,earn1=wage1*hours1,selfemp1=selfemp1,kidage=kidage, &
                tenure=tenure,rent=rent,ctband=ctband,banddratio=banddratio,ccexp=childcare,maint=maintenance)
        else
            fam = fam_gen(couple=.false.,age1=age1,hrs1=hours1,earn1=wage1*hours1,selfemp1=selfemp1,&
                tenure=tenure,rent=rent,ctband=ctband,banddratio=banddratio,maint=maintenance)
        end if
    end if

    !budget constraint
    do s = 1, numsys
        if (doHours) then
            call kinkshours(sys(s),fam,adult,wageOrHours,range0,range1,bcout,taxlevel=taxlevel,taxout=(/taxout/),verbose=.false.,correct=.true.)
        else
            call kinksearn(sys(s),fam,adult,wageOrHours,range0,range1,bcout,taxlevel=taxlevel,taxout=(/taxout/),verbose=.false.,correct=.true.)
        end if

        kinks_num(s)  = bcout%kinks_num
        kinks_hrs(:,s)  = bcout%kinks_hrs
        kinks_earn(:,s) = bcout%kinks_earn
        kinks_net(:,s)  = bcout%kinks_net
        kinks_mtr(:,s)  = bcout%kinks_mtr
    end do

    !income
    do s = 1, numsys
        call calcNetInc(sys(s),fam,net)
        !return short name and income
        call netoutDesc(net,netoutLevel,netoutName,netoutAmt(:,s),netoutNum)
    end do

end subroutine returnbudcon

