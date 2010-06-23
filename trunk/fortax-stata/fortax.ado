*! FORTAX for Stata version 0.10.2 Copyright (c) 2009 Andrew Shephard

cap program drop _fortax
cap program define _fortax, plugin using("fortax.plugin")

cap program drop fortax
program define fortax

#delimit ;
syntax [if] [in] , [listsys] [listfam] [listnet] [sys(string)] [sysfile(string)]
    [age1(varname numeric)] [selfemp1(varname numeric)] [hrs1(varname numeric)] [earn1(varname numeric)] 
    [age2(varname numeric)] [selfemp2(varname numeric)] [hrs2(varname numeric)] [earn2(varname numeric)]
    [couple(varname numeric)] [married(varname numeric)] [ccexp(varname numeric)] [maint(varname numeric)] 
    [nkids(varname numeric)] [kidage1(varname numeric)] [kidage2(varname numeric)] [kidage3(varname numeric)]
    [kidage4(varname numeric)] [kidage5(varname numeric)] [kidage6(varname numeric)] [kidage7(varname numeric)]
    [kidage8(varname numeric)] [kidage9(varname numeric)] [kidage10(varname numeric)] [nothads(varname numeric)]
    [tenure(varname numeric)] [rent(varname numeric)] [rentcap(varname numeric)] [region(varname numeric)]
    [ctband(varname numeric)] [banddratio(varname numeric)] [intdate(varname numeric)]
    [netout(namelist)] [netoutvar(namelist)] [uprate(real 1.0)] [replace] [label];

local variables = "age1 selfemp1 hrs1 earn1 age2 selfemp2 hrs2 earn2
    couple married ccexp maint nkids kidage1 kidage2 kidage3 kidage4 kidage5
    kidage6 kidage7 kidage8 kidage9 kidage10 nothads tenure rent rentcap
    region ctband banddratio intdate";

local netvariables = "pretaxearn posttaxearn chben matgrant fc wtc ctc incsup 
    hben polltax polltaxben ctax ctaxben dispinc pretax tottax chcaresub fsm
    taxable1 inctax1 natins1 pretaxearn1 posttaxearn1
    taxable2 inctax2 natins2 pretaxearn2 posttaxearn2";

#delimit cr

if ("`listsys'"~="" | "`listfam'"~="" | "`listnet'"~="") {
    plugin call _fortax, "0" `listsys' `listfam' `listnet'
    exit 0
}

if ("`sys'"=="" & "`sysfile'"=="") {
	di as error "Either sys or sysfile must be specified"
	exit 198
	}
	
if ("`sys'"~="") {
    local sysmode = 0
}
else {
    local sysmode = 1
}

if (`uprate'<=0) {
    di as error "Uprating factor must be strictly positive"
    exit 999
}
if (missing(`uprate')) {
    di as error "Uprating factor must be non-missing"
    exit 999
}

local varlist ""
local arglist ""
foreach var of local variables {
    if "``var''"~="" {
        local arglist `"`arglist' `var'"'
        local varlist `"`varlist' ``var''"'
    }
}

//counts number of elements
local nfamlist : list sizeof arglist
local nnetlist : list sizeof netout
local nnetvarlist : list sizeof netoutvar

//if netoutvar is not specified, set to netout

if `nnetvarlist'>0 {
    if `nnetlist'~=`nnetvarlist' {
        di as error "netout and netoutvar should contain the same number of elements"
        exit 198
    }
}
else {
    local netoutvar = "`netout'"
}

//check whether netout contains valid entries
if `nnetlist'>0 {
    foreach var of local netout {
        local x : list posof "`var'" in netvariables
        if `x'<=0 {
            di as error "`var' is not a valid output variable"
            exit 198
        }
    }
}
else {
    local netout = "dispinc"
    local netoutvar = "dispinc"
    local nnetlist = 1
}

//if replace is not specified, check whether output variables exist
if "`replace'"=="" {
    foreach var of local netoutvar {
        cap confirm variable `var'
        if _rc==0 {
            di as error "variable `var' already defined"
            exit 110
        }
    }
}

foreach var of local netoutvar {
    cap confirm variable `var'
    if _rc~=0 {
        qui gen double `var' = .
    }
    else {
        confirm numeric variable `var'
        local vartype : type `var'
        if "`vartype'"=="byte" {
            di as text "warning: output variable `var' is of type byte"
        }
        if "`vartype'"=="int" {
            di as text "warning: output variable `var' is of type int"
        }
        if "`vartype'"=="long" {
            di as text "warning: output variable `var' is of type long"
        }
    }
}

if "`label'"=="" {
    local labmode = 0
}
else {
    local labmode = 1
}
    
plugin call _fortax `varlist' `netoutvar' `if' `in', "1" `sysmode' "`sys'`sysfile'" `labmode' `nfamlist' `nnetlist' `uprate' `arglist' `netout'

//label variables
if "`label'"~="" {
    local i = 1
    foreach var of local netoutvar {
        label var `var' "`fortaxlabel`i''"
        local ++i
        }
}

end program
