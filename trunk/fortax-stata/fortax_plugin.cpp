// This file is part of FORTAX for Stata.  
// (c) 2009 Andrew Shephard; andrubuntu@gmail.com
// 
// FORTAX for Stata is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// FORTAX for Stata is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with FORTAX for Stata.  If not, see <http://www.gnu.org/licenses/>. 


    #include "stplugin.h"
    #include <string.h>
    #include <stdio.h>
    #include <stdlib.h>
    
    #define MODE 0
    #define SYSMODE 1
    #define SYSNAME 2
    #define LABMODE 3
    #define NFAMLIST 4
    #define NNETLIST 5
    #define UPRATE 6
    #define ARGLIST 7

    //right trim char
    char* rtrim(char* string) {
        char* original = string + strlen(string);
        while(*--original == ' ');
            *(original + 1) = '\0';
            return string;
    }

    STDLL stata_call(int argc, char *argv[])  {

        void c_statasys(const char*,const int*);
        void c_stataincome();
        void c_statafaminit();
        void c_statafamfin();
        void c_statafamset(const char*, const int*, const double*);
        void c_statanetget(const char*, const int*, double *);
        void c_statalabelget(const char*, const int*, char *);
        void c_get_sysdb(const char*, const int*, int *);
        void c_statauprate(const double*);
        
        ST_int     j;
        ST_double  z;
		ST_retcode rc;
        double b;
        char sysstr[256];
        char famstr[16];
        char netstr[16];
		int i ;
        int famstrlen;
        int sysstrlen;
        int netstrlen;
        int mode, sysmode;
        int ifail;
		int nfamlist;
        int nnetlist;
        int label;
        double uprate;

		/*        
          for(i=0;i < argc; i++) {
              SF_display(argv[i]) ;
              SF_display("\n") ;
          }*/

        switch(atoi(argv[MODE])) {
            case 0:
                //SF_display("list\n");
                mode = 0;
                break;
            case 1:
                //SF_display("calc\n");
                mode = 1;
                switch(atoi(argv[SYSMODE])) {
                    case 0:
                        sysmode = 0;
                        break;
                    case 1:
                        sysmode = 1;
                        break;
                    default:
                        SF_error("invalid system mode\n");
                        return(198);
                        break;
                }
                break;
            default:
                SF_error("invalid mode\n");
                return(198);
                break;
        }
            
        if (mode==0) {
            for(i=1;i < argc; i++) {
                if (strcmp(argv[i],"listnet")==0)  {
                    SF_display(" {bf:{ul:listnet}}\n");
                    SF_display("\n");
                    SF_display(" Description                  {c |} Internal name\n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                    SF_display(" Pre-tax Earnings             {c |} {res:pretaxearn}    \n");
                    SF_display(" Post-tax Earnings            {c |} {res:posttaxearn}   \n");
                    SF_display(" Pre-tax Income               {c |} {res:pretax}        \n");
                    SF_display(" Child Benefit                {c |} {res:chben}         \n");
                    SF_display(" Maternity Grant              {c |} {res:matgrant}      \n");
                    SF_display(" Family Credit/WFTC           {c |} {res:fc}            \n");
                    SF_display(" Working Tax Credit           {c |} {res:wtc}           \n");
                    SF_display(" Child Tax Credit             {c |} {res:ctc}           \n");
                    SF_display(" Income Support               {c |} {res:incsup}        \n");
                    SF_display(" Housing Benefit              {c |} {res:hben}          \n");
                    SF_display(" Community Charge             {c |} {res:polltax}       \n");
                    SF_display(" Community Charge Benefit     {c |} {res:polltaxben}    \n");
                    SF_display(" Council Tax                  {c |} {res:ctax}          \n");
                    SF_display(" Council Tax Benefit          {c |} {res:ctaxben}       \n");
                    SF_display(" Disposable Income            {c |} {res:dispinc}       \n");
                    SF_display(" Total Tax                    {c |} {res:tottax}        \n");
                    SF_display(" Childcare Subsidy            {c |} {res:chcaresub}     \n");
                    SF_display(" Free School Meals            {c |} {res:fsm}           \n");
                    SF_display(" {it:Adult 1}, Pre-tax Earnings    {c |} {res:pretaxearn1}   \n");
                    SF_display(" {it:Adult 1}, Post-tax Earnings   {c |} {res:posttaxearn1}  \n");
                    SF_display(" {it:Adult 1}, Taxable Income      {c |} {res:taxable1}      \n");
                    SF_display(" {it:Adult 1}, Income Tax          {c |} {res:inctax1}       \n");
                    SF_display(" {it:Adult 1}, National Insurance  {c |} {res:natins1}       \n");
                    SF_display(" {it:Adult 2}, Pre-tax Earnings    {c |} {res:pretaxearn2}  \n");
                    SF_display(" {it:Adult 2}, Post-tax Earnings   {c |} {res:posttaxearn2}  \n");
                    SF_display(" {it:Adult 2}, Taxable Income      {c |} {res:taxable2}      \n");
                    SF_display(" {it:Adult 2}, Income Tax          {c |} {res:inctax2}       \n");
                    SF_display(" {it:Adult 2}, National Insurance  {c |} {res:natins2}       \n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                }
                else if (strcmp(argv[i],"listfam")==0)  {
                    SF_display(" {bf:{ul:listfam}}\n");
                    SF_display("\n");
                    SF_display(" Description                  {c |} Internal name\n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                    SF_display(" Couple                       {c |} {res:couple}    \n");
                    SF_display(" Couple is married            {c |} {res:married}    \n");
                    SF_display(" Childcare expenditure        {c |} {res:ccexp}    \n");
                    SF_display(" Maintenance income           {c |} {res:maint}    \n");
                    SF_display(" Number of children           {c |} {res:nkids}    \n");
                    SF_display(" Age of child 1...10          {c |} {res:kidage1...kidage10}    \n");
                    SF_display(" Number of other adults       {c |} {res:nothads}    \n");
                    SF_display(" Housing tenure type          {c |} {res:tenure}    \n");
                    SF_display(" Rent                         {c |} {res:rent}    \n");
                    SF_display(" Rent cap                     {c |} {res:rentcap}    \n");
                    SF_display(" Region                       {c |} {res:region}    \n");
                    SF_display(" Council Tax band             {c |} {res:ctband}    \n");
                    SF_display(" Local tax multipler          {c |} {res:banddratio}    \n");
                    SF_display(" Interview date               {c |} {res:intdate}    \n");
                    SF_display(" {it:Adult 1}, Age                 {c |} {res:age1}    \n");
                    SF_display(" {it:Adult 1}, Earnings            {c |} {res:earn1}    \n");
                    SF_display(" {it:Adult 1}, Hours of work       {c |} {res:hrs1}    \n");
                    SF_display(" {it:Adult 1}, Self-employed       {c |} {res:selfemp1}    \n");
                    SF_display(" {it:Adult 2}, Age                 {c |} {res:age2}    \n");
                    SF_display(" {it:Adult 2}, Earnings            {c |} {res:earn2}    \n");
                    SF_display(" {it:Adult 2}, Hours of work       {c |} {res:hrs2}    \n");
                    SF_display(" {it:Adult 2}, Self-employed       {c |} {res:selfemp2}    \n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                }
                else if (strcmp(argv[i],"listsys")==0) {
                    SF_display(" {bf:{ul:listsys}}\n");
                    SF_display("\n");
                    SF_display(" System description           {c |} Internal name\n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                    SF_display(" April 1991                   {c |} {res:April91}    \n");
                    SF_display(" April 1992                   {c |} {res:April92}    \n");
                    SF_display(" April 1993                   {c |} {res:April93}    \n");            
                    SF_display(" April 1994                   {c |} {res:April94}    \n");
                    SF_display(" April 1995                   {c |} {res:April95}    \n");
                    SF_display(" April 1996                   {c |} {res:April96}    \n");
                    SF_display(" April 1997                   {c |} {res:April97}    \n");
                    SF_display(" April 1998                   {c |} {res:April98}    \n");
                    SF_display(" April 1999                   {c |} {res:April99}    \n");
                    SF_display(" April 2000                   {c |} {res:April00}    \n");
                    SF_display(" April 2001                   {c |} {res:April01}    \n");
                    SF_display(" April 2002                   {c |} {res:April02}    \n");
                    SF_display(" April 2003                   {c |} {res:April03}    \n");
                    SF_display(" April 2004                   {c |} {res:April04}    \n");
                    SF_display(" April 2005                   {c |} {res:April05}    \n");
                    SF_display(" April 2006                   {c |} {res:April06}    \n");
                    SF_display(" April 2007                   {c |} {res:April07}    \n");
                    SF_display(" April 2008                   {c |} {res:April08}    \n");
                    SF_display(" April 2009                   {c |} {res:April09}    \n");
                    SF_display(" Autumn 1999                  {c |} {res:Autumn99}    \n");
                    SF_display(" Autumn 2000                  {c |} {res:Autumn00}    \n");
                    SF_display(" Autumn 2001                  {c |} {res:Autumn01}    \n");
                    SF_display(" Autumn 2002                  {c |} {res:Autumn02}    \n");
                    SF_display(" Autumn 2003                  {c |} {res:Autumn03}    \n");
                    SF_display(" Autumn 2004                  {c |} {res:Autumn04}    \n");
                    SF_display(" Autumn 2005                  {c |} {res:Autumn05}    \n");
                    SF_display(" Autumn 2006                  {c |} {res:Autumn06}    \n");
                    SF_display(" Autumn 2007                  {c |} {res:Autumn07}    \n");
                    SF_display(" Autumn 2008                  {c |} {res:Autumn08}    \n");
                    SF_display(" Autumn 2009                  {c |} {res:Autumn09}    \n");
                    SF_display("{hline 30}{c +}{hline 16}\n");
                }
            }
            
            return(0);
        }
            
       
        //load tax system
        strcpy(sysstr,argv[SYSNAME]);
        sysstrlen = strlen(sysstr);
        
        //load from database
        if (sysmode==0) {            
            c_get_sysdb(sysstr,&sysstrlen,&ifail);
            if (ifail>0) {
                SF_error("system name ");
                SF_error(sysstr);
                SF_error(" does not exist in database\n");
                return(601);
            }
        }
        //load from file (first check if file exists)
        else {            
            FILE *stream;
            stream = fopen(sysstr, "r");
            if (stream == NULL) {
                SF_error("system file ");
                SF_error(sysstr);
                SF_error(" does not exist\n");
                return(601);
            }
            fclose(stream);
            //call fortax to load
            //note that if there are any problems reading
            //file this will crash stata at the moment
            //because no control is passed back
            c_statasys(sysstr,&sysstrlen);
        }

        //uprate systems
        uprate = atof(argv[UPRATE]);
        c_statauprate(&uprate);
        
        //output variable labels?
        label    = atoi(argv[LABMODE]);
        //number of family components set
        nfamlist = atoi(argv[NFAMLIST]);
        //number of net-income output variables
        nnetlist = atoi(argv[NNETLIST]);
        
        
        for(j = SF_in1(); j <= SF_in2(); j++) {
            if(SF_ifobs(j)) {
  
                //initialize family type
                c_statafaminit();
                
                //set family characteristics
                for(i=1;i <=nfamlist; i++) {
                    if ((rc = SF_vdata(i,j,&z))) return(rc);
                    if(SF_is_missing(z)==false) {
                        strcpy(famstr,argv[ARGLIST+i-1]);
                        famstrlen = strlen(famstr);
                        c_statafamset(famstr,&famstrlen,&z);
                    }
                }
                
                c_statafamfin();
                
                //calculate incomes
                c_stataincome();
                
                //set net incomes
                for(i=1;i<=nnetlist;i++) {                   
                    strcpy(netstr,argv[ARGLIST-1+nfamlist+i]);
                    netstrlen = strlen(netstr);
                    c_statanetget(netstr,&netstrlen,&b);
                    //copy to stata
                    if ((rc = SF_vstore(nfamlist+i, j, b))) return(rc);
                }                
            }
        }
        
        char macname[255];
        char macval[255];

        if (label>0) {
            for(i=1;i<=nnetlist;i++) {
                sprintf(macname, "%s%d", "_fortaxlabel",i);
                strcpy(netstr,argv[ARGLIST-1+nfamlist+i]);
                netstrlen = strlen(netstr);
                c_statalabelget(netstr,&netstrlen,macval);
                rtrim(macval);
                if ((rc = SF_macro_save(macname,macval))) return(rc);
                
            }
        }
        return(0) ;
    }

