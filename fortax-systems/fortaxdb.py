#!/usr/bin/env python
# -*- coding: utf-8 -*-

# This file is part of FORTAX Systems;
# (c) 2010 Andrew Shephard; andrubuntu@gmail.com
#
# FORTAX Systems is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# FORTAX Systems is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.

# You should have received a copy of the GNU General Public License along
# with FORTAX Systems.  If not, see <http://www.gnu.org/licenses/>.

import csv
import sys
import os.path
import datetime
from operator import itemgetter
    
class varClass(object):
    """Class docstring."""

    def __init__(self, varindex, varname, vartype, vararray, vardata, varperiod, varlabel):
        """Method docstring."""
        self.varindex = varindex
        self.varname  = varname
        self.vartype  = vartype
        self.vararray = vararray
        self.period   = varperiod
        self.label    = varlabel
        self.data     = vardata
        self.fileLink = [False for ixD in range(len(self.data))]
        
        for ixD in range(len(self.data)):
            if self.data[ixD][0:2]=='@>':
                self.fileLink[ixD] = True
    
class fortaxVar():
    
    def find(self, prefix, varname, header):
        for h, hstr in enumerate(header):
            if hstr[0:len(prefix)]==prefix:
                if hstr[len(prefix):]==varname:
                    return h
        return -1

    def __init__(self,fname,sysname,opts,args):
                
        #check whether file exists
        if not(os.path.isfile(fname)):
            print 'file '+fname+' does not exist'
            sys.exit()
        
        #open file
        sysfile = csv.reader(open(fname), delimiter=',', quotechar='"')

        #initialize
        
        self.sysname     = sysname   #system name
        db               = []        #data in file fname
        existHeader      = False     #if a file header exists
        self.existLabel  = False     #if labels exist for variable
        self.existPeriod = False     #if period exist for variable        
        self.indexFile   = False     #if file is index file
        
        #read sysfile line by line
        
        for row in sysfile:
            row2 = [x.strip() for x in row]
            if row2[0]=='@#':      #data line
                db.append(row2)
            elif row2[0]=='@^':    #header line
                if existHeader:
                    print 'error: multiple header records detected in '+fname
                    sys.exit()                    
                self.header = row2
                existHeader = True
            elif row2[0]=='@!':    #header line of index file
                if existHeader:
                    print 'error: multiple header records detected in '+fname
                    sys.exit()                    
                self.header = row2
                existHeader = True
                self.indexFile = True
            elif row2[0]=='@?':    #label line
                if self.existLabel:
                    print 'error: multiple label records detected in '+fname
                    sys.exit()                    
                self.label = row2
                self.existLabel = True
            elif row2[0]=='@.':    #period line
                if self.existPeriod:
                    print 'error: multiple period records detected in '+fname
                    sys.exit()                    
                self.period = row2
                self.existPeriod = True
        
        #check header exists
        if not existHeader:
            print 'error: no header record exists in '+fname
            sys.exit()
            
        #column with date information
        try:
            dateInd = self.header.index('date')
        except:
            print 'error: no date column in '+fname
            sys.exit()
        
        #column with label information
        try:
            labelInd = self.header.index('label')
        except:
            print 'error: no label column'
            sys.exit()
            
        if self.indexFile:
            try:
                indexInd = self.header.index('index')
            except:
                print 'error: index file but no index column'
                sys.exit()

        #sort data by label, date, index
        if self.indexFile:
            db = sorted(db, key=itemgetter(indexInd))
            
        db = sorted(db, key=itemgetter(dateInd))
        db = sorted(db, key=itemgetter(labelInd))


        
        #number of entries in db
        self.numRec = len(db)
        
        #for each line in db, create lists with date and label information
        #dates will be stored as integers
        #self.idDate  = [int(0) for ixD in range(self.numRec)]
        #self.idLabel = ['' for ixD in range(self.numRec)]
        self.idDate = []
        self.idLabel = []
        self.idDate2 = []
        self.idLabel2 = []    
        #if self.indexFile:
        #    self.idIndex = [int(0) for ixD in range(self.numRec)]
            
        #check whether the date is valid
        for ixD in range(self.numRec):
            if not checkDate(db[ixD][dateInd]):
                print 'error: invalid YYYYMMDD date format in '+fname
                sys.exit()
            else:
                self.idDate.append(int(db[ixD][dateInd]))
                self.idLabel.append(db[ixD][labelInd])

        for ixD in range(self.numRec):
            if (ixD==0) or (db[ixD][labelInd]!=db[ixD-1][labelInd]) or (db[ixD][dateInd]!=db[ixD-1][dateInd]):
                self.idDate2.append(int(db[ixD][dateInd]))
                self.idLabel2.append(db[ixD][labelInd])
                        
        #extract information from the database db
        self.getFortaxVar(db)
        
        #self.missingFortaxVar()
        #self.checkFortaxVar()
        #clean up junk
        del self.header
        #del self.idDate
        if self.existPeriod:
            del self.period
        if self.existLabel:
            del self.label
        del self.existLabel
        del self.existPeriod
        del self.idLabel
        
    def missingFortaxVar(self):
        for var in self.varlist:
            for ixD in range(self.numRec):
                if var.data[ixD]=='':
                    if var.vartype=='range':
                        var.data[ixD] = '0' 
                    if var.vartype in ('amount','minamount','rate'):
                        var.data[ixD] = '0.0'
                    if var.vartype in ('bool'):
                        var.data[ixD] = '0'
                
    def getFortaxVar(self,db):
        
        #varlist will contain the list of components in this system part
        self.varlist = []
        for h, hstr in enumerate(self.header):
            if hstr[0:2]=='@_':
                thisvar=hstr[2:].split('.')
                #check whether we have a valid fortax variable construct
                valid, varName, varType, varArray = self.validVarConstruct(thisvar)
                if not valid:
                    print 'error: ' + hstr + ' is not a valid fortax variable construct'
                    sys.exit()
                    
                varData = []
                
                #if an index file, the data will be a list. note that index is ORDINAL
                if self.indexFile:
                    newObs = True
                    for ixD in range(self.numRec):
                        if newObs:
                            this = []
                            newObs = False
                        this.append(db[ixD][h])
                        
                        if ixD<self.numRec-1:
                            if (self.idLabel[ixD]!=self.idLabel[ixD+1]) or (self.idDate[ixD]!=self.idDate[ixD+1]):
                                varData.append(this)
                                newObs = True
                    varData.append(this)
                else:
                    for ixD in range(self.numRec):
                        varData.append(db[ixD][h])
                        
                #read label if it exists, otherwise set equal to variable name
                if self.existLabel:
                    varLabel = self.label[h]
                else:
                    varLabel = varName
                    
                #read period if it exists. this needs to be strictly positive,
                #otherwise set equal to -1
                if self.existPeriod:
                    varPeriod = self.period[h]
                else:
                    varPeriod = -1

                #append to varlist
                self.varlist.append(varClass(h,varName,varType,varArray,varData,varPeriod,varLabel))
        
    def validVarConstruct(self,thisvar):
        """Check whether we have a valid fortax variable construct."""
        validLength = self.validVarConstructLength(thisvar)
        if not validLength:
            return False, '', '', False
        validName, varName = self.validVarConstructName(thisvar[0])
        if not validName:
            return False, '', '', False 
        validType, varType, varArray = self.validVarConstructType(thisvar[1])
        if not validType:
            return False, '', '', False
       
        return True, varName, varType, varArray
    
    def validVarConstructLength(self,varlen):
        """Return True if length 3, False otherwise."""
        if len(varlen)!=2:
            print 'variable must specify name and type'
            return False
        else:
            return True
            
    def validVarConstructName(self,varname):
        """Return True if legal Fortan variable name, False otherwise."""
        if (len(varname[0])>32):
            return False, ''
        if not(varname[0][0].isalpha()):
            return False, ''       
        for ch in varname[0][1:]:
            if not(ch.isalpha() or ch.isdigit() or ch=='_'):
                return False, ''
            
        return True, varname

    def validVarConstructType(self,vartype):
        """Return True if valid Fortax type, False otherwise."""
        indArray = vartype.find('[]')
        if indArray>0:
            thisType = vartype[0:indArray]
            isArray = True
        else:
            thisType = vartype
            isArray = False
            
        if thisType in ('rng','range'):
            type = 'range'
        elif thisType in ('rate'):
            type = 'rate'
        elif thisType in ('amt','amount'):
            type = 'amount'
        elif thisType in ('minamt','minamount'):
            type = 'minamount'
        elif thisType in ('bool'):
            type = 'bool'
        else:
            print 'variable type must be range, rate, amount, minamount, bool (or abbreviated forms)'
            return False, ''
    
        return True, type, isArray
    
    def validVarConstructStorage(self,varStorage):
        """Return True if valid Fortax storage, False otherwise."""
        if varStorage in fortaxStorageName.integer:
            storage = fortaxStorage.integer
        elif varStorage in fortaxStorageName.logical:
            storage = fortaxStorage.logical
        elif varStorage in fortaxStorageName.double:
            storage = fortaxStorage.double
        elif varStorage in fortaxStorageName.integerarray:
            storage = fortaxStorage.integerarray
        elif varStorage in fortaxStorageName.logicalarray:
            storage = fortaxStorage.logicalarray
        elif varStorage in fortaxStorageName.doublearray:
            storage = fortaxStorage.doublearray
        else:
            print 'storage must be integer, integerarray, logical, logicalarray, double, doublearray (or abbreviated forms)'
            return False
        return True, storage
    
    def validName(varname):
        """Return True is legal Fortan variable name, False otherwise."""
        if (len(varname[0])>32):
            return False
        if not(varname[0][0].isalpha()):
            return False        
        for ch in varname[0][1:]:
            if not(ch.isalpha() or ch.isdigit() or ch=='_'):
                return False
            
        return True
                        
    def validPeriod(period):
        """Determine whether period is valid."""
        try:
            i = float(period)
        except ValueError:
            return False
        else:
            if i>0:
                return True
            else:
                return False        
 

def checkDate(datestr):
    """Return True if a valid date, False otherwise."""
    try:
        year = int(datestr[0:4])
    except:
        return False
        
    try:
        month = int(datestr[4:6])
    except:
        return False
    
    try:
        day = int(datestr[6:8])
    except:
        return False
    
    try:
        datetime.date(year, month, day)
    except ValueError:
        return False
    
    return True
    
def getFortaxSysIndex(db,date):
    if not checkDate(date):
        print 'error: invalid date'
        sys.exit()
    else:
        intDate = int(date)
    
    if db.indexFile:
        if date<db.idDate2[0]:
            print 'error: requested date is out-of-range'
            sys.exit()
        if intDate>=db.idDate2[-1]:
            dateIndex = db.numRec-1
        else:
            for ixD in range(len(db.idDate2)):                
                if intDate>=db.idDate2[ixD] and intDate<db.idDate2[ixD+1]:
                    dateIndex = ixD        
    else:
        if date<db.idDate[0]:
            print 'error: requested date is out-of-range'
            sys.exit()
        if intDate>=db.idDate[-1]:
            dateIndex = db.numRec-1
        else:
            for ixD in range(db.numRec-1):
                if intDate>=db.idDate[ixD] and intDate<db.idDate[ixD+1]:
                    dateIndex = ixD
            
    return dateIndex

def fortaxFileLinks(db):
    fileLinks = []
    for thisDb in db:
        for var in thisDb.varlist:
            for ixD in range(thisDb.numRec):
                if var.data[ixD][0:2]=='@>':
                    thisLink = var.data[ixD][2:]
                    try:
                        fileLinks.index(thisLink)
                    except:
                        fileLinks.append(thisLink)
    return fileLinks
                
def recursiveLinking(db):
    for thisDb in db:
        for var in thisDb.varlist:
            for ixD in range(len(var.data)): #range(thisDb.numRec):
                if isinstance(var.data[ixD],list):
                    for a in var.data[ixD]:
                        if a[0:2]=='@>':
                            return True
                else:
                    if var.data[ixD][0:2]=='@>':
                            return True
    return False

def getLinkValue(db_name,db_link,var_name,date):
    for thisDb in db_link:
        if thisDb.sysname==db_name:
            dateIndex = getFortaxSysIndex(thisDb,date)
            for var in thisDb.varlist:
                if var.varname==var_name:
                    return var.data[dateIndex]
    return None

def writeXml(db,db_link,date):

    print '<?xml version="1.0"?>'
    print '<fortax>'
    for thisDb in db:
        #get date index
        dateIndex = getFortaxSysIndex(thisDb,date)
        print '<system basename="'+thisDb.sysname+'">'
        for var in thisDb.varlist:
            xmlStr = '   <'
            if var.vartype=='range':
                xmlStr = xmlStr+'finteger'
            elif var.vartype in ('amount','minamount','rate'):
                xmlStr = xmlStr+'fdouble'
            elif var.vartype in ('bool'):
                xmlStr = xmlStr+'flogical'
            
            if var.vararray:
                xmlStr = xmlStr+'array'
            if var.fileLink[dateIndex]:
                linkVal2 = getLinkValue(var.data[dateIndex][2:],db_link,var.varname,date)
                linkVal = linkVal2[0]
                for val in linkVal2[1:]:
                    linkVal = linkVal + ','+val
            else:
                linkVal = var.data[dateIndex]
                
            xmlStr = xmlStr + ' name="'+var.varname+'" value="'
            xmlStr = xmlStr + linkVal
            xmlStr = xmlStr + '">'
            print xmlStr
        print '</system>'
    print '</fortax>'
