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

class fortaxStorage():
    integer      = 1
    logical      = 2
    double       = 3
    integerarray = 4
    logicalarray = 5
    doublearray  = 6

class fortaxStorageName():
    integer  = ['i','int','integer']
    logical  = ['l','log','logical']
    double   = ['d','dble','double']
    integerarray  = ['ia','intarray','integerarray']
    logicalarray  = ['la','logarray','logicalarray']
    doublearray   = ['da','dblearray','doublearray']
    
class varClass(object):
    """Class docstring."""

    def __init__(self, varindex, varname, vartype, varstorage, vardata, varlabel):
        """Method docstring."""
        self.varindex = varindex
        self.varname = varname
        self.vartype = vartype
        self.varstorage = varstorage
        self.period = -1
        self.source = -1
        self.label = varlabel
        self.data = vardata
        
    def findPeriod(self,header):
        self.period = self.find('@.',self.varname,header)
           
    def findSource(self,header):
        self.source = self.find('@~',self.varname,header)
        
    def find(self, prefix, varname, header):
        for h, hstr in enumerate(header):
            if hstr[0:len(prefix)]==prefix:
                if hstr[len(prefix):]==varname:
                    return h
        return -1
    
class fortaxVar():
    def __init__(self,fname,opts,args):
        
        #check whether file exists
        if not(os.path.isfile(fname)):
            print 'file '+fname+' does not exist'
            sys.exit()
        
        #open file
        sysfile = csv.reader(open(fname), delimiter=',', quotechar='"')

        #initialize
        self.db = []
        existHeader = False
        self.existLabel  = False
        self.numRec = 0
        #read file
        for row in sysfile:
            row2 = [x.strip() for x in row]
            if row2[0]=='@#':
                self.db.append(row2)
                self.numRec = self.numRec+1
            elif row2[0]=='@^':
                if existHeader:
                    print 'error: multiple header records detected in '+fname
                    sys.exit()                    
                self.header = row2
                existHeader = True
            elif row2[0]=='@?':
                if self.existLabel:
                    print 'error: multiple label records detected in '+fname
                    sys.exit()                    
                self.label = row2
                self.existLabel = True

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
            
        idDate  = [int(0) for ixD in range(self.numRec)]
        idLabel = ['' for ixD in range(self.numRec)]
        for ixD in range(self.numRec):
            if not checkDate(self.db[ixD][dateInd]):
                print 'error: invalid YYYYMMDD date format in '+fname
                sys.exit()
            else:
                idDate[ixD]  = self.db[ixD][dateInd]
                idLabel[ixD] = self.db[ixD][labelInd]
                
        #now get variables
        self.varlist = []
        self.getFortaxVar()
        
        for var in self.varlist:
            var.findSource(self.header)
            var.findPeriod(self.header)
        
            
    def getFortaxVar(self):
        for h, hstr in enumerate(self.header):
            if hstr[0:2]=='@_':
                thisvar=hstr[2:].split('.')
                valid, varName, varType, varStorage = self.validVarConstruct(thisvar)
                if not valid:
                    print 'error: ' + thisvar + ' is not a valid fortax variable construct'
                    sys.exit()
                varData = []
                for ixD in range(self.numRec):
                    try:
                        varData.append(self.db[ixD][h])
                    except:
                        print 'error: data missing for '+varName #+' in '+self.fname
                        sys.exit()
                        
                if self.existLabel:
                    varLabel = self.label
                else:
                    varLabel = varName
                    
                self.varlist.append(varClass(h,varName,varType,varStorage,varData,varLabel))
            
    def validVarConstruct(self,thisvar):
        """Check whether we have a valid fortax variable construct."""
        validLength = self.validVarConstructLength(thisvar)
        if not validLength:
            return False        
        validName, varName = self.validVarConstructName(thisvar[0])
        if not validName:
            return False 
        validType, varType = self.validVarConstructType(thisvar[1])
        if not validType:
            return False 
        validStorage, varStorage = self.validVarConstructStorage(thisvar[2])
        if not validStorage:
            return False 
        
        return True, varName, varType, varStorage
    
    def validVarConstructLength(self,varlen):
        """Return True if length 3, False otherwise."""
        if len(varlen)!=3:
            print 'variable must specify name, type and storage'
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
        if vartype in ('rng','range'):
            type = 'range'
        elif vartype in ('rate'):
            type = 'rate'
        elif vartype in ('a','amt','amount'):
            type = 'amount'
        elif vartype in ('ma','minamt','minamount'):
            type = 'minamount'
        elif vartype in ('n','null'):
            type = 'null'
        else:
            print 'variable type must be range, rate, amount, minamount, null (or abbreviated forms)'
            return False, ''
    
        return True, type
    
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