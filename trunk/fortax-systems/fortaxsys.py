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

import optparse
import csv
import fortaxdb
import os.path

def main():
    parser = optparse.OptionParser(version='%prog version 0.01a')
    
    parser.add_option(
        '-i', '--includes',
        help='generate FORTAX include files',
        dest='include',
        default=False,
        action='store_true'
    )
    parser.add_option(
        '-d', '--date',
        help='system date YYYYMMDD',
        dest='date',
        action='store_true'
    )
    
    parser.add_option(
        '-l', '--label',
        help='system label',
        dest='label',
        default='historic',
        action='store_true'
    )
    
    (opts, args) = parser.parse_args()
    
    print args
    print opts
    
    sysComponents = []
    sysNames = []

    #open the csv file that lists the system components
    if not(os.path.isfile('syslist.csv')):
        print 'file syslist.csv does not exist'
        sys.exit()
          
    sysList = csv.reader(open('syslist.csv'), delimiter=',', quotechar='"')

    if opts.include:
        f = open("syslist.inc", "w")
        f.write('#undef _$typelist\n')

    for row in sysList:
        sysComponents.append(row[0])
        sysNames.append(row[1])
        if opts.include:
            f.write('#define _$typelist '+row[0]+'\n')
            f.write('#include "'+row[0]+'.inc"'+'\n\n')
            f.write('#undef _$typelist\n')
            f.write('')
            
    if opts.include:
        f.close()

    #construct internal system database
    fdb = []
    for index, sysname in enumerate(sysNames):        
        fdb.append(fortaxdb.fortaxVar(sysname,opts,args))
        
        #check whether file exists
        if not(os.path.isfile(sysname)):
            print 'file '+sysname+' does not exist'
            sys.exit()
            
        sysfile = csv.reader(open(sysname), delimiter=',', quotechar='"')

    
if __name__ == "__main__":
    main()