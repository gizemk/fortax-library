#!/usr/bin/python
# -*- coding: utf-8 -*-

# This file is part of FORTAX Calculator;
# (c) 2009 Andrew Shephard; andrubuntu@gmail.com
#
# FORTAX Calculator is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# FORTAX Calculator is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.

# You should have received a copy of the GNU General Public License along
# with FORTAX Calculator.  If not, see <http://www.gnu.org/licenses/>.

import sys 
import os
import csv
import string
from   PyQt4.QtCore import *
from   PyQt4.QtGui  import *

from   returnbudcon import *

#UIgrid is user interface produced with QtDesigner, myTable defines a table class
from UIgrid   import *
from myTable  import *

#resources file
import resources

class myMainWindow(QMainWindow):
    """ modify the mainwindow class so it emits a signal when resized
    """
    def resizeEvent(self,event):
        self.emit(SIGNAL("signalResize()"))

class StartQT4(myMainWindow): #QMainWindow
    def __init__(self, *args):
        myMainWindow.__init__(self, *args)

        #main interface setup
        self.ui = Ui_MainWindow()
        self.ui.setupUi(self)

        #icon
        icon = QIcon(':/icons/icon.png')
        self.setWindowIcon(icon)

        #initialize some variables
        self.numkids = 0
        self.numsys  = 0
        self.doHours = True
        self.headersKinks=["Hours", "Earnings", "Income", "Rate"]

        self.dir    = os.path.dirname(sys.executable)
        self.dir    = os.getcwd()
        self.resdir = os.path.join(self.dir,'resources')
        self.sysDir = os.path.join(self.resdir,'systems')
        self.savdir = self.dir

        if os.path.isdir(self.sysDir):
            self.ui.lineSysDir.setText(self.sysDir)
            self.sysList()

        self.ui.gridLayout_2.removeWidget(self.ui.tabKinks)
        self.ui.tabKinks.setParent(None)
        del self.ui.tabKinks

        self.ui.tabKinks = QTabWidget(self.ui.centralwidget)
        self.ui.tabKinks.setObjectName("tabKinks")
        self.ui.gridLayout_2.addWidget(self.ui.tabKinks, 0, 1, 2, 1)

        thispage=QWidget()
        gridLayout = QVBoxLayout()
        tableKinks = myTableKinks(self.ui.tabKinks)

        figButton  = QPushButton(self.ui.tabKinks)
        copyButton = QPushButton(self.ui.tabKinks)
        saveButton = QPushButton(self.ui.tabKinks)

        figButton.setText("Figure")        
        copyButton.setText("Copy")
        saveButton.setText("Save")
        
        tableKinks.setObjectName('tableKinks0')
        tableKinks.horizontalHeader().setVisible(True)
        tableKinks.verticalHeader().setVisible(False)
        tableKinks.setHorizontalHeaderLabels(self.headersKinks)
        
        gridLayout.addWidget(tableKinks)
        hbox = QHBoxLayout()
        hbox.addStretch(1)
        hbox.addWidget(figButton)
        hbox.addWidget(copyButton)
        hbox.addWidget(saveButton)
        gridLayout.addLayout(hbox)
        thispage.setLayout(gridLayout)
        self.ui.tabKinks.addTab(thispage,'System')
            
        self.ui.pushHideShowIncomes.setText("Show incomes")
        self.ui.pushHideShowKinks.setText("Show kinks")
        self.ui.pushHideShowSettings.setText("Show settings")

        showHidewidth = 0
        showHidewidth = max(showHidewidth,self.ui.pushHideShowIncomes.width())
        showHidewidth = max(showHidewidth,self.ui.pushHideShowKinks.width())
        showHidewidth = max(showHidewidth,self.ui.pushHideShowSettings.width())

        self.ui.pushHideShowIncomes.setText("Hide incomes")
        self.ui.pushHideShowKinks.setText("Hide kinks")
        self.ui.pushHideShowSettings.setText("Hide settings")

        showHidewidth = max(showHidewidth,self.ui.pushHideShowIncomes.width())
        showHidewidth = max(showHidewidth,self.ui.pushHideShowKinks.width())
        showHidewidth = max(showHidewidth,self.ui.pushHideShowSettings.width())

        self.ui.pushHideShowIncomes.setMinimumWidth(showHidewidth)
        self.ui.pushHideShowKinks.setMinimumWidth(showHidewidth)
        self.ui.pushHideShowSettings.setMinimumWidth(showHidewidth)

        self.ui.tabIncome.setTabEnabled(2,False)

        self.ui.tableIncomes = myTable(self.ui.tabIncomesFamily)
        self.ui.tabKinks.setEnabled(False)

        self.ui.gridLayoutIncomes.addWidget(self.ui.tableIncomes)
        self.ui.tableIncomes.setObjectName("tableIncomes")
        self.ui.tableIncomes.horizontalHeader().setVisible(False)

        self.ui.tableIncomeAd1 = myTable(self.ui.tabIncomesAd1)
        self.ui.gridLayoutIncomeAd1.addWidget(self.ui.tableIncomeAd1)
        self.ui.tableIncomeAd1.setObjectName("tableIncomeAd1")
        self.ui.tableIncomeAd1.horizontalHeader().setVisible(False)

        self.ui.tableIncomeAd2 = myTable(self.ui.tabIncomesAd2)
        self.ui.gridLayoutIncomeAd2.addWidget(self.ui.tableIncomeAd2)
        self.ui.tableIncomeAd2.setObjectName("tableIncomeAd2")
        self.ui.tableIncomeAd2.horizontalHeader().setVisible(False)

        #define some actions
        self.connect(self.ui.addKid, SIGNAL('clicked()'), self.addKidAction)
        self.connect(self.ui.removeKid, SIGNAL('clicked()'), self.removeKidAction)
        self.connect(self.ui.checkCouple,SIGNAL('clicked()'), self.coupleAction)
        self.connect(self.ui.toolSysBrowse,SIGNAL('clicked()'), self.sysBrowse)
        self.connect(self.ui.lineSysDir,SIGNAL('returnPressed()'), self.sysList)
        self.connect(self.ui.lineSysDir,SIGNAL('editingFinished()'), self.sysList)
        self.connect(self.ui.pushFortax,SIGNAL('clicked()'), self.calc)
        self.connect(self.ui.pushCopyIncomes,SIGNAL('clicked()'), self.ui.tableIncomes.copyTable)
        self.connect(self.ui.pushSaveIncomes,SIGNAL('clicked()'), self.ui.tableIncomes.saveTable)
        self.connect(self.ui.pushCopyIncomeAd1,SIGNAL('clicked()'), self.ui.tableIncomeAd1.copyTable)
        self.connect(self.ui.pushSaveIncomeAd1,SIGNAL('clicked()'), self.ui.tableIncomeAd1.saveTable)
        self.connect(self.ui.pushCopyIncomeAd2,SIGNAL('clicked()'), self.ui.tableIncomeAd2.copyTable)
        self.connect(self.ui.pushSaveIncomeAd2,SIGNAL('clicked()'), self.ui.tableIncomeAd2.saveTable)
        self.connect(self.ui.checkPriceDate,SIGNAL('clicked()'), self.checkDateAction)
        self.connect(self.ui.radioHoursBc,SIGNAL('clicked()'), self.bcModeAction)
        self.connect(self.ui.radioEarnBc,SIGNAL('clicked()'), self.bcModeAction)
        self.connect(self.ui.pushHideShowSettings,SIGNAL('clicked()'), self.hideShowSettings)
        self.connect(self.ui.pushHideShowKinks,SIGNAL('clicked()'), self.hideShowKinks)
        self.connect(self.ui.pushHideShowIncomes,SIGNAL('clicked()'), self.hideShowIncomes)
        self.connect(self,SIGNAL('signalResize()'), self.adjustAllColumns)
        self.connect(self.ui.tabKinks,SIGNAL('currentChanged(QWidget *)'), self.adjustAllColumns)
        self.connect(self.ui.pushFigures,SIGNAL('clicked()'), self.allFigures)

        #define shortcut keys
        shortcutCalc = QShortcut(self)
        shortcutCalc.setKey("Ctrl+Return")
        self.connect(shortcutCalc,SIGNAL('activated()'), self.calc)

        shortcutSettingsPanel = QShortcut(self)
        shortcutSettingsPanel.setKey("Ctrl+1")
        self.connect(shortcutSettingsPanel,SIGNAL('activated()'), self.hideShowSettings)    

        shortcutKinksPanel = QShortcut(self)
        shortcutKinksPanel.setKey("Ctrl+2")
        self.connect(shortcutKinksPanel,SIGNAL('activated()'), self.hideShowKinks) 
        
        shortcutIncomesPanel = QShortcut(self)
        shortcutIncomesPanel.setKey("Ctrl+3")
        self.connect(shortcutIncomesPanel,SIGNAL('activated()'), self.hideShowIncomes) 
      
        self.taxFile = os.path.join(self.resdir,'taxlist.csv')
        if not os.path.isfile(self.taxFile):
            self.ui.comboTaxOut.setEnabled(False)
        else:
            reader = csv.reader(open(self.taxFile, "rb"))
            self.taxLongName   = []
            self.taxLongNameAd = []
            self.taxShortName  = []
            self.taxLevel      = []

            for row in reader:
                self.taxLongName.append(row[0])
                self.taxShortName.append(row[1])
                self.taxLevel.append(row[2])
                if row[2]=='ad1':
                    self.taxLongNameAd.append('Adult 1: '+row[0])
                elif row[2]=='ad2':
                    self.taxLongNameAd.append('Adult 2: '+row[0])
                else:
                    self.taxLongNameAd.append(row[0])

            self.taxNum = len(self.taxLevel)
            self.ui.comboTaxOut.clear()
            self.ui.comboTaxOut.addItems(self.taxLongNameAd)
            ind = self.ui.comboTaxOut.findText("Disposable Income")
            if ind>=0:
                self.ui.comboTaxOut.setCurrentIndex(ind)

        self.rpiFile = os.path.join(self.resdir,'rpi.csv')

        if not os.path.isfile(self.rpiFile):
            self.ui.checkPriceDate.setEnabled(False)
        else:
            reader = csv.reader(open(self.rpiFile, "rb"), delimiter=',', quoting=csv.QUOTE_NONE)
            reader.next() #skip header
            self.priceDate  = []
            self.priceIndex = []
            for row in reader:
                self.priceDate.append(int(row[0]))
                self.priceIndex.append(float(row[1]))
            self.priceNum = len(self.priceDate)

            date = QDate.fromString(QString(str(self.priceDate[0])),"yyyyMMdd")
            self.ui.datePriceDate.setMinimumDate(date)

            date = QDate.fromString(QString(str(self.priceDate[self.priceNum-1])),"yyyyMMdd")
            self.ui.datePriceDate.setMaximumDate(date)
        
            self.ui.datePriceDate.setDate(date)

        self.bcModeAction()
        self.coupleAction()

    def allFigures(self):
        """ plot budget constraints for all active systems
        """
        if self.doHours:
            columnX = 0; columnY = 2
            pound = u"\u00A3" #pound
            labelX = 'Hours per week'
            labelY = self.incomeComponent
            titleString = 'Budget constraint, fixed ' + pound + string.strip(format(self.wageOrHours,"7.2f")) + ' hourly wage'
        else:
            columnX = 1; columnY = 2
            labelX = 'Earnings, pounds per week'
            labelY = self.incomeComponent
            titleString = 'Budget constraint, fixed at ' + string.strip(format(self.wageOrHours,"7.0f")) + ' hours'

        dataX = []
        dataY = []
        leg  = []

        for s in range(self.numsys):
            objname = QString("tableKinks"+str(s))
            tableKinks = self.ui.tabKinks.findChild(myTableKinks,objname)
            if tableKinks:
                dataX.append(array(tableKinks.tableData[:,columnX]))
                dataY.append(array(tableKinks.tableData[:,columnY]))
                leg.append(tableKinks.tableName)
        
        aw = figureWindow(dataX=dataX,dataY=dataY,
            labelLegend=leg,labelX=labelX,labelY=labelY,
            labelTitle=titleString,sourceNote=self.source)
        aw.exec_()

    def adjustAllColumns(self):
        """ if adjust all column sizes
        """
        if self.ui.tabKinks.isVisible():
            for s in range(self.numsys):
                objname = QString("tableKinks"+str(s))
                tableKinks = self.ui.tabKinks.findChild(myTableKinks,objname)
                if tableKinks:
                    tableKinks.adjustColumnSizes()

        if self.ui.tableIncomes.isVisible():
            self.ui.tableIncomes.adjustColumnSizes()
        if self.ui.tableIncomeAd1.isVisible():
            self.ui.tableIncomeAd1.adjustColumnSizes()
        if self.ui.tableIncomeAd2.isVisible():
            self.ui.tableIncomeAd2.adjustColumnSizes()

    def hideShowIncomes(self):
        """ hide/show incomes panel
        """
        if (not self.ui.tabSettings.isVisible()) and (not self.ui.tabKinks.isVisible()):
            return

        if self.ui.tabIncome.isVisible():
            self.ui.tabIncome.setVisible(False)
            self.ui.pushHideShowIncomes.setText("Show incomes")
            if not self.ui.tabKinks.isVisible():
                self.ui.pushHideShowSettings.setEnabled(False)
            if not self.ui.tabSettings.isVisible():
                self.ui.pushHideShowKinks.setEnabled(False)
        else:
            self.ui.tabIncome.setVisible(True)
            self.ui.pushHideShowIncomes.setText("Hide incomes")
            self.ui.pushHideShowIncomes.setMinimumWidth(100)
            self.ui.pushHideShowIncomes.setEnabled(True)
            self.ui.pushHideShowKinks.setEnabled(True)
            self.ui.pushHideShowSettings.setEnabled(True)
        self.adjustAllColumns()

    def hideShowSettings(self):
        """ hide/show settings panel
        """
        if (not self.ui.tabIncome.isVisible()) and (not self.ui.tabKinks.isVisible()):
            return

        if self.ui.tabSettings.isVisible():
            self.ui.tabSettings.setVisible(False)
            self.ui.pushHideShowSettings.setText("Show settings")
            if not self.ui.tabKinks.isVisible():
                self.ui.pushHideShowIncomes.setEnabled(False)
            if not self.ui.tabIncome.isVisible():
                self.ui.pushHideShowKinks.setEnabled(False)
        else:
            self.ui.tabSettings.setVisible(True)
            self.ui.pushHideShowSettings.setText("Hide settings")
            self.ui.pushHideShowIncomes.setEnabled(True)
            self.ui.pushHideShowKinks.setEnabled(True)
            self.ui.pushHideShowSettings.setEnabled(True)
        self.adjustAllColumns()

    def hideShowKinks(self):
        """ hide/show kinks panel
        """
        if (not self.ui.tabSettings.isVisible()) and (not self.ui.tabIncome.isVisible()):
            return

        if self.ui.tabKinks.isVisible():
            self.ui.tabKinks.setVisible(False)
            self.ui.pushHideShowKinks.setText("Show kinks")
            if not self.ui.tabSettings.isVisible():
                self.ui.pushHideShowIncomes.setEnabled(False)
            if not self.ui.tabIncome.isVisible():
                self.ui.pushHideShowSettings.setEnabled(False)
        else:
            self.ui.tabKinks.setVisible(True)
            self.ui.pushHideShowKinks.setText("Hide kinks")
            self.ui.pushHideShowIncomes.setEnabled(True)
            self.ui.pushHideShowKinks.setEnabled(True)
            self.ui.pushHideShowSettings.setEnabled(True)
        self.adjustAllColumns()

    def bcModeAction(self):
        """ change visibility depending on whether earnings/hours kinks
        """
        if self.ui.radioHoursBc.isChecked():
            self.ui.labelEarnHours.setEnabled(False)
            self.ui.doubleSpinEarnHours.setEnabled(False)
            self.ui.labelEarnStart.setEnabled(False)
            self.ui.doubleSpinEarnStart.setEnabled(False)
            self.ui.labelEarnEnd.setEnabled(False)
            self.ui.doubleSpinEarnEnd.setEnabled(False)
            self.ui.labelWage.setEnabled(True)
            self.ui.doubleSpinWage.setEnabled(True)
            self.ui.labelHoursStart.setEnabled(True)
            self.ui.doubleSpinHoursStart.setEnabled(True)
            self.ui.labelHoursEnd.setEnabled(True)
            self.ui.doubleSpinHoursEnd.setEnabled(True)
        else:
            self.ui.labelEarnHours.setEnabled(True)
            self.ui.doubleSpinEarnHours.setEnabled(True)
            self.ui.labelEarnStart.setEnabled(True)
            self.ui.doubleSpinEarnStart.setEnabled(True)
            self.ui.labelEarnEnd.setEnabled(True)
            self.ui.doubleSpinEarnEnd.setEnabled(True)
            self.ui.labelWage.setEnabled(False)
            self.ui.doubleSpinWage.setEnabled(False)
            self.ui.labelHoursStart.setEnabled(False)
            self.ui.doubleSpinHoursStart.setEnabled(False)
            self.ui.labelHoursEnd.setEnabled(False)
            self.ui.doubleSpinHoursEnd.setEnabled(False)

    def checkDateAction(self):
        """ if using uprating date change cal visibility
        """
        if self.ui.checkPriceDate.isChecked():
            self.ui.labelPriceDate.setEnabled(True)
            self.ui.datePriceDate.setEnabled(True)
        else:
            self.ui.labelPriceDate.setEnabled(False)
            self.ui.datePriceDate.setEnabled(False)


    def calc(self):
        """ performs calculations, calls FORTAX
        """
        nosys = False

        #is system okay?
        if self.ui.listSysFiles.currentRow()<0:
            nosys = True
        else:
            selsys = self.ui.listSysFiles.selectedItems()
            self.numsys = 0
            sysname = []
            sysnameNoExt = []
            for thissys in selsys:
                sysname.append(str(thissys.text()))
                sysnameNoExt.append(os.path.splitext(str(thissys.text()))[0])
                self.numsys+=1

        if nosys:
            reply = QMessageBox.warning(self, 'FORTAX: Error',
                "No system is selected", QMessageBox.Ok)
            return
        
        #check ranges of hours/earnings
        if self.ui.radioHoursBc.isChecked():
            self.wageOrHours = self.ui.doubleSpinWage.value()
            range0 = self.ui.doubleSpinHoursStart.value()
            range1 = self.ui.doubleSpinHoursEnd.value()
            if range0>range1:
                reply = QMessageBox.warning(self, 'FORTAX: Error',
                    "End hours must be larger than start hours", QMessageBox.Ok)
                return
        else:
            self.wageOrHours = self.ui.doubleSpinEarnHours.value()
            range0 = self.ui.doubleSpinEarnStart.value()
            range1 = self.ui.doubleSpinEarnEnd.value()
            if range0>range1:
                reply = QMessageBox.warning(self, 'FORTAX: Error',
                    "End earn must be larger than start earn", QMessageBox.Ok)
                return

        #hours or earnings kinks?
        if self.ui.radioHoursBc.isChecked():
            self.doHours = True
        else:
            self.doHours = False

        adult       = self.ui.comboAdult.currentIndex()+1
        couple      = self.ui.checkCouple.isChecked()
        married     = self.ui.checkMarried.isChecked()
        age1        = self.ui.spinAge1.value()
        age2        = self.ui.spinAge2.value()
        wage1       = self.ui.doubleSpinWage1.value()
        wage2       = self.ui.doubleSpinWage2.value()
        hours1      = self.ui.doubleSpinHrs1.value()
        hours2      = self.ui.doubleSpinHrs2.value()
        selfemp1    = self.ui.checkSelfEmp1.isChecked()
        selfemp2    = self.ui.checkSelfEmp2.isChecked()
        tenure      = self.ui.comboTenure.currentIndex()+1
        rent        = self.ui.doubleSpinRent.value()
        ctband      = self.ui.comboCouncilTax.currentIndex()+1
        banddratio  = self.ui.doubleSpinBandD.value()
        childcare   = self.ui.doubleSpinChildcare.value()
        maintenance = self.ui.doubleSpinMaintenance.value()

        self.ui.pushFigures.setEnabled(True)
        if couple:
            self.ui.tabIncome.setTabEnabled(2,True)
        else:
            self.ui.tabIncome.setTabEnabled(2,False)

        setprice = self.ui.checkPriceDate.isChecked()
        if (setprice):
            pricetarget = int(QDate.toString(self.ui.datePriceDate.date(),"yyyyMMdd"))
            self.source = "Source: User calculation using FORTAX. Incomes expressed in" + QDate.toString(self.ui.datePriceDate.date()," MMMM yyyy ") + "prices."
        else:
            pricetarget = 0
            self.source = "Source: User calculation using FORTAX. Incomes expressed in nominal prices."        

        kids = zeros(max(self.numkids,1),int)
        if self.numkids>0:
            kids[0] = self.ui.kid1.value()
            if self.numkids>1:
                kids[1] = self.ui.kid2.value()
                if self.numkids>2:
                    kids[2] = self.ui.kid3.value()
                    if self.numkids>3:
                        kids[3] = self.ui.kid4.value()
                        if self.numkids>4:
                            kids[4] = self.ui.kid5.value()
                            if self.numkids>5:
                                kids[5] = self.ui.kid6.value()
                                if self.numkids>6:
                                    kids[6] = self.ui.kid7.value()
                                    if self.numkids>7:
                                        kids[7] = self.ui.kid8.value()
                                        if self.numkids>8:
                                            kids[8] = self.ui.kid9.value()
                                            if self.numkids>9:
                                                kids[9] = self.ui.kid10.value()

        
        taxout   = self.taxShortName[self.ui.comboTaxOut.currentIndex()]
        taxlevel = self.taxLevel[self.ui.comboTaxOut.currentIndex()]
        self.incomeComponent = self.taxLongName[self.ui.comboTaxOut.currentIndex()]        

        #convert system list to csv
        sysnamecsv = ','.join([str(i) for i in sysname])

        #call fortax to calculate incomes and budget constraints
        ret = returnbudcon(self.wageOrHours,range0,range1,self.doHours,self.sysDir,self.numsys,sysnamecsv,
             taxout,taxlevel,adult,couple,married,
             age1,wage1,hours1,selfemp1,age2,wage2,hours2,selfemp2,
             self.numkids,kids,tenure,rent,ctband,banddratio,childcare,maintenance,
             setprice,pricetarget,self.priceDate,self.priceIndex,self.priceNum)
        
        #detailed incomes
        headers0=[]; headers1=[]; headers2=[]
        m0=0; m1=0; m2=0
        list0=[]; list1=[]; list2=[]

        #ret 0, kinks_hrs
        #ret 1, kinks_earn
        #ret 2, kinks_net
        #ret 3, kinks_mtr
        #ret 4, kinks_num
        #ret 5, netoutLevel
        #ret 6, netoutName
        #ret 7, netoutAmt
        #ret 8  netoutNum

        data0 = zeros((ret[8],self.numsys),float)
        data1 = zeros((ret[8],self.numsys),float)
        data2 = zeros((ret[8],self.numsys),float)

        #note: only amt differs by system
        for item in range(ret[8]):
             if any(ret[7][item])<>0:
                 shortName = string.strip(ndarray.tostring(ret[6][item]))
                 taxLevel  = string.strip(ndarray.tostring(ret[5][item]))
 
                 nameIndex = -1
                 for name in range(self.taxNum):
                     if self.taxLevel[name]==taxLevel and self.taxShortName[name]==shortName:
                         nameIndex = name
                         break
                 if taxLevel=='tu':
                     if nameIndex>=0:
                         headers0.append(self.taxLongName[nameIndex])
                         data0[m0]=ret[7][item]
                         m0+=1
                 elif taxLevel=='ad1':
                     if nameIndex>=0:
                         headers1.append(self.taxLongName[nameIndex])
                         data1[m1]=ret[7][item]
                         m1+=1
                 elif taxLevel=='ad2':
                     if nameIndex>=0:
                         headers2.append(self.taxLongName[nameIndex])
                         data2[m2]=ret[7][item]
                         m2+=1

        if m0>0:
            self.ui.tableIncomes.setData(data0[0:m0],cellFormat="7.2f",readOnly=True)
            self.ui.tableIncomes.setHorizontalHeaderLabels(sysnameNoExt)
            self.ui.tableIncomes.horizontalHeader().setVisible(True)
            self.ui.tableIncomes.setVerticalHeaderLabels(headers0)
            self.ui.tableIncomes.adjustColumnSizes()
            self.ui.pushCopyIncomes.setEnabled(True)
            self.ui.pushSaveIncomes.setEnabled(True)            
        else:
            self.ui.tableIncomes.reset()
            self.ui.pushCopyIncomes.setEnabled(False)
            self.ui.pushSaveIncomes.setEnabled(False)

        if m1>0:
            self.ui.labelAd1NotWorking.setVisible(False)
            self.ui.tableIncomeAd1.setData(data1[0:m1],cellFormat="7.2f",readOnly=True)
            self.ui.tableIncomeAd1.setHorizontalHeaderLabels(sysnameNoExt)
            self.ui.tableIncomeAd1.horizontalHeader().setVisible(True)
            self.ui.tableIncomeAd1.setVerticalHeaderLabels(headers1)
            self.ui.tableIncomeAd1.adjustColumnSizes()
            self.ui.pushCopyIncomeAd1.setEnabled(True)
            self.ui.pushSaveIncomeAd1.setEnabled(True)
        else:
            self.ui.tableIncomeAd1.reset()
            self.ui.labelAd1NotWorking.setVisible(True)
            self.ui.pushCopyIncomeAd1.setEnabled(False)
            self.ui.pushSaveIncomeAd1.setEnabled(False)

        if m2>0:
            self.ui.labelAd2NotWorking.setVisible(False)
            self.ui.tableIncomeAd2.setData(data2[0:m2],cellFormat="7.2f",readOnly=True)
            self.ui.tableIncomeAd2.setHorizontalHeaderLabels(sysnameNoExt)
            self.ui.tableIncomeAd2.horizontalHeader().setVisible(True)
            self.ui.tableIncomeAd2.setVerticalHeaderLabels(headers2)
            self.ui.tableIncomeAd2.adjustColumnSizes()
            self.ui.pushCopyIncomeAd2.setEnabled(True)
            self.ui.pushSaveIncomeAd2.setEnabled(True)
        else:
            self.ui.labelAd2NotWorking.setVisible(True)
            self.ui.tableIncomeAd2.reset()
            self.ui.pushCopyIncomeAd2.setEnabled(False)
            self.ui.pushSaveIncomeAd2.setEnabled(False)

        #destroy kinks tab
        self.ui.gridLayout_2.removeWidget(self.ui.tabKinks)
        self.ui.tabKinks.setParent(None)
        del self.ui.tabKinks

        #create new kinks tab
        self.ui.tabKinks = QTabWidget(self.ui.centralwidget)
        self.ui.tabKinks.setObjectName("tabKinks")
        self.ui.gridLayout_2.addWidget(self.ui.tabKinks, 0, 1, 2, 1)
        self.ui.tabKinks.setEnabled(True)
        
        #create blank page (will delete later)
        thispage=QWidget()
        self.ui.tabKinks.addTab(thispage,'blank')

        for s in range(self.numsys):
            numKinks = ret[4][s]
            my_array = zeros([numKinks,4],float)
            for i in range(numKinks):
                my_array[i,0] = ret[0][i][s]
                my_array[i,1] = ret[1][i][s]
                my_array[i,2] = ret[2][i][s]
                my_array[i,3] = ret[3][i][s]

            thispage=QWidget()
            gridLayout = QVBoxLayout()
            tableKinks = myTableKinks(self.ui.tabKinks)
            figButton = QPushButton(self.ui.tabKinks)
            figButton.setText("Figure")
            copyButton = QPushButton(self.ui.tabKinks)
            copyButton.setText("Copy")
            saveButton = QPushButton(self.ui.tabKinks)
            saveButton.setText("Save")

            figButton.setToolTip(QApplication.translate("MainWindow", "Show figure under "+sysnameNoExt[s]+" system", 
                None, QApplication.UnicodeUTF8))
            copyButton.setToolTip(QApplication.translate("MainWindow", "Copy budget constraint under "+sysnameNoExt[s]+" system to clipboard", 
                None, QApplication.UnicodeUTF8))
            saveButton.setToolTip(QApplication.translate("MainWindow", "Save budget constraint under "+sysnameNoExt[s]+" system as .csv file", 
                None, QApplication.UnicodeUTF8))

            tableKinks.setObjectName('tableKinks'+str(s))
            tableKinks.setData(my_array,cellFormat="7.2f",readOnly=True)
            tableKinks.horizontalHeader().setVisible(True)
            tableKinks.verticalHeader().setVisible(False)
            tableKinks.setHorizontalHeaderLabels(self.headersKinks)

            #extra stuff if used for plotting
            tableKinks.setTableName(sysnameNoExt[s])
            if self.doHours:
                pound = u"\u00A3"
                tableKinks.setFigureLabelY(self.incomeComponent)
                tableKinks.setFigureLabelX('Hours per week')
                titleString = 'Budget constraint, fixed ' + pound + string.strip(format(self.wageOrHours,"7.2f")) + ' hourly wage'
                tableKinks.setFigureLabelTitle(titleString)
                tableKinks.setFigureColumnX(0)
                tableKinks.setFigureColumnY(2)
                tableKinks.setSourceNote(self.source)
            else:
                tableKinks.setFigureLabelY(self.incomeComponent)
                tableKinks.setFigureLabelX('Earnings, pounds per week')
                titleString = 'Budget constraint, fixed at ' + string.strip(format(self.wageOrHours,"7.0f")) + ' hours'
                tableKinks.setFigureLabelTitle(titleString)
                tableKinks.setFigureColumnX(1)
                tableKinks.setFigureColumnY(2)
                tableKinks.setSourceNote(self.source)

            gridLayout.addWidget(tableKinks)

            #add buttons
            hbox = QHBoxLayout()
            hbox.addStretch(1)
            hbox.addWidget(figButton)
            hbox.addWidget(copyButton)
            hbox.addWidget(saveButton)
            gridLayout.addLayout(hbox)
            thispage.setLayout(gridLayout)
            
            self.connect(figButton,SIGNAL('clicked()'),  tableKinks.drawKinks)
            self.connect(copyButton,SIGNAL('clicked()'), tableKinks.copyTable)
            self.connect(saveButton,SIGNAL('clicked()'), tableKinks.saveTable)

            self.ui.tabKinks.addTab(thispage,sysnameNoExt[s])            
            tableKinks.adjustColumnSizes()
            
        self.ui.tabKinks.removeTab(0)

    def toggleAdult2(self):
        """ if adult added, change visibilty and update count
        """
        if self.ui.checkCouple.isChecked():
            self.ui.labelAdult2.setEnabled(True)
            self.ui.age2lab.setEnabled(True)
            self.ui.wage2lab.setEnabled(True)
            self.ui.hrs2lab.setEnabled(True)
            self.ui.spinAge2.setEnabled(True)
            self.ui.doubleSpinWage2.setEnabled(True)
            self.ui.doubleSpinHrs2.setEnabled(True)
            self.ui.checkSelfEmp2.setEnabled(True)
        else:
            self.ui.labelAdult2.setEnabled(False)
            self.ui.age2lab.setEnabled(False)
            self.ui.wage2lab.setEnabled(False)
            self.ui.hrs2lab.setEnabled(False)
            self.ui.spinAge2.setEnabled(False)
            self.ui.doubleSpinWage2.setEnabled(False)
            self.ui.doubleSpinHrs2.setEnabled(False)
            self.ui.checkSelfEmp2.setEnabled(False)   

    def addKidAction(self):
        """ if child added, change visibilty and update count
        """
        self.numkids = min(self.numkids+1,10)
        if self.numkids==1:
            self.ui.kid1.setEnabled(True)
            self.ui.kid1lab.setEnabled(True)
        elif self.numkids==2:
            self.ui.kid2.setEnabled(True)
            self.ui.kid2lab.setEnabled(True)
        elif self.numkids==3:
            self.ui.kid3.setEnabled(True)
            self.ui.kid3lab.setEnabled(True)
        elif self.numkids==4:
            self.ui.kid4.setEnabled(True)
            self.ui.kid4lab.setEnabled(True)
        elif self.numkids==5:
            self.ui.kid5.setEnabled(True)
            self.ui.kid5lab.setEnabled(True)
        elif self.numkids==6:
            self.ui.kid6.setEnabled(True)
            self.ui.kid6lab.setEnabled(True)
        elif self.numkids==7:
            self.ui.kid7.setEnabled(True)
            self.ui.kid7lab.setEnabled(True)
        elif self.numkids==8:
            self.ui.kid8.setEnabled(True)
            self.ui.kid8lab.setEnabled(True)
        elif self.numkids==9:
            self.ui.kid9.setEnabled(True)
            self.ui.kid9lab.setEnabled(True)
        elif self.numkids==10:
            self.ui.kid10.setEnabled(True)
            self.ui.kid10lab.setEnabled(True)

    def removeKidAction(self):
        """ if child removed, change visibilty and update count
        """
        if self.numkids==1:
            self.ui.kid1.setEnabled(False)
            self.ui.kid1lab.setEnabled(False)
        elif self.numkids==2:
            self.ui.kid2.setEnabled(False)
            self.ui.kid2lab.setEnabled(False)
        elif self.numkids==3:
            self.ui.kid3.setEnabled(False)
            self.ui.kid3lab.setEnabled(False)
        elif self.numkids==4:
            self.ui.kid4.setEnabled(False)
            self.ui.kid4lab.setEnabled(False)
        elif self.numkids==5:
            self.ui.kid5.setEnabled(False)
            self.ui.kid5lab.setEnabled(False)
        elif self.numkids==6:
            self.ui.kid6.setEnabled(False)
            self.ui.kid6lab.setEnabled(False)
        elif self.numkids==7:
            self.ui.kid7.setEnabled(False)
            self.ui.kid7lab.setEnabled(False)
        elif self.numkids==8:
            self.ui.kid8.setEnabled(False)
            self.ui.kid8lab.setEnabled(False)
        elif self.numkids==9:
            self.ui.kid9.setEnabled(False)
            self.ui.kid9lab.setEnabled(False)
        elif self.numkids==10:
            self.ui.kid10.setEnabled(False)
            self.ui.kid10lab.setEnabled(False)

        self.numkids = max(self.numkids-1,0)

    def coupleAction(self):
        """ change visibility if a couple is selected
        """
        self.ui.comboAdult.setCurrentIndex(0)
        if self.ui.checkCouple.isChecked():
            self.ui.comboAdult.setEnabled(True)
            self.ui.checkMarried.setEnabled(True)
            self.toggleAdult2()
        else:
            self.ui.comboAdult.setEnabled(False)
            self.ui.checkMarried.setEnabled(False)
            self.toggleAdult2()

    def sysBrowse(self):
        """ browse for system file
        """
        filename = QFileDialog.getExistingDirectory(self, 'Open directory',self.sysDir)
        if filename<>"":
            self.ui.lineSysDir.setText(filename)
            self.sysList()
            self.sysDir = filename #os.path.dirname(str(filename))
                
    def sysList(self):
        """ list tax systems
        """
        #filelist = []
        self.ui.listSysFiles.clear()
        filepath = self.ui.lineSysDir.text()
        if os.path.isdir(filepath):
            files = os.listdir(filepath)
            for p in files:
                filep = os.path.join(str(filepath),p)
                if os.path.isfile(filep):
                    f = open(filep, 'r')
                    head=f.read(21)
                    if head=='<?xml version="1.0"?>':
                        f.readline() #nextline
                        head2 =f.read(8)
                        if head2=='<fortax>':
                            self.ui.listSysFiles.addItem(p)
                            #filelist.append(p)
                    f.close()

            #print filelist
            #filelist2 = []
            #for filename in filelist:
                #filelist2.append(os.path.splitext(filename)[0])

            #print filelist2

            if self.ui.listSysFiles.count()>0:
                self.ui.listSysFiles.sortItems()
                self.ui.listSysFiles.setCurrentRow(0)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    myapp = StartQT4()
    myapp.show()
    sys.exit(app.exec_())

