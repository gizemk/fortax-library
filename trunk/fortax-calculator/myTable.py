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

import os
import csv
from PyQt4.QtCore import *
from PyQt4.QtGui  import *
#from numpy        import *
from numpy        import ndarray, zeros, array

#import matplotlib for figure plotting
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure

import resources

class figureWindow(QDialog):
    """ budget constraont figure class
    """
    def __init__(self,dataX,dataY,labelLegend="",labelX="",labelY="",labelTitle="",labelFigure="Figure",sourceNote=""):
        QDialog.__init__(self)
        self.setAttribute(Qt.WA_DeleteOnClose)
        self.setWindowTitle(labelFigure)

        l  = QVBoxLayout(self)
        sc = MyMplCanvas(self, width=5, height=4, dpi=100)
     
        datanum = 0
        for data in dataX:
            datanum+=1

        for data in range(datanum):
            sc.axes.plot(dataX[data],dataY[data],label=labelLegend[data],linewidth=1)
    
        sc.axes.set_xlabel(labelX,size='x-small')
        sc.axes.set_ylabel(labelY,size='x-small')
        sc.axes.set_title(labelTitle,size='small')
        
        sc.axes.grid(True)
        labels = sc.axes.get_xticklabels() + sc.axes.get_yticklabels()
        for label in labels:
            label.set_size('x-small')

        leg = sc.axes.legend(loc='best')
        for t in leg.get_texts():
            t.set_fontsize('x-small')
        
        l.addWidget(sc)

        if sourceNote<>"":
            source = QLabel(sourceNote)
            l.addWidget(source)

        self.setFocus()
        
class MyMplCanvas(FigureCanvas):
    
    def __init__(self, parent=None, width=5, height=4, dpi=100):
        fig = Figure(figsize=(width, height), dpi=dpi)
        self.axes = fig.add_subplot(111)
        #hold to allow multiple plots
        self.axes.hold(True)

        FigureCanvas.__init__(self, fig)
        self.setParent(parent)

        FigureCanvas.setSizePolicy(self,
                                   QSizePolicy.Expanding,
                                   QSizePolicy.Expanding)
        FigureCanvas.updateGeometry(self)        

class myTable(QTableWidget):
    def __init__(self, parent):
        QTableWidget.__init__(self, parent)

        font = QFont()
        font.setPointSize(8)
        self.setFont(font)
        self.tableName = ''
        self.tableData = []
        #self.removeAction()
        self.__initActions__()
        self.__initContextMenus__()
    savdir = '/home/andrew/'

    def setSavdir(self,savdir):
        myTable.savdir = savdir

    def getSavdir(self):
        return myTable.savdir

    def setTableName(self,name):
        self.tableName = name

    def getTableName(self):
        return tableName

    def adjustColumnSizes(self):
        self.hide()
        self.show()
        if self.columnCount()>0:
            available = self.width()-self.verticalHeader().width()-2
            for col in range(self.columnCount()):
                available-=self.columnWidth(col)
            perColumn = available/self.columnCount()
            for col in range(self.columnCount()):
                self.setColumnWidth(col,max(self.columnWidth(col)+perColumn,70))

    def reset(self):
        self.setColumnCount(0)
        self.setRowCount(0)

    def setData(self,data,cellFormat="",readOnly=False):
        dataSize = data.shape

        self.setColumnCount(dataSize[1])
        self.setRowCount(dataSize[0])
        self.tableData = data

        m = 0
        for row in data:
            n = 0
            for item in row:
                newitem = QTableWidgetItem(format(item,cellFormat))
                if readOnly:
                    newitem.setFlags(Qt.ItemIsSelectable|Qt.ItemIsEnabled)
                self.setItem(m, n, newitem)
                n+=1
            m+=1

    def getData(self,getHeader=False):
        if getHeader:
            horizontalHeader = self.existHorizontalHeaders()
            verticalHeader   = self.existVerticalHeaders()
        else:
            horizontalHeader = False
            verticalHeader = False

        data = []

        if horizontalHeader:
            header = self.getHorizontalHeaders()
            if verticalHeader:
               header.insert(0,"")
            data.append(header)

        header = self.getVerticalHeaders()

        for row in range(self.rowCount()):
            thisrow = []
            if verticalHeader:
                thisrow.append(header[row])
            for col in range(self.columnCount()):
                cell = self.item(row, col)
                if cell:
                    thisrow.append(str(cell.text()))
                else:
                    thisrow.append("")                
            data.append(thisrow)
        return data

    def copyTable(self,getHeader=True):
        if getHeader:
            horizontalHeader = self.existHorizontalHeaders()
            verticalHeader   = self.existVerticalHeaders()
        else:
            horizontalHeader = False
            verticalHeader = False

        clipStr = QString()

        #horizontalHeader
        if horizontalHeader:
            header = self.getHorizontalHeaders()
            if verticalHeader:
               header.insert(0,"")
            for head in header:
                clipStr.append(head)
                clipStr.append("\t")
            clipStr.chop(1)
            clipStr.append("\n")

        #verticalHeader
        header = self.getVerticalHeaders()

        for row in range(self.rowCount()):
            if verticalHeader:
                clipStr.append(header[row])
                clipStr.append(QString("\t"))
            for col in range(self.columnCount()):
                cell = self.item(row, col)
                if cell:
                    clipStr.append(cell.text())
                else:
                    clipStr.append(QString(""))
                clipStr.append(QString("\t"))
            clipStr.chop(1)
            clipStr.append(QString("\n"))
       
        cb = QApplication.clipboard()
        cb.setText(clipStr)

    def saveTable(self,filename=""):
        data = self.getData(True)
        noFilename = False
        if filename=="":
            filename = QFileDialog.getSaveFileName(self, 'Save file',myTable.savdir,'*.csv')
            noFilename = True
        if filename<>"":
            writer = csv.writer(open(filename, 'w'))
            writer.writerows(data)
            if noFilename:
                myTable.savdir = os.path.dirname(str(filename))

    def existHorizontalHeaders(self):
        exist = False
        for col in range(self.columnCount()):
            cell = self.horizontalHeaderItem(col)
            if cell:
                exist = True
                break
        return exist

    def existVerticalHeaders(self):
        exist = False
        for row in range(self.rowCount()):
            cell = self.verticalHeaderItem(row)
            if cell:
                exist = True
                break
        return exist

    def getHorizontalHeaders(self):
        headers = []
        for col in range(self.columnCount()):
            cell = self.horizontalHeaderItem(col)
            if cell:
                headers.append(str(cell.text()))
            else:
                headers.append("")
        return headers

    def getVerticalHeaders(self):
        headers = []
        for row in range(self.rowCount()):
            cell = self.verticalHeaderItem(row)
            if cell:
                headers.append(str(cell.text()))
            else:
                headers.append("")
        return headers


    def __initActions__(self):
        icon = QIcon(':/icons/copy.png')
        self.copyAction = QAction("&Copy selection",self)
        self.copyAction.setIcon(icon)
        #QIconSet(QPixmap(editcut)),

        self.copyAction.setShortcut("Ctrl+C")
        self.copyAction.setShortcutContext(Qt.WidgetShortcut)
        self.addAction(self.copyAction)
        self.connect(self.copyAction, SIGNAL("triggered()"), self.copyCells)

    def __initContextMenus__(self):
        #icon = QPixmap(':/icons/copy.png')
        #self.setPixMap(icon)
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.connect(self, SIGNAL("customContextMenuRequested(QPoint)"), self.tableWidgetContext)

    def tableWidgetContext(self, point):
        tw_menu = QMenu("Menu", self)
        tw_menu.addAction(self.copyAction)
        tw_menu.exec_(self.mapToGlobal(point))

    def copyCells(self):

        rowSel = [False]*self.rowCount()
        colSel = [False]*self.columnCount()

        horizontalHeader = self.existHorizontalHeaders()
        verticalHeader   = self.existVerticalHeaders()

        #loop to determine if any cells in each row or column are selected
        for row in range(self.rowCount()):
            for col in range(self.columnCount()):
                cell = self.item(row, col)
                if self.isItemSelected(cell):
                    rowSel[row] = True
                    colSel[col] = True

        clipStr = QString()

        if horizontalHeader:
            header = self.getHorizontalHeaders()
            if verticalHeader:
                clipStr.append("\t")
            for col in range(self.columnCount()):
                if colSel[col]:
                    clipStr.append(header[col])
                    clipStr.append("\t")
            clipStr.chop(1)
            clipStr.append("\n")

        if verticalHeader:
            header = self.getVerticalHeaders()

        for row in range(self.rowCount()):
            if rowSel[row]:
                if verticalHeader:
                    clipStr.append(header[row])
                    clipStr.append(QString("\t"))
                for col in range(self.columnCount()):
                    if colSel[col]:
                        cell = self.item(row, col)
                        if self.isItemSelected(cell):
                            clipStr.append(cell.text())
                            clipStr.append(QString("\t"))
                        else:
                            clipStr.append(QString("\t"))
                        #clipStr.append(QString("\t"))
                clipStr.chop(1)
                clipStr.append(QString("\n"))

        cb = QApplication.clipboard()
        cb.setText(clipStr)

        return

class myTableKinks(myTable):
    """ modify myTable class to containt information for plotting kinks
    """
    def __init__(self, parent):
        myTable.__init__(self,parent)
        self.labelX = ""
        self.labelY = "Net income"
        self.labelTitle = "Budget constraint"
        self.columnX = 0
        self.columnY = 1
        self.sourceNote = ""

    def setSourceNote(self,source):
        self.sourceNote = source

    def getSourceNote(self,source):
        return self.sourceNote

    def setFigureColumnX(self,col):
        self.columnX = col

    def setFigureColumnY(self,col):
        self.columnY = col

    def setFigureLabelY(self,label):
        self.labelY = label

    def setFigureLabelX(self,label):
        self.labelX = label

    def setFigureLabelY(self,label):
        self.labelY = label

    def setFigureLabelTitle(self,label):
        self.labelTitle = label

    def getFigureLabelX(self):
        return self.labelX

    def getFigureLabelY(self):
        return self.labelY

    def getFigureLabelTitle(self):
        return self.labelTitle

    def drawKinks(self):
        data = array(self.tableData)
        aw = figureWindow(dataX=[data[:,self.columnX]],dataY=[data[:,self.columnY]],
            labelLegend=[self.tableName],
            labelX=self.labelX,labelY=self.labelY,
            labelTitle=self.labelTitle,sourceNote=self.sourceNote)
        aw.exec_()

