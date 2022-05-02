#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Copyright (c) 2015 Antonio Cervellino, Ruggero Frison, Federica Bertolotti, Antonietta Guagliardi
#
#     This file is part of the Claude / Debussy program suite
#     
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 3 of the License (29 June 2007), 
#     or (at your option) any later version.
#     
#     The Claude / Debussy program suite is distributed in the hope that it will be 
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License, version 3, 29 June 2007 for more details.
#     
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA or
#     see <http://www.gnu.org/licenses/>.
#
#===================================================================================================

##############
import sys
import os
import tempfile
import glob
import wx
import wx.lib.dialogs
# import wx.lib.shas bad mtimecrolledpanel as scrolled
sys.modules['wx.lib.agw'] = None
import subprocess
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
import numpy as np
from numpy import sin, cos, tan, arcsin, arccos, arctan, arctan2, exp, log, log10, sqrt, degrees, radians, pi, e, sign
# import matplotlib.pyplot as plt
sys.path.append(gset.GUI_Path)
# from debfuncx import Q, q, d, get_sep, indmtx, srtmtx
# from plotter import indmtx, srtmtx
###############

def Q(tt,l):
    return 4*pi*sin(radians(tt/2))/l

def q(tt,l):
    return 2*sin(radians(tt/2))/l

def d(tt,l):
    return l/(2*sin(radians(tt/2)))

####__sorting functions for mtx files
def indmtx(a):
    l = np.argwhere(a==max(a)).flatten()
    c = np.arange(len(a))
    d = np.delete(c,l)
    e = np.delete(c,l-min(l))
    return l,d,e

def srtmtx(a,l,d,e):
    b = np.zeros(len(a))
    b[l-min(l)] = a[l]
    b[e] = a[d]
    return b

def missing(aaa):
    la=glob.glob(a)
    if len(la) > 0 :
        mis = False
    else : 
        mis = True
    return mis
##--------------

#if gset.GUI_Path.endswith(gv.SEP) : gset.GUI_Path = gset.GUI_Path.strip(gv.SEP)
##__GUI window
verbose = False
hlptxt = '''For a 'XY-plot' select the corresponding button at the top left corner of the custom plotting window.\n\
Select the file to plot either entering the name (with path if necessary) or using the 'Browse' button;\n\
Enter the column number for the x-axis in the field 'x', this will be 'xj' variable, where 'j' is the line number in the plotting window;\n\
Enter the column number for the y-axis in the field 'y', this will be 'yj' variable, where 'j' is the line number in the plotting window;\n\
This is enough to obtain a plot.\n\
Optionally:\n\
- the secondary y-axis can be enabled entering the column number in the field 'z',  this will be 'zj' variable, where 'j' is the line number in the plotting window;\n\
- use the field 'Transform' to perform some operation on the colums, for example, to make a Int. vs Q plot on the first line type:\n\
4*pi*sin(radians(x1/2))/<yourlambda>, y1 or Q(x1, <yourlambda>), y1 (since the functions Q and q = Q/(2*pi) and d(x1, <yourlambda>) have been defined); to plot the difference (y2-y1) vs x2 type: x2, y2-y1; it is also possible to use the notation fj[k] to identify a column, where 'j' is the file (i.e. line number) and 'k' the column number within the file.\n\
- use the 'Options' field to customise the colour, marker and label according to the matplotlib syntax, for example to use a red line type: 'r-', or to use a label: label = '<your label>', or both: 'r-', label = '<your label>';\n\
- to modify the range on one of the axes use the corrensponding field entering the lower and upper limits sepatared by a blank space, e.g.: 10.1 50.5\n\
- for the labels on one of the axes enter the text in the corresponding field, for mathematical symbols use TeX-like syntax enclosing them within $-sign, e.g. 2$\\theta$\n\
- for the lagend the following keywors are valid:\n\
-- loc = # where # is an integer 0 <= # <= 10 with the meaning:\n\
       0  : best\n\
       1  : upper right\n\
       2  : upper left\n\
       3  : lower left\n\
       4  : lower right\n\
       5  : right\n\
       6  : center left\n\
       7  : center right\n\
       8  : lower center\n\
       9  : upper center\n\
       10 : center
-- fontsize = # : a float number in points
-- markerscale = # : a float number
-- numpoints = # : an integer number (e.g. 1 or 2) specifying the number of points in the legend for line
-- scatterpoints = # : an integer number specifying of points in the legend for scatter plot
-- frameon = True/False : if True (default), draw or not a frame around the legend
--labelspacing = # : float number, the vertical space between the legend entries
-- ncol = # : integer number of columns
-- columnspacing = # : float number, the spacing between columns
-- title = 'some text' : the legend title\n
- and so on...\n\
*- more on 'Options' (some examples):\n\
--- for a scatter plot with circle markers, white face and red edge, size 5 and edge width 3\n\
    the Option string would be: 'o', mfc = 'w', mec = 'r', ms = 5, mew = 3\n\
    where:\n\
    mfc : marker face color\n\
    mec : marker edge color\n\
    ms : marker size\n\
    mew : marker edge width\n\
    additionally, there are other keywords:\n\
    ls : linestyle {'-', 'None', '--', '-.', '-', ':'}\n\
    lw : linewidth {float value in points}\n\
    marker :  {0, 1, 2, 3, 4, 'D', 6, 7, 's', '|', 'None', 'x', 5, '_', '^', 'd', 'h', '+', '*', ', ', 'o', '.', '1'\n\
                'p', '3', '2', '4', 'H', 'v', '8', '<', '>'}\n\
--- to produce a bar-type plot, just add the following: <type='bar'> in the 'Options' field\
 [where the angle brackets should me omitted (here used  only to delimit the field)].\n\
#~~~~~~~~~~~~~~~~\n\
For a 2D-map\n\
Use the 'File', 'x', 'y' and 'z' to define the file, the x- and y-axis and the intensity scale, respectively.\n\
The 'Transform' field can be used to perform some opertations with the columns, e.g., w.r.t. the syntax described above:\
 f1[8], f1[9], f1[4]-f2[4] will produce a map using the 8th coulmn of the first file as x-axis, the 9th column of the first file\
  as y-axis and the difference between the 4th columns of the first and second file as intensity scale.\n\
The 'Options' field can be used to pass any of the options of the matplotlib-imshow function, e.g.:\
interpolation = 'nearest', alpha = 0.5 for more details see: http://matplotlib.org/api/pyplot_api.html#matplotlib.pyplot.imshow.\n\
With the 'map threshold' field it is possible to apply a threshold on the intensity scale, calculated along the specified axis, e.\n\
g. typing: f1[4] 0.01, only the values of the map having a value greater than 0.01 in the 4th column of the specified file will be plotted.'''
####___MAIN WINDOW
class CustomPlotter(wx.Frame):
  def __init__(self):
    # create a frame, no parent, default to wxID_ANY
    wx.Frame.__init__(self, None, wx.ID_ANY, title = 'custom plotting', \
    pos = (50, 50), size = (850, 700))##, style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
    #     self.Bind(wx.EVT_CLOSE, self.OnClose)
    clr = self.GetBackgroundColour()
    #     self.frame = parent

    if gset.Platform[:3] == 'dar':
        font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
        self.SetFont(font)

    vbox = wx.BoxSizer(wx.VERTICAL)

    hbox0 = wx.BoxSizer(wx.HORIZONTAL)
    self.rb1 = wx.RadioButton(self, -1, 'XY-plot', style = wx.RB_GROUP)
    self.rb1.SetValue(True)
    hbox0.Add(self.rb1, flag = wx.ALL, border = 5)
    self.rb2 = wx.RadioButton(self, -1, '2D MAP')
    hbox0.Add(self.rb2, flag = wx.ALL, border = 5)
    vbox.Add(hbox0, proportion = 1, flag = wx.ALIGN_LEFT, border = 3)

    hbox1 = wx.BoxSizer(wx.HORIZONTAL)
    nmf = wx.StaticText(self, label = "#", size = (25, -1))
    hbox1.Add(nmf)
    tcf = wx.TextCtrl(self, style = wx.TE_CENTRE)
    tcf.SetBackgroundColour(clr)
    tcf.SetValue('File')
    hbox1.Add(tcf, wx.EXPAND, border = 3)
    btnf = wx.Button(self, label = "  ", size = (30, -1))
    hbox1.Add(btnf, border = 3)
    tcolx = wx.TextCtrl(self, size = (30, -1), style = wx.TE_CENTRE)
    tcolx.SetBackgroundColour(clr)
    tcolx.SetValue('x\ncol #')
    hbox1.Add(tcolx)
    tcoly = wx.TextCtrl(self, size = (30, -1), style = wx.TE_CENTRE)
    tcoly.SetBackgroundColour(clr)
    tcoly.SetValue('y\ncol #')
    hbox1.Add(tcoly)
    tcolz = wx.TextCtrl(self, size = (30, -1), style = wx.TE_CENTRE)
    tcolz.SetBackgroundColour(clr)
    tcolz.SetValue('z\ncol #')
    hbox1.Add(tcolz)
    tcsc = wx.TextCtrl(self, style = wx.TE_CENTRE)
    tcsc.SetBackgroundColour(clr)
    tcsc.SetValue('Transform')
    hbox1.Add(tcsc, wx.EXPAND)
    tcopt = wx.TextCtrl(self, size = (200, -1), style = wx.TE_CENTRE)
    tcopt.SetBackgroundColour(clr)
    tcopt.SetValue('Options')
    hbox1.Add(tcopt, wx.EXPAND)
    vbox.Add(hbox1, proportion = 1, flag = wx.ALL|wx.EXPAND, border = 3)
    
    self.nline = 11
    for fn in range(1, self.nline):
        hboxn = wx.BoxSizer(wx.HORIZONTAL)
        nmf = wx.StaticText(self, label = "%i"%(fn), size = (25, -1))
        hboxn.Add(nmf)
        tcf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'file%i'%fn)
        hboxn.Add(tcf, wx.EXPAND, border = 3)
        btnf = wx.Button(self, label = "...", size = (30, -1))
        hboxn.Add(btnf, border = 3)
        btnf.Bind(wx.EVT_BUTTON, lambda event, place = 'file%i'%fn:\
           self.btnf_click(event, place))
        btnf.SetToolTip(wx.ToolTip("Browse and select a file .."))
        tcolx = wx.TextCtrl(self, size = (30, -1), name = 'file%i_x'%fn)
        hboxn.Add(tcolx)
        tcoly = wx.TextCtrl(self, size = (30, -1), name = 'file%i_y'%fn)
        hboxn.Add(tcoly)
        tcolz = wx.TextCtrl(self, size = (30, -1), name = 'file%i_z'%fn)
        hboxn.Add(tcolz)
        tcsc = wx.TextCtrl(self, name = 'file%i_tra'%fn)
        hboxn.Add(tcsc, wx.EXPAND)
        tcopt = wx.TextCtrl(self, size = (200, -1), name = 'file%i_opt'%fn)
        hboxn.Add(tcopt, wx.EXPAND)
        vbox.Add(hboxn, proportion = 1, flag = wx.ALL|wx.EXPAND, border = 3)

    hbox2 = wx.BoxSizer(wx.HORIZONTAL)
    txt = wx.StaticText(self, label = "non mandatory options", size = (200, -1))
    hbox2.Add(txt)
    txt1 = wx.StaticText(self, label = "", size = (125, -1))
    hbox2.Add(txt1)
    txt2 = wx.StaticText(self, label = "", size = (200, -1))
    hbox2.Add(txt2)
    vbox.Add(hbox2, proportion = 1, flag = wx.ALL, border = 0)
    hbox3 = wx.BoxSizer(wx.HORIZONTAL)
    xrng = wx.StaticText(self, label = "x-range", size = (125, -1))
    hbox3.Add(xrng)
    txrng = wx.TextCtrl(self, size = (200, -1), name = 'range')
    hbox3.Add(txrng)
    hxrng = wx.StaticText(self, label = "xmin xmax", size = (250, -1))
    hbox3.Add(hxrng)
    vbox.Add(hbox3, proportion = 1, flag = wx.ALL, border = 0)
    hbox4 = wx.BoxSizer(wx.HORIZONTAL)
    yrng = wx.StaticText(self, label = "y-range", size = (125, -1))
    hbox4.Add(yrng)
    tyrng = wx.TextCtrl(self, size = (200, -1), name = 'yrange')
    hbox4.Add(tyrng)
    hyrng = wx.StaticText(self, label = "ymin ymax", size = (250, -1))
    hbox4.Add(hyrng)
    vbox.Add(hbox4, proportion = 1, flag = wx.ALL, border = 0)
    hbox5 = wx.BoxSizer(wx.HORIZONTAL)
    zrng = wx.StaticText(self, label = "y2 / int.-range", size = (125, -1))
    hbox5.Add(zrng)
    tzrng = wx.TextCtrl(self, size = (200, -1), name = 'zrange')
    hbox5.Add(tzrng)
    hzrng = wx.StaticText(self, label = "y2min / int. min  y2max / int. max", size = (250, -1))
    hbox5.Add(hzrng)
    vbox.Add(hbox5, proportion = 1, flag = wx.ALL, border = 0)
    hbox6 = wx.BoxSizer(wx.HORIZONTAL)
    thre = wx.StaticText(self, label = "map threshold", size = (125, -1))
    hbox6.Add(thre)
    tthre = wx.TextCtrl(self, size = (200, -1), name = 'threshold')
    hbox6.Add(tthre)
    hthre = wx.StaticText(self, label = "col#  value", size = (250, -1))
    hbox6.Add(hthre)
    vbox.Add(hbox6, proportion = 1, flag = wx.ALL, border = 0)
    hbox7 = wx.BoxSizer(wx.HORIZONTAL)
    xlbl = wx.StaticText(self, label = "x-axis : label", size = (125, -1))
    hbox7.Add(xlbl)
    txlbl = wx.TextCtrl(self, size = (200, -1), name = 'xlabel')
    hbox7.Add(txlbl)
    hxlbl = wx.StaticText(self, label = "some text (e.g. %s)"%(r'2$\theta$ [deg]'), size = (250, -1))
    hbox7.Add(hxlbl)
    self.cbxaxt = wx.CheckBox(self, label = 'top', size = (50, -1))
    self.cbxaxt.SetValue(True)
    hbox7.Add(self.cbxaxt)
    self.cbxaxb = wx.CheckBox(self, label = 'bottom', size = (70, -1))
    self.cbxaxb.SetValue(True)
    hbox7.Add(self.cbxaxb, flag = wx.LEFT, border = 5)
    self.cbxtc = wx.CheckBox(self, label = 'ticks')
    self.cbxtc.SetValue(True)
    hbox7.Add(self.cbxtc, flag = wx.LEFT, border = 5)
    self.cbxtlbl = wx.CheckBox(self, label = 'ticklabels')
    self.cbxtlbl.SetValue(True)
    hbox7.Add(self.cbxtlbl, flag = wx.LEFT, border = 5)
    vbox.Add(hbox7, proportion = 1, flag = wx.ALL, border = 0)
    hbox8 = wx.BoxSizer(wx.HORIZONTAL)
    ylbl = wx.StaticText(self, label = "y-axis : label", size = (125, -1))
    hbox8.Add(ylbl)
    tylbl = wx.TextCtrl(self, size = (200, -1), name = 'ylabel')
    hbox8.Add(tylbl)
    hylbl = wx.StaticText(self, label = "some text (e.g. %s)"%(r'2$\theta$ [deg]'), size = (250, -1))
    hbox8.Add(hylbl)
    self.cbyaxl = wx.CheckBox(self, label = 'left', size = (50, -1))
    self.cbyaxl.SetValue(True)
    hbox8.Add(self.cbyaxl)
    self.cbyaxr = wx.CheckBox(self, label = 'right', size = (70, -1))
    self.cbyaxr.SetValue(True)
    hbox8.Add(self.cbyaxr, flag = wx.LEFT, border = 5)
    self.cbytc = wx.CheckBox(self, label = 'ticks')
    self.cbytc.SetValue(True)
    hbox8.Add(self.cbytc, flag = wx.LEFT, border = 5)
    self.cbytlbl = wx.CheckBox(self, label = 'ticklabels')
    self.cbytlbl.SetValue(True)
    hbox8.Add(self.cbytlbl, flag = wx.LEFT, border = 5)
    vbox.Add(hbox8, proportion = 1, flag = wx.ALL, border = 0)
    hbox9 = wx.BoxSizer(wx.HORIZONTAL)
    zlbl = wx.StaticText(self, label = "y2-axis : label", size = (125, -1))
    hbox9.Add(zlbl)
    tzlbl = wx.TextCtrl(self, size = (200, -1), name = 'zlabel')
    hbox9.Add(tzlbl)
    hzlbl = wx.StaticText(self, label = "some text (e.g. %s)"%(r'2$\theta$ [deg]'), size = (250, -1))
    hbox9.Add(hzlbl)
    #     self.cbzax = wx.CheckBox(self, label = 'axis')
    #     self.cbzax.SetValue(True)
    #     hbox9.Add(self.cbzax)
    self.cbztc = wx.CheckBox(self, label = 'ticks')
    self.cbztc.SetValue(True)
    hbox9.Add(self.cbztc, flag = wx.LEFT, border = 130)
    self.cbztlbl = wx.CheckBox(self, label = 'ticklabels')
    self.cbztlbl.SetValue(True)
    hbox9.Add(self.cbztlbl, flag = wx.LEFT, border = 5)
    vbox.Add(hbox9, proportion = 1, flag = wx.ALL, border = 0)
    hbox91 = wx.BoxSizer(wx.HORIZONTAL)
    titl = wx.StaticText(self, label = "legend", size = (125, -1))
    hbox91.Add(titl)
    ttitl = wx.TextCtrl(self, size = (200, -1), name = 'legend')
    hbox91.Add(ttitl)
    htitl = wx.StaticText(self, label = "legend options (e.g. %s)"%(r'loc = n with n = 0-10, for other see Help'), size = (400, -1))
    hbox91.Add(htitl)
    #     self.rbl1 = wx.RadioButton(self, -1, 'on', style = wx.RB_GROUP)
    #     self.rbl1.SetValue(False)
    #     hbox91.Add(self.rbl1, flag = wx.LEFT, border = 5)
    #     self.rbl2 = wx.RadioButton(self, -1, 'off')
    #     self.rbl2.SetValue(True)
    #     hbox91.Add(self.rbl2, flag = wx.LEFT, border = 5)
    vbox.Add(hbox91, proportion = 1, flag = wx.ALL, border = 0)
    hbox10 = wx.BoxSizer(wx.HORIZONTAL)
    titl = wx.StaticText(self, label = "title", size = (125, -1))
    hbox10.Add(titl)
    ttitl = wx.TextCtrl(self, size = (200, -1), name = 'title')
    hbox10.Add(ttitl)
    htitl = wx.StaticText(self, label = "some text", size = (250, -1))
    hbox10.Add(htitl)
    vbox.Add(hbox10, proportion = 1, flag = wx.ALL, border = 0)
    hbox11 = wx.BoxSizer(wx.HORIZONTAL)
    wdt = wx.StaticText(self, label = "width", size = (125, -1))
    hbox11.Add(wdt)
    twdt = wx.TextCtrl(self, size = (200, -1), name = 'width')
    hbox11.Add(twdt)
    hwdt = wx.StaticText(self, label = "20.32 cm", size = (250, -1))
    hbox11.Add(hwdt)
    vbox.Add(hbox11, proportion = 1, flag = wx.ALL, border = 0)
    hbox12 = wx.BoxSizer(wx.HORIZONTAL)
    high = wx.StaticText(self, label = "height", size = (125, -1))
    hbox12.Add(high)
    thigh = wx.TextCtrl(self, size = (200, -1), name = 'height')
    hbox12.Add(thigh)
    hhigh = wx.StaticText(self, label = "15.24 cm", size = (250, -1))
    hbox12.Add(hhigh)
    vbox.Add(hbox12, proportion = 1, flag = wx.ALL, border = 0)
    hbox13 = wx.BoxSizer(wx.HORIZONTAL)
    ftyp = wx.StaticText(self, label = "font type", size = (125, -1))
    hbox13.Add(ftyp)
    tftyp = wx.TextCtrl(self, size = (200, -1), name = 'fonttype')
    hbox13.Add(tftyp)
    hftyp = wx.StaticText(self, label = "serif", size = (250, -1))
    hbox13.Add(hftyp)
    vbox.Add(hbox13, proportion = 1, flag = wx.ALL, border = 0)
    hbox14 = wx.BoxSizer(wx.HORIZONTAL)
    fsiz = wx.StaticText(self, label = "font size", size = (125, -1))
    hbox14.Add(fsiz)
    tfsiz = wx.TextCtrl(self, size = (200, -1), name = 'fontsize')
    hbox14.Add(tfsiz)
    hfsiz = wx.StaticText(self, label = "11", size = (250, -1))
    hbox14.Add(hfsiz)
    vbox.Add(hbox14, proportion = 1, flag = wx.ALL, border = 0)

    hbox15 = wx.BoxSizer(wx.HORIZONTAL)
    help_btn = wx.Button(self, label = 'Help')
    help_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
    help_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
    hbox15.Add(help_btn, flag = wx.ALL, border = 3)
    sav_btn = wx.Button(self, label = 'SAVE')
    sav_btn.Bind(wx.EVT_BUTTON, self.SaveInp)
    sav_btn.SetToolTip(wx.ToolTip("Save input data"))    
    hbox15.Add(sav_btn, flag = wx.ALL, border = 3)
    load_btn = wx.Button(self, label = 'LOAD')
    load_btn.Bind(wx.EVT_BUTTON, self.LoadInp)
    load_btn.SetToolTip(wx.ToolTip("Load input data"))    
    hbox15.Add(load_btn, flag = wx.ALL, border = 3)
    close_btn = wx.Button(self, label = "Close")
    close_btn.Bind(wx.EVT_BUTTON, self.OnClose)
    close_btn.SetToolTip(wx.ToolTip("Close the window"))
    hbox15.Add(close_btn, flag = wx.ALL, border = 3)
    clear_btn = wx.Button(self, label = "Clear")
    clear_btn.Bind(wx.EVT_BUTTON, self.ClearAll)
    clear_btn.SetToolTip(wx.ToolTip("Clear all fields"))
    hbox15.Add(clear_btn, flag = wx.ALL, border = 3)
    plot_btn = wx.Button(self, label = "PLOT")
    plot_btn.Bind(wx.EVT_BUTTON, lambda event, mode = 'plot':self.DoPlot(event, mode))
    plot_btn.SetToolTip(wx.ToolTip("Do the plot"))    
    hbox15.Add(plot_btn, flag = wx.ALL, border = 3)
    replot_btn = wx.Button(self, label = "REPLOT")
    replot_btn.Bind(wx.EVT_BUTTON, lambda event, mode = 'replot':self.DoPlot(event, mode))
    replot_btn.SetToolTip(wx.ToolTip("Refresh the last plot"))    
    hbox15.Add(replot_btn, flag = wx.ALL, border = 3)
    vbox.Add(hbox15, proportion = 1, flag = wx.ALIGN_RIGHT, border = 3)

    self.nplot = 0
    self.SetSizer(vbox)
    # show the frame
    #     self.vbox.Layout()
    #     self.Fit()
    self.Show(True)
  
  ##_____DEFINITIONS

  def btnf_click(self, event, place):
    """
    Create and show the Open FileDialog
    """
    dialog = wx.FileDialog(self, message = "Choose one file", 
    defaultFile = "", wildcard = '*', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
    if dialog.ShowModal()  ==  wx.ID_OK:
      fi =  dialog.GetPaths()[0]
      if len(fi)>0:
          xx = wx.FindWindowByName('%s'%place)
          xx.SetValue(fi)
    dialog.Destroy()
    
  def MakeHeader(self, pfile, gp):
      print('import sys', file = pfile)
      print('import numpy as np', file = pfile)
      print('from numpy import sin, cos, tan, arcsin, arccos, arctan, arctan2, exp, log, log10, sqrt, degrees, radians, pi, e, sign', file = pfile)
      #print('import matplotlib', file = pfile)
      #print("matplotlib.use('WX')" , file = pfile)
      print('import matplotlib.pyplot as plt', file = pfile)
      print('sys.path.append(r"%s")'%gp, file = pfile)
      print('import debfuncx ', file = pfile)
      print('###################################################', file = pfile)

  def ReadWrite(self, event, mode):
    xx = wx.FindWindowByName('file1')
    f1n = xx.GetValue().strip()
    xx = wx.FindWindowByName('file1_x')
    f1x = xx.GetValue().strip()
    xx = wx.FindWindowByName('file1_y')
    f1y = xx.GetValue().strip()
    xx = wx.FindWindowByName('file1_z')
    f1z = xx.GetValue().strip()
    xx = wx.FindWindowByName('file1_tra')
    f1t = xx.GetValue().strip()
    del xx
    case1 = (len(f1n)>0 and len(f1x)>0 and len(f1y)>0)
    case2 = (len(f1n)>0 and len(f1x)>0 and len(f1y) == 0 and len(f1z)>0)
    case3 = (len(f1n)>0 and len(f1x) == 0 and len(f1y) == 0 and len(f1z) == 0 and len(f1t)>0)
    case4 = (len(f1n)>0 and len(f1x) == 0 and len(f1y) == 0 and len(f1z) == 0 and len(f1t) == 0)
    if case1 or case2 or case3 or case4:
      #cstmplt = tempfile.NamedTemporaryFile(delete = False)
      cstmplt = open(os.path.join(gset.GUI_Path, "customplot.py"), 'w')
      self.MakeHeader(cstmplt, gset.GUI_Path)
      foty = 'serif'
      fosy = 11
      fw, fh = 8, 6
      xx = wx.FindWindowByName('fonttype')
      if len(xx.GetValue().strip())>0 : foty = xx.GetValue().strip()
      print('plt.rc("font", family = "%s")'%foty, file = cstmplt)
      xx = wx.FindWindowByName('fontsize')
      if len(xx.GetValue().strip())>0 : fosy = int(xx.GetValue().strip())
      print('plt.rc("font", size = %i)'%fosy, file = cstmplt) #font of title, xlabel, etc.
      print('plt.rc(("xtick", "ytick"), labelsize = %i)'%fosy, file = cstmplt) #sets fontsize of labels on xticks, yticks.
      print('plt.rc("legend", fontsize = %i)'%fosy, file = cstmplt)
      xx = wx.FindWindowByName('width')
      vx = xx.GetValue().strip()
      if len(vx)>0 : fw = float(vx)*2.54
      xx = wx.FindWindowByName('height')
      vx = xx.GetValue().strip()
      if len(vx)>0 : fh = float(vx)*2.54
      xx = wx.FindWindowByName('legend')
      vx = xx.GetValue().strip()
      if len(vx)>0 : lgdopt = vx
      print('######', file = cstmplt)
      if self.rb1.GetValue():
        if verbose: print('XY-plot')
        print('# XY-plot', file = cstmplt)
#         print('plt.ion()', file = cstmplt)
        if mode == 'plot' :
            print('fig = plt.figure(num = %i, figsize = (%2f, %2f))'%(self.nplot, fw, fh), file = cstmplt)
            print('ax = plt.subplot(111)', file = cstmplt)
        k = 0
        for i in range(1, self.nline):
            xx = wx.FindWindowByName('file%i'%i)
            fn = xx.GetValue().strip()
            xx = wx.FindWindowByName('file%i_x'%i)
            fx = xx.GetValue().strip()
            xx = wx.FindWindowByName('file%i_y'%i)
            fy = xx.GetValue().strip()
            xx = wx.FindWindowByName('file%i_z'%i)
            fz = xx.GetValue().strip()
            xx = wx.FindWindowByName('file%i_tra'%i)
            ft = xx.GetValue().strip()
            xx = wx.FindWindowByName('file%i_opt'%i)
            fo = xx.GetValue().strip()
            if len(fn)>0 and len(fx)>0 and len(fy)>0:
                if verbose: print('A ', i)
                k += 1
                print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                print('lines = rfln.readlines()', file = cstmplt)
                print('rfln.close()', file = cstmplt)
                print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                print('ncls = len(lline)', file = cstmplt)
                print('f%i = []'%(i), file = cstmplt)
                print('f%i += [0]'%(i), file = cstmplt)
                print('for c in range(ncls):', file = cstmplt)
                print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                print('x%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fx)-1), file = cstmplt)
                print('y%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fy)-1), file = cstmplt)
                pltstr = 'ax.plot(x%i, y%i)'%(i, i)
                ax2, lgd2 = False, False
                if len(fz)>0:
                  print('z%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fz)-1), file = cstmplt)
                  print('ax2 = ax.twinx()', file = cstmplt)
                  print('ax2.plot(x%i, z%i)'%(i, i), file = cstmplt)
                  ax2, lgd2 = True, True
                if len(ft)>0: pltstr = 'ax.plot(%s)'%(ft)
                if len(fo)>0: 
                    if 'bar' in fo:
                        i1 = fo.find('type')
                        i2 = fo.find('bar')
                        pltstr = pltstr.replace('ax.plot', 'ax.bar')
                        print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                        fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                    pltstr = pltstr[:-1]+', %s)'%(fo)
                print(pltstr, file = cstmplt)
            if len(fn)>0 and len(fx)>0 and len(fy) == 0 and len(fz)>0:
                if verbose: print('B ', i)
                k += 1
                print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                print('lines = rfln.readlines()', file = cstmplt)
                print('rfln.close()', file = cstmplt)
                print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                print('ncls = len(lline)', file = cstmplt)
                print('f%i = []'%(i), file = cstmplt)
                print('f%i += [0]'%(i), file = cstmplt)
                print('for c in range(ncls):', file = cstmplt)
                print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                print('x%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fx)-1), file = cstmplt)
                print('z%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fz)-1), file = cstmplt)
                print('ax2 = ax.twinx()', file = cstmplt)
                pltstr = 'ax2.plot(x%i, z%i)'%(i, i)
                if len(ft)>0 : pltstr = 'ax2.plot(%s)'%(ft)
                if len(fo)>0: 
                    if 'bar' in fo:
                        i1 = fo.find('type')
                        i2 = fo.find('bar')
                        pltstr = pltstr.replace('ax.plot', 'ax2.bar')
                        print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                        fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                    pltstr = pltstr[:-1]+', %s)'%(fo)
                print(pltstr, file = cstmplt)
                ax2, lgd2 = True, True
            if len(fn) == 0 and len(fx)>0:
                if verbose: print('C ', i)
                for j in range(i-1, 0, -1):
                    xx0 = wx.FindWindowByName('file%i'%(j))
                    fn = xx0.GetValue().strip()
                    if len(fn)>0:
                        break
                if len(fy)>0:
                    if verbose: print('Ca ', i)
                    k += 1
                    print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                    print('lines = rfln.readlines()', file = cstmplt)
                    print('rfln.close()', file = cstmplt)
                    print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                    print('ncls = len(lline)', file = cstmplt)
                    print('f%i = []'%(i), file = cstmplt)
                    print('f%i += [0]'%(i), file = cstmplt)
                    print('for c in range(ncls):', file = cstmplt)
                    print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                    print('x%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fx)-1), file = cstmplt)
                    print('y%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fy)-1), file = cstmplt)
                    pltstr = 'ax.plot(x%i, y%i)'%(i, i)
                    ax2, lgd2 = False, False
                    if len(fz)>0:
                        if verbose: print('Cb ', i)
                        print('z%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fz)-1), file = cstmplt)
                        print('ax2 = ax.twinx()', file = cstmplt)
                        print('ax2.plot(x%i, z%i)'%(i, i), file = cstmplt)
                        ax2, lgd2 = True, True
                    if len(ft)>0: pltstr = 'ax.plot(%s)'%(ft)
                    if len(fo)>0: 
                        if 'bar' in fo:
                            i1 = fo.find('type')
                            i2 = fo.find('bar')
                            pltstr = pltstr.replace('ax.plot', 'ax.bar')
                            print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                            fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                        pltstr = pltstr[:-1]+', %s)'%(fo)
                    print(pltstr, file = cstmplt)
                elif len(fy) == 0 and len(fz)>0:
                    if verbose: print('Cc ', i)
                    k += 1
                    print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                    print('lines = rfln.readlines()', file = cstmplt)
                    print('rfln.close()', file = cstmplt)
                    print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                    print('ncls = len(lline)', file = cstmplt)
                    print('f%i = []'%(i), file = cstmplt)
                    print('f%i += [0]'%(i), file = cstmplt)
                    print('for c in range(ncls):', file = cstmplt)
                    print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                    print('x%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fx)-1), file = cstmplt)
                    print('z%i = np.loadtxt(r"%s", usecols = ([%i]), unpack = True)'%(i, fn, int(fz)-1), file = cstmplt)
                    print('ax2 = ax.twinx()', file = cstmplt)
                    pltstr = 'ax2.plot(x%i, z%i)'%(i, i)
                    if len(ft)>0: pltstr = 'ax2.plot(%s)'%(ft)
                    if len(fo)>0: 
                        if 'bar' in fo:
                            i1 = fo.find('type')
                            i2 = fo.find('bar')
                            pltstr = pltstr.replace('ax.plot', 'ax2.bar')
                            print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                            fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                        pltstr = pltstr[:-1]+', %s)'%(fo)
                    print(pltstr, file = cstmplt)
                    if verbose: print(lgd2)
                    ax2, lgd2 = True, True
                    if verbose: print(lgd2)
            if len(fn)>0 and len(fx) == 0 and len(fy) == 0 and len(fz) == 0 and len(ft)>0:
                if verbose: print('D ', i)
                k += 1
                print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                print('lines = rfln.readlines()', file = cstmplt)
                print('rfln.close()', file = cstmplt)
                print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                print('ncls = len(lline)', file = cstmplt)
                print('f%i = []'%(i), file = cstmplt)
                print('f%i += [0]'%(i), file = cstmplt)
                print('for c in range(ncls):', file = cstmplt)
                print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                pltstr = 'ax.plot(%s)'%(ft)
                if len(fo)>0: 
                    if 'bar' in fo:
                        i1 = fo.find('type')
                        i2 = fo.find('bar')
                        pltstr = pltstr.replace('ax.plot', 'ax.bar')
                        print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                        fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                    pltstr = pltstr[:-1]+', %s)'%(fo)
                print(pltstr, file = cstmplt)
                ax2, lgd2 = False, False
            if len(fn) == 0  and len(fx) == 0 and len(fy) == 0 and len(fz) == 0 and len(ft)>0:
                if verbose: print('E ', i)
                for j in range(i-1, 0, -1):
                    xx0 = wx.FindWindowByName('file%i'%(j))
                    fn = xx0.GetValue().strip()
                    if len(fn)>0:
                        break
                k += 1
                print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                print('lines = rfln.readlines()', file = cstmplt)
                print('rfln.close()', file = cstmplt)
                print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                print('ncls = len(lline)', file = cstmplt)
                print('f%i = []'%(i), file = cstmplt)
                print('f%i += [0]'%(i), file = cstmplt)
                print('for c in range(ncls):', file = cstmplt)
                print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                ax2, lgd2 = False, False
                pltstr = 'ax.plot(%s)'%(ft)
                if len(fo)>0: 
                    if 'bar' in fo:
                        i1 = fo.find('type')
                        i2 = fo.find('bar')
                        pltstr = pltstr.replace('ax.plot', 'ax.bar')
                        print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                        fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                    pltstr = pltstr[:-1]+', %s)'%(fo)
                print(pltstr)
            if (len(fn) > 0 and len(fx) == 0 and len(fy) == 0 and len(fz) == 0 and len(ft) == 0):
                if verbose: print('F ', i)
                k += 1
                print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
                print('lines = rfln.readlines()', file = cstmplt)
                print('rfln.close()', file = cstmplt)
                print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
                print('ncls = len(lline)', file = cstmplt)
                print('f%i = []'%(i), file = cstmplt)
                print('f%i += [0]'%(i), file = cstmplt)
                print('for c in range(ncls):', file = cstmplt)
                print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
                pltstr = 'ax.plot(f%i[1], f%i[2])'%(i, i)
                if len(fo)>0: 
                    if 'bar' in fo:
                        i1 = fo.find('type')
                        i2 = fo.find('bar')
                        pltstr = pltstr.replace('ax.plot', 'ax.bar')
                        print('w = list(np.diff(x%i)/2)+[(np.diff(x%i)[-1])/2]'%(i, i), file = cstmplt)
                        fo = fo.replace(fo[i1:i2+4], "align = 'center', width = w")
                    pltstr = pltstr[:-1]+', %s)'%(fo)
                print(pltstr, file = cstmplt)
                ax2, lgd2 = False, False
        xx = wx.FindWindowByName('range')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
            xmin, xmax = float(vx.split()[0]), float(vx.split()[1])
            print('ax.set_xlim(%f, %f)'%(xmin, xmax), file = cstmplt)
        xx = wx.FindWindowByName('yrange')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          ymin, ymax = float(vx.split()[0]), float(vx.split()[1])
          print('ax.set_ylim(%s, %s)'%(ymin, ymax), file = cstmplt)
        xx = wx.FindWindowByName('zrange')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          zmin, zmax = float(vx.split()[0]), float(vx.split()[1])
          print('ax2.set_ylim(%s, %s)'%(zmin, zmax), file = cstmplt)
        xx = wx.FindWindowByName('xlabel')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          xl = vx
          print('ax.set_xlabel(r"%s")'%xl, file = cstmplt)
        xx = wx.FindWindowByName('ylabel')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          yl = vx
          print('ax.set_ylabel(r"%s")'%yl, file = cstmplt)
        xx = wx.FindWindowByName('zlabel')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          zl = vx
          print('ax2.set_ylabel(r"%s")'%zl, file = cstmplt)
        if self.cbxaxt.GetValue() == False:
          print("ax.spines['top'].set_visible(False)", file = cstmplt)
          print("ax.xaxis.tick_bottom()", file = cstmplt)
        if self.cbxaxb.GetValue() == False:
          print("ax.spines['bottom'].set_visible(False)", file = cstmplt)
          print("ax.xaxis.tick_top()", file = cstmplt)
        if self.cbyaxl.GetValue() == False:
          print("ax.spines['left'].set_visible(False)", file = cstmplt)
          print("ax.yaxis.tick_right()", file = cstmplt)
        if self.cbyaxr.GetValue() == False:
          print("ax.spines['right'].set_visible(False)", file = cstmplt)
          print("ax.yaxis.tick_left()", file = cstmplt)
        if self.cbxtc.GetValue() == False:
          print('ax.set_xticks([])', file = cstmplt)
        if self.cbxtlbl.GetValue() == False:
          print('ax.set_xticklabels([])', file = cstmplt)
        if self.cbytc.GetValue() == False:
          print('ax.set_yticks([])', file = cstmplt)
        if self.cbytlbl.GetValue() == False:
          print('ax.set_yticklabels([])', file = cstmplt)
        if ax2:
            if self.cbztc.GetValue() == False:
              print('ax2.set_yticks([])', file = cstmplt)
            if self.cbztlbl.GetValue() == False:
              print('ax2.set_yticklabels([])', file = cstmplt)
        xx = wx.FindWindowByName('title')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          tl = vx
          print('ax.set_title(r"%s", fontsize = %i)'%(tl, fosy), file = cstmplt)
        print('handles, labels = ax.get_legend_handles_labels()', file = cstmplt)
        print('if len(labels)>0:', file = cstmplt)
        print('    ax.legend(loc = 0)', file = cstmplt)
        xx = wx.FindWindowByName('legend')
        vx = xx.GetValue().strip()
        if len(vx)>0 :
          print('    ax.legend(%s)'%(lgdopt), file = cstmplt)
        if verbose : print(lgd2)
        if lgd2 : print('ax2.legend(loc = 7)', file = cstmplt)
        if (self.cbytc.GetValue() and self.cbytlbl.GetValue()):
          print('plt.ticklabel_format(axis = "y", style = "sci", scilimits = (-1, 4))', file = cstmplt)
        print('plt.show()', file = cstmplt)
#         if mode == 'replot' : print('plt.draw()'
      
      elif self.rb2.GetValue():
        print('# 2D-map', file = cstmplt)
        k = 0
        fo = ''
        for i in range(1, self.nline):
          xx = wx.FindWindowByName('file%i'%i)
          fn = xx.GetValue().strip()
          xx = wx.FindWindowByName('file%i_x'%i)
          fx = xx.GetValue().strip()
          xx = wx.FindWindowByName('file%i_y'%i)
          fy = xx.GetValue().strip()
          xx = wx.FindWindowByName('file%i_z'%i)
          fz = xx.GetValue().strip()
          xx = wx.FindWindowByName('file%i_tra'%i)
          ft = xx.GetValue().strip()
          xx = wx.FindWindowByName('file%i_opt'%i)
          if len(xx.GetValue().strip()) > 0: fo = xx.GetValue().strip()
          if (len(fn)>0 or len(ft)>0):
            k += 1
            if len(fn) == 0:
                for j in range(i-1, 0, -1):
                    xx0 = wx.FindWindowByName('file%i'%(j))
                    fn = xx0.GetValue().strip()
                    if len(fn)>0:
                        break
            print('##__file %i'%i, file = cstmplt)
            print('rfln = open(r"%s", "r")'%(fn), file = cstmplt)
            print('lines = rfln.readlines()', file = cstmplt)
            print('rfln.close()', file = cstmplt)
            print('lline = lines[int(len(lines)/2)].split()', file = cstmplt)
            print('ncls = len(lline)', file = cstmplt)
            print('f%i = []'%(i), file = cstmplt)
            print('f%i += [0]'%(i), file = cstmplt)
            print('for c in range(ncls):', file = cstmplt)
            print('  f%i += [np.loadtxt(r"%s", usecols = ([c]), unpack = True)]'%(i, fn), file = cstmplt)
            print('si, si1, si2 = debfuncx.indmtx(f1[1])', file = cstmplt)
            print('for nc in range(1, ncls+1):', file = cstmplt)
            print('  f%i[nc] = debfuncx.srtmtx(f%i[nc], si, si1, si2)'%(i, i), file = cstmplt)
            if len(fx)>0: xcl, ycl, zcl = 'f%i[%s]'%(i, fx), 'f%i[%s]'%(i, fy), 'f%i[%s]'%(i, fz)
            if len(ft)>0:
              transfstr = ft.split(',')
              xcl, ycl, zcl = transfstr[0].strip(), transfstr[1].strip(), transfstr[2].strip()
            print('##__', file = cstmplt)
        ##
        xmin, xmax = 0.0, 0.0
        ymin, ymax = 0.0, 0.0
        zmin, zmax = 0.0, 0.0
        thrco = 'zcl'
        thres = 0.0
        xl, yl, tl = '', '', ''
        xx = wx.FindWindowByName('range')
        vx = xx.GetValue().strip()
        if len(vx)>0 : xmin, xmax = float(vx.split()[0]), float(vx.split()[1])
        xx = wx.FindWindowByName('yrange')
        vx = xx.GetValue().strip()
        if len(vx)>0 : ymin, ymax = float(vx.split()[0]), float(vx.split()[1])
        xx = wx.FindWindowByName('zrange')
        vx = xx.GetValue().strip()
        if len(vx)>0 : zmin, zmax = float(vx.split()[0]), float(vx.split()[1])
        xx = wx.FindWindowByName('threshold')
        vx = xx.GetValue().strip()
        if len(vx)>0 : thrco, thres = (vx.split()[0]), float(vx.split()[1])
        xx = wx.FindWindowByName('xlabel')
        vx = xx.GetValue().strip()
        if len(vx)>0 : xl = vx
        xx = wx.FindWindowByName('ylabel')
        vx = xx.GetValue().strip()
        if len(vx)>0 : yl = vx
        xx = wx.FindWindowByName('title')
        vx = xx.GetValue().strip()
        if len(vx)>0 : tl = vx
        print('xcl, ycl, zcl = %s, %s, %s'%(xcl, ycl, zcl), file = cstmplt)
        print('xra = [%f, %f]'%(xmin, xmax), file = cstmplt)
        print('yra = [%f, %f]'%(ymin, ymax), file = cstmplt)
        print('zra = [%f, %f]'%(zmin, zmax), file = cstmplt)
        print('thrco = %s'%thrco, file = cstmplt)
        print('thres = %f'%thres, file = cstmplt)
        print('mapopt = "%s"'%fo, file = cstmplt)
        print('xlbl = "%s"'%xl, file = cstmplt)
        print('ylbl = "%s"'%yl, file = cstmplt)
        print('titl = "%s"'%tl, file = cstmplt)
        print('fwdt = %f'%fw, file = cstmplt)
        print('fhig = %f'%fh, file = cstmplt)
        print('ftyp = "%s"'%foty, file = cstmplt)
        print('fsiz = %i'%fosy, file = cstmplt)
        ##
        print('eps=1.e-8', file = cstmplt)
        print('ii,i1=[],[]', file = cstmplt)
        print('if (abs(thres-0.)>eps):', file = cstmplt)
        print('  if verbose : print("mapping with threshold at %f)"'%thres, file = cstmplt)
        print('  ii=np.argwhere(thrco-thres<eps).flatten()', file = cstmplt)
        print('  i1=np.delete(np.arange(len(zcl)),ii)', file = cstmplt)
        print('  if len(i1)==0:', file = cstmplt)
        print('    print("THRESHOLD TOO LOW, NOTHING TO PLOT!)"', file = cstmplt)
        print('    exit()', file = cstmplt)
        print('  elif len(i1)>0:', file = cstmplt)
        print('    zcl[ii]=-1', file = cstmplt)
        print('    zcl1=zcl[i1]', file = cstmplt)
        print('    cmap=plt.cm.Spectral_r.set_under("w")', file = cstmplt)
        print('    vmi,vma=min(zcl1),max(zcl1)', file = cstmplt)
        print('else:', file = cstmplt)
        print('  cmap=plt.cm.Spectral_r', file = cstmplt)
        print('  vmi,vma=min(zcl),max(zcl)', file = cstmplt)
        print('if zra[1]>0:', file = cstmplt)
        print('  vmi,vma=zra[0],zra[1]', file = cstmplt)
        print('if xra[1]>0:', file = cstmplt)
        print('  xmin,xmax=xra[0],xra[1]', file = cstmplt)
        print('else:', file = cstmplt)
        print('  xmin,xmax=0,max(xcl)', file = cstmplt)
        print('if yra[1]>0:', file = cstmplt)
        print('  ymin,ymax=yra[0],yra[1]', file = cstmplt)
        print('else:', file = cstmplt)
        print('  ymin,ymax=0,max(ycl)', file = cstmplt)
        print('npy=np.argwhere(xcl==max(xcl)).flatten()[1]', file = cstmplt)
        print('npx=len(np.argwhere(xcl==max(xcl)).flatten())', file = cstmplt)
        print('img=np.reshape(zcl[::-1],(npx,npy))', file = cstmplt)
        print('plt.figure(figsize=(fwdt,fhig))', file = cstmplt)
        print('plt.subplots_adjust(wspace=0.2)', file = cstmplt)
        print('#ax=plt.subplot(111)', file = cstmplt)
        print('ax=plt.axes([0.125,0.15,0.95-0.25,0.95-0.2])', file = cstmplt)
        print('map = ax.imshow(img, cmap = cmap, extent = [xmin, xmax, ymin, ymax],\n\
         vmin = vmi, vmax = vma, %s)'%fo, file = cstmplt)
        print('plt.colorbar(map,format="%f")', file = cstmplt)
        print('plt.xlabel(r"%s"%xlbl)', file = cstmplt)
        print('plt.ylabel(r"%s"%ylbl)', file = cstmplt)
        print('plt.title(titl,fontsize=fsiz)', file = cstmplt)
        print('plt.show()', file = cstmplt)
      cstmplt.close()
    else: print('CUSTOM PLOTTING : NOTHING TO DO')
    return cstmplt.name

  def DoPlot(self, event, mode):
      if (self.nplot == 0 and mode == 'replot') : print("CUSTOM PLOTTING : NOTHING TO REPLOT")
      if (self.nplot >= 1 and mode == 'replot') : self.proc.terminate()
      if mode == 'plot' : self.nplot += 1
      plotfile = self.ReadWrite(event, 'plot')
      if gset.Platform[:3].lower() == 'lin': 
          cmd = ['python3', plotfile]
      elif gset.Platform[:3].lower() == 'dar':
      	  cmd = ['pythonw', plotfile]
      elif gset.Platform[:3].lower() == 'win':
          cmd = ['python', plotfile]	   
#      cmd = ['pythonw', plotfile]
      self.proc = subprocess.Popen(cmd, stdin = subprocess.PIPE, stderr = subprocess.STDOUT)
#       else:
#           self.proc.communicate("execfile('%s')"%plotfile)

  def SaveInp(self, event):
      spt = '||'
      dialog = wx.FileDialog(
          self, message = "Save file ", defaultDir = os.getcwd(), 
          defaultFile = "customplot.inp", wildcard = '*.inp', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
          )
      if dialog.ShowModal() == wx.ID_OK:
          fout= dialog.GetPath()
          outf = open(fout, 'w')
          if self.rb1.GetValue():
              print('TYPE    XY-plot', file = outf)
          elif self.rb2.GetValue():
              print('TYPE    2D-map', file = outf)
          for i in range(1, self.nline):
              xx = wx.FindWindowByName('file%i'%i)
              fn = xx.GetValue().strip()
              if len(fn) == 0: fn = '  '
              xx = wx.FindWindowByName('file%i_x'%i)
              fx = xx.GetValue().strip()
              if len(fx) == 0: fx = '  '
              xx = wx.FindWindowByName('file%i_y'%i)
              fy = xx.GetValue().strip()
              if len(fy) == 0: fy = '  '
              xx = wx.FindWindowByName('file%i_z'%i)
              fz = xx.GetValue().strip()
              if len(fz) == 0: fz = '  '
              xx = wx.FindWindowByName('file%i_tra'%i)
              ft = xx.GetValue().strip()
              if len(ft) == 0: ft = '  '
              xx = wx.FindWindowByName('file%i_opt'%i)
              fo = xx.GetValue().strip()
              if len(fo) == 0: fo = '  '
              print('FILE    ',fn, spt, fx,spt, fy, spt, fz, spt, ft, spt, fo, file = outf)
          xx = wx.FindWindowByName('range')
          vx = xx.GetValue().strip()
          print('XRANGE    ',vx)
          xx = wx.FindWindowByName('yrange')
          vx = xx.GetValue().strip()
          print('YRANGE    ',vx)
          xx = wx.FindWindowByName('zrange')
          vx = xx.GetValue().strip()
          print('ZRANGE    ',vx)
          xx = wx.FindWindowByName('threshold')
          vx = xx.GetValue().strip()
          print('THRESH    ',vx, file = outf)
          xx = wx.FindWindowByName('xlabel')
          vx = xx.GetValue().strip()
          print('XLABEL    ',vx, file = outf)
          xx = wx.FindWindowByName('ylabel')
          vx = xx.GetValue().strip()
          print('YLABEL    ',vx, file = outf)
          xx = wx.FindWindowByName('zlabel')
          vx = xx.GetValue().strip()
          print('ZLABEL    ',vx, file = outf)
          vx = self.cbxaxt.GetValue()
          print('XAXT    ',vx, file = outf)
          vx = self.cbxaxb.GetValue()
          print('XAXB    ',vx, file = outf)
          vx = self.cbyaxl.GetValue()
          print('YAXL    ',vx, file = outf)
          vx = self.cbyaxr.GetValue()
          print('YAXR    ',vx, file = outf)
          vx = self.cbxtc.GetValue()
          print('XTC    ',vx, file = outf)
          vx = self.cbxtlbl.GetValue()
          print('XTLBL    ',vx, file = outf)
          vx = self.cbytc.GetValue()
          print('YTC    ',vx, file = outf)
          vx = self.cbytlbl.GetValue()
          print('YTLBL    ',vx, file = outf)
          vx = self.cbztc.GetValue()
          print('ZTC    ',vx, file = outf)
          vx = self.cbztlbl.GetValue()
          print('ZTLBL    ',vx, file = outf)
          xx = wx.FindWindowByName('legend')
          vx = xx.GetValue().strip()
          print('LEGEND    ',vx, file = outf)
          xx = wx.FindWindowByName('title')
          vx = xx.GetValue().strip()
          print('TITLE    ',vx, file = outf)
          xx = wx.FindWindowByName('width')
          vx = xx.GetValue().strip()
          print('WIDTH    ',vx, file = outf)
          xx = wx.FindWindowByName('height')
          vx = xx.GetValue().strip()
          print('HEIGHT    ',vx, file = outf)
          xx = wx.FindWindowByName('fonttype')
          vx = xx.GetValue().strip()
          print('FTYPE    ',vx, file = outf)
          xx = wx.FindWindowByName('fontsize')
          vx = xx.GetValue().strip()
          print('FSIZE    ',vx, file = outf)
          outf.close()
      else:
          _userCancel = dialog.Destroy()
          return _userCancel
          dialog.Destroy()

  
  def LoadInp(self, event):
      dialog = wx.FileDialog(self, message = "Choose one .inp file", 
      defaultFile = "customplt.inp", wildcard = '*.inp', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
      fin = ''
      if dialog.ShowModal() == wx.ID_OK:
        fin = dialog.GetPath()
      dialog.Destroy()
      if len(fin) > 0:
          infile = open(fin, 'r')
          lines = infile.readlines()
          infile.close()
          for l in range(len(lines)):
              rline = lines[l].partition(' ')
              if rline[0] == 'TYPE':
                  if  'XY-plot' in rline[-1]: self.rb1.SetValue(True)
                  elif '2D-map' in rline[-1]: self.rb2.SetValue(True)
              if rline[0] == 'FILE':
                  vxs = rline[2].split('||')
                  xx = wx.FindWindowByName('file%i'%l)
                  fn = xx.SetValue(vxs[0].strip())
                  xx = wx.FindWindowByName('file%i_x'%l)
                  fx = xx.SetValue(vxs[1].strip())
                  xx = wx.FindWindowByName('file%i_y'%l)
                  fy = xx.SetValue(vxs[2].strip())
                  xx = wx.FindWindowByName('file%i_z'%l)
                  fz = xx.SetValue(vxs[3].strip())
                  xx = wx.FindWindowByName('file%i_tra'%l)
                  ft = xx.SetValue(vxs[4].strip())
                  xx = wx.FindWindowByName('file%i_opt'%l)
                  fo = xx.SetValue(vxs[5].strip())
              if rline[0] == 'XRANGE':
                  xx = wx.FindWindowByName('range')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'YRANGE':
                  xx = wx.FindWindowByName('yrange')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'ZRANGE':
                  xx = wx.FindWindowByName('zrange')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'THRESH':
                  xx = wx.FindWindowByName('threshold')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'XLABEL':
                  xx = wx.FindWindowByName('xlabel')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'YLABEL':
                  xx = wx.FindWindowByName('ylabel')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'ZLABEL':
                  xx = wx.FindWindowByName('zlabel')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'XAXT':
                  if rline[2] == 'True': vx = self.cbxaxt.SetValue(True)
                  if rline[2] == 'False': vx = self.cbxaxt.SetValue(False)
              if rline[0] == 'XAXB':
                  if rline[2] == 'True': vx = self.cbxaxb.SetValue(True)
                  if rline[2] == 'False': vx = self.cbxaxb.SetValue(False)
              if rline[0] == 'YAXL':
                  if rline[2] == 'True': vx = self.cbyaxl.SetValue(True)
                  if rline[2] == 'False': vx = self.cbyaxl.SetValue(False)
              if rline[0] == 'YAXR':
                  if rline[2] == 'True': vx = self.cbyaxr.SetValue(True)
                  if rline[2] == 'False': vx = self.cbyaxr.SetValue(False)
              if rline[0] == 'XTC':
                  if rline[2] == 'True': vx = self.cbxtc.SetValue(True)
                  if rline[2] == 'False': vx = self.cbxtc.SetValue(False)
              if rline[0] == 'XTLBL':
                  if rline[2] == 'True': vx = self.cbxtlbl.SetValue(True)
                  if rline[2] == 'False': vx = self.cbxtlbl.SetValue(False)
              if rline[0] == 'YTC':
                  if rline[2] == 'True': vx = self.cbytc.SetValue(True)
                  if rline[2] == 'False': vx = self.cbytc.SetValue(False)
              if rline[0] == 'YTLBL':
                  if rline[2] == 'True': vx = self.cbytlbl.SetValue(True)
                  if rline[2] == 'False': vx = self.cbytlbl.SetValue(False)
              if rline[0] == 'ZTC':
                  if rline[2] == 'True': vx = self.cbztc.SetValue(True)
                  if rline[2] == 'False': vx = self.cbztc.SetValue(False)
              if rline[0] == 'ZTLBL':
                  if rline[2] == 'True': vx = self.cbztlbl.SetValue(True)
                  if rline[2] == 'False': vx = self.cbztlbl.SetValue(False)
              if rline[0] == 'LEGEND':
                  xx = wx.FindWindowByName('legend')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'TITLE':
                  xx = wx.FindWindowByName('title')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'WIDTH':
                  xx = wx.FindWindowByName('width')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'HEIGHT':
                  xx = wx.FindWindowByName('height')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'FTYPE':
                  xx = wx.FindWindowByName('fonttype')
                  vx = xx.SetValue(rline[2].strip())
              if rline[0] == 'FSIZE':
                  xx = wx.FindWindowByName('fontsize')
                  vx = xx.SetValue(rline[2].strip())
      else:
          _userCancel = dialog.Destroy()
          return _userCancel
          dialog.Destroy()

  def ClearAll(self, event):
      self.rb1.SetValue(True)
      for i in range(1, self.nline):
          xx = wx.FindWindowByName('file%i'%i)
          fn = xx.Clear()
          xx = wx.FindWindowByName('file%i_x'%i)
          fx = xx.Clear()
          xx = wx.FindWindowByName('file%i_y'%i)
          fy = xx.Clear()
          xx = wx.FindWindowByName('file%i_z'%i)
          fz = xx.Clear()
          xx = wx.FindWindowByName('file%i_tra'%i)
          ft = xx.Clear()
          xx = wx.FindWindowByName('file%i_opt'%i)
          fo = xx.Clear()
          xx = wx.FindWindowByName('range')
          vx = xx.Clear()
          xx = wx.FindWindowByName('yrange')
          vx = xx.Clear()
          xx = wx.FindWindowByName('zrange')
          vx = xx.Clear()
          xx = wx.FindWindowByName('threshold')
          vx = xx.Clear()
          xx = wx.FindWindowByName('xlabel')
          vx = xx.Clear()
          xx = wx.FindWindowByName('ylabel')
          vx = xx.Clear()
          xx = wx.FindWindowByName('zlabel')
          vx = xx.Clear()
      self.cbxaxt.SetValue(True)
      self.cbxaxb.SetValue(True)
      self.cbyaxl.SetValue(True)
      self.cbyaxr.SetValue(True)
      self.cbxtc.SetValue(True)
      self.cbxtlbl.SetValue(True)
      self.cbytc.SetValue(True)
      self.cbytlbl.SetValue(True)
      self.cbztc.SetValue(True)
      self.cbztlbl.SetValue(True)
      xx = wx.FindWindowByName('legend')
      vx = xx.Clear()
      xx = wx.FindWindowByName('title')
      vx = xx.Clear()
      xx = wx.FindWindowByName('fonttype')
      vx = xx.Clear()
      xx = wx.FindWindowByName('fontsize')
      vx = xx.Clear()
      xx = wx.FindWindowByName('width')
      vx = xx.Clear()
      xx = wx.FindWindowByName('height')
      vx = xx.Clear()
  
  def OnHelp(self, help_btn):
        """
        Launch the Help window
        """
        msg = hlptxt
        
        dialog = wx.lib.dialogs.ScrolledMessageDialog(self, msg, "custom plotting help", size = (600, 400))
        dialog.ShowModal()
 
        dialog.Destroy()


  ### CLOSE 
  def OnClose(self, event):
#     dialog = wx.MessageDialog(self, "Close?", 
#     "Confirm Exit", wx.OK|wx.CANCEL|wx.ICON_QUESTION)
#     result = dialog.ShowModal()
#     dialog.Destroy()
#     if result  ==  wx.ID_OK:
      self.Destroy()
      

# application = wx.App()
# # call class MyFrame
# window = MainWindow(None)
# # start the event loop
# application.MainLoop()
#----------------------------------------------------------------------
if __name__  ==  "__main__":
    app = wx.App(False)
    frame = CustomPlotter()
    app.MainLoop()
