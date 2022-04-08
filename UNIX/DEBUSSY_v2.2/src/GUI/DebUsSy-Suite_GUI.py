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

# from sys import excepthook as sys_excepthook
# from sys import path as sys_path
# from sys import modules as sys_modules
# from sys import argv as sys_argv
# 
# from os import getpid as os_getpid
# from os import path as os_path
# from os import chdir
# from os import system
# 
# from threading import Thread as threading_Thread
# 
# import subprocess
# 
# from numpy import loadtxt, diff, reshape, degrees, radians, sin, cos, asarray, arange, amin, amax, random, all, sqrt, zeros, ones, argwhere, 


import os
import sys
import time
import logging
import traceback
import wx
import wx.lib.dialogs
import wx.lib.scrolledpanel as scrolled
import wx.lib.agw.genericmessagedialog as GMD
import threading
import subprocess
from debfuncx import missing#, ExceptionDialog
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
from setw import SettingsWindow
from cled4 import ClaudeGUI
from debed4 import DebussyGUI
##########################################################################################
gui_pid = os.getpid()
aboutText = '- DebUsSy - \n  A. Cervellino R. Frison F. Bertolotti and A. Guagliardi \
\n Journal of Applied Crystallography\n 48, 2026-2032, 2015'
tmpText = '.. WORK IN PROGRESS! -- COMING SOON ...'
manuals = ('-Select a Manual-','Claude Manual','Debussy Manual')
# xterm_opt_0 = 'xterm -font -*-helvetica-medium-r-normal-*-11-*-*-*-*-*-*-* -geometry 80x30'.split()
xterm_opt = 'xterm -geometry 120x30'.split()
xterm_opt0 = 'xterm -T TESTING -e tail -f PIPE_PATH'.split()
wtm = 86400
wtms = 5

if gset.PGM_Path[-1] != gv.SEP : gset.PGM_Path = gset.PGM_Path+gv.SEP
if gset.GUI_Path[-1] != gv.SEP : gset.GUI_Path = gset.PGM_Path+gv.SEP
########################################################################
class ExceptionLogging(object):
 
    #----------------------------------------------------------------------
    def __init__(self, fn):
        self.fn = fn
 
        # create logging instance
        self.log = logging.getLogger("DebussySuite")
        self.log.setLevel(logging.INFO)
 
        # create a logging file handler / formatter
        log_fh = logging.FileHandler(gset.DEB_Path+"error.log")
        formatter = logging.Formatter("%(asctime)s - %(name)s - %(message)s")
        log_fh.setFormatter(formatter)
        self.log.addHandler(log_fh)
 
    #----------------------------------------------------------------------
    def __call__(self,evt):
        try:
            self.fn(self, evt)
        except Exception as e:
            self.log.exception("Exception")

##----------------------------------------------------------------------------------------
class ExceptionDialog(GMD.GenericMessageDialog):
    """"""
 
    #----------------------------------------------------------------------
    def __init__(self, msg):
        """Constructor"""
        GMD.GenericMessageDialog.__init__(self, None, msg, "Exception!",
                                          wx.OK|wx.ICON_ERROR)
        if gset.Platform.startswith('dar'):
            font = wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)

#----------------------------------------------------------------------
def ExceptionHook1(etype, value, trace):
    """
    Handler for all unhandled exceptions.
 
    :param `etype`: the exception type (`SyntaxError`, `ZeroDivisionError`, etc...);
    :type `etype`: `Exception`
    :param string `value`: the exception error message;
    :param string `trace`: the traceback header, if any (otherwise, it prints the
     standard Python header: ``Traceback (most recent call last)``.
    """
    efile=gset.DEB_Path+"DebussySuite_ERR.log"
    itxt="\n  Traceback message appended to the %s file"%efile
    frame = wx.GetApp().GetTopWindow()
    tmp = traceback.format_exception(etype, value, trace)
    exception = "".join(tmp)
    
    with open(efile, "a") as elog:
        elog.write('\n%s\n'%('#'+90*'*'))
        elog.write(time.asctime()+'\n')
        elog.write(exception)

    dlg = ExceptionDialog(exception+itxt)
    dlg.ShowModal()
    dlg.Destroy()

##########################################################################################
# @ExceptionLogging
class PanelX(scrolled.ScrolledPanel):
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """Constructor"""
        wx.Panel.__init__(self, parent=parent,size=(850,10))
        
        notebook = wx.Notebook(self)
        Ctab = ClaudeGUI(notebook)
        notebook.AddPage(Ctab, "C La U De")
 
        Dtab = DebussyGUI(notebook)
        notebook.AddPage(Dtab, "DEBUSSY")

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(notebook, 1, wx.ALL|wx.EXPAND, 5)

        self.SetSizer(sizer)
        self.Layout()
 
########################################################################
class DebussySuite(wx.Frame):
    """
    Main window frame 
    """
 
    #----------------------------------------------------------------------
    def __init__(self):
        """Constructor"""        
        ds = wx.GetDisplaySize()
        wx.Frame.__init__(self, None, wx.ID_ANY, 
                          "DebUsSy Suite GUI",
                          size=(900,700),pos=(-1, 20))
                          #size=(ds[0]*0.5,ds[1]*0.5))
        self.Bind(wx.EVT_CLOSE, self.OnClose)
        if gset.Platform.startswith('dar'):
            font = wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)
        
        print(ds)

        sys.excepthook = ExceptionHook1
#         menuBar = wx.MenuBar()
#         menu = wx.Menu()
#         m_exit = menu.Append(wx.ID_EXIT, "E&xit\tAlt-X", "Close window and exit program.")
#         self.Bind(wx.EVT_MENU, self.OnClose, m_exit)
#         menuBar.Append(menu, "&File")
#         menu = wx.Menu()
#         m_about = menu.Append(wx.ID_ABOUT, "&About", "Information about this program")
#         self.Bind(wx.EVT_MENU, self.OnAbout, m_about)
#         menuBar.Append(menu, "&Help")
#         self.SetMenuBar(menuBar)

        menubar = wx.MenuBar()
        DebMenu = wx.Menu()
        about_item = DebMenu.Append(wx.ID_ANY, '&About DebUsSy-Suite ...')
        self.Bind(wx.EVT_MENU, self.OnAbout, about_item)
        set_item = DebMenu.Append(wx.ID_ANY, '&Settings ...', 'Customise the application')
        self.Bind(wx.EVT_MENU, self.prefs_click, set_item)
        close_item = DebMenu.Append(wx.ID_ANY, '&Close ...', 'Closes the window')
        self.Bind(wx.EVT_MENU, self.OnClose, close_item)
        menubar.Append(DebMenu, 'DebUsSy-Suite GUI')

        HelpMenu = wx.Menu()
        CManu_item = HelpMenu.Append(wx.ID_ANY, '&Claude Manual ...', 'See the Claude Manual')
        self.Bind(wx.EVT_MENU,lambda event, imanu=1:\
           self.mans_select(event, imanu), CManu_item)
        DManu_item = HelpMenu.Append(wx.ID_ANY, '&Debussy Manual ...', 'See the Debussy Manual')
        self.Bind(wx.EVT_MENU, lambda event, imanu=2:\
           self.mans_select(event, imanu), DManu_item)
        menubar.Append(HelpMenu, 'Help')

        self.SetMenuBar(menubar)

        notebook = wx.Notebook(self)
        Ctab = ClaudeGUI(notebook)
        notebook.AddPage(Ctab, "CLAUDE")

        Dtab = DebussyGUI(notebook)
        notebook.AddPage(Dtab, "DEBUSSY")


        #--- Creating a statusbar 
        self.CreateStatusBar(name='status')

        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(notebook, 1, wx.ALL|wx.EXPAND, 5)
        self.SetSizer(sizer)
        self.Layout()
        self.Show(True)
    #----------------------------------------------------------------------
    ####____SETTINGS
    def prefs_click(self,event):
        self.settings_frame = SettingsWindow()
        self.settings_frame.Show()

    ### ABOUT
    def OnAbout(self, event):
        dlg = wx.MessageDialog(self,aboutText,"About DebUsSy-Suite", wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

    ####____MANUALS
    def mans_select(self, event,imanu):
      if imanu>0:
            manu=manuals[imanu].partition(' ')[0]
            self.path_manu = gset.DEB_Path+'MANUALS'+gv.SEP+manu+'_v'+gset.DEB_Version+'_Manual.pdf'
            self.manual = threading.Thread(target = self.run_man)
            self.manual.start()
    def run_man(self):
        if gset.Platform.startswith('lin'):
            man=['xdg-open', self.path_manu]  ## PLATFORM SPECIFIC !!
        elif gset.Platform.startswith('dar'):
            man = ['open', self.path_manu]
        elif gset.Platform.startswith('win'):
            man = [self.path_manu]
        manual_process = subprocess.Popen(man)

    ### CLOSE 
    def OnClose(self, event):
        dlg = wx.MessageDialog(self,"Close DebUsSy-Suite GUI?",
          "Confirm Exit", wx.OK|wx.CANCEL|wx.ICON_QUESTION)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            self.Destroy()

#----------------------------------------------------------------------
if __name__ == "__main__":
    app = wx.App()#redirect=True,filename=None)
    app.SetAppName('DebussySuite')
    frame = DebussySuite()
    app.MainLoop()
