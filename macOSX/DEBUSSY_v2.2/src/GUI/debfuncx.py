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
import sys
import os.path
import time
import glob
# import psutil
import numpy as np
from numpy import sin,cos,tan,arcsin,arccos,arctan,arctan2,exp,log,log10,sqrt,degrees,radians,pi,e
import wx
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
# import wx.lib.agw.genericmessagedialog as GMD
##########################################################################################

def get_sep(plt = 'linux'):
    if plt[:3].lower() == 'lin':
        sep='/'
    elif plt[:3].lower() == 'dar':
        sep='/'
    elif plt[:3].lower() == 'win':
        sep='\\'
    return sep
sep = get_sep(gset.Platform)
gv.SEP = sep
##----------------------------------------------------------------
def SetFolder(self, event):
    global wpath, _userCancel
    dialog =  wx.DirDialog(self, "Please choose your project directory:", \
    defaultPath = os.getcwd(), style = wx.DD_CHANGE_DIR , pos = (10, 10))
    if dialog.ShowModal()  ==  wx.ID_OK:
        wpath = dialog.GetPath()
        #print(dialog.GetPath())
        xx = wx.FindWindowByName('status')
        datax = xx.GetStatusText()
        xx.SetStatusText('working path: %s'%(wpath))
        os.chdir(wpath)
        return wpath
    else:
        _userCancel = dialog.Destroy()
        return _userCancel
        dialog.Destroy() 

def SetPath(self, infile):
    fpath = os.path.abspath(infile)
    wpath = os.path.dirname(fpath)
    xx = wx.FindWindowByName('status')
    xx.SetStatusText('working path: %s'%(wpath))
    os.chdir(wpath)

def GetInFile(self, ftyp = 'dwa'):
    infile, fl, pathfile = '', [], ''
    if '.' in ftyp: srcstr = ftyp
    else: srcstr = '*.' + ftyp
    fl = glob.glob(srcstr)
    if len(fl) == 1:
         pathfile = os.getcwd() + gv.SEP + fl[0]
         SetPath(self, pathfile)
    else:
        dlg = wx.FileDialog(self, message = "Choose .%s file"%ftyp, defaultDir = os.getcwd(), 
        defaultFile = "", wildcard = srcstr, style = wx.FD_OPEN)
        if dlg.ShowModal()  ==  wx.ID_OK:
            pathfile = dlg.GetPaths()[0]
            SetPath(self, pathfile)
            dlg.Destroy()
        else:
            _userCancel = dlg.Destroy()
            dlg.Destroy() 
    return pathfile

def phase_from_tqi(self, tqifile):
    smproot = tqifile.rpartition(gv.SEP)[-1]
    dbnx = '00'
    shpx = 'SPH', 'QBE', 'PAR', 'HEX', 'CYL', 'CSH'
    shx = 'csdkmcoskdovnfvnjfknvfnvalkcdsl;mwp'
    ks = 0
    for s in shpx:
        ks += 1
        if s in tqifile:
            shx = s
            break
    i0 = tqifile.rfind(shx)
    if i0 > 0:
        if ks <= 2:
            smproot = tqifile[:i0-6]
            dbnx = '03'
        elif ks == 6:
            smproot = tqifile[:i0-11]
            dbnx = '05'
        elif (ks > 2 and ks < 6):
            smproot = tqifile[:i0-11]
            dbnx = '04'
    else:
        ii = tqifile.rfind('tqi')
        smproot = tqifile[:ii-1]
        dbnx = '01'
    return smproot, dbnx

##----------------------------------------------------------------
# def kill_proc_tree(pid, including_parent=True):    
#     parent = psutil.Process(pid)
#     children = parent.children(recursive=True)
#     for child in children:
#         child.kill()
#     psutil.wait_procs(children, timeout=5)
#     if including_parent:
#         parent.kill()
#         parent.wait(5)
##----------------------------------------------------------------
def toBuffer(self, buffer, text, find=False):
    if find: xx = wx.FindWindowByName(buffer)
    else: xx = buffer
    xx.AppendText(text)
    xx.AppendText('\n')

##----------------------------------------------------------------
def getDebussy_sum(self, file_output, buffer, file_err=None, find_buffer=False):
    if find_buffer: xx = wx.FindWindowByName(buffer)
    else: xx = buffer
    if os.stat(file_output).st_size > 0: 
        bf = open(file_output, 'r')
        bfl = bf.readlines()
        bf.close()
        for l in bfl:
            xx.AppendText(l)
        xx.AppendText('\n')
    if file_err != None:
        if os.stat(file_err).st_size > 0:
            ebf = open(file_err, 'r')
            ebfl = ebf.readlines()
            ebf.close()
            for l in ebfl:
                xx.AppendText(l)
            xx.AppendText('\n')

##----------------------------------------------------------------
def file2Buffer(self, file_input, buffer, find_buffer=False):
    if find_buffer: xx = wx.FindWindowByName(buffer)
    else: xx = buffer
    if os.stat(file_input).st_size > 0: 
        bf = open(file_input, 'r')
        bfl = bf.readlines()
        bf.close()
        for l in bfl:
            xx.AppendText(l)
        xx.AppendText('\n')

##----------------------------------------------------------------

def get_term(platform = 'linux', opt = []):
    if platform[:3].lower() == 'lin':
        terminal = 'xterm -geometry 120x30'.split() + opt
    elif platform[:3].lower() == 'dar':
        terminal = 'xterm -geometry 120x30'.split() + opt
    elif platform[:3].lower() == 'win':
        terminal = 'cmd.exe'.split()
    return terminal
##----------------------------------------------------------------

def Q(tt,l):
    return 4*pi*sin(radians(tt/2))/l
##----------------------------------------------------------------

def q(tt,l):
    return 2*sin(radians(tt/2))/l
##----------------------------------------------------------------

def d(tt,l):
    return l/(2*sin(radians(tt/2)))
##----------------------------------------------------------------
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
##----------------------------------------------------------------



# def OnIOErr(msg):
#     """
#     Display the error message
#     """
# #     if gset.Platform[:3]=='dar':
# #         font = wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
# #         self.SetFont(font)
#     dlg = wx.MessageDialog(None, msg, 'ERROR', wx.OK | wx.ICON_ERROR)
#     dlg.ShowModal()
#     dlg.Destroy()

##----------------------------------------------------------------------------------------
# class ExceptionDialog(GMD.GenericMessageDialog):
#     """"""
#  
#     #----------------------------------------------------------------------
#     def __init__(self, msg):
#         """Constructor"""
#         GMD.GenericMessageDialog.__init__(self, None, msg, "Exception!",
#                                           wx.OK|wx.ICON_ERROR)
#         if gset.Platform[:3]=='dar':
#             font = wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
#             self.SetFont(font)
##----------------------------------------------------------------------------------------

