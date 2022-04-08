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
import os
from shutil import copy2 as shutil_copy2
import wx
import wx.lib.dialogs
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
###############
verbose=False
####___MAIN WINDOW
class SettingsWindow(wx.Frame):
  def __init__(self):
    # create a frame, no parent, default to wxID_ANY
    wx.Frame.__init__(self, None, wx.ID_ANY, title='DebUsSy_GUI settings window',\
    pos=(-1, -1), size=(600,150))##,style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
#     self.Bind(wx.EVT_CLOSE, self.OnClose)
#     self.frame = parent
    
    vbox = wx.BoxSizer(wx.VERTICAL)
    
    hbox01 = wx.BoxSizer(wx.HORIZONTAL)
    fldt = wx.StaticText(self, label="User's home folder",size=(200,-1))
    hbox01.Add(fldt, border=10)
    btnfld = wx.Button(self, label="...",size=(50,20))
    hbox01.Add(btnfld,border=10)
    btnfld.Bind(wx.EVT_BUTTON, self.btnfld_click)
    btnfld.SetToolTip(wx.ToolTip("Browse and select your home folder"))
    self.fldf = wx.TextCtrl(self,style=wx.TE_LEFT)
    self.fldf.SetValue(gset.User_Path)
    hbox01.Add(self.fldf,1,border=10)
    self.pathd=''
    vbox.Add(hbox01,0,wx.EXPAND|wx.ALL,border=5)

    hbox02 = wx.BoxSizer(wx.HORIZONTAL)
    edtt = wx.StaticText(self, label="Text gset.Editor program",size=(200,-1))
    hbox02.Add(edtt, border=10)
    btnedt = wx.Button(self, label="...",size=(50,20))
    hbox02.Add(btnedt,border=10)
    btnedt.Bind(wx.EVT_BUTTON, self.btnedt_click)
    btnedt.SetToolTip(wx.ToolTip("Browse and select your favourite \
    Text gset.Editor program (executable file)"))
    self.edtf = wx.TextCtrl(self,style=wx.TE_LEFT)
    self.edtf.SetValue(gset.Editor)
    hbox02.Add(self.edtf,1,border=10)
    self.edt=''
    vbox.Add(hbox02,0,wx.EXPAND|wx.ALL,border=5)

    hbox03 = wx.BoxSizer(wx.HORIZONTAL)
    avt = wx.StaticText(self, label="Atomistic viewer program",size=(200,-1))
    hbox03.Add(avt, border=10)
    btnav = wx.Button(self, label="...",size=(50,20))
    hbox03.Add(btnav,border=10)
    btnav.Bind(wx.EVT_BUTTON, self.btnav_click)
    btnav.SetToolTip(wx.ToolTip("Browse and select your favourite \
    Atomistic viewer program (executable file)"))
    self.avf = wx.TextCtrl(self,style=wx.TE_LEFT)
    self.avf.SetValue(gset.AtomViewer)
    hbox03.Add(self.avf,1,border=10)
    self.av=''
    vbox.Add(hbox03,0,wx.EXPAND|wx.ALL,border=5)

    hbox3 = wx.BoxSizer(wx.HORIZONTAL)
#     msg = wx.StaticText(self, label="RESTART THE PROGRAM AFTER CHANGING",size=(400,-1))
#     hbox3.Add(msg, flag=wx.ALL, border=3)
#     help_btn = wx.Button(self, label='Help')
#     help_btn.Bind(wx.EVT_BUTTON, OnHelp)
#     help_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
#     hbox3.Add(help_btn, flag=wx.ALL, border=3)
    close_btn = wx.Button(self, label="Close")
    close_btn.Bind(wx.EVT_BUTTON, self.OnClose)
    close_btn.SetToolTip(wx.ToolTip("Close the window"))
    hbox3.Add(close_btn, flag=wx.ALL, border=3)
    ok_btn = wx.Button(self, label="OK")
    ok_btn.Bind(wx.EVT_BUTTON, self.Ok)
    ok_btn.SetToolTip(wx.ToolTip("Update settings"))    
    hbox3.Add(ok_btn, flag=wx.ALL, border=3)
    vbox.Add(hbox3, proportion=1, flag=wx.ALIGN_RIGHT, border=3)

    self.SetSizer(vbox)
    # show the frame
    self.Show(True)
  
  ##_____DEFINITIONS
  
  ###____SET DIR
  def btnfld_click(self,event):
    global _selectedDir, _userCancel
    dialog =  wx.DirDialog(self, "Please choose your project directory:",\
    defaultPath=os.getcwd(),style=wx.DD_CHANGE_DIR , pos = (10,10))
    if dialog.ShowModal() == wx.ID_OK:
      self.pathd = dialog.GetPath()
      if len(self.pathd) > 0:
          self.fldf.SetValue(self.pathd)
          gset.User_Path = self.pathd
      return self.pathd
    else:
      _userCancel = dialog.Destroy()
      return _userCancel
    dialog.Destroy() 

  ###____SET EDITOR
  def btnedt_click(self,event):
    """
    Create and show the Open FileDialog
    """
    dlg = wx.FileDialog(self, message="Choose your favourite Text gset.Editor",
    defaultFile="", style=wx.OPEN | wx.CHANGE_DIR)
    if dlg.ShowModal() == wx.ID_OK:
      self.edt = dlg.GetPaths()
      if len(self.edt) > 0:
          self.edtf.AppendText(self.edt[0])
          gset.Editor = self.edt[0]
    dlg.Destroy()

  ###____SET Atomistic viewer
  def btnav_click(self,event):
    """
    Create and show the Open FileDialog
    """
    dlg = wx.FileDialog(self, message="Choose your favourite Atomistic viewer",
    defaultFile="", style=wx.OPEN | wx.CHANGE_DIR)
    if dlg.ShowModal() == wx.ID_OK:
      self.av = dlg.GetPaths()
      if len(self.av) > 0:
          self.avf.AppendText(self.av[0])
          gset.AtomViewer = self.av[0]
    dlg.Destroy()

  def Ok(self,event):
    if verbose : print(gset.Platform, gset.DEB_Path, gset.PGM_Path, gset.GUI_Path, gset.User_Path, gset.Editor, gset.AtomViewer)
    if verbose : print(len(self.pathd),self.pathd,len(self.edt),self.edt, len(self.av),self.av)
    if (len(self.pathd)>0 or len(self.edt)>0 or len(self.av)>0):
        setfn = gset.GUI_Path + 'gui_settings.py'
        shutil_copy2(gset.GUI_Path+'debussy_src_header.txt', setfn)
        setf = open(setfn, 'a')
        print("Platform = '%s'"%gset.Platform, file = setf)
        print("DEB_Path = '%s'"%gset.DEB_Path, file = setf)
        print("PGM_Path = '%s'"%gset.PGM_Path, file = setf)
        print("GUI_Path = '%s'"%gset.GUI_Path, file = setf)
        print("User_Path = '%s'"%gset.User_Path, file = setf)
        print("Editor = '%s'"%gset.Editor, file = setf)
        print("AtomViewer = '%s'"%gset.AtomViewer, file = setf)
        setf.close()
#         print('*** PREFERENCES UPDATED ***\n*** PLEASE RESTART THE PROGRAM ***')
        self.Destroy()
    else:
        print('*** SETTING PREFERENCES : NOTHING TO DO! ***')

#   def OnHelp(self, help_btn):
#         """
#         Launch the Help window
#         """
#         f = open(gset.GUI_Path+"setwhlp", "r")
#         msg = f.read()
#         f.close()
# 
#         dlg = wx.lib.dialogs.ScrolledMessageDialog(self, msg, "DebUsSy_GUI settings help",size=(600,400))
#         dlg.ShowModal()
#  
#         dlg.Destroy()


  ### CLOSE 
  def OnClose(self, event):
#     dlg = wx.MessageDialog(self,"Close?",
#     "Confirm Exit", wx.OK|wx.CANCEL|wx.ICON_QUESTION)
#     result = dlg.ShowModal()
#     dlg.Destroy()
#     if result == wx.ID_OK:
      self.Destroy()
      

# application = wx.App()
# # call class MyFrame
# window = SettingsWindow(None)
# # start the event loop
# application.MainLoop()
if __name__ == "__main__":
    app = wx.App(False)
    frame = SettingsWindow()
    app.MainLoop()
