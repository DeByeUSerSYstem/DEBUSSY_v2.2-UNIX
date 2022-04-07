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
####
import sys
import os
import wx
import wx.lib.dialogs
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
from debfuncx import get_sep
##########################################################################################

##----------------------------------------------------------------------------------------
verbose=False
###########
platform=sys.platform
if platform.startswith('lin') :
    plat=1
    sep ='/'
    pgmpath=os.getcwd()+sep
    pgmname='cif2pha_DebUsSy'
    pgm0=pgmpath+pgmname
    if verbose : print('pgm0 ',pgm0)
    pgm='"%s"'%pgm0
    if verbose : print('pgm ',pgm)
elif platform.startswith('dar') :
    plat=2
    sep='/' 
    pgmpath=os.getcwd()+sep
    pgmname='cif2pha_DebUsSy'
    pgm0=pgmpath+pgmname
    if verbose : print('pgm0 ',pgm0)
    pgm='"%s"'%pgm0
    if verbose : print('pgm ',pgm)
elif platform.startswith('win') :
    plat=3
    sep='\\'
    pgmpath=os.getcwd()+sep
    pgmname='cif2pha_DebUsSy_WIN.exe'
    pgm0=pgmpath+pgmname
    if verbose : print('pgm0 ',pgm0)
    pgm='"%s"'%pgm0
    if verbose : print('pgm ',pgm)

##
aboutText ='CIF to PHA conversion Utility - Beta version\n\
\n\
Istituto di Cristallografia,\n\
Consiglio Nazionale delle Ricerche\n\
IC-CNR, Como'
##
helpText = '             ________INSTRUCTIONS_________  \n \
* denotes mandatory inputs \n \
* File(s) : Browse and select the .CIF file(s) \n \
            (multiple selection (also from different folders, allowed). \n\
Output folder : Browse and select the folder where you want the output files to be saved \n \
                (if no selection is performed, the output folder is set to be the same as \n \
                the original .CIF file). \n \
Inorganic/Organic structure selection : default choice is "Inorganic".\n\
~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~\n'
####___MAIN WINDOW
class MainWindow(wx.Frame):
  def __init__(self,parent):
    # create a frame, no parent, default to wxID_ANY
    wx.Frame.__init__(self, parent, wx.ID_ANY, title='CIF to PHA converter',\
    pos=(-1, -1), size=(630,430))##,style = wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
#     self.Bind(wx.EVT_CLOSE, self.OnClose)
    self.SetBackgroundColour((232,232,232))
    self.frame = parent
    
    self.vbox = wx.BoxSizer(wx.VERTICAL)
    
    hbox0 = wx.BoxSizer(wx.HORIZONTAL)
    self.flst = wx.StaticText(self, label="File(s)",size=(100,-1))
    hbox0.Add(self.flst, border=10)
    self.btnf = wx.Button(self, label="...",size=(50,20))
    hbox0.Add(self.btnf,border=10)
    self.btnf.Bind(wx.EVT_BUTTON, self.btnf_click)
    self.btnf.SetToolTip(wx.ToolTip("Browse and select one or more file(s) .."))
    self.flsf = wx.TextCtrl(self,style=wx.TE_MULTILINE)
    hbox0.Add(self.flsf,1,wx.EXPAND,border=10)
    self.xyzs,self.flist='',[]
    self.vbox.Add(hbox0,1,wx.EXPAND|wx.ALL,border=5)


    hbox01 = wx.BoxSizer(wx.HORIZONTAL)
    self.fldt = wx.StaticText(self, label="Output folder",size=(100,-1))
    hbox01.Add(self.fldt, border=10)
    self.btnfld = wx.Button(self, label="...",size=(50,20))
    hbox01.Add(self.btnfld,border=10)
    self.btnfld.Bind(wx.EVT_BUTTON, self.btnfld_click)
    self.btnfld.SetToolTip(wx.ToolTip("Browse and select the output folder"))
    self.fldf = wx.TextCtrl(self,style=wx.TE_LEFT)
    hbox01.Add(self.fldf,1,border=10)
    self.pathd=''
    self.vbox.Add(hbox01,0,wx.EXPAND|wx.ALL,border=5)


    hbox03 = wx.BoxSizer(wx.HORIZONTAL)
    self.rb1 = wx.RadioButton(self, -1, 'Inorganic', style=wx.RB_GROUP)
    self.rb1.SetValue(True)
    hbox03.Add(self.rb1, border=1)
    self.rb2 = wx.RadioButton(self, -1, 'Organic')
    hbox03.Add(self.rb2,  border=1)
    self.vbox.Add(hbox03, 0,flag=wx.ALIGN_CENTRE,border=5)

    hbox4 = wx.BoxSizer(wx.HORIZONTAL)
    self.clear_btn = wx.Button(self, id=-1, label='Clear')
    self.clear_btn.Bind(wx.EVT_BUTTON, self.OnClear)
    # optional tooltip
    self.clear_btn.SetToolTip(wx.ToolTip("Clear input"))
    hbox4.Add(self.clear_btn,flag=wx.ALL, border=10)
    self.vbox.Add(hbox4, proportion=0, flag=wx.ALIGN_LEFT)

    hbox3 = wx.BoxSizer(wx.HORIZONTAL)
    self.about_btn = wx.Button(self, id=-1, label='About')
    self.about_btn.Bind(wx.EVT_BUTTON, self.OnAbout)
    # optional tooltip
    self.about_btn.SetToolTip(wx.ToolTip("About this program"))
    hbox3.Add(self.about_btn,flag=wx.TOP|wx.BOTTOM, border=6)
    #self.help_btn = wx.Button(self, label='Help')
    #self.help_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
    #self.help_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
    #hbox3.Add(self.help_btn, flag=wx.ALL, border=3)
    self.close_btn = wx.Button(self, label="Close")
    self.close_btn.Bind(wx.EVT_BUTTON, self.OnClose)
    self.close_btn.SetToolTip(wx.ToolTip("Close the window"))
    hbox3.Add(self.close_btn, flag=wx.TOP|wx.BOTTOM, border=6)
    self.do_btn = wx.Button(self, label="DO")
    self.do_btn.Bind(wx.EVT_BUTTON, self.Do)
    self.do_btn.SetToolTip(wx.ToolTip("Do the job"))    
    hbox3.Add(self.do_btn, flag=wx.TOP|wx.BOTTOM, border=6)
    self.vbox.Add(hbox3, proportion=0, flag=wx.ALIGN_RIGHT)

    text = wx.TextCtrl(self,wx.ID_ANY,style=wx.TE_MULTILINE| wx.HSCROLL)
    self.text = text
    redir=RedirectText(self.text)
    sys.stdout=redir
    redirerr=RedirectText(self.text)
    sys.stderr=redirerr
    self.vbox.Add(self.text,2,wx.EXPAND|wx.ALL,border=5)
    self.text.AppendText(helpText)
    self.text.AppendText(' \n')

    
#     self.tbuffer = wx.TextCtrl(self,style=wx.TE_MULTILINE| wx.HSCROLL)
#     redir=RedirectText(self.tbuffer)
#     sys.stdout=redir
#     redirerr=RedirectText(self.tbuffer)
#     sys.stderr=redirerr
#     self.vbox.Add(self.tbuffer,2,wx.EXPAND|wx.ALL,border=5)
#     
#     self.tbuffer.AppendText(helpText)

    self.SetSizer(self.vbox)
    # show the frame
#     self.vbox.Layout()
#     self.Fit()
    self.Show(True)
  
  ##_____DEFINITIONS
  
  def btnf_click(self,event):
    """
    Create and show the Open FileDialog
    """
    dlg = wx.FileDialog(self, message="Choose one or more file(s)",
    defaultFile="", wildcard='*.cif', style=wx.OPEN | wx.MULTIPLE | wx.CHANGE_DIR)
    #self.flsf.Clear()
    if dlg.ShowModal() == wx.ID_OK:
      self.xyzs = dlg.GetPaths()
      if verbose : print(len(self.xyzs),self.xyzs)
      k0=0
      if len(self.flsf.GetValue())>0:
          nf0=self.flsf.GetValue().split('\n')
          if verbose : print('nf0 ',len(nf0))
          k0=len(nf0)-1
      for k in range(len(self.xyzs)):
          self.flsf.AppendText('%i. %s\n'%(k0+k+1,self.xyzs[k]))
    dlg.Destroy()
    
  ###____SET DIR
  def btnfld_click(self,event):
    global _selectedDir, _userCancel
    dialog =  wx.DirDialog(self, "Please choose the output directory:",\
    defaultPath=os.getcwd(),style=wx.DD_CHANGE_DIR , pos = (10,10))
    if dialog.ShowModal() == wx.ID_OK:
      self.pathd=dialog.GetPath()
      self.fldf.SetValue(self.pathd)
      #print(dialog.GetPath())
      _selectedDir = dialog.GetPath()
      return _selectedDir
    else:
      _userCancel = dialog.Destroy()
      return _userCancel
    dialog.Destroy() 

  def on_text(self, text):
      self.text.AppendText(text)

  def Do(self,event):
    fnames=[]
    if verbose : print('text input \n',self.flsf.GetValue())
#     if len(self.xyzs)>0:
#       for k in range(len(self.xyzs)):
#         fnames+=[self.xyzs[k].strip()]
    if (len(self.flsf.GetValue())>0):
      if verbose : print('reading filenames from text input')
      str0=self.flsf.GetValue().splitlines()
      for i in range(len(str0)):
          if verbose : print('str0 ',i,str0[i])
          str1=str0[i].partition(' ')
          if verbose :print('str1 ',str1,str1[0])
          str2=str1[-1]
          if verbose : print('str2 ',str2)
          fnames+=[str2]
    if verbose : print('files ',fnames)
    if len(fnames)==0:
        print(' File(s) NOT selected !')
    outfld=''
    if len(self.pathd)>0:
        outfld=self.pathd+sep
        sel_outfld=True
    elif (len(self.pathd)==0 and len(self.fldf.GetValue())>0):
        outfld=self.fldf.GetValue().strip()+sep
        sel_outfld=True
    if len(outfld)==0: sel_outfld=False
    if verbose : print('outfld ',sel_outfld,outfld)
    if self.rb1.GetValue(): ioflag='ino'
    elif self.rb2.GetValue(): ioflag='org'
    if (len(fnames)>0): 
      if verbose : print('ioflag ',ioflag)
      for fk in range(len(fnames)):
          ciffile=fnames[fk].rpartition(sep)[-1]
          cifname=ciffile[:-4]
          if sel_outfld==False:
              outfld=fnames[fk].rpartition(sep)[0]+sep
          phafile=outfld+cifname+'.pha'
          outlog=outfld+cifname+'.log'
          self.cmd=pgm+' '+fnames[fk]+' '+phafile+' '+ioflag
          print(' %3i.  %-10s %s \n %5s  %-10s %s'%(fk+1,'CONVERTING',fnames[fk],' ','INTO',phafile))
          if verbose : print('last check ', fk+1,self.cmd,pgm,fnames[fk],phafile,ioflag)
          os.system(self.cmd)
          if os.path.isfile(outlog):
              logfile=open(outlog,'r')
              logl=logfile.readlines()
              logfile.close()
              for line in logl:
                  self.text.AppendText('%7s%s'%(' ',line))
          print(' %4s  %s \n'%(' ','... JOB DONE !'))
      fnames,outfld,phafile,ioflag=[],'','','ino'
      self.fldf.SetValue('')
      self.pathd=''
      self.flsf.Clear()
      print('\n')

  ### CLEAR
  def OnClear(self, event):
      self.fldf.SetValue('')
      self.pathd=''
      self.flsf.Clear()
      return

  ### CLOSE 
  def OnClose(self, event):
      self.Destroy()

  ### ABOUT
  def OnAbout(self, event):
    dlg = wx.MessageDialog(self,aboutText,"About  Cif2Pha", wx.OK)
    result = dlg.ShowModal()
    dlg.Destroy()
    if result == wx.ID_OK:
      dlg.Destroy()

class RedirectText:
  def __init__(self,aWxTextCtrl):
    self.out=aWxTextCtrl

  def write(self,string):
    self.out.WriteText(string)


application = wx.App()
# call class MyFrame
window = MainWindow(None)
# start the event loop
application.MainLoop()
