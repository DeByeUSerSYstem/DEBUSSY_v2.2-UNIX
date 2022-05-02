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
import threading
import subprocess
import wx
import wx.lib.dialogs
from shutil import copy2 as shutil_copy2
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
from debfuncx import SetPath, GetInFile, toBuffer, get_term, getDebussy_sum, file2Buffer
from readerC import reader
from debyer import debyer
from plotterC import plotter
from customw7 import CustomPlotter
########################################################################
verbose = False
# xterm_opt = 'xterm -geometry 120x30'.split()
xterm_opt = get_term(gset.Platform)
xterm_opt0 = 'xterm -T TESTING -e tail -f PIPE_PATH'.split()
wtm = 86400
wtms = 5
if not gset.PGM_Path.endswith(gv.SEP) : gset.PGM_Path = gset.PGM_Path+gv.SEP
if not gset.GUI_Path.endswith(gv.SEP) : gset.GUI_Path = gset.GUI_Path+gv.SEP
###
class DWA(wx.lib.scrolledpanel.ScrolledPanel):
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        if gset.Platform.startswith('dar'):
            font = wx.Font(11, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)
 
        self.frame = parent
        self.vbox = wx.BoxSizer(wx.VERTICAL)

        hboxb = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hboxb.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .dwa')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_dwa)
        ldin_btn.SetToolTip(wx.ToolTip("Load input information from a .dwa file"))    
        hboxb.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAV_dwa)
        svf_btn.SetToolTip(wx.ToolTip("Save input values to a .dwa file"))    
        hboxb.Add(svf_btn, flag = wx.ALL, border = 3)
        self.vbox.Add(hboxb, flag = wx.ALIGN_RIGHT)

        hbox0n = wx.BoxSizer(wx.HORIZONTAL)
        namet = wx.StaticText(self, label = "Name")
#         namet.SetFont(font)
        hbox0n.Add(namet)
        self.namef = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (160, -1))
        self.namef.SetMaxLength(132)
        hbox0n.Add(self.namef)
        extt = wx.StaticText(self, label = ".dwa")
        hbox0n.Add(extt)
        self.vbox.Add(hbox0n)
        
        self.datbox = wx.BoxSizer(wx.VERTICAL)

        dtxt = wx.StaticText(self, label = "************ Datasets section ************")
        self.datbox.Add(dtxt, flag = wx.ALIGN_CENTRE)
        hbox001 = wx.BoxSizer(wx.HORIZONTAL)
        ld_btn  = wx.Button(self, label = '+', size = (50, -1))
        ld_btn.Bind(wx.EVT_BUTTON, self.Add_Dataset)
        ld_btn.SetToolTip(wx.ToolTip("Add a dataset"))    
        hbox001.Add(ld_btn, flag = wx.BOTTOM, border = 10)
        rm_btn  = wx.Button(self, label = '-', size = (50, -1))
        rm_btn.Bind(wx.EVT_BUTTON, self.Del_Dataset)
        rm_btn.SetToolTip(wx.ToolTip("Remove a dataset"))    
        hbox001.Add(rm_btn, flag = wx.BOTTOM|wx.LEFT, border = 10)
        self.datbox.Add(hbox001, flag = wx.ALIGN_RIGHT)

        self.dflags = '# tech data form rang blnk blnc cheb youn wave esdw inst beam mono pola geom'.split()
        self.ndataset, self.du = 0, len(self.dflags)
        datset = self.Do_Datset()
        
        self.vbox.Add(self.datbox, 0, flag = wx.EXPAND|wx.TOP, border = 5)

        self.strubox = wx.BoxSizer(wx.VERTICAL)

        stxt = wx.StaticText(self, label = "************ Structures section ************")
        self.strubox.Add(stxt, flag = wx.ALIGN_CENTRE|wx.TOP, border = 10)
        hbox001 = wx.BoxSizer(wx.HORIZONTAL)
        ld_btn  = wx.Button(self, label = '+', size = (50, -1))
        ld_btn.Bind(wx.EVT_BUTTON, self.Add_Struc)
        ld_btn.SetToolTip(wx.ToolTip("Add a structure"))    
        hbox001.Add(ld_btn, flag = wx.BOTTOM, border = 10)
        rm_btn  = wx.Button(self, label = '-', size = (50, -1))
        rm_btn.Bind(wx.EVT_BUTTON, self.Del_Struc)
        rm_btn.SetToolTip(wx.ToolTip("Remove a structure"))    
        hbox001.Add(rm_btn, flag = wx.BOTTOM|wx.LEFT, border = 10)
        self.strubox.Add(hbox001, flag = wx.ALIGN_RIGHT)
        
        self.sflags = '% stru dbn db prot nclu nrod shap chem cell micr parx'.split() ##natc spgn 
        self.nstruset, self.su = 0, len(self.sflags)
        struset = self.Do_Struc()

        self.vbox.Add(self.strubox, 0, flag = wx.EXPAND|wx.TOP, border = 5)

        self.refbox = wx.BoxSizer(wx.VERTICAL)

        reft = wx.StaticText(self, label = "************ Refinement section ************")
        self.refbox.Add(reft, flag = wx.ALIGN_CENTRE|wx.TOP, border = 10)

        self.rflags = 'simu calm rfil outs rpdf'.split()
        self.ru = len(self.rflags)
        refset = self.Do_Ref()

        self.vbox.Add(self.refbox, 0, flag = wx.EXPAND|wx.TOP, border = 5)

        self.SetSizer(self.vbox)

#-------------------------------------------------------------------

    def Do_Datset(self):
        self.ndataset += 1
        hbox0n = wx.BoxSizer(wx.HORIZONTAL)
        ndatt = wx.StaticText(self, label = "#", size = (15, -1))
        hbox0n.Add(ndatt, flag = wx.TOP, border = 5)
#         hbox0n.Add((5, -1))
        ndatf = wx.StaticText(self, label = "%i"%self.ndataset, style = wx.ALIGN_LEFT, size = (30, -1), \
        name = '#%i'%self.ndataset)
        hbox0n.Add(ndatf, flag = wx.TOP, border = 5)
        hbox0n.Add((40, -1))
        techf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (40, -1), name = 'tech%i'%self.ndataset)
        techf.SetMaxLength(132)
        hbox0n.Add(techf, flag = wx.TOP, border = 5)
        ndath = wx.StaticText(self, label = r"!")
        hbox0n.Add(ndath, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox0n, 1, flag = wx.EXPAND)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        datt = wx.StaticText(self, label = "data", size = (45, -1))
        hbox01.Add(datt, flag = wx.TOP, border = 5)
        dat_btn  = wx.Button(self, label = '..', size = (30, -1))
        dat_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'data%i'%self.ndataset:\
         self.LD_file(event, place))
        dat_btn.SetToolTip(wx.ToolTip("Add a data file"))    
        hbox01.Add(dat_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        datf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'data%i'%self.ndataset)
        datf.SetMaxLength(132)
        hbox01.Add(datf, 1, flag = wx.EXPAND|wx.TOP, border = 5)
        dath = wx.StaticText(self, label = r"!")
        hbox01.Add(dath, flag = wx.TOP, border = 5)
        formt = wx.StaticText(self, label = r"form", size = (45, -1))
        hbox01.Add(formt, flag = wx.TOP|wx.LEFT, border = 5)
        formf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (40, -1), name = 'form%i'%self.ndataset)
        formf.SetMaxLength(132)
        hbox01.Add(formf, flag = wx.TOP, border = 5)
        formh = wx.StaticText(self, label = "!")
        hbox01.Add(formh, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox01, 1, flag = wx.EXPAND)

        hbox02 = wx.BoxSizer(wx.HORIZONTAL)
        rant = wx.StaticText(self, label = "rang", size = (45, -1))
        hbox02.Add(rant, flag = wx.TOP, border = 5)
        hbox02.Add((40, -1))
        ranf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'rang%i'%self.ndataset)
        ranf.SetMaxLength(132)
        hbox02.Add(ranf, flag = wx.TOP, border = 5)
        ranh = wx.StaticText(self, label = r"!")
        hbox02.Add(ranh, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox02)

        hbox03 = wx.BoxSizer(wx.HORIZONTAL)
        blnkt = wx.StaticText(self, label = "blnk", size = (45, -1))
        hbox03.Add(blnkt, flag = wx.TOP, border = 5)
        bln_btn  = wx.Button(self, label = '..', size = (30, -1))
        bln_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'blnk%i'%self.ndataset :\
         self.LD_file(event, place))
        bln_btn.SetToolTip(wx.ToolTip("Add a blank file"))    
        hbox03.Add(bln_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        blnkf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'blnk%i'%self.ndataset)
        blnkf.SetMaxLength(132)
        hbox03.Add(blnkf, 1, flag = wx.EXPAND|wx.TOP, border = 5)
        blnkh = wx.StaticText(self, label = r"?")
        hbox03.Add(blnkh, flag = wx.TOP, border = 5)
        blnct = wx.StaticText(self, label = r"blnc", size = (45, -1))
        hbox03.Add(blnct, flag = wx.TOP|wx.LEFT, border = 5)
        blncf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (40, -1), name = 'blnc%i'%self.ndataset)
        blncf.SetMaxLength(132)
        hbox03.Add(blncf, flag = wx.TOP, border = 5)
        blnch = wx.StaticText(self, label = "*")
        hbox03.Add(blnch, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox03, 1, flag = wx.EXPAND)

        hbox04 = wx.BoxSizer(wx.HORIZONTAL)
        chebt = wx.StaticText(self, label = "cheb", size = (45, -1))
        hbox04.Add(chebt, flag = wx.TOP, border = 5)
        hbox04.Add((40, -1))
        chebf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'cheb%i'%self.ndataset)
        chebf.SetMaxLength(132)
        hbox04.Add(chebf, flag = wx.TOP, border = 5)
        chebh = wx.StaticText(self, label = r"?")
        hbox04.Add(chebh, flag = wx.TOP, border = 5)
        yount = wx.StaticText(self, label = r"youn", size = (45, -1))
        hbox04.Add(yount, flag = wx.TOP|wx.LEFT, border = 5)
        younf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'youn%i'%self.ndataset)
        younf.SetMaxLength(132)
        hbox04.Add(younf, flag = wx.TOP, border = 5)
        younh = wx.StaticText(self, label = "*")
        hbox04.Add(younh, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox04)

        hbox05 = wx.BoxSizer(wx.HORIZONTAL)
        wavet = wx.StaticText(self, label = "wave", size = (45, -1))
        hbox05.Add(wavet, flag = wx.TOP, border = 5)
        hbox05.Add((40, -1))
        wavef = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'wave%i'%self.ndataset)
        wavef.SetMaxLength(132)
        hbox05.Add(wavef, flag = wx.TOP, border = 5)
        waveh = wx.StaticText(self, label = r"!")
        hbox05.Add(waveh, flag = wx.TOP, border = 5)
        esdwt = wx.StaticText(self, label = r"esdw", size = (45, -1))
        hbox05.Add(esdwt, flag = wx.TOP|wx.LEFT, border = 5)
        esdwf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'esdw%i'%self.ndataset)
        esdwf.SetMaxLength(132)
        hbox05.Add(esdwf, flag = wx.TOP, border = 5)
        esdwh = wx.StaticText(self, label = "*")
        hbox05.Add(esdwh, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox05)

        hbox07 = wx.BoxSizer(wx.HORIZONTAL)
        instt = wx.StaticText(self, label = "inst", size = (45, -1))
        hbox07.Add(instt, flag = wx.TOP, border = 5)
        instf1 = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (30, -1), name = 'inst%i'%self.ndataset)
        hbox07.Add(instf1, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        inpvt = wx.StaticText(self, label = r"pv", size = (30, -1))
        hbox07.Add(inpvt, flag = wx.TOP|wx.LEFT, border = 5)
        inpvf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (225, -1), name = 'inpv%i'%self.ndataset)
        inpvf.SetMaxLength(132)
        hbox07.Add(inpvf, flag = wx.TOP, border = 5)
        inpvh = wx.StaticText(self, label = r"?")
        hbox07.Add(inpvh, flag = wx.TOP, border = 5)
        zxwt = wx.StaticText(self, label = u"\u03B6 \u03BE \u03C9", size = (45, -1))
        hbox07.Add(zxwt, flag = wx.TOP|wx.LEFT, border = 5)
        zxwf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'izxw%i'%self.ndataset)
        zxwf.SetMaxLength(132)
        hbox07.Add(zxwf, flag = wx.TOP, border = 5)
        zxwh = wx.StaticText(self, label = "?")
        hbox07.Add(zxwh, flag = wx.TOP, border = 5)

          ##__here for a later use
#         i000t = wx.StaticText(self, label = r"i000", size = (45, -1))
#         hbox07.Add(i000t, flag = wx.TOP|wx.LEFT, border = 5)
#         i000f = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (148, -1), name = 'i000%i'%self.ndataset)
#         i000f.SetMaxLength(132)
#         hbox07.Add(i000f, flag = wx.TOP, border = 5)
#         i000h = wx.StaticText(self, label = "?")
#         hbox07.Add(i000h, flag = wx.TOP, border = 5)

        self.datbox.Add(hbox07)

        hbox06 = wx.BoxSizer(wx.HORIZONTAL)
        beamt = wx.StaticText(self, label = "beam", size = (45, -1))
        hbox06.Add(beamt, flag = wx.TOP, border = 5)
        hbox06.Add((40, -1))
        beamf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'beam%i'%self.ndataset)
        beamf.SetMaxLength(132)
        hbox06.Add(beamf, flag = wx.TOP, border = 5)
        beamh = wx.StaticText(self, label = r"!")
        hbox06.Add(beamh, flag = wx.TOP, border = 5)
        monot = wx.StaticText(self, label = r"mono", size = (45, -1))
        hbox06.Add(monot, flag = wx.TOP|wx.LEFT, border = 5)
        monof = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (102, -1), name = 'mono%i'%self.ndataset)
        monof.SetMaxLength(132)
        hbox06.Add(monof, flag = wx.TOP, border = 5)
        monoh = wx.StaticText(self, label = "?")
        hbox06.Add(monoh, flag = wx.TOP, border = 5)
        polat = wx.StaticText(self, label = r"pola", size = (45, -1))
        hbox06.Add(polat, flag = wx.TOP|wx.LEFT, border = 5)
        polaf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'pola%i'%self.ndataset)
        polaf.SetMaxLength(132)
        hbox06.Add(polaf, flag = wx.TOP, border = 5)
        polah = wx.StaticText(self, label = "?")
        hbox06.Add(polah, flag = wx.TOP, border = 5)
        geomt = wx.StaticText(self, label = "geom", size = (45, -1))
        hbox06.Add(geomt, flag = wx.TOP|wx.LEFT, border = 5)
        geomf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (148, -1), name = 'geom%i'%self.ndataset)
        geomf.SetMaxLength(132)
        hbox06.Add(geomf, flag = wx.TOP, border = 5)
        geomh = wx.StaticText(self, label = r"!")
        hbox06.Add(geomh, flag = wx.TOP, border = 5)
        self.datbox.Add(hbox06)

    def Add_Dataset(self, event):
        self.Do_Datset()
        self.datbox.Layout()
        self.vbox.Layout()
        self.frame.Layout()

    def Del_Dataset(self, event):
        if self.ndataset >= 2:
            wp = 2 + 8 * (self.ndataset - 1)
            for p in range(wp+7, wp-1, -1):
                self.datbox.Hide(p)
                self.datbox.Remove(p)
            self.ndataset -= 1
            self.datbox.Layout()
            self.vbox.Layout()
            self.frame.Layout()

    def Do_Struc(self):
        self.nstruset += 1
        hbox0n = wx.BoxSizer(wx.HORIZONTAL)
        nstrut = wx.StaticText(self, label = "%", size = (25, -1))
        hbox0n.Add(nstrut, flag = wx.TOP, border = 5)
        nstruf = wx.StaticText(self, label = "%i"%self.nstruset, style = wx.ALIGN_LEFT, size = (30, -1), \
        name = '%'+'%i'%self.nstruset)
        hbox0n.Add(nstruf, flag = wx.TOP, border = 5)
        hbox0n.Add((40, -1))
#         stru_btn  = wx.Button(self, label = '..', size = (30, -1))
#         stru_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'stru%i'%self.nstruset :\
#          self.LD_file(event, place))
#         stru_btn.SetToolTip(wx.ToolTip("Add a structure file"))    
#         hbox0n.Add(stru_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        struf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'stru%i'%self.nstruset)
        struf.SetMaxLength(132)
        hbox0n.Add(struf, flag = wx.TOP, border = 5)
        struh = wx.StaticText(self, label = r"!")
        hbox0n.Add(struh, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox0n, 1, flag = wx.EXPAND)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        dbt = wx.StaticText(self, label = "db", size = (25, -1))
        hbox01.Add(dbt, flag = wx.TOP, border = 5)
        dbnf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (30, -1), name = 'dbn%i'%self.nstruset)
        dbnf.SetMaxLength(132)
        hbox01.Add(dbnf, flag = wx.TOP, border = 5)
        db_btn  = wx.Button(self, label = '..', size = (30, -1))
        db_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'db%i'%self.nstruset :\
         self.LD_file(event, place))
        db_btn.SetToolTip(wx.ToolTip("Add a database"))    
        hbox01.Add(db_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        dbf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'db%i'%self.nstruset)
        dbf.SetMaxLength(132)
        hbox01.Add(dbf, 1, flag = wx.EXPAND|wx.TOP, border = 5)
        dbh = wx.StaticText(self, label = r"!")
        hbox01.Add(dbh, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox01, 1, flag = wx.EXPAND)

        hbox03 = wx.BoxSizer(wx.HORIZONTAL)
        nclut = wx.StaticText(self, label = "nclu", size = (45, -1))
        hbox03.Add(nclut, flag = wx.TOP, border = 5)
        hbox03.Add((50, -1))
        ncluf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'nclu%i'%self.nstruset)
        ncluf.SetMaxLength(132)
        hbox03.Add(ncluf, flag = wx.TOP, border = 5)
        ncluh = wx.StaticText(self, label = r"!")
        hbox03.Add(ncluh, flag = wx.TOP, border = 5)
        nrodt = wx.StaticText(self, label = "nrod", size = (45, -1))
        hbox03.Add(nrodt, flag = wx.TOP|wx.LEFT, border = 5)
        nrodf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'nrod%i'%self.nstruset)
        nrodf.SetMaxLength(132)
        hbox03.Add(nrodf, flag = wx.TOP, border = 5)
        nrodh = wx.StaticText(self, label = r"!")
        hbox03.Add(nrodh, flag = wx.TOP, border = 5)
        shat = wx.StaticText(self, label = "shap", size = (45, -1))
        hbox03.Add(shat, flag = wx.TOP|wx.LEFT, border = 5)
        shaf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'shap%i'%self.nstruset)
        shaf.SetMaxLength(132)
        hbox03.Add(shaf, flag = wx.TOP, border = 5)
        shah = wx.StaticText(self, label = r"!")
        hbox03.Add(shah, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox03)

        hbox02 = wx.BoxSizer(wx.HORIZONTAL)
        prott = wx.StaticText(self, label = "prot", size = (45, -1))
        hbox02.Add(prott, flag = wx.TOP, border = 5)
        hbox02.Add((50, -1))

#         prot_btn  = wx.Button(self, label = '..', size = (30, -1))
#         prot_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'prot%i'%self.nstruset :\
#          self.LD_file(event, place))
#         prot_btn.SetToolTip(wx.ToolTip("Add a prototype structure file"))    
#         hbox02.Add(prot_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)

        protf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'prot%i'%self.nstruset)
        protf.SetMaxLength(132)
        hbox02.Add(protf, flag = wx.TOP, border = 5)
        proth = wx.StaticText(self, label = r"?")
        hbox02.Add(proth, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox02, 1, flag = wx.EXPAND)

        hbox04 = wx.BoxSizer(wx.HORIZONTAL)
        chemt = wx.StaticText(self, label = "chem", size = (45, -1))
        hbox04.Add(chemt, flag = wx.TOP, border = 5)
        hbox04.Add((50, -1))
        chemf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'chem%i'%self.nstruset)
        chemf.SetMaxLength(132)
        hbox04.Add(chemf, flag = wx.TOP, border = 5)
        chemh = wx.StaticText(self, label = r"?")
        hbox04.Add(chemh, flag = wx.TOP, border = 5)
#         natt = wx.StaticText(self, label = r"natc", size = (45, -1))
#         hbox04.Add(natt, flag = wx.TOP|wx.LEFT, border = 5)
#         natf = wx.TextCtrl(self, style = wx.TE_LEFT, size = (260, -1), name = 'natc%i'%self.nstruset)
#         hbox04.Add(natf, flag = wx.TOP, border = 5)
#         nath = wx.StaticText(self, label = "!")
#         hbox04.Add(nath, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox04)

        hbox05 = wx.BoxSizer(wx.HORIZONTAL)
#         sgt = wx.StaticText(self, label = "spgn", size = (45, -1))
#         hbox05.Add(sgt, flag = wx.TOP, border = 5)
#         hbox05.Add((50, -1))
#         sgf = wx.TextCtrl(self, style = wx.TE_RIGHT, name = 'spgn%i'%self.nstruset)
#         hbox05.Add(sgf, flag = wx.TOP, border = 5)
#         sgh = wx.StaticText(self, label = r"!")
#         hbox05.Add(sgh, flag = wx.TOP, border = 5)
        celt = wx.StaticText(self, label = "cell", size = (45, -1))
        hbox05.Add(celt, flag = wx.TOP, border = 5)
        hbox05.Add((50, -1))
        celf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'cell%i'%self.nstruset)
        celf.SetMaxLength(132)
        hbox05.Add(celf, flag = wx.TOP, border = 5)
        celh = wx.StaticText(self, label = r"?")
        hbox05.Add(celh, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox05, 1, flag = wx.EXPAND)

        hbox06 = wx.BoxSizer(wx.HORIZONTAL)
        mict = wx.StaticText(self, label = "micr", size = (45, -1))
        hbox06.Add(mict, flag = wx.TOP, border = 5)
        hbox06.Add((50, -1))
        micf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'micr%i'%self.nstruset)
        micf.SetMaxLength(132)
        hbox06.Add(micf, flag = wx.TOP, border = 5)
        mich = wx.StaticText(self, label = r"?")
        hbox06.Add(mich, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox06, 1, flag = wx.EXPAND)

        hbox07 = wx.BoxSizer(wx.HORIZONTAL)
        part = wx.StaticText(self, label = "parx", size = (45, -1))
        hbox07.Add(part, flag = wx.TOP, border = 5)
        hbox07.Add((10, -1))
        par_btn  = wx.Button(self, label = '..', size = (30, -1))
        par_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'parx%i'%self.nstruset:\
         self.LD_file(event, place))
        par_btn.SetToolTip(wx.ToolTip("Select a parameter file"))    
        hbox07.Add(par_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        parf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (260, -1), name = 'parx%i'%self.nstruset)
        parf.SetMaxLength(132)
        hbox07.Add(parf, flag = wx.TOP, border = 5)
        parh = wx.StaticText(self, label = r"!")
        hbox07.Add(parh, flag = wx.TOP, border = 5)
        self.strubox.Add(hbox07)

    def Add_Struc(self, event):
        self.Do_Struc()
        self.strubox.Layout()
        self.vbox.Layout()
        self.frame.Layout()

    def Del_Struc(self, event):
        if self.nstruset >= 2:
            wp = 2 + 8 * (self.nstruset - 1)
            for p in range(wp+7, wp-1, -1):
                self.strubox.Hide(p)
                self.strubox.Remove(p)
            self.nstruset -= 1
            self.strubox.Layout()
            self.vbox.Layout()
            self.frame.Layout()

    def Do_Ref(self):
        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        simt = wx.StaticText(self, label = "simu", size = (45, -1))
        hbox01.Add(simt, flag = wx.TOP, border = 5)
        hbox01.Add((40, -1))
        simf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (50, -1), name = 'simu')
        simf.SetMaxLength(132)
        hbox01.Add(simf, flag = wx.TOP, border = 5)
        simh = wx.StaticText(self, label = r"*")
        hbox01.Add(simh, flag = wx.TOP, border = 5)
        self.refbox.Add(hbox01)

        hbox02 = wx.BoxSizer(wx.HORIZONTAL)
        calmt = wx.StaticText(self, label = "calm", size = (45, -1))
        hbox02.Add(calmt, flag = wx.TOP, border = 5)
        hbox02.Add((40, -1))
        calmf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (50, -1), name = 'calm')
        calmf.SetMaxLength(132)
        hbox02.Add(calmf, flag = wx.TOP, border = 5)
        calmh = wx.StaticText(self, label = r"*")
        hbox02.Add(calmh, flag = wx.TOP, border = 5)
        self.refbox.Add(hbox02)

        hbox03 = wx.BoxSizer(wx.HORIZONTAL)
        rfilt = wx.StaticText(self, label = "rfil", size = (45, -1))
        hbox03.Add(rfilt, flag = wx.TOP, border = 5)
        rfil_btn  = wx.Button(self, label = '..', size = (30, -1))
        rfil_btn.Bind(wx.EVT_BUTTON, lambda event, place = 'rfil': self.LD_file(event, place))
        rfil_btn.SetToolTip(wx.ToolTip("Select a refinement file"))    
        hbox03.Add(rfil_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        rfilf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (250, -1), name = 'rfil')
        rfilf.SetMaxLength(132)
        hbox03.Add(rfilf, flag = wx.TOP, border = 5)
        rfilh = wx.StaticText(self, label = r"!")
        hbox03.Add(rfilh, flag = wx.TOP, border = 5)
        self.refbox.Add(hbox03)

        hbox04 = wx.BoxSizer(wx.HORIZONTAL)
        outt = wx.StaticText(self, label = "outs", size = (45, -1))
        hbox04.Add(outt, flag = wx.TOP, border = 5)
        hbox04.Add((40, -1))
#         out_btn  = wx.Button(self, label = '..', size = (30, -1))
#         out_btn.Bind(wx.EVT_BUTTON, self.LD_dat)
#         out_btn.SetToolTip(wx.ToolTip("Select an output file"))    
#         hbox04.Add(out_btn, flag = wx.TOP|wx.LEFT|wx.RIGHT, border = 5)
        outf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (250, -1), name = 'outs')
        outf.SetMaxLength(132)
        hbox04.Add(outf, flag = wx.TOP, border = 5)
        outh = wx.StaticText(self, label = r"*")
        hbox04.Add(outh, flag = wx.TOP, border = 5)
        self.refbox.Add(hbox04)

        hbox05 = wx.BoxSizer(wx.HORIZONTAL)
        rpdft = wx.StaticText(self, label = "rpdf", size = (45, -1))
        hbox05.Add(rpdft, flag = wx.TOP, border = 5)
        hbox05.Add((40, -1))
        rpdff = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (50, -1), name = 'rpdf')
        rpdff.SetMaxLength(132)
        hbox05.Add(rpdff, flag = wx.TOP, border = 5)
        rpdfh = wx.StaticText(self, label = r"*")
        hbox05.Add(rpdfh, flag = wx.TOP, border = 5)
        self.refbox.Add(hbox05)

    def get_SMProot(self, filein):
        smproot = filein
        dbnx = '00'
        shpx = 'SPH', 'QBE', 'PAR', 'HEX', 'CYL', 'CSH'
        shx = 'csdkmcoskdovnfvnjfknvfnvalkcdsl;mwp'
        ks = 0
        for s in shpx:
            ks += 1
            if s in filein:
                shx = s
                break
        i0 = filein.rfind(shx)
        if i0 > 0:
            if ks <= 2:
                smproot = filein[:i0-4]
                dbnx = '03'
            elif ks == 6:
                smproot = filein[:i0-11]
                dbnx = '05'
            elif (ks > 2 and ks < 6):
                smproot = filein[:i0-11]
                dbnx = '04'
        else:
            ii = filein.rfind('smp')
            smproot = filein[:ii-1]
            dbnx = '01'
        return smproot, dbnx

    def LD_file(self, event, place):
      """
      Create and show the Open FileDialog
      """
      fi = ''
      dlg = wx.FileDialog(self, message = "Choose one file", defaultDir = os.getcwd(),
      defaultFile = "", wildcard = '*', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
      if dlg.ShowModal()  ==  wx.ID_OK:
        fi =  dlg.GetPaths()[0]
        if len(fi) > 0:
            fi1 = fi
            if place[:2] == 'db': 
                fi0 = self.get_SMProot(fi.strip())
                fi1 = fi0[0]
                xx = wx.FindWindowByName('dbn%s'%place[2])
                xx.SetValue(fi0[1])
            xx = wx.FindWindowByName('%s'%place)
            xx.SetValue(fi1)
      dlg.Destroy()

    def LD_dwa(self, event):
        """
        something
        """
        fin, dwainfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one .dwa file", defaultDir = os.getcwd(), 
        defaultFile = "", wildcard = '*.dwa', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal()  ==  wx.ID_OK:
          fin = dlg.GetPaths()
        dlg.Destroy()
        if len(fin) > 0:
            fin = fin[0]
            SetPath(self, fin)
            dwaobj = reader(fin)
            gv.DWA = dwaobj
            dat_size = dwaobj.ndataset
            datgui_size = self.ndataset
            for i in range(datgui_size):
                for ij in range(1, self.du):
                    xx = wx.FindWindowByName('%s%i'%(self.dflags[ij], i+1))
                    xx.SetValue('')
            stru_size = dwaobj.nstructure
            strugui_size = self.nstruset
            for i in range(strugui_size):
                for ij in range(1, self.su):
                    xx = wx.FindWindowByName('%s%i'%(self.sflags[ij], i+1))
                    xx.SetValue('')
            ref_size = 1
            refgui_size = self.ru
            for i in range(refgui_size):
                    xx = wx.FindWindowByName('%s'%(self.rflags[i]))
                    xx.SetValue('')
            ##__name
            dwaf = dwaobj.dwafile
            dwan0 = dwaf.rpartition('.dwa')[0]
            dwan = dwan0.rpartition(gv.SEP)[-1]
            self.namef.SetValue(dwan)
            ##__datasets
            while dat_size < datgui_size:
                self.Del_Dataset(event)
                datgui_size = self.ndataset
            while dat_size > datgui_size:
                self.Add_Dataset(event)
                datgui_size = self.ndataset
            if dat_size  ==  datgui_size:
                for i in range(datgui_size):
                    ii = self.du*i
                    for ij in range(1, self.du):
                        if dwaobj.ndataset > 0:
                            key = '%s%i'%(self.dflags[ij], i+1)
                            if key in dwaobj.dwainfo:
                                xx = wx.FindWindowByName('%s%i'%(self.dflags[ij], i+1))
                                if 'inst' in key:
                                    sx = dwaobj.dwainfo['%s%i'%(self.dflags[ij], i+1)].split()
                                    xx.SetValue(sx[0])
                                    xx = wx.FindWindowByName('%s%i'%('inpv', i+1))
                                    xx.SetValue(' '.join(sx[1:7]))
                                    xx = wx.FindWindowByName('%s%i'%('izxw', i+1))
                                    xx.SetValue(' '.join(sx[7:10]))
                                    ##__here for a later use
                                    # xx = wx.FindWindowByName('%s%i'%('i000', i+1))
                                    # xx.SetValue(' '.join(sx[10:]))
                                else:
                                    xx.SetValue(dwaobj.dwainfo['%s%i'%(self.dflags[ij], i+1)])
            ##__structures
            while stru_size < strugui_size:
                self.Del_Struc(event)
                strugui_size = self.nstruset
            while stru_size > strugui_size:
                self.Add_Struc(event)
                strugui_size = self.nstruset
            if stru_size  ==  strugui_size:
                for i in range(strugui_size):
                    ii = self.su*i
                    for ij in range(1, self.su):
                        if dwaobj.nstructure > 0:
                            key = '%s%i'%(self.sflags[ij], i+1)
                            if key in dwaobj.dwainfo:
                                xx = wx.FindWindowByName('%s%i'%(self.sflags[ij], i+1))
                                xx.SetValue(dwaobj.dwainfo['%s%i'%(self.sflags[ij], i+1)])
            ##__refinement
            for i in range(refgui_size):
                if dwaobj.nrefinement > 0:
                    key = '%s'%(self.rflags[i])
                    if key in dwaobj.dwainfo:
                        xx = wx.FindWindowByName('%s'%(self.rflags[i]))
                        xx.SetValue(dwaobj.dwainfo['%s'%(self.rflags[i])])
            del(xx)

    def SAV_dwa(self, event):
        fout = ''
        dlg = wx.FileDialog(
            self, message = "Save file as ...", defaultDir = os.getcwd(), 
            defaultFile = "", wildcard = '*.dwa', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dlg.ShowModal()  ==  wx.ID_OK:
            fout = dlg.GetPath()
            dlg.Destroy()
        if len(fout) > 0:
            SetPath(self, fout)
            try:
                dwaout = open(fout, 'w')
            except IOError:
                print("Error: can\'t find file or write data")
            else:
                ##__datasets
                print("************ Datasets section ************", file = dwaout)
                datgui_size = self.ndataset
                for i in range(datgui_size):
                    xx = wx.FindWindowByName('%s%i'%(self.dflags[0], i+1))
                    yy = wx.FindWindowByName('%s%i'%(self.dflags[1], i+1))
                    print('#%s%8s%s'%(xx.GetLabel(), ' ', yy.GetValue()), file = dwaout)
                    for ij in range(2, self.du):
                        xx = wx.FindWindowByName('%s%i'%(self.dflags[ij], i+1))
                        xxv = xx.GetValue()
                        if len(xx.GetValue()) > 0:
                            if 'inst' in self.dflags[ij]:
                                xx1v = ''
                                xx1 = wx.FindWindowByName('%s%i'%('inpv', i+1))
                                xx1v += ' ' + xx1.GetValue()
                                xx1 = wx.FindWindowByName('%s%i'%('izxw', i+1))
                                xx1v += ' ' + xx1.GetValue()
                                ##__here for a later use
                                # xx1 = wx.FindWindowByName('%s%i'%('i000', i+1))
                                # xx1v += xx1.GetValue()
                                xx1v += ' 0.00 0.00 0.00'
                                xxv += ' ' + xx1v
                            print('%2s%s%4s%s'%(' ', self.dflags[ij], ' ', xxv), file = dwaout)
                ##__structures
                print("************ Structures section ************", file = dwaout)
                strugui_size = self.nstruset
                for i in range(strugui_size):
                    xx = wx.FindWindowByName('%s%i'%(self.sflags[0], i+1))
                    yy = wx.FindWindowByName('%s%i'%(self.sflags[1], i+1))
                    print('%s%s%8s%s'%(self.sflags[0], xx.GetLabel(), ' ', yy.GetValue()), file = dwaout)
                    xx = wx.FindWindowByName('%s%i'%(self.sflags[2], i+1))
                    yy = wx.FindWindowByName('%s%i'%(self.sflags[3], i+1))
                    if (len(xx.GetValue()) and len(yy.GetValue())) > 0:
                        print('%2s%s%s%4s%s'%(' ', self.sflags[3], xx.GetValue(), ' ', yy.GetValue()), file = dwaout)
                    for ij in range(4, self.su):
                        xx = wx.FindWindowByName('%s%i'%(self.sflags[ij], i+1))
                        if len(xx.GetValue()) > 0:
                            print('%2s%s%4s%s'%(' ', self.sflags[ij], ' ', xx.GetValue()), file = dwaout)
                ##__structures
                print("************ Refinement section ************", file = dwaout)
                refgui_size = self.ru
                for i in range(refgui_size):
                    xx = wx.FindWindowByName('%s'%(self.rflags[i]))
                    if len(xx.GetValue()):
                        print('%2s%s%4s%s'%(' ', self.rflags[i], ' ', xx.GetValue()), file = dwaout)
                dwaout.close()
                gv.DWA = reader(fout)
                del(xx)
                del(yy)

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'Debussy Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
class PARX(wx.lib.scrolledpanel.ScrolledPanel):
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        if gset.Platform.startswith('dar'):
            font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)
            
        self.frame = parent
        self.vbox = wx.BoxSizer(wx.VERTICAL)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hbox01.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .par')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_par)
        ldin_btn.SetToolTip(wx.ToolTip("Load input information from a .par file"))    
        hbox01.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAVE_par)
        svf_btn.SetToolTip(wx.ToolTip("Save input values to a .par file"))    
        hbox01.Add(svf_btn, flag = wx.ALL, border = 3)
        self.vbox.Add(hbox01, flag = wx.ALIGN_RIGHT)

        hbox0n = wx.BoxSizer(wx.HORIZONTAL)
        namet = wx.StaticText(self, label = "Name")
        hbox0n.Add(namet)
        self.namef = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (160, -1))
        self.namef.SetMaxLength(132)
        hbox0n.Add(self.namef)
        extt = wx.StaticText(self, label = ".par")
        hbox0n.Add(extt)
        #hbox0n.Add((260, -1))
        self.cbbpar = wx.CheckBox(self, label='auto update *.par with *Best.par', size=(-1,-1))
        self.cbbpar.SetValue(False)
        hbox0n.Add(self.cbbpar, flag = wx.RIGHT)

        self.vbox.Add(hbox0n, flag = wx.EXPAND)

        self.parbox = wx.BoxSizer(wx.VERTICAL)
        self.parwidget = 0

        self.collbl = [' ', 'LOWER_BOUND', 'VALUE', 'UPPER_BOUND', 'REF_FLAG']
        self.ncols = len(self.collbl)
        
        ##__size distribution grid
        sizet = wx.StaticText(self, label = "Size distribution")
        self.parbox.Add(sizet)
        self.parwidget += 1

        self.size_flags = 'AV1LN SD1LN AV2LN SD2LN PHILN'.split()
        self.nrows_size = len(self.size_flags)+1
        self.size_grid = wx.grid.Grid(self, -1)
        self.size_grid.CreateGrid(self.nrows_size, self.ncols+3)
        self.size_grid.SetRowLabelSize(0) 
        self.size_grid.SetColLabelSize(0)
        self.bgc = self.size_grid.GetLabelBackgroundColour()
        
        for row in range(0, self.nrows_size):
            self.size_grid.SetCellBackgroundColour(row, 0, self.bgc)
            if row > 0 : 
                self.size_grid.SetCellValue(row, 0, self.size_flags[row-1])
                self.size_grid.SetReadOnly(row, 0, True)
            for col in (1, 3, 5, 7):
                if row > 0: self.size_grid.SetCellFont(row, col, wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))
                self.size_grid.SetCellAlignment(row, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
                if col < 7: self.size_grid.SetCellSize(row, col, 1, 2)
        i = 0
        for col in (0, 1, 3, 5, 7):
            self.size_grid.SetCellBackgroundColour(0, col, self.bgc)
            self.size_grid.SetCellValue(0, col, self.collbl[i])
            self.size_grid.SetCellAlignment(0, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
            self.size_grid.SetReadOnly(0, col, True)
            i += 1
        self.parbox.Add(self.size_grid, flag = wx.TOP|wx.BOTTOM, border = 5)
        self.parwidget += 1
        self.Set_IniPars('size')
        

        ##__strain grid
        straint = wx.StaticText(self, label = "Strain distribution")
        self.parbox.Add(straint)
        self.parwidget += 1
        self.str_flags = 'STcod VALn1 -- STR_i STR_1 STR_C STR_W'.split()
        self.nrows_str = len(self.str_flags)
        self.str_grid = wx.grid.Grid(self, -1)
        self.str_grid.CreateGrid(self.nrows_str, self.ncols+3)
        self.str_grid.SetRowLabelSize(0) 
        self.str_grid.SetColLabelSize(0)
        self.bgc = self.str_grid.GetLabelBackgroundColour()
        
        for row in range(0, self.nrows_str):
            self.str_grid.SetCellBackgroundColour(row, 0, self.bgc)
            self.str_grid.SetCellValue(row, 0, self.str_flags[row])
            if row!= 2 : self.str_grid.SetReadOnly(row, 0, True)
            for col in (1, 3, 5, 7):
                if row !=  2: self.str_grid.SetCellFont(row, col, wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))
                self.str_grid.SetCellAlignment(row, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
                if col < 7: self.str_grid.SetCellSize(row, col, 1, 2)
        i = 0
        for col in (0, 1, 3, 5, 7):
            self.str_grid.SetCellBackgroundColour(2, col, self.bgc)
            self.str_grid.SetCellValue(2, col, self.collbl[i])
            self.str_grid.SetCellAlignment(2, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
            self.str_grid.SetReadOnly(2, col, True)
            i += 1
        self.parbox.Add(self.str_grid, flag = wx.TOP|wx.BOTTOM, border = 5)
        self.parwidget += 1
        self.Set_IniPars('strain')

        ##__atom stuff
        self.atx = []
        atot = wx.StaticText(self, label = "Atomic parameters")
        self.parbox.Add(atot)
        self.parwidget += 1
        hbox001 = wx.BoxSizer(wx.HORIZONTAL)
        ad_btn  = wx.Button(self, label = '+', size = (50, -1))
        ad_btn.Bind(wx.EVT_BUTTON, self.Add_Ato)
        ad_btn.SetToolTip(wx.ToolTip("Add one atom"))    
        hbox001.Add(ad_btn, flag = wx.TOP, border = 10)
        rm_btn  = wx.Button(self, label = '-', size = (50, -1))
        rm_btn.Bind(wx.EVT_BUTTON, self.Del_Ato)
        rm_btn.SetToolTip(wx.ToolTip("Remove one atom"))    
        hbox001.Add(rm_btn, flag = wx.TOP|wx.LEFT, border = 10)
        self.ato_numf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (40, -1))
        self.ato_numf.SetMaxLength(132)
        hbox001.Add(self.ato_numf, flag = wx.TOP|wx.LEFT, border = 10)
        self.parbox.Add(hbox001, flag = wx.ALIGN_RIGHT)
        self.parwidget += 1
        ##__atom grid
        self.ato_flags = 'ATO -- OKK_I OKK_0 OKK_L BTH_I BTH_0 BTH_L'.split()
        self.nrows_ato = len(self.ato_flags)
        self.lawlbl = 'OKK_law BTH_law'.split()
        ##__create the grid
        self.atom_grid = wx.grid.Grid(self, -1)
        self.atom_grid.CreateGrid(self.nrows_ato, self.ncols+3)
        self.atom_grid.SetRowLabelSize(0) 
        self.atom_grid.SetColLabelSize(0)
        self.bgc = self.atom_grid.GetLabelBackgroundColour()
        self.parbox.Add(self.atom_grid, flag = wx.TOP|wx.BOTTOM, border = 5)
        self.parwidget += 1
        

        ##___add 1st atom
        self.nato = 1
        atom = self.Do_Ato()
        self.Set_IniPars('atom')

        self.vbox.Add(self.parbox, flag = wx.TOP|wx.ALIGN_CENTRE, border = 5)

        self.SetSizer(self.vbox)

#-------------------------------------------------------------------

    def Clear_pars(self):
        ##__name
        self.namef.SetValue('')
#         ##__size_grid
#         self.size_grid.SelectBlock(1, 1, self.nrows_size-1, self.ncols+3)
#         self.size_grid.ClearSelection()
#         ##__strain_grid
#         self.str_grid.SelectBlock(1, 1, self.nrows_size-1, self.ncols+3)
#         self.str_grid.ClearSelection()
        ##__size_grid
        for row in range(1, self.nrows_size):
            for col in (1, 3, 5, 7):
                self.size_grid.SetCellValue(row, col, '')
        ##__strain_grid
        for row in range(2):
            self.str_grid.SetCellValue(row, 1, '')
        for row in range(3, self.nrows_str):
            for col in (1, 3, 5, 7):
                self.str_grid.SetCellValue(row, col, '')
        ##__atom_grid
        rowa0 = (self.nato-1)*self.nrows_ato
        rowa1 = rowa0+self.nrows_ato
        for i in range(self.nato):
            r = (i)*self.nrows_ato
            self.atom_grid.SetCellValue(r, 2, '')
            self.atom_grid.SetCellValue(r, 4, '')
            for row in range(rowa0+2, rowa1):
                for col in (1, 3, 5, 7):
                    self.atom_grid.SetCellValue(row, col, '')

    def Set_IniPars(self, xgrid):
        ##__size_grid
        if xgrid == 'size':
            avln_ipar = ['0.5000000000000000', '5.0000000000000000', '15.000000000000000', '0']
            sdln_ipar = ['0.5000000000000000', '2.0000000000000000', '10.000000000000000', '0']
            phln_ipar = ['-90.00000000000000', '45.000000000000000', '90.000000000000000', '0']
            size_ipar = [avln_ipar, sdln_ipar, avln_ipar, sdln_ipar, phln_ipar]
            i = 0
            for row in range(1, self.nrows_size):
                for col in (1, 3, 5, 7):
                    self.size_grid.SetCellValue(row, col, size_ipar[i][int((col-1)/2)])
                i += 1
        ##__strain_grid
        if xgrid == 'strain':
            stco_ipar = '1'
            valn_ipar = '3'
            stri_ipar = ['0.9900000000000000', '1.0000000000000000', '1.0100000000000000', '0']
            str1_ipar = ['0.9900000000000000', '1.0000000000000000', '1.0100000000000000', '0']
            strc_ipar = ['0.9900000000000000', '1.0000000000000000', '1.0100000000000000', '0']
            strw_ipar = ['0.9900000000000000', '1.0000000000000000', '1.0100000000000000', '0']
            strain_ipar = [stco_ipar, valn_ipar, stri_ipar, str1_ipar, strc_ipar, strw_ipar]
            for row in range(2):
                self.str_grid.SetCellValue(row, 1, strain_ipar[row])
            for row in range(3, self.nrows_str):
                for col in (1, 3, 5, 7):
                    self.str_grid.SetCellValue(row, col, strain_ipar[row-1][int((col-1)/2)])
        ##__atom_grid
        if xgrid == 'atom':
            olaw_ipar = '1'
            blaw_ipar = '1'
            okki_ipar = ['0.0000000000000000', '1.0000000000000000', '1.0000000000000000', '0']
            okk0_ipar = ['0.0000000000000000', '1.0000000000000000', '1.0000000000000000', '0']
            okkl_ipar = ['1.0000000000000000', '100.00000000000000', '1000.0000000000000', '0']
            bthi_ipar = ['0.3000000000000000', '0.5000000000000000', '2.0000000000000000', '0']
            bth0_ipar = ['0.3000000000000000', '0.5000000000000000', '2.0000000000000000', '0']
            bthl_ipar = ['1.0000000000000000', '100.00000000000000', '1000.0000000000000', '0']
            ato_ipar = [olaw_ipar, blaw_ipar, okki_ipar, okk0_ipar, okkl_ipar, bthi_ipar, bth0_ipar, bthl_ipar]
            rowa0 = (self.nato-1)*self.nrows_ato
            rowa1 = rowa0+self.nrows_ato
            r = (self.nato-1)*self.nrows_ato
            self.atom_grid.SetCellValue(r, 2, ato_ipar[0])
            self.atom_grid.SetCellValue(r, 4, ato_ipar[1])
            j = 1
            for row in range(rowa0+2, rowa1):
                j += 1
                for col in (1, 3, 5, 7):
                    self.atom_grid.SetCellValue(row, col, ato_ipar[j][int((col-1)/2)])


    def Do_Ato(self):
        rowa0 = (self.nato-1)*self.nrows_ato
        rowa1 = rowa0+self.nrows_ato
        i = 0
        for row in range(rowa0, rowa1):
            self.atom_grid.SetCellBackgroundColour(row, 0, self.bgc)
            if row == rowa0 : self.atom_grid.SetCellValue(row, 0, self.ato_flags[i]+'%02i'%self.nato)
            else : self.atom_grid.SetCellValue(row, 0, self.ato_flags[i])
            if row!= rowa0+1 : self.atom_grid.SetReadOnly(row, 0, True)
            if row > rowa0:
                for col in (1, 3, 5, 7):
                    if row > rowa0+1: self.atom_grid.SetCellFont(row, col, wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))
                    self.atom_grid.SetCellAlignment(row, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
                    if col < 7 : self.atom_grid.SetCellSize(row, col, 1, 2)
            i += 1
        i = 0
        for col in (0, 1, 3, 5, 7):
            self.atom_grid.SetCellBackgroundColour(rowa0+1, col, self.bgc)
            self.atom_grid.SetCellValue(rowa0+1, col, self.collbl[i])
            self.atom_grid.SetCellAlignment(rowa0+1, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
            self.atom_grid.SetReadOnly(rowa0+1, col, True)
            i += 1
        i = 0
        for col in (1, 3):
            self.atom_grid.SetCellBackgroundColour(rowa0, col, self.bgc)
            self.atom_grid.SetCellValue(rowa0, col, self.lawlbl[i])
            self.atom_grid.SetCellAlignment(rowa0, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
            self.atom_grid.SetReadOnly(rowa0, col, True)
            i += 1
        self.atom_grid.SetCellFont(rowa0, 2, wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))
        self.atom_grid.SetCellFont(rowa0, 4, wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))

    def Add_Atox(self):
        self.nato += 1
        self.atom_grid.AppendRows(self.nrows_ato)
        self.Do_Ato()
        self.Set_IniPars('atom')
        self.parbox.Layout()
        self.vbox.Layout()
        self.Layout()
        self.frame.Layout()

    def Add_Ato(self, event):
        self.nato += 1
        self.atom_grid.AppendRows(self.nrows_ato)
        self.Do_Ato()
        self.Set_IniPars('atom')
        self.parbox.Layout()
        self.vbox.Layout()
        self.Layout()
        self.frame.Layout()

    def Del_Atox(self):
        if self.nato > 1:
            rowa0 = (self.nato-1)*self.nrows_ato
            self.atom_grid.DeleteRows(rowa0, self.nrows_ato)
            self.nato -= 1
            self.parbox.Layout()
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()

    def Del_Ato(self, event):
        if self.nato > 1:
            rowa0 = (self.nato-1)*self.nrows_ato
            self.atom_grid.DeleteRows(rowa0, self.nrows_ato)
            self.nato -= 1
            self.parbox.Layout()
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()

    def LD_par(self, event):
        """
        something
        """
        self.parfile, bestpar, parinfo = '', '', ()
        if self.cbbpar.GetValue() == True:
            dlg = wx.FileDialog(self, message = "Choose one _Best.par file", defaultDir = os.getcwd(), 
            defaultFile = "", wildcard = '*.par', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
            if dlg.ShowModal()  ==  wx.ID_OK:
              bestpar = dlg.GetPaths()[0]
            dlg.Destroy()
            if len(bestpar) > 0:
                if not 'Best' in bestpar: 
                    xx = wx.FindWindowByName('DebussyBuffer')
                    toBuffer(self, xx, '\n  Auto update load *_Best.par : it seems you did not select a *_Best par file, auto update will not be performed!')
                    del xx
                else:
                    ib = bestpar.find('Best')
                    self.parfile = bestpar[:ib-3]+'.par'
                    shutil_copy2(bestpar, self.parfile)
        else:
            dlg = wx.FileDialog(self, message = "Choose one .par file", defaultDir = os.getcwd(), 
            defaultFile = "", wildcard = '*.par', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
            if dlg.ShowModal()  ==  wx.ID_OK:
              self.parfile = dlg.GetPaths()[0]
            dlg.Destroy()
        if len(self.parfile) > 0:
            SetPath(self, self.parfile)
            parinfo = reader(self.parfile)
            ##__clear parameters cells
            self.Clear_pars()
            ##__check atom number
            par_nato = len(parinfo.atox)
            if self.nato > par_nato:
                if verbose : print('removing %i atom(s) from the grid'%(self.nato-par_nato))
                while self.nato > par_nato:
                    self.Del_Atox()
            elif self.nato < par_nato:
                if verbose : print('adding %i atom(s) to the grid'%(-self.nato+par_nato))
                while self.nato < par_nato:
                    self.Add_Atox()
            ##__load values
            ##__name
            f0 = self.parfile.rpartition('.par')[0]
            fn = f0.rpartition(gv.SEP)[-1]
            self.namef.SetValue(fn)
            ##__size_grid
            for row in range(1, self.nrows_size):
                xx = parinfo.dis[row-1].split()
                i = 0
                for col in range(1,9,2):
                    self.size_grid.SetCellValue(row, col, xx[i])
                    i += 1
            ##__strain_grid
            for row in range(2):
                self.str_grid.SetCellValue(row, 1, parinfo.str[row])
            for row in range(3, self.nrows_str):
                xx = parinfo.str[row-1].split()
                i = 0
                for col in (1, 3, 5, 7):
                    self.str_grid.SetCellValue(row, col, xx[i])
                    i += 1
            ##__atom_grid
            for i in range(self.nato):
                rowa0 = i * self.nrows_ato
                rowa1 = rowa0 + self.nrows_ato
                r = i * self.nrows_ato
                xx = parinfo.atox[i][0].split()
                self.atom_grid.SetCellValue(r, 2, xx[0])
                self.atom_grid.SetCellValue(r, 4, xx[1])
                j = 1
                for row in range(rowa0+2, rowa1):
                    jj = 0
                    for col in (1, 3, 5, 7):
                        xx = parinfo.atox[i][j].split()
                        self.atom_grid.SetCellValue(row, col, xx[jj])
                        jj += 1
                    j += 1

    def Bestpar2par(self, event):
        """
        Retrieves the *_Best.par file of a given structure (.par file)
        saves it as new .par file (overwriting) and loads it.
        """
        self.parfile = ''
        xx = wx.FindWindowByName('DebussyBuffer')
        if len(self.namef.GetValue()) > 0:
            parstr = self.namef.GetValue()
            if type(gv.DWA) == str: 
                toBuffer(self, xx, '\n  load *_Best.par : .dwa file not set, cannot retrieve necessary information, please edit!')
                return
            for i in range(gv.DWA.nstructure):
                if parstr == gv.DWA.dwainfo['parx%i'%(i+1)].rpartition('.par')[0]:
                    shutil_copy2(parstr+'%02i_Best.par'%(i+1),gv.DWA.dwainfo['parx%i'%(i+1)])
                    self.parfile = gv.DWA.dwainfo['parx%i'%(i+1)]
                else:
                    toBuffer(self, xx, '\n  load *_Best.par : Name field in .par does not match any parx filed in %s, please check!'%(gv.DWA.dwafile.rpartition(gv.SEP)[-1]))
        else:
            toBuffer(self, xx, '\n  load *_Best.par : Name field empty -- cannot retrieve files, please edit!')
        

    def SAVE_par(self, event):
        fout = ''
        dlg = wx.FileDialog(
            self, message = "Save file as ...", defaultDir = os.getcwd(),
            defaultFile = "", wildcard = '*.par', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dlg.ShowModal()  ==  wx.ID_OK:
            fout = dlg.GetPath()
        dlg.Destroy()
        if len(fout) > 0:
            SetPath(self, fout)
            try:
                parout = open(fout, 'w')
            except IOError:
                print("Error: can\'t find file or write data")
            else:
                for row in range(2):
                    print('%5s%9s%s'%(self.str_flags[row], ' ', self.str_grid.GetCellValue(row, 1)), file = parout)
                for row in range(1, self.nrows_size):
                    xx = ''
                    for col in (1, 3, 5):
                        s0 = self.size_grid.GetCellValue(row, col)
                        f0 = float(s0)
                        xx += '%6s%18f'%(' ', f0)
                    for col in [7]:
                        s0 = self.size_grid.GetCellValue(row, col)
                        xx += '%6s%s'%(' ', s0)
                    print('%5s%s'%(self.size_flags[row-1], xx), file = parout)
                for row in range(3, self.nrows_str):
                    xx = ''
                    for col in (1, 3, 5):
                        s0 = self.str_grid.GetCellValue(row, col)
                        f0 = float(s0)
                        xx += '%6s%18f'%(' ', f0)
                    for col in [7]:
                        s0 = self.str_grid.GetCellValue(row, col)
                        xx += '%6s%s'%(' ', s0)
                    print('%5s%s'%(self.str_flags[row], xx), file = parout)
                for i in range(self.nato):
                    rowa0 = i * self.nrows_ato
                    rowa1 = rowa0 + self.nrows_ato
                    r = i * self.nrows_ato
                    xx = 'ATO%02i'%(i+1)
                    xx += '%9s%s'%(' ', self.atom_grid.GetCellValue(r, 2))
                    xx += '%4s%s'%(' ', self.atom_grid.GetCellValue(r, 4))
                    print('%s'%xx, file = parout)
                    j = 2
                    for row in range(rowa0+2, rowa1):
                        xx = ''
                        for col in (1, 3, 5):
                            s0 = self.atom_grid.GetCellValue(row, col)
                            f0 = float(s0)
                            xx += '%6s%18f'%(' ', f0)
                        for col in [7]:
                            s0 = self.atom_grid.GetCellValue(row, col)
                            xx += '%6s%s'%(' ', s0)
                        print('%5s%s'%(self.ato_flags[j], xx), file = parout)
                        j += 1
                parout.close()

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'CLaUDe Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
algo = ('--', 'COMPLEX','ANNEAL', 'NELMEA', 'BOBYQA')
class REFX(wx.lib.scrolledpanel.ScrolledPanel):
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        if gset.Platform.startswith('dar'):
            font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)

        self.frame = parent
        self.vbox = wx.BoxSizer(wx.VERTICAL)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hbox01.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .ref')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_ref)
        ldin_btn.SetToolTip(wx.ToolTip("Load input information from a .ref file"))    
        hbox01.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAVE_ref)
        svf_btn.SetToolTip(wx.ToolTip("Save input values to a .ref file"))    
        hbox01.Add(svf_btn, flag = wx.ALL, border = 3)
        self.vbox.Add(hbox01, flag = wx.ALIGN_RIGHT)

        hbox0n = wx.BoxSizer(wx.HORIZONTAL)
        namet = wx.StaticText(self, label = "Name")
        hbox0n.Add(namet)
        self.namef = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (160, -1))
        self.namef.SetMaxLength(132)
        hbox0n.Add(self.namef)
        extt = wx.StaticText(self, label = ".ref")
        hbox0n.Add(extt)
        self.vbox.Add(hbox0n)

        hbox_s0 = wx.BoxSizer(wx.HORIZONTAL)
        add_stage_btn  = wx.Button(self, label = '+', size = (50, -1))
        add_stage_btn.Bind(wx.EVT_BUTTON, self.Add_StageX)
        add_stage_btn.SetToolTip(wx.ToolTip("Add one Stage"))    
        hbox_s0.Add(add_stage_btn, flag = wx.TOP, border = 10)
        rem_stage_btn  = wx.Button(self, label = '-', size = (50, -1))
        rem_stage_btn.Bind(wx.EVT_BUTTON, self.Del_StageX)
        rem_stage_btn.SetToolTip(wx.ToolTip("Remove one Satge"))    
        hbox_s0.Add(rem_stage_btn, flag = wx.TOP|wx.LEFT, border = 10)
        self.vbox.Add(hbox_s0, flag = wx.ALIGN_LEFT)

        self.ref = []
        self.nstage = 0
        self.wbs_count = 0
        self.Do_Stage()


        self.SetSizer(self.vbox)

#-------------------------------------------------------------------

    def Do_Stage(self):
        self.ref += [[[], 0]]
        xstage = len(self.ref)
        xstruc = len(self.ref[xstage-1][0])
        stage_box = wx.BoxSizer(wx.VERTICAL)
        hbox_sx = wx.BoxSizer(wx.HORIZONTAL)
        staget = wx.StaticText(self, label = "Stage    # :")
        hbox_sx.Add(staget, flag = wx.TOP|wx.LEFT, border = 10)
        stagenf = wx.StaticText(self, label = "%i"%(xstage))
        hbox_sx.Add(stagenf, flag = wx.TOP|wx.LEFT, border = 10)
        tolt = wx.StaticText(self, label = "tol")
        hbox_sx.Add(tolt, flag = wx.TOP|wx.LEFT, border = 10)
        tolf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (160, -1), name = 'stage%i_tol'%xstage)
        tolf.SetMaxLength(132)
        hbox_sx.Add(tolf, flag = wx.TOP|wx.LEFT, border = 10)
        alg_btn = wx.ComboBox(self, id = -1, size = (150, -1), choices = algo, style = wx.CB_READONLY, name = 'stage%i_alg'%xstage)
        hbox_sx.Add(alg_btn, flag = wx.TOP|wx.LEFT, border = 8)
        hbox_sx.Add((50, -1))
        add_stru_btn  = wx.Button(self, label = '+', size = (30, -1))
        add_stru_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i%i'%(xstage, xstruc): self.Add_StruX(event, place))
        add_stru_btn.SetToolTip(wx.ToolTip("Add one Structure"))    
        hbox_sx.Add(add_stru_btn, flag = wx.TOP, border = 10)
        rem_stru_btn  = wx.Button(self, label = '-', size = (30, -1))
        rem_stru_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i%i'%(xstage, xstruc): self.Del_StruX(event, place))
        rem_stru_btn.SetToolTip(wx.ToolTip("Remove one Structure"))    
        hbox_sx.Add(rem_stru_btn, flag = wx.TOP|wx.LEFT, border = 10)        
        stage_box.Add(hbox_sx, wx.ALIGN_RIGHT)
        place = '%i%i'%(xstage, xstruc)
        self.Do_Dataset(stage_box, place)
        self.Do_Structure(stage_box, place)
        self.vbox.Add(stage_box, userData = 'stage%i'%xstage)

    def Add_StageX(self, event):
        if verbose : print('ref 0', self.ref)
        self.Do_Stage()
        self.vbox.Layout()
        self.Layout()
        self.frame.Layout()
        if verbose : print('ref 1', self.ref)

    def Del_StageX(self, event):
        if verbose : print('ref 0', self.ref)
        istage = len(self.ref)
        if istage  >= 2:
            self.vbox.Hide(istage+2)
            self.vbox.Remove(istage+2)
            del(self.ref[istage-1])
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()
            if verbose : print('ref 1', self.ref)

    def Find_StageX(self, place):
        if verbose : print('place ', place)
        istage = int(place[0])
        istruc = int(place[1])
        c = self.vbox.GetChildren()
        for ic in c:
            ud = ic.GetUserData()
            uud = str(ud)
            if verbose : print(uud)
            if uud[:6] == 'stage%s'%place[0]:
                if verbose : print('***stage***')
                stagex = ic.GetSizer() ## from sizeritem to subsizer
        return stagex

    def Add_StruX(self, event, place):
        if verbose : print('place ', place)
        if verbose : print('ref 0', self.ref)
        istage = int(place[0])
        istruc = int(place[1])
        c = self.vbox.GetChildren()
        for ic in c:
            ud = ic.GetUserData()
            uud = str(ud)
            if verbose : print(uud)
            if uud[:6] == 'stage%s'%place[0]:
                if verbose : print('***stage***')
                stagex = ic.GetSizer() ## from sizeritem to subsizer
                self.Do_Structure(stagex, place)
                stagex.Layout()
                self.vbox.Layout()
                self.Layout()
                self.frame.Layout()
                if verbose : print('ref 1', self.ref)

    def Del_StruX(self, event, place):
        if verbose : print('place ', place)
        if verbose : print('ref 0', self.ref)
        istage = int(place[0])
        istruc = int(place[1])
        xstruc = len(self.ref[istage-1][0])
        if xstruc >= 2:
            c = self.vbox.GetChildren()
            for ic in c:
                ud = ic.GetUserData()
                uud = str(ud)
                if verbose : print(uud)
                if uud[:6] == 'stage%s'%place[0]:
                    if verbose : print('***stage***')
                    stagex = ic.GetSizer() ## from sizeritem to subsizer
                    stagex.Hide(xstruc)
                    stagex.Remove(xstruc)
                    del(self.ref[istage-1][0][xstruc-1])
            stagex.Layout()
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()
            if verbose : print('ref 1', self.ref)


    def Do_Structure(self, stgb, pos):
        
        istage = int(pos[0])
        istruc = int(pos[1])
        self.ref[istage-1][0] += [0]
        xstruc = len(self.ref[istage-1][0])
        
        struc_box = wx.BoxSizer(wx.VERTICAL)
        hbox_stru0 = wx.BoxSizer(wx.HORIZONTAL)
        hbox_stru0.Add((50, -1))
        strut = wx.StaticText(self, label = "Structure    %")
        hbox_stru0.Add(strut, flag = wx.TOP, border = 10)
        strunt = wx.StaticText(self, label = "%i"%(xstruc))
        hbox_stru0.Add(strunt, flag = wx.TOP, border = 10)
        struc_box.Add(hbox_stru0)
        hbox_stru1 = wx.BoxSizer(wx.HORIZONTAL)
        hbox_stru1.Add((50, -1))
        add_ato_btn  = wx.Button(self, label = '+', size = (30, -1))
        place = '%i%i'%(istage, xstruc)
        add_ato_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i%i'%(istage, xstruc) :self.Add_AtomX(event, place))
        add_ato_btn.SetToolTip(wx.ToolTip("Add one atom"))    
        hbox_stru1.Add(add_ato_btn, flag = wx.TOP, border = 5)
        del_ato_btn  = wx.Button(self, label = '-', size = (30, -1))
        del_ato_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i%i'%(istage, xstruc) : self.Del_AtomX(event, place))
        del_ato_btn.SetToolTip(wx.ToolTip("Remove one atom"))    
        hbox_stru1.Add(del_ato_btn, flag = wx.TOP, border = 5)
        struc_box.Add(hbox_stru1)
        struc_box.Add((0, 5))
        gbs_sxd = wx.GridBagSizer(4, 4)
        sizet = wx.StaticText(self, label = "Size")
        gbs_sxd.Add(sizet, pos = (0, 0), flag = wx.ALIGN_CENTRE)
        sizet1 = wx.StaticText(self, label = "Dist.")
        gbs_sxd.Add(sizet1, pos = (1, 0), flag = wx.ALIGN_CENTRE)
        cb_size = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_size'%(istage, xstruc))
        cb_size.SetValue(False)
        gbs_sxd.Add(cb_size, pos = (2, 0), flag = wx.ALIGN_CENTRE)        
        straint = wx.StaticText(self, label = "Strain")
        gbs_sxd.Add(straint, pos = (0, 1), flag = wx.ALIGN_CENTRE)
        straint1 = wx.StaticText(self, label = "Dist.")
        gbs_sxd.Add(straint1, pos = (1, 1), flag = wx.ALIGN_CENTRE)
        cb_strain = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_strain'%(istage, xstruc))
        cb_strain.SetValue(False)
        gbs_sxd.Add(cb_strain, pos = (2, 1), flag = wx.ALIGN_CENTRE)
        self.ref[istage-1][0][xstruc-1] += 1
        xnato = self.ref[istage-1][0][xstruc-1]
        atomt = wx.StaticText(self, label = "ATO%02i"%xnato)
        gbs_sxd.Add(atomt, pos = (0, 2*xnato), span = (1, 2), flag = wx.ALIGN_CENTRE)
        okkt = wx.StaticText(self, label = "OKK")
        gbs_sxd.Add(okkt, pos = (1, 2*xnato), flag = wx.ALIGN_CENTRE)
        btht = wx.StaticText(self, label = "BTH")
        gbs_sxd.Add(btht, pos = (1, 2*xnato+1), flag = wx.ALIGN_CENTRE)
        cb_okk = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_ato%i_okk'%(istage, xstruc, xnato))
        cb_okk.SetValue(False)
        gbs_sxd.Add(cb_okk, pos = (2, 2*xnato), flag = wx.ALIGN_CENTRE)
        cb_bth = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_ato%i_bth'%(istage, xstruc, xnato))
        cb_bth.SetValue(False)
        gbs_sxd.Add(cb_bth, pos = (2, 2*xnato+1), flag = wx.ALIGN_CENTRE)
        struc_box.Add(gbs_sxd, flag = wx.LEFT, border = 50, userData = 'stage%i_struc%i_gbs'%(istage, xstruc))
        stgb.Insert(xstruc, struc_box, userData = 'stage%i_struc%i'%(istage, xstruc))


    def Add_AtomX(self, event, pos):
        istage = int(pos[0])
        istruc = int(pos[1])
        c = self.vbox.GetChildren()
        for ic in c:
            ud = ic.GetUserData()
            uud = str(ud)
            if verbose : print(uud)
            if uud[:6] == 'stage%s'%pos[0]:
                if verbose : print('***stage***')
                stagex = ic.GetSizer() ## from sizeritem to subsizer
                cc = stagex.GetChildren()
                for jc in cc:
                    ud = jc.GetUserData()
                    uud = str(ud)
                    if uud == 'stage%s_struc%s'%(pos[0], pos[1]):
                        if verbose : print('***struc***')
                        strucx = jc.GetSizer()
                        ccc = strucx.GetChildren()
                        for kc in ccc:
                            ud = kc.GetUserData()
                            uud = str(ud)
                            if uud == 'stage%s_struc%s_gbs'%(pos[0], pos[1]):
                                if verbose : print('***gbs***')
                                gbs = kc.GetSizer()
                                istage = int(pos[0])
                                istruc = int(pos[1])
                                self.ref[istage-1][0][istruc-1] += 1
                                xnato = self.ref[istage-1][0][istruc-1]
                                ###___add widgets
                                atomt = wx.StaticText(self, label = "ATO%02i"%xnato)
                                gbs.Add(atomt, pos = (0, 2*xnato), span = (1, 2), flag = wx.ALIGN_CENTRE)
                                
                                okkt = wx.StaticText(self, label = "OKK")
                                gbs.Add(okkt, pos = (1, 2*xnato), flag = wx.ALIGN_CENTRE)
                                
                                btht = wx.StaticText(self, label = "BTH")
                                gbs.Add(btht, pos = (1, 2*xnato+1), flag = wx.ALIGN_CENTRE)
                                
                                cb_okk = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_ato%i_okk'%(istage, istruc, xnato))
                                cb_okk.SetValue(False)
                                gbs.Add(cb_okk, pos = (2, 2*xnato), flag = wx.ALIGN_CENTRE)
                                
                                cb_bth = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_struc%i_ato%i_bth'%(istage, istruc, xnato))
                                cb_bth.SetValue(False)
                                gbs.Add(cb_bth, pos = (2, 2*xnato+1), flag = wx.ALIGN_CENTRE)
        strucx.Layout()
        stagex.Layout()
        self.vbox.Layout()
        self.Layout()
        self.frame.Layout()

    def Del_AtomX(self, event, pos):
        istage = int(pos[0])
        istruc = int(pos[1])
        xnato = self.ref[istage-1][0][istruc-1]
        if verbose : print('xnato ', xnato)
        if xnato >= 2:
            c = self.vbox.GetChildren()
            for ic in c:
                ud = ic.GetUserData()
                uud = str(ud)
                if verbose : print(uud)
                if uud[:6] == 'stage%s'%pos[0]:
                    if verbose : print('***stage***')
                    stagex = ic.GetSizer() ## from sizeritem to subsizer
                    cc = stagex.GetChildren()
                    for jc in cc:
                        ud = jc.GetUserData()
                        uud = str(ud)
                        if uud == 'stage%s_struc%s'%(pos[0], pos[1]):
                            if verbose : print('***struc***')
                            strucx = jc.GetSizer()
                            ccc = strucx.GetChildren()
                            for kc in ccc:
                                ud = kc.GetUserData()
                                uud = str(ud)
                                if uud == 'stage%s_struc%s_gbs'%(pos[0], pos[1]):
                                    if verbose : print('***gbs***')
                                    gbs = kc.GetSizer()
                                    wp = 5*xnato+1
                                    for i in range(wp+4, wp-1, -1):
                                        gbs.Hide(i)
                                        gbs.Remove(i)
                                    self.ref[istage-1][0][istruc-1] -= 1
                                    if verbose : print('atom ', self.ref[istage-1][0][istruc-1])
            strucx.Layout()
            stagex.Layout()
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()

    def Do_Dataset(self, stgb, pos):
        
        istage = int(pos[0])
        self.ref[istage-1][1] += 1
        xdataset = self.ref[istage-1][1]
        
        data_box = wx.BoxSizer(wx.VERTICAL)
        hbox_d0 = wx.BoxSizer(wx.HORIZONTAL)
        hbox_d0.Add((50, -1))
        datat = wx.StaticText(self, label = "Dataset    #")
        hbox_d0.Add(datat, flag = wx.TOP, border = 10)
        datant = wx.StaticText(self, label = "%i"%(xdataset), name = 'stage%i_data'%(istage))
        hbox_d0.Add(datant, flag = wx.TOP, border = 10)
        hbox_d0.Add((50, -1))
        add_data_btn  = wx.Button(self, label = '+', size = (30, -1))
        add_data_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i0'%(istage): self.Add_DataX(self, place))
        add_data_btn.SetToolTip(wx.ToolTip("Add one Dataset"))    
        hbox_d0.Add(add_data_btn, flag = wx.TOP, border = 5)
        del_data_btn  = wx.Button(self, label = '-', size = (30, -1))
        del_data_btn.Bind(wx.EVT_BUTTON, lambda event, place = '%i0'%(istage): self.Del_DataX(self, place))
        del_data_btn.SetToolTip(wx.ToolTip("Remove one Dataset"))    
        hbox_d0.Add(del_data_btn, flag = wx.TOP, border = 5)
        data_box.Add(hbox_d0, userData = 'stage%i_data0'%(istage))
        hbox_dd = wx.BoxSizer(wx.HORIZONTAL)
        hbox_dx = wx.BoxSizer(wx.VERTICAL)
        dxt = wx.StaticText(self, label = "1")
        hbox_dx.Add(dxt, flag = wx.ALIGN_CENTRE|wx.TOP, border = 10)
        cb_dx = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_data%i'%(istage, xdataset))
        cb_dx.SetValue(True)
        hbox_dx.Add(cb_dx, flag = wx.ALIGN_CENTRE|wx.TOP, border = 5)
        hbox_dd.Add(hbox_dx)
        data_box.Add(hbox_dd, flag = wx.LEFT, border = 50, userData = 'stage%i_dataX'%(istage))
        stgb.Add(data_box, userData = 'stage%i_data'%(istage))


    def Add_DataX(self, event, pos):
        if verbose : print('place ', pos)
        if verbose : print('ref 0', self.ref)
        istage = int(pos[0])
        istruc = int(pos[1])
        c = self.vbox.GetChildren()
        for ic in c:
            ud = ic.GetUserData()
            uud = str(ud)
            if verbose : print(uud)
            if uud[:6] == 'stage%s'%pos[0]:
                if verbose : print('***stage***')
                stagex = ic.GetSizer() ## from sizeritem to subsizer
                cc = stagex.GetChildren()
                for jc in cc:
                    ud = jc.GetUserData()
                    uud = str(ud)
                    if uud == 'stage%s_data'%(pos[0]):
                        if verbose : print('***data***')
                        self.ref[istage-1][1] += 1
                        xdataset = self.ref[istage-1][1]
                        datax = jc.GetSizer()
                        ccc = datax.GetChildren()
                        for kc in ccc:
                            ud = kc.GetUserData()
                            uud = str(ud)
                            if uud == 'stage%s_dataX'%(pos[0]):
                                if verbose : print('dataX')
                                zc = kc.GetSizer()
                                hbox_dx = wx.BoxSizer(wx.VERTICAL)
                                dxt = wx.StaticText(self, label = "%i"%xdataset)
                                hbox_dx.Add(dxt, flag = wx.ALIGN_CENTRE|wx.TOP, border = 10)
                                cb_dx = wx.CheckBox(self, label = '', size = (-1, -1), name = 'stage%i_data%i'%(istage, xdataset))
                                cb_dx.SetValue(True)
                                hbox_dx.Add(cb_dx, flag = wx.ALIGN_CENTRE|wx.TOP, border = 5)
                                zc.Add(hbox_dx, flag = wx.LEFT, border = 20)                
                        dnx = wx.FindWindowByName('stage%i_data'%(istage))
                        dnx.SetLabel("%i"%xdataset)
        zc.Layout()
        datax.Layout()
        stagex.Layout()
        self.vbox.Layout()
        self.Layout()
        self.frame.Layout()
        if verbose : print('ref 1', self.ref)



    def Del_DataX(self, event, pos):
        if verbose : print('place ', pos)
        if verbose : print('ref 0', self.ref)
        istage = int(pos[0])
        istruc = int(pos[1])
        c = self.vbox.GetChildren()
        xdataset = self.ref[istage-1][1]
        if xdataset >= 2:
            for ic in c:
                ud = ic.GetUserData()
                uud = str(ud)
                if verbose : print(uud)
                if uud[:6] == 'stage%s'%pos[0]:
                    if verbose : print('***stage***')
                    stagex = ic.GetSizer() ## from sizeritem to subsizer
                    cc = stagex.GetChildren()
                    for jc in cc:
                        ud = jc.GetUserData()
                        uud = str(ud)
                        if uud == 'stage%s_data'%(pos[0]):
                            if verbose : print('***data***')
                            datax = jc.GetSizer()
                            ccc = datax.GetChildren()
                            for kc in ccc:
                                ud = kc.GetUserData()
                                uud = str(ud)
                                if uud == 'stage%s_dataX'%(pos[0]):
                                    if verbose : print('dataX')
                                    zc = kc.GetSizer()
                                    zc.Hide(xdataset-1)
                                    zc.Remove(xdataset-1)
                                    self.ref[istage-1][1] -= 1
                                    xdataset = self.ref[istage-1][1]
                            dnx = wx.FindWindowByName('stage%i_data'%(istage))
                            dnx.SetLabel("%i"%xdataset)
            zc.Layout()
            datax.Layout()
            stagex.Layout()
            self.vbox.Layout()
            self.Layout()
            self.frame.Layout()
            if verbose : print('ref 1', self.ref)



    def LD_ref(self, event):
        """
        something
        """
        fin, refinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one .ref file", defaultDir = os.getcwd(), 
        defaultFile = "", wildcard = '*.ref', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal()  ==  wx.ID_OK:
            fin = dlg.GetPaths()
        dlg.Destroy()
        if len(fin) > 0:
            fin = fin[0]
            SetPath(self, fin)
            refinfo = reader(fin)
            f0 = fin.rpartition('.ref')[0]
            fin = f0.rpartition(gv.SEP)[-1]
            self.namef.SetValue(fin)
            gui_nstage = len(self.ref)
            ref_nstage = len(refinfo.ref)
            while gui_nstage < ref_nstage:
                if verbose : print('ADDING %i STAGE(S) ..'%(ref_nstage - gui_nstage))
                self.Add_StageX(event)
                gui_nstage = len(self.ref)
            while gui_nstage > ref_nstage:
                if verbose : print('REMOVING %i STAGE(S) ..'%(-ref_nstage + gui_nstage))
                self.Del_StageX(event)
                gui_nstage = len(self.ref)
            for i in range(gui_nstage):
                xx = wx.FindWindowByName('stage%i_tol'%(i+1))
                xx.SetValue("%s"%refinfo.ref[i][0])
                xx = wx.FindWindowByName('stage%i_alg'%(i+1))
                xx.SetValue("%s"%refinfo.ref[i][1])
                
                gui_nstruc = len(self.ref[i][0])
                ref_nstruc = len(refinfo.ref[i][2])
                while gui_nstruc < ref_nstruc:
                    if verbose : print('ADDING %i STRUCTURE(S)'%(ref_nstruc - gui_nstruc))
                    self.Add_StruX(event, place = '%i0'%(i+1))
                    gui_nstruc = len(self.ref[i][0])
                while gui_nstruc > ref_nstruc:
                    if verbose : print('REMOVING %i STRUCTURE(S)'%(-ref_nstruc + gui_nstruc))
                    self.Del_StruX(event, place = '%i0'%(i+1))
                    gui_nstruc = len(self.ref[i][0])
                
                for j in range(gui_nstruc):
                    gui_nato = self.ref[i][0][j]
                    ref_nato = (len(refinfo.ref[i][2][j].split()) - 2) / 2
                    while gui_nato < ref_nato:
                        if verbose : print('ADDING %i ATOM(S)'%(ref_nato - gui_nato))
                        self.Add_AtomX(event, pos = '%i%i'%(i+1, j+1))
                        gui_nato = self.ref[i][0][j]
                    while gui_nato > ref_nato:
                        if verbose : print('REMOVING %i ATOM(S)'%(-ref_nato + gui_nato))
                        self.Del_AtomX(event, pos = '%i%i'%(i+1, j+1))
                        gui_nato = self.ref[i][0][j]
                    ref_stru = refinfo.ref[i][2][j].split()
                    xx = wx.FindWindowByName('stage%i_struc%i_size'%(i+1, j+1))
                    if ref_stru[0] == '1' : xx.SetValue(True)
                    if ref_stru[0] == '0' : xx.SetValue(False)
                    xx = wx.FindWindowByName('stage%i_struc%i_strain'%(i+1, j+1))
                    if ref_stru[1] == '1' : xx.SetValue(True)
                    if ref_stru[1] == '0' : xx.SetValue(False)
                    for k in range(gui_nato):
                        xx = wx.FindWindowByName('stage%i_struc%i_ato%i_okk'%(i+1, j+1, k+1))
                        if ref_stru[2*(k+1)] == '1' : xx.SetValue(True)
                        if ref_stru[2*(k+1)] == '0' : xx.SetValue(False)
                        xx = wx.FindWindowByName('stage%i_struc%i_ato%i_bth'%(i+1, j+1, k+1))
                        if ref_stru[2*(k+1)+1] == '1' : xx.SetValue(True)
                        if ref_stru[2*(k+1)+1] == '0' : xx.SetValue(False)
                gui_ndata = self.ref[i][1]
                ref_data = refinfo.ref[i][3].split()
                ref_ndata = len(ref_data)
                while gui_ndata < ref_ndata:
                    if verbose : print('ADDING %i DATA(S)'%(ref_ndata - gui_ndata))
                    self.Add_DataX(event, pos = '%i0'%(i+1))
                    gui_ndata = self.ref[i][0][j]
                while gui_ndata > ref_ndata:
                    if verbose : print('REMOVING %i DATA(S)'%(-ref_ndata + gui_ndata))
                    self.Del_DataX(event, pos = '%i0'%(i+1))
                    gui_ndata = self.ref[i][0][j]
                for k in range(gui_ndata):
                    xx = wx.FindWindowByName('stage%i_data%i'%(i+1, k+1))
                    if ref_data[k] == '1' : xx.SetValue(True)
                    if ref_stru[k] == '0' : xx.SetValue(False)


    def SAVE_ref(self, event):
        fout = ''
        dlg = wx.FileDialog(
            self, message = "Save file as ...", defaultDir = os.getcwd(),
            defaultFile = "", wildcard = '*.ref', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dlg.ShowModal()  ==  wx.ID_OK:
            fout = dlg.GetPath()
        dlg.Destroy()
        if len(fout) > 0:
            SetPath(self, fout)
            try:
                refout = open(fout, 'w')
            except IOError:
                print("Error: can\'t find file or write data")
            else:
                gui_nstage = len(self.ref)
                print(' Number of stages #  %i'%gui_nstage, file = refout)
                for i in range(gui_nstage):
                    xx = wx.FindWindowByName('stage%i_tol'%(i+1))
                    tolx = xx.GetValue()
                    xx = wx.FindWindowByName('stage%i_alg'%(i+1))
                    algx = xx.GetValue()
                    print(' stage #  %i %s %s'%(i+1, tolx, algx), file = refout)
                    gui_nstruc = len(self.ref[i][0])
                    for j in range(gui_nstruc):
                        stru_flag = '%'+str(j+1).lstrip()
                        print(' %s'%stru_flag, file = refout)
                        str1, str2 = '!', ' '
                        xx = wx.FindWindowByName('stage%i_struc%i_size'%(i+1, j+1))
                        sizex = xx.GetValue()
                        if sizex: size_flag = '1'
                        else :size_flag = '0'
                        str1 += ' %s'%'1'
                        str2 += ' %s'%size_flag
                        xx = wx.FindWindowByName('stage%i_struc%i_strain'%(i+1, j+1))
                        strainx = xx.GetValue()
                        if strainx : strain_flag = '1'
                        else : strain_flag = '0'
                        str1 += ' %s'%'2'
                        str2 += ' %s'%strain_flag
                        gui_nato = self.ref[i][0][j]
                        for k in range(gui_nato):
                            xx = wx.FindWindowByName('stage%i_struc%i_ato%i_okk'%(i+1, j+1, k+1))
                            okkx = xx.GetValue()
                            if okkx: okk_flag = '1'
                            else :okk_flag = '0'
                            str1 += ' %i'%(2*(k+1)+1)
                            str2 += ' %s'%okk_flag
                            xx = wx.FindWindowByName('stage%i_struc%i_ato%i_bth'%(i+1, j+1, k+1))
                            bthx = xx.GetValue()
                            if bthx: bth_flag = '1'
                            else :bth_flag = '0'
                            str1 += ' %i'%(2*(k+2))
                            str2 += ' %s'%bth_flag
                        print(str1, file = refout)
                        print(str2, file = refout)
                    gui_ndata = self.ref[i][1]
                    data_flag = str(gui_ndata).lstrip()
                    print(' #%s'%data_flag, file = refout)
                    str1 = ' '
                    for k in range(gui_ndata):
                        xx = wx.FindWindowByName('stage%i_data%i'%(i+1, k+1))
                        datax = xx.GetValue()
                        if datax : data_flag = '1'
                        else : data_flag = '0'
                        str1 += ' %s'%data_flag
                    print(str1, file = refout)
                refout.close()


    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'Debussy Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
class ButtonsPanel(wx.Panel):
    """"""
 
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """Constructor"""
        wx.Panel.__init__(self, parent = parent)

        self.runx = False

        self.vbox = wx.BoxSizer(wx.VERTICAL)
#         self.setjob_btn = wx.Button(self, id = -1, label = 'Set folder ...', size = (80, 25))
#         self.setjob_btn.Bind(wx.EVT_BUTTON, self.SetDir)
#         self.setjob_btn.SetToolTip(wx.ToolTip("Set working folder"))
#         self.vbox.Add(self.setjob_btn, flag = wx.ALL, border = 10)
        dbox = wx.StaticBox(self, -1, 'RUN')##, (508, 40), size = (320, 58))
        self.boxsizerd = wx.StaticBoxSizer(dbox, wx.VERTICAL)    
        self.simulation_btn = wx.Button(self, id = -1, label = 'Simulation', size = (90, 28), name='simulation')  
#         self.simulation_btn.Bind(wx.EVT_BUTTON, self.simulation_click)
        self.simulation_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='dwa', button_name='simulation', label="Simulation", stop_flag='sim_stop', job_type='sim', buffer_name='DebussyBuffer':\
           self.ref_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name))
        self.simulation_btn.SetToolTip(wx.ToolTip("Run Simulation"))
        self.boxsizerd.Add(self.simulation_btn)
        self.refinement_btn = wx.Button(self, id = -1, label = 'Refinement', size = (90, 28), name='refinement')
#         self.refinement_btn.Bind(wx.EVT_BUTTON, self.refinement_click)
        self.refinement_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='dwa', button_name='refinement', label="Refinement", stop_flag='ref_stop', job_type='ref', buffer_name='DebussyBuffer':\
           self.ref_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name))
        self.refinement_btn.SetToolTip(wx.ToolTip("Run Refinement"))
        self.boxsizerd.Add(self.refinement_btn)
        self.std_btn = wx.Button(self, id = -1, label = 'STD', size = (90, 28), name='std')
#         self.std_btn.Bind(wx.EVT_BUTTON, self.std_click)
        self.std_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='dwa', button_name='std', label="STD", stop_flag='ref_stop', job_type='std', buffer_name='DebussyBuffer':\
           self.ref_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name))
        self.std_btn.SetToolTip(wx.ToolTip("Run STD calculation"))
        self.boxsizerd.Add(self.std_btn)
        self.vbox.Add(self.boxsizerd, flag = wx.LEFT|wx.RIGHT|wx.TOP, border = 10)

        ###____PLOT  - DEBUSSY
        pbox = wx.StaticBox(self, -1, 'PLOT')
        self.boxsizerp = wx.StaticBoxSizer(pbox, wx.VERTICAL)
#         self.pdata_btn = wx.Button(self, id = -1, label = 'Data', size = (100, 28))
#         self.pdata_btn.Bind(wx.EVT_BUTTON, self.pdata_click)
#         self.pdata_btn.SetToolTip(wx.ToolTip("Plot Data"))
#         self.boxsizerp.Add(self.pdata_btn)
#         self.dts = ''
        hboxix = wx.BoxSizer(wx.HORIZONTAL)
        self.rbtt = wx.RadioButton(self, - 1, u"2\u03B8", style = wx.RB_GROUP, name='plot_itt')
        self.rbtt.SetValue(True)
        hboxix.Add(self.rbtt, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.rbq = wx.RadioButton(self, - 1, 'q', name='plot_iq')
#         hboxix.Add(self.rbq, flag = wx.BOTTOM|wx.TOP, border = 2)
        self.rbQ = wx.RadioButton(self, - 1, 'Q', name='plot_iQ')
        hboxix.Add(self.rbQ, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.rbd = wx.RadioButton(self, - 1, 'd', name='plot_id')
#         hboxix.Add(self.rbd, flag = wx.BOTTOM|wx.TOP, border = 2)
        self.boxsizerp.Add(hboxix)
#         self.rblogq = wx.RadioButton(self, - 1, "log(q)", name='plot_logq')
#         self.rblogq.SetValue(False)
        self.rblogQ = wx.RadioButton(self, - 1, 'log(Q)', name='plot_logQ')

        hboxix2 = wx.BoxSizer(wx.HORIZONTAL)
#         hboxix2.Add(self.rblogq, flag = wx.BOTTOM|wx.TOP, border = 2)
        hboxix2.Add(self.rblogQ, flag = wx.BOTTOM|wx.TOP, border = 2)
        self.boxsizerp.Add(hboxix2)


        hboxix3 = wx.BoxSizer(wx.HORIZONTAL)
        self.cblogI = wx.CheckBox(self, - 1, "log(I)", style = wx.RB_GROUP, name='plot_logI')
        self.cblogI.SetValue(False)
        hboxix3.Add(self.cblogI, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.cbsqrtI = wx.CheckBox(self, - 1, 'sqrt(I)', name='plot_sqrtI')
#         self.cbsqrtI.SetValue(False)
#         hboxix3.Add(self.cbsqrtI, flag = wx.BOTTOM|wx.TOP, border = 2)
        self.boxsizerp.Add(hboxix3)

        self.cbhkl = wx.CheckBox(self, label='hkl', size=(-1,-1))
        self.cbhkl.SetValue(False)
        self.boxsizerp.Add(self.cbhkl)
        self.psim_btn = wx.Button(self, id = -1, label = 'Simulation', size = (100, 28))
        self.psim_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'sim':\
           self.pcal_click(event, what))
        self.psim_btn.SetToolTip(wx.ToolTip("Plot Simulation"))
        self.boxsizerp.Add(self.psim_btn)
        self.pbestfit_btn = wx.Button(self, id = -1, label = 'Best fit', size = (100, 28))
        self.pbestfit_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'ref':\
           self.pcal_click(event, what))
        self.pbestfit_btn.SetToolTip(wx.ToolTip("Plot Best fit"))
        self.boxsizerp.Add(self.pbestfit_btn)
        self.psize_btn = wx.Button(self, id = -1, label = 'Size', size = (100, 28))
        self.psize_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'siz':\
           self.pcal_click(event, what))
        self.psize_btn.SetToolTip(wx.ToolTip("Plot size distributions"))
        self.boxsizerp.Add(self.psize_btn)
        self.platexp_btn = wx.Button(self, id = -1, label = 'Lattice exp.', size = (100, 28))
        self.platexp_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'cel':\
           self.pcal_click(event, what))
        self.platexp_btn.SetToolTip(wx.ToolTip("Plot isotropic lattice expansion factor"))
        self.boxsizerp.Add(self.platexp_btn)
        self.psof_btn = wx.Button(self, id = -1, label = 'S.O.F.', size = (100, 28))
        self.psof_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'sof':\
           self.pcal_click(event, what))
        self.psof_btn.SetToolTip(wx.ToolTip("Plot Site Occupation Factors"))
        self.boxsizerp.Add(self.psof_btn)
        self.pbth_btn = wx.Button(self, id = -1, label = 'BTH', size = (100, 28))
        self.pbth_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'bth':\
           self.pcal_click(event, what))
        self.pbth_btn.SetToolTip(wx.ToolTip("Plot thermal parameters"))
        self.boxsizerp.Add(self.pbth_btn)
        self.pcstm_btn = wx.Button(self, id = -1, label = 'Custom', size = (100, 28))
        self.pcstm_btn.Bind(wx.EVT_BUTTON, self.pcstm_click)
        self.pcstm_btn.SetToolTip(wx.ToolTip("Custom plot"))
        self.boxsizerp.Add(self.pcstm_btn)        
        self.vbox.Add(self.boxsizerp, flag = wx.LEFT|wx.RIGHT|wx.TOP, border = 10)

        self.SetSizer(self.vbox)
        self.Layout()

    #----------------------------------------------------------------------
    ####____DEBUSSY

    ##__most complete option, looks however much more computationally expensive 
    def ref_click(self, event, input_type, button_name, label, stop_flag, job_type, buffer_name):
        input_file = GetInFile(self, input_type)
        button = wx.FindWindowByName(button_name)
        if len(input_file) > 0:
            infile = input_file.rpartition(gv.SEP)[-1]
            msg = '\n  %s  running on %s in %s'%(label, infile, os.getcwd())
            xx = wx.FindWindowByName(buffer_name)
            toBuffer(self, xx, msg)
            if job_type == 'sim':
                if self.rbtt.GetValue(): what = job_type + '_tt'
#                 elif self.rbq.GetValue() == True: what = job_type + '_q'
                elif self.rbQ.GetValue() == True: what = job_type + '_Q'
#                 elif self.rbd.GetValue() == True: what = job_type + '_d'
#                 elif self.rblogq.GetValue() == True: what = job_type + '_logq'
                elif self.rblogQ.GetValue() == True: what = job_type + '_logQ'
                if self.cblogI.GetValue() == True: what += '_logI'
#                 elif self.cbsqrtI.GetValue() == True: what += '_sqrtI'
                if self.cbhkl.GetValue() == True: what = what + '_hkl'
            else:
                ##__self-updating plot
                what = 'liveref' 
                if self.rbtt.GetValue(): what = what + '_tt'
#                 elif self.rbq.GetValue() == True: what = what + '_q'
                elif self.rbQ.GetValue() == True: what = what + '_Q'
#                 elif self.rbd.GetValue() == True: what = what + '_d'
#                 elif self.rblogq.GetValue() == True: what = what + '_logq'
                elif self.rblogQ.GetValue() == True: what += '_logQ'
                if self.cblogI.GetValue() == True: what += '_logI'
#                 elif self.cbsqrtI.GetValue() == True: what += '_sqrtI'
                if self.cbhkl.GetValue() == True: what = what + '_hkl'
                #plotter(input_file, what)
                if gset.Platform[:3].lower() == 'lin': 
                    cmd = ['python3', gset.GUI_Path + 'plotterC.py', input_file, what]
                elif gset.Platform[:3].lower() == 'dar':
      	            cmd = ['pythonw', gset.GUI_Path + 'plotterC.py', input_file, what]
                elif gset.Platform[:3].lower() == 'win':
                    cmd = ['python', gset.GUI_Path + 'plotterC.py', input_file, what]  
                #cmd = ['pythonw', gset.GUI_Path + 'plotterC.py', input_file, what]
                plot_proc = subprocess.Popen(cmd)
            thread = threading.Thread(target = self.ref_run, args = (job_type, input_file, stop_flag, buffer_name, button, label, what, ))
            thread.setDaemon(True)
            thread.start()

    def ref_run(self, job_type, input_file, stop_flag, buffer_name, button, label, plot_type):
        self.stop_flag = 0 
        pgm = debyer(job_type, input_file)
        if gset.Platform[:3].lower() == 'win':
            runfile = open('drun.bat', 'w')
            print("%s"%pgm, file = runfile)
            print("pause", file = runfile)
            runfile.close()
            os.system('start "%s" %s'%(label,'drun.bat'))
        else:
            xterm_opt2 = ['-T', label, '-sb', '-e', '%s; sleep %i'%(pgm, wtm)]
            terminal = 'xterm -geometry 120x30'.split()
            cmd1 = terminal + xterm_opt2
            proc = subprocess.Popen(cmd1, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    ####____PLOT

    ##__plot calculated stuff
    def pcal_click(self, event, what, liveplot=False):
        self.dws = GetInFile(self, 'dwa')
        if len(self.dws) > 0:
            if self.rbtt.GetValue(): what = what + '_tt'
#             elif self.rbq.GetValue() == True: what = what + '_q'
            elif self.rbQ.GetValue() == True: what = what + '_Q'
#             elif self.rbd.GetValue() == True: what = what + '_d'
#             elif self.rblogq.GetValue() == True: what = what + '_logq'
            elif self.rblogQ.GetValue() == True: what += '_logQ'
            if self.cblogI.GetValue() == True: what += '_logI'
#             elif self.cbsqrtI.GetValue() == True: what += '_sqrtI'
            if self.cbhkl.GetValue() == True: what = what + '_hkl'
            plotter(self.dws, what)
#             cmd = ['python', gset.GUI_Path + 'plotterC.py', self.dws, what, liveplot]
#             proc = subprocess.Popen(cmd)
#             if what == 'data': plot_data(dwaobj)
#             elif what == 'sim': plot_sim(dwaobj)
#             elif what == 'ref': plot_ref(dwaobj)
#             elif what == 'siz': plot_size(dwaobj)
#             elif what == 'cel': plot_cel(dwaobj)
#             elif what == 'sof': plot_sof(dwaobj)
#             elif what == 'bth': plot_bth(dwaobj)
#         elif len(dwal) == 0:
#             print ' *** STOP : .dwa file not found! Check working folder. ***'

    ##__Custom plot
    def pcstm_click(self, event):
        self.customplt_frame = CustomPlotter()
        self.customplt_frame.Show()
########################################################################
class DebussyGUI(wx.Panel):
    """
    Frame that holds all other widgets
    """
 
    #----------------------------------------------------------------------
    def __init__(self, parent):
        """Constructor"""
        wx.Panel.__init__(self, parent = parent, size = (700, 760))

        splitterH = wx.SplitterWindow(self)        
        splitterV = wx.SplitterWindow(splitterH)

        notebook = wx.Notebook(splitterV, -1, wx.DefaultPosition, wx.DefaultSize, wx.NB_TOP)
        tab1 = DWA(notebook)
        notebook.AddPage(tab1, "DWA")
        tab2 = PARX(notebook)
        notebook.AddPage(tab2, "PAR")
        tab3 = REFX(notebook)
        notebook.AddPage(tab3, "REF")

        rightP = ButtonsPanel(splitterV)
        # split the window
        splitterV.SplitVertically(notebook, rightP)
        if gset.Platform.startswith('dar'): splitterV.SetMinimumPaneSize(150)
        else: splitterV.SetMinimumPaneSize(650)
        splitterV.SetSashGravity(1.0)
        
        ###___TEXT BUFFER BOX
        textbuffer = wx.TextCtrl(splitterH,-1,style=wx.TE_MULTILINE, name='DebussyBuffer')

        splitterH.SplitHorizontally(splitterV, textbuffer)
        if gset.Platform.startswith('dar'): splitterH.SetMinimumPaneSize(480)
        else: splitterH.SetMinimumPaneSize(400)
        splitterV.SetSashGravity(1.0)
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(splitterH, 1, wx.ALL|wx.EXPAND, 5)

        self.SetSizer(sizer)
        self.Layout()
#----------------------------------------------------------------------
if __name__  ==  "__main__":
    app = wx.App(False)
    frame = DebussyGUI()
    app.MainLoop()
