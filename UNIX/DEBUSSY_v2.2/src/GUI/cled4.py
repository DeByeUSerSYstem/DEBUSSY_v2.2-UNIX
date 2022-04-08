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
import wx.grid
import wx.lib.dialogs
from debfuncx import SetFolder, SetPath, GetInFile, toBuffer, get_term
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
from readerC import reader
from builderC import builder
from customw7 import CustomPlotter
##########################################################################################
xterm_opt = 'xterm  -geometry 120x30'.split()
xterm_opt0 = 'xterm  -T TESTING  -e tail  -f PIPE_PATH'.split()
wtm = 86400
wtms = 5
if gset.PGM_Path[-1]!= gv.SEP : gset.PGM_Path = gset.PGM_Path + gv.SEP
if gset.GUI_Path[-1]!= gv.SEP : gset.GUI_Path = gset.GUI_Path + gv.SEP
if gset.DEB_Path[-1]!= gv.SEP : gset.DEB_Path = gset.DEB_Path + gv.SEP

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
class Population(wx.lib.scrolledpanel.ScrolledPanel):
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()
        
        if gset.Platform.startswith('dar'):
            font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)

        vbox = wx.BoxSizer(wx.VERTICAL)

        hbox41 = wx.BoxSizer(wx.HORIZONTAL)
#         close_btn = wx.Button(self, label = "Close")
#         close_btn.Bind(wx.EVT_BUTTON, self.OnClose)
#         close_btn.SetToolTip(wx.ToolTip("Close the window"))
#         hbox41.Add(close_btn, flag = wx.ALL, border = 3)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hbox41.Add(hlp_btn, flag = wx.ALL, border = 3)
        clin_btn  = wx.Button(self, label = 'CLEAR')
        clin_btn.Bind(wx.EVT_BUTTON, self.Del_inpinfo)
        clin_btn.SetToolTip(wx.ToolTip("Clear input information"))    
        hbox41.Add(clin_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .ddb')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_DBinp)
        ldin_btn.SetToolTip(wx.ToolTip("Load input information from a .ddb file"))    
        hbox41.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SaveInpFile)
        svf_btn.SetToolTip(wx.ToolTip("Save input values to input files"))    
        hbox41.Add(svf_btn, flag = wx.ALL, border = 3)

        vbox.Add(hbox41, flag = wx.ALIGN_RIGHT)

        self.sgs_list = self.get_SGlist(sep = gv.SEP)[0]
        self.sgnt = self.get_SGlist(sep = gv.SEP)[1]
        self.ps = self.get_SGlist(sep = gv.SEP)[2]

        phabox = wx.StaticBox(self, - 1, 'Phase')
        phaboxsizer = wx.StaticBoxSizer(phabox, wx.VERTICAL)

        hbox001 = wx.BoxSizer(wx.HORIZONTAL)
        ld_btn  = wx.Button(self, label = 'LOAD .cif/.pha')
        ld_btn.Bind(wx.EVT_BUTTON, self.LD_pha)
        ld_btn.SetToolTip(wx.ToolTip("Load phase information from a .cif or a .pha file"))    
        hbox001.Add(ld_btn, flag = wx.BOTTOM, border = 10)
        namet = wx.StaticText(self, label = "Name")
#         hbox001.Add(namet, flag = wx.LEFT|wx.ALIGN_RIGHT, border = 10)
        hbox001.Add(namet, flag = wx.LEFT, border = 10)
        self.namef = wx.TextCtrl(self, style = wx.TE_LEFT, size = (150, - 1))
        self.namef.SetMaxLength(132)
#         hbox001.Add(self.namef, flag = wx.ALIGN_RIGHT)
        hbox001.Add(self.namef)
        phaboxsizer.Add(hbox001, flag = wx.EXPAND)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        sgt = wx.StaticText(self, label = "SG #")
#         hbox01.Add(sgt, flag = wx.LEFT|wx.ALIGN_RIGHT, border = 10)
        hbox01.Add(sgt, flag = wx.LEFT, border = 10)
#         self.sgf = wx.TextCtrl(self, style = wx.TE_RIGHT)
#         hbox01.Add(self.sgf)
        self.sgs_btn = wx.ComboBox(self, id = -1, size = (160, - 1), choices = self.sgs_list, style = wx.CB_DROPDOWN)
        self.sgs_btn.Bind(wx.EVT_COMBOBOX, self.sgs_select)
        self.sgs_btn.SetToolTip(wx.ToolTip("Select the the space group number"))
#         hbox01.Add(self.sgs_btn, flag = wx.ALIGN_RIGHT)
        hbox01.Add(self.sgs_btn)
        nat = wx.StaticText(self, label = "At. Species #")
#         hbox01.Add(nat, flag = wx.LEFT|wx.ALIGN_RIGHT, border = 10)
        hbox01.Add(nat, flag = wx.LEFT)
        self.naf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (50, - 1))
        self.naf.SetMaxLength(132)
#         hbox01.Add(self.naf, flag = wx.ALIGN_RIGHT)
        hbox01.Add(self.naf)
        phaboxsizer.Add(hbox01, flag = wx.ALIGN_RIGHT)

        hbox02 = wx.BoxSizer(wx.HORIZONTAL)
        self.pear = ''
        cct = wx.StaticText(self, label = "Constr")
        hbox02.Add(cct)
        self.rbccp = wx.RadioButton(self, - 1, 'P', style = wx.RB_GROUP)
        self.rbccp.SetValue(True)
        hbox02.Add(self.rbccp)
        self.rbccs = wx.RadioButton(self, - 1, 'S')
        hbox02.Add(self.rbccs)
        hbox02.Add((30, - 1))
        ceort = wx.StaticText(self, label = "Cell Origin ")
        hbox02.Add(ceort)
        xort = wx.StaticText(self, label = "X")
        hbox02.Add(xort, flag = wx.LEFT, border = 5)
        self.xorf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (85, - 1))
        self.xorf.SetMaxLength(132)
        hbox02.Add(self.xorf)
        yort = wx.StaticText(self, label = "Y")
        hbox02.Add(yort, flag = wx.LEFT, border = 5)
        self.yorf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (85, - 1))
        self.yorf.SetMaxLength(132)
        hbox02.Add(self.yorf)
        zort = wx.StaticText(self, label = "Z")
        hbox02.Add(zort, flag = wx.LEFT, border = 5)
        self.zorf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (85, - 1))
        self.zorf.SetMaxLength(132)
        hbox02.Add(self.zorf)
        phaboxsizer.Add(hbox02, flag = wx.ALIGN_RIGHT|wx.TOP, border = 3)

        cellbl = ['', 'a', 'b', 'c', 'alpha', 'beta', 'gamma']
        asylbl = ['Atom', 'Type', 'X', 'Y', 'Z', 'B', 'S.O.F.']
        self.asygrid = wx.grid.Grid(self, - 1, size = (575, 152))
        self.gridrows, self.gridcols = 100, 7
        self.asygrid.CreateGrid(self.gridrows, self.gridcols)
        self.asygrid.SetRowLabelSize(0) 
        self.asygrid.SetColLabelSize(0)
        bgc = self.asygrid.GetLabelBackgroundColour()
        for col in range(7):
            self.asygrid.SetCellBackgroundColour(0, col, bgc)
            self.asygrid.SetCellBackgroundColour(2, col, bgc)
            self.asygrid.SetCellValue(0, col, cellbl[col])
            self.asygrid.SetReadOnly(0, col, True)
            self.asygrid.SetCellAlignment(0, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
            self.asygrid.SetCellValue(2, col, asylbl[col])
            self.asygrid.SetReadOnly(2, col, True)
            if col == 0:
                self.asygrid.SetCellAlignment(2, col, wx.ALIGN_LEFT, wx.ALIGN_CENTRE)
                self.asygrid.SetCellAlignment(1, col, wx.ALIGN_LEFT, wx.ALIGN_CENTRE)
            else:
                self.asygrid.SetCellAlignment(2, col, wx.ALIGN_CENTRE, wx.ALIGN_CENTRE)
        self.asygrid.SetCellBackgroundColour(1, 0, bgc)
        self.asygrid.SetCellValue(1, 0, 'Cell')
        self.asygrid.SetReadOnly(1, 0, True)
        phaboxsizer.Add(self.asygrid, flag = wx.TOP, border = 5)
        hbox031 = wx.BoxSizer(wx.HORIZONTAL)
        occt = wx.StaticText(self, label = "SOF=1 ")
        hbox031.Add(occt, flag = wx.BOTTOM|wx.TOP, border = 10)
        self.rbocc1 = wx.RadioButton(self, - 1, 'yes', style = wx.RB_GROUP)
        self.rbocc1.SetValue(True)
        hbox031.Add(self.rbocc1, flag = wx.BOTTOM|wx.TOP, border = 10)
        self.rbocc0 = wx.RadioButton(self, - 1, 'no')
        hbox031.Add(self.rbocc0, flag = wx.BOTTOM|wx.TOP, border = 10)
        phaboxsizer.Add(hbox031)
        hbox03 = wx.BoxSizer(wx.HORIZONTAL)
        part = wx.StaticText(self, label = "PARACRYSTALLINE MODEL")
        hbox03.Add(part, flag = wx.BOTTOM|wx.TOP, border = 5)
        parh = wx.StaticText(self, label = "(Non - mandatory option)")
        hbox03.Add(parh, flag = wx.BOTTOM|wx.TOP, border = 5)
        phaboxsizer.Add(hbox03)
        hbox04 = wx.BoxSizer(wx.HORIZONTAL)
        self.rbis = wx.RadioButton(self, - 1, 'Isotropic', style = wx.RB_GROUP)
        hbox04.Add(self.rbis, flag = wx.BOTTOM, border = 5)
        self.rban = wx.RadioButton(self, - 1, 'Anisotropic')
        hbox04.Add(self.rban, flag = wx.BOTTOM, border = 5)
        self.parff = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (400, - 1))
        self.parff.SetMaxLength(132)
        hbox04.Add(self.parff, 1, flag = wx.EXPAND|wx.BOTTOM, border = 5)
        phaboxsizer.Add(hbox04)

        vbox.Add(phaboxsizer)

        shabox = wx.StaticBox(self, - 1, 'Shape')
        shaboxsizer = wx.StaticBoxSizer(shabox, wx.VERTICAL)

        hbox21 = wx.BoxSizer(wx.HORIZONTAL)
        shat = wx.StaticText(self, label = "Shape of Clusters", style = wx.TE_RIGHT, size = (120, - 1))
        hbox21.Add(shat)
        hbox21.Add((30, - 1))
        self.rbshas = wx.RadioButton(self, - 1, 'SPH', style = wx.RB_GROUP)
        self.rbshas.SetValue(True)
        hbox21.Add(self.rbshas)
        hbox21.Add((30, - 1))
        self.rbshaq = wx.RadioButton(self, - 1, 'QBE')
        hbox21.Add(self.rbshaq)
        hbox21.Add((30, - 1))
        self.rbshap = wx.RadioButton(self, - 1, 'PAR')
        hbox21.Add(self.rbshap)
        hbox21.Add((30, - 1))
        self.rbshac = wx.RadioButton(self, - 1, 'CYL')
        hbox21.Add(self.rbshac)
        hbox21.Add((30, - 1))
        self.rbshah = wx.RadioButton(self, - 1, 'HEX')
        hbox21.Add(self.rbshah)
        hbox21.Add((30, - 1))
        shaboxsizer.Add(hbox21)
#         hbox21 = wx.BoxSizer(wx.HORIZONTAL)
#         shat = wx.StaticText(self, label = "Shape of Clusters", style = wx.TE_RIGHT, size = (120, - 1))
#         hbox21.Add(shat)
#         self.shaf = wx.TextCtrl(self, style = wx.TE_RIGHT)
#         self.shaf.SetMaxLength(132)
#         hbox21.Add(self.shaf)
#         shah = wx.StaticText(self, label = "(SPH/QBE/PAR/CYL/HEX)")
#         hbox21.Add(shah)
#         shaboxsizer.Add(hbox21)

        spht = wx.StaticText(self, label = "______ SPH/QBE shape")
        shaboxsizer.Add(spht, flag = wx.LEFT|wx.TOP, border = 10)
        hbox22 = wx.BoxSizer(wx.HORIZONTAL)
        dt = wx.StaticText(self, label = "Diameter/Edge max", style = wx.TE_RIGHT, size = (130, - 1))
        hbox22.Add(dt, flag = wx.BOTTOM|wx.TOP, border = 3)
        self.df = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.df.SetMaxLength(132)
        hbox22.Add(self.df, flag = wx.BOTTOM|wx.TOP, border = 3)
        dh = wx.StaticText(self, label = "[nm]", style = wx.TE_LEFT, size = (40, - 1))
        hbox22.Add(dh, flag = wx.BOTTOM|wx.TOP, border = 3)
        dopt = wx.StaticText(self, label = "or", style = wx.TE_CENTRE, size = (40, - 1))
        hbox22.Add(dopt, flag = wx.BOTTOM|wx.TOP, border = 3)
        nt = wx.StaticText(self, label = "Number of shells\n max", style = wx.TE_RIGHT, size = (120, - 1))
        hbox22.Add(nt, flag = wx.BOTTOM|wx.TOP, border = 3)
        self.nf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.nf.SetMaxLength(132)
        hbox22.Add(self.nf, flag = wx.BOTTOM|wx.TOP, border = 3)
        nh = wx.StaticText(self, label = "", style = wx.TE_LEFT, size = (40, - 1))
        hbox22.Add(nh, flag = wx.BOTTOM|wx.TOP, border = 3)
        shaboxsizer.Add(hbox22, flag = wx.BOTTOM|wx.TOP, border = 3)

        part = wx.StaticText(self, label = "______ PAR/CYL/HEX shape")
        shaboxsizer.Add(part, flag = wx.LEFT, border = 10)
        hbox23 = wx.BoxSizer(wx.HORIZONTAL)
        dpt = wx.StaticText(self, label = "Diameter max", style = wx.TE_RIGHT, size = (130, - 1))
        hbox23.Add(dpt, flag = wx.TOP, border = 3)
        self.dpf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.dpf.SetMaxLength(132)
        hbox23.Add(self.dpf, flag = wx.TOP, border = 3)
        dph = wx.StaticText(self, label = "[nm]", style = wx.TE_LEFT, size = (40, - 1))
        hbox23.Add(dph, flag = wx.TOP, border = 3)
        pand = wx.StaticText(self, label = "or", style = wx.TE_CENTRE, size = (40, - 1))
        hbox23.Add(pand, flag = wx.TOP, border = 3)

        n1pt = wx.StaticText(self, label = "Number of shells\n in ab - plane", style = wx.TE_RIGHT, size = (120, - 1))
        hbox23.Add(n1pt, flag = wx.BOTTOM|wx.TOP, border = 3)
        self.n1pf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.n1pf.SetMaxLength(132)
        hbox23.Add(self.n1pf, flag = wx.BOTTOM|wx.TOP, border = 3)
        n1ph = wx.StaticText(self, label = "", style = wx.TE_LEFT, size = (40, - 1))
        hbox23.Add(n1ph, flag = wx.BOTTOM|wx.TOP, border = 3)
        shaboxsizer.Add(hbox23, 1)

#         pand = wx.StaticText(self, label = "|", style = wx.TE_CENTRE, size = (60, - 1))
#         shaboxsizer.Add(pand, flag = wx.ALIGN_CENTRE)

        hbox24 = wx.BoxSizer(wx.HORIZONTAL)
        lt = wx.StaticText(self, label = "Length max", style = wx.TE_RIGHT, size = (130, - 1))
        hbox24.Add(lt, flag = wx.TOP, border = 3)
        self.lf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.lf.SetMaxLength(132)
        hbox24.Add(self.lf, flag = wx.TOP, border = 3)
        lh = wx.StaticText(self, label = "[nm]", style = wx.TE_LEFT, size = (40, - 1))
        hbox24.Add(lh, flag = wx.TOP, border = 3)

        pand = wx.StaticText(self, label = "or", style = wx.TE_CENTRE, size = (40, - 1))
        hbox24.Add(pand, flag = wx.BOTTOM|wx.TOP, border = 3)
        n2pt = wx.StaticText(self, label = "Number of layers\n along c - axis", style = wx.TE_RIGHT, size = (120, - 1))
        hbox24.Add(n2pt, flag = wx.BOTTOM|wx.TOP, border = 3)
        self.n2pf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.n2pf.SetMaxLength(132)
        hbox24.Add(self.n2pf, flag = wx.BOTTOM|wx.TOP, border = 3)
        n2ph = wx.StaticText(self, label = "", style = wx.TE_LEFT, size = (40, - 1))
        hbox24.Add(n2ph, flag = wx.BOTTOM|wx.TOP, border = 3)
        shaboxsizer.Add(hbox24)

#         xyzt = wx.StaticText(self, label = "XYZ")
#         hbox25.Add(xyzt, flag = wx.BOTTOM|wx.TOP|wx.LEFT, border = 10)
#         self.rbxyz1 = wx.RadioButton(self, - 1, 'yes', style = wx.RB_GROUP)
#         hbox25.Add(self.rbxyz1, flag = wx.BOTTOM|wx.TOP, border = 10)
#         self.rbxyz0 = wx.RadioButton(self, - 1, 'no')
#         self.rbxyz0.SetValue(True)
#         hbox25.Add(self.rbxyz0, flag = wx.BOTTOM|wx.TOP, border = 10)
#         shaboxsizer.Add(hbox25)
        
        vbox.Add(shaboxsizer)

        hboxST = wx.BoxSizer(wx.HORIZONTAL)
        sambox = wx.StaticBox(self, - 1, 'Sampling')
        samboxsizer = wx.StaticBoxSizer(sambox, wx.VERTICAL)
        hbox31 = wx.BoxSizer(wx.HORIZONTAL)
        sat = wx.StaticText(self, label = "Sampling ", size = (80, - 1))
        hbox31.Add(sat)

#         self.saf = wx.TextCtrl(self, style = wx.TE_RIGHT)
#         self.saf.SetMaxLength(132)
#         hbox31.Add(self.saf)
#         shah = wx.StaticText(self, label = "(One/...)")
#         hbox31.Add(shah)

        self.samrb1 = wx.RadioButton(self, - 1, 'One', style = wx.RB_GROUP)
        self.samrb1.SetValue(True)
        hbox31.Add(self.samrb1, flag = wx.LEFT, border = 20)
        self.samrb2 = wx.RadioButton(self, - 1, 'All')
        hbox31.Add(self.samrb2, flag = wx.LEFT, border = 10)

        samboxsizer.Add(hbox31)       



        hbox32 = wx.BoxSizer(wx.HORIZONTAL)
        wlt = wx.StaticText(self, label = "Wavelength", size = (80, - 1))
        hbox32.Add(wlt, flag = wx.TOP, border = 5)
        self.wlf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.wlf.SetMaxLength(132)
        hbox32.Add(self.wlf, flag = wx.TOP, border = 5)
        wlh = wx.StaticText(self, label = u"[\u00C5]")
        hbox32.Add(wlh, flag = wx.TOP, border = 5)
        ttmt = wx.StaticText(self, label = u"2\u03B8 max")
        hbox32.Add(ttmt, flag = wx.TOP|wx.LEFT, border = 5)
        self.ttmf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.ttmf.SetMaxLength(132)
        hbox32.Add(self.ttmf, flag = wx.TOP, border = 5)
        ttmh = wx.StaticText(self, label = "[deg]")
        hbox32.Add(ttmh, flag = wx.TOP, border = 5)
        samboxsizer.Add(hbox32)
        hboxST.Add(samboxsizer)

        tdbox = wx.StaticBox(self, - 1, 'To Do')
        tdboxsizer = wx.StaticBoxSizer(tdbox, wx.VERTICAL)
#         hbox25 = wx.BoxSizer(wx.HORIZONTAL)
#         tdt = wx.StaticText(self, label = "TO DO")
#         hbox25.Add(tdt, flag = wx.BOTTOM|wx.TOP, border = 10)
        self.rball = wx.RadioButton(self, - 1, 'all_clusters', style = wx.RB_GROUP)
        self.rball.SetValue(True)
        tdboxsizer.Add(self.rball, flag = wx.TOP, border = 5)
        self.rbla = wx.RadioButton(self, - 1, 'largest_only')
        tdboxsizer.Add(self.rbla, flag = wx.TOP, border = 5)
#         tdboxsizer.Add(hbox25)
        hboxST.Add(tdboxsizer)

        vbox.Add(hboxST)

        self.SetSizer(vbox)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ### CLOSE 
    def OnClose(self, event):
        self.Destroy()

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'CLaUDe Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

    def get_SGlist(self, sep = '/'):
        sgf = open(gset.DEB_Path + 'ext_database' + gv.SEP + 'SpaceGroups' + gv.SEP + 'SG_Centering_PS.txt', 'r')
        sgfl = sgf.readlines()
        sgf.close()
        sgn, sgsym, sgnt, sgs_list, ps, lsgsym = [], [], [], ['    '], [], []
        for l in range(1, len(sgfl)):
            rline = sgfl[l].split()
            sgn += [rline[0]]
            lsgsym += [len(rline[2])]
            sgsym += [rline[2]]
            s1 = rline[3].strip('SPG_grp/SG_Nr_').strip('.grp')
            s2 = s1.lstrip('0')
            sgnt += [s2.replace('_', ' ')]
            ps += [rline[4]]
        #     print('*** SG ', sgn[l - 1], sgsym[l - 1], sgnt[l - 1])
            sgst = '%-12s%6s'%(sgsym[l - 1], sgnt[l - 1])
            sgs_list += [sgst]
        return sgs_list, sgnt, ps

    def sgs_select(self, sgs_btn):
        item = sgs_btn.GetSelection()
        if item > 0:
            self.sgs_btn.SetValue(self.sgnt[item - 1])
            self.pear = self.ps[item - 1]

    def get_SG(self, rawsg):
        spacegr = ''
        if len(rawsg) > 0:
            if rawsg[0].isdigit():
                spacegr = rawsg
                for c in range(len(rawsg)):
                    if rawsg[c].isalpha():
                        if rawsg[c - 1].isspace(): spacegr = rawsg
                        else: spacegr = rawsg[:c] + ' ' + rawsg[c:]
                        break
            else:
                # print('\nWARNING : Invalid Space Group number !! please enter/select it manually')
                spacegr = rawsg
#                 for sk in range(len(sgsym)):
#                     if sgsym[sk] == rawsg: 
#                         spacegr = sgnt[sk]
#                         print(sgsym[sk], rawsg, sgnt[sk])
#                         break
#                     else:
#                         print('no')
        return spacegr

    def get_PS(self, spacegr):
        pears = ''
        if len(spacegr) > 0:
            for i in range(len(self.sgnt)):
                if spacegr == self.sgnt[i]: pears = self.ps[i] + '00'
        return pears

    def Del_phainfo(self, event):
        self.namef.SetValue('')
        ##__space group
#         self.sgf.SetValue('')
        self.sgs_btn.SetValue('')
        ##__
        self.naf.SetValue('')
        self.xorf.SetValue('')
        self.yorf.SetValue('')
        self.zorf.SetValue('')
        for col in range(6):
          self.asygrid.SetCellValue(1, col + 1, '')
        for row in range(100 - 3):
          for col in range(7):
              self.asygrid.SetCellValue(row + 3, col, '')
        self.rbis.SetValue(True)
        self.parff.SetValue('')

    def Del_inpinfo(self, event):
        self.Del_phainfo(event)
#         self.shaf.SetValue('')
        self.rbshas.SetValue(True)
        self.df.SetValue('')
        self.nf.SetValue('')
        self.dpf.SetValue('')
        self.lf.SetValue('')
        self.n1pf.SetValue('')
        self.n2pf.SetValue('')
        self.rball.SetValue(True)
        self.rbocc1.SetValue(True)
        self.samrb1.SetValue(True)
        self.wlf.SetValue('')
        self.ttmf.SetValue('')

    def LD_pha(self, event):
        """
        something
        """
        pha, fin = '', ''
        dlg = wx.FileDialog(self, message = "Choose one .cif or .pha file", 
        defaultFile = "", wildcard = '*', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
          fin = dlg.GetPaths()
        dlg.Destroy()
        self.Del_phainfo(event)
        if len(fin) > 0:
          SetPath(self, fin[0])
          nfin = fin[0].rpartition(gv.SEP)[-1]
          efin = nfin.rpartition('.')[-1]
          self.phan = nfin.rpartition('.')[0]
          if (efin.lower() == 'cif' or efin.lower() == 'pha'):
              if (efin.lower() == 'cif'):
                  fin = fin[0].rpartition(gv.SEP)[-1]
                  fout = os.getcwd() + gv.SEP + self.phan + '.pha'
                  cmd = [gset.PGM_Path + 'cif2pha_DebUsSy', fin, fout]
                  proc = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.STDOUT)
                  if proc.communicate()[1] == None:
                      pha = fout
                  else :
                      print('\n >> >> >    Error: .cif file conversion failed : %s'%proc.communicate()[1])
              elif (efin == 'pha'):
                  pha = fin[0]
              inputdata = reader(pha)
              asyvals = inputdata.phaseinfo
              self.namef.SetValue(self.phan)
              ##__space group
#               self.sgf.SetValue(self.get_SG(asyvals[2]))
              self.sgs_btn.SetValue(self.get_SG(asyvals[2]))
              ##__
              nx = []
              for row in range(len(asyvals[3])):
                  nx += [int(asyvals[3][row][1])]
              self.naf.SetValue(str(max(nx)))
              self.xorf.SetValue('0.0')
              self.yorf.SetValue('0.0')
              self.zorf.SetValue('0.0')
              for col in range(6):
                  self.asygrid.SetCellValue(1, col + 1, asyvals[1][col])
                  self.asygrid.SetCellAlignment(1, col + 1, wx.ALIGN_RIGHT, - 1)
              for row in range(len(asyvals[3])):
                  for col in range(7):
                      self.asygrid.SetCellValue(row + 3, col, asyvals[3][row][col])
                      if col == 1:
                          self.asygrid.SetCellAlignment(row + 3, col, wx.ALIGN_CENTRE, - 1)
                      elif col > 1:
                          self.asygrid.SetCellAlignment(row + 3, col, wx.ALIGN_RIGHT, - 1)
          elif efin.lower() == 'xyz':
              print('for xyz phase files use single cluster database instead !')
#               pha = fin[0]
#               self.Del_phainfo(event)
#               self.namef.SetValue(nfin)
#               fxyz = open(fin[0], 'r')
#               lxyz = fxyz.readlines()
#               fxyz.close()
#               nat_xyz = int(lxyz[0].strip())
#               nrows = self.asygrid.GetNumberRows()
#               if nrows<nat_xyz: self.asygrid.AppendRows(nat_xyz - nrows + 3)
#               for row in range(nat_xyz):
#                   j = 0
#                   for col in [0, 2, 3, 4, 6]:
#                       self.asygrid.SetCellValue(row + 3, col, lxyz[row + 2].split()[j].strip())
#                       j += 1
#                       if col > 1:
#                           self.asygrid.SetCellAlignment(row + 3, col, wx.ALIGN_RIGHT, - 1)

    def LD_DBinp(self, event):
        """
        something
        """
        self.Del_inpinfo(event)
        fin, inpinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one .ddb file", 
        defaultFile = "*", wildcard = '*.ddb', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        ok = 0
        if dlg.ShowModal() == wx.ID_OK:
          fin = dlg.GetPaths()
          ok = 1
        dlg.Destroy()
        if len(fin) > 0:
            fin = fin[0]
            SetPath(self, fin)
            path_fin = fin.rpartition(gv.SEP)[0]
            inpinfo = reader(fin)
            if inpinfo.phas[0].rpartition('.')[-1].lower() == 'xyz':
                dlg = wx.MessageDialog(self,'Invalid phase file format!\n\
                 For xyz type files use the "Single Cluster Database Tab" instead.',"ERROR", style = wx.OK|wx.CENTRE|wx.ICON_ERROR)
                result = dlg.ShowModal()
                dlg.Destroy()
                if result == wx.ID_OK:
                    dlg.Destroy()
            elif len(inpinfo.phas[0]) > 0:
                ##__phase
                self.Del_phainfo(event)
                self.namef.SetValue(os.path.basename(inpinfo.phas[0]).rpartition('.')[0])
                ##_____space group
#                 self.sgf.SetValue(self.get_SG(inpinfo.phas[1]))
                self.sgs_btn.SetValue(self.get_SG(inpinfo.phas[1]))
                ##______
                self.naf.SetValue(inpinfo.phas[2])
#                 self.pear = self.get_PS(self.sgf.GetValue().strip())
                self.pear = self.get_PS(self.sgs_btn.GetValue().strip())
                if inpinfo.phas[3].lower() == 'p':
                    self.rbccp.SetValue(True)
                elif inpinfo.phas[3].lower() == 's':
                    self.rbccs.SetValue(True)
                self.xorf.SetValue(inpinfo.phas[4].split()[0])
                self.yorf.SetValue(inpinfo.phas[4].split()[1])
                self.zorf.SetValue(inpinfo.phas[4].split()[2])
                if (inpinfo.phas[6].lower() == 'y' or inpinfo.phas[6] == '1'):
                    self.rbocc1.SetValue(True)
                if (inpinfo.phas[6].lower() == 'n' or inpinfo.phas[6] == '0'):
                    self.rbocc0.SetValue(True)
                if len(inpinfo.phas[7]) > 0:
                    if inpinfo.phas[7].split()[0].lower() == 'WelbIsot':
                        self.rbis.SetValue(True)
                    elif inpinfo.phas[7].split()[0].lower() == 'WelbAnys':
                        self.rban.SetValue(True)
                    self.parff.SetValue(inpinfo.phas[7].partition(' ')[-1])
                else :
                    self.rbis.SetValue(True)
                    self.parff.SetValue('')
                ##__shape
#                 self.shaf.SetValue(inpinfo.shap[0])
                if 'sph' in inpinfo.shap[0].lower(): self.rbshas.SetValue(True)
                elif 'qbe' in inpinfo.shap[0].lower(): self.rbshaq.SetValue(True)
                elif 'par' in inpinfo.shap[0].lower(): self.rbshap.SetValue(True)
                elif 'cyl' in inpinfo.shap[0].lower(): self.rbshac.SetValue(True)
                elif 'hex' in inpinfo.shap[0].lower(): self.rbshah.SetValue(True)
                self.df.SetValue(inpinfo.shap[1])
                self.nf.SetValue(inpinfo.shap[2])
                self.dpf.SetValue(inpinfo.shap[3])
                self.lf.SetValue(inpinfo.shap[4])
                self.n1pf.SetValue(inpinfo.shap[5])
                self.n2pf.SetValue(inpinfo.shap[6])
#                 if (inpinfo[18].lower() == 'y' or inpinfo[18] == '1'):
#                     self.rbxyz1.SetValue(True)
#                 if (inpinfo[18].lower() == 'n' or inpinfo[18] == '0'):
#                     self.rbxyz0.SetValue(True)
                ##__sampling
                if (inpinfo.samp[0] == 'One' or inpinfo.samp[0] == 'one'):
                    self.samrb1.SetValue(True)
                elif (inpinfo.samp[0] == 'All' or inpinfo.samp[0] == 'all'):
                    self.samrb2.SetValue(True)
#                 self.saf.SetValue(inpinfo.samp[0])
                self.wlf.SetValue(inpinfo.samp[1])
                self.ttmf.SetValue(inpinfo.samp[2])
                ##__todo
                if inpinfo.todo == 'all_clusters':
                    self.rball.SetValue(True)
                elif inpinfo.todo == 'largest_only':
                    self.rbla.SetValue(True)
                ##______loading pha/xyz file
                pha = inpinfo.phas[0]
                epha = inpinfo.phas[0].rpartition('.')[-1]
                if epha.lower() == 'pha': 
                    if os.path.isfile(pha):
                        inputdata = reader(pha)
                        asyvals = inputdata.phaseinfo
                        if len(asyvals[3]) > 0:
                            for col in range(6):
                                self.asygrid.SetCellValue(1, col + 1, asyvals[1][col])
                                self.asygrid.SetCellAlignment(1, col + 1, wx.ALIGN_RIGHT, - 1)
                            for row in range(len(asyvals[3])):
                                for col in range(7):
                                    self.asygrid.SetCellValue(row + 3, col, asyvals[3][row][col])
                                    if col == 1:
                                        self.asygrid.SetCellAlignment(row + 3, col, wx.ALIGN_CENTRE, -1)
                                    elif col > 1:
                                        self.asygrid.SetCellAlignment(row + 3, col, wx.ALIGN_RIGHT, -1)
                    else:
                        msg = "Can't find %s file or read data!"%pha
                        dlg = wx.MessageDialog(self,msg, "ERROR", wx.OK)
                        result = dlg.ShowModal()
                        dlg.Destroy()
                        if result == wx.ID_OK:
                            dlg.Destroy()

    def SaveInp(self, outfile = None):
        cwd = os.getcwd() + gv.SEP
        xpha = self.namef.GetValue()
        fname = cwd + xpha + '.ddb'
        if outfile != None:
            ff = outfile.rpartition(gv.SEP)[-1]
            fname = ff.rpartition('.')[0]
        SetPath(self, fname + '.ddb')
        inpout = open(fname + '.ddb', 'w')
        ##__phase
        print('!', file = inpout)
        print('! PHASE SECTION', file = inpout)
        print('! ', file = inpout)
        print('Phase_Name (.pha/.xyz) (M) :   %s.pha'%(cwd + xpha), file = inpout)
#         xx = self.get_SG(self.sgf.GetValue().strip())
        xx = self.get_SG(self.sgs_btn.GetValue().strip())
        print('Spacegroupnumber_orig (M) :   ', xx, file = inpout)
        print('Atomic Species No. (M) :   ', self.naf.GetValue(), file = inpout)
        print('Cell Origin (M) :   %s %s %s'%(self.xorf.GetValue(), \
                     self.yorf.GetValue(), self.zorf.GetValue()), file = inpout)
#         self.pear = self.get_PS(self.sgf.GetValue().strip())
        self.pear = self.get_PS(self.sgs_btn.GetValue().strip())
        print('Pearson Symbol (M) (max 4 ch.) :   ', self.pear, file = inpout)
        if self.rbccp.GetValue():
            print('Constr :   P', file = inpout)
        elif self.rbccp.GetValue() == False:
            print('Constr :   S', file = inpout)
        if self.rbocc1.GetValue():
            print('OCC1  y', file = inpout)
        elif self.rbocc1.GetValue() == False:
            print('OCC1  n', file = inpout)
        if len(self.parff.GetValue()) > 0:
            if self.rbis.GetValue() : welb = 'WelbIsot'
            elif self.rbis.GetValue() == False : welb = 'WelbAnys'
            print("PARA  %s %s"%(welb, self.parff.GetValue()), file = inpout)
        ##__shape
        print('!', file = inpout)
        print('! SHAPE SECTION', file = inpout)
        print('! ', file = inpout)
        clu_shape = ''
        if self.rbshas.GetValue() : clu_shape = 'SPH'
        elif self.rbshaq.GetValue() : clu_shape = 'QBE'
        elif self.rbshap.GetValue() : clu_shape = 'PAR'
        elif self.rbshac.GetValue() : clu_shape = 'CYL'
        elif self.rbshah.GetValue() : clu_shape = 'HEX'
        print('Shape of Clusters (M) :   ', clu_shape, file = inpout)
#         print('Shape of Clusters (M) :   ', self.shaf.GetValue(), file = inpout)
        print('Diam_max/Edge_max of SPH/QBE (nm) :   ', self.df.GetValue(), file = inpout)
        print('N_max of SPH/QBE :   ', self.nf.GetValue(), file = inpout)
        print('D_max of PAR/CYL/HEX (nm) :   ', self.dpf.GetValue(), file = inpout)
        print('L_max of PAR/CYL/HEX (nm) :   ', self.lf.GetValue(), file = inpout)
        print('N1_max of PAR/CYL/HEX :   ', self.n1pf.GetValue(), file = inpout)
        print('N2_max of PAR/CYL/HEX :   ', self.n2pf.GetValue(), file = inpout)
#                 if self.rbxyz1.GetValue():
#                     print('XYZ?  y', file = inpout)
#                 elif self.rbxyz1.GetValue() == False:
#                     print('XYZ?  n', file = inpout)
        ##__sampling
        print('!', file = inpout)
        print('! SAMPLING SECTION', file = inpout)
        print('! ', file = inpout)
        if self.samrb2.GetValue():
            print('Sampling (M) :   All', file = inpout)
        else:
            print('Sampling (M) :   One', file = inpout)
#         print('Sampling (M) :   ', self.saf.GetValue(), file = inpout)
        print('Wavelength (M) :   ', self.wlf.GetValue(), file = inpout)
        print('2-Theta Max (M) :   ', self.ttmf.GetValue(), file = inpout)
        ##__sampling
        print('!', file = inpout)
        print('! TODO SECTION', file = inpout)
        print('! ', file = inpout)
        if self.rball.GetValue():
            print('TODO  all_clusters', file = inpout)
        elif self.rball.GetValue() == False:
            print('TODO  largest_only', file = inpout)
        inpout.close()
        ##__write .pha file
        ck = 0
        for col in range(6):
            if len(self.asygrid.GetCellValue(1, col + 1)) > 0 : ck += 1
        if ck == 6:
            phaname = self.namef.GetValue().strip()
            phaout = open(cwd + phaname + '.pha', 'w')
            print('Title ', self.namef.GetValue(), file = phaout)
            celcons = ''
            for col in range(6):
                celcons += '%2s%7s'%(' ', self.asygrid.GetCellValue(1, col + 1))
            print('Cell ', celcons, file = phaout)
#             print(phaout, 'Space ', self.sgf.GetValue())
            print('Space ', self.sgs_btn.GetValue(), file = phaout)
            nsites = 0
            for row in range(self.gridrows - 3):
                atco = ''
                fillcell = 0
                for col in range(7):
                    xx =  self.asygrid.GetCellValue(row + 3, col)
                    if (type(xx) != None and len(xx) > 0):
                        fillcell += 1
                if fillcell == 0: break
                if fillcell > 1: nsites += 1
                if fillcell < 7:
                    dlg = wx.MessageDialog(self,'One or more value of the asymmetric unit is missing,\n \
                    incomplete .pha file!',"ERROR", wx.OK)
                    result = dlg.ShowModal()
                    dlg.Destroy()
                    if result == wx.ID_OK:
                        dlg.Destroy()
            for row in range(nsites):
                atco = ''
                for col in range(7):
                    atco += '%2s%7s'%(' ', self.asygrid.GetCellValue(row + 3, col))
                print('Coord ', atco, file = phaout)
            phaout.close()
    
    def SaveInpFile(self, event):
        fout = ''
        dialog = wx.FileDialog(
            self, message = "Save file ", defaultDir = os.getcwd(), wildcard = '*.ddb',
             style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dialog.ShowModal() == wx.ID_OK:
            fout = dialog.GetPath()
            pathdb = fout.rpartition(gv.SEP)[0]
            os.chdir(pathdb)
            if len(fout.rpartition(gv.SEP)[-1]) > 0:
                self.SaveInp(fout)
        else:
            _userCancel = dialog.Destroy()
            return _userCancel
            dialog.Destroy()

########################################################################
class Single(wx.lib.scrolledpanel.ScrolledPanel):
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        vbox = wx.BoxSizer(wx.VERTICAL)

        hbox3 = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hbox3.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .ddb')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_DBinp)
        ldin_btn.SetToolTip(wx.ToolTip("Load input information from a .ddb file"))    
        hbox3.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAV_DBinp)
        svf_btn.SetToolTip(wx.ToolTip("Save input values to input files"))    
        hbox3.Add(svf_btn, flag = wx.ALL, border = 3)

        vbox.Add(hbox3, flag = wx.ALIGN_RIGHT|wx.BOTTOM|wx.TOP, border = 20)

        phabox = wx.StaticBox(self, - 1, 'Phase')
        phaboxsizer = wx.StaticBoxSizer(phabox, wx.VERTICAL)
        self.xyz = ''
        hbox0 = wx.BoxSizer(wx.HORIZONTAL)
        flst = wx.StaticText(self, label = "File", size = (100, - 1))
        hbox0.Add(flst, flag = wx.LEFT, border = 10)
        brw_btn = wx.Button(self, label = "...", size = (50, 20))
        brw_btn.Bind(wx.EVT_BUTTON, self.brw_btn_click)
        brw_btn.SetToolTip(wx.ToolTip("Browse and select one .xyz file .."))
        hbox0.Add(brw_btn, flag = wx.LEFT, border = 20)
        self.flsf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (400, - 1))
        self.flsf.SetMaxLength(132)
        #self.flsf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        hbox0.Add(self.flsf, 1, wx.LEFT|wx.RIGHT, border = 10)
        phaboxsizer.Add(hbox0)#, flag = wx.EXPAND|wx.ALIGN_LEFT|wx.TOP, border = 20)

        hbox01 = wx.BoxSizer(wx.HORIZONTAL)
        dist = wx.StaticText(self, label = "Reduced\nminimal distance", size = (120, - 1))
        hbox01.Add(dist, flag = wx.LEFT, border = 10)
        self.disf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.disf.SetMaxLength(132)
        hbox01.Add(self.disf, 1, flag = wx.LEFT, border = 10)
        phaboxsizer.Add(hbox01, flag = wx.ALIGN_LEFT|wx.TOP, border = 20)
        
        hbox02 = wx.BoxSizer(wx.HORIZONTAL)
        dent = wx.StaticText(self, label = r"Density", size = (120, - 1))
        hbox02.Add(dent, flag = wx.LEFT, border = 10)
        self.denf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.denf.SetMaxLength(132)
        hbox02.Add(self.denf, flag = wx.EXPAND|wx.LEFT, border = 10)
        udt = wx.StaticText(self, label = r"[g/cm3]", size = (120, - 1))
        hbox02.Add(udt, flag = wx.LEFT, border = 5)
        phaboxsizer.Add(hbox02, flag = wx.ALIGN_LEFT|wx.TOP, border = 20)

        vbox.Add(phaboxsizer)

        sambox = wx.StaticBox(self, - 1, 'Sampling')
        samboxsizer = wx.StaticBoxSizer(sambox, wx.VERTICAL)
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        samt = wx.StaticText(self, label = "Sampling", size = (100, - 1))
        hbox1.Add(samt, flag = wx.LEFT, border = 10)
        self.rb1 = wx.RadioButton(self, - 1, 'One', style = wx.RB_GROUP)
#         self.rb1.SetValue(True)
        hbox1.Add(self.rb1, flag = wx.LEFT, border = 20)
        self.rb2 = wx.RadioButton(self, - 1, 'All')
        hbox1.Add(self.rb2, flag = wx.LEFT, border = 10)

        samboxsizer.Add(hbox1, flag = wx.ALIGN_LEFT|wx.TOP, border = 20)

        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        wlt = wx.StaticText(self, label = "Wavelength [A]", size = (120, - 1))
        hbox2.Add(wlt, flag = wx.LEFT, border = 10)
        self.wlf = wx.TextCtrl(self, style = wx.TE_RIGHT, size = (200, - 1))
        self.wlf.SetMaxLength(132)
        hbox2.Add(self.wlf, 1, flag = wx.LEFT, border = 10)
        ttmt = wx.StaticText(self, label = u"2\u03B8 max [deg]", size = (120, - 1))
        hbox2.Add(ttmt, flag = wx.LEFT, border = 20)
        self.ttmf = wx.TextCtrl(self, style = wx.TE_RIGHT)
        self.ttmf.SetMaxLength(132)
        hbox2.Add(self.ttmf, flag = wx.EXPAND|wx.LEFT, border = 10)

        samboxsizer.Add(hbox2, flag = wx.ALIGN_LEFT|wx.TOP, border = 20)
        vbox.Add(samboxsizer)

        self.SetSizer(vbox)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    def brw_btn_click(self, event):
      """
      Create and show the Open FileDialog
      """
      dlg = wx.FileDialog(self, message = "Choose one .xyz file", 
      defaultFile = "", wildcard = '*.xyz', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
      if dlg.ShowModal() == wx.ID_OK:
        fi =  dlg.GetPaths()[0]
        if len(fi) > 0:
          self.xyz = fi
          self.flsf.SetValue(self.xyz)
      dlg.Destroy()

    def Del_inpinfo(self, event):
        self.flsf.SetValue('')
        self.disf.SetValue('')
        self.denf.SetValue('')
        self.rb1.SetValue(True)
        self.wlf.SetValue('')
        self.ttmf.SetValue('')

    def LD_DBinp(self, event):
        """
        something
        """
        self.Del_inpinfo(event)
        fin, inpinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one .ddb file", 
        defaultFile = "*", wildcard = '*.ddb', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
          fin = dlg.GetPaths()
        dlg.Destroy()
        if len(fin) > 0:
            fin = fin[0]
            SetPath(self, fin)
            path_fin = fin.rpartition(gv.SEP)[0]
            inpinfo = reader(fin)
        if len(inpinfo.phas[0]) > 0:
            ##__phase
            self.flsf.SetValue(inpinfo.phas[0])
            if inpinfo.phas[0].rpartition('.')[-1].lower() == 'pha':
                dlg = wx.MessageDialog(self,'Invalid phase file format!\n\
                 For pha type files use the "Population Cluster Database Tab" instead.',"ERROR", style = wx.OK|wx.CENTRE|wx.ICON_ERROR)
                result = dlg.ShowModal()
                dlg.Destroy()
                if result == wx.ID_OK:
                    dlg.Destroy()
            self.disf.SetValue(inpinfo.phas[8])
            self.denf.SetValue(inpinfo.phas[9])
            ##__sampling
            if inpinfo.samp[0].lower() == 'all': self.rb2.SetValue(True)
            else : self.rb1.SetValue(True)
            self.wlf.SetValue(inpinfo.samp[1])
            self.ttmf.SetValue(inpinfo.samp[2])

    def SAV_DBinp(self, event):
        fout = ''
        dialog = wx.FileDialog(
            self, message = "Save file ", defaultDir = os.getcwd(), 
            wildcard = '*.ddb', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dialog.ShowModal() == wx.ID_OK:
            fout = dialog.GetPath()
            pathdb = fout.rpartition(gv.SEP)[0]
            os.chdir(pathdb)
            cwd = os.getcwd() + gv.SEP
            if len(fout) > 0:
                inpout = open(fout, 'w')
                SetPath(self, fout)
                ##__phase
                print('!', file = inpout)
                print('! PHASE SECTION', file = inpout)
                print('! ', file = inpout)
                xpha = self.flsf.GetValue()
                epha = xpha.rpartition('.')[-1]
                print('Phase_Name (.pha/.xyz) (M) :   %s'%xpha, file = inpout)
                print('Spacegroupnumber_orig (M) :   ', file = inpout)
                print('Atomic Species No. (M) :   ', file = inpout)
                print('Cell Origin (M) :   %s %s %s'%('', '', ''), file = inpout)
                print('Pearson Symbol (M) (max 4 ch.) :   ', file = inpout)
                print('Constr :   ', file = inpout)
                print('OCC1  n', file = inpout)
                if len(self.disf.GetValue()) > 0:
                    print("Reduced minimal distance  %s"%(self.disf.GetValue()), file = inpout)
                if len(self.denf.GetValue()) > 0:
                    print("Density (g/cm^3)  %s"%(self.denf.GetValue()), file = inpout)
                ##__shape
                print('!', file = inpout)
                print('! SHAPE SECTION', file = inpout)
                print('! ', file = inpout)
                print('Shape of Clusters (M) :   ', file = inpout)
                print('Diam_max/Edge_max of SPH/QBE (nm) :   ', file = inpout)
                print('N_max of SPH/QBE :   ', file = inpout)
                print('D_max of PAR/CYL/HEX (nm) :   ', file = inpout)
                print('L_max of PAR/CYL/HEX (nm) :   ', file = inpout)
                print('N1_max of PAR/CYL/HEX :   ', file = inpout)
                print('N2_max of PAR/CYL/HEX :   ', file = inpout)
                ##__sampling
                print('!', file = inpout)
                print('! SAMPLING SECTION', file = inpout)
                print('! ', file = inpout)
                if self.rb1.GetValue() : 
                    print('Sampling (M) :   One', file = inpout)
                elif self.rb2.GetValue():
                    print('Sampling (M) :   All', file = inpout)
                print('Wavelength (M) :   ', self.wlf.GetValue(), file = inpout)
                print('2-Theta Max (M) :   ', self.ttmf.GetValue(), file = inpout)
                ##__todo
                print('!', file = inpout)
                print('! TODO SECTION', file = inpout)
                print('! ', file = inpout)
                print('TODO  largest_only', file = inpout)
                inpout.close()

    def LD_MOLinp(self, event):
        """
        something
        """
        fin, molinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one molmkd.ini file", 
        defaultFile = "molmkd.ini", wildcard = '*.ini', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
            fin = dlg.GetPaths()
            if len(fin) > 0:
                fin = fin[0]
                SetPath(self, fin)
                inputdata = reader(fin)
                molinfo = inputdata.molecinfo
        dlg.Destroy()
        if len(molinfo[0]) > 0:
            ##__file
            self.flsf.SetValue(molinfo[0])
            ##__sampling
            if (molinfo[1] == 'One' or molinfo[1] == 'one'):
                self.rb1.SetValue(True)
            elif (molinfo[1] == 'All' or molinfo[1] == 'all'):
                self.rb2.SetValue(True)
            ##_wavelength
            self.wlf.SetValue(molinfo[2])
            ##ttmax
            self.ttmf.SetValue(molinfo[3])
            ##__distance
            self.disf.SetValue(molinfo[4])
            ##__density
            self.denf.SetValue(molinfo[5])

    def SAV_MOLinp(self, event):
        dialog =  wx.DirDialog(self, "Please choose your Database directory:", \
        defaultPath = gset.User_Path, style = wx.DD_CHANGE_DIR , pos = (-1, -1))
        if dialog.ShowModal() == wx.ID_OK:
            path_molinp = dialog.GetPath()
            iniout = open(path_molinp + gv.SEP + 'molmkd.ini', 'w')
            SetPath(self, path_molinp + gv.SEP + 'molmkd.ini')
            ##__file
            print(self.flsf.GetValue().strip(), file = iniout)
            ##__sampling
            if self.rb1.GetValue():
                samp = 'one'
            if self.rb2.GetValue():
                samp = 'all'
            print('%s  %10s  %8s'%(samp, self.wlf.GetValue(), self.ttmf.GetValue()), file = iniout)
            ##__reduced minimal distance
            print(self.disf.GetValue(), file = iniout)
            ##__density
            print('dens  %10s'%(self.denf.GetValue()), file = iniout)
            iniout.close()
        else:
            _userCancel = dialog.Destroy()
            return _userCancel
            dialog.Destroy()

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'CLaUDe Editor', wx.ICON_INFORMATION)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
class Diffractor(wx.lib.scrolledpanel.ScrolledPanel):
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        if gset.Platform.startswith('dar'):
            font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)

        vbox = wx.BoxSizer(wx.VERTICAL)

        hboxb = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hboxb.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .inp')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_DIFFinp)
        ldin_btn.SetToolTip(wx.ToolTip("Load a diffractor.inp input file"))    
        hboxb.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAV_DIFFinp)
        svf_btn.SetToolTip(wx.ToolTip("Save in the input file (diffractor.inp)"))    
        hboxb.Add(svf_btn, flag = wx.ALL, border = 3)

        vbox.Add(hboxb, flag = wx.ALIGN_RIGHT|wx.TOP, border = 0)

        self.kwd = 'VARX WLEN RANG RAYS SOFQ DIVI IMAX HKLS CMPT PATH FILE NATO ZELE BATO ATOC\
             FPRI FDPR NSCL'.split()
        man = '*  twotheta / q,!  wavelength e.g. 1.54139,!  min max step,!  X / e / n,*  0 / 1,*  fa2 / f2a / Za2 / Z2a,*  0.0,*  0 / 1,\
*  [add Compton scattering (0 / 1)],!  [path to SAMPTO folder],!  [.smp file],!  [# atomic specie e.g. 3],!  [Z values e.g. 1 2 4],\
!  [B factors e.g. 0.3 0.3 0.3],*  [occupancy e.g.  1.0 1.0 1.0],*  [f1 anomalous scattering factors],\
*  [f2 anomalous scattering factors],*  [neutron scattering legths]'.split(',')
        self.ipath, self.ifile = 8, 9
        self.fld = []
        for k in range(len(self.kwd)):
            hbox = wx.BoxSizer(wx.HORIZONTAL)
            flagt = wx.StaticText(self, label = self.kwd[k], size = (50, - 1))
            hbox.Add(flagt, flag = wx.LEFT, border = 10)
            if (self.kwd[k] == 'PATH' or self.kwd[k] == 'FILE'):
                brw_btn = wx.Button(self, label = "...", size = (50, 20))
                if (self.kwd[k] == 'PATH'):
                    brw_btn.Bind(wx.EVT_BUTTON, self.SetSAMdir)
                elif (self.kwd[k] == 'FILE'):
                    brw_btn.Bind(wx.EVT_BUTTON, self.SetSMP)
                brw_btn.SetToolTip(wx.ToolTip("Browse and select .."))
                hbox.Add(brw_btn, flag = wx.LEFT, border = 10)
                self.flagf = wx.TextCtrl(self, style = wx.TE_LEFT, name = self.kwd[k], size = (260, 20))
                #self.flagf.SetMaxLength(132)
#                 self.fld += [self.flagf]
                hbox.Add(self.flagf, 1, wx.LEFT, border = 10)
            else :
                self.flagf = wx.TextCtrl(self, style = wx.TE_LEFT, name = self.kwd[k], size = (260, 20))
                #self.flagf.SetMaxLength(132)
#                 self.fld += [self.flagf]
                hbox.Add(self.flagf, 1, wx.LEFT, border = 70)
            mant = wx.StaticText(self, label = man[k], size = (300, - 1))
            hbox.Add(mant, flag = wx.LEFT, border = 10)
#             vbox.Add(hbox, flag = wx.EXPAND|wx.ALIGN_LEFT|wx.TOP, border = 10)
            vbox.Add(hbox, flag = wx.ALIGN_LEFT|wx.TOP, border = 10)

        hboxt = wx.BoxSizer(wx.HORIZONTAL)
        legend = '! : always mandatory\n? : conditionally optional\n* : always optional'
        legt = wx.StaticText(self, label = legend, size = (500, - 1))
        hboxt.Add(legt, 1, flag = wx.LEFT, border = 10)
        vbox.Add(hboxt, flag = wx.ALIGN_LEFT|wx.TOP, border = 10)

        self.SetSizer(vbox)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    def SetSAMdir(self, event):
        dialog =  wx.DirDialog(self, "Please choose a SAMPTO *  folder", \
          defaultPath = os.getcwd(), style = wx.DD_CHANGE_DIR , pos = (-1, -1))
        if dialog.ShowModal() == wx.ID_OK:
            self.samdir = dialog.GetPath()
            xx = wx.FindWindowByName('PATH')
            xx.SetValue(self.samdir[:-4])
        dialog.Destroy() 


    def SetSMP(self, event):
      """
      Create and show the Open FileDialog
      """
      dlg = wx.FileDialog(self, message = "Choose one file", 
      defaultFile = "", wildcard = '*.smp', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
      if dlg.ShowModal() == wx.ID_OK:
        fi =  dlg.GetPaths()[0]
        if len(fi) > 0:
          self.smpf = fi.rpartition(gv.SEP)[-1]
          xx = wx.FindWindowByName('FILE')
          xx.SetValue(self.smpf)
      dlg.Destroy()
      

    def LD_DIFFinp(self, event):
        """
        something
        """
        fin, diffinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one diffractor.inp file", 
        defaultFile = "diffractor.inp", wildcard = '*.inp', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
            fin = dlg.GetPaths()
            if len(fin) > 0:
                fin = fin[0]
                SetPath(self, fin)
                inputdata = reader(fin)
                for k in self.kwd:
                    if k in inputdata.diffinfo:
                        xx = wx.FindWindowByName(k)
                        xx.SetValue(inputdata.diffinfo[k])
                del(xx)

        dlg.Destroy()

    def SAV_DIFFinp(self, event):
        fout = ''
        dlg = wx.FileDialog(
            self, message = "Save file ", defaultDir = os.getcwd(), 
            defaultFile = "diffractor.inp", wildcard = '*.inp', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dlg.ShowModal() == wx.ID_OK:
            fout = dlg.GetPath()
            dlg.Destroy()
            if len(fout) > 0:
                SetPath(self, fout)
                diffout = open(fout, 'w')
                for k in self.kwd:
                    xx = wx.FindWindowByName(k)
                    ival = xx.GetValue()
                    if len(ival) > 0:
                        print('%s  %s'%(k, ival), file = diffout)
                diffout.close()
        else:
            _userCancel = dlg.Destroy()
            return _userCancel
            dlg.Destroy()

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'CLaUDe Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
class Gofr(wx.lib.scrolledpanel.ScrolledPanel):
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    def __init__(self, parent):
        """"""
        wx.lib.scrolledpanel.ScrolledPanel.__init__(self, parent = parent)
        self.SetAutoLayout(1)
        self.SetupScrolling()

        if gset.Platform.startswith('dar'):
            font = wx.Font(12, wx.FONTFAMILY_DEFAULT, wx.FONTSTYLE_NORMAL, wx.FONTWEIGHT_NORMAL)
            self.SetFont(font)

        vbox = wx.BoxSizer(wx.VERTICAL)

        hboxb = wx.BoxSizer(wx.HORIZONTAL)
        hlp_btn  = wx.Button(self, label = 'Help')
        hlp_btn.Bind(wx.EVT_BUTTON, self.OnHelp)
        hlp_btn.SetToolTip(wx.ToolTip("Open the Help window"))    
        hboxb.Add(hlp_btn, flag = wx.ALL, border = 3)
        ldin_btn  = wx.Button(self, label = 'LOAD .inp')
        ldin_btn.Bind(wx.EVT_BUTTON, self.LD_DoPDFinp)
        ldin_btn.SetToolTip(wx.ToolTip("Load a dopdf.inp input file"))    
        hboxb.Add(ldin_btn, flag = wx.ALL, border = 3)
        svf_btn  = wx.Button(self, label = 'SAVE')
        svf_btn.Bind(wx.EVT_BUTTON, self.SAV_DoPDFinp)
        svf_btn.SetToolTip(wx.ToolTip("Save in the input file (dopdf.inp)"))    
        hboxb.Add(svf_btn, flag = wx.ALL, border = 3)

        vbox.Add(hboxb, flag = wx.ALIGN_RIGHT|wx.TOP, border = 0)

        self.kwd = 'NFIL FILE NCOLS TTCOL I_COL E_COL RAYS WLEN ARANG RRANG VALEN DONOR SCALE\
             BROAD NQCUT QCUTV QMIN INCOH NATO ZELE CHEM'.split() ##_removed [BINW] 
        man = '!  [n. of files to process (1)],!  filename,*  [n. of columns to read from file (3)],\
*  [column number for 2theta values (1)],*  [column number for intensity values (2)],*  [column number for sigma values (3)],\
?  X / n / e [radiation type],!  [wavelength (0.1000)],!  min max step for 2theta [input array],!  min max step for r [output array],\
*  0 / 1 [to apply modified X-ray form factors for 5<=Z<=9],*  fa2 / f2a / Za2 / Z2a [normalisation options],\
*  m / t / p [scaling option],*  broadening option,!  1 [n. of Q cut-offs],!  [Q-cutoff(s) value(s) (20.00)],\
!  [effective Qmin values(s) (0.1)],!  1 / 0 [subtract incoherent scattering],!  # [n. of atomic specie],\
!  Z1 Z2 .. [Z values],!  c1 c2.. [fractional compositions]'.split(',')
        self.ifile = 1
        self.fld = []
        for k in range(len(self.kwd)):
            hbox = wx.BoxSizer(wx.HORIZONTAL)
            flagt = wx.StaticText(self, label = self.kwd[k], size = (50, - 1))
            hbox.Add(flagt, flag = wx.LEFT, border = 10)
            if (self.kwd[k] == 'PATH' or self.kwd[k] == 'FILE'):
                brw_btn = wx.Button(self, label = "...", size = (50, 20))
                if (self.kwd[k] == 'PATH'):
                    brw_btn.Bind(wx.EVT_BUTTON, self.SetSAMdir)
                elif (self.kwd[k] == 'FILE'):
                    brw_btn.Bind(wx.EVT_BUTTON, self.SetFIL)
                brw_btn.SetToolTip(wx.ToolTip("Browse and select .."))
                hbox.Add(brw_btn, flag = wx.LEFT, border = 10)
                self.flagf = wx.TextCtrl(self, style = wx.TE_LEFT, name = self.kwd[k] + '_PDF', size = (260, 20))
                #self.flagf.SetMaxLength(132)
#                 self.fld += [self.flagf]
                hbox.Add(self.flagf, 1, wx.LEFT, border = 10)
            else :
                self.flagf = wx.TextCtrl(self, style = wx.TE_LEFT, name = self.kwd[k] + '_PDF', size = (260, 20))
                #self.flagf.SetMaxLength(132)
#                 self.fld += [self.flagf]
                hbox.Add(self.flagf, 1, wx.LEFT, border = 70)
            mant = wx.StaticText(self, label = man[k], size = (400, - 1))
            hbox.Add(mant, flag = wx.LEFT, border = 10)
            vbox.Add(hbox, flag = wx.ALIGN_LEFT|wx.TOP, border = 10)

        hboxt = wx.BoxSizer(wx.HORIZONTAL)
        legend = '! : always mandatory\n? : conditionally optional\n *  : always optional'
        legt = wx.StaticText(self, label = legend, size = (500, - 1))
        hboxt.Add(legt, 1, flag = wx.LEFT, border = 10)
        vbox.Add(hboxt, flag = wx.ALIGN_LEFT|wx.TOP, border = 10)


        self.SetSizer(vbox)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    def SetSAMdir(self, event):
        dialog =  wx.DirDialog(self, "Please choose a SAMPTO *  folder", \
          defaultPath = os.getcwd(), style = wx.DD_CHANGE_DIR , pos = (-1, -1))
        if dialog.ShowModal() == wx.ID_OK:
            self.samdir = dialog.GetPath()
            xx = wx.FindWindowByName('PATH')
            xx.SetValue(self.samdir[:-4])
        dialog.Destroy() 


    def SetFIL(self, event):
      """
      Create and show the Open FileDialog
      """
      dlg = wx.FileDialog(self, message = "Choose one file", 
      defaultFile = "", wildcard = '*', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
      if dlg.ShowModal() == wx.ID_OK:
        fi =  dlg.GetPaths()[0]
        print('pdf ', fi)
        if len(fi) > 0:
          xx = wx.FindWindowByName('FILE_PDF')
          xx.SetValue(fi)
      dlg.Destroy()
      

    def LD_DoPDFinp(self, event):
        """
        something
        """
        fin, dopfinfo = '', ()
        dlg = wx.FileDialog(self, message = "Choose one dopdf.inp file", 
        defaultFile = "dopdf.inp", wildcard = '*.inp', style = wx.FD_OPEN | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
            fin = dlg.GetPaths()[0]
            if len(fin) > 0:
                SetPath(self, fin)
                fin = fin
                inputdata = reader(fin)
        dlg.Destroy()
        if len(inputdata.pdfinfo) > 0:
            for k in self.kwd:
                if k in inputdata.pdfinfo:
                    xx = wx.FindWindowByName(k + '_PDF')
                    xx.SetValue(inputdata.pdfinfo[k])
                    del(xx)
        
    
    def SAV_DoPDFinp(self, event):
        fout = ''
        dlg = wx.FileDialog(
            self, message = "Save file ", defaultDir = os.getcwd(), 
            defaultFile = "dopdf.inp", wildcard = '*.inp', style = wx.FD_SAVE|wx.FD_OVERWRITE_PROMPT|wx.FD_CHANGE_DIR
            )
        if dlg.ShowModal() == wx.ID_OK:
            fout = dlg.GetPath()
            dlg.Destroy()
            if len(fout) > 0:
                SetPath(self, fout)
                dopdfout = open(fout, 'w')
                for k in self.kwd:
                    xx = wx.FindWindowByName(k + '_PDF')
                    ival = xx.GetValue()
                    if len(ival) > 0:
                        print('%s  %s'%(k, ival), file = dopdfout)
                dopdfout.close()
        else:
            _userCancel = dlg.Destroy()
            return _userCancel
            dlg.Destroy()

    ### HELP/ABOUT
    def OnHelp(self, event):
        aboutText = ".. WORK IN PROGRESS! - -  COMING SOON ..."
        dlg = wx.MessageDialog(self, aboutText, 'CLaUDe Editor', wx.OK)
        result = dlg.ShowModal()
        dlg.Destroy()
        if result == wx.ID_OK:
            dlg.Destroy()

########################################################################
class ButtonsPanel(wx.Panel):
    """""" 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    print(gset.GUI_Path)
    def __init__(self, parent):
        """Constructor"""
        wx.Panel.__init__(self, parent = parent)

        self.vbox = wx.BoxSizer(wx.VERTICAL)
#         self.setjob_btn = wx.Button(self, id = -1, label = 'Set folder ...', size = (80, 25))
#         self.setjob_btn.Bind(wx.EVT_BUTTON, self.SetDir)
#         self.setjob_btn.SetToolTip(wx.ToolTip("Set working folder"))
#         self.vbox.Add(self.setjob_btn, flag = wx.ALL, border = 10)
        cbox = wx.StaticBox(self, - 1, 'MAKE')##(18, 40), size = (470, 58))
        self.boxsizerc = wx.StaticBoxSizer(cbox, wx.VERTICAL)
        self.cell_btn = wx.Button(self, id = -1, label = 'CELL', size = (80, 28), name='cell_btn') 
#         self.cell_btn.Bind(wx.EVT_BUTTON, self.cell_click)
        self.cell_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='ddb', button_name='cell_btn', label="CELL", stop_flag='cell_stop', job_type='cell', buffer_name='ClaudeBuffer', pre_program='%sDB_PHA_CLU_x1.0'%(gset.PGM_Path):\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.cell_btn.SetToolTip(wx.ToolTip("Make the UNIT CELL"))
        self.boxsizerc.Add(self.cell_btn)
        self.xyz_btn = wx.Button(self, id = -1, label = 'XYZ', size = (80, 28), name='xyz_btn')
#         self.xyz_btn.Bind(wx.EVT_BUTTON, self.xyz_click)
        self.xyz_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='ddb', button_name='xyz_btn', label="XYZ", stop_flag='xyz_stop', job_type='xyz', buffer_name='ClaudeBuffer', pre_program='%sDB_PHA_CLU_x1.0'%(gset.PGM_Path):\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.xyz_btn.SetToolTip(wx.ToolTip("Make SPHERE/ROD CLUSTERS"))
        self.boxsizerc.Add(self.xyz_btn)
        self.buildla_btn = wx.Button(self, id = -1, label = 'LARGEST', size = (80, 28), name='buildla_btn')
#         self.buildla_btn.Bind(wx.EVT_BUTTON, self.buildla_click)
        self.buildla_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='ddb', button_name='buildla_btn', label="LARGEST", stop_flag='buildla_stop', job_type='largest', buffer_name='ClaudeBuffer', pre_program='%sDB_PHA_CLU_x1.0'%(gset.PGM_Path):\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.buildla_btn.SetToolTip(wx.ToolTip("Build LARGEST ONLY (all steps in one)"))
        self.boxsizerc.Add(self.buildla_btn)
        self.buildal_btn = wx.Button(self, id = -1, label = 'DATABASE', size = (80, 28), name='buildal_btn')
#         self.buildal_btn.Bind(wx.EVT_BUTTON, self.buildal_click)
        self.buildal_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='ddb', button_name='buildal_btn', label="DATABASE", stop_flag='buildal_stop', job_type='db', buffer_name='ClaudeBuffer', pre_program='%sDB_PHA_CLU_x1.0'%(gset.PGM_Path):\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.buildal_btn.SetToolTip(wx.ToolTip("Build DATABASE (all steps in one)"))
        self.boxsizerc.Add(self.buildal_btn)    
        self.molec_btn = wx.Button(self, id = -1, label = 'MOLEC', size = (80, 28), name='molec_btn')
#         self.molec_btn.Bind(wx.EVT_BUTTON, self.molec_click)
        self.molec_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='ddb', button_name='molec_btn', label="MOLEC", stop_flag='molec_stop', job_type='molec', buffer_name='ClaudeBuffer', pre_program='%sDB_PHA_CLU_x1.0'%(gset.PGM_Path):\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.molec_btn.SetToolTip(wx.ToolTip("Build DATABASE for a non crystalline phase"))
        self.boxsizerc.Add(self.molec_btn)
        self.pattern_btn = wx.Button(self, id = -1, label = 'PATTERN', size = (80, 28), name='pattern_btn')
#         self.pattern_btn.Bind(wx.EVT_BUTTON, self.pattern_click)
        self.pattern_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='diffractor.inp', button_name='pattern_btn', label="PATTERN", stop_flag='pattern_stop', job_type='pattern', buffer_name='ClaudeBuffer', pre_program=None:\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.pattern_btn.SetToolTip(wx.ToolTip("Calculate Single Cluster Powder Diffraction Pattern"))
        self.boxsizerc.Add(self.pattern_btn)
        self.dopdf_btn = wx.Button(self, id = -1, label = 'PDF', size = (80, 28), name='dopdf_btn')
#         self.dopdf_btn.Bind(wx.EVT_BUTTON, self.dopdf_click)
        self.dopdf_btn.Bind(wx.EVT_BUTTON, lambda event, input_type='dopdf.inp', button_name='dopdf_btn', label="PDF", stop_flag='pdf_stop', job_type='pdf', buffer_name='ClaudeBuffer', pre_program=None:\
           self.mk_click(event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program))
        self.dopdf_btn.SetToolTip(wx.ToolTip("Calculate the PDF of a Powder Diffraction Pattern"))
        self.boxsizerc.Add(self.dopdf_btn)
        self.vbox.Add(self.boxsizerc, flag = wx.LEFT|wx.RIGHT|wx.TOP, border = 10)

        ###____PLOT  - CLAUDE
        pbox1 = wx.StaticBox(self, - 1, 'PLOT')
        boxsizerp1 = wx.StaticBoxSizer(pbox1, wx.VERTICAL)
        self.pxyz_btn = wx.Button(self, id = -1, label = 'XYZ', size = (80, 28))
        self.pxyz_btn.Bind(wx.EVT_BUTTON, self.pxyz_click)
        self.pxyz_btn.SetToolTip(wx.ToolTip("See single cluster"))
        boxsizerp1.Add(self.pxyz_btn)
        self.tqis = ''
        hboxix = wx.BoxSizer(wx.HORIZONTAL)
        self.rbitt = wx.RadioButton(self, - 1, u"2\u03B8", style = wx.RB_GROUP)
        self.rbitt.SetValue(True)
        hboxix.Add(self.rbitt, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.rbiq = wx.RadioButton(self, - 1, 'q')
#         hboxix.Add(self.rbiq, flag = wx.BOTTOM|wx.TOP, border = 2)
        self.rbiQ = wx.RadioButton(self, - 1, 'Q')
        hboxix.Add(self.rbiQ, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.rbid = wx.RadioButton(self, - 1, 'd')
#         hboxix.Add(self.rbid, flag = wx.BOTTOM|wx.TOP, border = 2)
        boxsizerp1.Add(hboxix)
#         self.rblogq = wx.RadioButton(self, - 1, 'log(q)', name='plot_logq')
        self.rblogQ = wx.RadioButton(self, - 1, 'log(Q)', name='plot_logQ')
        hboxix2 = wx.BoxSizer(wx.HORIZONTAL)
#         hboxix2.Add(self.rblogq, flag = wx.BOTTOM|wx.TOP, border = 2)
        hboxix2.Add(self.rblogQ, flag = wx.BOTTOM|wx.TOP, border = 2)
        boxsizerp1.Add(hboxix2)

        hboxix3 = wx.BoxSizer(wx.HORIZONTAL)
        self.cblogI = wx.CheckBox(self, - 1, "log(I)", name='plot_logI')
        self.cblogI.SetValue(False)
        hboxix3.Add(self.cblogI, flag = wx.BOTTOM|wx.TOP, border = 2)
#         self.cbsqrtI = wx.CheckBox(self, - 1, "sqrt(I)", name='plot_sqrtI')
#         self.cbsqrtI.SetValue(False)
#         hboxix3.Add(self.cbsqrtI, flag = wx.BOTTOM|wx.TOP, border = 2)
        boxsizerp1.Add(hboxix3)
        self.cbhkl = wx.CheckBox(self, label='hkl', size=(-1,-1))
        self.cbhkl.SetValue(False)
        boxsizerp1.Add(self.cbhkl)

        self.ppattern_btn = wx.Button(self, id = -1, label = 'PATTERN', size = (80, 28))
        self.ppattern_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'tqi':\
           self.ppattern_click(event, what))
        self.ppattern_btn.SetToolTip(wx.ToolTip("Plot powder diffraction pattern simulation"))
        boxsizerp1.Add(self.ppattern_btn)
        self.gofrs = ''
        self.pgofr_btn = wx.Button(self, id = -1, label = 'G(r)', size = (80, 28))
        self.pgofr_btn.Bind(wx.EVT_BUTTON, lambda event, what = 'rpdfn':\
           self.ppattern_click(event, what))
        self.pgofr_btn.SetToolTip(wx.ToolTip("Plot radial pair distribution function"))
        boxsizerp1.Add(self.pgofr_btn)
        self.pcstmc_btn = wx.Button(self, id = -1, label = 'Custom', size = (80, 28))
        self.pcstmc_btn.Bind(wx.EVT_BUTTON, self.pcstm_click)
        self.pcstmc_btn.SetToolTip(wx.ToolTip("Custom plot"))
        boxsizerp1.Add(self.pcstmc_btn)
        self.vbox.Add(boxsizerp1, flag = wx.LEFT|wx.RIGHT|wx.TOP, border = 10)
#         self.textbuffer = wx.TextCtrl(self,wx.ID_ANY,style=wx.TE_MULTILINE)#,size=(1200,600))##pos=(8,110) |wx.TE_READONLY)
#         self.vbox.Add(self.textbuffer,1,flag=wx.EXPAND)
        self.SetSizer(self.vbox)
        self.Layout()

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 

    def SetDir(self, event):
        SetFolder(self, event)

    ####____CLAUDE
    def mk_click(self, event, input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program=None):
        input_file = GetInFile(self, input_type).rpartition(gv.SEP)[-1]
        #print 'click ', input_type, button_name, label, stop_flag, job_type, buffer_name, pre_program, input_file
        button = wx.FindWindowByName(button_name)
        if len(input_file) > 0:
            msg = '\n  %s running on %s in %s'%(label, input_file, os.getcwd())
            xx = wx.FindWindowByName(buffer_name)
            toBuffer(self, xx, msg)
            thread = threading.Thread(target=self.mk_run, args=(job_type, input_file, stop_flag, buffer_name, button, label, pre_program, ))
            thread.setDaemon(True)
            thread.start()

    def mk_run(self, job_type, input_file, stop_flag, buffer_name, button, label, pre_program=None):
        #print 'run ', job_type, input_file, stop_flag, buffer_name, label, pre_program
        self.stop_flag = 0
        if job_type == 'pattern':
            pgmx = gset.PGM_Path + 'MK_PATTERN_x1.0'
        if job_type == 'pdf':
            pgmx = gset.PGM_Path + 'MK_G_OF_R_x1.0'
        if pre_program != None:
            xx = wx.FindWindowByName(buffer_name)
            pgm0  = pre_program + ' %s'%input_file
            proc0 = subprocess.run(pgm0.split())
            if proc0.returncode != 0:
                dlg = wx.MessageDialog(self,proc0,"ERROR", wx.OK)
                result = dlg.ShowModal()
                dlg.Destroy()
                if result == wx.ID_OK:
                    dlg.Destroy()
                    return
            mk = builder(job_type, input_file)
            pgmx = mk.pgm
        if gset.Platform[:3].lower() == 'win':
            runfile = open('crun.bat', 'w')
            print("%s"%pgmx, file = runfile)
            print("pause", file = runfile)
            runfile.close()
            os.system('start "%s" %s'%(label,'crun.bat'))
        else:
            xterm_opt2 = ['-T', label, '-sb', '-e', '%s; sleep %i'%(pgmx, wtm)]
            terminal = 'xterm -geometry 120x30'.split()
            cmd1 = terminal + xterm_opt2
            proc1 = subprocess.Popen(cmd1, stdin = subprocess.PIPE, stdout = subprocess.PIPE, stderr = subprocess.STDOUT)

    ####____PLOT
    ##__plot Cluster
    def pxyz_click(self, event):
        if gset.Platform.startswith('lin'):
            if gset.AtomViewer.endswith('sh') : cmd = ['sh', gset.AtomViewer]
            else : cmd = ['. ', gset.AtomViewer]
        elif gset.Platform.startswith('dar'):
            if gset.AtomViewer.endswith('sh') : cmd = ['sh', gset.AtomViewer]
            elif gset.AtomViewer.endswith('app') : cmd = ['open', gset.AtomViewer]
            else : cmd = ['. ', gset.AtomViewer]
        elif gset.Platform.startswith('win'):
            cmd = [gset.AtomViewer]
        proc = subprocess.Popen(cmd)

    ##__plot PATTERN
    def ppattern_click(self, event, what):
        fins = []
        dlg = wx.FileDialog(self, message = "Choose one or more  * %s file"%what, 
        defaultFile = "", wildcard = '*.%s'%what, style = wx.FD_OPEN | wx.FD_MULTIPLE | wx.FD_CHANGE_DIR)
        if dlg.ShowModal() == wx.ID_OK:
            fins = dlg.GetPaths()
            dlg.Destroy()
        if len(fins) > 0:
            fs = ''
            for fk in fins:
                fs += '%s'%(' '+fk)
            if self.rbitt.GetValue(): what = what + '_tt'
#             elif self.rbiq.GetValue() == True: what = what + '_q'
            elif self.rbiQ.GetValue() == True: what = what + '_Q'
#             elif self.rbid.GetValue() == True: what = what + '_d'
#             elif self.rblogq.GetValue() == True: what = what + '_logq'
            elif self.rblogQ.GetValue() == True: what = what + '_logQ'
            if self.cblogI.GetValue() == True: what = what + '_logI'
#             if self.cbsqrtI.GetValue() == True: what = what + '_sqrtI'
            if self.cbhkl.GetValue() == True: what = what + '_hkl'
#             cmd = ['python', os.path.abspath(gset.GUI_Path) + "plotterC.py", fs, what]
            ##__HERE AND/OR ELSEWHERE THE PYTHON/PYTHONW SHOULD BE FIXED !!!!
            cmd = ['pythonw', gset.GUI_Path + "plotterC.py", fs, what]
            proc  =  subprocess.Popen(cmd)

    ##__Custom plot
    def pcstm_click(self, event):
        self.customplt_frame = CustomPlotter()
        self.customplt_frame.Show()

########################################################################
class ClaudeGUI(wx.Panel):
    """
    Frame that holds all other widgets
    """
 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
    def __init__(self, parent):
        """Constructor"""
        wx.Panel.__init__(self, parent = parent, size = (700, 760))

        splitterH = wx.SplitterWindow(self)        
        splitterV = wx.SplitterWindow(splitterH)

        notebook = wx.Notebook(splitterV, -1, wx.DefaultPosition, wx.DefaultSize, wx.NB_TOP)
        tab1 = Population(notebook)
        notebook.AddPage(tab1, "POPULATION DATABASE")
        tab12 = Single(notebook)
        notebook.AddPage(tab12, "SINGLE CLUSTER DATABASE")
        tab3 = Diffractor(notebook)
        notebook.AddPage(tab3, "PATTERN")
        tab4 = Gofr(notebook)
        notebook.AddPage(tab4, "PDF")

        rightP = ButtonsPanel(splitterV)
        # split the window
        splitterV.SplitVertically(notebook, rightP)
        if gset.Platform.startswith('dar'): splitterV.SetMinimumPaneSize(150)
        else: splitterV.SetMinimumPaneSize(650)
        splitterV.SetSashGravity(1.0)
        
        ###___TEXT BUFFER BOX
        textbuffer = wx.TextCtrl(splitterH,-1,style=wx.TE_MULTILINE, name='ClaudeBuffer')

        splitterH.SplitHorizontally(splitterV, textbuffer)
        if gset.Platform.startswith('dar'): splitterH.SetMinimumPaneSize(480)
        else: splitterH.SetMinimumPaneSize(400)
        splitterV.SetSashGravity(1.0)
        
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(splitterH, 1, wx.ALL|wx.EXPAND, 5)

        self.SetSizer(sizer)
        self.Layout()
 
#         self.Show()
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - 
# if __name__ == "__main__":
#     app = wx.App(False)
#     frame = ClaudeGUI()
#     app.MainLoop()
