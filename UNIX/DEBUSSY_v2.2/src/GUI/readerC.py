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
import numpy as np
import os
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
##########################################################################################

##----------------------------------------------------------------------------------------

class reader:
    """ A class that reads Claude and Debussy input files.
        R Frison - Institute of Crystallography
        ruggero.frison@ic.cnr.it
        v 0.1
        Usage : refer to attributes help.
    """
    
    def __init__(self, infile, form  =  None, rang  =  [0., 0., 0.], verbose = False):
        
        
#         if type(infile)  !=  str:
#             print('STOP : file name expected (string), got ', type(infile), ' instead.')
#         elif type(infile) == str:
        fname  =  infile.rpartition(gv.SEP)[-1]
        if fname.rpartition('.')[-1].lower() == 'dwa':
            self.dwa(infile, verbose)
        if fname.rpartition('.')[-1].lower() == 'pha':
            self.pha(infile, verbose)
        #if infile.lower() == 'db_pha_clu_info.inp':
        if fname.rpartition('.')[-1].lower() == 'ddb':
            self.ddb(infile, verbose)
        if fname.lower() == 'mk_molec.ini':
            self.molini(infile, verbose)
        if fname.lower() == 'diffractor.inp':
            self.diffinp(infile, verbose)
        if fname.lower() == 'dopdf.inp':
            self.dopdfinp(infile)
        if fname.rpartition('.')[-1].lower() == 'cel':
            self.cell(infile, verbose)
        if fname.rpartition('.')[-1].lower() == 'dat':
            self.dat(infile, verbose)
        if fname.rpartition('.')[-1].lower() == 'par':
            self.par(infile, verbose)
        if fname.rpartition('.')[-1].lower() == 'ref':
            self.ref(infile, verbose)
    
    def __ckerr__(self, indata, posi):
        if len(indata) < 2:
            dlg = wx.MessageDialog(self, 'Line %i empty after keyword!'%posi, "ERROR", wx.OK)
            result = dlg.ShowModal()
            dlg.Destroy()
            if result == wx.ID_OK:
                dlg.Destroy()
    
    def dwa(self, infile, verbose = False):
        '''
        Reads *dwa file (according to Debussy_Manual_v4.4)
        Mandatory inputs: filename.
        Returns:
    
        0.    *dwa file FILENAME
    
        1.0.  TECHNIQUE
        1.1.  DATA_FILENAME
        1.2.  FORM
        1.3.  ANGRANGE
        1.4.  BLANK_FILENAME
        1.5.  NBLNK
        1.6.  CHEB_COEFF
        1.7.  YOUN_COEFF
        1.8.  WAVELENGTH
        1.9.  WAVELENGTH_ESD
        1.10. RADIATION
        1.11. MONOCHROMATOR
        1.11. POLARISATION
        1.12. GEOMETRY
        1.13. BEST_CAL_FILENAME
    
        2.0.  STRUCTURE_NAME
        2.1.  DATABASE INDEXES
        2.2.  DATABASES
        2.3.  PROTOCOL_STRUCTURE_NAME
        2.4.  N_CLUSTERS
        2.5.  N_RODS
        2.6.  SPHAPE
        2.7.  CHEMICAL_COMPOSITION
        2.8.  SPACE_GROUP_#
        2.9.  N_ATOMS_UNIT_CELL
        2.10. PARAMETERS_FILE
        2.11. MATRIX-REUSLTS_FILENAME
    
        3.0.  SIM_FLAG
        3.1.  OUTPUT_FLAG
        3.2.  REFINEMENT-FILE_NAME
        3.3.  ROOT_OUTPUT_FILE
        3.4.  OUTPUT_FILE
        '''
        path = ''
        if infile == 'debussy.inp':
            try:
                file1 = open(path+infile, 'r')
            except IOError:
                print("ERROR : can\'t find file or read data")
            else:
                self.dwafile = file1.readlines()[0][:-1]
                file1.close()
                if verbose : print(self.dwafile)
        else:
            self.dwafile = infile
        try:
            f = open(path+self.dwafile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            
            self.datasets, self.structures, self.refinement = [], [], []
            dflags = '# tech data form rang blnk blnc cheb youn wave esdw inst beam mono pola geom'.split()
            sflags = '% stru dbn db nclu nrod shap prot chem natc spgn cell micr parx'.split()
            rflags = 'simu calm rfil outs rpdf'.split()
            self.dwainfo = {}
            
            ndataset, nstructure, nrefinement = 0, 0, 1
            for l in range(len(lines)):
                if verbose: print('line|',lines[l][0], '|', lines[l], '|', lines[l][-1])
                rline = lines[l].split()
                self.__ckerr__(rline, l+1)
                rline0 = rline[0].lower()
                ##__datasets
                if rline0 == 'data':
                    ndataset += 1
                    self.dwainfo['#'+ '%i'%ndataset] = str(ndataset)
                    self.dwainfo['tech'+ '%i'%ndataset] = lines[l-1].split()[1]
                    self.dwainfo['data'+ '%i'%ndataset] = rline[1]
                    rootdat = rline[1].rpartition(gv.SEP)[-1].rpartition('.')[0]
                    self.dwainfo['bestcal'+ '%i'%ndataset] = rootdat+'_Best.cal'
                elif rline0 == 'beam':
                    self.dwainfo['beam'+ '%i'%ndataset] = rline[1]
                    if rline[1][0].lower() == 'x':
                        self.dwainfo['beamt'+ '%i'%ndataset] = 'XPD'
                    elif rline[1][0].lower() == 's':
                        self.dwainfo['beamt'+ '%i'%ndataset] = 'SPD'
                    elif beam0[0].lower() == 'n':
                        self.dwainfo['beamt'+ '%i'%ndataset] = 'NPD'
                    elif beam0[0].lower() == 'e':
                        self.dwainfo['beamt'+ '%i'%ndataset] = 'EPD'
                    else: self.dwainfo['beamt'+ '%i'%ndataset] = ''
                for flag in dflags[3:12] + dflags[13:16]:
                    if rline0 == flag:
                        self.dwainfo[flag+ '%i'%ndataset] = ' '.join(rline[1:])
                ##__structures
                if rline0[0] == '%':
                    nstructure += 1
                    self.dwainfo['%'+ '%i'%nstructure] = str(nstructure)
                    self.dwainfo['stru'+ '%i'%nstructure] = rline[1]
                    strun = rline[1]
                    if (int(lines[l + 1].split()[0][-1]) <= 3):
                        self.dwainfo['mtx'+ '%i'%nstructure] = strun + '0' + rline[0][-1] + '_plot1D.mtx'
                    elif (int(lines[l + 1].split()[0][-1]) >= 4):
                        self.dwainfo['mtx'+ '%i'%nstructure] = strun + '0' + rline[0][-1] + '_plot2D.mtx'
                elif rline0[:2] == 'db':
                    self.dwainfo['dbn'+ '%i'%nstructure] = rline0[2:]
                    self.dwainfo['db'+ '%i'%nstructure] = rline[1]
                for flag in sflags[4:]:
                    if rline0 == flag:
                        self.dwainfo[flag+ '%i'%nstructure] = ' '.join(rline[1:])
                ##__refinement
                for flag in rflags[:-1]:
                    if rline0 == flag:
                        self.dwainfo[flag] = ' '.join(rline[1:])
                if rline0 == 'outs':
                    self.dwainfo['outs'] = rline[1]
                    for i in range(ndataset):
                        self.dwainfo['outf'+ '%i'%ndataset] = rline[1] + '_' + \
                          self.dwainfo['beamt%i'%(ndataset)] + '#%02i.cal'%(ndataset)
                if rline0 == 'rpdf':
                    self.dwainfo['rpdf'] = rline[1]

            self.ndataset, self.nstructure, self.nrefinement = ndataset, nstructure, nrefinement
            return self.dwafile, self.ndataset, self.nstructure, self.dwainfo

    #-------------------------------------------------------------------

    def pha(self, infile, verbose = False):
        '''
        Read .pha Debussy file
        '''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find '%s' file or read data"%infile)
            return ()
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
#                     elif (line.strip('\n').split()[0][0] != '!' or line.strip('\n').split()[0][0] != '>'):
                elif not (line.strip('\n').startswith('!') or line.strip('\n').startswith('>')):
                    lines += [line.strip('\n').strip()]
            f.close()
            phana, phace, phasg, phaco = '', '', '', []
            for l in range(len(lines)):
                ll = lines[l].split()
                if (ll[0] == 'Title' or ll[0] == 'title'):
                    phana = ll[1]
                    if verbose : print(phana)
                elif (ll[0] == 'Cell' or ll[0] == 'cell'):
                    phace = ll[1:]
                    if verbose : print(phace)
                elif (ll[0] == 'Space' or ll[0] == 'space'):
                    phasg = ' '.join(ll[1:])
                    if verbose : print(phasg)
                elif (ll[0] == 'Coord' or ll[0] == 'coord'):
                    phaco += [ll[1:]]
                    if verbose : print(phaco)
            self.phaseinfo  =  phana, phace, phasg, phaco
            return self.phaseinfo
    
    #-------------------------------------------------------------------
    
    def ddb(self, infile, verbose = False):
        '''
        Read DB_PHA_CLU_info.inp Debussy file
        '''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find '%s' file or read data"%infile)
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            pha, sg, natsp, celor, pear, constr, shap, dsph, nsph, dpar, lpar, n1par, n2par, todo, occ1, xyz, \
                   para, samp, wave, ttm, rmd, rho = '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', \
                   '', '', '', '', '', ''
            for l in range(len(lines)):
                if ('Phase'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    pha = ll[-1].strip()
                    if verbose : print(pha)
                elif ('Spacegroupnumber_orig'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    sg = ll[-1].strip()
                    if verbose : print(sg)
                elif ('Atomic Species No'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    natsp = ll[-1].strip()
                    if verbose : print(natsp)
                elif ('Cell Origin'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    celor = ll[-1].strip()
                    if verbose : print(celor)
                elif ('Pearson'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    pear = ll[-1].strip()
                    if verbose : print(pear)
                elif ('Constr'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    constr = ll[-1].strip()
                    if verbose : print(pear)
                elif ('PARA'.lower() in lines[l].lower()):
                    ll = lines[l].partition(' ')
                    para = ll[-1].strip()
                    if verbose : print(para)
                elif ('Reduced'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(' ')
                    rmd = ll[-1].strip()
                    if verbose : print(rmd)
                elif ('Density'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(' ')
                    rho = ll[-1].strip()
                    if verbose : print(rho)
                elif ('Shape of Clusters'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    shap = ll[-1].strip()
                    if verbose : print(shap)
                elif ('Diam_max'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    dsph = ll[-1].strip()
                    if verbose : print(dsph)
                elif ('N_max'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    nsph = ll[-1].strip()
                    if verbose : print(nsph)
                elif ('D_max of PAR/CYL/HEX'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    dpar = ll[-1].strip()
                    if verbose : print(dpar)
                elif ('L_max of PAR/CYL/HEX'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    lpar = ll[-1].strip()
                    if verbose : print(lpar)
                elif ('N1_max of PAR/CYL/HEX'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    n1par = ll[-1].strip()
                    if verbose : print(n1par)
                elif ('N2_max of PAR/CYL/HEX'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    n2par = ll[-1].strip()
                    if verbose : print(n2par)
                elif ('TODO'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(' ')
                    todo = ll[-1].strip()
                    if verbose : print(todo)
                elif ('OCC1'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(' ')
                    occ1 = ll[-1].strip()
                    if verbose : print(occ1)
                elif ('XYZ?'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(' ')
                    xyz = ll[-1].strip()
                    if verbose : print(xyz)
                elif ('Sampling'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    samp = ll[-1].strip()
                    if verbose : print(samp)
                elif ('Wavelength'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    wave = ll[-1].strip()
                    if verbose : print(wave)
                elif ('2-Theta Max'.lower() in lines[l].lower()):
                    ll = lines[l].rpartition(':')
                    ttm = ll[-1].strip()
                    if verbose : print(ttm)
            self.phas  =  (pha, sg, natsp, constr, celor, pear, occ1, para, rmd, rho)
            self.shap  =  (shap, dsph, nsph, dpar, lpar, n1par, n2par)
            self.samp  =  (samp, wave, ttm)
            self.todo  =  (todo)
            return self.phas, self.shap, self.samp, self.todo
        
    #-------------------------------------------------------------------

    def molini(self, infile, verbose = False):
        '''
        Read molmkd.ini Debussy file
        '''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            if len(lines)<3:
                print('  SOMETHING MISSING IN molmkd.ini - STOP  -')
            elif len(lines) >= 3:
                xyz, sam, wlen, ttma, rmidi, rho = '', '', '', '', '', ''
                rline = lines[0].lstrip()
                xyz = rline.rstrip()
                rline = lines[1].split()
                if len(rline)>1:
                    sam = rline[0].lstrip()
                    wlen = rline[1].lstrip()
                    ttma = rline[2].lstrip()
                    if len(lines) >= 3:
                        rline = lines[2].lstrip()
                        rmidi = rline.rstrip()
                        if len(lines) >= 4:
                            rline = lines[3].split()
                            rho = rline[1].strip()
                elif len(rline) == 1:
                    sam = rline[0].strip()
                    rline = lines[2].split()
                    wlen = rline[0].lstrip()
                    ttma = rline[1].lstrip()
                    if len(lines) >= 4:
                        rline = lines[3].lstrip()
                        rmidi = rline.rstrip()
                        if len(lines) >= 5:
                            rline = lines[4].split()
                            rho = rline[1].strip()
            if verbose : print(xyz, sam, wlen, ttma, rmidi, rho)
            self.molecinfo  =  xyz, sam, wlen, ttma, rmidi, rho
            return self.molecinfo
    
    #-------------------------------------------------------------------
    
    def diffinp(self, infile, verbose = False):
        '''
        Read diffractor.inp Debussy file
        '''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            kwd = 'VARX WLEN RANG RAYS SOFQ DIVI IMAX HKLS CMPT PATH FILE NATO ZELE BATO ATOC\
             FPRI FDPR NSCL'.split()
            self.diffinfo = {}
            for l in range(len(lines)):
                if verbose : print(lines[l])
                rline = lines[l].partition(' ')
                rline0 = rline[0].lower()
                for k in kwd:
                    if rline0 ==  k.lower():
                        self.diffinfo[k] = rline[-1].lstrip()
            return self.diffinfo
    
    #-------------------------------------------------------------------
    
    def dopdfinp(self, infile, verbose = False):
        '''
        Read dopdf.inp Debussy file
        '''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            kwd = 'NFIL FILE NCOLS TTCOL I_COL E_COL RAYS WLEN ARANG RRANG VALEN DONOR SCALE\
             BROAD NQCUT QCUTV QMIN INCOH NATO ZELE CHEM'.split() ##_removed [BINW] 
            self.pdfinfo = {}
            for l in range(len(lines)):
                if verbose : print(lines[l])
                rline = lines[l].partition(' ')
                rline0 = rline[0].lower()
                for k in kwd:
                    if rline0 ==  k.lower():
                        self.pdfinfo[k] = rline[-1].lstrip()
            return self.pdfinfo
    
    #-------------------------------------------------------------------
    
    def cell(self, infile, verbose = False):
        '''
        Read Claude .cel file
        '''
        verbose  =  True
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            if verbose: print(lines)
            rline = lines[0].split()
            self.sg = rline[0]
            self.celpar = rline[1:7]
            self.nasp = rline [7]
            self.nat = lines[1].split()
            self.zat = lines[2].split()
            self.ato = np.loadtxt(infile, skiprows = 3, unpack = True, usecols = ([0]), dtype = 'str')
            self.xyzob = np.loadtxt(infile, skiprows = 3, usecols = (1,2,3,4,5), dtype = 'str')
    
    #-------------------------------------------------------------------
    
    def dat(self, infile, form  =  None, rang = [0., 0., 0.], verbose = False):
        '''
        dat(filename, form, rang):
        Returns angles, intensities and standard deviations (if present)
        lists from *.dat files of different formats (according to Debussy_Manual)
        Mandatory inputs: filename and form.
        'rang' is required if form = 1, 3.
        '''
        
        ndat, nskh, nskf = 1, 0, 0
        if len(fo) >= 3:
            nskh = fo[-2]
            nskf = fo[-1]
        if (len(fo) == 2 or len(fo) == 4):
            ndat = fo[1]
        
        f = open(infile, 'r')
        datfile = f.readlines()[nskh][:-1]
        ncol = len(datfile.split())
        
        col = [[], [], []]
        if fo[0] == '1':
            for c in range(ncol):
                col[0] += [range(ra[0], ra[1]+ra[2], ra[2])]
                col[1] += [np.loadtxt(infile, skiprows = nskh, usecols = range(ncol), unpack = True)[c]]
        if fo[0] == '2':
            ttc = range(0, ncol, 2)
            intc = range(1, ncol, 2)
            if (int(ndat)>1):            
                for c in range(ncol/2):
                    col[0] += [np.loadtxt(infile, skiprows = nskh, usecols = ttc, unpack = True)[ttc[c]]]
                    col[1] += [np.loadtxt(infile, skiprows = nskh, usecols = intc, unpack = True)[intc[c]]]
            else:
                col[0], col[1] = np.loadtxt(infile, skiprows = nskh, unpack = True)
        if fo[0] == '3':
            intc = range(0, ncol, 2)
            sdec = range(1, ncol, 2)
            if (int(ndat)>1):
                for c in range(ncol/2):
                    col[0] += [range(ra[0], ra[1]+ra[2], ra[2])]
                    col[1] += [np.loadtxt(infile, skiprows = nskh, usecols = intc, unpack = True)[intc[c]]]
                    col[2] += [np.loadtxt(infile, skiprows = nskh, usecols = sdec, unpack = True)[sdec[c]]]
            col[0] = range(ra[0], ra[1]+ra[2], ra[2])
            col[1], col[2] = np.loadtxt(infile, skiprows = nskh, unpack = True)
        if fo[0] == '4':
            ttc = range(0, ncol, 3)
            intc = range(1, ncol, 3)
            sdec = range(2, ncol, 3)
            if (int(ndat)>1):
                for c in range(ncol/3):
                    col[0] += [np.loadtxt(infile, skiprows = nskh, usecols = ttc, unpack = True)[ttc[c]]]
                    col[1] += [np.loadtxt(infile, skiprows = nskh, usecols = intc, unpack = True)[intc[c]]]
                    col[2] += [np.loadtxt(infile, skiprows = nskh, usecols = sdec, unpack = True)[sdec[c]]]
            else:
                col[0], col[1], col[2] = np.loadtxt(infile, skiprows = nskh, unpack = True)
        f.close()    
        return (col)
    
    #-------------------------------------------------------------------
    
    def par(self, infile, verbose = False):
        '''
        Reads *par file (according to Debussy_Manual)
        Mandatory inputs: filename.
        Returns:
        '''
        path = ''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            kwd = 'STcod VALn1 AV1LN SD1LN AV2LN SD2LN PHILN STR_i STR_1 STR_C STR_W\
             ATO01 OKK_I OKK_0 OKK_L BTH_I BTH_0 BTH_L'.split()
            stcod, valn1, av1ln, sd1ln, av2ln, sd2ln, philn, str_i, str_1, str_c, str_w, \
            ato_law, okk_i, okk_0, okk_l, bth_i, bth_0, bth_l = '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
            dis, str, atox = [], [], []
            for l in range(len(lines)):
                if verbose : print(lines[l])
                rline = lines[l].partition(' ')
                if rline[0].lower() == 'STcod'.lower():
                    stcod = rline[-1].lstrip()
                if rline[0].lower() == 'VALn1'.lower():
                    valn1 = rline[-1].lstrip()
                if rline[0].lower() == 'AV1LN'.lower():
                    av1ln = rline[-1].lstrip()
                if rline[0].lower() == 'SD1LN'.lower():
                    sd1ln = rline[-1].lstrip()
                if rline[0].lower() == 'AV2LN'.lower():
                    av2ln = rline[-1].lstrip()
                if rline[0].lower() == 'SD2LN'.lower():
                    sd2ln = rline[-1].lstrip()
                if rline[0].lower() == 'PHILN'.lower():
                    philn = rline[-1].lstrip()
                if rline[0].lower() == 'STR_i'.lower():
                    str_i = rline[-1].lstrip()
                if rline[0].lower() == 'STR_1'.lower():
                    str_1 = rline[-1].lstrip()
                if rline[0].lower() == 'STR_C'.lower():
                    str_c = rline[-1].lstrip()
                if rline[0].lower() == 'STR_W'.lower():
                    str_w = rline[-1].lstrip()
                if rline[0][:3].lower() == 'ATO'.lower():
                    ato_law = rline[-1].lstrip()
                    for i in range(1, 7, 1):
                        aline = lines[l+i].partition(' ')
                        if aline[0].lower() == 'OKK_I'.lower():
                            okk_i = aline[-1].lstrip()
                        if aline[0].lower() == 'OKK_0'.lower():
                            okk_0 = aline[-1].lstrip()
                        if aline[0].lower() == 'OKK_L'.lower():
                            okk_l = aline[-1].lstrip()
                        if aline[0].lower() == 'BTH_I'.lower():
                            bth_i = aline[-1].lstrip()
                        if aline[0].lower() == 'BTH_0'.lower():
                            bth_0 = aline[-1].lstrip()
                        if aline[0].lower() == 'BTH_L'.lower():
                            bth_l = aline[-1].lstrip()
                    atox += [[ato_law, okk_i, okk_0, okk_l, bth_i, bth_0, bth_l]]
            dis += [av1ln, sd1ln, av2ln, sd2ln, philn]
            str += [stcod, valn1, str_i, str_1, str_c, str_w]
            self.dis, self.str, self.atox  =  dis, str, atox
            return self.dis, self.str, self.atox
    
    #-------------------------------------------------------------------
    
    def ref(self, infile, verbose = False):
        '''
        Reads *ref file (according to Debussy_Manual)
        Mandatory inputs: filename.
        Returns:
        '''
        path = ''
        try:
            f = open(infile, 'r')
        except IOError:
            print("ERROR : can\'t find file or read data")
        else:
            lines = []
            for line in f:
                if not line.strip():
                    continue
                elif not line.strip('\n').startswith('!'):
                    lines += [line.strip('\n').strip()]
            f.close()
            nstage = 0
            self.ref, stage, struc, data = [], [], [], ''
            tol, alg = '', ''
            for l in range(len(lines)):
                rline = lines[l].partition(' ')
                if rline[0].lower() == 'stage':
                    nstage += 1
                    lx = lines[l].split()
                    tol = lx[3]
                    stage += [tol]
                    alg = lx[4]
                    stage += [alg]
                if rline[0][0] == '%':
                    struc += [lines[l+1]]
                if rline[0][0] == '#':
                    stage += [struc]
                    data = lines[l+1]
                    stage += [data]
                    self.ref += [stage]
                    stage, struc, data = [], [], ''
            return self.ref

if __name__ == '__main__':
    print('    running reader ')

