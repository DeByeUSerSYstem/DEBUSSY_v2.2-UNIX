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
import os
import glob
import subprocess
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
from readerC import reader
############################
if gset.PGM_Path[-1] != gv.SEP : gset.PGM_Path = gset.PGM_Path + gv.SEP

def debyer(simu, inputfile = 'debussy.inp', p1 = gset.PGM_Path):

    dwa = reader(inputfile)

    dwal = []
    dwabn = dwa.dwafile.rpartition('/')[-1]
    dwaf = open(dwa.dwafile, 'r')
    for line in dwaf:
        if not line.strip():
            continue
        else:
            dwal+= [line.strip('\n')]
    dwaf.close()

    if (simu == 'sim' or simu == 'SIM'):
        if dwa.dwainfo['simu'] == '1':
            pgm = p1+'Debussy %s'%dwabn
        else:
            dwaf = open(dwa.dwafile, 'w')
            for l in range(len(dwal)):
                nline = dwal[l].rstrip()
                if nline.split()[0] == 'simu':
                    nline = '  simu    1'
                print(nline, file = dwaf)
            dwaf.close()
            pgm = p1+'Debussy %s'%dwabn
    elif (simu == 'ref' or simu == 'REF'):
        if dwa.dwainfo['simu'] == '0':
            pgm = p1+'Debussy %s'%dwabn
        else:
            dwaf = open(dwa.dwafile, 'w')
            for l in range(len(dwal)):
                nline = dwal[l].rstrip()
                #print(nline)
                if nline.split()[0] == 'simu':
                    nline = '  simu    0'
                print(nline, file = dwaf)
            dwaf.close()
            pgm = p1+'Debussy %s'%dwabn
    elif (simu == 'std' or simu == 'std'):
        if dwa.dwainfo['simu'] == '-1':
            pgm = p1+'Debussy %s'%dwabn
        else:
            dwaf = open(dwa.dwafile, 'w')
            for l in range(len(dwal)):
                nline = dwal[l].rstrip()
                #print(nline)
                if nline.split()[0] == 'simu':
                    nline = '  simu    -1'
                print(nline, file = dwaf)
            dwaf.close()
            pgm = p1+'Debussy %s'%dwabn
    return pgm
