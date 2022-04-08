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
from debfuncx import get_sep
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
from readerC import reader
##########################################################################################
sep = get_sep(gset.Platform)
##----------------------------------------------------------------------------------------
if gset.PGM_Path[-1] != sep : gset.PGM_Path = gset.PGM_Path+sep
if gset.GUI_Path[-1] != sep : gset.GUI_Path = gset.GUI_Path+sep

class builder:
    """ A class that reads Claude and Debussy input files.
        R Frison - Institute of Crystallography
        ruggero.frison@ic.cnr.it
        v 0.1
        Usage : refer to attributes help.
    """
    
    def __init__(self, todo, infile, p1 = gset.PGM_Path):
        
        self.dbfile = infile
        
        if todo.lower()  ==  'cell':
            self.mkCELL(todo)
        elif todo.lower()  ==  'xyz':
            self.mkXYZ(todo, self.dbfile)
        elif (todo.lower()  ==  'largest' or todo.lower()  ==  'largest_only'):
            self.mkLARGEST(todo, self.dbfile)
        elif (todo.lower() == 'db' or todo.lower() == 'database'):
            self.mkDB(todo, self.dbfile)
        elif (todo.lower() == 'mol' or todo.lower() == 'molec'):
            self.mkMOLEC(todo)
    
    def mkCELL(self, mk, path = gset.PGM_Path):
        
        self.pgm = '%sMK_CELL_x1.0'%(path)

    def mkXYZ(self, mk, infile, path = gset.PGM_Path):
        
        ddbi = reader(infile)
        shpx = ddbi.shap[0].lower()
        if (shpx == 'sph'):
            inifn = 'sphmkQ.ini'
        elif (shpx == 'qbe'):
            inifn = 'qbemkQ.ini'
        elif (shpx != 'sph' and shpx != 'qbe'):
            inifn = 'clumk.ini'
        if (shpx == 'sph' or shpx == 'qbe'):
            try:
                inif = open(inifn, 'a')
            except IOError:
                print("Error: can\'t find or read %s file"%inifn)
            else:
                print('XYZ? y', file = inif)
                inif.close()  
                if (shpx == 'sph'):
                    self.pgm = '%sMK_SPHERE_x1.0'%(path)
#                         self.pgm = '%sMK_BALL_x1.0'%(path)
                elif (shpx == 'qbe'):
                    self.pgm = '%sMK_QBE_x1.0'%(path)
        elif (shpx != 'sph' and shpx != 'qbe'):
            pha = reader(ddbi.phas[0])
            cpar = pha.phaseinfo[1]
            #if (abs(float(cpar[3]) - 90.0) >= 1.0e-4 or abs(float(cpar[4]) - 90.0) >= 1.0e-4):
            #    self.pgm = '%sMK_XYZ_x1.0'%(path)
            #else:
            self.pgm = '%sMK_RODS_x1.0'%(path)
    
    def mkLARGEST(self, mk, infile, path = gset.PGM_Path):
        
        try:
            infof = open(self.dbfile, 'r')
        except IOError:
            print("Error: can\'t find or read %s file"%self.dbfile)
        else:
            lines = []
            for line in infof:
                if not line.strip():
                    continue
#                 elif line.strip('\n').split()[0][0] != '!':
                else:
                    lines += [line.strip('\n').lstrip()]
            #print(len(lines))
            infof.close()
            for l in range(len(lines)):
                nline = lines[l].rstrip()
                if ('Phase_Name'.lower() in nline.lower()):
                    fpha = nline.split()[-1]
                if (nline.split()[0] == 'Shape'):
                    shpl = l
                if (nline.split()[0] == 'TODO' or nline.split()[0] == 'todo'):
                    smpl = l
            lines[smpl] = 'TODO largest_only'
            infof = open(self.dbfile, 'w')
            for l in range(len(lines)):
                print(lines[l], file = infof)
            infof.close()  
            shpx = lines[shpl].split()[-1].lower()
            if (shpx == 'sph'):
                self.pgm = '%sMK_BALL_x1.0'%(path)  
            elif (shpx == 'qbe'):
                self.pgm = '%sMK_QBE_x1.0'%(path)
            else:
                pha = reader(fpha)
                cpar = pha.phaseinfo[1]
                if (abs(float(cpar[3]) - 90.0) >= 1.0e-4 or abs(float(cpar[4]) - 90.0) >= 1.0e-4):
                    self.pgm = '%sMK_LAYER_OBL_x1.0'%(path)
                else:
                    self.pgm = '%sMK_LAYER_GEN_x1.0'%(path)
    
    def mkDB(self, mk, infile, path = gset.PGM_Path):
        
        try:
            infof = open(self.dbfile, 'r')
        except IOError:
            print("Error: can\'t find or read %s file"%self.dbfile)
        else:
            lines = []
            for line in infof:
                if not line.strip():
                    continue
                elif line.strip('\n').split()[0][0] != '!':
                    lines += [line.strip('\n').lstrip()]
            #print(len(lines))
            infof.close()
            for l in range(len(lines)):
                nline = lines[l].rstrip()
                if ('Phase_Name'.lower() in nline.lower()):
                    fpha = nline.split()[-1]
                if (nline.split()[0] == 'Shape'):
                    shpl = l
                if (nline.split()[0] == 'TODO' or nline.split()[0] == 'todo'):
                    smpl = l
            lines[smpl] = 'TODO all_clusters'
            infof = open(self.dbfile, 'w')
            for l in range(len(lines)):
                print(lines[l], file =  infof)
            infof.close()
            shpx = lines[shpl].split()[-1].lower()
            if (shpx == 'sph'):
                self.pgm = '%sMK_BALL_x1.0'%(path)
            elif (shpx == 'qbe'):
                self.pgm = '%sMK_QBE_x1.0'%(path)
            else:
                    pha = reader(fpha)
                    cpar = pha.phaseinfo[1]
                    if (abs(float(cpar[3]) - 90.0) >= 1.0e-4 or abs(float(cpar[4]) - 90.0) >= 1.0e-4):
                        self.pgm = '%sMK_LAYER_OBL_x1.0'%(path)
                    else:
                        self.pgm = '%sMK_LAYER_GEN_x1.0'%(path)
    
    def mkMOLEC(self, mk, path = gset.PGM_Path):
        
        self.pgm = '%sMK_MOLEC_x1.0'%(path)

##__end of class

#exit()
