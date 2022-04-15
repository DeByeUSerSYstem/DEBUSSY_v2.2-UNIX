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
from shutil import copy2 as shutil_copy2
import numpy as np
import matplotlib
matplotlib.use('WXAgg')
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from readerC import reader
import gui_settings as gset #[Platform,DEB_Path,PGM_Path,GUI_Path,User_Path,Editor,AtomViewer]
import gui_variables as gv
from debfuncx import phase_from_tqi
import matplotlib.scale as mscale
import matplotlib.transforms as mtransforms
import matplotlib.ticker as ticker
##########################################################################################

##----------------------------------------------------------------------------------------
##PLOT SETTINGS
#plt.rc('text', usetex=True)#sets all text to Latex
plt.rc('font', family='serif')
plt.rc('font',size=11) #font of plt.title,plt.xlabel, etc.
#plt.rc('lines',lw=2.0)#line width, is overwritten by setting coulours/linestylies
plt.rc(('xtick','ytick'),labelsize=10) #sets fontsize of labels on xticks, yticks.
plt.rc("axes", linewidth=0.5)
plt.rc('legend',numpoints=1)
#plt.rc('plt.legend',frameon=False)
plt.rc('legend',fontsize=11)
##parameters controlling plt.axes thickness
# ax.tick_params(axis='both',which='major',length=6 ,width=2)
# ax.tick_params(axis='both',which='minor',length=4 ,width=2)
##
#colorlist=["k", "r", "g", "m", "b", "c", "y", "k"]
#markerlist=["o", "v", "^", "*", "+", "x", "D", "1"]
#linelist=['-',':']
dcolor=["r", "g", "m", "b", "c", "y", "k"]
bcolor=['b','y']
lcolor=['r','g','b','c','m','k','y']
##########################################################################################



class SquareRootScale(mscale.ScaleBase):
    """
    ScaleBase class for generating square root scale.
    """

    name = 'squareroot'

    def __init__(self, axis, **kwargs):
        mscale.ScaleBase.__init__(self)

    def set_default_locators_and_formatters(self, axis):
        axis.set_major_locator(ticker.AutoLocator())
        axis.set_major_formatter(ticker.ScalarFormatter())
        axis.set_minor_locator(ticker.NullLocator())
        axis.set_minor_formatter(ticker.NullFormatter())

    def limit_range_for_scale(self, vmin, vmax, minpos):
        return  max(0., vmin), vmax

    class SquareRootTransform(mtransforms.Transform):
        input_dims = 1
        output_dims = 1
        is_separable = True

        def transform_non_affine(self, a): 
            return np.array(a)**0.5

        def inverted(self):
            return SquareRootScale.InvertedSquareRootTransform()

    class InvertedSquareRootTransform(mtransforms.Transform):
        input_dims = 1
        output_dims = 1
        is_separable = True

        def transform(self, a):
            return np.array(a)**2

        def inverted(self):
            return SquareRootScale.SquareRootTransform()

    def get_transform(self):
        return self.SquareRootTransform()

mscale.register_scale(SquareRootScale)
##########################################################################################


class plotter:
    """ A class for plotting Claude and Debussy output files.
        R Frison - Institute of Crystallography
        ruggero.frison@ic.cnr.it
        v 0.1
        Usage : refer to attributes help.
    """
    
    def __init__(self, filein, plot_type):
        
        if filein.endswith('.dwa'):
            inobj = reader(filein)
        else: 
            inobj = filein
        if plot_type.startswith('tqi'):
            self.plot_i(inobj, plot_type)
        elif plot_type.startswith('rpdfn'):
            self.plot_rpdf(inobj)
        elif plot_type.startswith('data'):
            self.plot_data(inobj)
        elif plot_type.startswith('sim'):
            self.plot_cal(inobj, plot_type)
        elif plot_type.startswith('ref'):
            self.plot_cal(inobj, plot_type)
        elif plot_type.startswith('liveref'):
            self.plot_liveref(inobj, plot_type)
        elif plot_type.startswith('siz'):
            self.plot_size(inobj)
        elif plot_type.startswith('cel'):
            self.plot_cel(inobj)
        elif plot_type.startswith('sof'):
            self.plot_sof(inobj)
        elif plot_type.startswith('bth'):
            self.plot_bth(inobj)
        
        self.verbose=False
    ###***********************************************
    def get_wave(self, dwa_object):
        ndat = dwa_object.ndataset
        for i in range(ndat):
            wlx = np.asarray(dwa_object.dwainfo['wave%i'%(i+1)].strip().split(), dtype=float)
        if len(wlx) == 1: wave = wlx[0]
        else:
            wave = np.average(wlx[:2], weights=[1.0,wlx[2]])
        return wave

    ###***********************************************
    def get_hkl(self, dwa_object, ftype='.hkl', xtype = 'tt'):
        hkl_all = []
        for ii in range(dwa_object.ndataset):
            hklphase = []
            for ik in range(dwa_object.nstructure):
                phase = dwa_object.dwainfo['stru%i'%(ik+1)]
                if os.path.isfile(phase+ftype):
                    hkl = np.loadtxt(phase+ftype, skiprows=1, usecols=(0,1,2), dtype=int)
                    dspac = np.loadtxt(phase+ftype, skiprows=1, unpack=True, usecols=([3]))
                    if xtype == 'q':
                        xhkl = 1 / dspac
                    if xtype == 'Q':
                        xhkl = 2 * np.pi / dspac
                    elif xtype == 'd':
                        xhkl = dspac
                    elif xtype == 'tt':
                        wlen = self.get_wave(dwa_object)
                        xhkl = 2*np.degrees(np.arcsin(wlen/(2*dspac)))
                    hklphase += [(phase,hkl,xhkl)]
                else: hklphase += [(None,None,None)]
            hkl_all += [(hklphase)]
        return hkl_all

    ###***********************************************
    def plot_pattern(self, fins):
        '''
        Returns a Intensity vs. 2theta plot of calculated diffraction patterns of atomic clusters.
        '''    
        plt.figure()
        ax=plt.subplot(111)
        fl = fins.split()
        for c in range(len(fl)):
            try:
                tt,q,intsy = np.loadtxt(fl[c],skiprows=1,usecols=(0,1,2),unpack=True)
            except IOError:
                print(">>>>>   Error: can\'t find file or read data    <<<<<")
            else:
                cn=fl[c].rpartition(gv.SEP)[-1]
                clu=cn.rpartition('.tqi')[0]
                ax.plot(tt,intsy,'%s-'%dcolor[c],label=r'%s'%(clu))
        plt.xlabel(r'2$\theta$ (deg)', fontsize=13)
        plt.ylabel(r"Intensity (arb. units)", fontsize=13)
        plt.legend(loc=0)
        ax.ticklabel_format(axis='y',style='sci',scilimits=(-1,4))
        #ax.tick_params(axis='both',which='major',length=6 ,width=2)
        #ax.tick_params(axis='both',which='minor',length=4 ,width=2)
        plt.show()
        return

    ###***********************************************
    def plot_i(self, fins, varx):
        '''
        Returns a Intensity vs. 2theta/q/d plot of calculated diffraction intensity of atomic clusters.
        '''    
        toplot = False
        plt.figure()
        ax=plt.subplot(111)
#        fig = plt.figure()
        fl = fins.split()
        points_with_annotation = []
#        axes = plt.axes()
        nfl = len(fl)
        xvall, yvall, wl_all, color1 = [], [], [], []
        for c in range(nfl):
            try:
                tt, q, intsy = np.loadtxt(fl[c], skiprows=1, usecols=(0,1,2), unpack=True)
            except IOError:
                print(">>>>>   Error: can\'t find file or read data    <<<<<")
            else:
                toplot = True
                cn = fl[c].rpartition(gv.SEP)[-1]
                clu = cn.rpartition('.tqi')[0]
                plot_hkl = False
                im = int(len(tt) / 2)
                wl_all += [2*np.sin(np.radians(tt[im]/2))/q[im]]
                if c > len(dcolor): color1 += [np.random.rand(3,)]
                else: color1 += [dcolor[c]]
                sx = len(tt)
                ix0 = range(sx)
                if '_tt' in varx:
                    xv = tt
                    xla = r'2$\theta$ (deg)'
                elif '_q' in varx: 
                    xv = q
#                     ax.plot(xv,intsy,'%s-'%color1[c],label=r'%s'%(clu))
                    xla = r'$q$ (1/$\AA$)'
                elif '_Q' in varx: 
                    ix0 = range(sx)
                    xv = 2 * np.pi * q
#                     ax.plot(xv,intsy,'%s-'%color1[c],label=r'%s'%(clu))
                    xla = r'$Q$ (1/$\AA$)'
                elif '_d' in varx:
                    ix0 = np.nonzero(q) 
                    xv = 1 / q[ix0]
#                     ax.plot(xv,intsy[ix0],'%s-'%color1[c],label=r'%s'%(clu))
                    xla = r'$d$ ($\AA$)'
                elif '_logq' in varx:
                    ix0 = range(sx)
                    xv = q
                    plt.xscale('log')
                    xla = r'$q$ (1/$\AA$)'
                elif '_logQ' in varx:
                    ix0 = range(sx)
                    xv = 2 * np.pi * q
                    plt.xscale('log')
                    xla = r'$Q$ (1/$\AA$)'
                yv = intsy[ix0]
                yla = r"Intensity (arb. units)"
                if '_logI' in varx:
                    plt.yscale('log')
                    yla = r"Intensity (arb. units)"
                elif '_sqrtI' in varx:
                    yv = np.sqrt(intsy[ix0])
                    yla = r"sqrt(I) (arb. units)"
                xvall += [xv] 
                yvall += [yv]
                ax.plot(xv, yv, '%s-'%color1[c], label = r'%s'%(clu))
        if ('_hkl' in varx and toplot == True):
            yplot = ax.get_ylim()
            ymins = []
            for c in range(nfl): ymins += [min(yvall[c])]
            imin = min(ymins)
            yps = (imin-yplot[0])/(nfl*2)
            ypv = np.arange(yplot[0],imin+yps,yps)
            phase_c = 'grey'
            for c in range(nfl):
                phase = phase_from_tqi(self,fl[c])[0]
                if not os.path.isfile(phase+'.hkl'): continue
                if c > 0: 
                    if phase == phase_from_tqi(self,fl[c-1])[0]: continue
                phase_hkl = phase.rpartition(gv.SEP)[-1]+' hkl'
                hkl_i = np.loadtxt(phase+'.hkl', skiprows=1, usecols=(0,1,2), dtype=int)
                hkl_d = np.loadtxt(phase+'.hkl', skiprows=1, unpack=True, usecols=([3]))
                if nfl > 1: phase_c = color1[c]
                if '_tt' in varx: xhkl = 2*np.degrees(np.arcsin(wl_all[c]/(2*hkl_d)))
                elif ('_q' in varx or '_logq' in varx): xhkl = 1/hkl_d
                elif ('_Q' in varx or '_logQ' in varx): xhkl = 2 * np.pi / hkl_d
                elif ('_d' in varx): xhkl = hkl_d
                iix = np.argwhere(np.logical_and(xhkl>np.amin(xvall[c]),xhkl<np.amax(xvall[c])))
                xxhkl = xhkl[iix]
                labhkl = hkl_i[iix]
                ky = 0
                yp = ypv[-2::-2][c]
                for kk in range(len(xxhkl)):
                    xp = xxhkl[kk]
                    if kk == 0: point, = plt.plot(xp,yp,c=phase_c,marker='|',ls='',label=r'%s'%(phase_hkl)) ##change colour with phase
                    else: point, = plt.plot(xp,yp,c=phase_c,marker='|',ls='')
                    shkl = ''
                    for kl in range(3): 
                        shkl += ' '+str(labhkl[kk][0][kl])
                    shkl = shkl.strip(' ')
                    if np.all(xxhkl[:kk]-xp): 
                        annotation = axes.annotate("%s" %shkl, xy=(xp,yp), xycoords='data',
                            xytext=(xp,yp), textcoords='data', horizontalalignment="left")
                    if not np.all(xxhkl[:kk]-xp): 
                        zzy = xxhkl[:kk]-xp==0.0
                        i00 = np.nonzero(zzy)
                        ky = len(i00[0])
                        annotation = axes.annotate("%s" %shkl, xy=(xp,yp), xycoords='data',
                            xytext=(xp,yp+ky*yps), textcoords='data', horizontalalignment="left")
                    annotation.set_visible(False)
                    points_with_annotation.append([point, annotation])
                def on_move(event):
                    visibility_changed = False
                    for point, annotation in points_with_annotation:
                        should_be_visible = (point.contains(event)[0] == True)
                        if should_be_visible != annotation.get_visible():
                            visibility_changed = True
                            annotation.set_visible(should_be_visible)
                    if visibility_changed:        
                        plt.draw()
                on_move_id = fig.canvas.mpl_connect('motion_notify_event', on_move)
        del xvall, yvall, wl_all
        plt.xlabel(xla, fontsize=13)
        plt.ylabel(yla, fontsize=13)
        plt.legend(loc=0)
        if not '_logI' in varx: ax.ticklabel_format(axis='y',style='sci',scilimits=(-1,4))
        #ax.tick_params(axis='both',which='major',length=6 ,width=2)
        #ax.tick_params(axis='both',which='minor',length=4 ,width=2)
        plt.show()
        return

    ###***********************************************
    def plot_rpdf(self, fins):
        '''
        Returns a RPDF plot of diffraction patterns.
        '''    
        plt.figure()
        ax=plt.subplot(111)
        fl = fins.split()
        for c in range(len(fl)):
            try:
                r,gr=np.loadtxt(fl[c],skiprows=1,usecols=(0,1),unpack=True)
            except IOError:
                print(">>>>>   Error: can\'t find file or read data    <<<<<")
            else:
                cn=fl[c].rpartition(gv.SEP)[-1]
                clu=cn.rpartition('.rpdfn')[0]
                ax.plot(r,gr,'%s-'%dcolor[c],label=r'%s'%(clu))
        plt.xlabel(r'r ($\AA$)', fontsize=13)
        plt.ylabel(r"G(r)", fontsize=13)
        plt.legend(loc=0)
        ax.ticklabel_format(axis='y',style='sci',scilimits=(-1,4))
        #ax.tick_params(axis='both',which='major',length=6 ,width=2)
        #ax.tick_params(axis='both',which='minor',length=4 ,width=2)
        plt.show()
        return

    ###***********************************************
    def plot_data(self, dwaobj):
        '''
        Returns a Intensity vs. 2Theta plot for each dataset.
        '''

        ndat = dwaobj.ndataset

        for d in range(ndat):
            if (int(dwaobj.dwainfo['form%i'%(d+1)][0])%2) > 0:
                col = dat_reader(dwaobj.dwainfo['data%i'%(d+1)], dwaobj.dwainfo['form%i'%(d+1)], dwaobj.dwainfo['rang%i'%(d+1)])
            else:
                #print(dats[d],form[d])
                col = dat_reader(dwaobj.dwainfo['data%i'%(d+1)], dwaobj.dwainfo['form%i'%(d+1)])

            plt.figure()
            ax=plt.subplot(111)
            for k in range(len(col[1])):
                ax.plot(col[0][k],col[1][k],'%s.'%dcolor[d],label=r'%s'%(dwaobj.dwainfo['data%i'%(d+1)].rpartition(gv.SEP)[-1]))
                ###continues..
            plt.xlabel(r'2$\theta$ (deg)', fontsize=13)
            plt.ylabel(r"Intensity [counts]", fontsize=13)
            ax.ticklabel_format(axis='y',style='sci',scilimits=(-1,4))
            #ax.tick_params(axis='both',which='major',length=6 ,width=2)
            #ax.tick_params(axis='both',which='minor',length=4 ,width=2)
        plt.show()
        return


    ###***********************************************
    def plot_cal(self, dwaobj, varx):
        '''
        Returns a Intensity vs. 2theta/q/Q/d of Debye Function refinements for each dataset.
        '''
        
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
    
        for i in range(ndat):
            dat = dwaobj.dwainfo['data%i'%(i+1)].rpartition(gv.SEP)[-1]
            #print(dat)
            if 'blnc%i'%(i+1) in dwaobj.dwainfo:
                nblnk = int(dwaobj.dwainfo['blnc%i'%(i+1)][0])
            else:
                nblnk = 1
            ncol = 3 + nstr + nblnk
            wave = self.get_wave(dwaobj)
            try:
                if 'sim' in varx:
                    col = np.loadtxt(dwaobj.dwainfo['outf%i'%(i+1)],skiprows = 0,unpack = True)
                elif 'ref' in varx:
                    col = np.loadtxt(dwaobj.dwainfo['bestcal%i'%(i+1)], skiprows=2, unpack=True)
            except IOError:
                if 'sim' in varx:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(dwaobj.dwainfo['outf%i'%(i+1)]))
                elif 'ref' in varx:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(dwaobj.dwainfo['bestcal%i'%(i+1)]))
            else:
                fig = plt.figure()
                ax = plt.subplot(111)
                liny = True
                imin = []
                if '_q' in varx:
                    xx = 2 * np.sin(np.radians(col[0])/2) / wave
                    xla = r'$q$ (1/$\AA$)'
                    svarx = 'q'
                elif '_logq' in varx: 
                    xx = 2 * np.sin(np.radians(col[0])/2) / wave
                    xla = r'$q$ (1/$\AA$)'
                    svarx = 'q'
                    plt.xscale('log')
                elif '_Q' in varx: 
                    xx = 4 * np.pi * np.sin(np.radians(col[0])/2) / wave
                    xla = r'$Q$ (1/$\AA$)'
                    svarx = 'Q'
                elif '_logQ' in varx: 
                    xx = 4 * np.pi * np.sin(np.radians(col[0])/2) / wave
                    xla = r'$Q$ (1/$\AA$)'
                    svarx = 'Q'
                    plt.xscale('log')
                elif '_d' in varx:
                    xx = wave / (2 * np.sin(np.radians(col[0])/2))
                    xla = r'$d$ ($\AA$)'
                    svarx = 'd'
                else: 
                    xx = col[0]
                    xla = r'$2\theta$ (deg)'
                    svarx = 'tt'
                yla = r'Intensity (arb. units)'
                if '_logI' in varx:
                    liny = False 
                    plt.yscale('log')
                    yla = r'Intensity (arb. units)'
                elif '_sqrtI' in varx:
                    liny = False  
                    # plt.yscale('squareroot')
                    yla = r"$sqrt(I)$ (arb. units)"
                    if dwaobj.dwainfo['data%i'%(i+1)].lower() == 'none':
                        ax.plot(xx,np.sqrt(col[2]), 'g-', label=r'Calc.')
                        imin += [min(np.sqrt(col[2]))]
                    else:
                        ax.plot(xx, np.sqrt(col[1]), '.', ms=4, mfc='w', mec='k', label=r'Obs.')
                        imin += [min(np.sqrt(col[1]))]
                        ax.plot(xx, np.sqrt(col[2]), 'g-', label=r'Calc.')
                        imin += [min(np.sqrt(col[2]))]
                        ax.plot(xx, np.sqrt(col[1])-np.sqrt(col[2]), 'r-', label=r'Diff.')
                        imin += [min(np.sqrt(col[1])-np.sqrt(col[2]))]
                        ax.plot(xx, col[1]*0.0, 'k-')
                        imin += [0.0]
                    for b in range(nblnk):
                        if nblnk == 1 : blabel = 'Backgr.'
                        else : blabel = 'Backgr. %i'%(b+1)
                        ax.plot(xx, np.sqrt(col[3+nstr+b]),'%s-'%bcolor[b], label=r'%s'%(blabel))
                        imin += [min(np.sqrt(col[3+nstr+b]))]
                if not '_sqrtI' in varx:
                    if dwaobj.dwainfo['data%i'%(i+1)].lower() == 'none':
                        ax.plot(xx,col[2], 'g-', label=r'Calc.')
                        imin += [min(col[2])]
                    else:
                        ax.plot(xx, col[1], '.', ms=4, mfc='w', mec='k', label=r'Obs.')
                        imin += [min(col[1])]
                        ax.plot(xx, col[2], 'g-', label=r'Calc.')
                        imin += [min(col[2])]
                        if not '_logI' in varx: 
                            ax.plot(xx, col[1]-col[2], 'r-', label=r'Diff.')
                            imin += [min(col[1]-col[2])]
                            ax.plot(xx, col[1]*0.0, 'k-')
                            imin += [0.0]
                            for b in range(nblnk):
                                if nblnk == 1 : blabel = 'Backgr.'
                                else : blabel = 'Backgr. %i'%(b+1)
                                ax.plot(xx, col[3+nstr+b],'%s-'%bcolor[b], label=r'%s'%(blabel))
                                imin += [min(col[3+nstr+b])]

                if '_hkl' in varx:
                    hklxall = self.get_hkl(dwaobj,ftype='.hkl',xtype=svarx)
                    hklx = hklxall[i]
                    axes = plt.axes()
                    points_with_annotation = []
                    ymin = min(imin)
                    yplot = ax.get_ylim()
                    yps = (ymin-yplot[0])/(nstr*2)
                    ypv = np.arange(yplot[0],ymin+yps,yps)
                    for ik in range(nstr):
                        #if len(hklx[ik]) == 0: continue
                        if hklx[ik][0] == None: continue
                        if nstr > 1: phase_c = np.random.rand(3,)
                        else: phase_c = 'grey'
                        phase_hkl = hklx[ik][0].rpartition(gv.SEP)[-1]+' hkl'
                        if ik > 0:
                            if hklx[ik][0] == hklx[ik-1][0]: continue
                        iix = np.argwhere(np.logical_and(hklx[ik][2]>np.amin(xx),hklx[ik][2]<np.amax(xx)))
                        xxhkl = hklx[ik][2][iix]
                        labhkl = hklx[ik][1][iix]
                        ky = 0
                        yp = ypv[-2::-2][ik]
                        for kk in range(len(xxhkl)):
                            xp = xxhkl[kk]
                            if kk == 0: point, = plt.plot(xp,yp,c=phase_c,marker='|',ls='',label=r'%s'%(phase_hkl)) ##change colour with phase
                            else: point, = plt.plot(xp,yp,c=phase_c,marker='|',ls='')
                            shkl = ''
                            for kl in range(3): 
                                shkl += ' '+str(labhkl[kk][0][kl])
                            shkl = shkl.strip(' ')
                            if np.all(xxhkl[:kk]-xp): 
                                annotation = axes.annotate("%s" %shkl, xy=(xp,yp), xycoords='data',
                                    xytext=(xp,yp), textcoords='data', horizontalalignment="left")
                            if not np.all(xxhkl[:kk]-xp): 
                                zzy = xxhkl[:kk]-xp==0
                                i00 = np.nonzero(zzy)
                                ky = len(i00[0])
                                annotation = axes.annotate("%s" %shkl, xy=(xp,yp), xycoords='data',
                                    xytext=(xp,yp+ky*yps), textcoords='data', horizontalalignment="left")
                            annotation.set_visible(False)
                            points_with_annotation.append([point, annotation])
                    def on_move(event):
                        visibility_changed = False
                        for point, annotation in points_with_annotation:
                            should_be_visible = (point.contains(event)[0] == True)
                            if should_be_visible != annotation.get_visible():
                                visibility_changed = True
                                annotation.set_visible(should_be_visible)
                        if visibility_changed:        
                            plt.draw()
                    on_move_id = fig.canvas.mpl_connect('motion_notify_event', on_move)
                del imin
                if not '_logI' in varx: ax.ticklabel_format(axis = 'y', style='sci', scilimits=(-1,6))
                #ax.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                #ax.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                plt.xlabel(xla, fontsize=13) ## LaTex symbols!!!!!!!!!?????
                plt.ylabel(yla, fontsize=13)
                if 'ref' in varx: plt.title('%s Best fit'%dat)
                plt.legend()
        plt.show()
        return

    ###***********************************************
    def plot_liveref(self, dwaobj, varx):
        '''
        Returns a Intensity vs. 2theta/q/d of Debye Function refinements for each dataset.
        '''
        
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
        
        time0 = time.time() 
        for i in range(ndat):
            ifig = plt.figure()
            ax1 = plt.subplot(211)
            ax2 = plt.subplot(212)
            
            gof, cycs = [], []
            def fillaxes(r):
                dat = dwaobj.dwainfo['data%i'%(i+1)].rpartition(gv.SEP)[-1]
                if 'blnc%i'%(i+1) in dwaobj.dwainfo:
                    nblnk = int(dwaobj.dwainfo['blnc%i'%(i+1)][0])
                else:
                    nblnk = 1
                ncol = 3 + nstr + nblnk
                wlx = np.asarray(dwaobj.dwainfo['wave%i'%(i+1)].strip().split(), dtype=float)
                if len(wlx) == 1: wave = wlx[0]
                else:
                    wave = np.average(wlx[:2], weights=[1.0,wlx[2]])
                kk = 0
                bcal = dwaobj.dwainfo['bestcal%i'%(i+1)]
                bcal2 = dwaobj.dwainfo['bestcal%i'%(i+1)]+'2'
                #refout = dwaobj.dwafile.rpartition('.')[0]+'_ref.out'
                if os.path.isfile(bcal):
                    if os.path.getmtime(bcal) > time0:
                        if kk == 0:
                            shutil_copy2(bcal,bcal2)
                        else:
                            s0 = os.stat(bcal).st_size
                            s1 = os.stat(bcal2).st_size
                            if s0 >= s1: shutil_copy2(bcal,bcal2)
    #                     if os.path.isfile(refout):
    #                         f = open(refout, 'r')
    #                         lines = f.readlines()
    #                         f.close()
    #                         for ll in range(len(lines)):
    #                             if 'GoF =' in lines[ll]:
    #                                 gof.append(float(lines[ll].partition('GoF =')[-1].split()[0].strip()))
    #                         cycs = range(1,len(gof)+1)
                        with open(bcal2, 'r') as f:
                            rl = f.readline()
                            f.close()
                        rll = rl.split()
                        chi2 = float(rll[-1])
                        gof.append(np.sqrt(chi2))
                        cycs.append(float(rll[-2]))
                        col = np.loadtxt(bcal2, skiprows=2, unpack=True)
    #                 except IOError:
    #                     print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(dwaobj.dwainfo['bestcal%i'%(i+1)]))
    #                 else:
    #                     rbvtt =  wx.FindWindowByName('plot_itt')
    #                     rbvq =  wx.FindWindowByName('plot_iq')
    #                     rbvd =  wx.FindWindowByName('plot_id')
    #                     if rbvq.GetValue(): 
    #                     #if varx.endswith('q'): 
    #                         xx = 2 * np.sin(np.radians(col[0])/2) / wave
    #                         xla = r'$q$ (1/$\AA$)'
    #                     elif rbvd.GetValue(): 
    #                     #elif varx.endswith('d'): 
    #                         xx = wave / (2 * np.sin(np.radians(col[0])/2))
    #                         xla = r'$d$ ($\AA$)'
    #                     else: 
    #                         xx = col[0]
    #                         xla = r'$2\theta$ (deg)'
#                         if varx.endswith('q'): 
#                             xx = 2 * np.sin(np.radians(col[0])/2) / wave
#                             xla = r'$q$ (1/$\AA$)'
#                         elif varx.endswith('d'): 
#                             xx = wave / (2 * np.sin(np.radians(col[0])/2))
#                             xla = r'$d$ ($\AA$)'
#                         else: 
#                             xx = col[0]
#                             xla = r'$2\theta$ (deg)'
                        liny = True
                        if '_q' in varx:
                            xx = 2 * np.sin(np.radians(col[0])/2) / wave
                            xla = r'$q$ (1/$\AA$)'
                            svarx = 'q'
                        elif '_logq' in varx: 
                            xx = 2 * np.sin(np.radians(col[0])/2) / wave
                            xla = r"$log(q)$"
                            svarx = 'q'
                            plt.xscale('log')
                        elif '_Q' in varx: 
                            xx = 4 * np.pi * np.sin(np.radians(col[0])/2) / wave
                            xla = r'$Q$ (1/$\AA$)'
                            svarx = 'Q'
                        elif '_logQ' in varx: 
                            xx = 4 * np.pi * np.sin(np.radians(col[0])/2) / wave
                            xla = r"$log(Q)$"
                            svarx = 'Q'
                            plt.xscale('log')
                        elif '_d' in varx:
                            xx = wave / (2 * np.sin(np.radians(col[0])/2))
                            xla = r'$d$ ($\AA$)'
                            svarx = 'd'
                        else: 
                            xx = col[0]
                            xla = r'$2\theta$ (deg)'
                            svarx = 'tt'
                        yla = r'Intensity (arb. units)'
                        if '_logI' in varx:
                            liny = False 
                            plt.yscale('log')
                            yla = r"log(I)"
                        elif '_sqrtI' in varx:
                            liny = False  
                            # plt.yscale('squareroot')
                            yla = r"$sqrt(I)$ (arb. units)"
                            ax1.clear()
                            ax1.plot(xx, np.sqrt(col[1]), '.', ms=4, mfc='w', mec='k', label=r'Obs.')
                            ax1.plot(xx,np.sqrt(col[2]), 'g-', label=r'Calc.')
                            ax1.plot(xx, np.sqrt(col[1])-np.sqrt(col[2]), 'r-', label=r'Diff.')
                            ax1.plot(xx, col[1]*0.0, 'k-')
                            for b in range(nblnk):
                                if nblnk == 1 : blabel = 'Backgr.'
                                else : blabel = 'Backgr. %i'%(b+1)
                                ax1.plot(xx, np.sqrt(col[3+nstr+b]),'%s-'%bcolor[b], label=r'%s'%(blabel))
                        if not '_sqrtI' in varx:
                            ax1.clear()
                            ax1.plot(xx, col[1], '.', ms=4, mfc='w', mec='k', label=r'Obs.')
                            ax1.plot(xx,col[2], 'g-', label=r'Calc.')
                            ax1.plot(xx, col[1]-col[2], 'r-', label=r'Diff.')
                            ax1.plot(xx, col[1]*0.0, 'k-')
                            for b in range(nblnk):
                                if nblnk == 1 : blabel = 'Backgr.'
                                else : blabel = 'Backgr. %i'%(b+1)
                                ax1.plot(xx, col[3+nstr+b],'%s-'%bcolor[b], label=r'%s'%(blabel))
                        if not '_logI' in varx: ax1.ticklabel_format(axis = 'y', style='sci', scilimits=(-1,6))
                        ax1.set_xlabel(xla, fontsize=13)
                        ax1.set_ylabel(yla, fontsize=13)
                        ax1.set_title('%s Best fit'%dat)
                        ax1.legend()
                        ax2.clear()
                        ax2.plot(cycs, gof, 'ko', ms=1.5)
                        ax2.set_xlabel('cycle')
                        ax2.set_ylabel('GoF')
                        kk += 1
            liveplot = animation.FuncAnimation(ifig, fillaxes, interval=2000)
            plt.show()
        return

    ###***********************************************
    ####__sorting functions for mtx files
    def indmtx(self, a):
      l = np.argwhere(a==max(a)).flatten()
      c = np.arange(len(a))
      d = np.delete(c,l)
      e = np.delete(c,l-min(l))
      return l,d,e

    def srtmtx(self, a,l,d,e):
      b = np.zeros(len(a))
      b[l-min(l)] = a[l]
      b[e] = a[d]
      return b
    ###***********************************************
    def plot_size(self, dwaobj):
    
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
    
        for i in range(nstr):
            mtx = dwaobj.dwainfo['mtx%i'%(i+1)]
            dbx = dwaobj.dwainfo['db%i'%(i+1)]
            ###retrieve number of atomic species
            clxf = glob.glob(dbx + '*.smp_INFO')
            clx = open(clxf[0], 'r')
            lclx = clx.readline()
            clx.close()
            nats = int(lclx.split()[1])
            ###
            ## MONOVARIATE
            if (mtx[-6]=='1'):
                mtxr = mtx.rpartition('_plot1D')[0]
                ncol = 9 + 2 * nats
                col = []
                maxos,minos,maxds,minds = [],[],[],[]
                try:
                    col = np.loadtxt(mtx, skiprows = 2, unpack = True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
    #                si,si1,si2 = indmtx(col[0])
    #                for nc in range(len(col)):
    #                    col[nc] = srtmtx(col[nc],si,si1,si2)
                    #x1,y1 = Edges(col[3],col[1])
                    #x2,y2 = Edges(col[3],col[2])
                    plt.figure()
                    ax1 = plt.subplot(211)
                    plt.subplots_adjust(hspace = 0.1)
                    w = list(np.diff(col[3])/2)+[(np.diff(col[3])[-1])/2]
                    ax1.bar(col[3],col[1],color = 'r',edgecolor = 'k',align = 'center',width = w,label = r'Number distribution')
                    #ax1.fill(x1,y1,'g',ec = 'g',alpha = 0.75,label = r'Number fraction')
                    xticklabels  = ax1.get_xticklabels()
                    plt.setp(xticklabels, visible = False)
                    #ax1.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                    #ax1.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                    plt.ylabel(r'Number Fraction', fontsize=13)
                    plt.title('%s'%mtxr)
                    ax1.legend(loc = 1)
                    ax2 = plt.subplot(212)
                    ax2.bar(col[3],col[2],color = 'b',edgecolor = 'k',align = 'center',width = w,label = r'Mass distribution')
                    #ax2.fill(x2,y2,'orange',ec = 'orange',alpha = 0.75,label = r'Mass fraction')
                    #ax2.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                    #ax2.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                    plt.xlabel(r'Diameter (nm)', fontsize=13)
                    plt.ylabel(r'Mass Fraction', fontsize=13)
                    ##
                    ax2.legend(loc = 1)
                    #savefig('plot_dis1D.pdf')
        
            ## BIVARIATE
            elif (mtx[-6]=='2'):
                mtxr = mtx.rpartition('_plot2D')[0]
                ncol = 10 + 2 * nats
                b,h,nf,mf = [],[],[],[]
                maxos,minos,maxds,minds = [],[],[],[]
                try:
                    col = np.loadtxt(mtx, skiprows=2, unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    si,si1,si2 = self.indmtx(col[0])
                    for nc in range(len(col)):
                      col[nc] = self.srtmtx(col[nc],si,si1,si2)            
                    #b = np.reshape(col[7],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                    #h = np.reshape(col[8],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                    nf = np.reshape(col[2][::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                    mf = np.reshape(col[3][::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                    #print('b',b.shape)
                    #print('h',h.shape)
                    #sys.exit()
                    fwdt = 9
                    plt.figure(figsize = (13,6))
                    ax1 = plt.subplot(121)
                    ax1.set_aspect('equal')
                    #plt.subplots_adjust(vspace = 0.1)
                    #sc1 = ax1.scatter(col[7],col[8],marker = 's',s = 50,c = col[2],edgecolor = 'none')#,\
                    #             cmap = plt.cm.spectral,alpha = 0.55)
                    #sc1 = ax1.pcolor(b,h,nf,edgecolors = 'none',cmap = plt.cm.spectral)
                    im1 = ax1.imshow(nf, cmap = plt.cm.Spectral_r,
                               extent = [0, max(col[7]), 0, max(col[8])], alpha=1., interpolation='nearest')
                    plt.colorbar(im1)
                    #xticklabels =ax1.get_xticklabels()
                    #plt.setp(xticklabels, visible=False)
                    plt.xlim(min(col[7]),max(col[7]))
                    plt.ylim(min(col[8]),max(col[8]))
                    #ax1.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                    #ax1.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                    plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13, position=(0.5,0.5))
                    plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13, position=(0.5,0.5))
                    plt.title('%s \n Number distribution'%mtxr)
                    #ax1.legend(loc = 0)
                    ax2 = plt.subplot(122)
                    ax1.set_aspect('equal')
                    #sc2 = ax2.scatter(col[7],col[8],marker = 's',s = 50,c = col[3],edgecolor = 'none')#,\
                    #             cmap = plt.cm.spectral,alpha = 0.35)
                    #sc2 = ax2.pcolor(b,h,mf, cmap = plt.cm.spectral,edgecolors = 'none')
                    im2 = ax2.imshow(mf, cmap = plt.cm.Spectral_r,
                               extent = [0, max(col[7]), 0, max(col[8])], alpha=1., interpolation='nearest')
                    plt.colorbar(im2)
                    plt.xlim(min(col[7]),max(col[7]))
                    plt.ylim(min(col[8]),max(col[8]))
                    #ax2.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                    #ax2.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                    plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13, position=(0.5,0.5))
                    plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13, position=(0.5,0.5))
                    plt.title('%s \n Mass distribution'%mtxr)
                    #ax2.legend(loc = 0)
                    #savefig('plot_dis2D.pdf')
        plt.show()
        return 

    ###***********************************************
    def plot_cel(self, dwaobj):
        '''
        Returns plots of the lattice constant(s) 
        '''
        eps = 1.0e-5
    
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
    
        for i in range(nstr):
            mtx = dwaobj.dwainfo['mtx%i'%(i+1)]
            dbx = dwaobj.dwainfo['db%i'%(i+1)]
            ##__natc (and spgn) is not reliable flag in v2.0
            ##  retrieve the info from smp_INFO files
            # nats = dwaobj.dwainfo['natc%i'%(i+1)]
            ###retrieve number of atomic species and lattice constants
            clxf = glob.glob(dbx + '*.smp_INFO')
            clx = open(clxf[0], 'r')
            lclx = clx.readlines()
            clx.close()
            nats = int(lclx[0].split()[1])
            labcabg = lclx[-1].split()
            abcabg = []
            for ii in range(6):
                abcabg+=[float(labcabg[ii])]
            ###
            if 'prot%i'%(i+1) in dwaobj.dwainfo:
                if dwaobj.dwainfo['prot%i'%(i+1)] == 'yes':
                    if 'cell%i'%(i+1) in dwaobj.dwainfo:
                        labcabg = dwaobj.dwainfo['cell%i'%(i+1)].split()
                        abcabg = []
                        for ii in range(len(labcabg)):
                            abcabg+=[float(labcabg[ii])]
                        ###
            ## MONOVARIATE
            if (mtx[-6]=='1'):
                mtxr = mtx.rpartition('_plot1D')[0]
                ncol = 9 + 2 * nats
                maxle,minle,maxds,minds = [],[],[],[]
                try:
                    col = np.loadtxt(mtx, skiprows = 2, unpack = True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    ize = np.argwhere(col[8] <= eps)
                    ig = np.delete(range(len(col[8])),ize)
                    sizp,lep = col[3][ig], col[8][ig]
                    if abcabg[0] == abcabg[2]:
                        plt.figure()
                        ax1 = plt.subplot(111)
                        ax1.plot(sizp,lep*abcabg[0],'.',c = '%s'%lcolor[0],label = r'$a$')
                        maxle+=[max(lep)*abcabg[0]]
                        minle+=[min(lep)*abcabg[0]]
                        #xticklabels =ax1.get_xticklabels()
                        #plt.setp(xticklabels, visible = False)
                        #ax1.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                        #ax1.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                        plt.ylim(min(minle)-min(minle)*0.01,max(maxle)+max(maxle)*0.01)
                        plt.xlabel(r'Diameter (nm)', fontsize=13)
                        plt.ylabel(r'Lattice parameter, $a$ ($\AA$)', fontsize=13)
                        ax1.legend(loc = 0)
                        plt.title('%s'%mtxr)
                    elif abcabg[0] != abcabg[2]:
                        plt.figure()
                        ax1 = plt.subplot(211)
                        ax1.plot(sizp,lep*abcabg[0],'.',c = '%s'%lcolor[0],label = r'$a = b$')
                        maxle+=[max(lep)*abcabg[0]]
                        minle+=[min(lep)*abcabg[0]]
                        #xticklabels  = ax1.get_xticklabels()
                        #plt.setp(xticklabels, visible = False)
                        #ax1.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                        #ax1.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                        plt.ylim(min(minle)-min(minle)*0.01,max(maxle)+max(maxle)*0.01)
                        plt.xlabel(r'Diameter (nm)', fontsize=13)
                        plt.ylabel(r'Lattice parameter, $a = b$ ($\AA$)', fontsize=13)
                        ax1.legend(loc = 0)
                        plt.title('%s'%mtxr)
                        ax1 = plt.subplot(212)
                        ax1.plot(sizp,lep*abcabg[2],'.',c = '%s'%lcolor[1],label = r'$c$')
                        maxle+=[max(lep)*abcabg[2]]
                        minle+=[min(lep)*abcabg[2]]
                        #xticklabels =ax1.get_xticklabels()
                        #plt.setp(xticklabels, visible = False)
                        #ax1.tick_params(axis = 'both',which = 'major',length = 6 ,width = 2)
                        #ax1.tick_params(axis = 'both',which = 'minor',length = 4 ,width = 2)
                        plt.ylim(min(minle)-min(minle)*0.01,max(maxle)+max(maxle)*0.01)
                        plt.xlabel(r'Diameter (nm)', fontsize=13)
                        plt.ylabel(r'Lattice parameter, $c$ ($\AA$)', fontsize=13)
                        ax1.legend(loc = 0)
            ## BIVARIATE
            elif (mtx[-6]=='2'):
                mtxr = mtx.rpartition('_plot2D')[0]
                b,h,nf,mf = [],[],[],[]
                maxos,minos,maxds,minds = [],[],[],[]
                ncol = 10 + 2 * nats
                try:
                    col = np.loadtxt(mtx, skiprows=2, unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    si,si1,si2 = self.indmtx(col[0])
                    for nc in range(len(col)):
                        col[nc] = self.srtmtx(col[nc],si,si1,si2)
                    ize = np.argwhere(col[9] <= eps)
                    ig = np.delete(range(len(col[9])),ize)
                    dab,lc,lep = col[7][ig],col[8][ig],col[9][ig]
                    nf_eff,mf_eff = col[0][ig],col[1][ig]
                    if abcabg[0] == abcabg[2]:
                        aa = lep * abcabg[0]
                        if str(min(lep)) == str(max(lep)):
                            plt.figure()
                            ax = plt.subplot(211)
                            ax.plot(dab,aa,'o',ms = 5,c = '%s'%lcolor[0])
                            plt.xlim(min(dab),max(dab))
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $a = b$ ($\AA$)', fontsize=13)
                            plt.title('%s'%mtxr)
                            ax = plt.subplot(212)
                            ax.plot(lc,aa,'o',ms = 5,c = '%s'%lcolor[1])
                            plt.xlim(min(lc),max(lc))
                            plt.xlabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $a = b$ ($\AA$)', fontsize=13)
                        elif str(min(lep))!=str(max(lep)):
                            aex = col[9]*abcabg[0]
                            lef = np.reshape(aex[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                            plt.figure()
                            ax = plt.subplot(111)
                            imle = ax.imshow(lef,cmap = plt.cm.Spectral_r, extent = [0, max(col[7]), 0, max(col[8])], 
                                alpha = 1., vmin = min(aex),vmax = max(aex), interpolation = 'nearest')
                            plt.colorbar(imle)
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.title('%s, $a$'%mtxr)
                            #ax1.legend(loc = 0)
                    if abcabg[0] != abcabg[2]:
                        aa = lep*abcabg[0]
                        cc = lep*abcabg[2]
                        if str(min(lep))==str(max(lep)):
                            plt.figure()
                            ax = plt.subplot(211)
                            ax.plot(dab,aa,'o',ms = 5,c = '%s'%lcolor[0])
                            plt.xlim(min(dab),max(dab))
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $a = b$ ($\AA$)', fontsize=13)
                            plt.title('%s'%mtxr,fontsize = 11)
                            ax = plt.subplot(212)
                            ax.plot(lc,aa,'o',ms = 5,c = '%s'%lcolor[1])
                            plt.xlim(min(lc),max(lc))
                            plt.xlabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $a = b$ ($\AA$)', fontsize=13)
                            plt.figure()
                            ax = plt.subplot(211)
                            ax.plot(dab,cc,'o',ms = 5,c = '%s'%lcolor[0])
                            plt.xlim(min(dab),max(dab))
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $c$ ($\AA$)', fontsize=13)
                            plt.title('%s'%mtxr)
                            ax = plt.subplot(212)
                            ax.plot(lc,cc,'o',ms = 5,c = '%s'%lcolor[1])
                            plt.xlim(min(lc),max(lc))
                            plt.xlabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.ylabel(r'Lattice parameter, $c$ ($\AA$)', fontsize=13)
                        elif str(min(lep))!=str(max(lep)):
                            aex,cex = col[9]*abcabg[0],col[9]*abcabg[2]
                            lef = np.reshape(aex[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                            plt.figure()
                            ax = plt.subplot(111)
                            imle = ax.imshow(lef,cmap = plt.cm.Spectral_r, extent = [0, max(col[7]), 0, max(col[8])], 
                                    alpha = 1., vmin = min(aex), vmax = max(aex), interpolation = 'nearest')
                            plt.colorbar(imle)
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.title('%s, $a$'%mtxr,fontsize = 11)
                            lef = np.reshape(cex[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                            plt.figure()
                            ax = plt.subplot(111)
                            imle = ax.imshow(lef,cmap = plt.cm.Spectral_r, extent=[0, max(col[7]), 0, max(col[8])],
                                  alpha = 1., vmin = min(cex), vmax = max(cex), interpolation = 'nearest')
                            plt.colorbar(imle)
                            plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                            plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                            plt.title('%s, $c$'%mtxr)
                            #ax1.legend(loc = 0)
        plt.show()
        return 

    ###***********************************************
    def plot_sof(self, dwaobj):
        '''
        Returns plots of the S.O.F. for each atomic species 
        '''
    
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
    
        for i in range(nstr):
            mtx = dwaobj.dwainfo['mtx%i'%(i+1)]
            dbx = dwaobj.dwainfo['db%i'%(i+1)]
            ##__natc (and spgn) is not reliable flag in v2.0
            ##  retrieve the info from smp_INFO files
            # nats = dwaobj.dwainfo['natc%i'%(i+1)]
            ###retrieve number of atomic species
            clxf = glob.glob(dbx + '*.smp_INFO')
            clx = open(clxf[0], 'r')
            lclx = clx.readline()
            clx.close()
            nats = int(lclx.split()[1])
            ###
            ## MONOVARIATE
            if (mtx[-6]=='1'):
                mtxr = mtx.rpartition('_plot1D')[0]
                ncol = 9 + 2 * nats
                col = []
                maxos, minos, maxds, minds = [], [], [], []
                try:
                    col=np.loadtxt(mtx,skiprows=2,unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    ##plot atomic factors
                    plt.figure()
                    ax1=plt.subplot(111)
                    for o in range(nats):
                        ax1.plot(col[3],col[9+2*o],'.',c='%s'%lcolor[o],label=r'S.O.F. ATOM%i'%(o+1))
                        maxos+=[max(col[9+2*o])]
                        minos+=[min(col[9+2*o])]
                    #xticklabels =ax1.get_xticklabels()
                    #plt.setp(xticklabels, visible=False)
                    #ax1.tick_params(axis='both',which='major',length=6 ,width=2)
                    #ax1.tick_params(axis='both',which='minor',length=4 ,width=2)
                    plt.ylim(min(minos)-min(minos)*0.01,max(maxos)+max(maxos)*0.01)
                    plt.xlabel(r'Diameter (nm)', fontsize=13)
                    plt.ylabel(r'Site Occupation Factor', fontsize=13)
                    plt.title('%s'%mtxr,fontsize=11)
                    ax1.legend(loc=0)
            ## BIVARIATE
            elif (mtx[-6]=='2'):
                mtxr = mtx.rpartition('_plot2D')[0]
                ncol = 10 + 2 * nats
                b, h, nf, mf = [], [], [], []
                maxos,minos,maxds,minds=[],[],[],[]
                try:
                    col = np.loadtxt(mtx,skiprows=2,unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    si,si1,si2=self.indmtx(col[0])
                    for nc in range(len(col)):
                      col[nc]=self.srtmtx(col[nc],si,si1,si2)            
                    ##plot atomic factors
                    for a in range(nats):
                      ol=col[10+2*a]
                      bl,ll=col[7],col[8]
                      if str(min(ol))==str(max(ol)):
                        plt.figure()
                        ax=plt.subplot(211)
                        ax.plot(bl,ol,'o',ms=5,c='%s'%lcolor[a])
                        plt.xlim(min(bl),max(bl))
                        plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                        plt.ylabel(r'Site Occupation Factor', fontsize=13)
                        plt.title('%s \n ATOM %i'%(mtxr,a+1),fontsize=11)
                        ax=plt.subplot(212)
                        ax.plot(ll,ol,'o',ms=5,c='%s'%lcolor[a])
                        plt.xlim(min(ll),max(ll))
                        plt.xlabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                        plt.ylabel(r'Site Occupation Factor', fontsize=13)
                      elif str(min(ol))!=str(max(ol)):
                        #of=np.reshape(col[10+2*a][::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                        of=np.reshape(ol[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                        plt.figure()
                        ax=plt.subplot(111)
                        #ax1.plot(col[4],col[10+2*o],'-',c='%s'%lcolor[o],label=r'OCC. ATOM%i'%(o+1))
                        #imo=ax.imshow(of,cmap=plt.cm.Spectral_r,extent=[0,max(col[7]),0,max(col[8])],alpha=1.)
                        imo = ax.imshow(of, cmap = plt.cm.Spectral_r, extent=[0, max(col[7]), 0, max(col[8])],
                            alpha = 1., vmin = min(ol), vmax = max(ol), interpolation = 'nearest')
                        plt.colorbar(imo)
                        #ax1.tick_params(axis='both',which='major',length=6 ,width=2)
                        #ax1.tick_params(axis='both',which='minor',length=4 ,width=2)
                        plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                        plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                        #ax1.legend(loc=0)
                        plt.title('%s \n S.O.F. ATOM %i'%(mtxr,a+1),fontsize=11)
        plt.show()
        return

    ###***********************************************    
    def plot_bth(self, dwaobj):
        '''
        Returns plots of the BTH for each atomic species 
        '''
    
        ndat = dwaobj.ndataset
        nstr = dwaobj.nstructure
    
        mlim=1.e-20
        eps=1.e-8
        for i in range(nstr):
            mtx = dwaobj.dwainfo['mtx%i'%(i+1)]
            dbx = dwaobj.dwainfo['db%i'%(i+1)]
            ##__natc (and spgn) is not reliable flag in v2.0
            ##  retrieve the info from smp_INFO files
            # nats = dwaobj.dwainfo['natc%i'%(i+1)]
            ###retrieve number of atomic species
            clxf = glob.glob(dbx + '*.smp_INFO')
            clx = open(clxf[0], 'r')
            lclx = clx.readline()
            clx.close()
            nats = int(lclx.split()[1])
            ###
            ## MONOVARIATE
            if (mtx[-6]=='1'):
                mtxr=mtx.rpartition('_plot1D')[0]
                ncol = 9 + 2 * nats
                maxos,minos,maxds,minds=[],[],[],[]
                try:
                    col=np.loadtxt(mtx,skiprows=2,unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    ##plot atomic factors
                    plt.figure()
                    ax2=plt.subplot(111)
                    for b in range(nats):
                        dwl=col[10+2*b]
                        ax2.plot(col[3],col[10+2*b],'.',c='%s'%lcolor[b],label=r'BTH ATOM%i'%(b+1))
                        maxds+=[max(dwl)]#[max(col[10+2*b])]
                        minds+=[min(dwl)]#[min(col[10+2*b])]
                    #ax2.tick_params(axis='both',which='major',length=6 ,width=2)
                    #ax2.tick_params(axis='both',which='minor',length=4 ,width=2)
                    plt.ylim(min(minds)-min(minds)*0.01,max(maxds)+max(maxds)*0.01)
                    plt.xlabel(r'Diameter (nm)', fontsize=13)
                    plt.ylabel(r'Thermal parameter $B$ [${\mathrm{\AA^2}}$]', fontsize=13)
                    ax2.legend(loc=0)
                    #savefig('plot_OCC-DW.pdf')
            ## BIVARIATE
            elif (mtx[-6]=='2'):
                mtxr=mtx.rpartition('_plot2D')[0]
                nats=nats
                ncol=10 + 2 * nats
                b,h,nf,mf=[],[],[],[]
                maxos,minos,maxds,minds=[],[],[],[]
                try:
                    col=np.loadtxt(mtx,skiprows=2,unpack=True)
                except IOError:
                    print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
                else:
                    si,si1,si2=self.indmtx(col[0])
                    for nc in range(len(col)):
                      col[nc]=self.srtmtx(col[nc],si,si1,si2)
                    ##plot atomic factors
                    for a in range(nats):
                      dwl=col[11+2*a]
                      bl,ll=col[7],col[8]
                      if str(min(dwl))==str(max(dwl)):
                        plt.figure()
                        ax=plt.subplot(211)
                        ax.plot(bl,dwl,'o',ms=5,c='%s'%lcolor[a])
                        plt.xlim(min(bl),max(bl))
                        plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                        plt.ylabel(r'Thermal parameter $B$ [${\mathrm{\AA^2}}$]', fontsize=13)
                        plt.title('%s \n ATOM %i'%(mtxr,a+1),fontsize=11)
                        ax=plt.subplot(212)
                        ax.plot(ll,dwl,'o',ms=5,c='%s'%lcolor[a])
                        plt.xlim(min(ll),max(ll))
                        plt.xlabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                        plt.ylabel(r'Thermal parameter $B$ [${\mathrm{\AA^2}}$]', fontsize=13)
                      elif str(min(dwl))!=str(max(dwl)):
                        #dwf=np.reshape(col[11+2*a][::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                        dwf=np.reshape(dwl[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
                        plt.figure()
                        ax=plt.subplot(111)
                        #ax.plot(col[4],col[10+2*o],'-',c='%s'%lcolor[o],label=r'OCC. ATOM%i'%(o+1))
                        imdw = ax.imshow(dwf, cmap = plt.cm.Spectral_r.set_under('w'), 
                                 extent = [0, max(col[7]), 0, max(col[8])], alpha=1., vmin = min(dwl), vmax = max(dwl), interpolation = 'nearest')
                        plt.colorbar(imdw)
                        #ax1.tick_params(axis='both',which='major',length=6 ,width=2)
                        #ax1.tick_params(axis='both',which='minor',length=4 ,width=2)
                        plt.xlabel(r'D$_{\rm{ab}}$ (nm)', fontsize=13)
                        plt.ylabel(r'L$_{\rm{c}}$ (nm)', fontsize=13)
                        plt.title('%s \n BTH ATOM %i'%(mtxr,a+1),fontsize=11)
        plt.show()
        return

    ###***********************************************
    def plot_map(self, mtx,xc,yc,zc,xrng=[0,0],yrng=[0,0],zrng=[0,0],thre=[4,0.0],xlbl='',ylbl='',titl='',fwdt=20.32,fhig=15.24,foty='serif',fosi=11):
        '''
        Returns a colormap of ... 
        '''

        plt.rc('font',family=foty)
        plt.rc('font',size=fosi) #font of plt.title,plt.xlabel, etc.
        plt.rc(('xtick','ytick'),labelsize=fosi) #sets fontsize of labels on xticks, yticks.
        plt.rc('plt.legend',fontsize=fosi)
    
        threc=int(thre[0])-1
        threv=float(thre[-1])
        col=[]
        eps=1.e-8
        col=[]
        try:
            col=np.loadtxt(mtx,usecols=(0,1,threc,xc,yc,zc),unpack=True)
        except IOError:
            print(">>>>>   Error: can\'t find file %s or read data    <<<<<"%(mtx))
        else:
            si,si1,si2=self.indmtx(col[0])
            for nc in range(len(col)):
                col[nc]=self.srtmtx(col[nc],si,si1,si2)
            cx,cy,cz=col[3],col[4],col[5]
            ii,i1=[],[]
            if (abs(threv-0.)>eps):
              if self.verbose : print('mapping with threshold at %f'%threv)
              ii=np.argwhere(col[2]-threv<eps).flatten()
              i1=np.delete(np.arange(len(cz)),ii)#np.argwhere(col[2]-threv>=eps).flatten()
              if len(i1)==0:
                print('THRESHOLD TOO LOW, NOTHING TO PLOT!')
                exit()
              elif len(i1)>0:
                cz[ii]=-1
                cz1=cz[i1]
                cmap=plt.cm.Spectral_r.set_under('w')
                vmi,vma=min(cz1),max(cz1)
            else:
              cmap=plt.cm.Spectral_r
              vmi,vma=min(cz),max(cz)
            if zrng[1]>0:
              vmi,vma=zrng[0],zrng[1]
            if xrng[1]>0:
              xmin,xmax=xrng[0],xrng[1]
            else:
              xmin,xmax=0,max(cx)
            if yrng[1]>0:
              ymin,ymax=yrng[0],yrng[1]
            else:
              ymin,ymax=0,max(cy)
            npx,npy=(max(col[1])-min(col[1])+1),(max(col[0])-min(col[0])+1)
            img=np.reshape(cz[::-1],(int(max(col[1])-min(col[1])+1),int(max(col[0])-min(col[0])+1)))
            plt.figure(figsize=(fwdt,fhig))
            plt.subplots_adjust(wspace=0.2)
            #ax=plt.subplot(111)
            ax=plt.axes([0.125,0.15,0.95-0.25,0.95-0.2])
            map = ax.imshow(img, cmap = cmap, extent = [xmin, xmax, ymin, ymax], alpha =1.,
                  vmin = vmi, vmax = vma, interpolation = 'nearest')
            plt.colorbar(map,format='%.3f')
            plt.xlabel(r'%s'%xlbl)
            plt.ylabel(r'%s'%ylbl)
            plt.title(titl,fontsize=fosi)
        #     savefig('map.pdf', bbox_inches='tight',pad_inches=0.1)
            plt.show()
        return

    ###***********************************************
    def plot_map_vec(self, vx,vy,vz,thrvec,thrval=0.0,xrng=[0,0],yrng=[0,0],zrng=[0,0],
            xlbl='',ylbl='',titl='',fwdt=20.32,fhig=15.24,foty='serif',fosi=11, interp = 'nearest'):
        '''
        Returns a colormap of ... 
        '''

        verbose=True
        plt.rc('font',family=foty)
        plt.rc('font',size=fosi) #font of plt.title,plt.xlabel, etc.
        plt.rc(('xtick','ytick'),labelsize=fosi) #sets fontsize of labels on xticks, yticks.
        plt.rc('legend',fontsize=fosi)
    
        col=[]
        eps=1.e-8
        ii,i1=[],[]
        if (abs(thrval-0.)>eps):
          if verbose : print('mapping with threshold at %f'%thrval)
          ii=np.argwhere(thrvec-thrval<eps).flatten()
          i1=np.delete(np.arange(len(vz)),ii)#np.argwhere(col[2]-threv>=eps).flatten()
          if len(i1)==0:
            print('THRESHOLD TOO LOW, NOTHING TO PLOT!')
            return
          elif len(i1)>0:
            vz[ii]=-1
            vz1=vz[i1]
            cmap=plt.cm.Spectral_r.set_under('w')
            vmi,vma=min(vz1),max(vz1)
        else:
          cmap=plt.cm.Spectral_r
          vmi,vma=min(vz),max(vz)
        if zrng[1]>0:
          vmi,vma=zrng[0],zrng[1]
        if xrng[1]>0:
          xmin,xmax=xrng[0],xrng[1]
        else:
          xmin,xmax=0,max(vx)
        if yrng[1]>0:
          ymin,ymax=yrng[0],yrng[1]
        else:
          ymin,ymax=0,max(vy)
        npy=np.argwhere(vx==max(vx)).flatten()[1]
        npx=len(np.argwhere(vx==max(vx)).flatten())
        img=np.reshape(vz[::-1],(npx,npy))
        plt.figure(figsize=(fwdt,fhig))
        plt.subplots_adjust(wspace=0.2)
        #ax=plt.subplot(111)
        ax=plt.axes([0.125,0.15,0.95-0.25,0.95-0.2])
        map = ax.imshow(img, cmap = cmap, extent = [xmin, xmax, ymin, ymax],
        alpha = 1., vmin = vmi, vmax = vma, interpolation = interp)
        plt.colorbar(map,format='%f')
        plt.xlabel(r'%s'%xlbl)
        plt.ylabel(r'%s'%ylbl)
        plt.title(titl,fontsize=fosi)
    #     savefig('map.pdf', bbox_inches='tight',pad_inches=0.1)
        plt.show()
        return

if __name__  ==  '__main__':
    nargs = len(sys.argv)
    if nargs == 3:
        plotter(sys.argv[1], sys.argv[2])
    elif nargs == 4:
        plotter(sys.argv[1], sys.argv[2], sys.argv[3])
    elif nargs < 3:
        print('  something wrong in input, missing some info..')


