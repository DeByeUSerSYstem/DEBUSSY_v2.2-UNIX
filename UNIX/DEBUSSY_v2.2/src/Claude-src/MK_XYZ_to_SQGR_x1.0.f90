!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Copyright (c) 2010 Antonio Cervellino, Antonietta Guagliardi
!
!     This file is part of the Claude / Debussy program suite
!     
!     This program is free software; you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation; either version 3 of the License (29 June 2007), 
!     or (at your option) any later version.
!     
!     The Claude / Debussy program suite is distributed in the hope that it will be 
!     useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License, version 3, 29 June 2007 for more details.
!     
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA
!
!==========================================================================================
program karn_evil_9
use Jmol_XYZfile
use helpinput

implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
real(DP),parameter :: default_num_density_Am3 = 0.016d0
character(10),parameter :: file_inp0 = 'xyz2gr.ini'
character(512) :: pwd,rlx,inpath, file_inp,namestr
integer(I4B) :: lpwd,lrlx,linpath
character(len=13),dimension(:,:),allocatable :: finu
Logical      :: DB_exist, isordnot, orthoXY, eqatpair,do_bypair, global_uiso
character(512) :: RL, wripai
real(DP)  :: Taux(3),Taux2(3),veff1(2),veff2(2)
real(DP)  :: mass_density_gcm3=zero
real(DP),allocatable :: auvex(:),sauvex(:),f_ave_sq(:),therbro(:)


print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_XYZ_to_SQGR Program    '
print*, ' '


call def_eps

DB_exist = .true.
do_bypair= .false.


!___ learn what is the currrent directory ($PWD)

call GET_PWD(pwd=pwd,lpwd=lpwd)

! reading name root of input file and its path

 iu99 = FIND_UNIT()

  OPEN(UNIT=iu99,status='old', &
       form='formatted',access='sequential', &
       file=pwd(1:lpwd)//file_inp0,action='READ', iostat=ierr)
  IF (ierr /=0) THEN
     print*, ' Error opening input file: ', file_inp0
     STOP
  ENDIF


!!!!! READING .xyz filename

read(iu99,'(a)') rlx ; print'(a)',trim(rlx)
rlx=trim(adjustl(rlx))
lrlx=len_trim(rlx)
jj=0
do j=lrlx,1,-1
  if (rlx(j:j)==separator) then
    jj=j
    exit
  endif
enddo
lroot=jj-1
namestr=''
namestr(1:lrlx-jj)=rlx(jj+1:lrlx)
Lnamestr=lrlx-jj
linpath=max(0,lroot)
inpath=''
if (linpath>0) THEN
  inpath(1:linpath+1)=rlx(1:linpath+1)
  file_inp=inpath(1:linpath+1)//namestr(1:Lnamestr)
else
  file_inp=namestr(1:Lnamestr)
endif
Lfile_inp=len_trim(file_inp)
namestr = namestr(1:Lnamestr-4)//'_001'
Lnamestr = Lnamestr
if (linpath > 0) then
  if (inpath(linpath:linpath)/=separator) then
    linpath=linpath+1
    inpath(linpath:linpath)=separator
  endif
endif
if (verbose) print'(a)',' INPATH:   '//inpath(1:linpath)
if (verbose) print'(a)',' NAMESTR:  '//namestr(1:Lnamestr)
print'(a)',' Input File          = '//trim(file_inp)

read(iu99,'(a)',iostat=ioa) rl; print'(a)',trim(rl)
multiple_steps=.true.
working_wavelength=zero
working_ttmax=160.d0
working_ttmin=0.d0
if (ioa==0) then
  rl=trim(adjustl(rl))
  lrl=len_trim(rl)
  call LOWCASE(rl(1:lrl))
  if (lrl>=3) then
    if (rl(1:3)=='one') then
      multiple_steps=.false.
      read(rl(4:lrl),*,iostat=ioa2) working_wavelength,working_ttmin,working_ttmax,working_dtt
      if (ioa2/=0) then
        working_wavelength=wl_ourqmax
        working_ttmax=160.d0
        working_ttmin=0.d0
        working_dtt=0.01d0
      endif
    endif
  endif
  working_ttmin = working_dtt*REAL(NINT(working_ttmin/working_dtt),DP)  ! avoid roundoff on step
  n_qSvec=1+CEILING((working_ttmax-working_ttmin)/working_dtt)
  allocate(sauvex(n_qSvec),auvex(n_qSvec),Q_S(n_qSvec),S_Q(n_qSvec),dQvv(n_qSvec),AtScFk(n_qSvec), f_ave_sq(n_qSvec), &
           therbro(n_qSvec))
  Q_S=(two/working_wavelength)*[(sin((working_ttmin+working_dtt*k)*duet2r),k=0,n_qSvec-1)]
  dQvv=(working_dtt*duet2r*two/working_wavelength)*[(cos((working_ttmin+working_dtt*k)*duet2r),k=0,n_qSvec-1)]
  S_Q=zero
  if (verbose) print*, 'working_wavelength,working_ttmax',working_wavelength,working_ttmax,working_ttmin,working_dtt
  if (verbose) print*, 'n. of q-points : ',n_qSvec,minval(Q_S),sum(Q_S)/n_qSvec,maxval(Q_S)
else
  stop 'missing wavelength/ 2thetamax'
endif


read(iu99,'(a)') rl; print'(a)',trim(rl)
rl=trim(adjustl(rl))
lrl=len_trim(rl)
call LOWCASE(rl(1:lrl))
if (lrl>=5.and.rl(1:5)=='rrang') then
  read(rl(6:lrl),*,iostat=ioa2) rmin,rmax,dr
  if (ioa2/=0) then
    rmin= 0.5d0
    rmax=100.d0
    dr=0.01d0
  endif
  n_rGvec = 1+CEILING((rmax-rmin)/dr)
  allocate(rGvec(n_rGvec),gRvec(n_rGvec))
  rGvec=[(rmin+k*dr,k=0,n_rGvec-1)]
  gRvec=zero
endif

read(iu99,'(a)') rl; print'(a)',trim(rl)
rl=trim(adjustl(rl))
lrl=len_trim(rl)
call LOWCASE(rl(1:lrl))
if (lrl>=5.and.rl(1:5)=='byatp') then
  read(rl(6:lrl),*,iostat=ioa2) ido_bypair
  if (ioa2/=0) then
    ido_bypair = 0
  endif
  do_bypair=(ido_bypair/=0)
  print*, 'Calculating G(r) and S(q) as sum of contributions by atom pairs'
endif

read(iu99,'(a)') rl; print'(a)',trim(rl)
rl=trim(adjustl(rl))
lrl=len_trim(rl)
call LOWCASE(rl(1:lrl))
if (lrl>=4.and.rl(1:4)=='dens') then
  read(rl(5:lrl),*,iostat=ioa2) x
  if (ioa2==0) then
    mass_density_gcm3=x
  else
    mass_density_gcm3=zero
  endif
endif
print*,'Cluster mass density [g/cm^3] = ',mass_density_gcm3
redmindis=zero
read(iu99,'(a)') rl; print'(a)',trim(rl)
rl=trim(adjustl(rl))
lrl=len_trim(rl)
call LOWCASE(rl(1:lrl))
if (lrl>=4.and.rl(1:4)=='redm') then
  read(rl(5:lrl),*,iostat=ioa2) x
  if (ioa2==0) then
    redmindis=x
  else
    redmindis=zero
  endif
endif
print*,'Tolerance on minimum distances [Angstroem] = ',redmindis

uiso_common=zero
read(iu99,'(a)') rl; print'(a)',trim(rl)
rl=trim(adjustl(rl))
lrl=len_trim(rl)
call LOWCASE(rl(1:lrl))
if (lrl>=4.and.rl(1:4)=='uiso') then
  read(rl(5:lrl),*,iostat=ioa2) x
  if (ioa2==0) then
    uiso_common=x
  else
    uiso_common=zero
  endif
endif
print*,'Forced overall isotropic thermal r.m.s. vibration amplitude [Angstroem] = ',uiso_common
global_uiso = (uiso_common > sceps_DP)

close(iu99)

print'(a,1x,f17.8)', ' Working Wavelength     = ',working_wavelength
print'(a,1x,f17.8)', ' 2*Theta Min/Max/Delta  = ',working_ttmin,working_ttmax,working_dtt
print*, ' '
print'(a,1x,f17.8)', ' Reduced minimal distance = ',redmindis
print'(a,1x,f17.8)', ' Mass Density [g/cm^3]    = ',mass_density_gcm3
if (mass_density_gcm3<sceps_DP) print'(a,1x,f11.4,a)',' Mass Density [g/cm^3] will be recalculated '//&
        'based on a default number density of ',default_num_density_Am3,' atoms/Angstroem^3'


!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path

sampled_folder='DISTANCES'; lsampled_folder=9
basepath_sampled=inpath(1:linpath); lbasepath_sampled=linpath

call DO_SETUP
if (verbose) print*,'Setup done!'
call read_XYZ_only(file_inp, redmindis)
if (verbose) print*,'File .xyz read!'
if (verbose) then
  print*,Celty(1)%termcon
  print*,Celty(1)%termcon_all
  print*,Celty(1)%termcon_ineq
  print*,Celty(1)%summul
endif  

if (do_bypair) then
  print*,'Will produce pairwise output...',do_bypair
  allocate(S_Q_byp(n_qSvec,n_at_pair_V(1)), gR_byp(n_rGvec, n_at_pair_V(1)) )
  S_Q_byp=zero; gR_byp=zero
endif
if (verbose) print*,'Sq, Gr arrays allocated!'


summul=zero
if (verbose) print*,'Start evaluating distances...'
call CPU_TIME(t0)

DO ipa=1,n_at_pair_V(1)
  if (verbose) print*,'Read Pair: ',ipa,' [',Celty(1)%zappa(:,ipa),'] {', &
     Celty(1)%Z_at(Celty(1)%zappa(:,ipa)),'} (',Celty(1)%nat(Celty(1)%zappa(:,ipa)),')'
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  if (verbose) print*,is1,Celty(1)%Z_at(is1),Celty(1)%nat(is1),themolec(is1)%ispec,themolec(is1)%zspec,themolec(is1)%nathis
  if (verbose) print*,is2,Celty(1)%Z_at(is2),Celty(1)%nat(is2),themolec(is2)%ispec,themolec(is2)%zspec,themolec(is2)%nathis
enddo
if (verbose) print*,allocated(themolec),size(themolec)
Celty(1)%termcon_all = zero
f_ave_sq = zero
sf_ave_sq= zero
u8pi2=0.125d0/(pi*pi)
dpi2=pi2*pi
DO ipa=1,n_at_pair_V(1)
  if (verbose) print*,'Doing Pair: ',ipa,' [',Celty(1)%zappa(:,ipa),'] {', &
      Celty(1)%Z_at(Celty(1)%zappa(:,ipa)),'} (',Celty(1)%nat(Celty(1)%zappa(:,ipa)),')'
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  eqatpair=(is1==is2)
  veff1=Anomalous_X(Z_e=izz1,wavelength=working_wavelength)
  veff2=Anomalous_X(Z_e=izz2,wavelength=working_wavelength)
  AtScFk = (veff1(1)+FormFact_EPDL97(q=Q_S,Z_e=izz1)) * &
           (veff2(1)+FormFact_EPDL97(q=Q_S,Z_e=izz2)) + veff1(2)*veff2(2)
  if (verbose) then
    print*,'AtScFk of pair [ ',ipa,' : ',izz1,izz2,' ] calculated'
    print*,'m/a/M : ',minval(AtScFk),sum(AtScFk)/size(AtScFk),maxval(AtScFk)
  endif
  if (eqatpair) then
    if (verbose) print*,ipa,is1,is2,izz1,izz2
  endif
  do ia1=1,Celty(1)%nat(is1)
    iini=1
    if (eqatpair) iini=ia1
    Taux=themolec(is1)%coo_xyzbo(1:3,ia1)
    ooo=two*themolec(is1)%coo_xyzbo(5,ia1)
    if (global_uiso) then
      uuus=uiso_common**2                          ! this is <u^2> = B/(8*pi^2)
    else
      uuus=max(zero,themolec(is1)%coo_xyzbo(4,ia1)) * u8pi2  ! this is <u^2> = B/(8*pi^2)
    endif
    do ia2=iini,Celty(1)%nat(is2)
      Taux2=Taux-themolec(is2)%coo_xyzbo(1:3,ia2)
      ooo2=ooo*themolec(is2)%coo_xyzbo(5,ia2)
      if (global_uiso) then
        uuu2s=uuus*two                                               ! this is again <u^2> = B/(8*pi^2)
      else
        uuu2s=uuus+max(zero,themolec(is1)%coo_xyzbo(4,ia2)) * u8pi2  ! this is again <u^2> = B/(8*pi^2)
      endif
      if (eqatpair .and. ia1==ia2) then
        ooo2=half*ooo2
      endif
      d00=sqrt(sum(Taux2**2))
!      if (verbose) print*,d00,ooo2
      if (d00<Celty(1)%minallowdist(ipa)) then
        Celty(1)%termcon_all(ipa)=Celty(1)%termcon_all(ipa)+ooo2
        if (d00>s4eps_DP .and. verbose) &
          print*,'Small D: ',ipa,is1,is2,izz1,izz2,ia1,ia2,d00,Celty(1)%minallowdist(ipa)
      else
        Celty(1)%summul(ipa) = Celty(1)%summul(ipa)+ooo2
        sauvex = pi2*Q_S*d00
        where (abs(sauvex)>sceps_DP)
          auvex=sin(sauvex)/sauvex
        elsewhere
          auvex=one
        end where
        auvex = auvex * ooo2 * AtScFk
        f_ave_sq = f_ave_sq + AtScFk * ooo2
        sf_ave_sq = sf_ave_sq + ooo2
        if (uuu2s<=sceps_DP) then
          S_Q = S_Q + auvex
        else
          ccq=dpi2*uuu2s
          therbro=exp(-ccq*(Q_S**2))
          S_Q = S_Q + auvex * therbro
        endif
        if (do_bypair) then
          if (uuu2s<=sceps_DP) then
            S_Q_byp(:,ipa) = S_Q_byp(:,ipa) + auvex
          else
            S_Q_byp(:,ipa) = S_Q_byp(:,ipa) + auvex * therbro
          endif
        endif
      endif
    enddo
  enddo
ENDDO
if (verbose) print*,'<f>**2 : ',minval(f_ave_sq),sum(f_ave_sq)/size(f_ave_sq),maxval(f_ave_sq),sf_ave_sq
f_ave_sq = f_ave_sq / sf_ave_sq
S_Q = S_Q / f_ave_sq
if (verbose) print*,'S_Q : ',minval(S_Q),sum(S_Q)/size(S_Q),maxval(S_Q),do_bypair
if (do_bypair) then
  DO ipa=1,n_at_pair_V(1)
    S_Q_byp(:,ipa) = S_Q_byp(:,ipa) * dQvv / f_ave_sq
  ENDDO
endif
!_____ recalc. G(r)
do iq=1, n_qSvec
  gRvec(:)=gRvec(:) + S_Q(iq) * dQvv(iq) * Q_S(iq) * sin(pi2*Q_S(iq)*rGvec(:))
enddo
gRvec=gRvec*eight*Pi

if (do_bypair) then
  DO ipa=1,n_at_pair_V(1)
    do iq=1, n_qSvec
      Gr_byp(:,ipa) = Gr_byp(:,ipa) +S_Q_byp(iq,ipa)*Q_S(iq)*sin(pi2*Q_S(iq)*rGvec(:))
    enddo
  ENDDO
  Gr_byp=Gr_byp*eight*pi
endif

!_______________________________________________________



call CPU_TIME(t1)
print*, ' '
print*,' Done!   CPU(Time) ',t1-t0, 'sec.'
print*, ' '
if (verbose) then
  print*,Celty(1)%termcon
  print*,Celty(1)%termcon_all
  print*,Celty(1)%termcon_ineq
  print*,Celty(1)%summul
endif

Celty(1)%termcon=Celty(1)%termcon_all(Celty(1)%point_eqpair(:))
Celty(1)%termcon_ineq=Celty(1)%termcon_all(Celty(1)%point_neqpair(:))

Z_at_glo = Celty(1)%Z_at
nat_glo = Celty(1)%nat
zappa_glo = Celty(1)%zappa
ndi_glo = Celty(1)%ndi
xnat_glo = Celty(1)%xnat
termcon_glo = Celty(1)%termcon
termcon_all_glo = Celty(1)%termcon_all
termcon_ineq_glo = Celty(1)%termcon_ineq
summul_glo = Celty(1)%summul
! eval. volume based on density ...
wmol=zero
xna_all=zero
do ia=1,n_sp_atom_V(1)
  iz=Celty(1)%Z_at(ia)
  xn=Celty(1)%xnat(ia)
  wmol=wmol+xn*atwei(iz)
  xna_all=xna_all+xn
enddo

if (mass_density_gcm3>0.000001d0) then
  xnum_density = xna_all * (mass_density_gcm3/wmol) * N_A * 1.0d-24
  vol_A3 = xna_all / xnum_density
  print'(a,1x,f17.8)',' Number density = ', xnum_density
  print'(a,1x,f17.8)',' Cluster Volume', vol_A3
else
  vol_A3 = xna_all / default_num_density_Am3
  print'(a,1x,f17.8)',' Number density (using default value)             = ', default_num_density_Am3
  print'(a,1x,f17.8)',' Cluster Volume (based on default Number Density) = ', vol_A3
endif
print'(a,1x,f17.8)',' Final Mass Density [g/cm^3]    = ',mass_density_gcm3
cell_volume=vol_A3
ncenters_cell=1
cell_volume_red=cell_volume
!allocate(numcells_byphase(1))
!numcells_byphase=1
    
Size_Abscissa(1)=one
xxx=exp(unter*log(six*vol_A3/pi)) / ten
!print*,'vvv',vol_A3,xxx
print'(a,1x,f17.8)',' Estimated cluster diameter (nm) = ',xxx

Actual_Diam(:) = xxx

!call OUTSAMP(finumb=namestr(1:lnamestr)//'.smp',Bpre=zero)
iuos=find_unit()
open(iuos,status='replace',file=namestr(1:lnamestr)//'.Dsq')
if (do_bypair) then
  wripai=''
  do ipa=1,n_at_pair_V(1)
    is1=Celty(1)%zappa(1,ipa)
    is2=Celty(1)%zappa(2,ipa)
    write(wripai((ipa-1)*9+1:ipa*9),'("S(q)_",2a2)') symb_of_Z( Celty(1)%Z_at(is1) ), symb_of_Z( Celty(1)%Z_at(is2) )
  enddo
  write(iuos,'("#   S(q)_tot ",a)') wripai(1:9*n_at_pair_V(1))
  do iq=1,n_qSvec
    write(iuos,*)Q_S(iq),S_Q(iq),S_Q_byp(iq,:)
  enddo
else
  wripai=''
  write(iuos,'("#   S(q)_tot ")')
  do iq=1,n_qSvec
    write(iuos,*)Q_S(iq),S_Q(iq)
  enddo
endif
close(iuos)


iuog=find_unit()
open(iuog,status='replace',file=namestr(1:lnamestr)//'.Dgr')
if (do_bypair) then
  wripai=''
  do ipa=1,n_at_pair_V(1)
    is1=Celty(1)%zappa(1,ipa)
    is2=Celty(1)%zappa(2,ipa)
    write(wripai((ipa-1)*9+1:ipa*9),'("G(r)_",2a2)') symb_of_Z( Celty(1)%Z_at(is1) ), symb_of_Z( Celty(1)%Z_at(is2) )
  enddo
  write(iuog,'("#   G(r)_tot ",a)') wripai(1:9*n_at_pair_V(1))
  do ir=1,n_rGvec
    write(iuog,*)rGvec(ir),gRvec(ir),gR_byp(ir,:)
  enddo
else
  wripai=''
  write(iuog,'("#   G(r)_tot ")')
  do ir=1,n_rGvec
    write(iuog,*)rGvec(ir),gRvec(ir)
  enddo
endif
close(iuog)



print*, ' '
print*, '******* JOB SMP SINGLE CLUSTER DONE! *******'

end program karn_evil_9
