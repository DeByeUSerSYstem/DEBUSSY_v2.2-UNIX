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
character(10),parameter :: file_inp0 = 'molmkd.ini'
character(512) :: pwd,rlx,inpath, file_inp,namestr
integer(I4B) :: lpwd,lrlx,linpath
character(len=13),dimension(:,:),allocatable :: finu
Logical      :: DB_exist, isordnot, orthoXY, eqatpair
character(512) :: RL
real(DP)  :: Taux(3),Taux2(3)
real(DP)  :: mass_density_gcm3=zero


print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_MOLEC Program    '
print*, ' '

!!FB cputime
 call CPU_TIME(t0)

call def_eps

DB_exist = .true.


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

read(iu99,'(a)') rlx
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

read(iu99,'(a)',iostat=ioa) rl
multiple_steps=.true.
working_wavelength=zero
working_ttmax=160.d0
if (ioa==0) then
  rl=trim(adjustl(rl))
  lrl=len_trim(rl)
  call LOWCASE(rl(1:lrl))
  if (lrl>=3) then
    if (rl(1:3)=='one') then
      multiple_steps=.false.
      read(rl(4:lrl),*,iostat=ioa2) working_wavelength,working_ttmax
      if (ioa2/=0) then
        working_wavelength=wl_ourqmax
        working_ttmax=160.d0
      endif
    endif
  endif
  if (verbose) print*, 'working_wavelength,working_ttmax',working_wavelength,working_ttmax
else
  stop 'missing wavelength/ 2thetamax'
endif

mass_density_gcm3=zero
read(iu99,'(a)') rl
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

redmindis=zero
read(iu99,'(a)') rl
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


close(iu99)


print'(a,1x,f17.8)', ' Working Wavelength = ',working_wavelength
print'(a,1x,f17.8)', ' 2*Theta Max        = ',working_ttmax
print*, ' '
print'(a,1x,f17.8)', ' Reduced minimal distance = ',redmindis
print'(a,1x,f17.8)', ' Mass Density [g/cm^3]    = ',mass_density_gcm3
if (mass_density_gcm3<sceps_DP) print'(a,1x,f11.4,a)',' Mass Density [g/cm^3] will be recalculated '//&
        'based on a default number density of ',default_num_density_Am3,' atoms/Angstroem^3'


!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path

sampled_folder='DISTANCES'; lsampled_folder=9
basepath_sampled=inpath(1:linpath); lbasepath_sampled=linpath

!print*, working_wavelength
call DO_SETUP
!print*, working_wavelength


call read_XYZ_only(file_inp, redmindis)
if (verbose) then
  print*,Celty(1)%termcon
  print*,Celty(1)%termcon_all
  print*,Celty(1)%termcon_ineq
  print*,Celty(1)%summul
endif  

summul=zero
if (verbose) print*,'Start evaluating dstances...'
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
DO ipa=1,n_at_pair_V(1)
  if (verbose) print*,'Doing Pair: ',ipa,' [',Celty(1)%zappa(:,ipa),'] {', &
      Celty(1)%Z_at(Celty(1)%zappa(:,ipa)),'} (',Celty(1)%nat(Celty(1)%zappa(:,ipa)),')'
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  eqatpair=(is1==is2)
  if (eqatpair) then
    if (verbose) print*,ipa,is1,is2,izz1,izz2
  endif
  do ia1=1,Celty(1)%nat(is1)
    iini=1
    if (eqatpair) iini=ia1
    Taux=themolec(is1)%coo_xyzbo(1:3,ia1)
    ooo=two*themolec(is1)%coo_xyzbo(5,ia1)
    do ia2=iini,Celty(1)%nat(is2)
      Taux2=Taux-themolec(is2)%coo_xyzbo(1:3,ia2)
      ooo2=ooo*themolec(is2)%coo_xyzbo(5,ia2)
      if (eqatpair .and. ia1==ia2) then
        ooo2=half*ooo2
      endif
      !!19.12.2016  
     d00=sqrt(sum(Taux2**2))
      if (verbose) print*,d00,ooo2
! for adimensional db Celty(1)%minallowdist(ipa) must be reduced (default = 0.99 see FORSAMP.f90)
  !   if (d00<0.01d0) then
  if (d00<Celty(1)%minallowdist(ipa)) then
        Celty(1)%termcon_all(ipa)=Celty(1)%termcon_all(ipa)+ooo2
        if (d00>s4eps_DP .and. verbose) &
          print*,'Small D: ',ipa,is1,is2,izz1,izz2,ia1,ia2,d00,Celty(1)%minallowdist(ipa)
      else
        Celty(1)%summul(ipa) = Celty(1)%summul(ipa)+ooo2
        call SAM_ONE(d0=d00,mu0=ooo2,jpair=ipa)
      endif
    enddo
  enddo
ENDDO
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

call OUTSAMP(finumb=namestr(1:lnamestr)//'.smp',Bpre=zero)

!! FB cputime
 call CPU_TIME(t1)
  write(*,*)  '------->   CPU time = ',t1-t0,' s'

print*, ' '
print*, '******* JOB SMP SINGLE CLUSTER DONE! *******'

end program karn_evil_9
