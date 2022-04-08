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
module crycomp
use nano_deftyp
use ATOMIX
use Sultans_Of_Swing,only : DVds_calc

integer(I4B),parameter :: small_den=48, &
                          large_den=10000, &
                          simple_den=small_den*large_den, &
                          spad=3,half_SD=simple_den/2
real(DP),parameter     :: xmall_den=48.0_DP, &
                          ximple_den=480000.0_DP
real(DP),save          :: the_den
integer(I4B),save :: i_the_den
real(DP),parameter     :: min_prec=0.0001_DP
real(DP),parameter     :: default_B=0.5_DP,default_O=1.0_DP
!.208333333333333333333333333333333333e-5_DP

real(DP),save :: abcabg(6),mten(3,3),dvmat(3,3)
integer(I4B),save  :: nat,space_dim,ngrp,nat_tot,ll_pear,imode=0
character(len=8),save :: son_of_pear
character(len=1),save :: cut_cell = 'P', break_asu='y'
integer(I4B),allocatable,save :: atlab(:)
integer(I4B),allocatable,save  :: grm(:,:,:),g0(:,:),igrt(:,:)
real(DP),allocatable,save      :: grt(:,:)


real(DP),allocatable,save      :: Rincell_P(:,:)
character(len=2),allocatable,save  :: Aincell_P(:)
contains

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine READ_GROUP(f1,nrspg)
implicit none
character(len=*),intent(IN) :: f1
integer(I4B),intent(OUT) :: nrspg

integer(I4B) :: iu1,i,io

nrspg = 1
read(f1(15:17),*,iostat=io) nrspg
if (io/=0) print*,'Warning: unable to read space group number: defaulting to  1'

iu1=FIND_UNIT()
open(iu1,status='old',action='read',file=trim(path_SpaceGroups)//trim(adjustl(f1)))
read(iu1,*)
read(iu1,*)ngrp,space_dim
if (ALLOCATED(grt)) deallocate(grt)
if (ALLOCATED(igrt)) deallocate(igrt)
if (ALLOCATED(grm)) deallocate(grm)
if (ALLOCATED(g0)) deallocate(g0)
allocate(grt(space_dim,ngrp),grm(space_dim,space_dim,ngrp),g0(space_dim,space_dim),igrt(space_dim,ngrp))
! print*,' ngrp,space_dim = ',ngrp,space_dim
do i=1,ngrp
  read(iu1,*)g0
  grm(:,:,i)=transpose(g0)
  read(iu1,*)grt(:,i)
enddo
grt=REAL(NINT(144.0_DP*grt),DP)/144.0_DP
igrt=NINT(grt*the_den)
close(iu1)
end subroutine READ_GROUP
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine GETINTF(dimv,r,pr,irn)
implicit none
integer(I4B),intent(IN)   :: dimv
real(DP),intent(IN)       :: r(dimv),pr
integer(I4B),intent(OUT)  :: irn(dimv)
integer(I4B)  :: j,kk
real(DP)  :: rx0,prx,xkk

prx=max(pr,min_prec)
do j=1,dimv
!  xkk=xmall_den*r(j)
!  kk=nint(xkk)
!  rx0=abs(r(j)-real(kk,DP)/xmall_den)
!  if (rx0 < prx) then
!    irn(j)=kk*large_den
!  else
    kk=NINT(the_den*r(j))
    irn(j)=kk
!  endif
enddo
end subroutine GETINTF
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine DO_MTEN
implicit none
integer(I4B)  :: jng
real(DP)      :: coang(4:6),siang(4:6)

do jng=4,6
  if (abs(abcabg(jng)-90.0_DP) < s4eps_DP) then
    coang(jng)=0.0_DP
  else if (abs(abcabg(jng)-120.0_DP) < s4eps_DP) then
    coang(jng)=-.5_DP
  else if (abs(abcabg(jng)-135.0_DP) < s4eps_DP) then
    coang(jng)=-1.0_DP/sr2
  else
    coang(jng)=cos(Pi*abcabg(jng)/180.0_CP)
  endif
  siang=sqrt(max(zero,one-coang(jng)**2))
enddo
where(siang<sceps_DP) siang=zero; where(abs(coang)<sceps_DP) coang=zero
mten(1,1) = abcabg(1)*abcabg(1)
mten(1,2) = abcabg(1)*abcabg(2)*coang(6)
mten(1,3) = abcabg(1)*abcabg(3)*coang(5)
mten(2,1) = abcabg(2)*abcabg(1)*coang(6)
mten(2,2) = abcabg(2)*abcabg(2)
mten(2,3) = abcabg(2)*abcabg(3)*coang(4)
mten(3,1) = abcabg(3)*abcabg(1)*coang(5)
mten(3,2) = abcabg(3)*abcabg(2)*coang(4)
mten(3,3) = abcabg(3)*abcabg(3)
where(abs(mten)<sceps_DP) mten=zero

end subroutine DO_MTEN
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine constrP_output(iuout,psymw,abcabgw,natsp,natall,coocell_P,elcell_P,muat,ZZat)
implicit none
integer(I4B),intent(IN)                       :: iuout,natsp,natall
integer(I4B),dimension(natsp),intent(IN)      :: muat,ZZat
character(len=*),intent(IN)                   :: psymw
character(len=2),dimension(natall),intent(IN) :: elcell_P
real(DP), intent(IN)                          :: abcabgw(6),coocell_P(5,natall)
integer(I4B) :: i,lwlin,jll,jlm,ige
character(len=128) :: wlin

write(iuout,'(1x,a,6(1x,g14.8),i6)')trim(adjustl(psymw)),abcabgw,natsp
write(iuout,*)muat
write(iuout,*)ZZat
do i=1,natall
  write(wlin(1:128),'(a2,5(1x,f24.15),1x)')elcell_P(i)(1:2),coocell_P(:,i)
  do jll=127,3,-1
   if (wlin(jll:jll)=='0'.and.wlin(jll+1:jll+1)==' ') then
     ige=0
     do jlm=jll-1,jll-5
       if (wlin(jlm:jlm)=='E'.or.wlin(jlm:jlm)=='e') then
         ige=1
         exit
       endif
     enddo
     if (ige==0) wlin(jll:jll)=' '
   endif
 enddo
 write(iuout,'(a)')trim(adjustl(wlin))
enddo
close(iuout)

end subroutine constrP_output

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine constrP_output_xyz(iuout,charnam,natall,coocell_P,elcell_P)
implicit none
integer(I4B),intent(IN)                       :: iuout,natall
character(len=*),intent(IN)                   :: charnam
character(len=2),dimension(natall),intent(IN) :: elcell_P
real(DP), intent(IN)                          :: coocell_P(5,natall)
integer(I4B) :: i,lwlin,jll,jlm,ige
character(len=256) :: wlin
real(DP)  :: aux3(3),aux5(5),rrr

write(iuout,*)natall
write(iuout,'(a)')trim(adjustl(charnam))
do i=1,natall
  aux5=coocell_P(:,i)
  aux3=matmul(dvmat,aux5(1:3))
  aux5(1:3)=aux3
  rrr=sqrt(sum(aux3**2))
  where(ABS(aux5)<sceps_DP) aux5=zero
  write(wlin(1:111),'(a2,3(1x,f20.10),3(1x,f14.6),1x)')elcell_P(i)(1:2),aux5,rrr
  do jll=110,3,-1
    if (wlin(jll:jll)=='0'.and.wlin(jll+1:jll+1)==' ') then
      ige=0
      do jlm=jll-1,jll-5
        if (wlin(jlm:jlm)=='E'.or.wlin(jlm:jlm)=='e') then
          ige=1
          exit
        endif
      enddo
      if (ige==0) wlin(jll:jll)=' '
    endif
  enddo
  write(iuout,'(a)')trim(adjustl(wlin(1:111)))
enddo
close(iuout)

end subroutine constrP_output_xyz
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine CELL_ONION(celnm,pears,abcabgw,nat_sp,natall,coocell_P,elcell_P,muat,zats,DVds)
implicit none
character(len=*),intent(INOUT) :: celnm,pears
integer(I4B),intent(IN)                       :: nat_sp,natall
integer(I4B),dimension(nat_sp),intent(IN)      :: muat,zats
character(len=2),dimension(natall),intent(IN) :: elcell_P
real(DP), intent(IN)                          :: abcabgw(6),coocell_P(5,natall),DVds(3,3)
!_ Local
character(len=1024) :: rl
character(len=2) :: sye
integer(I4B),allocatable :: nvdv(:),mudv(:,:)
real(DP),allocatable :: cooats(:,:,:),obats(:,:,:),Bbbarr(:,:,:),vdv(:,:)
real(DP) :: DVrs(3,3),LVds(3),aux(3),xcv(6),occlast(nat_sp)
integer(I4B) :: NC1(3),iaux(3)
integer(I4B) :: muatC(nat_sp),kain(nat_sp),mxind(nat_sp)
integer(I4B) :: iu3,iu2,kk,isp,i1,i2,i3,jd,ja,muatM,mmm,NUCC,kcel,kat,i,Iflag,KKK,Ndym,Ndymm1, &
                iplaz,isin,jac,jj,lcelnm,mprec
real(DP) :: volcel,Delta_R,ddd,ddd0,okk,xfr,xl1,xl2,xlen

celnm=trim(adjustl(celnm))
lcelnm=len_trim(celnm)

iu2=find_unit()
open(iu2,status='replace',file=celnm(1:lcelnm))

iu3=find_unit()
open(iu3,status='replace',file=celnm(1:lcelnm)//'.xyz')

write(iu2,'(a,6(1x,g18.12),i4)') trim(adjustl(pears)), abcabgw, nat_sp
muatM=maxval(muat)
allocate(cooats(3,muatM,nat_sp),obats(2,muatM,nat_sp))
cooats=-999.d0; obats=-999.d0
kk=0
do isp=1,nat_sp
  do ja=1,muat(isp)
    kk=kk+1
    sye(1:2) = elcell_P(kk)(1:2)
    cooats(:,ja,isp)=coocell_P(1:3,kk)
    obats(:,ja,isp) =coocell_P(4:5,kk)
  enddo
enddo




DVrs=zero
DVrs(1,1)=one/DVds(1,1)
DVrs(2,2)=one/DVds(2,2)
DVrs(3,3)=one/DVds(3,3)
DVrs(2,1)=-DVrs(1,1)*DVrs(2,2)*DVds(1,2)
DVrs(3,1)= DVrs(1,1)*DVrs(2,2)*DVrs(3,3)*(DVds(1,2)*DVds(2,3)-DVds(1,3)*DVds(2,2))
DVrs(3,2)=-DVrs(3,3)*DVrs(2,2)*DVds(2,3)
do i=1,3
  LVds(i)=sqrt(sum(DVrs(i:3,i)**2))
enddo
volcel=DVds(1,1)*DVds(2,2)*DVds(3,3)
Delta_R=exp(unter*log( volcel*0.75d0/pi ))

NC1 = 1+ceiling((half+two)*Delta_R*LVds)
NUCC = product(2*NC1+1)
print*,NC1,NUCC
muatC=muat*NUCC
mmm=maxval(muatC)
if (ALLOCATED(Bbbarr)) deallocate(Bbbarr)
allocate(Bbbarr(6,mmm,nat_sp),vdv(mmm,nat_sp),nvdv(nat_sp),mudv(mmm,nat_sp))
Bbbarr=-999.d0
do isp=1,nat_sp
  kcel=0
  kat=0
  do i1=-NC1(1),NC1(1)
    iaux(1)=i1
    do i2=-NC1(2),NC1(2)
      iaux(2)=i2
      do i3=-NC1(3),NC1(3)
        iaux(3)=i3
        kcel=kcel+1
        do ja=1,muat(isp)
          jac=ja+muat(isp)*(kcel-1)
          kat=kat+1
          aux=matmul(DVds,iaux+cooats(:,ja,isp))
          xlen=sqrt(sum(aux**2))
          Bbbarr(1:3,kat,isp) = real(iaux,DP)+cooats(:,ja,isp)
          Bbbarr(4:5,kat,isp) = obats(:,ja,isp)
          Bbbarr(6,kat,isp) = xlen
        enddo
      enddo
    enddo
  enddo
!  print*,'Fill: ',isp,kat,muatC(isp),minval(Bbbarr(6,1:kat,isp)),sum(Bbbarr(6,1:kat,isp))/kat,maxval(Bbbarr(6,1:kat,isp)), &
!                  count(Bbbarr(6,1:kat,isp)<sceps_DP)
enddo
do isp=1,nat_sp
  KKK=muatC(isp)
!  print*,kkk,muat(isp),mmm,size(Bbbarr,2)
  Iflag=1
  DO
    IF (Iflag == 0) exit
    Ndym=KKK
    Iflag=0
    Ndymm1=Ndym-1
    DO I=1,Ndymm1
      xl1=Bbbarr(6,I,isp)
      xl2=Bbbarr(6,I+1,isp)
      
      IF (xl1>xl2+sceps_DP) THEN
        xcv=Bbbarr(:,I,isp)
        Bbbarr(:,I,isp)=Bbbarr(:,I+1,isp)
        Bbbarr(:,I+1,isp)=xcv
        KKK=I
        Iflag=1
      ENDIF
    ENDDO
  ENDDO
  kat=muatC(isp)
!  print*,'Sort: ',isp,kat,muatC(isp),minval(Bbbarr(6,1:kat,isp)),sum(Bbbarr(6,1:kat,isp))/kat,maxval(Bbbarr(6,1:kat,isp)),&
!                  count(Bbbarr(6,1:kat,isp)<sceps_DP)
enddo
kain=0
occlast=zero
nvdv=0
mudv=0
vdv=-one
mxind=0
do isp=1,nat_sp
  do ja=1,muatC(isp)
    ddd=Bbbarr(6,ja,isp)
    isin=0
    iplaz=0
    do jj=1,nvdv(isp)
      if (abs(ddd-vdv(jj,isp))<sceps_DP) then
        isin=1
        iplaz=jj
        exit
      endif
    enddo
    if (isin==0) then
      nvdv(isp)=nvdv(isp)+1
      vdv(nvdv(isp),isp)=ddd
      mudv(nvdv(isp),isp)=1
    else
      mudv(iplaz,isp)=mudv(iplaz,isp)+1
    endif
  enddo
  kain(isp)=0
  do jj=1,nvdv(isp)
    mprec=kain(isp)
    kain(isp) = mprec + mudv(jj,isp)
    if (kain(isp)==muat(isp)) then
      mxind(isp)=jj
      occlast(isp)=one
      exit
    else if (kain(isp)>muat(isp)) then
      xfr = REAL(muat(isp)-mprec,DP)/REAL(mudv(jj,isp),DP)
      mxind(isp)=jj
      occlast(isp)=xfr
      exit
    endif
  enddo
enddo
write(iu2,*) kain
write(iu2,*) zats
write(iu3,*) sum(kain)
write(iu3,'(a)')celnm(1:lcelnm)//' - '//trim(adjustl(pears))

!do isp=1,nat_sp
!  do jd=1,mxind(isp)
!    print*,isp,jd,mudv(jd,isp),vdv(jd,isp)
!  enddo
!enddo
do isp=1,nat_sp
  sye=symb_of_Z(zats(isp))
  do jd=1,mxind(isp)
    ddd0=vdv(jd,isp)
    okk=one
    if (jd==mxind(isp)) okk = occlast(isp)
    do ja=1,muatC(isp)
      if (abs(ddd0-Bbbarr(6,ja,isp))<sceps_DP) then
        aux=matmul(DVds,Bbbarr(1:3,ja,isp))
        write(iu2,'(a2,3(1x,f16.8),2(1x,f14.6))')sye,Bbbarr(1:3,ja,isp),Bbbarr(4,ja,isp)*okk,Bbbarr(5,ja,isp)
        write(iu3,'(a2,3(1x,f16.8),3(1x,f14.6))')sye,aux,Bbbarr(4,ja,isp)*okk,Bbbarr(5,ja,isp),Bbbarr(6,ja,isp)
      endif
    enddo
  enddo
enddo
close(iu2);close(iu3)
deallocate(cooats,obats,Bbbarr,vdv,nvdv,mudv)
end subroutine CELL_ONION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

end module crycomp
!_______________________________________________________________________________
program mkcell
use crycomp
use atomix
implicit integer(I4B)(i-n),real(CP)(a-h,o-z)

integer(I4B),parameter  :: nsecle=4
real(CP) :: auxr(3)
integer(I4B)  :: auxi(3)
real(CP),allocatable     :: xyzbA(:,:),xyzbC(:,:)
character(2),dimension(:),allocatable :: namel
real(CP)  :: oricell(3)=zero,cm_asy(3)=zero
integer(I4B)  :: ioricell(3)=0,icm_asy(3)=0, num_species_in, flag_species_in
character(2)  :: oneat,oneat2
character(1)  :: uffa
integer(I4B),dimension(:,:),allocatable :: pattas
integer(I4B),allocatable  :: coo_int(:),mu_at(:),zz_at(:),toasy(:), &
                             patt_pairs(:,:),mu_at1(:),zz_at1(:)
integer(I4B),allocatable  :: pattv(:,:),ive1(:),ive2(:),orb1a(:,:)
integer(I4B),allocatable  :: atsort(:,:),fnd(:),mulp(:)
character(512) :: f1,f2,f3,f4,healin,tit,lollo,line_ori
character(64) :: torsym
character(10) :: finafil
character(4)  :: sg_char
character(2)  :: set
character(17) :: chfloat='1234567890.-+eEdD'
integer(I4B)  :: lchfloat=17
character(13) :: chfloat_S='1234567890.-+'
character(len=1),dimension(nsecle) :: secle=['c','2','R','$']
integer(I4B)  :: lchfloat_S=13, firstnumbp, isle
logical :: ini_file_exists, verbose_all

! after line 798 do the option "whole asu"

call def_eps


verbose_all = .false.

print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_CELL Program    '
print*, ' '

Iatom_origin=0
finafil='mkcell.ini'
INQUIRE(FILE=finafil, EXIST=ini_file_exists)
if (ini_file_exists) then
  iux=find_unit()
  open(iux,status='old',file=finafil,action='read')
  
  read(iux,'(a)')f2
  f2=trim(adjustl(f2))
  lline=len_trim(adjustl(f2))
  print'("  Reading .pha file from mkcell.ini : ",a)',f2(1:lline)

  read(iux,'(a)')f1
  f1=trim(adjustl(f1))
  lline=len_trim(adjustl(f1))
  print'("  Reading .grp file from mkcell.ini : ",a)',f1(1:lline)
  
  read(iux,*,iostat=iox)num_species_in     !flag_species_in
  if (iox/=0) then
    print*,'ERROR! Number of species in mkcell.ini is missed. Program stops '
    stop 'ERROR! Number of species in mkcell.ini is missed. Program stops '
  endif
  
  read(iux,'(a)')line_ori
  line_ori=trim(adjustl(line_ori))
  lline=len_trim(line_ori)
  read(line_ori(1:lline),*,iostat=iox) oricell
  if (iox/=0) then
    if (line_ori(1:5)=='Atom0') then
      read(line_ori(6:lline),*) Iatom_origin
    endif
  endif
!___________ defaults
  cut_cell = 'P'
  son_of_pear='none'
  break_asu='y'
  ll_pear=4
  klp=0
  KASHMIR:do
!________ Try to read Pearson symbol and cell cutout mode from following lines
    if (klp>=2) exit KASHMIR
    read(iux,'(a)',iostat=iox)lollo
    if (iox/=0) exit KASHMIR
    lollo=trim(adjustl(lollo))
    llollo=len_trim(lollo)
    if (llollo<3) cycle KASHMIR
    if (llollo>=1.and.ANY(lollo(1:1)==['#','!','>'])) cycle KASHMIR
    klp=klp+1
    if (llollo<=5.and.llollo>=3) then
      son_of_pear=lollo(1:llollo)
      ll_pear=llollo
    else
      if (llollo>5 .and. lollo(1:6) == 'constr') then
        RUMBLE_ON:do i=7,llollo
          if (lollo(i:i)=='S'.or.lollo(i:i)=='P') then
            cut_cell = lollo(i:i)
            exit RUMBLE_ON
          endif
        enddo RUMBLE_ON
      endif
      if (llollo>4 .and. lollo(1:5) == 'break') then
        MOBY_DICK:do i=7,llollo
          if (lollo(i:i)=='y'.or.lollo(i:i)=='n') then
            break_asu = lollo(i:i)
            exit MOBY_DICK
          endif
        enddo MOBY_DICK
      endif
    endif
  enddo KASHMIR
  close(iux)
  

else
  print*,' Could not find file mkcell.ini !'
  print*,' Please, supply the required files & Info (see manual!):'
  print*,' >>    File .pha : '
  read(*,'(a)') f2
  f2=trim(adjustl(f2))
  print*,' >>    File .grp : '
  read(*,'(a)')f1
  f1=trim(adjustl(f1))
  lf1 = len_trim(f1)
  if (lf1>8 .and. f1(1:8)=='SPG_grp'//separator) then
    continue
  else
    f1='SPG_grp'//separator//f1(1:lf1)
    lf1=lf1+8
  endif
  print*,' >>    Pearson symbol: '
  read(*,'(a)',iostat=iox)son_of_pear
  if (iox==0) then
    son_of_pear=trim(adjustl(son_of_pear))
    ll_pear=len_trim(son_of_pear)
  endif
  if (iox/=0.or.ll_pear==0) son_of_pear='none'
  print*,' >>    Cell origin [frac. coord. ]: '
  read(*,'(a)',iostat=iox)line_ori
  line_ori=trim(adjustl(line_ori))
  lline=len_trim(line_ori)
  
  read(line_ori(1:lline),*,iostat=iox2)oricell
  if (iox2/=0) then
    oricell=zero
    ioricell=0
    read(line_ori(1:lline),*,iostat=iox3)Iatom_origin
    if (iox3/=0) then
      oricell=zero
      ioricell=0
    endif
  endif
endif
ll_pear = len_trim(son_of_pear)



!_____________________________________________ Input close



iu2=FIND_UNIT()
open(iu2,status='old',action='read',file=trim(f2))

i=0
nuh=3
!_____ COUNT header
!_____ three header lines, maybe ONE comment line, perhaps some blank lines
do 
  read(iu2,'(a)')healin
  IF (verbose_all) print*,' >>    READ>> ',healin
  call clean_line(healin)
  call lowcase(healin)
  healin=trim(adjustl(healin))
  call clean_line(healin)
  call lowcase(healin)
  llh=len_trim(healin)
  if (llh==0) then
!______ ALLOW blank line
    nuh=nuh+1 
    cycle
  endif
  if (healin(1:1)=='>'.or.healin(1:1)=='#'.or.healin(1:1)=='!'.or.healin(1:1)=='%') then
!______ ALLOW comment line
    nuh=nuh+1 
    cycle
  endif
  i=i+1
  if (healin(1:4)=='cell') read(healin(5:llh),*)abcabg
  if (i==3) exit
enddo
IF (verbose) print*, '  '
!IF (verbose) print'(a15,6(1x,g15.8))','  >>    cell : ',abcabg
print'(a6)','  >>  '
print'(a26,6(1x,g15.8))','  >>   Cell parameters    : ',abcabg
IF (verbose) print*, '  '




! Count atoms
k=0
do
  read(iu2,'(a)',end=1,err=1)healin
  call clean_line(healin)
  call lowcase(healin)
  healin=trim(adjustl(healin))
  call clean_line(healin)
  call lowcase(healin)
  llh=len_trim(healin)
  if (llh==0) cycle
  if (healin(1:1)=='>'.or.healin(1:1)=='#'.or.healin(1:1)=='!'.or.healin(1:1)=='%') cycle
  IF (verbose_all) print*,' >>    DO_COUNT : ',trim(healin)
  k=k+1
enddo
1 rewind(iu2)
do iuh=1,nuh
  read(iu2,*)
enddo
nat=k
the_den = real(nat,DP) * ximple_den
i_the_den = nint(the_den)

ioricell=nint(the_den*oricell)
ioricell=modulo(ioricell,i_the_den)
oricell=real(ioricell,DP)/the_den
where(oricell<sceps_DP) oricell=zero


call DO_MTEN

call READ_GROUP(trim(f1),number_spg)
if (ll_pear==4.and.son_of_pear(1:ll_pear)=='none') then
  if (len_trim(f1)==21) then
    write(son_of_pear(1:ll_pear),'("#",i3.3)') number_spg
  else if (len_trim(f1)==24) then
    if (ANY(secle==f1(20:20))) then
      write(son_of_pear(1:ll_pear),'("$",i3.3)') number_spg
    else
      print*, 'MK_CELL: bad space group filename = '//trim(f1)
      stop 'MK_CELL: bad space group filename'
    endif
  endif
endif

!! May2017 building SG code
f1=trim(adjustl(f1))
lf1=len_trim(f1)
lenght:  select case(lf1)
    case (21) lenght
      sg_char(1:1)='#'
    case (24) lenght
     ii=index(f1(1:lf1), '_', .true.)
     read(f1(ii+1:ii+2), '(a2)') set 
      if (set(1:1)=='o') then 
         sg_char(1:1)=set(2:2)
      else 
      if (set(1:1)=='U') then 
            if (set(2:2)=='b') sg_char(1:1)='1'
            if (set(2:2)=='c') sg_char(1:1)='2'
        else
        if (set(1:1)=='s') then  
             if (set(2:2)=='H') sg_char(1:1)='1'
             if (set(2:2)=='R') sg_char(1:1)='2'
        else
          sg_char(1:1)='#'  
       endif
      endif
     endif       
    case default lenght
    sg_char(1:1)='#'
 end select  lenght

write(sg_char(2:4),'(i3.3)') number_spg

!!

flag_species_in = 0
IF (nat /= num_species_in) flag_species_in = 1

imode=0
if (flag_species_in>0) then
  imode=1
  allocate(atlab(nat))
  atlab=[(i,i=1,nat)]
endif
IF (verbose_all) print*,'>> imode, flag_species_in, nat, num_species_in ',imode, flag_species_in, nat, num_species_in
allocate(mu_at(nat),zz_at(nat),xyzbA(space_dim+2,nat), &
         xyzbC(space_dim,nat*ngrp),namel(nat),toasy(nat*ngrp), &
         ive1(space_dim),ive2(space_dim), &
         atsort(nat*ngrp,100),fnd(nat*ngrp), &
         orb1a(space_dim,ngrp),coo_int(space_dim))
IF (verbose) print*,' >>   # of atoms, # of species = ',nat, num_species_in
IF (verbose) print*, '  '

namel=''

k=0
do
  read(iu2,'(a)',err=22,end=22)healin
  call clean_line(healin)
  call lowcase(healin)
  healin=trim(adjustl(healin))
  call clean_line(healin)
  call lowcase(healin)
  llh=len_trim(healin)
  if (llh==0) cycle
  if (healin(1:1)=='>'.or.healin(1:1)=='#'.or.healin(1:1)=='!'.or.healin(1:1)=='%') cycle
  k=k+1
  print*,' >>   ',trim(healin)
  IF (verbose_all) print*,'  '
  firstnumbp=0
  ! find first number position
  ATLINE:do ii=1,llh
    uffa=healin(ii:ii)
    isfirstnumb=0
    do jj=1,lchfloat_S
      if (uffa(1:1)==chfloat_S(jj:jj)) then
        isfirstnumb=1
        firstnumbp=ii
        exit ATLINE
      endif
    enddo
  enddo ATLINE
  torsym=TRIM(ADJUSTL(healin(1:firstnumbp-1)))
!  print'(a,i3,a)','Atomline ',k,' <'//trim(torsym)//'> '
  ltorsym=len_trim(torsym)
  if (ltorsym==1) then
    namel(k)(1:1)=torsym(1:1)
    namel(k)(2:2)=' '
  else if (ltorsym==2) then
    namel(k)(1:2)=torsym(1:2)
  else
    iib=max( 1+INDEX(torsym(1:ltorsym),' ',.true.), ltorsym - 1 )
    namel(k)(1:ltorsym-iib+1)=torsym(iib:ltorsym)
!    print'(a,a)',' <'//torsym(iib:ltorsym)//'> ',' <'//namel(k)(1:ltorsym-iib+1)//'> '
    if (ltorsym-iib+1 == 1) namel(k)(2:2)=' '
  endif
  
!  kch=0
!  do ii=1,llh
!    uffa=healin(ii:ii)
!    isch=0
!    do jj=1,26
!      if (uffa(1:1)==UPCA(jj:jj).or.uffa(1:1)==LOCA(jj:jj)) then
!        isch=1
!        exit
!      endif
!    enddo
!    if (isch==1) kch=ii
!    
!  enddo
!
!  namel(k)(1:2) = healin(kch-1:kch)
!  if (namel(k)(1:1)==' ') then
!    namel(k)(1:1)=namel(k)(2:2)
!    namel(k)(2:2)=' '
!  endif
  uffa=namel(k)(1:1)
  do jj=1,26
    if (uffa(1:1)==LOCA(jj:jj)) then
      namel(k)(1:1)=UPCA(jj:jj)
      exit
    endif
  enddo
  
  IF (verbose_all) then
    print'(i4,a)',k," : element <"//namel(k)(1:2)//">"
!    print'(a)',healin(firstnumbp:llh)
  endif
  kch=firstnumbp-1
  if (imode==0) then

    read(healin(kch+1:llh),*,iostat=io000)ignor,xyzbA(:,k)
    if (io000/=0) then
      read(healin(kch+1:llh),*,iostat=io1)xyzbA(:,k)
      if (io1/=0) then
        read(healin(kch+1:llh),*,iostat=io2)xyzbA(:4,k)
        xyzbA(5,k)=default_O
        if (io2/=0) then
          read(healin(kch+1:llh),*,iostat=io3)xyzbA(:3,k)
          if (io3/=0) then
            print'(a,2i8)','At line:',kch+1,llh
            print'(a)',healin(kch+1:llh)
            stop 'trouble reading atoms mode 0'
          endif
          xyzbA(4,k)=default_B
          xyzbA(5,k)=default_O
        endif
      endif
    endif
    
  else if (imode==1) then
  
    read(healin(kch+1:llh),*,iostat=io1)atlab(k),xyzbA(:,k)
    if (io1/=0) then
      read(healin(kch+1:llh),*,iostat=io2)atlab(k),xyzbA(:4,k)
      xyzbA(5,k)=default_O
      if (io2/=0) then
        read(healin(kch+1:llh),*,iostat=io3)atlab(k),xyzbA(:3,k)
        if (io3/=0) then
          print'(a,2i8)','At line:',kch+1,llh
          print'(a)',healin(kch+1:llh)
          stop 'trouble reading atoms mode 1'
        endif
        xyzbA(4,k)=default_B
        xyzbA(5,k)=default_O
      endif
    endif
  
  endif
  if (Iatom_origin==k) then
    oricell = xyzbA(:3,k)
    ioricell=nint(the_den*oricell)
    ioricell=modulo(ioricell,i_the_den)
    oricell=real(ioricell,DP)/the_den
    where(oricell<sceps_DP) oricell=zero
  endif
IF (verbose_all) print'(i4,3(1x,g14.8))',k,xyzbA(:3,k)
  if (k==nat) exit
enddo
22 close(iu2)
precx=min_prec


if (imode==1) then
  num_species_in=maxval(atlab)
!____________ Consistency check 
  do j=1,num_species_in
    kj=count(atlab==j)
    if (kj==0) then
      print*,'Atomic species ',j, 'has no representative in the .pha file. Please edit and rerun.'
      stop 'Missing atomic species... stopping'
    endif
  enddo
endif


!____________ Consistency check done
ALLOCATE(mu_at1(num_species_in),zz_at1(num_species_in))

do k=1,nat
  call GETINTF(space_dim,xyzbA(1:space_dim,k),precx,coo_int)
  xyzbA(1:space_dim,k)=real(coo_int,DP)/the_den
  IF (verbose_all) print'(i4,3(1x,g14.8))',k,xyzbA(:3,k)
enddo
dvmat = DVds_calc(abcabg)
where(abs(dvmat)<100.d0*eps_DP) dvmat=zero
! print*,'DVds : '
IF (verbose) print*,'Crystal-to-Orthonormal Coordinates Matrix : '
IF (verbose) print'(3(1x,g22.15))',dvmat(1,:)
IF (verbose) print'(3(1x,g22.15))',dvmat(2,:)
IF (verbose) print'(3(1x,g22.15))',dvmat(3,:)

lf2=len_trim(f2)
f3 = f2(:lf2-4)//'.cel'

t=s4eps_DP
kall=0
mu_at=0
toasy=0
do iat=1,nat
  cm_asy=cm_asy+xyzbA(1:3,iat)
enddo
cm_asy=cm_asy/real(nat,DP)
icm_asy=nint(cm_asy*the_den)

do iat=1,nat
  o0 = xyzbA(5,iat)
  b0 = xyzbA(4,iat)
  ive1=nint(xyzbA(1:3,iat)*the_den)
  norb1a=0
  do ig=1,ngrp
    ive2=matmul(grm(:,:,ig),ive1)+igrt(:,ig) - ioricell
    ive2=modulo(ive2,i_the_den)
    keep=1
    ckeq:do ior=1,norb1a
      ixx=maxval(abs(ive2-orb1a(:,ior)))
      IF (ixx==0) then
        keep=0
        EXIT ckeq
      endif
    enddo ckeq
    IF (keep==1) then
      norb1a=norb1a+1
      orb1a(:,norb1a)=ive2
    endif
  enddo
  mu_at(iat) = norb1a
  do ior=1,norb1a
    kall=kall+1
    toasy(kall) = iat
    xyzbC(:,kall) = REAL(orb1a(:,ior),DP)/the_den
  enddo
enddo
nat_tot=kall
do iat=1,nat
  lla=len_trim(namel(iat)(1:2))
  if (lla==1) namel(iat)(2:2)=' '
  ZZ_at(iat)=0
  do js=0,n_elements
    if (symb_of_Z(js)(1:2)==namel(iat)(1:2)) then
      ZZ_at(iat)=js
      exit
    endif
  enddo
  if (ZZ_at(iat)==0) print'(a,i2,a,a2)','Trouble assigning Z at atom ',iat,' named ',namel(iat)(1:2)
enddo
nat1=nat
if (imode==1) nat1=num_species_in

!_____ Prepare output for construction "P"
 
if (imode==0) then
  if (allocated(Rincell_P)) deallocate(Rincell_P)
  if (allocated(Aincell_P)) deallocate(Aincell_P)
  allocate(Rincell_P(5,nat_tot),Aincell_P(nat_tot))
  do iat=1,nat_tot
    iata=toasy(iat)
    Aincell_P(iat)(1:2) = namel(iata)(1:2)
    Rincell_P(1:3,iat) = xyzbC(:,iat); Rincell_P(4,iat) = xyzbA(5,iata); Rincell_P(5,iat) = xyzbA(4,iata)
  enddo

  if (cut_cell=='P') then
    iu3=find_unit()
    open(iu3,status='replace',file=trim(f3))
    call constrP_output(iu3,sg_char(1:4),abcabg(1:6),nat,nat_tot,Rincell_P,Aincell_P,mu_at,ZZ_at)
    !call constrP_output(iu3,son_of_pear(1:ll_pear),abcabg(1:6),nat,nat_tot,Rincell_P,Aincell_P,mu_at,ZZ_at)
    iu3=find_unit()
    open(iu3,status='replace',file=trim(f3)//'.xyz')
    call constrP_output_xyz(iu3,trim(f3)//' - '//son_of_pear(1:ll_pear),nat_tot,Rincell_P,Aincell_P)
  else
    call CELL_ONION(f3,sg_char(1:4),abcabg,nat,nat_tot,Rincell_P,Aincell_P,mu_at,ZZ_at,dvmat)
    !call CELL_ONION(f3,son_of_pear(1:ll_pear),abcabg,nat,nat_tot,Rincell_P,Aincell_P,mu_at,ZZ_at,dvmat)
  endif
  
else if (imode==1) then
  mu_at1=-999
  zz_at1=-999
  do j=1,num_species_in
    mu_at1(j)=0
    zz_at1(j)=-1
    do k=1,nat
      if (atlab(k)==j) then
        mu_at1(j)=mu_at1(j)+mu_at(k)
        izz=zz_at1(j)
        if (izz==-1) then
          zz_at1(j)=zz_at(k)
        else
          if (zz_at1(j)/=zz_at(k)) then
            print*,'ERROR: atoms with different Z are labeled as equal!!! Please edit the .pha file.'
            stop 'ERROR: atoms with different Z are labeled as equal!!! Please edit the .pha file.'
          endif
        endif
      endif
    enddo
  enddo
  if (allocated(Rincell_P)) deallocate(Rincell_P)
  if (allocated(Aincell_P)) deallocate(Aincell_P)
  allocate(Rincell_P(5,nat_tot),Aincell_P(nat_tot))
  
  do iat0=1,num_species_in
    do iat1=1,nat_tot
      iata=toasy(iat1)
      if (atlab(iata)/=iat0) cycle
      Aincell_P(iat1)(1:2) = namel(iata)(1:2)
      Rincell_P(1:3,iat1) = xyzbC(:,iat1); Rincell_P(4,iat1) = xyzbA(5,iata); Rincell_P(5,iat1) = xyzbA(4,iata)
    enddo
  enddo
  
  if (cut_cell=='P') then
    iu3=find_unit()
    open(iu3,status='replace',file=trim(f3))
    call constrP_output(iu3,sg_char(1:4),abcabg(1:6),num_species_in,nat_tot,Rincell_P,Aincell_P,mu_at1,ZZ_at1)
   ! call constrP_output(iu3,son_of_pear(1:ll_pear),abcabg(1:6),num_species_in,nat_tot,Rincell_P,Aincell_P,mu_at1,ZZ_at1)
    iu3=find_unit()
    open(iu3,status='replace',file=trim(f3)//'.xyz')
    call constrP_output_xyz(iu3,trim(f3)//' - '//son_of_pear(1:ll_pear),nat_tot,Rincell_P,Aincell_P)
  else
    call CELL_ONION(f3,sg_char(1:4),abcabg,num_species_in,nat_tot,Rincell_P,Aincell_P,mu_at1,ZZ_at1,dvmat)
  !  call CELL_ONION(f3,son_of_pear(1:ll_pear),abcabg,num_species_in,nat_tot,Rincell_P,Aincell_P,mu_at1,ZZ_at1,dvmat)
  endif
  
endif
close(iu3)

print*, '  '
print*, '******* JOB CELL DONE! *******'

end program mkcell
