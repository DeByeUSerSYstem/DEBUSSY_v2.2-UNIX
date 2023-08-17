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
! X-ray scatterig factors: D. Waasmaier and A. Kirfel, Acta Cryst. A51 (1995) 416--431
! or @techreport{EPDL97,
!	institution = {Lawrence Livermore National Laboratory, Livermore, CA, USA},
!	author = {Dermott E. Cullen and John H. Hubbell and Lynn Kissel},
!	number = {UCRL-50400, Vol.~6, Rev.~5},
!	title = {{EPDL97: the Evaluated Photon Data Library, '97 Version}},
!	year =  {September 1997}
!}
! neutron scattering lengths: V.F. Sears, Neutron News 3/3 (1992) 29--37
! electron scattering factors: Mott-Bethe's formula
module Compton_Anom_Xray
use nano_deftyp
integer(I4B),parameter :: n_elements_EPDL97=100
 TYPE,PUBLIC     :: Xray_inel
! atom's S_incoh, F_coh, fpr,fdpr from EPDL97
     integer(I4B)   :: Zatom,isfilled=0
     integer(I4B)   :: N_S_incoh,N_F_coh,N_Fpr,N_Fdpr
     REAL(DP),DIMENSION(:),allocatable       :: q_S_incoh,q_F_coh, E_Fpr, E_Fdpr  ! scale of gaussian terms
     REAL(DP),DIMENSION(:),allocatable       :: S_incoh, F_coh, Fpr, Fdpr 
 END TYPE Xray_inel
 
 type(Xray_Inel),dimension(0:n_elements_EPDL97),save :: Xray_EPDL97
 
! character(512)  :: path_EPDL97=''

 integer(I4B),dimension(0:n_elements_EPDL97,4),save  :: dimarr_EPDL97=0
 logical,save  :: dimarr_read=.false.
 character(2),dimension(0:n_elements_EPDL97),save :: symb_of_Z1
 DATA symb_of_Z1(0:0)/'Zz'/
 DATA symb_of_Z1(1:2)/'H ','He'/
 DATA symb_of_Z1(3:10)/'Li','Be','B ','C ','N ','O ','F ','Ne'/
 DATA symb_of_Z1(11:18)/'Na','Mg','Al','Si','P ','S ','Cl','Ar'/
 DATA symb_of_Z1(19:36)/'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr'/
 DATA symb_of_Z1(37:54)/'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe'/
 DATA symb_of_Z1(55:86)/'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ', &
                       'Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn'/
 DATA symb_of_Z1(87:n_elements_EPDL97)/'Fr','Ra','Ac','Th','Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
 
! include 'EPDL97.inc'
 
 INTERFACE COMP_COMPTON_S
   MODULE PROCEDURE ComptonS_V, ComptonS_S
 END INTERFACE COMP_COMPTON_S

 INTERFACE FormFact_EPDL97
   MODULE PROCEDURE FormFactEPDL97_V, FormFactEPDL97_S
 END INTERFACE FormFact_EPDL97

  CONTAINS


!*****************************************************************************************************
 subroutine read_EPDL97_dimarr()
  IMPLICIT NONE
  INTEGER(I4B)                 :: i,iuEPDL97_NV,ii
  
  iuEPDL97_NV=find_unit()
  do i=1,len_trim(path_EPDL97)
    if (path_EPDL97(i:i)==separator) then
      path_EPDL97(i:i) = separator(1:1)
    endif
  enddo
  open(iuEPDL97_NV,status='old',action='read',file=trim(path_EPDL97)//'EPDL97_NV.dat')
  do i=1,n_elements_EPDL97
    read(iuEPDL97_NV, *)ii,dimarr_EPDL97(i,:)
  enddo
  close(iuEPDL97_NV)
  dimarr_read=.true.
  
 end subroutine read_EPDL97_dimarr
!*****************************************************************************************************
 subroutine read_EPDL97_files(Z_e)
  IMPLICIT NONE
  INTEGER(I4B),intent(IN)  :: Z_e
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j
  REAL(DP)                     :: qhs, Z0, x,y
  character(len=80),dimension(4) :: fn

  if (.not.dimarr_read) call read_EPDL97_dimarr()

  Z_el=Z_e
!  print*,Z_el,Z_e
  if (Z_el<0) then
    print*, 'Fatal Error: called read_EPDL97_files with Unknown element ',Z_e
    STOP 'Fatal Error: called read_EPDL97_files with Unknown element '
  ENDIF
  
  if (Z_el == 0) then
    print*,'Z_el = 0'
    Xray_EPDL97(Z_el)%Zatom=0
    Xray_EPDL97(Z_el)%isfilled=1
    Xray_EPDL97(Z_el)%N_F_coh=0
    Xray_EPDL97(Z_el)%N_S_incoh=0
    Xray_EPDL97(Z_el)%N_Fpr=0
    Xray_EPDL97(Z_el)%N_Fdpr=0
    allocate(Xray_EPDL97(Z_el)%q_S_incoh(0), Xray_EPDL97(Z_el)%q_F_coh(0), &
             Xray_EPDL97(Z_el)%E_Fpr(0), Xray_EPDL97(Z_el)%E_Fdpr(0), Xray_EPDL97(Z_el)%S_incoh(0), &
             Xray_EPDL97(Z_el)%F_coh(0), Xray_EPDL97(Z_el)%Fpr(0), Xray_EPDL97(Z_el)%Fdpr(0))
    return
  endif

  fn(1)='Z_000_F_coh_EPDL97.dat'
  fn(2)='Z_000_S_incoh_EPDL97.dat'
  fn(3)='Z_000_Fdpr_anom_EPDL97.dat'
  fn(4)='Z_000_Fpr_anom_EPDL97.dat'
  do j=1,4
    lfn(j)=len_trim(trim(adjustl(fn(j)(:))))
  enddo
  iwz=index(fn(1)(1:lfn(1)),'000')
  do j=1,4
    write(fn(j)(iwz:iwz+2),'(i3.3)') Z_el
  enddo

  Xray_EPDL97(Z_el)%Zatom=Z_el
  Xray_EPDL97(Z_el)%isfilled=1
  if (.not.dimarr_read) call read_EPDL97_dimarr()
    Xray_EPDL97(Z_el)%N_F_coh  = dimarr_EPDL97(Z_el,1)
    Xray_EPDL97(Z_el)%N_S_incoh= dimarr_EPDL97(Z_el,2)
    Xray_EPDL97(Z_el)%N_Fdpr   = dimarr_EPDL97(Z_el,3)
    Xray_EPDL97(Z_el)%N_Fpr    = dimarr_EPDL97(Z_el,4)
    allocate(Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh), &
             Xray_EPDL97(Z_el)%S_incoh(Xray_EPDL97(Z_el)%N_S_incoh), &
             Xray_EPDL97(Z_el)%q_F_coh(Xray_EPDL97(Z_el)%N_F_coh), &
             Xray_EPDL97(Z_el)%F_coh(Xray_EPDL97(Z_el)%N_F_coh), &
             Xray_EPDL97(Z_el)%E_Fpr(Xray_EPDL97(Z_el)%N_Fpr), &
             Xray_EPDL97(Z_el)%Fpr(Xray_EPDL97(Z_el)%N_Fpr), &
             Xray_EPDL97(Z_el)%E_Fdpr(Xray_EPDL97(Z_el)%N_Fdpr), &
             Xray_EPDL97(Z_el)%Fdpr(Xray_EPDL97(Z_el)%N_Fdpr))
  
  do j=1,4
    iuEPDL97=find_unit()
    open(iuEPDL97,status='old',action='read',file=trim(path_EPDL97)//trim(fn(j)(:)))
    do i=1,dimarr_EPDL97(Z_el,j)
      read(iuEPDL97,*) x,y
      if (j==1) then
        Xray_EPDL97(Z_el)%q_F_coh(i) = x
        Xray_EPDL97(Z_el)%F_coh(i)   = y
      else if (j==2) then
        Xray_EPDL97(Z_el)%q_S_incoh(i) = x
        Xray_EPDL97(Z_el)%S_incoh(i)   = y
      else if (j==3) then
        Xray_EPDL97(Z_el)%E_Fdpr(i) = x
        Xray_EPDL97(Z_el)%Fdpr(i)   = y
      else if (j==4) then
        Xray_EPDL97(Z_el)%E_Fpr(i) = x
        Xray_EPDL97(Z_el)%Fpr(i)   = y
      endif
    enddo
    close(iuEPDL97)
  enddo
 END subroutine read_EPDL97_files
 !*****************************************************************************************************
 FUNCTION ComptonS_V(q,elsy,Z_e, wavelength, beam_pol_ang_ecc)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  INTEGER(I4B),intent(IN),optional  :: Z_e
  REAL(DP),intent(IN) :: wavelength
  REAL(DP),intent(IN),optional  :: beam_pol_ang_ecc(2) !  angle (deg) of the closest ellypse axis (def. as b) to the scattering plane;
                                                       !  ratio b/a of electric field
  REAL(DP),dimension(:),intent(IN) :: q
  REAL(DP),dimension(size(q))  :: ComptonS_V
  REAL(DP),dimension(size(q))  :: temp, beta,  c2t
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j, guess(2),nq,lobo,upbo,midBin,bin,ig,guok
  REAL(DP)                     :: qhs,Z0,x,y,x1,x2,y1,y2,slop,intcp,yy,wls4,ebeam_ev,erat,deltaE,ang_pol,eps_pol,sin_pol2,cos_pol2

  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    print*,elsy,Z_el
    if (Z_el<0) then
      print*, 'Fatal Error: called ComptonS_V with Unknown element ',elsy
      STOP 'Fatal Error: called ComptonS_V with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    print*,elsy,Z_el,Z_e
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called ComptonS_V with unmatching Z and symbol'
      stop 'Fatal Error: called ComptonS_V with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called ComptonS_V with Unknown element ',elsy
        STOP 'Fatal Error: called ComptonS_V with Unknown element '
      ENDIF
    endif
  endif
  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
  nq=size(q)
  
  guess=[1,2]
  do i=1,nq
  !__locate
    x = q(i)
    if (x<=Xray_EPDL97(Z_el)%q_S_incoh(1)+eps_DP) then
      ComptonS_V(i) = Xray_EPDL97(Z_el)%S_incoh(1) * x / Xray_EPDL97(Z_el)%q_S_incoh(1)
      cycle
    else if (x>Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh)-eps_DP) then
      x1 = log(Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh-1))
      x2 = log(Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
      y1 = log(Xray_EPDL97(Z_el)%S_incoh(Xray_EPDL97(Z_el)%N_S_incoh-1))
      y2 = log(Xray_EPDL97(Z_el)%S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
      ComptonS_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
      cycle
    endif
!    x = max(x,Xray_EPDL97(Z_el)%q_S_incoh(1))
!    x = min(x,Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
    guok=0
    do ig=1,2
      if (Xray_EPDL97(Z_el)%q_S_incoh(guess(ig)) <= x .and. Xray_EPDL97(Z_el)%q_S_incoh(guess(ig)+1) > x) then
        guok=1
        bin=guess(ig)
      endif
    enddo
    if (guok==0) then
      lobo = 1
      upbo = Xray_EPDL97(Z_el)%N_S_incoh - 1
      do
        if (lobo>upbo) exit
        midBin = (lobo + upbo)/2
        if ( x < Xray_EPDL97(Z_el)%q_S_incoh(midBin) ) then
          upbo = midBin-1
        else
          lobo = midBin+1
        endif
      enddo
      bin = upbo
    endif
    guess = [bin,bin+1]
    if (bin == 1 .and. Xray_EPDL97(Z_el)%S_incoh(bin)<eps_DP) then
      x1 = Xray_EPDL97(Z_el)%q_S_incoh(bin)
      x2 = Xray_EPDL97(Z_el)%q_S_incoh(bin+1)
      y1 = Xray_EPDL97(Z_el)%S_incoh(bin)
      y2 = Xray_EPDL97(Z_el)%S_incoh(bin+1)
      ComptonS_V(i) = y1 + (y2-y1) * (x-x1) / (x2-x1)
    else if (bin>=2.and.bin<=Xray_EPDL97(Z_el)%N_S_incoh-2) then
      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_S_incoh(bin-1:bin+2)), &
                                log(Xray_EPDL97(Z_el)%S_incoh(bin-1:bin+2)) )
      ComptonS_V(i) = exp( yy )
    else if (bin==Xray_EPDL97(Z_el)%N_S_incoh-1) then
      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_S_incoh(bin-2:bin+1)), &
                                log(Xray_EPDL97(Z_el)%S_incoh(bin-2:bin+1)) )
      ComptonS_V(i) = exp( yy )
    else if (bin==Xray_EPDL97(Z_el)%N_S_incoh) then
      x1 = log(Xray_EPDL97(Z_el)%q_S_incoh(bin))
      x2 = log(Xray_EPDL97(Z_el)%q_S_incoh(bin+1))
      y1 = log(Xray_EPDL97(Z_el)%S_incoh(bin))
      y2 = log(Xray_EPDL97(Z_el)%S_incoh(bin+1))
      ComptonS_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
    endif
  enddo
  wls4 = half*(wavelength**2)
  temp = wls4*q*q      !  this is 2*sin^2(theta)
  ebeam_ev = 1.d3*KeV_to_Angstroem/wavelength
  erat = ebeam_ev/mc2elec_eV
!   beta = one/(one+erat*two*temp)
  beta = one/(one+erat*temp)  !!__RF
!   c2t = one-two*temp   ! this is cos(2*theta) 
  c2t = one - temp   !!__RF this is cos(2*theta) 
  if (PRESENT(beam_pol_ang_ecc)) then
    ang_pol=beam_pol_ang_ecc(1)
    eps_pol=beam_pol_ang_ecc(2)
  else ! default - random or circular polarization
    ang_pol=zero
    eps_pol=one
  endif
  sin_pol2 = max(zero,min(one,(sin(ang_pol*degrees_to_radians))**2))
  cos_pol2 = max(zero,min(one,one-sin_pol2))
  temp = beta*beta*half*( beta + (one/beta) - two * (one-c2t**2) * (sin_pol2+eps_pol*cos_pol2)/(one+eps_pol) )
  !temp = erat*temp
  !temp = one/(one + temp*(two+temp)) ! this is (E2/E1)**2, see I.T. vol.C, ch.7.4, eq.7.4.3.1, 
                                     ! need stated after 7.4.3.5
  ComptonS_V = ComptonS_V * temp
  
 END FUNCTION ComptonS_V
 
!*****************************************************************************************************
 FUNCTION ComptonS_S(q,elsy,Z_e, wavelength,beam_pol_ang_ecc)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  INTEGER(I4B),intent(IN),optional  :: Z_e
  REAL(DP),intent(IN),optional  :: beam_pol_ang_ecc(2) !  1: angle (deg) of the closest ellypse axis (def. as b) to the scattering plane;
                                                       !  2: ratio b/a of electric field
  REAL(DP),intent(IN) :: q, wavelength
  REAL(DP) :: ComptonS_S
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j
  REAL(DP)                     :: qhs, Z0, arr(1)

  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called ComptonS_S with Unknown element ',elsy
      STOP 'Fatal Error: called ComptonS_S with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called ComptonS_S with unmatching Z and symbol'
      stop 'Fatal Error: called ComptonS_S with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called ComptonS_S with Unknown element ',elsy
        STOP 'Fatal Error: called ComptonS_S with Unknown element '
      ENDIF
    endif
  endif
  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
  if (PRESENT(beam_pol_ang_ecc)) then
    arr = ComptonS_V(q=[q],Z_e=Z_e, wavelength=wavelength, beam_pol_ang_ecc=beam_pol_ang_ecc)
  else
    arr = ComptonS_V(q=[q],Z_e=Z_e, wavelength=wavelength)
  endif
  ComptonS_S = arr(1)
  
 END FUNCTION ComptonS_S
!*****************************************************************************************************
 ! older version- no pol
!*****************************************************************************************************
! FUNCTION ComptonS_V(q,elsy,Z_e, wavelength)
!  IMPLICIT NONE
!  CHARACTER(2),intent(IN),optional :: elsy
!  INTEGER(I4B),intent(IN),optional  :: Z_e
!  REAL(DP),intent(IN) :: wavelength
!  REAL(DP),dimension(:),intent(IN) :: q
!  REAL(DP),dimension(size(q))  :: ComptonS_V
!  REAL(DP),dimension(size(q))  :: temp
!  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
!  INTEGER(I4B)                      :: iwz,lfn(4),j, guess(2),nq,lobo,upbo,midBin,bin,ig,guok
!  REAL(DP)                     :: qhs, Z0, x,y,x1,x2,y1,y2,slop,intcp,yy,wls4,ebeam_ev,erat, deltaE
!
!  Z_el=0
!  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
!    return
!  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
!    Z_el=Z_e
!  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
!    Z_el=-1
!    do i=0,n_elements_EPDL97
!      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
!      Z_el = i
!      EXIT
!    enddo
!    print*,elsy,Z_el
!    if (Z_el<0) then
!      print*, 'Fatal Error: called ComptonS_V with Unknown element ',elsy
!      STOP 'Fatal Error: called ComptonS_V with Unknown element '
!    ENDIF
!  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
!    Z_el=-1
!    do i=0,n_elements_EPDL97
!      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
!      Z_el = i
!      EXIT
!    enddo
!    print*,elsy,Z_el,Z_e
!    if (Z_el /= Z_e) then
!      print*,'Fatal Error: called ComptonS_V with unmatching Z and symbol'
!      stop 'Fatal Error: called ComptonS_V with unmatching Z and symbol'
!    else
!      if (Z_el<0) then
!        print*, 'Fatal Error: called ComptonS_V with Unknown element ',elsy
!        STOP 'Fatal Error: called ComptonS_V with Unknown element '
!      ENDIF
!    endif
!  endif
!  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
!  nq=size(q)
!  
!  guess=[1,2]
!  do i=1,nq
!  !__locate
!    x = q(i)
!    if (x<=Xray_EPDL97(Z_el)%q_S_incoh(1)+eps_DP) then
!      ComptonS_V(i) = Xray_EPDL97(Z_el)%S_incoh(1) * x / Xray_EPDL97(Z_el)%q_S_incoh(1)
!      cycle
!    else if (x>Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh)-eps_DP) then
!      x1 = log(Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh-1))
!      x2 = log(Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
!      y1 = log(Xray_EPDL97(Z_el)%S_incoh(Xray_EPDL97(Z_el)%N_S_incoh-1))
!      y2 = log(Xray_EPDL97(Z_el)%S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
!      ComptonS_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
!      cycle
!    endif
!    x = max(x,Xray_EPDL97(Z_el)%q_S_incoh(1))
!    x = min(x,Xray_EPDL97(Z_el)%q_S_incoh(Xray_EPDL97(Z_el)%N_S_incoh))
!    guok=0
!    do ig=1,2
!      if (Xray_EPDL97(Z_el)%q_S_incoh(guess(ig)) <= x .and. Xray_EPDL97(Z_el)%q_S_incoh(guess(ig)+1) > x) then
!        guok=1
!        bin=guess(ig)
!      endif
!    enddo
!    if (guok==0) then
!      lobo = 1
!      upbo = Xray_EPDL97(Z_el)%N_S_incoh - 1
!      do
!        if (lobo>upbo) exit
!        midBin = (lobo + upbo)/2
!        if ( x < Xray_EPDL97(Z_el)%q_S_incoh(midBin) ) then
!          upbo = midBin-1
!        else
!          lobo = midBin+1
!        endif
!      enddo
!      bin = upbo
!    endif
!    guess = [bin,bin+1]
!    if (bin == 1 .and. Xray_EPDL97(Z_el)%S_incoh(bin)<eps_DP) then
!      x1 = Xray_EPDL97(Z_el)%q_S_incoh(bin)
!      x2 = Xray_EPDL97(Z_el)%q_S_incoh(bin+1)
!      y1 = Xray_EPDL97(Z_el)%S_incoh(bin)
!      y2 = Xray_EPDL97(Z_el)%S_incoh(bin+1)
!      ComptonS_V(i) = y1 + (y2-y1) * (x-x1) / (x2-x1)
!    else if (bin>=2.and.bin<=Xray_EPDL97(Z_el)%N_S_incoh-2) then
!      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_S_incoh(bin-1:bin+2)), &
!                                log(Xray_EPDL97(Z_el)%S_incoh(bin-1:bin+2)) )
!      ComptonS_V(i) = exp( yy )
!    else if (bin==Xray_EPDL97(Z_el)%N_S_incoh-1) then
!      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_S_incoh(bin-2:bin+1)), &
!                                log(Xray_EPDL97(Z_el)%S_incoh(bin-2:bin+1)) )
!      ComptonS_V(i) = exp( yy )
!    else if (bin==Xray_EPDL97(Z_el)%N_S_incoh) then
!      x1 = log(Xray_EPDL97(Z_el)%q_S_incoh(bin))
!      x2 = log(Xray_EPDL97(Z_el)%q_S_incoh(bin+1))
!      y1 = log(Xray_EPDL97(Z_el)%S_incoh(bin))
!      y2 = log(Xray_EPDL97(Z_el)%S_incoh(bin+1))
!      ComptonS_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
!    endif
!  enddo
!  wls4 = half*(wavelength**2)
!  temp = wls4*q*q
!  ebeam_ev = 1.d3*KeV_to_Angstroem/wavelength
!  erat = ebeam_ev/mc2elec_eV
!  temp = erat*temp
!  temp = one/(one + temp*(two+temp)) ! this is (E2/E1)**2, see I.T. vol.C, ch.7.4, eq.7.4.3.1, 
!                                     ! need stated after 7.4.3.5
!  ComptonS_V = ComptonS_V * temp
!  
! END FUNCTION ComptonS_V
!*****************************************************************************************************
! FUNCTION ComptonS_S(q,elsy,Z_e, wavelength)
!  IMPLICIT NONE
!  CHARACTER(2),intent(IN),optional :: elsy
!  INTEGER(I4B),intent(IN),optional  :: Z_e
!  REAL(DP),intent(IN) :: q, wavelength
!  REAL(DP) :: ComptonS_S
!  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
!  INTEGER(I4B)                      :: iwz,lfn(4),j
!  REAL(DP)                     :: qhs, Z0, arr(1)
!
!  Z_el=0
!  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
!    return
!  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
!    Z_el=Z_e
!  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
!    Z_el=-1
!    do i=0,n_elements_EPDL97
!      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
!      Z_el = i
!      EXIT
!    enddo
!    if (Z_el<0) then
!      print*, 'Fatal Error: called ComptonS_S with Unknown element ',elsy
!      STOP 'Fatal Error: called ComptonS_S with Unknown element '
!    ENDIF
!  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
!    Z_el=-1
!    do i=0,n_elements_EPDL97
!      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
!      Z_el = i
!      EXIT
!    enddo
!    if (Z_el /= Z_e) then
!      print*,'Fatal Error: called ComptonS_S with unmatching Z and symbol'
!      stop 'Fatal Error: called ComptonS_S with unmatching Z and symbol'
!    else
!      if (Z_el<0) then
!        print*, 'Fatal Error: called ComptonS_S with Unknown element ',elsy
!        STOP 'Fatal Error: called ComptonS_S with Unknown element '
!      ENDIF
!    endif
!  endif
!  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
!  arr = ComptonS_V(q=[q],Z_e=Z_e, wavelength=wavelength)
!  ComptonS_S = arr(1)
!  
! END FUNCTION ComptonS_S
!*****************************************************************************************************
 FUNCTION FormFactEPDL97_V(q,elsy,Z_e)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  INTEGER(I4B),intent(IN),optional  :: Z_e
  REAL(DP),dimension(:),intent(IN) :: q
  REAL(DP),dimension(size(q))  :: FormFactEPDL97_V
  REAL(DP),dimension(size(q))  :: temp
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j, guess(2),nq,lobo,upbo,midBin,bin,ig,guok
  REAL(DP)                     :: qhs, Z0, x,y,x1,x2,y1,y2,slop,intcp,yy,wls4,ebeam_ev,erat, deltaE

  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFactEPDL97_V with Unknown element ',elsy
      STOP 'Fatal Error: called FormFactEPDL97_V with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFactEPDL97_V with unmatching Z and symbol'
      stop 'Fatal Error: called FormFactEPDL97_V with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFactEPDL97_V with Unknown element ',elsy
        STOP 'Fatal Error: called FormFactEPDL97_V with Unknown element '
      ENDIF
    endif
  endif
  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
  nq=size(q)
  
  guess=[1,2]
  do i=1,nq
  !__locate
    x = q(i)
    if (x<=eps_DP) then
      FormFactEPDL97_V(i) = real(Z_el,DP)
      cycle
    endif
    if (x>Xray_EPDL97(Z_el)%q_F_coh(Xray_EPDL97(Z_el)%N_F_coh)-eps_DP) then
      x1 = log(Xray_EPDL97(Z_el)%q_F_coh(Xray_EPDL97(Z_el)%N_F_coh-1))
      x2 = log(Xray_EPDL97(Z_el)%q_F_coh(Xray_EPDL97(Z_el)%N_F_coh))
      y1 = log(Xray_EPDL97(Z_el)%F_coh(Xray_EPDL97(Z_el)%N_F_coh-1))
      y2 = log(Xray_EPDL97(Z_el)%F_coh(Xray_EPDL97(Z_el)%N_F_coh))
      FormFactEPDL97_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
      cycle
    endif
!    x = max(x,Xray_EPDL97(Z_el)%q_F_coh(1))
!    x = min(x,Xray_EPDL97(Z_el)%q_F_coh(Xray_EPDL97(Z_el)%N_F_coh))
    guok=0
    do ig=1,2
      if (Xray_EPDL97(Z_el)%q_F_coh(guess(ig)) <= x .and. Xray_EPDL97(Z_el)%q_F_coh(guess(ig)+1) > x) then
        guok=1
        bin=guess(ig)
      endif
    enddo
    if (guok==0) then
      lobo = 1
      upbo = Xray_EPDL97(Z_el)%N_F_coh - 1
      do
        if (lobo>upbo) exit
        midBin = (lobo + upbo)/2
        if ( x < Xray_EPDL97(Z_el)%q_F_coh(midBin) ) then
          upbo = midBin-1
        else
          lobo = midBin+1
        endif
      enddo
      bin = upbo
    endif
    guess = [bin,bin+1]
    if (bin<=2) then
      yy=INTERPOLPOL3(x, Xray_EPDL97(Z_el)%q_F_coh(1:4), &
                         Xray_EPDL97(Z_el)%F_coh(1:4) )
      FormFactEPDL97_V(i) = yy
    else if (bin>=3.and.bin<=Xray_EPDL97(Z_el)%N_F_coh-2) then
      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_F_coh(bin-1:bin+2)), &
                                log(Xray_EPDL97(Z_el)%F_coh(bin-1:bin+2)) )
      FormFactEPDL97_V(i) = exp( yy )
    else if (bin==Xray_EPDL97(Z_el)%N_F_coh-1) then
      yy=INTERPOLPOL3(log(x), log(Xray_EPDL97(Z_el)%q_F_coh(bin-2:bin+1)), &
                                log(Xray_EPDL97(Z_el)%F_coh(bin-2:bin+1)) )
      FormFactEPDL97_V(i) = exp( yy )
    else if (bin==Xray_EPDL97(Z_el)%N_F_coh) then
      x1 = log(Xray_EPDL97(Z_el)%q_F_coh(bin))
      x2 = log(Xray_EPDL97(Z_el)%q_F_coh(bin+1))
      y1 = log(Xray_EPDL97(Z_el)%F_coh(bin))
      y2 = log(Xray_EPDL97(Z_el)%F_coh(bin+1))
      FormFactEPDL97_V(i) = exp( y1 + (y2-y1) * (log(x)-x1) / (x2-x1) )
    endif
  enddo
  
 END FUNCTION FormFactEPDL97_V
!*****************************************************************************************************
 FUNCTION FormFactEPDL97_S(q,elsy,Z_e)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  INTEGER(I4B),intent(IN),optional  :: Z_e
  REAL(DP),intent(IN) :: q
  REAL(DP) :: FormFactEPDL97_S
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j
  REAL(DP)                     :: qhs, Z0, arr(1)

  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFactEPDL97_S with Unknown element ',elsy
      STOP 'Fatal Error: called FormFactEPDL97_S with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFactEPDL97_S with unmatching Z and symbol'
      stop 'Fatal Error: called FormFactEPDL97_S with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFactEPDL97_S with Unknown element ',elsy
        STOP 'Fatal Error: called FormFactEPDL97_S with Unknown element '
      ENDIF
    endif
  endif
  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
  arr = FormFactEPDL97_V(q=[q],Z_e=Z_e)
  FormFactEPDL97_S = arr(1)
  
 END FUNCTION FormFactEPDL97_S
!*****************************************************************************************************
 FUNCTION Anomalous_X(elsy,Z_e, wavelength)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  INTEGER(I4B),intent(IN),optional  :: Z_e
  REAL(DP),intent(IN) :: wavelength
  REAL(DP),dimension(2)  :: Anomalous_X
  REAL(DP)  :: temp
  INTEGER(I4B)                      :: i,Z_el,iuEPDL97
  INTEGER(I4B)                      :: iwz,lfn(4),j, guess(2),nq,lobo,upbo,midBin,bin,ig,guok
  REAL(DP)                     :: qhs, Z0, x,y,x1,x2,y1,y2,slop,intcp,yy,wls4,ebeam_Kev,erat, deltaE

  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called Anomalous_X with Unknown element ',elsy
      STOP 'Fatal Error: called Anomalous_X with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements_EPDL97
      IF (trim(elsy) /= trim(symb_of_Z1(i))) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called Anomalous_X with unmatching Z and symbol'
      stop 'Fatal Error: called Anomalous_X with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called Anomalous_X with Unknown element ',elsy
        STOP 'Fatal Error: called Anomalous_X with Unknown element '
      ENDIF
    endif
  endif
  if (Xray_EPDL97(Z_el)%isfilled /= 1) CALL read_EPDL97_files(Z_e=Z_el)
  
  ebeam_Kev = KeV_to_Angstroem/wavelength
  Anomalous_X = zero
  !__locate
  x = ebeam_Kev
  ! __ EXTRAPOLATE IF OUT
  if (x<=Xray_EPDL97(Z_el)%E_Fpr(1)+eps_DP) then
    Anomalous_X(1) = Xray_EPDL97(Z_el)%Fpr(1) * x / Xray_EPDL97(Z_el)%E_Fpr(1)
    Anomalous_X(2) = Xray_EPDL97(Z_el)%Fdpr(1) * x / Xray_EPDL97(Z_el)%E_Fdpr(1)
  else if (x>Xray_EPDL97(Z_el)%E_Fpr(Xray_EPDL97(Z_el)%N_Fpr)-eps_DP) then
    Anomalous_X(1) = Xray_EPDL97(Z_el)%Fpr(Xray_EPDL97(Z_el)%N_Fpr)
    Anomalous_X(2) = Xray_EPDL97(Z_el)%Fdpr(Xray_EPDL97(Z_el)%N_Fdpr)
  else
    !_ first, f'
    lobo = 1
    upbo = Xray_EPDL97(Z_el)%N_Fpr - 1
    do
      if (lobo>upbo) exit
      midBin = (lobo + upbo)/2
      if ( x < Xray_EPDL97(Z_el)%E_Fpr(midBin) ) then
        upbo = midBin-1
      else
        lobo = midBin+1
      endif
    enddo
    bin = upbo
    if (bin == 1) then
      x1 = Xray_EPDL97(Z_el)%E_Fpr(bin)
      x2 = Xray_EPDL97(Z_el)%E_Fpr(bin+1)
      y1 = Xray_EPDL97(Z_el)%Fpr(bin)
      y2 = Xray_EPDL97(Z_el)%Fpr(bin+1)
      Anomalous_X(1) = y1 + (y2-y1) * (x-x1) / (x2-x1)
    else if (bin>=2.and.bin<=Xray_EPDL97(Z_el)%N_Fpr-2) then
      Anomalous_X(1)=interp_leastD2(x, Xray_EPDL97(Z_el)%E_Fpr(bin-1:bin+2), &
                                     Xray_EPDL97(Z_el)%Fpr(bin-1:bin+2) )
    else if (bin>=Xray_EPDL97(Z_el)%N_Fpr-1) then
      Anomalous_X(1)=interp_leastD2(x, Xray_EPDL97(Z_el)%E_Fpr(bin-2:bin+1), &
                                     Xray_EPDL97(Z_el)%Fpr(bin-2:bin+1) )
    endif
    !_ repeat for f''
    lobo = 1
    upbo = Xray_EPDL97(Z_el)%N_Fdpr - 1
    do
      if (lobo>upbo) exit
      midBin = (lobo + upbo)/2
      if ( x < Xray_EPDL97(Z_el)%E_Fdpr(midBin) ) then
        upbo = midBin-1
      else
        lobo = midBin+1
      endif
    enddo
    bin = upbo
    if (bin == 1) then
      x1 = Xray_EPDL97(Z_el)%E_Fdpr(bin)
      x2 = Xray_EPDL97(Z_el)%E_Fdpr(bin+1)
      y1 = Xray_EPDL97(Z_el)%Fdpr(bin)
      y2 = Xray_EPDL97(Z_el)%Fdpr(bin+1)
      Anomalous_X(2) = y1 + (y2-y1) * (x-x1) / (x2-x1)
    else if (bin>=2.and.bin<=Xray_EPDL97(Z_el)%N_Fdpr-2) then
      Anomalous_X(2)=interp_leastD2(x, Xray_EPDL97(Z_el)%E_Fdpr(bin-1:bin+2), &
                                     Xray_EPDL97(Z_el)%Fdpr(bin-1:bin+2) )
    else if (bin>=Xray_EPDL97(Z_el)%N_Fdpr-1) then
      Anomalous_X(2)=interp_leastD2(x, Xray_EPDL97(Z_el)%E_Fdpr(bin-2:bin+1), &
                                     Xray_EPDL97(Z_el)%Fdpr(bin-2:bin+1) )
    endif
  endif
  
 END FUNCTION Anomalous_X
!*****************************************************************************************************
function INTERPOLPOL3(xv, x4, y4)
implicit none
real(DP),intent(IN) :: xv, x4(4), y4(4)
real(DP) :: INTERPOLPOL3
real(DP) :: numx,denx
integer(I4B) :: j,m
!________ Cubic Lagrange interpolation
INTERPOLPOL3 = zero
do j=1,4
  numx=one
  denx=one
  do m=1,4
    if (m==j) cycle
    numx = numx*(xv-x4(m))
    denx = denx*(x4(j)-x4(m))
  enddo
  INTERPOLPOL3 = INTERPOLPOL3 + numx*y4(j)/denx
enddo

end function INTERPOLPOL3
!*****************************************************************************************************
function interp_leastD2(xv,x4,y4)
implicit none
real(DP),intent(IN) :: xv, x4(4),y4(4)
real(DP) :: interp_leastD2
real(DP) :: d0,d1,d2,dy0,dy1,dy2,critL,critR,xx,dxx,d10,d12

d0=x4(2)-x4(1)
d1=x4(3)-x4(2)
d2=x4(4)-x4(3)
dy0=y4(2)-y4(1)
dy1=y4(3)-y4(2)
dy2=y4(4)-y4(3)
d10=(d1+d0)*d1
d12=(d1+d2)*d2

critL = abs(d1*dy0 - d0*dy1)/(d0*(d0 + d1))
critR = abs(d2*dy1 - d1*dy2)/d12
xx = xv-x4(1)
dxx=d0-xx
if (critL<=critR) then
  !Use Left Parabolic Interpolant
  interp_leastD2 =y4(1) + xx*( (d1*dy0-d0*dy1)*dxx + dy0*d10 ) / (d0*d10)
else
  interp_leastD2 = y4(2) + dxx*( (d1*dy2-dy1*d2)*(d1 + dxx) - dy1*d12 ) / (d1*d12)
endif

end function interp_leastD2
!*****************************************************************************************************
end module Compton_Anom_Xray
!_________________________________________________________________________________________________________________________________
MODULE ATOTYP
 USE nano_deftyp

 TYPE,PUBLIC     :: atoscaf

! atomic scattering factor = con_t + SUM(mul_g(i) * exp(-coe_g(i)*q^2/4), i=1..5)

     REAL(CP)                                :: con_t   ! constant term
     REAL(CP),DIMENSION(5)                   :: mul_g   ! scale of gaussian terms
     REAL(CP),DIMENSION(5)                   :: coe_g   ! exponent coefficient of gaussian term
 END TYPE atoscaf

 TYPE,PUBLIC     :: atomX
     CHARACTER(2)                            :: el_name
     LOGICAL                                 :: exist_ion(-6:7)
     CHARACTER(4),DIMENSION(-6:7)            :: ion_name
     TYPE(atoscaf),DIMENSION(-6:7)           :: X_scaf
 END TYPE atomX
 

END MODULE ATOTYP
!_______________________________________________________________________________
MODULE ATOMIX
 USE ATOTYP
 USE Compton_Anom_Xray

 PRIVATE
 PUBLIC   :: FORMFACT, symb_of_Z, Z_of_symb, n_elements, setup_done, INI_scaf, atwei, &
             bcohr,bcohi,bincr,binci,sig_coh,sig_inc,sig_tot,sig_abs, at_radii, &
             ffcoef, KeV_to_Angstroem, class_el_radius, epsilon0, COMP_COMPTON_S, &
             FormFact_EPDL97, Anomalous_X, use_EPDL97, at_xscat !, &
!             ZPartitionCoeff

 INTEGER,PARAMETER                  :: n_elements = 98
! real(CP),save :: ZPartitionCoeff(2,n_elements,n_elements)=one

 TYPE(atomX),dimension(n_elements),save  :: at_xscat
 character(2),dimension(0:n_elements),save :: symb_of_Z
 real(DP),dimension(0:n_elements),save     :: atwei,bcohr,bcohi,bincr,binci,sig_coh,sig_inc,sig_tot,sig_abs,at_radii
 LOGICAL,save                            :: setup_done=.false., use_EPDL97=.true.


 INTERFACE FORMFACT
   MODULE PROCEDURE FormFact_GEN_SP,FormFact_GEN,FormFact_GEN_V_SP,FormFact_GEN_V
 END INTERFACE FORMFACT
 


 DATA setup_done/.false./
 DATA symb_of_Z(0:0)/'Zz'/
 DATA symb_of_Z(1:2)/'H ','He'/
 DATA symb_of_Z(3:10)/'Li','Be','B ','C ','N ','O ','F ','Ne'/
 DATA symb_of_Z(11:18)/'Na','Mg','Al','Si','P ','S ','Cl','Ar'/
 DATA symb_of_Z(19:36)/'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr'/
 DATA symb_of_Z(37:54)/'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe'/
 DATA symb_of_Z(55:86)/'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ', &
                       'Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn'/
 DATA symb_of_Z(87:n_elements)/'Fr','Ra','Ac','Th','Pa','U ','Np','Pu','Am','Cm','Bk','Cf'/
!! FB Atomic radii Cs modified 26.03.2016
 !DATA at_radii/zero,0.23d0,0.31d0,1.34d0,0.85d0,0.73d0, 0.60d0,0.54d0,0.48d0, 0.42d0,0.38d0,1.54d0,1.27d0,1.11d0,1.02d0,0.94d0,&
  DATA at_radii/zero,0.23d0,0.31d0,1.34d0,0.85d0,0.73d0, 0.40d0,0.34d0,0.28d0, 0.42d0,0.38d0,1.54d0,1.27d0,1.11d0,1.02d0,0.94d0,&
 0.88d0,0.79d0,0.71d0,1.96d0,1.33d0,1.14d0,1.08d0,1.06d0,1.03d0,1.03d0,1.02d0,0.96d0,1.01d0,1.20d0,1.31d0,1.21d0,1.14d0,1.06d0,&
 1.03d0,0.94d0,0.88d0,2.11d0,1.39d0,1.24d0,1.21d0,1.16d0,1.13d0,1.10d0,1.03d0,1.06d0,1.12d0,1.37d0,1.48d0,1.44d0,1.32d0,1.27d0,&
 1.21d0,1.15d0,1.08d0,1.67d0,1.49d0,1.39d0,1.31d0,1.28d0,1.85d0,1.85d0,1.85d0,1.85d0,1.32d0,1.75d0,1.75d0,1.75d0,1.75d0,1.75d0,&
 1.75d0,1.31d0,1.22d0,1.19d0,1.15d0,1.10d0,1.09d0,1.07d0,1.10d0,1.23d0,1.49d0,1.48d0,1.37d0,1.35d0,1.29d0,1.27d0,1.20d0,1.94d0,&
 1.59d0,1.40d0,1.36d0,1.29d0,1.18d0,1.16d0,1.59d0,1.73d0, two,two,two/

 DATA atwei/zero, &
 1.00794_DP,4.002602_DP,6.941_DP,9.012182_DP,10.811_DP,12.0107_DP,14.0067_DP,15.9994_DP,18.9984032_DP,20.1797_DP,&
 22.98976928_DP,24.305_DP,26.9815386_DP,28.0855_DP,30.973762_DP,32.065_DP,35.453_DP,39.948_DP,39.0983_DP,40.078_DP,&
 44.955912_DP,47.867_DP,50.9415_DP,51.9961_DP,54.938045_DP,55.845_DP,58.933195_DP,58.6934_DP,63.546_DP,65.409_DP,69.723_DP,&
 72.64_DP,74.9216_DP,78.96_DP,79.904_DP,83.798_DP,85.4678_DP,87.62_DP,88.90585_DP,91.224_DP,92.90638_DP,95.94_DP,98._DP,&
 101.07_DP,102.9055_DP,106.42_DP,107.8682_DP,112.411_DP,114.818_DP,118.71_DP,121.76_DP,127.6_DP,126.90447_DP,131.293_DP,&
 132.9054519_DP,137.327_DP,138.90547_DP,140.116_DP,140.90765_DP,144.242_DP,145._DP,150.36_DP,151.964_DP,157.25_DP,&
 158.92535_DP,162.5_DP,164.93032_DP,167.259_DP,168.93421_DP,173.04_DP,174.967_DP,178.49_DP,180.94788_DP,183.84_DP,&
 186.207_DP,190.23_DP,192.217_DP,195.084_DP,196.966569_DP,200.59_DP,204.3833_DP,207.2_DP,208.9804_DP,209._DP,210._DP,&
 222._DP,223._DP,226._DP,227._DP,232.03806_DP,231.03588_DP,238.02891_DP,237._DP,239._DP,243._DP,247._DP,247._DP,251._DP/

data bcohr/zero, &
-3.739_DP, 3.26_DP, -1.9_DP, 7.79_DP, 5.3_DP, 6.646_DP, 9.36_DP, 5.803_DP, 5.654_DP, 4.566_DP, 3.63_DP, 5.375_DP, &
3.449_DP, 4.1491_DP, 5.13_DP, 2.847_DP, 9.577_DP, 1.909_DP, 3.67_DP, 4.7_DP, 12.29_DP, -3.438_DP, -0.3824_DP, 3.635_DP, &
-3.73_DP, 9.45_DP, 2.49_DP, 10.3_DP, 7.718_DP, 5.68_DP, 7.288_DP, 8.185_DP, 6.58_DP, 7.97_DP, 6.795_DP, 7.81_DP, 7.09_DP, &
7.02_DP, 7.75_DP, 7.16_DP, 7.054_DP, 6.715_DP, 6.8_DP, 7.03_DP, 5.88_DP, 5.91_DP, 5.922_DP, 4.87_DP, 4.065_DP, 6.225_DP, &
5.57_DP, 5.8_DP, 5.28_DP, 4.92_DP, 5.42_DP, 5.07_DP, 8.24_DP, 4.84_DP, 4.58_DP, 7.69_DP, 12.6_DP, 0.8_DP, 7.22_DP, 6.5_DP, &
7.38_DP, 16.9_DP, 8.01_DP, 7.79_DP, 7.07_DP, 12.43_DP, 7.21_DP, 7.7_DP, 6.91_DP, 4.86_DP, 9.2_DP, 10.7_DP, 10.6_DP, 9.6_DP, &
7.63_DP, 12.692_DP, 8.776_DP, 9.405_DP, 8.532_DP, zero, zero, zero, zero, 10._DP, zero, 10.31_DP, 9.1_DP, 8.417_DP, 10.55_DP, &
zero, 8.3_DP, zero, zero, zero/
data bcohi/zero, &
zero, zero, zero, zero, 0.213_DP, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, 0.7_DP, 0.0539_DP, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, 1.65_DP, 1.26_DP, 13.82_DP, zero, 0.276_DP, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero/
data bincr/zero, &
zero, zero, zero, 0.12_DP, zero, zero, zero, zero, -0.082_DP, zero, 3.59_DP, zero, 0.256_DP, zero, 0.2_DP, zero, zero, &
zero, zero, zero, -6._DP, zero, zero, zero, 1.79_DP, zero, -6.2_DP, zero, zero, zero, zero, zero, -0.69_DP, zero, zero, zero, &
zero, zero, 1.1_DP, zero, -0.139_DP, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, 1.58_DP, 3.04_DP, 1.29_DP,&
zero, zero, zero, -0.35_DP, zero, 3.2_DP, zero, zero, zero, -0.17_DP, zero, -1.7_DP, zero, 0.9_DP, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, -1.84_DP, zero, zero, zero, zero, 0.259_DP, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
2._DP, zero, zero, zero/
data binci/zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, &
zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero/
data sig_coh/zero, &
1.7568_DP, 1.34_DP, 0.454_DP, 7.63_DP, 3.54_DP, 5.551_DP, 11.01_DP, 4.232_DP, 4.017_DP, 2.62_DP, 1.66_DP, 3.631_DP, &
1.495_DP, 2.163_DP, 3.307_DP, 1.0186_DP, 11.5257_DP, 0.458_DP, 1.69_DP, 2.78_DP, 19._DP, 1.485_DP, 0.0184_DP, 1.66_DP, 1.75_DP, &
11.22_DP, 0.779_DP, 13.3_DP, 7.485_DP, 4.054_DP, 6.675_DP, 8.42_DP, 5.44_DP, 7.98_DP, 5.8_DP, 7.67_DP, 6.32_DP, 6.19_DP, 7.55_DP,&
6.44_DP, 6.253_DP, 5.67_DP, 5.8_DP, 6.21_DP, 4.34_DP, 4.39_DP, 4.407_DP, 3.04_DP, 2.08_DP, 4.871_DP, 3.9_DP, 4.23_DP, 3.5_DP, &
2.96_DP, 3.69_DP, 3.23_DP, 8.53_DP, 2.94_DP, 2.64_DP, 7.43_DP, 20._DP, 0.422_DP, 6.57_DP, 29.3_DP, 6.84_DP, 35.9_DP, 8.06_DP, &
7.63_DP, 6.28_DP, 19.42_DP, 6.53_DP, 7.6_DP, 6._DP, 2.97_DP, 10.6_DP, 14.4_DP, 14.1_DP, 11.58_DP, 7.32_DP, 20.24_DP, 9.678_DP, &
11.115_DP, 9.148_DP, zero, zero, zero, zero, 13._DP, zero, 13.36_DP, 10.4_DP, 8.903_DP, 14._DP, zero, 8.7_DP, zero, zero, zero/
data sig_inc/zero, &
80.26_DP, zero, 0.92_DP, 0.0018_DP, 1.7_DP, 0.001_DP, 0.5_DP, 0.0008_DP, 0.0008_DP, 0.008_DP, 1.62_DP, 0.08_DP, &
0.0082_DP, 0.004_DP, 0.005_DP, 0.007_DP, 5.3_DP, 0.225_DP, 0.27_DP, 0.05_DP, 4.5_DP, 2.87_DP, 5.08_DP, 1.83_DP, 0.4_DP, 0.4_DP, &
4.8_DP, 5.2_DP, 0.55_DP, 0.077_DP, 0.16_DP, 0.18_DP, 0.06_DP, 0.32_DP, 0.1_DP, 0.01_DP, 0.5_DP, 0.06_DP, 0.15_DP, 0.02_DP, &
0.0024_DP, 0.04_DP, 0.5_DP, 0.4_DP, 0.3_DP, 0.093_DP, 0.58_DP, 3.46_DP, 0.54_DP, 0.022_DP, 0.007_DP, 0.09_DP, 0.31_DP, zero, &
0.21_DP, 0.15_DP, 1.13_DP, 0.001_DP, 0.015_DP, 9.2_DP, 1.3_DP, 39._DP, 2.5_DP, 151._DP, 0.004_DP, 54.4_DP, 0.36_DP, 1.1_DP, &
0.1_DP, 4._DP, 0.7_DP, 2.6_DP, 0.01_DP, 1.63_DP, 0.9_DP, 0.3_DP, zero, 0.13_DP, 0.43_DP, 6.6_DP, 0.21_DP, 0.003_DP, 0.0084_DP, &
zero, zero, zero, zero, zero, zero, zero, 0.1_DP, 0.005_DP, 0.5_DP, zero, 0.3_DP, zero, zero, zero/
data sig_tot/zero, &
82.02_DP, 1.34_DP, 1.37_DP, 7.63_DP, 5.24_DP, 5.551_DP, 11.51_DP, 4.232_DP, 4.018_DP, 2.628_DP, 3.28_DP, 3.71_DP, &
1.503_DP, 2.167_DP, 3.312_DP, 1.026_DP, 16.8_DP, 0.683_DP, 1.96_DP, 2.83_DP, 23.5_DP, 4.35_DP, 5.1_DP, 3.49_DP, 2.15_DP, &
11.62_DP, 5.6_DP, 18.5_DP, 8.03_DP, 4.131_DP, 6.83_DP, 8.6_DP, 5.5_DP, 8.3_DP, 5.9_DP, 7.68_DP, 6.8_DP, 6.25_DP, 7.7_DP, &
6.46_DP, 6.255_DP, 5.71_DP, 6.3_DP, 6.6_DP, 4.6_DP, 4.48_DP, 4.99_DP, 6.5_DP, 2.62_DP, 4.892_DP, 3.9_DP, 4.32_DP, 3.81_DP, &
zero, 3.9_DP, 3.38_DP, 9.66_DP, 2.94_DP, 2.66_DP, 16.6_DP, 21.3_DP, 39._DP, 9.2_DP, 180._DP, 6.84_DP, 90.3_DP, 8.42_DP, &
8.7_DP, 6.38_DP, 23.4_DP, 7.2_DP, 10.2_DP, 6.01_DP, 4.6_DP, 11.5_DP, 14.7_DP, 14._DP, 11.71_DP, 7.75_DP, 26.8_DP, 9.89_DP, &
11.118_DP, 9.156_DP, zero, zero, 12.6_DP, zero, 13._DP, zero, 13.36_DP, 10.5_DP, 8.908_DP, 14.5_DP, zero, 9._DP, zero, zero,zero/
data sig_abs/zero, &
0.3326_DP, 0.00747_DP, 70.5_DP, 0.0076_DP, 767._DP, 0.0035_DP, 1.9_DP, 0.00019_DP, 0.0096_DP, 0.039_DP, 0.53_DP, &
0.063_DP, 0.231_DP, 0.171_DP, 0.172_DP, 0.53_DP, 33.5_DP, 0.675_DP, 2.1_DP, 0.43_DP, 27.5_DP, 6.09_DP, 5.08_DP, 3.05_DP, 13.3_DP,&
2.56_DP, 37.18_DP, 4.49_DP, 3.78_DP, 1.11_DP, 2.75_DP, 2.2_DP, 4.5_DP, 11.7_DP, 6.9_DP, 25._DP, 0.38_DP, 1.28_DP, 1.28_DP, &
0.185_DP, 1.15_DP, 2.48_DP, 20._DP, 2.56_DP, 144.8_DP, 6.9_DP, 63.3_DP, 2520._DP, 193.8_DP, 0.626_DP, 4.91_DP, 4.7_DP, 6.15_DP, &
23.9_DP, 29._DP, 1.1_DP, 8.97_DP, 0.63_DP, 11.5_DP, 50.5_DP, 168.4_DP, 5922._DP, 4530._DP, 49700._DP, 23.4_DP, 994._DP, 64.7_DP, &
159._DP, 100._DP, 34.8_DP, 74._DP, 104.1_DP, 20.6_DP, 18.3_DP, 89.7_DP, 16._DP, 425._DP, 10.3_DP, 98.65_DP, 372.3_DP, 3.43_DP, &
0.171_DP, 0.0338_DP, zero, zero, zero, zero, 12.8_DP, zero, 7.37_DP, 200.6_DP, 7.57_DP, 175.9_DP, zero, 75.3_DP, zero, zero, zero/



CONTAINS

 SUBROUTINE INI_scaf
   IMPLICIT NONE
   INTEGER      :: I,Iz,icha_ion,iastat,iu

   do I=1,n_elements
     at_xscat(I)%el_name      = symb_of_Z(I)
     at_xscat(I)%exist_ion(:) = .false.
   enddo


    iZ =    1
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.000049_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.413048_DP,   0.294953_DP,   0.187491_DP,   0.080701_DP,   0.023736_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  15.569946_DP,  32.398468_DP,   5.711404_DP,  61.889874_DP,   1.334118_DP/)
    iZ =    2
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.000487_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.732354_DP,   0.753896_DP,   0.283819_DP,   0.190003_DP,   0.039139_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  11.553918_DP,   4.595831_DP,   1.546299_DP,  26.463964_DP,   0.377523_DP/)
    iZ =    3
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.002542_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.974637_DP,   0.158472_DP,   0.811855_DP,   0.262416_DP,   0.790108_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.334946_DP,   0.342451_DP,  97.102966_DP, 201.363831_DP,   1.409234_DP/)
    iZ =    4
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.002511_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.533712_DP,   0.638283_DP,   0.601052_DP,   0.106139_DP,   1.118414_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  42.662079_DP,   0.595420_DP,  99.106499_DP,   0.151340_DP,   1.843093_DP/)
    iZ =    5
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.003823_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   2.085185_DP,   1.064580_DP,   1.062788_DP,   0.140515_DP,   0.641784_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  23.494068_DP,   1.137894_DP,  61.238976_DP,   0.114886_DP,   0.399036_DP/)
    iZ =    6
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.297983_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   2.657506_DP,   1.078079_DP,   1.490909_DP,  -4.241070_DP,   0.713791_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  14.780758_DP,   0.776775_DP,  42.086842_DP,  -0.000294_DP,   0.239535_DP/)
    iZ =    7
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -11.804902_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.893780_DP,   3.277479_DP,   1.858092_DP,   0.858927_DP,   0.912985_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.000158_DP,  10.232723_DP,  30.344690_DP,   0.656065_DP,   0.217287_DP/)
    iZ =    8
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.027014_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   2.960427_DP,   2.508818_DP,   0.637853_DP,   0.722838_DP,   1.142756_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  14.182259_DP,   5.936858_DP,   0.112726_DP,  34.958481_DP,   0.390240_DP/)
    iZ =    9
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.032557_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.511943_DP,   2.772244_DP,   0.678385_DP,   0.915159_DP,   1.089261_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  10.687859_DP,   4.380466_DP,   0.093982_DP,  27.255203_DP,   0.313066_DP/)
    iZ =   10
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.025576_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.183749_DP,   2.905726_DP,   0.520513_DP,   1.135641_DP,   1.228065_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   8.175457_DP,   3.252536_DP,   0.063295_DP,  21.813910_DP,   0.224952_DP/)
    iZ =   11
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.079712_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.910127_DP,   3.081783_DP,   1.262067_DP,   1.098938_DP,   0.560991_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.281434_DP,   9.119178_DP,   0.102763_DP, 132.013947_DP,   0.405878_DP/)
    iZ =   12
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.126842_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.708971_DP,   1.194814_DP,   1.558157_DP,   1.170413_DP,   3.239403_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.875207_DP, 108.506081_DP,   0.111516_DP,  48.292408_DP,   1.928171_DP/)
    iZ =   13
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.139509_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.730796_DP,   2.313951_DP,   1.541980_DP,   1.117564_DP,   3.154754_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.628931_DP,  43.051167_DP,   0.095960_DP, 108.932388_DP,   1.555918_DP/)
    iZ =   14
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.145073_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   5.275329_DP,   3.191038_DP,   1.511514_DP,   1.356849_DP,   2.519114_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.631338_DP,  33.730728_DP,   0.081119_DP,  86.288643_DP,   1.170087_DP/)
    iZ =   15
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.155233_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.950541_DP,   4.146930_DP,   1.494560_DP,   1.522042_DP,   5.729711_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.908139_DP,  27.044952_DP,   0.071280_DP,  67.520187_DP,   1.981173_DP/)
    iZ =   16
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.154722_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.372157_DP,   5.154568_DP,   1.473732_DP,   1.635073_DP,   1.209372_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.514347_DP,  22.092527_DP,   0.061373_DP,  55.445175_DP,   0.646925_DP/)
    iZ =   17
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.146773_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.446071_DP,   6.870609_DP,   6.151801_DP,   1.750347_DP,   0.634168_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.052357_DP,   1.193165_DP,  18.343416_DP,  46.398396_DP,   0.401005_DP/)
    iZ =   18
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.265954_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.188004_DP,   6.638454_DP,   0.454180_DP,   1.929593_DP,   1.523654_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.956221_DP,  15.339877_DP,  15.339862_DP,  39.043823_DP,   0.062409_DP/)
    iZ =   19
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.253614_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   8.163991_DP,   7.146945_DP,   1.070140_DP,   0.877316_DP,   1.486434_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  12.816323_DP,   0.808945_DP, 210.327011_DP,  39.597652_DP,   0.052821_DP/)
    iZ =   20
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.196255_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   8.593655_DP,   1.477324_DP,   1.436254_DP,   1.182839_DP,   7.113258_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  10.460644_DP,   0.041891_DP,  81.390381_DP, 169.847839_DP,   0.688098_DP/)
    iZ =   21
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.157765_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.476566_DP,   1.487278_DP,   1.600187_DP,   9.177463_DP,   7.099750_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  53.131023_DP,   0.035325_DP, 137.319489_DP,   9.098031_DP,   0.602102_DP/)
    iZ =   22
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.102473_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   9.818524_DP,   1.522646_DP,   1.703101_DP,   1.768774_DP,   7.082555_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   8.001879_DP,   0.029763_DP,  39.885422_DP, 120.157997_DP,   0.532405_DP/)
    iZ =   23
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.067744_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  10.473575_DP,   1.547881_DP,   1.986381_DP,   1.865616_DP,   7.056250_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   7.081940_DP,   0.026040_DP,  31.909672_DP, 108.022842_DP,   0.474882_DP/)
    iZ =   24
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.065510_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.007069_DP,   1.555477_DP,   2.985293_DP,   1.347855_DP,   7.034779_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.366281_DP,   0.023987_DP,  23.244839_DP, 105.774498_DP,   0.429369_DP/)
    iZ =   25
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.147293_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.709542_DP,   1.733414_DP,   2.673141_DP,   2.023368_DP,   7.003180_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   5.597120_DP,   0.017800_DP,  21.788420_DP,  89.517914_DP,   0.383054_DP/)
    iZ =   26
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.304931_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  12.311098_DP,   1.876623_DP,   3.066177_DP,   2.070451_DP,   6.975185_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   5.009415_DP,   0.014461_DP,  18.743040_DP,  82.767876_DP,   0.346506_DP/)
    iZ =   27
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.936572_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  12.914510_DP,   2.481908_DP,   3.466894_DP,   2.106351_DP,   6.960892_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.507138_DP,   0.009126_DP,  16.438129_DP,  76.987320_DP,   0.314418_DP/)
    iZ =   28
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -2.762697_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  13.521865_DP,   6.947285_DP,   3.866028_DP,   2.135900_DP,   4.284731_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.077277_DP,   0.286763_DP,  14.622634_DP,  71.966080_DP,   0.004437_DP/)
    iZ =   29
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -3.254477_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  14.014192_DP,   4.784577_DP,   5.056806_DP,   1.457971_DP,   6.932996_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.738280_DP,   0.003744_DP,  13.034982_DP,  72.554794_DP,   0.265666_DP/)
    iZ =   30
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -36.915829_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  14.741002_DP,   6.907748_DP,   4.642337_DP,   2.191766_DP,  38.424042_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.388232_DP,   0.243315_DP,  11.903689_DP,  63.312130_DP,   0.000397_DP/)
    iZ =   31
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.847395_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.758946_DP,   6.841123_DP,   4.121016_DP,   2.714681_DP,   2.395246_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.121754_DP,   0.226057_DP,  12.482196_DP,  66.203621_DP,   0.007238_DP/)
    iZ =   32
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.018726_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.540613_DP,   1.567900_DP,   3.727829_DP,   3.345098_DP,   6.785079_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.866618_DP,   0.012198_DP,  13.432163_DP,  58.866047_DP,   0.210974_DP/)
    iZ =   33
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -2.984117_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.025642_DP,   4.503441_DP,   3.715904_DP,   3.937200_DP,   6.790175_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.597739_DP,   0.003012_DP,  14.272119_DP,  50.437996_DP,   0.193015_DP/)
    iZ =   34
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -3.160982_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.354071_DP,   4.653248_DP,   4.259489_DP,   4.136455_DP,   6.749163_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.349787_DP,   0.002550_DP,  15.579460_DP,  45.181202_DP,   0.177432_DP/)
    iZ =   35
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -2.492088_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.550570_DP,   5.411882_DP,   3.937180_DP,   3.880645_DP,   6.707793_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.119226_DP,  16.557184_DP,   0.002481_DP,  42.164009_DP,   0.162121_DP/)
    iZ =   36
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -2.810592_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.655279_DP,   6.848105_DP,   4.171004_DP,   3.446760_DP,   6.685200_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.908231_DP,  16.606236_DP,   0.001598_DP,  39.917473_DP,   0.146896_DP/)
    iZ =   37
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.139548_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   8.123134_DP,   2.138042_DP,   6.761702_DP,   1.156051_DP,  17.679546_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  15.142385_DP,  33.542667_DP,   0.129372_DP, 224.132507_DP,   1.713368_DP/)
    iZ =   38
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.140251_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.730219_DP,   9.795867_DP,   6.099763_DP,   2.620025_DP,   0.600053_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.563060_DP,  14.310868_DP,   0.120574_DP, 135.771317_DP,   0.120574_DP/)
    iZ =   39
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.131787_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.792040_DP,  10.253252_DP,   5.714949_DP,   3.170516_DP,   0.918251_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.429691_DP,  13.132816_DP,   0.112173_DP, 108.197029_DP,   0.112173_DP/)
    iZ =   40
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.124859_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.859772_DP,  10.911038_DP,   5.821115_DP,   3.512513_DP,   0.746965_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.310692_DP,  12.319285_DP,   0.104353_DP,  91.777542_DP,   0.104353_DP/)
    iZ =   41
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.123452_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.958399_DP,  12.063054_DP,   5.007015_DP,   3.287667_DP,   1.531019_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.211590_DP,  12.246687_DP,   0.098615_DP,  75.011948_DP,   0.098615_DP/)
    iZ =   42
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.108770_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.236218_DP,  17.987711_DP,  12.973127_DP,   3.451426_DP,   0.210899_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.090780_DP,   1.108310_DP,  11.468720_DP,  66.684151_DP,   0.090780_DP/)
    iZ =   43
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.074784_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.840963_DP,   3.428236_DP,   1.373012_DP,  12.947364_DP,   6.335469_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.005729_DP,  41.901382_DP, 119.320541_DP,   9.781542_DP,   0.083391_DP/)
    iZ =   44
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.043992_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.271624_DP,  17.906738_DP,  14.123269_DP,   3.746008_DP,   0.908235_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.077040_DP,   0.928222_DP,   9.555345_DP,  35.860680_DP, 123.552246_DP/)
    iZ =   45
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.995452_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.216648_DP,  17.919739_DP,   3.854252_DP,   0.840326_DP,  15.173498_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.070789_DP,   0.856121_DP,  33.889484_DP, 121.686691_DP,   9.029517_DP/)
    iZ =   46
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.883099_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.121511_DP,   4.784063_DP,  16.631683_DP,   4.318258_DP,  13.246773_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.062549_DP,   0.784031_DP,   8.751391_DP,  34.489983_DP,   0.784031_DP/)
    iZ =   47
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.756603_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.073874_DP,  17.155437_DP,   4.173344_DP,   0.852238_DP,  17.988686_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.055333_DP,   7.896512_DP,  28.443739_DP, 110.376106_DP,   0.716809_DP/)
    iZ =   48
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.603504_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.080986_DP,  18.019468_DP,   4.018197_DP,   1.303510_DP,  17.974669_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.048990_DP,   7.273646_DP,  29.119284_DP,  95.831207_DP,   0.661231_DP/)
    iZ =   49
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.333097_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.196477_DP,  18.816183_DP,   4.050479_DP,   1.638929_DP,  17.962912_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.042072_DP,   6.695665_DP,  31.009790_DP, 103.284348_DP,   0.610714_DP/)
    iZ =   50
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.119024_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.325171_DP,   6.281571_DP,   4.498866_DP,   1.856934_DP,  17.917318_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.118104_DP,   0.036915_DP,  32.529045_DP,  95.037186_DP,   0.565651_DP/)
    iZ =   51
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.290506_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   5.394956_DP,   6.549570_DP,  19.650681_DP,   1.827820_DP,  17.867832_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  33.326523_DP,   0.030974_DP,   5.564929_DP,  87.130966_DP,   0.523992_DP/)
    iZ =   52
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.806668_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.660302_DP,   6.940756_DP,  19.847015_DP,   1.557175_DP,  17.802427_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  33.031654_DP,   0.025750_DP,   5.065547_DP,  84.101616_DP,   0.487660_DP/)
    iZ =   53
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.448811_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.884502_DP,   6.736593_DP,   8.110516_DP,   1.170953_DP,  17.548716_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.628591_DP,   0.027754_DP,  31.849096_DP,  84.406387_DP,   0.463550_DP/)
    iZ =   54
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -6.065902_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.978920_DP,  11.774945_DP,   9.332182_DP,   1.244749_DP,  17.737501_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.143356_DP,   0.010142_DP,  28.796200_DP,  75.280685_DP,   0.413616_DP/)
    iZ =   55
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -2.322802_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.418674_DP,   8.314444_DP,  10.323193_DP,   1.383834_DP,  19.876251_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.399828_DP,   0.016872_DP,  25.605827_DP, 233.339676_DP,   3.826915_DP/)
    iZ =   56
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -5.183497_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.747343_DP,  17.368477_DP,  10.465718_DP,   2.592602_DP,  11.003653_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.481823_DP,   0.371224_DP,  21.226641_DP, 173.834274_DP,   0.010719_DP/)
    iZ =   57
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -21.745489_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.966019_DP,  27.329655_DP,  11.018425_DP,   3.086696_DP,  17.335455_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.197408_DP,   0.003446_DP,  19.955492_DP, 141.381973_DP,   0.341817_DP/)
    iZ =   58
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -38.386017_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.355122_DP,  43.988499_DP,  20.546650_DP,   3.130670_DP,  11.353665_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.328369_DP,   0.002047_DP,   3.088196_DP, 134.907654_DP,  18.832960_DP/)
    iZ =   59
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -3.871068_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  21.551311_DP,  17.161730_DP,  11.903859_DP,   2.679103_DP,   9.564197_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.995675_DP,   0.312491_DP,  17.716705_DP, 152.192825_DP,   0.010468_DP/)
    iZ =   60
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -57.189842_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.331244_DP,  62.783924_DP,  12.160097_DP,   2.663483_DP,  22.239950_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.300269_DP,   0.001320_DP,  17.026001_DP, 148.748993_DP,   2.910268_DP/)
    iZ =   61
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -45.973682_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.286388_DP,  51.560162_DP,  12.478557_DP,   2.675515_DP,  22.960947_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.286620_DP,   0.001550_DP,  16.223755_DP, 143.984512_DP,   2.796480_DP/)
    iZ =   62
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -17.452166_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  23.700363_DP,  23.072214_DP,  12.777782_DP,   2.684217_DP,  17.204367_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.689539_DP,   0.003491_DP,  15.495437_DP, 139.862473_DP,   0.274536_DP/)
    iZ =   63
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -31.586687_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.186195_DP,  37.156837_DP,  13.103387_DP,   2.707246_DP,  24.419271_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.261678_DP,   0.001995_DP,  14.787360_DP, 134.816299_DP,   2.581883_DP/)
    iZ =   64
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -43.505684_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  24.898117_DP,  17.104952_DP,  13.222581_DP,   3.266152_DP,  48.995213_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.435028_DP,   0.246961_DP,  13.996325_DP, 110.863091_DP,   0.001383_DP/)
    iZ =   65
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -26.851971_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  25.910013_DP,  32.344139_DP,  13.765117_DP,   2.751404_DP,  17.064405_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.373912_DP,   0.002034_DP,  13.481969_DP, 125.836510_DP,   0.236916_DP/)
    iZ =   66
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -83.279831_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  26.671785_DP,  88.687576_DP,  14.065445_DP,   2.768497_DP,  17.067781_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.282593_DP,   0.000665_DP,  12.920230_DP, 121.937187_DP,   0.225531_DP/)
    iZ =   67
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -41.165253_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  27.150190_DP,  16.999819_DP,  14.059334_DP,   3.386979_DP,  46.546471_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.169660_DP,   0.215414_DP,  12.213148_DP, 100.506783_DP,   0.001211_DP/)
    iZ =   68
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -77.135223_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.174887_DP,  82.493271_DP,  14.624002_DP,   2.802756_DP,  17.018515_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.120995_DP,   0.000640_DP,  11.915256_DP, 114.529938_DP,   0.207519_DP/)
    iZ =   69
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -70.839813_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.925894_DP,  76.173798_DP,  14.904704_DP,   2.814812_DP,  16.998117_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.046203_DP,   0.000656_DP,  11.465375_DP, 111.411980_DP,   0.199376_DP/)
    iZ =   70
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -60.313812_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  29.676760_DP,  65.624069_DP,  15.160854_DP,   2.830288_DP,  16.997850_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.977630_DP,   0.000720_DP,  11.044622_DP, 108.139153_DP,   0.192110_DP/)
    iZ =   71
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -51.049416_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  30.122866_DP,  15.099346_DP,  56.314899_DP,   3.540980_DP,  16.943729_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.883090_DP,  10.342764_DP,   0.000780_DP,  89.559250_DP,   0.183849_DP/)
    iZ =   72
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -49.719837_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  30.617033_DP,  15.145351_DP,  54.933548_DP,   4.096253_DP,  16.896156_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.795613_DP,   9.934469_DP,   0.000739_DP,  76.189705_DP,   0.175914_DP/)
    iZ =   73
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -44.119026_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.066359_DP,  15.341823_DP,  49.278297_DP,   4.577665_DP,  16.828321_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.708732_DP,   9.618455_DP,   0.000760_DP,  66.346199_DP,   0.168002_DP/)
    iZ =   74
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -32.864574_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.507900_DP,  15.682498_DP,  37.960129_DP,   4.885509_DP,  16.792112_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.629485_DP,   9.446448_DP,   0.000898_DP,  59.980675_DP,   0.160798_DP/)
    iZ =   75
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -37.412682_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.888456_DP,  16.117104_DP,  42.390297_DP,   5.211669_DP,  16.767591_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.549238_DP,   9.233474_DP,   0.000689_DP,  54.516373_DP,   0.152815_DP/)
    iZ =   76
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -43.677956_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.210297_DP,  16.678440_DP,  48.559906_DP,   5.455839_DP,  16.735533_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.473531_DP,   9.049695_DP,   0.000519_DP,  50.210201_DP,   0.145771_DP/)
    iZ =   77
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.018893_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.004436_DP,   1.975454_DP,  17.070105_DP,  15.939454_DP,   5.990003_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.353767_DP,  81.014175_DP,   0.128093_DP,   7.661196_DP,  26.659403_DP/)
    iZ =   78
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.050394_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.273891_DP,  18.445440_DP,  17.063745_DP,   5.555933_DP,   1.575270_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.316992_DP,   8.797154_DP,   0.124741_DP,  40.177994_DP,   1.316997_DP/)
    iZ =   79
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -6.279078_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.777390_DP,  19.317156_DP,  32.979683_DP,   5.595453_DP,  10.576854_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.122737_DP,   8.621570_DP,   1.256902_DP,  38.008820_DP,   0.000601_DP/)
    iZ =   80
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.076478_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.839890_DP,  20.023823_DP,  28.428564_DP,   5.881564_DP,   4.714706_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.115905_DP,   8.256927_DP,   1.195250_DP,  39.247227_DP,   1.195250_DP/)
    iZ =   81
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.066939_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.630795_DP,  19.386616_DP,  32.808571_DP,   1.747191_DP,   6.356862_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.110704_DP,   7.181401_DP,   1.119730_DP,  90.660263_DP,  26.014978_DP/)
    iZ =   82
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.049824_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.419567_DP,  32.738590_DP,   6.530247_DP,   2.342742_DP,  19.916475_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.105499_DP,   1.055049_DP,  25.025890_DP,  80.906593_DP,   6.664449_DP/)
    iZ =   83
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.040914_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.282274_DP,  32.725136_DP,   6.678302_DP,   2.694750_DP,  20.576559_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.101180_DP,   1.002287_DP,  25.714146_DP,  77.057549_DP,   6.291882_DP/)
    iZ =   84
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.046556_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.289164_DP,  32.807171_DP,  21.095163_DP,   2.505901_DP,   7.254589_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.098121_DP,   0.966265_DP,   6.046622_DP,  76.598068_DP,  28.096128_DP/)
    iZ =   85
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.995684_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.011461_DP,  32.615547_DP,   8.113899_DP,   2.884082_DP,  21.377867_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.092639_DP,   0.904416_DP,  26.543257_DP,  68.372963_DP,   5.499512_DP/)
    iZ =   86
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.020977_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.070229_DP,  32.641106_DP,  21.489658_DP,   2.299218_DP,   9.480184_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.090437_DP,   0.876409_DP,   5.239687_DP,  69.188477_DP,  27.632641_DP/)
    iZ =   87
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.003472_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.007385_DP,  32.663830_DP,  21.594351_DP,   1.598497_DP,  11.121192_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.087031_DP,   0.840187_DP,   4.954467_DP, 199.805801_DP,  26.905106_DP/)
    iZ =   88
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.981773_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.563690_DP,  21.396671_DP,  11.298093_DP,   2.834688_DP,  15.914965_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.801980_DP,   4.590666_DP,  22.758972_DP, 160.404388_DP,   0.083544_DP/)
    iZ =   89
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.939212_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.914053_DP,  32.535042_DP,  21.553976_DP,  11.433394_DP,   3.612409_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.080511_DP,   0.770669_DP,   4.352206_DP,  21.381622_DP, 130.500748_DP/)
    iZ =   90
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.922533_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.784024_DP,  32.454899_DP,  21.849222_DP,   4.239077_DP,  11.736191_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.077067_DP,   0.735137_DP,   4.097976_DP, 109.464111_DP,  20.512138_DP/)
    iZ =   91
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.886066_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.740208_DP,  21.973675_DP,  12.957398_DP,   3.683832_DP,  15.744058_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.709545_DP,   4.050881_DP,  19.231543_DP, 117.255005_DP,   0.074040_DP/)
    iZ =   92
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.854444_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.679275_DP,  32.824306_DP,  13.660459_DP,   3.687261_DP,  22.279434_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.071206_DP,   0.681177_DP,  18.236156_DP, 112.500038_DP,   3.930325_DP/)
    iZ =   93
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.769391_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.999901_DP,  22.638077_DP,  14.219973_DP,   3.672950_DP,  15.683245_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.657086_DP,   3.854918_DP,  17.435474_DP, 109.464485_DP,   0.068033_DP/)
    iZ =   94
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.664200_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  33.281178_DP,  23.148544_DP,  15.153755_DP,   3.031492_DP,  15.704215_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.634999_DP,   3.856168_DP,  16.849735_DP, 121.292038_DP,   0.064857_DP/)
    iZ =   95
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.541160_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  33.435162_DP,  23.657259_DP,  15.576339_DP,   3.027023_DP,  15.746100_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.612785_DP,   3.792942_DP,  16.195778_DP, 117.757004_DP,   0.061755_DP/)
    iZ =   96
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.390840_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.804837_DP,  33.480801_DP,  24.150198_DP,   3.655563_DP,  15.499866_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.058619_DP,   0.590160_DP,   3.674720_DP, 100.736191_DP,  15.408296_DP/)
    iZ =   97
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.213169_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.889072_DP,  33.625286_DP,  24.710381_DP,   3.707139_DP,  15.839268_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.055503_DP,   0.569571_DP,   3.615472_DP,  97.694786_DP,  14.754303_DP/)
    iZ =   98
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.005326_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  33.794075_DP,  25.467693_DP,  16.048487_DP,   3.657525_DP,  16.008982_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.550447_DP,   3.581973_DP,  14.357388_DP,  96.064972_DP,   0.052450_DP/)

!______________ FINISHED NEUTRAL ATOMS; DOING IONS

    iZ =    1
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "H1- "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.000425_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.702260_DP,   0.763666_DP,   0.248678_DP,   0.261323_DP,   0.023017_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  23.945604_DP,  74.897919_DP,   6.773289_DP, 233.583450_DP,   1.337531_DP/)
    iZ =    3
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Li1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.001764_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.432724_DP,   0.549257_DP,   0.376575_DP,  -0.336481_DP,   0.976060_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.260367_DP,   1.042836_DP,   7.885294_DP,   0.260368_DP,   3.042539_DP/)

    iZ =    4
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Be2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.653773_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.055430_DP,  -2.372617_DP,   1.044914_DP,   0.544233_DP,   0.381737_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.001226_DP,   0.001227_DP,   1.542106_DP,   0.456279_DP,   4.047479_DP/)

    iZ =    4
    icha_ion =   -2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cval"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.019722_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.258489_DP,   0.728215_DP,   1.119856_DP,   2.168133_DP,   0.705239_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  10.683769_DP,   0.208177_DP,   0.836097_DP,  24.603704_DP,  58.954273_DP/)

!_____ New entries
!_____ Core-only form factors for light covalent elements

    iZ =    5
    icha_ion =   3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.
    at_xscat(iZ)%ion_name(icha_ion) = "B3+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    -6.10875_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ 6.260585d0,  0.887678d0,  0.797078d0,  0.163403d0,   0.d0/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/ 0.001637d0,  0.522327d0,  1.400067d0,  3.198460d0,   58.954273_DP/)
    
    iZ =    6
    icha_ion =   4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.
    at_xscat(iZ)%ion_name(icha_ion) = "C4+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    -6.10794_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ 6.261320d0,  0.890229d0,  0.795518d0,  0.160751d0,   0.d0/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/ 0.001618d0,  0.345630d0,  0.966600d0,  2.121265d0,   58.954273_DP/)
    
    iZ =    7
    icha_ion =   5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.
    at_xscat(iZ)%ion_name(icha_ion) = "N5+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    -6.10925_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ 6.259986d0,  0.888784d0,  0.791351d0,  0.168628d0,   0.d0/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/ 0.002260d0,  0.267246d0,  0.934512d0,  0.188306d0,   58.954273_DP/)
    
    iZ =    8
    icha_ion =   6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.
    at_xscat(iZ)%ion_name(icha_ion) = "O6+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    -6.10893_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ 6.260308d0,  0.889621d0,  0.789114d0,  0.169520d0,   0.d0/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/ 0.002579d0,  0.195482d0,  0.716123d0,  0.120777d0,   58.954273_DP/)
    
    iZ =    9
    icha_ion =   7
    at_xscat(iZ)%exist_ion(icha_ion) = .true.
    at_xscat(iZ)%ion_name(icha_ion) = "F7+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    -6.10865_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ 6.260595d0,  0.890250d0,  0.787361d0,  0.170154d0,   0.d0/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/ 0.002449d0,  0.149871d0,  0.564412d0,  0.087221d0,   58.954273_DP/)


!_____ New entries - END

    iZ =    8
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "O1- "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.046136_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.106934_DP,   3.235142_DP,   1.148886_DP,   0.783981_DP,   0.676953_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  19.868080_DP,   6.960252_DP,   0.170043_DP,  65.693512_DP,   0.630757_DP/)
    iZ =    8
    icha_ion =   -2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "O2- "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.025429_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.990247_DP,   2.300563_DP,   0.607200_DP,   1.907882_DP,   1.167080_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  16.639956_DP,   5.636819_DP,   0.108493_DP,  47.299709_DP,   0.379984_DP/)
    iZ =    9
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "F1- "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.069525_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   0.457649_DP,   3.841561_DP,   1.432771_DP,   0.801876_DP,   3.395041_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.917243_DP,   5.507803_DP,   0.164955_DP,  51.076206_DP,  15.821679_DP/)
    iZ =   11
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Na1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.045300_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.148690_DP,   4.073989_DP,   0.767888_DP,   0.995612_DP,   0.968249_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.594987_DP,   6.046925_DP,   0.070139_DP,  14.122657_DP,   0.217037_DP/)
    iZ =   12
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mg2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.058851_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.062918_DP,   4.135106_DP,   0.853742_DP,   1.036792_DP,   0.852520_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.015803_DP,   4.417941_DP,   0.065307_DP,   9.669710_DP,   0.187818_DP/)
    iZ =   13
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Al3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.019397_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.132015_DP,   0.912049_DP,   1.102425_DP,   0.614876_DP,   3.219136_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.528641_DP,   7.378344_DP,   0.133708_DP,   0.039065_DP,   1.644728_DP/)
    iZ =   14
    icha_ion =    0
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "    "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.146030_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   2.879033_DP,   3.072960_DP,   1.515981_DP,   1.390030_DP,   4.995051_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.239713_DP,  38.706276_DP,   0.081481_DP,  93.616333_DP,   2.770293_DP/)
    iZ =   14
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Si4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.097266_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   3.676722_DP,   3.828496_DP,   1.258033_DP,   0.419024_DP,   0.720421_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.446851_DP,   3.013144_DP,   0.064397_DP,   0.206254_DP,   5.970222_DP/)
    iZ =   17
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cl1-"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -34.916603_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   1.061802_DP,   7.139886_DP,   6.524271_DP,   2.355626_DP,  35.829403_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.144727_DP,   1.171795_DP,  19.467655_DP,  60.320301_DP,   0.000436_DP/)
    iZ =   19
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "K1+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.257164_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/ -17.609339_DP,   1.494873_DP,   7.150305_DP,  10.899569_DP,  15.808228_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  18.840979_DP,   0.053453_DP,   0.812940_DP,  22.264105_DP,  14.351593_DP/)
    iZ =   20
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ca2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -21.013187_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   8.501441_DP,  12.880483_DP,   9.765095_DP,   7.156669_DP,   0.711160_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  10.525848_DP,  -0.004033_DP,   0.010692_DP,   0.684443_DP,  27.231771_DP/)
    iZ =   21
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sc3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.118642_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.104348_DP,   1.511488_DP, -53.669773_DP,  38.404816_DP,  24.532240_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.601957_DP,   0.033386_DP,  12.572138_DP,  10.859736_DP,  14.125230_DP/)
    iZ =   22
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ti2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.150362_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.040119_DP,   1.496285_DP,   9.657304_DP,   0.006534_DP,   1.649561_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.537072_DP,   0.031914_DP,   8.009958_DP, 201.800293_DP,  24.039482_DP/)
    iZ =   22
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ti3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -35.111282_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  36.587933_DP,   7.230255_DP,  -9.086077_DP,   2.084594_DP,  17.294008_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.000681_DP,   0.522262_DP,   5.262317_DP,  15.881716_DP,   6.149805_DP/)
    iZ =   22
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ti4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.110628_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  45.355537_DP,   7.092900_DP,   7.483858_DP, -43.498817_DP,   1.678915_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   9.252186_DP,   0.523046_DP,  13.082852_DP,  10.193876_DP,   0.023064_DP/)
    iZ =   23
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "V2+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.533379_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.754356_DP,   2.064100_DP,   2.576998_DP,   2.011404_DP,   7.126177_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   7.066315_DP,   0.014993_DP,   7.066308_DP,  22.055786_DP,   0.467568_DP/)
    iZ =   23
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "V3+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.474921_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   9.958480_DP,   1.596350_DP,   1.483442_DP, -10.846044_DP,  17.332867_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.763041_DP,   0.056895_DP,  17.750029_DP,   0.328826_DP,   0.388013_DP/)
    iZ =   23
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "V5+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.552676_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.575018_DP,   8.448095_DP,   1.612040_DP,  -9.721855_DP,   1.534029_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.682708_DP,   5.566640_DP,  10.527077_DP,   0.907961_DP,   0.066667_DP/)
    iZ =   24
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cr2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.049870_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  10.598877_DP,   1.565858_DP,   2.728280_DP,   0.098064_DP,   6.959321_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.151846_DP,   0.023519_DP,  17.432816_DP,  54.002388_DP,   0.426301_DP/)
    iZ =   24
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cr3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.192123_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.989310_DP,   1.765079_DP,   2.627125_DP,   1.829380_DP,   6.980908_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.068867_DP,   0.018342_DP,   6.068887_DP,  16.309284_DP,   0.420864_DP/)
    iZ =   25
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mn2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -24.566132_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.287712_DP,  26.042414_DP,   3.058096_DP,   0.090258_DP,   7.088306_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   5.506225_DP,   0.000774_DP,  16.158575_DP,  54.766354_DP,   0.375580_DP/)
    iZ =   25
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mn3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.093713_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.926972_DP,   2.081342_DP,  11.128379_DP,   2.375107_DP,  -0.419287_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.378315_DP,   0.015054_DP,   5.379957_DP,  14.429586_DP,   0.004939_DP/)
    iZ =   25
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mn4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.672146_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  12.409131_DP,   7.466993_DP,   1.809947_DP, -12.138477_DP,  10.780248_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.300400_DP,   0.112814_DP,  12.520756_DP,   0.168653_DP,   5.173237_DP/)
    iZ =   26
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Fe2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -9.676919_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.776765_DP,  11.165097_DP,   3.533495_DP,   0.165345_DP,   7.036932_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.912232_DP,   0.001748_DP,  14.166556_DP,  42.381958_DP,   0.341324_DP/)
    iZ =   26
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Fe3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -61.930725_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   9.721638_DP,  63.403847_DP,   2.141347_DP,   2.629274_DP,   7.033846_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.869297_DP,   0.000293_DP,   4.867602_DP,  13.539076_DP,   0.338520_DP/)
    iZ =   27
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Co2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -24.796852_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.993840_DP,  26.285812_DP,  12.254289_DP,   0.246114_DP,   4.017407_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.310779_DP,   0.000684_DP,   4.400528_DP,  35.741447_DP,  12.536393_DP/)
    iZ =   27
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Co3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -1.147345_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.861739_DP,   2.678570_DP,  12.281889_DP,   3.501741_DP,  -0.179384_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.309794_DP,   0.008142_DP,   4.331703_DP,  11.914167_DP,  11.914167_DP/)
    iZ =   28
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ni2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -36.344471_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  12.519017_DP,  37.832058_DP,   4.387257_DP,   0.661552_DP,   6.949072_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.933053_DP,   0.000442_DP,  10.449184_DP,  23.860998_DP,   0.283723_DP/)
    iZ =   28
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ni3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.317618_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  13.579366_DP,   1.902844_DP,  12.859268_DP,   3.811005_DP,  -6.838595_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.313140_DP,   0.012621_DP,   3.906407_DP,  10.894311_DP,   0.344379_DP/)
    iZ =   29
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cu1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -14.849320_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  12.960763_DP,  16.342150_DP,   1.110102_DP,   5.520682_DP,   6.915452_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.576010_DP,   0.000975_DP,  29.523218_DP,  10.114283_DP,   0.261326_DP/)
    iZ =   29
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cu2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -14.878383_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  11.895569_DP,  16.344978_DP,   5.799817_DP,   1.048804_DP,   6.789088_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.378519_DP,   0.000924_DP,   8.133653_DP,  20.526524_DP,   0.254741_DP/)
    iZ =   30
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Zn2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -8.945248_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  13.340772_DP,  10.428857_DP,   5.544489_DP,   0.762295_DP,   6.869172_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.215913_DP,   0.001413_DP,   8.542680_DP,  21.891756_DP,   0.239215_DP/)
    iZ =   31
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ga3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -33.875122_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  13.123875_DP,  35.288189_DP,   6.126979_DP,   0.611551_DP,   6.724807_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.809960_DP,   0.000323_DP,   6.831534_DP,  16.784311_DP,   0.212002_DP/)
    iZ =   32
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ge4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.086542_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.876636_DP,   6.779091_DP,   9.969591_DP,   3.135857_DP,   0.152389_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.025174_DP,   0.176650_DP,   3.573822_DP,   7.685848_DP,  16.677574_DP/)
    iZ =   35
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Br1-"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.152674_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.714310_DP,   6.466926_DP,   6.947385_DP,   4.402674_DP,  -0.697279_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.122554_DP,  19.050768_DP,   0.152708_DP,  58.690361_DP,  58.690372_DP/)
    iZ =   37
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Rb1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.133263_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.684320_DP,   7.761588_DP,   6.680874_DP,   2.668883_DP,   0.070974_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.710209_DP,  14.919863_DP,   0.128542_DP,  31.654478_DP,   0.128543_DP/)
    iZ =   38
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sr2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.125309_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.694973_DP,   1.275762_DP,   6.154252_DP,   9.234786_DP,   0.515995_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.550888_DP,  30.133041_DP,   0.118774_DP,  13.821799_DP,   0.118774_DP/)
    iZ =   39
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Y3+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   19.023842_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  46.660366_DP,  10.369686_DP,   4.623042_DP, -62.170834_DP,  17.471146_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/  -0.019971_DP,  13.180257_DP,   0.176398_DP,  -0.016727_DP,   1.467348_DP/)
    iZ =   40
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Zr4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.827902_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.802956_DP,  17.699253_DP,  10.650647_DP,  -0.248108_DP,   0.250338_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.096228_DP,   1.296127_DP,  11.240715_DP,  -0.219259_DP,  -0.219021_DP/)
    iZ =   41
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Nb3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -8.339573_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.714323_DP,   1.675213_DP,   7.483963_DP,   8.322464_DP,  11.143573_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.172419_DP,  30.102791_DP,   0.080255_DP,  -0.002983_DP,  10.456687_DP/)
    iZ =   41
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Nb5+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -68.024780_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.580206_DP,   7.633277_DP,  10.793497_DP,   0.180884_DP,  67.837921_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.165852_DP,   0.078558_DP,   9.507652_DP,  31.621656_DP,  -0.000438_DP/)
    iZ =   42
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mo3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -1.898764_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.447050_DP,  17.778122_DP,  11.886068_DP,   1.997905_DP,   1.789626_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.072000_DP,   1.073145_DP,   9.834720_DP,  28.221746_DP,  -0.011674_DP/)
    iZ =   42
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mo5+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -78.056595_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   7.929879_DP,  17.667669_DP,  11.515987_DP,   0.500402_DP,  77.444084_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.068856_DP,   1.068064_DP,   9.046229_DP,  26.558945_DP,  -0.000473_DP/)
    iZ =   42
    icha_ion =    6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Mo6+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.141916_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  34.757683_DP,   9.653037_DP,   6.584769_DP, -18.628115_DP,   2.490594_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.301770_DP,   7.123843_DP,   0.094097_DP,   1.617443_DP,  12.335434_DP/)
    iZ =   44
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ru3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -51.905243_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.894758_DP,  13.579529_DP,  10.729251_DP,   2.474095_DP,  48.227997_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.902827_DP,   8.740579_DP,   0.045125_DP,  24.764954_DP,  -0.001699_DP/)
    iZ =   44
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ru4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -17.241762_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.845776_DP,  13.455084_DP,  10.229087_DP,   1.653524_DP,  14.059795_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.901070_DP,   8.482392_DP,   0.045972_DP,  23.015272_DP,  -0.004889_DP/)
    iZ =   45
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Rh3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.960843_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.758621_DP,  14.569813_DP,   5.298320_DP,   2.533579_DP,   0.879753_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.841779_DP,   8.319533_DP,   0.069050_DP,  23.709131_DP,   0.069050_DP/)
    iZ =   45
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Rh4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.959941_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.716188_DP,  14.446654_DP,   5.185801_DP,   1.703448_DP,   0.989992_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.840572_DP,   8.100647_DP,   0.068995_DP,  22.357307_DP,   0.068995_DP/)
    iZ =   46
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pd2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.879336_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.122282_DP,  15.651012_DP,   3.513508_DP,   9.060790_DP,   8.771199_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.062424_DP,   8.018296_DP,  24.784275_DP,   0.776457_DP,   0.776457_DP/)
    iZ =   46
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pd4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.915874_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.152421_DP, -96.069023_DP,  31.622141_DP,  81.578255_DP,  17.801403_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.063951_DP,  11.090354_DP,  13.466152_DP,   9.758302_DP,   0.783014_DP/)
    iZ =   47
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ag1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.785127_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.091192_DP,   4.019526_DP,  16.948174_DP,   4.258638_DP,  13.889437_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.056305_DP,   0.719340_DP,   7.758938_DP,  27.368349_DP,   0.719340_DP/)
    iZ =   47
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ag2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.068247_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.401808_DP,  48.699802_DP,   4.799859_DP, -32.332523_DP,  16.356710_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.068167_DP,   0.942270_DP,  20.639496_DP,   1.100365_DP,   6.883131_DP/)
    iZ =   48
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cd2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.664795_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.093711_DP,  43.909691_DP,  17.041306_DP, -39.675117_DP,  17.958918_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.050624_DP,   8.654143_DP,  15.621396_DP,  11.082067_DP,   0.667591_DP/)
    iZ =   49
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "In3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    0.293677_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.206277_DP,  18.497746_DP,   3.078131_DP,  10.524613_DP,   7.401234_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.041357_DP,   6.605563_DP,  18.792250_DP,   0.608082_DP,   0.608082_DP/)
    iZ =   50
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sn2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.042519_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   6.353672_DP,   4.770377_DP,  14.672025_DP,   4.235959_DP,  18.002131_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.034720_DP,   6.167891_DP,   6.167879_DP,  29.006456_DP,   0.561774_DP/)
    iZ =   50
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sn4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.172219_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.445732_DP,   6.420892_DP,   4.562980_DP,   1.713385_DP,  18.033537_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   6.280898_DP,   0.033144_DP,   6.280899_DP,  17.983601_DP,   0.557980_DP/)
    iZ =   51
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sb3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    1.516108_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  10.189171_DP,  57.461918_DP,  19.356573_DP,   4.862206_DP, -45.394096_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.089485_DP,   0.375256_DP,   5.357987_DP,  22.153736_DP,   0.297768_DP/)
    iZ =   51
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sb5+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -0.445371_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.920622_DP,   6.647932_DP,  12.724075_DP,   1.555545_DP,   7.600591_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.522315_DP,   0.029487_DP,   5.718210_DP,  16.433775_DP,   5.718204_DP/)
    iZ =   53
    icha_ion =   -1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "I1- "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -3.341004_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  20.010330_DP,  17.835524_DP,   8.104130_DP,   2.231118_DP,   9.158548_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   4.565931_DP,   0.444266_DP,  32.430672_DP,  95.149040_DP,   0.014906_DP/)
    iZ =   55
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Cs1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -19.394306_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.939056_DP,  24.967621_DP,  10.375884_DP,   0.454243_DP,  17.660248_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.770511_DP,   0.004040_DP,  25.311275_DP,  76.537766_DP,   0.384730_DP/)
    iZ =   56
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ba2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -59.618172_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.750200_DP,  17.513683_DP,  10.884892_DP,   0.321585_DP,  65.149834_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.430748_DP,   0.361590_DP,  21.358307_DP,  70.309402_DP,   0.001418_DP/)
    iZ =   57
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "La3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -76.846909_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  19.688887_DP,  17.345703_DP,  11.356296_DP,   0.099418_DP,  82.358124_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.146211_DP,   0.339586_DP,  18.753832_DP,  90.345459_DP,   0.001072_DP/)
    iZ =   58
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ce3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -80.313423_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  26.593231_DP,  85.866432_DP,  -6.677695_DP,  12.111847_DP,  17.401903_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   3.280381_DP,   0.001012_DP,   4.313575_DP,  17.868504_DP,   0.326962_DP/)
    iZ =   58
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ce4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -3.515096_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.457533_DP,  25.659941_DP,  11.691037_DP,  19.695251_DP, -16.994749_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.311812_DP,  -0.003793_DP,  16.568687_DP,   2.886395_DP,  -0.008931_DP/)
    iZ =   59
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pr3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -30.500784_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  20.879841_DP,  36.035797_DP,  12.135341_DP,   0.283103_DP,  17.167803_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.870897_DP,   0.002364_DP,  16.615236_DP,  53.909359_DP,   0.306993_DP/)
    iZ =   59
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pr4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -9.016722_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.496082_DP,  21.538509_DP,  20.403114_DP,  12.062211_DP,  -7.492043_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.294457_DP,  -0.002742_DP,   2.772886_DP,  15.804613_DP,  -0.013556_DP/)
    iZ =   60
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Nd3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -50.541992_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.120077_DP,  56.038139_DP,  21.468307_DP,  10.000671_DP,   2.905866_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.291295_DP,   0.001421_DP,   2.743681_DP,  14.581367_DP,  22.485098_DP/)
    iZ =   61
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pm3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -46.767181_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  22.221066_DP,  17.068142_DP,  12.805423_DP,   0.435687_DP,  52.238770_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.635767_DP,   0.277039_DP,  14.927315_DP,  45.768017_DP,   0.001455_DP/)
    iZ =   62
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Sm3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -9.714854_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.618565_DP,  19.538092_DP,  13.398946_DP,  -4.358811_DP,  24.490461_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.006001_DP,   0.306379_DP,  14.979594_DP,   0.748825_DP,   2.454492_DP/)
    iZ =   63
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Eu2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -26.204315_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  23.899035_DP,  31.657497_DP,  12.955752_DP,   1.700576_DP,  16.992199_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.467332_DP,   0.002230_DP,  13.625002_DP,  35.089481_DP,   0.253136_DP/)
    iZ =   63
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Eu3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -19.768026_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.758327_DP,  33.498665_DP,  24.067188_DP,  13.436883_DP,  -9.019134_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.244474_DP,  -0.003901_DP,   2.487526_DP,  14.568011_DP,  -0.015628_DP/)
    iZ =   64
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Gd3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -88.147179_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  24.344999_DP,  16.945311_DP,  13.866931_DP,   0.481674_DP,  93.506378_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.333971_DP,   0.239215_DP,  12.982995_DP,  43.876347_DP,   0.000673_DP/)
    iZ =   65
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Tb3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -33.950317_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  24.878252_DP,  16.856016_DP,  13.663937_DP,   1.279671_DP,  39.271294_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   2.223301_DP,   0.227290_DP,  11.812528_DP,  29.910065_DP,   0.001527_DP/)
    iZ =   66
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Dy3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -85.150650_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.864344_DP,  90.383461_DP,  13.675473_DP,   1.687078_DP,  25.540651_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.216275_DP,   0.000593_DP,  11.121207_DP,  26.250975_DP,   2.135930_DP/)
    iZ =   67
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ho3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -58.026505_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.837524_DP,  63.221336_DP,  13.703766_DP,   2.061602_DP,  26.202621_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.206873_DP,   0.000796_DP,  10.500283_DP,  24.031883_DP,   2.055060_DP/)
    iZ =   68
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Er3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -17.513460_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.810127_DP,  22.681061_DP,  13.864114_DP,   2.294506_DP,  26.864477_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.198293_DP,   0.002126_DP,   9.973341_DP,  22.836388_DP,   1.979442_DP/)
    iZ =   69
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Tm3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -10.192087_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.787500_DP,  15.350905_DP,  14.182357_DP,   2.299111_DP,  27.573771_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.190852_DP,   0.003036_DP,   9.602934_DP,  22.526880_DP,   1.912862_DP/)
    iZ =   70
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Yb2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -23.214935_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.443794_DP,  16.849527_DP,  14.165081_DP,   3.445311_DP,  28.308853_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.863896_DP,   0.183811_DP,   9.225469_DP,  23.691355_DP,   0.001463_DP/)
    iZ =   70
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Yb3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -18.103676_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.191629_DP,  16.828087_DP,  14.167848_DP,   2.744962_DP,  23.171774_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.842889_DP,   0.182788_DP,   9.045957_DP,  20.799847_DP,   0.001759_DP/)
    iZ =   71
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Lu3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -20.626528_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.828693_DP,  16.823227_DP,  14.247617_DP,   3.079559_DP,  25.647667_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.776641_DP,   0.175560_DP,   8.575531_DP,  19.693701_DP,   0.001453_DP/)
    iZ =   72
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Hf4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -18.820383_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  29.267378_DP,  16.792543_DP,  14.785310_DP,   2.184128_DP,  23.791996_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.697911_DP,   0.168313_DP,   8.190025_DP,  18.277578_DP,   0.001431_DP/)
    iZ =   73
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ta5+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =  -11.542459_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  29.539469_DP,  16.741854_DP,  15.182070_DP,   1.642916_DP,  16.437447_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.612934_DP,   0.160460_DP,   7.654408_DP,  17.070732_DP,   0.001858_DP/)
    iZ =   74
    icha_ion =    6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "W6+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.945157_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  29.729357_DP,  17.247808_DP,  15.184488_DP,   1.154652_DP,   0.739335_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.501648_DP,   0.140803_DP,   6.880573_DP,  14.299601_DP,  14.299618_DP/)
    iZ =   76
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Os4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.988390_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  17.113485_DP,  15.792370_DP,  23.342392_DP,   4.090271_DP,   7.671292_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.131850_DP,   7.288542_DP,   1.389307_DP,  19.629425_DP,   1.389307_DP/)
    iZ =   77
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ir3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.009459_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.537575_DP,  16.363338_DP,  15.597141_DP,   5.051404_DP,   1.436935_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.334144_DP,   7.451918_DP,   0.127514_DP,  21.705648_DP,   0.127515_DP/)
    iZ =   77
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ir4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.006865_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  30.391249_DP,  16.146996_DP,  17.019068_DP,   4.458904_DP,   0.975372_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.328519_DP,   7.181766_DP,   0.127337_DP,  19.060146_DP,   1.328519_DP/)
    iZ =   78
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pt2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.032512_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.986849_DP,  17.249048_DP,  15.269374_DP,   5.760234_DP,   1.694079_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.281143_DP,   7.625512_DP,   0.123571_DP,  24.190826_DP,   0.123571_DP/)
    iZ =   78
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pt4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.094551_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  41.932713_DP,  16.339224_DP,  17.653894_DP,   6.012420_DP, -12.036877_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.111409_DP,   6.466086_DP,   0.128917_DP,  16.954155_DP,   0.778721_DP/)
    iZ =   79
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Au1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.040792_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.124306_DP,  16.716476_DP,  16.814100_DP,   7.311565_DP,   0.993064_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.216073_DP,   7.165378_DP,   0.118715_DP,  20.442486_DP,  53.095985_DP/)
    iZ =   79
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Au3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.042679_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  31.704271_DP,  17.545767_DP,  16.819551_DP,   5.522640_DP,   0.361725_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.215561_DP,   7.220506_DP,   0.118812_DP,  20.050970_DP,   1.215562_DP/)
    iZ =   80
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Hg1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.068430_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  28.866837_DP,  19.277540_DP,  16.776051_DP,   6.281459_DP,   3.710289_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.173967_DP,   7.583842_DP,   0.115351_DP,  29.055994_DP,   1.173968_DP/)
    iZ =   80
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Hg2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.052869_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.411079_DP,  18.690371_DP,  16.711773_DP,   9.974835_DP,  -3.847611_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.162980_DP,   7.329806_DP,   0.114518_DP,  22.009489_DP,  22.009493_DP/)
    iZ =   81
    icha_ion =    1
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Tl1+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.054030_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.295044_DP,  16.570049_DP,  17.991013_DP,   1.535355_DP,   7.554591_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.101544_DP,   0.110020_DP,   6.528559_DP,  52.495068_DP,  20.338634_DP/)
    iZ =   81
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Tl3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =   -9.256075_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.525639_DP,  19.139185_DP,  17.100321_DP,   5.891115_DP,  12.599463_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.094966_DP,   6.900992_DP,   0.103667_DP,  18.489614_DP,  -0.001401_DP/)
    iZ =   82
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pb2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.065623_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  27.392647_DP,  16.496822_DP,  19.984501_DP,   6.813923_DP,   5.233910_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.058874_DP,   0.106305_DP,   6.708123_DP,  24.395554_DP,   1.058874_DP/)
    iZ =   82
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pb4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.044678_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.505657_DP,  20.014240_DP,  14.645661_DP,   5.029499_DP,   1.760138_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   1.047035_DP,   6.670321_DP,   0.105279_DP,  16.525040_DP,   0.105279_DP/)
    iZ =   83
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Bi3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.043703_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  32.461437_DP,  19.438683_DP,  16.302486_DP,   7.322662_DP,   0.431704_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.997930_DP,   6.038867_DP,   0.101338_DP,  18.371586_DP,  46.361046_DP/)
    iZ =   83
    icha_ion =    5
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Bi5+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    4.113663_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  16.734028_DP,  20.580494_DP,   9.452623_DP,  61.155834_DP, -34.041023_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.105076_DP,   4.773282_DP,  11.762162_DP,   1.211775_DP,   1.619408_DP/)
    iZ =   88
    icha_ion =    2
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ra2+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.956572_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/   4.986228_DP,  32.474945_DP,  21.947443_DP,  11.800013_DP,  10.807292_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.082597_DP,   0.791468_DP,   4.608034_DP,  24.792431_DP,   0.082597_DP/)
    iZ =   89
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Ac3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.838984_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.584983_DP,  32.022125_DP,  21.456327_DP,   0.757593_DP,  12.341252_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.077438_DP,   0.739963_DP,   4.040735_DP,  47.525002_DP,  19.406845_DP/)
    iZ =   90
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Th4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.831122_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.515445_DP,  32.090691_DP,  13.996399_DP,  12.918157_DP,   7.635514_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.074499_DP,   0.711663_DP,   3.871044_DP,  18.596891_DP,   3.871044_DP/)
    iZ =   92
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "U3+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.706622_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.360309_DP,  32.395657_DP,  21.961290_DP,   1.325894_DP,  14.251453_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.067815_DP,   0.654643_DP,   3.643409_DP,  39.604965_DP,  16.330570_DP/)
    iZ =   92
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "U4+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.705863_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.355091_DP,  32.235306_DP,   0.557745_DP,  14.396367_DP,  21.751173_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.067789_DP,   0.652613_DP,  42.354237_DP,  15.908239_DP,   3.553231_DP/)
    iZ =   92
    icha_ion =    6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "U6+ "
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.700591_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.333844_DP,  31.770849_DP,  21.274414_DP,  13.872636_DP,   0.048519_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.067644_DP,   0.646384_DP,   3.317894_DP,  14.650250_DP,  75.339699_DP/)
    iZ =   93
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Np3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.603370_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.378152_DP,  32.572132_DP,  22.206125_DP,   1.413295_DP,  14.828381_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.064613_DP,   0.631420_DP,   3.561936_DP,  37.875511_DP,  15.546129_DP/)
    iZ =   93
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Np4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.603039_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.373926_DP,  32.423019_DP,  21.969994_DP,   0.662078_DP,  14.969350_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.064597_DP,   0.629658_DP,   3.476389_DP,  39.438942_DP,  15.135764_DP/)
    iZ =   93
    icha_ion =    6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Np6+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.600942_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.359986_DP,  31.992825_DP,  21.412458_DP,   0.066574_DP,  14.568174_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.064528_DP,   0.624505_DP,   3.253441_DP,  67.658318_DP,  13.980832_DP/)
    iZ =   94
    icha_ion =    3
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pu3+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.428895_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.356004_DP,  32.769127_DP,  22.680210_DP,   1.351055_DP,  15.416232_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.060590_DP,   0.604663_DP,   3.491509_DP,  37.260635_DP,  14.981921_DP/)
    iZ =   94
    icha_ion =    4
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pu4+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.480408_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.416219_DP,  32.610569_DP,  22.256662_DP,   0.719495_DP,  15.518152_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.061456_DP,   0.607938_DP,   3.411848_DP,  37.628792_DP,  14.464360_DP/)

    iZ =   94
    icha_ion =    6
    at_xscat(iZ)%exist_ion(icha_ion) = .true.

    at_xscat(iZ)%ion_name(icha_ion) = "Pu6+"
    at_xscat(iZ)%X_scaf(icha_ion)%con_t =    3.502325_DP
    at_xscat(iZ)%X_scaf(icha_ion)%mul_g = (/  15.436506_DP,  32.289719_DP,  14.726737_DP,  15.012391_DP,   7.024677_DP/)
    at_xscat(iZ)%X_scaf(icha_ion)%coe_g = (/   0.061815_DP,   0.606541_DP,   3.245363_DP,  13.616438_DP,   3.245364_DP/)

!    iu=find_unit()
!    open(iu,status='old',action='read',form='unformatted',file='PartitionCoeff.dat')
!    read(iu)ZPartitionCoeff
!    close(iu)
    setup_done = .true.

    
 END SUBROUTINE INI_scaf
!*****************************************************************************************
FUNCTION FFcoef(elsy,Z_e,Ion,Radiation_Type)
  IMPLICIT NONE
  CHARACTER(2),intent(IN),optional :: elsy
  CHARACTER(1),intent(IN),optional :: Radiation_Type
  INTEGER,intent(IN),optional  :: Ion,Z_e
  INTEGER                      :: i,Z_el,Ionc
  CHARACTER(1)                 :: is_xne='x'
  REAL(DP)                     :: FFcoef(11)
  
  if (verbose) print*, '  FFcoef'
  IF (.not.setup_done) CALL INI_scaf
  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    FFcoef = zero
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FFcoef with Unknown element ',elsy
      STOP 'Fatal Error: called FFcoef with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FFcoef with unmatching Z and symbol'
      stop 'Fatal Error: called FFcoef with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FFcoef with Unknown element ',elsy
        STOP 'Fatal Error: called FFcoef with Unknown element '
      ENDIF
    endif
  endif
  
  if (Z_el == 0) then
    FFcoef=zero
    return
  endif

  is_xne='x'
  if (PRESENT(Radiation_Type)) then
    is_xne=Radiation_Type
    call LOWCASE(is_xne)
    if (is_xne=='s') is_xne='x'
    if (.not.(ANY([is_xne=='x',is_xne=='n',is_xne=='e']))) then
      print*,'Fatal Error: called FFcoef with Unknown Radiation Type'
      stop 'Fatal Error: called FFcoef with Unknown Radiation Type'
    endif
  endif
  

  
  if (is_xne=='n') then
    FFcoef(1)  = bcohr(Z_el)
    FFcoef(2:) = zero
  else
    Ionc=0
    IF (PRESENT(Ion)) then
      Ionc=Ion
    endif
    FFcoef = [at_xscat(Z_el)%X_scaf(Ionc)%con_t, &
              (at_xscat(Z_el)%X_scaf(Ionc)%mul_g(i),i=1,5), &
              (at_xscat(Z_el)%X_scaf(Ionc)%coe_g(i),i=1,5)]
  ENDIF
 END FUNCTION FFcoef
!*****************************************************************************************************
 FUNCTION FormFact_GEN(q,elsy,Z_e,Ion,Radiation_Type)
  IMPLICIT NONE
  REAL(DP),intent(IN)              :: q
  CHARACTER(2),intent(IN),optional :: elsy
  CHARACTER(1),intent(IN),optional :: Radiation_Type
  INTEGER,intent(IN),optional  :: Ion,Z_e
  INTEGER                      :: i,Z_el,Ionc
  CHARACTER(1)                 :: is_xne='x'
  REAL(DP)                     :: FormFact_GEN
  REAL(DP)                     :: qhs, Z0


  IF (.not.setup_done) CALL INI_scaf
  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    FormFact_GEN = zero
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFact with Unknown element ',elsy
      STOP 'Fatal Error: called FormFact with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFact with unmatching Z and symbol'
      stop 'Fatal Error: called FormFact with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFact with Unknown element ',elsy
        STOP 'Fatal Error: called FormFact with Unknown element '
      ENDIF
    endif
  endif
  
  if (Z_el == 0) then
    FormFact_GEN=zero
    return
  endif

  is_xne='x'
  if (PRESENT(Radiation_Type)) then
    is_xne=Radiation_Type
    call LOWCASE(is_xne)
    if (is_xne=='s') is_xne='x'
    if (.not.(ANY([is_xne=='x',is_xne=='n',is_xne=='e']))) then
      print*,'Fatal Error: called FormFact with Unknown Radiation Type'
      stop 'Fatal Error: called FormFact with Unknown Radiation Type'
    endif
  endif
  
  if (is_xne=='n') then
    FormFact_GEN = bcohr(Z_el)
  else
    Ionc=0
    IF (PRESENT(Ion)) then
      Ionc=Ion
    endif
    if (Ionc==0.and.(use_EPDL97)) then
      FormFact_GEN = FormFact_EPDL97(q=q,Z_e=Z_el)
    else
      qhs = unqua * (q**2)
      FormFact_GEN = at_xscat(Z_el)%X_scaf(Ionc)%con_t
      DO i=1,5
        FormFact_GEN = FormFact_GEN + at_xscat(Z_el)%X_scaf(Ionc)%mul_g(i) &
                         * exp(-qhs * at_xscat(Z_el)%X_scaf(Ionc)%coe_g(i))
      ENDDO
    endif
    if (is_xne=='x') return
!____ electrons case    
    Z0 = at_xscat(Z_el)%X_scaf(Ionc)%con_t+sum(at_xscat(Z_el)%X_scaf(Ionc)%mul_g(1:5))
!    FormFact_GEN = unqua*(Z0-FormFact_GEN)/qhs
    qhs=max(eps_DP,q**2)
    FormFact_GEN = (Z0-FormFact_GEN)/qhs
  ENDIF
 END FUNCTION FormFact_GEN
!*****************************************************************************************************
 FUNCTION FormFact_GEN_SP(q,elsy,Z_e,Ion,Radiation_Type)
  IMPLICIT NONE
  REAL(SP),intent(IN)              :: q
  CHARACTER(2),intent(IN),optional :: elsy
  CHARACTER(1),intent(IN),optional :: Radiation_Type
  INTEGER,intent(IN),optional  :: Ion,Z_e
  INTEGER                      :: i,Z_el,Ionc
  CHARACTER(1)                 :: is_xne='x'
  REAL(SP)                     :: FormFact_GEN_SP
  REAL(SP)                     :: qhs, Z0


  IF (.not.setup_done) CALL INI_scaf
  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    FormFact_GEN_SP = 0.0_SP
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFact with Unknown element ',elsy
      STOP 'Fatal Error: called FormFact with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFact with unmatching Z and symbol'
      stop 'Fatal Error: called FormFact with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFact with Unknown element ',elsy
        STOP 'Fatal Error: called FormFact with Unknown element '
      ENDIF
    endif
  endif
  
  if (Z_el == 0) then
    FormFact_GEN_SP=0.0_SP
    return
  endif

  is_xne='x'
  if (PRESENT(Radiation_Type)) then
    is_xne=Radiation_Type
    call LOWCASE(is_xne)
    if (is_xne=='s') is_xne='x'
    if (.not.(ANY([is_xne=='x',is_xne=='n',is_xne=='e']))) then
      print*,'Fatal Error: called FormFact with Unknown Radiation Type'
      stop 'Fatal Error: called FormFact with Unknown Radiation Type'
    endif
  endif
  Ionc=0
  IF (PRESENT(Ion)) then
    Ionc=Ion
  endif
  
  if (is_xne=='n') then
    FormFact_GEN_SP = bcohr(Z_el)
  else
    Ionc=0
    IF (PRESENT(Ion)) then
      Ionc=Ion
    endif
    if (Ionc==0.and.(use_EPDL97)) then
      FormFact_GEN_SP = real( FormFact_EPDL97(q=real(q,DP),Z_e=Z_el), SP)
    else
      qhs = unqua * (q**2)
      FormFact_GEN_SP = at_xscat(Z_el)%X_scaf(Ionc)%con_t
      DO i=1,5
        FormFact_GEN_SP = FormFact_GEN_SP + at_xscat(Z_el)%X_scaf(Ionc)%mul_g(i) &
                               * exp(-qhs * at_xscat(Z_el)%X_scaf(Ionc)%coe_g(i))
      ENDDO
    endif
    if (is_xne=='x') return
!____ electrons case    
    Z0 = at_xscat(Z_el)%X_scaf(Ionc)%con_t+sum(at_xscat(Z_el)%X_scaf(Ionc)%mul_g(1:5))
!    FormFact_GEN_SP = unqua*(Z0-FormFact_GEN_SP)/qhs
    qhs=max(q**2,eps_DP)
    FormFact_GEN_SP = (Z0-FormFact_GEN_SP)/qhs
  ENDIF
 END FUNCTION FormFact_GEN_SP
!*****************************************************************************************************

!*****************************************************************************************************
 FUNCTION FormFact_GEN_V(q,elsy,Z_e,Ion,Radiation_Type)
  IMPLICIT NONE
  REAL(DP),dimension(:),intent(IN) :: q
  CHARACTER(2),intent(IN),optional :: elsy
  CHARACTER(1),intent(IN),optional :: Radiation_Type
  INTEGER,intent(IN),optional  :: Ion,Z_e
  INTEGER                      :: i,Z_el,Ionc
  CHARACTER(1)                 :: is_xne='x'
  REAL(DP),dimension(size(q,1))  :: FormFact_GEN_V
  REAL(DP),dimension(size(q,1))  :: qhs
  REAL(DP)                     :: Z0

  IF (.not.setup_done) CALL INI_scaf
  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    FormFact_GEN_V = zero
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFact with Unknown element ',elsy
      STOP 'Fatal Error: called FormFact with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFact with unmatching Z and symbol'
      stop 'Fatal Error: called FormFact with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFact with Unknown element ',elsy
        STOP 'Fatal Error: called FormFact with Unknown element '
      ENDIF
    endif
  endif
  
  if (Z_el == 0) then
    FormFact_GEN_V=zero
    return
  endif
   

  is_xne='x'
  if (PRESENT(Radiation_Type)) then
    is_xne=Radiation_Type
    call LOWCASE(is_xne)
    if (is_xne=='s') is_xne='x'
    if (.not.(ANY([is_xne=='x',is_xne=='n',is_xne=='e']))) then
      print*,'Fatal Error: called FormFact with Unknown Radiation Type'
      stop 'Fatal Error: called FormFact with Unknown Radiation Type'
    endif
  endif
  

  
  if (is_xne=='n') then
    FormFact_GEN_V = bcohr(Z_el)
    if (verbose)  print*,'cccccccccccccccccccccccccccV ',is_xne,' ',Z_el,bcohr(Z_el)
  else
    if (verbose) print*,'dddddddddddddddddddddddddddV ',is_xne,' ',Z_el
    Ionc=0
    IF (PRESENT(Ion)) then
      Ionc=Ion
    endif
    if (Ionc==0.and.(use_EPDL97)) then
      FormFact_GEN_V = FormFact_EPDL97(q=q,Z_e=Z_el)
    else
      qhs = unqua * (q**2)
      FormFact_GEN_V = at_xscat(Z_el)%X_scaf(Ionc)%con_t
      DO i=1,5
        FormFact_GEN_V = FormFact_GEN_V + at_xscat(Z_el)%X_scaf(Ionc)%mul_g(i) &
                         * exp(-qhs * at_xscat(Z_el)%X_scaf(Ionc)%coe_g(i))
      ENDDO
    endif
    if (is_xne=='x') return
    
!____ electrons case    
    Z0 = at_xscat(Z_el)%X_scaf(Ionc)%con_t+sum(at_xscat(Z_el)%X_scaf(Ionc)%mul_g(1:5))
!    print*,'debug Z0a ',ionc,is_xne,' ',Z0,Z_el
!    print*,'debug Z0b ',minval(qhs),maxval(qhs),any(ISNAN(qhs))
!    print*,'debug Z0c ',minval(FormFact_GEN_V),maxval(FormFact_GEN_V),any(ISNAN(FormFact_GEN_V))
!    FormFact_GEN_V = unqua*(Z0-FormFact_GEN_V)/qhs
    qhs=max(q**2,eps_DP)
    FormFact_GEN_V = (Z0-FormFact_GEN_V)/qhs
  ENDIF
 END FUNCTION FormFact_GEN_V
!*****************************************************************************************************
 FUNCTION FormFact_GEN_V_SP(q,elsy,Z_e,Ion,Radiation_Type)
  IMPLICIT NONE
  REAL(SP),dimension(:),intent(IN) :: q
  CHARACTER(2),intent(IN),optional :: elsy
  CHARACTER(1),intent(IN),optional :: Radiation_Type
  INTEGER,intent(IN),optional  :: Ion,Z_e
  INTEGER                      :: i,Z_el,Ionc
  CHARACTER(1)                 :: is_xne='x'
  REAL(SP),dimension(size(q,1))  :: FormFact_GEN_V_SP
  REAL(DP),dimension(size(q,1))  :: qhs
  REAL(DP)                     :: Z0

  IF (.not.setup_done) CALL INI_scaf
  Z_el=0
  if ( (.not.PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    FormFact_GEN_V_SP = 0.0_SP
    return
  else if ( (PRESENT(Z_e)) .and. (.not.PRESENT(elsy)) ) then
    Z_el=Z_e
  else if ( (.not.PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el<0) then
      print*, 'Fatal Error: called FormFact with Unknown element ',elsy
      STOP 'Fatal Error: called FormFact with Unknown element '
    ENDIF
  else if ( (PRESENT(Z_e)) .and. (PRESENT(elsy)) ) then
    Z_el=-1
    do i=0,n_elements
      IF (elsy /= symb_of_Z(i)) CYCLE
      Z_el = i
      EXIT
    enddo
    if (Z_el /= Z_e) then
      print*,'Fatal Error: called FormFact with unmatching Z and symbol'
      stop 'Fatal Error: called FormFact with unmatching Z and symbol'
    else
      if (Z_el<0) then
        print*, 'Fatal Error: called FormFact with Unknown element ',elsy
        STOP 'Fatal Error: called FormFact with Unknown element '
      ENDIF
    endif
  endif
  
  if (Z_el == 0) then
    FormFact_GEN_V_SP=0.0_SP
    return
  endif

  is_xne='x'
  if (PRESENT(Radiation_Type)) then
    is_xne=Radiation_Type
    call LOWCASE(is_xne)
    if (is_xne=='s') is_xne='x'
    if (.not.(ANY([is_xne=='x',is_xne=='n',is_xne=='e']))) then
      print*,'Fatal Error: called FormFact with Unknown Radiation Type'
      stop 'Fatal Error: called FormFact with Unknown Radiation Type'
    endif
  endif
  Ionc=0
  IF (PRESENT(Ion)) then
    Ionc=Ion
  endif
  
  if (is_xne=='n') then
    FormFact_GEN_V_SP = bcohr(Z_el)
  else
    Ionc=0
    IF (PRESENT(Ion)) then
      Ionc=Ion
    endif
    if (Ionc==0.and.(use_EPDL97)) then
      FormFact_GEN_V_SP = real( FormFact_EPDL97(q=real(q,DP),Z_e=Z_el), SP)
    else
      qhs = unqua * (q**2)
      FormFact_GEN_V_SP = at_xscat(Z_el)%X_scaf(Ionc)%con_t
      DO i=1,5
        FormFact_GEN_V_SP = FormFact_GEN_V_SP + at_xscat(Z_el)%X_scaf(Ionc)%mul_g(i) &
                                    * exp(-qhs * at_xscat(Z_el)%X_scaf(Ionc)%coe_g(i))
      ENDDO
    endif
    if (is_xne=='x') return
!____ electrons case    
    Z0 = at_xscat(Z_el)%X_scaf(Ionc)%con_t+sum(at_xscat(Z_el)%X_scaf(Ionc)%mul_g(1:5))
    qhs=max(q**2,eps_DP)
    FormFact_GEN_V_SP = (Z0-FormFact_GEN_V_SP)/qhs
  ENDIF
 END FUNCTION FormFact_GEN_V_SP
!*****************************************************************************************************
 REAL(CP) FUNCTION FormFact1(q,Z_e,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(DP),optional,intent(IN) :: fprime,fdprime
  REAL(DP),intent(IN)          :: q
  INTEGER,intent(IN)           :: Z_e
  INTEGER,intent(IN),optional  :: Ion
  REAL(CP)                     :: qhs
  integer                      :: i5,Ionc

  Ionc = 0
  IF (PRESENT(Ion)) THEN
    Ionc = Ion
  ENDIF

  IF (.not.setup_done) CALL INI_scaf
!___________________ Null Scatterer
  if (Z_e==0) then
    FormFact1=zero
    return
  endif

  qhs = 0.25_DP*q*q

  FormFact1 = at_xscat(Z_e)%X_scaf(Ionc)%con_t
  DO i5=1,5
    FormFact1 = FormFact1 + at_xscat(Z_e)%X_scaf(Ionc)%mul_g(i5) * exp(-qhs * at_xscat(Z_e)%X_scaf(Ionc)%coe_g(i5))
  ENDDO
  if (PRESENT(fprime).and.PRESENT(fdprime)) then
    FormFact1 = sqrt((FormFact1+fprime)**2+fdprime**2)
  endif
 END FUNCTION FormFact1

 REAL(CP) FUNCTION FormFact2(q,Z_e,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(SP),optional,intent(IN) :: fprime,fdprime
  REAL(SP),intent(IN)          :: q
  INTEGER,intent(IN)           :: Z_e
  INTEGER,intent(IN),optional  :: Ion

  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact2 = FormFact1(q=REAL(q,DP),Z_e=Z_e,Ion=Ion,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
    else
      FormFact2 = FormFact1(q=REAL(q,DP),Z_e=Z_e,Ion=Ion)
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact2 = FormFact1(q=REAL(q,DP),Z_e=Z_e,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
    else
      FormFact2 = FormFact1(q=REAL(q,DP),Z_e=Z_e)
    endif
  ENDIF
 END FUNCTION FormFact2

 REAL(CP) FUNCTION FormFact3(q,elsy,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(DP),optional,intent(IN) :: fprime,fdprime
  REAL(DP),intent(IN)          :: q
  CHARACTER(2),intent(IN)      :: elsy
  INTEGER,intent(IN),optional  :: Ion
  INTEGER                      :: i,Z_e

  Z_e = 0
  if (elsy==symb_of_Z(Z_e)(1:2)) then
    FormFact3=zero
    return
  endif
  do i=1,n_elements
    IF (elsy /= symb_of_Z(i)) CYCLE
    Z_e = i
    EXIT
  enddo
  IF (Z_e == 0) THEN
      print*, 'Unknown element ',elsy
      STOP
  ENDIF
  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact3 = FormFact1(q=q,Z_e=Z_e,Ion=Ion,fprime=fprime,fdprime=fdprime)
    else
      FormFact3 = FormFact1(q=q,Z_e=Z_e,Ion=Ion)
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact3 = FormFact1(q=q,Z_e=Z_e,fprime=fprime,fdprime=fdprime)
    else
      FormFact3 = FormFact1(q=q,Z_e=Z_e)
    endif
  ENDIF
 END FUNCTION FormFact3

 REAL(CP) FUNCTION FormFact4(q,elsy,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(SP),optional,intent(IN) :: fprime,fdprime
  REAL(SP),intent(IN)          :: q
  CHARACTER(2),intent(IN)      :: elsy
  INTEGER,intent(IN),optional  :: Ion
  INTEGER                      :: i,Z_e

  Z_e = 0
  if (elsy==symb_of_Z(Z_e)(1:2)) then
    FormFact4=zero
    return
  endif
  do i=1,n_elements
    IF (elsy /= symb_of_Z(i)) CYCLE
    Z_e = i
    EXIT
  enddo
  IF (Z_e == 0) THEN
      print*, 'Unknown element ',elsy
      STOP
  ENDIF
  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact4 = FormFact1(q=REAL(q,DP),Z_e=Z_e,Ion=Ion,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
    else
      FormFact4 = FormFact1(q=REAL(q,DP),Z_e=Z_e,Ion=Ion)
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      FormFact4 = FormFact1(q=REAL(q,DP),Z_e=Z_e,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
    else
      FormFact4 = FormFact1(q=REAL(q,DP),Z_e=Z_e)
    endif
  ENDIF
 END FUNCTION FormFact4

 FUNCTION FormFact5(q,elsy,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(DP),optional,intent(IN)     :: fprime,fdprime
  REAL(DP),dimension(:),intent(IN) :: q
  CHARACTER(2),intent(IN)          :: elsy
  INTEGER,intent(IN),optional      :: Ion
  INTEGER                          :: i,Z_e
  REAL(CP),dimension(size(q,1))    :: FormFact5

  Z_e = 0
  if (elsy==symb_of_Z(Z_e)(1:2)) then
    FormFact5=zero
    return
  endif
  do i=1,n_elements
    IF (elsy /= symb_of_Z(i)) CYCLE
    Z_e = i
    EXIT
  enddo
  IF (Z_e == 0) THEN
      print*, 'Unknown element ',elsy
      STOP
  ENDIF
  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact5(i) = FormFact1(q=q(i),Z_e=Z_e,Ion=Ion,fprime=fprime,fdprime=fdprime)
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact5(i) = FormFact1(q=q(i),Z_e=Z_e,Ion=Ion)
      ENDDO
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact5(i) = FormFact1(q=q(i),Z_e=Z_e,fprime=fprime,fdprime=fdprime)
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact5(i) = FormFact1(q=q(i),Z_e=Z_e)
      ENDDO
    endif
  ENDIF
 END FUNCTION FormFact5

 FUNCTION FormFact6(q,elsy,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(SP),optional,intent(IN)     :: fprime,fdprime
  REAL(SP),dimension(:),intent(IN) :: q
  CHARACTER(2),intent(IN)          :: elsy
  INTEGER,intent(IN),optional      :: Ion
  INTEGER                          :: i,Z_e
  REAL(CP),dimension(size(q,1))    :: FormFact6

  Z_e = 0
  if (elsy==symb_of_Z(Z_e)(1:2)) then
    FormFact6=zero
    return
  endif
  do i=1,n_elements
    IF (elsy /= symb_of_Z(i)) CYCLE
    Z_e = i
    EXIT
  enddo
  IF (Z_e == 0) THEN
      print*, 'Unknown element ',elsy
      STOP
  ENDIF
  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact6(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,Ion=Ion,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact6(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,Ion=Ion)
      ENDDO
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact6(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact6(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e)
      ENDDO
    endif
  ENDIF
 END FUNCTION FormFact6

 FUNCTION FormFact7(q,Z_e,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(DP),optional,intent(IN)     :: fprime,fdprime
  REAL(DP),dimension(:),intent(IN) :: q
  INTEGER,intent(IN)               :: Z_e
  INTEGER,intent(IN),optional      :: Ion
  INTEGER                          :: i
  REAL(CP),dimension(size(q,1))    :: FormFact7

  if (Z_e==0) then
    FormFact7=zero
    return
  endif

  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact7(i) = FormFact1(q=q(i),Z_e=Z_e,Ion=Ion,fprime=fprime,fdprime=fdprime)
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact7(i) = FormFact1(q=q(i),Z_e=Z_e,Ion=Ion)
      ENDDO
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact7(i) = FormFact1(q=q(i),Z_e=Z_e,fprime=fprime,fdprime=fdprime)
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact7(i) = FormFact1(q=q(i),Z_e=Z_e)
      ENDDO
    endif
  ENDIF
 END FUNCTION FormFact7

 FUNCTION FormFact8(q,Z_e,Ion,fprime,fdprime)
  IMPLICIT NONE
  REAL(SP),optional,intent(IN)     :: fprime,fdprime
  REAL(SP),dimension(:),intent(IN) :: q
  INTEGER,intent(IN)               :: Z_e
  INTEGER,intent(IN),optional      :: Ion
  INTEGER                          :: i
  REAL(CP),dimension(size(q,1))    :: FormFact8

  if (Z_e==0) then
    FormFact8=zero
    return
  endif

  IF (PRESENT(Ion)) THEN
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact8(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,Ion=Ion,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact8(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,Ion=Ion)
      ENDDO
    endif
  ELSE
    if (PRESENT(fprime).and.PRESENT(fdprime)) then
      DO i=1,size(q,1)
        FormFact8(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e,fprime=REAL(fprime,DP),fdprime=REAL(fdprime,DP))
      ENDDO
    else
      DO i=1,size(q,1)
        FormFact8(i) = FormFact1(q=REAL(q(i),DP),Z_e=Z_e)
      ENDDO
    endif
  ENDIF
 END FUNCTION FormFact8
!********************************************************************
function Z_OF_SYMB(aa)
implicit none
character(len=*),intent(IN) :: aa
integer(I4B) :: Z_OF_SYMB
integer(I4B) :: laa,i
character(len=2) :: syc
character(len=1) :: a1

Z_OF_SYMB=0
laa=len_trim(adjustl(aa))
if (laa==0) return
if (laa==1) then
  syc=trim(adjustl(aa))//' '
else if (laa>=2) then
  syc=trim(adjustl(aa))
  a1=syc(2:2)
  call LOWCASE(a1)
  syc(2:2)=a1
endif
a1=syc(1:1)
call UPCASE(a1)
syc(1:1)=a1

do i=1,n_elements
  IF (syc /= symb_of_Z(i)) CYCLE
  Z_OF_SYMB = i
  EXIT
enddo

end function Z_OF_SYMB
!********************************************************************
 


 END MODULE ATOMIX
!_________________________________________________________________________________________________
module rhapsody_in_blue
use nano_deftyp
use linalg_tools
use atomix

real(DP),allocatable,save :: ff_SquaredAverage(:), ff_AverageSquared(:), fofa2_inc(:)
real(DP),allocatable,save :: Qvec(:), dQvec(:),Bsub_bkg(:)
real(DP),allocatable,save :: SofQ_arr(:), e_SofQ_arr(:), SofQ_arr1(:), BB(:) ! put here SofQ
integer(I4B),save  :: np_Qvec
real(DP),save      :: scale_factor_I, dx_eval, av_xstep, radius_fraction=2.0d0, Slop_Zero
real(DP),save      :: Soper_QT = 5.d0, Packing_Frac = one, num_at_vol_den = 0.064d0, mass_density_gcm3=1.5d0
logical, save :: mod_valence=.true.

!!__Soper-like variables  ACRF 19072018
real(DP),save  :: R_max0=1.d4, dR_ft, Rmin_line=1.d0, rho_n ! below this, G(r) is a line -4*Pi*rho_n*r
real(DP),save :: QT_Soper
integer(I4B),save  :: np_ft, npmin_ft
real(DP),allocatable,save :: r_ft(:),d_ft(:),b_ft(:),aux_ft(:)
logical, save :: Soper_action=.true.
!!__


contains
!***************************************************************************************************
function COLSREAD(nc,cols,colco,iunit,nr,ios,cyc)
implicit none
integer(I4B),intent(IN)  :: nc,nr,iunit
integer(I4B),intent(IN)  :: cols(2,nr)
real(DP),intent(IN)      :: colco(2,nr)
integer(I4B),intent(OUT) :: ios,cyc
real(DP),dimension(nr) :: COLSREAD
real(DP),dimension(nc) :: rcols
real(DP) :: zrrc(2)
integer(I4B) :: i,ll,ios2
character(999) :: rl

COLSREAD=zero
cyc=0

read(iunit,'(a)',iostat=ios) rl
if (ios /= 0) return
rl=trim(adjustl(rl))
ll=len_trim(rl)
read(rl(1:ll),*,iostat=ios2) rcols
if (ios2 /= 0) then
  cyc=1
  return
endif
do i=1,nr
  zrrc = one
  where (cols(:,i)>0) zrrc = rcols(cols(:,i))
  COLSREAD(i) = sum( colco(:,i) * zrrc )
enddo

end function COLSREAD
!***************************************************************************************************
subroutine RCXXX(a,numc,coec)
implicit none
character(len=*),intent(in) :: a
integer(I4B),intent(OUT) :: numc(2)
real(DP),intent(OUT)     :: coec(2)
integer(I4B) :: io1,lta

!  reads alternatively from a string:
!  - 2 integers and 2 reals
!  - if not possible, 1 integer only

numc=0; coec=zero
lta=len_trim(a)
read(a(1:lta),*,iostat=io1) numc,coec
if (io1/=0) then
  read(a(1:lta),*,iostat=io1) numc(1)
  numc(2) = 0; coec=[one,zero]
endif

end subroutine RCXXX
!***************************************************************************************************
subroutine rebin_ttqQ(todo, xin, yin, zin, lore, dxin0, dxout0, xout1, yout1, eout1)
implicit none
character(len=*), intent(IN) :: todo
real(DP), intent(IN)           :: xin(:), yin(:), zin(:), lore
real(DP),optional,intent(IN) :: dxin0, dxout0
integer(4) :: npmq
integer(4) :: np,ii,ii1,ii2,ii0,nk,iw,npk,ik
real(DP), save :: loremin=5.d0
real(DP), parameter :: dtt0=0.0036d0, dtth=0.5d0*dtt0, Kea=12.398418573430595d0,iKea=1.d0/Kea
real(DP) :: pi, x,y,z,fopi,gamx,con_dQ,dQ_min,dQ_x,dq,dtt,dxin,dxin1,&
           dxout,x1,x2,beta,eps,yy,zz,ee,fac
real(DP), allocatable :: yout(:),eout(:), aux(:)
real(DP), allocatable, intent(OUT) :: xout1(:), yout1(:),eout1(:)
logical, allocatable :: essex(:)
character(4) :: todo1

!! dQ = pi^2 * d(2theta) / (180 * wlen) = pi^2 * d(2\theta) * EE / (180 * Kea)
!! evaluated at 2\theta = 120 deg

pi = 4.d0 * atan(1.d0)

todo1 = trim(adjustl(todo))
print*, 'todo ', todo1
ee = lore
if (ee<loremin) ee = Kea / ee 

np = size(xin)

! allocate(xout1(npmq),yout1(npmq),eout1(npmq),yout(npmq),eout(npmq),essex(npmq))
! allocate(yout(npmq),eout(npmq),essex(npmq))


eps = epsilon(1.d0)
if (todo1 == 'tt2Q') then
  dxin = 0.0036d0
  dxout = pi**2 * ee * iKea * dxin * cos(maxval(xin*duet2r)) / 90.0d0  
  if (PRESENT(dxin0)) then
    dxin = dxin0
  endif
  if (PRESENT(dxout0)) then
    dxout = dxout0
  endif
  print*, 'Rebinning d(2theta)  = ', dxin, ' to dQ = ', dxout
  
  fopi = 4.0d0 * pi
  gamx = atan(1.d0) / 90.d0
  npmq = fopi * ee * iKea * (sin(maxval(xin*duet2r))-sin(minval(xin*duet2r))) / dxout
  allocate(yout(npmq),eout(npmq),essex(npmq),aux(npmq))
  essex = .false.
  yout = zero
  eout = zero
  
  dxin1 = 0.5d0 * dxin
  
  do ii0 = 1,np
    x = xin(ii0)
    y = yin(ii0)
    z = zin(ii0)
    x1 = fopi * ee * iKea * sin(gamx*(x-dxin1))
    x2 = fopi * ee * iKea * sin(gamx*(x+dxin1))
    yy = y * dxout / (x2-x1)
    zz = z * dxout / (x2-x1)
    if (z<eps) zz = one
    ii1 = floor(x1/dxout) - 1
    ii2 = ceiling(x2/dxout) + 1
    do ii = ii1, ii2
      beta = max(0.d0, min(x2,dxout*ii)-max(x1,dxout*(ii-1))) / dxout
      if (beta < eps) cycle
      yout(ii) = yout(ii) + beta / (zz**2)
      eout(ii) = eout(ii) + beta * yy / (zz**2)
      essex(ii) = .true.
    enddo
  enddo
else if (todo1 == 'tt2q') then
  print*, 'Rebinning d(2theta)  = ', dxin, ' to dq = ', dxout
  
  gamx = atan(1.d0) / 90.d0
  npmq = 2.0d0 * pi * ee * iKea * (sin(maxval(xin*duet2r))-sin(minval(xin*duet2r))) / dxout
  allocate(yout(npmq),eout(npmq),essex(npmq),aux(npmq))
  essex = .false.
  yout = zero
  eout = zero
  
  dxin1 = 0.5d0 * dxin
  
  do ii0 = 1,np
    x = xin(ii0)
    y = yin(ii0)
    z = zin(ii0)
    x1 = two * ee * iKea * sin(gamx*(x-dxin1))
    x2 = two * ee * iKea * sin(gamx*(x+dxin1))
    yy = y * dxout / (x2-x1)
    zz = z * dxout / (x2-x1)
    if (z<eps) zz = one
    ii1 = floor(x1/dxout) - 1
    ii2 = ceiling(x2/dxout) + 1
    do ii = ii1, ii2
      beta = max(0.d0, min(x2,dxout*ii)-max(x1,dxout*(ii-1)))/dxout
      if (beta < eps) cycle
      yout(ii) = yout(ii) + beta / (zz**2)
      eout(ii) = eout(ii) + beta * yy / (zz**2)
      essex(ii) = .true.
    enddo
  enddo
else if (todo1 == 'q2tt') then
  dxin = xin(2) - xin(1)
  dxout = 180.0d0 / (pi * ee * iKea) * dxin 
  if (PRESENT(dxin0)) then
    dxin = dxin0
  endif
  if (PRESENT(dxout0)) then
    dxout = dxout0
  endif
  print*, 'Rebinning dq  = ', dxin, ' to d(2theta) = ', dxout
  
  fac = 360.0d0 / pi
  gamx = atan(1.d0) / 90.d0

  npmq = fac * (asin(maxval(xin/(two *ee * iKea))) - asin(minval(xin/(two *ee * iKea)))) / dxout
  allocate(yout(npmq),eout(npmq),essex(npmq),aux(npmq))
  essex = .false.
  yout = zero
  eout = zero
  
  dxin1 = 0.5d0 * dxin
  
  do ii0 = 1,np
    x = xin(ii0)
    y = yin(ii0)
    z = zin(ii0)
    !! ee * iKea = 1 / lambda
    x1 = fac * asin(x / (two *ee * iKea) - dxin1)
    x2 = fac * asin(x / (two *ee * iKea) + dxin1)
    yy = y * dxout / (x2-x1)
    zz = z * dxout / (x2-x1)
    if (z<eps) zz = one
    ii1 = floor(x1/dxout) - 1
    ii2 = ceiling(x2/dxout) + 1
    do ii = ii1, ii2
      beta = max(0.d0, min(x2,dxout*ii)-max(x1,dxout*(ii-1)))/dxout
      if (beta < eps) cycle
      yout(ii) = yout(ii) + beta / (zz**2)
      eout(ii) = eout(ii) + beta * yy / (zz**2)
      essex(ii) = .true.
    enddo
  enddo
else if (todo1 == 'q2Q') then
  dxin = xin(2) - xin(1)
  dxout = two * pi * dxin 
  if (PRESENT(dxin0)) then
    dxin = dxin0
  endif
  if (PRESENT(dxout0)) then
    dxout = dxout0
  endif
  print*, 'Rebinning dq  = ', dxin, ' to dQ = ', dxout
  
  fac = two * pi
  gamx = atan(1.d0) / 90.d0

  npmq = fac * (maxval(xin) - minval(xin)) / dxout
  allocate(yout(npmq),eout(npmq),essex(npmq),aux(npmq))
  essex = .false.
  yout = zero
  eout = zero

  
  dxin1 = 0.5d0 * dxin
  
  do ii0 = 1,np
    x = xin(ii0)
    y = yin(ii0)
    z = zin(ii0)
    !! ee * iKea = 1 / lambda
    x1 = fac * (x - dxin1)
    x2 = fac * (x + dxin1)
    yy = y * dxout / (x2-x1)
    zz = z * dxout / (x2-x1)
    if (z<eps) zz = one
    ii1 = floor(x1/dxout) - 1
    ii2 = ceiling(x2/dxout) + 1
    do ii = ii1, ii2
      beta = max(0.d0, min(x2,dxout*ii)-max(x1,dxout*(ii-1)))/dxout
      if (beta < eps) cycle
      yout(ii) = yout(ii) + beta / (zz**2)
      eout(ii) = eout(ii) + beta * yy / (zz**2)
      essex(ii) = .true.
    enddo
  enddo
endif

aux = 0.d0
where(essex(:))
  aux = eout(:) / yout(:)
  eout(:) = 1.d0 / sqrt(yout(:))
  yout(:) = aux
end where

! allocate(xout1(npmq),yout1(npmq),eout1(npmq))
! yout1 = zero
! eout1 = zero
! xout1 = zero
! do iw = 1, npmq
!     xout1(iw) = iw * dxout
!     yout1(iw) = yout(iw)
!     eout1(iw) = eout(iw)
! enddo


npk = 0
do iw=1,npmq
  if (essex(iw)) npk = npk +1
enddo
allocate(xout1(npk),yout1(npk),eout1(npk))
xout1 = zero
yout1 = zero
eout1 = zero
ik = 0
do iw=1,npmq
  if (essex(iw)) then
    ik = ik + 1
    xout1(ik) = iw * dxout
    yout1(ik) = yout(iw)
    eout1(ik) = eout(iw)
  endif
enddo

end subroutine rebin_ttqQ
!***************************************************************************************************
subroutine setup_QdQ(xarr,dx_scal,dx_vec,twotheta1_q2_bigq3,wlen)
! transforms abscissa (2theta, q, Q) into Q; evaluates dQ.
! xarr                          is the original abscissa, 
! twotheta1_q2_bigq3            (self-explaining) a flag telling what it is, 
! dx_scal            [OPTIONAL] the constant abscissa step (if it is so and it is known),
! dx_vec             [OPTIONAL] the variable abscissa step (if it is so and it is known),
! IF NEITHER dx_scal NOR dx_vec ARE GIVEN: COSTANT STEP IS ASSUMED AND THAT IS EVALUATED.
! wlen                          is the wavelength.
implicit none
real(DP),dimension(:),intent(IN) :: xarr
real(DP),intent(IN) :: wlen
real(DP),intent(IN),optional :: dx_scal
real(DP),dimension(:),intent(IN),optional :: dx_vec
integer(I4B),intent(IN) :: twotheta1_q2_bigq3

integer(I4B) :: npq,npr,iq
real(DP) :: pi4_wl,ddd

npq=size(xarr)

if (present(dx_vec)) then
  if (size(dx_vec)/=npq) then
    print'(a,i7,a,i7,a)','setup_QdQ: size(dx_vec) = ',size(dx_vec),' /= size(xarr) = ',&
                          npq,'. Stop.'
    stop 'setup_QdQ: size(dx_vec) /= size(yarr). Stop.'
  endif
endif
np_Qvec=npq
if (allocated(Qvec))  deallocate(Qvec)
if (allocated(dQvec)) deallocate(dQvec)
allocate(Qvec(np_Qvec),dQvec(np_Qvec))

xvariable: SELECT CASE(twotheta1_q2_bigq3)
CASE (1) xvariable
  pi4_wl=pi2*two/wlen
  Qvec = pi4_wl * sin(duet2r * xarr)
  if (PRESENT(dx_vec)) then
    ddd = pi4_wl * two
    av_xstep=sum(dx_vec)/size(dx_vec)
    dQvec = ddd * cos(duet2r * xarr) * sin(half*duet2r*dx_vec)
  else
    if (PRESENT(dx_scal)) then
      av_xstep=dx_scal
      ddd = pi4_wl * two * sin(half*duet2r*dx_scal)
      dQvec = cos(duet2r * xarr) * ddd
    else
      dx_eval = evaluate_step_array(xarr)
      av_xstep=dx_eval
      ddd = pi4_wl * two * sin(half*duet2r*dx_eval)
      dQvec = cos(duet2r * xarr) * ddd
    endif
  endif
CASE (2) xvariable
  Qvec = xarr*pi2
  if (PRESENT(dx_vec)) then
    dQvec = dx_vec*pi2
    av_xstep=sum(dx_vec)/size(dx_vec)
  else
    if (PRESENT(dx_scal)) then
      dQvec = dx_scal*pi2
      av_xstep=dx_scal
    else
      dQvec = one
      av_xstep=one
    endif
  endif
CASE (3) xvariable
  Qvec = xarr
  if (PRESENT(dx_vec)) then
    dQvec = dx_vec
    av_xstep=sum(dx_vec)/size(dx_vec)
  else
    if (PRESENT(dx_scal)) then
      dQvec = dx_scal
      av_xstep=dx_scal
    else
      dQvec = one
      av_xstep=one
    endif
  endif
CASE DEFAULT xvariable
  print*,'setup_QdQ: unclear what to do, RETURNING : ',twotheta1_q2_bigq3
  return
END SELECT xvariable

end subroutine setup_QdQ
!***************************************************************************************************
subroutine Setup_ScatLen_or_FF(wlen,comp_X,comp_Z,rad_type,use_incoh,use_Z_to_scale_Xray,beam_pol_ang_ecc)
implicit none
real(DP),intent(IN)                  :: wlen
real(DP),dimension(:),intent(IN)     :: comp_X
real(DP),optional,intent(IN)         :: beam_pol_ang_ecc(2)
integer(I4B),dimension(:),intent(IN) :: comp_Z
character(len=1),intent(IN)          :: rad_type !  'x','n','e'
logical,intent(IN) :: use_incoh,use_Z_to_scale_Xray

real(DP) :: sc_comp,opi2,sumima,yfrac,bsl_fm,siga_valen
real(DP),allocatable :: fofa(:,:), fofam(:), fofa2(:), fo_inc(:), anosf(:,:), aux_fof(:),aux2_fof(:)
integer(I4B) :: nn,nato,nni
!----- calcolo fofa2 = <f>^2 per normalizzare I(Q)

nato=size(comp_Z)
if (size(comp_X)/=nato) then
  print*,'Setup_ScatLen_or_FF: size(comp_X) = ',size(comp_X),' differs from size(comp_Z) = ',nato,' : STOP'
  stop 'Setup_ScatLen_or_FF: size(comp_X) differs from size(comp_Z) : STOP'
endif

if (allocated(ff_SquaredAverage)) deallocate(ff_SquaredAverage)
if (allocated(ff_AverageSquared)) deallocate(ff_AverageSquared)
if (allocated(fofa2_inc)) deallocate(fofa2_inc)
allocate(ff_SquaredAverage(np_Qvec),ff_AverageSquared(np_Qvec),fofa2_inc(np_Qvec))
allocate(fofa(np_Qvec,nato), fofam(np_Qvec), fofa2(np_Qvec), fo_inc(np_Qvec), anosf(2,nato), &
         aux_fof(np_Qvec),aux2_fof(np_Qvec))

fofa=zero; fofam=zero; fofa2=zero; fo_inc=zero

sc_comp=one/sum(comp_X)
opi2=one/pi2
Qvec  =  Qvec*opi2  ! temporarily
sumima=zero
do nn=1,nato
  yfrac=comp_X(nn)*sc_comp
  print*,nn,comp_Z(nn),rad_type
  if (rad_type=='x') then
    print'(a,i3,a,i3,a,i3,a,f16.8)','Element # ',nn,' of',nato,' : Z = ',comp_Z(nn),'; X = ',comp_X(nn)
    fofa(:,nn) = FORMFACT(q=Qvec,Z_e=comp_Z(nn))
!_______________ consider valence broadening... Gaussian width??? Now Gaussian HWHM=radius_fraction*at_radii
    if ((comp_Z(nn)>=5.and.comp_Z(nn)<=9) .and. mod_valence) then
      aux_fof = max(zero, FORMFACT(q=Qvec,Z_e=comp_Z(nn),Ion=comp_Z(nn)-2))
      !fof(:,1)= FormFact(q=q,Z_e=6,Ion=4,Radiation_Type='x')
      siga_valen = (radius_fraction * at_radii(comp_Z(nn)) / sqrt(two*log(two)))
      aux2_fof = (fofa(:,nn)-aux_fof) * exp(-half*((Qvec*siga_valen)**2))
      fofa(:,nn) = aux_fof+aux2_fof
    endif
        
    anosf(:,nn) = Anomalous_X(Z_e=comp_Z(nn), wavelength=wlen)
    fofa2(:)  =  fofa2(:) + ( anosf(2,nn)**2 + (anosf(1,nn)+fofa(:,nn))**2 ) * yfrac
    if (.not.use_Z_to_scale_Xray) then
      sumima=sumima+anosf(2,nn) * yfrac
      fofam(:)  =  fofam(:)+(fofa(:,nn)+anosf(1,nn))*yfrac
    else if (use_Z_to_scale_Xray) then
      fofam(:)  =  fofam(:)+real(comp_Z(nn),DP)*yfrac
    endif
    if (use_incoh) then
      fo_inc = fo_inc + yfrac * COMP_COMPTON_S(q=Qvec, Z_e=comp_Z(nn), wavelength=wlen, beam_pol_ang_ecc=beam_pol_ang_ecc)
    endif
  else if (rad_type=='e') then
    fofa(:,nn) = FormFact(q=Qvec,Z_e=comp_Z(nn),Radiation_Type='e')
    fofa2(:)  =  fofa2(:) + (fofa(:,nn)**2) * yfrac
    fofam(:)  =  fofam(:) +  fofa(:,nn)     * yfrac
  else if (rad_type=='n') then
    bsl_fm = bcohr(comp_Z(nn))
    if (use_incoh) then
      fo_inc = fo_inc + five * sqrt( sig_inc(comp_Z(nn))/pi ) * yfrac
    endif
    fofa2  =  fofa2 + ((bsl_fm**2) * yfrac)
    fofam  =  fofam + ( bsl_fm     * yfrac)
  endif
enddo
sumima=sumima**2
ff_AverageSquared = fofam**2
if (rad_type=='x'.and.sumima>sceps_DP) ff_AverageSquared = ff_AverageSquared + sumima
ff_SquaredAverage = fofa2
fofa2_inc = ff_SquaredAverage + fo_inc

!__ resetting
deallocate(fofa, fofam, fofa2, fo_inc, anosf)
Qvec  =  Qvec*pi2

end subroutine Setup_ScatLen_or_FF
!***************************************************************************************************
subroutine eval_scale(yarr,e_yarr,wlen,sc_mode,scale_val, tail_frac)
implicit none
real(DP),dimension(:),intent(IN)          :: yarr
real(DP),dimension(:),optional,intent(IN) :: e_yarr
real(DP),optional,intent(IN)         :: tail_frac
real(DP),intent(IN)                  :: wlen
character(len=1),intent(IN)          :: sc_mode  !  'm','p','t'
real(DP),optional,intent(IN)         :: scale_val

real(DP) :: scale_val1,sIrawq,sfofa2q,cdq, tail_frac1
real(DP),allocatable :: xaux(:),yaux(:),waux(:)
integer(I4B) :: np_tail,np,n1

scale_val1=one
if (PRESENT(scale_val)) then
  scale_val1=scale_val
endif

tail_frac1 = 0.05d0
if (PRESENT(tail_frac)) then
  tail_frac1 = tail_frac
endif

np=size(yarr)

scalmode: SELECT CASE(sc_mode)
CASE ('n') scalmode
  scale_factor_I = zero
  print*,'No subtraction [n] '
  return
CASE ('m') scalmode
  scale_factor_I = scale_val1
  print*,'Scale factor used [m] : ',scale_factor_I
  return
CASE ('t') scalmode
  np_tail = max(10,nint(tail_frac1*real(np,DP)))
  n1=np-np_tail+1
  allocate(xaux(np_tail),yaux(np_tail),waux(np_tail))
  cdq=np_tail/sum(dQvec(n1:np))
  yaux=dQvec(n1:np)*cdq
  if (.not.PRESENT(e_yarr)) then
    scale_factor_I = sum(yarr(n1:np)) / &
                     sum(ff_SquaredAverage(n1:np) * yaux)
  else if (PRESENT(e_yarr)) then
    if (ALL(e_yarr<eps_DP)) then
      scale_factor_I = sum(yarr(n1:np)) / &
                       sum(ff_SquaredAverage(n1:np) * yaux)
    else
      waux=one/((max(eps_DP,e_yarr(n1:np)))**2)
      scale_factor_I = sum(yarr(n1:np) * ff_SquaredAverage(n1:np) * waux * yaux) / &
                       sum(ff_SquaredAverage(n1:np) * ff_SquaredAverage(n1:np) * waux * yaux)
    endif
  endif
  deallocate(xaux,yaux)
  print*,'Scale factor used [t] : ',scale_factor_I
CASE ('p') scalmode
  sIrawq = sum(yarr*Qvec*dQvec)
  sfofa2q= sum(ff_SquaredAverage*Qvec*dQvec)
  scale_factor_I = sIrawq/sfofa2q
  print*,'Scale factor used [p] : ',scale_factor_I
CASE DEFAULT scalmode
  scale_factor_I = one
  print'(a)','eval_scale: unknown scaling mode '//sc_mode//'   [known ones = m, p, t]'
  print'(a)','Unclear what to do, set scale_factor_I = 1.000000 and RETURN...'
  return
END SELECT scalmode
end subroutine eval_scale
!***************************************************************************************************
subroutine eval_scale_CON0(yarr,e_yarr,wlen,sc_mode,scale_val, tail_frac,head_frac, deg_bkg, low_sp_fr)
implicit none
real(DP),dimension(:),intent(IN)          :: yarr
real(DP),dimension(:),optional,intent(IN) :: e_yarr
real(DP),optional,intent(IN)         :: tail_frac,head_frac
real(DP),intent(IN)                  :: wlen
character(len=1),intent(IN)          :: sc_mode  !  'm','p','t'
real(DP),optional,intent(IN)         :: scale_val
real(DP),optional,intent(IN)         :: low_sp_fr
integer(I4B),optional,intent(IN)     :: deg_bkg

real(DP) :: scale_val1,sIrawq,sfofa2q,cdq, tail_frac1, head_frac1,tail_ave,low_sp_fr1
real(DP) :: Vche, Uche, sumAR
real(DP),allocatable :: xaux(:),yaux(:),waux(:),zaux(:),vaux(:),  value_P(:), ZZwarr(:)
real(DP),allocatable :: xauxh(:),yauxh(:),wauxh(:),zauxh(:),vauxh(:), Asvd(:,:),Bsvd(:),Xsvd(:),xche(:),Tche(:,:)
integer(I4B) :: np_tail,np_head,np,n1,n2, ncoe_bkg,deg_bkg1,k,j,np_tail0
real(DP) :: Delta_r_Shannon, value_J, value_gamma, value_G, value_GQM3, value_gJ, value_alpha, value_chi2, value_gof, &
            value_rho, eee, fac_dQ
integer(I4B) :: iu_con, ncoe_bkgm1
integer(I4B) :: Cheby_or_lowfreq=2

scale_val1=one
if (PRESENT(scale_val)) then
  scale_val1=scale_val
endif

tail_frac1 = 0.05d0
if (PRESENT(tail_frac)) then
  tail_frac1 = tail_frac
endif
head_frac1 = 0.01d0
if (PRESENT(head_frac)) then
  head_frac1 = head_frac
endif

np=size(yarr)
Delta_r_Shannon=pi/Qvec(np)
ncoe_bkg=3
deg_bkg1=0
if (PRESENT(deg_bkg)) then
  deg_bkg1=max(0,deg_bkg)
  ncoe_bkg=1+deg_bkg1+2
  Cheby_or_lowfreq=1
endif
if (PRESENT(low_sp_fr)) then
  low_sp_fr1=low_sp_fr
  if (low_sp_fr1>0.05d0) then
    deg_bkg1=max(0,floor(low_sp_fr1/Delta_r_Shannon))
    ncoe_bkg=1+deg_bkg1+2
    Cheby_or_lowfreq=2
  endif
endif
ncoe_bkgm1=ncoe_bkg-1

scalmode: SELECT CASE(sc_mode)
CASE ('n') scalmode
  scale_factor_I = zero
  print*,'No subtraction [n] '
  return
CASE ('m') scalmode
  scale_factor_I = scale_val1
  print*,'Scale factor used [m] : ',scale_factor_I
  return
CASE ('t') scalmode
  np_tail = max(10,nint(tail_frac1*real(np,DP)))
  n1=np-np_tail+1
  allocate(xaux(np_tail),yaux(np_tail),waux(np_tail))
  cdq=np_tail/sum(dQvec(n1:np))
  yaux=dQvec(n1:np)*cdq
  if (.not.PRESENT(e_yarr)) then
    scale_factor_I = sum(yarr(n1:np)) / &
                     sum(fofa2_inc(n1:np) * yaux)
  else if (PRESENT(e_yarr)) then
    if (ALL(e_yarr<eps_DP)) then
      scale_factor_I = sum(yarr(n1:np)) / &
                       sum(fofa2_inc(n1:np) * yaux)
    else
      waux=one/((max(eps_DP,e_yarr(n1:np)))**2)
      scale_factor_I = sum(yarr(n1:np) * fofa2_inc(n1:np) * waux * yaux) / &
                       sum(fofa2_inc(n1:np) * fofa2_inc(n1:np) * waux * yaux)
    endif
  endif
  deallocate(xaux,yaux)
  print*,'Scale factor used [t] : ',scale_factor_I
  
  
  
CASE ('z') scalmode !!!! Constraining also the origin...
  np_tail = max(10,nint(tail_frac1*real(np,DP)))
  np_head = max(10,nint(head_frac1*real(np,DP)))
  n1=np-np_tail+1
  n2=np_head
  print*, 'np n1 n2 ',np,n1,n2
  !!__RF 09082017
  np_tail0 = max(10,nint(0.05d0*real(np,DP)))
  tail_ave = sum(yarr(np-np_tail0+1:np))/np_tail0
  !!__end
  allocate(xaux(np_tail),yaux(np_tail),waux(np_tail),zaux(np_tail),vaux(np_tail))
  allocate(xauxh(np_head),yauxh(np_head),wauxh(np_head),zauxh(np_head),vauxh(np_head), &
           Asvd(np_head+np_tail,ncoe_bkg),Bsvd(np_head+np_tail),Xsvd(ncoe_bkg),xche(np),Tche(np,0:deg_bkg1))
  if (allocated(Bsub_bkg)) deallocate(Bsub_bkg)
  allocate(Bsub_bkg(np))
  Bsub_bkg=zero
  Asvd=zero
  cdq=np_tail/sum(dQvec(n1:np))
  yaux=yarr(n1:np)!*dQvec(n1:np)!*cdq
!   yaux=tail_ave 
  print*, 'tail_ave ', tail_ave, yaux(1), yaux(np_tail)
  yauxh=yarr(1:n2)!*dQvec(1:n2)!*cdq
  if (.not.PRESENT(e_yarr)) then
    vaux=one
    waux=one
    vauxh=one
    wauxh=one
  else if (PRESENT(e_yarr)) then
    vaux=one/((max(eps_DP,e_yarr(n1:np))))
    waux=vaux**2
    vauxh=one/((max(eps_DP,e_yarr(1:n2))))
    wauxh=vauxh**2
  endif
  Bsvd(1:np_head) = yauxh*vauxh
  Bsvd(1+np_head:np_head+np_tail) = yaux*vaux
  zaux=fofa2_inc(n1:np)-ff_AverageSquared(n1:np)
  zauxh=fofa2_inc(1:n2)-ff_AverageSquared(1:n2)
  
  Tche(:,0)=one
  if (Cheby_or_lowfreq==1) then
    ! Chebyshev mode
    Uche = one/(Qvec(np)-Qvec(1))
    Vche = - (Qvec(np)+Qvec(1)) 
    xche = (two * Qvec + VChe)*Uche
    if (deg_bkg1>0) then
      Tche(:,1)=xche
      if (deg_bkg1>1) then
        do k=2,deg_bkg1
          Tche(:,k)=two*xche*Tche(:,k-1)-Tche(:,k-2)
        enddo
      endif
    endif
  else if (Cheby_or_lowfreq==2) then
    do k=1,deg_bkg1
      Tche(:,k)=sin(k*Qvec*Delta_r_Shannon)/(k*Delta_r_Shannon)
    enddo
  endif
  
  Asvd(1:np_head,1) = Qvec(1:n2)*ff_AverageSquared(1:n2)*vauxh
  
  Asvd(1:np_head,2) = zauxh*vauxh
  Asvd(1+np_head:np_head+np_tail,2) = fofa2_inc(n1:np)*vaux
  
  do k=0,deg_bkg1
    Asvd(1:np_head,3+k) = Tche(1:np_head,k)*vauxh
    Asvd(1+np_head:np_head+np_tail,3+k) = Tche(n1:np,k)*vaux
  enddo
  
  call SING_VAL_LSPRE(a=Asvd,b=Bsvd,thresh1=sceps_DP,x=Xsvd)

!___  The next parameters need to be declared/stored in the same place as scale_factor_I
  Slop_Zero = Xsvd(1)/Xsvd(2)          !___ initial slope. Utility is below the 1 Fv SI (1 Cz IU, 1 Mnk CGS) threshold for now
  scale_factor_I = one/Xsvd(2)         !___ scale factor to apply to yaux AFTER subtraction of constant bkg (next)
  Bsub_bkg = Xsvd(3)
  do k=1,deg_bkg1
    Bsub_bkg=Bsub_bkg+Xsvd(k+3)*Tche(:,k)
  enddo
  Bsub_bkg = Bsub_bkg * scale_factor_I  !___ to be subtracted from scale_factor_I * yaux where yaux=I_obs
  deallocate(xaux,yaux)
  print*,'Scale factor used [z] : ',scale_factor_I
  
  iu_con=find_unit()
  open(iu_con,status='replace',file='control_file_optZ.txt')
  sumAR = zero
  do j=1,np
    eee=zero
    if (PRESENT(e_yarr)) eee=e_yarr(j)
    write(iu_con,*)Qvec(j),dQvec(j),scale_factor_I*yarr(j),scale_factor_I*eee,Bsub_bkg(j), &
                           one+(scale_factor_I*yarr(j)-Bsub_bkg(j)-fofa2_inc(j))/ff_AverageSquared(j), &
                           fofa2_inc(j), ff_AverageSquared(j)
    sumAR = sumAR + Qvec(j)*dQvec(j)*(scale_factor_I*yarr(j)-Bsub_bkg(j)-ff_AverageSquared(j))/fofa2_inc(j)
  enddo
  close(iu_con)
  print*,'Area Q*[S(Q)-1] = ',sumAR,sumAR/sum(Qvec(1:np)*dQvec(1:np))
  
!__________________ Case W  
  
CASE ('w') scalmode !!!! Constraining also the origin...

  !  U = fofa2_inc
  !  R = ff_AverageSquared

  fac_dQ = ( Qvec(np)-Qvec(1)+half*(dQvec(np)+dQvec(1)) )/sum(dQvec(1:np))
  if (abs(fac_DQ-one) > sceps_DP) print*, 'dQ factor = ',fac_dQ

  np_tail = max(10,nint(tail_frac1*real(np,DP)))
  np_head = max(10,nint(head_frac1*real(np,DP)))
  n1=np-np_tail+1
  n2=np_head
  print*, 'np n1 n2 ',np,n1,n2
  !!__RF 09082017
  np_tail0 = max(10,nint(0.05d0*real(np,DP)))
  tail_ave = sum(yarr(np-np_tail0+1:np))/np_tail0
  !!__end
  allocate(ZZwarr(np))
  allocate(xaux(np_tail),yaux(np_tail),waux(np_tail),zaux(np_tail),vaux(np_tail))
  allocate(xauxh(np_head),yauxh(np_head),wauxh(np_head),zauxh(np_head),vauxh(np_head), &
           Asvd(np_head+np_tail,ncoe_bkgm1),Bsvd(np_head+np_tail),Xsvd(ncoe_bkgm1),xche(np),Tche(np,0:deg_bkg1))
  allocate(value_P(0:deg_bkg1))
  
  if (allocated(Bsub_bkg)) deallocate(Bsub_bkg)
  allocate(Bsub_bkg(np))
  Bsub_bkg=zero
  
  ZZwarr = Qvec*dQvec/ff_AverageSquared

  cdq=np_tail/sum(dQvec(n1:np))
  yaux=yarr(n1:np)
!   yaux=tail_ave 
  print*, 'tail_ave ', tail_ave, yaux(1), yaux(np_tail)
  yauxh=yarr(1:n2)
  if (.not.PRESENT(e_yarr)) then
    vaux=one
    waux=one
    vauxh=one
    wauxh=one
  else if (PRESENT(e_yarr)) then
    vaux=one/((max(eps_DP,e_yarr(n1:np))))
    waux=vaux**2
    vauxh=one/((max(eps_DP,e_yarr(1:n2))))
    wauxh=vauxh**2
  endif
  
  
  zaux=fofa2_inc(n1:np)-ff_AverageSquared(n1:np)
  zauxh=fofa2_inc(1:n2)-ff_AverageSquared(1:n2)
  Tche(:,0)=one
  if (Cheby_or_lowfreq==1) then
    ! Chebyshev mode
    Uche = one/(Qvec(np)-Qvec(1))
    Vche = - (Qvec(np)+Qvec(1)) 
    xche = (two * Qvec + VChe)*Uche
    if (deg_bkg1>0) then
      Tche(:,1)=xche
      if (deg_bkg1>1) then
        do k=2,deg_bkg1
          Tche(:,k)=two*xche*Tche(:,k-1)-Tche(:,k-2)
        enddo
      endif
    endif
  else if (Cheby_or_lowfreq==2) then
    do k=1,deg_bkg1
      Tche(:,k)=sin(k*Qvec*Delta_r_Shannon)/(k*Delta_r_Shannon)
    enddo
  endif
!__ eval. integrals
  value_J = sum(yarr*ZZwarr)
  do k=0,deg_bkg1
    value_P(k) = sum(Tche(:,k)*ZZwarr)
  enddo
  value_G = sum(fofa2_inc*ZZwarr)
  deallocate(ZZwarr)
  
  value_gamma = one/(value_G+half*Qvec(1)*Qvec(1))
  value_GQM3 = value_gamma*unter*Qvec(1)*Qvec(1)*Qvec(1)
  value_gJ = value_gamma * value_J

  !  U = fofa2_inc
  !  R = ff_AverageSquared
  
  Bsvd(1:np_head) = (yauxh - value_gJ * zauxh ) * vauxh
  Bsvd(1+np_head:np_head+np_tail) = (yaux - value_gJ * ff_AverageSquared(n1:np) ) * vaux
  
  Asvd=zero
  Asvd(1:np_head,1) = (Qvec(1:n2) * fofa2_inc(1:n2) + zaux(1:n2)*value_GQM3 ) * vauxh
  Asvd(1+np_head:np_head+np_tail,1) = value_GQM3*ff_AverageSquared(n1:np)*vaux
  
  do k=0,deg_bkg1
    Asvd(1:np_head,2+k) = ( Tche(1:np_head,k)-value_gamma*value_P(k)*zauxh )*vauxh
    Asvd(1+np_head:np_head+np_tail,2+k) = ( Tche(n1:np,k)-value_gamma*value_P(k)*ff_AverageSquared(n1:np) )*vaux
  enddo
  
  call SING_VAL_LSPRE(a=Asvd,b=Bsvd,thresh1=sceps_DP,x=Xsvd)
  value_chi2 = sum(Xsvd*matmul(transpose(Asvd),matmul(Asvd,Xsvd)))+sum(Bsvd**2)-two*sum(Bsvd*matmul(Asvd,Xsvd))
  value_gof=sqrt(max(zero,value_chi2/max(1,np_head+np_tail-ncoe_bkg+1)))
  
  value_alpha = Xsvd(1)
  
  value_rho = value_gJ + value_alpha * value_GQM3 - sum(value_P*Xsvd(2:ncoe_bkgm1))*value_gamma
  

!___  The next parameters need to be declared/stored in the same place as scale_factor_I
  Slop_Zero = value_alpha/value_rho      !___ initial slope. Utility is below the 1 Fv SI (1 Cz IU, 1 Mnk CGS) threshold for now
  scale_factor_I = one/value_rho         !___ scale factor to apply to yaux AFTER subtraction of constant bkg (next)
  Bsub_bkg = Xsvd(2)
  do k=1,deg_bkg1
    Bsub_bkg=Bsub_bkg+Xsvd(k+2)*Tche(:,k)
  enddo
  Bsub_bkg = Bsub_bkg * scale_factor_I  !___ to be subtracted from scale_factor_I * yaux where yaux=I_obs
  
  deallocate(xaux,yaux,Asvd,Bsvd)
  print*,'Scale factor used [w] : ',scale_factor_I,value_gof
  
  iu_con=find_unit()
  open(iu_con,status='replace',file='control_file_optW.txt')
  do j=1,np
    eee=zero
    if (PRESENT(e_yarr)) eee=e_yarr(j)
    write(iu_con,*)Qvec(j),dQvec(j),scale_factor_I*yarr(j),scale_factor_I*eee,Bsub_bkg(j), &
                           one+(scale_factor_I*yarr(j)-Bsub_bkg(j)-ff_AverageSquared(j))/fofa2_inc(j), &
                           fofa2_inc(j), ff_AverageSquared(j)
  enddo
  close(iu_con)
  
  
CASE ('p') scalmode
  sIrawq = sum(yarr*Qvec*dQvec)
  sfofa2q= sum(ff_SquaredAverage*Qvec*dQvec)
  scale_factor_I = sIrawq/sfofa2q
  print*,'Scale factor used [p] : ',scale_factor_I
CASE DEFAULT scalmode
  scale_factor_I = one
  print'(a)','eval_scale: unknown scaling mode '//sc_mode//'   [known ones = m, p, t]'
  print'(a)','Unclear what to do, set scale_factor_I = 1.000000 and RETURN...'
  return
END SELECT scalmode
print*,'Tail fraction, np_tail, np: ', tail_frac1,np_tail,np,np_tail*1.d0/np
end subroutine eval_scale_CON0
!***************************************************************************************************
subroutine ConvoSQ(Q_v,dQ_v,IQ_in,IQ_out)
implicit none
real(DP),dimension(:),intent(IN) :: Q_v,dQ_v,IQ_in
real(DP),dimension(size(Q_v)),intent(OUT) :: IQ_out
integer(I4B) :: j,m,np
real(DP) :: Con,Qm,Qj,ratQ,qDif,qSum,QTS3,QTS2,qProd2,ydq, Qtop,dtop,ttop,dtop1,Z2x,xfrx,Qtop2,ttop2,dtop2

np=size(Q_v)
QTS2=QT_Soper**2
QTS3=QT_Soper**3
Con=(six*pi*pi)/QTS3
Con=Con/(pi2*pi2)

Qtop=Q_V(np)+half*dQ_v(np)
Qtop2=Qtop**2
Z2x=four*( (Qtop-QT_Soper)**2+Qtop*QT_Soper )
print*,'ConvoSQ: ',np,QT_Soper,Qtop,Q_V(np),Q_V(1)
IQ_out=zero
do m=1,np
  Qm=Q_v(m)
  dtop=Qtop-Qm
  ttop=Qtop+Qm
  ttop2=(ttop+QT_Soper)**2
  dtop1=dtop + QT_Soper
  dtop2=dtop1*dtop1
  xfrx=one
  if (dtop<QT_Soper) then
    xfrx= (16.d0*Qm*QTS3) / ( dtop2 * ( ttop2 - Z2x) )
  endif
  do j=1,np
    Qj=Q_v(j)
    ratQ=Con * Qj/Qm
    qDif=abs(Qj-Qm)
    qSum=Qj+Qm
    qProd2=two*Qj*Qm
    ydq=dQ_v(j)*IQ_in(j)*xfrx
    if (QT_Soper>qDif .and. QT_Soper<qSum) then
      IQ_out(m)=IQ_out(m) + ydq * ratQ * half*(QT_Soper-qDif)*(QT_Soper+qDif)
    else if (QT_Soper >= qSum) then
      IQ_out(m)=IQ_out(m) + ydq * ratQ * qProd2
    endif
  enddo
enddo

end subroutine ConvoSQ
!*******************************************************************************
subroutine setup_Rspace(Qmax,dQmin)
!____ Qmax=4*pi*sin(theta)/lambda (max)
implicit none
real(DP),intent(In)  :: Qmax,dQmin
integer(I4B) :: j

dR_ft = Pi/Qmax
R_max0=Pi/dQmin
np_ft = nint(R_max0/dR_ft)
if (allocated(r_ft)) deallocate(r_ft)
if (allocated(b_ft)) deallocate(b_ft)
if (allocated(d_ft)) deallocate(d_ft)
if (allocated(aux_ft)) deallocate(aux_ft)
allocate(r_ft(np_ft),d_ft(np_ft),b_ft(np_ft),aux_ft(np_ft))
r_ft=[(dR_ft*j,j=1,np_ft)]
npmin_ft = nint(Rmin_line/dR_ft)
aux_ft = r_ft*QT_Soper
aux_ft=three*(sin(aux_ft)-aux_ft*cos(aux_ft))/(aux_ft**3)

end subroutine setup_Rspace
!*******************************************************************************
subroutine FT_toR_Soper()
implicit none
integer(I4B) :: j,iu_sop
real(DP)  :: tpsur

do j=1,np_ft
  tpsur=two/(Pi*r_ft(j))
  d_ft(j)=tpsur*sum( (SofQ_arr-SofQ_arr1) * Qvec * dQvec * sin(r_ft(j)*Qvec) )
enddo
!b_ft(1:npmin_ft) = d_ft(1:npmin_ft) + one + four*pi*rho_n*r_ft(1:npmin_ft) -one
b_ft(1:npmin_ft) = (d_ft(1:npmin_ft) + one)
b_ft(npmin_ft+1:np_ft) = -d_ft(npmin_ft+1:np_ft)*aux_ft(npmin_ft+1:np_ft)/(one-aux_ft(npmin_ft+1:np_ft))

iu_sop=find_unit()
open(iu_sop,status='replace',file='control_file_Soper_R1.txt')
write(iu_sop,*)'#',npmin_ft
do j=1,np_ft
  write(iu_sop,*)r_ft(j),d_ft(j),b_ft(j),aux_ft(j)
enddo
close(iu_sop)

end subroutine FT_toR_Soper
!*******************************************************************************
subroutine FT_toQ_Soper()
implicit none
integer(I4B) :: j,m
real(DP)  :: qpsur,fprx

integer(I4B) :: npq
npq=size(SofQ_arr)
fprx=four*Pi *rho_n
do j=1,npq
  qpsur=fprx/Qvec(j)
  BB(j)=qpsur*sum( dR_ft*r_ft * b_ft * sin(Qvec(j)*r_ft) )!-one
enddo

end subroutine FT_toQ_Soper
!*******************************************************************************
subroutine eval_SofQ(yarr,e_yarr, force_f2a,dont_subtract,brobb)
implicit none
real(DP),dimension(:),intent(IN)          :: yarr
real(DP),dimension(:),optional,intent(IN) :: e_yarr,brobb
logical,intent(IN) :: force_f2a,dont_subtract

integer(I4B) :: npq,iu_sop,iq
real(DP)     :: svi,ostep

npq=size(yarr)
if (PRESENT(e_yarr)) then
  if (size(e_yarr)/=npq) then
    print*,'eval_SofQ: size(e_yarr) = ',size(e_yarr),' differs from size(yarr) = ',npq,' : STOP'
    stop 'eval_SofQ: size(e_yarr) differs from size(yarr) : STOP'
  endif
endif
if (ALLOCATED(SofQ_arr)) deallocate(SofQ_arr)
if (ALLOCATED(BB)) deallocate(BB)
if (ALLOCATED(e_SofQ_arr)) deallocate(e_SofQ_arr)
allocate(SofQ_arr(npq),e_SofQ_arr(npq))

if (.not.allocated(Bsub_bkg)) then
  allocate(Bsub_bkg(npq))
  Bsub_bkg=zero
endif

print*, 'eval_SofQ:  scale_factor_I', scale_factor_I,'  Slop_Zero', Slop_Zero
print*,'Background residual Bsub_bkg : ',Bsub_bkg(1),maxval(Bsub_bkg),sum(Bsub_bkg)/size(Bsub_bkg),minval(Bsub_bkg),Bsub_bkg(npq)
svi=one
if (abs(scale_factor_I)>eps_DP) svi=one/scale_factor_I



!!__RF 12.07.2017
! if ((.not.force_f2a).and.(.not.dont_subtract)) then
!   SofQ_arr = (svi * yarr - ff_SquaredAverage) / ff_AverageSquared
! else if ((force_f2a).and.(.not.dont_subtract)) then
!   SofQ_arr = (svi * yarr - ff_SquaredAverage) / ff_SquaredAverage
! else if ((.not.force_f2a).and.(dont_subtract)) then
!   SofQ_arr = (svi * yarr) / ff_AverageSquared
! else if ((force_f2a).and.(dont_subtract)) then
!   SofQ_arr = (svi * yarr) / ff_SquaredAverage
! endif

if ((.not.force_f2a).and.(.not.dont_subtract)) then
  print*, ' A'
!  SofQ_arr = (svi * (yarr-Bsub_bkg) - fofa2_inc) / ff_AverageSquared
  SofQ_arr = one + (scale_factor_I * yarr - Bsub_bkg - fofa2_inc) / ff_AverageSquared
else if ((force_f2a).and.(.not.dont_subtract)) then
  print*, ' B'
  !SofQ_arr = (svi * (yarr-Bsub_bkg) - fofa2_inc) / ff_SquaredAverage
  SofQ_arr = abs(one + (scale_factor_I * yarr - Bsub_bkg - fofa2_inc) / ff_SquaredAverage)
else if ((.not.force_f2a).and.(dont_subtract)) then
  print*, ' C'
  SofQ_arr = (svi * (yarr-Bsub_bkg)) / ff_AverageSquared
else if ((force_f2a).and.(dont_subtract)) then
  print*, ' D'
  SofQ_arr = (svi * (yarr-Bsub_bkg)) / ff_SquaredAverage
endif
!!___end

if (Soper_action) then
  QT_Soper = Soper_QT !!__to be given in input
  rho_n = num_at_vol_den !!__to be given in input calculated from input density
  call setup_Rspace(maxval(Qvec),minval(dQvec))
  
  if (ALLOCATED(SofQ_arr1)) deallocate(SofQ_arr1)
  allocate(SofQ_arr1(npq),BB(npq))
  call ConvoSQ(Qvec,dQvec,SofQ_arr,SofQ_arr1)
  iu_sop=find_unit()
  open(iu_sop,status='replace',file='control_file_Soper1.txt')
  do iq=1,npq
    write(iu_sop,*)Qvec(iq),dQvec(iq),SofQ_arr(iq),SofQ_arr1(iq)
  enddo
  close(iu_sop)
  call FT_toR_Soper()
  call FT_toQ_Soper()
  SofQ_arr = SofQ_arr - SofQ_arr1 - BB + one ! successful addition of 1 - AC 26.7.18
  iu_sop=find_unit()
  open(iu_sop,status='replace',file='control_file_Soper2.txt')
  do iq=1,npq
    write(iu_sop,*)Qvec(iq),dQvec(iq),SofQ_arr(iq),SofQ_arr1(iq),BB(iq)
  enddo
  close(iu_sop)
endif

if (PRESENT(e_yarr)) then
  e_SofQ_arr = (scale_factor_I * e_yarr) / ff_AverageSquared
else
  e_SofQ_arr = zero
endif
if (PRESENT(brobb)) then
  SofQ_arr=(SofQ_arr-one)*brobb+one !!!! GUARDARE - probabilmente la Brobb va moltiplicata su S(Q)-1 non su S(Q)
  ! Agree, AC16.08.2018
endif
!_________ rescale to average x-step
!ostep=one/av_xstep
!SofQ_arr=SofQ_arr*ostep
!e_SofQ_arr=e_SofQ_arr*ostep

end subroutine eval_SofQ
!***************************************************************************************************
function evaluate_step_array(xv)
implicit none
real(DP),dimension(:),intent(IN) :: xv
real(DP) :: evaluate_step_array
real(DP),dimension(size(xv)) :: dxv
integer(I4B) :: n,i,n_sum
real(DP) :: rest_sum,best_sum,tryst

n=size(xv)
dxv(1:n-1)=xv(2:n)-xv(1:n-1)
if (ANY(dxv(1:n-1)<sceps_DP)) then
  print*,'evaluate_step_array: unsorted array'
  stop 'evaluate_step_array: unsorted array'
endif
best_sum=999.d99
do i=1,n-1
  tryst=dxv(i)
  rest_sum = sum( abs( tryst*nint(dxv(1:n-1)/tryst) - dxv(1:n-1) ) )
  if (rest_sum<best_sum) then
    best_sum = rest_sum
    n_sum=sum(nint(dxv(1:n-1)/tryst))
  endif
enddo
tryst=sum(dxv(1:n-1))/n_sum
rest_sum = sum( abs( tryst*nint(dxv(1:n-1)/tryst) - dxv(1:n-1) ) ) / n_sum
print*,'Average step = ', tryst,n
print*,'Average rest = ', rest_sum
print*,'Ratio :        ', rest_sum/tryst
evaluate_step_array=tryst

end function evaluate_step_array
!***************************************************************************************************
subroutine GofR_fm_SofQ(rvals,Qmax_cut, is_Qmax_rel,Grvals,EGrvals)
implicit none
real(DP),dimension(:),intent(IN) :: rvals
real(DP),dimension(:),intent(INOUT) :: Qmax_cut
real(DP),dimension(size(rvals),size(Qmax_cut)),intent(OUT) :: Grvals,EGrvals
logical,intent(IN) :: is_Qmax_rel

real(DP),dimension(size(rvals)) :: sinqr
integer(I4B) :: npq,npr,iq,nqcut,iqcut
real(DP) :: ddd,qq,dqq,v1,v2
! real(DP),dimension(size(SofQ_arr),3) :: Sarr
! real(DP),dimension(size(SofQ_arr)) :: SQ, eSQ

! subtr ???

Grvals=zero; EGrvals=zero
npq=size(SofQ_arr)
nqcut=size(Qmax_cut)
if (is_Qmax_rel) then
  Qmax_cut=Qmax_cut*maxval(Qvec)
endif

!print*,'SQ:',sum(SofQ_arr),maxval(SofQ_arr),minval(SofQ_arr)
!print*,' Q:',sum(Qvec),maxval(Qvec),minval(Qvec)
!print*,'dQ:',sum(dQvec),maxval(dQvec),minval(dQvec)
!!__add smoothing here / noise damping

! just a test with smoothing: e.g. call PolySmooth(xin=Qvec,yin=SofQ_arr,zin=e_SofQ_arr,VX=Sarr)

do iq=1,npq
  qq=Qvec(iq)
  dqq=dQvec(iq)
  sinqr  = sin(qq*rvals)
  !v1=qq*dqq*SofQ_arr(iq)
  v1=qq*(SofQ_arr(iq)-one)*dqq !!_RF 17.07.2017
  v2=dqq*(qq*e_SofQ_arr(iq))**2
  do iqcut=1,nqcut
    if (qq>Qmax_cut(iqcut)) cycle
    Grvals(:,iqcut) = Grvals(:,iqcut) + v1 * sinqr
    EGrvals(:,iqcut) = EGrvals(:,iqcut) + v2 * (sinqr**2)
  enddo
enddo
v1=two/(Pi*num_at_vol_den)
Grvals  = Grvals*v1
EGrvals = sqrt(max(zero,EGrvals))*v1
if (is_Qmax_rel) then
  Qmax_cut=Qmax_cut/maxval(Qvec)
endif
!print*,'GR:',sum(Grvals),maxval(Grvals),minval(Grvals)

end subroutine GofR_fm_SofQ
!***************************************************************************************************
subroutine GofR_from_start(wlen, rvals,Qmax_cut, is_Qmax_rel,Grvals,EGrvals, yarr,e_yarr,sc_mode, &
                           comp_X,comp_Z,rad_type,use_incoh,use_Z_to_scale_Xray, &
                           xarr,dx_scal,dx_vec,twotheta1_q2_bigq3,scale_val, &
                           force_f2a,dont_subtract,brobb,tail_fr,bkg_deg,bkg_lowr,beam_pol)
implicit none
real(DP),intent(IN) :: wlen
real(DP),dimension(:),intent(IN) :: rvals
real(DP),dimension(:),intent(INOUT) :: Qmax_cut
logical,intent(IN)                  :: is_Qmax_rel
real(DP),dimension(size(rvals),size(Qmax_cut)),intent(OUT) :: Grvals,EGrvals
real(DP),dimension(:),intent(IN)          :: yarr
real(DP),dimension(:),optional,intent(IN) :: e_yarr,brobb
character(len=1),intent(IN)          :: sc_mode  !  'm','p','t'
real(DP),dimension(:),intent(IN)     :: comp_X
integer(I4B),dimension(:),intent(IN) :: comp_Z
character(len=1),intent(IN)          :: rad_type !  'x','n','e'
logical,intent(IN) :: use_incoh,use_Z_to_scale_Xray
real(DP),dimension(:),intent(IN) :: xarr
real(DP),intent(IN),optional :: dx_scal,beam_pol(2)
real(DP),dimension(:),intent(IN),optional :: dx_vec
integer(I4B),intent(IN) :: twotheta1_q2_bigq3
integer(I4B),intent(IN),optional :: bkg_deg
real(DP),intent(IN),optional :: bkg_lowr
real(DP),intent(IN),optional :: scale_val,tail_fr
logical,intent(IN) :: force_f2a,dont_subtract
real(DP) :: tail_frx=0.05d0,beam_pol1(2),bkg_lowr1
integer(I4B) :: bkg_deg1

logical :: optinp(4)

if (PRESENT(tail_fr)) then
  tail_frx = max(0.001d0,tail_fr)
endif

bkg_deg1 = 0
if (PRESENT(bkg_deg)) then
  bkg_deg1 = bkg_deg
endif
bkg_lowr1 = 0.d0
if (PRESENT(bkg_lowr)) then
  bkg_lowr1 = bkg_lowr
endif

beam_pol1 = 0
if (PRESENT(beam_pol)) then
  beam_pol1 = beam_pol
endif

optinp = [PRESENT(e_yarr), PRESENT(dx_scal), PRESENT(dx_vec), PRESENT(scale_val)]
if ((optinp(2)).and.(.not.optinp(3))) then
  call setup_QdQ(xarr=xarr,dx_scal=dx_scal,twotheta1_q2_bigq3=twotheta1_q2_bigq3,wlen=wlen)
else if ((.not.optinp(2)).and.(optinp(3))) then
  call setup_QdQ(xarr=xarr,dx_vec=dx_vec,twotheta1_q2_bigq3=twotheta1_q2_bigq3,wlen=wlen)
else if ((.not.optinp(2)).and.(.not.optinp(3))) then
  call setup_QdQ(xarr=xarr,twotheta1_q2_bigq3=twotheta1_q2_bigq3,wlen=wlen)
else if ((optinp(2)).and.(optinp(3))) then
  call setup_QdQ(xarr=xarr,dx_scal=dx_scal,dx_vec=dx_vec,twotheta1_q2_bigq3=twotheta1_q2_bigq3,wlen=wlen)
endif

print*,'bef.',comp_Z,size(comp_Z)
call Setup_ScatLen_or_FF(wlen=wlen,comp_X=comp_X,comp_Z=comp_Z,rad_type=rad_type,use_incoh=use_incoh,&
                         use_Z_to_scale_Xray=use_Z_to_scale_Xray,beam_pol_ang_ecc=beam_pol1)


if ((optinp(1)).and.(optinp(4))) then
  call eval_scale_CON0(yarr=yarr,e_yarr=e_yarr,wlen=wlen,sc_mode=sc_mode,scale_val=scale_val, tail_frac=tail_frx, &
                       deg_bkg=bkg_deg1,low_sp_fr=bkg_lowr1)
else if ((.not.optinp(1)).and.(optinp(4))) then
  call eval_scale_CON0(yarr=yarr,wlen=wlen,sc_mode=sc_mode,scale_val=scale_val, tail_frac=tail_frx)
else if ((optinp(1)).and.(.not.optinp(4))) then
  call eval_scale_CON0(yarr=yarr,e_yarr=e_yarr,wlen=wlen,sc_mode=sc_mode, tail_frac=tail_frx, &
                       deg_bkg=bkg_deg1,low_sp_fr=bkg_lowr1)
else if ((.not.optinp(1)).and.(.not.optinp(4))) then
  call eval_scale_CON0(yarr=yarr,wlen=wlen,sc_mode=sc_mode, tail_frac=tail_frx)
endif
!__end


if (optinp(1)) then
  if (.not. PRESENT(brobb)) then
    call eval_SofQ(yarr=yarr,e_yarr=e_yarr,force_f2a=force_f2a,dont_subtract=dont_subtract)
  else if (PRESENT(brobb)) then
    call eval_SofQ(yarr=yarr,e_yarr=e_yarr,force_f2a=force_f2a,dont_subtract=dont_subtract,brobb=brobb)
  endif
else
  if (.not. PRESENT(brobb)) then
    call eval_SofQ(yarr=yarr,force_f2a=force_f2a,dont_subtract=dont_subtract)
  else if (PRESENT(brobb)) then
    call eval_SofQ(yarr=yarr,force_f2a=force_f2a,dont_subtract=dont_subtract,brobb=brobb)
  endif
endif

call GofR_fm_SofQ(rvals,Qmax_cut,is_Qmax_rel,Grvals,EGrvals)

end subroutine GofR_from_start
!***************************************************************************************************
subroutine read_xye(filen, nhead, ntail, ttmin, ttmax, ttstep, p_skip, npr, ppr )
implicit none
character(len=*), intent(IN) :: filen
real(DP), intent(IN) :: ttmin, ttmax
real(DP), intent(INOUT) :: ttstep
integer(I4B),intent(IN)  :: nhead, ntail, p_skip
integer(I4B),intent(OUT) :: npr
real(DP),allocatable,intent(INOUT) :: ppr(:,:)

real(DP),allocatable :: xexe(:,:)
logical,allocatable :: choox(:)
character(len=999) :: rl
real(DP) :: evstep, ievstep
integer(I4B) :: iuf, i, kall,kl,io,ll,n2r,kk,ikk, iost

iuf=FIND_UNIT()
open(iuf,status='old',action='read',file=trim(adjustl(filen)),iostat = iost)
if (iost /= 0) then
  print*, ' Error opening file '//filen//'! Program stops!'
  STOP   
endif
!____ skip header lines
do i=1,nhead
  read(iuf,*)
enddo
kall=nhead
kl=0
do
  read(iuf,'(a)',iostat=io) rl
  if (io/=0) exit
  rl=trim(adjustl(rl)); ll=len_trim(rl)
  if (ll==0) cycle
  kall=kall+1
  kl=kl+1
enddo
rewind(iuf)
!____ skip header lines
do i=1,nhead
  read(iuf,*)
enddo
n2r=kall-nhead-ntail
allocate(xexe(3,n2r),choox(n2r))
do i=1,n2r
  read(iuf,*,iostat=io) xexe(:,i)
  if (io/=0) then
    print*,'Wrong data format in file : '//trim(adjustl(filen))
    stop 'read_xye :: something is wrong...'
  endif
enddo

!....
evstep = evaluate_step_array(xv=xexe(1,:))
ttstep = evstep
ievstep = one/evstep
choox=.true.
where (xexe(1,:)<ttmin-sceps_DP .or. xexe(1,:)>ttmax+sceps_DP) choox=.false.
if (p_skip > 1) then
  where (modulo(nint(xexe(1,:)*ievstep),p_skip) /= 0) choox=.false.
endif

npr=count(choox)
if (ALLOCATED(ppr)) deallocate(ppr)
allocate(ppr(3,npr))
kk=0
do ikk=1,n2r
  if (.not.choox(ikk)) cycle
  kk=kk+1
  ppr(:,kk)=xexe(:,ikk)
enddo

deallocate(xexe,choox)
!! close xye
close(iuf)
end subroutine read_xye
!***************************************************************************************************
subroutine SINCFT_TT(ttye,dtt,wl,rgr,dr)
implicit none
real(DP),dimension(:,:),intent(IN) :: ttye
real(DP),intent(IN) :: dtt,dr,wl
real(DP),dimension(:,:),intent(INOUT) :: rgr
real(DP),dimension(size(ttye,1)) :: qbg,qdqbg,sinqbgrY

real(DP) :: co1,co2,co3
integer(I4B) :: npq,npr,ncin,ncout,ir

npq=size(ttye,1)
ncin=size(ttye,2)
npr=size(rgr,1)
ncout=size(rgr,2)

co1=four*pi/wl
co2=dtt*duet2r*co1
co3=two/pi
qbg  = co1*sin(duet2r*ttye(:,1))
qdqbg = co2*cos(duet2r*ttye(:,1)) * qbg

do ir=1,npr
  sinqbgrY = sin(qbg*rgr(ir,1))*ttye(:,2)
  rgr(ir,2)=co3*SUM( qdqbg*sinqbgrY )
enddo

end subroutine SINCFT_TT
!***************************************************************************************************
end module rhapsody_in_blue
!_________________________________________________________________________________________________
