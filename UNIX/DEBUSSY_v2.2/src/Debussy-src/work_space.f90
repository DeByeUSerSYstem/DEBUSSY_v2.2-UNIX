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
MODULE OBS_WSPACE
Use SPECIAL_TYPES

 TYPE(data_SETS),DIMENSION(:),pointer,save   :: OBS_DATA_W => NULL()
 TYPE(IRF_SETS),DIMENSION(:),pointer,save    :: IRF_CURVES_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: NDATA_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: ILAMBDA_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: MONO_POSIT_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: DB_INDEX_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: ITYPE_DB_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: N2USE_W => NULL()
 REAL(CP),DIMENSION(:,:), pointer,save       :: LAMBDAS_W => NULL()
 REAL(CP),DIMENSION(:,:), pointer,save       :: POLARIZB_W => NULL()
 REAL(CP),DIMENSION(:), pointer,save       :: COBRA_W => NULL()
 REAL(CP),DIMENSION(:,:), pointer,save       :: CELL_P_W => NULL()
 INTEGER(I4B),DIMENSION(:,:), pointer,save   :: Z_ATOM_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: NSP_AT_W => NULL()
 INTEGER(I4B),DIMENSION(:), pointer,save     :: NPAIR_AT_W => NULL()
 INTEGER(I4B),pointer,save                   :: NSTR_W => NULL(), NSET_W => NULL(), NSET_BACK_W => NULL(), &
                                                NSPECMAX_W => NULL(), NAMO_W => NULL() 
 CHARACTER(5),DIMENSION(:),pointer,save      :: TECHNIQUE_W => NULL()
 CHARACTER(1),DIMENSION(:),pointer,save      :: XDATA_TYPE_W => NULL()
 CHARACTER(1),DIMENSION(:),pointer,save      :: RADIATION_W => NULL()
 CHARACTER(128),DIMENSION(:),pointer,save    :: STRUCTURE_NAME_W => NULL()
 CHARACTER(128),DIMENSION(:),pointer,save    :: BLANK_FILENAME_W => NULL()
 INTEGER(I4B),DIMENSION(:),pointer,save      :: BLANK_NCOMPS_W => NULL()
 INTEGER(I4B),DIMENSION(:),pointer,save      :: POROD_BKG_W => NULL()
 CHARACTER(128),pointer,save                 :: REFINEMENT_FILE_W => NULL(),OUTPUT_FILE_W => NULL()
 INTEGER(I4B),DIMENSION(:),pointer,save      :: CHEB_NC_W => NULL(), YOUNG_NC_W => NULL()
 REAL(CP), pointer,save                      :: QMAX_D_W => NULL()
 REAL(CP), pointer,save                      :: AMO_DCORR_W => NULL() 
 REAL(CP), pointer,save                      :: AMO_DCORR0_W => NULL() 
 LOGICAL, pointer,save                       :: DO_AMORPH_W => NULL()
 LOGICAL, pointer,save                       :: SIM_NODATA_W => NULL()
 TYPE(PHA_INFO),DIMENSION(:),pointer,save    :: ALL_PHA_INFO_W => NULL()
 LOGICAL,save                                :: OBS_WSPACE_DONE = .false.
 INTEGER(I4B),DIMENSION(:),pointer,save      :: INST_FLAG_W => NULL()

!DATA NSTR_W/NULL()/
!DATA NSET_W/NULL()/
!DATA NSET_BACK_W/NULL()/
!DATA NSPECMAX_W/NULL()/
!DATA NAMO_W/NULL()/
!DATA QMAX_D_W/NULL()/


END MODULE OBS_WSPACE
!_______________________________________________________________________________

MODULE BACKCO
 USE OBS_WSPACE
 USE LINALG_TOOLS

 TYPE,public     :: BKGR_TERMS
   INTEGER(I4B)                    :: dimback, che_back, nbpol,nbbla 

!_ che_back = flag  2=Young,
!_                  3=Chebyshev,
!_                  4=Chebyshev+Blank

   REAL(DP)                        :: min_of_B
   REAL(CP),DIMENSION(:,:),Allocatable :: lin_bkgr_sep 
   REAL(CP),DIMENSION(:),Allocatable   :: lin_bkgr_tot 
   REAL(CP),DIMENSION(:),Allocatable   :: lin_bkgr_blank 
   REAL(CP),DIMENSION(:),Allocatable   :: LORCORR2 
 END TYPE BKGR_TERMS

 TYPE(BKGR_TERMS),DIMENSION(:),POINTER   :: BACKGROUND

 INTEGER, PARAMETER     :: nmax_Y = 11, nmax_C = 50, nmax_B = 1
 

CONTAINS

 SUBROUTINE PREPARE_BACK
  IMPLICIT NONE
  INTEGER(I4B) :: i

  IF (.not.OBS_WSPACE_DONE) STOP 'FATAL: obs-workspace not ready!'

  ALLOCATE(BACKGROUND(NSET_W))

  datasets:DO i = 1, NSET_W
  !! Defaults :: const. bkg.

     BACKGROUND(i)%che_back = 2  ! Young
     BACKGROUND(i)%dimback  = 1  ! 1 coeff. (constant)

  !! Actual

     IF (LEN_TRIM(BLANK_FILENAME_W(i)) == 0 .and. CHEB_NC_W(i) == 0) THEN
       ! Use YOUNG
       BACKGROUND(i)%che_back = 2            ! Young
       BACKGROUND(i)%dimback  = YOUNG_NC_W(i)  ! 
       BACKGROUND(i)%nbpol    = BACKGROUND(i)%dimback
       BACKGROUND(i)%nbbla    = 0
     ELSE IF (LEN_TRIM(BLANK_FILENAME_W(i)) == 0 .and. YOUNG_NC_W(i) == 0) THEN
       ! Use CHEB
       BACKGROUND(i)%che_back = 3            ! Cheb
       BACKGROUND(i)%dimback  = CHEB_NC_W(i)  ! 
       BACKGROUND(i)%nbpol    = BACKGROUND(i)%dimback
       BACKGROUND(i)%nbbla    = 0
     ELSE IF (LEN_TRIM(BLANK_FILENAME_W(i)) > 0 .and. CHEB_NC_W(i) >= 0) THEN
       ! Use BLANK + CHEB
       BACKGROUND(i)%che_back = 4            ! Blank + Cheb
       BACKGROUND(i)%dimback  = CHEB_NC_W(i) + BLANK_NCOMPS_W(i)  !
       BACKGROUND(i)%nbpol    = BACKGROUND(i)%dimback-BLANK_NCOMPS_W(i)
       BACKGROUND(i)%nbbla    = BLANK_NCOMPS_W(i)
     ENDIF

     if (allocated(BACKGROUND(i)%lin_bkgr_sep)) deallocate(BACKGROUND(i)%lin_bkgr_sep)
     allocate(BACKGROUND(i)%lin_bkgr_sep(NDATA_W(i),BACKGROUND(i)%dimback))
     SELECT CASE(BACKGROUND(i)%che_back)
     CASE(2)
        CALL MAKE_YOUNG(i)
     CASE(3)
        CALL MAKE_CHEBYSHEV(i)
     CASE(4)
        CALL MAKE_CHEBYSHEV(i)
        CALL READ_BLANK(i)
     END SELECT

     CALL FILL_CORR_LOR(i)

     ALLOCATE(BACKGROUND(i)%lin_bkgr_tot(NDATA_W(i)), BACKGROUND(i)%lin_bkgr_blank(NDATA_W(i)))
     BACKGROUND(i)%lin_bkgr_tot(1:NDATA_W(i)) = zero
     BACKGROUND(i)%lin_bkgr_blank(1:NDATA_W(i)) = zero

   ENDDO datasets

 END SUBROUTINE PREPARE_BACK
!***********************************************
 SUBROUTINE FILL_CORR_LOR(i)
  implicit none
  INTEGER(I4B),INTENT(in)       :: i
  INTEGER(I4B)                  :: ntheta2
  REAL(CP),DIMENSION(:),POINTER :: thetas => NULL()
  REAL(CP),DIMENSION(:,:),ALLOCATABLE :: polcor

  ntheta2 = NDATA_W(i)
  allocate(BACKGROUND(i)%LORCORR2(ntheta2))

  IF (ASSOCIATED(THETAS)) nullify(THETAS)
  THETAS => OBS_DATA_W(i)%t2data
  BACKGROUND(i)%LORCORR2(:) = one 
  !!! it would be 1.d0/( COS(cr1th*THETAS) ) if deltaq=const

  IF (RADIATION_W(i) == 'X') THEN 
     IF (MONO_POSIT_W(i) > 0) THEN
       allocate(polcor(1:2,ntheta2))
       IF (MONO_POSIT_W(i) == 1)  &
         polcor(MONO_POSIT_W(i),:) =  (one + COBRA_W(i)*COS(cr1th*THETAS)*COS(cr1th*THETAS))/(one+COBRA_W(i))
       IF (MONO_POSIT_W(i) == 2)  &
         polcor(MONO_POSIT_W(i),:) =  COBRA_W(i) + (one - COBRA_W(i)*COS(cr1th*THETAS)*COS(cr1th*THETAS))
         BACKGROUND(i)%LORCORR2(:) = BACKGROUND(i)%LORCORR2(:) *  polcor(MONO_POSIT_W(i),:) 
       deallocate(polcor)
     ENDIF
     BACKGROUND(i)%LORCORR2(:) = (one + COS(cr2th*THETAS)**2) * half
   ENDIF

  IF (ASSOCIATED(THETAS)) nullify(THETAS)  

 END SUBROUTINE FILL_CORR_LOR
!***********************************************
 SUBROUTINE READ_BLANK(i)
  implicit none
  INTEGER(I4B),INTENT(in)             :: i
  real(DP),allocatable  :: mcolread(:),tempb(:,:)
  INTEGER(I4B) :: ncolfil

   INTEGER(I4B)     :: ilog_blank,iopenblank,j, io,kpr,kpb,llbl,io2,ilox(1),kk,iwl
   REAL(CP)    :: xx,dxx,tol,mmbl(2),ttx,yl,yr,wl,wr
   character(1024) :: rlbl

   tol=1.d-5
   ilog_blank = FIND_UNIT()
   ncolfil=BLANK_NCOMPS_W(i)-POROD_BKG_W(i)
   open(ilog_blank,file=TRIM(ADJUSTL(BLANK_FILENAME_W(i))),status='old', &
        action='read',iostat=iopenblank)
   IF (iopenblank/=0) then
     if (ncolfil>0) then
       STOP 'Blank file not found '
     endif
   ENDIF
!_____ Count
   
   kpb=0
   kpr=0
   if (ncolfil>0) then
     if (allocated(mcolread)) deallocate(mcolread)
     allocate(mcolread(ncolfil))
     do
       READ(ilog_blank,'(a)',iostat=io)rlbl
       if (io/=0) exit
       rlbl=trim(adjustl(rlbl)); llbl=len_trim(rlbl)
       if (llbl==0) cycle
       if (ANY(['#','!','>']==rlbl(1:1))) cycle
       read(rlbl(1:llbl),*,iostat=io2) xx, mcolread(:)
       if (io2/=0) cycle
       kpb=kpb+1
     enddo
     rewind(ilog_blank)
     if (ALLOCATED(tempb)) deallocate(tempb)
     allocate(tempb(kpb,1+ncolfil))
 !    print*,'Count blank points = ',kpb
     do
       READ(ilog_blank,'(a)',iostat=io)rlbl
       if (io/=0) exit
       rlbl=trim(adjustl(rlbl)); llbl=len_trim(rlbl)
       if (llbl==0) cycle
       if (ANY(['#','!','>']==rlbl(1:1))) cycle
       read(rlbl(1:llbl),*,iostat=io2) xx, mcolread(:)
       if (io2/=0) cycle
       kpr=kpr+1
       tempb(kpr,1) = xx
       tempb(kpr,2:) = mcolread
     enddo
     close(ilog_blank)
     mmbl = [minval(tempb(1:kpr,1)),maxval(tempb(1:kpr,1))]
     do j=1,NDATA_W(i)
       xx=OBS_DATA_W(i)%t2data(j)
       if (xx<mmbl(1)-half*OBS_DATA_W(i)%step_data_t2) then
         print'(a,1x,f14.6,a,1x,f14.6,a)','Incorrect blank file: minimum angle ',mmbl(1),&
                ' is insufficiently low to cover the data [req. : ',xx,']'
         STOP 'Incorrect blank file: minimum angle insufficiently low to cover the data'
       endif
       if (xx>mmbl(2)+half*OBS_DATA_W(i)%step_data_t2) then
         print'(a,1x,f14.6,a,1x,f14.6,a)','Incorrect blank file: maximum angle ',mmbl(2),&
                ' is insufficiently high to cover the data [req. : ',xx,']'
         STOP 'Incorrect blank file: maximum angle insufficiently high to cover the data'
       endif
      !________________ Interpolating...
       ilox=MINLOC(ABS(xx-tempb(:,1)))
       ttx=xx-tempb(ilox(1),1)
       if (ttx>sceps_DP) then
         if (ilox(1)<kpb) then
           dxx=tempb(ilox(1)+1,1)-tempb(ilox(1),1)
           do kk=1,ncolfil
             yl=tempb(ilox(1),kk+1)
             yr=tempb(ilox(1)+1,kk+1)
             wl=(-xx+tempb(ilox(1)+1,1))/dxx
             wr=one-wl
             BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+kk)=wl*yl+wr*yr
           enddo
         else if (ilox(1)==kpb) then
           dxx=tempb(kpb,1)-tempb(kpb-1,1)
           do kk=1,ncolfil
             yl=tempb(kpb-1,kk+1)
             yr=tempb(kpb,kk+1)
             BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+kk)=yr+((ttx*(yr-yl))/dxx)
           enddo
         endif
       else if (ttx<-sceps_DP) then
         if (ilox(1)>1) then
           dxx=tempb(ilox(1),1)-tempb(ilox(1)-1,1)
           do kk=1,ncolfil
             yl=tempb(ilox(1)-1,kk+1)
             yr=tempb(ilox(1),kk+1)
             wr=(xx-tempb(ilox(1)-1,1))/dxx
             wl=one-wr
             BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+kk)=wl*yl+wr*yr
           enddo
         else if (ilox(1)==1) then
           dxx=tempb(2,1)-tempb(1,1)
           do kk=1,ncolfil
             yl=tempb(1,kk+1)
             yr=tempb(2,kk+1)
             BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+kk)=yl-((abs(ttx)*(yr-yl))/dxx)
           enddo
         endif
       else if (abs(ttx)<=sceps_DP) then
         do kk=1,ncolfil
           BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+kk)=tempb(ilox(1),kk+1)
         enddo
       endif
     enddo
   endif
   if (allocated(tempb)) deallocate(tempb)
   if (allocated(mcolread)) deallocate(mcolread)
!___ Porod term as last
   if (POROD_BKG_W(i)==1) then
     do j=1,NDATA_W(i)
       BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+BLANK_NCOMPS_W(i)) = one / &
       (max(eps_DP, ( obs_data_w(i)%qdata(j,1)**4 ) ))  ! q^-4 Porod term
       if (ILAMBDA_W(i)>1) then
         BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+BLANK_NCOMPS_W(i)) = &
           BACKGROUND(i)%lin_bkgr_sep(j,BACKGROUND(i)%nbpol+BLANK_NCOMPS_W(i)) + &
             LAMBDAS_W(0,i) / (max(eps_DP, ( obs_data_w(i)%qdata(j,2)**4 ) ))  ! q^-4 Porod term
       endif
     enddo
   endif
   
!print*,'RB debug: Porod, NB = ',POROD_BKG_W(i),BLANK_NCOMPS_W(i)
!do j=1,BLANK_NCOMPS_W(i)
!  print*,'RB debug: ',j,minval(BACKGROUND(i)%lin_bkgr_sep(:,j)),maxval(BACKGROUND(i)%lin_bkgr_sep(:,j))
!enddo

 END SUBROUTINE READ_BLANK

!***********************************************
 SUBROUTINE MAKE_CHEBYSHEV(i)
  implicit none
  INTEGER(I4B),INTENT(in)             :: i
  INTEGER(I4B)                        :: j, ifin
  REAL(DP)                            :: a,b


  ifin = BACKGROUND(i)%nbpol
  if (ifin==0) return
  
  b = MAXVAL(OBS_DATA_W(i)%t2data)
  a = two/(b-MINVAL(OBS_DATA_W(i)%t2data))
  b = one-a*b
  BACKGROUND(i)%lin_bkgr_sep(:,1)=one
  if (ifin==1) return
  BACKGROUND(i)%lin_bkgr_sep(:,2)= a * OBS_DATA_W(i)%t2data + b
  if (ifin==2) return
  do j=3,ifin
    BACKGROUND(i)%lin_bkgr_sep(:,j) = two*BACKGROUND(i)%lin_bkgr_sep(:,j-1)*BACKGROUND(i)%lin_bkgr_sep(:,2) &
                                    - BACKGROUND(i)%lin_bkgr_sep(:,j-2)
  enddo

 END SUBROUTINE MAKE_CHEBYSHEV
 !***********************************************
 SUBROUTINE MAKE_YOUNG(i)
  implicit none
  INTEGER(I4B),INTENT(in)       :: i
  INTEGER(I4B)                  :: j,k,iimin(1)
  REAL(DP)                      :: a,b, t2d, t2max, theta2_0, t2, duez

  a = MINVAL(OBS_DATA_W(i)%t2data)
  b = MAXVAL(OBS_DATA_W(i)%t2data)

   iimin = MINLOC(OBS_DATA_W(i)%vdata)
   theta2_0 = OBS_DATA_W(i)%t2data(iimin(1))

   t2max = MAXVAL(OBS_DATA_W(i)%t2data)
   t2d   = t2max-MINVAL(OBS_DATA_W(i)%t2data)
   do j=1,BACKGROUND(i)%dimback
     IF (j==1) THEN
       BACKGROUND(i)%lin_bkgr_sep(:,j) = one
       CYCLE
     ENDIF
     do k=1,NDATA_W(i) 
       t2 = OBS_DATA_W(i)%t2data(k)
       duez = (t2/theta2_0)-1.d0
       IF (ABS(duez)<sceps_DP) CYCLE
       BACKGROUND(i)%lin_bkgr_sep(k,j) = duez**(j-1)
     enddo
   enddo

 END SUBROUTINE MAKE_YOUNG

END MODULE backco
!___________________________________________________________________________________________________________________________________
module sicut
use linalg_tools
public
real(DP),parameter :: gatth=8.49042441684950824693087264685254568_DP, alpha0=2.0_DP/0.85_DP
real(DP),save      :: alpha,beta,zeta,gamma
real(DP),allocatable,save  :: Lef(:),Rig(:),Cen(:),Lar(:)
integer(I4B),save     :: ichofun=2

contains
!__________
function gfunv(v,i,ifun)
implicit none
real(DP),dimension(:),intent(IN) :: v
integer(I4B),intent(IN) :: i,ifun
real(DP),dimension(size(v))  :: gfunv

IF (ifun==1) then
  gfunv=BOXFV(v,i)
ELSE IF (ifun==2) then
  gfunv=GAUSV(v,i)
ELSE IF (ifun==3) then
  gfunv=SINCV(v,i)
ENDIF

end function gfunv
!__________
function sincv(v,i)
implicit none
real(DP),dimension(:),intent(IN) :: v
integer(I4B),intent(IN) :: i
real(DP),dimension(size(v))  :: sincv,vaux
real(DP)  :: sigg

sigg=Lar(i)/Pi
sincv=zero
vaux=abs(v-Cen(i))/sigg
where (abs(vaux)<0.001_DP)
  sincv = 1.0_DP+unses*(-1.0_DP+0.05_DP*vaux*vaux)*vaux*vaux
elsewhere
  sincv = sin(vaux)/vaux
end where

end function sincv
!__________
function gausv(v,i)
implicit none
real(DP),dimension(:),intent(IN) :: v
integer(I4B),intent(IN) :: i
real(DP),dimension(size(v))  :: gausv,vaux
real(DP)  :: sigg,hsm2

sigg=Lar(i)/sqrt(2*Pi)
hsm2=-half/(sigg*sigg)
gausv=zero
vaux=abs(v-Cen(i))
where (vaux<=gatth*sigg) gausv = exp(hsm2*vaux*vaux)

end function gausv
!__________
function boxfv(v,i)
implicit none
real(DP),dimension(:),intent(IN) :: v
integer(I4B),intent(IN) :: i
real(DP),dimension(size(v))  :: boxfv,vaux
real(DP)  :: sigg

sigg=Lar(i)*half
boxfv=zero
vaux=abs(v-Cen(i))
where (vaux<=sigg) boxfv = 1.0_DP

end function boxfv
!__________
subroutine gammafinder(nn,q1,q2)
implicit none
real(DP),intent(IN) :: q1,q2
integer(I4B),intent(IN) :: nn
integer(I4B) :: i1,i,iwhatdo,nn1,nn2
real(DP)  :: qamp

iwhatdo=2
IF (iwhatdo==0) then
  zeta  = q1/q2
  qamp  = q2-q1
  alpha = min(alpha0,half/zeta)
  IF (ALLOCATED(Lef)) deallocate(Lef)
  IF (ALLOCATED(Rig)) deallocate(Rig)
  IF (ALLOCATED(Lar)) deallocate(Lar)
  IF (ALLOCATED(Cen)) deallocate(Cen)
  allocate(Lef(nn),Rig(nn),Cen(nn),Lar(nn))

  gamma = (Psi_SUI0(nn)-alpha)/Psi_SSS0(nn)

  Lef(1)=q1
  Lar(1) = qamp/alpha
  do i=2,nn
    Lar(i) = Lar(1)/REAL(i,DP)
  enddo
  do i=1,nn-1
    i1=1+i
    Lef(i1) = Lef(i) + Lar(i) -gamma*sqrt(Lar(i)*Lar(i1))
  enddo
  do i=1,nn
    Rig(i) = Lef(i)+Lar(i)
    Cen(i) = Lef(i)+Lar(i)*half
  enddo
  Rig(nn) = q2
else if (iwhatdo==1) then
  zeta  = q1/q2
  qamp  = q2-q1
  alpha = min(alpha0,half/zeta)
  IF (ALLOCATED(Lef)) deallocate(Lef)
  IF (ALLOCATED(Rig)) deallocate(Rig)
  IF (ALLOCATED(Lar)) deallocate(Lar)
  IF (ALLOCATED(Cen)) deallocate(Cen)
  allocate(Lef(nn),Rig(nn),Cen(nn),Lar(nn))

  gamma = (Psi_SUI0(nn)-alpha)/Psi_SSS0(nn)

  Rig(nn)=q2
  Lar(nn) = qamp/alpha
  do i=nn-1,1,-1
    Lar(i) = Lar(nn)/REAL(nn-i+1,DP)
  enddo
  do i=nn,2,-1
    i1=i-1
    Rig(i1) = Rig(i) - Lar(i) + gamma*sqrt(Lar(i)*Lar(i1))
  enddo
  do i=1,nn
    Lef(i) = Rig(i)-Lar(i)
    Cen(i) = Lef(i)+Lar(i)*half
  enddo
  Lef(1) = q1
else if (iwhatdo==2) then
  zeta  = q1/q2
  qamp  = q2-q1
  alpha = min(alpha0,half/zeta)
  IF (ALLOCATED(Lef)) deallocate(Lef)
  IF (ALLOCATED(Rig)) deallocate(Rig)
  IF (ALLOCATED(Lar)) deallocate(Lar)
  IF (ALLOCATED(Cen)) deallocate(Cen)
  allocate(Lef(nn),Rig(nn),Cen(nn),Lar(nn))

  nn1=nn/2
  nn2=nn-nn1

  gamma = (Psi_SUI0(nn1)-alpha)/Psi_SSS0(nn1)

  Lef(1)=q1
  Lar(1) = qamp/alpha
  do i=2,nn1
    Lar(i) = Lar(1)/REAL(i,DP)
  enddo
  do i=1,nn1-1
    i1=1+i
    Lef(i1) = Lef(i) + Lar(i) -gamma*sqrt(Lar(i)*Lar(i1))
  enddo
  do i=1,nn1
    Rig(i) = Lef(i)+Lar(i)
    Cen(i) = Lef(i)+Lar(i)*half
  enddo
  Rig(nn1) = q2

  gamma = (Psi_SUI0(nn2)-alpha)/Psi_SSS0(nn2)

  Rig(nn2+nn1)=q2
  Lar(nn2+nn1) = qamp/alpha
  do i=nn2-1,1,-1
    Lar(i+nn1) = Lar(nn2+nn1)/REAL(nn2-i+1,DP)
  enddo
  do i=nn2,2,-1
    i1=i-1
    Rig(i1+nn1) = Rig(i+nn1) - Lar(i+nn1) + gamma*sqrt(Lar(i+nn1)*Lar(i1+nn1))
  enddo
  do i=1,nn2
    Lef(i+nn1) = Rig(i+nn1)-Lar(i+nn1)
    Cen(i+nn1) = Lef(i+nn1)+Lar(i+nn1)*half
  enddo
  Lef(1+nn1) = q1

endif


end subroutine gammafinder
!*************************
subroutine gammaex
implicit none

IF (ALLOCATED(Lef)) deallocate(Lef)
IF (ALLOCATED(Rig)) deallocate(Rig)
IF (ALLOCATED(Lar)) deallocate(Lar)
IF (ALLOCATED(Cen)) deallocate(Cen)

end subroutine gammaex
!*************************
function Psi_SUI(nn,xx)
implicit none
integer(I4B),intent(IN) :: nn
real(DP),intent(IN) :: xx
real(DP)  :: Psi_SUI
integer(I4B)  :: i

psi_SUI=1.0_DP/xx
IF (nn<1) then
  STOP 'Out-of-range Psi(nn) with nn<1'
ELSE IF (nn==1) then
  return
else if (nn>=2) then
  do i=1,nn-1
    psi_SUI=psi_SUI+1.0_DP/(xx+REAL(i,DP))
  enddo
endif
end function Psi_SUI
!*************************
function Psi_SUI0(nn)
implicit none
integer(I4B),intent(IN) :: nn
real(DP)  :: Psi_SUI0
integer(I4B)  :: i

IF (nn<1) STOP 'Out-of-range Psi0(nn) with nn<1'

psi_SUI0=1.0_DP
IF (nn==1) then
  return
else if (nn>=2) then
  do i=2,nn
    psi_SUI0=psi_SUI0+1.0_DP/(REAL(i,DP))
  enddo
endif
end function Psi_SUI0
!**************************
function Psi_SSS(nn,xx)
!___ sum(sqrt(1/((k+xx)*(k+xx+1))),k = 1 .. nn-1)
implicit none
integer(I4B),intent(IN) :: nn
real(DP),intent(IN) :: xx
real(DP)  :: Psi_SSS
integer(I4B)  :: i,i1

psi_SSS=zero
IF (nn<=1) then
  STOP 'Out-of-range Psi_SSS(nn) with nn<1'
else 
  do i=1,nn-1
    i1=i+1
    psi_SSS=psi_SSS+1.0_DP/sqrt((xx+REAL(i,DP))*(xx+REAL(i1,DP)))
  enddo
endif
end function Psi_SSS
!**************************
function Psi_SSS0(nn)
!___ sum(1/sqrt(k*(k+1)),k = 1 .. nn-1)
implicit none
integer(I4B),intent(IN) :: nn
real(DP)  :: Psi_SSS0
integer(I4B)  :: i,ipr

IF (nn<=1) STOP 'Out-of-range Psi_SSS0(nn) with nn<=1'
psi_SSS0=zero
do i=1,nn-1
  ipr=(i+1)*i
  psi_SSS0=psi_SSS0+1.0_DP/sqrt(REAL(ipr,DP))
enddo
end function Psi_SSS0
!**************************
subroutine Take_Five(qv,base0,baseT,posi,inormalize,nbc_in)
implicit none
real(DP),dimension(:),intent(IN)              :: qv
integer(I4B),dimension(:),intent(IN),optional :: posi
integer(I4B),intent(IN),optional              :: inormalize,nbc_in
real(DP),dimension(:,:),intent(IN)            :: base0
real(DP),dimension(:,:),intent(OUT)           :: baseT

real(DP),allocatable                    :: funb(:)
real(DP),allocatable                    :: xall(:)
integer(I4B),allocatable                :: posi1(:)
real(DP)                                :: qmin,qmax,xfun,goll,wrd
integer(I4B)                            :: ifun,i,j,nuv,np,nv,nv1,nbc

np = size(qv)
nv1 = size(base0,2)
nbc=0
if (PRESENT(nbc_in)) nbc=max(nbc_in,0)

nv=nv1-nbc
IF ((size(base0,1)/=np .or. size(baseT,1)/=nv) .or. size(baseT,2)/=nv) STOP 'Take_Five: dimensioning!'
IF (PRESENT(posi)) then
  if (size(posi)/=nv1) STOP 'Take_Five: POSI dimensioning!'
ENDIF
qmin=minval(qv)
qmax=maxval(qv)
IF (ALLOCATED(funb)) deallocate(funb)
IF (ALLOCATED(xall)) deallocate(xall)
IF (ALLOCATED(posi1)) deallocate(posi1)
ALLOCATE(xall(nv1),posi1(nv1),funb(np))


IF (present(posi)) THEN
  posi1=posi
ELSE
  posi1=1
  posi1(1:nbc) = 0
ENDIF

call GAMMAFINDER(nv,qmin,qmax)
ifun=2
goll=eps_DP*REAL(nv1,DP)
do i=1,nv
  funb(:)=gausV(qv,i)
  wrd=sum(funb*funb)
  IF (PRESENT(inormalize)) then
    IF (inormalize==1) then
      xfun=sqrt(sum(funb*funb))
      funb=funb/xfun
    endif
  endif
  wrd=sum(funb*funb)
  call SING_VAL_LSPOS(a=base0,b=funb,thresh1=goll, &
                        x=xall,req_pos=posi1, num_effvar=nuv, Nbc=nbc,Nac=nv,iprint1=0,flaus=1)
  baseT(:,i) = xall(nbc+1:)
  funb=funb-matmul(base0,xall)
  wrd=100.0_DP*sqrt(sum(funb*funb)/wrd)
enddo
! free space
IF (ALLOCATED(funb)) deallocate(funb)
IF (ALLOCATED(xall)) deallocate(xall)
IF (ALLOCATED(posi1)) deallocate(posi1)
call GAMMAEX
end subroutine Take_Five
!**************************
subroutine Take_Five_2(qv,base0,baseT,posi,inormalize,nbc_in,Delta)
implicit none
real(DP),dimension(:),intent(IN)              :: qv
integer(I4B),dimension(:),intent(IN),optional :: posi
integer(I4B),intent(IN),optional              :: inormalize,nbc_in
real(DP),dimension(:,:),intent(IN)            :: base0
real(DP),dimension(:,:),intent(OUT)           :: baseT
real(DP),intent(IN)                           :: Delta

real(DP),allocatable                    :: funb(:)
real(DP),allocatable                    :: xall(:)
integer(I4B),allocatable                :: posi1(:)
real(DP)                                :: qmin,qmax,xfun,goll,wrd
integer(I4B)                            :: ifun,i,j,nuv,np,nv,nv1,nbc

np = size(qv)
nv1 = size(base0,2)
nbc=0
if (PRESENT(nbc_in)) nbc=max(nbc_in,nbc)

nv=nv1-nbc
IF ((size(base0,1)/=np .or. size(baseT,1)/=nv) .or. size(baseT,2)/=nv) &
      STOP 'Take_Five: dimensioning!'
IF (PRESENT(posi)) then
  if (size(posi)/=nv1) STOP 'Take_Five: POSI dimensioning!'
ENDIF
qmin=minval(qv)
qmax=maxval(qv)
IF (ALLOCATED(funb)) deallocate(funb)
IF (ALLOCATED(xall)) deallocate(xall)
IF (ALLOCATED(posi1)) deallocate(posi1)
ALLOCATE(xall(nv1),posi1(nv1),funb(np))


IF (present(posi)) THEN
  posi1=posi
ELSE
  posi1= 0
ENDIF

call GAMMAFINDER(nv,qmin,qmax)
ifun=2
goll=eps_DP*REAL(nv1,DP)
do i=1,nv
  funb(:)=Pi2*Delta*qv
  funb = sin(funb)/funb
  wrd=sum(funb*funb)
  IF (PRESENT(inormalize)) then
    IF (inormalize==1) then
      xfun=sqrt(sum(funb*funb))
      funb=funb/xfun
    endif
  endif
  wrd=sum(funb*funb)
  call SING_VAL_LSPOS(a=base0,b=funb,thresh1=goll, &
                        x=xall,req_pos=posi1, num_effvar=nuv, Nbc=nbc,Nac=nv,iprint1=0,flaus=1)
  baseT(:,i) = xall(nbc+1:)
  funb=funb-matmul(base0,xall)
  wrd=100.0_DP*sqrt(sum(funb*funb)/wrd)
enddo
! free space
IF (ALLOCATED(funb)) deallocate(funb)
IF (ALLOCATED(xall)) deallocate(xall)
IF (ALLOCATED(posi1)) deallocate(posi1)
call GAMMAEX
end subroutine Take_Five_2
!**************************
end module sicut
!_______________________________________________________________________________
MODULE CALC_WSPACE
use OBS_WSPACE
use BACKCO
use SICUT
use ATOMIX
use NANO_TYPES
use specfun_AC

 integer(I4B),parameter :: NumParSiz = 5, NumParStrain = 4, NumParPha = NumParSiz+NumParStrain, &
                           NumPar_DW=3,NumPar_Oc=3,NumPar_at = NumPar_DW+NumPar_Oc, NumPar_at_red = 2
 
 REAL(DP),parameter :: BCAMO = 143.898832167882848662606878778194283_DP, BTAMO =58.0_DP * 0.25_DP 
!________ here BCAMO is 2*Pi^2*2.7^2 = 2*Pi^2*rho^2 of the Gaussian sampling

 TYPE,PUBLIC     :: SKALES
     INTEGER(I4B)                            :: ns_cons,ns_free,nc_pha,nc_amo,nc_bkg,ntot ! linear parameters of different classes
     REAL(CP)                                :: SETscal = one, E_SETscal = zero, NAT_Scale = 1.0_DP
     REAL(CP)                                :: WD_SQSUM, N_OB_PT
     REAL(CP),DIMENSION(:),allocatable           :: STRscal, E_STRscal, STRscal_W
     REAL(CP),DIMENSION(:,:),allocatable         :: COVm_STRscal
     REAL(CP),DIMENSION(:),allocatable           :: AMOscal, E_AMOscal
     REAL(CP),DIMENSION(:),allocatable           :: BKGscal, E_BKGscal
     REAL(CP),DIMENSION(:),allocatable           :: ALLscal, E_ALLscal
 END TYPE SKALES

 TYPE,PUBLIC     :: wsp_PAR
     CHARACTER(5),DIMENSION(:),allocatable     :: nano_names
     REAL(CP),DIMENSION(:),allocatable         :: nano_par0,    nano_par0E
     REAL(CP),DIMENSION(:),allocatable         :: nano_parcurr, nano_parcurrE
     REAL(CP),DIMENSION(:),allocatable         :: nano_parUP,   nano_parLO
     INTEGER(I4B),DIMENSION(:),allocatable     :: nano_mask, nano_parref, nano_dostage, &
                                                  nano_doit
     INTEGER(I4B),DIMENSION(:),allocatable     :: law_B, law_O
     INTEGER(I4B)                          :: Numero
     INTEGER(I4B)                          :: Numero_red
     INTEGER(I4B)                          :: str_cod
     INTEGER(I4B)                          :: n1,n2
 END TYPE wsp_PAR

 TYPE,PUBLIC     :: wsp_CALC
     REAL(CP),DIMENSION(:,:),allocatable         :: qshdata
     REAL(CP),DIMENSION(:,:),allocatable         :: calc_spline
     REAL(CP),DIMENSION(:,:),allocatable         :: twopi_q_a
     REAL(CP),DIMENSION(:),allocatable           :: vdata
     REAL(CP),DIMENSION(:,:,:),allocatable       :: ascaf
     REAL(CP),DIMENSION(:,:,:),allocatable       :: incoh
 END TYPE wsp_CALC
 TYPE(wsp_CALC), allocatable, save   :: CALTOT_W(:)
 TYPE(wsp_CALC), allocatable, save   :: CALPHA_W(:,:)

 TYPE,PUBLIC     :: wsp_MAT
     REAL(CP),DIMENSION(:,:,:,:),allocatable :: Umat
!no     REAL(CP),DIMENSION(:,:,:,:),allocatable :: Wmat
!no     REAL(CP),DIMENSION(:,:,:,:),allocatable :: Tmat
!no     REAL(CP),DIMENSION(:,:,:,:),allocatable :: TUmat
     REAL(CP),DIMENSION(:,:),allocatable     :: cotes
 END TYPE wsp_MAT
 TYPE(wsp_MAT), allocatable, save    :: WS_PHA_SET(:,:)
 
 TYPE,PUBLIC     :: MT_pairs_clusiz
     integer(I4B)                        :: atpai_dim, nclu_dim
     REAL(CP),DIMENSION(:,:),allocatable :: VVvalue
     REAL(CP),DIMENSION(:,:),allocatable :: MTvalue
     REAL(CP),DIMENSION(:),allocatable   :: DiamClu
 END TYPE MT_pairs_clusiz

 TYPE(MT_pairs_clusiz), allocatable, save    :: OccMT(:),DWalMT(:)
 TYPE(wsp_PAR), allocatable, save    :: PARAPHAS(:)
 TYPE(wsp_PAR), save                 :: PARAGLOB


 TYPE(SKALES), allocatable, save     :: Scales(:)

 TYPE,public     :: AMOR_TERMS
   REAL(DP)                        :: min_of_A,distAmin
   INTEGER(I4B)                    :: j000A
   REAL(CP),DIMENSION(:),allocatable   :: addbase
   REAL(CP),DIMENSION(:,:),allocatable :: precondy
   REAL(CP),DIMENSION(:,:),allocatable :: invcondy
   REAL(CP),DIMENSION(:),allocatable :: yadd
   REAL(CP),DIMENSION(:,:),allocatable :: amo_scat_sep
   REAL(CP),DIMENSION(:),allocatable   :: amo_scat_tot
   REAL(CP),DIMENSION(:,:),allocatable :: logbase_sep
   INTEGER(I2B),DIMENSION(:,:),allocatable :: sigbase_sep
   INTEGER(I2B),DIMENSION(:),allocatable :: cold_start
 END TYPE AMOR_TERMS
 
  TYPE,PUBLIC     :: Edelic
     integer(I4B) :: lark1,lark2
     REAL(CP),DIMENSION(:),allocatable      :: Meadows
  END TYPE Edelic

  TYPE,PUBLIC     :: Psych
     TYPE(Edelic),DIMENSION(:),allocatable  :: Grantchester
  END TYPE Psych
  
  TYPE(Psych),DIMENSION(:,:),allocatable,save :: Umma_Gumma
  LOGICAL,DIMENSION(:,:),allocatable,save     :: INST_READY
  INTEGER(I4B),SAVE                           :: N_INST_PASS=3
 
 integer(I4B),allocatable,save  :: ncut_0_off(:,:)
 real(DP),allocatable,save      :: DISTRU(:,:,:),E_DISTRU(:,:,:)
 TYPE(AMOR_TERMS),DIMENSION(:),allocatable,save  :: AMORPHOUS
 REAL(CP),save                          :: delta_Amor
 REAL(CP),save                          :: srnpoi,wstol

 REAL(CP),DIMENSION(:,:,:),allocatable,save     :: diam_logar
!  REAL(CP),DIMENSION(:,:),allocatable           :: diam_logar
 REAL(CP),DIMENSION(:),allocatable,save     :: xlogan, A_mat, V_mat
! REAL(CP),DIMENSION(:,:),allocatable,save   :: B_mat, O_mat
 REAL(CP),DIMENSION(:,:),allocatable,save   :: DA_mat  , DV_mat  , DB_mat , &
                                           D2A_mat , D2V_mat 

 LOGICAL,SAVE       :: InitStage, InitCycle
 LOGICAL,SAVE       :: illogik=.false., showall =.false.
 INTEGER(I4B),save  :: kount_calc,kount_grad,kount_stage,kount_mstage
 real(DP),allocatable,SAVE  :: mataux(:,:),vetaux(:),shmaux(:,:),shvaux(:)
 INTEGER(I4B),SAVE  :: n1aux,n2aux

 data InitStage/.true./
 data InitCycle/.true./
 data kount_calc,kount_grad,kount_stage,kount_mstage/0,0,0,0/

contains

  subroutine assoc_CAL
    implicit none
    integer(I4B)  :: i,j,j2,nallo,k,iwl,j000A0,nupa,nupairmax,k1,k2,inop,nnn,ia,na, iuan,ioan,ifndz(2),izzf
    real(DP)      :: w(2),uu,distAmin0,fanom1(2),fanom2(2),xnd, xxx6,yyy6
    logical       :: do_orth = .true., exanom=.false.
    real(DP)      :: cellA(6),cosal,cosbe,cosga,xfac,xfack,volcel, cocon
    character(len=3) :: chaset
    integer(I4B) :: iuinco
    character(len=7) :: wword

!______ Little trivial tasks
    do_orth = .true.

    IF (allocated(xlogan)) deallocate(xlogan)
    IF (allocated(diam_logar)) deallocate(diam_logar)
    IF (allocated(A_mat)) deallocate(A_mat)
    IF (allocated(V_mat)) deallocate(V_mat)
!    IF (allocated(B_mat)) deallocate(B_mat)
    IF (allocated(DA_mat)) deallocate(DA_mat)
    IF (allocated(DV_mat)) deallocate(DV_mat)
    IF (allocated(DB_mat)) deallocate(DB_mat)
    IF (allocated(D2A_mat)) deallocate(D2A_mat)
    IF (allocated(D2V_mat)) deallocate(D2V_mat)
    nupairmax = MAXVAL(NPAIR_AT_W(1:NSTR_W))
    ALLOCATE(xlogan(NLARGEST1), diam_logar(NSTR_W,NLARGEST1,2), &        
             A_mat(NLARGEST1),     V_mat(NLARGEST1),  &! B_mat(NLARGEST1), &
             DA_mat(4,NLARGEST1),  DV_mat(4,NLARGEST1), DB_mat(2,NLARGEST1), &
             D2A_mat(10,NLARGEST1),D2V_mat(3,NLARGEST1))
!__________________________________ new (or washed with Perlana)
    IF (allocated(OccMT)) deallocate(OccMT)
    IF (allocated(DWalMT)) deallocate(DWalMT)
    ALLOCATE(OccMT(NSTR_W),DWalMT(NSTR_W))
    diam_logar=-one/eps_DP
    do j=1,NSTR_W
      nnn = N2USE_W(j)
      IF (DB_INDEX_W(j) == 4 .or. DB_INDEX_W(j) == 5) nnn = N2USE_ab(j,2) * N2USE_c(j,2)
      OccMT(j)%atpai_dim = NPAIR_AT_W(j) 
      OccMT(j)%nclu_dim  = nnn
      DWalMT(j)%atpai_dim = NPAIR_AT_W(j) 
      DWalMT(j)%nclu_dim  = nnn
      ALLOCATE( OccMT(j)%MTvalue(NPAIR_AT_W(j),nnn), DWalMT(j)%MTvalue(NPAIR_AT_W(j),nnn) )
      ALLOCATE( OccMT(j)%VVvalue(NSP_AT_W(j),nnn), DWalMT(j)%VVvalue(NSP_AT_W(j),nnn) )
      ALLOCATE( OccMT(j)%DiamClu(nnn) )
            
      IF (DB_INDEX_W(j) == 1) THEN
        diam_logar(1,1,1) = log(max(eps_DP,nano_iav(j)%struk(1)%act_diam(1)))
      else IF (DB_INDEX_W(j) == 2) THEN
        do k=1,nnn
          xnd = CELL_P_W(1,j) * (k + half) * sd_conv(ITYPE_DB_W(j)) 
          if (ITYPE_DB_W(j) == 4) xnd = CELL_P_W(1,j) * sd_conv(ITYPE_DB_W(j)) &
                                        * (k**3 + 1.5d0*k**2 + 0.25d0*k + 13.d0/8.d0)**unter
          OccMT(j)%DiamClu(k) = 2.d0 * 0.1d0 * xnd
          !diam_logar(j,1,k) = log(max(eps_DP,nano_iav(j)%struk(k)%act_diam(1)))
        enddo
      else IF (DB_INDEX_W(j) == 3) THEN
        cellA=ALL_PHA_INFO_W(j)%cepa(1:6)
        cosal = cos(degrees_to_radians*cellA(4))
        cosbe = cos(degrees_to_radians*cellA(5))
        cosga = cos(degrees_to_radians*cellA(6))
        volcel = sqrt(one-cosga*cosga-cosbe*cosbe-cosal*(cosal-two*cosga*cosbe))*cellA(1)*cellA(2)*cellA(3)
        ! 23.02.2015
        ! eliminate
        ! xfac = nano_iav(j)%struk(1)%xnat(1) / REAL(ALL_PHA_INFO(j)%pha_MASSNUMBER(1),DP)
        do k=1,nnn
          xfack = nano_iav(j)%struk(k)%xnat(1) / nano_iav(j)%struk(1)%xnat(1)
          OccMT(j)%DiamClu(k) = nano_iav(j)%struk(k)%act_diam(1)
           diam_logar(j,k,1) = log(max(eps_DP,nano_iav(j)%struk(k)%act_diam(1)))
        enddo
!        print*, 'CALC_WSPACE - diam_logar(1,1,j) = ',j,diam_logar(1,1,j)
      !!__RF 15.07.14  (DB04 and DB05 splitted)
      else IF (DB_INDEX_W(j) == 4) THEN
        cellA=ALL_PHA_INFO_W(j)%cepa(1:6)
        cosal = cos(degrees_to_radians*cellA(4))
        cosbe = cos(degrees_to_radians*cellA(5))
        cosga = cos(degrees_to_radians*cellA(6))
        volcel = sqrt(one-cosga*cosga-cosbe*cosbe-cosal*(cosal-two*cosga*cosbe))*cellA(1)*cellA(2)*cellA(3)
        ! 23.02.2015
        ! eliminate
        ! xfac = nano_iav(j)%struk(1)%xnat(1) / REAL(ALL_PHA_INFO(j)%pha_MASSNUMBER(1),DP)
        do k=1,nnn
          xfack=(REAL(nano_iav(j)%struk(k)%nat(1),DP) / REAL(nano_iav(j)%struk(1)%nat(1),DP))
          OccMT(j)%DiamClu(k) = (((nano_iav(j)%struk(k)%celvolr)*(nano_iav(j)%struk(k)%ncelpha(1))/1000)&
                             *six/pi)**unter
          diam_logar(j,k,:) = log(max(eps_DP,nano_iav(j)%struk(k)%act_diam(:)))
        enddo
!        print*, 'CALC_WSPACE - diam_logar(1,1,j) = ',j,diam_logar(1,1,j)
      else IF (DB_INDEX_W(j) == 5) THEN
        cellA=ALL_PHA_INFO_W(j)%cepa(1:6)
        cosal = cos(degrees_to_radians*cellA(4))
        cosbe = cos(degrees_to_radians*cellA(5))
        cosga = cos(degrees_to_radians*cellA(6))
        volcel = sqrt(one-cosga*cosga-cosbe*cosbe-cosal*(cosal-two*cosga*cosbe))*cellA(1)*cellA(2)*cellA(3)
        ! 23.02.2015
        ! eliminate
        ! xfac = nano_iav(j)%struk(1)%xnat(1) / REAL(ALL_PHA_INFO(j)%pha_MASSNUMBER(1),DP)
        do k=1,nnn
          xfack=(REAL(nano_iav(j)%struk(k)%nat(1),DP) / REAL(nano_iav(j)%struk(1)%nat(1),DP))
          OccMT(j)%DiamClu(k) = sum(nano_iav(j)%struk(k)%act_diam(:))     
          diam_logar(j,k,:) = log(max(eps_DP,nano_iav(j)%struk(k)%act_diam(:)))
        enddo
!        print*, 'CALC_WSPACE - diam_logar(1,1,j) = ',j,diam_logar(1,1,j)
      endif
    enddo
!    
!    do j=1,NSTR_W
!      nnn = N2USE_W(j)
!!      do k=1,nnn
!      print*, 'CALC_WSPACE - diam_logar(1,k,j) = ',j,diam_logar(:,1,j),diam_logar(:,nnn,j)
!!      enddo
!    enddo  
    
!__________________________________ end new (or washed with Perlana)


!________________ SPACE FOR INSTRUMENTAL IRF

    if (ANY(INST_FLAG_W(:) /= 0)) then
      if (ALLOCATED(Umma_Gumma)) deallocate(Umma_Gumma)
      if (ALLOCATED(INST_READY)) deallocate(INST_READY)
      ALLOCATE(Umma_Gumma(NSET_W,N_INST_PASS),INST_READY(NSET_W,N_INST_PASS))
      do i=1,NSET_W
        if (INST_FLAG_W(i) /= 0) then
          do j=1,N_INST_PASS
            ALLOCATE( Umma_Gumma(i,j)%Grantchester( NDATA_W(i) ) )
            INST_READY(i,j) = .false.
          enddo
        else
          do j=1,N_INST_PASS
          !___ not using IRF for this set?
            ALLOCATE( Umma_Gumma(i,j)%Grantchester( 0:0 ) )
            Umma_Gumma(i,j)%Grantchester(0)%lark1=0
            Umma_Gumma(i,j)%Grantchester(0)%lark2=0
            ALLOCATE(Umma_Gumma(i,j)%Grantchester(0)%Meadows(0:0))
            Umma_Gumma(i,j)%Grantchester(1)%Meadows(0)=zero
            INST_READY(i,j) = .true.
          enddo
        endif
      enddo
    endif

!________________ SPACE FOR INSTRUMENTAL IRF - done

    ALLOCATE(AMORPHOUS(NSET_W))
    IF (DO_AMORPH_W) THEN
      Delta_Amor = 0.85_DP * half/QMAX_D_W
      j000A0 = FLOOR((AMO_DCORR0_W)/Delta_Amor)
      distAmin0 = REAL(j000A0,DP)*Delta_Amor
      NAMO_W = MAX(1,NINT((AMO_DCORR_W)/Delta_Amor)-j000A0)
      do i=1,NSET_W
        AMORPHOUS(i)%j000A = j000A0
        AMORPHOUS(i)%distAmin = distAmin0
      enddo
    ELSE
      NAMO_W=0
    ENDIF

    xlogan(1) = zero
    do i=2,NLARGEST1
      xlogan(i) = LOG(REAL(i,DP))
    enddo

!______ Initialize atomic scattering factors
    IF (.not.setup_done) call INI_SCAF

!______ ALLOCATE workspace for Ical
!______ n. phases = NSTR_W
!______ n. datasets = NSET_W

    IF (allocated(Scales)) deallocate(Scales)
    ALLOCATE(Scales(NSET_W))

    do i = 1,NSET_W
      Scales(i)%NAT_Scale = 1.0_DP/SQRT(REAL(NDATA_W(i),DP)/SUM(OBS_DATA_W(i)%vdata*OBS_DATA_W(i)%vdata*OBS_DATA_W(i)%wdata))
      Scales(i)%nc_pha = NSTR_W
      Scales(i)%nc_amo = NAMO_W
      Scales(i)%nc_bkg = BACKGROUND(i)%DIMBACK
      
      IF (NSET_BACK_W==NSET_W) THEN
        Scales(i)%ns_cons = NSTR_W+NAMO_W
        Scales(i)%ns_free = BACKGROUND(i)%DIMBACK
      ELSE
        stop 'Ungodly background information'
        Scales(i)%ns_cons = NSTR_W+NAMO_W+BACKGROUND(i)%DIMBACK
        Scales(i)%ns_free = 0
      ENDIF
      Scales(i)%ntot = Scales(i)%ns_cons + Scales(i)%ns_free

      Scales(i)%SETscal   = one
      Scales(i)%E_SETscal = zero

      IF (allocated(Scales(i)%STRscal)) deallocate(Scales(i)%STRscal)
      ALLOCATE(Scales(i)%STRscal(Scales(i)%nc_pha))
      IF (allocated(Scales(i)%STRscal_W)) deallocate(Scales(i)%STRscal_W)
      ALLOCATE(Scales(i)%STRscal_W(Scales(i)%nc_pha))
      IF (allocated(Scales(i)%ALLscal)) deallocate(Scales(i)%ALLscal)
      ALLOCATE(Scales(i)%ALLscal(Scales(i)%nc_pha+Scales(i)%nc_amo+Scales(i)%nc_bkg))
      IF (allocated(Scales(i)%AMOscal)) deallocate(Scales(i)%AMOscal)
      ALLOCATE(Scales(i)%AMOscal(Scales(i)%nc_amo))
      IF (allocated(Scales(i)%BKGscal)) deallocate(Scales(i)%BKGscal)
      ALLOCATE(Scales(i)%BKGscal(Scales(i)%nc_bkg))

      IF (allocated(Scales(i)%E_STRscal)) deallocate(Scales(i)%E_STRscal)
      ALLOCATE(Scales(i)%E_STRscal(Scales(i)%nc_pha))
      IF (allocated(Scales(i)%E_ALLscal)) deallocate(Scales(i)%E_ALLscal)
      ALLOCATE(Scales(i)%E_ALLscal(Scales(i)%nc_pha+Scales(i)%nc_amo+Scales(i)%nc_bkg))
      IF (allocated(Scales(i)%E_AMOscal)) deallocate(Scales(i)%E_AMOscal)
      ALLOCATE(Scales(i)%E_AMOscal(Scales(i)%nc_amo))
      IF (allocated(Scales(i)%E_BKGscal)) deallocate(Scales(i)%E_BKGscal)
      ALLOCATE(Scales(i)%E_BKGscal(Scales(i)%nc_bkg))
      
      IF (allocated(Scales(i)%COVm_STRscal)) deallocate(Scales(i)%COVm_STRscal)
      ALLOCATE(Scales(i)%COVm_STRscal(Scales(i)%nc_pha,Scales(i)%nc_pha))

    enddo

    IF (allocated(CALTOT_W)) deallocate(CALTOT_W)
    ALLOCATE(CALTOT_W(NSET_W))
    IF (allocated(CALPHA_W)) deallocate(CALPHA_W)
    ALLOCATE(CALPHA_W(NSET_W,NSTR_W))
    IF (allocated(ncut_0_off)) deallocate(ncut_0_off)
    IF (allocated(DISTRU)) deallocate(DISTRU)
    IF (allocated(E_DISTRU)) deallocate(E_DISTRU)
    ALLOCATE(ncut_0_off(2,NSTR_W),DISTRU(NLARGEST1,NSTR_W,3),E_DISTRU(NLARGEST1,NSTR_W,3))

    do i = 1,NSET_W

!______ THIS WE USE TO STORE THE TOTAL Ical 

      ALLOCATE(CALTOT_W(i)%vdata(NDATA_W(i)))
      CALTOT_W(i)%vdata = zero

!______ THIS WE USE TO STORE q^2/2 TO CALCULATE THERMAL FACTORS, RENORMALIZATIONS,...

      ALLOCATE(CALTOT_W(i)%qshdata(NDATA_W(i),ILAMBDA_W(i)))   ! here  q^2/2 = 2 q^2/4
      CALTOT_W(i)%qshdata(:,:) = half * obs_data_w(i)%qdata(:,:)**2
      ALLOCATE(CALTOT_W(i)%calc_spline(NDATA_W(i),0:3)) !___ Cubic spline coeff.s 
    enddo

    do j = 1,NSTR_W
      do i = 1,NSET_W
        write(chaset,'(i3.3)') i
        w = (/1.0_DP, LAMBDAS_W(0,i)/)
!______ THIS WE USE TO STORE Ical
        ALLOCATE(CALPHA_W(i,j)%vdata(NDATA_W(i)))
        CALPHA_W(i,j)%vdata = zero

!______ THIS WE USE TO STORE THE POINTWISE ATOMIC SCATTERING FACTORS
        ALLOCATE(CALPHA_W(i,j)%ascaf(NDATA_W(i),NPAIR_AT_W(j), ILAMBDA_W(i)))
        ALLOCATE(CALPHA_W(i,j)%incoh(NDATA_W(i),NSP_AT_W(j), ILAMBDA_W(i)))
        
        IF (DB_INDEX_W(j) /= 2) THEN
          if (RADIATION_W(i)=='X'.or.RADIATION_W(i)=='S') then
            do k=1,NSP_AT_W(j)
              CALPHA_W(i,j)%incoh(:,k,1) = COMP_COMPTON_S(q=obs_data_w(i)%qdata(:,1), &
                        Z_e=nano_iav(j)%struk(1)%Z_at(k), wavelength=LAMBDAS_W(1,i), &
                        beam_pol_ang_ecc=[POLARIZB_W(2,i), POLARIZB_W(1,i)])
                                             !! Z_e=nano_iav(j)%struk(1)%Z_at(k), wavelength=LAMBDAS_W(1,i))
                                             !! Z_e=ALL_PHA_INFO_W(j)%pha_Z(k), wavelength=LAMBDAS_W(1,i))
              do iwl=2,ILAMBDA_W(i)
                CALPHA_W(i,j)%incoh(:,k,iwl) = w(iwl) * &
                                         COMP_COMPTON_S(q=obs_data_w(i)%qdata(:,iwl), &
                                             Z_e=nano_iav(j)%struk(1)%Z_at(k), wavelength=LAMBDAS_W(iwl,i), &
                                             beam_pol_ang_ecc=[POLARIZB_W(2,i), POLARIZB_W(1,i)])
                                             !!Z_e=ALL_PHA_INFO_W(j)%pha_Z(k), wavelength=LAMBDAS_W(iwl,i))
              enddo
            enddo
          else
            CALPHA_W(i,j)%incoh = zero
          endif
        ELSE
          if (RADIATION_W(i)=='X'.or.RADIATION_W(i)=='S') then
            do k=1,NSP_AT_W(j)
              k1=nano_iav(j)%struk(1)%zappa(1,k)
              CALPHA_W(i,j)%incoh(:,k,1) = COMP_COMPTON_S(q=obs_data_w(i)%qdata(:,1), &
                                             Z_e=k1, wavelength=LAMBDAS_W(1,i), &
                        beam_pol_ang_ecc=[POLARIZB_W(2,i), POLARIZB_W(1,i)])
              do iwl=2,ILAMBDA_W(i)
                CALPHA_W(i,j)%incoh(:,k,iwl) = w(iwl) * &
                                         COMP_COMPTON_S(q=obs_data_w(i)%qdata(:,iwl), &
                                             Z_e=k1, wavelength=LAMBDAS_W(iwl,i), &
                        beam_pol_ang_ecc=[POLARIZB_W(2,i), POLARIZB_W(1,i)])
              enddo
            enddo
          endif
        ENDIF
        !!__ACRF 19.07.2017  output of Incoherent component for all atomic specie 
        !! for each dataset and structure
        if (verbose) then
          iuinco=find_unit()
          write(wword,'(i3.3,"_",i3.3)')j,i
          open(iuinco,status='replace',file='IncohScat_'//wword//'.out')
          write(iuinco,*)'# ',LAMBDAS_W(1,i),nano_iav(j)%struk(1)%Z_at(1:NSP_AT_W(j)), POLARIZB_W(2,i), POLARIZB_W(1,i)
          do k=1,SIZE(CALPHA_W(i,j)%incoh(:,1,1))
            write(iuinco,*)OBS_DATA_W(i)%t2data(k),CALPHA_W(i,j)%incoh(k,1:NSP_AT_W(j),1)
          enddo
          close(iuinco)
        endif
        !!__end
        do k=1,NPAIR_AT_W(j)
          k1=nano_iav(j)%struk(1)%zappa(1,k)
          k2=nano_iav(j)%struk(1)%zappa(2,k)
          fanom1=zero; fanom2=zero
          if (RADIATION_W(i)=='X'.or.RADIATION_W(i)=='S') then
            exanom=.false.
            inquire(file='External_Anom_Fac'//chaset//'.txt',exist=exanom)
            if (.not.exanom) then
              fanom1 = Anomalous_X(Z_e=k1, wavelength=LAMBDAS_W(1,i))
              fanom2 = Anomalous_X(Z_e=k2, wavelength=LAMBDAS_W(1,i))
            else
              iuan=find_unit()
              open(iuan,status='old',action='read',file='External_Anom_Fac'//chaset//'.txt')
              ifndz=0
              izzf=0
              do
                read(iuan,*,iostat=ioan)izzf,xxx6,yyy6
                if (ioan/=0) exit
                if (izzf==k1) then
                  fanom1=[xxx6,yyy6]
                  ifndz(1)=1
                endif
                if (izzf==k2) then
                  fanom2=[xxx6,yyy6]
                  ifndz(2)=1
                endif
              enddo
              close(iuan)
              if (ANY(ifndz/=1)) stop 'External Anomalous Factor not found'
            endif
            CALPHA_W(i,j)%ascaf(:,k,1) = fanom1(2)*fanom2(2) + &
                 ( fanom1(1) + FormFact(q=obs_data_w(i)%qdata(:,1),Z_e=k1, radiation_type=RADIATION_W(i)) ) &
               * ( fanom2(1) + FormFact(q=obs_data_w(i)%qdata(:,1),Z_e=k2, radiation_type=RADIATION_W(i)) )
                                                          
            do iwl=2,ILAMBDA_W(i)
            !____ These stay the same, multiwl case not used
                fanom1 = Anomalous_X(Z_e=k1, wavelength=LAMBDAS_W(iwl,i))
                fanom2 = Anomalous_X(Z_e=k2, wavelength=LAMBDAS_W(iwl,i))
              CALPHA_W(i,j)%ascaf(:,k,iwl) = w(iwl) * ( fanom1(2)*fanom2(2) + &
                          ( fanom1(1) + FormFact(q=obs_data_w(i)%qdata(:,iwl),Z_e=k1,radiation_type=RADIATION_W(i)) ) &
                        * ( fanom2(1) + FormFact(q=obs_data_w(i)%qdata(:,iwl),Z_e=k2,radiation_type=RADIATION_W(i)) ) )
            enddo
          else
            CALPHA_W(i,j)%ascaf(:,k,1) = &
                 ( FormFact(q=obs_data_w(i)%qdata(:,1),Z_e=k1, radiation_type=RADIATION_W(i)) ) &
               * ( FormFact(q=obs_data_w(i)%qdata(:,1),Z_e=k2, radiation_type=RADIATION_W(i)) )
                                                          
            do iwl=2,ILAMBDA_W(i)
              CALPHA_W(i,j)%ascaf(:,k,iwl) = w(iwl) * &
                          ( FormFact(q=obs_data_w(i)%qdata(:,iwl),Z_e=k1,radiation_type=RADIATION_W(i)) ) &
                        * ( FormFact(q=obs_data_w(i)%qdata(:,iwl),Z_e=k2,radiation_type=RADIATION_W(i)) )
            enddo
          endif
        enddo

        ALLOCATE(CALPHA_W(i,j)%twopi_q_a(NDATA_W(i),ILAMBDA_W(i)))   ! here  2*Pi*q*a 
        CALPHA_W(i,j)%twopi_q_a = OBS_DATA_W(i)%qdata(:,:)*two_pi*CELL_P_W(1,j)
      enddo
    enddo

!______ FILL UP AMORPHOUS SCATTERING TERMS
    IF (DO_AMORPH_W.and.NAMO_W>0) THEN
      do i=1,NSET_W
        ALLOCATE(AMORPHOUS(i)%amo_scat_sep(NDATA_W(i),NAMO_W))
        ALLOCATE(AMORPHOUS(i)%logbase_sep(NDATA_W(i),NAMO_W),AMORPHOUS(i)%sigbase_sep(NDATA_W(i),NAMO_W))
        ALLOCATE(AMORPHOUS(i)%amo_scat_tot(NDATA_W(i)))
        ALLOCATE(AMORPHOUS(i)%addbase(NAMO_W))
        AMORPHOUS(i)%amo_scat_tot = zero
        AMORPHOUS(i)%addbase      = 1.0_DP
        ALLOCATE(AMORPHOUS(i)%cold_start(BACKGROUND(i)%dimback+NAMO_W+NSTR_W))
        AMORPHOUS(i)%cold_start=0
        AMORPHOUS(i)%cold_start(BACKGROUND(i)%dimback+1:BACKGROUND(i)%dimback+NAMO_W)=1
        ALLOCATE(AMORPHOUS(i)%precondy(NAMO_W,NAMO_W),AMORPHOUS(i)%yadd(NAMO_W), &
                 AMORPHOUS(i)%invcondy(NAMO_W,NAMO_W))
        call MAKE_AMOR(i)
      enddo
    ENDIF
    
    
    do_orth = .false.
    do i=1,NSET_W
      srnpoi = Scales(i)%NAT_Scale
      inop = 0
      if (BACKGROUND(i)%che_back == 4) inop = 1
      IF (do_orth) then
        n1aux = NDATA_W(i)
        n2aux = BACKGROUND(i)%dimback - inop
        if (ALLOCATED(mataux)) DEALLOCATE(mataux)
        if (ALLOCATED(vetaux)) DEALLOCATE(vetaux)
        ALLOCATE(mataux(n1aux,n2aux),vetaux(n1aux))
        mataux(:,1:n2aux) = BACKGROUND(i)%lin_bkgr_sep(:,1:n2aux)
        vetaux = sqrt(OBS_DATA_W(i)%wdata)

        do j=1,n2aux
          mataux(:,j) = mataux(:,j)*vetaux
        enddo

        call ORTHONORMALIZE_C(mataux,srnpoi)

        vetaux = 1.0_DP/vetaux
        do j=1,n2aux
          mataux(:,j) = mataux(:,j) * vetaux
        enddo

        do j=1,n2aux
          BACKGROUND(i)%lin_bkgr_sep(:,j) = mataux(:,j)
        enddo
        if (ALLOCATED(mataux)) DEALLOCATE(mataux)
        if (ALLOCATED(vetaux)) DEALLOCATE(vetaux)
      ELSE

        do j=1,BACKGROUND(i)%dimback
          cocon=srnpoi/sqrt(SUM(BACKGROUND(i)%lin_bkgr_sep(:,j)*BACKGROUND(i)%lin_bkgr_sep(:,j)*OBS_DATA_W(i)%wdata(:)))
          BACKGROUND(i)%lin_bkgr_sep(:,j) = BACKGROUND(i)%lin_bkgr_sep(:,j) * cocon
!          print*,'WWW Debug : cocon = ',j,cocon,one/cocon
        enddo
      ENDIF
    enddo


!______ ALLOCATE workspace for parameters

    i=0
    nallo = Param_data(i)%npar_ref
    PARAGLOB%Numero = nallo
    ALLOCATE(PARAGLOB%nano_par0(nallo), &
             PARAGLOB%nano_names(nallo), &
             PARAGLOB%nano_par0E(nallo), &
             PARAGLOB%nano_parcurr(nallo), &
             PARAGLOB%nano_parcurrE(nallo), &
             PARAGLOB%nano_parUP(nallo), &
             PARAGLOB%nano_parLO(nallo), &
             PARAGLOB%nano_mask(nallo), &
             PARAGLOB%nano_dostage(nallo), &
             PARAGLOB%nano_doit(nallo), &
             PARAGLOB%nano_parref(nallo))

    PARAGLOB%nano_names = Param_data(i)%NamePar         ! NAMES from file
    PARAGLOB%nano_par0  = Param_data(i)%PhasePar(2,:)   ! VALUES from file
    PARAGLOB%nano_par0E = Param_data(i)%PhasePar(2,:)   ! VALUES from file
    PARAGLOB%nano_parUP = Param_data(i)%PhasePar(3,:)   ! UPPER BOUNDS from file
    PARAGLOB%nano_parLO = Param_data(i)%PhasePar(1,:)   ! LOWER BOUNDS from file
    PARAGLOB%nano_parref= Param_data(i)%flag_ref  ! TO BE MULTIPLIED BY THE CURENT STRATEGY VECTOR TO OBTAIN NANO_DOIT
    PARAGLOB%nano_doit  = PARAGLOB%nano_parref * 1      ! STRAT.


    ALLOCATE(PARAPHAS(NSTR_W))

    do i=1,NSTR_W
      
      nallo = Param_data(i)%npar_ref   !!!! e' ok?
      !!! dovrebbe essere NumParPha + NumPar_at * NSP_AT_W(i)
      if (nallo /= NumParPha + NumPar_at * NSP_AT_W(i)) then
        print*,'Structure',i,': Number of parameters: ',nallo,' is NOT = 9+6*Nat: ',NumParPha + NumPar_at * NSP_AT_W(i)
        stop 'Structure: Number of parameters: nallo is NOT = 9+6*Nat '
      endif
      PARAPHAS(i)%Numero = nallo
!!!      PARAPHAS(i)%Numero_red = NumParPha + 2   !!! AG: 22.01.2014  (9+2 changed into 2(size+strain)+2(O+B)*atoms_species)
      PARAPHAS(i)%Numero_red = 2 + 2*NSP_AT_W(i)

      PARAPHAS(i)%str_cod = Param_data(i)%strain_cod
      PARAPHAS(i)%n1= Param_data(i)%strain_n1
      ALLOCATE(PARAPHAS(i)%nano_par0(nallo), &
               PARAPHAS(i)%nano_names(nallo), &
               PARAPHAS(i)%nano_par0E(nallo), &
               PARAPHAS(i)%nano_parcurr(nallo), &
               PARAPHAS(i)%nano_parcurrE(nallo), &
               PARAPHAS(i)%nano_parUP(nallo), &
               PARAPHAS(i)%nano_parLO(nallo), &
               PARAPHAS(i)%nano_mask(nallo), &
               PARAPHAS(i)%nano_dostage(nallo), &
               PARAPHAS(i)%nano_doit(nallo), &
               PARAPHAS(i)%law_O(NSP_AT_W(i)), &
               PARAPHAS(i)%law_B(NSP_AT_W(i)), &
               PARAPHAS(i)%nano_parref(nallo))
               
      PARAPHAS(i)%law_O = Param_data(i)%law_refOB(:,1)
      PARAPHAS(i)%law_B = Param_data(i)%law_refOB(:,2)
!!!!! fare la ref...finqi

      PARAPHAS(i)%nano_names = Param_data(i)%NamePar         ! NAMES from file
      PARAPHAS(i)%nano_par0  = Param_data(i)%PhasePar(2,:)   ! VALUES from file
      PARAPHAS(i)%nano_par0E = Param_data(i)%PhasePar(2,:)   ! VALUES from file
      PARAPHAS(i)%nano_parUP = Param_data(i)%PhasePar(3,:)   ! UPPER BOUNDS from file
      PARAPHAS(i)%nano_parLO = Param_data(i)%PhasePar(1,:)   ! LOWER BOUNDS from file
      PARAPHAS(i)%nano_parref(:NumParPha)= Param_data(i)%flag_ref(:NumParPha) ! TO BE MULTIPLIED BY THE CURRENT 
                                                                              ! STRATEGY VECTOR TO OBTAIN NANO_DOIT
!______________  strain a'/a convert to (a'-a)/a = da/a


      do ia=1,NSP_AT_W(i)
        na = NumParPha+(ia-1)*NumPar_at
        PARAPHAS(i)%nano_parref(na+1:na+NumPar_Oc) = Param_data(i)%flag_ref(na+1:na+NumPar_Oc)
        PARAPHAS(i)%nano_parref(na+NumPar_Oc+1:na+NumPar_Oc+NumPar_DW) = &
                                Param_data(i)%flag_ref(na+NumPar_Oc+1:na+NumPar_Oc+NumPar_DW)
      enddo
      PARAPHAS(i)%nano_doit  = PARAPHAS(i)%nano_parref * 1   ! STRAT.
   
!______ Masking
      call MAKE_MASK(PARAPHAS(i)%nano_doit, PARAPHAS(i)%nano_mask)
!____________ kzzrl, fare il _dostage

!______ INPUT SPACE DEALLOCATION
      deallocate(Param_data(i)%NamePar, Param_data(i)%PhasePar, Param_data(i)%flag_ref)
                 
    enddo
!______ INPUT SPACE DEALLOCATION
    deallocate(Param_data)

!______ ALLOCATE big matrix workspace for calculations
    ALLOCATE(WS_PHA_SET(NSTR_W,NSET_W))
    do i=1,NSTR_W
      nupa = NPAIR_AT_W(i)
      do j=1,NSET_W
        ALLOCATE(WS_PHA_SET(i,j)%Umat(NDATA_W(j), N2USE_W(i), ILAMBDA_W(j), nupa))
!no        ALLOCATE(WS_PHA_SET(i,j)%Wmat(NDATA_W(j), N2USE_W(i), ILAMBDA_W(j), nupa))
!no        ALLOCATE(WS_PHA_SET(i,j)%Tmat(NDATA_W(j), N2USE_W(i), ILAMBDA_W(j), nupa))
!no        ALLOCATE(WS_PHA_SET(i,j)%TUmat(NDATA_W(j), N2USE_W(i), ILAMBDA_W(j), nupa))
        ALLOCATE(WS_PHA_SET(i,j)%cotes(N2USE_W(i), nupa))
      enddo
    enddo

  end subroutine assoc_CAL
!***********************************************
 SUBROUTINE MAKE_AMOR(i)
  implicit none
  INTEGER(I4B),INTENT(in)       :: i     !dataset index
  INTEGER(I4B)                  :: j,iwl,jatom,nubas,iun
  REAL(DP)                      :: q,col_norm,q2pid,q2pidj,BCAMOx,weil
  REAL(DP),allocatable       :: qv(:),auxv1(:),auxv2(:),auxv3(:),Asvx(:,:),Usvx(:,:),Vsvx(:,:),Wsvx(:),TRMX(:,:)
  REAL(DP),allocatable       :: matbas(:,:)

  allocate(qv(NDATA_W(i)),auxv1(NDATA_W(i)),auxv2(NDATA_W(i)),auxv3(NDATA_W(i)))

   jatom = 1
   AMORPHOUS(i)%amo_scat_sep(:,:) = zero
   iwl=1      !_______ WE DO NOT LOOP ON wl.s

   qv = OBS_DATA_W(i)%qdata(:,iwl)
   auxv1 = Pi2 * qv * Delta_Amor
   BCAMOx =  BCAMO * Delta_Amor * Delta_Amor
   auxv3 = LOG(CALPHA_W(i,1)%ascaf(:,jatom,iwl) ) + BCAMOx*qv*qv
   do j=1,NAMO_W
     auxv2 = real(j+AMORPHOUS(i)%j000A,DP) * auxv1
     auxv2 = sin(auxv2) *qv*qv/ auxv2
     WHERE (abs(auxv2)>eps_DP)
       AMORPHOUS(i)%sigbase_sep(:,j) = NINT(SIGN(1.0_DP,auxv2)) 
       AMORPHOUS(i)%logbase_sep(:,j) = LOG(ABS(auxv2)) + auxv3
       AMORPHOUS(i)%amo_scat_sep(:,j)= auxv2*exp(auxv3)
     ELSEWHERE 
       AMORPHOUS(i)%sigbase_sep(:,j) = 0
       AMORPHOUS(i)%logbase_sep(:,j) = zero
     END WHERE
   enddo

!__________ NEW PART 1
   nubas=NAMO_W+1
   ALLOCATE(matbas(NDATA_W(i),nubas),Asvx(NDATA_W(i),NAMO_W),TRMX(NAMO_W,NAMO_W), &
            Usvx(NDATA_W(i),NAMO_W),Vsvx(NAMO_W,NAMO_W),Wsvx(NAMO_W))
   matbas(:,1)=1.0_DP
   matbas(:,2:) = AMORPHOUS(i)%amo_scat_sep(:,:)
   do j=1,nubas
     col_norm = 1.0_DP/sqrt(sum(matbas(:,j)*matbas(:,j)))
     matbas(:,j)=matbas(:,j)*col_norm
   enddo
!  **** find transformation
   if (NAMO_W>2) then
     call Take_Five_2(qv=qv,base0=matbas,baseT=TRMX,nbc_in=1,Delta=delta_amor)
   else if (NAMO_W==2) then
     TRMX(1,:) = (/half,half/)
     TRMX(2,:) = (/zero,1.0_DP/)
   else if (NAMO_W==1) then
     TRMX=1.0_DP
   endif

   AMORPHOUS(i)%precondy=TRMX
   Asvx= matmul(matbas(:,2:),TRMX)
   do j=1,NAMO_W
     AMORPHOUS(i)%yadd(j)=-SUM(Asvx(:,j))/NDATA_W(i)
     Asvx(:,j)=Asvx(:,j)+AMORPHOUS(i)%yadd(j)
   enddo
   AMORPHOUS(i)%amo_scat_sep(:,:)=Asvx
   call SING_VAL_DECOMP(a0=Asvx,U=Usvx,W=Wsvx,V=Vsvx)
   do j=1,NAMO_W
     if (Wsvx(j)<eps_DP*10.0_DP) then
       Wsvx(j)=zero
     else
       Wsvx(j)=1.0_DP/Wsvx(j)
     endif
   enddo
   
   do j=1,NAMO_W
     AMORPHOUS(i)%invcondy(j,:)=matmul(Vsvx,matmul(transpose(matbas(:,2:)),Wsvx(j)*Usvx(:,j)))
   enddo
   TRMX=matmul(TRMX,AMORPHOUS(i)%invcondy)
   do j=1,NAMO_W
     TRMX(j,j)=TRMX(j,j)-1.0_DP
   enddo


   deallocate(qv,auxv1,auxv2,auxv3,matbas,Asvx,Usvx,Vsvx,Wsvx,TRMX)

 END SUBROUTINE MAKE_AMOR
!***********************************************!***********************************************
 SUBROUTINE REFRESH_AMOR(i,u0,u1)
  implicit none
  INTEGER(I4B),INTENT(in)       :: i     !dataset index
  REAL(DP),intent(IN)           :: u0, u1
  INTEGER(I4B)                  :: j,k,iwl,jatom,iuf
  REAL(DP)                      :: q,qecoL,q2pid,q2pidj,gauwidth,weil,sagu
  REAL(DP),allocatable       :: qv(:),auxv1(:)

   allocate(qv(NDATA_W(i)),auxv1(NDATA_W(i)))

   IF (showall) then
     iuf=find_unit()
     open(iuf,status='replace',file='SHOWALL.out')
     write(iuf,*) NAMO_W
   endif

   AMORPHOUS(i)%amo_scat_sep = zero
   AMORPHOUS(i)%addbase = zero
   iwl=1   
   qv = OBS_DATA_W(i)%qdata(:,iwl)
   do j=1,NAMO_W
     gauwidth = u0+real(j+AMORPHOUS(i)%j000A,DP)*u1
     gauwidth = -Pi2*Pi*gauwidth*gauwidth
     auxv1 = (qv*qv)*gauwidth
     WHERE (AMORPHOUS(i)%sigbase_sep(:,j) /= 0) &
       AMORPHOUS(i)%amo_scat_sep(:,j) = REAL(AMORPHOUS(i)%sigbase_sep(:,j),DP) * &
                                        EXP( AMORPHOUS(i)%logbase_sep(:,j) + auxv1)
   enddo

   do j=1,NAMO_W
     sagu=sqrt(sum(AMORPHOUS(i)%amo_scat_sep(:,j)*AMORPHOUS(i)%amo_scat_sep(:,j)))
     AMORPHOUS(i)%amo_scat_sep(:,j)=AMORPHOUS(i)%amo_scat_sep(:,j)/sagu
     IF (showall) write(iuf,*)1.0_DP/sagu
   enddo
   AMORPHOUS(i)%amo_scat_sep(:,:) = matmul(AMORPHOUS(i)%amo_scat_sep,AMORPHOUS(i)%precondy)
   if (showall) write(iuf,*)AMORPHOUS(i)%precondy
   do j=1,NAMO_W
     sagu=-minval(AMORPHOUS(i)%amo_scat_sep(:,j))
     AMORPHOUS(i)%yadd(j)=sagu
     AMORPHOUS(i)%amo_scat_sep(:,j)=AMORPHOUS(i)%amo_scat_sep(:,j)+sagu
     IF (showall) write(iuf,*)sagu
   enddo

   do j=1,NAMO_W
     qecoL = Scales(i)%NAT_Scale/sqrt(SUM(  AMORPHOUS(i)%amo_scat_sep(1:NDATA_W(i),j) &
                                          * AMORPHOUS(i)%amo_scat_sep(1:NDATA_W(i),j) &
                                          * OBS_DATA_W(i)%wdata(1:NDATA_W(i)) ))
     AMORPHOUS(i)%amo_scat_sep(1:NDATA_W(i),j) = AMORPHOUS(i)%amo_scat_sep(1:NDATA_W(i),j) * qecoL
     AMORPHOUS(i)%addbase(j) = qecoL
     IF (showall) write(iuf,*)qecoL
   enddo
   if (showall) close(iuf)

   deallocate(qv,auxv1)
   
   showall = .false.

 END SUBROUTINE REFRESH_AMOR
!***********************************************

END MODULE CALC_WSPACE
!___________________________________________________________________________________________________
MODULE STRATEGY
   USE CALC_WSPACE
!   INTEGER(I4B), PARAMETER                  :: npar0_ref_loc = 12
   INTEGER(I4B), PARAMETER                  :: npar0_ref_loc = NumParPha ! sarebbe 9 non 12
   TYPE(ref_strat), allocatable, save       :: ref_stage(:)
   INTEGER(I4B),save                        :: NSTAG = 0, &
                                               RUN_numpar_NL, RUN_numpar_LI, RUN_numpar_all

!**************
CONTAINS
!**************

 SUBROUTINE read_ref_file
   IMPLICIT NONE
   integer(I4B)                             :: iu, ll, ls, ls2, i, j, jj, stagen, kstr, nr_rline
!   integer(I4B)                             :: astat, isu1, irb,iro,iat
   integer(I4B)                             :: astat, isu1, iat, isiz, istrai, na, na_red,l,kk
   integer(I4B), allocatable                :: irob(:)   
   CHARACTER(256)                           :: rline
   logical                                  :: right_blank, virg_pos, flag_read
   REAL(CP)                                 :: x


   iu = FIND_UNIT()
   OPEN(UNIT=iu,status='old', &
           form='formatted',access='sequential', &
           file=TRIM(REFINEMENT_FILE_W),action='READ',iostat=astat)

   IF (astat /= 0) THEN
       print'(1x,a,a,a)','ERROR opening refinement file ',TRIM(REFINEMENT_FILE_W),' The program stops and the bus continues'
   ENDIF
  
   right_blank = .true.
   
! new loop to control the correct number of stages    (AG+FB - 03.04.2015)
   stagen = 0
    STAGE_READ_0: do
      read(iu,'(a)',end=2) rline
      call clean_line(rline)
      rline=trim(adjustl(rline))
      IF (rline(1:1)=='!' .or. rline(1:1)=='>' .or. len_trim(rline) == 0) CYCLE STAGE_READ_0
      ll=len_trim(rline)    
      IF (rline(1:5) == 'stage') then
          stagen = stagen + 1
      ENDIF
   enddo STAGE_READ_0
     
 2 rewind(iu)
 
! print*, 'stagen = ', stagen

   nr_rline = 0
!________ Read # of stages
   STAGE_READ: do
      read(iu,'(a)',end=1) rline
      nr_rline =  nr_rline + 1
      call clean_line(rline)
      rline=trim(adjustl(rline))
      IF (rline(1:1)=='!' .or. rline(1:1)=='>' .or. len_trim(rline) == 0) CYCLE STAGE_READ
      ll=len_trim(rline)
      ls = SCAN(rline(1:ll),'#',right_blank)
      IF (ls == 0) then
             print*, 'ERROR! something wrong in .ref file for # of stages - The program stops.'
             print*, 'ERROR at line ', nr_rline, ' : ',rline(1:ll)
              STOP
          ENDIF
      read(rline(ls+1:ll),*) NSTAG                               ! 1st relevant line
!      print*, 'NSTAG = ', NSTAG
      IF (NSTAG /= stagen) then
             print*, 'WARNING! Wrong # of stages at the first line of .ref file!'
             print*, 'The following # of stages will be performed: ', stagen
             NSTAG = stagen
      ENDIF
      exit STAGE_READ
   enddo STAGE_READ
! no more comment lines after STAGE_READ

   IF (NSTAG <= 0) then
     print*, 'ERROR! something wrong in .ref file for # of stages - The program stops.'
     print*, 'ERROR at line ', nr_rline, ' : ',rline(1:ll)
     STOP
   ENDIF

   IF (allocated(ref_stage)) deallocate (ref_stage)
   allocate( ref_stage(NSTAG) )

   STAGE_READ_LOOP: do stagen=1,NSTAG
!_________________ 1st line of each stage: index, [main_crit], method
      read(iu,'(a)',end=1) rline                                 ! 2nd relevant line
      nr_rline =  nr_rline + 1
      call clean_line(rline)
      rline=trim(adjustl(rline))
      ll=len_trim(rline)
      call lowcase(rline(1:ll))
      ls = SCAN(rline(1:ll),'#',right_blank)
      ls2 = SCAN(rline(1:ll),' ',right_blank)
      IF (rline(ls2+1:ll) == 'complex') THEN              
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = sceps_CP
          ENDIF
          ref_stage(i)%stage_type = 'C'
      ELSE IF (rline(ls2+1:ll) == 'newton') THEN              
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = sceps_CP
          ENDIF
          ref_stage(i)%stage_type = 'N'
      ELSE IF (rline(ls2+1:ll) == 'anneal') THEN              
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = 0.01d0
          ENDIF
          ref_stage(i)%stage_type = 'A'
      ELSE IF (rline(ls2+1:ll) == 'bobyqa') THEN              
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = sceps_CP
          ENDIF
          ref_stage(i)%stage_type = 'B'
      ELSE IF (rline(ls2+1:ll) == 'nelmea') THEN              
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = sceps_CP
          ENDIF
          ref_stage(i)%stage_type = 'M'
      ELSE     ! Defaulting to Nelder-Mead...          
          read(rline(ls+1:ls2),*,iostat=astat) i,x
          IF (astat==0) then
            ref_stage(i)%main_crit = x
          ELSE              
            read(rline(ls+1:ls2),*) i
            ref_stage(i)%main_crit = sceps_CP
          ENDIF
          ref_stage(i)%stage_type = 'C'
      ENDIF
      ref_stage(i)%stage_n = stagen
! stagen = number in reading order
! i      = number as given

      allocate( ref_stage(i)%param_set(0:nstr_w) )

      STAGE_STRUC_LOOP: do j=1,NSTR_W
! FIRST LINE: reading structure index kstr
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
          call clean_line(rline)
          rline=trim(adjustl(rline))
          ll=len_trim(rline)
          ls = SCAN(rline(1:ll),'%')
          IF (ls == 0) then
             print*, 'ERROR! something wrong in .ref file for Phase %',j,', The program stops.'
             print*, 'ERROR at line ', nr_rline, ' : ',rline(1:ll)
              STOP
          ENDIF
          read(rline(ls+1:ll),*) kstr                      
          IF (kstr < 0 .or. kstr > nstr_w) THEN
              print*, 'WARNING! wrong structure number %',kstr,', the program stops.'
              STOP
          ENDIF
          allocate( ref_stage(i)%param_set(kstr)%ref_nolin(1:PARAPHAS(kstr)%Numero) )
          
          IF (allocated(irob)) deallocate (irob)
          allocate( irob(1:2*NSP_AT_W(kstr)) )
          
! reading not relevant line
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
! reading parameter flags
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
          call clean_line(rline)
          rline=trim(adjustl(rline))
          IF (DB_INDEX_W(kstr) == 1) THEN
                ELIM_VIRG: do 
                   ls = SCAN(rline,';')
                   if (ls > 0 ) then
                       rline(ls:ls) = ' ' 
                   else
                       exit ELIM_VIRG
                   endif
                enddo ELIM_VIRG
          ENDIF
          read(rline,*, iostat=isu1) ref_stage(i)%param_set(kstr)%ref_nolin(1:PARAPHAS(kstr)%Numero_red)
          IF (isu1 /= 0) THEN
              print*, 'FATAL ERROR! Wrong Number of refined Parameters in file .ref for Phase :', kstr
              STOP
          ENDIF
            
          l=0 
          do kk =1,  PARAPHAS(kstr)%Numero_red
            if (ref_stage(i)%param_set(kstr)%ref_nolin(kk)==0) l=l+1
          enddo
          if  (l==PARAPHAS(kstr)%Numero_red) then 
            print*, 'ERROR in ref flags given in '//TRIM(REFINEMENT_FILE_W)
            STOP
          endif 
          
          isiz=ref_stage(i)%param_set(kstr)%ref_nolin(1)
          istrai=ref_stage(i)%param_set(kstr)%ref_nolin(2)
          irob(1:2*NSP_AT_W(kstr)) = ref_stage(i)%param_set(kstr)%ref_nolin(3:PARAPHAS(kstr)%Numero_red)
          
          ref_stage(i)%param_set(kstr)%ref_nolin(1:NumParSiz) = isiz
          ref_stage(i)%param_set(kstr)%ref_nolin(NumParSiz+1:NumParPha) = istrai

          do iat=1,NSP_AT_W(kstr)
            na = NumPar_at * (iat-1)
            na_red = NumPar_at_red * (iat-1)
            if (PARAPHAS(kstr)%law_O(iat) == 1) then
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + 1) = 1*irob(na_red+1)
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + 2:NumParPha + na +NumPar_Oc) = 0
            else if (PARAPHAS(kstr)%law_O(iat) == 2) then
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + 1: &
                                                     NumParPha + na + NumPar_Oc) = 1*irob(na_red+1)
            endif
            if (PARAPHAS(kstr)%law_B(iat) == 1) then
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + NumPar_Oc + 1)= 1*irob(na_red+2)
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + NumPar_Oc + 2:&
                                                     NumParPha + na + NumPar_Oc + NumPar_DW) = 0
            else if (PARAPHAS(kstr)%law_B(iat) == 2) then
              ref_stage(i)%param_set(kstr)%ref_nolin(NumParPha + na + NumPar_Oc+1: &
                                                     NumParPha + na + NumPar_Oc + NumPar_DW) = 1*irob(na_red+2)
            endif
          enddo
      enddo STAGE_STRUC_LOOP

      kstr = 0
      allocate( ref_stage(i)%param_set(kstr)%ref_nolin(1:PARAGLOB%Numero) )
      IF (DO_AMORPH_W) then
        STAGE_AMO_LOOP: do
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
          call clean_line(rline)
          rline=trim(adjustl(rline))
          ll=len_trim(rline)
          ls = SCAN(rline(1:ll),'%')
                                
          IF (ls == 0 .or. (ls>0.and.rline(ls+1:ls+3) /= 'amo') ) THEN
              print'(a,/,a,/,a)', 'ERRROR! not found %amo part in .ref at line: ',rline(1:ll),', the program stops.'
              STOP
          ENDIF
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
          call clean_line(rline)
          rline=trim(adjustl(rline))
          read(rline,*,iostat=astat) ref_stage(i)%ref_amo,ref_stage(i)%param_set(kstr)%ref_nolin
          IF (astat/=0) then
            read(rline,*,iostat=astat) ref_stage(i)%ref_amo
            ref_stage(i)%param_set(kstr)%ref_nolin=0
          ENDIF
          EXIT STAGE_AMO_LOOP
        enddo STAGE_AMO_LOOP
      ENDIF
      
      allocate( ref_stage(i)%ref_back(1:nset_w) )
      flag_read = .false.
      STAGE_SETS_LOOP: do
          read(iu,'(a)',end=1) rline
          nr_rline =  nr_rline + 1
          call clean_line(rline)
          rline=trim(adjustl(rline))
          ll=len_trim(rline)
          ls = SCAN(rline(1:ll),'#')
          
!          print*, 'flag_read ', nr_rline, rline(1:ll), flag_read
          IF (ls > 0) THEN 
              flag_read = .true.
              CYCLE STAGE_SETS_LOOP
          ENDIF    
          IF (.not. flag_read .and. ls == 0) then             
             print*, 'ERROR! something wrong in .ref file, about dataset # - The program stops.'
             print*, 'ERROR at line ', nr_rline, ' : ',rline(1:ll)
              STOP
          ENDIF
          read(rline,*) ref_stage(i)%ref_back(1:nset_w)
          exit STAGE_SETS_LOOP
      enddo STAGE_SETS_LOOP

   enddo STAGE_READ_LOOP

 1 close(iu)

   call EXPAND_STAGES

 END SUBROUTINE read_ref_file
!*********************************************************************************
 SUBROUTINE EXPAND_STAGES

   IMPLICIT NONE
   INTEGER(I4B)   :: i,k,kk,kkk,k2,k3,kk2,kstr,kset,kagl

   RUN_numpar_NL = SUM(PARAPHAS(1:NSTR_W)%Numero) + PARAGLOB%Numero
   RUN_numpar_LI = NSET_W - 1 + NSTR_W + NAMO_W
   do i = 1,NSET_BACK_W
     RUN_numpar_LI =  RUN_numpar_LI + BACKGROUND(i)%DIMBACK
   enddo
   RUN_numpar_all = RUN_numpar_NL+RUN_numpar_LI

   DO i=1,NSTAG

     IF (associated(ref_stage(i)%long_flag)) nullify(ref_stage(i)%long_flag)
     ALLOCATE(ref_stage(i)%long_flag(RUN_numpar_all))

     ref_stage(i)%long_flag = 0
     k=0
     do kstr=1,NSTR_W
       ref_stage(i)%long_flag(k+1:k+PARAPHAS(kstr)%Numero) = ref_stage(i)%PARAM_SET(kstr)%REF_NOLIN
       k=k+PARAPHAS(kstr)%Numero
     enddo
     IF (DO_AMORPH_W) then
        ref_stage(i)%long_flag(k+1:k+PARAGLOB%Numero) = ref_stage(i)%PARAM_SET(0)%REF_NOLIN
        kagl = SUM(ref_stage(i)%PARAM_SET(0)%REF_NOLIN)
        k=k+PARAGLOB%Numero
     ELSE
        kagl = 0
     ENDIF
     
!_________ LIN. PAR. s

     ref_stage(i)%stgpar_NL = SUM(ref_stage(i)%long_flag(:k))

     ref_stage(i)%long_flag(k+1:k+NSET_W - 1 + NSTR_W) = 1
     k=k+NSET_W - 1 + NSTR_W

     ref_stage(i)%long_flag(k+1:k+NAMO_W) = ref_stage(i)%REF_AMO
     k=k+NAMO_W
     
     do kset=1,NSET_W
       ref_stage(i)%long_flag(k+1:k+BACKGROUND(kset)%dimback) = ref_stage(i)%REF_BACK(kset)
       if (NSET_BACK_W == 1) EXIT
     enddo
     ref_stage(i)%stgpar_all = SUM(ref_stage(i)%long_flag)
     ref_stage(i)%stgpar_LI = ref_stage(i)%stgpar_all - ref_stage(i)%stgpar_NL

!_______ LONG_FLAG is done

     IF (associated(ref_stage(i)%long_mask)) nullify(ref_stage(i)%long_mask)
     ALLOCATE(ref_stage(i)%long_mask(ref_stage(i)%stgpar_all))
     call MAKE_MASK(ref_stage(i)%long_flag,ref_stage(i)%long_mask)

!_______ LONG_MASK is done

     IF (associated(ref_stage(i)%back_flag)) nullify(ref_stage(i)%back_flag)
     ALLOCATE(ref_stage(i)%back_flag(ref_stage(i)%stgpar_all))
     k2=0
     do k=1,ref_stage(i)%stgpar_NL-kagl

       ref_stage(i)%back_flag(k)%which_type = 1
       ref_stage(i)%back_flag(k)%kset       = 0

       kk = ref_stage(i)%long_mask(k)
       k2 = 0
       dokkk:do kkk=1,NSTR_W
         k3 = k2+PARAPHAS(kkk)%Numero
         IF (kk > k2 .and. kk <= k3) THEN
           ref_stage(i)%back_flag(k)%nup = kk-k2
           ref_stage(i)%back_flag(k)%kstr = kkk
           EXIT dokkk
         ENDIF
         k2 = k3
       enddo dokkk
     enddo
     do k=ref_stage(i)%stgpar_NL - kagl + 1, ref_stage(i)%stgpar_NL

       ref_stage(i)%back_flag(k)%which_type = 0
       ref_stage(i)%back_flag(k)%kset       = 0
       ref_stage(i)%back_flag(k)%kstr = 0

       kk = ref_stage(i)%long_mask(k)
       k3 = k2+PARAGLOB%Numero
       IF (kk > k2 .and. kk <= k3) THEN
          ref_stage(i)%back_flag(k)%nup = kk-k2
       ENDIF
       k2 = k3
       
     enddo

     do k=ref_stage(i)%stgpar_NL + 1, ref_stage(i)%stgpar_all

       kk = ref_stage(i)%long_mask(k) - RUN_numpar_NL

       IF (kk <= NSET_W-1) THEN
         ref_stage(i)%back_flag(k)%which_type = 2
         ref_stage(i)%back_flag(k)%kset       = kk-1
         ref_stage(i)%back_flag(k)%kstr = 0
         ref_stage(i)%back_flag(k)%nup  = 0
       ELSE IF (kk > NSET_W-1 .and. kk<=NSET_W-1+NSTR_W) THEN
         ref_stage(i)%back_flag(k)%which_type = 3
         ref_stage(i)%back_flag(k)%kset       = 0
         ref_stage(i)%back_flag(k)%kstr = kk - (NSET_W-1)
         ref_stage(i)%back_flag(k)%nup  = 0
       ELSE IF (kk > NSET_W-1+NSTR_W .and. kk<=NSET_W-1+NSTR_W+NAMO_W) THEN
         ref_stage(i)%back_flag(k)%which_type = 4
         ref_stage(i)%back_flag(k)%kset       = 0
         ref_stage(i)%back_flag(k)%kstr = 0
         ref_stage(i)%back_flag(k)%nup  = kk - (NSET_W-1+NSTR_W)
       ELSE IF (kk > NSET_W-1+NSTR_W+NAMO_W) THEN
         ref_stage(i)%back_flag(k)%which_type = 5
         ref_stage(i)%back_flag(k)%kstr = 0
         IF (NSET_BACK_W == 1) THEN
           ref_stage(i)%back_flag(k)%kset = 1
           ref_stage(i)%back_flag(k)%nup  = kk - (NSET_W-1+NSTR_W+NAMO_W)
         ELSE
           kk2 = kk - (NSET_W-1+NSTR_W+NAMO_W)
           k2 = 0
           dokkk2:do kkk=1,NSET_W
             k3 = k2 + BACKGROUND(kkk)%dimback
             IF (kk2 > k2 .and. kk2 <= k3) THEN
               ref_stage(i)%back_flag(k)%nup = kk2 - k2
               ref_stage(i)%back_flag(k)%kset = kkk
               EXIT dokkk2
             ENDIF
             k2 = k3
           enddo dokkk2
         ENDIF
       ENDIF
       
     enddo

   ENDDO

 END SUBROUTINE EXPAND_STAGES

 FUNCTION PLACE_IN_LONG_FLAG(istg,kstr,kset,which_type,nup)

   INTEGER(I4B),intent(IN)  :: istg,kstr,kset,which_type,nup
    !! which_type = 0  :: AMO/GLOB parameter (non-linear)
    !! which_type = 1  :: phase parameter (non-linear)
    !! which_type = 2  :: SETscal (kset)
    !! which_type = 3  :: STRscal (kstr)
    !! which_type = 4  :: AMOscal (nup)
    !! which_type = 5  :: BKGscal (kset,kstr,nup)
   INTEGER(I4B)             :: PLACE_IN_LONG_FLAG
   INTEGER(I4B)             :: i,j


   PLACE_IN_LONG_FLAG = 0

   SELECT CASE(which_type)
     CASE(0)
       IF (ref_stage(istg)%PARAM_SET(kstr)%REF_NOLIN(nup) /= 0) &
          PLACE_IN_LONG_FLAG = SUM(PARAPHAS(1:NSTR_W)%Numero) + nup
     CASE(1)
       IF (ref_stage(istg)%PARAM_SET(kstr)%REF_NOLIN(nup) /= 0) &
          PLACE_IN_LONG_FLAG = SUM(PARAPHAS(1:kstr-1)%Numero) + nup
     CASE(2)
       IF (kset > 1) &
          PLACE_IN_LONG_FLAG = RUN_numpar_NL + kset-1
     CASE(3)
       PLACE_IN_LONG_FLAG = RUN_numpar_NL + NSET_W-1 + kstr
     CASE(4)
       IF (ref_stage(istg)%REF_AMO /= 0) &
          PLACE_IN_LONG_FLAG = RUN_numpar_NL + NSET_W-1 + NSTR_W + nup
     CASE(5)
       if (NSET_BACK_W == 1) THEN
         IF (ref_stage(istg)%REF_BACK(kset) /= 0) &
            PLACE_IN_LONG_FLAG = RUN_numpar_NL + NSET_W-1 + NSTR_W + NAMO_W + nup
       else if (NSET_BACK_W == NSET_W) THEN
         IF (ref_stage(istg)%REF_BACK(kset) /= 0) &
            PLACE_IN_LONG_FLAG = RUN_numpar_NL + NSET_W-1 + NSTR_W + NAMO_W + SUM(BACKGROUND(1:kset-1)%dimback) + nup
       endif
     CASE DEFAULT
       STOP 'Guess what went wrong?'
   END SELECT

 END FUNCTION PLACE_IN_LONG_FLAG

 subroutine STAGE_assoc_PAR(kstage)
    implicit none
    integer(I4B),INTENT(IN)  :: kstage
    integer(I4B)             :: i

    PARAGLOB%nano_dostage  = PARAGLOB%nano_parref * ref_stage(kstage)%PARAM_SET(0)%REF_NOLIN
    PARAGLOB%nano_doit     = PARAGLOB%nano_dostage
    call MAKE_MASK(PARAGLOB%nano_dostage, PARAGLOB%nano_mask)

    do i=1,NSTR_W
      PARAPHAS(i)%nano_dostage  = PARAPHAS(i)%nano_parref * ref_stage(kstage)%PARAM_SET(i)%REF_NOLIN
      PARAPHAS(i)%nano_doit  = PARAPHAS(i)%nano_dostage
   
!______ Masking
      call MAKE_MASK(PARAPHAS(i)%nano_dostage, PARAPHAS(i)%nano_mask)

    enddo

 end subroutine STAGE_assoc_PAR

 subroutine MAKE_PSEND(kstr, nparstage, parstage, npsend, psend)
    implicit none
    integer(I4B),INTENT(IN)                  :: kstr,nparstage
    REAL(CP),INTENT(IN),DIMENSION(:)         :: parstage
    integer(I4B),INTENT(INOUT)               :: npsend
    REAL(CP),INTENT(INOUT),DIMENSION(:)      :: psend


 end subroutine MAKE_PSEND

END MODULE STRATEGY
!___________________________________________________________________________________________________


