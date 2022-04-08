!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     Copyright (c) 2011 Antonio Cervellino, Antonietta Guagliardi
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
module GURKEN_2012
 use nano_deftyp

 TYPE,PUBLIC     :: GURK
     INTEGER(I4B)      :: zeta,nato,focc
     REAL(DP),DIMENSION(:,:),allocatable :: ACOO
     REAL(DP)  :: OKKU,OKKU_init
     CHARACTER(2) :: SymbA
 END TYPE GURK

real(DP),parameter :: cos30=sr3*half
real(DP),save :: trigdetcel=one, cogeo0=one, vbox3(3)=[one,one,one]

contains
!***************************************************************
function COGEO(al,be,ga)
implicit none
real(DP),intent(IN)  :: al,be,ga
real(DP) :: cogeo,coal,cobe,coga

if (max(abs(al-90.0_DP),abs(be-90.0_DP),abs(ga-90.0_DP))<sceps_DP) then
  cogeo=one
  cogeo0=one
  trigdetcel=one
  return
endif
if (abs(al-90.0_DP)<sceps_DP) then
  coal=zero
else
  coal = cos(degrees_to_radians*al)
endif
if (abs(be-90.0_DP)<sceps_DP) then
  cobe=zero
else
  cobe = cos(degrees_to_radians*be)
endif
if (abs(ga-90.0_DP)<sceps_DP) then
  coga=zero
else
  coga = cos(degrees_to_radians*ga)
endif

trigdetcel = one - coga*coga - cobe*cobe - coal * ( coal - two * coga * cobe )
cogeo = sqrt( trigdetcel )
cogeo0 = cogeo
end function COGEO
!***************************************************************
function ThreeFac(al,be,ga, a,b,c)
implicit none
real(DP),intent(IN)  :: al,be,ga, a,b,c
real(DP) :: ThreeFac(3),sial,sibe,siga

if (max(abs(al-90.0_DP),abs(be-90.0_DP),abs(ga-90.0_DP))<sceps_DP) then
  ThreeFac(:)=one
  return
endif
if (abs(al-90.0_DP)<sceps_DP) then
  ThreeFac(1) = a
else
  sial = sin(degrees_to_radians*al)
  ThreeFac(1) = a/abs(sial)  
endif
if (abs(be-90.0_DP)<sceps_DP) then
  ThreeFac(2) = b
else
  sibe = sin(degrees_to_radians*be)
  ThreeFac(2) = b/abs(sibe)  
endif
if (abs(ga-90.0_DP)<sceps_DP) then
  ThreeFac(3) = c
else
  siga = sin(degrees_to_radians*ga)
  ThreeFac(3) = c/abs(siga)  
endif

vbox3 = ThreeFac

end function ThreeFac
!***************************************************************
function CosCry(ang_deg)
implicit none
real(DP),intent(IN)  :: ang_deg
real(DP) :: CosCry


if (abs(ang_deg)<sceps_DP) then
  CosCry=one
else if (abs(ang_deg-30.d0)<sceps_DP) then
  CosCry=cos30
else if (abs(ang_deg-60.d0)<sceps_DP) then
  CosCry=half
else if (abs(ang_deg-90.d0)<sceps_DP) then
  CosCry=zero
else if (abs(ang_deg-120.d0)<sceps_DP) then
  CosCry=-half
else if (abs(ang_deg-150.d0)<sceps_DP) then
  CosCry=-cos30
else if (abs(ang_deg-180.d0)<sceps_DP) then
  CosCry=-one
else 
  CosCry=cos(degrees_to_radians*ang_deg)
endif

end function CosCry
!***************************************************************
function EigMTDS(al_C,be_C,ga_C, a_C,b_C,c_C)
implicit none
real(DP),intent(IN)  :: al_C,be_C,ga_C, a_C,b_C,c_C
real(DP) :: EigMTDS(3),cal,cbe,c2ga,c2al,c2be,cga,zuk,zuki,a,b,c,d,p,q,dtp,dtpk,&
            tk(3),toadd, &
            a2,b2,c2
integer(I4B) :: k

a2=a_C**2
b2=b_C**2
c2=c_C**2
cal=CosCry(al_C)
cbe=CosCry(be_C)
cga=CosCry(ga_C)
c2al=two*cal*cal-one
c2be=two*cbe*cbe-one
c2ga=two*cga*cga-one

a=one
b=-(a2+b2+c2)
c=(a2*b2*(one-c2ga)+b2*c2*(one-c2al)+c2*a2*(one-c2be))*half
d=a2*b2*c2*(one-four*cal*cbe*cga+c2al+c2be+c2ga)*half

!__________ specialized for a=1
p=(c-b*b*unter)
q=(b*(two*b*b-nine*c) +27.d0*d)/27.d0
toadd = -b*unter

zuk=sqrt(-unter*p)
zuki=one/zuk
dtp=duter*pi
do k=0,2
  dtpk=real(-k,DP)*dtp
  tk(k+1) = two*zuk*cos( unter*ACOS(max(-one,min(one, 1.5d0*q*zuki/p ))) + dtpk )
enddo
EigMTDS = tk+toadd

end function EigMTDS
!***************************************************************
function LATBOX(al,be,ga, a,b,c, Rsph)
implicit none
real(DP),intent(IN)  :: al,be,ga, a,b,c, Rsph
real(DP) :: xgeo,vf3(3)
integer(I4B) :: LATBOX(3)
integer(I4B) :: i

xgeo=COGEO(al,be,ga)
vf3=ThreeFac(al,be,ga, a,b,c)
vf3=vf3*xgeo
do i=1,3
  LATBOX(i) = CEILING(Rsph/vf3(i))
enddo
end function LATBOX
!***************************************************************
end module GURKEN_2012
!___________________________________________________________________________________________________
module paper_blood
use nano_deftyp
use GURKEN_2012
real(DP),parameter     :: Delta0 = 0.03_DP, rho=2.7_DP, one_sqrtpi2=.398942280401432677939946059934381870_DP, &
                          facnorg=one_sqrtpi2/rho, maxexparg = 42.d0, &
                          zoka=0.8_DP,rho_sq=rho*rho
real(DP),parameter     :: sqrt216ln2 = 12.2360038820257076132121431993506568d0
real(DP),save          :: max_intrinsic_sigma = zero
integer(I4B),parameter :: Ndelta=32,Nq=10001,N1cont_min=33
integer(I4B),save      :: N1cont=N1cont_min
real(DP),dimension(2),parameter :: updown=[one,-one]
real(DP),allocatable,save  :: atx(:,:,:),ato(:,:),atb(:,:)
character(len=7),parameter :: centering_vals = 'PABCIRF'
integer(I4B),parameter     :: centering_mult(7) = [1,2,2,2,2,3,4]
character(len=1),save      :: centering_type = 'P'
character(len=10),save     :: spgroup_name = 'P1        '
character(len=24),save     :: spgroup_file = 'SPG_grp/SG_Nr_001.grp   '
character(len=40),save     :: spgroup_IDENT= '001[P1]SPG_grp/SG_Nr_001.grp            '
integer(I4B),save          :: nspgroup=1,ncenters_cell = 1
real(DP),save              :: cell_volume, cell_volume_red
integer(I4B),allocatable,save :: atisubu(:,:,:)
real(DP),save     :: thr_S1D
real(DP),allocatable,save     :: for_samv_P(:,:),for_samv_N(:,:)
integer(I4B),save :: centersum(Ndelta)=0,bnd_N(2,Ndelta)

real(DP),save     :: Delta,co2,wg,Deltas(Ndelta),qtop(Ndelta),cng,beta,leps,lepsii,cnorg(Ndelta),cnorgD2(Ndelta), &
                     diamax,Deltas_sq(Ndelta) !!!externally
integer(I4B),save :: nbeta,nbeta1,dimens(Ndelta),dimemax,fileflag=0,idotest=0

real(DP),parameter:: w_presamp0=two**(-13), sigmaG_presamp0=w_presamp0*half/sr3, &
                     Bth_presamp0=duter*Pi*Pi*w_presamp0*w_presamp0 !!!externally maybe
real(DP),save    :: w_presamp=w_presamp0, sigmaG_presamp=sigmaG_presamp0, &
                     Bth_presamp=Bth_presamp0 !!!externally maybe
real(DP),save     :: a_latt_cioc=one           !!!externally
real(DP),save     :: planeMT(2,2),planeDV(2,2)          !!!externally
!!!integer(I4B),save :: n_sp_atom=0, n_at_pair=0  !!!externally

type,public :: celarrays
 integer(I4B) ::  n_sp_atom=0,  n_at_pair=0
 integer(I4B),allocatable :: nat(:)
 real(DP),allocatable :: xnat(:)
 integer(I4B),allocatable :: ndi(:)
 integer(I4B),allocatable :: Z_at(:)
 integer(I4B),allocatable :: zappa(:,:)
 real(DP),allocatable :: summul(:)
 real(DP),allocatable :: termcon(:)
 real(DP),allocatable :: termcon_all(:)
 real(DP),allocatable :: termcon_ineq(:)
 integer(I4B),allocatable :: point_eqpair(:)
 integer(I4B),allocatable :: point_neqpair(:)
 real(DP),allocatable :: ZZP(:),minallowdist(:)
 real(DP),allocatable :: atx(:,:,:),atb(:,:),ato(:,:)
 real(DP),allocatable :: atxCSU(:,:,:)
end type celarrays
type(celarrays),allocatable,save :: Celty(:)
integer(I4B),save :: ncelltypes, ncellpairs=1
integer(I4B),allocatable,save :: n_sp_atom_V(:), n_at_pair_V(:)
!__________________________________________________________________ global for sampling - begin
integer(I4B),save              :: n_sp_atom_glo, n_at_pair_glo
 real(DP),allocatable,save     :: numcells_byphase(:)
 integer(I4B),allocatable,save :: Z_at_glo(:),nat_glo(:),zappa_glo(:,:),ndi_glo(:)
 real(DP),allocatable,save     :: xnat_glo(:),termcon_glo(:),termcon_all_glo(:),termcon_ineq_glo(:),summul_glo(:)
!_____ read_multicell
integer(I4B),allocatable,save      :: naspe_cells(:),address_atsp(:,:),return_address_atsp(:,:),  &
                                      glopair(:,:)
integer(I4B),save                  :: naspe_all,sumnascel, napair_all
character(len=333),dimension(:),allocatable :: celfinames
!__________________________________________________________________ global for sampling - end


real(DP),allocatable,save     :: samv(:,:,:,:),qt(:),Iq0(:),Iqt(:),Lorf(:),aux(:),urg(:,:)
integer(I4B),allocatable,save :: nqt(:)
character(132),save :: nampath2,namfilin,namfilou
integer(I4B),save   :: lenpath2,lenfilin,lenfilou

logical,save :: multiple_steps=.true.
integer(I4B),save   :: Ndelta1=1,Ndelta2=Ndelta
real(DP),parameter     :: wl_gamma_60Co=4.768d-3,wl_ourqmax=0.147721162951831208905011453688428452d0
real(DP),save     :: working_wavelength=zero,working_ttmax=160.d0, working_ttmin=0.d0, working_dtt=0.01d0, &
                     rmin=0.5d0,rmax=100.d0,dr=0.01d0
real(DP),allocatable,save     :: rGvec(:),gRvec(:),S_Q(:),S_Q_byp(:,:),Q_S(:),dQvv(:),AtScFk(:),gR_byp(:,:)
integer(I4B),save   :: n_rGvec,n_qSvec

character(512),save :: basepath_sampled
character(64),save  :: sampled_folder='DISTANCES',sampled_folder_READ='DIST_2DIM'
integer(I4B),save   :: lbasepath_sampled,lsampled_folder=9

INTEGER(I4B), save       :: N_max_SPH, N1_max_ROD, N2_max_ROD
REAL(DP),save           :: Orig_Cell(3), R_max_SPH, D_max_ROD, L_max_ROD !!! , wavelen, th2max
character(132),save     :: Pha_name
CHARACTER(16),save      :: Pears_symb, Strukt_B, Chem_F
CHARACTER(3),save       :: Shape_Clu
integer(I4B),save ::  num_grow_dir = 1
real(DP),allocatable,save :: growing_diam(:,:)
real(DP),save  :: Size_Abscissa(2)=[one,zero], Actual_Diam(2)=[one,zero],&
                  abcabgy(6)=[one,one,one,90.d0,90.d0,90.d0]

! stacking f. probabilities
real(DP),save :: alpha_STF, beta_STF

  contains
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function GET_CENTERING_MULT(a1)
implicit none
character(len=1),intent(IN) :: a1
integer(I4B) :: GET_CENTERING_MULT
integer(I4B) :: i,io

 if (verbose) print'("&&&&&&&& ",a1," -0- ",i1)',a1,GET_CENTERING_MULT
!__ Set default...
   GET_CENTERING_MULT=1
!__ First, try to read a1 as a number (the number of centers, directly)
   read(a1(1:1),'(i1)',iostat=io) GET_CENTERING_MULT
!__ If it succeeds, that's it, otherwise...
 if (verbose)  print'("&&&&&&&& ",a1," -1- ",i1)',a1,GET_CENTERING_MULT
!__ If the former does not succeed, try as a crystallographic letter
   if (io/=0) then
     GET_CENTERING_MULT=1
     do i=1,7
       if (a1==centering_vals(i:i)) then
         GET_CENTERING_MULT=centering_mult(i)
         exit
       endif
     enddo
 if (verbose) print'("&&&&&&&& ",a1," -2- ",i1)',a1,GET_CENTERING_MULT
   endif

end function GET_CENTERING_MULT
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine DO_SETUP
implicit none
integer(I4B) :: i
real(DP)  :: working_qmax, max_sigma_rel
!
  call DEF_EPS
  thr_S1D=half*eps_DP**2
  Deltas = (/(i,i=1,Ndelta)/)*Delta0
  Deltas_sq = Deltas**2
  leps=log(eps_DP)
  lepsii=-one/leps
  beta = sr2*sqrt(-leps)
  nbeta=1+ceiling(beta*rho)
  cnorg  = facnorg/Deltas
  cnorgD2= facnorg*Deltas
  qtop=half*zoka/Deltas
  Ndelta1=1; Ndelta2=Ndelta ! default: do all steps
  if (.not.multiple_steps) then
    working_wavelength=max(working_wavelength,wl_ourqmax)
    working_ttmax=min(working_ttmax,160.d0)
    working_qmax=two*sin(duet2r*working_ttmax)/working_wavelength
    do i=Ndelta,1,-1
      if (qtop(i)>working_qmax) then
        Ndelta1=i
        Ndelta2=i
        exit
      endif
    enddo
    if (Ndelta2>Ndelta1) then
      print'(a,/,a,/,a,1x,g14.6,/,a,1x,g14.6)','WARNING: you selected a very short wavelength. ',&
             'Maybe you use low 2theta maximum. Watch the qmax!',&
             'qmax = 2 * sin(theta) /wavelength      = ',working_qmax,&
             'Qmax = 4 * Pi * sin(theta) /wavelength = ',working_qmax*Pi2
      Ndelta1=1
      Ndelta2=1
    endif
  endif
!________ NEW may 2011 - 

  do i=Ndelta1,Ndelta2
    max_sigma_rel=sqrt( (max_intrinsic_sigma/Deltas(i))**2 + rho_sq)
    N1cont = max( N1cont, ceiling(max_sigma_rel*sqrt216ln2) )
  enddo
  allocate(for_samv_P(-N1cont:N1cont,Ndelta1:Ndelta2),for_samv_N(N1cont,Ndelta1:Ndelta2))
!  print'(a,i6,a,g18.12)','N1cont = ',N1cont,'; max. intrinsic sigma = ',max_intrinsic_sigma
!
end subroutine DO_SETUP
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine ALLOCALL(ncelltypes_in,icel1)
implicit none
integer(I4B),optional,intent(IN) :: ncelltypes_in,icel1
integer(I4B) :: i,nap,nas,icel
logical :: present_args(2)

present_args = [present(ncelltypes_in),present(icel1)]

if ( present_args(1).neqv.present_args(2)) then
  print*,'ALLOCALL error - optional arguments should be BOTH or NONE! they are : ',PRESENT(ncelltypes_in),PRESENT(icel1)
  stop 'ALLOCALL error - optional arguments should be BOTH or NONE! they are not.'
endif


!print*, 'ALLOCALL , ncelltypes_in = ', ncelltypes_in

if (.not.present_args(1)) ncelltypes = 1
if (.not.present_args(2)) then
   icel = 1
else
   icel = icel1
endif

if (allocated(Celty).and.icel==1) then
  do i=1,size(Celty)
    deallocate(Celty(i)%nat)
    deallocate(Celty(i)%xnat)
    deallocate(Celty(i)%ndi)
    deallocate(Celty(i)%Z_at)
    deallocate(Celty(i)%zappa)
    deallocate(Celty(i)%summul)
    deallocate(Celty(i)%termcon)
    deallocate(Celty(i)%termcon_all)
    deallocate(Celty(i)%termcon_ineq)
    deallocate(Celty(i)%point_eqpair)
    deallocate(Celty(i)%point_neqpair)
    deallocate(Celty(i)%minallowdist)
    deallocate(Celty(i)%ZZP)
  enddo
  deallocate(Celty)
endif
if (.not.allocated(Celty)) then
!  print*,'Allocating Celty to ',ncelltypes
  allocate(Celty(ncelltypes))
endif


i=icel
nas = n_sp_atom_V(i)
nap = n_at_pair_V(i)
Celty(i)%n_sp_atom=nas
Celty(i)%n_at_pair=nap
if (Celty(i)%n_sp_atom==0) then
  print*,i,'No atoms, no allocation, stopping'
  stop
endif
if (Celty(i)%n_at_pair==0) then
  print*,i,'No pairs, no allocation, stopping'
  stop
endif
! print*,'Allocating Celty(',i,') subarrays to # at.species = ',nas,' and # pairs = ',nap
allocate(Celty(i)%nat(1:nas), Celty(i)%Z_at(1:nas), Celty(i)%xnat(1:nas), Celty(i)%ndi(1:nap),&
         Celty(i)%zappa(2,1:nap), Celty(i)%summul(1:nap), Celty(i)%ZZP(nap), &
         Celty(i)%termcon(1:nas), Celty(i)%termcon_all(nap), Celty(i)%termcon_ineq(nap-nas), &
         Celty(i)%point_eqpair(nas), Celty(i)%point_neqpair(nap-nas),Celty(i)%minallowdist(nap))
Celty(i)%termcon=zero
Celty(i)%termcon_ineq=zero
Celty(i)%termcon_all=zero
Celty(i)%summul=zero
Celty(i)%minallowdist=0.99d0


end subroutine ALLOCALL
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine ALLOCALL_GLO(ncelltypes_in)
implicit none
integer(I4B),optional,intent(IN) :: ncelltypes_in
integer(I4B) :: i,j,kk,ii,kapa,ias1,ias2
logical :: present_args(1)


present_args = [present(ncelltypes_in)]



if (.not.present_args(1)) then
  ncelltypes = 1
endif
if (verbose) print*,'In ALLOCALL_GLO :', present_args, ncelltypes


!____________ Set also global for output in .smp_INFO

if (ncelltypes > 1) then
  if (verbose) print*,'ncelltypes > 1',ncelltypes
  n_sp_atom_glo=naspe_all
  n_at_pair_glo=napair_all
  if (verbose) print*,n_sp_atom_glo,n_at_pair_glo
  allocate(Z_at_glo(n_sp_atom_glo),nat_glo(n_sp_atom_glo),zappa_glo(2,n_at_pair_glo),ndi_glo(n_at_pair_glo), &
           xnat_glo(n_sp_atom_glo),termcon_glo(n_sp_atom_glo), &
           termcon_all_glo(n_at_pair_glo),termcon_ineq_glo(n_at_pair_glo-n_sp_atom_glo), &
           summul_glo(n_at_pair_glo))
  if (verbose) then
     print*,'allocation OK'
     print*,'Cell1 spec. ',address_atsp(:,1)
     print*,'Cell2 spec. ',address_atsp(:,2)
     print*,'Cell1 spec. ',return_address_atsp(:,1)
     print*,'Cell2 spec. ',return_address_atsp(:,2)
  endif
  do j=1,naspe_all
    CTP:do i=1,ncelltypes
      ii=return_address_atsp(j,i)
      print*,ii,i,j
      if (ii>0) then
        if (verbose) print*,j,' Calling Celty(',i,')',ii
        Z_at_glo(j) = Celty(i)%Z_at(ii)
        exit CTP
      endif
    enddo CTP
  enddo
  nat_glo = 0
  kk=0
  do j=1,naspe_all
    do i=j,naspe_all
      kk=kk+1
      zappa_glo(:,kk) = [j,i]
    enddo
  enddo
  ndi_glo = 0
  xnat_glo = zero
  termcon_glo = zero
  termcon_all_glo = zero
  termcon_ineq_glo = zero
  summul_glo = zero
  if (verbose) print*,'zeroing OK'
else
  n_sp_atom_glo=n_sp_atom_V(1)
  n_at_pair_glo=n_at_pair_V(1)
  naspe_all=n_sp_atom_glo
  napair_all=n_at_pair_glo
  allocate(Z_at_glo(n_sp_atom_glo),nat_glo(n_sp_atom_glo),zappa_glo(2,n_at_pair_glo),ndi_glo(n_at_pair_glo), &
           xnat_glo(n_sp_atom_glo),termcon_glo(n_sp_atom_glo), &
           termcon_all_glo(n_at_pair_glo),termcon_ineq_glo(n_at_pair_glo-n_sp_atom_glo), &
           summul_glo(n_at_pair_glo))
  Z_at_glo = Celty(1)%Z_at
  nat_glo = Celty(1)%nat
  zappa_glo = Celty(1)%zappa
  ndi_glo = Celty(1)%ndi
  
   if (verbose) print*, 'ALLOCALL_GLO - Celty(1)%nat, Celty(1)%xnat =', Celty(1)%nat, Celty(1)%xnat
  xnat_glo = Celty(1)%xnat
  termcon_glo = Celty(1)%termcon
  termcon_all_glo = Celty(1)%termcon_all
  termcon_ineq_glo = Celty(1)%termcon_ineq
  summul_glo = Celty(1)%summul


  allocate(address_atsp(naspe_all,ncelltypes),return_address_atsp(naspe_all,ncelltypes),&
         glopair(2,napair_all))
  address_atsp(:,1)=[(ias1,ias1=1,naspe_all)]
  return_address_atsp=address_atsp
  glopair=0
  kapa=0
  do ias1=1,naspe_all
    do ias2=ias1,naspe_all
      kapa=kapa+1
      glopair(:,kapa)=[ias1,ias2]
    enddo
  enddo
endif
if (verbose) print*,'In ALLOCALL_GLO done',glopair

end subroutine ALLOCALL_GLO
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine DO_ALLOK(nusam)
implicit none
integer(I4B),optional,intent(IN) :: nusam
integer(I4B) :: nusam1,i
real(DP) :: max_sigma_rel

nusam1=1
if (present(nusam)) then
  if (nusam>1) then
    nusam1=nusam
  endif
endif

  dimens(Ndelta1:Ndelta2) = N1cont+CEILING(diamax/Deltas(Ndelta1:Ndelta2))
  dimemax=maxval(dimens(Ndelta1:Ndelta2))
  if (allocated(samv)) deallocate(samv)
  allocate(samv(dimemax,n_at_pair_glo,Ndelta1:Ndelta2,nusam1))
  samv=zero
  
  if (idotest==1) then
    if (allocated(nqt)) deallocate(nqt)
    if (allocated(qt)) deallocate(qt)
    if (allocated(Iqt)) deallocate(Iqt)
    if (allocated(Iq0)) deallocate(Iq0)
    if (allocated(aux)) deallocate(aux)
    if (allocated(urg)) deallocate(urg)
    if (allocated(Lorf)) deallocate(Lorf)
    allocate(qt(Nq),Iq0(Nq),Iqt(Nq),Lorf(Nq),nqt(Ndelta1:Ndelta2),aux(Nq),urg(Nq,Ndelta1:Ndelta2))
  endif

!
end subroutine DO_ALLOK
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine READFILE2SAMP
implicit none
integer(I4B) :: i,iu,kk,k,k2,iii,kat,k1,io,ll
real(DP)    :: d00,m00
character(256) :: rl

ncelltypes=1
allocate(n_sp_atom_V(1),n_at_pair_V(1))
!______________Only one cell here

 iu=FIND_UNIT()
 diamax=0.0_DP
 open(iu,status='old',file=namfilin,action='read')
 read(iu,*)
 read(iu,'(a)') rl
 call CLEAN_LINE(rl)
 rl=trim(adjustl(rl))
 ll=len_trim(rl)
 read(rl(1:ll),*,iostat=io) n_sp_atom_V(1),n_at_pair_V(1), a_latt_cioc,w_presamp, sigmaG_presamp,Bth_presamp
 n_sp_atom_glo=n_sp_atom_V(1)
 n_at_pair_glo=n_at_pair_V(1)
 if (io/=0) then
   read(rl(1:ll),*) n_sp_atom_V(1),n_at_pair_V(1), a_latt_cioc
   w_presamp=two**(-13)
   sigmaG_presamp=w_presamp*half/sr3
   Bth_presamp=duter*Pi*Pi*w_presamp*w_presamp
 endif
 
 call ALLOCALL(ncelltypes_in=1,icel1=1)
 
 read(iu,*) Celty(1)%nat(:),Celty(1)%Z_at(:),Celty(1)%xnat(:),Celty(1)%ndi(:)
 Celty(1)%summul=0.0_DP
 if (ALL(Celty(1)%ndi==0)) then
   fileflag=1
   kk=0
   do k=1,n_sp_atom_V(1)
     do k2=k,n_sp_atom_V(1)
       kk=kk+1
       read(iu,*) Celty(1)%Z_at(k), Celty(1)%Z_at(k2), Celty(1)%ndi(kk)
       Celty(1)%ZZP(kk)=real(Celty(1)%Z_at(k)*Celty(1)%Z_at(k2),DP)
       do i=1,Celty(1)%ndi(kk)
          read(iu,*) d00, m00
          diamax=max(diamax,d00*a_latt_cioc)
          Celty(1)%summul(kk) = Celty(1)%summul(kk)+m00
       enddo
     enddo
   enddo
 else
   fileflag=0
   kk=0
   do k=1,n_sp_atom_V(1)
     do k2=k,n_sp_atom_V(1)
       kk=kk+1
       read(iu,*) Celty(1)%Z_at(k), Celty(1)%Z_at(k2)
       Celty(1)%ZZP(kk)=real(Celty(1)%Z_at(k)*Celty(1)%Z_at(k2),DP)
       do i=1,Celty(1)%ndi(kk)
          read(iu,*) d00, m00
          diamax=max(diamax,d00*a_latt_cioc)
          Celty(1)%summul(kk) = Celty(1)%summul(kk)+m00
       enddo
     enddo
   enddo
 endif
 close(iu)

 kk=0
 do k=1,n_sp_atom_V(1)
   do k2=k,n_sp_atom_V(1)
     kk=kk+1
     Celty(1)%zappa(:,kk)=(/k,k2/)
   enddo
 enddo
 Celty(1)%point_eqpair=0
 Celty(1)%point_neqpair=0
 kk=0
 do k=1,n_at_pair_V(1)
   k1=Celty(1)%zappa(1,k)
   k2=Celty(1)%zappa(2,k)
   if (k1==k2) then
     Celty(1)%point_eqpair(k1)=k
   else
     kk=kk+1
     Celty(1)%point_neqpair(kk)=k
   endif
 enddo
 do kk=1,n_at_pair_V(1)
   kat = 0
   do k1=1,n_sp_atom_V(1)
     iii=k1*(3+2*n_sp_atom_V(1)-k1)
     iii=-n_sp_atom_V(1)+(iii/2)
     if (iii==kk) then
       kat=k1
       exit
     endif
   enddo
   if (kat>0) then
     Celty(1)%termcon(kat) = Celty(1)%xnat(kat)*Celty(1)%xnat(kat)-Celty(1)%summul(kk)
   endif
 enddo
 Celty(1)%termcon_all(Celty(1)%point_eqpair(:))=Celty(1)%termcon
 Celty(1)%termcon_all(Celty(1)%point_neqpair(:))=zero
 Celty(1)%termcon_ineq=zero
 
!____________ Set also global for output in .smp_INFO

call ALLOCALL_GLO()

end subroutine READFILE2SAMP
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine RUNSAMP
implicit none
integer(I4B) :: i,iu,kk,k,k2,iii
real(DP)    :: d00,m00

 iu=FIND_UNIT()
 diamax=0.0_DP
 open(iu,status='old',file=namfilin,action='read')
 read(iu,*)
 read(iu,*)
 read(iu,*)
 kk=0
 do k=1,n_sp_atom_V(1)
   do k2=k,n_sp_atom_V(1)
     kk=kk+1
     if (fileflag==0) then
       read(iu,*) Celty(1)%Z_at(k), Celty(1)%Z_at(k2)
     else if (fileflag==1) then
       read(iu,*) Celty(1)%Z_at(k), Celty(1)%Z_at(k2), iii
     endif
     do i=1,Celty(1)%ndi(kk)
        read(iu,*) d00, m00
        d00=d00*a_latt_cioc
        call SAM_ONE(d00,m00,kk)
     enddo
   enddo
 enddo
 close(iu)

end subroutine RUNSAMP
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine SAM_ONE(d0,mu0,jpair,isam, sigma_intrinsic)
implicit none
real(DP),intent(IN)    :: d0,mu0
real(DP),optional,intent(IN)    :: sigma_intrinsic
integer(I4B),intent(IN) :: jpair
integer(I4B),intent(IN),optional :: isam
integer(I4B) :: i,k,nd0,n1,n2,isam1
real(DP) :: u,ww,lmu,amu,smu,smud,k_dl
real(DP) :: ss_intr=zero, ss_intr_sq=zero
real(DP) :: ss_tot,ss_tot_sq
logical  :: increased_width = .false.

isam1=1
if (present(isam)) then
  if (isam>1) then
    isam1=isam
  endif
endif
if (present(sigma_intrinsic)) then
  if (sigma_intrinsic>eps_DP) then
    ss_intr=sigma_intrinsic
    ss_intr_sq=ss_intr*ss_intr
    increased_width = .true.
  endif
endif

 amu=abs(mu0)
 if (amu<sceps_DP) return
 smu=sign(one,mu0)/d0
 lmu=log(amu)
 nbeta1 = N1cont !1+ceiling(beta*rho*sqrt(one+lmu*lepsii))
 if (.not.increased_width) then
   do i=Ndelta1,Ndelta2
     Delta = Deltas(i)
     co2   = cnorgD2(i)
     smud=smu*co2
     nd0=nint(d0/Delta)
     n1=max(1,nd0-nbeta1)
     n2=min(dimens(i),nd0+nbeta1)
     ww=one/(Delta*rho)
     do k=n1,n2
       k_dl=k*smud
       u=(k*Delta-d0)*ww
       u=-half*u*u+lmu
       samv(k,jpair,i,isam1)=samv(k,jpair,i,isam1)+k_dl*exp(u)
     enddo
     n2=min(dimens(i),-nd0+nbeta1)
     if (n2 <= 0) cycle
     n1=max(1,-nd0-nbeta1)
     do k=n1,n2
       k_dl=k*smud
       u=(k*Delta+d0)*ww
       u=-half*u*u+lmu
       samv(k,jpair,i,isam1)=samv(k,jpair,i,isam1)-k_dl*exp(u)
     enddo
   enddo
 else
   do i=Ndelta1,Ndelta2
     Delta = Deltas(i)
     ss_tot_sq = rho_sq*Deltas_sq(i) + ss_intr_sq
     ss_tot    = sqrt(ss_tot_sq)
     co2   = one_sqrtpi2*Deltas_sq(i)/ss_tot !cnorgD2(i)
     smud=smu*co2
     nd0=nint(d0/Delta)
     n1=max(1,nd0-nbeta1)
     n2=min(dimens(i),nd0+nbeta1)
     ww=one/ss_tot
     do k=n1,n2
       k_dl=k*smud
       u=(k*Delta-d0)*ww
       u=-half*u*u+lmu
       if (-u > maxexparg) cycle
       samv(k,jpair,i,isam1)=samv(k,jpair,i,isam1)+k_dl*exp(u)
     enddo
     n2=min(dimens(i),-nd0+nbeta1)
     if (n2 <= 0) cycle
     n1=max(1,-nd0-nbeta1)
     do k=n1,n2
       k_dl=k*smud
       u=(k*Delta+d0)*ww
       u=-half*u*u+lmu
       if (-u > maxexparg) cycle
       samv(k,jpair,i,isam1)=samv(k,jpair,i,isam1)-k_dl*exp(u)
     enddo
   enddo
 endif
end subroutine SAM_ONE
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function SINC_ONE(d0,mu0,qx,nn)
implicit none
integer(I4B),intent(IN) :: nn
real(DP),intent(IN)    :: d0,mu0
real(DP),dimension(:),intent(IN)    :: qx
real(DP),dimension(nn)   :: SINC_ONE
!integer(I4B) :: 
!real(DP) :: 


SINC_ONE=pi2*d0*qx
where(SINC_ONE>sceps_DP) 
  SINC_ONE=mu0*sin(SINC_ONE)/SINC_ONE
elsewhere
  SINC_ONE=one
end where

end function SINC_ONE
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine SAM_ONE_TOALL(d0, sigma_intrinsic)
implicit none
real(DP),intent(IN)    :: d0
real(DP),optional,intent(IN)    :: sigma_intrinsic
integer(I4B) :: i,k,nd0,n1,n2,isam1,n1rel,n2rel,istop
real(DP) :: u,ww,lmu,amu,smu,smud,k_dl,ka,dXinv
real(DP) :: ss_intr=zero, ss_intr_sq=zero
real(DP) :: ss_tot,ss_tot_sq
logical  :: increased_width = .false.

!print*,'Call me',d0

for_samv_P=zero; for_samv_N=zero
dXinv=one/d0
increased_width = .false.
if (present(sigma_intrinsic)) then
  if (sigma_intrinsic>sceps_DP) then
    ss_intr=sigma_intrinsic
    ss_intr_sq=ss_intr*ss_intr
    increased_width = .true.
  endif
endif

!istop=0
if (.not.increased_width) then
  do i=Ndelta1,Ndelta2
    Delta = Deltas(i)
    co2   = cnorgD2(i)*dXinv
    nd0=nint(d0/Delta)
    centersum(i) = nd0
    n1=max(1,nd0-N1cont)
    n2=min(dimens(i),nd0+N1cont)
    n1rel=n1-nd0
    n2rel=n2-nd0
    ww=one/(Delta*rho)
    do k=n1rel,n2rel
      ka=k+nd0
      k_dl=ka*co2
      u=(ka*Delta-d0)*ww
      u=-half*u*u
      for_samv_P(k,i)=for_samv_P(k,i)+k_dl*exp(u)
    enddo
    n2=min(dimens(i),-nd0+N1cont)
    bnd_N(2,i)=n2
    if (n2 <= 0) cycle
    n1=max(1,-nd0-N1cont)
    bnd_N(1,i)=n1
    do k=n1,n2
      ka=k
      k_dl=ka*co2
      u=(ka*Delta+d0)*ww
      u=-half*u*u
      for_samv_N(k,i)=for_samv_N(k,i)-k_dl*exp(u)
    enddo
  enddo
else
  do i=Ndelta1,Ndelta2
    Delta = Deltas(i)
   ! co2   = cnorgD2(i)*dXinv
    ss_tot_sq = rho_sq*Deltas_sq(i) + ss_intr_sq
    ss_tot    = sqrt(ss_tot_sq)
    co2   = one_sqrtpi2*dXinv*Deltas_sq(i)/ss_tot !cnorgD2(i)
    nd0=nint(d0/Delta)
    centersum(i) = nd0
    n1=max(1,nd0-N1cont)
    n2=min(dimens(i),nd0+N1cont)
    n1rel=n1-nd0
    n2rel=n2-nd0
    ww=one/ss_tot
    do k=n1rel,n2rel
      ka=k+nd0
      k_dl=ka*co2
      u=(ka*Delta-d0)*ww
      u=-half*u*u
      for_samv_P(k,i)=for_samv_P(k,i)+k_dl*exp(u)
    enddo
!    if (ANY(ISNAN(for_samv_P(n1rel:n2rel,i)))) then
!      print*,'ERROR + ',i,d0,sigma_intrinsic
!      istop=1
!    endif
    n2=min(dimens(i),-nd0+N1cont)
    bnd_N(2,i)=n2
    if (n2 <= 0) cycle
    n1=max(1,-nd0-N1cont)
    bnd_N(1,i)=n1
    do k=n1,n2
      ka=k
      k_dl=ka*co2
      u=(ka*Delta+d0)*ww
      u=-half*u*u
      for_samv_N(k,i)=for_samv_N(k,i)-k_dl*exp(u)
    enddo
!    if (ANY(ISNAN(for_samv_N(bnd_N(1,i):bnd_N(2,i),i)))) then
!      print*,'ERROR - ',i,d0,sigma_intrinsic
!      istop=1
!    endif
  enddo
!  if (istop==1) stop 'stoop'
endif
end subroutine SAM_ONE_TOALL
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine OUTSAMP(isam,finumb,Bpre)
implicit none
integer(I4B),intent(IN),optional :: isam
real(DP),intent(IN),optional     :: Bpre
integer(I4B)   :: i,iii, iu2,iu,kk,k,lengo,lengdir,isam1,lfinumb1,iume,is1,is2,natw(2)
real(DP)       :: Delta,cfrt,xnatw(2)
character(len=*),intent(IN),optional  :: finumb
character(576) :: sampval_fn,sampinfo_fn

if (present(Bpre)) then
  Bth_presamp=Bpre
  w_presamp=sqrt(three*Bpre*half)/Pi
  sigmaG_presamp=half*w_presamp/sr3
endif

isam1=1
if (present(isam)) then
  isam1=isam
endif
lfinumb1=0
if (present(finumb)) then
  lfinumb1=len_trim(finumb)
endif

do i=Ndelta1,Ndelta2
  Delta=Deltas(i)

  iii=nint(1000.0_DP*Delta)
  
  !__ COMPOSE output file name into variable sampval_fn
  
  sampval_fn=''
  sampval_fn(1:lbasepath_sampled)=basepath_sampled(1:lbasepath_sampled)
  sampval_fn(1+lbasepath_sampled:1+lsampled_folder+lbasepath_sampled)=sampled_folder(1:lsampled_folder)//separator
  lengdir=1+lsampled_folder+lbasepath_sampled
  if (i==Ndelta1) then
    call SYSTEM(trim(mkdir_command)//' '//sampval_fn(1:lengdir)//' > tmp.out 2> tmp.err')
    call SYSTEM(trim(delete_command)//' tmp.out tmp.err')
  endif
  sampval_fn(1+1+lsampled_folder+lbasepath_sampled:6+1+lsampled_folder+lbasepath_sampled)='SAMPTO'
!   sampval_fn(1:lenpath2)=nampath2(1:lenpath2)
  lengo=6+1+lsampled_folder+lbasepath_sampled
  
  write(sampval_fn(1+lengo:5+lengo),'(i3.3,"A'//separator//'")')iii
  call SYSTEM(trim(mkdir_command)//' '//sampval_fn(1:5+lengo)//' > tmp.out 2> tmp.err')
  call SYSTEM(trim(delete_command)//' tmp.out tmp.err')
  sampval_fn(6+lengo:5+lengo+lenfilou)=namfilou(1:lenfilou)
  if (PRESENT(finumb)) then
    sampval_fn(6+lengo+lenfilou:5+lengo+lenfilou+lfinumb1)=finumb(1:lfinumb1)
  endif
  
!   print*,'cklen', len_trim(sampval_fn),4+lenpath2+lenfilou
!   print*,"<"//trim(sampval_fn)//">",len_trim(sampval_fn)

  iu=FIND_UNIT()
  open(iu,status='replace',file=trim(sampval_fn))

  sampinfo_fn=trim(sampval_fn)//'_INFO'

  write(iu,'("#&& ",a)')trim(sampinfo_fn)

  iu2=find_unit()
  open(iu2,status='replace',file=trim(sampinfo_fn))
  iume=0
  cfrt=eps_DP*eps_DP
  do kk=1,n_at_pair_glo
    do k=dimens(i),1,-1
      if (abs(samv(k,kk,i,isam1))>cfrt) then
        iume=max(iume,k)
        exit
      endif
    enddo
  enddo
!  print*,'OUTSAMP : ',iume,maxval(samv),i,dimens(i),size(samv,1),size(samv,2),size(samv,3),size(samv,4)
  do kk=1,n_at_pair_glo
    do k=1,iume
      write(iu,*)samv(k,kk,i,isam1)
    enddo
  enddo
  
  close(iu)
  
!write(iu2,*)dimens(i)
  write(iu2,'(3i12,3(1x,g12.6),1x,g16.10)')iume,n_sp_atom_glo, n_at_pair_glo, a_latt_cioc, rho, Delta, Bth_presamp
  do k=1,n_sp_atom_glo
    write(iu2,'(2i4,i16,2(1x,g24.16))')k,Z_at_glo(k),nat_glo(k),xnat_glo(k),termcon_glo(k)    
  enddo
  do kk=1,n_at_pair_glo
    write(iu2,'(i4,i12,2i4,1x,g24.16)')kk,ndi_glo(kk),zappa_glo(:,kk),summul_glo(kk)
  enddo
!!  if ((.not.allocated(numcells_byphase)) .or. ncelltypes==1) then
    write(iu2,'("Qmax",1x,g24.16)')qtop(i)
!!  else
!!    write(iu2,'("Qmax",1x,g24.16,i4,4(1x,g24.16))')qtop(i)!,ncelltypes,numcells_byphase
!!  endif
  !if (maxval(abs(termcon_ineq_glo))>sceps_DP) 
  write(iu2,*) termcon_all_glo
!___________________ NEW PART
  write(iu2,'(" Nr_Phases      ",i4)') ncelltypes
  write(iu2,'(" N_Grow_Dimensions      ",i4)')num_grow_dir
  write(iu2,*) "Size_Abscissa",Size_Abscissa(1:num_grow_dir)
  write(iu2,*) "Actual_Diameters",Actual_Diam(1:num_grow_dir)
  if (allocated(numcells_byphase)) then
    write(iu2,*) " Nr_Cells_Phases    ",numcells_byphase
  else
    write(iu2,*) " Nr_Cells_Phases      1"
  endif
  write(iu2,*) " Cell__Centers_Vol_Volr ",ncenters_cell,cell_volume, cell_volume_red
  write(iu2,'(" Nr_Atom_Species  ",i4)') n_sp_atom_glo
  do k=1,n_sp_atom_glo
    if (ncelltypes==1) then
      write(iu2,'(2i4,i16,1x,g24.16)')k,Z_at_glo(k),nat_glo(k),xnat_glo(k)
    else if (ncelltypes==2) then
      natw=0; xnatw=zero
      is1=return_address_atsp(k,1)
      is2=return_address_atsp(k,2)
!      print*,'RADD ',k,is1,is2,size(Celty(1)%nat),size(Celty(2)%nat)
      if (is1>0) then
        natw(1)  = Celty(1)%nat(is1)*numcells_byphase(1)
        xnatw(1) = Celty(1)%xnat(is1)*numcells_byphase(1)
      endif
      if (is2>0) then
        natw(2)  =  Celty(2)%nat(is2)*numcells_byphase(2)
        xnatw(2) = Celty(2)%xnat(is2)*numcells_byphase(2)
      endif
      write(iu2,'(2i4,2i16,2(1x,g24.16))')k,Z_at_glo(k),natw,xnatw
    endif
  enddo
  write(iu2,'(6(1x,g14.8),1x,a)')abcabgy,trim(spgroup_IDENT)
  close(iu2)
enddo

end subroutine OUTSAMP
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end module paper_blood
!___________________________________________________________________________________________________
module GEODIS
use nano_deftyp
use paper_blood
  
  type,public :: FlatPat
    real(DP)      :: Zval,Zmul_inv
    integer(I4B)  :: num_XYs
    real(DP),dimension(:,:),allocatable   :: xydist 
    real(DP),dimension(:,:),allocatable   :: xydist_SUB
    real(DP),dimension(:),allocatable   :: scalds 
    real(DP),dimension(:),allocatable   :: patmu 
    real(DP),dimension(:),allocatable   :: patbb 
    integer(I4B),dimension(:),allocatable   :: pat_InterSubuPoint
  END TYPE FlatPat
  type,public :: PatPair
    integer(I4B)  :: Z1Z2(2),I1I2(2),num_Zs
    type(FlatPat),dimension(:),allocatable   :: patbyz
  END TYPE PatPair
  
type(PatPair),allocatable,save  :: ThePatVec(:)
integer(I4B),save  :: Number_Atom_Pairs

  type,public :: LocOcc1Z
    integer(I4B)  :: niad
    real(DP),dimension(:,:),allocatable   :: occ1z 
  END TYPE LocOcc1Z
  type,public :: LocOcc
    integer(I4B)  :: nzv
    TYPE(LocOcc1Z),dimension(:),allocatable   :: occu 
  END TYPE LocOcc
  
  
  type,public :: SQDRESS
    real(DP),allocatable :: occ_LAT(:)
    type(LocOcc),dimension(:),allocatable       :: Dressing 
  end type SQDRESS
  type,public :: Rsquare
    integer(I4B)                            :: Iside(2),Iprog,Iexr(2,2),allod(2,2)
    integer(I4B),DIMENSION(:),allocatable   :: NumLatPoi
    real(DP),DIMENSION(:),allocatable       :: XNumLatPoi
    real(DP),DIMENSION(:,:,:),allocatable       :: Rsqr 
    TYPE(SQDRESS),DIMENSION(:,:),allocatable    :: Dsqr 
  END TYPE Rsquare
  
  type,public :: LayerFam
    character(3)                         :: geometry
    real(DP),DIMENSION(2,2)              :: PlaneMT,PlaneDV
    real(DP)                             :: cell_area,dr0
    integer(I4B)                         :: Nsidemin(2),Nsidemax(2),Nprogmin,Nprogmax
    type(Rsquare),dimension(:),allocatable   :: layers_grid 
  END TYPE LayerFam
  
type(LayerFam),save  :: TheLayers
real(DP), allocatable,save     :: patte(:,:,:,:)
integer(I4B),allocatable,save  :: nbypair(:,:),numpatv(:,:)
logical,save :: merge_do=.false.  
  
INTERFACE metric_dotprod2D
   MODULE PROCEDURE metric_dotprod2D_R, metric_dotprod2D_I
 END INTERFACE metric_dotprod2D
  
!_______________ NEW / 02.03.2011
integer(I4B),save     :: Number_Stack_Planes = 1
real(DP),save         :: TrStack(2) = zero, TrStackA(2)
!integer(I4B),save     :: Number_Stack_Planes = 3
!real(DP),save         :: TrStack(2) = [unter, duter] ! hexagonal
real(DP),parameter    :: TrStack_HCPFCC(2) = [unter, duter] ! hexagonal
real(DP),save         :: abcabg_loc(6)
 
  
contains

!********************************************************************
function metric_dotprod2D_R(mt22,vec1,vec2)
implicit none
real(DP),intent(IN) :: mt22(2,2)
real(DP),intent(IN) :: vec1(2)
real(DP),optional,intent(IN) :: vec2(2)
real(DP) :: metric_dotprod2D_R
real(DP) :: auxer(2)
logical :: offdiag

offdiag=(abs(mt22(1,2))>sceps_DP)
auxer=[mt22(1,1),mt22(2,2)]*vec1
if (offdiag) auxer=auxer+mt22(1,2)*[vec1(2),vec1(1)]
if (present(vec2)) then
  metric_dotprod2D_R=sum(vec2*auxer)
else
  metric_dotprod2D_R=sum(vec1*auxer)
endif

end function metric_dotprod2D_R
!********************************************************************
function metric_dotprod2D_I(mt22,vec1,vec2)
implicit none
real(DP),intent(IN) :: mt22(2,2)
integer(I4B),intent(IN) :: vec1(2)
integer(I4B),optional,intent(IN) :: vec2(2)
real(DP) :: metric_dotprod2D_I
real(DP) :: auxer(2)
logical :: offdiag

offdiag=(abs(mt22(1,2))>sceps_DP)
auxer=[mt22(1,1),mt22(2,2)]*vec1
if (offdiag) auxer=auxer+mt22(1,2)*[vec1(2),vec1(1)]
if (present(vec2)) then
  metric_dotprod2D_I=sum(vec2*auxer)
else
  metric_dotprod2D_I=sum(vec1*auxer)
endif

end function metric_dotprod2D_I
!********************************************************************
subroutine Patterson_Sorter()
implicit none
integer(I4B) :: ipai, i1,jz,kz,kxy,numxy, KKK,Iflag,Ndymm1,Ndym,I,Ip1,ixc
integer(I4B),allocatable :: wkv(:)
real(DP),allocatable :: zetacvals(:)
integer(I4B) :: ugoxx(2),nzetacvals,isnewz,i2z,kzv(1)
real(DP) :: zold,zval,zmul,xc2(2),xxx
logical  :: toxc




Number_Atom_Pairs = n_at_pair_V(1)
if (verbose) print*,'Patterson sorter: n_at_pair_V(1) = ',n_at_pair_V(1)
if (allocated(ThePatVec)) deallocate(ThePatVec)
allocate(ThePatVec(n_at_pair_V(1)))
if (.not.ALLOCATED(Celty(1)%zappa)) stop 'zappa unallocated'
do ipai=1,n_at_pair_V(1)
  ugoxx = Celty(1)%zappa(1:2,ipai)
  ThePatVec(ipai)%I1I2 = ugoxx
  ThePatVec(ipai)%Z1Z2 = [Celty(1)%Z_at(ugoxx(1)),Celty(1)%Z_at(ugoxx(2))]
  if (allocated(wkv)) deallocate(wkv)
  allocate(wkv(numpatv(ipai,1)))
  if (allocated(zetacvals)) deallocate(zetacvals)
  allocate(zetacvals(numpatv(ipai,1)))
!  zetacvals=patte(3,1:numpatv(ipai,1),ipai,1)
  nzetacvals=0
  zetacvals=zero
  do i1=1,numpatv(ipai,1)
    zval=patte(3,i1,ipai,1)
    isnewz=1
    do i2z=1,nzetacvals
      if (ABS(zval-zetacvals(i2z))<sceps_DP) then
        isnewz=0
        exit
      endif
    enddo
    if (isnewz==1) then
      nzetacvals=nzetacvals+1
      zetacvals(nzetacvals)=zval
    endif
  enddo
  KKK=nzetacvals
  Iflag=1
  DO
    IF (Iflag == 0) exit
    Ndym=KKK
    Iflag=0
    Ndymm1=Ndym-1
    DO I=1,Ndymm1
      Ip1=I+1
      IF (zetacvals(I)>zetacvals(Ip1)+sceps_DP) THEN
        zval=zetacvals(I)
        zetacvals(I)=zetacvals(Ip1)
        zetacvals(Ip1)=zval
        KKK=I
        Iflag=1
      ENDIF
    ENDDO
  ENDDO
  !finqi
  if (verbose) print'(a,i6,2(4x,2i3),i8)','Patterson sorter: pair, n.vec. = ',&
       ipai,ThePatVec(ipai)%I1I2,ThePatVec(ipai)%Z1Z2,numpatv(ipai,1)
  do i1=1,numpatv(ipai,1)
    zval=patte(3,i1,ipai,1)
    kzv=minloc(abs(zval-zetacvals(1:nzetacvals)))
    wkv(i1)=kzv(1)
  enddo
  kz=nzetacvals
  ThePatVec(ipai)%num_Zs = kz
  allocate(ThePatVec(ipai)%patbyz(kz))
  do jz=1,kz
    numxy=count(wkv==jz)
    zval=sum(patte(3,1:numpatv(ipai,1),ipai,1),MASK=(wkv==jz))/numxy
    zmul=two
    if (abs(zval)<sceps_DP) then
      zval=zero
      zmul=one
    endif
    if (abs(zval-half)<sceps_DP) zval=half
    if (abs(zval-one)<sceps_DP) zval=one
    
    ThePatVec(ipai)%patbyz(jz)%Zval     = zval
    ThePatVec(ipai)%patbyz(jz)%Zmul_inv = zmul
    ThePatVec(ipai)%patbyz(jz)%num_XYs  = numxy
    allocate(ThePatVec(ipai)%patbyz(jz)%xydist(2,numxy), &
             ThePatVec(ipai)%patbyz(jz)%xydist_SUB(2,numxy), &
             ThePatVec(ipai)%patbyz(jz)%scalds(numxy), &
             ThePatVec(ipai)%patbyz(jz)%patmu(numxy), &
             ThePatVec(ipai)%patbyz(jz)%patbb(numxy)) !, &
!             ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(numxy))
    kxy=0
    do i1=1,numpatv(ipai,1)
      if (wkv(i1)<jz) cycle
      if (wkv(i1)>jz) exit
      kxy=kxy+1
      ThePatVec(ipai)%patbyz(jz)%xydist(:,kxy) = patte(1:2,i1,ipai,1)
      ThePatVec(ipai)%patbyz(jz)%xydist_SUB(:,kxy) = patte(6:7,i1,ipai,1)
      ThePatVec(ipai)%patbyz(jz)%scalds(kxy) = metric_dotprod2D(mt22=PlaneMT,vec1=patte(1:2,i1,ipai,1))
      !sum(patte(1:2,i1,ipai,1)**2)
      ThePatVec(ipai)%patbyz(jz)%patmu(kxy) = patte(4,i1,ipai,1)
      ThePatVec(ipai)%patbyz(jz)%patbb(kxy) = patte(5,i1,ipai,1)
!!      ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(kxy) = nint(patte(6,i1,ipai,1))
    enddo
    
    !sort by xy-dist. in plane
    KKK=kxy
    Iflag=1
    DO
      IF (Iflag == 0) exit
      Ndym=KKK
      Iflag=0
      Ndymm1=Ndym-1
      DO I=1,Ndymm1
        Ip1=I+1
        toxc = (ThePatVec(ipai)%patbyz(jz)%scalds(I)>ThePatVec(ipai)%patbyz(jz)%scalds(Ip1))
        IF (toxc) THEN
          xc2=ThePatVec(ipai)%patbyz(jz)%xydist(:,I)
          ThePatVec(ipai)%patbyz(jz)%xydist(:,I)=ThePatVec(ipai)%patbyz(jz)%xydist(:,Ip1)
          ThePatVec(ipai)%patbyz(jz)%xydist(:,Ip1)=xc2
          xc2=ThePatVec(ipai)%patbyz(jz)%xydist_SUB(:,I)
          ThePatVec(ipai)%patbyz(jz)%xydist_SUB(:,I)=ThePatVec(ipai)%patbyz(jz)%xydist_SUB(:,Ip1)
          ThePatVec(ipai)%patbyz(jz)%xydist_SUB(:,Ip1)=xc2
          xxx=ThePatVec(ipai)%patbyz(jz)%scalds(I)
          ThePatVec(ipai)%patbyz(jz)%scalds(I)=ThePatVec(ipai)%patbyz(jz)%scalds(Ip1)
          ThePatVec(ipai)%patbyz(jz)%scalds(Ip1)=xxx
          xxx=ThePatVec(ipai)%patbyz(jz)%patmu(I)
          ThePatVec(ipai)%patbyz(jz)%patmu(I)=ThePatVec(ipai)%patbyz(jz)%patmu(Ip1)
          ThePatVec(ipai)%patbyz(jz)%patmu(Ip1)=xxx
          xxx=ThePatVec(ipai)%patbyz(jz)%patbb(I)
          ThePatVec(ipai)%patbyz(jz)%patbb(I)=ThePatVec(ipai)%patbyz(jz)%patbb(Ip1)
          ThePatVec(ipai)%patbyz(jz)%patbb(Ip1)=xxx
!          ixc=ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(I)
!          ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(I)=ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(Ip1)
!          ThePatVec(ipai)%patbyz(jz)%pat_InterSubuPoint(Ip1)=ixc
          KKK=I
          Iflag=1
        ENDIF
      ENDDO
    ENDDO
    
  enddo
enddo

if (allocated(wkv)) deallocate(wkv)
if (allocated(zetacvals)) deallocate(zetacvals)
end subroutine Patterson_Sorter
!********************************************************************
subroutine FILLSQUARE(Nbeg,Nend,geo,mt22,dv22,smoowid1)
implicit none
integer(I4B),intent(IN)            :: Nbeg,Nend
character(3),intent(IN)            :: geo
real(DP),DIMENSION(2,2),intent(IN) :: mt22,dv22
real(DP),optional,intent(IN)       :: smoowid1
integer(I4B)  :: i1,i1m,i2,j1,j2,k1,k2,mab(2),jjf,aui(2),iif,iig,iplanes,itr3(2),nbord,nbor2,ipl1,ipl2,d_ipl, &
                 ix1,iy1,ixu,ixl,iyu,iyl, hwbro
real(DP) :: sq_nth_radius,dddd,cccc(2),mmmm(2),oo1,oo2,xplan,gg,smoowid
real(DP),allocatable :: ebro(:)

smoowid=zero
if (PRESENT(smoowid1)) then
  smoowid=smoowid1
endif
iplanes=1
itr3=NINT(TrStack*Number_Stack_Planes)
nbord=0
if (Number_Stack_Planes>1) nbord=1
if (smoowid>sceps_DP) then
  hwbro=ceiling(2.326347874040841d0*smoowid)
  nbord=nbord+hwbro
  allocate(ebro(-hwbro:hwbro))
  gg = srhalf/smoowid
  ebro=half*(one-ERF([(gg*(i1+half),i1=-hwbro,hwbro)]))
endif
nbor2=2*nbord

TheLayers%geometry = geo
TheLayers%PlaneMT  = mt22
TheLayers%PlaneDV  = dv22
TheLayers%cell_area= dv22(1,1)*dv22(2,2)-dv22(1,2)*dv22(2,1)
TheLayers%dr0      = sqrt(TheLayers%cell_area/pi)
TheLayers%Nprogmin = Nbeg
TheLayers%Nprogmax = Nend

ALLOCATE(TheLayers%layers_grid(Nbeg:Nend))



if (geo=='PAR') then
  TheLayers%Nsidemin = Nbeg
  TheLayers%Nsidemax = Nend
  do i1=Nbeg,Nend
    TheLayers%layers_grid(i1)%Iside=i1
    TheLayers%layers_grid(i1)%Iprog=i1
    TheLayers%layers_grid(i1)%Iexr(:,1) =[1-nbord,i1+nbord]
    TheLayers%layers_grid(i1)%Iexr(:,2) =[1-nbord,i1+nbord]
    allocate(TheLayers%layers_grid(i1)%NumLatPoi(Number_Stack_Planes),TheLayers%layers_grid(i1)%XNumLatPoi(Number_Stack_Planes))
    ALLOCATE(TheLayers%layers_grid(i1)%Rsqr(TheLayers%layers_grid(i1)%Iexr(1,1):TheLayers%layers_grid(i1)%Iexr(2,1),&
                                            TheLayers%layers_grid(i1)%Iexr(1,2):TheLayers%layers_grid(i1)%Iexr(2,2),&
                                            Number_Stack_Planes))

    TheLayers%layers_grid(i1)%Rsqr = one
 !   if (Number_Stack_Planes > 1) then
      ixu=(TheLayers%layers_grid(i1)%Iexr(2,1)-nbord)*Number_Stack_Planes
      ixl=(TheLayers%layers_grid(i1)%Iexr(1,1)+nbord)*Number_Stack_Planes
      iyu=(TheLayers%layers_grid(i1)%Iexr(2,2)-nbord)*Number_Stack_Planes
      iyl=(TheLayers%layers_grid(i1)%Iexr(1,2)+nbord)*Number_Stack_Planes
      do iplanes=1,Number_Stack_Planes
        do j2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
          iy1=itr3(2)*(iplanes-1)+Number_Stack_Planes*j2
          do j1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
            ix1=itr3(1)*(iplanes-1)+Number_Stack_Planes*j1
            if ( (ix1>ixu .or. ix1 <ixl) .or. (iy1>iyu .or. iy1<iyl) ) then
              TheLayers%layers_grid(i1)%Rsqr(j1,j2,iplanes) = zero
            endif
          enddo
        enddo
        TheLayers%layers_grid(i1)%NumLatPoi(iplanes) = COUNT(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP)
        TheLayers%layers_grid(i1)%XNumLatPoi(iplanes) = SUM(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes), &
                                                          MASK=(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP))
      enddo
!    endif


    TheLayers%layers_grid(i1)%allod(:,1)=[-i1-nbor2+1,i1+nbor2-1]
    TheLayers%layers_grid(i1)%allod(:,2)=[-i1-nbor2+1,i1+nbor2-1]
    ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(TheLayers%layers_grid(i1)%allod(1,1):TheLayers%layers_grid(i1)%allod(2,1),&
                                            TheLayers%layers_grid(i1)%allod(1,2):TheLayers%layers_grid(i1)%allod(2,2)))

    do iif=TheLayers%layers_grid(i1)%allod(1,1), TheLayers%layers_grid(i1)%allod(2,1)
      do iig=TheLayers%layers_grid(i1)%allod(1,2), TheLayers%layers_grid(i1)%allod(2,2)
        ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT(-Number_Stack_Planes+1:Number_Stack_Planes-1))
        TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT=zero
      enddo
    enddo

    if (Number_Stack_Planes == 1) then
      do j2=TheLayers%layers_grid(i1)%allod(1,2),TheLayers%layers_grid(i1)%allod(2,2)
        jjf=i1-abs(j2)
        do j1=TheLayers%layers_grid(i1)%allod(1,1),TheLayers%layers_grid(i1)%allod(2,1)
          TheLayers%layers_grid(i1)%Dsqr(j1,j2)%occ_LAT(0)=real(jjf*(i1-abs(j1)),DP)
        enddo
      enddo
    else if (Number_Stack_Planes > 1) then
      do ipl1=0,Number_Stack_Planes-1;do ipl2=0,Number_Stack_Planes-1
        d_ipl=ipl1-ipl2
        do j1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
          do j2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
            oo1=TheLayers%layers_grid(i1)%Rsqr(j1,j2,ipl1+1)
            if (oo1<sceps_DP) CYCLE
            do k1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
              do k2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
                oo2=TheLayers%layers_grid(i1)%Rsqr(k1,k2,ipl2+1)
                if (oo2<sceps_DP) CYCLE
                oo2=oo2*oo1
                aui=[k1-j1,k2-j2]
                TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) = &
                    TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) + oo2
              enddo
            enddo
            !think - do it better?
          enddo
        enddo
      enddo;enddo
    endif
  enddo
else if (geo=='HEX') then
  TheLayers%Nsidemin = 2*Nbeg-1
  TheLayers%Nsidemax = 2*Nend-1
  do i1=Nbeg,Nend
    i2=2*i1-1
    i1m=i1-1
    TheLayers%layers_grid(i1)%Iside=i2
    TheLayers%layers_grid(i1)%Iprog=i1
    TheLayers%layers_grid(i1)%Iexr(:,1) =[-i1m-nbord,i1m+nbord]
    TheLayers%layers_grid(i1)%Iexr(:,2) =[-i1m-nbord,i1m+nbord]
    allocate(TheLayers%layers_grid(i1)%NumLatPoi(Number_Stack_Planes),TheLayers%layers_grid(i1)%XNumLatPoi(Number_Stack_Planes))
    ALLOCATE(TheLayers%layers_grid(i1)%Rsqr(TheLayers%layers_grid(i1)%Iexr(1,1):TheLayers%layers_grid(i1)%Iexr(2,1),&
                                            TheLayers%layers_grid(i1)%Iexr(1,2):TheLayers%layers_grid(i1)%Iexr(2,2),&
                                            Number_Stack_Planes))
    TheLayers%layers_grid(i1)%Rsqr = one
    
    ixu=(TheLayers%layers_grid(i1)%Iexr(2,1)-nbord)*Number_Stack_Planes
    ixl=(TheLayers%layers_grid(i1)%Iexr(1,1)+nbord)*Number_Stack_Planes
    iyu=(TheLayers%layers_grid(i1)%Iexr(2,2)-nbord)*Number_Stack_Planes
    iyl=(TheLayers%layers_grid(i1)%Iexr(1,2)+nbord)*Number_Stack_Planes
    do iplanes=1,Number_Stack_Planes
      do j2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
        iy1=itr3(2)*(iplanes-1)+Number_Stack_Planes*j2
        do j1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
          ix1=itr3(1)*(iplanes-1)+Number_Stack_Planes*j1
          if ( ( (ix1>ixu .or. ix1 <ixl) .or. (iy1>iyu .or. iy1<iyl) ) .or. &
                (abs(ix1+iy1) > ixu) ) then
            TheLayers%layers_grid(i1)%Rsqr(j1,j2,iplanes)=zero
          endif
        enddo
      enddo
      TheLayers%layers_grid(i1)%NumLatPoi(iplanes) = COUNT(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP)
      TheLayers%layers_grid(i1)%XNumLatPoi(iplanes) = SUM(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes), &
                                                        MASK=(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP))
    enddo    

    TheLayers%layers_grid(i1)%allod(:,1)=[-2*i1m-nbor2,2*i1m+nbor2]
    TheLayers%layers_grid(i1)%allod(:,2)=[-2*i1m-nbor2,2*i1m+nbor2]
    ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(TheLayers%layers_grid(i1)%allod(1,1):TheLayers%layers_grid(i1)%allod(2,1), &
                                            TheLayers%layers_grid(i1)%allod(1,2):TheLayers%layers_grid(i1)%allod(2,2)))
    do iif=TheLayers%layers_grid(i1)%allod(1,1), TheLayers%layers_grid(i1)%allod(2,1)
      do iig= TheLayers%layers_grid(i1)%allod(1,2), TheLayers%layers_grid(i1)%allod(2,2)
        ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT(-Number_Stack_Planes+1:Number_Stack_Planes-1))
        TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT=zero
      enddo
    enddo

    do ipl1=0,Number_Stack_Planes-1;do ipl2=0,Number_Stack_Planes-1
      d_ipl=ipl1-ipl2
      do j1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
        do j2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
          oo1=TheLayers%layers_grid(i1)%Rsqr(j1,j2,ipl1+1)
          if (oo1<sceps_DP) CYCLE
          do k1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
            do k2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
              oo2=TheLayers%layers_grid(i1)%Rsqr(k1,k2,ipl2+1)
              if (oo2<sceps_DP) CYCLE
              oo2=oo2*oo1
              aui=[k1-j1,k2-j2]
              TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) = &
                TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) + oo2
            enddo
          enddo
        enddo
      enddo
    enddo;enddo
  enddo
else if (geo=='CYL') then
  cccc= one/(abcabg_loc(1:2)*sin(degrees_to_radians*(abcabg_loc(6))))
  TheLayers%Nsidemin = huge(1_I4B)
  TheLayers%Nsidemax = -1
  do i1=Nbeg,Nend
    sq_nth_radius=i1*TheLayers%dr0
    mab=ceiling(cccc*sq_nth_radius)
    sq_nth_radius=sq_nth_radius**2
    TheLayers%layers_grid(i1)%Iside=2*mab+1
    TheLayers%Nsidemax=2*mab+1
    if (i1==Nbeg) TheLayers%Nsidemin=2*mab+1
    TheLayers%layers_grid(i1)%Iprog=i1
    TheLayers%layers_grid(i1)%Iexr(:,1) =[-mab(1)-nbord,mab(1)+nbord]
    TheLayers%layers_grid(i1)%Iexr(:,2) =[-mab(2)-nbord,mab(2)+nbord]
    
    allocate(TheLayers%layers_grid(i1)%NumLatPoi(Number_Stack_Planes),TheLayers%layers_grid(i1)%XNumLatPoi(Number_Stack_Planes))
    ALLOCATE(TheLayers%layers_grid(i1)%Rsqr(TheLayers%layers_grid(i1)%Iexr(1,1):TheLayers%layers_grid(i1)%Iexr(2,1),&
                                            TheLayers%layers_grid(i1)%Iexr(1,2):TheLayers%layers_grid(i1)%Iexr(2,2),&
                                            Number_Stack_Planes))
    TheLayers%layers_grid(i1)%Rsqr = one
    
    
    do iplanes=1,Number_Stack_Planes
      xplan = real(iplanes - 1,DP)
      do j1=TheLayers%layers_grid(i1)%Iexr(1,1), TheLayers%layers_grid(i1)%Iexr(2,1)
        do j2=TheLayers%layers_grid(i1)%Iexr(1,2), TheLayers%layers_grid(i1)%Iexr(2,2)
          mmmm=real([j1,j2],DP)
          if (iplanes>1) mmmm=mmmm+TrStack*xplan
          dddd = mmmm(1)*(mt22(1,1)*mmmm(1) + two*mt22(1,2))+mt22(2,2)*mmmm(2)*mmmm(2) !sum(mmmm*matmul(mt22,mmmm))
          if (dddd >= sq_nth_radius+sceps_DP) then
            TheLayers%layers_grid(i1)%Rsqr(j1,j2,iplanes)=zero
          else if (abs(dddd-sq_nth_radius)<sceps_DP) then
            TheLayers%layers_grid(i1)%Rsqr(j1,j2,iplanes)=half
          endif
        enddo
      enddo
      TheLayers%layers_grid(i1)%NumLatPoi(iplanes) = COUNT(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP)
      TheLayers%layers_grid(i1)%XNumLatPoi(iplanes) = SUM(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes), &
                                                        MASK=(TheLayers%layers_grid(i1)%Rsqr(:,:,iplanes) > sceps_DP))
    enddo
    !______ CTRL
!    print*,geo,' # ',i1,': N. lattice nodes ',TheLayers%layers_grid(i1)%NumLatPoi(:),TheLayers%layers_grid(i1)%XNumLatPoi(:),&
!           'Expected : ',pi*sq_nth_radius/TheLayers%cell_area
           
    TheLayers%layers_grid(i1)%allod(:,1)=[-2*mab(1)-nbor2,2*mab(1)+nbor2]
    TheLayers%layers_grid(i1)%allod(:,2)=[-2*mab(2)-nbor2,2*mab(2)+nbor2]
    ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(TheLayers%layers_grid(i1)%allod(1,1):TheLayers%layers_grid(i1)%allod(2,1),&
                                            TheLayers%layers_grid(i1)%allod(1,2):TheLayers%layers_grid(i1)%allod(2,2)))
    do iif=TheLayers%layers_grid(i1)%allod(1,1), TheLayers%layers_grid(i1)%allod(2,1)
      do iig=TheLayers%layers_grid(i1)%allod(1,2), TheLayers%layers_grid(i1)%allod(2,2)
        ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT(-Number_Stack_Planes+1:Number_Stack_Planes-1))
        TheLayers%layers_grid(i1)%Dsqr(iif,iig)%occ_LAT=zero
      enddo
    enddo
    
    do ipl1=0,Number_Stack_Planes-1;do ipl2=0,Number_Stack_Planes-1
      d_ipl=ipl1-ipl2
      do j1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
        do j2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
          oo1=TheLayers%layers_grid(i1)%Rsqr(j1,j2,ipl1+1)
          if (oo1<sceps_DP) CYCLE
          do k1=TheLayers%layers_grid(i1)%Iexr(1,1),TheLayers%layers_grid(i1)%Iexr(2,1)
            do k2=TheLayers%layers_grid(i1)%Iexr(1,2),TheLayers%layers_grid(i1)%Iexr(2,2)
              oo2=TheLayers%layers_grid(i1)%Rsqr(k1,k2,ipl2+1)
              if (oo2<sceps_DP) CYCLE
              oo2=oo2*oo1
              aui=[k1-j1,k2-j2]
              TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) = &
                TheLayers%layers_grid(i1)%Dsqr(aui(1),aui(2))%occ_LAT(d_ipl) + oo2
            enddo
          enddo
        enddo
      enddo
    enddo;enddo
  enddo
  
endif

if (allocated(ebro)) deallocate(ebro)

end subroutine FILLSQUARE
!********************************************************************
subroutine DRESSLAYERS()
implicit none
integer(I4B)  :: Nbeg,Nend,Nzv,Niad
integer(I4B)  :: i1,jd1,jd2,jp,jz,jaxy,jp2,jz2,jaxy2,allost,ll0,ll1,mm0,mm1,&
                 inn1,inn2,dinn1,dinn2,nd7,kk1,kk2,japl,japl2
character(3)  :: geo
real(DP) :: aux(2),aux2(2),ooo,ooo2

Nbeg = TheLayers%Nprogmin
Nend = TheLayers%Nprogmax
! shift occ1z to within Dsqr!!!

nd7=Number_Stack_Planes-1

do i1=Nbeg,Nend
  do jd1=TheLayers%layers_grid(i1)%allod(1,1),TheLayers%layers_grid(i1)%allod(2,1)
    do jd2=TheLayers%layers_grid(i1)%allod(1,2),TheLayers%layers_grid(i1)%allod(2,2)
      allocate(TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(Number_Atom_Pairs))
      do jp=1,Number_Atom_Pairs
        Nzv=ThePatVec(jp)%num_Zs
        TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(jp)%Nzv = Nzv
        ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(jp)%occu(Nzv))
        do jz=1,Nzv
          Niad = ThePatVec(jp)%patbyz(jz)%num_XYs
          TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(jp)%occu(jz)%niad = Niad
          ALLOCATE(TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(jp)%occu(jz)%occ1z(Niad,-nd7:nd7),stat=allost)
          if (allost/=0) stop 'Not able to allocate DRESSED LAYERS'
          !
          do kk2=-nd7,nd7
            TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%Dressing(jp)%occu(jz)%occ1z(:,kk2) = &
                                                                ThePatVec(jp)%patbyz(jz)%patmu(1:Niad) &
                                                              * TheLayers%layers_grid(i1)%Dsqr(jd1,jd2)%occ_LAT(kk2)
          enddo
        enddo
      enddo
    enddo
  enddo
enddo


end subroutine DRESSLAYERS

end module GEODIS
!___________________________________________________________________________________________________
!___________________________________________________________________________________________________
Module SAMPLING_2DIME
use paper_blood
use nano_deftyp
use GEODIS
use specfun_AC

logical,save :: scalable_subu=.false.
real(DP), save :: c_ref
real(DP), allocatable, save :: wplanes(:)
integer(I4B),save :: num_Kind_Planes_SF = 1, NumPairPlanes_EqNEq = 1


  real(DP),dimension(:),pointer,save :: poin2sam => NULL()
  real(DP),dimension(:),pointer,save :: poin2sam2 => NULL()
  
!____ a simple 1-D vector of sampled values ...
  type,public :: SimpleVec
    integer(I4B)  :: Sdim
    real(DP),dimension(:),pointer   :: revec => NULL()
  END TYPE SimpleVec
  
!____ texture-related dimensions
  type,public :: Multiple_SBF_Sam_L0
  !  Store the sampled flat-plane distances 
    integer(I4B)  :: Mdim,Mdim1
    TYPE(SimpleVec),dimension(:,:),allocatable   :: val_byM
  END TYPE Multiple_SBF_Sam_L0

  type,public :: Multiple_SBF_Sam !___ bla bla
  !  Store the sampled flat-plane distances 
    integer(I4B)  :: Lmax
    TYPE(Multiple_SBF_Sam_L0),dimension(:),allocatable   :: val_byL
  END TYPE Multiple_SBF_Sam

  type,public :: Samv_Delta ! higher-order Spherical Bessel Functions
  !  Store the sampled flat-plane distances 
  !  SBF = Spherical Bessel Functions
    integer(I4B)  :: num_S
    TYPE(Multiple_SBF_Sam),dimension(:),allocatable   :: val_SBF
  END TYPE Samv_Delta
!____ end texture-related dimensions

!__________________________ for storing different z-levels in a plane (same pair, ...)
  type,public :: Samv_Thick_Plane
  !  Store the sampled flat-plane distances 
    integer(I4B)  :: num_ZV
    real(DP),dimension(:),allocatable         :: Z_values
    TYPE(Samv_Delta),dimension(:),allocatable :: PSD_Z
  END TYPE Samv_Thick_Plane
  
!__________________________ for storing different pairs of atoms
  type,public :: Samv_AllPairs
  !  Store the sampled flat-plane distances 
    integer(I4B)  :: num_Pairs
!    integer(I4B)  :: num_Pairs, num_Atom_Species
!    integer(I4B),dimension(:,:),allocatable  :: Z_Pairs,Ind_Pairs
!    integer(I4B),dimension(:),allocatable    :: List_Atom_Species_Z
    TYPE(Samv_Thick_Plane),dimension(:),allocatable :: OPlane
  END TYPE Samv_AllPairs
  
!__________________________ for storing different kinds of planes (e.g. A, B, C in fcc/hcp)
  type,public :: Plane_Kinds
  !  Store the sampled flat-plane distances 
    TYPE(Samv_AllPairs),dimension(:),allocatable :: ABCPlanes
  END TYPE Plane_Kinds

!__________________________ for storing planes of different diameter (ab)
  type,public :: Plane_Sizes
  ! Plane cuts from N1_ab to N2_ab
    real(DP)      :: c_reference  ! c-spacing of each plane, not of a set of N planes
    integer(I4B)  :: num_Kind_Planes
    character(LEN=1),dimension(:),allocatable  :: Plane_Flag
    integer(I4B)  :: n1_AB,n2_AB !___ Min, Max; min=1, max=NCL2MK(1)
    TYPE(Plane_Kinds),dimension(:),allocatable :: AB_Cuts
  END TYPE Plane_Sizes
  
  !___________________
  
  TYPE(Plane_Sizes),save :: This_NC 
  
contains
!******************************************************************
SUBROUTINE ALLOPLAN(n1n2_AB_size, c_reference, Texture_LMax,Texture_SamSym,Texture_CrySym, weread)
implicit none
integer(I4B),dimension(2),intent(IN) :: n1n2_AB_size
real(DP),intent(IN)  :: c_reference
integer(I4B),intent(IN) :: Texture_LMax
character(len=3),intent(IN) :: Texture_SamSym,Texture_CrySym
logical,intent(IN),optional :: weread
!>>> LOCAL
integer(I4B)  :: jab,jene,jpai,nzv,jz,nSBF,jSBF,nL2do,jL2do,diag_I(2),nsap
real(DP) :: diam_max
logical :: doweread

doweread=.false.
if (PRESENT(weread)) then
  doweread = weread
endif

c_ref = c_reference
This_NC%n1_AB  = n1n2_AB_size(1)
This_NC%n2_AB  = n1n2_AB_size(2)
This_NC%c_reference  = c_reference
This_NC%num_Kind_Planes  = num_Kind_Planes_SF
ALLOCATE( This_NC%Plane_Flag(num_Kind_Planes_SF) )
if (num_Kind_Planes_SF==1) then
  This_NC%Plane_Flag = ['A']
  NumPairPlanes_EqNEq = 1
else if (num_Kind_Planes_SF==2) then
  This_NC%Plane_Flag = ['A','B']
  TrStack = zero
  NumPairPlanes_EqNEq = 2
else if (num_Kind_Planes_SF==3) then
  This_NC%Plane_Flag = ['A','B','C']
  TrStack = TrStack_HCPFCC
  NumPairPlanes_EqNEq = 2
endif
allocate(wplanes(NumPairPlanes_EqNEq))
if (NumPairPlanes_EqNEq==1) then
  wplanes=one
else if (NumPairPlanes_EqNEq==2) then
  wplanes=[one/num_Kind_Planes_SF, one/(num_Kind_Planes_SF*(num_Kind_Planes_SF-1))]
endif

ALLOCATE( This_NC%AB_Cuts( This_NC%n1_AB : This_NC%n2_AB ) )
do jab=This_NC%n1_AB, This_NC%n2_AB
  if (.not.doweread) then
    Diag_I = TheLayers%layers_grid(jab)%Iexr(2,:)-TheLayers%layers_grid(jab)%Iexr(1,:) !*
    diam_max = SQRT(metric_dotprod2D(mt22=planeMT, vec1=Diag_I))                       !*
  endif
  ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes( NumPairPlanes_EqNEq ))
  do jene=1,NumPairPlanes_EqNEq
    This_NC%AB_Cuts(jab)%ABCPlanes%Num_Pairs = n_at_pair_V(1)
    ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane( n_at_pair_V(1) ))
    do jpai = 1,n_at_pair_V(1)
      nzv=ThePatVec(jpai)%num_Zs
      This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%num_ZV = nzv
      ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(nzv), &
                This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%Z_values(nzv) )
      This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%Z_values = [(ThePatVec(jpai)%patbyz(jz)%Zval, jz=1,nzv)]
      if (Texture_LMax==0) then
        do jz=1,nzv
          nSBF=1
          This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%num_S=nSBF
          ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(nSBF) )
          do jSBF=1,nSBF
            This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%Lmax = Texture_LMax
            nL2do=1
            ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%val_byL(nL2do) )
            do jL2do=1,nL2do
              This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%val_byL(jL2do)%Mdim = 1
              This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%val_byL(jL2do)%Mdim1 = 1              
              ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%val_byL(jL2do)%val_byM(1,1) )
              if (.not.doweread) then
                nsap = CEILING(diam_max / Deltas(Ndelta1)) + Ndelta + N1cont
                This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz)%val_SBF(jSBF)%val_byL(jL2do)%val_byM(1,1)%Sdim = nsap
                
                ALLOCATE( This_NC%AB_Cuts(jab)%ABCPlanes(jene)%OPlane(jpai)%PSD_Z(jz &
                          )%val_SBF(jSBF)%val_byL(jL2do)%val_byM(1,1)%revec(0:nsap) )
              endif
            enddo
          enddo
        enddo
      else
        print*,'Texture not yet in'
        stop 'Texture not yet in'
      endif
    enddo
  enddo
enddo

end SUBROUTINE ALLOPLAN  
!******************************************************************
subroutine SAM_ONE_PLANAR(radial_d0,mu0,v2sumto,nv2sumto)
implicit none
real(DP),intent(IN)    :: radial_d0,mu0
integer(I4B),intent(IN)    :: nv2sumto
real(DP),intent(INOUT) :: v2sumto(0:nv2sumto)
integer(I4B) :: i,k,nradial_d0,n1,n2
real(DP) :: u,ww,lmu,amu,smu,ww2l,yy1,yy

 amu=abs(mu0)
 if (amu<sceps_DP) return
 smu=sign(one,mu0)
 lmu=log(amu)
 nbeta1=1+ceiling(beta*rho*sqrt(one+lmu*lepsii))
 i=Ndelta1
 Delta = Deltas(i)
 nradial_d0=nint(radial_d0/Delta)
 n1=max(0,nradial_d0-nbeta1)
 n2=min(nv2sumto,nradial_d0+nbeta1)
 ww=one/(Delta*rho)
 ww2l=lmu!-two*log(Delta)
 yy1=radial_d0*ww/rho
 do k=n1,n2
   u=(k*Delta-radial_d0)*ww
   u = -half*u*u + ww2l
   yy=yy1*real(k,DP)
   v2sumto(k)=v2sumto(k)+smu*exp(u)*Spec_BesselI0_Expmx(yy)
 enddo

end subroutine SAM_ONE_PLANAR
!******************************************************************
!******************************************************************
!subroutine RESAM_PLANARto3D(z0,mu_z,v2sum3D,nv2sum3D,v_2d,nv_2d)
!implicit none
!real(DP),intent(IN)    :: z0,mu_z
!integer(I4B),intent(IN)    :: nv2sum3D
!real(DP),intent(INOUT) :: v2sum3D(nv2sum3D)
!integer(I4B),intent(IN)    :: nv_2D
!real(DP),intent(IN) :: v_2D(0:nv_2D)
!integer(I4B) :: i,k,nz0,n1,n2,m,m2,k2
!real(DP) :: u,ww,lmu,amu,smu,ww2l,yy1,yy
!real(DP) :: sqrtpl,sqrtmn,con,cc
!
! amu=abs(mu_z)
! if (amu<sceps_DP) return
! smu=sign(one,mu_z)
! lmu=log(amu)
! nbeta1=1+ceiling(beta*rho*sqrt(one+lmu*lepsii))
! i=Ndelta1
! Delta = Deltas(i)
! do m=1,nv2sum3D
!   m2=m**2
!   con = m2*Delta/(rho*sqrt(pi2))
!   do k=0,m
!     k2=k**2
!     cc=m/sqrt(real(m2+k2),DP)
!     sqrtmn = sqrt(real(m2-k2),DP)
!     ww=(Delta*sqrtmn-z0)/(Delta*rho)
!     u=log(con+cc+v_2D(k)) - half*(ww**2)
!     v2sum3D(m)=v2sum3D(m)+exp(u)
!   enddo
! enddo
!
!end subroutine RESAM_PLANARto3D
!******************************************************************
subroutine OUTSAM_PLANAR(jl_AB, jene, finumb, nat0,xnat0,termcon0,summul0)
implicit none
integer(I4B),intent(IN)    :: jl_AB, jene
character(len=*),intent(IN)  :: finumb
real(DP),dimension(:,:),intent(IN)     :: xnat0,termcon0,summul0
integer(I4B),dimension(:,:),intent(IN) :: nat0
integer(I4B) :: i,isam1,iii,lengdir,lengo,lfinumb1,iu_1,iu_2,iume,jp,jz,n_uncut,n_cut,k,iume2,ji
real(DP) :: u,ww,lmu,amu,smu,ww2l,yy1,yy,cfrt,x
character(512)  :: makepath

isam1 = Ndelta1
i=Ndelta1
Delta=Deltas(i)
lfinumb1=len_trim(finumb)

iii=nint(1000.0_DP*Delta)
  
!__ COMPOSE output file name into variable makepath
  
makepath=''
makepath(1:lbasepath_sampled)=basepath_sampled(1:lbasepath_sampled)
makepath(1+lbasepath_sampled:1+lsampled_folder+lbasepath_sampled)=sampled_folder(1:lsampled_folder)//separator
lengdir=1+lsampled_folder+lbasepath_sampled

call SYSTEM(trim(mkdir_command)//' '//makepath(1:lengdir)//' > tmp.out 2> tmp.err')
call SYSTEM(trim(delete_command)//' tmp.out tmp.err')

makepath(1+1+lsampled_folder+lbasepath_sampled:6+1+lsampled_folder+lbasepath_sampled)='SAMP2D'
lengo=6+1+lsampled_folder+lbasepath_sampled
  
write(makepath(1+lengo:5+lengo),'(i3.3,"A'//separator//'")')iii
call SYSTEM(trim(mkdir_command)//' '//makepath(1:5+lengo)//' > tmp.out 2> tmp.err')
call SYSTEM(trim(delete_command)//' tmp.out tmp.err')

makepath(6+lengo:5+lengo+lenfilou)=namfilou(1:lenfilou)
makepath(6+lengo+lenfilou:5+lengo+lenfilou+lfinumb1)=finumb(1:lfinumb1)
  
iu_1 = FIND_UNIT()
OPEN(iu_1,status='replace',file=trim(makepath))
iu_2 = FIND_UNIT()
OPEN(iu_2,status='replace',file=trim(makepath)//'_INFO')

cfrt=eps_DP
write(iu_1,'("#&& ",a)')trim(makepath)//'_INFO'
write(iu_2,'(a)') finumb(1:lfinumb1)
Bth_presamp = zero
iume=0
iume2=0
do jp=1,n_at_pair_V(1)
  do jz=1,ThePatVec(jp)%num_Zs
    n_uncut=This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%Sdim
    iume2=max(iume2,n_uncut)
    do k=n_uncut,0,-1
      x = This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%revec(k)
      if (abs(x)>cfrt) then
        iume=max(iume,k)
        exit
      endif
    enddo
  enddo
enddo
if (iume==0) iume=iume2
write(iu_2,'(a)')'Nptsam, jl_AB, n_sp_atom_V(1), n_at_pair_V(1), isam1, c_ref, rho, Delta, Bth_presamp'
write(iu_2,*)iume, jl_AB, n_sp_atom_V(1), n_at_pair_V(1), isam1, c_ref, rho, Delta, Bth_presamp

do k=1,n_sp_atom_V(1)
  write(iu_2,'(2i4,3(1x,g24.16))')k,Celty(1)%Z_at(k), &
                                       sum(nat0(k,:))/real(Number_Stack_Planes,DP),&
                                       sum(xnat0(k,:))/real(Number_Stack_Planes,DP),&
                                       sum(termcon0(Celty(1)%point_eqpair(k),:))/real(Number_Stack_Planes,DP) 
  
enddo
do k=1,n_at_pair_V(1)
  write(iu_2,'(3i4,(1x,g24.16))')k,Celty(1)%zappa(:,k), &
                                       sum(summul0(k,:))/real(Number_Stack_Planes,DP)
enddo

write(iu_2,'(a)')'jp (jp=1,n_at_pair), N_z '
  write(iu_2,'(a)')'jz, z_value, z_mul, iume, n_uncut ; values[iume]'
do jp=1,n_at_pair_V(1)
  write(iu_2,'(2i7 )') jp,ThePatVec(jp)%num_Zs
  do jz=1,ThePatVec(jp)%num_Zs
    n_uncut=This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%Sdim
    write(iu_2,'(i7,1x,g22.16,3i7)') jz, &
          ThePatVec(jp)%patbyz(jz)%Zval,nint(ThePatVec(jp)%patbyz(jz)%Zmul_inv), iume, n_uncut
    do ji=0,iume
      write(iu_1,*)This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%revec(ji)
    enddo
  enddo
enddo
close(iu_1)
close(iu_2)

end subroutine OUTSAM_PLANAR
!******************************************************************
subroutine INPUTSAM_PLANAR(jl_AB, jene, finumb, nat0,xnat0,termcon0,summul0)
implicit none
integer(I4B),intent(IN)    :: jl_AB, jene
character(len=*),intent(IN)  :: finumb

real(DP),dimension(:,:),intent(INOUT)     :: xnat0,termcon0,summul0
integer(I4B),dimension(:,:),intent(INOUT) :: nat0

integer(I4B) :: i,isam1,iii,lengdir,lengo,lfinumb1,iu_1,iu_2,iume,jp,jz,n_uncut,n_cut,k,iume2,ji
integer(I4B) :: num_Zs_in, nZmul_inv, jl_AB_read, kin
real(DP) :: u,ww,lmu,amu,smu,ww2l,yy1,yy,cfrt,x
real(DP) :: avtermcon0_STP,avnat0_STP,avxnat0_STP,avsummul0_STP, zval_in, rho_in
character(512)  :: makepath

isam1 = Ndelta1
i=Ndelta1
Delta=Deltas(i)
lfinumb1=len_trim(finumb)

iii=nint(1000.0_DP*Delta)
  
!__ COMPOSE output file name into variable makepath
  
makepath=''
makepath(1:lbasepath_sampled)=basepath_sampled(1:lbasepath_sampled)
makepath(1+lbasepath_sampled:1+lsampled_folder+lbasepath_sampled)=sampled_folder_READ(1:lsampled_folder)//separator
lengdir=1+lsampled_folder+lbasepath_sampled

!call SYSTEM(trim(mkdir_command)//' '//makepath(1:lengdir)//' > tmp.out 2> tmp.err')
!call SYSTEM(trim(delete_command)//' tmp.out tmp.err')

makepath(1+1+lsampled_folder+lbasepath_sampled:6+1+lsampled_folder+lbasepath_sampled)='SAMP2D'
lengo=6+1+lsampled_folder+lbasepath_sampled
  
write(makepath(1+lengo:5+lengo),'(i3.3,"A'//separator//'")')iii
!call SYSTEM(trim(mkdir_command)//' '//makepath(1:5+lengo)//' > tmp.out 2> tmp.err')
!call SYSTEM(trim(delete_command)//' tmp.out tmp.err')

makepath(6+lengo:5+lengo+lenfilou)=namfilou(1:lenfilou)
makepath(6+lengo+lenfilou:5+lengo+lenfilou+lfinumb1)=finumb(1:lfinumb1)
  
iu_1 = FIND_UNIT()
OPEN(iu_1,status='old',action='read',file=trim(makepath))
read(iu_1,*) ! skip first row containng the _INFO filename

iu_2 = FIND_UNIT()
OPEN(iu_2,status='old',action='read',file=trim(makepath)//'_INFO')

cfrt=eps_DP
read(iu_2,*) !(iu_2,'(a)') finumb(1:lfinumb1)
Bth_presamp = zero
iume=0
iume2=0

read(iu_2,*) !write(iu_2,'(a)')'Nptsam, jl_AB, n_sp_atom_V(1), n_at_pair_V(1), isam1, c_ref, rho, Delta, Bth_presamp'
read(iu_2,*)iume, jl_AB_read, n_sp_atom_V(1), n_at_pair_V(1), isam1, c_ref, rho_in, Delta, Bth_presamp

do k=1,n_sp_atom_V(1)
  read(iu_2,*)kin,Celty(1)%Z_at(k), &
                avnat0_STP, & !            sum(nat0(k,:))/real(Number_Stack_Planes,DP),&
                avxnat0_STP, & !           sum(xnat0(k,:))/real(Number_Stack_Planes,DP),&
                avtermcon0_STP !           sum(termcon0(Celty(1)%point_eqpair(k),:))/real(Number_Stack_Planes,DP) 
  nat0(k,:) = avnat0_STP
  xnat0(k,:) = avxnat0_STP
  termcon0(k,:) = avtermcon0_STP
enddo
do k=1,n_at_pair_V(1)
  read(iu_2,*)kin,Celty(1)%zappa(:,k), &
              avsummul0_STP  !  sum(summul0(k,:))/real(Number_Stack_Planes,DP)
  summul0(k,:)=avsummul0_STP
enddo

read(iu_2,*) !write(iu_2,'(a)')'jp (jp=1,n_at_pair), N_z '
read(iu_2,*) !write(iu_2,'(a)')'jz, z_value, z_mul, iume, n_uncut ; values[iume]'
do jp=1,n_at_pair_V(1)
  read(iu_2,*) kin,num_Zs_in
  if (ThePatVec(jp)%num_Zs /= num_Zs_in) stop '_INFO files give different Patterson z-values'
  do jz=1,ThePatVec(jp)%num_Zs
    read(iu_2,*) kin, &
          Zval_in,nZmul_inv, iume, n_uncut
!          ThePatVec(jp)%patbyz(jz)%Zval,nint(ThePatVec(jp)%patbyz(jz)%Zmul_inv), iume, n_uncut
    This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%Sdim=iume
    ALLOCATE(This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%revec(0:iume) )
!finqui
    do ji=0,iume
      read(iu_1,*)This_NC%AB_Cuts(jl_AB)%ABCPlanes(jene)%OPlane(jp)%PSD_Z(jz)%val_SBF(1)%val_byL(1)%val_byM(1,1)%revec(ji)
    enddo
  enddo
enddo
close(iu_1)
close(iu_2)

end subroutine INPUTSAM_PLANAR
!******************************************************************
  
end Module SAMPLING_2DIME
!___________________________________________________________________________________________________
module Sultans_Of_Swing
use paper_blood
use GEODIS
use ATOMIX
use helpinput

!****************** TYPES FOR CELY ***************************************
  type,public :: AtSpXYZ
    real(DP),dimension(:,:),allocatable   :: xyz
  END TYPE AtSpXYZ
  type,public :: tramem
    real(DP),dimension(3)   :: tra
    real(DP),dimension(3,3)   :: rot
  END TYPE tramem

  type,public :: SubUnitComp
    integer(I4B) :: n_members
    integer(I4B),dimension(:),allocatable :: i_at, z_at, n_at 
    real(DP),dimension(:),allocatable     :: xn_at, Occup, B
    TYPE(AtSpXYZ),dimension(:),allocatable  :: xyz_sp
    TYPE(tramem),dimension(:),allocatable   :: memb_trans
  END TYPE SubUnitComp

integer(I4B),save  :: nsubunits,nsubunits_all,nintersubu,membmax
integer(I4B),allocatable,save  :: subu_members(:),subu_scalable(:),subu_nasp(:),imatrix(:,:,:,:)
real(DP),allocatable,save      :: subu_center1st(:,:),subu_cendis(:,:)
type(SubUnitComp),allocatable,save :: Subunits(:)
real(DP),save :: DVi33(3,3),DVd33(3,3),sidestep_c(2),MTd33(3,3)
logical :: is_cely, is_c_orthogonal
character(len=1),dimension(8),parameter :: GS_1stch=['#','$','b','c','1','2','H','R']

  contains
  
!******************************************************************************
subroutine Pearson_to_SpGroup(pear, sgnum, sglet)
implicit none
character(len=*),intent(IN) :: pear
character(len=1),intent(OUT) :: sglet
integer(I4B),intent(OUT) :: sgnum
integer(I4B) :: io,lpe

sglet=' '
sgnum=-1
lpe=len_trim(pear)
if (ANY(GS_1stch==pear(1:1))) then
  sglet = pear(1:1)
  read(pear(2:lpe),*,iostat=io) sgnum
  if (io/=0) then
    sglet=' '
    sgnum=-1
    return
  endif
endif


end subroutine Pearson_to_SpGroup
!******************************************************************************
subroutine Find_The_SpGroup(sgnum, sglet)
implicit none

character(len=1),intent(IN) :: sglet
integer(I4B),intent(IN) :: sgnum
character(len=10) :: sgnam
character(len=24) :: sgfil
character(len=24) :: sgfil2
character(len=10) :: sgnam2
character(len=99) :: sg_rl,xggx,sg_rl2,xggx2
integer(I4B) :: iugr,lsgf,n1,n2,n1b,n2b,iogr,iogr2,i_spa,i_spa2
character(len=1) :: aa

iugr=find_unit()
open(iugr,status='old',action='READ', form='formatted',access='sequential',&
     file=trim(path_SpaceGroups)//'SG_Centering.txt')
read(iugr,*)
spgroup_name='P1        '
centering_type='P'
do
  read(iugr,'(a)',iostat=iogr)sg_rl
  if (iogr/=0) exit
  sg_rl=trim(sg_rl)
  read(sg_rl(1:12),*) n1,n2
  xggx=trim(adjustl(sg_rl(13:)))
  i_spa = INDEX(xggx,' ')
  sgnam = trim(adjustl(xggx(1:i_spa)))
  sgfil = trim(adjustl(xggx(i_spa+1:)))
  if (n1==sgnum) then
    if (sglet=='#'.or.(sglet=='b'.or.(sglet=='H'.or.sglet=='1'))) then
      continue
    else if (sglet=='$'.or.(sglet=='c'.or.(sglet=='R'.or.sglet=='2'))) then ! read one more line...
      
      read(iugr,'(a)',iostat=iogr2)sg_rl2
      if (iogr2/=0) exit
      sg_rl2=trim(sg_rl2)
      read(sg_rl2(1:12),*) n1b,n2b
      xggx2=trim(adjustl(sg_rl2(13:)))
      i_spa2 = INDEX(xggx2,' ')
      sgnam2 = trim(adjustl(xggx2(1:i_spa2)))
      sgfil2 = trim(adjustl(xggx2(i_spa2+1:)))
      if (n1b/=sgnum) then
        stop 'Find_The_SpGroup error: space group incorrectly given'
      else
        sgnam=sgnam2
        sgfil=sgfil2
        n2=n2b
      endif
    endif
    !___ Write in SAVED variables...
    spgroup_name=trim(sgnam)
    spgroup_file = trim(sgfil)
    lsgf=len_trim(spgroup_file)
    if (lsgf==24) then
      aa=sgfil(lsgf-4:lsgf-4)
    else
      aa='#'
    endif
    spgroup_IDENT=''
    write(spgroup_IDENT,'(i3.3,"[",a,"]",a)')nspgroup,trim(sgnam),trim(sgfil)
    spgroup_IDENT=trim(adjustl(spgroup_IDENT))
    !___ USE NUMERIC CENTERING TYPE : number of centers...
    write(centering_type,'(i1)') n2
    exit
  endif
enddo
close(iugr)


end subroutine Find_The_SpGroup
!******************************************************************************
function DVds_calc(abcabg)
implicit none
real(DP),intent(IN) :: abcabg(6)
real(DP) :: DVds_calc(3,3)
real(DP) :: cga,sga,osga,cbe,sbe,cal,sal,c2al,c2be,c2ga

DVds_calc=zero
DVds_calc(1,1) = abcabg(1)
cga=accomo(Cos(degrees_to_radians*abcabg(6)))
sga=accomo(Sin(degrees_to_radians*abcabg(6)))
osga=one/sga
DVds_calc(1,2) = abcabg(2)*cga
DVds_calc(2,2) = abcabg(2)*sga
cbe=accomo(Cos(degrees_to_radians*abcabg(5)))
sbe=accomo(Sin(degrees_to_radians*abcabg(5)))
cal=accomo(Cos(degrees_to_radians*abcabg(4)))
sal=accomo(Sin(degrees_to_radians*abcabg(4)))
c2al=accomo(cal*cal-sal*sal)
c2be=accomo(cbe*cbe-sbe*sbe)
c2ga=accomo(cga*cga-sga*sga)

DVds_calc(1,3) = abcabg(3)*cbe
DVds_calc(2,3) = abcabg(3)*(cal-cbe*cga)*osga
DVds_calc(3,3) = abcabg(3)*sqrt(max(zero,four*cal*cbe*cga-one-c2al-c2be-c2ga))*osga*srhalf

end function DVds_calc
!******************************************************************************
function accomo(x)
implicit none
real(DP),intent(IN) :: x
real(DP) :: accomo

if (abs(x)<sceps_DP) then
  accomo=zero
else if (abs(x-one)<sceps_DP) then
  accomo=one
else
  accomo=x
endif

end function accomo
!******************************************************************************
function eumat(eulan,scf)
implicit none
real(DP),dimension(3),intent(IN) :: eulan
real(DP),optional,intent(IN) :: scf
real(DP),dimension(3,3) :: eu1,eu2,eu3,eumat
real(DP),dimension(3) :: eulan1
real(DP) :: coeu,sieu

eulan1=eulan
if (PRESENT(scf)) then
  eulan1=eulan*scf
endif
eu1=zero;eu2=zero;eu3=zero

coeu=cos(eulan1(1)); sieu=sin(eulan1(1))
eu1(1,1)=coeu;eu1(2,2)=coeu
eu1(1,2)=sieu;eu1(2,1)=-sieu
eu1(3,3)=one
coeu=cos(eulan1(2)); sieu=sin(eulan1(2))
eu2(1,1)=coeu;eu2(3,3)=coeu
eu2(1,3)=-sieu;eu2(3,1)=sieu
eu2(2,2)=one
coeu=cos(eulan1(3)); sieu=sin(eulan1(3))
eu3(1,1)=coeu;eu3(2,2)=coeu
eu3(1,2)=sieu;eu3(2,1)=-sieu
eu3(3,3)=one
eumat=matmul(eu3,matmul(eu2,eu1))


end function eumat
!******************************************************************************
subroutine readallo_cely(cely_fn_V,lcely_fn_V, ncl2mk,dcl2mk,itra,geom, diace,cdx,c_lattice, occ1, ncelltypes1 )
implicit none
character(len=3),parameter :: commy='!#>'
character(len=*),dimension(:),intent(IN) :: cely_fn_V
integer(I4B),intent(IN)     :: itra
integer(I4B),dimension(:),intent(IN)     :: lcely_fn_V
integer(I4B),optional,intent(IN)  :: ncelltypes1
logical,intent(IN)          :: occ1
integer(I4B),intent(INOUT)  :: ncl2mk(2)
real(DP),intent(INOUT)      :: dcl2mk(2)
real(DP),intent(OUT)        :: diace,cdx,c_lattice
character(len=3),intent(IN) :: geom
!_________________ LOCAL
real(DP) :: base_area_max,side_base_max,cell_base_area,xxx_base_max,r0c,auxC(3),auxA(3)
integer(I4B) :: igoon
real(DP) :: a,b,c,alpha_DEG,beta_DEG,gamma_DEG, gmm,cg,sg,c2g,bsg,bcg, rely(5),DVinv(2,2)
integer(I4B) :: iu,ierr,ll,iep,lpearsy,nmabys,i,j,k,kz, kp,i1,i2,ifk,kk,k1,k2,icomm,ll2,jat2
integer(I4B) :: ifail,isubu,isubu2, kkk1,jm0,ipes1,ipes2,iugr,iogr,icel,jsiz, ind
character(512) :: rl,rl2,rlgr
character(len=10) :: sgnrrr
character(len=24) :: sgnfff
character(8) :: pearsy
character(2) :: ch2
logical :: orthoXY
logical :: allow_sghemb
logical,dimension(:),allocatable :: crys_vs_abso
real(DP),allocatable :: prov_oB(:,:)
integer(I4B),allocatable :: prov_izn(:,:),oBabys(:,:),kato(:)
integer(I4B),allocatable :: nabys(:),zabys(:)  ! local in readallo_cely

character(len=40) :: namestr_full
integer(I4B)      :: lnamestr_full,jasp,jat,io5,io4,io3,jmem,jmem2,nspmx,i_this_as,iii,isp,iro,iu_xyz,n1,n2,kx,ix
integer(I4B)      :: sgnum1=-1,lpee
character(len=1)  :: sglet1=' '
real(DP)          :: eulang(3), acczc(3),accz



allow_sghemb = (((geom=='SPH'.or.geom=='QBE').or.geom=='LQB').or.geom=='CSH')
allow_sghemb = (allow_sghemb .or. ((geom=='PAR'.or.geom=='CYL').or.geom=='HEX'))
allow_sghemb = (allow_sghemb .or. (geom=='SET'))
nsubunits_all = 0
membmax=0
if (PRESENT(ncelltypes1)) then
  ncelltypes = ncelltypes1
else if (.not.PRESENT(ncelltypes1)) then
  ncelltypes = 1
endif
if (verbose) print*,'Ready to read .cel* files: # .cel* files = ',ncelltypes
ncellpairs = ncelltypes*(ncelltypes+1)
ncellpairs = ncellpairs / 2
ALLOCATE(n_sp_atom_V(ncelltypes), n_at_pair_V(ncelltypes))
if (allocated(nbypair)) deallocate(nbypair)
if (allocated(numpatv)) deallocate(numpatv)
if (ncelltypes>1) then
  ALLOCATE(nbypair(napair_all,ncelltypes),numpatv(napair_all,ncellpairs))
  nbypair=0
  numpatv=0
endif
call DO_SETUP

do icel=1,ncelltypes
  
  ll=len_trim(adjustl(cely_fn_V(icel)))
  if (ll/=lcely_fn_V(icel)) then
    print*, 'readallo_cely: incongruous filename '//cely_fn_V(icel)
    stop 'readallo_cely: incongruous filename in cely_fn_V '
  endif
  is_cely=(cely_fn_V(icel)(lcely_fn_V(icel)-4:lcely_fn_V(icel))=='.cely')
  iu=find_unit()
  if (verbose) print'(a)','cely file is '//trim(cely_fn_V(icel))
  OPEN(UNIT=iu,status='old',action='READ', file=trim(cely_fn_V(icel)), iostat=ierr)
  IF (ierr /=0) THEN
    print*, 'readallo_cely: Error opening .cely file: ', cely_fn_V(icel),icel
    STOP 'readallo_cely: Error opening .cely file named as cely_fn_V(icel)'
  ENDIF
  iu_xyz=find_unit()
  OPEN(UNIT=iu_xyz,status='replace', &
       file=trim(cely_fn_V(icel))//'.xyz', iostat=ierr)
  print*,'Selected Shape      = '//geom
  
  spgroup_name='P1        '
  centering_type='P'
  nspgroup=1
  spgroup_file = 'SPG_grp/SG_Nr_001.grp   '
  spgroup_IDENT= '001[P1]SPG_grp/SG_Nr_001.grp            '
  pearsy='aP01'
  
  if (is_cely) then
    !______________________________________ CASE .cely
    call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
  !print'(a)',rl2(1:ll2)
    if (ifail==1) stop 'cely missing line 1'
    namestr_full = rl2(1:ll2)
    lnamestr_full=ll2
    !_________________ Read from 2nd part of line 1 : Pearson symbol OR space group number
    ipes1=INDEX(namestr_full(1:ll2),' ')+1
    ipes2=ipes1+INDEX(namestr_full(ipes1:ll2),' ')-1
    pearsy=''
    pearsy=namestr_full(ipes1:ipes2)
    lpearsy=ipes2-ipes1+1
    lpee=min(lpearsy,4)
    if (icel==1) then
    
!________ Reading space group - try
      call Pearson_to_SpGroup(pearsy(1:lpee), sgnum1, sglet1)
      if (sgnum1 == -1 .and. sglet1 == ' ') then ! failed, it is a true Pearson symbol
        print'(a)','Pearson symbol given = '//pearsy(1:lpee)
!______ Old-style Pearson symbol...
        centering_type = pearsy(2:2)
        if (.not. ANY([centering_type=='P', centering_type=='F', centering_type=='I', centering_type=='A', &
                   centering_type=='B', centering_type=='C', centering_type=='R'])) then
          centering_type='1'
          print*,'Readallo_cely warning: Pearson symbol/space group not read correctly, assuming P centering'
        endif
      else
        if (verbose) print'(a)','True Space Group given = '//pearsy(1:lpee)
      
        nspgroup=sgnum1
        if (nspgroup < 1 .or. nspgroup > 230) then
          centering_type='P'
          print*,'Readallo_cely warning: Pearson symbol/space group not read correctly, assuming P centering'
          nspgroup=1
        else
          call Find_The_SpGroup(sgnum=nspgroup, sglet=sglet1)
           ind=index(spgroup_IDENT, "[")
           print*,'Space Group number, setting, centering : ',nspgroup,' ',sglet1,' ',spgroup_IDENT(ind+1:ind+1)
          !print*,'Space Group Found : ',nspgroup,' ',sglet1,' ',centering_type
        endif
      endif
    endif

    call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
  !print'(a)',rl2(1:ll2)
    if (ifail==1) then
      stop 'cely missing line 2'
      print'(a)',rl2(1:ll2)
    endif
    if (geom/='SET') then
      if (rl2(1:4)=='CELL') then
        if (icel==1) then
          read(rl2(5:ll2),*)abcabgy
        endif
      else
        stop 'cely: no cell '
      endif
    else if (geom=='SET') then
      abcabgy = [one,one,one,90.d0,90.d0,90.d0]
    endif
    
    !print*,'DEBUG 1: ',abcabgy
    
    !_ use cell info, build crystalmatrices
    if (icel==1) then
      a=abcabgy(1); b=abcabgy(2); c=abcabgy(3)
      alpha_DEG=abcabgy(4); beta_DEG=abcabgy(5); gamma_DEG=abcabgy(6)
      is_c_orthogonal = .true.
      if ((abs(alpha_DEG-90.d0)>sceps_DP.or.abs(beta_DEG-90.d0)>sceps_DP)) then
        is_c_orthogonal = .false.
      endif
      if ((.not.is_c_orthogonal).and.(.not.allow_sghemb)) then
        print*,' Cells with alpha and/or beta /= 90 deg are partially dealt with! '
        !STOP ' Cells with alpha and/or beta /= 90 deg are not dealt with! Stop.'
      endif
      DVd33 = DVds_calc(abcabgy)
      sidestep_c = DVd33(1:2,3)
      MTd33(1,:) = [DVd33(1,1)**2,DVd33(1,1)*DVd33(1,2),DVd33(1,1)*DVd33(1,3)]
      MTd33(2,2:)= [DVd33(1,2)**2 + DVd33(2,2)**2,DVd33(1,2)*DVd33(1,3) + DVd33(2,2)*DVd33(2,3)]
      MTd33(3,3) = DVd33(1,3)**2 + DVd33(2,3)**2 + DVd33(3,3)**2
      MTd33(2:3,1)= MTd33(1,2:3)
      MTd33(3,2)  = MTd33(2,3)
      
      !print*,'DEBUG 2: DVd33'
      do ix=1,3
        print*,ix,DVd33(ix,1),',',DVd33(ix,2),',',DVd33(ix,3),'},'
      enddo
      WHERE(ABS(MTd33)<ten*eps_DP) MTd33=zero
      gmm=gamma_DEG*degrees_to_radians
      if (abs(gamma_DEG)-90.d0<sceps_DP) then
        sg=one
        cg=zero
        c2g=-one
        orthoXY=.true.
      else
        sg=sin(gmm)
        cg=cos(gmm)
        c2g=cos(two*gmm)
        orthoXY=.false.
      endif
      c_lattice = c
      bsg=b*sg
      bcg=b*cg
      PlaneDV = DVd33(1:2,1:2)
!      PlaneDV=zero
!      PlaneDV(1,1:2)=[a,bcg]; PlaneDV(2,2)=bsg
      DVi33=zero
      DVi33(1,:)=[one, -DVd33(1,2)/DVd33(2,2),(-DVd33(1,3)*DVd33(2,2) + DVd33(1,2)*DVd33(2,3))/(DVd33(2,2)*DVd33(3,3))] &
                / DVd33(1,1)
      DVi33(2,2:3)=[ one, -DVd33(2,3)/DVd33(3,3) ]/DVd33(2,2)
      DVi33(3,3) = one/DVd33(3,3)
      WHERE(ABS(DVi33)<ten*eps_DP) DVi33=zero
      
      !print*,'DEBUG 3: DVi33'
      do ix=1,3
        print*,ix,DVi33(ix,1),',',DVi33(ix,2),',',DVi33(ix,3),'},'
      enddo
      DVinv=DVi33(1:2,1:2)
!      DVinv(1,1)=one/a
!      DVinv(:,2)=[-cg/(a*sg),one/bsg]
!      DVi33=zero
!      DVi33(1:2,1:2)=DVinv
!      DVi33(3,3)=one/c
!      DVd33=zero
!      DVd33(1:2,1:2)=PlaneDV
!      DVd33(3,3)=c
      WHERE(ABS(PlaneDV)<ten*eps_DP) PlaneDV=zero
      PlaneMT = MTd33(1:2,1:2)
!      PlaneMT(1,:)=[a,   bcg]*a
!      PlaneMT(2,2)=b*b; PlaneMT(2,1) = PlaneMT(1,2)
      WHERE(ABS(PlaneMT)<ten*eps_DP) PlaneMT=zero
      cell_volume = a*b*c*COGEO(alpha_DEG, beta_DEG, gamma_DEG)
    endif    
    call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
  !print'(a)',rl2(1:ll2)
    if (ifail==1) stop 'cely missing line 3'
    if (rl2(1:9)=='NSUBUNITS') then
      read(rl2(10:ll2),*)nsubunits
    else
      stop 'cely: no # subunits '
    endif
    allocate(subu_members(nsubunits),subu_scalable(nsubunits),subu_nasp(nsubunits), &
             Subunits(nsubunits),subu_center1st(3,nsubunits))
    allocate(crys_vs_abso(nsubunits))
             subu_members=0;subu_nasp=0; subu_scalable=1;subu_center1st=zero
    
    call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
    if (ifail==1) stop 'cely missing line 4'
    if (rl2(1:16)=='NSUBUNIT_MEMBERS') then
      read(rl2(17:ll2),*)subu_members
    else
      stop 'cely: no # subunit memners '
    endif
    call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
    if (ifail==1) stop 'cely missing line 5'
    if (rl2(1:13)=='SUBUNIT_LOOP:') then
      continue
    else
      stop 'cely: missing start subunit loop '
    endif
    do isubu=1,nsubunits
      Subunits(isubu)%n_members = subu_members(isubu)
      nsubunits_all=nsubunits_all+subu_members(isubu)
      membmax=max(membmax,subu_members(isubu))
      allocate(Subunits(isubu)%memb_trans(subu_members(isubu)))
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) then
        print'(a)',rl2(1:ll2)
        stop 'cely missing line 6'
      endif
      if (rl2(1:4)=='SUBU') then
        read(rl2(5:ll2),*) isubu2
        if (isubu2/=isubu) stop 'cely unaligned subunits'
      else
        stop 'cely: missing start subunit loop '
      endif
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) stop 'cely missing line 7'
      if (ll2==8.and.rl2=='SCALABLE') then
         subu_scalable(isubu)=1
      else if (ll2==5.and.rl2=='FIXED') then
         subu_scalable(isubu)=0
      else
        stop 'cely: is it scalable? '
      endif
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) stop 'cely missing line 8'
      read(rl2(1:ll2),*)subu_nasp(isubu)
      jasp=subu_nasp(isubu)
      ALLOCATE(Subunits(isubu)%i_at(jasp), &
               Subunits(isubu)%z_at(jasp), Subunits(isubu)%n_at(jasp), Subunits(isubu)%xn_at(jasp), &
               Subunits(isubu)%Occup(jasp), Subunits(isubu)%B(jasp),Subunits(isubu)%xyz_sp(jasp))
  
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) stop 'cely missing line 9'
      if (rl2(1:13)=='SPECIES_LOOP:') then
        continue
      else
        stop 'cely: missing start species loop '
      endif
      do jasp=1,subu_nasp(isubu)
        call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
        if (ifail==1) stop 'cely missing line 10'
        read(rl2(1:ll2),*) Subunits(isubu)%i_at(jasp), &
                           Subunits(isubu)%z_at(jasp), Subunits(isubu)%n_at(jasp), Subunits(isubu)%xn_at(jasp), &
                           Subunits(isubu)%Occup(jasp), Subunits(isubu)%B(jasp)
        if (occ1) then
          Subunits(isubu)%Occup(jasp) = sign(one,Subunits(isubu)%Occup(jasp))
          !?
          Subunits(isubu)%xn_at(jasp) = Subunits(isubu)%n_at(jasp) * sign(one,Subunits(isubu)%Occup(jasp))
        endif
        allocate(Subunits(isubu)%xyz_sp(jasp)%xyz(3,Subunits(isubu)%n_at(jasp)))
      enddo
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) stop 'cely missing line 11'
      if (rl2(1:10)=='ATOM_LOOP:') then
        if ((ll2>10.and.rl2(ll2:ll2)=='C').or.(ll2==10.and.rl2(ll2:ll2)==':')) then 
          crys_vs_abso(isubu)=.true.
        else if (ll2>10.and.rl2(ll2:ll2)=='A') then
          crys_vs_abso(isubu)=.false.
        endif
      else
        stop 'cely: missing start atom loop '
      endif
      acczc=zero
      do jasp=1,subu_nasp(isubu)
        do jat=1,Subunits(isubu)%n_at(jasp)
          call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
          if (ifail==1) stop 'cely missing line 12'
          iro=1
          read(rl2(1:ll2),*,iostat=io5)ch2,jat2,rely(1:5)
          if (io5/=0) then
            read(rl2(1:ll2),*,iostat=io4)ch2,jat2,rely(1:4)
            if (io4/=0) then
              iro=0
              read(rl2(1:ll2),*,iostat=io3)ch2,jat2,rely(1:3)
              if (io3/=0) stop 'error reading symbol, x,y,z'
            endif
          endif
          if (jat2 /= Subunits(isubu)%i_at(jasp)) stop 'cely: Misaligned species index'
          if (ch2 /= symb_of_z(Subunits(isubu)%z_at(jasp))) stop 'cely: Misaligned symbol'
          Subunits(isubu)%xyz_sp(jasp)%xyz(1:3,jat)=rely(1:3)
          
          acczc=acczc+rely(1:3)*Subunits(isubu)%z_at(jasp)*Subunits(isubu)%Occup(jasp)
          accz=accz+Subunits(isubu)%z_at(jasp)*Subunits(isubu)%Occup(jasp)
        enddo
      enddo
      subu_center1st(:,isubu) = acczc/accz
      !______ finqi
      call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
      if (ifail==1) stop 'cely missing line 13'
      if (rl2(1:13)=='MEMBERS_LOOP:') then
        continue
      else
        stop 'cely: missing start atom loop '
      endif
        
      do jmem=1,subu_members(isubu) 
        !____ this is 'MMB' followed by member counter
        call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
        if (ifail==1) stop 'cely missing line 14'
        if (rl2(1:3)=='MMB') then
          read(rl2(4:ll2),*) jmem2
          if (jmem2/=jmem) stop 'cely: misaligned members / tr.rot. '
        else
          stop 'cely: missing MMB '
        endif
        !____ this is 'TRC' or 'TRA' followed by a translation vector 
        call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
        if (ifail==1) stop 'cely missing line 15'
        if (rl2(1:3)=='TRA') then
          read(rl2(4:ll2),*) Subunits(isubu)%memb_trans(jmem)%tra
        else if (rl2(1:3)=='TRC') then
          read(rl2(4:ll2),*) auxC
          Subunits(isubu)%memb_trans(jmem)%tra = matmul(DVd33,auxC)
        else
          stop 'cely: missing subunit translation '
        endif
        !____ this is either 
        !____ 'RTM' followed by 9 numbers defining row-by-row a rotation matrix (absolute coord.), 
        !____ or
        !____ 'RTE' followed by 3 numbers defining Euler angles defining a rotation matrix (abs. coord.)
        !____ latter not yet implemented
        call READNEXT_COMM(comms=commy,iunit=iu,istop=0, ifail=ifail,rlin=rl2,lrlin=ll2); print'(a)',rl2(1:ll2)
        if (ifail==1) then
          print'(a)',rl2(1:ll2)
          print*,jmem,isubu,subu_members(isubu),nsubunits, subu_members
          stop 'cely missing line 16'
        endif
        if (rl2(1:3)=='RTM') then
          read(rl2(4:ll2),*) Subunits(isubu)%memb_trans(jmem)%rot
          Subunits(isubu)%memb_trans(jmem)%rot=TRANSPOSE(Subunits(isubu)%memb_trans(jmem)%rot)
        else if (rl2(1:3)=='RTE') then
          read(rl2(4:ll2),*) eulang
          Subunits(isubu)%memb_trans(jmem)%rot=eumat(eulan=eulang,scf=degrees_to_radians)
        else
          stop 'cely: missing subunit rotation matrix '
        endif
      enddo
    enddo
    close(iu)
    !___________Closed the .cely file...
    nintersubu = nsubunits_all**2
    allocate(subu_cendis(3,nintersubu))
    allocate(imatrix(nsubunits,membmax,nsubunits,membmax))
    subu_cendis=zero
    kkk1=0
    do isubu=1,nsubunits
        do jmem=1,subu_members(isubu)
          do isubu2=1,nsubunits
            do jmem2=1,subu_members(isubu2)
              kkk1=kkk1+1
              subu_cendis(:,kkk1) = matmul(DVi33, &
                                    Subunits(isubu)%memb_trans(jmem)%tra &
                                  - Subunits(isubu2)%memb_trans(jmem2)%tra)
              imatrix(isubu,jmem,isubu2,jmem2) = kkk1
            enddo
          enddo
        enddo
    enddo
    if (icel==1) then
      cell_base_area=a*bsg
      if (itra==1) then ! D to N, mode = PAR
        base_area_max=pi*unqua*((DCL2MK(1)*ten)**2)
        select case (geom)
        case('PAR')
           side_base_max=sqrt(base_area_max/cell_base_area)
           NCL2MK(1)=ceiling(side_base_max)
           cell_volume_red = cell_volume
        case('HEX')
           xxx_base_max=base_area_max/cell_base_area
           NCL2MK(1)=ceiling((-three+sqrt(max(nine,-three+twelve*xxx_base_max)))/six)
           cell_volume_red = cell_volume
        case('CYL')
           r0c=sqrt(cell_base_area/pi)
           NCL2MK(1)=max(0,-1+ceiling(half*DCL2MK(1)*ten/r0c))
           cell_volume_red = cell_volume
        case('SPH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
           NCL2MK(1)=max(0,-1+ceiling(half*DCL2MK(1)*ten/r0c))
        case('QBE')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red ))
!           NCL2MK(1) = max(0,-1+ceiling(half* ( exp(unter*log(unses*pi)) ) * DCL2MK(1)*ten/r0c))
           NCL2MK(1) = max(0,-1+ceiling(DCL2MK(1)*ten/r0c))
        case('SET')
           ncenters_cell = 1
           cell_volume_red = one
           r0c=one
           NCL2MK=1; DCL2MK=zero
        case('CSH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
           NCL2MK(1)=max(1,ceiling(half*DCL2MK(1)*ten/r0c))
        case default
           stop 'Unknown shape '
        end select
        NCL2MK(2)=ceiling(DCL2MK(2)*ten/c)
        if ((geom=='SPH'.or.geom=='CSH').or.(geom=='QBE'.or.geom=='LQB')) NCL2MK(2)=NCL2MK(1)
      else if (itra==2) then ! N to D
        DCL2MK(2)=c*NCL2MK(2)
        select case (geom)
        case('PAR')
           base_area_max=cell_base_area*(NCL2MK(1)**2)
           cell_volume_red = cell_volume
        case('HEX')
           base_area_max=cell_base_area*(1+3*NCL2MK(1)*(NCL2MK(1)+1))
           cell_volume_red = cell_volume
        case('CYL')
           base_area_max=cell_base_area*((1+NCL2MK(1))**2)
           cell_volume_red = cell_volume
        case('SPH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
        case('QBE')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red ))
        case('SET')
           ncenters_cell = 1
           cell_volume_red = one
           r0c=one
           NCL2MK=1; DCL2MK=zero
        case('CSH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
        case default
        stop 'Unknown shape '
        end select
        if (geom/='SET') then
          if ((geom/='SPH' .and. geom/='CSH') .and. geom/='QBE') then
            DCL2MK(1)=two*SQRT(base_area_max/pi)+s4eps_DP
          else if (geom=='SPH' ) then
            DCL2MK(1)=(two*(NCL2MK(1)+1)*r0c)+s4eps_DP
            DCL2MK(2)=DCL2MK(1)
          else if (geom=='QBE' ) then
            DCL2MK(1)=((NCL2MK(1)+1)*r0c)+s4eps_DP
            DCL2MK(2)=DCL2MK(1)
          else if (geom=='CSH') then
            DCL2MK(1:2)=(two*(NCL2MK(1:2)-1)*r0c)+s4eps_DP
          endif
          DCL2MK=DCL2MK/ten
        endif
      else if (itra==0) then !__ only in case SET
        NCL2MK=1; DCL2MK=zero
      endif
      allocate(growing_diam(0:maxval(NCL2MK),2))
      growing_diam=zero
      if (geom=='SET') then
        growing_diam(1,1) = one
      else if (geom=='SPH') then
        do jsiz=0,NCL2MK(1)
          growing_diam(jsiz,1) = two*(jsiz+1)*r0c
        enddo
      else if (geom=='QBE') then
        do jsiz=0,NCL2MK(1)
          growing_diam(jsiz,1) = (jsiz+1)*r0c*exp(unter*log(six/pi))
        enddo
      else if (geom=='CSH') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = two*(jsiz-1)*r0c
        enddo
      else if (geom=='PAR') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*jsiz*SQRT(cell_base_area/pi), c*jsiz]
        enddo
      else if (geom=='HEX') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*SQRT((cell_base_area*(1+3*jsiz*(jsiz+1)))/pi), c*jsiz]
        enddo
      else if (geom=='CYL') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*(1+jsiz)*SQRT(cell_base_area/pi), c*jsiz]
        enddo
      endif
      growing_diam=growing_diam/ten
      igoon=1
      if (geom=='PAR') then
        if (minval(NCL2MK)==0) igoon=0
      else 
        if (NCL2MK(1)<0.or.NCL2MK(2)<1) igoon=0
      endif
      if (igoon==0) then
        print*,'No clusters to do! stop.'
        stop 'No clusters to do! stop.'
      endif
      if (geom=='PAR') then
        diace=max(PlaneMT(1,1),PlaneMT(2,2),sum(PlaneMT),PlaneMT(1,1)+PlaneMT(2,2)-PlaneMT(1,2)-PlaneMT(2,1))
      else if (geom=='HEX') then
        diace=max(PlaneMT(1,1),PlaneMT(2,2),sum(PlaneMT))
      else if (geom=='CYL') then
        diace=two*sqrt(cell_base_area/pi)
      else if (geom=='SET') then
        diace=one
      else if ((geom=='SPH'.or.geom=='CSH').or.geom=='QBE') then
        diace=r0c
      endif
      cdx=(real(NCL2MK(2)+1,DP)*c)**2    
    endif
  !__ used cell info
    nspmx=sum(subu_nasp(1:nsubunits))
    allocate(prov_izn(0:2,nspmx),prov_oB(2,nspmx),kato(nspmx))
    prov_izn=0;prov_oB=zero
    n_sp_atom_V(icel)=0
    do isubu=1,nsubunits
      do jasp=1,subu_nasp(isubu)
        n_sp_atom_V(icel)=max(n_sp_atom_V(icel),Subunits(isubu)%i_at(jasp))
      enddo
    enddo
    
    do isubu=1,nsubunits
      do jasp=1,subu_nasp(isubu)
        i_this_as=Subunits(isubu)%i_at(jasp)
        if (prov_izn(0,i_this_as) /= 0) then
          iii=maxval(abs([i_this_as, Subunits(isubu)%z_at(jasp), &
                          Subunits(isubu)%n_at(jasp) * Subunits(isubu)%n_members]-prov_izn(0:2,i_this_as)))
          if (iii>0) stop 'Confusion with Atomic Species'
        else
          prov_izn(:,i_this_as) = [i_this_as, Subunits(isubu)%z_at(jasp), &
                                   Subunits(isubu)%n_at(jasp) * Subunits(isubu)%n_members ]
          prov_oB(:,i_this_as)  = [Subunits(isubu)%Occup(jasp), Subunits(isubu)%B(jasp)]
        endif
      enddo
    enddo
    print'(a,i4)',' Nr. of Atom Species = ',n_sp_atom_V(icel)
    if (allocated(nabys)) then
      deallocate(nabys)
      deallocate(zabys)
      deallocate(oBabys)
    endif
    allocate(nabys(n_sp_atom_V(icel)),zabys(n_sp_atom_V(icel)),oBabys(2,n_sp_atom_V(icel)))
    do i=1,n_sp_atom_V(icel)
      iii=prov_izn(0,i)
      print*,i,iii
      zabys(iii)=prov_izn(1,i)
      nabys(iii)=prov_izn(2,i)
      oBabys(:,iii)=prov_oB(:,i)
    enddo
    deallocate(prov_izn,prov_oB)
    nmabys=maxval(nabys)
    if (allocated(atx)) then
      deallocate(atx)
      deallocate(ato)
      deallocate(atb)
      deallocate(atisubu)
    endif
    
    
!____________ AC 05 Aug 15 - shift here
    n_at_pair_V(icel)=n_sp_atom_V(icel)*(n_sp_atom_V(icel)+1)
    n_at_pair_V(icel)=n_at_pair_V(icel)/2
    print'(a,i4)',' Nr. of Atom Pairs   = ',n_at_pair_V(icel)


    if (ncelltypes==1) ALLOCATE(nbypair(n_at_pair_V(1),1),numpatv(n_at_pair_V(1),1))

    call ALLOCALL(ncelltypes_in=ncelltypes,icel1=icel)
!____________ AC 05 Aug 15 end
    
    
    
    allocate(atx(3,nmabys,n_sp_atom_V(icel)),ato(nmabys,n_sp_atom_V(icel)),atb(nmabys,n_sp_atom_V(icel)))
    allocate(Celty(icel)%atx(3,nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%ato(nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%atb(nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%atxCSU(3,nmabys,n_sp_atom_V(icel)))
    allocate(atisubu(2,nmabys,n_sp_atom_V(icel)))
    atx=zero; ato=zero; atb=one; atisubu=0
    kato=0
    do isubu=1,nsubunits
      do jasp=1,subu_nasp(isubu)
        isp=0
        do i=1,n_sp_atom_V(icel)
          if (i==Subunits(isubu)%i_at(jasp)) then
            isp=i
            exit
          endif
        enddo
        do jat=1,Subunits(isubu)%n_at(jasp)
          if (crys_vs_abso(isubu)) then
            do jmem=1, subu_members(isubu)
              auxC = Subunits(isubu)%xyz_sp(jasp)%xyz(:,jat)
              auxA = Subunits(isubu)%memb_trans(jmem)%tra + &
                     matmul(Subunits(isubu)%memb_trans(jmem)%rot,matmul(DVd33,auxC))
  !            print'(2i4,3(1x,"  [",3(1x,f14.7),"]  "))',jat,jmem,auxC,auxA,Subunits(isubu)%memb_trans(jmem)%tra 
              auxC = matmul(DVi33,auxA)
  !            print'(2i4,1(1x,"  [",3(1x,f14.7),"]  "),2(/,3(3(1x,g12.6),2x)))',jat,jmem,auxC,DVd33,DVi33
              kato(isp)=kato(isp)+1
              atx(:,kato(isp),isp) = auxC
              !print*,'DEBUG X ',isp,kato(isp),atx(:,kato(isp),isp)
              ato(kato(isp),isp) = Subunits(isubu)%Occup(jasp)
              atb(kato(isp),isp) = Subunits(isubu)%B(jasp)
              atisubu(:,kato(isp),isp) = [isubu,jmem]
              Celty(icel)%atxCSU(:,kato(isp),isp) = matmul(DVi33,Subunits(isubu)%memb_trans(jmem)%tra)
              
            enddo
          else
            do jmem=1, subu_members(isubu)
              auxA = Subunits(isubu)%xyz_sp(jasp)%xyz(:,jat)
              auxA = Subunits(isubu)%memb_trans(jmem)%tra + &
                     matmul(Subunits(isubu)%memb_trans(jmem)%rot,auxA)
              auxC = matmul(DVi33,auxA)
              kato(isp)=kato(isp)+1
              atx(:,kato(isp),isp) = auxC
              ato(kato(isp),isp) = Subunits(isubu)%Occup(jasp)
              atb(kato(isp),isp) = Subunits(isubu)%B(jasp)
              atisubu(:,kato(isp),isp) = [isubu,jmem]
              Celty(icel)%atxCSU(:,kato(isp),isp) = matmul(DVi33,Subunits(isubu)%memb_trans(jmem)%tra)
            enddo
          endif
        enddo
      enddo
    enddo
    where(abs(atx)<sceps_DP) atx=zero
    where(abs(atx-unses)<s4eps_DP) atx=unses
    where(abs(atx-unqua)<sceps_DP) atx=unqua
    where(abs(atx-unter)<s4eps_DP) atx=unter
    where(abs(atx-half)<sceps_DP) atx=half
    where(abs(atx-duter)<s4eps_DP) atx=duter
    where(abs(atx-one+unqua)<sceps_DP) atx=one-unqua
    where(abs(atx-one+unses)<sceps_DP) atx=one-unses
    where(abs(atx-one)<sceps_DP) atx=one
    
    write(iu_xyz,*)sum(nabys(1:n_sp_atom_V(1)))
    write(iu_xyz,'(a)')trim(cely_fn_V(icel))
    do isp=1,n_sp_atom_V(1)
      do i1=1,nabys(isp)
        write(iu_xyz,'(a2,5(1x,g16.10))')symb_of_z(zabys(isp)),matmul(DVd33,atx(:,i1,isp)),ato(i1,isp),atb(i1,isp)
      enddo
    enddo
    close(iu_xyz)
    
!____________ AC 05 Aug 15 - ADD
    do i1=1,n_sp_atom_V(icel)
      Celty(icel)%nat(i1)=nabys(i1)
      Celty(icel)%xnat(i1)=sum(ato(1:nabys(i1),i1)) !! sum(Celty(icel)%ato(1:nabys(i1),i1))
      if (verbose) print*,'cely debg1 ',i1,icel,n_sp_atom_V(icel),Celty(icel)%nat(i1),Celty(icel)%xnat(i1)
    enddo
    kp=0
    do i1=1,n_sp_atom_V(icel);do i2=i1,n_sp_atom_V(icel)
      kp=kp+1
      ifk=2
      if (i1==i2) ifk=1
      nbypair(kp,icel)=nabys(i1)*nabys(i2)*ifk
      Celty(icel)%ZZP(kp)=REAL(zabys(i1)*zabys(i2),DP)
      Celty(icel)%minallowdist(kp)=at_radii(zabys(i1))+at_radii(zabys(i2))
      Celty(icel)%zappa(:,kp)=[i1,i2]
      if (verbose) print*,'>>>>>>>>>>>>>>>> cely filling nbypair:',icel, i1,i2,kp,nbypair(kp,icel)
    enddo;enddo
!____________ AC 05 Aug 15 end
!____________ AC 05 Aug 15 - REMOVE (SHIFT UP)
!    n_at_pair_V(icel)=n_sp_atom_V(icel)*(n_sp_atom_V(icel)+1)
!    n_at_pair_V(icel)=n_at_pair_V(icel)/2
!    print'(a,i4)',' Nr. of Atom Pairs   = ',n_at_pair_V(icel)
!
!
!    if (ncelltypes==1) ALLOCATE(nbypair(n_at_pair_V(1),1),numpatv(n_at_pair_V(1),1))
!
!    call ALLOCALL(ncelltypes_in=ncelltypes,icel1=icel)
!____________ AC 05 Aug 15 end
    
    Celty(icel)%atx = atx
    Celty(icel)%atb = atb
    Celty(icel)%ato = ato
  
    a_latt_cioc=one
    Celty(icel)%Z_at=zabys
    Celty(icel)%point_eqpair=0
    Celty(icel)%point_neqpair=0
    kk=0
    do k=1,n_at_pair_V(icel)
      k1=Celty(icel)%zappa(1,k)
      k2=Celty(icel)%zappa(2,k)
      if (k1==k2) then
        Celty(icel)%point_eqpair(k1)=k
      else
        kk=kk+1
        Celty(icel)%point_neqpair(kk)=k
      endif
    enddo
    
  else if (.not.is_cely) then
    !______________________________________ CASE .cel NOT .cely
    rl=''
    read(iu,'(a)') rl
    call clean_line(rl)
    rl=trim(adjustl(rl))
    ll=len_trim(rl)

    if (icel==1) then
    
      iep=index(rl(1:ll),' ')
      pearsy=rl(1:iep-1)
      lpearsy=iep-1
      lpee=min(lpearsy,4)
      

!________ Reading space group - try
      call Pearson_to_SpGroup(pearsy(1:lpee), sgnum1, sglet1)
      if (sgnum1 == -1 .and. sglet1 == ' ') then ! failed, it is a true Pearson symbol
!______ Old-style Pearson symbol...
        print'(a)','Pearson symbol given = '//pearsy(1:lpee)
        centering_type = pearsy(2:2)
        if (.not. ANY([centering_type=='P', centering_type=='F', centering_type=='I', centering_type=='A', &
                   centering_type=='B', centering_type=='C', centering_type=='R'])) then
          centering_type='1'
          print*,'Readallo_cely warning: Pearson symbol/space group not read correctly, assuming P centering'
        endif
      else
       if (verbose)  print'(a)','True Space Group given = '//pearsy(1:lpee)
      
        nspgroup=sgnum1
      
        if (nspgroup < 1 .or. nspgroup > 230) then
          centering_type='P'
          print*,'Readallo_cely warning: Pearson symbol/space group not read correctly, assuming P centering'
          nspgroup=1
        else
          call Find_The_SpGroup(sgnum=nspgroup, sglet=sglet1)
          ind=index(spgroup_IDENT, "[")
          print*,'Space Group number, setting, centering :',nspgroup,' ',sglet1,' ',spgroup_IDENT(ind+1:ind+1)
        endif
      endif
        
      read(rl(iep:ll),*)a,b,c,alpha_DEG,beta_DEG,gamma_DEG,n_sp_atom_V(1)
      
      if ((abs(alpha_DEG-90.d0)>sceps_DP.or.abs(beta_DEG-90.d0)>sceps_DP).and.(.not.allow_sghemb)) then
        print*,' Cells with alpha and/or beta /= 90 deg are not dealt with! Stop.'
        STOP ' Cells with alpha and/or beta /= 90 deg are not dealt with! Stop.'
      endif
      if (geom=='SET') then
        abcabgy = [one,one,one,90.d0,90.d0,90.d0]
        cell_volume = one
      else
        abcabgy=[a,b,c,alpha_DEG,beta_DEG,gamma_DEG]
        cell_volume = a*b*c*COGEO(alpha_DEG, beta_DEG, gamma_DEG)
      endif
      print*,'Nr. of Atom Species = ',n_sp_atom_V(1)
      gmm=gamma_DEG*degrees_to_radians
      if (abs(gamma_DEG)-90.d0<sceps_DP) then
        sg=one
        cg=zero
        c2g=-one
        orthoXY=.true.
      else
        sg=sin(gmm)
        cg=cos(gmm)
        c2g=cos(two*gmm)
        orthoXY=.false.
      endif
      c_lattice = c
      bsg=b*sg
      bcg=b*cg
      PlaneDV=zero
      PlaneDV(1,1:2)=(/a,bcg/); PlaneDV(2,2)=bsg
      DVd33=zero
      DVd33(1:2,1:2)=PlaneDV
      DVd33(3,3)=c
      WHERE(ABS(PlaneDV)<ten*eps_DP) PlaneDV=zero
      PlaneMT(1,:)=[a**2,   a*b*cg]
      PlaneMT(2,:)=[a*b*cg, b**2  ]
      
      cell_base_area=a*bsg
      if (itra==1) then ! D to N, mode = PAR
        base_area_max=pi*unqua*((DCL2MK(1)*ten)**2)
        select case (geom)
        case('PAR')
           side_base_max=sqrt(base_area_max/cell_base_area)
           NCL2MK(1)=ceiling(side_base_max)
           cell_volume_red = cell_volume
        case('HEX')
           xxx_base_max=base_area_max/cell_base_area
           NCL2MK(1)=ceiling((-three+sqrt(max(nine,-three+twelve*xxx_base_max)))/six)
           cell_volume_red = cell_volume
        case('CYL')
           r0c=sqrt(cell_base_area/pi)
           NCL2MK(1)=max(0,-1+ceiling(half*DCL2MK(1)*ten/r0c))
           cell_volume_red = cell_volume
        case('SPH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
           NCL2MK(1)=max(0,-1+ceiling(half*DCL2MK(1)*ten/r0c))
        case('QBE')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red ))
!           NCL2MK(1) = max(0,-1+ceiling(half* ( exp(unter*log(unses*pi)) ) * DCL2MK(1)*ten/r0c))
           NCL2MK(1) = max(0,-1+ceiling(DCL2MK(1)*ten/r0c))
        case('SET')
           ncenters_cell = 1
           cell_volume_red = one
           r0c=one
           NCL2MK=1
        case('CSH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
           NCL2MK(1)=max(1,ceiling(half*DCL2MK(1)*ten/r0c))
        case default
           stop 'Unknown shape '
        end select
        NCL2MK(2)=ceiling(DCL2MK(2)*ten/c)
        if ((geom=='SPH'.or.geom=='CSH').or.(geom=='QBE'.or.geom=='LQB')) NCL2MK(2)=NCL2MK(1)
      else if (itra==2) then ! N to D
        DCL2MK(2)=c*NCL2MK(2)
        select case (geom)
        case('PAR')
           base_area_max=cell_base_area*(NCL2MK(1)**2)
           cell_volume_red = cell_volume
        case('HEX')
           base_area_max=cell_base_area*(1+3*NCL2MK(1)*(NCL2MK(1)+1))
           cell_volume_red = cell_volume
        case('CYL')
           base_area_max=cell_base_area*((1+NCL2MK(1))**2)
           cell_volume_red = cell_volume
        case('SPH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
        case('QBE')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red ))
        case('SET')
           ncenters_cell = 1
           cell_volume_red = one
           r0c=one
           DCL2MK=zero
        case('CSH')
           ncenters_cell = GET_CENTERING_MULT(centering_type)
           cell_volume_red = cell_volume / real(max(1,ncenters_cell),DP)
           r0c=exp(unter*log( cell_volume_red*unqua*three/pi ))
        case default
        stop 'Unknown shape '
        end select
        
        if (geom/='SET') then
          if ((geom/='SPH' .and. geom/='CSH') .and. geom/='QBE') then
            DCL2MK(1)=two*SQRT(base_area_max/pi)+s4eps_DP
          else if (geom=='SPH' ) then
            DCL2MK(1)=(two*(NCL2MK(1)+1)*r0c)+s4eps_DP
            DCL2MK(2)=DCL2MK(1)
          else if (geom=='QBE' ) then
            DCL2MK(1)=((NCL2MK(1)+1)*r0c)+s4eps_DP
            DCL2MK(2)=DCL2MK(1)
          else if (geom=='CSH') then
            DCL2MK(1:2)=(two*(NCL2MK(1:2)-1)*r0c)+s4eps_DP
          endif
          DCL2MK=DCL2MK/ten
        endif
        DCL2MK=DCL2MK/ten
      else if (itra==0) then ! only for SET
        NCL2MK=1; DCL2MK=zero
      endif
      allocate(growing_diam(0:maxval(NCL2MK),2))
      growing_diam=zero
      if (geom=='SET') then
        growing_diam(1,1) = one
      else if (geom=='SPH') then
        do jsiz=0,NCL2MK(1)
          growing_diam(jsiz,1) = two*(jsiz+1)*r0c
        enddo
      else if (geom=='QBE') then
        do jsiz=0,NCL2MK(1)
          growing_diam(jsiz,1) = (jsiz+1)*r0c*exp(unter*log(six/pi))
        enddo
      else if (geom=='CSH') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = two*(jsiz-1)*r0c
        enddo
      else if (geom=='PAR') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*jsiz*SQRT(cell_base_area/pi)  , c*jsiz]
        enddo
      else if (geom=='HEX') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*SQRT((cell_base_area*(1+3*jsiz*(jsiz+1)))/pi)  , c*jsiz]
        enddo
      else if (geom=='CYL') then
        do jsiz=1,maxval(NCL2MK)
          growing_diam(jsiz,:) = [two*(1+jsiz)*SQRT(cell_base_area/pi)  , c*jsiz]
        enddo
      endif
      growing_diam=growing_diam/ten
      igoon=1
      if (geom=='PAR') then
        if (minval(NCL2MK)==0) igoon=0
      else 
        if (NCL2MK(1)<0.or.NCL2MK(2)<1) igoon=0
      endif
      if (igoon==0) then
        print*,'No clusters to do! stop.'
        stop 'No clusters to do! stop.'
      endif
      if (geom=='PAR') then
        diace=max(PlaneMT(1,1),PlaneMT(2,2),sum(PlaneMT),PlaneMT(1,1)+PlaneMT(2,2)-PlaneMT(1,2)-PlaneMT(2,1))
      else if (geom=='HEX') then
        diace=max(PlaneMT(1,1),PlaneMT(2,2),sum(PlaneMT))
      else if (geom=='CYL') then
        diace=two*sqrt(cell_base_area/pi)
      else if (geom=='SET') then
        diace=one
      else if ((geom=='SPH'.or.geom=='CSH').or.geom=='QBE') then
        diace=r0c
      endif
      cdx=(real(NCL2MK(2)+1,DP)*c)**2
    endif
    
    ! end if icel==1
    
    if (allocated(nabys)) deallocate(nabys)
    if (allocated(zabys)) deallocate(zabys)
    if (ncelltypes>1) n_sp_atom_V(icel) = naspe_cells(icel)
    n_at_pair_V(icel)=n_sp_atom_V(icel)*(n_sp_atom_V(icel)+1)
    n_at_pair_V(icel)=n_at_pair_V(icel)/2
    
    if (verbose) print*, 'READALLO .cel branch: ncelltypes = ', ncelltypes, icel, n_sp_atom_V(icel)
    if (verbose) print*, 'READALLO .cel branch: cell diameter, cell volume, cell volume/#centers = ', &
    diace,cell_volume,cell_volume_red
    
    if (ncelltypes==1) ALLOCATE(nbypair(n_at_pair_V(1),1),numpatv(n_at_pair_V(1),1))
    call ALLOCALL(ncelltypes_IN = ncelltypes, icel1 = icel)
    
    if (verbose) print*, 'ncelltypes on exit = ', ncelltypes, icel
    
    allocate(nabys(n_sp_atom_V(icel)),zabys(n_sp_atom_V(icel)))
    read(iu,*) nabys
    read(iu,*) zabys
    nmabys=maxval(nabys)
    
    if (allocated(atx)) then
      deallocate(atx)
      deallocate(ato)
      deallocate(atb)
    endif
    
    allocate(atx(3,nmabys,n_sp_atom_V(icel)),ato(nmabys,n_sp_atom_V(icel)),atb(nmabys,n_sp_atom_V(icel)))
    allocate(Celty(icel)%atx(3,nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%ato(nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%atb(nmabys,n_sp_atom_V(icel)), &
             Celty(icel)%atxCSU(3,nmabys,n_sp_atom_V(icel)))
    atx=zero; ato=zero; atb=one
    do i=1,n_sp_atom_V(icel)
      do j=1,nabys(i)
        read(iu,'(a)')rl
        rl=trim(adjustl(rl))
        ch2=rl(1:2)
        kz=0
        do k=1,n_elements
          if (ch2==symb_of_Z(k)(1:2)) then
            kz=k
            exit
          endif
        enddo
        if (kz/=zabys(i)) stop 'unsorted species!'
        read(rl(3:),*)atx(:,j,i),ato(j,i),atb(j,i)
        if (occ1) ato(j,i) = sign(one,ato(j,i))
        if (verbose) print*,atx(:,j,i),ato(j,i)
      enddo
    enddo
    close(iu)
    where(abs(atx)<sceps_DP) atx=zero
    where(abs(atx-unses)<s4eps_DP) atx=unses
    where(abs(atx-unqua)<sceps_DP) atx=unqua
    where(abs(atx-unter)<s4eps_DP) atx=unter
    where(abs(atx-half)<sceps_DP) atx=half
    where(abs(atx-duter)<s4eps_DP) atx=duter
    where(abs(atx-one+unqua)<sceps_DP) atx=one-unqua
    where(abs(atx-one+unses)<sceps_DP) atx=one-unses
    where(abs(atx-one)<sceps_DP) atx=one
  
    write(iu_xyz,*)sum(nabys(1:n_sp_atom_V(icel)))
    write(iu_xyz,'(a)')trim(cely_fn_V(icel))
    do isp=1,n_sp_atom_V(icel)
      do i1=1,nabys(isp)
        write(iu_xyz,'(a2,5(1x,g16.10))')symb_of_z(zabys(isp)),matmul(DVd33,atx(:,i1,isp)),ato(i1,isp),atb(i1,isp)
      enddo
    enddo
    close(iu_xyz)
    
!!    n_at_pair_V(icel)=n_sp_atom_V(icel)*(n_sp_atom_V(icel)+1)
!!    n_at_pair_V(icel)=n_at_pair_V(icel)/2
    print*,'Nr. of Atom Pairs = ',n_at_pair_V(icel)

!!    call ALLOCALL(ncelltypes_in=ncelltypes,icel1=icel)
    
    do i1=1,n_sp_atom_V(icel)
      Celty(icel)%nat(i1)=nabys(i1)
      Celty(icel)%xnat(i1)=sum(ato(1:nabys(i1),i1)) !! sum(Celty(icel)%ato(1:nabys(i1),i1))
      if (verbose) print*,'debg1 ',i1,icel,n_sp_atom_V(icel),Celty(icel)%nat(i1),Celty(icel)%xnat(i1)
    enddo
    kp=0
    do i1=1,n_sp_atom_V(icel);do i2=i1,n_sp_atom_V(icel)
      kp=kp+1
      ifk=2
      if (i1==i2) ifk=1
      nbypair(kp,icel)=nabys(i1)*nabys(i2)*ifk
      Celty(icel)%ZZP(kp)=REAL(zabys(i1)*zabys(i2),DP)
      Celty(icel)%minallowdist(kp)=at_radii(zabys(i1))+at_radii(zabys(i2))
      Celty(icel)%zappa(:,kp)=[i1,i2]
      if (verbose) print*,'>>>>>>>>>>>>>>>> filling nbypair:',icel, i1,i2,kp,nbypair(kp,icel)
    enddo;enddo
    
    Celty(icel)%atxCSU = atx
    Celty(icel)%atx = atx
    Celty(icel)%atb = atb
    Celty(icel)%ato = ato
!
    
    Celty(icel)%Z_at=zabys
    Celty(icel)%point_eqpair=0
    Celty(icel)%point_neqpair=0
    kk=0
    do k=1,n_at_pair_V(icel)
      k1=Celty(icel)%zappa(1,k)
      k2=Celty(icel)%zappa(2,k)
      if (k1==k2) then
        Celty(icel)%point_eqpair(k1)=k
      else
        kk=kk+1
        Celty(icel)%point_neqpair(kk)=k
      endif
    enddo
  
  endif
  if (verbose) print*,'CELL a,b,c,alpha,beta,gamma : ',abcabgy
enddo

if (ncelltypes==1) then
  napair_all=n_at_pair_V(1)
  naspe_all =n_sp_atom_V(1)
endif

call allocall_glo(ncelltypes)
call MAKE_PATTERSON()
end subroutine readallo_cely
!******************************************************************************
subroutine MAKE_PATTERSON()
implicit none
!_________________ LOCAL
real(DP) :: Yaux(3),YauxCSU(3),xc(8),ooo,bbb,xxx,chosig,xmuz,t00,t0,t1
integer(I4B) :: KKK,NMPerPair,is1,is2,ipa,j1,j2,ics,isnew,jadd,jck,Iflag, &
                I0,I0p1,Ndym,Ndymm1,locsubu(4),iiimx,jv, &
                icel1,icel2,kcelp,iga1,iga2,ixc,icelb1,icelb2
integer(I4B),allocatable :: mace(:,:)
logical :: isordnot

allocate(mace(ncelltypes,ncelltypes))
kcelp=0
do icel1=1,ncelltypes
  do icel2=icel1,ncelltypes
    kcelp=kcelp+1
    mace(icel1,icel2)=kcelp
    mace(icel2,icel1)=kcelp
  enddo
enddo

!print*,'DEBUG MAKE_PATTERSON: ncelltypes = ',ncelltypes

ixc=0
numpatv=0
NMPerPair=maxval(nbypair)
if (verbose) print*,'MAKE_PATTERSON: NMPerPair = ',NMPerPair,ncelltypes
if (verbose) print*,'allocating patte: ',8,NMPerPair,napair_all,ncellpairs
allocate(patte(8,NMPerPair,napair_all,ncellpairs))
patte=zero
do icel1=1,ncelltypes
   if (verbose) print*,'RR1',return_address_atsp(:,icel1)
  do icel2=icel1,ncelltypes
    if (verbose) print*,'RR2',return_address_atsp(:,icel2)
    kcelp=mace(icel1,icel2)
    do ipa=1,napair_all
      iga1=glopair(1,ipa)
      iga2=glopair(2,ipa)
!___________ Part 1      
      is1=return_address_atsp(iga1,icel1)
      is2=return_address_atsp(iga2,icel2)
      !print*,'DEBUGZ;',glopair(:,ipa),is1,is2,Celty(icel1)%nat(is1),Celty(icel2)%nat(is2)
      if (min(is1,is2)==0) goto 1999
      if (verbose) then
         print'(a,2i3,a,2i4,a,2i4)','1st : Cells  - Species pair [',icel1,is1,'] [',icel2,is2,'] G ',iga1,iga2
         print*,'Species pair ini',kcelp,ipa,iga1,iga2,numpatv(ipa,kcelp),sum(patte(4,1:numpatv(ipa,kcelp),ipa,kcelp)),&
              Celty(icel1)%nat(is1),Celty(icel2)%nat(is2)
      endif        
!      print*,'!!!!!',icel1,icel2,ipa,is1,is2,Celty(icel1)%nat,Celty(icel2)%nat
      !call CPU_TIME(t00);t0=t00
      do j1=1,Celty(icel1)%nat(is1) !call CPU_TIME(t1);print'("at ",i5,2(1x,f14.6))',j1,t1-t0,t1-t00;t0=t1
       do j2=1,Celty(icel2)%nat(is2)
        !print*,'DEBUG_O1',icel1,j1,is1,icel2,j2,is2
        !print*,'DEBUG_O2',Celty(icel1)%ato(j1,is1),Celty(icel2)%ato(j2,is2)
        ooo=Celty(icel1)%ato(j1,is1)*Celty(icel2)%ato(j2,is2)
    ! skip 'empty' pair
        if (abs(ooo)<sceps_DP) CYCLE
        Yaux=(Celty(icel1)%atx(:,j1,is1)-Celty(icel2)%atx(:,j2,is2)) 
        YauxCSU=(Celty(icel1)%atxCSU(:,j1,is1)-Celty(icel2)%atxCSU(:,j2,is2)) 
        where(abs(Yaux)<sceps_DP) Yaux=zero
        where(abs(YauxCSU)<sceps_DP) YauxCSU=zero
    ! skip inversion equivalents in same-cell case
        xmuz=one
        if ((icel1==icel2).and.(is1==is2)) then
          if (Yaux(3)<-sceps_DP) then
            CYCLE
          else if (abs(Yaux(3))<=sceps_DP) then
            if (Yaux(2)<-sceps_DP) then
              CYCLE
            else if (abs(Yaux(2))<=sceps_DP) then
              if (Yaux(1)<-sceps_DP) CYCLE
            endif
          endif
    ! skip inversion equivalents in different-cell case
        else if (icel1/=icel2) then
          if (abs(Yaux(3))>sceps_DP) then
            chosig=sign(one,Yaux(3))
          else
            if (abs(Yaux(2))>sceps_DP) then
              chosig=sign(one,Yaux(2))
            else
              if (abs(Yaux(1))>sceps_DP) then
                chosig=sign(one,Yaux(1))
              else
                chosig=one
              endif
            endif
          endif
          Yaux=Yaux*chosig
          YauxCSU=YauxCSU*chosig
        endif
        bbb=Celty(icel1)%atb(j1,is1)+Celty(icel2)%atb(j2,is2)
        isnew=1
        jadd=0
        do jck=1,numpatv(ipa,kcelp)
          xxx=maxval(abs(Yaux-patte(1:3,jck,ipa,kcelp))) &
             +maxval(abs(YauxCSU-patte(6:8,jck,ipa,kcelp)))
          if (xxx<s4eps_DP) then
            isnew=0
            jadd=jck
            exit
          endif
        enddo
        if (isnew==1) then
          numpatv(ipa,kcelp)=numpatv(ipa,kcelp)+1
          patte(1:3,numpatv(ipa,kcelp),ipa,kcelp)=Yaux
          patte(4,numpatv(ipa,kcelp),ipa,kcelp)=ooo!*xmuz
          patte(5,numpatv(ipa,kcelp),ipa,kcelp)=bbb
          patte(6:8,numpatv(ipa,kcelp),ipa,kcelp)=YauxCSU  !real(iiimx,DP)
        else
          patte(4,jadd,ipa,kcelp)=patte(4,jadd,ipa,kcelp)+ooo!*xmuz
        endif
      enddo;enddo
      if (verbose) print*,'Species pair end',kcelp,ipa,iga1,iga2,numpatv(ipa,kcelp),sum(patte(4,1:numpatv(ipa,kcelp),ipa,kcelp))
!___________ Part 2      
      1999 if (ixc==1) cycle
      if (iga1==iga2.or.icel1==icel2) cycle
      
      icelb1=icel2
      icelb2=icel1
      is1=return_address_atsp(iga1,icelb1)
      is2=return_address_atsp(iga2,icelb2)
      if (min(is1,is2)==0) cycle
      if (verbose) then
         print'(a,2i3,a,2i4,a,2i4)','2nd : Cells  - Species pair [',icelb1,is1,'] [',icelb2,is2,'] G ',iga1,iga2
         print*,'Species pair ini',kcelp,ipa,iga1,iga2,numpatv(ipa,kcelp),sum(patte(4,1:numpatv(ipa,kcelp),ipa,kcelp))
      endif   
!      print*,'!!!!!',icelb1,icelb2,ipa,is1,is2,Celty(icelb1)%nat,Celty(icelb2)%nat
      do j1=1,Celty(icelb1)%nat(is1); do j2=1,Celty(icelb2)%nat(is2)
        ooo=Celty(icelb1)%ato(j1,is1)*Celty(icelb2)%ato(j2,is2)
      ! skip 'empty' pair
        if (abs(ooo)<sceps_DP) CYCLE
        Yaux=(Celty(icelb1)%atx(:,j1,is1)-Celty(icelb2)%atx(:,j2,is2)) !*updown(ics)
        YauxCSU=(Celty(icelb1)%atxCSU(:,j1,is1)-Celty(icelb2)%atxCSU(:,j2,is2)) !*updown(ics)
        where(abs(Yaux)<sceps_DP) Yaux=zero
        where(abs(YauxCSU)<sceps_DP) YauxCSU=zero
      ! skip inversion equivalents in same-cell case
        xmuz=one
      ! skip inversion equivalents in different-cell case
        if (icelb1/=icelb2) then
          if (abs(Yaux(3))>sceps_DP) then
            chosig=sign(one,Yaux(3))
          else
            if (abs(Yaux(2))>sceps_DP) then
              chosig=sign(one,Yaux(2))
            else
              if (abs(Yaux(1))>sceps_DP) then
                chosig=sign(one,Yaux(1))
              else
                chosig=one
                !xmuz=two    ! zero distance in different cells -> multiply by two (NO)
              endif
            endif
          endif
          Yaux=Yaux*chosig
          YauxCSU=YauxCSU*chosig
        endif
        bbb=Celty(icelb1)%atb(j1,is1)+Celty(icelb2)%atb(j2,is2)
        isnew=1
        jadd=0
        do jck=1,numpatv(ipa,kcelp)
          xxx=maxval(abs(Yaux-patte(1:3,jck,ipa,kcelp))) &
             +maxval(abs(YauxCSU-patte(6:8,jck,ipa,kcelp)))
!          if (is_cely) xxx=xxx+abs(iiimx-patte(6,jck,ipa))
          if (xxx<s4eps_DP) then
            isnew=0
            jadd=jck
            exit
          endif
        enddo
        if (isnew==1) then
          numpatv(ipa,kcelp)=numpatv(ipa,kcelp)+1
          patte(1:3,numpatv(ipa,kcelp),ipa,kcelp)=Yaux
          patte(4,numpatv(ipa,kcelp),ipa,kcelp)=ooo!*xmuz
          patte(5,numpatv(ipa,kcelp),ipa,kcelp)=bbb
          patte(6:8,numpatv(ipa,kcelp),ipa,kcelp)=YauxCSU  !real(iiimx,DP)
        else
          patte(4,jadd,ipa,kcelp)=patte(4,jadd,ipa,kcelp)+ooo!*xmuz
        endif
      enddo;enddo
      print*,'Species pair end',kcelp,ipa,iga1,iga2,numpatv(ipa,kcelp),sum(patte(4,1:numpatv(ipa,kcelp),ipa,kcelp))
      
    enddo
  enddo
enddo
!____ sort by z,y,x
do icel1=1,ncelltypes
  do icel2=icel1,ncelltypes
    kcelp=mace(icel1,icel2)
    do ipa=1,napair_all
  !____ sort by z,y,x
      KKK=numpatv(ipa,kcelp)
      Iflag=1
      DO
       IF (Iflag == 0) exit
       Ndym=KKK
       Iflag=0
       Ndymm1=Ndym-1
       DO I0=1,Ndymm1
         I0p1=I0+1
         isordnot=.false.
         if (patte(3,I0,ipa,kcelp)>patte(3,I0p1,ipa,kcelp)+sceps_DP) then
           isordnot = .true.
         else if (abs(patte(3,I0,ipa,kcelp)-patte(3,I0p1,ipa,kcelp))<=sceps_DP) then
           if (patte(2,I0,ipa,kcelp)>patte(2,I0p1,ipa,kcelp)+sceps_DP) then
             isordnot = .true.
           else if (abs(patte(2,I0,ipa,kcelp)-patte(2,I0p1,ipa,kcelp))<=sceps_DP) then
             if (patte(1,I0,ipa,kcelp)>patte(1,I0p1,ipa,kcelp)+sceps_DP) isordnot = .true.
           endif
         endif
         IF (isordnot) THEN
           xc=patte(:,I0,ipa,kcelp)
           patte(:,I0,ipa,kcelp)=patte(:,I0p1,ipa,kcelp)
           patte(:,I0p1,ipa,kcelp)=xc
           KKK=I0
           Iflag=1
         ENDIF
       ENDDO
      ENDDO
      IF (verbose) THEN
        print*,'Pair #',ipa,': Number of vectors = ',numpatv(ipa,kcelp),sum(patte(4,1:numpatv(ipa,kcelp),ipa,kcelp))
        do jv=1,numpatv(ipa,kcelp)
          print'(i4,3(1x,f10.3),3x,1x,f9.2,i5)',jv,patte(1:4,jv,ipa,kcelp),nint(patte(6,jv,ipa,kcelp))
        enddo
      endif
    enddo
  enddo
enddo
if (ncelltypes == 1) then 
  call Patterson_Sorter()
endif

end subroutine MAKE_PATTERSON
end module Sultans_Of_Swing
!___________________________________________________________________________________________________
module Jmol_XYZfile
use nano_deftyp
use atomix
use paper_blood
!use GEODIS,only : patte !,nbypair,numpatv
real(DP),parameter :: B_default=0.5d0,Occ_default=one
integer(I4B),allocatable :: nabys(:),zabys(:)

type,public :: onespec
    integer(I4B)  :: ispec=0,zspec=0,nathis=0
    real(DP)  :: avocc=zero,avb=one
    real(DP),dimension(:,:),allocatable       :: coo_xyzbo 
end type onespec

type(onespec),allocatable,save :: themolec(:) 
integer(I4B),save :: n_all_at,n_all_at_sp
real(DP),save :: madiamol=one


contains

subroutine read_XYZ_only(fn,redmind)
implicit none
character(len=*),intent(INOUT) :: fn
real(DP),intent(IN) :: redmind
character(2) :: syer
character(256) :: rl_xyz
integer(I4B) :: lfn,iuxyz,j,bo0ob1,islab,lrl_xyz,i,Z_a, &
                coat(0:100)=0,coal(100)=0,zoal(100), &
                m,conum(2,10),jc,kb,ke,nnu,lab,isp,kk,k,k1,k2,kat
integer(I4B),allocatable :: cous(:)
real(DP) :: xyzboL(6),aco45(0:2,4:5),mami(2,3),geocen(0:3),rcenmax
logical :: b_is_4

mami(1,:)= 99.d99
mami(2,:)=-99.d99
b_is_4 = .true.
zoal=-1
islab=0
fn=trim(adjustl(fn))
lfn=len_trim(fn)
iuxyz=find_unit()
open(iuxyz,status='old',action='read',file=fn(1:lfn))
read(iuxyz,*)n_all_at
read(iuxyz,*)
geocen=zero
do j=1,n_all_at
  read(iuxyz,'(a)')rl_xyz
  call CLEAN_LINE(rl_xyz)
  rl_xyz=trim(adjustl(rl_xyz))
  lrl_xyz=len_trim(rl_xyz)
  syer=rl_xyz(1:2)
  do i=0,n_elements
    IF (syer /= symb_of_Z(i)) CYCLE
    Z_a = i
    EXIT
  enddo
  if (j==1) then
    kb=0 ! count space-nonspace
    do jc=4,lrl_xyz
      if (rl_xyz(jc-1:jc-1)==' '.and.rl_xyz(jc:jc)/=' ') then
        kb=kb+1
        conum(1,kb)=jc
      endif
    enddo
!print*,'kb = count space-nonspace = ',kb
    ke=0 ! count nonspace-space = end-of-word
    do jc=4,lrl_xyz-1
      if (rl_xyz(jc+1:jc+1)==' '.and.rl_xyz(jc:jc)/=' ') then
        ke=ke+1
        conum(2,ke)=jc
      endif
    enddo
!print*,'ke = count nonspace-space (1) = ',ke
    if (rl_xyz(lrl_xyz:lrl_xyz)/=' ') then ! add 1 to space-nonspace
      ke=ke+1
      conum(2,ke)=lrl_xyz
    endif
!print*,'ke = count nonspace-space (2) = ',ke
  
    if (ke/=kb) then
      print'(a)',rl_xyz(3:lrl_xyz)
      print'(a,2i5)','bad line', kb,ke
      stop 'bad line'
    endif  
    if (ke>6.or.ke<3) then
      print'(a)',rl_xyz(3:lrl_xyz)
      print'(a,2i5)','messy', kb,ke
      stop 'messy'
    endif
    if (ke==3) then
      b_is_4 = .false.
    endif
    nnu=ke
    if (nnu==6) islab=1
  endif
!print*,'reading',nnu,islab,ke,kb
  xyzbol=zero
  read(rl_xyz(3:lrl_xyz),*)xyzbol(1:nnu)
  geocen(0)=geocen(0)+one
  geocen(1:3)=geocen(1:3)+xyzbol(1:3)
  
  if (islab/=1) then
    coat(Z_a)=coat(Z_a)+1
  else
    lab=nint(xyzbol(6))
    coal(lab)=coal(lab)+1
    if (coal(lab)>1) then
      if (zoal(lab)/=Z_a) then
        print'(a)',rl_xyz(3:lrl_xyz)
        print'(a,8i5)','mixing atoms', j,nnu,kb,ke,lab,coal(lab),zoal(lab),Z_a
        stop 'mixing atoms'
      endif
    else
      zoal(lab)=Z_a
    endif
  endif
enddo
rewind(iuxyz)
geocen(1:3)=geocen(1:3)/geocen(0)
read(iuxyz,*)
read(iuxyz,*)
if (islab==0) then
  n_all_at_sp = count(coat>0)
  allocate(nabys(n_all_at_sp),zabys(n_all_at_sp),cous(n_all_at_sp))
  allocate(themolec(n_all_at_sp))
  nabys=0;zabys=0
  m=0
  do i=0,n_elements
    IF (coat(i)==0) CYCLE
    m=m+1
    nabys(m) = coat(i)
    zabys(m) = i
  enddo
else
  n_all_at_sp = count(coal>0)
  allocate(nabys(n_all_at_sp),zabys(n_all_at_sp),cous(n_all_at_sp))
  allocate(themolec(n_all_at_sp))
  nabys=0;zabys=0
  m=0
  do i=0,n_elements
    IF (coal(i)==0) CYCLE
    m=m+1
    nabys(m) = coal(i)
    zabys(m) = zoal(i)
  enddo
endif
cous=0

do isp=1,n_all_at_sp
  themolec(isp)%ispec  = isp
  themolec(isp)%nathis = nabys(isp)
  themolec(isp)%zspec  = zabys(isp)
  allocate(themolec(isp)%coo_xyzbo(5,nabys(isp)))
enddo
rcenmax=zero
do j=1,n_all_at
  read(iuxyz,'(a)')rl_xyz
  call CLEAN_LINE(rl_xyz)
  rl_xyz=trim(adjustl(rl_xyz))
  lrl_xyz=len_trim(rl_xyz)
  syer=rl_xyz(1:2)
  do i=0,n_elements
    IF (syer /= symb_of_Z(i)) CYCLE
    Z_a = i
    EXIT
  enddo
  read(rl_xyz(3:lrl_xyz),*)xyzbol(1:nnu)
  if (islab==1) then
    isp=nint(xyzbol(6))
  else
    do m=1,n_all_at_sp
      if (zabys(m)==Z_a) then
        isp=m
        exit
      endif
    enddo
  endif
  cous(isp)=cous(isp)+1
  xyzbol(1:3)=xyzbol(1:3)-geocen(1:3)
  rcenmax=max(rcenmax,sqrt(sum(xyzbol(1:3)**2)))
  if (nnu==3) then
    themolec(isp)%coo_xyzbo(1:3,cous(isp))=xyzbol(1:nnu)
    themolec(isp)%coo_xyzbo(4:5,cous(isp))=[0.5d0,1.d0]
  else if (nnu==4) then
    themolec(isp)%coo_xyzbo(1:3,cous(isp))=xyzbol(1:3)
    themolec(isp)%coo_xyzbo(4:5,cous(isp))=[0.5d0,xyzbol(4)]
  else if (nnu>=5) then
    themolec(isp)%coo_xyzbo(1:5,cous(isp))=xyzbol(1:5)
  endif
  mami(1,:)=min(mami(1,:),themolec(isp)%coo_xyzbo(1:3,cous(isp)))
  mami(2,:)=max(mami(2,:),themolec(isp)%coo_xyzbo(1:3,cous(isp)))
enddo
close(iuxyz)
deallocate(cous)
madiamol= one+min( sqrt(max(  zero,sum( (mami(2,:)-mami(1,:))**2) )), two*rcenmax )
diamax=madiamol
allocate(n_sp_atom_V(1),n_at_pair_V(1))
n_sp_atom_V(1) = n_all_at_sp
n_at_pair_V(1) = (n_all_at_sp*(n_all_at_sp+1))/2
n_at_pair_glo = n_at_pair_V(1)
n_sp_atom_glo = n_sp_atom_V(1)
call DO_ALLOK()

!open(iuxyz,status='replace',file=fn(1:lfn)//'_INFO')
!________ variables for sampling
call ALLOCALL()

Celty(1)%nat = nabys
forall(isp=1:n_sp_atom_V(1))
  Celty(1)%xnat(isp) = SUM(themolec(isp)%coo_xyzbo(5,1:nabys(isp)))
  Celty(1)%termcon(isp) = SUM(themolec(isp)%coo_xyzbo(5,1:nabys(isp))**2)
end forall
Celty(1)%ndi=0
Celty(1)%Z_at=zabys
kk=0
do k=1,n_sp_atom_V(1)
  do k2=k,n_sp_atom_V(1)
    kk=kk+1
    Celty(1)%zappa(:,kk)=(/k,k2/)
    Celty(1)%minallowdist(kk)=at_radii(zabys(k))+at_radii(zabys(k2)) - max(redmind,zero)
  enddo
enddo

Celty(1)%point_eqpair=0
Celty(1)%point_neqpair=0
kk=0
do k=1,n_at_pair_V(1)
  k1=Celty(1)%zappa(1,k)
  k2=Celty(1)%zappa(2,k)
  if (k1==k2) then
    Celty(1)%point_eqpair(k1)=k
  else
    kk=kk+1
    Celty(1)%point_neqpair(kk)=k
  endif
enddo
Celty(1)%termcon_all(Celty(1)%point_eqpair(:))=Celty(1)%termcon
Celty(1)%termcon_all(Celty(1)%point_neqpair(:))=zero
Celty(1)%termcon_ineq=zero

call ALLOCALL_GLO()
allocate(numcells_byphase(1))
numcells_byphase=one

end subroutine read_XYZ_only
!***********************************************************************************

end module Jmol_XYZfile
!___________________________________________________________________________________________________
module paracry_corr
use nano_deftyp
integer(I4B),parameter :: nGV1=17
real(DP),parameter :: einin=eight/nine,trq=three*unqua
real(DP),save :: sigma_a_sq,sigma_b_sq,corr_coe,sigma_c_sq
real(DP),save :: GV1D(-nGV1:nGV1),GV2D(-nGV1:nGV1,-nGV1:nGV1),GX(-nGV1:nGV1),GY(-nGV1:nGV1), &
                 GRS0(2,0:1,-nGV1:nGV1,-nGV1:nGV1)
real(DP),save :: eigenval_G(2),eigenang_G, avd0sq
real(DP),allocatable,save  :: linwei(:),lind0s(:),lindzs(:)
integer(I4B),allocatable,save  :: linind(:,:)
integer(I4B),save :: nGVlin,flagok(-nGV1:nGV1,-nGV1:nGV1)


contains
!********************************************************************************************
subroutine EIGEN_GAU(vara0,varb0,cocoe)
implicit none
real(DP),intent(IN) :: vara0,varb0,cocoe
integer(I4B) :: i
real(DP) :: cocoe2,p1,p2,ab1,ab2,tau

if (max(vara0,varb0)<=sceps_DP) then
  eigenval_G=zero
  eigenang_G=zero
  return
endif

ab1=abs(cocoe)
ab2=abs(vara0-varb0)
if (ab1<=sceps_DP.and.ab2<=sceps_DP) then
  eigenang_G = zero
  eigenval_G = sqrt(vara0)
else if (ab1>sceps_DP.and.ab2<=sceps_DP) then
  eigenang_G = pi*unqua
  eigenval_G = sqrt([vara0*(one+cocoe),vara0*(one-cocoe)])
else if (ab1>sceps_DP.and.ab2>sceps_DP) then
  cocoe2=cocoe**2
  p1=half*(vara0+varb0)
  p2=half*sqrt( ab2**2+four*cocoe2*vara0*varb0 )
  eigenval_G = sqrt([p1+p2,p1-p2])
  tau = (vara0-varb0)*half/p1
  eigenang_G = half * acos(tau)
else if (ab1<=sceps_DP.and.ab2>sceps_DP) then
  eigenang_G = zero
  eigenval_G = sqrt([vara0,varb0])
endif

end subroutine EIGEN_GAU
!********************************************************************************************
subroutine FILL_GV()
implicit none
integer(I4B) :: i,j,k
real(DP)     :: GV_norm

GV1D = [(exp(-half*unqua*real(i**2,DP)),i=-nGV1,nGV1)]
do i=-nGV1,nGV1
  GV2D(:,i) = GV1D*GV1D(i)
enddo
GX=[(half*real(i,DP),i=-nGV1,nGV1)]
GY=GX
GRS0=zero
!GV_norm = one/sum(GV2D)
!GV2d=GV2d*GV_norm
flagok=0
where(GV2d>sceps_DP) flagok=1
nGVlin = count(flagok==1)
allocate(linwei(nGVlin),lind0s(nGVlin),lindzs(nGVlin)) 
lind0s=zero; lindzs=zero; linwei=zero

k=0
do i=-nGV1,nGV1; do j=-nGV1,nGV1
  if (flagok(i,j)==0) cycle
  k=k+1
  linwei(k)=GV2d(i,j)
enddo;enddo
nGVlin=k
GV_norm = one/sum(linwei(1:k))
linwei(1:k)=GV_norm*linwei(1:k)

end subroutine FILL_GV
!********************************************************************************************
subroutine EST_SD0(xya0,va,vb, broadening_in)
implicit none
real(DP),intent(IN)  :: xya0(3),va,vb
logical,intent(IN)   :: broadening_in
integer(I4B) :: i,j,k
real(DP)     :: steps(2),aux(2),bux(2),ang,rrr1(2),rrr(2),ddd,ddds

if (.not.broadening_in) then
  return
endif
call EIGEN_GAU(va,vb,corr_coe)
ang = eigenang_G-xya0(3)
steps=eigenval_G*half
aux = [ cos(ang), sin(ang)]*steps(1)
bux = [-sin(ang), cos(ang)]*steps(2)

k=0
do j=-nGV1,nGV1
  rrr1=xya0(1:2)+bux*j
  do i=-nGV1,nGV1
  if (flagok(i,j)==0) cycle
    rrr=rrr1+aux*i
    ddds=sum(rrr**2)
    k=k+1
    lind0s(k)=ddds
  enddo
enddo
avd0sq = sum(lind0s(1:nGVlin)*linwei(1:nGVlin))

end subroutine EST_SD0
!********************************************************************************************
subroutine CORREC_SD0(dzsq,cen,wid, broadening_in, d0unc)
implicit none
real(DP),intent(IN)  :: dzsq, d0unc
real(DP),intent(OUT) :: wid, cen
logical,intent(IN)   :: broadening_in
integer(I4B) :: i,j
real(DP)     :: smom(2)

if (.not.broadening_in) then
  cen = d0unc     ! GRS0(1,0,0,0)
  wid = zero
  return
endif

lindzs=lind0s + dzsq
smom(1) = sum(linwei*sqrt(lindzs))
smom(2) = avd0sq+dzsq

cen = trq * (smom(1)+sqrt(smom(1)**2 - einin*smom(2)))
wid = sqrt( unter * ( smom(2) - cen**2 ) )

end subroutine CORREC_SD0
!********************************************************************************************
function variance_rs(r,s,sigmax,xa,yb)
! xa,yb in crystal coord.s
implicit none
real(DP),intent(IN) :: r,s,sigmax,xa,yb
real(DP) :: variance_rs
real(DP) :: axa,ayb,absrL,abssL,sigr,sigs,phars

axa=abs(xa)
ayb=abs(yb)
absrL=log(abs(r))*axa
abssL=log(abs(s))*ayb
sigr=sign(one,r)
sigs=sign(one,s)
phars=modulo( ( (one-sigr)*axa+(one-sigs)*ayb)+sceps_DP, four)-sceps_DP

if (phars>sceps_DP) then
  variance_rs = two*(sigmax**2)*(one-exp(absrL+abssL)*cos(pi_over_2*phars))
else
  variance_rs = two*(sigmax**2)*(one-exp(absrL+abssL))
endif

end function variance_rs
!********************************************************************************************
function radial_variance_2D(phi_xy)
! phi_xy azimutal angle = atan2(y,x) | x,y Cartesian
implicit none
real(DP),intent(IN) :: phi_xy
real(DP) :: radial_variance_2D

radial_variance_2D = half * ((sigma_a_sq+sigma_b_sq)+(sigma_a_sq-sigma_b_sq)*cos(two*phi_xy))
if (abs(corr_coe)>eps_DP) radial_variance_2D = radial_variance_2D &
                                             + sqrt(sigma_a_sq*sigma_b_sq)*corr_coe*sin(two*phi_xy)

end function radial_variance_2D
!********************************************************************************************
function radial_variance_2D_P(d0,cx,cy,sigmax,slo)
! phi_xy azimutal angle = atan2(y,x) | x,y Cartesian
implicit none
real(DP),intent(IN) :: d0,cx,cy,sigmax,slo
real(DP) :: radial_variance_2D_P

radial_variance_2D_P = min(sigmax**2,slo*d0*sin(two*atan2(cy,cx)))

end function radial_variance_2D_P
!********************************************************************************************
function radial_variance_3D(rv2D,u)
! u cos(polar angle) = z/r| x,y,z Cartesian, r= sqrt(x**2+y**2+z**2)
implicit none
real(DP),intent(IN) :: rv2D,u
real(DP) :: radial_variance_3D
real(DP) :: u2

if (abs(u)<eps_DP) then
  radial_variance_3D = rv2D
else
  u2=u**2
  radial_variance_3D = u2*(sigma_c_sq-rv2D)+rv2D
endif

end function radial_variance_3D
!********************************************************************************************

end module paracry_corr
!___________________________________________________________________________________________________
module INPUT_CLUMKFILE
use nano_deftyp
use helpinput
use paper_blood

integer(I4B),allocatable,save :: paolopa(:,:)

 CONTAINS
!********************************************************************************************
subroutine me_Al(fn_clumkMULT,usedlin,name_files,path_cel_files)
!_____________________________ Reads the 1st part (multiple cells specification) of file coshmkQ.ini
implicit none
character(len=*),intent(IN) :: fn_clumkMULT
integer(I4B),intent(OUT)    :: usedlin
character(len=777),intent(OUT) :: name_files,path_cel_files
integer(I4B) :: iu,ll,isp,ity,iac, ias1, ias2,ity2,iac2,iac2i,kcelp,kapa,iii,iii1,idot
character(len=333) :: rl

usedlin=0
iu=find_unit()
open(iu,status='old',action='read',file=trim(adjustl(fn_clumkMULT)))
do
  read(iu,'(a)') rl
  usedlin=usedlin+1
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0) cycle
  if (rl(1:1)=='!') cycle
  exit
enddo
isp=index(rl(1:ll),' ')
read(rl(isp:ll),*) ncelltypes
print*,'ME_AL: Cell types # ',ncelltypes
ncellpairs = ncelltypes*(ncelltypes+1)
ncellpairs=ncellpairs/2
allocate(celfinames(ncelltypes),naspe_cells(ncelltypes),paolopa(2,ncellpairs))
kcelp=0
do ias1=1,ncelltypes
  do ias2=ias1,ncelltypes
    kcelp=kcelp+1
    paolopa(:,kcelp) = [ias1,ias2]
  enddo
enddo

do
  read(iu,'(a)') rl
  usedlin=usedlin+1
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0) cycle
  if (rl(1:1)=='!') cycle
  exit
enddo
isp=index(rl(1:ll),' ')
path_cel_files=trim(adjustl(rl(isp:ll)))

celfinames=''
name_files=''
do ity=1,ncelltypes
  do
    read(iu,'(a)') rl
    usedlin=usedlin+1
    rl=trim(adjustl(rl))
    ll=len_trim(rl)
    if (ll==0) cycle
    if (rl(1:1)=='!') cycle
    exit
  enddo
  celfinames(ity)(:)=rl(1:ll)
  idot=index(rl(1:ll),'.',.true.)
  print*,'ME_AL 1: ',ity,idot,ll,rl(1:ll)
  name_files=trim(trim(name_files)//celfinames(ity)(1:idot-1))
  if (ity<ncelltypes) name_files=trim(trim(name_files)//'_')
  print*,'ME_AL 2: ',ity,idot,ll,trim(name_files)
  do
    read(iu,'(a)') rl
    usedlin=usedlin+1
    rl=trim(adjustl(rl))
    ll=len_trim(rl)
    if (ll==0) cycle
    if (rl(1:1)=='!') cycle
    exit
  enddo
  isp=index(rl(1:ll),' ')
  read(rl(isp:ll),*) naspe_cells(ity)
enddo
do
  read(iu,'(a)') rl
  usedlin=usedlin+1
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0) cycle
  if (rl(1:1)=='!') cycle
  exit
enddo
print*,'ME_AL: Cell species # ',naspe_cells


isp=index(rl(1:ll),' ')
read(rl(isp:ll),*) naspe_all
napair_all=naspe_all*(naspe_all+1)
napair_all=napair_all/2
sumnascel = sum(naspe_cells)
allocate(address_atsp(naspe_all,ncelltypes),return_address_atsp(naspe_all,ncelltypes),&
         glopair(2,napair_all))
address_atsp=0; return_address_atsp=0
do ity=1,ncelltypes
  do iac=1,naspe_cells(ity)
    do
      read(iu,'(a)') rl
      usedlin=usedlin+1
      rl=trim(adjustl(rl))
      ll=len_trim(rl)
      if (ll==0) cycle
      if (rl(1:1)=='!') cycle
      exit
    enddo
    read(rl(1:ll),*) ias1,ias2
    address_atsp(ias1,ity)=ias2
    return_address_atsp(ias2,ity)=ias1
  enddo
enddo
print*,'ME_AL: Cell address # ',address_atsp
glopair=0
kapa=0
do ias1=1,naspe_all
  do ias2=ias1,naspe_all
    kapa=kapa+1
    glopair(:,kapa)=[ias1,ias2]
  enddo
enddo

do
  read(iu,'(a)') rl
  usedlin=usedlin+1
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0) cycle
  if (rl(1:1)=='!') cycle
  if (ll>=4.and.rl(1:4)=='NEXT') exit
enddo
close(iu)

end subroutine me_Al
!********************************************************************************************
subroutine openread_clumkX(clumkfn, rlx,lrlx, itra,nclu,dclu, clusha, parapar,smoothing_width, wlenttmax, &
                           do_allsizes, sample_allsteps, step_base,para_model,occ1, do_the_xyz,nskip, &
                           rlx_2nd,lrlx_2nd, density_gcm3_set, min_latokk_xyz)
implicit none
character(len=*),intent(IN) :: clumkfn
character(len=*),intent(OUT) :: rlx
character(len=*),optional,intent(OUT) :: rlx_2nd
character(len=3),intent(OUT) :: clusha
integer(I4B),optional,intent(IN) :: nskip
integer(I4B),intent(OUT) :: lrlx,itra,nclu(2),step_base
integer(I4B),optional,intent(OUT) :: lrlx_2nd
real(DP),optional,intent(OUT) :: density_gcm3_set, min_latokk_xyz
real(DP), intent(OUT) :: dclu(2),parapar(9),smoothing_width, wlenttmax(2)
logical, intent(OUT)  :: do_allsizes, sample_allsteps, occ1, do_the_xyz
character(8),intent(OUT)  :: para_model

character(132) :: pwd,rl
character(4) :: kwdi
character(3) :: samode
character(32) :: clu2make,rline
real(DP) :: density_gcm3_set1=2.d0, min_latokk_xyz1
integer(I4B) :: iu99,lpwd,ierr,lrl, ias1, ias2, ioa, l1, icanc,ioe, nskip1, isp,irrr
logical  :: is_set=.false.

do_the_xyz = .false.
para_model = 'WelbAnys'
iu99 = FIND_UNIT()
occ1 = .true.
min_latokk_xyz1 = zero

call GET_PWD(pwd=pwd,lpwd=lpwd)
!clumkfn=trim(adjustl(clumkfn))

nskip1=0
if (PRESENT(nskip)) then
  nskip1=nskip
endif
OPEN(UNIT=iu99,status='old', form='formatted',access='sequential', &
   file=pwd(1:lpwd)//trim(clumkfn),action='READ', iostat=ierr)
IF (ierr /=0) THEN
  print*, ' Error opening input file: ', pwd(1:lpwd)//trim(clumkfn)
  STOP
ENDIF
do l1=1,nskip1
  read(iu99,*)
enddo

if (nskip1>0) then
  rlx=''
  lrlx=0
else
  read(iu99,'(a)') rlx
  rlx=trim(adjustl(rlx))
  lrlx=len_trim(rlx)
  if (PRESENT(rlx_2nd).and.PRESENT(lrlx_2nd)) then
    read(iu99,'(a)') rlx_2nd
    rlx_2nd=trim(adjustl(rlx_2nd))
    lrlx_2nd=len_trim(rlx_2nd)
    is_set = .true.
  endif
endif
itra=0; nclu=0; dclu=zero
read(iu99,'(a)')RL
RL=trim(adjustl(RL))
LRL=len_trim(RL)
!if (is_set) then
  itra=0; Nclu=0; Dclu=zero
!else
  if (rl(1:1)=='D') then
    read(RL(2:LRL),*,IOSTAT=ias1)Dclu
    IF (ias1 /= 0) THEN
      read(RL(2:LRL),*)Dclu(1)
      Dclu(2)=Dclu(1)
      if (verbose) print*,'D[2] not read. Set D[2] = D[1]'
    ENDIF
    Dclu=max(Dclu,zero)
    itra=1
  else if (rl(1:1)=='N') then
    read(RL(2:LRL),*,IOSTAT=ias1)Nclu
    IF (ias1 /= 0) THEN
      read(RL(2:LRL),*)Nclu(1)
      Nclu(2)=Nclu(1)
      if (verbose) print*,'N[2] not read. Set N[2] = N[1]'
    ENDIF
     Nclu=max(Nclu,0)
    itra=2
  endif
  if (itra==0) then
    if (is_set) then
      Nclu=0; Dclu=zero
    else
      print*,' ERROR! Could not read (N or D) size - stopping'
      stop 'Could not read (N or D)  size - stopping'
    endif
  endif
!endif

step_base=1
clusha='PAR'
read(iu99,'(a)') rl
rl=trim(adjustl(rl))
lrl=len_trim(rl)
clusha=rl(1:3)
if (is_set) clusha = 'SET'

!defaults
parapar(1:4) = one
parapar(5:9) = zero
do_allsizes = .true.
sample_allsteps = .false.
wlenttmax = [one,160.d0]
smoothing_width=zero
do 
  read(iu99,'(a)',iostat=ioa)rl
  if (ioa/=0) exit
  rl=trim(adjustl(rl))
  lrl=len_trim(rl)
  if (lrl<4) cycle
  if (rl(1:1)=='!') cycle
  icanc=index(rl(1:lrl),'!')
  if (icanc>0) lrl=icanc
  kwdi=rl(1:4)
  if (kwdi=='TODO') then
    clu2make = trim(adjustl(rl(5:lrl)))
    l1=len_trim(clu2make)
    call LOWCASE(clu2make)
    if (clu2make(1:12)=='largest_only') then
      do_allsizes = .false.
    else if (clu2make(1:12)=='all_clusters') then
      do_allsizes = .true.
      if (verbose) print'(a)',trim(clu2make)
      step_base=1
      if (l1>12) then
        read(clu2make(13:l1),*,iostat=ioe) step_base
        if (ioe/=0) then
          step_base=1
        endif
        if (verbose) print*,'step base ', step_base
      endif
    else
      print*,'TODO instructon unclear (valid : all_clusters, largest_only). Stop'
      stop 'TODO instructon unclear (valid : all_clusters, largest_only). Stop'
    endif
  else if (kwdi=='DENS') then
    read(rl(5:lrl),*,iostat=ias1)density_gcm3_set1
    if (ias1/=0) then
      density_gcm3_set1 = 2.d0
    endif
  else if (kwdi=='PARA') then
    read(rl(5:lrl),*,iostat=ias1)parapar
    if (ias1/=0) then
      read(rl(5:lrl),*,iostat=ias2)para_model,parapar
      if (ias2/=0) then
        print*,'PARA instructon unclear (valid : 9 real numbers). Stop'
        stop 'PARA instructon unclear (valid : 9 real numbers). Stop'
      endif
    endif
  else if (kwdi=='SURF') then
    read(rl(5:lrl),*,iostat=ias1)smoothing_width
    if (ias1/=0) then
      print*,'SURF instructon unclear (valid : 1 real number, 0.0 or >). Stop'
      stop 'SURF instructon unclear (valid : 1 real number, 0.0 or >). Stop'
    endif
  else if (kwdi=='OCC1') then
    rline=trim(adjustl(rl(5:)))
    if (((rline(1:1)=='y').or. &
        (rline(1:1)=='Y')).or. &
        (rline(1:1)=='1')) then
      occ1 = .true.
    else if (((rline(1:1)=='n').or. &
        (rline(1:1)=='N')).or. &
        (rline(1:1)=='0')) then
      occ1 = .false.
    endif
  else if (kwdi=='XYZ?') then
    rline=trim(adjustl(rl(5:)))
    if (((rline(1:1)=='y').or. &
        (rline(1:1)=='Y')).or. &
        (rline(1:1)=='1')) then
      do_the_xyz = .true.
    else if (((rline(1:1)=='n').or. &
        (rline(1:1)=='N')).or. &
        (rline(1:1)=='0')) then
      do_the_xyz = .false.
    endif
    isp = index(trim(rline),' ')
    if (isp==0) then
      min_latokk_xyz1=zero
    else
      read(rline(isp+1:),*,iostat=irrr) min_latokk_xyz1
      if (irrr/=0) min_latokk_xyz1 = zero
    endif
  else if (kwdi=='SAMP') then
    rl=trim(adjustl(rl(5:lrl)))
    lrl=len_trim(rl)
    samode=rl(1:3)
    call LOWCASE(samode)
    if (samode=='one') then
      read(rl(4:lrl),*,iostat=ias1)wlenttmax
      if (ias1/=0) then
        print*,'SAMP instructon unclear (valid : "one" followed by wavelength and 2theta_max, "all"). Stop'
        stop 'SAMP instructon unclear (valid : "one" followed by wavelength and 2theta_max, "all"). Stop'
      endif
      sample_allsteps=.false.
    else if (samode=='all') then
      sample_allsteps=.true.
    else
      print*,'SAMP instructon unclear (valid : "one" followed by wavelength and 2theta_max, "all"). Stop'
      stop 'SAMP instructon unclear (valid : "one" followed by wavelength and 2theta_max, "all"). Stop'
    endif
  endif
enddo
close(iu99)
if (PRESENT(density_gcm3_set)) then
  density_gcm3_set=density_gcm3_set1
endif
if (PRESENT(min_latokk_xyz)) then
  min_latokk_xyz=min_latokk_xyz1
endif

end subroutine openread_clumkX

end module INPUT_CLUMKFILE
!___________________________________________________________________________________________________
!___________________________________________________________________________________________________
