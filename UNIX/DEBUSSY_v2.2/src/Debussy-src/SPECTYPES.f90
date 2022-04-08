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
module SPECIAL_TYPES
 use nano_deftyp

  real(DP),parameter  :: Upscale_Par = 100.d0, Downscale_Par = one/Upscale_Par

  type,public :: param_type
     INTEGER(I4B),DIMENSION(:),POINTER         :: REF_NOLIN => NULL()
  END TYPE param_type

  type,public :: expand_addr
     INTEGER(I4B)        :: kstr=0, kset=0, which_type=0, nup=0
    !! which_type = 0  :: global parameter (non-linear)
    !! which_type = 1  :: phase parameter (non-linear)
    !! which_type = 2  :: SETscal (kset)
    !! which_type = 3  :: STRscal (kstr)
    !! which_type = 4  :: AMOscal (nup)
    !! which_type = 5  :: BKGscal (kset,kstr,nup)
  END TYPE expand_addr

  type,public :: ref_strat
     INTEGER(I4B)                           :: stage_n
     CHARACTER(1)                           :: stage_type
     type(param_type),DIMENSION(:),POINTER  :: PARAM_SET => NULL()
     INTEGER(I4B)                           :: REF_AMO
     INTEGER(I4B), DIMENSION(:),POINTER     :: REF_BACK => NULL()
     INTEGER(I4B)                           :: stgpar_NL,stgpar_LI,stgpar_all
     INTEGER(I4B), DIMENSION(:),POINTER     :: long_flag => NULL()
     INTEGER(I4B), DIMENSION(:),POINTER     :: long_mask => NULL()
     REAL(CP)                               :: main_crit
     type(expand_addr),DIMENSION(:),POINTER :: back_flag => NULL()

  END TYPE ref_strat

  type,public :: bivec
     REAL(CP),DIMENSION(:),POINTER             :: BU => NULL()
  END TYPE bivec

  type,public :: trivec
     REAL(CP),DIMENSION(:,:),POINTER           :: TU => NULL()
  END TYPE trivec

  TYPE,PUBLIC     :: data_SETS
     integer(I4B) :: num_data
     real(DP) :: step_data_t2
     REAL(CP),DIMENSION(:,:),POINTER         :: qdata => NULL()
     REAL(CP),DIMENSION(:),POINTER           :: vdata => NULL()
     REAL(CP),DIMENSION(:),POINTER           :: wdata => NULL()
     REAL(CP),DIMENSION(:),POINTER           :: t2data => NULL()
  END TYPE data_SETS

  TYPE,PUBLIC     :: Param_VA
     CHARACTER(5),DIMENSION(:),POINTER       :: NamePar => NULL()
     REAL(CP),DIMENSION(:,:),POINTER         :: PhasePar => NULL()
     CHARACTER(5),DIMENSION(:),POINTER       :: NameAto => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: flag_ref => NULL()
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: law_refOB => NULL()
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: flag_refOB => NULL()
     INTEGER(I4B)                            :: npar_ref
     INTEGER(I4B)                            :: strain_cod
     INTEGER(I4B)                            :: strain_n1
  END TYPE Param_VA

  TYPE,PUBLIC     :: Coord_AT
     REAL(CP),DIMENSION(:,:,:),POINTER       :: coo_prim
     REAL(CP),DIMENSION(:),POINTER           :: Start_par
     INTEGER(I4B)                            :: Nat_tot, Nat_prim, Nat_spec, Npar_free
     INTEGER(I4B),DIMENSION(:),POINTER       :: Z_at
     INTEGER(I4B),DIMENSION(3,3)             :: Mat_spg
!     CHARACTER(2)                            :: Pears_symb
  END TYPE Coord_AT

 TYPE(Param_VA),DIMENSION(:),ALLOCATABLE    :: Param_data

  TYPE,PUBLIC     :: nanoiav
     INTEGER(I4B)                            :: esse,niav,natclu,numspat=1,numpair=1, n_subsampl,&
                                                nrpha,ngdim,celcen,natspglo
     REAL(CP)                                :: rho,delta,cnorg,widg,qtop, clu_mass, mu_subsampl,&
                                                celvol,celvolr
     REAL(DP),DIMENSION(6)                   :: abcabg_db
     REAL(CP),DIMENSION(:,:),POINTER         :: pseudomult => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: xnat => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: termcon => NULL()
     
     REAL(DP),DIMENSION(:),POINTER           :: size_abx => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: act_diam => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: ncelpha => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: specZ => NULL()
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: specN => NULL()
     REAL(DP),DIMENSION(:,:),POINTER         :: specXN => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: specind => NULL()
     
     REAL(DP),DIMENSION(:),POINTER           :: occupair => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: occusite => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: DebyeWallerB => NULL()
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: occumatr => NULL()
     
     REAL(DP),DIMENSION(:),POINTER           :: termcon_allP => NULL()
!     INTEGER(I4B),DIMENSION(:),POINTER       :: poi_eq => NULL()
  
     INTEGER(I4B),DIMENSION(:),POINTER       :: Z_at  => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: nat  => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: ndi  => NULL()
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: zappa  => NULL()
     REAL(DP),DIMENSION(:),POINTER           :: summul => NULL()
  END TYPE nanoiav

  TYPE,PUBLIC     :: nanoiav_mult
     INTEGER(I4B)                            :: dimstruk=0
     INTEGER(I4B)                            :: dimstruk1=0,dimstruk2=0
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: post_office => NULL()   ! addressing from 2 indices to 1
     INTEGER(I4B),DIMENSION(:,:),POINTER     :: post_office_I => NULL() ! addressing from 1 index to 2
     TYPE(nanoiav),DIMENSION(:),POINTER      :: struk => NULL()
  END TYPE nanoiav_mult


  TYPE,PUBLIC     :: PHA_INFO
     CHARACTER(132)                          :: PHA_FILE                        ! keep
     LOGICAL                                 :: skipme=.true.                   
     CHARACTER(12)                           :: SPAGRO
     CHARACTER(3)                            :: clushape
     INTEGER(I4B)                            :: numat_asy_cell, numat_dwa
     REAL(CP)                                :: cepa(6), diamMAX, acell_PROT
     REAL(CP),DIMENSION(:,:),POINTER         :: pha_xyzbo => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: pha_Z => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: pha_Ion => NULL()
     INTEGER(I4B),DIMENSION(:),POINTER       :: pha_MASSNUMBER => NULL()
     CHARACTER(2),DIMENSION(:),POINTER       :: pha_SYMB => NULL()
     REAL(CP),DIMENSION(:),POINTER           :: pha_PAIR_averBTH => NULL()
     REAL(CP),DIMENSION(:),POINTER           :: pha_PAIR_prodOKK => NULL()
  END TYPE PHA_INFO

  TYPE,PUBLIC     :: PARAM_INFO
     LOGICAL                                 :: extform, varallB, varallO
     INTEGER(I4B)                            :: N_strain_func,N_B_func
  END TYPE PARAM_INFO

!****************
CONTAINS
!****************

 SUBROUTINE destroy_iav(rp,i,ii)
   INTEGER(I4B),INTENT(IN)                           :: i,ii
   TYPE(nanoiav),DIMENSION(i,ii),INTENT(INOUT)       :: rp
   INTEGER(I4B)                                      :: astat

   call destroy_iavS(rp(i,ii))

 END SUBROUTINE destroy_iav

 SUBROUTINE destroy_iavS(rp)
   TYPE(nanoiav),INTENT(INOUT)       :: rp
   INTEGER(I4B)                      :: astat

   astat=0
   if (associated(rp%pseudomult)) nullify(rp%pseudomult)
   if (associated(rp%xnat)) nullify(rp%xnat)
   if (associated(rp%termcon)) nullify(rp%termcon)
   if (associated(rp%termcon_allP)) nullify(rp%termcon_allP)
   if (associated(rp%occupair)) nullify(rp%occupair)
   if (associated(rp%DebyeWallerB)) nullify(rp%DebyeWallerB)
   if (associated(rp%occusite)) nullify(rp%occusite)
   if (associated(rp%occumatr)) nullify(rp%occumatr)
!   if (associated(rp%poi_eq)) nullify(rp%poi_eq)
   if (associated(rp%Z_at)) nullify(rp%Z_at)
   if (associated(rp%nat)) nullify(rp%nat)
   if (associated(rp%ndi)) nullify(rp%ndi)
   if (associated(rp%zappa)) nullify(rp%zappa)
   if (associated(rp%summul)) nullify(rp%summul)

 END SUBROUTINE destroy_iavS
!*********************************************************************** 
 function fill_poi_eq(N,Np)
 implicit none
 integer(I4B),intent(IN) :: N,Np
 integer(I4B) :: i1,i2,k
 integer(I4B),dimension(Np) :: fill_poi_eq
 k=0
 do i1=1,N
   do i2=i1,N
     k=k+1
     if (i1==i2) fill_poi_eq(i1)=k
   enddo
 enddo
 end function fill_poi_eq
 
end module SPECIAL_TYPES
!______________________________________________________________________________
