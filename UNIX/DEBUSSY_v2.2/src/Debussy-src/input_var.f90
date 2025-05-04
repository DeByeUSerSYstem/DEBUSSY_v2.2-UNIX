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
module input_variables

use special_types

CHARACTER(256)  :: main_name

INTEGER(I4B), TARGET,SAVE    :: NSpecMax = 10
INTEGER(I4B),TARGET,SAVE     :: NPH_DB1 = 0
INTEGER(I4B),TARGET,SAVE     :: NPH_DB2_USED= 0
INTEGER(I4B),TARGET,SAVE     :: NPH_DB3 = 0
INTEGER(I4B),TARGET,SAVE     :: NPH_DB4 = 0
INTEGER(I4B),TARGET,SAVE     :: NPH_DB5 = 0

CHARACTER(5),DIMENSION(:),ALLOCATABLE,TARGET,save   :: TECHNIQUE
CHARACTER(1),DIMENSION(:),ALLOCATABLE,TARGET,save   :: XDATA_TYPE
CHARACTER(1),DIMENSION(:),ALLOCATABLE,TARGET,save   :: RADIATION
CHARACTER(128),DIMENSION(:),ALLOCATABLE,TARGET,save :: DATA_FILENAME, BLANK_FILENAME, STRUCTURE_NAME
CHARACTER(8),DIMENSION(:),ALLOCATABLE,TARGET,save   :: GEOM
CHARACTER(2),DIMENSION(:,:),ALLOCATABLE,TARGET,save :: ATOM

CHARACTER(256),DIMENSION(:),ALLOCATABLE :: PATH_NAME, PARAMETER_FILE, PROTOTYPE_PHA_FILE

CHARACTER(128),TARGET,save :: REFINEMENT_FILE, OUTPUT_FILE

INTEGER(I4B),ALLOCATABLE,TARGET,save    :: N_EVERY(:),MICRO_FLAG(:)
INTEGER(I4B),ALLOCATABLE,TARGET,save    :: NDATA(:),n2read(:),n2read_ab(:,:), n2read_c(:,:)
INTEGER(I4B),ALLOCATABLE,TARGET,save    :: ILAMBDA(:), MONO_POSIT(:), DB_INDEX(:), IND_SPACE_GROUP(:,:), Nsp_at(:), &
                                           Z_ATOM(:,:), ITYPE_DB(:), PARAM_LIM(:), N2USE(:), &
                                           N2USE_ab(:,:), N2USE_c(:,:), NPAIR_AT(:), NATCEL_PER_SPEC(:,:), &
                                           BLANK_NCOMPS(:),POROD_BKG(:)
REAL(CP),ALLOCATABLE,TARGET,save        :: ANGRANGE(:,:),LAMBDAS(:,:),CORR_ABS(:),COBRA(:),WAVE_ERR(:), CELL_P(:,:),POLARIZB(:,:)
REAL(CP), TARGET,save                   :: AMO_DCORR, AMO_DCORR0
REAL(CP), TARGET,save                   :: QMAX_D
LOGICAL,ALLOCATABLE, TARGET,save        :: PROTOTYPING(:)
LOGICAL, TARGET,save                    :: DO_AMORPH=.false.
LOGICAL, TARGET,save                    :: SIM_NODATA=.false.
LOGICAL,ALLOCATABLE, TARGET,save        :: FULLPOL(:)


INTEGER(I4B),target,save  :: SIMUL_FLAG, CALC_FLAG, CALC_RPDF, MAKEFIL_FLAG, NSET, NSTR, NSTR23, NSET_BACK, NAMO=0
INTEGER(I4B),target,save  :: RND_SEED_STATE
!____ Variables for reading 
INTEGER(I4B),allocatable,TARGET,save :: FFORM(:), NSKIP_HEAD(:), NSKIP_FOOT(:), CHEB_NC(:), YOUNG_NC(:)

INTEGER(I4B),DIMENSION(:),ALLOCATABLE,TARGET,save   :: INST_FLAG
!_ 0 :: no IRF
!__ 1 :: Caglioti + XYZ (Fullprof) GFW^2=U *tan^2\theta + V*tan\theta+W ;
!                                  LFW = X * tan\theta + Y / cos\theta + Z, see FullProf
!__ 2 :: Masciocchi/Topas          FW = ha + hb * tan\theta + hc / cos\theta ;
!                                  ETA= lora + lorb * tan\theta + lorc / cos\theta
REAL(CP),DIMENSION(:,:),ALLOCATABLE,TARGET,save                :: INST_6P_var,INST_5P_con
!   In INST_6P_var  : 6 pos. for the classical IRF par.s
!_  Position 7: "axial asym."  (1/(L*cot(2th0))) * Exp[ (2th-2th0)/(L*cot(2th)) ] * Heaviside[2th0-2th] :: L calculated from v.g.p.
!____ INST_5P_con ::
!_  1) "capillary"
!_  2) "wobbling"
!_  3) "pixel"
!_  4) ""
!_  5) ""

TYPE(PHA_INFO),ALLOCATABLE,TARGET  :: ALL_PHA_INFO(:)


end module input_variables
