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
module clean

private
public :: clean_line, LOWCASE,UPCASE, UPCA,LOCA, ISNUMBER, DIREXIST, &
          separator,opsystem,delete_command, ls_command, cp_command, mkdir_command, &
          path_EPDL97, path_SpaceGroups,verbose

character(26),PARAMETER  :: UPCA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
                            LOCA = 'abcdefghijklmnopqrstuvwxyz'
                            
!________ The following lines are in file local_system.inc ________________________________________
!________ If you need, edit them in file local_system.inc _________________________________________
!
!!___ predefined syntax of some system commands
!
!character(1),dimension(3),parameter :: separators=['/','/','\']
!character(7),dimension(3),parameter :: opsystems      = ['MacOSX ', 'Linux  ', 'Windows']
!character(5),dimension(3),parameter :: delete_commands= ['rm -f',   'rm -f',   'del  ']
!character(3),dimension(3),parameter :: ls_commands    = ['ls ',     'ls ',     'dir']
!character(4),dimension(3),parameter :: cp_commands    = ['cp  ',    'cp  ',    'copy']
!character(5),dimension(3),parameter :: mkdir_commands = ['mkdir',   'mkdir',   'md   ']
!
!!_ Specify operating system giving the appropriate value to the variable isystem (below) 
!!_ according to the following scheme:
!!______ 1 = Mac OS X
!!______ 2 = Linux
!!______ 3 = Windows
!
!integer,parameter :: isystem = 1  
!__________________________________________________________________________________________________

                            
include 'local_system.inc'
include 'EPDL97.inc'
! !__AC-RF 03.07.2014
!character(256),save :: path_EPDL97= &
!"../../ext_database/EPDL97/"
!character(256),save :: path_SpaceGroups= &
!"../../ext_database/SpaceGroups/"
! !__

!character(256),save :: path_EPDL97
!character(256),save :: path_SpaceGroups

character(1),parameter :: separator      = separators(isystem)
character(7),parameter :: opsystem       = opsystems(isystem)
character(5),parameter :: delete_command = delete_commands(isystem)
character(3),parameter :: ls_command     = ls_commands(isystem)
character(4),parameter :: cp_command     = cp_commands(isystem)
character(5),parameter :: mkdir_command  = mkdir_commands(isystem)

character(96),PARAMETER :: uchara = ' !'//"'"//'#$%&'//'"'//&
'()'//'*'//'+'//',-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~'
character(10),PARAMETER :: nmbrs='1234567890'
integer       :: chamask(0:127),iacZup,iaczlo
logical       :: init_clean_done

data init_clean_done/.false./

contains

!****************************************************************************************
  subroutine INIT_clean
  implicit none
  INTEGER :: i,j,LL

    chamask = 0

    LL = len_trim(uchara)
    do i=1,LL
      j = IACHAR(uchara(i:i))
      chamask(j) = 1
    enddo
    iacZup = IACHAR('Z'); iaczlo = IACHAR('z')
    init_clean_done = .true.
  end subroutine INIT_clean
!****************************************************************************************
  subroutine CLEAN_line(line)
    implicit none
    character(*),intent(INOUT)  :: line
    integer                     :: ll,i,k
    character(1)                :: aa

    IF (.not.init_clean_done) CALL INIT_clean
    ll = len_trim(line)
    do i=1,ll
      aa = line(i:i)
      k = IACHAR(aa)
      IF (chamask(k) == 0) line(i:i) = ' '
    enddo
  end subroutine CLEAN_line
!****************************************************************************************
  subroutine LOWCASE(a)
    implicit none
    character(*), INTENT(INOUT) :: a
    character(1)             :: b
    integer                  :: i,j,k

    j = len_trim(a)
    do k=1,j
       b = a(k:k)
       i = SCAN(UPCA,b)
       IF (i==0) CYCLE
       a(k:k) = LOCA(i:i)
     enddo

  end subroutine LOWCASE
!****************************************************************************************
  subroutine UPCASE(a)
    implicit none
    character(*), INTENT(INOUT) :: a
    character(1)             :: b
    integer                  :: i,j,k

    j = len_trim(a)
    do k=1,j
       b = a(k:k)
       i = SCAN(LOCA,b)
       IF (i==0) CYCLE
       a(k:k) = UPCA(i:i)
     enddo

  end subroutine UPCASE
!****************************************************************************************
  function ISNUMBER(a)
    implicit none
    character(len=1), INTENT(IN) :: a
    integer                  :: i,j,k
    logical :: isnumber

    isnumber=.false.
    do k=1,10
      if (a(1:1)==nmbrs(k:k)) then
        isnumber=.true.
        exit
      endif
    enddo

  end function ISNUMBER
!****************************************************************************************
  function DIREXIST(adir)
    implicit none
    character(*),intent(IN)  :: adir
    logical                  :: DIREXIST

    inquire(file=trim(adjustl(adir))//separator//'.',exist=DIREXIST)
    
  end function DIREXIST
!****************************************************************************************
function nfields(a)
implicit none
character(len=*),intent(INOUT) :: a
integer(4) :: nfields
integer(4) :: i,ll
character(len=1),dimension(:),allocatable :: b

a = trim(adjustl(a))
ll = len_trim(a)
nfields=1+count([((a(i:i)==' '.and.a(1+i:1+i)/=' '),i=2,ll-1)])

end function nfields
!****************************************************************************************

end module clean
!______________________________________________________________________________
MODULE nano_deftyp
use clean

    IMPLICIT NONE

!Determine the system's precision and range
        INTEGER, PARAMETER :: I8B = SELECTED_INT_KIND(17)
        INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
        INTEGER, PARAMETER :: I2B = SELECTED_INT_KIND(4)
        INTEGER, PARAMETER :: I1B = SELECTED_INT_KIND(2)
        INTEGER, PARAMETER :: SP  = KIND(1.0)
        INTEGER, PARAMETER :: DP  = KIND(1.0D0)
        INTEGER, PARAMETER :: CP  = DP
        INTEGER, PARAMETER :: SPC = KIND((1.0,0.0))
        INTEGER, PARAMETER :: DPC = KIND((1.0D0,1.0D0))
        INTEGER, PARAMETER :: CPC = DPC
        INTEGER, PARAMETER :: LGT = KIND(.true.)
!Flags for array args
type,public :: DBarr0
  integer(I1B) :: gg=0
end type DBarr0
type,public :: DBarr1
  integer(I1B) :: gg=0
end type DBarr1
type,public :: DBarr2
  integer(I1B) :: gg=0
end type DBarr2
type(DBarr0),save :: scal_arg
type(DBarr1),save :: vec1_arg 
type(DBarr2),save :: vec2_arg 

! AC 20.05.2012 begin
integer(I4B),parameter :: MAX_ATO = 10
integer(I4B),dimension(MAX_ATO,MAX_ATO),parameter :: Hot_Stuff=transpose(reshape([&
1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
1, 3, 0, 0, 0, 0, 0, 0, 0, 0, &
1, 4, 6, 0, 0, 0, 0, 0, 0, 0, &
1, 5, 8, 10, 0, 0, 0, 0, 0, 0, &
1, 6, 10, 13, 15, 0, 0, 0, 0, 0, &
1, 7, 12, 16, 19, 21, 0, 0, 0, 0, &
1, 8, 14, 19, 23, 26, 28, 0, 0, 0, &
1, 9, 16, 22, 27, 31, 34, 36, 0, 0, &
1, 10, 18, 25, 31, 36, 40, 43, 45, 0, &
1, 11, 20, 28, 35, 41, 46, 50, 53, 55],[MAX_ATO,MAX_ATO]))
! ! AC 20.05.2012 end

 REAL(CP),parameter ::zero=0.0_CP,one=1.0_CP,oneh=1.5_CP,two=2.0_CP,three=3.0_CP,four=4.0_CP,five=5.0_CP, &
                      six=6.0_CP,seven=7.0_CP,eight=8.0_CP,nine=9.0_CP,ten=10.0_CP,eleven=11.0_CP, &
                      twelve=12.0_CP,thirteen=13.0_CP,fourteen=14.0_CP,fifteen=15.0_CP,sixteen=16.0_CP, &
                      seventeen=17.0_CP,eighteen=18.0_CP,nineteen=19.0_CP,twenty=20.0_CP,hundred=100.0_CP
 REAL(CP), PARAMETER   :: unmez=0.5_CP,half=unmez,unqua=0.25_CP, &
                          unter = 0.333333333333333333333333333333333333_CP, duter = 0.666666666666666666666666666666666667_CP, &
                          quter = 1.33333333333333333333333333333333333_CP, citer = 1.666666666666666666666666666666666667_CP, &
                          unses = 0.166666666666666666666666666666666667_CP,unqui=0.2_CP,uncen=0.01_CP,undec=0.1_CP,unven=0.05_CP,&
                          trdut = 2.08008382305190411453005682435788539_CP, &
                          duunt = 1.25992104989487316476721060727822835_CP, &
                            sr3 = 1.73205080756887729352744634150587237_CP, sr5=2.23606797749978969640917366873127624_CP,&
                            sr2 = 1.41421356237309504880168872420969808_CP,srhalf=.707106781186547524400844362104849040_CP

!Identify the smallest non-zero number and the number of significant figures for SP and DP
    REAL(SP), PARAMETER   ::  tiny_SP     = TINY(1.0_SP)
    REAL(DP), PARAMETER   ::  tiny_DP     = TINY(one)
    REAL(CP), PARAMETER   ::  tiny_CP     = TINY(1.0_CP)
    INTEGER,  PARAMETER   ::  sig_fig_SP  = PRECISION(1.0_SP)
    INTEGER,  PARAMETER   ::  sig_fig_DP  = PRECISION(one)
    INTEGER,  PARAMETER   ::  sig_fig_CP  = PRECISION(1.0_CP)
    REAL(SP), PARAMETER   ::  eps_SP      = epsilon(1.0_SP)
    REAL(DP), PARAMETER   ::  eps_DP      = epsilon(one)
    REAL(CP), PARAMETER   ::  eps_CP      = epsilon(1.0_CP)

!Identify the largest number for SP and DP
    REAL(SP), PARAMETER   ::  biggest_SP  = HUGE(1.0_SP)
    REAL(DP), PARAMETER   ::  biggest_DP  = HUGE(one)
    REAL(CP), PARAMETER   ::  biggest_CP  = HUGE(1.0_CP)
    REAL(DP), PARAMETER   ::  radix_DP  = real(radix(1.0_DP),DP)
    
    INTEGER(I1B),PARAMETER   ::  biggest_I1B  = HUGE(1_I1B)
    INTEGER(I2B),PARAMETER   ::  biggest_I2B  = HUGE(1_I2B)
    INTEGER(I4B),PARAMETER   ::  biggest_I4B  = HUGE(1_I4B)

!Values related to pi and e
    REAL(CP), PARAMETER   ::  pi          = 3.14159265358979323846264338327950288419716939937510_CP
    REAL(CP), PARAMETER   ::  two_pi      = 2.0_CP*pi
    REAL(CP), PARAMETER   ::  pi2         = 2.0_CP*pi
    REAL(CP), PARAMETER   ::  pisqa       = Pi*Pi
    REAL(CP), PARAMETER   ::  twoonpi     = 2.0_CP/pi
    REAL(CP), PARAMETER   ::  sqPi2 = 2.50662827463100050241576528481104525_CP
    REAL(CP), parameter   ::  chczz= 0.9189385332046727417803297_CP
    REAL(CP), PARAMETER   ::  pi2sqrt     = sqPi2
    REAL(CP), PARAMETER   ::  four_pi     = 4.0_CP*pi
    REAL(CP), PARAMETER   ::  four_pi_o3  = four_pi/3.0_CP
    REAL(CP), PARAMETER   ::  pi_over_2   = pi*half
    REAL(CP), PARAMETER   ::  invsqrt2Pi = .398942280401432677939946059934381870_CP
    REAL(CP), PARAMETER   ::  logarPi    = 1.14472988584940017414342735135305871_CP
    REAL(CP), PARAMETER   ::  natural_e   = 2.71828182845904523536028747135266249775724709369995_CP

!Conversions for radians to degrees and degrees to radians
    REAL(CP), PARAMETER   ::  degrees_to_radians = pi/180.0_CP
    REAL(CP), PARAMETER   ::  radians_to_degrees = 180.0_CP/pi
    REAL(CP), PARAMETER   ::  duet2r             = pi/360.0_CP
    REAL(CP), PARAMETER   ::  dueterPi = 2.09439510239319549230842892218633525d0
    REAL(CP), PARAMETER   ::  sqrt_of_pi=1.77245385090551602729816748334114518_CP, sqrtPi=sqrt_of_pi

    REAL(CP),parameter      ::  cr2th=.174532925199432957692369076848861271d-1, & ! this is Pi/180
                                cr1th=.872664625997164788461845384244306356d-2 ! this is Pi/360

!Various numerical constants for the program
 REAL(CP), PARAMETER :: sd_conv(4) = (/0.583772162470367996204712426850884949_CP, &
                                      0.598201023383066038612927144727213727_CP, &
                                      0.368822646686040336119084682224115555_CP, &
                                      0.368822646686040336119084682224115555_CP/)
 !REAL(CP),PARAMETER     :: skip_tail = 1.0d-3,valargerfc = 2.1851242191330042657059596475098423_CP
 !REAL(CP),PARAMETER     :: skip_tail = 5.0d-3,valargerfc = 1.82138636771844967304021031862099524_CP
 REAL(CP), PARAMETER     :: skip_tail = 1.0d-2,valargerfc = 1.64497635713318705017720343524951162_CP
 REAL(CP), PARAMETER   :: sssss = 0.707106781186547524400844362104849039_CP, &
                          sqrt2log2=1.17741002251547469101156932645969964_CP, &
                          tolarg1 = 1.01_CP*valargerfc, tolarg2 = 1.001_CP*valargerfc, &
                      logar2=.693147180559945309417232121458176568_CP,logar10=2.30258509299404568401799145468436421_CP
 REAL(DP), PARAMETER   :: D1MACH(5) = (/tiny_DP,biggest_DP,half*eps_DP,eps_DP,log10(radix_DP)/)

  REAL(DP),parameter            :: minisinc = 0.0_DP!0.2172336282112216574_DP

!Physical constants (in SI units unless otherwise indicated)
    REAL(CP), PARAMETER   ::  G_gc        = 6.673e-11_CP                              !Universal gravitational constant (N m^2/kg^2)
    REAL(CP), PARAMETER   ::  c_sl        = 2.99792458e08_CP                          !Speed of light (m/s)
    REAL(CP), PARAMETER   ::  h_Pc        = 6.62606876e-34_CP                         !Planck's constant (J s)
    REAL(CP), PARAMETER   ::  hbar        = h_Pc/two_pi                               !hbar (J s)
    REAL(CP), PARAMETER   ::  k_B         = 1.3806503e-23_CP                          !Boltzmann's constant (J/K)
    REAL(CP), PARAMETER   ::  sigma_SB    = 2*pi**5*k_B**4/(15*c_sl**2*h_Pc**3)       !Stefan-Boltzmann constant (J/m^2/s/K^4)
    REAL(CP), PARAMETER   ::  a_rad       = 4*sigma_SB/c_sl                           !radiation constant (J/m^3/K)
    REAL(CP), PARAMETER   ::  a_rad_o3    = a_rad/3                                   !a/3 (J/m^3/K)
    REAL(CP), PARAMETER   ::  four_ac_o3  = 4*a_rad_o3*c_sl                           !4*ac/3 (J/m^2/s/K^4)
    REAL(CP), PARAMETER   ::  m_p         = 1.67262158e-27_CP                         !mass of the proton (kg)
    REAL(CP), PARAMETER   ::  m_n         = 1.67492716e-27_CP                         !mass of the neutron (kg)
    REAL(CP), PARAMETER   ::  m_e         = 9.1093818897e-31_CP                       !mass of the electron (kg)
    REAL(CP), PARAMETER   ::  m_H         = 1.673532499e-27_CP                        !mass of the hydrogen atom (kg)
    REAL(CP), PARAMETER   ::  amu         = 1.66053873e-27_CP                         !one atomic mass unit (kg)
    REAL(CP), PARAMETER   ::  e_cgs       = 4.803204197e-10_CP                        !charge on the electron (in esu)
    REAL(CP), PARAMETER   ::  e_SI        = 1.602176462e-19_CP                        !charge on the electron (in coulombs)
    REAL(CP), PARAMETER   ::  eV          = 1.602176462e-19_CP                      !one electron volt (J)
    REAL(CP), PARAMETER   ::  keV         = eV*1.0e3_CP                               !one keV (J)
    REAL(CP), PARAMETER   ::  MeV         = eV*1.0e6_CP                               !one MeV (J)
    REAL(CP), PARAMETER   ::  GeV         = eV*1.0e9_CP                               !one GeV (J)
    REAL(CP), PARAMETER   ::  N_A         = 6.02214199e23_CP                          !Avagadro's number
    REAL(CP), PARAMETER   ::  R_gas       = 8.314472_CP                               !Gas constant (J/mol/K)

!Time constants
    REAL(CP), PARAMETER   ::  hr          = 3600.0_CP                                 !hour (in s)
    REAL(CP), PARAMETER   ::  day         = 24.0_CP*hr                                !Solar day (in s)
    REAL(CP), PARAMETER   ::  yr          = 3.155815e7_CP                             !Sidereal year (in s)
    REAL(CP), PARAMETER   ::  J_yr        = 365.25_CP*day                             !Julian year (in s)

!Astronomical length constants (in SI units)
    REAL(CP), PARAMETER   ::  AU          = 1.4959787066e11_CP                        !Astronomical unit (m)
    REAL(CP), PARAMETER   ::  pc          = 206264.806_CP*AU                          !Parsec (m)
    REAL(CP), PARAMETER   ::  ly          = c_sl*J_yr                                 !Light Year (Julian; m)

!Astronomical constants (in SI units)
    REAL(CP), PARAMETER   ::  M_Sun       = 1.9891e30_CP                              !Mass of the Sun (kg)
    REAL(CP), PARAMETER   ::  R_Sun       = 6.95508e8_CP                              !Radius of the Sun (m)
    REAL(CP), PARAMETER   ::  S_Sun       = 1.365e3_CP                                !Solar irradiance (J/m^2/s)
    REAL(CP), PARAMETER   ::  L_Sun       = four_pi*AU**2*S_Sun                       !Luminosity of the Sun (W)
    REAL(CP), PARAMETER   ::  Te_Sun      = (L_Sun/(four_pi*R_Sun**2*sigma_SB))**(1/4)!Effective temperature of the Sun (K)
    REAL(CP), PARAMETER   ::  M_Earth     = 5.9736e24_CP                              !Mass of Earth (kg)
    REAL(CP), PARAMETER   ::  R_Earth     = 6.378136e6_CP                             !Radius of Earth (m)
    
!________________ X-ray parameters
real(8),parameter :: HPlanck=h_Pc, clight=c_sl, oneeV=eV, KeV_to_Angstroem=HPlanck*clight*1.d7/oneeV, &
                     mc2elec_eV = m_e*c_sl*c_sl/oneeV, epsilon0 = 8.854187817d-12, &
                     class_el_radius = (e_SI*e_SI)/(m_e*c_sl*c_sl*four*Pi*epsilon0)
    

!_________ Uncomment the following for Portland buggy compiler
!    REAL(DP),save         ::  sceps_SP    = 1.0e-3
!    REAL(DP),save           ::  sceps_DP    = 1.0d-8
!    REAL(DP),save           ::  xsmall=0.58e-8_DP
!    real(DP),save           ::  XLARGE      = 67108864.0_DP
!    REAL(DP),save           ::  sceps_CP    = 1.0d-8
!    REAL(DP),save           ::  s4eps_SP    = 0.03
!    REAL(DP),save           ::  s4eps_DP    =1.0d-4
!    REAL(CP),save           ::  s4eps_CP    =1.0e-4_CP
!    REAL(DP),save           ::  log2epsi    = 52.0_DP
!    REAL(DP),save           ::  ln2eps      = -52.0_DP
!    REAL(DP),save           ::  lneps       = -36.0436533891171560896960703158251815_DP
!    REAL(DP),save           ::  rmaxreal    = 0.5e+154_DP
!    REAL(DP),save           ::  RMAXEXP     = 708.503061461606_DP
!    REAL(DP),save           ::  RMAXGONI    = 3.53711887601422e+15_DP
!_________ Uncomment the former for Portland buggy compiler
!_________ Comment the following for Portland buggy compiler
REAL(DP),parameter :: sceps_SP    = eps_SP**(half)
REAL(DP),parameter :: sceps_DP    = eps_DP**(half)
REAL(DP),parameter :: xlarge      = one/sceps_DP
REAL(DP),parameter :: sceps_CP    = eps_CP**(half)
REAL(DP),parameter :: s4eps_SP    = eps_SP**(0.25_DP)
REAL(DP),parameter :: s4eps_DP    = eps_DP**(0.25_DP)
REAL(DP),parameter :: s4eps_CP    = eps_CP**(0.25_DP)
REAL(DP),parameter :: rmaxreal  = biggest_DP**half
REAL(DP),parameter :: RMAXEXP  = log(biggest_DP) - logar2
REAL(DP),parameter :: RMAXGONI = one/eps_DP
REAL(DP),parameter :: lneps= log(eps_DP)
REAL(DP),parameter :: ln2eps=lneps/logar2
REAL(DP),parameter :: log2epsi = -ln2eps
REAL(DP),parameter :: xsmall=sqrt(oneh*eps_DP)/Pi
!_________ Comment the former for Portland buggy compiler
    

  CONTAINS

SUBROUTINE def_eps!_______ Added to overcome some compiler's stypsis when dealing with parameters
!_________ Uncomment the following for Portland buggy compiler
!   sceps_SP    = eps_SP**(half)
!   sceps_DP    = eps_DP**(half)
!   xlarge      = one/sceps_DP
!   sceps_CP    = eps_CP**(half)
!   s4eps_SP    = eps_SP**(0.25_DP)
!   s4eps_DP    = eps_DP**(0.25_DP)
!   s4eps_CP    = eps_CP**(0.25_DP)
!   rmaxreal  = biggest_DP**half
!   RMAXEXP  = log(biggest_DP) - logar2
!   RMAXGONI = one/eps_DP
!   lneps= log(eps_DP)
!   ln2eps=lneps/logar2
!   log2epsi = -ln2eps
!   xsmall=sqrt(oneh*eps_DP)/Pi
!_________ Uncomment the former for Portland buggy compiler
END SUBROUTINE def_eps
!**********************************************
FUNCTION FIND_UNIT()

  INTEGER(I4B)  :: iu
  INTEGER(I4B)  :: FIND_UNIT
  LOGICAL       :: busy_unit

!!!!! Select a free unit
  iuloop:do iu=10,99
    inquire(UNIT=iu, OPENED=busy_unit)
    IF (.not.(busy_unit)) THEN
       FIND_UNIT = iu
       EXIT iuloop
    ENDIF
  end do iuloop

END FUNCTION FIND_UNIT
!**********************************************
subroutine MAKE_MASK(a,b)
  implicit none
  integer(I4B),intent(IN),dimension(:)    :: a
  integer(I4B),intent(OUT),dimension(:)   :: b
  integer(I4B)  :: n,i,k

  n = SIZE(a)
  ! IF (n /= size(b)) STOP 'MAKE_MASK: dim.s'
  k=0
  b=0
  do i=1,n
    IF (a(i) == 0) CYCLE
    k=k+1
    b(k) = i
  enddo
end subroutine MAKE_MASK
!**********************************************
Function SMP_2_INDICES(smp_file)
implicit none 
character(132),intent(IN) :: smp_file 
integer(I4B)              :: k,l, ll,firstc,lastc
integer,dimension(2) :: SMP_2_INDICES

ll = len_trim(smp_file)
!firstc = SCAN(smp_file(1:ll),'_a')-1
lastc = INDEX(smp_file(1:ll),'.')-1
if (lastc <= 0) then
    STOP 'ERROR in SMP_2_INDICES'
else
    read(smp_file(ll-7:ll-4),'(i3)') l
    read(smp_file(ll-12:ll-9),'(i3)') k
endif


SMP_2_INDICES(1) = k
SMP_2_INDICES(2) = l

end Function SMP_2_INDICES
!**********************************************
Function ONE_FM_TWO(k,l,n1,n2) 
implicit none 
integer,intent(IN) :: k,l,n1,n2 
integer ONE_FM_TWO 

! ONE_FM_TWO = (l-1)*n1+k !ERR
ONE_FM_TWO = l*n1-k
IF(modulo(ONE_FM_TWO,n1) == 0) ONE_FM_TWO = n1*l

end Function ONE_FM_TWO 
!**********************************************
Function TWO_FM_ONE(J,n1,n2) 
implicit none 
integer,intent(IN) :: J,n1,n2 
integer,dimension(2) :: TWO_FM_ONE 

TWO_FM_ONE(2) = J/n1+min(1,modulo(J,n1))
!TWO_FM_ONE(1) = J-TWO_FM_ONE(2)*n1
TWO_FM_ONE(1) = TWO_FM_ONE(2)*n1-J
IF(TWO_FM_ONE(1) == 0) TWO_FM_ONE(1) = n1

end Function TWO_FM_ONE
!**********************************************

! ND 2024 - randomize seed based on a state integer + mod seed randomization
subroutine init_rnd_seed(state)
  implicit none
  integer(I4B), intent(inout) ::  state
  integer(I4B), allocatable ::  seed(:)
  integer(I4B)  ::  dt_seed(8), s_size, i, num

  ! initialize the seed
  call random_seed(size=s_size)
  if(allocated(seed)) deallocate(seed)
  allocate(seed(1:s_size))
  call random_seed(get=seed)

  ! the seed is generated starting from a state integer
  ! if the state integer is zero, auto-generate it
  if (state .eq. 0) then
    call date_and_time(values=dt_seed)
    state = dt_seed(8)*dt_seed(7)*dt_seed(6) ! milliseconds, seconds, minutes
    state = ieor(state, int(getpid(), kind(state))) ! combine to pid in case of parallel exec
  end if

  ! increase entropy generating random number starting from the state
  num = state
  do i = lbound(seed, 1), ubound(seed, 1)
    num = mod(8121*num+28411, 13445684) ! mod randomization
    seed(i) = num
  end do

  ! put the generated seed
  call random_seed(put=seed)
end subroutine


END MODULE nano_deftyp
!______________________________________________________________________________
!
module HELPINPUT
use nano_deftyp
contains

subroutine READNEXT(iunit,istop,ifail,rlin,lrlin)
implicit none
integer(I4B),intent(IN) :: iunit,istop
integer(I4B),intent(OUT) :: ifail,lrlin
character(len=*),intent(OUT) :: rlin
character(len=512) :: arl
integer(I4B) :: ll,io
ifail=0
rlin=''
do
  read(iunit,'(a)', end=10,iostat=io) arl
  if (io/=0) then
    ifail=1
    if (istop==1) then
      print*,'Input error - file ended prematurely! Stopping...'
      stop 'Input error - file ended prematurely! Stopping...'
    else
      print*,'Input error - file ended prematurely! Continuing...'
      return
    endif
  endif
  ifail=0
  call CLEAN_LINE(arl)
  rlin=trim(adjustl(arl))
  lrlin=len_trim(rlin)
  if (lrlin==0) cycle
  if (lrlin>0) exit
enddo
10 continue

end subroutine READNEXT
!******************************************************
subroutine READNEXT_COMM(comms,iunit,istop, ifail,rlin,lrlin)
implicit none
integer(I4B),intent(IN) :: iunit,istop
character(len=*),intent(IN) :: comms
integer(I4B),intent(OUT) :: ifail,lrlin
character(len=*),intent(OUT) :: rlin
character(len=512) :: arl,fna
character(len=7) :: isseq
integer(I4B) :: ll,io,lcomms,jcomms,iscomm
logical :: exf

lcomms=len_trim(comms)
!print*,comms
!print*,lcomms
inquire(iunit,exist=exf)
if (.not.exf) then
  print*,'file not existing'
  stop 'file not existing'
endif
inquire(iunit,opened=exf)
if (.not.exf) then
  print*,'file not open'
  stop 'file not open'
endif
inquire(iunit,sequential=isseq)
if (trim(isseq)/='YES') then
  print*,'file not sequential',isseq
  stop 'file not sequential'
endif
inquire(iunit,read=isseq)
if (trim(isseq)/='YES') then
  print*,'file cannot be read',isseq
  stop 'file cannot be read'
endif
inquire(iunit,name=fna)
!print*,'Unit is ',iunit
!print'(a)','File is '//trim(fna)
ifail=0
rlin=''; lrlin=0
mainr:do
  arl=''
  read(iunit,'(a)', iostat=io) arl
!  print*,io,arl
  if (io/=0) then
    ifail=1
!    print*,io,arl
    if (istop==1) then
      print*,'Input error - file ended prematurely! Stopping...'
      stop 'Input error - file ended prematurely! Stopping...'
    else
      print*,'Input error - file ended prematurely! Continuing...'
      return
    endif
    exit mainr
  endif
  
  ifail=0
  call CLEAN_LINE(arl)
  arl=trim(adjustl(arl))
  ll=len_trim(arl)
  iscomm=0
  do jcomms=1,lcomms
    if (arl(1:1)==comms(jcomms:jcomms)) then
      iscomm=1
      cycle mainr
    endif
  enddo
  if (iscomm==1) then
!    print*,'Comment line'
    cycle mainr
  endif
  iscomm=scan(arl(1:ll),'!')
  if (iscomm==0) iscomm=ll+1
  rlin=arl(1:iscomm-1)
  lrlin=len_trim(rlin)
  if (lrlin==0) cycle mainr
  if (lrlin>0) exit mainr
enddo mainr

end subroutine READNEXT_COMM
!******************************************************
subroutine GET_PWD(pwd,lpwd)
implicit none
include 'local_system.inc'
character(LEN=*),intent(INOUT) :: pwd
integer(I4B),intent(OUT)       :: lpwd
integer(I4B) :: iupwd

pwd=''
if (isystem==3) then
  call system('cd > tmp.pwd')
 else
  call system('pwd > tmp.pwd')
endif
iupwd=find_unit()
open(iupwd,status='old',action='read',file='tmp.pwd')
read(iupwd,'(a)')pwd
pwd=trim(adjustl(pwd))
lpwd=len_trim(pwd)
if (pwd(lpwd:lpwd)/=separator) then
  lpwd=lpwd+1
  pwd(lpwd:lpwd)=separator
endif
close(iupwd)
call system(trim(delete_command)//' tmp.pwd')

end subroutine GET_PWD
!******************************************************

end module HELPINPUT
!_______________________________________________________________________
module Silicon_Darwin
use nano_deftyp
real(DP),parameter :: conttshifdeg = -0.09710734114121289d0, & ! correction to theta :: theta_Bragg = theta_eff + conttshifdeg
                      condarwiddeg =  0.1839654239095608d0,  &
                      FWHM2sigma   = 0.42466090014400952136075141705144481d0, &
                      aSicry = 5.43095d0

contains
!****************************************************************************
function dtt_refrac_Si111_E(EkeV)
implicit none
real(DP),intent(IN) :: EkeV
real(DP) :: dtt_refrac_Si111_E

dtt_refrac_Si111_E = conttshifdeg/(EkeV**2)

end function dtt_refrac_Si111_E
!****************************************************************************
function darwid_FWHM_Si111_E(EkeV)
implicit none
real(DP),intent(IN) :: EkeV
real(DP) :: darwid_FWHM_Si111_E

darwid_FWHM_Si111_E = condarwiddeg/(EkeV**2)

end function darwid_FWHM_Si111_E
!****************************************************************************
function darwid_sigma_Si111_E(EkeV)
implicit none
real(DP),intent(IN) :: EkeV
real(DP) :: darwid_sigma_Si111_E

darwid_sigma_Si111_E = darwid_FWHM_Si111_E(EkeV)*FWHM2sigma

end function darwid_sigma_Si111_E
!****************************************************************************
function darwid_sigma_Si111_wl(wlen)
implicit none
real(DP),intent(IN) :: wlen
real(DP) :: darwid_sigma_Si111_wl

darwid_sigma_Si111_wl = darwid_FWHM_Si111_E(KeV_to_Angstroem/wlen)*FWHM2sigma

end function darwid_sigma_Si111_wl
!****************************************************************************
function Theta_of_E(EkeV)
implicit none
real(DP),intent(IN) :: EkeV
real(DP) :: Theta_of_E(3)

Theta_of_E(1) = KeV_to_Angstroem/EkeV
Theta_of_E(2) = radians_to_degrees * ASIN(sr3*half*Theta_of_E(1)/aSicry)
Theta_of_E(3) = Theta_of_E(2) - dtt_refrac_Si111_E(EkeV) 

end function Theta_of_E
!****************************************************************************
end module Silicon_Darwin
!_______________________________________________________________________________

