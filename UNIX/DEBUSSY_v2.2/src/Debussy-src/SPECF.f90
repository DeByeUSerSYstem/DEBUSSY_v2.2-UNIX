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
module use_libcerf
! interfaces for the libcerf.* c library of error functions
! http://apps.jcns.fz-juelich.de/libcerf
! 
use ISO_C_BINDING
use nano_deftyp
private
public :: Voigt_F, w_of_z_F, cdawson_F, Erfcx_F, Erfi_F, Im_w_of_x_F, &
          Dawson_F, cerf_F, cerfc_F, cerfcx_F, cerfi_F
! - - - - - - - - - - - - 
INTERFACE
FUNCTION voigt ( x, sigma, gamma ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
! voigt(x,sigma,gamma) = \int_|R G(t,sigma) L(x-t,gamma) dt
! WITH: G(x,sigma) = (1/(sqrt(2*pi)*|sigma|)) * exp(-x**2/(2*sigma**2)) ;
!       L(x,gamma) = |gamma| / (pi * ( x**2 + gamma**2 )) .
real(C_DOUBLE),intent(IN),VALUE :: x, sigma, gamma
real(C_DOUBLE) :: voigt
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION cerf ( z ) bind(c)
!  cerf(z) = erf(z)
USE ISO_C_BINDING
IMPLICIT NONE
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: cerf
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION cerfc ( z ) bind(c)
!  cerfc(z) = erfc(z)
USE ISO_C_BINDING
IMPLICIT NONE
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: cerfc
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION cerfcx ( z ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  cerfcx(z) = exp(z**2) * erfc(z)
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: cerfcx
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION erfcx ( x ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  erfcx(x) = exp(x**2) * erfc(x)
real(C_DOUBLE),intent(IN),VALUE :: x
real(C_DOUBLE) :: erfcx
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION cerfi ( z ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  cerfi(z) = -I * erf(I*z)
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: cerfi
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION erfi ( x ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  erfi(x) = -I * erf(I*x)
real(C_DOUBLE),intent(IN),VALUE :: x
real(C_DOUBLE) :: erfi
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION w_of_z ( z ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  w_of_z(z) = [ Faddeeva function w(z) ] 
!            = exp(-z**2) * erfc(-I*z)
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: w_of_z
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION im_w_of_x ( x ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  Im_w_of_x(x) = Im[w(x)]
real(C_DOUBLE),intent(IN),VALUE :: x
real(C_DOUBLE) :: im_w_of_x
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION cdawson ( z ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  cdawson(z) = [ Dawson's integral D(z) ] 
!             = exp(-z**2) \int_0^z exp(t**2) dt 
!             = sqrt(pi)/2 * exp(-z**2) * erfi(z)
complex(C_DOUBLE_COMPLEX),intent(IN),VALUE :: z
complex(C_DOUBLE_COMPLEX) :: cdawson
END FUNCTION
END INTERFACE
! - - - - - - - - - - - - 
INTERFACE
FUNCTION dawson ( x ) bind(c)
USE ISO_C_BINDING
IMPLICIT NONE
!  dawson(x) = [ Dawson's integral D(x) ] 
!            = exp(-x**2) \int_0^x exp(t**2) dt 
!            = sqrt(pi)/2 * exp(-x**2) * erfi(x)
real(C_DOUBLE),intent(IN),VALUE :: x
real(C_DOUBLE) :: dawson
END FUNCTION
END INTERFACE

contains
!********************************************************
function Voigt_F(x,sig,w)
! voigt(x,sigma,gamma) = \int_|R G(t,sigma) L(x-t,gamma) dt
! WITH: G(x,sigma) = (1/(sqrt(2*pi)*|sigma|)) * exp(-x**2/(2*sigma**2)) ;
!       L(x,gamma) = |gamma| / (pi * ( x**2 + gamma**2 )) .
implicit none
real(DP),intent(IN) :: x,sig,w
real(DP) :: Voigt_F

Voigt_F = real(voigt( x=real(x,C_DOUBLE), sigma=real(sig,C_DOUBLE), gamma=real(w,C_DOUBLE) ), DP)

end function Voigt_F
!********************************************************
function Erfcx_F(x)
!  erfcx(x) = exp(x**2) * erfc(x)
implicit none
real(DP),intent(IN) :: x
real(DP) :: Erfcx_F

Erfcx_F = real(erfcx( x=real(x,C_DOUBLE) ), DP)

end function Erfcx_F
!********************************************************
function Erfi_F(x)
!  erfi(x) = -I * erf(I*x)
implicit none
real(DP),intent(IN) :: x
real(DP) :: Erfi_F

Erfi_F = real(erfi( x=real(x,C_DOUBLE) ), DP)

end function Erfi_F
!********************************************************
function Im_w_of_x_F(x)
!  Im_w_of_x(x) = Im[w(x)]
implicit none
real(DP),intent(IN) :: x
real(DP) :: Im_w_of_x_F

Im_w_of_x_F = real(im_w_of_x( x=real(x,C_DOUBLE) ), DP)

end function Im_w_of_x_F
!********************************************************
function Dawson_F(x)
!  dawson(x) = [ Dawson's integral D(x) ] 
!            = exp(-x**2) \int_0^x exp(t**2) dt 
!            = sqrt(pi)/2 * exp(-x**2) * erfi(x)
implicit none
real(DP),intent(IN) :: x
real(DP) :: Dawson_F

Dawson_F = real(dawson( x=real(x,C_DOUBLE) ), DP)

end function Dawson_F
!********************************************************
! Complex
!********************************************************
function cerf_F ( z )
!  cerf(z) = erf(z)
implicit none
complex(DPC),intent(IN) :: z
complex(DPC) :: cerf_F

cerf_F = CMPLX(cerf( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function cerf_F
!********************************************************
function cerfc_F ( z )
!  cerfc(z) = erfc(z)
implicit none
complex(DPC),intent(IN) :: z
complex(DPC) :: cerfc_F

cerfc_F = CMPLX(cerfc( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function cerfc_F
!********************************************************
function cerfcx_F ( z )
implicit none
!  cerfcx(z) = exp(z**2) * erfc(z)
complex(DPC),intent(IN) :: z
complex(DPC) :: cerfcx_F

cerfcx_F = CMPLX(cerfcx( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function cerfcx_F
!********************************************************
function cerfi_F ( z )
implicit none
!  cerfi(z) = -I * erf(I*z)
complex(DPC),intent(IN) :: z
complex(DPC) :: cerfi_F

cerfi_F = CMPLX(cerfi( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function cerfi_F
!********************************************************
function w_of_z_F ( z )
implicit none
!  w_of_z(z) = [ Faddeeva function w(z) ] 
!            = exp(-z**2) * erfc(-I*z)
complex(DPC),intent(IN) :: z
complex(DPC) :: w_of_z_F

w_of_z_F = CMPLX(w_of_z( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function w_of_z_F
!********************************************************
function cdawson_F ( z )
implicit none
!  cdawson(z) = [ Dawson's integral D(z) ] 
!             = exp(-z**2) \int_0^z exp(t**2) dt 
!             = sqrt(pi)/2 * exp(-z**2) * erfi(z)
complex(DPC),intent(IN) :: z
complex(DPC) :: cdawson_F

cdawson_F = CMPLX(cdawson( z=CMPLX(z,kind=C_DOUBLE_COMPLEX) ), kind=DPC)

end function cdawson_F
!********************************************************
end module use_libcerf
!___________________________________________________________________________________________________
MODULE strangef_V
use nano_deftyp
private
public :: sqpi_ea2_erfca,cacio

real(DP),parameter  :: amx0 = 5792.61855726752199432553093800392867_DP, &
                       amx1 = 41.9198763601262551806165903391085738_DP, &
                       amx2 = 17.4708383195836356762224255870008932_DP, &
                       amx3 = 8.01272588682296863726379289844332204_DP, &
                       amx3_7=amx3*0.875_DP,amx3_6=amx3*0.75_DP,amx3_5=amx3*0.625_DP, &
                       amx3_4=amx3*half,amx3_3=amx3*0.375_DP,amx3_2=amx3*0.25_DP,amx3_1=amx3*0.125_DP, &
                       amx4 = 6.00080504298140915549850946766947725_DP, &
                       amx5 = 2.71828182845904523536028747135266250_DP, &
                       amx00 = 0.001_DP


real(DP),parameter  :: tmx0 = two/(two+amx0), &
                       tmx1 = two/(two+amx1), &
                       tmx2 = two/(two+amx2), &
                       tmx3 = two/(two+amx3), &
                       tmx3_7=two/(two+amx3_7),tmx3_6=two/(two+amx3_6),tmx3_5=two/(two+amx3_5), &
                       tmx3_4=two/(two+amx3_4),tmx3_3=two/(two+amx3_3),tmx3_2=two/(two+amx3_2),tmx3_1=two/(two+amx3_1), &
                       tmx4 = two/(two+amx4), &
                       tmx5 = two/(two+amx5), &
                       tmx00 = two/(two+amx00), &
                       tero=one

integer(I4B),save  :: cacio

integer(I4B),parameter  :: NChc00 = 3
real(DP),dimension(NChc00+1),parameter :: Chc00 = (/-.314060961065449783736285263553024051e-3_DP, &
.314070424615571616897302137058996574e-3_DP, -.946291979789866804816395384441666586e-8_DP, &
-.630418169241215477479863332973321372e-12_DP/)
!, .942381007064782814956490982812807959e-16_DP/)
integer(I4B),parameter  :: NChc0 = 14
real(DP),dimension(NChc0+1),parameter :: Chc0 = (/ -.219784215807206239426775474447957162_DP, &
  .222656316675948392042889410588401786_DP,-.00254760454228384433951073618646340864_DP, &
 -.000343637242210470727290814243529289970_DP,.0000187635008641188297847346333710282124_DP, &
  .480954754700293359746722152806937275e-6_DP,-.107829352010391211514878412888807033e-6_DP, &
  .403498610377484907325401247335479080e-8_DP,.294516909589675757865948583459063115e-9_DP, &
 -.414495276185867497041193709662751408e-10_DP,.131284017243382725290938622117613900e-11_DP, &
  .136055287595363063687467872376992571e-12_DP,-.176500594392586561842687613841591275e-13_DP, &
  .573318787813985911931304187063726867e-15_DP,.566909303696407948676533511505193435e-16_DP/)
integer(I4B),parameter  :: NChc1 = 10
real(DP),dimension(NChc1+1),parameter :: Chc1=(/ &
-.558793062792659596620755691538313995_DP,.113909688152792173692230047178087323_DP, &
.315800593076581881379656913999870566e-3_DP, -.586121512275779245289484505966995160e-4_DP, &
-.215540528060207806922930885237273647e-6_DP, .736217469115871389299854659264966068e-7_DP, &
-.542169604372851780135893088369662660e-9_DP, -.108394526400253674923033221366869609e-9_DP, &
.291019606588960251634619458225526471e-11_DP, .130179191468719003539737056447288274e-12_DP, &
-.829738625800138984324436370067851267e-14_DP/)
integer(I4B),parameter  :: NChc2 = 9
real(DP),dimension(NChc2+1),parameter :: Chc2=(/ &
-.738750975697476602314189565509123357_DP,.661296348105580674144417751648090957e-1_DP,&
.303370616501836186309653572357209953e-3_DP, -.104882596329771492264943625113757216e-4_DP, &
-.173888042626462739018330505338169184e-6_DP, .452382648812932282520611798809613529e-8_DP, &
.102958891626966490707588470359798206e-9_DP, -.306190891106010816405667393649212467e-11_DP, &
-.596253906488515586186974747160278944e-13_DP, .254133493061079478211989556029754327e-14_DP/)
integer(I4B),parameter  :: NChc3 = 8
real(DP),dimension(NChc3+1),parameter:: Chc3=(/ &
-.847263711633508338169466966568048624_DP,.425233699485144350093262252077344283e-1_DP,&
 .175755186173965316508381086458574239e-3_DP, -.229846563080941922853200133788408541e-5_DP, &
-.452382767421148731657744910507734992e-7_DP, .251856578050247762268256933571301374e-9_DP, &
.131036142619724477397550294497707079e-10_DP, -.387139519618853266524116613145459155e-13_DP, &
-.431190437054592610414318114104592350e-14_DP/)
integer(I4B),parameter  :: NChc4 = 8
real(DP),dimension(NChc4+1),parameter:: Chc4=(/ &
-.919165863130042551188194228334155087_DP,.294573293050147037882621460777582077e-1_DP,&
 .100077107136416106842774501270831558e-3_DP, -.604391575553891524225173285431756481e-6_DP, &
-.122961544552684856223592413482428015e-7_DP, -.327480555913853044379512354850514393e-11_DP, &
.167585384269352320605320595979701218e-11_DP, .734313021990495970855334395687228425e-14_DP, &
-.263074979864826171994422696016847980e-15_DP/)
integer(I4B),parameter  :: NChc5 = 7
real(DP),dimension(NChc5+1),parameter :: Chc5=(/ &
-.970131931594908424924653848652053260_DP,.215502414347799105747269737137070905e-1_DP,&
 .593537401611818306491608631054113255e-4_DP, -.183020686812533717509882152374101726e-6_DP, &
-.377861290236007669211276851698453743e-8_DP, -.854305128205184358170835403910645690e-11_DP, &
.251600670899595250166180895334835284e-12_DP, .183903517856984328762500824155621227e-14_DP/)
integer(I4B),parameter  :: NChc6 = 7
real(DP),dimension(NChc6+1),parameter :: Chc6=(/ &
-1.00808615248533531998981877531913821_DP,.164266634836673704616273677315213789e-1_DP,&
 .369116863787411877401431635520855337e-4_DP, -.614090765745717843936290061519865163e-7_DP, &
-.131052945797453893080722031062042186e-8_DP, -.381003580193535605936832312777439699e-11_DP, &
.444545016104263416547797827314550531e-13_DP, .383317075669349621798864140710057889e-15_DP/)
integer(I4B),parameter  :: NChc7 = 6
real(DP),dimension(NChc7+1),parameter :: Chc7=(/ &
-1.03742583773034460485743989443707500_DP,.129260348069345978979263152737414468e-1_DP,&
 .239812932353298867785802445691995738e-4_DP, -.220432312115933309398765164368209822e-7_DP,&
 -.505298031529250583621666832334312876e-9_DP, -.152792994747793457086115335898440786e-11_DP,&
 .903826495828725157301480639035877905e-14_DP/)

contains


function sqpi_ea2_erfca(aa)
real(DP),intent(IN) :: aa
!\begin{verbatim}
!function sqpi_ea2_erfca(aa)
!real(DP),intent(IN) :: aa
!\end{verbatim}
! \[  F(x)=\sqrt(\pi) \exp(x^2) \erfc(x), \qquad x\equiv{\mathtt{XX}};\quad F\equiv{\mathtt{sqpi\_ea2\_erfca}} \]
!\begin{verbatim}
!end function sqpi_ea2_erfca
!\end{verbatim}
real(DP)   :: sqpi_ea2_erfca,a,a2,am2,t
logical    :: isneg


 if (aa<-sceps_DP) then
   isneg=.true.
   a=-aa
 else if (aa>sceps_DP) then
   isneg=.false.
   a=aa
 else if (abs(aa)<=sceps_DP) then
   cacio=-1
   sqpi_ea2_erfca = sqrt_of_pi-two*aa
   return
 endif
 a2=a*a
 am2=one/a2

 if (a>amx0) then
   cacio= 10000
   sqpi_ea2_erfca = two/(a+sqrt(two+a2))
 else if (a<=amx0.and.a>amx1) then
   cacio= 1000
   sqpi_ea2_erfca = (-.125_DP*am2+1.15_DP - sqrt((-.084375_DP*am2+.1125_DP)*am2+.0225_DP))/a
 else if (a<=amx1.and.a>amx2) then
   cacio= 100
   sqpi_ea2_erfca = ((157.3125_DP*am2+89.2321428571428571428571428571428571_DP)*am2+11.0_DP-sqrt( &
        (((24717.0619419642857142857142857142857_DP*am2+28134.8772321428571428571428571428571_DP)*am2 &
        +11183.1074617346938775510204081632653_DP)*am2+1794.64285714285714285714285714285714_DP)*am2+100.0_DP &
        ))/a
 else if (a<=amx2.and.a>amx3) then
   cacio= 10
   sqpi_ea2_erfca = (((5.5078125_DP*am2+3.484375_DP)*am2-.267992424242424242424242424242424242_DP)*am2 &
                   +1.00509906759906759906759906759906760_DP-sqrt( ((((( &
                   30.6637957379534527972027972027972028_DP*am2+37.6308201076267482517482517482517483_DP)*am2 &
                  +10.8356182391826923076923076923076923_DP)*am2+1.34408234994172494172494172494172494_DP)*am2 &
                  +.817130411413081867627322172776718231e-1_DP)*am2+.236604462456735184007911280638553366e-2_DP)*am2 &
                  +.260004903798610092316386022679728973e-4_DP))/a
 else if (a<=amx3.and.a>amx3_7) then
   cacio= 8
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_7,a=tmx3,c=Chc7,x=t))
 else if (a<=amx3_7.and.a>amx3_6) then
   cacio= 7
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_6,a=tmx3_7,c=Chc6,x=t))
 else if (a<=amx3_6.and.a>amx3_5) then
   cacio= 6
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_5,a=tmx3_6,c=Chc5,x=t))
 else if (a<=amx3_5.and.a>amx3_4) then
   cacio= 5
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_4,a=tmx3_5,c=Chc4,x=t))
 else if (a<=amx3_4.and.a>amx3_3) then
   cacio= 4
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_3,a=tmx3_4,c=Chc3,x=t))
 else if (a<=amx3_3.and.a>amx3_2) then
   cacio= 3
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_2,a=tmx3_3,c=Chc2,x=t))
 else if (a<=amx3_2.and.a>amx3_1) then
   cacio= 2
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tmx3_1,a=tmx3_2,c=Chc1,x=t))
 else if (a<=amx3_1.and.a>amx00) then
   cacio= 1
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tero,a=tmx3_1,c=Chc0,x=t))
 else if (a<=amx00) then
   cacio= 0
   t=two/(two+a)
   sqpi_ea2_erfca = sqrt_of_pi*t*exp(Tcheblocal(b=tero,a=tmx00,c=Chc00,x=t))
 endif
!...
 if (isneg) sqpi_ea2_erfca = two*sqrt_of_pi*exp(a2)-sqpi_ea2_erfca

end function sqpi_ea2_erfca
!****************************
 FUNCTION Tcheblocal(a,b,c,x)
  IMPLICIT NONE
  REAL(DP), INTENT(IN)               :: x
  REAL(DP), INTENT(IN)      :: a,b
  REAL(DP), DIMENSION(:), INTENT(IN) :: c

  REAL(DP)                           :: Tcheblocal
  REAL(DP), DIMENSION(size(c))       :: cfr

  INTEGER(I4B)    :: j,m
  REAL(DP)        :: d,dd,sv,y,y2,crout

  crout = (x-a)*(x-b)

  if (crout > eps_DP) STOP 'x not in range in Tcheblocal'

  m=size(c)
  cfr = zero
  cfr(m) = one
  y=MIN(one,MAX(-one,(two*x-a-b)/(b-a)))

  IF (MAXVAL(ABS(cfr-c)) < eps_DP) THEN
    Tcheblocal = cos(REAL(m-1,DP)*ACOS(y))
  ELSE
    d=zero
    dd=zero
    y2=two*y
    do j=m,2,-1
      sv=d
      d=y2*d-dd+c(j)
      dd=sv
    end do
    Tcheblocal=y*d-dd+c(1)
  ENDIF

 END FUNCTION Tcheblocal
!***********************

end module strangef_V
!___________________________________________________________________________________________________________________________________
module SPECIAL_CISI
use nano_deftyp
private
public :: Spec_Si, Spec_Ci, gamma_EulerMascheroni

REAL(DP), PARAMETER :: piby2 = pi_over_2
REAL(DP), PARAMETER :: gamma_EulerMascheroni = 0.577215664901532860606512090082402431_DP
!   MACHINE-DEPENDENT PARAMETERS:
REAL(DP), PARAMETER :: xvhigh_SICI=Pi/eps_DP
real(DP),save :: xlow_SICI,xhig_SICI
logical,save  :: INI_SIandCI=.false.

interface Spec_Si
  module procedure DSININT
end interface
interface Spec_Ci
  module procedure DCOSINT
end interface

contains

FUNCTION dsinint(xvalue) result(fn_val)
!   This program calculates the value of the sine-integral defined as
!
!       DSININT(x) = Integral (0 to x) sin(t)/t  dt
!
!      W.J. CODY  Algorithm 665: MACHAR: A subroutine to dynamically
!      determine machine parameters, ACM Trans. Math. Soft. 14 (1988) 303-311.
!
!   AUTHOR: Allan MacLeod
!           Dept. of Mathematics and Statistics
!           University of Paisley
!           Scotland
!           (e-mail: macl_ms0@paisley.ac.uk)

IMPLICIT NONE
REAL(DP), INTENT(IN) :: xvalue
REAL(DP)             :: fn_val

!  DATA VALUES
!  VALUES FOR SINE-INTEGRAL FOR 0 <= |X| <= 6

REAL(DP), PARAMETER :: asintn(0:7) = (/ 1.0_DP,  &
          -0.44663998931312457298E-1_DP, 0.11209146443112369449E-2_DP,  &
          -0.13276124407928422367E-4_DP, 0.85118014179823463879E-7_DP,  &
          -0.29989314303147656479E-9_DP, 0.55401971660186204711E-12_DP, &
          -0.42406353433133212926E-15_DP /)
REAL(DP), PARAMETER :: asintd(0:7) = (/ 1.0_DP,  &
           0.10891556624243098264E-1_DP, 0.59334456769186835896E-4_DP,  &
           0.21231112954641805908E-6_DP, 0.54747121846510390750E-9_DP,  &
           0.10378561511331814674E-11_DP, 0.13754880327250272679E-14_DP,&
           0.10223981202236205703E-17_DP /)

!  VALUES FOR FI(X) FOR 6 <= X <= 12

REAL(DP), PARAMETER :: afn1(0:7) = (/ 0.99999999962173909991_DP,   &
          0.36451060338631902917E3_DP, 0.44218548041288440874E5_DP, &
          0.22467569405961151887E7_DP, 0.49315316723035561922E8_DP, &
          0.43186795279670283193E9_DP, 0.11847992519956804350E10_DP,&
          0.45573267593795103181E9_DP /)
REAL(DP), PARAMETER :: afd1(0:7) = (/ 1.0_DP, 0.36651060273229347594E3_DP,  &
                  0.44927569814970692777E5_DP, 0.23285354882204041700E7_DP,  &
                  0.53117852017228262911E8_DP, 0.50335310667241870372E9_DP,  &
                  0.16575285015623175410E10_DP, 0.11746532837038341076E10_DP /)

!   VALUES OF GI(X) FOR 6 <= X <=12

REAL(DP), PARAMETER :: agn1(0:8) = (/ 0.99999999920484901956_DP,     &
          0.51385504875307321394E3_DP, 0.92293483452013810811E5_DP,   &
          0.74071341863359841727E7_DP, 0.28142356162841356551E9_DP,   &
          0.49280890357734623984E10_DP, 0.35524762685554302472E11_DP, &
          0.79194271662085049376E11_DP, 0.17942522624413898907E11_DP /)
REAL(DP), PARAMETER :: agd1(0:8) = (/ 1.0_DP, 0.51985504708814870209E3_DP,  &
                  0.95292615508125947321E5_DP, 0.79215459679762667578E7_DP,  &
                  0.31977567790733781460E9_DP, 0.62273134702439012114E10_DP,  &
                  0.54570971054996441467E11_DP, 0.18241750166645704670E12_DP,  &
                  0.15407148148861454434E12_DP /)

!   VALUES FOR FI(X) FOR X > 12

REAL(DP), PARAMETER :: afn2(0:7) = (/ 0.19999999999999978257E1_DP,   &
          0.22206119380434958727E4_DP, 0.84749007623988236808E6_DP,   &
          0.13959267954823943232E9_DP, 0.10197205463267975592E11_DP,  &
          0.30229865264524075951E12_DP, 0.27504053804288471142E13_DP, &
          0.21818989704686874983E13_DP /)
REAL(DP), PARAMETER :: afd2(0:7) = (/ 1.0_DP, 0.11223059690217167788E4_DP,  &
                  0.43685270974851313242E6_DP, 0.74654702140658116258E8_DP,  &
                  0.58580034751805687471E10_DP, 0.20157980379272098841E12_DP,  &
                  0.26229141857684496445E13_DP, 0.87852907334918467516E13_DP /)

!   VALUES FOR GI(X) FOR X > 12

REAL(DP), PARAMETER :: agn2(0:8) = (/ 0.59999999999999993089E1_DP,   &
          0.96527746044997139158E4_DP, 0.56077626996568834185E7_DP,   &
          0.15022667718927317198E10_DP, 0.19644271064733088465E12_DP, &
          0.12191368281163225043E14_DP, 0.31924389898645609533E15_DP, &
          0.25876053010027485934E16_DP, 0.12754978896268878403E16_DP /)
REAL(DP), PARAMETER :: agd2(0:8) = (/ 1.0_DP, 0.16287957674166143196E4_DP,  &
                  0.96636303195787870963E6_DP, 0.26839734750950667021E9_DP,  &
                  0.37388510548029219241E11_DP, 0.26028585666152144496E13_DP,  &
                  0.85134283716950697226E14_DP, 0.11304079361627952930E16_DP,  &
                  0.42519841479489798424E16_DP /)
              ! Local variables

INTEGER   :: i, indsgn
REAL(DP) :: cx, fival, gival, sumden, sumnum, sx, x, xsq
!   START COMPUTATION

if (.not. INI_SIandCI) then
  xhig_SICI=two*sr3/sceps_DP
  xlow_SICI=three/sceps_DP
  INI_SIandCI = .true.
endif

x = xvalue
indsgn = 1
IF ( x < zero ) THEN
  x = -x
  indsgn = -1
END IF

!   CODE FOR 0 <= |X| <= 6

IF ( x <= six ) THEN
  IF ( x < xlow_SICI ) THEN
    fn_val = x
  ELSE
    sumnum = zero
    sumden = zero
    xsq = x * x
    DO i = 7 , 0 , -1
      sumnum = sumnum * xsq + asintn(i)
      sumden = sumden * xsq + asintd(i)
    END DO
    fn_val = x * sumnum / sumden
  END IF
END IF

!   CODE FOR 6 < |X| <= 12

IF ( x > six .AND. x <= twelve ) THEN
  sumnum = zero
  sumden = zero
  xsq = one / ( x * x )
  DO i = 7 , 0 , -1
    sumnum = sumnum * xsq + afn1(i)
    sumden = sumden * xsq + afd1(i)
  END DO
  fival = sumnum / ( x * sumden )
  sumnum = zero
  sumden = zero
  DO i = 8 , 0 , -1
    sumnum = sumnum * xsq + agn1(i)
    sumden = sumden * xsq + agd1(i)
  END DO
  gival = xsq * sumnum / sumden
  fn_val = piby2 - fival * COS(x) - gival * SIN(x)
END IF

!   CODE FOR |X| > 12

IF ( x > twelve ) THEN
  IF ( x > xvhigh_SICI ) THEN
    fn_val = piby2
  ELSE
    cx = COS(x)
    sx = SIN(x)
    xsq = one / ( x * x )
    IF ( x > xhig_SICI ) THEN
      fn_val = piby2 - cx / x - sx * xsq
    ELSE
      sumnum = zero
      sumden = zero
      DO i = 7 , 0 , -1
        sumnum = sumnum * xsq + afn2(i)
        sumden = sumden * xsq + afd2(i)
      END DO
      fival =  ( one - xsq * sumnum / sumden ) / x
      sumnum = zero
      sumden = zero
      DO i = 8 , 0 , -1
        sumnum = sumnum * xsq + agn2(i)
        sumden = sumden * xsq + agd2(i)
      END DO
      gival =  ( one - xsq * sumnum / sumden ) * xsq
      fn_val = piby2 - cx * fival - sx * gival
    END IF
  END IF
END IF
IF ( indsgn == -1 ) fn_val = -fn_val

END FUNCTION dsinint
!**********************************************************************************
FUNCTION dcosint(xvalue) result(fn_val)
!   This program calculates the value of the cosine-integral defined as
!
!   DCOSINT(x) = Gamma + Ln(x) + Integral (0 to x) [cos(t)-1]/t  dt
!
!                where Gamma is Euler-Mascheroni's constant.
!
!      W.J. CODY  Algorithm 665: MACHAR: A subroutine to dynamically
!      determine machine parameters, ACM Trans. Math. Soft. 14 (1988) 303-311.
!
!   AUTHOR: Allan MacLeod
!           Dept. of Mathematics and Statistics
!           University of Paisley
!           Scotland
!           (e-mail: macl_ms0@paisley.ac.uk)

IMPLICIT NONE
REAL(DP), INTENT(IN) :: xvalue
REAL(DP)             :: fn_val

! Local variables
INTEGER   :: i
REAL(DP) :: cx, dif, fival, gival, logval, root, vsum, sumden, sumnum, &
             sx, x, xsq

!   DATA VALUES

REAL(DP), PARAMETER :: logl1 = 0.046875_DP, logl2 = 0.25_DP

!  VALUES FOR COS-INTEGRAL FOR 0 < X <= 3

REAL(DP), PARAMETER :: ac1n(0:5) = (/ -0.24607411378767540707_DP,   &
         0.72113492241301534559E-2_DP, -0.11867127836204767056E-3_DP,  &
         0.90542655466969866243E-6_DP, -0.34322242412444409037E-8_DP,  &
         0.51950683460656886834E-11_DP /)
REAL(DP), PARAMETER :: ac1d(0:5) = (/ 1.0_DP, 0.12670095552700637845E-1_DP, &
                 0.78168450570724148921E-4_DP, 0.29959200177005821677E-6_DP, &
                 0.73191677761328838216E-9_DP, 0.94351174530907529061E-12_DP /)

!  VALUES FOR COS-INTEGRAL FOR 3 < X <= 6

REAL(DP), PARAMETER :: ac2n(0:7) = (/ -0.15684781827145408780_DP,     &
        0.66253165609605468916E-2_DP,  -0.12822297297864512864E-3_DP,  &
        0.12360964097729408891E-5_DP,  -0.66450975112876224532E-8_DP,  &
        0.20326936466803159446E-10_DP, -0.33590883135343844613E-13_DP, &
        0.23686934961435015119E-16_DP /)
REAL(DP), PARAMETER :: ac2d(0:6) = (/ 1.0_DP, 0.96166044388828741188E-2_DP,  &
                 0.45257514591257035006E-4_DP, 0.13544922659627723233E-6_DP,  &
                 0.27715365686570002081E-9_DP, 0.37718676301688932926E-12_DP, &
                 0.27706844497155995398E-15_DP /)

!  VALUES FOR FI(X) FOR 6 <= X <= 12

REAL(DP), PARAMETER :: afn1(0:7) = (/ 0.99999999962173909991E0_DP,  &
          0.36451060338631902917E3_DP, 0.44218548041288440874E5_DP,  &
          0.22467569405961151887E7_DP, 0.49315316723035561922E8_DP,  &
          0.43186795279670283193E9_DP, 0.11847992519956804350E10_DP, &
          0.45573267593795103181E9_DP /)
REAL(DP), PARAMETER :: afd1(0:7) = (/ 1.0_DP, 0.36651060273229347594E3_DP,  &
                  0.44927569814970692777E5_DP, 0.23285354882204041700E7_DP,  &
                  0.53117852017228262911E8_DP, 0.50335310667241870372E9_DP,  &
                  0.16575285015623175410E10_DP, 0.11746532837038341076E10_DP /)

!   VALUES OF GI(X) FOR 6 <= X <=12

REAL(DP), PARAMETER :: agn1(0:8) = (/ 0.99999999920484901956E0_DP,  &
         0.51385504875307321394E3_DP,  0.92293483452013810811E5_DP,  &
         0.74071341863359841727E7_DP,  0.28142356162841356551E9_DP,  &
         0.49280890357734623984E10_DP, 0.35524762685554302472E11_DP, &
         0.79194271662085049376E11_DP, 0.17942522624413898907E11_DP /)
REAL(DP), PARAMETER :: agd1(0:8) = (/ 1.0_DP,  0.51985504708814870209E3_DP,  &
                  0.95292615508125947321E5_DP,  0.79215459679762667578E7_DP,  &
                  0.31977567790733781460E9_DP,  0.62273134702439012114E10_DP, &
                  0.54570971054996441467E11_DP, 0.18241750166645704670E12_DP, &
                  0.15407148148861454434E12_DP /)

!   VALUES FOR FI(X) FOR X > 12

REAL(DP), PARAMETER :: afn2(0:7) = (/ 0.19999999999999978257E1_DP,   &
          0.22206119380434958727E4_DP, 0.84749007623988236808E6_DP,   &
          0.13959267954823943232E9_DP, 0.10197205463267975592E11_DP,  &
          0.30229865264524075951E12_DP, 0.27504053804288471142E13_DP, &
          0.21818989704686874983E13_DP /)
REAL(DP), PARAMETER :: afd2(0:7) = (/ 1.0_DP,  0.11223059690217167788E4_DP,  &
                  0.43685270974851313242E6_DP,  0.74654702140658116258E8_DP,  &
                  0.58580034751805687471E10_DP, 0.20157980379272098841E12_DP, &
                  0.26229141857684496445E13_DP, 0.87852907334918467516E13_DP /)

!   VALUES FOR GI(X) FOR X > 12

REAL(DP), PARAMETER :: agn2(0:8) = (/  0.59999999999999993089E1_DP,  &
          0.96527746044997139158E4_DP,  0.56077626996568834185E7_DP,  &
          0.15022667718927317198E10_DP, 0.19644271064733088465E12_DP, &
          0.12191368281163225043E14_DP, 0.31924389898645609533E15_DP, &
          0.25876053010027485934E16_DP, 0.12754978896268878403E16_DP /)
REAL(DP), PARAMETER :: agd2(0:8) = (/ 1.0_DP,  0.16287957674166143196E4_DP,  &
                  0.96636303195787870963E6_DP,  0.26839734750950667021E9_DP,  &
                  0.37388510548029219241E11_DP, 0.26028585666152144496E13_DP,  &
                  0.85134283716950697226E14_DP, 0.11304079361627952930E16_DP,  &
                  0.42519841479489798424E16_DP /)

!   VALUES FOR AN APPROXIMATION TO LN(X/ROOT)

REAL(DP), PARAMETER :: p(0:2) = (/   0.83930008362695945726E1_DP,  &
        -0.65306663899493304675E1_DP, 0.569155722227490223_DP /)
REAL(DP), PARAMETER :: q(0:1) = (/ 0.41965004181347972847E1_DP,  &
                                   -0.46641666676862479585E1_DP /)

!   VALUES OF THE FIRST TWO ROOTS OF THE COSINE-INTEGRAL

REAL(DP), PARAMETER :: rt1n = 631.0_DP, rt1d = 1024.0_DP, &
                        rt1r = 0.29454812071623379711E-3_DP
REAL(DP), PARAMETER :: rt2n = 3465.0_DP, rt2d = 1024.0_DP, &
                        rt2r = 0.39136005118642639785E-3_DP

!   START COMPUTATION


if (.not. INI_SIandCI) then
  xhig_SICI=two*sr3/sceps_DP
  xlow_SICI=three/sceps_DP
  INI_SIandCI = .true.
endif

x = xvalue
IF ( x <= zero ) THEN
  fn_val = zero
  RETURN
END IF
IF ( x <= six ) THEN
  
!   CODE FOR 3 < X < =  6
  
  IF ( x > three ) THEN
    sumnum = zero
    sumden = zero
    xsq = x * x
    DO i = 7 , 0 , -1
      sumnum = sumnum * xsq + ac2n( i )
    END DO
    DO i = 6 , 0 , -1
      sumden = sumden * xsq + ac2d( i )
    END DO
    root = rt2n / rt2d
    dif = ( x - root ) - rt2r
    vsum = root + rt2r
    IF ( ABS(dif) < logl2 ) THEN
      cx = dif / ( vsum + x )
      xsq = cx * cx
      sx = p(0) + xsq * ( p(1) + xsq * p(2) )
      sx = sx / ( q(0) + xsq * ( q(1) + xsq ) )
      logval = cx * sx
    ELSE
      logval = LOG( x / vsum )
    END IF
    fn_val = logval + dif * ( x + vsum ) * sumnum / sumden
  ELSE
    
!   CODE FOR 0 < X < =  3
    
    IF ( x > xlow_SICI ) THEN
      sumnum = zero
      sumden = zero
      xsq = x * x
      DO i = 5 , 0 , -1
        sumnum = sumnum * xsq + ac1n( i )
        sumden = sumden * xsq + ac1d( i )
      END DO
      root = rt1n / rt1d
      dif = ( x - root ) - rt1r
      vsum = root + rt1r
      IF ( ABS(dif) < logl1 ) THEN
        cx = dif / ( vsum + x )
        xsq = cx * cx
        sx = p(0) + xsq * ( p(1) + xsq * p(2) )
        sx = sx / ( q(0) + xsq * ( q(1) + xsq ) )
        logval = cx * sx
      ELSE
        logval = LOG( x / vsum )
      END IF
      fn_val = logval + dif * ( x + vsum ) * sumnum / sumden
    ELSE
      fn_val = gamma_EulerMascheroni + LOG( x )
    END IF
  END IF
END IF

!   CODE FOR 6 < X < =  12

IF ( x > six .AND. x <= twelve ) THEN
  sumnum = zero
  sumden = zero
  xsq = one / ( x * x )
  DO i = 7 , 0 , -1
    sumnum = sumnum * xsq + afn1( i )
    sumden = sumden * xsq + afd1( i )
  END DO
  fival = sumnum / ( x * sumden )
  sumnum = zero
  sumden = zero
  DO i = 8 , 0 , -1
    sumnum = sumnum * xsq + agn1( i )
    sumden = sumden * xsq + agd1( i )
  END DO
  gival = xsq * sumnum / sumden
  fn_val = fival * SIN( x ) - gival * COS( x )
END IF

!   CODE FOR X > 12

IF ( x > twelve ) THEN
  IF ( x > xvhigh_SICI ) THEN
    fn_val = zero
  ELSE
    cx = COS( x )
    sx = SIN( x )
    xsq = one / ( x * x )
    IF ( x > xhig_SICI ) THEN
      fn_val = sx / x - cx * xsq
    ELSE
      sumnum = zero
      sumden = zero
      DO i = 7 , 0 , -1
        sumnum = sumnum * xsq + afn2( i )
        sumden = sumden * xsq + afd2( i )
      END DO
      fival = ( one - xsq * sumnum / sumden ) / x
      sumnum = zero
      sumden = zero
      DO i = 8 , 0 , -1
        sumnum = sumnum * xsq + agn2( i )
        sumden = sumden * xsq + agd2( i )
      END DO
      gival = ( one - xsq * sumnum / sumden ) * xsq
      fn_val = sx * fival - cx * gival
    END IF
  END IF
END IF

END FUNCTION dcosint
!**********************************************************************************

end module SPECIAL_CISI
!___________________________________________________________________________________________________________________________________
module SPECIAL_GAMMAERF
use nano_deftyp
private
public :: Spec_GAMMA,Spec_LNGAMMA,Spec_PSI,Spec_ERF,Spec_ERFC,Spec_ERFCX,Spec_DAW

interface Spec_GAMMA
  module procedure DGAMMA
end interface
interface Spec_LNGAMMA
  module procedure LNGAMMA
end interface
interface Spec_PSI
  module procedure PSI
end interface
interface Spec_ERF
  module procedure DERF
end interface
interface Spec_ERFC
  module procedure DERFC
end interface
interface Spec_ERFCX
  module procedure DERFCX
end interface
interface Spec_DAW
  module procedure DAW
end interface

!____________ Numerics for GAMMA
real(DP),parameter  :: max_arg_gamma=171.624376956302720791055387902302405_DP
!__ obtained solving LNGAMMA(x)=log(huge(1.d0))
dimension :: C(7),P(8),Q(8)
!----------------------------------------------------------------------
!  Numerator and denominator coefficients for rational minimax
!     approximation over (1,2).
!----------------------------------------------------------------------
real(DP),parameter :: P=(/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,&
       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,&
       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,&
       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/)
real(DP),parameter :: Q=(/-3.08402300119738975254353D+1,3.15350626979604161529144D+2,&
      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,&
        2.25381184209801510330112D+4,4.75584627752788110767815D+3,&
      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/)
!----------------------------------------------------------------------
!  Coefficients for minimax approximation over (12, INF).
!----------------------------------------------------------------------
real(DP),parameter :: C=(/-1.910444077728D-03,8.4171387781295D-04,&
     -5.952379913043012D-04,7.93650793500350248D-04,&
     -2.777777777777681622553D-03,8.333333333333333331554247D-02,&
      5.7083835261D-03/)
!____________ End Numerics for GAMMA

!____________ Numerics for DAW
  real(DP),parameter :: SIX25=6.25_DP, ONE225=12.25_DP, TWO5=25.0_DP, &
                 XMAX=min(half/tiny_DP, biggest_DP)

dimension :: P1(10),P2(10),P3(10),P4(10),Q1(10),Q2(9),Q3(9),Q4(9)
!----------------------------------------------------------------------
!  Mathematical constants.
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!  Coefficients for R(9,9) approximation for  |x| < 2.5
!----------------------------------------------------------------------
real(DP),parameter :: P1=(/-2.69020398788704782410D-12, 4.18572065374337710778D-10,-1.34848304455939419963D-08, &
        9.28264872583444852976D-07, &
       -1.23877783329049120592D-05, 4.07205792429155826266D-04,-2.84388121441008500446D-03, 4.70139022887204722217D-02, &
       -1.38868086253931995101D-01, 1.00000000000000000004D+00/)
real(DP),parameter :: Q1=(/ 1.71257170854690554214D-10, 1.19266846372297253797D-08,4.32287827678631772231D-07, &
        1.03867633767414421898D-05, &
        1.78910965284246249340D-04, 2.26061077235076703171D-03,2.07422774641447644725D-02, 1.32212955897210128811D-01, &
        5.27798580412734677256D-01, 1.00000000000000000000D+00/)
!----------------------------------------------------------------------
!  Coefficients for R(9,9) approximation in J-fraction form
!     for  x in [2.5, 3.5)
!----------------------------------------------------------------------
real(DP),parameter :: P2=(/-1.70953804700855494930D+00,-3.79258977271042880786D+01,2.61935631268825992835D+01, &
        1.25808703738951251885D+01, &
       -2.27571829525075891337D+01, 4.56604250725163310122D+00,-7.33080089896402870750D+00, 4.65842087940015295573D+01, &
       -1.73717177843672791149D+01, 5.00260183622027967838D-01/)
real(DP),parameter :: Q2=(/ 1.82180093313514478378D+00, 1.10067081034515532891D+03,-7.08465686676573000364D+00, &
        4.53642111102577727153D+02, &
        4.06209742218935689922D+01, 3.02890110610122663923D+02,1.70641269745236227356D+02, 9.51190923960381458747D+02, &
        2.06522691539642105009D-01/)
!----------------------------------------------------------------------
!  Coefficients for R(9,9) approximation in J-fraction form
!     for  x in [3.5, 5.0]
!----------------------------------------------------------------------
real(DP),parameter :: P3=(/-4.55169503255094815112D+00,-1.86647123338493852582D+01,-7.36315669126830526754D+00,&
      -6.68407240337696756838D+01, &
       4.84507265081491452130D+01, 2.69790586735467649969D+01,-3.35044149820592449072D+01, 7.50964459838919612289D+00, &
             -1.48432341823343965307D+00, 4.99999810924858824981D-01/)
real(DP),parameter :: Q3=(/ 4.47820908025971749852D+01, 9.98607198039452081913D+01,1.40238373126149385228D+01, &
              3.48817758822286353588D+03, &
             -9.18871385293215873406D+00, 1.24018500009917163023D+03,-6.88024952504512254535D+01,-2.31251575385145143070D+00, &
              2.50041492369922381761D-01/)
!----------------------------------------------------------------------
!  Coefficients for R(9,9) approximation in J-fraction form
!     for  |x| > 5.0
!----------------------------------------------------------------------
real(DP),parameter :: P4=(/-8.11753647558432685797D+00,-3.84043882477454453430D+01,-2.23787669028751886675D+01,&
       -2.88301992467056105854D+01, &
       -5.99085540418222002197D+00,-1.13867365736066102577D+01,-6.52828727526980741590D+00,-4.50002293000355585708D+00, &
       -2.50000000088955834952D+00, 5.00000000000000488400D-01/)
real(DP),parameter :: Q4=(/ 2.69382300417238816428D+02, 5.04198958742465752861D+01,6.11539671480115846173D+01, &
        2.08210246935564547889D+02, &
        1.97325365692316183531D+01,-1.22097010558934838708D+01,-6.99732735041547247161D+00,-2.49999970104184464568D+00, &
        7.49999999999027092188D-01/)
!----------------------------------------------------------------------
!____________ End Numerics for DAW


!____________ Numerics for PSI
real(DP),save :: xlarge_PSI=271029746912941.21_DP
logical :: xlarge_PSI_set=.false.
real(DP),parameter  :: xmin1=MAX(one/biggest_DP,tiny_DP), xinf=biggest_DP, & !xsmall= .58e-8_DP,&
                xmax1=one/eps_DP,&
                fourth=unqua,piov4=fourth*Pi, &
                X01=187.0_DP,X01D=128.0_DP,X02=6.9464496836234126266e-04_DP
dimension :: p1psi(9),p2psi(7),q1psi(8),q2psi(6)
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x)/(x-x0)  over [0.5, 3.0]
!----------------------------------------------------------------------
real(DP),parameter :: p1psi=(/4.5104681245762934160D-03,5.4932855833000385356D+00,&
       3.7646693175929276856D+02,7.9525490849151998065D+03,&
       7.1451595818951933210D+04,3.0655976301987365674D+05,&
       6.3606997788964458797D+05,5.8041312783537569993D+05,&
       1.6585695029761022321D+05/)
real(DP),parameter :: q1psi=(/9.6141654774222358525D+01,2.6287715790581193330D+03,&
       2.9862497022250277920D+04,1.6206566091533671639D+05,&
       4.3487880712768329037D+05,5.4256384537269993733D+05,&
       2.4242185002017985252D+05,6.4155223783576225996D-08/)
!----------------------------------------------------------------------
!  Coefficients for approximation to  psi(x) - ln(x) + 1/(2x) 
!     for  x > 3.0
!----------------------------------------------------------------------
real(DP),parameter :: p2psi=(/-2.7103228277757834192D+00,-1.5166271776896121383D+01,&
       -1.9784554148719218667D+01,-8.8100958828312219821D+00,&
       -1.4479614616899842986D+00,-7.3689600332394549911D-02,&
       -6.5135387732718171306D-21/)
real(DP),parameter :: q2psi=(/ 4.4992760373789365846D+01, 2.0240955312679931159D+02,&
        2.4736979003315290057D+02, 1.0742543875702278326D+02,&
        1.7463965060678569906D+01, 8.8427520398873480342D-01/)
!----------------------------------------------------------------------
!____________ End Numerics for PSI


!____________ Numerics for ERF
real(DP),parameter :: SQRPI=one/sqrtPi,THRESH=0.46875D0,xneg=-26.628D0,xsmall_ERF=half*eps_DP, &
                      XBIG=26.543D0,xmax2=(one/tiny_DP)/sqrtPi !2.53D307

dimension :: Axerf(5),Bxerf(4),Cxerf(9),Dxerf(8),Pxerf(6),Qxerf(5)
!------------------------------------------------------------------
!  Coefficients for approximation to  erf  in first interval
!------------------------------------------------------------------
real(DP),parameter :: Axerf=(/3.16112374387056560D00,1.13864154151050156D02,&
             3.77485237685302021D02,3.20937758913846947D03,&
             1.85777706184603153D-1/)
real(DP),parameter :: Bxerf=(/2.36012909523441209D01,2.44024637934444173D02,&
             1.28261652607737228D03,2.84423683343917062D03/)
!------------------------------------------------------------------
!  Coefficients for approximation to  erfc  in second interval
!------------------------------------------------------------------
real(DP),parameter :: Cxerf=(/5.64188496988670089D-1,8.88314979438837594D0,&
             6.61191906371416295D01,2.98635138197400131D02,&
             8.81952221241769090D02,1.71204761263407058D03,&
             2.05107837782607147D03,1.23033935479799725D03,&
             2.15311535474403846D-8/)
real(DP),parameter :: Dxerf=(/1.57449261107098347D01,1.17693950891312499D02,&
             5.37181101862009858D02,1.62138957456669019D03,&
             3.29079923573345963D03,4.36261909014324716D03,&
             3.43936767414372164D03,1.23033935480374942D03/)
!------------------------------------------------------------------
!  Coefficients for approximation to  erfc  in third interval
!------------------------------------------------------------------
real(DP),parameter :: Pxerf=(/3.05326634961232344D-1,3.60344899949804439D-1,&
             1.25781726111229246D-1,1.60837851487422766D-2,&
             6.58749161529837803D-4,1.63153871373020978D-2/)
real(DP),parameter :: Qxerf=(/2.56852019228982242D00,1.87295284992346047D00,&
             5.27905102951428412D-1,6.05183413124413191D-2,&
             2.33520497626869185D-3/)
!____________ End Numerics for ERF
!____________ Numerics for LNGAMMA
real(DP),parameter :: THRHAL=1.5_DP,PNT68=0.6796875_DP, &
                      xbig2=2.55D305,FRTBIG=2.25D76

dimension :: Clgam(7),p1lgam(8),p2lgam(8),P4lgam(8),q1lgam(8),q2lgam(8),Q4lgam(8)
!----------------------------------------------------------------------
!  Numerator and denominator coefficients for rational minimax
!     approximation over (0.5,1.5).
!----------------------------------------------------------------------
real(DP),parameter :: D1=-5.772156649015328605195174D-1
real(DP),parameter :: p1lgam=(/4.945235359296727046734888D0,2.018112620856775083915565D2,&
              2.290838373831346393026739D3,1.131967205903380828685045D4,&
              2.855724635671635335736389D4,3.848496228443793359990269D4,&
              2.637748787624195437963534D4,7.225813979700288197698961D3/)
real(DP),parameter :: q1lgam=(/6.748212550303777196073036D1,1.113332393857199323513008D3,&
              7.738757056935398733233834D3,2.763987074403340708898585D4,&
              5.499310206226157329794414D4,6.161122180066002127833352D4,&
              3.635127591501940507276287D4,8.785536302431013170870835D3/)
!----------------------------------------------------------------------
!  Numerator and denominator coefficients for rational minimax
!     Approximation over (1.5,4.0).
!----------------------------------------------------------------------
real(DP),parameter :: D2=4.227843350984671393993777D-1
real(DP),parameter :: p2lgam=(/4.974607845568932035012064D0,5.424138599891070494101986D2,&
              1.550693864978364947665077D4,1.847932904445632425417223D5,&
              1.088204769468828767498470D6,3.338152967987029735917223D6,&
              5.106661678927352456275255D6,3.074109054850539556250927D6/)
real(DP),parameter :: q2lgam=(/1.830328399370592604055942D2,7.765049321445005871323047D3,&
              1.331903827966074194402448D5,1.136705821321969608938755D6,&
              5.267964117437946917577538D6,1.346701454311101692290052D7,&
              1.782736530353274213975932D7,9.533095591844353613395747D6/)
!----------------------------------------------------------------------
!  Numerator and denominator coefficients for rational minimax
!     Approximation over (4.0,12.0).
!----------------------------------------------------------------------
real(DP),parameter :: D4=1.791759469228055000094023D0
real(DP),parameter :: P4lgam=(/1.474502166059939948905062D4,2.426813369486704502836312D6,&
              1.214755574045093227939592D8,2.663432449630976949898078D9,&
            2.940378956634553899906876D10,1.702665737765398868392998D11,&
            4.926125793377430887588120D11,5.606251856223951465078242D11/)
real(DP),parameter :: Q4lgam=(/2.690530175870899333379843D3,6.393885654300092398984238D5,&
              4.135599930241388052042842D7,1.120872109616147941376570D9,&
            1.488613728678813811542398D10,1.016803586272438228077304D11,&
            3.417476345507377132798597D11,4.463158187419713286462081D11/)
!----------------------------------------------------------------------
!  Coefficients for minimax approximation over (12, INF).
!----------------------------------------------------------------------
real(DP),parameter :: Clgam=(/-1.910444077728D-03,8.4171387781295D-04,&
           -5.952379913043012D-04,7.93650793500350248D-04,&
           -2.777777777777681622553D-03,8.333333333333333331554247D-02,&
            5.7083835261D-03/)
!----------------------------------------------------------------------                  
! !----------------------------------------------------------------------
! !____________ End Numerics for LNGAMMA
! 


contains

FUNCTION DGAMMA(X)
implicit none
!----------------------------------------------------------------------
!
! This routine calculates the GAMMA function for a real argument X.
!   Computation is based on an algorithm outlined in reference 1.
!   The program uses rational functions that approximate the GAMMA
!   function to at least 20 significant decimal digits.  Coefficients
!   for the approximation over the interval (1,2) are unpublished.
!   Those for the approximation for X >= 12 are from reference 2.
!   The accuracy achieved depends on the arithmetic system, the
!   compiler, the intrinsic functions, and proper selection of the
!   machine-dependent constants.
!
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value biggest_DP for singularities or
!     when overflow would occur.  The computation is believed
!     to be free of underflow and overflow.
!
!
!  Intrinsic functions required are:
!
!     INT, DBLE, EXP, LOG, REAL, SIN
!
!
! References: "An Overview of Software Development for Special
!              Functions", W. J. Cody, Lecture Notes in Mathematics,
!              506, Numerical Analysis Dundee, 1975, G. A. Watson
!              (ed.), Springer Verlag, Berlin, 1976.
!
!              Computer Approximations, Hart, Et. Al., Wiley and
!              sons, New York, 1968.
!
!  Latest modification: October 12, 1989
!
!  Authors: W. J. Cody and L. Stoltz
!           Applied Mathematics Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!----------------------------------------------------------------------
INTEGER :: I,N
LOGICAL :: PARITY
real(DP) :: DGAMMA, FACT,RES,xxsum,X,XDEN,XNUM,Y,Y1,YSQ,Z
!----------------------------------------------------------------------
PARITY = .FALSE.
FACT = ONE
N = 0
Y = X
IF (Y <= ZERO) THEN
!----------------------------------------------------------------------
!  Argument is negative
!----------------------------------------------------------------------
   Y = -X
   Y1 = AINT(Y)
   RES = Y - Y1
   IF (RES .NE. ZERO) THEN
      IF (Y1 .NE. AINT(Y1*HALF)*TWO) PARITY = .TRUE.
      FACT = -PI / SIN(PI*RES)
      Y = Y + ONE
   ELSE
      RES = biggest_DP
      GO TO 900
   END IF
END IF
!----------------------------------------------------------------------
!  Argument is positive
!----------------------------------------------------------------------
IF (Y < eps_DP) THEN
!----------------------------------------------------------------------
!  Argument < eps_DP
!----------------------------------------------------------------------
   IF (Y >= tiny_DP) THEN
      RES = ONE / Y
   ELSE
      RES = biggest_DP
      GO TO 900
   END IF
ELSE IF (Y < TWELVE) THEN
Y1 = Y
IF (Y < ONE) THEN
!----------------------------------------------------------------------
!  0.0 < argument < 1.0
!----------------------------------------------------------------------
      Z = Y
      Y = Y + ONE
   ELSE
!----------------------------------------------------------------------
!  1.0 < argument < 12.0, reduce argument if necessary
!----------------------------------------------------------------------
      N = INT(Y) - 1
      Y = Y - real(N,kind=DP)
      Z = Y - ONE
END IF
!----------------------------------------------------------------------
!  Evaluate approximation for 1.0 < argument < 2.0
!----------------------------------------------------------------------
XNUM = ZERO
XDEN = ONE
DO I = 1, 8
   XNUM = (XNUM + P(I)) * Z
   XDEN = XDEN * Z + Q(I)
enddo
RES = XNUM / XDEN + ONE
IF (Y1 < Y) THEN
!----------------------------------------------------------------------
!  Adjust result_I for case  0.0 < argument < 1.0
!----------------------------------------------------------------------
      RES = RES / Y1
   ELSE IF (Y1 > Y) THEN
!----------------------------------------------------------------------
!  Adjust result_I for case  2.0 < argument < 12.0
!----------------------------------------------------------------------
      DO I = 1, N
         RES = RES * Y
         Y = Y + ONE
      Enddo
END IF
   ELSE
!----------------------------------------------------------------------
!  Evaluate for argument >= 12.0,
!----------------------------------------------------------------------
IF (Y <= max_arg_gamma) THEN
      YSQ = Y * Y
      xxsum = C(7)
      DO I = 1, 6
         xxsum = xxsum / YSQ + C(I)
      Enddo
      xxsum = xxsum/Y - Y + chczz
      xxsum = xxsum + (Y-HALF)*LOG(Y)
      RES = EXP(xxsum)
   ELSE
      RES = biggest_DP
      GO TO 900
END IF
END IF
!----------------------------------------------------------------------
!  Final adjustments and return
!----------------------------------------------------------------------
IF (PARITY) RES = -RES
IF (FACT .NE. ONE) RES = FACT / RES
  900 DGAMMA = RES
END function DGAMMA
!************************************************************************
FUNCTION DAW(XX)
!___________ From SPECFUN:
!----------------------------------------------------------------------
!
! This function program evaluates Dawson's integral, 
!
!                       2  / x   2
!                     -x   |    t
!             F(x) = e     |   e    dt
!                          |
!                          / 0
!
! \[  F(x)= \exp(-x^2) \int_0^x{\mathrm{d}}t \exp(t^2), \qquad x\equiv{\mathtt{XX}};\quad F\equiv{\mathtt{DAW}} \]
!
!   for a real argument x.
!
!   The calling sequence for this function is 
!
!                   Y=DAW(X)
!
!   The main computation uses rational Chebyshev approximations
!   published in Math. Comp. 24, 171-178 (1970) by Cody, Paciorek
!   and Thacher.  This transportable program is patterned after the
!   machine-dependent FUNPACK program DDAW(X), but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   approximations that are theoretically accurate to about 19
!   significant decimal digits.  The accuracy achieved depends on the
!   arithmetic system, the compiler, the intrinsic functions, and
!   proper selection of the machine-dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Error Returns
!
!  The program returns 0.0 for |X| > XMAX.
!
! Intrinsic functions required are:
!
!     ABS
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division 
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: June 15, 1988
!
!----------------------------------------------------------------------
  implicit none
  REAL(DP),intent(IN) :: XX
  INTEGER(I4B)  :: I
  REAL(DP) :: DAW,FRAC,SUMP,SUMQ,W2,X,Y

  X = XX
  IF (ABS(X) > XLARGE) THEN
     IF (ABS(X) <= XMAX) THEN
       DAW = HALF / X
     ELSE
       DAW = ZERO
     endif
  ELSE IF (ABS(X) < sceps_DP) THEN
     DAW = X
  ELSE
     Y = X * X
     IF (Y < SIX25) THEN
!----------------------------------------------------------------------
!  ABS(X) .LT. 2.5 
!----------------------------------------------------------------------
        SUMP = P1(1)
        SUMQ = Q1(1)
        DO I = 2, 10
          SUMP = SUMP * Y + P1(I)
          SUMQ = SUMQ * Y + Q1(I)
        enddo
        DAW = X * SUMP / SUMQ
     ELSE IF (Y >= SIX25 .and. Y < ONE225) THEN
!----------------------------------------------------------------------
!  2.5 .LE. ABS(X) .LT. 3.5 
!----------------------------------------------------------------------
        FRAC = ZERO
        DO I = 1, 9
          FRAC = Q2(I) / (P2(I) + Y + FRAC)
        enddo
        DAW = (P2(10) + FRAC) / X
     ELSE IF (Y >= ONE225 .and. Y < TWO5) THEN
!----------------------------------------------------------------------
!  3.5 .LE. ABS(X) .LT. 5.0 
!---------------------------------------------------------------------
        FRAC = ZERO
        DO I = 1, 9
          FRAC = Q3(I) / (P3(I) + Y + FRAC)
        enddo
        DAW = (P3(10) + FRAC) / X
     ELSE
!----------------------------------------------------------------------
!  5.0 .LE. ABS(X) .LE. XLARGE 
!------------------------------------------------------------------
        W2 = ONE / (X * X)
        FRAC = ZERO
        DO I = 1, 9
          FRAC = Q4(I) / (P4(I) + Y + FRAC)
        enddo
        FRAC = P4(10) + FRAC
        DAW = (HALF + HALF * W2 * FRAC) / X
      endif
  endif
!---------- Last line of DAW ----------
END FUNCTION DAW
!*************************************************************************
subroutine SETUP_PSI
!_______ Solves numerically 1/(x*log(x)) = eps_DP/2
implicit none
real(DP)  :: la,v,v0
la=-log(half*eps_DP)
v=la-log(la)
v0=v
do
  v=la-log(v)
  if (abs((v-v0)/v)<sceps_DP) exit
  v0=v
enddo
xlarge_PSI=two/(eps_DP*v)
xlarge_PSI_set=.true.
end subroutine SETUP_PSI
!*************************************************************************
FUNCTION PSI(XX)
!----------------------------------------------------------------------
!
! This function program evaluates the logarithmic derivative of the
!   gamma function, 
!
!      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x))
!
!   for real x, where either
!
!          -xmax1 < x < -xmin (x not a negative integer), or
!            xmin < x.
!
!   The calling sequence for this function is 
!
!                  Y = PSI(X)
!
!   The main computation uses rational Chebyshev approximations
!   published in Math. Comp. 27, 123-127 (1973) by Cody, Strecok and
!   Thacher.  This transportable program is patterned after the
!   machine-dependent FUNPACK program PSI(X), but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   approximations that are theoretically accurate to 20 significant
!   decimal digits.  The accuracy achieved depends on the arithmetic
!   system, the compiler, the intrinsic functions, and proper selection
!   of the machine-dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   XINF   = largest positive machine number
!   XMAX1  = beta ** (p-1), where beta is the radix for the
!            floating-point system, and p is the number of base-beta
!            digits in the floating-point significand.  This is an
!            upper bound on non-integral floating-point numbers, and
!            the negative of the lower bound on acceptable negative
!            arguments for PSI.  If rounding is necessary, round this
!            value down.
!   XMIN1  = the smallest in magnitude acceptable argument.  We
!            recommend XMIN1 = MAX(1/XINF,xmin) rounded up, where
!            xmin is the smallest positive floating-point number.
!   XSMALL = absolute argument below which  PI*COTAN(PI*X)  may be
!            represented by 1/X.  We recommend XSMALL < sqrt(3 eps)/pi,
!            where eps is the smallest positive number such that
!            1+eps > 1. 
!   xlarge_PSI = argument beyond which PSI(X) may be represented by
!            LOG(X).  The solution to the equation
!               x*ln(x) = beta ** p
!            is a safe value.
!
!     Approximate values for some important machines are
!
!                        beta  p     eps     xmin       XINF  
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2  24  1.19E-07  1.18E-38   3.40E+38
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2  53  1.11D-16  2.23E-308  1.79D+308
!
!                         XMIN1      XMAX1     XSMALL    xlarge_PSI
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)  1.18E-38   8.38E+06  1.90E-04  1.20E+06
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  2.23D-308  4.50D+15  5.80D-09  2.71D+14
!
!*******************************************************************
!*******************************************************************
!
! Error Returns
!
!  The program returns XINF for  X < -XMAX1, for X zero or a negative
!    integer, or when X lies in (-XMIN1, 0), and returns -XINF
!    when X lies in (0, XMIN1).
!
! Intrinsic functions required are:
!
!     ABS, AINT, DBLE, INT, LOG, REAL, TAN
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division 
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: June 8, 1988
!
!----------------------------------------------------------------------
      implicit none
      INTEGER(I4B) :: I,N,NQ
      real(DP) :: AUG,DEN,PSI,SGN,UPPER,W,X,XX,Z
!----------------------------------------------------------------------
!  Zero of psi(x)
!----------------------------------------------------------------------
      if (.not.xlarge_PSI_set) call SETUP_PSI
      X = XX
      W = ABS(X)
      AUG = ZERO
!----------------------------------------------------------------------
!  Check for valid arguments, then branch to appropriate algorithm
!----------------------------------------------------------------------
      IF ((-X .GE. XMAX1) .OR. (W .LT. XMIN1)) THEN
            GO TO 410
         ELSE IF (X .GE. HALF) THEN
            GO TO 200
!----------------------------------------------------------------------
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!     Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.  
!----------------------------------------------------------------------
         ELSE IF (W .LE. XSMALL) THEN
            AUG = -ONE / X
            GO TO 150
      END IF
!----------------------------------------------------------------------
!  Argument reduction for cot
!----------------------------------------------------------------------
  100 IF (X .LT. ZERO) THEN
            SGN = PIOV4
         ELSE
            SGN = -PIOV4
      END IF
      W = W - AINT(W)
      NQ = INT(W * FOUR)
      W = FOUR * (W - real(NQ,kind=DP) * FOURTH)
!----------------------------------------------------------------------
!  W is now related to the fractional part of  4.0 * X.
!     Adjust argument to correspond to values in the first
!     quadrant and determine the sign.
!----------------------------------------------------------------------
      N = NQ / 2
      IF ((N+N) .NE. NQ) W = ONE - W
      Z = PIOV4 * W
      IF (MOD(N,2) .NE. 0) SGN = - SGN
!----------------------------------------------------------------------
!  determine the final value for  -pi * cotan(pi*x)
!----------------------------------------------------------------------
      N = (NQ + 1) / 2
      IF (MOD(N,2) .EQ. 0) THEN
!----------------------------------------------------------------------
!  Check for singularity
!----------------------------------------------------------------------
            IF (Z .EQ. ZERO) GO TO 410
            AUG = SGN * (FOUR / TAN(Z))
         ELSE
            AUG = SGN * (FOUR * TAN(Z))
      END IF
  150 X = ONE - X
  200 IF (X .GT. THREE) GO TO 300
!----------------------------------------------------------------------
!  0.5 <= X <= 3.0
!----------------------------------------------------------------------
      DEN = X
      UPPER = p1psi(1) * X
      DO 210 I = 1, 7
         DEN = (DEN + q1psi(I)) * X
         UPPER = (UPPER + p1psi(I+1)) * X
  210 CONTINUE
      DEN = (UPPER + p1psi(9)) / (DEN + q1psi(8))
      X = (X-X01/X01D) - X02
      PSI = DEN * X + AUG
      GO TO 500
!----------------------------------------------------------------------
!  3.0 < X 
!----------------------------------------------------------------------
  300 IF (X .LT. xlarge_PSI) THEN
         W = ONE / (X * X)
         DEN = W
         UPPER = p2psi(1) * W
         DO 310 I = 1, 5
            DEN = (DEN + q2psi(I)) * W
            UPPER = (UPPER + p2psi(I+1)) * W
  310    CONTINUE
         AUG = (UPPER + p2psi(7)) / (DEN + q2psi(6)) - HALF / X + AUG
      END IF
      PSI = AUG + LOG(X)
      GO TO 500
!----------------------------------------------------------------------
!  Error return
!----------------------------------------------------------------------
  410 PSI = XINF
      IF (X .GT. ZERO) PSI = -XINF
  500 RETURN
!---------- Last card of PSI ----------
END function psi
      
!*******************************************************************
SUBROUTINE CALERF(ARG,reslut,JINT)
!------------------------------------------------------------------
!
! This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x)
!   for a real argument  x.  It contains three FUNCTION type
!   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX),
!   and one SUBROUTINE type subprogram, CALERF.  The calling
!   statements for the primary entries are:
!
!                   Y=ERF(X)     (or   Y=DERF(X)),
!
!                   Y=ERFC(X)    (or   Y=DERFC(X)),
!   and
!                   Y=ERFCX(X)   (or   Y=DERFCX(X)).
!
!   The routine  CALERF  is intended for internal packet use only,
!   all computations within the packet being concentrated in this
!   routine.  The function subprograms invoke  CALERF  with the
!   statement
!
!          CALL CALERF(ARG,reslut,JINT)
!
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALERF
!       call              ARG                  reslut          JINT
!
!     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0
!     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1
!     ERFCX(ARG)    XNEG .LT. ARG .LT. xmax2   ERFCX(ARG)        2
!
!   The main computation evaluates near-minimax approximations
!   from "Rational Chebyshev approximations for the error function"
!   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
!   transportable program uses rational functions that theoretically
!   approximate  erf(x)  and  erfc(x)  to at least 18 significant
!   decimal digits.  The accuracy achieved depends on the arithmetic
!   system, the compiler, the intrinsic functions, and proper
!   selection of the machine-dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   tiny_DP   = the smallest positive floating-point number.
!   XINF   = the largest positive finite floating-point number.
!   XNEG   = the largest negative argument acceptable to ERFCX;
!            the negative of the solution to the equation
!            2*exp(x*x) = XINF.
!   xsmall_ERF = argument below which erf(x) may be represented by
!            2*x/sqrt(pi)  and above which  x*x  will not underflow.
!            A conservative value is the largest machine number X
!            such that   1.0 + X = 1.0   to machine precision.
!   XBIG   = largest argument acceptable to ERFC;  solution to
!            the equation:  W(x) * (1-0.5/x**2) = tiny_DP,  where
!            W(x) = exp(-x*x)/[x*sqrt(pi)].
!   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to
!            machine precision.  A conservative value is
!            1/[2*sqrt(xsmall_ERF)]. Here set to 1/sqrt(epsilon(1.d0))
!   xmax2   = largest acceptable argument to ERFCX; the minimum
!            of XINF and 1/[sqrt(pi)*tiny_DP].
!
!   Approximate values for some important machines are:
!
!                          tiny_DP       XINF        XNEG     xsmall_ERF
!
!  CDC 7600      (S.P.)  3.13E-294   1.26E+322   -27.220  7.11E-15
!  CRAY-1        (S.P.)  4.58E-2467  5.45E+2465  -75.345  7.11E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)  1.18E-38    3.40E+38     -9.382  5.96E-8
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16
!  IBM 195       (D.P.)  5.40D-79    7.23E+75    -13.190  1.39D-17
!  UNIVAC 1108   (D.P.)  2.78D-309   8.98D+307   -26.615  1.73D-18
!  VAX D-Format  (D.P.)  2.94D-39    1.70D+38     -9.345  1.39D-17
!  VAX G-Format  (D.P.)  5.56D-309   8.98D+307   -26.615  1.11D-16
!
!
!                          XBIG       XHUGE       xmax2
!
!  CDC 7600      (S.P.)  25.922      8.39E+6     1.80X+293
!  CRAY-1        (S.P.)  75.326      8.39E+6     5.45E+2465
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)   9.194      2.90E+3     4.79E+37
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307
!  IBM 195       (D.P.)  13.306      1.90D+8     7.23E+75
!  UNIVAC 1108   (D.P.)  26.582      5.37D+8     8.98D+307
!  VAX D-Format  (D.P.)   9.269      1.90D+8     1.70D+38
!  VAX G-Format  (D.P.)  26.569      6.71D+7     8.98D+307
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns  ERFC = 0      for  ARG .GE. XBIG;
!
!                       ERFCX = XINF  for  ARG .LT. XNEG;
!      and
!                       ERFCX = 0     for  ARG .GE. xmax2.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, EXP
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: March 19, 1990
!
!------------------------------------------------------------------
      implicit none
      INTEGER(I4B) :: I,JINT
      real(DP) :: ARG,DEL,reslut,X,XDEN,XNUM,Y,YSQ
!------------------------------------------------------------------
      X = ARG
      Y = ABS(X)
      IF (Y .LE. THRESH) THEN
!------------------------------------------------------------------
!  Evaluate  erf  for  |X| <= 0.46875
!------------------------------------------------------------------
            YSQ = ZERO
            IF (Y .GT. xsmall_ERF) YSQ = Y * Y
            XNUM = Axerf(5)*YSQ
            XDEN = YSQ
            DO 20 I = 1, 3
               XNUM = (XNUM + Axerf(I)) * YSQ
               XDEN = (XDEN + Bxerf(I)) * YSQ
   20       CONTINUE
            reslut = X * (XNUM + Axerf(4)) / (XDEN + Bxerf(4))
            IF (JINT .NE. 0) reslut = ONE - reslut
            IF (JINT .EQ. 2) reslut = EXP(YSQ) * reslut
            GO TO 800
!------------------------------------------------------------------
!  Evaluate  erfc  for 0.46875 <= |X| <= 4.0
!------------------------------------------------------------------
         ELSE IF (Y .LE. FOUR) THEN
            XNUM = Cxerf(9)*Y
            XDEN = Y
            DO 120 I = 1, 7
               XNUM = (XNUM + Cxerf(I)) * Y
               XDEN = (XDEN + Dxerf(I)) * Y
  120       CONTINUE
            reslut = (XNUM + Cxerf(8)) / (XDEN + Dxerf(8))
            IF (JINT .NE. 2) THEN
               YSQ = AINT(Y*sixteen)/sixteen
               DEL = (Y-YSQ)*(Y+YSQ)
               reslut = EXP(-YSQ*YSQ) * EXP(-DEL) * reslut
            END IF
!------------------------------------------------------------------
!  Evaluate  erfc  for |X| > 4.0
!------------------------------------------------------------------
         ELSE
            reslut = ZERO
            IF (Y .GE. XBIG) THEN
               IF ((JINT .NE. 2) .OR. (Y .GE. xmax2)) GO TO 300
               IF (Y .GE. one/sceps_DP) THEN
                  reslut = SQRPI / Y
                  GO TO 300
               END IF
            END IF
            YSQ = ONE / (Y * Y)
            XNUM = Pxerf(6)*YSQ
            XDEN = YSQ
            DO 240 I = 1, 4
               XNUM = (XNUM + Pxerf(I)) * YSQ
               XDEN = (XDEN + Qxerf(I)) * YSQ
  240       CONTINUE
            reslut = YSQ *(XNUM + Pxerf(5)) / (XDEN + Qxerf(5))
            reslut = (SQRPI -  reslut) / Y
            IF (JINT .NE. 2) THEN
               YSQ = AINT(Y*sixteen)/sixteen
               DEL = (Y-YSQ)*(Y+YSQ)
               reslut = EXP(-YSQ*YSQ) * EXP(-DEL) * reslut
            END IF
      END IF
!------------------------------------------------------------------
!  Fix up for negative argument, erf, etc.
!------------------------------------------------------------------
  300 IF (JINT .EQ. 0) THEN
            reslut = (HALF - reslut) + HALF
            IF (X .LT. ZERO) reslut = -reslut
         ELSE IF (JINT .EQ. 1) THEN
            IF (X .LT. ZERO) reslut = TWO - reslut
         ELSE
            IF (X .LT. ZERO) THEN
               IF (X .LT. XNEG) THEN
                     reslut = XINF
                  ELSE
                     YSQ = AINT(X*sixteen)/sixteen
                     DEL = (X-YSQ)*(X+YSQ)
                     Y = EXP(YSQ*YSQ) * EXP(DEL)
                     reslut = (Y+Y) - reslut
               END IF
            END IF
      END IF
  800 RETURN
END subroutine CALERF
!********************************************************************
FUNCTION DERF(X)
      implicit none
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erf(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
      INTEGER  :: JINT
      real(DP) :: DERF,X, reslut
!------------------------------------------------------------------
      JINT = 0
      CALL CALERF(X,reslut,JINT)
      DERF = reslut
END function derf
!*************************
FUNCTION DERFC(X)
      implicit none
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, January 8, 1985
!
!--------------------------------------------------------------------
      INTEGER  :: JINT
      real(DP) ::  DERFC,X, reslut
!------------------------------------------------------------------
      JINT = 1
      CALL CALERF(X,reslut,JINT)
      DERFC = reslut
      END function derfc
!*************************
FUNCTION DERFCX(X)
      implicit none
!------------------------------------------------------------------
!
! This subprogram computes approximate values for exp(x*x) * erfc(x).
!   (see comments heading CALERF).
!
!   Author/date: W. J. Cody, March 30, 1987
!
!------------------------------------------------------------------
      INTEGER  :: JINT
      real(DP) ::  DERFCX,X, reslut
!------------------------------------------------------------------
      JINT = 2
      CALL CALERF(X,reslut,JINT)
      DERFCX = reslut
END function derfcx
!***********************************************************************
FUNCTION lngamma(X)
!----------------------------------------------------------------------
!
! This routine calculates the LOG(GAMMA) function for a positive real
!   argument X.  Computation is based on an algorithm outlined in
!   references 1 and 2.  The program uses rational functions that
!   theoretically approximate LOG(GAMMA) to at least 18 significant
!   decimal digits.  The approximation for X > 12 is from reference
!   3, while approximations for X < 12.0 are similar to those in
!   reference 1, but are unpublished.  The accuracy achieved depends
!   on the arithmetic system, the compiler, the intrinsic functions,
!   and proper selection of the machine-dependent constants.
!
!
!*********************************************************************
!*********************************************************************
!
! Explanation of machine-dependent constants
!
! beta   - radix for the floating-point representation
! maxexp - the smallest positive power of beta that overflows
! xbig2   - largest argument for which LN(GAMMA(X)) is representable
!          in the machine, i.e., the solution to the equation
!                  LN(GAMMA(xbig2)) = beta**maxexp
! XINF   - largest machine representable floating-point number;
!          approximately beta**maxexp.
! eps_DP    - The smallest positive floating-point number such that
!          1.0+eps_DP > 1.0
! FRTBIG - Rough estimate of the fourth root of xbig2
!
!
!     Approximate values for some important machines are:
!
!                           beta      maxexp         xbig2
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)        2         128       4.08E+36
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)        2        1024       2.55D+305
!
!                           XINF        eps_DP        FRTBIG
!
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76
!
!**************************************************************
!**************************************************************
!
! Error returns
!
!  The program returns the value XINF for X <= 0.0 or when
!     overflow would occur.  The computation is believed to 
!     be free of underflow and overflow.
!
!
! Intrinsic functions required are:
!
!      LOG
!
!
! References:
!
!  1) W. J. Cody and K. E. Hillstrom, 'Chebyshev Approximations for
!     the Natural Logarithm of the Gamma Function,' Math. Comp. 21,
!     1967, pp. 198-203.
!
!  2) K. E. Hillstrom, ANL/AMD Program ANLC366S, DGAMMA/lngamma, May,
!     1969.
! 
!  3) Hart, Et. Al., Computer Approximations, Wiley and sons, New
!     York, 1968.
!
!
!  Authors: W. J. Cody and L. Stoltz
!           Argonne National Laboratory
!
!  Latest modification: June 16, 1988
!
!----------------------------------------------------------------------
implicit none
INTEGER  :: I
real(DP) ::  lngamma,CORR,RES,X,XDEN,&
    XM1,XM2,XM4,XNUM,Y,YSQ
!----------------------------------------------------------------------
Y = X
IF ((Y > ZERO) .AND. (Y <= xbig2)) THEN
   IF (Y <= eps_DP) THEN
      RES = -LOG(Y)
   ELSE IF (Y <= THRHAL) THEN
!----------------------------------------------------------------------
!  eps_DP < X <= 1.5
!----------------------------------------------------------------------
      IF (Y < PNT68) THEN
         CORR = -LOG(Y)
        XM1 = Y
      ELSE
         CORR = ZERO
         XM1 = (Y - HALF) - HALF
      END IF
      IF ((Y <= HALF) .OR. (Y >= PNT68)) THEN
         XDEN = ONE
         XNUM = ZERO
         DO I = 1, 8
            XNUM = XNUM*XM1 + p1lgam(I)
            XDEN = XDEN*XM1 + q1lgam(I)
         ENDDO
         RES = CORR + (XM1 * (D1 + XM1*(XNUM/XDEN)))
      ELSE
         XM2 = (Y - HALF) - HALF
         XDEN = ONE
         XNUM = ZERO
         DO I = 1, 8
            XNUM = XNUM*XM2 + p2lgam(I)
            XDEN = XDEN*XM2 + q2lgam(I)
         ENDDO
         RES = CORR + XM2 * (D2 + XM2*(XNUM/XDEN))
      ENDIF
   ELSE IF (Y <= FOUR) THEN
!----------------------------------------------------------------------
!  1.5 < X <= 4.0
!----------------------------------------------------------------------
      XM2 = Y - TWO
      XDEN = ONE
      XNUM = ZERO
      DO I = 1, 8
         XNUM = XNUM*XM2 + p2lgam(I)
         XDEN = XDEN*XM2 + q2lgam(I)
      ENDDO
      RES = XM2 * (D2 + XM2*(XNUM/XDEN))
   ELSE IF (Y <= TWELVE) THEN
!----------------------------------------------------------------------
!  4.0 < X <= 12.0
!----------------------------------------------------------------------
      XM4 = Y - FOUR
      XDEN = -ONE
      XNUM = ZERO
      DO I = 1, 8
         XNUM = XNUM*XM4 + P4lgam(I)
         XDEN = XDEN*XM4 + Q4lgam(I)
      ENDDO
      RES = D4 + XM4*(XNUM/XDEN)
   ELSE 
!----------------------------------------------------------------------
!  Evaluate for argument >= 12.0,
!----------------------------------------------------------------------
      RES = ZERO
      IF (Y <= FRTBIG) THEN
         RES = Clgam(7)
         YSQ = Y * Y
         DO I = 1, 6
            RES = RES / YSQ + Clgam(I)
         ENDDO
      END IF
      RES = RES/Y
      CORR = LOG(Y)
      RES = RES + chczz - HALF*CORR
      RES = RES + Y*(CORR-ONE)
   END IF
ELSE
!----------------------------------------------------------------------
!  Return for bad arguments
!----------------------------------------------------------------------
      RES = XINF
END IF
!----------------------------------------------------------------------
!  Final adjustments and return
!----------------------------------------------------------------------
lngamma = RES
END function lngamma
!**********************************************************************

end module SPECIAL_GAMMAERF
!______________________________________________________________________
module morespec
use nano_deftyp,ONLY : DP,zero,one,two,three,four,six,eight,twelve,fifteen,Pi,Pi2, &
                       twoonpi,half, D1MACH,nineteen,five
use SPECIAL_GAMMAERF,only : Spec_GAMMA
private
public :: Spec_BesselJ0,Spec_BesselJ1
public :: Spec_BesselY0,Spec_BesselY1
public :: Spec_BesselK0,Spec_BesselK1,Spec_BesselK0_Expx,Spec_BesslK1_Expx
public :: Spec_BesselI0,Spec_BesselI1,Spec_BesselI0_Expmx,Spec_BesslI1_Expmx
public :: Spec_Ei,Spec_E1,Spec_Ei_Expmx

real(DP),parameter :: twopi=pi2, fourty=40.0_DP, eighth=0.125_DP, oneov8=eighth, &
                      twopi1=6.28125d0,twopi2=1.93530717958647692528676655900576d-3, &
                      xsmalls=d1mach(3),xsmallh=half*xsmalls,sxsmall=sqrt(d1mach(3))*half, &
                      exp40=2.35385266837019985407899910749034805d17, two25=fifteen*fifteen, &
                      REC15 = 6.66666666666666666666666666666666666667D-2,two4=24.d0, &
                      ln_twoHUGEoversqrtPi = 708.984556597814569323810881082548835d0

interface Spec_Ei
  module procedure EI
end interface
interface Spec_E1
  module procedure EONE
end interface
interface Spec_Ei_Expmx
  module procedure EXPEI
end interface
                  
interface Spec_BesselJ0
  module procedure besj0
end interface
interface Spec_BesselJ1
  module procedure besj1
end interface

interface Spec_BesselY0
  module procedure besY0
end interface
interface Spec_BesselY1
  module procedure besY1
end interface

interface Spec_BesselK0
  module procedure besK0
end interface
interface Spec_BesselK1
  module procedure besK1
end interface
interface Spec_BesselK0_Expx
  module procedure beseK0
end interface
interface Spec_BesselK1_Expx
  module procedure beseK1
end interface

interface Spec_BesselI0
  module procedure besI0
end interface
interface Spec_BesselI1
  module procedure besI1
end interface
interface Spec_BesselI0_Expmx
  module procedure beseI0
end interface
interface Spec_BesselI1_Expmx
  module procedure beseI1
end interface



contains



SUBROUTINE CALCEI (ARG, RESLUT, INTX)
!----------------------------------------------------------------------
!
! This Fortran 77 packet computes the exponential integrals Ei(x),
!  E1(x), and  exp(-x)*Ei(x)  for real arguments  x  where
!
!           integral (from t=-infinity to t=x) (exp(t)/t),  x > 0,
!  Ei(x) =
!          -integral (from t=-x to t=infinity) (exp(t)/t),  x < 0,
!
!  and where the first integral is a principal value integral.
!  The packet contains three function type subprograms: EI, EONE,
!  and EXPEI;  and one subroutine type subprogram: CALCEI.  The
!  calling statements for the primary entries are
!
!                 Y = EI(X),            where  X  /=  0,
!
!                 Y = EONE(X),          where  X  >  0,
!  and
!                 Y = EXPEI(X),         where  X  /=  0,
!
!  and where the entry points correspond to the functions Ei(x),
!  E1(x), and exp(-x)*Ei(x), respectively.  The routine CALCEI
!  is intended for internal packet use only, all computations within
!  the packet being concentrated in this routine.  The function
!  subprograms invoke CALCEI with the Fortran statement
!         CALL CALCEI(ARG,RESLUT,INTX)
!  where the parameter usage is as follows
!
!     Function                  Parameters for CALCEI
!       Call                 ARG             RESLUT         INTX
!
!      EI(X)              X  /=  0          Ei(X)            1
!      EONE(X)            X  >  0          -Ei(-X)           2
!      EXPEI(X)           X  /=  0          exp(-X)*Ei(X)    3
!
!  The main computation involves evaluation of rational Chebyshev
!  approximations published in Math. Comp. 22, 641-649 (1968), and
!  Math. Comp. 23, 289-303 (1969) by Cody and Thacher.  This
!  transportable program is patterned after the machine-dependent
!  FUNPACK packet  NATSEI,  but cannot match that version for
!  efficiency or accuracy.  This version uses rational functions
!  that theoretically approximate the exponential integrals to
!  at least 18 significant decimal digits.  The accuracy achieved
!  depends on the arithmetic system, the compiler, the intrinsic
!  functions, and proper selection of the machine-dependent
!  constants.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta = radix for the floating-point system.
!   minexp = smallest representable power of beta.
!   maxexp = smallest power of beta that overflows.
!   XBIG = largest argument acceptable to EONE; solution to
!          equation:
!                     exp(-x)/x * (1 + 1/x) = beta ** minexp.
!   XINF = largest positive machine number; approximately
!                     beta ** maxexp
!   XMAX = largest argument acceptable to EI; solution to
!          equation:  exp(x)/x * (1 + 1/x) = beta ** maxexp.
!
!     Approximate values for some important machines are:
!
!                           beta      minexp      maxexp
!
!  CRAY-1        (S.P.)       2       -8193        8191
!  Cyber 180/185
!    under NOS   (S.P.)       2        -975        1070
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)       2        -126         128
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)       2       -1022        1024
!  IBM 3033      (D.P.)      16         -65          63
!  VAX D-Format  (D.P.)       2        -128         127
!  VAX G-Format  (D.P.)       2       -1024        1023
!
!                           XBIG       XINF       XMAX
!
!  CRAY-1        (S.P.)    5670.31  5.45E+2465   5686.21
!  Cyber 180/185
!    under NOS   (S.P.)     669.31  1.26E+322     748.28
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)      82.93  3.40E+38       93.24
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)     701.84  1.79D+308     716.35
!  IBM 3033      (D.P.)     175.05  7.23D+75      179.85
!  VAX D-Format  (D.P.)      84.30  1.70D+38       92.54
!  VAX G-Format  (D.P.)     703.22  8.98D+307     715.66
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The following table shows the types of error that may be
!  encountered in this routine and the function value supplied
!  in each case.
!
!       Error       Argument         Function values for
!                    Range         EI      EXPEI     EONE
!
!     UNDERFLOW  (-)X  >  XBIG     0        -         0
!     OVERFLOW      X  >=  XMAX    XINF      -         -
!     ILLEGAL X       X = 0       -XINF    -XINF     XINF
!     ILLEGAL X      X  <  0       -        -     USE ABS(X)
!
! Intrinsic functions required are:
!
!     ABS, SQRT, EXP
!
!
!  Author: W. J. Cody
!          Mathematics abd Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: September 9, 1988
!
!----------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: ARG
    integer,intent(IN)  :: INTX
    real(DP),intent(OUT):: RESLUT
    INTEGER  :: I
    real(DP) :: EI, FRAC, XUMP, XUMQ, T, W, X, XMX0, Y, YSQ
    real(DP) :: PX(10),QX(10)
!----------------------------------------------------------------------
!  Mathematical constants
!   EXP40 = exp(40)
!   X0 = zero of Ei
!   X01/X11 + X02 = zero of Ei to extra precision
!----------------------------------------------------------------------
    real(DP),parameter :: P037=0.037D0, X01=381.5D0, X11=1024.0D0, &
                          X02=-5.1182968633365538008D-5, &
                          X0 = 3.7250741078136663466D-1
!----------------------------------------------------------------------
! Machine-dependent constants
!----------------------------------------------------------------------
    real(DP),parameter :: XMAX = 716.355494456253340987559682670871926D0, &
                          XBIG = 701.844130992210579854754701371421155D0
!----------------------------------------------------------------------
! Coefficients  for -1.0 <= X < 0.0
!----------------------------------------------------------------------
    real(DP),parameter :: A(7)=(/ 1.1669552669734461083368D2, 2.1500672908092918123209D3,  &
    1.5924175980637303639884D4, 8.9904972007457256553251D4,           &
    1.5026059476436982420737D5, - 1.4815102102575750838086D5,         &
    5.0196785185439843791020D0 /)
    real(DP),parameter :: B(6)=(/ 4.0205465640027706061433D1, 7.5043163907103936624165D2,  &
    8.1258035174768735759855D3, 5.2440529172056355429883D4,           &
    1.8434070063353677359298D5, 2.5666493484897117319268D5 /)
!----------------------------------------------------------------------
! Coefficients for -4.0 <= X < -1.0
!----------------------------------------------------------------------
    real(DP),parameter :: C(9)=(/ 3.828573121022477169108D-1, 1.107326627786831743809D+1,  &
    7.246689782858597021199D+1, 1.700632978311516129328D+2,           &
    1.698106763764238382705D+2, 7.633628843705946890896D+1,           &
    1.487967702840464066613D+1, 9.999989642347613068437D-1,           &
    1.737331760720576030932D-8 /)
    real(DP),parameter :: D(9)=(/ 8.258160008564488034698D-2, 4.344836335509282083360D+0,  &
    4.662179610356861756812D+1, 1.775728186717289799677D+2,           &
    2.953136335677908517423D+2, 2.342573504717625153053D+2,           &
    9.021658450529372642314D+1, 1.587964570758947927903D+1,           &
    1.000000000000000000000D+0 /)
!----------------------------------------------------------------------
! Coefficients for X < -4.0
!----------------------------------------------------------------------
    real(DP),parameter :: E(10)=(/ 1.3276881505637444622987D+2, 3.5846198743996904308695D+4,&
    1.7283375773777593926828D+5, 2.6181454937205639647381D+5,         &
    1.7503273087497081314708D+5, 5.9346841538837119172356D+4,         &
    1.0816852399095915622498D+4, 1.0611777263550331766871D03,         &
    5.2199632588522572481039D+1, 9.9999999999999999087819D-1 /)
    real(DP),parameter :: F(10)=(/ 3.9147856245556345627078D+4, 2.5989762083608489777411D+5,&
    5.5903756210022864003380D+5, 5.4616842050691155735758D+5,         &
    2.7858134710520842139357D+5, 7.9231787945279043698718D+4,         &
    1.2842808586627297365998D+4, 1.1635769915320848035459D+3,         &
    5.4199632588522559414924D+1, 1.0D0 /)
!----------------------------------------------------------------------
!  Coefficients for rational approximation to ln(x/a), |1-x/a| < .1
!----------------------------------------------------------------------
    real(DP),parameter :: PLG(4)=(/ - 2.4562334077563243311D+01, 2.3642701335621505212D+02,&
    - 5.4989956895857911039D+02, 3.5687548468071500413D+02 /)
    real(DP),parameter :: QLG(4)=(/ - 3.5553900764052419184D+01, 1.9400230218539473193D+02,&
    - 3.3442903192607538956D+02, 1.7843774234035750207D+02 /)
!----------------------------------------------------------------------
! Coefficients for  0.0 < X < 6.0,
!  ratio of Chebyshev polynomials
!----------------------------------------------------------------------
    real(DP),parameter :: P(10)=(/ - 1.2963702602474830028590D01, &
    -1.2831220659262000678155D03, - 1.4287072500197005777376D04,       &
    - 1.4299841572091610380064D06, - 3.1398660864247265862050D05,     &
    - 3.5377809694431133484800D08, 3.1984354235237738511048D08,       &
    - 2.5301823984599019348858D10, 1.2177698136199594677580D10,       &
    - 2.0829040666802497120940D11 /)
    real(DP),parameter :: Q(10)=(/ 7.6886718750000000000000D01, &
    -5.5648470543369082846819D03, 1.9418469440759880361415D05, -       &
    4.2648434812177161405483D06, 6.4698830956576428587653D07, -       &
    7.0108568774215954065376D08, 5.4229617984472955011862D09, -       &
    2.8986272696554495342658D10, 9.8900934262481749439886D10, -       &
    8.9673749185755048616855D10 /)
!----------------------------------------------------------------------
! J-fraction coefficients for 6.0 <= X < 12.0
!----------------------------------------------------------------------
    real(DP),parameter ::  R(10)=(/ - 2.645677793077147237806D00, -                          &
    2.378372882815725244124D00, - 2.421106956980653511550D01,         &
    1.052976392459015155422D01, 1.945603779539281810439D01, -         &
    3.015761863840593359165D01, 1.120011024227297451523D01, -         &
    3.988850730390541057912D00, 9.565134591978630774217D00,           &
    9.981193787537396413219D-1 /)
    real(DP),parameter ::  S(9)=(/ 1.598517957704779356479D-4, 4.644185932583286942650D00,  &
    3.697412299772985940785D02, - 8.791401054875438925029D00,         &
    7.608194509086645763123D02, 2.852397548119248700147D01,           &
    4.731097187816050252967D02, - 2.369210235636181001661D02,         &
    1.249884822712447891440D00 /)
!----------------------------------------------------------------------
! J-fraction coefficients for 12.0 <= X < 24.0
!----------------------------------------------------------------------
    real(DP),parameter ::  P1(10)=(/ - 1.647721172463463140042D00, -                         &
    1.860092121726437582253D01, - 1.000641913989284829961D01, -       &
    2.105740799548040450394D01, - 9.134835699998742552432D-1, -       &
    3.323612579343962284333D01, 2.495487730402059440626D01,           &
    2.652575818452799819855D01, - 1.845086232391278674524D00,         &
    9.999933106160568739091D-1 /)
    real(DP),parameter ::  Q1(9)=(/ 9.792403599217290296840D01, 6.403800405352415551324D01, &
    5.994932325667407355255D01, 2.538819315630708031713D02,           &
    4.429413178337928401161D01, 1.192832423968601006985D03,           &
    1.991004470817742470726D02, - 1.093556195391091143924D01,         &
    1.001533852045342697818D00 /)
!----------------------------------------------------------------------
! J-fraction coefficients for  X  >=  24.0
!----------------------------------------------------------------------
    real(DP),parameter ::  P2(10)=(/ 1.75338801265465972390D02, - 2.23127670777632409550D02, &
    - 1.81949664929868906455D01, - 2.79798528624305389340D01, -       &
    7.63147701620253630855D00, - 1.52856623636929636839D01, -         &
    7.06810977895029358836D00, - 5.00006640413131002475D00, -         &
    3.00000000320981265753D00, 1.00000000000000485503D00 /)
    real(DP),parameter ::  Q2(9)=(/ 3.97845977167414720840D04, 3.97277109100414518365D00,   &
    1.37790390235747998793D02, 1.17179220502086455287D02,             &
    7.04831847180424675988D01, - 1.20187763547154743238D01, -         &
    7.99243595776339741065D00, - 2.99999894040324959612D00,           &
    1.99999999999048104167D00 /)
!----------------------------------------------------------------------
    X = ARG
    IF (X == ZERO) THEN
      EI = - D1mach(2)
      IF (INTX == 2) EI = - EI
    ELSE IF ( (X < ZERO) .OR. (INTX == 2) ) THEN
!----------------------------------------------------------------------
! Calculate EI for negative argument or for E1.
!----------------------------------------------------------------------
      Y = ABS (X)
      IF (Y <= ONE) THEN
        XUMP = A (7) * Y + A (1)
        XUMQ = Y + B (1)
        DO 110 I = 2, 6
          XUMP = XUMP * Y + A (I)
          XUMQ = XUMQ * Y + B (I)
110     enddo
        EI = LOG (Y) - XUMP / XUMQ
        IF (INTX == 3) EI = EI * EXP (Y)
      ELSE IF (Y <= FOUR) THEN
        W = ONE / Y
        XUMP = C (1)
        XUMQ = D (1)
        DO 130 I = 2, 9
          XUMP = XUMP * W + C (I)
          XUMQ = XUMQ * W + D (I)
130     enddo
        EI = - XUMP / XUMQ
        IF (INTX /= 3) EI = EI * EXP ( - Y)
      ELSE
        IF ( (Y > XBIG) .AND. (INTX < 3) ) THEN
          EI = ZERO
        ELSE
          W = ONE / Y
          XUMP = E (1)
          XUMQ = F (1)
          DO 150 I = 2, 10
            XUMP = XUMP * W + E (I)
            XUMQ = XUMQ * W + F (I)
150       enddo
          EI = - W * (ONE-W * XUMP / XUMQ)
          IF (INTX /= 3) EI = EI * EXP ( - Y)
        ENDIF
      ENDIF
      IF (INTX == 2) EI = - EI
    ELSE IF (X < SIX) THEN
!----------------------------------------------------------------------
!  To improve conditioning, rational approximations are expressed
!    in terms of Chebyshev polynomials for 0 <= X < 6, and in
!    continued fraction form for larger X.
!----------------------------------------------------------------------
      T = X + X
      T = T / THREE-TWO
      PX(1) = ZERO
      QX(1) = ZERO
      PX(2) = P(1)
      QX(2) = Q(1)
      DO 210 I = 2, 9
        PX (I + 1) = T * PX (I) - PX (I - 1) + P (I)
        QX (I + 1) = T * QX (I) - QX (I - 1) + Q (I)
210   enddo
      XUMP = HALF * T * PX (10) - PX (9) + P (10)
      XUMQ = HALF * T * QX (10) - QX (9) + Q (10)
      FRAC = XUMP / XUMQ
      XMX0 = (X - X01 / X11) - X02
      IF (ABS (XMX0)  >= P037) THEN
        EI = LOG (X / X0) + XMX0 * FRAC
        IF (INTX == 3) EI = EXP ( - X) * EI
      ELSE
!----------------------------------------------------------------------
! Special approximation to  ln(X/X0)  for X close to X0
!----------------------------------------------------------------------
        Y = XMX0 / (X + X0)
        YSQ = Y * Y
        XUMP = PLG (1)
        XUMQ = YSQ + QLG (1)
        DO 220 I = 2, 4
          XUMP = XUMP * YSQ + PLG (I)
          XUMQ = XUMQ * YSQ + QLG (I)
220     enddo
        EI = (XUMP / (XUMQ * (X + X0) ) + FRAC) * XMX0
        IF (INTX == 3) EI = EXP ( - X) * EI
      ENDIF
    ELSE IF (X < TWELVE) THEN
      FRAC = ZERO
      DO 230 I = 1, 9
        FRAC = S (I) / (R (I) + X + FRAC)
230   enddo
      EI = (R (10) + FRAC) / X
      IF (INTX /= 3) EI = EI * EXP (X)
    ELSE IF (X <= TWO4) THEN
      FRAC = ZERO
      DO 240 I = 1, 9
        FRAC = Q1 (I) / (P1 (I) + X + FRAC)
240   enddo
      EI = (P1 (10) + FRAC) / X
      IF (INTX /= 3) EI = EI * EXP (X)
    ELSE
      IF ( (X >= XMAX) .AND. (INTX < 3) ) THEN
        EI = D1mach(2)
      ELSE
        Y = ONE / X
        FRAC = ZERO
        DO 250 I = 1, 9
          FRAC = Q2 (I) / (P2 (I) + X + FRAC)
250     enddo
        FRAC = P2 (10) + FRAC
        EI = Y + Y * Y * FRAC
        IF (INTX /= 3) THEN
          IF (X <= XMAX - TWO4) THEN
            EI = EI * EXP (X)
          ELSE
!----------------------------------------------------------------------
! Calculation reformulated to avoid premature overflow
!----------------------------------------------------------------------
            EI = (EI * EXP (X - FOURTY) ) * EXP40
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    RESLUT = EI
    RETURN
!---------- Last line of CALCEI ----------
END SUBROUTINE CALCEI
FUNCTION EI (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   exponential integral  Ei(x), where  x  is real.
!
!  Author: W. J. Cody
!
!  Latest modification: January 12, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: INTX
    real(DP) :: EI, RESLUT
!--------------------------------------------------------------------
    INTX = 1
    CALL CALCEI (X, RESLUT, INTX)
    EI = RESLUT
    RETURN
!---------- Last line of EI ----------
END FUNCTION EI
FUNCTION EXPEI (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   function  exp(-x) * Ei(x), where  Ei(x)  is the exponential
!   integral, and  x  is real.
!
!  Author: W. J. Cody
!
!  Latest modification: January 12, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: INTX
    real(DP) :: EXPEI, RESLUT
!--------------------------------------------------------------------
    INTX = 3
    CALL CALCEI (X, RESLUT, INTX)
    EXPEI = RESLUT
    RETURN
!---------- Last line of EXPEI ----------
END FUNCTION EXPEI
FUNCTION EONE (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   exponential integral E1(x), where  x  is real.
!
!  Author: W. J. Cody
!
!  Latest modification: January 12, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: INTX
    real(DP) :: EONE, RESLUT
!--------------------------------------------------------------------
    INTX = 2
    CALL CALCEI (X, RESLUT, INTX)
    EONE = RESLUT
    RETURN
!---------- Last line of EONE ----------
END FUNCTION EONE
SUBROUTINE CALJY0 (ARG, RESLUT, JINT)
!---------------------------------------------------------------------
!
! This packet computes zero-order Bessel functions of the first and
!   second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
!   for Y0, and |X| <= XMAX for J0.  It contains two function-type
!   subprograms,  BESJ0  and  BESY0,  and one subroutine-type
!   subprogram,  CALJY0.  The calling statements for the primary
!   entries are:
!
!           Y = BESJ0(X)
!   and
!           Y = BESY0(X),
!
!   where the entry points correspond to the functions J0(X) and Y0(X),
!   respectively.  The routine  CALJY0  is intended for internal packet
!   use only, all computations within the packet being concentrated in
!   this one routine.  The function subprograms invoke  CALJY0  with
!   the statement
!           CALL CALJY0(ARG,RESLUT,JINT),
!   where the parameter usage is as follows:
!
!      Function                  Parameters for CALJY0
!       call              ARG             RESLUT          JINT
!
!     BESJ0(ARG)     |ARG|  <=  XMAX       J0(ARG)          0
!     BESY0(ARG)   0  <  ARG  <=  XMAX    Y0(ARG)          1
!
!   The main computation uses unpublished minimax rational
!   approximations for X  <=  8.0, and an approximation from the
!   book  Computer Approximations  by Hart, et. al., Wiley and Sons,
!   New York, 1968, for arguments larger than 8.0   Part of this
!   transportable packet is patterned after the machine-dependent
!   FUNPACK program BESJ0(X), but cannot match that version for
!   efficiency or accuracy.  This version uses rational functions
!   that are theoretically accurate to at least 18 significant decimal
!   digits for X <= 8, and at least 18 decimal places for X > 8.  The
!   accuracy achieved depends on the arithmetic system, the compiler,
!   the intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   XINF   = largest positive machine number
!   XMAX   = largest acceptable argument.  The functions AINT, SIN
!            and COS must perform properly for  ABS(X)  <=  XMAX.
!            We recommend that XMAX be a small integer multiple of
!            sqrt(1/eps), where eps is the smallest positive number
!            such that  1+eps > 1.
!   sxsmall = positive argument such that  1.0-(X/2)**2 = 1.0
!            to machine precision for all  ABS(X)  <=  sxsmall.
!            We recommend that  sxsmall < sqrt(eps)/beta, where beta
!            is the floating-point radix (usually 2 or 16).
!
!     Approximate values for some important machines are
!
!                          eps      XMAX     sxsmall      XINF
!
!  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322
!  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465
!  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38
!  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308
!  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75
!  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307
!  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38
!
!*******************************************************************
!*******************************************************************
!
! Error Returns
!
!  The program returns the value zero for  X  >  XMAX, and returns
!    -XINF when BESLY0 is called with a negative or zero argument.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, COS, LOG, SIN, SQRT
!
!
!  Latest modification: June 2, 1989
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    real(DP),parameter ::xmax=16.d0/sqrt(d1mach(4)),xinf=d1mach(2)
    INTEGER  :: I
    real(DP) :: AX, DOWN, PROD, RESJ, R0, R1, UP, W, WSQ, XDEN, &
    XNUM, XJ0, XJ1, XJ01, XJ02, XJ11, XJ12, XY, XY0, XY01,    &
    XY02, XY1, XY11, XY12, XY2, XY21, XY22, Z, ZSQ
    real(DP) :: PJ0 (7), PJ1 (8), PLG (4), PY0 (6), PY1 (7), PY2 (8),   &
    P0 (6), P1 (6), QJ0 (5), QJ1 (7), QLG (4), QY0 (5), QY1 (6),      &
    QY2 (7), Q0 (5), Q1 (5)
!-------------------------------------------------------------------
!  Mathematical constants
!    CONS = ln(.5) + Euler's gamma
!-------------------------------------------------------------------
    real(DP),parameter :: FIVE5=5.5d0, SIXTY4=64.d0, P17=1.716D-1, &
    TWO56=256.d0, CONS = -1.15931515658412448810720031375774137D-1 
!-------------------------------------------------------------------
!  Zeroes of Bessel functions
!-------------------------------------------------------------------
    DATA XJ0 / 2.4048255576957727686D+0 /, XJ1 /                      &
    5.5200781102863106496D+0 /, XY0 / 8.9357696627916752158D-1 /,     &
    XY1 / 3.9576784193148578684D+0 /, XY2 / 7.0860510603017726976D+0 /&
    , XJ01 / 616.0D+0 /, XJ02 / - 1.4244423042272313784D-03 /,        &
    XJ11 / 1413.0D+0 /, XJ12 / 5.4686028631064959660D-04 /, XY01 /    &
    228.0D+0 /, XY02 / 2.9519662791675215849D-03 /, XY11 / 1013.0D+0 /&
    , XY12 / 6.4716931485786837568D-04 /, XY21 / 1814.0D+0 /, XY22 /  &
    1.1356030177269762362D-04 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation to ln(x/a)
!--------------------------------------------------------------------
    DATA PLG / - 2.4562334077563243311D+01, 2.3642701335621505212D+02,&
    - 5.4989956895857911039D+02, 3.5687548468071500413D+02 /
    DATA QLG / - 3.5553900764052419184D+01, 1.9400230218539473193D+02,&
    - 3.3442903192607538956D+02, 1.7843774234035750207D+02 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!  J0(X) / (X**2 - XJ0**2),  sxsmall  <  |X|  <=  4.0
!--------------------------------------------------------------------
    DATA PJ0 / 6.6302997904833794242D+06, - 6.2140700423540120665D+08,&
    2.7282507878605942706D+10, - 4.1298668500990866786D+11, -         &
    1.2117036164593528341D-01, 1.0344222815443188943D+02, -           &
    3.6629814655107086448D+04 /
    DATA QJ0 / 4.5612696224219938200D+05, 1.3985097372263433271D+08,  &
    2.6328198300859648632D+10, 2.3883787996332290397D+12,             &
    9.3614022392337710626D+02 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!  J0(X) / (X**2 - XJ1**2),  4.0  <  |X|  <=  8.0
!-------------------------------------------------------------------
    DATA PJ1 / 4.4176707025325087628D+03, 1.1725046279757103576D+04,  &
    1.0341910641583726701D+04, - 7.2879702464464618998D+03, -         &
    1.2254078161378989535D+04, - 1.8319397969392084011D+03,           &
    4.8591703355916499363D+01, 7.4321196680624245801D+02 /
    DATA QJ1 / 3.3307310774649071172D+02, - 2.9458766545509337327D+03,&
    1.8680990008359188352D+04, - 8.4055062591169562211D+04,           &
    2.4599102262586308984D+05, - 3.5783478026152301072D+05, -         &
    2.5258076240801555057D+01 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!    (Y0(X) - 2 LN(X/XY0) J0(X)) / (X**2 - XY0**2),
!        sxsmall  <  |X|  <=  3.0
!--------------------------------------------------------------------
    DATA PY0 / 1.0102532948020907590D+04, - 2.1287548474401797963D+06,&
    2.0422274357376619816D+08, - 8.3716255451260504098D+09,           &
    1.0723538782003176831D+11, - 1.8402381979244993524D+01 /
    DATA QY0 / 6.6475986689240190091D+02, 2.3889393209447253406D+05,  &
    5.5662956624278251596D+07, 8.1617187777290363573D+09,             &
    5.8873865738997033405D+11 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!    (Y0(X) - 2 LN(X/XY1) J0(X)) / (X**2 - XY1**2),
!        3.0  <  |X|  <=  5.5
!--------------------------------------------------------------------
    DATA PY1 / - 1.4566865832663635920D+04, 4.6905288611678631510D+06,&
    - 6.9590439394619619534D+08, 4.3600098638603061642D+10, -         &
    5.5107435206722644429D+11, - 2.2213976967566192242D+13,           &
    1.7427031242901594547D+01 /
    DATA QY1 / 8.3030857612070288823D+02, 4.0669982352539552018D+05,  &
    1.3960202770986831075D+08, 3.4015103849971240096D+10,             &
    5.4266824419412347550D+12, 4.3386146580707264428D+14 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!    (Y0(X) - 2 LN(X/XY2) J0(X)) / (X**2 - XY2**2),
!        5.5  <  |X|  <=  8.0
!--------------------------------------------------------------------
    DATA PY2 / 2.1363534169313901632D+04, - 1.0085539923498211426D+07,&
    2.1958827170518100757D+09, - 1.9363051266772083678D+11, -         &
    1.2829912364088687306D+11, 6.7016641869173237784D+14, -           &
    8.0728726905150210443D+15, - 1.7439661319197499338D+01 /
    DATA QY2 / 8.7903362168128450017D+02, 5.3924739209768057030D+05,  &
    2.4727219475672302327D+08, 8.6926121104209825246D+10,             &
    2.2598377924042897629D+13, 3.9272425569640309819D+15,             &
    3.4563724628846457519D+17 /
!-------------------------------------------------------------------
!  Coefficients for Hart,s approximation,  |X| > 8.0
!-------------------------------------------------------------------
    DATA P0 / 3.4806486443249270347D+03, 2.1170523380864944322D+04,   &
    4.1345386639580765797D+04, 2.2779090197304684302D+04,             &
    8.8961548424210455236D-01, 1.5376201909008354296D+02 /
    DATA Q0 / 3.5028735138235608207D+03, 2.1215350561880115730D+04,   &
    4.1370412495510416640D+04, 2.2779090197304684318D+04,             &
    1.5711159858080893649D+02 /
    DATA P1 / - 2.2300261666214198472D+01, -                          &
    1.1183429920482737611D+02, - 1.8591953644342993800D+02, -         &
    8.9226600200800094098D+01, - 8.8033303048680751817D-03, -         &
    1.2441026745835638459D+00 /
    DATA Q1 / 1.4887231232283756582D+03, 7.2642780169211018836D+03,   &
    1.1951131543434613647D+04, 5.7105024128512061905D+03,             &
    9.0593769594993125859D+01 /
!-------------------------------------------------------------------
!  Check for error conditions
!-------------------------------------------------------------------
    AX = ABS (ARG)
    IF ( (JINT == 1) .AND. (ARG <= ZERO) ) THEN
      RESLUT = - XINF
      GOTO 2000
    ELSE IF (AX > XMAX) THEN
      RESLUT = ZERO
      GOTO 2000
    ENDIF
    IF (AX > EIGHT) GOTO 800
    IF (AX <= sqrt(d1mach(3))*half) THEN
      IF (JINT == 0) THEN
        RESLUT = ONE
      ELSE
        RESLUT = twoonpi * (LOG (AX) + CONS)
      ENDIF
      GOTO 2000
    ENDIF
!-------------------------------------------------------------------
!  Calculate J0 for appropriate interval, preserving
!     accuracy near the zero of J0
!-------------------------------------------------------------------
    ZSQ = AX * AX
    IF (AX <= FOUR) THEN
      XNUM = (PJ0 (5) * ZSQ + PJ0 (6) ) * ZSQ + PJ0 (7)
      XDEN = ZSQ + QJ0 (5)
      DO 50 I = 1, 4
        XNUM = XNUM * ZSQ + PJ0 (I)
        XDEN = XDEN * ZSQ + QJ0 (I)
50    enddo
      PROD = ( (AX - XJ01 / TWO56) - XJ02) * (AX + XJ0)
    ELSE
      WSQ = ONE-ZSQ / SIXTY4
      XNUM = PJ1 (7) * WSQ + PJ1 (8)
      XDEN = WSQ + QJ1 (7)
      DO 220 I = 1, 6
        XNUM = XNUM * WSQ + PJ1 (I)
        XDEN = XDEN * WSQ + QJ1 (I)
220   enddo
      PROD = (AX + XJ1) * ( (AX - XJ11 / TWO56) - XJ12)
    ENDIF
    RESLUT = PROD * XNUM / XDEN
    IF (JINT == 0) GOTO 2000
!-------------------------------------------------------------------
!  Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x),
!    where xn is a zero of Y0
!-------------------------------------------------------------------
    IF (AX <= THREE) THEN
      UP = (AX - XY01 / TWO56) - XY02
      XY = XY0
    ELSE IF (AX <= FIVE5) THEN
      UP = (AX - XY11 / TWO56) - XY12
      XY = XY1
    ELSE
      UP = (AX - XY21 / TWO56) - XY22
      XY = XY2
    ENDIF
    DOWN = AX + XY
    IF (ABS (UP)  < P17 * DOWN) THEN
      W = UP / DOWN
      WSQ = W * W
      XNUM = PLG (1)
      XDEN = WSQ + QLG (1)
      DO 320 I = 2, 4
        XNUM = XNUM * WSQ + PLG (I)
        XDEN = XDEN * WSQ + QLG (I)
320   enddo
      RESJ = twoonpi * RESLUT * W * XNUM / XDEN
    ELSE
      RESJ = twoonpi * RESLUT * LOG (AX / XY)
    ENDIF
!-------------------------------------------------------------------
!  Now calculate Y0 for appropriate interval, preserving
!     accuracy near the zero of Y0
!-------------------------------------------------------------------
    IF (AX <= THREE) THEN
      XNUM = PY0 (6) * ZSQ + PY0 (1)
      XDEN = ZSQ + QY0 (1)
      DO 340 I = 2, 5
        XNUM = XNUM * ZSQ + PY0 (I)
        XDEN = XDEN * ZSQ + QY0 (I)
340   enddo
    ELSE IF (AX <= FIVE5) THEN
      XNUM = PY1 (7) * ZSQ + PY1 (1)
      XDEN = ZSQ + QY1 (1)
      DO 360 I = 2, 6
        XNUM = XNUM * ZSQ + PY1 (I)
        XDEN = XDEN * ZSQ + QY1 (I)
360   enddo
    ELSE
      XNUM = PY2 (8) * ZSQ + PY2 (1)
      XDEN = ZSQ + QY2 (1)
      DO 380 I = 2, 7
        XNUM = XNUM * ZSQ + PY2 (I)
        XDEN = XDEN * ZSQ + QY2 (I)
380   enddo
    ENDIF
    RESLUT = RESJ + UP * DOWN * XNUM / XDEN
    GOTO 2000
!-------------------------------------------------------------------
!  Calculate J0 or Y0 for |ARG|  >  8.0
!-------------------------------------------------------------------
    800 Z = EIGHT / AX
    W = AX / TWOPI
    W = AINT (W) + ONEOV8
    W = (AX - W * TWOPI1) - W * TWOPI2
    ZSQ = Z * Z
    XNUM = P0 (5) * ZSQ + P0 (6)
    XDEN = ZSQ + Q0 (5)
    UP = P1 (5) * ZSQ + P1 (6)
    DOWN = ZSQ + Q1 (5)
    DO 850 I = 1, 4
      XNUM = XNUM * ZSQ + P0 (I)
      XDEN = XDEN * ZSQ + Q0 (I)
      UP = UP * ZSQ + P1 (I)
      DOWN = DOWN * ZSQ + Q1 (I)
850 enddo
    R0 = XNUM / XDEN
    R1 = UP / DOWN
    IF (JINT == 0) THEN
      RESLUT = SQRT (twoonpi / AX) * (R0 * COS (W) - Z * R1 * SIN (W) )
    ELSE
      RESLUT = SQRT (twoonpi / AX) * (R0 * SIN (W) + Z * R1 * COS (W) )
    ENDIF
    2000 RETURN
!---------- Last line of CALJY0 ----------
END SUBROUTINE CALJY0
real(DP) FUNCTION BESJ0 (X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for Bessel functions
!   of the first kind of order zero for arguments  |X| <= XMAX
!   (see comments heading CALJY0).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 0
    CALL CALJY0 (X, RESLUT, JINT)
    BESJ0 = RESLUT
    RETURN
!---------- Last line of BESJ0 ----------
END FUNCTION BESJ0
real(DP) FUNCTION BESY0 (X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for Bessel functions
!   of the second kind of order zero for arguments 0 < X <= XMAX
!   (see comments heading CALJY0).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALJY0 (X, RESLUT, JINT)
    BESY0 = RESLUT
    RETURN
!---------- Last line of BESY0 ----------
END FUNCTION BESY0
SUBROUTINE CALJY1 (ARG, RESLUT, JINT)
!---------------------------------------------------------------------
!
! This packet computes first-order Bessel functions of the first and
!   second kind (J1 and Y1), for real arguments X, where 0 < X <= XMAX
!   for Y1, and |X| <= XMAX for J1.  It contains two function-type
!   subprograms,  BESJ1  and  BESY1,  and one subroutine-type
!   subprogram,  CALJY1.  The calling statements for the primary
!   entries are:
!
!           Y = BESJ1(X)
!   and
!           Y = BESY1(X),
!
!   where the entry points correspond to the functions J1(X) and Y1(X),
!   respectively.  The routine  CALJY1  is intended for internal packet
!   use only, all computations within the packet being concentrated in
!   this one routine.  The function subprograms invoke  CALJY1  with
!   the statement
!           CALL CALJY1(ARG,RESLUT,JINT),
!   where the parameter usage is as follows:
!
!      Function                  Parameters for CALJY1
!       call              ARG             RESLUT          JINT
!
!     BESJ1(ARG)     |ARG|  <=  XMAX       J1(ARG)          0
!     BESY1(ARG)   0  <  ARG  <=  XMAX    Y1(ARG)          1
!
!   The main computation uses unpublished minimax rational
!   approximations for X  <=  8.0, and an approximation from the
!   book  Computer Approximations  by Hart, et. al., Wiley and Sons,
!   New York, 1968, for arguments larger than 8.0   Part of this
!   transportable packet is patterned after the machine-dependent
!   FUNPACK program BESJ1(X), but cannot match that version for
!   efficiency or accuracy.  This version uses rational functions
!   that are theoretically accurate to at least 18 significant decimal
!   digits for X <= 8, and at least 18 decimal places for X > 8.  The
!   accuracy achieved depends on the arithmetic system, the compiler,
!   the intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   XINF   = largest positive machine number
!   XMAX   = largest acceptable argument.  The functions AINT, SIN
!            and COS must perform properly for  ABS(X)  <=  XMAX.
!            We recommend that XMAX be a small integer multiple of
!            sqrt(1/eps), where eps is the smallest positive number
!            such that  1+eps > 1.
!   sxsmall = positive argument such that  1.0-(1/2)(X/2)**2 = 1.0
!            to machine precision for all  ABS(X)  <=  sxsmall.
!            We recommend that  sxsmall < sqrt(eps)/beta, where beta
!            is the floating-point radix (usually 2 or 16).
!
!     Approximate values for some important machines are
!
!                          eps      XMAX     sxsmall      XINF
!
!  CDC 7600      (S.P.)  7.11E-15  1.34E+08  2.98E-08  1.26E+322
!  CRAY-1        (S.P.)  7.11E-15  1.34E+08  2.98E-08  5.45E+2465
!  IBM PC (8087) (S.P.)  5.96E-08  8.19E+03  1.22E-04  3.40E+38
!  IBM PC (8087) (D.P.)  1.11D-16  2.68D+08  3.72D-09  1.79D+308
!  IBM 195       (D.P.)  2.22D-16  6.87D+09  9.09D-13  7.23D+75
!  UNIVAC 1108   (D.P.)  1.73D-18  4.30D+09  2.33D-10  8.98D+307
!  VAX 11/780    (D.P.)  1.39D-17  1.07D+09  9.31D-10  1.70D+38
!
!*******************************************************************
!*******************************************************************
!
! Error Returns
!
!  The program returns the value zero for  X  >  XMAX, and returns
!    -XINF when BESLY1 is called with a negative or zero argument.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, COS, LOG, SIN, SQRT
!
!
!  Author: W. J. Cody
!          Mathematics and Computer Science Division
!          Argonne National Laboratory
!          Argonne, IL 60439
!
!  Latest modification: November 10, 1987
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    real(DP),parameter ::xmax=16.d0/sqrt(d1mach(4)),xinf=d1mach(2)
    INTEGER  :: I
    real(DP) :: PJ0(7),PJ1(8),PLG(4),PY0(7),PY1(9),P0(6),P1(6),QJ0(5),QJ1(7),QLG(4),QY0(6),&
    QY1(8),Q0(6),Q1(6)
    real(DP) :: AX, DOWN, PROD, RESJ, R0, R1, &
    UP, W, WSQ, XDEN, XNUM, XJ0, XJ1, XJ01, XJ02, &
    XJ11, XJ12, XY, XY0, XY01, XY02, XY1, XY11, XY12, Z, ZSQ
!-------------------------------------------------------------------
!  Mathematical constants
!-------------------------------------------------------------------
    real(DP),parameter :: THROV8 = 0.375D0, P17 = 1.716D-1, &
                          TWO56 = 256.0D+0, RTPI2 = 7.97884560802865355879892119868763739D-1 
!-------------------------------------------------------------------
!  Zeroes of Bessel functions
!-------------------------------------------------------------------
    DATA XJ0 / 3.8317059702075123156D+0 /, XJ1 /                      &
    7.0155866698156187535D+0 /, XY0 / 2.1971413260310170351D+0 /,     &
    XY1 / 5.4296810407941351328D+0 /, XJ01 / 981.0D+0 /, XJ02 /       &
    - 3.2527979248768438556D-04 /, XJ11 / 1796.0D+0 /, XJ12 / -       &
    3.8330184381246462950D-05 /, XY01 / 562.0D+0 /, XY02 /            &
    1.8288260310170351490D-03 /, XY11 / 1390.0D+0 /, XY12 / -         &
    6.4592058648672279948D-06 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation to ln(x/a)
!--------------------------------------------------------------------
    DATA PLG / - 2.4562334077563243311D+01, 2.3642701335621505212D+02,&
    - 5.4989956895857911039D+02, 3.5687548468071500413D+02 /
    DATA QLG / - 3.5553900764052419184D+01, 1.9400230218539473193D+02,&
    - 3.3442903192607538956D+02, 1.7843774234035750207D+02 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!  J1(X) / (X * (X**2 - XJ0**2)),  sxsmall  <  |X|  <=  4.0
!--------------------------------------------------------------------
    DATA PJ0 / 9.8062904098958257677D+05, - 1.1548696764841276794D+08,&
    6.6781041261492395835D+09, - 1.4258509801366645672D+11, -         &
    4.4615792982775076130D+03, 1.0650724020080236441D+01, -           &
    1.0767857011487300348D-02 /
    DATA QJ0 / 5.9117614494174794095D+05, 2.0228375140097033958D+08,  &
    4.2091902282580133541D+10, 4.1868604460820175290D+12,             &
    1.0742272239517380498D+03 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!  J1(X) / (X * (X**2 - XJ1**2)),  4.0  <  |X|  <=  8.0
!-------------------------------------------------------------------
    DATA PJ1 / 4.6179191852758252280D+00, - 7.1329006872560947377D+03,&
    4.5039658105749078904D+06, - 1.4437717718363239107D+09,           &
    2.3569285397217157313D+11, - 1.6324168293282543629D+13,           &
    1.1357022719979468624D+14, 1.0051899717115285432D+15 /
    DATA QJ1 / 1.1267125065029138050D+06, 6.4872502899596389593D+08,  &
    2.7622777286244082666D+11, 8.4899346165481429307D+13,             &
    1.7128800897135812012D+16, 1.7253905888447681194D+18,             &
    1.3886978985861357615D+03 /
!-------------------------------------------------------------------
!  Coefficients for rational approximation of
!    (Y1(X) - 2 LN(X/XY0) J1(X)) / (X**2 - XY0**2),
!        sxsmall  <  |X|  <=  4.0
!--------------------------------------------------------------------
    DATA PY0 / 2.2157953222280260820D+05, - 5.9157479997408395984D+07,&
    7.2144548214502560419D+09, - 3.7595974497819597599D+11,           &
    5.4708611716525426053D+12, 4.0535726612579544093D+13, -           &
    3.1714424660046133456D+02 /
    DATA QY0 / 8.2079908168393867438D+02, 3.8136470753052572164D+05,  &
    1.2250435122182963220D+08, 2.7800352738690585613D+10,             &
    4.1272286200406461981D+12, 3.0737873921079286084D+14 /
!--------------------------------------------------------------------
!  Coefficients for rational approximation of
!    (Y1(X) - 2 LN(X/XY1) J1(X)) / (X**2 - XY1**2),
!        4.0  <  |X|  <=  8.0
!--------------------------------------------------------------------
    DATA PY1 / 1.9153806858264202986D+06, - 1.1957961912070617006D+09,&
    3.7453673962438488783D+11, - 5.9530713129741981618D+13,           &
    4.0686275289804744814D+15, - 2.3638408497043134724D+16, -         &
    5.6808094574724204577D+18, 1.1514276357909013326D+19, -           &
    1.2337180442012953128D+03 /
    DATA QY1 / 1.2855164849321609336D+03, 1.0453748201934079734D+06,  &
    6.3550318087088919566D+08, 3.0221766852960403645D+11,             &
    1.1187010065856971027D+14, 3.0837179548112881950D+16,             &
    5.6968198822857178911D+18, 5.3321844313316185697D+20 /
!-------------------------------------------------------------------
!  Coefficients for Hart,s approximation,  |X| > 8.0
!-------------------------------------------------------------------
    DATA P0 / - 1.0982405543459346727D+05, -                          &
    1.5235293511811373833D+06, - 6.6033732483649391093D+06, -         &
    9.9422465050776411957D+06, - 4.4357578167941278571D+06, -         &
    1.6116166443246101165D+03 /
    DATA Q0 / - 1.0726385991103820119D+05, -                          &
    1.5118095066341608816D+06, - 6.5853394797230870728D+06, -         &
    9.9341243899345856590D+06, - 4.4357578167941278568D+06, -         &
    1.4550094401904961825D+03 /
    DATA P1 / 1.7063754290207680021D+03, 1.8494262873223866797D+04,   &
    6.6178836581270835179D+04, 8.5145160675335701966D+04,             &
    3.3220913409857223519D+04, 3.5265133846636032186D+01 /
    DATA Q1 / 3.7890229745772202641D+04, 4.0029443582266975117D+05,   &
    1.4194606696037208929D+06, 1.8194580422439972989D+06,             &
    7.0871281941028743574D+05, 8.6383677696049909675D+02 /
!-------------------------------------------------------------------
!  Check for error conditions
!-------------------------------------------------------------------
    AX = ABS (ARG)
    IF ( (JINT == 1) .AND. ( (ARG <= ZERO) .OR. ( (ARG < HALF) .AND. (AX * XINF < twoonpi) ) ) ) THEN
      RESLUT = - XINF
      GOTO 2000
    ELSE IF (AX > XMAX) THEN
      RESLUT = ZERO
      GOTO 2000
    ENDIF
    IF (AX > EIGHT) THEN
      GOTO 800
    ELSE IF (AX <= sqrt(d1mach(3))*half) THEN
      IF (JINT == 0) THEN
        RESLUT = ARG * HALF
      ELSE
        RESLUT = - twoonpi / AX
      ENDIF
      GOTO 2000
    ENDIF
!-------------------------------------------------------------------
!  Calculate J1 for appropriate interval, preserving
!     accuracy near the zero of J1
!-------------------------------------------------------------------
  ZSQ = AX * AX
  IF (AX <= FOUR) THEN
    XNUM = (PJ0 (7) * ZSQ + PJ0 (6) ) * ZSQ + PJ0 (5)
    XDEN = ZSQ + QJ0 (5)
    DO 50 I = 1, 4
      XNUM = XNUM * ZSQ + PJ0 (I)
      XDEN = XDEN * ZSQ + QJ0 (I)
50  enddo
    PROD = ARG * ( (AX - XJ01 / TWO56) - XJ02) * (AX + XJ0)
  ELSE
    XNUM = PJ1 (1)
    XDEN = (ZSQ + QJ1 (7) ) * ZSQ + QJ1 (1)
    DO 220 I = 2, 6
      XNUM = XNUM * ZSQ + PJ1 (I)
      XDEN = XDEN * ZSQ + QJ1 (I)
220 enddo
    XNUM = XNUM * (AX - EIGHT) * (AX + EIGHT) + PJ1 (7)
    XNUM = XNUM * (AX - FOUR) * (AX + FOUR) + PJ1 (8)
    PROD = ARG * ( (AX - XJ11 / TWO56) - XJ12) * (AX + XJ1)
  ENDIF
  RESLUT = PROD * (XNUM / XDEN)
  IF (JINT == 0) GOTO 2000
!-------------------------------------------------------------------
!  Calculate Y1.  First find  RESJ = pi/2 ln(x/xn) J1(x),
!    where xn is a zero of Y1
!-------------------------------------------------------------------
  IF (AX <= FOUR) THEN
    UP = (AX - XY01 / TWO56) - XY02
    XY = XY0
  ELSE
    UP = (AX - XY11 / TWO56) - XY12
    XY = XY1
  ENDIF
  DOWN = AX + XY
  IF (ABS (UP)  < P17 * DOWN) THEN
    W = UP / DOWN
    WSQ = W * W
    XNUM = PLG (1)
    XDEN = WSQ + QLG (1)
    DO 320 I = 2, 4
      XNUM = XNUM * WSQ + PLG (I)
      XDEN = XDEN * WSQ + QLG (I)
320 enddo
    RESJ = twoonpi * RESLUT * W * XNUM / XDEN
  ELSE
    RESJ = twoonpi * RESLUT * LOG (AX / XY)
  ENDIF
!-------------------------------------------------------------------
!  Now calculate Y1 for appropriate interval, preserving
!     accuracy near the zero of Y1
!-------------------------------------------------------------------
  IF (AX <= FOUR) THEN
    XNUM = PY0 (7) * ZSQ + PY0 (1)
    XDEN = ZSQ + QY0 (1)
    DO 340 I = 2, 6
      XNUM = XNUM * ZSQ + PY0 (I)
      XDEN = XDEN * ZSQ + QY0 (I)
340 enddo
  ELSE
    XNUM = PY1 (9) * ZSQ + PY1 (1)
    XDEN = ZSQ + QY1 (1)
    DO 360 I = 2, 8
      XNUM = XNUM * ZSQ + PY1 (I)
      XDEN = XDEN * ZSQ + QY1 (I)
360 enddo
  ENDIF
  RESLUT = RESJ + (UP * DOWN / AX) * XNUM / XDEN
  GOTO 2000
!-------------------------------------------------------------------
!  Calculate J1 or Y1 for |ARG|  >  8.0
!-------------------------------------------------------------------
  800 Z = EIGHT / AX
  W = AINT (AX / TWOPI) + THROV8
  W = (AX - W * TWOPI1) - W * TWOPI2
  ZSQ = Z * Z
  XNUM = P0 (6)
  XDEN = ZSQ + Q0 (6)
  UP = P1 (6)
  DOWN = ZSQ + Q1 (6)
  DO 850 I = 1, 5
    XNUM = XNUM * ZSQ + P0 (I)
    XDEN = XDEN * ZSQ + Q0 (I)
    UP = UP * ZSQ + P1 (I)
    DOWN = DOWN * ZSQ + Q1 (I)
850 enddo
  R0 = XNUM / XDEN
  R1 = UP / DOWN
  IF (JINT == 0) THEN
    RESLUT = (RTPI2 / SQRT (AX) ) * (R0 * COS (W) - Z * R1 * SIN (W)&
    )
  ELSE
    RESLUT = (RTPI2 / SQRT (AX) ) * (R0 * SIN (W) + Z * R1 * COS (W)&
    )
  ENDIF
  IF ( (JINT == 0) .AND. (ARG < ZERO) ) RESLUT = - RESLUT
  2000 RETURN
!---------- Last card of CALJY1 ----------
END SUBROUTINE CALJY1
FUNCTION BESJ1 (X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for Bessel functions
!   of the first kind of order zero for arguments  |X| <= XMAX
!   (see comments heading CALJY1).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: BESJ1, RESLUT
!--------------------------------------------------------------------
    JINT = 0
    CALL CALJY1 (X, RESLUT, JINT)
    BESJ1 = RESLUT
    RETURN
!---------- Last card of BESJ1 ----------
END FUNCTION BESJ1
FUNCTION BESY1 (X)
!--------------------------------------------------------------------
!
! This subprogram computes approximate values for Bessel functions
!   of the second kind of order zero for arguments 0 < X <= XMAX
!   (see comments heading CALJY1).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: BESY1, RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALJY1 (X, RESLUT, JINT)
    BESY1 = RESLUT
    RETURN
!---------- Last card of BESY1 ----------
END FUNCTION BESY1
SUBROUTINE CALCI0 (ARG, RESLUT, JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the first kind
!   and order zero, I0(X) and EXP(-ABS(X))*I0(X), for real
!   arguments X.  It contains two function type subprograms, BESI0
!   and BESEI0, and one subroutine type subprogram, CALCI0.
!   The calling statements for the primary entries are
!
!                   Y=BESI0(X)
!   and
!                   Y=BESEI0(X)
!
!   where the entry points correspond to the functions I0(X) and
!   EXP(-ABS(X))*I0(X), respectively.  The routine CALCI0 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCI0 with the statement
!          CALL CALCI0(ARG,RESLUT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCI0
!       Call              ARG                  RESLUT          JINT
!
!     BESI0(ARG)    ABS(ARG)  <=  XMAX        I0(ARG)           1
!     BESEI0(ARG)    any real ARG        EXP(-ABS(ARG))*I0(ARG) 2
!
!   The main computation evaluates slightly modified forms of
!   minimax approximations generated by Blair and Edwards, Chalk
!   River (Atomic Energy of Canada Limited) Report AECL-4928,
!   October, 1974.  This transportable program is patterned after
!   the machine-dependent FUNPACK packet NATSI0, but cannot match
!   that version for efficiency or accuracy.  This version uses
!   rational functions that theoretically approximate I-SUB-0(X)
!   to at least 18 significant decimal digits.  The accuracy
!   achieved depends on the arithmetic system, the compiler, the
!   intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Positive argument such that 1.0 - X = 1.0 to
!            machine precision for all ABS(X)  <=  XSMALL.
!   XINF =   Largest positive machine number; approximately
!            beta**maxexp
!   XMAX =   Largest argument acceptable to BESI0;  Solution to
!            equation:
!               W(X) * (1+1/(8*X)+9/(128*X**2) = beta**maxexp
!            where  W(X) = EXP(X)/SQRT(2*PI*X)
!
!
!     Approximate values for some important machines are:
!
!                          beta       maxexp       XSMALL
!
! CRAY-1        (S.P.)       2         8191       3.55E-15
! Cyber 180/855
!   under NOS   (S.P.)       2         1070       3.55E-15
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       2          128       2.98E-8
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       2         1024       5.55D-17
! IBM 3033      (D.P.)      16           63       6.95D-18
! VAX           (S.P.)       2          127       2.98E-8
! VAX D-Format  (D.P.)       2          127       6.95D-18
! VAX G-Format  (D.P.)       2         1023       5.55D-17
!
!
!                               XINF          XMAX
!
! CRAY-1        (S.P.)       5.45E+2465     5682.810
! Cyber 180/855
!   under NOS   (S.P.)       1.26E+322       745.893
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       3.40E+38         91.900
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       1.79D+308       713.986
! IBM 3033      (D.P.)       7.23D+75        178.182
! VAX           (S.P.)       1.70D+38         91.203
! VAX D-Format  (D.P.)       1.70D+38         91.203
! VAX G-Format  (D.P.)       8.98D+307       713.293
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns XINF for BESI0 for ABS(ARG)  >  XMAX.
!
!
!  Intrinsic functions required are:
!
!     ABS, SQRT, EXP
!
!
!  Authors: W. J. Cody and L. Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!  Latest modification: June 7, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    INTEGER  :: I
    real(DP) :: A, B, XUMP, XUMQ, X, XX
    real(DP) :: P(15), PP(8), Q(5), QQ(7)
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
    real(DP),parameter :: XMAX = 713.986908544170024207295214148151438D0
!--------------------------------------------------------------------
!  Coefficients for XSMALL  <=  ABS(ARG)  <  15.0
!--------------------------------------------------------------------
    DATA P / - 5.2487866627945699800D-18, - 1.5982226675653184646D-14,&
    - 2.6843448573468483278D-11, - 3.0517226450451067446D-08, -       &
    2.5172644670688975051D-05, - 1.5453977791786851041D-02, -         &
    7.0935347449210549190D+00, - 2.4125195876041896775D+03, -         &
    5.9545626019847898221D+05, - 1.0313066708737980747D+08, -         &
    1.1912746104985237192D+10, - 8.4925101247114157499D+11, -         &
    3.2940087627407749166D+13, - 5.5050369673018427753D+14, -         &
    2.2335582639474375249D+15 /
    DATA Q / - 3.7277560179962773046D+03, 6.5158506418655165707D+06,  &
    - 6.5626560740833869295D+09, 3.7604188704092954661D+12, -         &
    9.7087946179594019126D+14 /
!--------------------------------------------------------------------
!  Coefficients for 15.0  <=  ABS(ARG)
!--------------------------------------------------------------------
    DATA PP / - 3.9843750000000000000D-01, 2.9205384596336793945D+00, &
    - 2.4708469169133954315D+00, 4.7914889422856814203D-01, -         &
    3.7384991926068969150D-03, - 2.6801520353328635310D-03,           &
    9.9168777670983678974D-05, - 2.1877128189032726730D-06 /
    DATA QQ / - 3.1446690275135491500D+01, 8.5539563258012929600D+01, &
    - 6.0228002066743340583D+01, 1.3982595353892851542D+01, -         &
    1.1151759188741312645D+00, 3.2547697594819615062D-02, -           &
    5.5194330231005480228D-04 /
!--------------------------------------------------------------------
    X = ABS (ARG)
    IF (X < xsmallh) THEN
      RESLUT = ONE
    ELSE IF (X < fifteen) THEN
!--------------------------------------------------------------------
!  xsmallh  <=   ABS(ARG)   <  15.0
!--------------------------------------------------------------------
      XX = X * X
      XUMP = P (1)
      DO 50 I = 2, 15
        XUMP = XUMP * XX + P (I)
50    enddo
      XX = XX - TWO25
      XUMQ = ( ( ( (XX + Q (1) ) * XX + Q (2) ) * XX + Q (3) )        &
      * XX + Q (4) ) * XX + Q (5)
      RESLUT = XUMP / XUMQ
      IF (JINT == 2) RESLUT = RESLUT * EXP ( - X)
    ELSE IF (X >= fifteen) THEN
      IF ( (JINT == 1) .AND. (X > XMAX) ) THEN
        RESLUT = D1mach(2)
      ELSE
!--------------------------------------------------------------------
!  15.0   <=   ABS(ARG)
!--------------------------------------------------------------------
        XX = ONE / X - REC15
        XUMP = ( ( ( ( ( (PP (1) * XX + PP (2) ) * XX + PP (3) )      &
        * XX + PP (4) ) * XX + PP (5) ) * XX + PP (6) ) * XX + PP (7) &
        ) * XX + PP (8)
        XUMQ = ( ( ( ( ( (XX + QQ (1) ) * XX + QQ (2) ) * XX + QQ (3) &
        ) * XX + QQ (4) ) * XX + QQ (5) ) * XX + QQ (6) ) * XX + QQ ( &
        7)
        RESLUT = XUMP / XUMQ
        IF (JINT == 2) THEN
          RESLUT = (RESLUT - PP (1) ) / SQRT (X)
        ELSE
!--------------------------------------------------------------------
!  Calculation reformulated to avoid premature overflow
!--------------------------------------------------------------------
          IF (X <=  (XMAX - fifteen) ) THEN
            A = EXP (X)
            B = ONE
          ELSE
            A = EXP (X - fourty)
            B = EXP40
          ENDIF
          RESLUT = ( (RESLUT * A - PP (1) * A) / SQRT (X) ) * B
        ENDIF
      ENDIF
    ENDIF
!--------------------------------------------------------------------
!  Return for ABS(ARG)  <  xsmallh
!--------------------------------------------------------------------
    RETURN
!----------- Last line of CALCI0 -----------
END SUBROUTINE CALCI0
real(DP) FUNCTION BESI0 (X)
!--------------------------------------------------------------------
!
! This long precision subprogram computes approximate values for
!   modified Bessel functions of the first kind of order zero for
!   arguments ABS(ARG)  <=  XMAX  (see comments heading CALCI0).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALCI0 (X, RESLUT, JINT)
    BESI0 = RESLUT
    RETURN
!---------- Last line of BESI0 ----------
END FUNCTION BESI0
real(DP) FUNCTION BESEI0 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the first kind of order zero
!   multiplied by EXP(-ABS(X)), where EXP is the
!   exponential function, ABS is the absolute value, and X
!   is any argument.
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 2
    CALL CALCI0 (X, RESLUT, JINT)
    BESEI0 = RESLUT
    RETURN
!---------- Last line of BESEI0 ----------
END FUNCTION BESEI0
SUBROUTINE CALCI1 (ARG, RESLUT, JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functioons of the first kind
!   and order one, I1(X) and EXP(-ABS(X))*I1(X), for real
!   arguments X.  It contains two function type subprograms, BESI1
!   and BESEI1, and one subroutine type subprogram, CALCI1.
!   The calling statements for the primary entries are
!
!                   Y=BESI1(X)
!   and
!                   Y=BESEI1(X)
!
!   where the entry points correspond to the functions I1(X) and
!   EXP(-ABS(X))*I1(X), respectively.  The routine CALCI1 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCI1 with the statement
!          CALL CALCI1(ARG,RESLUT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCI1
!       Call              ARG                  RESLUT          JINT
!
!     BESI1(ARG)    ABS(ARG)  <=  XMAX        I1(ARG)           1
!     BESEI1(ARG)    any real ARG        EXP(-ABS(ARG))*I1(ARG) 2
!
!   The main computation evaluates slightly modified forms of
!   minimax approximations generated by Blair and Edwards, Chalk
!   River (Atomic Energy of Canada Limited) Report AECL-4928,
!   October, 1974.  This transportable program is patterned after
!   the machine-dependent FUNPACK packet NATSI1, but cannot match
!   that version for efficiency or accuracy.  This version uses
!   rational functions that theoretically approximate I-SUB-1(X)
!   to at least 18 significant decimal digits.  The accuracy
!   achieved depends on the arithmetic system, the compiler, the
!   intrinsic functions, and proper selection of the machine-
!   dependent constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Positive argument such that 1.0 - X = 1.0 to
!            machine precision for all ABS(X)  <=  XSMALL.
!   XINF =   Largest positive machine number; approximately
!            beta**maxexp
!   XMAX =   Largest argument acceptable to BESI1;  Solution to
!            equation:
!               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp
!
!
!     Approximate values for some important machines are:
!
!                          beta       maxexp       XSMALL
!
! CRAY-1        (S.P.)       2         8191       3.55E-15
! Cyber 180/855
!   under NOS   (S.P.)       2         1070       3.55E-15
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       2          128       2.98E-8
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       2         1024       5.55D-17
! IBM 3033      (D.P.)      16           63       6.95D-18
! VAX           (S.P.)       2          127       2.98E-8
! VAX D-Format  (D.P.)       2          127       6.95D-18
! VAX G-Format  (D.P.)       2         1023       5.55D-17
!
!
!                               XINF          XMAX
!
! CRAY-1        (S.P.)       5.45E+2465     5682.810
! Cyber 180/855
!   under NOS   (S.P.)       1.26E+322       745.894
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)       3.40E+38         91.906
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)       1.79D+308       713.987
! IBM 3033      (D.P.)       7.23D+75        178.185
! VAX           (S.P.)       1.70D+38         91.209
! VAX D-Format  (D.P.)       1.70D+38         91.209
! VAX G-Format  (D.P.)       8.98D+307       713.293
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ABS(ARG)  >  XMAX.
!
!
! Intrinsic functions required are:
!
!     ABS, SQRT, EXP
!
!
!  Authors: W. J. Cody and L. Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL  60439
!
!  Latest modification: May 31, 1989
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    INTEGER  :: J
    real(DP) :: A, B, PBAR, XUMP, XUMQ, X, XX
    real(DP) :: P (15), PP (8), Q (5), QQ (6)
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
    real(DP),parameter :: XMAX = 713.987609588098491604751766620577039D0
!--------------------------------------------------------------------
!  Coefficients for xsmallh  <=  ABS(ARG)  <  15.0
!--------------------------------------------------------------------
    DATA P / - 1.9705291802535139930D-19, - 6.5245515583151902910D-16,&
    - 1.1928788903603238754D-12, - 1.4831904935994647675D-09, -       &
    1.3466829827635152875D-06, - 9.1746443287817501309D-04, -         &
    4.7207090827310162436D-01, - 1.8225946631657315931D+02, -         &
    5.1894091982308017540D+04, - 1.0588550724769347106D+07, -         &
    1.4828267606612366099D+09, - 1.3357437682275493024D+11, -         &
    6.9876779648010090070D+12, - 1.7732037840791591320D+14, -         &
    1.4577180278143463643D+15 /
    DATA Q / - 4.0076864679904189921D+03, 7.4810580356655069138D+06,  &
    - 8.0059518998619764991D+09, 4.8544714258273622913D+12, -         &
    1.3218168307321442305D+15 /
!--------------------------------------------------------------------
!  Coefficients for 15.0  <=  ABS(ARG)
!--------------------------------------------------------------------
    DATA PP / - 6.0437159056137600000D-02, 4.5748122901933459000D-01, &
    - 4.2843766903304806403D-01, 9.7356000150886612134D-02, -         &
    3.2457723974465568321D-03, - 3.6395264712121795296D-04,           &
    1.6258661867440836395D-05, - 3.6347578404608223492D-07 /
    DATA QQ / - 3.8806586721556593450D+00, 3.2593714889036996297D+00, &
    - 8.5017476463217924408D-01, 7.4212010813186530069D-02, -         &
    2.2835624489492512649D-03, 3.7510433111922824643D-05 /
    DATA PBAR / 3.98437500D-01 /
!--------------------------------------------------------------------
    X = ABS (ARG)
    IF (X < xsmallh) THEN
!--------------------------------------------------------------------
!  Return for ABS(ARG)  <  xsmallh
!--------------------------------------------------------------------
      RESLUT = HALF * X
    ELSE IF (X < fifteen) THEN
!--------------------------------------------------------------------
!  xsmallh  <=  ABS(ARG)  <  15.0
!--------------------------------------------------------------------
      XX = X * X
      XUMP = P (1)
      DO 50 J = 2, 15
        XUMP = XUMP * XX + P (J)
50    enddo
      XX = XX - TWO25
      XUMQ = ( ( ( (XX + Q (1) ) * XX + Q (2) ) * XX + Q (3) )        &
      * XX + Q (4) ) * XX + Q (5)
      RESLUT = (XUMP / XUMQ) * X
      IF (JINT == 2) RESLUT = RESLUT * EXP ( - X)
    ELSE IF ( (JINT == 1) .AND. (X > XMAX) ) THEN
      RESLUT = D1mach(2)
    ELSE
!--------------------------------------------------------------------
!  15.0  <=  ABS(ARG)
!--------------------------------------------------------------------
      XX = ONE / X - REC15
      XUMP = ( ( ( ( ( (PP (1) * XX + PP (2) ) * XX + PP (3) )        &
      * XX + PP (4) ) * XX + PP (5) ) * XX + PP (6) ) * XX + PP (7) ) &
      * XX + PP (8)
      XUMQ = ( ( ( ( (XX + QQ (1) ) * XX + QQ (2) ) * XX + QQ (3) )   &
      * XX + QQ (4) ) * XX + QQ (5) ) * XX + QQ (6)
      RESLUT = XUMP / XUMQ
      IF (JINT /= 1) THEN
        RESLUT = (RESLUT + PBAR) / SQRT (X)
      ELSE
!--------------------------------------------------------------------
!  Calculation reformulated to avoid premature overflow
!--------------------------------------------------------------------
        IF (X > XMAX - fifteen) THEN
          A = EXP (X - fourty)
          B = EXP40
        ELSE
          A = EXP (X)
          B = ONE
        ENDIF
        RESLUT = ( (RESLUT * A + PBAR * A) / SQRT (X) ) * B
!--------------------------------------------------------------------
!  Error return for ABS(ARG)  >  XMAX
!--------------------------------------------------------------------
      ENDIF
    ENDIF
    IF (ARG < ZERO) RESLUT = - RESLUT
    RETURN
!----------- Last line of CALCI1 -----------
END SUBROUTINE CALCI1
real(DP) FUNCTION BESI1 (X)
!--------------------------------------------------------------------
!
! This long precision subprogram computes approximate values for
!   modified Bessel functions of the first kind of order one for
!   arguments ABS(ARG)  <=  XMAX  (see comments heading CALCI1).
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALCI1 (X, RESLUT, JINT)
    BESI1 = RESLUT
    RETURN
!---------- Last line of BESI1 ----------
END FUNCTION BESI1
real(DP) FUNCTION BESEI1 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the first kind of order one
!   multiplied by EXP(-ABS(X)), where EXP is the
!   exponential function, ABS is the absolute value, and X
!   is any argument.
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN) :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 2
    CALL CALCI1 (X, RESLUT, JINT)
    BESEI1 = RESLUT
    RETURN
!---------- Last line of BESEI1 ----------
END FUNCTION BESEI1
SUBROUTINE CALCK0 (ARG, RESLUT, JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the second kind
!   and order zero, K0(X) and EXP(X)*K0(X), for real
!   arguments X.  It contains two function type subprograms, BESK0
!   and BESEK0, and one subroutine type subprogram, CALCK0.
!   the calling statements for the primary entries are
!
!                   Y=BESK0(X)
!   and
!                   Y=BESEK0(X)
!
!   where the entry points correspond to the functions K0(X) and
!   EXP(X)*K0(X), respectively.  The routine CALCK0 is
!   intended for internal packet use only, all computations within
!   the packet being concentrated in this routine.  The function
!   subprograms invoke CALCK0 with the statement
!          CALL CALCK0(ARG,RESLUT,JINT)
!   where the parameter usage is as follows
!
!      Function                     Parameters for CALCK0
!       Call              ARG                  RESLUT          JINT
!
!     BESK0(ARG)   0  <  ARG  <=  XMAX       K0(ARG)           1
!     BESEK0(ARG)     0  <  ARG           EXP(ARG)*K0(ARG)     2
!
!   The main computation evaluates slightly modified forms of near
!   minimax rational approximations generated by Russon and Blair,
!   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!   1969.  This transportable program is patterned after the
!   machine-dependent FUNPACK packet NATSK0, but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   functions that theoretically approximate K-SUB-0(X) to at
!   least 18 significant decimal digits.  The accuracy achieved
!   depends on the arithmetic system, the compiler, the intrinsic
!   functions, and proper selection of the machine-dependent
!   constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   XSMALL = Argument below which BESK0 and BESEK0 may
!            each be represented by a constant and a log.
!            largest X such that  1.0 + X = 1.0  to machine
!            precision.
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMAX   = Largest argument acceptable to BESK0;  Solution to
!            equation:
!               W(X) * (1-1/8X+9/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!
!
!     Approximate values for some important machines are:
!
!
!                           beta       minexp       maxexp
!
!  CRAY-1        (S.P.)       2        -8193         8191
!  Cyber 180/185
!    under NOS   (S.P.)       2         -975         1070
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)       2         -126          128
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)       2        -1022         1024
!  IBM 3033      (D.P.)      16          -65           63
!  VAX D-Format  (D.P.)       2         -128          127
!  VAX G-Format  (D.P.)       2        -1024         1023
!
!
!                          XSMALL       XINF         XMAX
!
! CRAY-1        (S.P.)    3.55E-15   5.45E+2465    5674.858
! Cyber 180/855
!   under NOS   (S.P.)    1.77E-15   1.26E+322      672.788
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)    5.95E-8    3.40E+38        85.337
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)    1.11D-16   1.79D+308      705.342
! IBM 3033      (D.P.)    1.11D-16   7.23D+75       177.852
! VAX D-Format  (D.P.)    6.95D-18   1.70D+38        86.715
! VAX G-Format  (D.P.)    5.55D-17   8.98D+307      706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ARG  <=  0.0, and the
!  BESK0 entry returns the value 0.0 for ARG  >  XMAX.
!
!
!  Intrinsic functions required are:
!
!     EXP, LOG, SQRT
!
!  Latest modification: March 19, 1990
!
!  Authors: W. J. Cody and Laura Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    INTEGER  :: I
    real(DP) :: XUMF, XUMG, XUMP, XUMQ, TEMP, X, XX
    real(DP) :: P (6), Q (2), PP (10), QQ (10), F (4), G (3)
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
    real(DP),parameter :: XMAX = 705.342690906186027895908504301149267D0 
!--------------------------------------------------------------------
!
!     Coefficients for xsmalls  <=   ARG   <=  1.0
!
!--------------------------------------------------------------------
    DATA P / 5.8599221412826100000D-04, 1.3166052564989571850D-01,    &
    1.1999463724910714109D+01, 4.6850901201934832188D+02,             &
    5.9169059852270512312D+03, 2.4708152720399552679D+03 /
    DATA Q / - 2.4994418972832303646D+02, 2.1312714303849120380D+04 /
    DATA F / - 1.6414452837299064100D+00, - 2.9601657892958843866D+02,&
    - 1.7733784684952985886D+04, - 4.0320340761145482298D+05 /
    DATA G / - 2.5064972445877992730D+02, 2.9865713163054025489D+04,  &
    - 1.6128136304458193998D+06 /
!--------------------------------------------------------------------
!
!     Coefficients for  1.0  <  ARG
!
!--------------------------------------------------------------------
    DATA PP / 1.1394980557384778174D+02, 3.6832589957340267940D+03,   &
    3.1075408980684392399D+04, 1.0577068948034021957D+05,             &
    1.7398867902565686251D+05, 1.5097646353289914539D+05,             &
    7.1557062783764037541D+04, 1.8321525870183537725D+04,             &
    2.3444738764199315021D+03, 1.1600249425076035558D+02 /
    DATA QQ / 2.0013443064949242491D+02, 4.4329628889746408858D+03,   &
    3.1474655750295278825D+04, 9.7418829762268075784D+04,             &
    1.5144644673520157801D+05, 1.2689839587977598727D+05,             &
    5.8824616785857027752D+04, 1.4847228371802360957D+04,             &
    1.8821890840982713696D+03, 9.2556599177304839811D+01 /
!--------------------------------------------------------------------
    X = ARG
    IF (X > ZERO) THEN
      IF (X <= ONE) THEN
!--------------------------------------------------------------------
!     0.0  <   ARG   <=  1.0
!--------------------------------------------------------------------
        TEMP = LOG (X)
        IF (X < xsmalls) THEN
!--------------------------------------------------------------------
!     Return for small ARG
!--------------------------------------------------------------------
          RESLUT = P (6) / Q (2) - TEMP
        ELSE
          XX = X * X
          XUMP = ( ( ( (P (1) * XX + P (2) ) * XX + P (3) ) * XX + P (&
          4) ) * XX + P (5) ) * XX + P (6)
          XUMQ = (XX + Q (1) ) * XX + Q (2)
          XUMF = ( (F (1) * XX + F (2) ) * XX + F (3) ) * XX + F (4)
          XUMG = ( (XX + G (1) ) * XX + G (2) ) * XX + G (3)
          RESLUT = XUMP / XUMQ - XX * XUMF * TEMP / XUMG - TEMP
          IF (JINT == 2) RESLUT = RESLUT * EXP (X)
        ENDIF
      ELSE IF ( (JINT == 1) .AND. (X > XMAX) ) THEN
!--------------------------------------------------------------------
!     Error return for ARG  >  XMAX
!--------------------------------------------------------------------
        RESLUT = ZERO
      ELSE
!--------------------------------------------------------------------
!     1.0  <  ARG
!--------------------------------------------------------------------
        XX = ONE / X
        XUMP = PP (1)
        DO 120 I = 2, 10
          XUMP = XUMP * XX + PP (I)
120     enddo
        XUMQ = XX
        DO 140 I = 1, 9
          XUMQ = (XUMQ + QQ (I) ) * XX
140     enddo
        XUMQ = XUMQ + QQ (10)
        RESLUT = XUMP / XUMQ / SQRT (X)
        IF (JINT == 1) RESLUT = RESLUT * EXP ( - X)
      ENDIF
    ELSE
!--------------------------------------------------------------------
!     Error return for ARG  <=  0.0
!--------------------------------------------------------------------
      RESLUT = D1mach(2)
    ENDIF
!--------------------------------------------------------------------
!     Update error counts, etc.
!--------------------------------------------------------------------
    RETURN
!---------- Last line of CALCK0 ----------
END SUBROUTINE CALCK0
real(DP) FUNCTION BESK0 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order zero
!   for arguments 0.0  <  ARG  <=  XMAX (see comments heading
!   CALCK0).
!
!  Authors: W. J. Cody and Laura Stoltz
!
!  Latest Modification: January 19, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALCK0 (X, RESLUT, JINT)
    BESK0 = RESLUT
    RETURN
!---------- Last line of BESK0 ----------
END FUNCTION BESK0
real(DP) FUNCTION BESEK0 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order zero
!   multiplied by the Exponential function, for arguments
!   0.0  <  ARG.
!
!  Authors: W. J. Cody and Laura Stoltz
!
!  Latest Modification: January 19, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 2
    CALL CALCK0 (X, RESLUT, JINT)
    BESEK0 = RESLUT
    RETURN
!---------- Last line of BESEK0 ----------
END FUNCTION BESEK0
SUBROUTINE CALCK1 (ARG, RESLUT, JINT)
!--------------------------------------------------------------------
!
! This packet computes modified Bessel functions of the second kind
!   and order one,  K1(X)  and  EXP(X)*K1(X), for real arguments X.
!   It contains two function type subprograms, BESK1  and  BESEK1,
!   and one subroutine type subprogram, CALCK1.  The calling
!   statements for the primary entries are
!
!                   Y=BESK1(X)
!   and
!                   Y=BESEK1(X)
!
!   where the entry points correspond to the functions K1(X) and
!   EXP(X)*K1(X), respectively.  The routine CALCK1 is intended
!   for internal packet use only, all computations within the
!   packet being concentrated in this routine.  The function
!   subprograms invoke CALCK1 with the statement
!          CALL CALCK1(ARG,RESLUT,JINT)
!   where the parameter usage is as follows
!
!      Function                      Parameters for CALCK1
!        Call             ARG                  RESLUT          JINT
!
!     BESK1(ARG)  D1mach(1)  <  ARG  <  XMAX    K1(ARG)          1
!     BESEK1(ARG)     D1mach(1)  <  ARG       EXP(ARG)*K1(ARG)    2
!
!   The main computation evaluates slightly modified forms of near
!   minimax rational approximations generated by Russon and Blair,
!   Chalk River (Atomic Energy of Canada Limited) Report AECL-3461,
!   1969.  This transportable program is patterned after the
!   machine-dependent FUNPACK packet NATSK1, but cannot match that
!   version for efficiency or accuracy.  This version uses rational
!   functions that theoretically approximate K-SUB-1(X) to at
!   least 18 significant decimal digits.  The accuracy achieved
!   depends on the arithmetic system, the compiler, the intrinsic
!   functions, and proper selection of the machine-dependent
!   constants.
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   D1mach(1) = Smallest acceptable argument, i.e., smallest machine
!            number X such that 1/X is machine representable.
!   XSMALL = Argument below which BESK1(X) and BESEK1(X) may
!            each be represented by 1/X.  A safe value is the
!            largest X such that  1.0 + X = 1.0  to machine
!            precision.
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMAX   = Largest argument acceptable to BESK1;  Solution to
!            equation:
!               W(X) * (1+3/8X-15/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!
!
!     Approximate values for some important machines are:
!
!                           beta       minexp       maxexp
!
!  CRAY-1        (S.P.)       2        -8193         8191
!  Cyber 180/185
!    under NOS   (S.P.)       2         -975         1070
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)       2         -126          128
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)       2        -1022         1024
!  IBM 3033      (D.P.)      16          -65           63
!  VAX D-Format  (D.P.)       2         -128          127
!  VAX G-Format  (D.P.)       2        -1024         1023
!
!
!                         D1mach(1)     XSMALL      XINF       XMAX
!
! CRAY-1                1.84E-2466  3.55E-15  5.45E+2465  5674.858
! Cyber 180/855
!   under NOS   (S.P.)  3.14E-294   1.77E-15  1.26E+322    672.789
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.18E-38    5.95E-8   3.40E+38      85.343
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  2.23D-308   1.11D-16  1.79D+308    705.343
! IBM 3033      (D.P.)  1.39D-76    1.11D-16  7.23D+75     177.855
! VAX D-Format  (D.P.)  5.88D-39    6.95D-18  1.70D+38      86.721
! VAX G-Format  (D.P.)  1.12D-308   5.55D-17  8.98D+307    706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  The program returns the value XINF for ARG  <=  0.0 and the
!   BESK1 entry returns the value 0.0 for ARG  >  XMAX.
!
!
!  Intrinsic functions required are:
!
!     LOG, SQRT, EXP
!
!
!  Authors: W. J. Cody and Laura Stoltz
!           Mathematics and Computer Science Division
!           Argonne National Laboratory
!           Argonne, IL 60439
!
!  Latest modification: January 28, 1988
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: ARG
    integer,intent(IN)   :: JINT
    real(DP),intent(OUT) :: RESLUT
    INTEGER  :: I
    real(DP) :: XUMF, XUMG, XUMP, XUMQ, X, XX
    real(DP) :: P(5), Q(3), PP(11), QQ(9), F(5), G(3)
!--------------------------------------------------------------------
!  Machine-dependent constants
!--------------------------------------------------------------------
    real(DP),parameter :: XMAX = 705.343398776792880905460244591441313D+0
!--------------------------------------------------------------------
!  Coefficients for  D1mach(1)  <=   ARG   <=  1.0
!--------------------------------------------------------------------
    DATA P / 4.8127070456878442310D-1, 9.9991373567429309922D+1,      &
    7.1885382604084798576D+3, 1.7733324035147015630D+5,               &
    7.1938920065420586101D+5 /
    DATA Q / - 2.8143915754538725829D+2, 3.7264298672067697862D+4,    &
    - 2.2149374878243304548D+6 /
    DATA F / - 2.2795590826955002390D-1, - 5.3103913335180275253D+1,  &
    - 4.5051623763436087023D+3, - 1.4758069205414222471D+5, -         &
    1.3531161492785421328D+6 /
    DATA G / - 3.0507151578787595807D+2, 4.3117653211351080007D+4,    &
    - 2.7062322985570842656D+6 /
!--------------------------------------------------------------------
!  Coefficients for  1.0  <   ARG
!--------------------------------------------------------------------
    DATA PP / 6.4257745859173138767D-2, 7.5584584631176030810D+0,     &
    1.3182609918569941308D+2, 8.1094256146537402173D+2,               &
    2.3123742209168871550D+3, 3.4540675585544584407D+3,               &
    2.8590657697910288226D+3, 1.3319486433183221990D+3,               &
    3.4122953486801312910D+2, 4.4137176114230414036D+1,               &
    2.2196792496874548962D+0 /
    DATA QQ / 3.6001069306861518855D+1, 3.3031020088765390854D+2,     &
    1.2082692316002348638D+3, 2.1181000487171943810D+3,               &
    1.9448440788918006154D+3, 9.6929165726802648634D+2,               &
    2.5951223655579051357D+2, 3.4552228452758912848D+1,               &
    1.7710478032601086579D+0 /
!--------------------------------------------------------------------
    X = ARG
    IF (X < D1mach(1)) THEN
!--------------------------------------------------------------------
!  Error return for  ARG   <  D1mach(1)
!--------------------------------------------------------------------
      RESLUT = D1mach(2)
    ELSE IF (X <= ONE) THEN
!--------------------------------------------------------------------
!  D1mach(1)  <=   ARG   <=  1.0
!--------------------------------------------------------------------
      IF (X < xsmalls) THEN
!--------------------------------------------------------------------
!  Return for small ARG
!--------------------------------------------------------------------
        RESLUT = ONE / X
      ELSE
        XX = X * X
        XUMP = ( ( ( (P (1) * XX + P (2) ) * XX + P (3) ) * XX + P (4)&
        ) * XX + P (5) ) * XX + Q (3)
        XUMQ = ( (XX + Q (1) ) * XX + Q (2) ) * XX + Q (3)
        XUMF = ( ( (F (1) * XX + F (2) ) * XX + F (3) ) * XX + F (4) )&
        * XX + F (5)
        XUMG = ( (XX + G (1) ) * XX + G (2) ) * XX + G (3)
        RESLUT = (XX * LOG (X) * XUMF / XUMG + XUMP / XUMQ) / X
        IF (JINT == 2) RESLUT = RESLUT * EXP (X)
      ENDIF
    ELSE IF ( (JINT == 1) .AND. (X > XMAX) ) THEN
!--------------------------------------------------------------------
!  Error return for  ARG   >  XMAX
!--------------------------------------------------------------------
      RESLUT = ZERO
    ELSE
!--------------------------------------------------------------------
!  1.0  <   ARG
!--------------------------------------------------------------------
      XX = ONE / X
      XUMP = PP (1)
      DO 120 I = 2, 11
        XUMP = XUMP * XX + PP (I)
120   enddo
      XUMQ = XX
      DO 140 I = 1, 8
        XUMQ = (XUMQ + QQ (I) ) * XX
140   enddo
      XUMQ = XUMQ + QQ (9)
      RESLUT = XUMP / XUMQ / SQRT (X)
      IF (JINT == 1) RESLUT = RESLUT * EXP ( - X)
    ENDIF
    RETURN
!---------- Last line of CALCK1 ----------
END SUBROUTINE CALCK1
real(DP) FUNCTION BESK1 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order one
!   for arguments  D1mach(1)  <=  ARG  <=  XMAX.
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 1
    CALL CALCK1 (X, RESLUT, JINT)
    BESK1 = RESLUT
    RETURN
!---------- Last line of BESK1 ----------
END FUNCTION BESK1
real(DP) FUNCTION BESEK1 (X)
!--------------------------------------------------------------------
!
! This function program computes approximate values for the
!   modified Bessel function of the second kind of order one
!   multiplied by the exponential function, for arguments
!   D1mach(1)  <=  ARG  <=  XMAX.
!
!--------------------------------------------------------------------
    implicit none
    real(DP),intent(IN)  :: X
    INTEGER  :: JINT
    real(DP) :: RESLUT
!--------------------------------------------------------------------
    JINT = 2
    CALL CALCK1 (X, RESLUT, JINT)
    BESEK1 = RESLUT
    RETURN
!---------- Last line of BESEK1 ----------
END FUNCTION BESEK1
SUBROUTINE RJBESL (X, ALPHA, NB, B, NCALC)
!---------------------------------------------------------------------
! This routine calculates Bessel functions J sub(N+ALPHA) (X)
!   for non-negative argument X, and non-negative order N+ALPHA.
!
!
!  Explanation of variables in the calling sequence.
!
!   X     - working precision non-negative real argument for which
!           J's are to be calculated.
!   ALPHA - working precision fractional part of order for which
!           J's or exponentially scaled J'r (J*exp(X)) are
!           to be calculated.  0 <= ALPHA < 1.0.
!   NB  - integer number of functions to be calculated, NB > 0.
!           The first function calculated is of order ALPHA, and the
!           last is of order (NB - 1 + ALPHA).
!   B  - working precision output vector of length NB.  If RJBESL
!           terminates normally (NCALC=NB), the vector B contains the
!           functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
!           corresponding exponentially scaled functions.
!   NCALC - integer output variable indicating possible errors.
!           Before using the vector B, the user should check that
!           NCALC=NB, i.e., all orders have been calculated to
!           the desired accuracy.  See Error Returns below.
!
!
!*******************************************************************
!*******************************************************************
!
!  Explanation of machine-dependent constants
!
!   it     = Number of bits in the mantissa of a working precision
!            variable
!   NSIG   = Decimal significance desired.  Should be set to
!            INT(LOG10(2)*it+1).  Setting NSIG lower will RESLUT
!            in decreased accuracy while setting NSIG higher will
!            increase CPU time without increasing accuracy.  The
!            truncation error is limited to a relative error of
!            T=.5*10**(-NSIG).
!   ENTEN  = 10.0 ** K, where K is the largest integer such that
!            ENTEN is machine-representable in working precision
!   ENSIG  = 10.0 ** NSIG
!   RTNSIG = 10.0 ** (-K) for the smallest integer K such that
!            K  >=  NSIG/4
!   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!   XLARGE = Upper limit on the magnitude of X.  If ABS(X)=N,
!            then at least N iterations of the backward recursion
!            will be executed.  The value of 10.0 ** 4 is used on
!            every machine.
!
!
!     Approximate values for some important machines are:
!
!
!                            it    NSIG    ENTEN       ENSIG
!
!   CRAY-1        (S.P.)     48     15    1.0E+2465   1.0E+15
!   Cyber 180/855
!     under NOS   (S.P.)     48     15    1.0E+322    1.0E+15
!   IEEE (IBM/XT,
!     SUN, etc.)  (S.P.)     24      8    1.0E+38     1.0E+8
!   IEEE (IBM/XT,
!     SUN, etc.)  (D.P.)     53     16    1.0D+308    1.0D+16
!   IBM 3033      (D.P.)     14      5    1.0D+75     1.0D+5
!   VAX           (S.P.)     24      8    1.0E+38     1.0E+8
!   VAX D-Format  (D.P.)     56     17    1.0D+38     1.0D+17
!   VAX G-Format  (D.P.)     53     16    1.0D+307    1.0D+16
!
!
!                           RTNSIG      ENMTEN      XLARGE
!
!   CRAY-1        (S.P.)    1.0E-4    1.84E-2466   1.0E+4
!   Cyber 180/855
!     under NOS   (S.P.)    1.0E-4    1.25E-293    1.0E+4
!   IEEE (IBM/XT,
!     SUN, etc.)  (S.P.)    1.0E-2    4.70E-38     1.0E+4
!   IEEE (IBM/XT,
!     SUN, etc.)  (D.P.)    1.0E-4    8.90D-308    1.0D+4
!   IBM 3033      (D.P.)    1.0E-2    2.16D-78     1.0D+4
!   VAX           (S.P.)    1.0E-2    1.17E-38     1.0E+4
!   VAX D-Format  (D.P.)    1.0E-5    1.17D-38     1.0D+4
!   VAX G-Format  (D.P.)    1.0E-4    2.22D-308    1.0D+4
!
!*******************************************************************
!*******************************************************************
!
!  Error returns
!
!    In case of an error,  NCALC  /=  NB, and not all J's are
!    calculated to the desired accuracy.
!
!    NCALC  <  0:  An argument is out of range. For example,
!       NBES  <=  0, ALPHA  <  0 or  >  1, or X is too large.
!       In this case, B(1) is set to zero, the remainder of the
!       B-vector is not calculated, and NCALC is set to
!       MIN(NB,0)-1 so that NCALC  /=  NB.
!
!    NB  >  NCALC  >  0: Not all requested function values could
!       be calculated accurately.  This usually occurs because NB is
!       much larger than ABS(X).  In this case, B(N) is calculated
!       to the desired accuracy for N  <=  NCALC, but precision
!       is lost for NCALC  <  N  <=  NB.  If B(N) does not vanish
!       for N  >  NCALC (because it is too small to be represented),
!       and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!       significant figures of B(N) can be trusted.
!
!
!  Intrinsic and other functions required are:
!
!     ABS, AINT, COS, DBLE, GAMMA (or DGAMMA), INT, MAX, MIN,
!
!     REAL, SIN, SQRT
!
!
!  Acknowledgement
!
!   This program is based on a program written by David J. Sookne
!   (2) that computes values of the Bessel functions J or I of real
!   argument and integer order.  Modifications include the restriction
!   of the computation to the J Bessel function of non-negative real
!   argument, the extension of the computation to arbitrary positive
!   order, and the elimination of most underflow.
!
!  References: "A Note on Backward Recurrence Algorithms," Olver,
!               F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
!               pp 941-947.
!
!              "Bessel Functions of Real Argument and Integer Order,"
!               Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
!               125-132.
!
!  Latest modification: March 19, 1990
!
!  Author: W. J. Cody
!          Applied Mathematics Division
!          Argonne National Laboratory
!          Argonne, IL  60439
!
!---------------------------------------------------------------------
    implicit none
    integer,intent(IN)   :: NB
    real(DP),intent(IN)  :: X, ALPHA
    integer,intent(OUT)  :: NCALC
    real(DP),intent(OUT) :: B(NB)
    INTEGER  :: I, J, K, L, M, MAGX, N, NBMX, NEND, NSTART
    real(DP) :: ALPEM, ALP2EM, CAPP, CAPQ,EM, EN, GNU,HALFX, P, PLAST, &
    POLD, PSAVE, PSAVEL, S, XUM, T, T1, TEMPA, TEMPB, TEMPC, TEST, TOVER, &
    XC, XIN, XK, XM, VCOS, VSIN, Z
!---------------------------------------------------------------------
!     Factorial(N)
!---------------------------------------------------------------------
    real(DP),parameter :: FACT(25)=(/one,one,two,six,24.D0,1.2D2,7.2D2, &
    5.04D3, 4.032D4, 3.6288D5, 3.6288D6, 3.99168D7, 4.790016D8,       &
    6.2270208D9, 8.71782912D10, 1.307674368D12, 2.0922789888D13,      &
    3.55687428096D14, 6.402373705728D15, 1.21645100408832D17,         &
    2.43290200817664D18, 5.109094217170944D19, 1.12400072777760768D21,&
    2.585201673888497664D22, 6.2044840173323943936D23 /)
    real(DP),parameter :: TWOFIV = 25.0D0, ONE30 = 130.0D0, THREE5 = 35.0D0
!---------------------------------------------------------------------
!  Machine-dependent parameters
!---------------------------------------------------------------------
    real(DP),parameter :: enten=d1mach(2),ensig=one/d1mach(3), rtnsig=sqrt(sqrt(d1mach(3))), &
                          enmten=four*d1mach(1), xlarge=16.d0/sqrt(d1mach(4)) !1.d4
!---------------------------------------------------------------------
!  Mathematical constants
!
!   PI2    - 2 / PI
!   TWOPI1 - first few significant digits of 2 * PI
!   TWOPI2 - (2*PI - TWOPI) to working precision, i.e.,
!            TWOPI1 + TWOPI2 = 2 * PI to extra precision.
!---------------------------------------------------------------------
! Statement functions for conversion and the gamma function.
!---------------------------------------------------------------------
!    FUNC (X) = DGAMMA (X)
!---------------------------------------------------------------------
! Check for out of range arguments.
!---------------------------------------------------------------------
    MAGX = INT (X)
    IF ( (NB > 0) .AND. (X >= ZERO) .AND. (X <= XLARGE) .AND. (      &
    ALPHA >= ZERO) .AND. (ALPHA < ONE) ) THEN
!---------------------------------------------------------------------
! Initialize RESLUT array to zero.
!---------------------------------------------------------------------
    NCALC = NB
    DO 20 I = 1, NB
      B (I) = ZERO
20  enddo
!---------------------------------------------------------------------
! Branch to use 2-term ascending series for small X and asymptotic
! form for large X when NB is not too large.
!---------------------------------------------------------------------
    IF (X < RTNSIG) THEN
!---------------------------------------------------------------------
! Two-term ascending series for small X.
!---------------------------------------------------------------------
      TEMPA = ONE
      ALPEM = ONE+ALPHA
      HALFX = ZERO
      IF (X > ENMTEN) HALFX = HALF * X
      IF (ALPHA /= ZERO) TEMPA = HALFX**ALPHA / (ALPHA * Spec_GAMMA(ALPHA) )
      TEMPB = ZERO
      IF ( (X + ONE)  > ONE) TEMPB = - HALFX * HALFX
      B (1) = TEMPA + TEMPA * TEMPB / ALPEM
      IF ( (X /= ZERO) .AND. (B (1)  == ZERO) ) NCALC = 0
      IF (NB /= 1) THEN
        IF (X <= ZERO) THEN
          DO 30 N = 2, NB
            B (N) = ZERO
30        enddo
        ELSE
!---------------------------------------------------------------------
! Calculate higher order functions.
!---------------------------------------------------------------------
          TEMPC = HALFX
          TOVER = (ENMTEN + ENMTEN) / X
          IF (TEMPB /= ZERO) TOVER = ENMTEN / TEMPB
          DO 50 N = 2, NB
            TEMPA = TEMPA / ALPEM
            ALPEM = ALPEM + ONE
            TEMPA = TEMPA * TEMPC
            IF (TEMPA <= TOVER * ALPEM) TEMPA = ZERO
            B (N) = TEMPA + TEMPA * TEMPB / ALPEM
            IF ( (B (N)  == ZERO) .AND. (NCALC > N) ) NCALC = N -  &
            1
50        enddo
        ENDIF
      ENDIF
    ELSE IF ( (X > TWOFIV) .AND. (NB <= MAGX + 1) ) THEN
!---------------------------------------------------------------------
! Asymptotic series for X  >  21.0.
!---------------------------------------------------------------------
      XC = SQRT (twoonpi / X)
      XIN = (EIGHTH / X) **2
      M = 11
      IF (X >= THREE5) M = 8
      IF (X >= ONE30) M = 4
      XM = FOUR * REAL(M,DP)
!---------------------------------------------------------------------
! Argument reduction for SIN and COS routines.
!---------------------------------------------------------------------
      T = AINT (X / (TWOPI1 + TWOPI2) + HALF)
      Z = ( (X - T * TWOPI1) - T * TWOPI2) - (ALPHA + HALF) / twoonpi
      VSIN = SIN (Z)
      VCOS = COS (Z)
      GNU = ALPHA + ALPHA
      DO 80 I = 1, 2
        S = ( (XM - ONE) - GNU) * ( (XM - ONE) + GNU) * XIN * HALF
        T = (GNU - (XM - THREE) ) * (GNU + (XM - THREE) )
        CAPP = S * T / FACT (2 * M + 1)
        T1 = (GNU - (XM + ONE) ) * (GNU + (XM + ONE) )
        CAPQ = S * T1 / FACT (2 * M + 2)
        XK = XM
        K = M + M
        T1 = T
        DO 70 J = 2, M
          XK = XK - FOUR
          S = ( (XK - ONE) - GNU) * ( (XK - ONE) + GNU)
          T = (GNU - (XK - THREE) ) * (GNU + (XK - THREE) )
          CAPP = (CAPP + ONE / FACT (K - 1) ) * S * T * XIN
          CAPQ = (CAPQ + ONE / FACT (K) ) * S * T1 * XIN
          K = K - 2
          T1 = T
70      enddo
        CAPP = CAPP + ONE
        CAPQ = (CAPQ + ONE) * (GNU * GNU - ONE) * (EIGHTH / X)
        B (I) = XC * (CAPP * VCOS - CAPQ * VSIN)
        IF (NB == 1) GOTO 300
        T = VSIN
        VSIN = - VCOS
        VCOS = T
        GNU = GNU + TWO
80    enddo
!---------------------------------------------------------------------
! If  NB  >  2, compute J(X,ORDER+I)  I = 2, NB-1
!---------------------------------------------------------------------
      IF (NB > 2) THEN
        GNU = ALPHA + ALPHA + TWO
        DO 90 J = 3, NB
          B (J) = GNU * B (J - 1) / X - B (J - 2)
          GNU = GNU + TWO
90      enddo
      ENDIF
!---------------------------------------------------------------------
! Use recurrence to generate RESLUTs.  First initialize the
! calculation of P*S.
!---------------------------------------------------------------------
    ELSE
      NBMX = NB - MAGX
      N = MAGX + 1
      EN = REAL(N + N, DP) + (ALPHA + ALPHA)
      PLAST = ONE
      P = EN / X
!---------------------------------------------------------------------
! Calculate general significance test.
!---------------------------------------------------------------------
      TEST = ENSIG + ENSIG
      IF (NBMX >= 3) THEN
!---------------------------------------------------------------------
! Calculate P*S until N = NB-1.  Check for possible overflow.
!---------------------------------------------------------------------
        TOVER = ENTEN / ENSIG
        NSTART = MAGX + 2
        NEND = NB - 1
        EN = REAL(NSTART + NSTART, DP) - TWO + (ALPHA + ALPHA)
        DO 130 K = NSTART, NEND
          N = K
          EN = EN + TWO
          POLD = PLAST
          PLAST = P
          P = EN * PLAST / X - POLD
          IF (P > TOVER) THEN
!---------------------------------------------------------------------
! To avoid overflow, divide P*S by TOVER.  Calculate P*S until
! ABS(P)  >  1.
!---------------------------------------------------------------------
            TOVER = ENTEN
            P = P / TOVER
            PLAST = PLAST / TOVER
            PSAVE = P
            PSAVEL = PLAST
            NSTART = N + 1
            100           N = N + 1
            EN = EN + TWO
            POLD = PLAST
            PLAST = P
            P = EN * PLAST / X - POLD
            IF (P <= ONE) GOTO 100
            TEMPB = EN / X
!---------------------------------------------------------------------
! Calculate backward test and find NCALC, the highest N such that
! the test is passed.
!---------------------------------------------------------------------
            TEST = POLD * PLAST * (HALF - HALF / (TEMPB * TEMPB) )
            TEST = TEST / ENSIG
            P = PLAST * TOVER
            N = N - 1
            EN = EN - TWO
            NEND = MIN (NB, N)
            DO 110 L = NSTART, NEND
              POLD = PSAVEL
              PSAVEL = PSAVE
              PSAVE = EN * PSAVEL / X - POLD
              IF (PSAVE * PSAVEL > TEST) THEN
                NCALC = L - 1
                GOTO 190
              ENDIF
110         enddo
            NCALC = NEND
            GOTO 190
          ENDIF
130     enddo
        N = NEND
        EN = REAL(N + N, DP) + (ALPHA + ALPHA)
!---------------------------------------------------------------------
! Calculate special significance test for NBMX  >  2.
!---------------------------------------------------------------------
        TEST = MAX (TEST, SQRT (PLAST * ENSIG) * SQRT (P + P) )
      ENDIF
!---------------------------------------------------------------------
! Calculate P*S until significance test passes.
!---------------------------------------------------------------------
      140     N = N + 1
      EN = EN + TWO
      POLD = PLAST
      PLAST = P
      P = EN * PLAST / X - POLD
      IF (P < TEST) GOTO 140
!---------------------------------------------------------------------
! Initialize the backward recursion and the normalization XUM.
!---------------------------------------------------------------------
      190     N = N + 1
      EN = EN + TWO
      TEMPB = ZERO
      TEMPA = ONE / P
      M = 2 * N - 4 * (N / 2)
      XUM = ZERO
      EM = REAL(N / 2, DP)
      ALPEM = (EM - ONE) + ALPHA
      ALP2EM = (EM + EM) + ALPHA
      IF (M /= 0) XUM = TEMPA * ALPEM * ALP2EM / EM
      NEND = N - NB
      IF (NEND > 0) THEN
!---------------------------------------------------------------------
! Recur backward via difference equation, calculating (but not
! storing) B(N), until N = NB.
!---------------------------------------------------------------------
        DO 200 L = 1, NEND
          N = N - 1
          EN = EN - TWO
          TEMPC = TEMPB
          TEMPB = TEMPA
          TEMPA = (EN * TEMPB) / X - TEMPC
          M = 2 - M
          IF (M /= 0) THEN
            EM = EM - ONE
            ALP2EM = (EM + EM) + ALPHA
            IF (N == 1) GOTO 210
            ALPEM = (EM - ONE) + ALPHA
            IF (ALPEM == ZERO) ALPEM = ONE
            XUM = (XUM + TEMPA * ALP2EM) * ALPEM / EM
          ENDIF
200     enddo
      ENDIF
!---------------------------------------------------------------------
! Store B(NB).
!---------------------------------------------------------------------
      210     B (N) = TEMPA
      IF (NEND >= 0) THEN
        IF (NB <= 1) THEN
          ALP2EM = ALPHA
          IF ( (ALPHA + ONE)  == ONE) ALP2EM = ONE
          XUM = XUM + B (1) * ALP2EM
          GOTO 250
        ELSE
!---------------------------------------------------------------------
! Calculate and store B(NB-1).
!---------------------------------------------------------------------
          N = N - 1
          EN = EN - TWO
          B (N) = (EN * TEMPA) / X - TEMPB
          IF (N == 1) GOTO 240
          M = 2 - M
          IF (M /= 0) THEN
            EM = EM - ONE
            ALP2EM = (EM + EM) + ALPHA
            ALPEM = (EM - ONE) + ALPHA
            IF (ALPEM == ZERO) ALPEM = ONE
            XUM = (XUM + B (N) * ALP2EM) * ALPEM / EM
          ENDIF
        ENDIF
      ENDIF
      NEND = N - 2
      IF (NEND /= 0) THEN
!---------------------------------------------------------------------
! Calculate via difference equation and store B(N), until N = 2.
!---------------------------------------------------------------------
        DO 230 L = 1, NEND
          N = N - 1
          EN = EN - TWO
          B (N) = (EN * B (N + 1) ) / X - B (N + 2)
          M = 2 - M
          IF (M /= 0) THEN
            EM = EM - ONE
            ALP2EM = (EM + EM) + ALPHA
            ALPEM = (EM - ONE) + ALPHA
            IF (ALPEM == ZERO) ALPEM = ONE
            XUM = (XUM + B (N) * ALP2EM) * ALPEM / EM
          ENDIF
230     enddo
      ENDIF
!---------------------------------------------------------------------
! Calculate B(1).
!---------------------------------------------------------------------
      B (1) = TWO * (ALPHA + ONE) * B (2) / X - B (3)
      240     EM = EM - ONE
      ALP2EM = (EM + EM) + ALPHA
      IF (ALP2EM == ZERO) ALP2EM = ONE
      XUM = XUM + B (1) * ALP2EM
!---------------------------------------------------------------------
! Normalize.  Divide all B(N) by XUM.
!---------------------------------------------------------------------
250   IF ( (ALPHA + ONE)  /= ONE) XUM = XUM * Spec_GAMMA(ALPHA) * &
      (X * HALF) ** ( - ALPHA)
      TEMPA = ENMTEN
      IF (XUM > ONE) TEMPA = TEMPA * XUM
      DO 260 N = 1, NB
        IF (ABS (B (N) )  < TEMPA) B (N) = ZERO
        B (N) = B (N) / XUM
260   enddo
    ENDIF
!---------------------------------------------------------------------
! Error return -- X, NB, or ALPHA is out of range.
!---------------------------------------------------------------------
  ELSE
    B (1) = ZERO
    NCALC = MIN (NB, 0) - 1
  ENDIF
!---------------------------------------------------------------------
! Exit
!---------------------------------------------------------------------
  300 RETURN
! ---------- Last line of RJBESL ----------
END SUBROUTINE RJBESL

SUBROUTINE RYBESL (X, ALPHA, NB, BY, NCALC)
!----------------------------------------------------------------------
!
!  This routine calculates Bessel functions Y SUB(N+ALPHA) (X)
!  for non-negative argument X, and non-negative order N+ALPHA.
!
!
! Explanation of variables in the calling sequence
!
! X     - Working precision non-negative real argument for which
!         Y's are to be calculated.
! ALPHA - Working precision fractional part of order for which
!         Y's are to be calculated.  0  <=  ALPHA  <  1.0.
! NB    - Integer number of functions to be calculated, NB  >  0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! BY    - Working precision output vector of length NB.  If the
!         routine terminates normally (NCALC=NB), the vector BY
!         contains the functions Y(ALPHA,X), ... , Y(NB-1+ALPHA,X),
!         If (0  <  NCALC  <  NB), BY(I) contains correct function
!         values for I  <=  NCALC, and contains the ratios
!         Y(ALPHA+I-1,X)/Y(ALPHA+I-2,X) for the rest of the array.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector BY, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   p      = Number of significant base-beta digits in the
!            significand of a floating-point number
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   EPS    = beta ** (-p)
!   DEL    = Machine number below which sin(x)/x = 1; approximately
!            SQRT(EPS).
!   XMIN   = Smallest acceptable argument for RBESY; approximately
!            max(2*beta**minexp,2/XINF), rounded up
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   THRESH = Lower bound for use of the asymptotic form; approximately
!            AINT(-LOG10(EPS/2.0))+1.0
!   XLARGE = Upper bound on X; approximately 1/DEL, because the sine
!            and cosine functions have lost about half of their
!            precision at that point.
!
!
!     Approximate values for some important machines are:
!
!                        beta    p     minexp      maxexp      EPS
!
!  CRAY-1        (S.P.)    2    48     -8193        8191    3.55E-15
!  Cyber 180/185
!    under NOS   (S.P.)    2    48      -975        1070    3.55E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2    24      -126         128    5.96E-8
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2    53     -1022        1024    1.11D-16
!  IBM 3033      (D.P.)   16    14       -65          63    1.39D-17
!  VAX           (S.P.)    2    24      -128         127    5.96E-8
!  VAX D-Format  (D.P.)    2    56      -128         127    1.39D-17
!  VAX G-Format  (D.P.)    2    53     -1024        1023    1.11D-16
!
!
!                         DEL      XMIN      XINF     THRESH  XLARGE
!
! CRAY-1        (S.P.)  5.0E-8  3.67E-2466 5.45E+2465  15.0E0  2.0E7
! Cyber 180/855
!   under NOS   (S.P.)  5.0E-8  6.28E-294  1.26E+322   15.0E0  2.0E7
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.0E-4  2.36E-38   3.40E+38     8.0E0  1.0E4
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  1.0D-8  4.46D-308  1.79D+308   16.0D0  1.0D8
! IBM 3033      (D.P.)  1.0D-8  2.77D-76   7.23D+75    17.0D0  1.0D8
! VAX           (S.P.)  1.0E-4  1.18E-38   1.70E+38     8.0E0  1.0E4
! VAX D-Format  (D.P.)  1.0D-9  1.18D-38   1.70D+38    17.0D0  1.0D9
! VAX G-Format  (D.P.)  1.0D-8  2.23D-308  8.98D+307   16.0D0  1.0D8
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error, NCALC  /=  NB, and not all Y's are
!  calculated to the desired accuracy.
!
!  NCALC  <  -1:  An argument is out of range. For example,
!       NB  <=  0, IZE is not 1 or 2, or IZE=1 and ABS(X)  >=
!       XMAX.  In this case, BY(1) = 0.0, the remainder of the
!       BY-vector is not calculated, and NCALC is set to
!       MIN0(NB,0)-2  so that NCALC  /=  NB.
!  NCALC = -1:  Y(ALPHA,X)  >=  XINF.  The requested function
!       values are set to 0.0.
!  1  <  NCALC  <  NB: Not all requested function values could
!       be calculated accurately.  BY(I) contains correct function
!       values for I  <=  NCALC, and and the remaining NB-NCALC
!       array elements contain 0.0.
!
!
! Intrinsic functions required are:
!
!     DBLE, EXP, INT, MAX, MIN, REAL, SQRT
!
!
! Acknowledgement
!
!  This program draws heavily on Temme's Algol program for Y(a,x)
!  and Y(a+1,x) and on Campbell's programs for Y_nu(x).  Temme's
!  scheme is used for  x < THRESH, and Campbell's scheme is used
!  in the asymptotic region.  Segments of code from both sources
!  have been translated into Fortran 77, merged, and heavily modified.
!  Modifications include parameterization of machine dependencies,
!  use of a new approximation for ln(gamma(x)), and built-in
!  protection against over/underflow.
!
! References: "Bessel functions J_nu(x) and Y_nu(x) of real
!              order and real argument," Campbell, J. B.,
!              Comp. Phy. Comm. 18, 1979, pp. 133-142.
!
!             "On the numerical evaluation of the ordinary
!              Bessel function of the second kind," Temme,
!              N. M., J. Comput. Phys. 21, 1976, pp. 343-350.
!
!  Latest modification: March 19, 1990
!
!  Modified by: W. J. Cody
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!----------------------------------------------------------------------
    implicit none
    INTEGER,intent(IN)  :: NB
    INTEGER,intent(OUT) :: NCALC
    real(DP),intent(IN) :: X,ALPHA
    real(DP),intent(OUT):: BY(NB)

    real(DP),parameter :: EPS=D1Mach(3), XINF=D1MACH(2)
    real(DP),parameter :: XMIN=max(two/XINF,two*D1MACH(1))
    real(DP),parameter :: DEL = sqrt(6.d0*D1MACH(4)),XLARGE=1.d0/DEL

    INTEGER  :: I, K, NA
    real(DP) :: ALFA, AYE, B, C, COSMU, D, DEN, DDIV, DIV, DMU, &
                D1, D2, E, EN, ENU, EN1, EVEN, EX, F, G, GAMMA, &
                H, ODD, P, PA, PA1, Q, &
                QA, QA1, Q0, R, S, SINMU, TERM, THRESH, &
                TWOBYX, XNA, X2, YA, YA1
!----------------------------------------------------------------------
!  Mathematical constants
!    FIVPI = 5*PI
!    PIM5 = 5*PI - 15
!    ONBPI = 1/PI
!    PIBY2 = PI/2
!    SQ2BPI = SQUARE ROOT OF 2/PI
!----------------------------------------------------------------------
    real(DP),parameter :: ONBPI = one/Pi, FIVPI=five*Pi, PIBY2 = half*Pi
    real(DP),parameter :: SQ2BPI = 7.97884560802865355879892119868763739d-1 
    real(DP),parameter :: PIM5=7.079632679489661923132169163975144D-1
!----------------------------------------------------------------------
!  Machine-dependent constants
!----------------------------------------------------------------------
!    DATA DEL/ 1.0D-8 /
!    DATA XLARGE,thresh / 1.0D8, 16.0d0 /
!----------------------------------------------------------------------
!  Coefficients for Chebyshev polynomial expansion of
!         1/gamma(1-x), abs(x)  <=  .5
!----------------------------------------------------------------------
    real(DP),parameter :: CH(21)=(/ - 0.67735241822398840964D-23, &
    -.61455180116049879894D-22, 0.29017595056104745456D-20,   &
    0.13639417919073099464D-18, 0.23826220476859635824D-17,   &
    -.90642907957550702534D-17, - 0.14943667065169001769D-14, &
    -.33919078305362211264D-13, - 0.17023776642512729175D-12, &
    0.91609750938768647911D-11, 0.24230957900482704055D-09,   &
    0.17451364971382984243D-08, - 0.33126119768180852711D-07, &
    -.86592079961391259661D-06, - 0.49717367041957398581D-05, &
    0.76309597585908126618D-04, 0.12719271366545622927D-02,   &
    0.17063050710955562222D-02, - 0.76852840844786673690D-01, &
    -.28387654227602353814D+00, 0.92187029365045265648D+00 /)
!----------------------------------------------------------------------
    THRESH=real(ceiling(-log10(EPS)),DP)
    EX = X
    ENU = ALPHA
    IF ( (NB > 0) .AND. (X >= XMIN) .AND. (EX < XLARGE) .AND. (     &
    ENU >= ZERO) .AND. (ENU < ONE) ) THEN
    XNA = AINT (ENU + HALF)
    NA = INT (XNA)
    IF (NA == 1) ENU = ENU - XNA
    IF (ENU ==  - HALF) THEN
      P = SQ2BPI / SQRT (EX)
      YA = P * SIN (EX)
      YA1 = - P * COS (EX)
    ELSE IF (EX < THREE) THEN
!----------------------------------------------------------------------
!  Use Temme's scheme for small X
!----------------------------------------------------------------------
      B = EX * HALF
      D = - LOG (B)
      F = ENU * D
      E = B** ( - ENU)
      IF (ABS (ENU)  < DEL) THEN
        C = ONBPI
      ELSE
        C = ENU / SIN (ENU * PI)
      ENDIF
!----------------------------------------------------------------------
!  Computation of sinh(f)/f
!----------------------------------------------------------------------
      IF (ABS (F)  < ONE) THEN
        X2 = F * F
        EN = nineteen
        S = ONE
        DO 80 I = 1, 9
          S = S * X2 / EN / (EN - ONE) + ONE
          EN = EN - TWO
80      enddo
      ELSE
        S = (E-ONE / E) * HALF / F
      ENDIF
!----------------------------------------------------------------------
!  Computation of 1/gamma(1-a) using Chebyshev polynomials
!----------------------------------------------------------------------
      X2 = ENU * ENU * EIGHT
      AYE = CH (1)
      EVEN = ZERO
      ALFA = CH (2)
      ODD = ZERO
      DO 40 I = 3, 19, 2
        EVEN = - (AYE+AYE+EVEN)
        AYE = - EVEN * X2 - AYE+CH (I)
        ODD = - (ALFA + ALFA + ODD)
        ALFA = - ODD * X2 - ALFA + CH (I + 1)
40    enddo
      EVEN = (EVEN * HALF + AYE) * X2 - AYE+CH (21)
      ODD = (ODD+ALFA) * TWO
      GAMMA = ODD * ENU + EVEN
!----------------------------------------------------------------------
!  End of computation of 1/gamma(1-a)
!----------------------------------------------------------------------
      G = E * GAMMA
      E = (E+ONE / E) * HALF
      F = TWO * C * (ODD * E+EVEN * S * D)
      E = ENU * ENU
      P = G * C
      Q = ONBPI / G
      C = ENU * PIBY2
      IF (ABS (C)  < DEL) THEN
        R = ONE
      ELSE
        R = SIN (C) / C
      ENDIF
      R = PI * C * R * R
      C = ONE
      D = - B * B
      H = ZERO
      YA = F + R * Q
      YA1 = P
      EN = ZERO
      100     EN = EN + ONE
      IF (ABS (G / (ONE+ABS (YA) ) ) + ABS (H / (ONE+ABS (YA1) ) )  &
       > EPS) THEN
      F = (F * EN + P + Q) / (EN * EN - E)
      C = C * D / EN
      P = P / (EN - ENU)
      Q = Q / (EN + ENU)
      G = C * (F + R * Q)
      H = C * P - EN * G
      YA = YA + G
      YA1 = YA1 + H
      GOTO 100
    ENDIF
    YA = - YA
    YA1 = - YA1 / B
  ELSE IF (EX < THRESH) THEN
!----------------------------------------------------------------------
!  Use Temme's scheme for moderate X
!----------------------------------------------------------------------
    C = (HALF - ENU) * (HALF + ENU)
    B = EX + EX
    E = (EX * ONBPI * COS (ENU * PI) / EPS)
    E = E * E
    P = ONE
    Q = - EX
    R = ONE+EX * EX
    S = R
    EN = TWO
    200     IF (R * EN * EN < E) THEN
    EN1 = EN + ONE
    D = (EN - ONE+C / EN) / S
    P = (EN + EN - P * D) / EN1
    Q = ( - B + Q * D) / EN1
    S = P * P + Q * Q
    R = R * S
    EN = EN1
    GOTO 200
  ENDIF
  F = P / S
  P = F
  G = - Q / S
  Q = G
  220     EN = EN - ONE
  IF (EN > ZERO) THEN
    R = EN1 * (TWO - P) - TWO
    S = B + EN1 * Q
    D = (EN - ONE+C / EN) / (R * R + S * S)
    P = D * R
    Q = D * S
    E = F + ONE
    F = P * E-G * Q
    G = Q * E+P * G
    EN1 = EN
    GOTO 220
  ENDIF
  F = ONE+F
  D = F * F + G * G
  PA = F / D
  QA = - G / D
  D = ENU + HALF - P
  Q = Q + EX
  PA1 = (PA * Q - QA * D) / EX
  QA1 = (QA * Q + PA * D) / EX
  B = EX - PIBY2 * (ENU + HALF)
  C = COS (B)
  S = SIN (B)
  D = SQ2BPI / SQRT (EX)
  YA = D * (PA * S + QA * C)
  YA1 = D * (QA1 * S - PA1 * C)
ELSE
!----------------------------------------------------------------------
!  Use Campbell's asymptotic scheme.
!----------------------------------------------------------------------
  NA = 0
  D1 = AINT (EX / FIVPI)
  I = INT (D1)
  DMU = ( (EX - fifteen * D1) - D1 * PIM5) - (ALPHA + HALF)        &
  * PIBY2
  IF (I - 2 * (I / 2)  == 0) THEN
    COSMU = COS (DMU)
    SINMU = SIN (DMU)
  ELSE
    COSMU = - COS (DMU)
    SINMU = - SIN (DMU)
  ENDIF
  DDIV = EIGHT * EX
  DMU = ALPHA
  DEN = SQRT (EX)
  DO 350 K = 1, 2
    P = COSMU
    COSMU = SINMU
    SINMU = - P
    D1 = (TWO * DMU - ONE) * (TWO * DMU + ONE)
    D2 = ZERO
    DIV = DDIV
    P = ZERO
    Q = ZERO
    Q0 = D1 / DIV
    TERM = Q0
    DO 310 I = 2, 20
      D2 = D2 + EIGHT
      D1 = D1 - D2
      DIV = DIV + DDIV
      TERM = - TERM * D1 / DIV
      P = P + TERM
      D2 = D2 + EIGHT
      D1 = D1 - D2
      DIV = DIV + DDIV
      TERM = TERM * D1 / DIV
      Q = Q + TERM
      IF (ABS (TERM)  <= EPS) GOTO 320
310 enddo
    320       P = P + ONE
    Q = Q + Q0
    IF (K == 1) THEN
      YA = SQ2BPI * (P * COSMU - Q * SINMU) / DEN
    ELSE
      YA1 = SQ2BPI * (P * COSMU - Q * SINMU) / DEN
    ENDIF
    DMU = DMU + ONE
350 enddo
ENDIF
IF (NA == 1) THEN
  H = TWO * (ENU + ONE) / EX
  IF (H > ONE) THEN
    IF (ABS (YA1)  > XINF / H) THEN
      H = ZERO
      YA = ZERO
    ENDIF
  ENDIF
  H = H * YA1 - YA
  YA = YA1
  YA1 = H
ENDIF
!----------------------------------------------------------------------
!  Now have first one or two Y's
!----------------------------------------------------------------------
BY (1) = YA
BY (2) = YA1
IF (YA1 == ZERO) THEN
  NCALC = 1
ELSE
  AYE = ONE+ALPHA
  TWOBYX = TWO / EX
  NCALC = 2
  DO 400 I = 3, NB
    IF (TWOBYX < ONE) THEN
      IF (ABS (BY (I - 1) ) * TWOBYX >= XINF / AYE) GOTO 450
    ELSE
      IF (ABS (BY (I - 1) )  >= XINF / AYE / TWOBYX) GOTO 450
    ENDIF
    BY (I) = TWOBYX * AYE * BY (I - 1) - BY (I - 2)
    AYE = AYE+ONE
    NCALC = NCALC + 1
400 enddo
ENDIF
450   DO 460 I = NCALC + 1, NB
BY (I) = ZERO
460 enddo
ELSE
  BY (1) = ZERO
  NCALC = MIN (NB, 0) - 1
ENDIF
900 RETURN
!---------- Last line of RYBESL ----------
END SUBROUTINE RYBESL

SUBROUTINE RIBESL(X, ALPHA, NB, IZE, B, NCALC)
!-------------------------------------------------------------------
!
!  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
!  for non-negative argument X, and non-negative order N+ALPHA,
!  with or without exponential scaling.
!
!
! Explanation of variables in the calling sequence
!
! X     - Working precision non-negative real argument for which
!         I's or exponentially scaled I's (I*EXP(-X))
!         are to be calculated.  If I's are to be calculated,
!         X must be less than EXPARG (see below).
! ALPHA - Working precision fractional part of order for which
!         I's or exponentially scaled I's (I*EXP(-X)) are
!         to be calculated.  0  <=  ALPHA  <  1.0.
! NB    - Integer number of functions to be calculated, NB  >  0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated,
!         and 2 if exponentially scaled I's are to be calculated.
! B     - Working precision output vector of length NB.  If the routine
!         terminates normally (NCALC=NB), the vector B contains the
!         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
!         corresponding exponentially scaled functions.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector B, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   it     = Number of bits in the mantissa of a working precision
!            variable
!   NSIG   = Decimal significance desired.  Should be set to
!            INT(LOG10(2)*it+1).  Setting NSIG lower will RESLUT
!            in decreased accuracy while setting NSIG higher will
!            increase CPU time without increasing accuracy.  The
!            truncation error is limited to a relative error of
!            T=.5*10**(-NSIG).
!   ENTEN  = 10.0 ** K, where K is the largest integer such that
!            ENTEN is machine-representable in working precision
!   ENSIG  = 10.0 ** NSIG
!   RTNSIG = 10.0 ** (-K) for the smallest integer K such that
!            K  >=  NSIG/4
!   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
!   XLARGE = Upper limit on the magnitude of X when IZE=2.  Bear
!            in mind that if ABS(X)=N, then at least N iterations
!            of the backward recursion will be executed.  The value
!            of 10.0 ** 4 is used on every machine.
!   EXPARG = Largest working precision argument that the library
!            EXP routine can handle and upper limit on the
!            magnitude of X when IZE=1; approximately
!            LOG(beta**maxexp)
!
!
!     Approximate values for some important machines are:
!
!                        beta       minexp      maxexp       it
!
!  CRAY-1        (S.P.)    2        -8193        8191        48
!  Cyber 180/855
!    under NOS   (S.P.)    2         -975        1070        48
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)    2         -126         128        24
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)    2        -1022        1024        53
!  IBM 3033      (D.P.)   16          -65          63        14
!  VAX           (S.P.)    2         -128         127        24
!  VAX D-Format  (D.P.)    2         -128         127        56
!  VAX G-Format  (D.P.)    2        -1024        1023        53
!
!
!                        NSIG       ENTEN       ENSIG      RTNSIG
!
! CRAY-1        (S.P.)    15       1.0E+2465   1.0E+15     1.0E-4
! Cyber 180/855
!   under NOS   (S.P.)    15       1.0E+322    1.0E+15     1.0E-4
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)    16       1.0D+308    1.0D+16     1.0D-4
! IBM 3033      (D.P.)     5       1.0D+75     1.0D+5      1.0D-2
! VAX           (S.P.)     8       1.0E+38     1.0E+8      1.0E-2
! VAX D-Format  (D.P.)    17       1.0D+38     1.0D+17     1.0D-5
! VAX G-Format  (D.P.)    16       1.0D+307    1.0D+16     1.0D-4
!
!
!                         ENMTEN      XLARGE   EXPARG
!
! CRAY-1        (S.P.)   1.84E-2466   1.0E+4    5677
! Cyber 180/855
!   under NOS   (S.P.)   1.25E-293    1.0E+4     741
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)   4.70E-38     1.0E+4      88
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)   8.90D-308    1.0D+4     709
! IBM 3033      (D.P.)   2.16D-78     1.0D+4     174
! VAX           (S.P.)   1.17E-38     1.0E+4      88
! VAX D-Format  (D.P.)   1.17D-38     1.0D+4      88
! VAX G-Format  (D.P.)   2.22D-308    1.0D+4     709
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error,  NCALC  /=  NB, and not all I's are
!  calculated to the desired accuracy.
!
!  NCALC  <  0:  An argument is out of range. For example,
!     NB  <=  0, IZE is not 1 or 2, or IZE=1 and ABS(X)  >=  EXPARG.
!     In this case, the B-vector is not calculated, and NCALC is
!     set to MIN0(NB,0)-1 so that NCALC  /=  NB.
!
!  NB  >  NCALC  >  0: Not all requested function values could
!     be calculated accurately.  This usually occurs because NB is
!     much larger than ABS(X).  In this case, B(N) is calculated
!     to the desired accuracy for N  <=  NCALC, but precision
!     is lost for NCALC  <  N  <=  NB.  If B(N) does not vanish
!     for N  >  NCALC (because it is too small to be represented),
!     and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
!     significant figures of B(N) can be trusted.
!
!
! Intrinsic functions required are:
!
!     DBLE, EXP, DGAMMA, GAMMA, INT, MAX, MIN, REAL, SQRT
!
!
! Acknowledgement
!
!  This program is based on a program written by David J.
!  Sookne (2) that computes values of the Bessel functions J or
!  I of real argument and integer order.  Modifications include
!  the restriction of the computation to the I Bessel function
!  of non-negative real argument, the extension of the computation
!  to arbitrary positive order, the inclusion of optional
!  exponential scaling, and the elimination of most underflow.
!  An earlier version was published in (3).
!
! References: "A Note on Backward Recurrence Algorithms," Olver,
!              F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
!              pp 941-947.
!
!             "Bessel Functions of Real Argument and Integer Order,"
!              Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
!              125-132.
!
!             "ALGORITHM 597, Sequence of Modified Bessel Functions
!              of the First Kind," Cody, W. J., Trans. Math. Soft.,
!              1983, pp. 242-245.
!
!  Latest modification: May 30, 1989
!
!  Modified by: W. J. Cody and L. Stoltz
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!-------------------------------------------------------------------
!X, ALPHA, NB, IZE, B, NCALC)
    implicit none
    INTEGER,intent(IN)  :: NB,IZE
    INTEGER,intent(OUT) :: NCALC
    real(DP),intent(IN) :: X,ALPHA
    real(DP),intent(OUT):: B(NB)
    INTEGER  :: K, L, MAGX, N, NBMX, NEND, NSIG, NSTART
    real(DP) :: EM, EMPAL, EMP2AL, EN, EXPARG, HALFX, P, &
    PLAST, POLD, PSAVE, PSAVEL, XUM, TEMPA, TEMPB, TEMPC, TEST, TOVER
!-------------------------------------------------------------------
!  Mathematical constants
!-------------------------------------------------------------------
    real(DP),parameter :: CONST = 1.585D0
!-------------------------------------------------------------------
!  Machine-dependent parameters
!-------------------------------------------------------------------
    real(DP),parameter :: enten=d1mach(2),ensig=one/d1mach(3), rtnsig=sqrt(sqrt(d1mach(3))), &
                          enmten=four*d1mach(1), xlarge=16.d0/sqrt(d1mach(4)) !1.d4
!-------------------------------------------------------------------
!  Statement functions for conversion
!-------------------------------------------------------------------
!    FUNC (X) = DGAMMA (X)
!-------------------------------------------------------------------
! Check for X, NB, OR IZE out of range.
!-------------------------------------------------------------------
    nsig=NINT(-LOG10(d1mach(3)))
    exparg=log(enten)
    IF ((NB>0).AND.(X>=ZERO).AND.(ALPHA>=ZERO).AND.(ALPHA<ONE).AND. &
       (((IZE==1).AND.(X<=EXPARG)).OR.((IZE==2).AND.(X<=XLARGE)))) THEN
!-------------------------------------------------------------------
! Use 2-term ascending series for small X
!-------------------------------------------------------------------
      NCALC = NB
      MAGX = INT (X)
      IF (X >= RTNSIG) THEN
!-------------------------------------------------------------------
! Initialize the forward sweep, the P-sequence of Olver
!-------------------------------------------------------------------
        NBMX = NB - MAGX
        N = MAGX + 1
        EN = REAL(N + N,DP) + (ALPHA + ALPHA)
        PLAST = ONE
        P = EN / X
!-------------------------------------------------------------------
! Calculate general significance test
!-------------------------------------------------------------------
        TEST = ENSIG + ENSIG
        IF (2 * MAGX > 5 * NSIG) THEN
          TEST = SQRT (TEST * P)
        ELSE
          TEST = TEST / CONST**MAGX
        ENDIF
        IF (NBMX >= 3) THEN
!-------------------------------------------------------------------
! Calculate P-sequence until N = NB-1.  Check for possible overflow.
!-------------------------------------------------------------------
          TOVER = ENTEN / ENSIG
          NSTART = MAGX + 2
          NEND = NB - 1
          DO 100 K = NSTART, NEND
            N = K
            EN = EN + TWO
            POLD = PLAST
            PLAST = P
            P = EN * PLAST / X + POLD
            IF (P > TOVER) THEN
!-------------------------------------------------------------------
! To avoid overflow, divide P-sequence by TOVER.  Calculate
! P-sequence until ABS(P)  >  1.
!-------------------------------------------------------------------
              TOVER = ENTEN
              P = P / TOVER
              PLAST = PLAST / TOVER
              PSAVE = P
              PSAVEL = PLAST
              NSTART = N + 1
 60           N = N + 1
              EN = EN + TWO
              POLD = PLAST
              PLAST = P
              P = EN * PLAST / X + POLD
              IF (P <= ONE) GOTO 60
              TEMPB = EN / X
!-------------------------------------------------------------------
! Calculate backward test, and find NCALC, the highest N
! such that the test is passed.
!-------------------------------------------------------------------
              TEST = POLD * PLAST / ENSIG
              TEST = TEST * (HALF - HALF / (TEMPB * TEMPB) )
              P = PLAST * TOVER
              N = N - 1
              EN = EN - TWO
              NEND = MIN0 (NB, N)
              DO 80 L = NSTART, NEND
                NCALC = L
                POLD = PSAVEL
                PSAVEL = PSAVE
                PSAVE = EN * PSAVEL / X + POLD
                IF (PSAVE * PSAVEL > TEST) GOTO 90
80            enddo
              NCALC = NEND+1
90            NCALC = NCALC - 1
              GOTO 120
            ENDIF
100       enddo
          N = NEND
          EN = real(N + N,DP) + (ALPHA + ALPHA)
!-------------------------------------------------------------------
! Calculate special significance test for NBMX  >  2.
!-------------------------------------------------------------------
          TEST = MAX (TEST, SQRT (PLAST * ENSIG) * SQRT (P + P) )
        ENDIF
!-------------------------------------------------------------------
! Calculate P-sequence until significance test passed.
!-------------------------------------------------------------------
110     N = N + 1
        EN = EN + TWO
        POLD = PLAST
        PLAST = P
        P = EN * PLAST / X + POLD
        IF (P < TEST) GOTO 110
!-------------------------------------------------------------------
! Initialize the backward recursion and the normalization XUM.
!-------------------------------------------------------------------
120     N = N + 1
        EN = EN + TWO
        TEMPB = ZERO
        TEMPA = ONE / P
        EM = REAL(N,DP) - ONE
        EMPAL = EM + ALPHA
        EMP2AL = (EM - ONE) + (ALPHA + ALPHA)
        XUM = TEMPA * EMPAL * EMP2AL / EM
        NEND = N - NB
        IF (NEND < 0) THEN
!-------------------------------------------------------------------
! N  <  NB, so store B(N) and set higher orders to zero.
!-------------------------------------------------------------------
          B (N) = TEMPA
          NEND = - NEND
          DO L = 1, NEND
            B(N + L) = ZERO
          enddo
        ELSE
          IF (NEND > 0) THEN
!-------------------------------------------------------------------
! Recur backward via difference equation, calculating (but
! not storing) B(N), until N = NB.
!-------------------------------------------------------------------
            DO 140 L = 1, NEND
              N = N - 1
              EN = EN - TWO
              TEMPC = TEMPB
              TEMPB = TEMPA
              TEMPA = (EN * TEMPB) / X + TEMPC
              EM = EM - ONE
              EMP2AL = EMP2AL - ONE
              IF (N == 1) GOTO 150
              IF (N == 2) EMP2AL = ONE
              EMPAL = EMPAL - ONE
              XUM = (XUM + TEMPA * EMPAL) * EMP2AL / EM
140         enddo
          ENDIF
!-------------------------------------------------------------------
! Store B(NB)
!-------------------------------------------------------------------
150       B (N) = TEMPA
          IF (NB <= 1) THEN
            XUM = (XUM + XUM) + TEMPA
            GOTO 230
          ENDIF
!-------------------------------------------------------------------
! Calculate and Store B(NB-1)
!-------------------------------------------------------------------
          N = N - 1
          EN = EN - TWO
          B (N) = (EN * TEMPA) / X + TEMPB
          IF (N == 1) GOTO 220
          EM = EM - ONE
          EMP2AL = EMP2AL - ONE
          IF (N == 2) EMP2AL = ONE
          EMPAL = EMPAL - ONE
          XUM = (XUM + B (N) * EMPAL) * EMP2AL / EM
        ENDIF
        NEND = N - 2
        IF (NEND > 0) THEN
!-------------------------------------------------------------------
! Calculate via difference equation and store B(N), until N = 2.
!-------------------------------------------------------------------
          DO 200 L = 1, NEND
            N = N - 1
            EN = EN - TWO
            B (N) = (EN * B (N + 1) ) / X + B (N + 2)
            EM = EM - ONE
            EMP2AL = EMP2AL - ONE
            IF (N == 2) EMP2AL = ONE
            EMPAL = EMPAL - ONE
            XUM = (XUM + B (N) * EMPAL) * EMP2AL / EM
200       enddo
        ENDIF
!-------------------------------------------------------------------
! Calculate B(1)
!-------------------------------------------------------------------
        B (1) = TWO * EMPAL * B (2) / X + B (3)
220     XUM = (XUM + XUM) + B (1)
!-------------------------------------------------------------------
! Normalize.  Divide all B(N) by XUM.
!-------------------------------------------------------------------
230     IF (ALPHA /= ZERO) XUM = XUM*Spec_GAMMA(ONE+ALPHA)*(X*HALF)**(-ALPHA)
        IF (IZE == 1) XUM = XUM * EXP ( - X)
        TEMPA = ENMTEN
        IF (XUM > ONE) TEMPA = TEMPA * XUM
        DO 260 N = 1, NB
          IF (B (N)  < TEMPA) B (N) = ZERO
          B (N) = B (N) / XUM
260     enddo
        RETURN
!-------------------------------------------------------------------
! Two-term ascending series for small X.
!-------------------------------------------------------------------
      ELSE
        TEMPA = ONE
        EMPAL = ONE+ALPHA
        HALFX = ZERO
        IF (X > ENMTEN) HALFX = HALF * X
        IF (ALPHA /= ZERO) TEMPA = HALFX**ALPHA / Spec_GAMMA(EMPAL)
        IF (IZE == 2) TEMPA = TEMPA * EXP ( - X)
        TEMPB = ZERO
        IF ( (X + ONE)  > ONE) TEMPB = HALFX * HALFX
        B (1) = TEMPA + TEMPA * TEMPB / EMPAL
        IF ( (X /= ZERO) .AND. (B (1)  == ZERO) ) NCALC = 0
        IF (NB > 1) THEN
          IF (X == ZERO) THEN
            DO 310 N = 2, NB
              B (N) = ZERO
310         enddo
          ELSE
!-------------------------------------------------------------------
! Calculate higher-order functions.
!-------------------------------------------------------------------
            TEMPC = HALFX
            TOVER = (ENMTEN + ENMTEN) / X
            IF (TEMPB /= ZERO) TOVER = ENMTEN / TEMPB
            DO 340 N = 2, NB
              TEMPA = TEMPA / EMPAL
              EMPAL = EMPAL + ONE
              TEMPA = TEMPA * TEMPC
              IF (TEMPA <= TOVER * EMPAL) TEMPA = ZERO
              B (N) = TEMPA + TEMPA * TEMPB / EMPAL
              IF ( (B (N)  == ZERO) .AND. (NCALC > N) ) NCALC = N -  &
              1
340         enddo
          ENDIF
        ENDIF
      ENDIF
    ELSE
      NCALC = MIN0 (NB, 0) - 1
    ENDIF
    RETURN
!---------- Last line of RIBESL ----------
END SUBROUTINE RIBESL
!___________________________________________________________________
SUBROUTINE RKBESL (X, ALPHA, NB, IZE, BK, NCALC)
!-------------------------------------------------------------------
!
!  This FORTRAN 77 routine calculates modified Bessel functions
!  of the second kind, K SUB(N+ALPHA) (X), for non-negative
!  argument X, and non-negative order N+ALPHA, with or without
!  exponential scaling.
!
!  Explanation of variables in the calling sequence
!
!  Description of output values ..
!
! X     - Working precision non-negative real argument for which
!         K's or exponentially scaled K's (K*EXP(X))
!         are to be calculated.  If K's are to be calculated,
!         X must not be greater than XMAX (see below).
! ALPHA - Working precision fractional part of order for which
!         K's or exponentially scaled K's (K*EXP(X)) are
!         to be calculated.  0  <=  ALPHA  <  1.0.
! NB    - Integer number of functions to be calculated, NB  >  0.
!         The first function calculated is of order ALPHA, and the
!         last is of order (NB - 1 + ALPHA).
! IZE   - Integer type.  IZE = 1 if unscaled K's are to be calculated,
!         and 2 if exponentially scaled K's are to be calculated.
! BK    - Working precision output vector of length NB.  If the
!         routine terminates normally (NCALC=NB), the vector BK
!         contains the functions K(ALPHA,X), ... , K(NB-1+ALPHA,X),
!         or the corresponding exponentially scaled functions.
!         If (0  <  NCALC  <  NB), BK(I) contains correct function
!         values for I  <=  NCALC, and contains the ratios
!         K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
! NCALC - Integer output variable indicating possible errors.
!         Before using the vector BK, the user should check that
!         NCALC=NB, i.e., all orders have been calculated to
!         the desired accuracy.  See error returns below.
!
!
!*******************************************************************
!*******************************************************************
!
! Explanation of machine-dependent constants
!
!   beta   = Radix for the floating-point system
!   minexp = Smallest representable power of beta
!   maxexp = Smallest power of beta that overflows
!   EPS    = The smallest positive floating-point number such that
!            1.0+EPS  >  1.0
!   XMAX   = Upper limit on the magnitude of X when IZE=1;  Solution
!            to equation:
!               W(X) * (1-1/8X+9/128X**2) = beta**minexp
!            where  W(X) = EXP(-X)*SQRT(PI/2X)
!   SQXMIN = Square root of beta**minexp
!   XINF   = Largest positive machine number; approximately
!            beta**maxexp
!   XMIN   = Smallest positive machine number; approximately
!            beta**minexp
!
!
!     Approximate values for some important machines are:
!
!                          beta       minexp      maxexp      EPS
!
!  CRAY-1        (S.P.)      2        -8193        8191    7.11E-15
!  Cyber 180/185
!    under NOS   (S.P.)      2         -975        1070    3.55E-15
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)      2         -126         128    1.19E-7
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)      2        -1022        1024    2.22D-16
!  IBM 3033      (D.P.)     16          -65          63    2.22D-16
!  VAX           (S.P.)      2         -128         127    5.96E-8
!  VAX D-Format  (D.P.)      2         -128         127    1.39D-17
!  VAX G-Format  (D.P.)      2        -1024        1023    1.11D-16
!
!
!                         SQXMIN       XINF        XMIN      XMAX
!
! CRAY-1        (S.P.)  6.77E-1234  5.45E+2465  4.59E-2467 5674.858
! Cyber 180/855
!   under NOS   (S.P.)  1.77E-147   1.26E+322   3.14E-294   672.788
! IEEE (IBM/XT,
!   SUN, etc.)  (S.P.)  1.08E-19    3.40E+38    1.18E-38     85.337
! IEEE (IBM/XT,
!   SUN, etc.)  (D.P.)  1.49D-154   1.79D+308   2.23D-308   705.342
! IBM 3033      (D.P.)  7.35D-40    7.23D+75    5.40D-79    177.852
! VAX           (S.P.)  5.42E-20    1.70E+38    2.94E-39     86.715
! VAX D-Format  (D.P.)  5.42D-20    1.70D+38    2.94D-39     86.715
! VAX G-Format  (D.P.)  7.46D-155   8.98D+307   5.57D-309   706.728
!
!*******************************************************************
!*******************************************************************
!
! Error returns
!
!  In case of an error, NCALC  /=  NB, and not all K's are
!  calculated to the desired accuracy.
!
!  NCALC  <  -1:  An argument is out of range. For example,
!       NB  <=  0, IZE is not 1 or 2, or IZE=1 and ABS(X)  >=
!       XMAX.  In this case, the B-vector is not calculated,
!       and NCALC is set to MIN0(NB,0)-2  so that NCALC  /=  NB.
!  NCALC = -1:  Either  K(ALPHA,X)  >=  XINF  or
!       K(ALPHA+NB-1,X)/K(ALPHA+NB-2,X)  >=  XINF.  In this case,
!       the B-vector is not calculated.  Note that again
!       NCALC  /=  NB.
!
!  0  <  NCALC  <  NB: Not all requested function values could
!       be calculated accurately.  BK(I) contains correct function
!       values for I  <=  NCALC, and contains the ratios
!       K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
!
!
! Intrinsic functions required are:
!
!     ABS, AINT, EXP, INT, LOG, MAX, MIN, SINH, SQRT
!
!
! Acknowledgement
!
!  This program is based on a program written by J. B. Campbell
!  (2) that computes values of the Bessel functions K of real
!  argument and real order.  Modifications include the addition
!  of non-scaled functions, parameterization of machine
!  dependencies, and the use of more accurate approximations
!  for SINH and SIN.
!
! References: "On Temme's Algorithm for the Modified Bessel
!              Functions of the Third Kind," Campbell, J. B.,
!              TOMS 6(4), Dec. 1980, pp. 581-586.
!
!             "A FORTRAN IV Subroutine for the Modified Bessel
!              Functions of the Third Kind of Real Order and Real
!              Argument," Campbell, J. B., Report NRC/ERB-925,
!              National Research Council, Canada.
!
!  Latest modification: May 30, 1989
!
!  Modified by: W. J. Cody and L. Stoltz
!               Applied Mathematics Division
!               Argonne National Laboratory
!               Argonne, IL  60439
!
!-------------------------------------------------------------------
    implicit none
    integer,intent(IN)   :: NB, IZE
    real(DP),intent(IN)  :: x,alpha
    integer,intent(OUT)  :: Ncalc
    real(DP),intent(OUT) :: BK(NB)
    INTEGER  :: I, IEND, ITEMP, J, K, M, MPLUS1
    real(DP) :: BLPHA, BK1, BK2, C, DM, D1, D2, D3, ENU, EX, F0, F1, F2, P0, &
                Q0, RATIO, TWONU, TWOX, T1, T2, WMINF, X2BY4
    real(DP) :: P(8), Q(7), R(5), S(4), T(6), ESTM(6), ESTF(7)
    real(DP), parameter :: EPS=D1MACH(4),XINF=D1MACH(2),XMIN=D1MACH(1)
    real(DP), parameter :: SQXMIN=sqrt(XMIN) !1.49D-154
    real(DP), parameter :: XMAX = 705.342690906186027895908504301149267D0
!---------------------------------------------------------------------
!  Mathematical constants
!    A = LOG(2.D0) - Euler's constant
!    D = SQRT(2.D0/PI)
!---------------------------------------------------------------------
    real(DP), parameter :: TINYX = 1.0D-10
    real(DP), parameter :: A = 0.115931515658412448810720031375774137D0, &
                           D = 0.797884560802865355879892119868763739D0 
!---------------------------------------------------------------------
!  Machine dependent parameters
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!  P, Q - Approximation for LOG(GAMMA(1+ALPHA))/ALPHA
!                                         + Euler's constant
!         Coefficients converted from hex to decimal and modified
!         by W. J. Cody, 2/26/82
!  R, S - Approximation for (1-ALPHA*PI/SIN(ALPHA*PI))/(2.D0*ALPHA)
!  T    - Approximation for SINH(Y)/Y
!---------------------------------------------------------------------
    DATA P / 0.805629875690432845D00, 0.204045500205365151D02,        &
    0.157705605106676174D03, 0.536671116469207504D03,                 &
    0.900382759291288778D03, 0.730923886650660393D03,                 &
    0.229299301509425145D03, 0.822467033424113231D00 /
    DATA Q / 0.294601986247850434D02, 0.277577868510221208D03,        &
    0.120670325591027438D04, 0.276291444159791519D04,                 &
    0.344374050506564618D04, 0.221063190113378647D04,                 &
    0.572267338359892221D03 /
    DATA R / - 0.48672575865218401848D+0, 0.13079485869097804016D+2,  &
    - 0.10196490580880537526D+3, 0.34765409106507813131D+3,           &
    0.34958981245219347820D-3 /
    DATA S / - 0.25579105509976461286D+2, 0.21257260432226544008D+3,  &
    - 0.61069018684944109624D+3, 0.42269668805777760407D+3 /
    DATA T / 0.16125990452916363814D-9, 0.25051878502858255354D-7,    &
    0.27557319615147964774D-5, 0.19841269840928373686D-3,             &
    0.83333333333334751799D-2, 0.16666666666666666446D+0 /
    DATA ESTM / 5.20583D1, 5.7607D0, 2.7782D0, 1.44303D1, 1.853004D2, &
    9.3715D0 /
    DATA ESTF / 4.18341D1, 7.1075D0, 6.4306D0, 4.25110D1, 1.35633D0,  &
    8.45096D1, 2.0D1 /
!---------------------------------------------------------------------
    EX = X
    ENU = ALPHA
    NCALC = MIN (NB, 0) - 2
    IF ( (NB > 0) .AND. ( (ENU >= ZERO) .AND. (ENU < ONE) ) .AND. ( &
    (IZE >= 1) .AND. (IZE <= 2) ) .AND. ( (IZE /= 1) .OR. (EX <= XMAX)&
    ) .AND. (EX > ZERO) ) THEN
    K = 0
    IF (ENU < SQXMIN) ENU = ZERO
    IF (ENU > HALF) THEN
      K = 1
      ENU = ENU - ONE
    ENDIF
    TWONU = ENU + ENU
    IEND = NB + K - 1
    C = ENU * ENU
    D3 = - C
    IF (EX <= ONE) THEN
!---------------------------------------------------------------------
!  Calculation of P0 = GAMMA(1+ALPHA) * (2/X)**ALPHA
!                 Q0 = GAMMA(1-ALPHA) * (X/2)**ALPHA
!---------------------------------------------------------------------
      D1 = ZERO
      D2 = P (1)
      T1 = ONE
      T2 = Q (1)
      DO 10 I = 2, 7, 2
        D1 = C * D1 + P (I)
        D2 = C * D2 + P (I + 1)
        T1 = C * T1 + Q (I)
        T2 = C * T2 + Q (I + 1)
10    enddo
      D1 = ENU * D1
      T1 = ENU * T1
      F1 = LOG (EX)
      F0 = A + ENU * (P (8) - ENU * (D1 + D2) / (T1 + T2) ) - F1
      Q0 = EXP ( - ENU * (A - ENU * (P (8) + ENU * (D1 - D2)        &
      / (T1 - T2) ) - F1) )
      F1 = ENU * F0
      P0 = EXP (F1)
!---------------------------------------------------------------------
!  Calculation of F0 =
!---------------------------------------------------------------------
      D1 = R (5)
      T1 = ONE
      DO 20 I = 1, 4
        D1 = C * D1 + R (I)
        T1 = C * T1 + S (I)
20    enddo
      IF (ABS (F1)  <= HALF) THEN
        F1 = F1 * F1
        D2 = ZERO
        DO 30 I = 1, 6
          D2 = F1 * D2 + T (I)
30      enddo
        D2 = F0 + F0 * F1 * D2
      ELSE
        D2 = SINH (F1) / ENU
      ENDIF
      F0 = D2 - ENU * D1 / (T1 * P0)
      IF (EX <= TINYX) THEN
!--------------------------------------------------------------------
!  X <= 1.0E-10
!  Calculation of K(ALPHA,X) and X*K(ALPHA+1,X)/K(ALPHA,X)
!--------------------------------------------------------------------
        BK(1) = F0 + EX * F0
        IF (IZE == 1) BK(1) = BK(1) - EX * BK(1)
        RATIO = P0 / F0
        C = EX * XINF
        IF (K /= 0) THEN
!--------------------------------------------------------------------
!  Calculation of K(ALPHA,X) and X*K(ALPHA+1,X)/K(ALPHA,X),
!  ALPHA  >=  1/2
!--------------------------------------------------------------------
          NCALC = - 1
          IF (BK(1)  >= C / RATIO) GOTO 500
          BK(1) = RATIO * BK(1) / EX
          TWONU = TWONU + TWO
          RATIO = TWONU
        ENDIF
        NCALC = 1
        IF (NB == 1) GOTO 500
!--------------------------------------------------------------------
!  Calculate  K(ALPHA+L,X)/K(ALPHA+L-1,X),  L  =  1, 2, ... , NB-1
!--------------------------------------------------------------------
        NCALC = - 1
        DO 80 I = 2, NB
          IF (RATIO >= C) GOTO 500
          BK(I) = RATIO / EX
          TWONU = TWONU + TWO
          RATIO = TWONU
80      enddo
        NCALC = 1
        GOTO 420
      ELSE
!--------------------------------------------------------------------
!  1.0E-10  <  X  <=  1.0
!--------------------------------------------------------------------
        C = ONE
        X2BY4 = EX * EX / FOUR
        P0 = HALF * P0
        Q0 = HALF * Q0
        D1 = - ONE
        D2 = ZERO
        BK1 = ZERO
        BK2 = ZERO
        F1 = F0
        F2 = P0
        100       D1 = D1 + TWO
        D2 = D2 + ONE
        D3 = D1 + D3
        C = X2BY4 * C / D2
        F0 = (D2 * F0 + P0 + Q0) / D3
        P0 = P0 / (D2 - ENU)
        Q0 = Q0 / (D2 + ENU)
        T1 = C * F0
        T2 = C * (P0 - D2 * F0)
        BK1 = BK1 + T1
        BK2 = BK2 + T2
        IF ( (ABS (T1 / (F1 + BK1) )  > EPS) .OR. (ABS (T2 /       &
        (F2 + BK2) )  > EPS) ) GOTO 100
        BK1 = F1 + BK1
        BK2 = TWO * (F2 + BK2) / EX
        IF (IZE == 2) THEN
          D1 = EXP (EX)
          BK1 = BK1 * D1
          BK2 = BK2 * D1
        ENDIF
        WMINF = ESTF (1) * EX + ESTF (2)
      ENDIF
    ELSE IF (EPS * EX > ONE) THEN
!--------------------------------------------------------------------
!  X  >  ONE/EPS
!--------------------------------------------------------------------
      NCALC = NB
      BK1 = ONE / (D * SQRT (EX) )
      DO 110 I = 1, NB
        BK (I) = BK1
110   enddo
      GOTO 500
    ELSE
!--------------------------------------------------------------------
!  X  >  1.0
!--------------------------------------------------------------------
      TWOX = EX + EX
      BLPHA = ZERO
      RATIO = ZERO
      IF (EX <= FOUR) THEN
!--------------------------------------------------------------------
!  Calculation of K(ALPHA+1,X)/K(ALPHA,X),  1.0  <=  X  <=  4.0
!--------------------------------------------------------------------
        D2 = AINT (ESTM (1) / EX + ESTM (2) )
        M = INT (D2)
        D1 = D2 + D2
        D2 = D2 - HALF
        D2 = D2 * D2
        DO 120 I = 2, M
          D1 = D1 - TWO
          D2 = D2 - D1
          RATIO = (D3 + D2) / (TWOX + D1 - RATIO)
120     enddo
!--------------------------------------------------------------------
!  Calculation of I(|ALPHA|,X) and I(|ALPHA|+1,X) by backward
!    recurrence and K(ALPHA,X) from the wronskian
!--------------------------------------------------------------------
        D2 = AINT (ESTM (3) * EX + ESTM (4) )
        M = INT (D2)
        C = ABS (ENU)
        D3 = C + C
        D1 = D3 - ONE
        F1 = XMIN
        F0 = (TWO * (C + D2) / EX + HALF * EX / (C + D2 + ONE) )    &
        * XMIN
        DO 130 I = 3, M
          D2 = D2 - ONE
          F2 = (D3 + D2 + D2) * F0
          BLPHA = (ONE+D1 / D2) * (F2 + BLPHA)
          F2 = F2 / EX + F1
          F1 = F0
          F0 = F2
130     enddo
        F1 = (D3 + TWO) * F0 / EX + F1
        D1 = ZERO
        T1 = ONE
        DO 140 I = 1, 7
          D1 = C * D1 + P (I)
          T1 = C * T1 + Q (I)
140     enddo
        P0 = EXP (C * (A + C * (P (8) - C * D1 / T1) - LOG (EX) ) ) &
        / EX
        F2 = (C + HALF - RATIO) * F1 / EX
        BK1 = P0 + (D3 * F0 - F2 + F0 + BLPHA) / (F2 + F1 + F0)     &
        * P0
        IF (IZE == 1) BK1 = BK1 * EXP ( - EX)
        WMINF = ESTF (3) * EX + ESTF (4)
      ELSE
!--------------------------------------------------------------------
!  Calculation of K(ALPHA,X) and K(ALPHA+1,X)/K(ALPHA,X), by backward
!  recurrence, for  X  >  4.0
!--------------------------------------------------------------------
        DM = AINT (ESTM (5) / EX + ESTM (6) )
        M = INT (DM)
        D2 = DM - HALF
        D2 = D2 * D2
        D1 = DM + DM
        DO 160 I = 2, M
          DM = DM - ONE
          D1 = D1 - TWO
          D2 = D2 - D1
          RATIO = (D3 + D2) / (TWOX + D1 - RATIO)
          BLPHA = (RATIO + RATIO * BLPHA) / DM
160     enddo
        BK1 = ONE / ( (D+D * BLPHA) * SQRT (EX) )
        IF (IZE == 1) BK1 = BK1 * EXP ( - EX)
        WMINF = ESTF (5) * (EX - ABS (EX - ESTF (7) ) ) + ESTF (6)
      ENDIF
!--------------------------------------------------------------------
!  Calculation of K(ALPHA+1,X) from K(ALPHA,X) and
!    K(ALPHA+1,X)/K(ALPHA,X)
!--------------------------------------------------------------------
      BK2 = BK1 + BK1 * (ENU + HALF - RATIO) / EX
    ENDIF
!--------------------------------------------------------------------
!  Calculation of 'NCALC', K(ALPHA+I,X), I  =  0, 1, ... , NCALC-1,
!  K(ALPHA+I,X)/K(ALPHA+I-1,X), I  =  NCALC, NCALC+1, ... , NB-1
!--------------------------------------------------------------------
    NCALC = NB
    BK (1) = BK1
    IF (IEND == 0) GOTO 500
    J = 2 - K
    IF (J > 0) BK (J) = BK2
    IF (IEND == 1) GOTO 500
    M = MIN (INT (WMINF - ENU), IEND)
    DO 190 I = 2, M
      T1 = BK1
      BK1 = BK2
      TWONU = TWONU + TWO
      IF (EX < ONE) THEN
        IF (BK1 >=  (XINF / TWONU) * EX) GOTO 195
        GOTO 187
      ELSE
        IF (BK1 / EX >= XINF / TWONU) GOTO 195
      ENDIF
      187     CONTINUE
      BK2 = TWONU / EX * BK1 + T1
      ITEMP = I
      J = J + 1
      IF (J > 0) BK (J) = BK2
190 enddo
    195   M = ITEMP
    IF (M == IEND) GOTO 500
    RATIO = BK2 / BK1
    MPLUS1 = M + 1
    NCALC = - 1
    DO 410 I = MPLUS1, IEND
      TWONU = TWONU + TWO
      RATIO = TWONU / EX + ONE / RATIO
      J = J + 1
      IF (J > 1) THEN
        BK (J) = RATIO
      ELSE
        IF (BK2 >= XINF / RATIO) GOTO 500
        BK2 = RATIO * BK2
      ENDIF
410 enddo
    NCALC = MAX (MPLUS1 - K, 1)
    IF (NCALC == 1) BK (1) = BK2
    IF (NB == 1) GOTO 500
    420   J = NCALC + 1
    DO 430 I = J, NB
      IF (BK (NCALC)  >= XINF / BK (I) ) GOTO 500
      BK (I) = BK (NCALC) * BK (I)
      NCALC = I
430 enddo
  ENDIF
  500 RETURN
!---------- Last line of RKBESL ----------
END SUBROUTINE RKBESL

end module morespec
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module FRESNEL
use nano_deftyp
private
public :: FresnelC,FresnelS,FresnelF,FresnelG

real(DP),PARAMETER :: PID2 = 1.570796326794896619231321691639751442099d0, &
     RPI = 0.3183098861837906715377675267450287240689d0, &
     RPISQ = RPI * RPI, onep2=1.2d0,onep6=1.6d0,onep9=1.9d0,twop4=2.4d0,treq=three*unqua
 logical,save :: INIT_Fresnel=.false.
 ! PID2 is pi / 2.
 ! RPI is the reciprocal of PI.
 ! RPISQ is the reciprocal of PI squared.
 ! UNQUA is 0.25d0 or 1/4.
 ! TREQ is 0.75d0 or 3/4.

 interface FresnelC
   module procedure DFRENC
 end interface
 interface FresnelS
   module procedure DFRENS
 end interface
 interface FresnelF
   module procedure DFRENF
 end interface
 interface FresnelG
   module procedure DFRENG
 end interface


 contains
 !  File: dfrenl.[for|f|c]
 !  Contains procedures: DFRENC(), DFRENF(), DFRENG(), DFRENS()
 !  and private low-level procedure: DFREN1().
 !.  Copyright (C) 1992, California Institute of Technology.
 !.  U. S. Government sponsorship under
 !.  NASA contract NAS7-918 is acknowledged.
 !>> 1996-01-08 DFRENL WV Snyder Use DCSPXX for cos(Pi/2 x**2), etc.
 !>> 1995-11-03 DFRENL Krogh  Removed blanks in numbers for C conversion.
 !>> 1994-11-02 DFRENL Krogh  Changes to use M77CON
 !>> 1994-10-18 DFRENL WV Snyder More specializing instructions
 !>> 1993-02-25 DFRENL CLL. Edited to eliminate ENTRY and EQUIVALENCE.
 !>> 1992-09-15 DFRENL WV Snyder Specializing instructions
 !>> 1992-04-13 DFRENL WV Snyder Declare DFRENF, DFRENG, DFRENS
 !>> 1992-03-18 DFRENL WV Snyder Move declarations for coefficient arrays
 !>> 1992-01-24 DFRENL WV Snyder Original code
 !--D replaces "?": ?FRENC, ?FREN1, ?FRENF, ?FRENG, ?FRENS, ?FRENL
 !--&   ?CSPXX, ?SNPXX
 ! Subprograms in this file compute the Fresnel Cosine and Sine
 ! integrals C(x) and S(x), and the auxiliary functions f(x) and g(x),
 ! for any X:
 !DFRENC(X) for Fresnel integral C(X)
 !DFRENS(X) for Fresnel integral S(x)
 !DFRENF(X) for Fresnel integral auxiliary function f(x)
 !DFRENG(X) for Fresnel integral auxiliary function g(x).
 !
 ! Developed by W. V. Snyder, Jet Propulsion Laboratory, 24 January 1992.
 !
 ! Ref: W. J. Cody, "Chebyshev Approximations for the Fresnel Integrals",
 ! Mathematics of Computation, 1968, pp 450-453 plus Microfiche Suppl.
 ! W. V. Snyder, "Algorithm 723: Fresnel Integrals," ACM Trans. Math.
 ! Softw. 19, 4 (December 1993) 452-456.
 ! Accuracies of highest order formulae, where E is relative error:
 !
 ! Range     Function   -log10(E)   Function   -log10(E)
 ! |X|<=1.2    C(x) 16.24 S(x) 17.26
 ! 1.2<|X|<=1.6C(x) 17.47 S(x) 18.66
 ! 1.6<|X|<=1.9f(x) 17.13 g(x) 16.25
 ! 1.9<|X|<=2.4f(x) 16.64 g(x) 15.65
 ! 2.4<|X|     f(x) 16.89 g(x) 15.58
 !
 ! Refer to Cody for accuracy of other approximations.
 !
 !==================================================================
 real(DP) function DFRENC(X)
 real(DP),intent(IN) ::  X
 DFRENC = DFREN1(1, X)
 RETURN
 END FUNCTION DFRENC
 !==================================================================
 real(DP) function DFRENF(X)
 real(DP),intent(IN) ::  X
 DFRENF = DFREN1(3, X)
 RETURN
 END FUNCTION DFRENF
 !==================================================================
 real(DP) function DFRENG(X)
 real(DP),intent(IN) ::  X
 DFRENG = DFREN1(4, X)
 RETURN
 END FUNCTION DFRENG
 !==================================================================
 real(DP) function DFRENS(X)
 real(DP),intent(IN) ::  X
 DFRENS = DFREN1(2, X)
 RETURN
 END FUNCTION DFRENS
 !==================================================================
 real(DP) function DFREN1(MODE, X)
 INTEGER,intent(IN)  ::  MODE
 real(DP),intent(IN) ::  X
 !MODE = 1 means compute C.
 !MODE = 2 means compute S.
 !MODE = 3 means compute F.
 !MODE = 4 means compute G.
 !------------------------------------------------------------------
 !Internal variables.
 ! AX is abs(x).
 ! BIGX is 1/sqrt(round-off).  If X > BIGX then to the working
 !   precision x**2 is an integer (which we assume to be a multiple
 !   of four), so cos(pi/2 * x**2) = 1, and sin(pi/2 * x**2) = 0.
 ! C and S are values of C(x) and S(x), respectively.
 ! CX and SX are cos(pi/2 * ax**2) and sin(pi/2 * ax**2), respectively.
 ! F and G are used to compute f(x) and g(x) when X > 1.6.
 ! HAVEC, HAVEF, HAVEG, HAVES are logical variables that indicate
 !   whether the values stored in C, F, G and S correspond to the
 !   value stored in X.  HAVEF indicates we have both F and G when
 !   XSAVE  <=  1.6, and HAVEC indicates we have both C and S when
 !   XSAVE  >  1.6.
 ! LARGEF is 1/(pi * underflow).  If X > LARGEF then f ~ 0.
 ! LARGEG is cbrt(1/(pi**2 * underflow)).  If X > LARGEG then g ~ 0.
 ! LARGEX is 1/sqrt(sqrt(underflow)).  If X > LARGEX then f ~ 1/(pi * x)
 !   and g ~ 1/(pi**2 * x**3).
 ! MODE indicates the function to be computed: 1 = C(x), 2 = S(x),
 !   3 = f(x), 4 = g(x).
 ! NEEDC, NEEDF, NEEDG, NEEDS are arrays indexed by MODE (MODE+4 when
 !   X  >  1.6) that indicate what functions are needed.
 ! WANTC indicates whether C and S must be computed from F and G.
 ! WANTF and WANTG indicate we computed F and G on the present call.
 ! XSAVE is the most recently provided value of X.
 ! X4 is either X ** 4 or (1.0/X) ** 4.
 !If you change the order of approximation, you must change the
 !declarations and DATA statements for the coefficient arrays,
 !and the executable statements that evaluate the approximations.
 !------------------------------------------------------------------
 real(DP),parameter :: bigx = one / D1MACH(4), &
                       largef = rpi / D1MACH(1)
 real(DP) :: AX, CX
 real(DP) :: SX, X4
 real(DP),SAVE :: C, F, G, LARGEG, LARGEX, S, XSAVE
 LOGICAL,save :: HAVEC, HAVEF, HAVEG, HAVES
 LOGICAL :: WANTC, WANTF, WANTG
 LOGICAL :: NEEDC(8), NEEDF(8), NEEDG(8), NEEDS(8)
 real(DP) :: PC1(0:4), QC1(1:4)
 real(DP) :: PC2(0:5), QC2(1:5)
 real(DP) :: PS1(0:4), QS1(1:4)
 real(DP) :: PS2(0:5), QS2(1:5)
 real(DP) :: PF1(0:5), QF1(1:5)
 real(DP) :: PF2(0:5), QF2(1:5)
 real(DP) :: PF3(0:6), QF3(1:6)
 real(DP) :: PG1(0:5), QG1(1:5)
 real(DP) :: PG2(0:5), QG2(1:5)
 real(DP) :: PG3(0:6), QG3(1:6)
 DATA C / zero /, F / half /, G / half /, S / zero /, XSAVE / zero /
 DATA HAVEC / .TRUE. /, HAVEF / .TRUE. /, HAVEG / .TRUE. /,  HAVES / .TRUE. /
 !              C(x)     S(x)    f(x)     g(x)     C(x)     S(x)     f(x)     g(x)
 DATA NEEDC / .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .FALSE., .FALSE. /
 DATA NEEDS / .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .TRUE.,  .FALSE., .FALSE. /
 DATA NEEDF / .FALSE., .FALSE., .TRUE.,  .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE. /
 DATA NEEDG / .FALSE., .FALSE., .FALSE., .TRUE.,  .TRUE.,  .TRUE.,  .FALSE., .TRUE.  /
 !
 !Coefficients for C(x), |X| <= 1.2
 DATA pc1 / 9.999999999999999421d-1, -1.994608988261842706d-1,    &
 1.761939525434914045d-2, -5.280796513726226960d-4,   &
 5.477113856826871660d-6 /
 DATA qc1 / 4.727921120104532689d-2, 1.099572150256418851d-3,&
 1.552378852769941331d-5, 1.189389014228757184d-7 /
 !
 !Coefficients for C(x), 1.2 < |X| <= 1.6
 DATA pc2 / 1.00000000000111043640d0, -2.07073360335323894245d-1, &
 1.91870279431746926505d-2, -6.71376034694922109230d-4,     &
 1.02365435056105864908d-5, -5.68293310121870728343d-8 /
 DATA qc2 / 3.96667496952323433510d-2, 7.88905245052359907842d-4,  &
 1.01344630866749406081d-5, 8.77945377892369265356d-8, &
 4.41701374065009620393d-10 /
 !
 !Coefficients for S(x), |X| <= 1.2
 DATA ps1 / 5.2359877559829887021d-1, -7.0748991514452302596d-2,  &
 3.8778212346368287939d-3, -8.4555728435277680591d-5, &
 6.7174846662514086196d-7 /
 DATA qs1 / 4.1122315114238422205d-2, 8.1709194215213447204d-4,    &
 9.6269087593903403370d-6, 5.9528122767840998345d-8 /
 !
 !coefficients for S(x), 1.2 < |X| <= 1.6
 DATA ps2 / 5.23598775598344165913d-1, -7.37766914010191323867d-2,&
 4.30730526504366510217d-3, -1.09540023911434994566d-4,     &
 1.28531043742724820610d-6, -5.76765815593088804567d-9 /
 DATA qs2 / 3.53398342767472162540d-2, 6.18224620195473216538d-4,  &
 6.87086265718620117905d-6, 5.03090581246612375866d-8, &
 2.05539124458579596075d-10 /
 !
 !coefficients for f(x), 1.6 < |X| <= 1.9
 DATA pf1 / 3.1830975293580985290d-1, 1.2226000551672961219d1,     &
 1.2924886131901657025d2, 4.3886367156695547655d2,     &
 4.1466722177958961672d2, 5.6771463664185116454d1 /
 DATA qf1 / 3.8713003365583442831d1, 4.1674359830705629745d2,&
 1.4740030733966610568d3, 1.5371675584895759916d3,     &
 2.9113088788847831515d2 /
 !coefficients for f(x), 1.9 < |X| <= 2.4
 DATA pf2 / 3.183098818220169217d-1, 1.958839410219691002d1, &
 3.398371349269842400d2, 1.930076407867157531d3, &
 3.091451615744296552d3, 7.177032493651399590d2 /
 DATA qf2 / 6.184271381728873709d1, 1.085350675006501251d3,  &
 6.337471558511437898d3, 1.093342489888087888d4, &
 3.361216991805511494d3 /
 !
 !coefficients for f(x), 2.4 < |X|
 DATA pf3 / -9.675460329952532343d-2, -2.431275407194161683d1,   &
 -1.947621998306889176d3, -6.059852197160773639d4, -7.076806952837779823d5, -2.417656749061154155d6, &
 -7.834914590078317336d5 /
 DATA qf3 / 2.548289012949732752d2, 2.099761536857815105d4,  &
 6.924122509827708985d5, 9.178823229918143780d6, &
 4.292733255630186679d7, 4.803294784260528342d7 /
 !++   end
 !
 !coefficients for g(x), 1.6 < |X| <= 1.9
 DATA pg1 / 1.013206188102747985d-1, 4.445338275505123778d0, &
 5.311228134809894481d1, 1.991828186789025318d2, &
 1.962320379716626191d2, 2.054214324985006303d1 /
 DATA qg1 / 4.539250196736893605d1, 5.835905757164290666d2,  &
 2.544731331818221034d3, 3.481121478565452837d3, &
 1.013794833960028555d3 /
 !
 !coefficients for g(x), 1.9 < |X| <= 2.4
 DATA pg2 / 1.01321161761804586d-1, 7.11205001789782823d0,   &
 1.40959617911315524d2, 9.08311749529593938d2,   &
 1.59268006085353864d3, 3.13330163068755950d2 /
 DATA qg2 / 7.17128596939302198d1, 1.49051922797329229d3,    &
 1.06729678030580897d4, 2.41315567213369742d4,   &
 1.15149832376260604d4 /
 !
 !coefficients for g(x), 2.4 < |X|
 DATA pg3 / -1.53989733819769316d-1, -4.31710157823357568d1,     &
 -3.87754141746378493d3, -1.35678867813756347d5, -1.77758950838029676d6, -6.66907061668636416d6, &
 -1.72590224654836845d6 /
 DATA qg3 / 2.86733194975899483d2, 2.69183180396242536d4,    &
 1.02878693056687506d6, 1.62095600500231646d7,   &
 9.38695862531635179d7, 1.40622441123580005d8 /
 !------------------------------------------------------------------
 IF (.not.INIT_Fresnel) then
   largeg = (rpi * largef)**unter
   largex = one / sqrt(sqrt(D1MACH(1) ) )
 ENDIF
 IF (x /= xsave) then
   havec = .false.
   havef = .false.
   haveg = .false.
   haves = .false.
 ENDIF
 ax = abs(x)
 IF (ax <= onep6) then
   x4 = ax**4
   IF (needc(mode) .and.(.not.havec)) then
     IF (ax <= onep2) then
       c = x * ( ( ( (pc1(4) * x4 + pc1(3) ) * x4 + pc1(2) )    &
       * x4 + pc1(1) ) * x4 + pc1(0) ) / ( ( ( (qc1(4) * x4 +   &
       qc1(3) ) * x4 + qc1(2) ) * x4 + qc1(1) ) * x4 + one)
     ELSE
       c = x * ( ( ( ( (pc2(5) * x4 + pc2(4) ) * x4 + pc2(3) )  &
       * x4 + pc2(2) ) * x4 + pc2(1) ) * x4 + pc2(0) ) /  &
       ( ( ( ( (qc2(5) * x4 + qc2(4) ) * x4 + qc2(3) ) * x4 +   &
       qc2(2) ) * x4 + qc2(1) ) * x4 + one)
     ENDIF
     havec = .true.
   ENDIF
   IF (needs(mode) .and.(.not.haves)) then
     IF (ax <= onep2) then
       s = x**3 * ( ( ( (ps1(4) * x4 + ps1(3) ) * x4 + ps1(2) ) &
       * x4 + ps1(1) ) * x4 + ps1(0) ) / ( ( ( (qs1(4) * x4 +   &
       qs1(3) ) * x4 + qs1(2) ) * x4 + qs1(1) ) * x4 + one)
     ELSE
       s = x**3 * ( ( ( ( (ps2(5) * x4 + ps2(4) ) * x4 + ps2(3) &
       ) * x4 + ps2(2) ) * x4 + ps2(1) ) * x4 + ps2(0) )  &
       / ( ( ( ( (qs2(5) * x4 + qs2(4) ) * x4 + qs2(3) )  &
       * x4 + qs2(2) ) * x4 + qs2(1) ) * x4 + one)
     ENDIF
     haves = .true.
   ENDIF
   IF ( (needf(mode) .or.needg(mode) ) .and.(.not.havef)) then
     cx = dcspxx(ax)
     sx = dsnpxx(ax)
     f = (half - s) * cx - (half - c) * sx
     g = (half - c) * cx + (half - s) * sx
     havef = .true.
   ENDIF
 ELSE
   IF (ax <= largex) then
     x4 = (one / ax)**4
     wantf = (needf(mode+4) .and.(.not.havef))
     IF (wantf) then
       IF (ax <= onep9) then
         f = ( ( ( ( (pf1(5) * x4 + pf1(4) ) * x4 + pf1(3) )    &
         * x4 + pf1(2) ) * x4 + pf1(1) ) * x4 + pf1(0) )  &
         / ( ( ( ( ( (qf1(5) * x4 + qf1(4) ) * x4 + qf1(3) )    &
         * x4 + qf1(2) ) * x4 + qf1(1) ) * x4 + one) * ax)
       ELSEIF (ax <= twop4) then
         f = ( ( ( ( (pf2(5) * x4 + pf2(4) ) * x4 + pf2(3) )    &
         * x4 + pf2(2) ) * x4 + pf2(1) ) * x4 + pf2(0) )  &
         / ( ( ( ( ( (qf2(5) * x4 + qf2(4) ) * x4 + qf2(3) )    &
         * x4 + qf2(2) ) * x4 + qf2(1) ) * x4 + one) * ax)
       ELSE
         f = (rpi + x4 * ( ( ( ( ( (pf3(6) * x4 + pf3(5) ) &
         * x4 + pf3(4) ) * x4 + pf3(3) ) * x4 + pf3(2) )  &
         * x4 + pf3(1) ) * x4 + pf3(0) ) /( ( ( ( ( (qf3(6)    &
         * x4 + qf3(5) ) * x4 + qf3(4) ) * x4 + qf3(3) )  &
         * x4 + qf3(2) ) * x4 + qf3(1) ) * x4 + one) ) / ax
       ENDIF
       havef = .true.
     ENDIF
     wantg = (needg(mode+4) .and. (.not.haveg))
     IF (wantg) then
       IF (ax <= onep9) then
         g = ( ( ( ( (pg1(5) * x4 + pg1(4) ) * x4 + pg1(3) )    &
         * x4 + pg1(2) ) * x4 + pg1(1) ) * x4 + pg1(0) )  &
         / ( ( ( ( ( (qg1(5) * x4 + qg1(4) ) * x4 + qg1(3) )    &
         * x4 + qg1(2) ) * x4 + qg1(1) ) * x4 + one) * ax**3)
       ELSEIF (ax <= twop4) then
         g = ( ( ( ( (pg2(5) * x4 + pg2(4) ) * x4 + pg2(3) )    &
         * x4 + pg2(2) ) * x4 + pg2(1) ) * x4 + pg2(0) )  &
         / ( ( ( ( ( (qg2(5) * x4 + qg2(4) ) * x4 + qg2(3) )    &
         * x4 + qg2(2) ) * x4 + qg2(1) ) * x4 + one) * ax**3)
       ELSE
         g = (rpisq + x4 * ( ( ( ( ( (pg3(6) * x4 + pg3(5) )     &
         * x4 + pg3(4) ) * x4 + pg3(3) ) * x4 + pg3(2) )  &
         * x4 + pg3(1) ) * x4 + pg3(0) ) / ( ( ( ( ( (qg3(6)    &
         * x4 + qg3(5) ) * x4 + qg3(4) ) * x4 + qg3(3) )  &
         * x4 + qg3(2) ) * x4 + qg3(1) ) * x4 + one) ) / ax**3
       ENDIF
       haveg = .true.
     ENDIF
   ELSE
     wantf = needf(mode)
     IF (wantf) then
       IF (x <= largef) then
         f = rpi / ax
       ELSE
         f = zero
       ENDIF
     ENDIF
     wantg = needg(mode)
     IF (wantg) then
       IF (x <= largeg) then
         g = rpisq / ax**3
       ELSE
         g = zero
       ENDIF
     ENDIF
   ENDIF
   wantc = ((needc(mode+4) .or.needs(mode+4) ) .and.(.not.havec))
   IF (wantc.or.x < zero) then
     cx = dcspxx(ax)
     sx = dsnpxx(ax)
     IF (wantc) then
       c = half + f * sx - g * cx
       s = half - f * cx - g * sx
       IF (x < zero) then
         c = - c
         s = - s
       ENDIF
       havec = .true.
     ENDIF
     IF (x<zero) then
 !  We COULD do the following before the preceeding, and then
 !  not put in a test in the preceeding for x  <  0, but
 !  even though the result_Is are mathematically identical, we
 !  would have some cancellation above if we did so.
       IF (wantg) g = cx + sx - g
       IF (wantf) f = cx - sx - f
     ENDIF
   ENDIF
 ENDIF
 xsave = x
 select case(MODE)
 case(1)
   DFREN1 = c
 case(2)
   DFREN1 = s
 case(3)
   DFREN1 = f
 case(4)
   DFREN1 = g
 end select
 END FUNCTION DFREN1
 real(DP) FUNCTION DCOSPX(X)
 real(DP),intent(IN) :: X
 !>> 1996-01-29 DCOSPX WVS JPL Add better acknowledgement for origins.
 !>> 1994-10-20 DCOSPX Krogh  Changes to use M77CON
 !>> 1994-04-22 DCOSPX CLL Made SP and DP codes similar.
 !>> 1993-05-06 DCOSPX WVS JPL Convert from NSWC to Math 77
 ! ----------------------------------------------------------------------
 ! This procedure was originally procedure COS1 from the Naval Surface
 ! Warfare Center library.
 ! ----------------------------------------------------------------------
 !
 ! EVALUATION OF COS(PI*X)
 !
 !--------------
 !
 !THE EXPANSION FOR SIN(PI*A) (ABS(A)  <=  1/4) USING A1,...,A13
 !IS ACCURATE TO WITHIN 2 UNITS OF THE 40-TH SIGNIFICANT DIGIT, AND
 !THE EXPANSION FOR COS(PI*A) (ABS(A)  <=  1/4) USING B1,...,B13
 !IS ACCURATE TO WITHIN 4 UNITS OF THE 40-TH SIGNIFICANT DIGIT.
 !
 !The polynomials using coefficients SA0, ... SA6 and SB1, ..., SB6
 !give approximations whose largest observed relative error in the
 !relevant intervals is 0.129d-14.
 !We will use this latter approximation when the machine epsilon
 !is larger than 0.2d-14.
 ! ----------------------------------------------------------------------
 !--D replaces "?": ?COSPX, ?ERM1
 ! ----------------------------------------------------------------------
 INTEGER :: N
 real(DP),parameter :: BIG = one/D1MACH(3) , EPS = D1MACH(3)
 real(DP),PARAMETER :: CUTOFF = 0.2d-14
 real(DP) :: A, T, W
 real(DP) :: A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13
 real(DP) :: B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13
 real(DP) :: SA0, SA1, SA2, SA3, SA4, SA5, SA6
 real(DP) :: SB1, SB2, SB3, SB4, SB5, SB6
 ! -----------------------
 DATA SA0 / .314159265358979D+01 /, SA1 / -.516771278004995D+01 /,&
 SA2 / .255016403987327D+01 /, SA3 / -.599264528932149D+00 /,     &
 SA4 / .821458689493251D-01 /, SA5 / -.737001831310553D-02 /,     &
 SA6 / .461514425296398D-03 /
 DATA SB1 / -.493480220054460D+01 /, SB2 / .405871212639605D+01 /,&
 SB3 / -.133526276691575D+01 /, SB4 / .235330543508553D+00 /,     &
 SB5 / -.258048861575714D-01 /, SB6 / .190653140279462D-02 /
 DATA A1 / -.1028083791780141522795259479153765743002D+00 /,&
 A2 / .3170868848763100170457042079710451905600D-02 /, A3 /  &
 -.4657026956105571623449026167864697920000D-04 /, A4 /     &
 .3989844942879455643410226655783424000000D-06 /, A5 / -.2237397227721999776371894030796800000000D-08 /, &
 A6 / .8847045483056962709715066675200000000000D-11 /, A7 / -.2598715447506450292885585920000000000000D-13 /, &
 A8 / .5893449774331011070033920000000000000000D-16 /, A9 / -.1062975472045522550784000000000000000000D-18 /, &
 A10 /.1561182648301780992000000000000000000000D-21 /, A11 / -.1903193516670976000000000000000000000000D-24 /, &
 A12 /.1956617650176000000000000000000000000000D-27 /, A13 / -.1711276032000000000000000000000000000000D-30 /
 ! -----------------------
 DATA B1 / -.3084251375340424568385778437461297229882D+00 /,&
 B2 / .1585434424381550085228521039855226435920D-01 /, B3 /  &
 -.3259918869273900136414318317506279360000D-03 /, B4 /     &
 .3590860448591510079069203991239232000000D-05 /, B5 / -.2461136950494199754009084061808640000000D-07 /, B6 / &
 .1150115912797405152263195572224000000000D-09 /, B7 / -.3898073171259675439899172864000000000000D-12 /, B8 / &
 .1001886461636271969091584000000000000000D-14 /, B9 / -.2019653396886572027084800000000000000000D-17 /, B10 /&
 .3278483561466560512000000000000000000000D-20 /, B11 / -.4377345082051788800000000000000000000000D-23 /, B12 /&
 .4891532381388800000000000000000000000000D-26 /, B13 / -.4617089843200000000000000000000000000000D-29 /
 ! -----------------------
 A = ABS(X)
 IF (A >= BIG) THEN
   DCOSPX = cos(Pi*modulo(X,two))
   RETURN
 ENDIF
 N = int(A)
 T = real(N,DP)
 A = A - T
 IF (A > treq) GOTO 20
 IF (A < unqua) GOTO 21
 !
 !  0.25  <=  A  <=  0.75
 !
 A = unqua + (unqua - A)
 IF (eps<cutoff) then
   T = sixteen * A * A
   W = ( ( ( ( ( ( ( ( ( ( ( ( (A13 * T + A12) * T + A11) * T +    &
   A10) * T + A9) * T + A8) * T + A7) * T + A6) * T + A5) * T + A4)&
   * T + A3) * T + A2) * T + A1) * T + half) + half
   DCOSPX = PI * A * W
 ELSE
   T = A * A
   DCOSPX = ( ( ( ( ( (SA6 * T + SA5) * T + SA4) * T + SA3)  &
   * T + SA2) * T + SA1) * T + SA0) * A
 ENDIF
 GOTO 30
 !
 !A  <  0.25  OR  A  >  0.75
 !
 20 A = unqua + (treq - A)
 N = N - 1
 21 CONTINUE
 IF (eps<cutoff) then
   T = sixteen * A * A
   DCOSPX = ( ( ( ( ( ( ( ( ( ( ( ( (B13 * T + B12) * T + B11)     &
   * T + B10) * T + B9) * T + B8) * T + B7) * T + B6) * T + B5)    &
   * T + B4) * T + B3) * T + B2) * T + B1) * T + half) + half
 ELSE
   T = A * A
   DCOSPX = ( ( ( ( ( (SB6 * T + SB5) * T + SB4) * T + SB3)  &
   * T + SB2) * T + SB1) * T + half) + half
 ENDIF
 !
 !TERMINATION
 !
 30 CONTINUE
 IF (modulo(N, 2)  /= 0) DCOSPX = - DCOSPX
 RETURN
 END FUNCTION DCOSPX
 !--D replaces "?": ?CSPXX, ?COSPX, ?SINPX
 !>> 1996-01-08 DCSPXX WV Snyder Original code
 !======================================================================
 real(DP) function DCSPXX(X)
 real(DP),intent(IN) :: X
 ! COS(PI * X * X / 2) carefully to avoid loss of precision for large X
 ! DCOSPX is used to compute COS(PI * X)
 ! BIGX = 1 / round-off = biggest integer exactly representable by F.P.
 !    If X > BIGX then to the working precision x**2 is an integer (which
 !    we assume to be a multiple of four), so cos(pi/2 * x**2) = 1.
 ! N = [X], and later [K F]
 ! F = X - N = fractional part of X
 ! K = [ N / 2 ]
 ! J = N mod 2
 ! M = [K F]
 ! G = K F - M = fractional part of K F

 INTEGER :: J, K, N
 real(DP),parameter :: BIGX = one / d1mach(4)
 real(DP) :: F, G

 f = abs(x)
 IF (f > bigx) then
 ! Assume x is an even integer.
   dcspxx = one
   RETURN
 ENDIF
 n = INT(f)
 f = f - real(n,DP)
 k = n / 2
 j = modulo(n, 2)
 g = k * f
 n = INT(g)
 g = g - real(n,DP)
 IF (j==0) then
   dcspxx = dcospx(half * f * f + g + g)
 ELSE
   dcspxx = -dsinpx(half * f * f + f + g + g)
 ENDIF
 RETURN
 END FUNCTION DCSPXX
 !=======================================================================
 real(DP) FUNCTION DSINPX(X)
 real(DP),intent(IN) :: X
 !>> 1996-01-29 SCOSPX WVS JPL Add better acknowledgement for origins.
 !>> 1994-10-20 DSINPX Krogh  Changes to use M77CON
 !>> 1994-04-22 DSINPX CLL Made SP and DP codes similar.
 !>> 1993-05-06 DSINPX WVS JPL Convert from NSWC to Math 77
 !--D replaces "?": ?SINPX, ?ERM1
 ! ----------------------------------------------------------------------
 ! This procedure was originally procedure SIN1 from the Naval Surface
 ! Warfare Center library.
 ! ----------------------------------------------------------------------
 !
 !EVALUATION OF SIN(PI*X)
 !
 !--------------
 !
 !THE EXPANSION FOR SIN(PI*A) (ABS(A)  <=  1/4) USING A1,...,A13
 !IS ACCURATE TO WITHIN 2 UNITS OF THE 40-TH SIGNIFICANT DIGIT, AND
 !THE EXPANSION FOR COS(PI*A) (ABS(A)  <=  1/4) USING B1,...,B13
 !IS ACCURATE TO WITHIN 4 UNITS OF THE 40-TH SIGNIFICANT DIGIT.
 !
 !The polynomials using coefficients SA0, ... SA6 and SB1, ..., SB6
 !give approximations whose largest observed relative error in the
 !relevant intervals is 0.129d-14.
 !We will use this latter approximation when the machine epsilon
 !is larger than 0.2d-14.
 !-----------------------------------------------------------------------
 INTEGER :: N
 real(DP) :: A, T, W
 real(DP) :: A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12,A13
 real(DP) :: B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12,B13
 real(DP) :: SA0, SA1, SA2, SA3, SA4, SA5, SA6
 real(DP) :: SB1, SB2, SB3, SB4, SB5, SB6
 real(DP),PARAMETER :: BIG=one/D1MACH(3), EPS=D1MACH(3)
 !------------------------
 real(DP),PARAMETER :: CUTOFF = 0.2d-14
 !------------------------
 DATA SA0 / .314159265358979D+01 /, SA1 / -.516771278004995D+01 /,&
      SA2 / .255016403987327D+01 /, SA3 / -.599264528932149D+00 /,     &
      SA4 / .821458689493251D-01 /, SA5 / -.737001831310553D-02 /,     &
      SA6 / .461514425296398D-03 /
 DATA SB1 / -.493480220054460D+01 /, SB2 / .405871212639605D+01 /,&
      SB3 / -.133526276691575D+01 /, SB4 / .235330543508553D+00 /,     &
      SB5 / -.258048861575714D-01 /, SB6 / .190653140279462D-02 /
 DATA A1 / -.1028083791780141522795259479153765743002D+00 /,&
      A2 / .3170868848763100170457042079710451905600D-02 /, &
      A3 /  -.4657026956105571623449026167864697920000D-04 /, &
      A4 / .3989844942879455643410226655783424000000D-06 /, A5 / -.2237397227721999776371894030796800000000D-08 /, &
      A6 / .8847045483056962709715066675200000000000D-11 /, A7 / -.2598715447506450292885585920000000000000D-13 /, &
      A8 / .5893449774331011070033920000000000000000D-16 /, A9 / -.1062975472045522550784000000000000000000D-18 /, &
      A10 /.1561182648301780992000000000000000000000D-21 /, A11 / -.1903193516670976000000000000000000000000D-24 /, &
      A12 /.1956617650176000000000000000000000000000D-27 /, A13 / -.1711276032000000000000000000000000000000D-30 /
 !------------------------
 DATA B1 / -.3084251375340424568385778437461297229882D+00 /,&
 B2 / .1585434424381550085228521039855226435920D-01 /, B3 /  &
 -.3259918869273900136414318317506279360000D-03 /, B4 /     &
 .3590860448591510079069203991239232000000D-05 /, B5 / -.2461136950494199754009084061808640000000D-07 /, B6 / &
 .1150115912797405152263195572224000000000D-09 /, B7 / -.3898073171259675439899172864000000000000D-12 /, B8 / &
 .1001886461636271969091584000000000000000D-14 /, B9 / -.2019653396886572027084800000000000000000D-17 /, B10 /&
 .3278483561466560512000000000000000000000D-20 /, B11 / -.4377345082051788800000000000000000000000D-23 /, B12 /&
 .4891532381388800000000000000000000000000D-26 /, B13 / -.4617089843200000000000000000000000000000D-29 /
 !------------------------
 A = ABS(X)
 IF (A >= BIG) THEN
   DSINPX = sin(Pi*modulo(X,two))
   RETURN
 ENDIF
 N = INT(A)
 T = real(N,DP)
 A = A - T
 IF (A > treq) GOTO 20
 IF (A < unqua) GOTO 21
 !
 !  0.25  <=  A  <=  0.75
 !
 A = unqua + (unqua - A)
 IF (eps < cutoff) then
   T = sixteen * A * A
   DSINPX = ( ( ( ( ( ( ( ( ( ( ( ( (B13 * T + B12) * T + B11)     &
   * T + B10) * T + B9) * T + B8) * T + B7) * T + B6) * T + B5)    &
   * T + B4) * T + B3) * T + B2) * T + B1) * T + half) + half
 ELSE
   T = A * A
   DSINPX = ( ( ( ( ( (SB6 * T + SB5) * T + SB4) * T + SB3)  &
   * T + SB2) * T + SB1) * T + half) + half
 ENDIF
 GOTO 30
 !
 !A  <  0.25  OR  A  >  0.75
 !
    20 A = unqua + (treq - A)
    21 CONTINUE
 IF (eps < cutoff) then
   T = sixteen * A * A
   W = ( ( ( ( ( ( ( ( ( ( ( ( (A13 * T + A12) * T + A11) * T +    &
   A10) * T + A9) * T + A8) * T + A7) * T + A6) * T + A5) * T + A4)&
   * T + A3) * T + A2) * T + A1) * T + half) + half
   DSINPX = PI * A * W
 ELSE
   T = A * A
   DSINPX = ( ( ( ( ( (SA6 * T + SA5) * T + SA4) * T + SA3)  &
   * T + SA2) * T + SA1) * T + SA0) * A
 ENDIF
 !
 !TERMINATION
 !
    30 IF (X < zero) DSINPX = - DSINPX
   IF (mod(N, 2)  /= 0) DSINPX = - DSINPX
 RETURN
 END FUNCTION DSINPX
 !--D replaces "?": ?SNPXX, ?COSPX, ?SINPX
 !>> 1996-01-08 DSNPXX WV Snyder Original code
 !====================================================
 real(DP) function DSNPXX(X)
 real(DP),intent(IN) :: X
 !
 ! SIN(PI * X * X / 2) carefully to avoid loss of precision for large X
 ! DSINPX is used to compute SIN(PI * X)
 ! BIGX = 1 / round-off = biggest integer exactly representable by F.P.
 !    If X > BIGX then to the working precision x**2 is an integer (which
 !    we assume to be a multiple of four), so sin(pi/2 * x**2) = 0.
 ! N = [X], and later [K F]
 ! F = X - N = fractional part of X
 ! K = [ N / 2 ]
 ! J = N mod 2
 ! G = K F - M = fractional part of K F

 INTEGER :: J, K, N
 real(DP) :: F, G
 real(DP),parameter :: BIGX = one / d1mach(4)

 f = abs(x)
 IF (f > bigx) then
 ! Assume x is an even integer.
   dsnpxx = zero
   RETURN
 ENDIF
 n = INT(f)
 f = f - real(n,DP)
 k = n / 2
 j = modulo(n, 2)
 g = k * f
 n = INT(g)
 g = g - real(n,DP)
 IF (j == 0) then
   dsnpxx = dsinpx(half * f * f + g + g)
 ELSE
   dsnpxx = dcospx(half * f * f + f + g + g)
 ENDIF
 RETURN
 END FUNCTION DSNPXX
 end module FRESNEL
 !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 module miscfun_testval
 use nano_deftyp
 private
 public :: number_testvalues,lenam,numsub,nasu,testval, &
abram0_values, debye1_values, struve_l1_values, abram1_values, debye2_values, synch1_values, &
abram2_values, debye3_values, synch2_values, airy_ai_int_values, debye4_values, tran02_values, &
airy_bi_int_values, exp3_int_values, tran03_values, airy_gi_values, goodwin_values, tran04_values, &
airy_hi_values, i0ml0_values, tran05_values, arctan_int_values, i1ml1_values, tran06_values, &
bessel_i0_int_values, lobachevsky_values, tran07_values, bessel_j0_int_values, stromgen_values, &
tran08_values, bessel_k0_int_values, struve_h0_values, tran09_values, bessel_y0_int_values, &
struve_h1_values, clausen_values, struve_l0_values

 integer, parameter :: number_testvalues = 20
 integer, parameter :: numsub=37,lenam=19
 real(DP),dimension(3,number_testvalues,numsub),save :: testval
 character(lenam),dimension(numsub),parameter :: nasu = (/&
 'abram0             ', &
 'abram1             ', &
 'abram2             ', &
 'airy_ai_integral   ', &
 'airy_bi_integral   ', &
 'airy_gi            ', &
 'airy_hi            ', &
 'arctan_integral    ', &
 'bessel_i0_integral ', &
 'bessel_j0_integral ', &
 'bessel_k0_integral ', &
 'bessel_y0_integral ', &
 'clausen            ', &
 'debye1             ', &
 'debye2             ', &
 'debye3             ', &
 'debye4             ', &
 'exp3_integral      ', &
 'goodwin            ', &
 'i0ml0              ', &
 'i1ml1              ', &
 'lobachevsky        ', &
 'stromgen           ', &
 'struve_h0          ', &
 'struve_h1          ', &
 'struve_l0          ', &
 'struve_l1          ', &
 'synch1             ', &
 'synch2             ', &
 'tran02             ', &
 'tran03             ', &
 'tran04             ', &
 'tran05             ', &
 'tran06             ', &
 'tran07             ', &
 'tran08             ', &
 'tran09             '/)

 contains

 subroutine abram0_values( n_data, x, fx )
 !*****************************************************************************80
 !
 !! ABRAM0_VALUES returns some values of the Abramowitz0 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      ABRAM0(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 - x / t ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    21 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, real(DP) X, the argument of the function.
 !
 !    Output, real(DP) FX, the value of the function.
 !
   implicit none


   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.87377726306985360531D+00, &
            0.84721859650456925922D+00, &
            0.77288934483988301615D+00, &
            0.59684345853450151603D+00, &
            0.29871735283675888392D+00, &
            0.15004596450516388138D+00, &
            0.11114662419157955096D+00, &
            0.83909567153151897766D-01, &
            0.56552321717943417515D-01, &
            0.49876496603033790206D-01, &
            0.44100889219762791328D-01, &
            0.19738535180254062496D-01, &
            0.86193088287161479900D-02, &
            0.40224788162540127227D-02, &
            0.19718658458164884826D-02, &
            0.10045868340133538505D-02, &
            0.15726917263304498649D-03, &
            0.10352666912350263437D-04, &
            0.91229759190956745069D-06, &
            0.25628287737952698742D-09 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
            0.0019531250D+00, &
            0.0078125000D+00, &
            0.0312500000D+00, &
            0.1250000000D+00, &
            0.5000000000D+00, &
            1.0000000000D+00, &
            1.2500000000D+00, &
            1.5000000000D+00, &
            1.8750000000D+00, &
            2.0000000000D+00, &
            2.1250000000D+00, &
            3.0000000000D+00, &
            4.0000000000D+00, &
            5.0000000000D+00, &
            6.0000000000D+00, &
            7.0000000000D+00, &
            10.0000000000D+00, &
            15.0000000000D+00, &
            20.0000000000D+00, &
            40.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine abram0_values
 subroutine debye1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! DEBYE1_VALUES returns some values of Debye's function of order 1.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      DEBYE1(x) = 1 / x * Integral ( 0 <= t <= x ) t / ( exp ( t ) - 1 ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    27 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.99951182471380889183D+00, &
             0.99221462647120597836D+00, &
             0.96918395997895308324D+00, &
             0.88192715679060552968D+00, &
             0.77750463411224827642D+00, &
             0.68614531078940204342D+00, &
             0.60694728460981007205D+00, &
             0.53878956907785587703D+00, &
             0.48043521957304283829D+00, &
             0.38814802129793784501D+00, &
             0.36930802829242526815D+00, &
             0.32087619770014612104D+00, &
             0.29423996623154246701D+00, &
             0.27126046678502189985D+00, &
             0.20523930310221503723D+00, &
             0.16444346567994602563D+00, &
             0.10966194482735821276D+00, &
             0.82246701178200016086D-01, &
             0.54831135561510852445D-01, &
             0.32898681336964528729D-01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine debye1_values
 subroutine struve_l1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! STRUVE_L1_VALUES returns some values of the Struve L1 function.
 !
 !  Discussion:
 !
 !    In Mathematica, the function can be evaluated by:
 !
 !      StruveL[1,x]
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.80950410749865126939d-06, &
            0.20724649092571514607d-03, &
            0.33191834066894516744d-02, &
            0.53942182623522663292d-01, &
            0.22676438105580863683d+00, &
            0.11027597873677158176d+01, &
            0.91692778117386847344d+01, &
            0.15541656652426660966d+03, &
            0.26703582852084829694d+04, &
            865058.801753046339064484069746417254d0,&
            0.11026046613094942620d+07, &
            0.22846209494153934787d+07, &
            0.42454972750111979449d+08, &
            0.48869614587997695539d+09, &
            0.56578651292431051863d+10, &
            0.76853203893832108948d+12, &
            0.14707396163259352103d+17, &
            0.29030785901035567967d+21, &
            0.58447515883904682813d+25, &
            0.11929750788892311875d+30 /)

! value at 16.0 entry 10 as read:
!            0.86505880175304633906d+06, &
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
             -0.0312500000D+00, &
              0.1250000000D+00, &
             -0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
             -4.0000000000D+00, &
              7.0000000000D+00, &
            -10.0000000000D+00, &
             16.0000000000D+00, &
             16.2500000000D+00, &
            -17.0000000000D+00, &
             20.0000000000D+00, &
             22.5000000000D+00, &
            -25.0000000000D+00, &
             30.0000000000D+00, &
            -40.0000000000D+00, &
             50.0000000000D+00, &
             60.0000000000D+00, &
            -70.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine struve_l1_values
 subroutine abram1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! ABRAM1_VALUES returns some values of the Abramowitz1 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      ABRAM1(x) = Integral ( 0 <= t < infinity ) t * exp ( -t^2 - x / t ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    21 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.49828219848799921792D+00, &
            0.49324391773047288556D+00, &
            0.47431612784691234649D+00, &
            0.41095983258760410149D+00, &
            0.25317617388227035867D+00, &
            0.14656338138597777543D+00, &
            0.11421547056018366587D+00, &
            0.90026307383483764795D-01, &
            0.64088214170742303375D-01, &
            0.57446614314166191085D-01, &
            0.51581624564800730959D-01, &
            0.25263719555776416016D-01, &
            0.11930803330196594536D-01, &
            0.59270542280915272465D-02, &
            0.30609215358017829567D-02, &
            0.16307382136979552833D-02, &
            0.28371851916959455295D-03, &
            0.21122150121323238154D-04, &
            0.20344578892601627337D-05, &
            0.71116517236209642290D-09 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
            0.0019531250D+00, &
            0.0078125000D+00, &
            0.0312500000D+00, &
            0.1250000000D+00, &
            0.5000000000D+00, &
            1.0000000000D+00, &
            1.2500000000D+00, &
            1.5000000000D+00, &
            1.8750000000D+00, &
            2.0000000000D+00, &
            2.1250000000D+00, &
            3.0000000000D+00, &
            4.0000000000D+00, &
            5.0000000000D+00, &
            6.0000000000D+00, &
            7.0000000000D+00, &
            10.0000000000D+00, &
            15.0000000000D+00, &
            20.0000000000D+00, &
            40.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine abram1_values
 subroutine debye2_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! DEBYE2_VALUES returns some values of Debye's function of order 2.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      DEBYE2(x) = 2 / x^2 * Integral ( 0 <= t <= x ) t^2 / ( exp ( t ) - 1 ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    27 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.99934911727904599738D+00, &
             0.98962402299599181205D+00, &
             0.95898426200345986743D+00, &
             0.84372119334725358934D+00, &
             0.70787847562782928288D+00, &
             0.59149637225671282917D+00, &
             0.49308264399053185014D+00, &
             0.41079413579749669069D+00, &
             0.34261396060786351671D+00, &
             0.24055368752127897660D+00, &
             0.22082770061202308232D+00, &
             0.17232915939014138975D+00, &
             0.14724346738730182894D+00, &
             0.12666919046715789982D+00, &
             0.74268805954862854626D-01, &
             0.47971498020121871622D-01, &
             0.21369201683658373846D-01, &
             0.12020564476446432799D-01, &
             0.53424751249537071952D-02, &
             0.19232910450553508562D-02 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine debye2_values
 subroutine synch1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! SYNCH1_VALUES returns some values of the synchrotron radiation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      SYNCH1(x) = x * Integral ( x <= t < infinity ) K(5/3)(t) dt
 !
 !    where K(5/3) is a modified Bessel function of order 5/3.
 !
 !  Modified:
 !
 !    05 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
              0.26514864547487397044D+00, &
              0.62050129979079045645D+00, &
              0.85112572132368011206D+00, &
              0.87081914687546885094D+00, &
              0.65142281535536396975D+00, &
              0.45064040920322354579D+00, &
              0.30163590285073940285D+00, &
              0.19814490804441305867D+00, &
              0.12856571000906381300D+00, &
              0.52827396697866818297D-01, &
              0.42139298471720305542D-01, &
              0.21248129774981984268D-01, &
              0.13400258907505536491D-01, &
              0.84260797314108699935D-02, &
              0.12884516186754671469D-02, &
              0.19223826430086897418D-03, &
              0.28221070834007689394D-04, &
              0.15548757973038189372D-05, &
              0.11968634456097453636D-07, &
              0.89564246772237127742D-10 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             25.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine synch1_values
 subroutine abram2_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! ABRAM2_VALUES returns some values of the Abramowitz2 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      ABRAM2(x) = Integral ( 0 <= t < infinity ) t^2 * exp ( -t^2 - x / t ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    22 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.44213858162107913430D+00, &
            0.43923379545684026308D+00, &
            0.42789857297092602234D+00, &
            0.38652825661854504406D+00, &
            0.26538204413231368110D+00, &
            0.16848734838334595000D+00, &
            0.13609200032513227112D+00, &
            0.11070330027727917352D+00, &
            0.82126019995530382267D-01, &
            0.74538781999594581763D-01, &
            0.67732034377612811390D-01, &
            0.35641808698811851022D-01, &
            0.17956589956618269083D-01, &
            0.94058737143575370625D-02, &
            0.50809356204299213556D-02, &
            0.28149565414209719359D-02, &
            0.53808696422559303431D-03, &
            0.44821756380146327259D-04, &
            0.46890678427324100410D-05, &
            0.20161544850996420504D-08 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
            0.0019531250D+00, &
            0.0078125000D+00, &
            0.0312500000D+00, &
            0.1250000000D+00, &
            0.5000000000D+00, &
            1.0000000000D+00, &
            1.2500000000D+00, &
            1.5000000000D+00, &
            1.8750000000D+00, &
            2.0000000000D+00, &
            2.1250000000D+00, &
            3.0000000000D+00, &
            4.0000000000D+00, &
            5.0000000000D+00, &
            6.0000000000D+00, &
            7.0000000000D+00, &
            10.0000000000D+00, &
            15.0000000000D+00, &
            20.0000000000D+00, &
            40.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine abram2_values
 subroutine debye3_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! DEBYE3_VALUES returns some values of Debye's function of order 3.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      DEBYE3(x) = 3 / x^3 * Integral ( 0 <= t <= x ) t^3 / ( exp ( t ) - 1 ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    28 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.99926776885985461940D+00, &
             0.98833007755734698212D+00, &
             0.95390610472023510237D+00, &
             0.82496296897623372315D+00, &
             0.67441556407781468010D+00, &
             0.54710665141286285468D+00, &
             0.44112847372762418113D+00, &
             0.35413603481042394211D+00, &
             0.28357982814342246206D+00, &
             0.18173691382177474795D+00, &
             0.16277924385112436877D+00, &
             0.11759741179993396450D+00, &
             0.95240802723158889887D-01, &
             0.77581324733763020269D-01, &
             0.36560295673194845002D-01, &
             0.19295765690345489563D-01, &
             0.57712632276188798621D-02, &
             0.24352200674805479827D-02, &
             0.72154882216335666096D-03, &
             0.15585454565440389896D-03 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine debye3_values
 subroutine synch2_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! SYNCH2_VALUES returns some values of the synchrotron radiation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      SYNCH2(x) = x * K(2/3)(x)
 !
 !    where K(2/3) is a modified Bessel function of order 2/3.
 !
 !  Modified:
 !
 !    05 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.13430727275667378338D+00, &
            0.33485265272424176976D+00, &
            0.50404224110911078651D+00, &
            0.60296523236016785113D+00, &
            0.49447506210420826699D+00, &
            0.36036067860473360389D+00, &
            0.24967785497625662113D+00, &
            0.16813830542905833533D+00, &
            0.11117122348556549832D+00, &
            0.46923205826101330711D-01, &
            0.37624545861980001482D-01, &
            0.19222123172484106436D-01, &
            0.12209535343654701398D-01, &
            0.77249644268525771866D-02, &
            0.12029044213679269639D-02, &
            0.18161187569530204281D-03, &
            0.26884338006629353506D-04, &
            0.14942212731345828759D-05, &
            0.11607696854385161390D-07, &
            0.87362343746221526073D-10 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             25.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine synch2_values
 subroutine airy_ai_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! AIRY_AI_INT_VALUES returns some values of the integral of the Airy function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      AIRY_AI_INT(x) = Integral ( 0 <= t <= x ) Ai(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    22 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            -0.75228838916610124300D+00, &
            -0.57348350185854889466D+00, &
            -0.76569840313421291743D+00, &
            -0.65181015505382467421D+00, &
            -0.55881974894471876922D+00, &
            -0.56902352870716815309D+00, &
            -0.47800749642926168100D+00, &
            -0.46567398346706861416D+00, &
            -0.96783140945618013679D-01, &
            -0.34683049857035607494D-03, &
             0.34658366917927930790D-03, &
             0.27657581846051227124D-02, &
             0.14595330491185717833D+00, &
             0.23631734191710977960D+00, &
             0.33289264538612212697D+00, &
             0.33318759129779422976D+00, &
             0.33332945170523851439D+00, &
             0.33333331724248357420D+00, &
             0.33333333329916901594D+00, &
             0.33333333333329380187D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
            -12.0000000000D+00, &
            -11.0000000000D+00, &
            -10.0000000000D+00, &
             -9.5000000000D+00, &
             -9.0000000000D+00, &
             -6.5000000000D+00, &
             -4.0000000000D+00, &
             -1.0000000000D+00, &
             -0.2500000000D+00, &
             -0.0009765625D+00, &
              0.0009765625D+00, &
              0.0078125000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              4.0000000000D+00, &
              4.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine airy_ai_int_values
 subroutine debye4_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! DEBYE4_VALUES returns some values of Debye's function of order 4.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      DEBYE4(x) = 4 / x^4 * Integral ( 0 <= t <= x ) t^4 / ( exp ( t ) - 1 ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    28 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.99921896192761576256D+00, &
             0.98755425280996071022D+00, &
             0.95086788606389739976D+00, &
             0.81384569172034042516D+00, &
             0.65487406888673697092D+00, &
             0.52162830964878715188D+00, &
             0.41189273671788528876D+00, &
             0.32295434858707304628D+00, &
             0.25187863642883314410D+00, &
             0.15185461258672022043D+00, &
             0.13372661145921413299D+00, &
             0.91471377664481164749D-01, &
             0.71227828197462523663D-01, &
             0.55676547822738862783D-01, &
             0.21967566525574960096D-01, &
             0.96736755602711590082D-02, &
             0.19646978158351837850D-02, &
             0.62214648623965450200D-03, &
             0.12289514092077854510D-03, &
             0.15927210319002161231D-04 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine debye4_values
 subroutine tran02_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN02_VALUES returns some values of the order 2 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN02(x) = Integral ( 0 <= t <= x ) t^2 exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.19531247930394515480D-02, &
            0.31249152314331109004D-01, &
            0.12494577194783451032D+00, &
            0.49655363615640595865D+00, &
            0.97303256135517012845D+00, &
            0.14121978695932525805D+01, &
            0.18017185674405776809D+01, &
            0.21350385339277043015D+01, &
            0.24110500490169534620D+01, &
            0.28066664045631179931D+01, &
            0.28777421863296234131D+01, &
            0.30391706043438554330D+01, &
            0.31125074928667355940D+01, &
            0.31656687817738577185D+01, &
            0.32623520367816009184D+01, &
            0.32843291144979517358D+01, &
            0.32897895167775788137D+01, &
            0.32898672226665499687D+01, &
            0.32898681336064325400D+01, &
            0.32898681336964528724D+01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran02_values
 subroutine airy_bi_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! AIRY_BI_INT_VALUES returns some values of the integral of the Airy function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      AIRY_BI_INT(x) = Integral ( 0 <= t <= x ) Bi(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    23 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.17660819031554631869D-01, &
            -0.15040424806140020451D-01, &
             0.14756446293227661920D-01, &
            -0.11847304264848446271D+00, &
            -0.64916741266165856037D-01, &
             0.97260832464381044540D-01, &
             0.50760058495287539119D-01, &
            -0.37300500963429492179D+00, &
            -0.13962988442666578531D+00, &
            -0.12001735266723296160D-02, &
             0.12018836117890354598D-02, &
             0.36533846550952011043D+00, &
             0.87276911673800812196D+00, &
             0.48219475263803429675D+02, &
             0.44006525804904178439D+06, &
             0.17608153976228301458D+07, &
             0.73779211705220007228D+07, &
             0.14780980310740671617D+09, &
             0.97037614223613433849D+11, &
             0.11632737638809878460D+15 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
            -12.0000000000D+00, &
            -10.0000000000D+00, &
             -8.0000000000D+00, &
             -7.5000000000D+00, &
             -7.0000000000D+00, &
             -6.5000000000D+00, &
             -4.0000000000D+00, &
             -1.0000000000D+00, &
             -0.2500000000D+00, &
             -0.0019531250D+00, &
              0.0019531250D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              4.0000000000D+00, &
              8.0000000000D+00, &
              8.5000000000D+00, &
              9.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00, &
             14.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine airy_bi_int_values
 subroutine exp3_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! EXP3_INT_VALUES returns some values of the EXP3 integral function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      EXP3_INT(x) = Integral ( 0 <= t <= x ) exp ( -t^3 ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    28 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.19531249963620212007D-02, &
             0.78124990686775522671D-02, &
             0.31249761583499728667D-01, &
             0.12493899888803079984D+00, &
             0.48491714311363971332D+00, &
             0.80751118213967145286D+00, &
             0.86889265412623270696D+00, &
             0.88861722235357162648D+00, &
             0.89286018500218176869D+00, &
             0.89295351429387631138D+00, &
             0.89297479112737843939D+00, &
             0.89297880579798112220D+00, &
             0.89297950317496621294D+00, &
             0.89297951152951902903D+00, &
             0.89297951156918122102D+00, &
             0.89297951156924734716D+00, &
             0.89297951156924917298D+00, &
             0.89297951156924921121D+00, &
             0.89297951156924921122D+00, &
             0.89297951156924921122D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
             0.0019531250D+00, &
             0.0078125000D+00, &
             0.0312500000D+00, &
             0.1250000000D+00, &
             0.5000000000D+00, &
             1.0000000000D+00, &
             1.2500000000D+00, &
             1.5000000000D+00, &
             1.8750000000D+00, &
             2.0000000000D+00, &
             2.1250000000D+00, &
             2.2500000000D+00, &
             2.5000000000D+00, &
             2.7500000000D+00, &
             3.0000000000D+00, &
             3.1250000000D+00, &
             3.2500000000D+00, &
             3.5000000000D+00, &
             3.7500000000D+00, &
             4.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine exp3_int_values
 subroutine tran03_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN03_VALUES returns some values of the order 3 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN03(x) = Integral ( 0 <= t <= x ) t^3 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.19073483296476379584D-05, &
            0.48826138243180786081D-03, &
            0.78074163848431205820D-02, &
            0.12370868718812031049D+00, &
            0.47984100657241749994D+00, &
            0.10269431622039754738D+01, &
            0.17063547219458658863D+01, &
            0.24539217444475937661D+01, &
            0.32106046629422467723D+01, &
            0.45792174372291563703D+01, &
            0.48722022832940370805D+01, &
            0.56143866138422732286D+01, &
            0.59984455864575470009D+01, &
            0.63033953673480961120D+01, &
            0.69579908688361166266D+01, &
            0.71503227120085929750D+01, &
            0.72110731475871876393D+01, &
            0.72123221966388461839D+01, &
            0.72123414161609465119D+01, &
            0.72123414189575656868D+01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran03_values
 subroutine airy_gi_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! AIRY_GI_VALUES returns some values of the Airy Gi function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      AIRY_GI(x) = Integral ( 0 <= t < infinity ) sin ( x*t+t^3/3) dt / pi
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    24 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.20468308070040542435D+00, &
             0.18374662832557904078D+00, &
            -0.11667221729601528265D+00, &
             0.31466934902729557596D+00, &
            -0.37089040722426257729D+00, &
            -0.25293059772424019694D+00, &
             0.28967410658692701936D+00, &
            -0.34644836492634090590D+00, &
             0.28076035913873049496D+00, &
             0.21814994508094865815D+00, &
             0.20526679000810503329D+00, &
             0.22123695363784773258D+00, &
             0.23521843981043793760D+00, &
             0.82834303363768729338D-01, &
             0.45757385490989281893D-01, &
             0.44150012014605159922D-01, &
             0.39951133719508907541D-01, &
             0.35467706833949671483D-01, &
             0.31896005100679587981D-01, &
             0.26556892713512410405D-01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
             -0.0019531250D+00, &
             -0.1250000000D+00, &
             -1.0000000000D+00, &
             -4.0000000000D+00, &
             -8.0000000000D+00, &
             -8.2500000000D+00, &
             -9.0000000000D+00, &
            -10.0000000000D+00, &
            -11.0000000000D+00, &
            -13.0000000000D+00, &
              0.0019531250D+00, &
              0.1250000000D+00, &
              1.0000000000D+00, &
              4.0000000000D+00, &
              7.0000000000D+00, &
              7.2500000000D+00, &
              8.0000000000D+00, &
              9.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine airy_gi_values
 subroutine goodwin_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! GOODWIN_VALUES returns some values of the Goodwin and Staton function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      GOODWIN(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 ) / ( t + x ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.59531540040441651584D+01, &
             0.45769601268624494109D+01, &
             0.32288921331902217638D+01, &
             0.19746110873568719362D+01, &
             0.96356046208697728563D+00, &
             0.60513365250334458174D+00, &
             0.51305506459532198016D+00, &
             0.44598602820946133091D+00, &
             0.37344458206879749357D+00, &
             0.35433592884953063055D+00, &
             0.33712156518881920994D+00, &
             0.29436170729362979176D+00, &
             0.25193499644897222840D+00, &
             0.22028778222123939276D+00, &
             0.19575258237698917033D+00, &
             0.17616303166670699424D+00, &
             0.16015469479664778673D+00, &
             0.14096116876193391066D+00, &
             0.13554987191049066274D+00, &
             0.11751605060085098084D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
             0.0019531250D+00, &
             0.0078125000D+00, &
             0.0312500000D+00, &
             0.1250000000D+00, &
             0.5000000000D+00, &
             1.0000000000D+00, &
             1.2500000000D+00, &
             1.5000000000D+00, &
             1.8750000000D+00, &
             2.0000000000D+00, &
             2.1250000000D+00, &
             2.5000000000D+00, &
             3.0000000000D+00, &
             3.5000000000D+00, &
             4.0000000000D+00, &
             4.5000000000D+00, &
             5.0000000000D+00, &
             5.7500000000D+00, &
             6.0000000000D+00, &
             7.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine goodwin_values
 subroutine tran04_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN04_VALUES returns some values of the order 4 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN04(x) = Integral ( 0 <= t <= x ) t^4 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.24835263919461834041D-08, &
            0.10172029353616724881D-04, &
            0.65053332405940765479D-03, &
            0.41150448004155727767D-01, &
            0.31724404523442648241D+00, &
            0.10079442901142373591D+01, &
            0.22010881024333408363D+01, &
            0.38846508619156545210D+01, &
            0.59648223973714765245D+01, &
            0.10731932392998622219D+02, &
            0.11940028876819364777D+02, &
            0.15359784316882182982D+02, &
            0.17372587633093742893D+02, &
            0.19122976016053166969D+02, &
            0.23583979156921941515D+02, &
            0.25273667677030441733D+02, &
            0.25955198214572256372D+02, &
            0.25975350935212241910D+02, &
            0.25975757522084093747D+02, &
            0.25975757609067315288D+02 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran04_values
 subroutine airy_hi_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! AIRY_HI_VALUES returns some values of the Airy Hi function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      AIRY_HI(x) = Integral ( 0 <= t < infinity ) exp ( x * t - t^3 / 3 ) dt / pi
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    24 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.40936798278458884024D+00, &
            0.37495291608048868619D+00, &
            0.22066960679295989454D+00, &
            0.77565356679703713590D-01, &
            0.39638826473124717315D-01, &
            0.38450072575004151871D-01, &
            0.35273216868317898556D-01, &
            0.31768535282502272742D-01, &
            0.28894408288051391369D-01, &
            0.24463284011678541180D-01, &
            0.41053540139998941517D+00, &
            0.44993502381204990817D+00, &
            0.97220515514243332184D+00, &
            0.83764237105104371193D+02, &
            0.80327744952044756016D+05, &
            0.15514138847749108298D+06, &
            0.11995859641733262114D+07, &
            0.21472868855967642259D+08, &
            0.45564115351632913590D+09, &
            0.32980722582904761929D+12 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
             -0.0019531250D+00, &
             -0.1250000000D+00, &
             -1.0000000000D+00, &
             -4.0000000000D+00, &
             -8.0000000000D+00, &
             -8.2500000000D+00, &
             -9.0000000000D+00, &
            -10.0000000000D+00, &
            -11.0000000000D+00, &
            -13.0000000000D+00, &
              0.0019531250D+00, &
              0.1250000000D+00, &
              1.0000000000D+00, &
              4.0000000000D+00, &
              7.0000000000D+00, &
              7.2500000000D+00, &
              8.0000000000D+00, &
              9.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine airy_hi_values
 subroutine i0ml0_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! I0ML0_VALUES returns some values of the I0ML0 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      I0ML0(x) = I0(x) - L0(x)
 !
 !    I0(x) is the modified Bessel function of the first kind of order 0,
 !    L0(x) is the modified Struve function of order 0.
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    30 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.99875755515461749793D+00, &
            0.99011358230706643807D+00, &
            0.92419435310023947018D+00, &
            0.73624267134714273902D+00, &
            0.55582269181411744686D+00, &
            0.34215154434462160628D+00, &
            0.17087174888774706539D+00, &
            0.81081008709219208918D-01, &
            0.53449421441089580702D-01, &
            0.39950321008923244846D-01, &
            0.39330637437584921392D-01, &
            0.37582274342808670750D-01, &
            0.31912486554480390343D-01, &
            0.25506146883504738403D-01, &
            0.21244480317825292412D-01, &
            0.15925498348551684335D-01, &
            0.12737506927242585015D-01, &
            0.84897750814784916847D-02, &
            0.63668349178454469153D-02, &
            0.50932843163122551114D-02 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0156250000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
              4.0000000000D+00, &
              8.0000000000D+00, &
             12.0000000000D+00, &
             16.0000000000D+00, &
             16.2500000000D+00, &
             17.0000000000D+00, &
             20.0000000000D+00, &
             25.0000000000D+00, &
             30.0000000000D+00, &
             40.0000000000D+00, &
             50.0000000000D+00, &
             75.0000000000D+00, &
            100.0000000000D+00, &
            125.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine i0ml0_values
 subroutine tran05_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN05_VALUES returns some values of the order 5 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN05(x) = Integral ( 0 <= t <= x ) t^5 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.36379780361036116971D-11, &
            0.23840564453948442379D-06, &
            0.60982205372226969189D-04, &
            0.15410004586376649337D-01, &
            0.23661587923909478926D+00, &
            0.11198756851307629651D+01, &
            0.32292901663684049171D+01, &
            0.70362973105160654056D+01, &
            0.12770557691044159511D+02, &
            0.29488339015245845447D+02, &
            0.34471340540362254586D+02, &
            0.50263092218175187785D+02, &
            0.60819909101127165207D+02, &
            0.70873334429213460498D+02, &
            0.10147781242977788097D+03, &
            0.11638074540242071077D+03, &
            0.12409623901262967878D+03, &
            0.12442270155632550228D+03, &
            0.12443132790838589548D+03, &
            0.12443133061720432435D+03 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran05_values
 subroutine arctan_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! ARCTAN_INT_VALUES returns some values of the inverse tangent integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      ARCTAN_INT(x) = Integral ( 0 <= t <= x ) arctan ( t ) / t dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    25 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.19531241721588483191D-02, &
            -0.39062433772980711281D-02, &
             0.78124470192576499535D-02, &
             0.15624576181996527280D-01, &
            -0.31246610349485401551D-01, &
             0.62472911335014397321D-01, &
             0.12478419717389654039D+00, &
            -0.24830175098230686908D+00, &
             0.48722235829452235711D+00, &
             0.91596559417721901505D+00, &
             0.12749694484943800618D+01, &
            -0.15760154034463234224D+01, &
             0.24258878412859089996D+01, &
             0.33911633326292997361D+01, &
             0.44176450919422186583D+01, &
            -0.47556713749547247774D+01, &
             0.50961912150934111303D+01, &
             0.53759175735714876256D+01, &
            -0.61649904785027487422D+01, &
             0.72437843013083534973D+01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
             -0.0039062500D+00, &
              0.0078125000D+00, &
              0.0156250000D+00, &
             -0.0312500000D+00, &
              0.0625000000D+00, &
              0.1250000000D+00, &
             -0.2500000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
             -2.0000000000D+00, &
              4.0000000000D+00, &
              8.0000000000D+00, &
             16.0000000000D+00, &
            -20.0000000000D+00, &
             25.0000000000D+00, &
             30.0000000000D+00, &
            -50.0000000000D+00, &
            100.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine arctan_int_values
 subroutine i1ml1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! I1ML1_VALUES returns some values of the I1ML1 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      I1ML1(x) = I1(x) - L1(x)
 !
 !    I1(x) is the modified Bessel function of the first kind of order 1,
 !    L1(x) is the modified Struve function of order 1.
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    30 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.97575346155386267134D-03, &
            0.77609293280609272733D-02, &
            0.59302966404545373770D-01, &
            0.20395212276737365307D+00, &
            0.33839472293667639038D+00, &
            0.48787706726961324579D+00, &
            0.59018734196576517506D+00, &
            0.62604539530312149476D+00, &
            0.63209315274909764698D+00, &
            0.63410179313235359215D+00, &
            0.63417966797578128188D+00, &
            0.63439268632392089434D+00, &
            0.63501579073257770690D+00, &
            0.63559616677359459337D+00, &
            0.63591001826697110312D+00, &
            0.63622113181751073643D+00, &
            0.63636481702133606597D+00, &
            0.63650653499619902120D+00, &
            0.63655609126300261851D+00, &
            0.63657902087183929223D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0156250000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
              4.0000000000D+00, &
              8.0000000000D+00, &
             12.0000000000D+00, &
             16.0000000000D+00, &
             16.2500000000D+00, &
             17.0000000000D+00, &
             20.0000000000D+00, &
             25.0000000000D+00, &
             30.0000000000D+00, &
             40.0000000000D+00, &
             50.0000000000D+00, &
             75.0000000000D+00, &
            100.0000000000D+00, &
            125.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine i1ml1_values
 subroutine tran06_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN06_VALUES returns some values of the order 6 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN06(x) = Integral ( 0 <= t <= x ) t^6 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.56843405953641209574D-14, &
            0.59601180165247401484D-08, &
            0.60978424397580572815D-05, &
            0.61578909866319494394D-02, &
            0.18854360275680840514D+00, &
            0.13319251347921659134D+01, &
            0.50857202271697616755D+01, &
            0.13729222365466557122D+02, &
            0.29579592481641441292D+02, &
            0.88600835706899853768D+02, &
            0.10916037113373004909D+03, &
            0.18224323749575359518D+03, &
            0.23765383125586756031D+03, &
            0.29543246745959381136D+03, &
            0.50681244381280455592D+03, &
            0.63878231134946125623D+03, &
            0.72699203556994876111D+03, &
            0.73230331643146851717D+03, &
            0.73248692015882096369D+03, &
            0.73248700462879996604D+03 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran06_values
 subroutine bessel_i0_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! BESSEL_I0_INT_VALUES returns some values of the Bessel I0 integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      I0_INT(x) = Integral ( 0 <= t <= x ) I0(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.19531256208818052282D-02, &
            -0.39062549670565734544D-02, &
             0.62520348032546565850D-01, &
             0.12516285581366971819D+00, &
            -0.51051480879740303760D+00, &
             0.10865210970235898158D+01, &
             0.27750019054282535299D+01, &
            -0.13775208868039716639D+02, &
             0.46424372058106108576D+03, &
             0.64111867658021584522D+07, &
            -0.10414860803175857953D+08, &
             0.44758598913855743089D+08, &
            -0.11852985311558287888D+09, &
             0.31430078220715992752D+09, &
            -0.83440212900794309620D+09, &
             0.22175367579074298261D+10, &
             0.58991731842803636487D+10, &
            -0.41857073244691522147D+11, &
             0.79553885818472357663D+12, &
             0.15089715082719201025D+17 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
             -0.0039062500D+00, &
              0.0625000000D+00, &
              0.1250000000D+00, &
             -0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
             -4.0000000000D+00, &
              8.0000000000D+00, &
             18.0000000000D+00, &
            -18.5000000000D+00, &
             20.0000000000D+00, &
            -21.0000000000D+00, &
             22.0000000000D+00, &
            -23.0000000000D+00, &
             24.0000000000D+00, &
             25.0000000000D+00, &
            -27.0000000000D+00, &
             30.0000000000D+00, &
             40.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine bessel_i0_int_values
 subroutine lobachevsky_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! LOBACHEVSKY_VALUES returns some values of the Lobachevsky function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      LOBACHEVSKY(x) = Integral ( 0 <= t <= x ) -ln ( abs ( cos ( t ) ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    31 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.12417639065161393857D-08, &
            0.79473344770001088225D-07, &
            0.50867598186208834198D-05, &
            0.32603097901207200319D-03, &
            0.21380536815408214419D-01, &
            0.18753816902083824050D+00, &
            0.83051199971883645115D+00, &
            0.18854362426679034904D+01, &
            0.21315988986516411053D+01, &
            0.21771120185613427221D+01, &
            0.22921027921896650849D+01, &
            0.39137195028784495586D+01, &
            0.43513563983836427904D+01, &
            0.44200644968478185898D+01, &
            0.65656013133623829156D+01, &
            0.10825504661504599479D+02, &
            0.13365512855474227325D+02, &
            0.21131002685639959927D+02, &
            0.34838236589449117389D+02, &
            0.69657062437837394278D+02 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0078125000D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              5.0000000000D+00, &
              6.0000000000D+00, &
              7.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00, &
            100.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine lobachevsky_values
 subroutine tran07_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN07_VALUES returns some values of the order 7 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN07(x) = Integral ( 0 <= t <= x ) t^7 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.92518563327283409427D-17, &
            0.15521095556949867541D-09, &
            0.63516238373841716290D-06, &
            0.25638801246626135714D-02, &
            0.15665328993811649746D+00, &
            0.16538225039181097423D+01, &
            0.83763085709508211054D+01, &
            0.28078570717830763747D+02, &
            0.72009676046751991365D+02, &
            0.28174905701691911450D+03, &
            0.36660227975327792529D+03, &
            0.70556067982603601123D+03, &
            0.99661927562755629434D+03, &
            0.13288914430417403901D+04, &
            0.27987640273169129925D+04, &
            0.39721376409416504325D+04, &
            0.49913492839319899726D+04, &
            0.50781562639825019000D+04, &
            0.50820777202028708434D+04, &
            0.50820803580047164618D+04 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran07_values
 subroutine bessel_j0_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! BESSEL_J0_INT_VALUES returns some values of the Bessel J0 integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      J0_INT(x) = Integral ( 0 <= t <= x ) J0(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.97656242238978822427D-03, &
             0.39062450329491108875D-02, &
            -0.62479657927917933620D-01, &
             0.12483733492120479139D+00, &
            -0.48968050664604505505D+00, &
             0.91973041008976023931D+00, &
            -0.14257702931970265690D+01, &
             0.10247341594606064818D+01, &
            -0.12107468348304501655D+01, &
             0.11008652032736190799D+01, &
            -0.10060334829904124192D+01, &
             0.81330572662485953519D+00, &
            -0.10583788214211277585D+01, &
             0.87101492116545875169D+00, &
            -0.88424908882547488420D+00, &
             0.11257761503599914603D+01, &
            -0.90141212258183461184D+00, &
             0.91441344369647797803D+00, &
            -0.94482281938334394886D+00, &
             0.92266255696016607257D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0009765625D+00, &
              0.0039062500D+00, &
             -0.0625000000D+00, &
              0.1250000000D+00, &
             -0.5000000000D+00, &
              1.0000000000D+00, &
             -2.0000000000D+00, &
              4.0000000000D+00, &
             -8.0000000000D+00, &
             16.0000000000D+00, &
            -16.5000000000D+00, &
             18.0000000000D+00, &
            -20.0000000000D+00, &
             25.0000000000D+00, &
            -30.0000000000D+00, &
             40.0000000000D+00, &
            -50.0000000000D+00, &
             75.0000000000D+00, &
            -80.0000000000D+00, &
            100.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine bessel_j0_int_values
 subroutine stromgen_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! STROMGEN_VALUES returns some values of the Stromgen function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      STROMGEN(X) = Integral ( 0 <= t <= X ) t^7 * exp(2*t) / (exp(t)-1)^3 dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    31 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.21901065985698662316D-15, &
            0.22481399438625244761D-12, &
            0.23245019579558857124D-09, &
            0.24719561475975007037D-06, &
            0.28992610989833245669D-03, &
            0.10698146390809715091D-01, &
            0.89707650964424730705D-01, &
            0.40049605719592888440D+00, &
            0.30504104398079096598D+01, &
            0.11367704858439426431D+02, &
            0.12960679405324786954D+02, &
            0.18548713944748505675D+02, &
            0.27866273821903121400D+02, &
            0.51963334071699323351D+02, &
            0.10861016747891228129D+03, &
            0.15378903316556621624D+03, &
            0.19302665532558721516D+03, &
            0.19636850166006541482D+03, &
            0.19651946766008214217D+03, &
            0.19651956920868316152D+03 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0078125000D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.1250000000D+00, &
              4.5000000000D+00, &
              5.0000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine stromgen_values
 subroutine tran08_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN08_VALUES returns some values of the order 8 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN08(x) = Integral ( 0 <= t <= x ) t^8 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.15488598634539359463D-19, &
            0.41574269117845953797D-11, &
            0.68050651245227411689D-07, &
            0.10981703519563009836D-02, &
            0.13396432776187883834D+00, &
            0.21153387806998617182D+01, &
            0.14227877028750735641D+02, &
            0.59312061431647843226D+02, &
            0.18139614577043147745D+03, &
            0.93148001928992220863D+03, &
            0.12817928112604611804D+04, &
            0.28572838386329242218D+04, &
            0.43872971687877730010D+04, &
            0.62993229139406657611D+04, &
            0.16589426277154888511D+05, &
            0.27064780798797398935D+05, &
            0.38974556062543661284D+05, &
            0.40400240716905025786D+05, &
            0.40484316504120655568D+05, &
            0.40484399001892184901D+05 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran08_values
 subroutine bessel_k0_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! BESSEL_K0_INT_VALUES returns some values of the Bessel K0 integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      K0_INT(x) = Integral ( 0 <= t <= x ) K0(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.78587929563466784589D-02, &
            0.26019991617330578111D-01, &
            0.24311842237541167904D+00, &
            0.39999633750480508861D+00, &
            0.92710252093114907345D+00, &
            0.12425098486237782662D+01, &
            0.14736757343168286825D+01, &
            0.15606495706051741364D+01, &
            0.15673873907283660493D+01, &
            0.15696345532693743714D+01, &
            0.15701153443250786355D+01, &
            0.15706574852894436220D+01, &
            0.15707793116159788598D+01, &
            0.15707942066465767196D+01, &
            0.15707962315469192247D+01, &
            0.15707963262340149876D+01, &
            0.15707963267948756308D+01, &
            0.15707963267948966192D+01, &
            0.15707963267948966192D+01, &
            0.15707963267948966192D+01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0009765625D+00, &
              0.0039062500D+00, &
              0.0625000000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
              4.0000000000D+00, &
              5.0000000000D+00, &
              6.0000000000D+00, &
              6.5000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             12.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00, &
             80.0000000000D+00, &
            100.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine bessel_k0_int_values
 subroutine struve_h0_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! STRUVE_H0_VALUES returns some values of the Struve H0 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      HO(x) = (2/pi) * Integral ( 0 <= t <= pi/2 ) sin ( x * cos ( t ) ) dt
 !
 !    In Mathematica, the function can be evaluated by:
 !
 !      StruveH[0,x]
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    01 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.12433974658847434366D-02, &
            -0.49735582423748415045D-02, &
             0.39771469054536941564D-01, &
            -0.15805246001653314198D+00, &
             0.56865662704828795099D+00, &
             0.66598399314899916605D+00, &
             0.79085884950809589255D+00, &
            -0.13501457342248639716D+00, &
             0.20086479668164503137D+00, &
            -0.11142097800261991552D+00, &
            -0.17026804865989885869D+00, &
            -0.13544931808186467594D+00, &
             0.94393698081323450897D-01, &
            -0.10182482016001510271D+00, &
             0.96098421554162110012D-01, &
            -0.85337674826118998952D-01, &
            -0.76882290637052720045D-01, &
             0.47663833591418256339D-01, &
            -0.70878751689647343204D-01, &
             0.65752908073352785368D-01 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
               0.0019531250D+00, &
              -0.0078125000D+00, &
               0.0625000000D+00, &
              -0.2500000000D+00, &
               1.0000000000D+00, &
               1.2500000000D+00, &
               2.0000000000D+00, &
              -4.0000000000D+00, &
               7.5000000000D+00, &
              11.0000000000D+00, &
              11.5000000000D+00, &
             -16.0000000000D+00, &
              20.0000000000D+00, &
              25.0000000000D+00, &
             -30.0000000000D+00, &
              50.0000000000D+00, &
              75.0000000000D+00, &
             -80.0000000000D+00, &
             100.0000000000D+00, &
            -125.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine struve_h0_values
 subroutine tran09_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! TRAN09_VALUES returns some values of the order 9 transportation function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      TRAN09(x) = Integral ( 0 <= t <= x ) t^9 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !  Modified:
 !
 !    06 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.26469772870084897671D-22, &
            0.11367943653594246210D-12, &
            0.74428246255329800255D-08, &
            0.48022728485415366194D-03, &
            0.11700243014358676725D+00, &
            0.27648973910899914391D+01, &
            0.24716631405829192997D+02, &
            0.12827119828849828583D+03, &
            0.46842894800662208986D+03, &
            0.31673967371627895718D+04, &
            0.46140886546630195390D+04, &
            0.11952718545392302185D+05, &
            0.20001612666477027728D+05, &
            0.31011073271851366554D+05, &
            0.10352949905541130133D+06, &
            0.19743173017140591390D+06, &
            0.33826030414658460679D+06, &
            0.36179607036750755227D+06, &
            0.36360622124777561525D+06, &
            0.36360880558827162725D+06 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
              3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
              5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
             10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
             30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine tran09_values
 subroutine bessel_y0_int_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! BESSEL_Y0_INT_VALUES returns some values of the Bessel Y0 integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      Y0_INT(x) = Integral ( 0 <= t <= x ) Y0(t) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    30 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            -0.91442642860172110926D-02, &
            -0.29682047390397591290D-01, &
            -0.25391431276585388961D+00, &
            -0.56179545591464028187D+00, &
            -0.63706937660742309754D+00, &
            -0.28219285008510084123D+00, &
             0.38366964785312561103D+00, &
            -0.12595061285798929390D+00, &
             0.24129031832266684828D+00, &
             0.17138069757627037938D+00, &
             0.18958142627134083732D+00, &
             0.17203846136449706946D+00, &
            -0.16821597677215029611D+00, &
            -0.93607927351428988679D-01, &
             0.88229711948036648408D-01, &
            -0.89324662736274161841D-02, &
            -0.54814071000063488284D-01, &
            -0.94958246003466381588D-01, &
            -0.19598064853404969850D-01, &
            -0.83084772357154773468D-02 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0078125000D+00, &
              0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
              4.0000000000D+00, &
              6.0000000000D+00, &
             10.0000000000D+00, &
             16.0000000000D+00, &
             16.2500000000D+00, &
             17.0000000000D+00, &
             20.0000000000D+00, &
             25.0000000000D+00, &
             30.0000000000D+00, &
             40.0000000000D+00, &
             50.0000000000D+00, &
             70.0000000000D+00, &
            100.0000000000D+00, &
            125.0000000000D+00 /)
 !
   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine bessel_y0_int_values
 subroutine struve_h1_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! STRUVE_H1_VALUES returns some values of the Struve H1 function.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      H1(x) = 2*x/pi * Integral ( 0 <= t <= pi/2 )
 !        sin ( x * cos ( t ) )^2 * sin ( t ) dt
 !
 !    In Mathematica, the function can be evaluated by:
 !
 !      StruveH[1,x]
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    02 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
            0.80950369576367526071D-06, &
            0.12952009724113229165D-04, &
            0.82871615165407083021D-03, &
            0.13207748375849572564D-01, &
            0.19845733620194439894D+00, &
            0.29853823231804706294D+00, &
            0.64676372828356211712D+00, &
            0.10697266613089193593D+01, &
            0.38831308000420560970D+00, &
        .80546911629822382012224727595868289d0, &
        .699276943036957460835808848745795509d0, &
        .817054111875970218847660454741902972d0, &
        .472688184291042879880707194329464336d0, &
            0.53880362132692947616D+00, &
            0.72175037834698998506D+00, &
            0.58007844794544189900D+00, &
            0.60151910385440804463D+00, &
            0.70611511147286827018D+00, &
            0.61631110327201338454D+00, &
            0.62778480765443656489D+00 /)

! wrong numbers as from miscfun.f entries 9,10,11,12
! 0.74854243745107710333D+00, &
! 0.84664854642567359993D+00, &
! 0.58385732464244384564D+00, &
! 0.80600584524215772824D+00, &
!
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
               0.0019531250D+00, &
              -0.0078125000D+00, &
               0.0625000000D+00, &
              -0.2500000000D+00, &
               1.0000000000D+00, &
               1.2500000000D+00, &
               2.0000000000D+00, &
              -4.0000000000D+00, &
               7.5000000000D+00, &
              11.0000000000D+00, &
              11.5000000000D+00, &
             -16.0000000000D+00, &
              20.0000000000D+00, &
              25.0000000000D+00, &
             -30.0000000000D+00, &
              50.0000000000D+00, &
              75.0000000000D+00, &
             -80.0000000000D+00, &
             100.0000000000D+00, &
            -125.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine struve_h1_values
 subroutine clausen_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! CLAUSEN_VALUES returns some values of the Clausen's integral.
 !
 !  Discussion:
 !
 !    The function is defined by:
 !
 !      CLAUSEN(x) = Integral ( 0 <= t <= x ) -ln ( 2 * sin ( t / 2 ) ) dt
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    25 August 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.14137352886760576684D-01, &
             0.13955467081981281934D+00, &
            -0.38495732156574238507D+00, &
             0.84831187770367927099D+00, &
             0.10139591323607685043D+01, &
            -0.93921859275409211003D+00, &
             0.72714605086327924743D+00, &
             0.43359820323553277936D+00, &
            -0.98026209391301421161D-01, &
            -0.56814394442986978080D+00, &
            -0.70969701784448921625D+00, &
             0.99282013254695671871D+00, &
            -0.98127747477447367875D+00, &
            -0.64078266570172320959D+00, &
             0.86027963733231192456D+00, &
             0.39071647608680211043D+00, &
             0.47574793926539191502D+00, &
             0.10105014481412878253D+01, &
             0.96332089044363075154D+00, &
            -0.61782699481929311757D+00 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
              0.0312500000D+00, &
             -0.1250000000D+00, &
              0.5000000000D+00, &
              1.0000000000D+00, &
             -1.5000000000D+00, &
              2.0000000000D+00, &
              2.5000000000D+00, &
             -3.0000000000D+00, &
              4.0000000000D+00, &
              4.2500000000D+00, &
             -5.0000000000D+00, &
              5.5000000000D+00, &
              6.0000000000D+00, &
              8.0000000000D+00, &
            -10.0000000000D+00, &
             15.0000000000D+00, &
             20.0000000000D+00, &
            -30.0000000000D+00, &
             50.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine clausen_values
 subroutine struve_l0_values ( n_data, x, fx )

 !*****************************************************************************80
 !
 !! STRUVE_L0_VALUES returns some values of the Struve L0 function.
 !
 !  Discussion:
 !
 !    In Mathematica, the function can be evaluated by:
 !
 !      StruveL[0,x]
 !
 !    The data was reported by McLeod.
 !
 !  Modified:
 !
 !    03 September 2004
 !
 !  Author:
 !
 !    John Burkardt
 !
 !  Reference:
 !
 !    Milton Abramowitz, Irene Stegun,
 !    Handbook of Mathematical Functions,
 !    US Department of Commerce, 1964.
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      special functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    Stephen Wolfram,
 !    The Mathematica Book,
 !    Fourth Edition,
 !    Wolfram Media / Cambridge University Press, 1999.
 !
 !  Parameters:
 !
 !    Input/output, integer,intent(INOUT) :: n_data.  The user sets N_DATA to 0 before the
 !    first call.  On each call, the routine increments N_DATA by 1, and
 !    returns the corresponding data; when there is no more data, the
 !    output value of N_DATA will be 0 again.
 !
 !    Output, , the argument of the function.
 !
 !    Output, real(DP),intent(OUT) :: x,fx, the value of the function.
 !
   implicit none



   real(DP),intent(OUT) :: x,fx
   real ( kind = DP ), save, dimension ( number_testvalues ) :: fx_vec = (/ &
             0.12433985199262820188D-02, &
            -0.19896526647882937004D-01, &
             0.79715713253115014945D-01, &
            -0.32724069939418078025D+00, &
             0.71024318593789088874D+00, &
             0.19374337579914456612D+01, &
            -0.11131050203248583431D+02, &
             0.16850062034703267148D+03, &
            -0.28156522493745948555D+04, &
             0.89344618796978400815D+06, &
             0.11382025002851451057D+07, &
            -0.23549701855860190304D+07, &
             0.43558282527641046718D+08, &
             0.49993516476037957165D+09, &
            -0.57745606064408041689D+10, &
             0.78167229782395624524D+12, &
            -0.14894774793419899908D+17, &
             0.29325537838493363267D+21, &
             0.58940770556098011683D+25, &
            -0.12015889579125463605D+30 /)
   integer,intent(INOUT) :: n_data

   real ( kind = DP ), save, dimension ( number_testvalues ) :: x_vec = (/ &
              0.0019531250D+00, &
             -0.0312500000D+00, &
              0.1250000000D+00, &
             -0.5000000000D+00, &
              1.0000000000D+00, &
              2.0000000000D+00, &
             -4.0000000000D+00, &
              7.0000000000D+00, &
            -10.0000000000D+00, &
             16.0000000000D+00, &
             16.2500000000D+00, &
            -17.0000000000D+00, &
             20.0000000000D+00, &
             22.5000000000D+00, &
            -25.0000000000D+00, &
             30.0000000000D+00, &
            -40.0000000000D+00, &
             50.0000000000D+00, &
             60.0000000000D+00, &
            -70.0000000000D+00 /)

   if ( n_data < 0 ) then
     n_data = 0
   end if

   n_data = n_data + 1

   if ( number_testvalues < n_data ) then
     n_data = 0
     x = 0.0D+00
     fx = 0.0D+00
   else
     x  = x_vec(n_data)
     fx = fx_vec(n_data)
   end if

   return
 end subroutine struve_l0_values
 end module miscfun_testval
 !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 module miscfun_AC
 use nano_deftyp
 use miscfun_testval
 private
 public :: Abramowitz, Transport_Integral, Synchrotron_Radiation, Debye_Function, &
           Airy_Ai_Integral, Airy_Bi_Integral, Airy_Gi, Airy_Hi, Arctan_Integral, &
           bessel_i0_Integral, bessel_j0_Integral, bessel_k0_Integral, bessel_y0_Integral, &
           clausen, exp3_Integral, goodwin, BesselI_minus_StruveL, lobachevsky, stromgen, &
           struve_h0, struve_h1, struve_l0, struve_l1, test_all_miscfun, file_test_miscfun
 !___ Global Variables
 character(19),parameter :: file_test_miscfun='test_of_miscfun.err'
 real(DP),parameter :: halfeps=half*eps_DP,xhigh000=one/eps_DP,xneg11=-one/(sr2*sr5*tiny_DP), &
                       xhigh2T(8)=(/two/eps_DP,three/eps_DP,four/eps_DP,five/eps_DP,six/eps_DP,&
                                    seven/eps_DP,eight/eps_DP,nine/eps_DP/), xhi11=-xneg11, &
                       minate=-eight,oneeps=one/eps_DP, onept5=oneh,ateen=eighteen, &
                       sorgo(9)=oneeps*(/one,two,three,four,five,six,seven,seven*eight,six*ten/), &
                       xmax=biggest_DP,xupper=oneeps,quart=unqua,pisq=pi*pi, twopi=pi2, &
                       onerpi=0.564189583547756286948079451560772587_DP, &
                       twobpi=0.636619772367581343075535053490057448_DP, &
                       piby2=Pi_over_2,piby4=pi*unqua
 integer(I4B),save :: write_errors=0
 real(DP),save :: titty1,titty2,titty3,titty0,titty4,titty5,titty6,tittyx(7),&
                  xhighL3,xhighL1,xneg1,xneg2,xneg2m,xhhh3,xsmallx=2.3406689d-8,sxhi,sxhi1, &
                  logtiny,loghuge,loghuge_eps,xhighP=713.758339_DP,lnxmin,xupper1,xlim

 logical :: setupMISC_done = .false.

 contains
 !*****************************************************************************80
 subroutine setupMISC
 !if (.not.setupMISC_done) call setupMISC
 implicit none
 integer :: j
 real(DP) :: xj
 if (setupMISC_done) return

 xhighL1=-lneps
 xhighL3=-logar2+lneps
 titty0=exp(0.2_DP*log(130.0_DP*tiny_DP))
 titty1=exp(unter*log(six*tiny_DP))
 titty2=sceps_DP*sqrt(ten)
 titty3=sceps_DP*three*sr2
 titty4=sqrt(tiny_DP)*oneh
 titty5=five*oneh*sceps_DP
 titty6=sqrt(tiny_DP)*sr5
 xneg1=-exp(xhighL1*duter)
 xneg2=-exp(-xhighL3*unter)
 xneg2m=-xneg2
 xhhh3=-one/(32.0_DP*sceps_DP)
 xsmallx=pi_over_2*sceps_DP
 sxhi=twenty*sr2/sceps_DP
 sxhi1=two*sr3*sr5/sceps_DP
 logtiny=log(tiny_DP)
 loghuge=log(biggest_DP)
 loghuge_eps=loghuge-lneps*three-three*log(sixteen)
 xhighP=loghuge+log(32.0_DP*five/three)
 lnxmin=logtiny
 xupper1=log(half)-lneps
 xlim=-logtiny
 do j=1,7
   xj=real(j+1,DP)
   tittyx(j)=exp(log(xj*tiny_DP)/xj)
 enddo
 setupMISC_done=.true.
 end subroutine setupMISC
 !****************************
 function addox(n,z)
 implicit none
 real(DP),intent(IN)      :: z
 integer(I4B),intent(IN)  :: n
 real(DP) :: addox, x
 integer(I4B) :: k

 x=one
 do k=2,n
   x=x*(two*real(k,DP)-one)*z
 enddo
 addox=two/(Pi*x)
 end function addox
 !>>>cheval.f90
 function cheval(n,a,t)
 !*****************************************************************************80
 !
 !! CHEVAL evaluates a Chebyshev series.
 !
 !  Discussion:
 !
 !    This FUNCTION evaluates a Chebyshev series, using the
 !    Clenshaw method with Reinsch modification, as analysed
 !    in the paper by Oliver.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !    J Oliver,
 !    An error analysis of the modified Clenshaw method for
 !    evaluating Chebyshev and Fourier series,
 !    Journal of the IMA,
 !    Volume 20, 1977, pages 379-391.
 !
 !  Parameters:
 !
 !    Input, integer :: N, the number of terms in the sequence.
 !
 !    Input, real(kind = DP) :: A(0:N), the coefficients of the Chebyshev series.
 !
 !    Input, real(kind = DP) :: T, the value at which the series is
 !    to be evaluated.
 !
 !    Output, real(kind = DP) :: CHEVAL, the value of the Chebyshev series at T.
 !
   implicit none
   integer,intent(IN) :: n
   real(kind = DP),intent(IN) :: a(0:n)
   real(kind = DP),intent(IN) :: t
   real(kind = DP) :: Cheval
   real(kind = DP) :: d1
   real(kind = DP) :: d2
   integer :: i
   real(kind = DP), parameter :: test = 0.6D+00
   real(kind = DP) :: tt
   real(kind = DP) :: u0
   real(kind = DP) :: u1
   real(kind = DP) :: u2

   u1 = zero
 !
 !  T <= -0.6, Reinsch modification.
 !
   if ( t <= -test ) then
     d1 = zero
     tt = ( t + half ) + half
     tt = tt + tt
     do i = n, 0, -1
       d2 = d1
       u2 = u1
       d1 = tt * u2 + a(i) - d2
       u1 = d1 - u2
     end do
     cheval = ( d1 - d2 ) / two
 !
 !  -0.6 < T < 0.6, Standard Clenshaw method.
 !
   else if ( t < test ) then
     u0 = zero
     tt = t + t
     do i = n, 0, -1
       u2 = u1
       u1 = u0
       u0 = tt * u1 + a(i) - u2
     end do
     cheval = ( u0 - u2 ) / two
 !
 !  0.6 <= T, Reinsch modification.
 !
   else
     d1 = zero
     tt = ( t - half ) - half
     tt = tt + tt
     do i = n, 0, -1
       d2 = d1
       u2 = u1
       d1 = tt * u2 + a(i) + d2
       u1 = d1 + u2
     enddo
     cheval = ( d1 + d2 ) / two
   endif
 end function cheval
 !******************************************************************
 !>>>abramowitz.f90
 function abramowitz(x,order)
 real(DP),intent(IN)  :: x
 integer(I4B),intent(IN)  :: order
 real(DP) :: abramowitz

 abramowitz=zero
 if (order==0) then
   abramowitz=abram0(xvalue=x)
 else if (order==1) then
   abramowitz=abram1(xvalue=x)
 else if (order==2) then
   abramowitz=abram2(xvalue=x)
 endif
 end function abramowitz
 !******************************************************************
 !>>>transport_integral.f90
 function transport_integral(x,order)
 real(DP),intent(IN)  :: x
 integer(I4B),intent(IN)  :: order
 real(DP) :: transport_integral

 transport_integral=zero
 if (order==2) then
   transport_integral=tran02(xvalue=x)
 else if (order==3) then
   transport_integral=tran03(xvalue=x)
 else if (order==4) then
   transport_integral=tran04(xvalue=x)
 else if (order==5) then
   transport_integral=tran05(xvalue=x)
 else if (order==6) then
   transport_integral=tran06(xvalue=x)
 else if (order==7) then
   transport_integral=tran07(xvalue=x)
 else if (order==8) then
   transport_integral=tran08(xvalue=x)
 else if (order==9) then
   transport_integral=tran09(xvalue=x)
 endif
 end function transport_integral
 !******************************************************************
 function synchrotron_radiation(x,order)
 !  order=1:    synchrotron_radiation(x,1) = x * Integral ( x <= t < infinity ) K(5/3)(t) dt
 !  order=2:    synchrotron_radiation(x,2) = x * K(2/3)(x)
 real(DP),intent(IN)  :: x
 integer(I4B),intent(IN)  :: order
 real(DP) :: synchrotron_radiation

 synchrotron_radiation=zero
 if (order==1) then
   synchrotron_radiation=synch1(xvalue=x)
 else if (order==2) then
   synchrotron_radiation=synch2(xvalue=x)
 endif
 end function synchrotron_radiation
 !******************************************************************
 function BesselI_minus_StruveL(x,order)
 real(DP),intent(IN)  :: x
 integer(I4B),intent(IN)  :: order
 real(DP) :: BesselI_minus_StruveL, cm2,cm1,ckk,c0
 integer(I4B)  :: j

 BesselI_minus_StruveL=zero
 if (order==0) then
   BesselI_minus_StruveL=i0ml0(xvalue=x)
 else if (order==1) then
   BesselI_minus_StruveL=i1ml1(xvalue=x)
 else if (order>1) then
   !
   cm2=i0ml0(xvalue=x)
   cm1=i1ml1(xvalue=x)
   ckk=addox(n=2,z=x)
   do j=2,order
     c0=-two*cm1*(real(j-1,DP)/x) + cm2 + ckk
     if (j==order) exit
     cm2=cm1
     cm1=c0
     ckk=ckk/(x*real(2*j+1,DP))
   enddo
   BesselI_minus_StruveL=c0
 endif

 end function BesselI_minus_StruveL
 !******************************************************************
 function Debye_Function(x,order)
 !  order=1:    Debye_Function(x,1) = x * Integral ( x <= t < infinity ) K(5/3)(t) dt
 !  order=2:    Debye_Function(x,2) = x * K(2/3)(x)
 real(DP),intent(IN)  :: x
 integer(I4B),intent(IN)  :: order
 real(DP) :: Debye_Function

 Debye_Function=zero
 if (order==1) then
   Debye_Function=debye1(xvalue=x)
 else if (order==2) then
   Debye_Function=debye2(xvalue=x)
 else if (order==3) then
   Debye_Function=debye3(xvalue=x)
 else if (order==4) then
   Debye_Function=debye4(xvalue=x)
 endif
 end function Debye_Function
 !******************************************************************
 !>>>abram0.f90
 function abram0(xvalue)
 !*****************************************************************************80
 !
 !! ABRAM0 evaluates the Abramowitz Function of order 0.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      ABRAM0(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 - x / t ) dt
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: ABRAM0, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP), dimension(0:8) :: ab0f = (/ &
     -0.68121927093549469816d0, &
     -0.78867919816149252495d0, &
      0.5121581776818819543d-1, &
     -0.71092352894541296d-3, &
      0.368681808504287d-5, &
     -0.917832337237d-8, &
      0.1270202563d-10, &
     -0.1076888d-13, &
      0.599d-17 /)
   real(kind = DP), dimension(0:8) :: ab0g = (/ &
      -0.60506039430868273190d0, &
      -0.41950398163201779803d0, &
       0.1703265125190370333d-1, &
      -0.16938917842491397d-3, &
       0.67638089519710d-6, &
      -0.135723636255d-8, &
       0.156297065d-11, &
      -0.112887d-14, &
       0.55d-18 /)
   real(kind = DP), dimension(0:8) :: ab0h = (/ &
       1.38202655230574989705d0, &
      -0.30097929073974904355d0, &
       0.794288809364887241d-2, &
      -0.6431910276847563d-4, &
       0.22549830684374d-6, &
      -0.41220966195d-9, &
       0.44185282d-12, &
      -0.30123d-15, &
       0.14d-18 /)
   real(kind = DP), dimension(0:27) :: ab0as = (/ &
      1.97755499723693067407d+0, &
     -0.1046024792004819485d-1, &
      0.69680790253625366d-3, &
     -0.5898298299996599d-4, &
      0.577164455305320d-5, &
     -0.61523013365756d-6, &
      0.6785396884767d-7, &
     -0.723062537907d-8, &
      0.63306627365d-9, &
     -0.989453793d-11, &
     -0.1681980530d-10, &
      0.673799551d-11, &
     -0.200997939d-11, &
      0.54055903d-12, &
     -0.13816679d-12, &
      0.3422205d-13, &
     -0.826686d-14, &
      0.194566d-14, &
     -0.44268d-15, &
      0.9562d-16, &
     -0.1883d-16, &
      0.301d-17, &
     -0.19d-18, &
     -0.14d-18, &
      0.11d-18, &
     -0.4d-19, &
      0.2d-19, &
     -0.1d-19 /)
   real(kind = DP) :: abram0
   real(kind = DP) :: asln
   real(kind = DP) :: asval
   real(kind = DP) :: fval
   real(kind = DP) :: gval
   real(kind = DP), parameter :: gval0 = 0.13417650264770070909D+00
   real(kind = DP) :: hval
   integer, parameter :: nterma = 22
   integer, parameter :: ntermf = 8
   integer, parameter :: ntermg = 8
   integer, parameter :: ntermh = 8
   real(kind = DP), parameter :: rt3bpi = 0.97720502380583984317D+00
   real(kind = DP), parameter :: rtpib2 = 0.88622692545275801365D+00
   real(kind = DP) :: t
   real(kind = DP) :: v
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'ABRAM0 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     abram0 = zero
   else if ( x == zero ) then
     abram0 = rtpib2
   else if ( x < sceps_DP ) then
     abram0 = rtpib2 + x * ( log ( x ) - gval0 )
   else if ( x <= two ) then
     t =  ( x * x / two - half ) - half
     fval = cheval ( ntermf, ab0f, t )
     gval = cheval ( ntermg, ab0g, t )
     hval = cheval ( ntermh, ab0h, t )
     abram0 = fval / onerpi + x * ( log ( x ) * hval - gval )
   else
     v = three * ( ( x / two ) ** ( two / three ) )
     t = ( six / v - half ) - half
     asval = cheval ( nterma, ab0as, t )
     asln = log ( asval / rt3bpi ) - v
     if ( asln < lnxmin ) then
       abram0 = zero
     else
       abram0 = exp ( asln )
     endif
   endif

   return
 end function abram0
 !******************************************************************
 !>>>abram1.f90
 function abram1(xvalue)

 !*****************************************************************************80
 !
 !! ABRAM1 evaluates the Abramowitz Function of order 1.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      ABRAM1(x) = Integral ( 0 <= t < infinity ) t * exp ( -t^2 - x / t ) dt
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: ABRAM1, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: ab1as(0:27)
   real(kind = DP) :: ab1f(0:9)
   real(kind = DP) :: ab1g(0:8)
   real(kind = DP) :: ab1h(0:8)
   real(kind = DP) :: abram1
   real(kind = DP) :: asln
   real(kind = DP) :: asval
   real(kind = DP) :: fval
   real(kind = DP) :: gval
   real(kind = DP) :: hval
   integer, parameter :: nterma = 23
   integer, parameter :: ntermf = 9
   integer, parameter :: ntermg = 8
   integer, parameter :: ntermh = 8
   real(kind = DP), parameter :: rt3bpi = 0.97720502380583984317D+00
   real(kind = DP) :: t
   real(kind = DP) :: v
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   data ab1f/1.47285192577978807369d0, &
             0.10903497570168956257d0, &
            -0.12430675360056569753d0, &
             0.306197946853493315d-2, &
            -0.2218410323076511d-4, &
             0.6989978834451d-7, &
            -0.11597076444d-9, &
             0.11389776d-12, &
            -0.7173d-16, &
             0.3d-19/
   data ab1g/0.39791277949054503528d0, &
            -0.29045285226454720849d0, &
             0.1048784695465363504d-1, &
            -0.10249869522691336d-3, &
             0.41150279399110d-6, &
            -0.83652638940d-9, &
             0.97862595d-12, &
            -0.71868d-15, &
             0.35d-18/
   data ab1h/0.84150292152274947030d0, &
            -0.7790050698774143395d-1, &
             0.133992455878390993d-2, &
            -0.808503907152788d-5, &
             0.2261858281728d-7, &
            -0.3441395838d-10, &
             0.3159858d-13, &
            -0.1884d-16, &
             0.1d-19/
   data ab1as(0)/  2.13013643429065549448d0/
   data ab1as(1)/  0.6371526795218539933d-1/
   data ab1as(2)/ -0.129334917477510647d-2/
   data ab1as(3)/  0.5678328753228265d-4/
   data ab1as(4)/ -0.279434939177646d-5/
   data ab1as(5)/  0.5600214736787d-7/
   data ab1as(6)/  0.2392009242798d-7/
   data ab1as(7)/ -0.750984865009d-8/
   data ab1as(8)/  0.173015330776d-8/
   data ab1as(9)/ -0.36648877955d-9/
   data ab1as(10)/ 0.7520758307d-10/
   data ab1as(11)/-0.1517990208d-10/
   data ab1as(12)/ 0.301713710d-11/
   data ab1as(13)/-0.58596718d-12/
   data ab1as(14)/ 0.10914455d-12/
   data ab1as(15)/-0.1870536d-13/
   data ab1as(16)/ 0.262542d-14/
   data ab1as(17)/-0.14627d-15/
   data ab1as(18)/-0.9500d-16/
   data ab1as(19)/ 0.5873d-16/
   data ab1as(20)/-0.2420d-16/
   data ab1as(21)/ 0.868d-17/
   data ab1as(22)/-0.290d-17/
   data ab1as(23)/ 0.93d-18/
   data ab1as(24)/-0.29d-18/
   data ab1as(25)/ 0.9d-19/
   data ab1as(26)/-0.3d-19/
   data ab1as(27)/ 0.1d-19/
 !
 !  Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'ABRAM1 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     abram1 = zero
   else if ( x == zero ) then
     abram1 = half
   else if ( x < halfeps ) then
     abram1 = half
   else if ( x < sceps_DP ) then
     abram1 = ( one - x / onerpi - x * x * log ( x ) ) * half
   else if ( x <= two ) then
     t = ( x * x / two - half ) - half
     fval = cheval ( ntermf, ab1f, t )
     gval = cheval ( ntermg, ab1g, t )
     hval = cheval ( ntermh, ab1h, t )
     abram1 = fval - x * ( gval / onerpi + x * log ( x ) * hval )
   else
     v = three *  ( ( x / two ) ** ( two / three ) )
     t =  ( six / v - half ) - half
     asval = cheval ( nterma, ab1as, t )
     asln = log ( asval * sqrt ( v / three ) / rt3bpi ) - v
     if ( asln < lnxmin ) then
       abram1 = zero
     else
       abram1 = exp ( asln )
     endif
   endif

   return
 end function abram1
 !******************************************************************
 !>>>abram2.f90
 function abram2(xvalue)

 !*****************************************************************************80
 !
 !! ABRAM2 evaluates the Abramowitz Function of order 2.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      ABRAM2(x) = Integral ( 0 <= t < infinity ) t^2 * exp ( -t^2 - x / t ) dt
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: ABRAM2, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: abram2
   integer, parameter :: nterma = 23
   integer, parameter :: ntermf = 9
   integer, parameter :: ntermg = 8
   integer, parameter :: ntermh = 7
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: ab2f(0:9),ab2g(0:8),ab2h(0:7),ab2as(0:26), &
        asln,asval,fval,gval,hval, &
        rtpib4,rt3bpi,t, v
   data ab2f/1.03612162804243713846d0, &
             0.19371246626794570012d0, &
            -0.7258758839233007378d-1, &
             0.174790590864327399d-2, &
            -0.1281223233756549d-4, &
             0.4115018153651d-7, &
            -0.6971047256d-10, &
             0.6990183d-13, &
            -0.4492d-16, &
             0.2d-19/
   data ab2g/1.46290157198630741150d0, &
             0.20189466883154014317d0, &
            -0.2908292087997129022d-1, &
             0.47061049035270050d-3, &
            -0.257922080359333d-5, &
             0.656133712946d-8, &
            -0.914110203d-11, &
             0.774276d-14, &
            -0.429d-17/
   data ab2h/0.30117225010910488881d0, &
            -0.1588667818317623783d-1, &
             0.19295936935584526d-3, &
            -0.90199587849300d-6, &
             0.206105041837d-8, &
            -0.265111806d-11, &
             0.210864d-14, &
            -0.111d-17/
   data ab2as(0)/  2.46492325304334856893d0/
   data ab2as(1)/  0.23142797422248905432d0/
   data ab2as(2)/ -0.94068173010085773d-3/
   data ab2as(3)/  0.8290270038089733d-4/
   data ab2as(4)/ -0.883894704245866d-5/
   data ab2as(5)/  0.106638543567985d-5/
   data ab2as(6)/ -0.13991128538529d-6/
   data ab2as(7)/  0.1939793208445d-7/
   data ab2as(8)/ -0.277049938375d-8/
   data ab2as(9)/  0.39590687186d-9/
   data ab2as(10)/-0.5408354342d-10/
   data ab2as(11)/ 0.635546076d-11/
   data ab2as(12)/-0.38461613d-12/
   data ab2as(13)/-0.11696067d-12/
   data ab2as(14)/ 0.6896671d-13/
   data ab2as(15)/-0.2503113d-13/
   data ab2as(16)/ 0.785586d-14/
   data ab2as(17)/-0.230334d-14/
   data ab2as(18)/ 0.64914d-15/
   data ab2as(19)/-0.17797d-15/
   data ab2as(20)/ 0.4766d-16/
   data ab2as(21)/-0.1246d-16/
   data ab2as(22)/ 0.316d-17/
   data ab2as(23)/-0.77d-18/
   data ab2as(24)/ 0.18d-18/
   data ab2as(25)/-0.4d-19/
   data ab2as(26)/ 0.1d-19/
   data rt3bpi/ 0.97720502380583984317d0/
   data rtpib4/ 0.44311346272637900682d0/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then

     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'ABRAM2 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     abram2 = zero

   else if ( x == zero ) then

     abram2 = rtpib4

   else if ( x < eps_DP ) then

     abram2 = rtpib4

   else if ( x < sceps_DP ) then

     abram2 = rtpib4 - half * x + x * x * x * log ( x ) / six

   else if ( x <= 2.0D+00 ) then

     t =  ( x * x / two - half ) - half
     fval = cheval ( ntermf, ab2f, t )
     gval = cheval ( ntermg, ab2g, t )
     hval = cheval ( ntermh, ab2h, t )
     abram2 = fval / onerpi + x * ( x * x * log ( x ) * hval - gval )

   else

     v = three * ( ( x / two ) ** ( two / three ) )
     t = ( six / v - half ) - half
     asval = cheval ( nterma, ab2as, t )
     asln = log ( asval / rt3bpi ) + log ( v / three ) - v

     if ( asln < lnxmin ) then
       abram2 = zero
     else
       abram2 = exp ( asln )
     endif

   endif

   return
 end function abram2
 !******************************************************************
 !>>>airy_ai_Integral.f90
 function airy_ai_Integral(xvalue)

 !*****************************************************************************80
 !
 !! AIRY_AI_INT calculates the integral of the Airy Function Ai.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      AIRY_AI_INT(x) = Integral ( 0 <= t <= x ) Ai(t) dt
 !
 !    The program uses Chebyshev expansions, the coefficients of which
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: AIRY_AI_INT, the value of the FUNCTION.
 !
   implicit none

   real(DP),parameter :: xhigh1_AI_INT=14.480884d0
   real(kind = DP) :: aaint1(0:25)
   real(kind = DP) :: aaint2(0:21)
   real(kind = DP) :: aaint3(0:40)
   real(kind = DP) :: aaint4(0:17)
   real(kind = DP) :: aaint5(0:17)
   real(kind = DP) :: airy_ai_Integral
   real(kind = DP) :: airzer
   real(kind = DP) :: arg
   real(kind = DP) :: forty1
   real(kind = DP) :: fr996
   real(kind = DP) :: gval
   real(kind = DP) :: hval
   real(kind = DP) :: ninhun
   integer, parameter :: nterm1 = 22
   integer, parameter :: nterm2 = 17
   integer, parameter :: nterm3 = 37
   integer :: nterm4
   integer :: nterm5
   real(kind = DP) :: pitim6
   real(kind = DP) :: rt2b3p
   real(kind = DP) :: t
   real(kind = DP) :: temp
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue
   real(kind = DP) :: z

   data aaint1(0)/  0.37713517694683695526d0/
   data aaint1(1)/ -0.13318868432407947431d0/
   data aaint1(2)/  0.3152497374782884809d-1/
   data aaint1(3)/ -0.318543076436574077d-2/
   data aaint1(4)/ -0.87398764698621915d-3/
   data aaint1(5)/  0.46699497655396971d-3/
   data aaint1(6)/ -0.9544936738983692d-4/
   data aaint1(7)/  0.542705687156716d-5/
   data aaint1(8)/  0.239496406252188d-5/
   data aaint1(9)/ -0.75690270205649d-6/
   data aaint1(10)/ 0.9050138584518d-7/
   data aaint1(11)/ 0.320529456043d-8/
   data aaint1(12)/-0.303825536444d-8/
   data aaint1(13)/ 0.48900118596d-9/
   data aaint1(14)/-0.1839820572d-10/
   data aaint1(15)/-0.711247519d-11/
   data aaint1(16)/ 0.151774419d-11/
   data aaint1(17)/-0.10801922d-12/
   data aaint1(18)/-0.963542d-14/
   data aaint1(19)/ 0.313425d-14/
   data aaint1(20)/-0.29446d-15/
   data aaint1(21)/-0.477d-17/
   data aaint1(22)/ 0.461d-17/
   data aaint1(23)/-0.53d-18/
   data aaint1(24)/ 0.1d-19/
   data aaint1(25)/ 0.1d-19/
   data aaint2(0)/  1.92002524081984009769d0/
   data aaint2(1)/ -0.4220049417256287021d-1/
   data aaint2(2)/ -0.239457722965939223d-2/
   data aaint2(3)/ -0.19564070483352971d-3/
   data aaint2(4)/ -0.1547252891056112d-4/
   data aaint2(5)/ -0.140490186137889d-5/
   data aaint2(6)/ -0.12128014271367d-6/
   data aaint2(7)/ -0.1179186050192d-7/
   data aaint2(8)/ -0.104315578788d-8/
   data aaint2(9)/ -0.10908209293d-9/
   data aaint2(10)/-0.929633045d-11/
   data aaint2(11)/-0.110946520d-11/
   data aaint2(12)/-0.7816483d-13/
   data aaint2(13)/-0.1319661d-13/
   data aaint2(14)/-0.36823d-15/
   data aaint2(15)/-0.21505d-15/
   data aaint2(16)/ 0.1238d-16/
   data aaint2(17)/-0.557d-17/
   data aaint2(18)/ 0.84d-18/
   data aaint2(19)/-0.21d-18/
   data aaint2(20)/ 0.4d-19/
   data aaint2(21)/-0.1d-19/
   data aaint3(0)/  0.47985893264791052053d0/
   data aaint3(1)/ -0.19272375126169608863d0/
   data aaint3(2)/  0.2051154129525428189d-1/
   data aaint3(3)/  0.6332000070732488786d-1/
   data aaint3(4)/ -0.5093322261845754082d-1/
   data aaint3(5)/  0.1284424078661663016d-1/
   data aaint3(6)/  0.2760137088989479413d-1/
   data aaint3(7)/ -0.1547066673866649507d-1/
   data aaint3(8)/ -0.1496864655389316026d-1/
   data aaint3(9)/  0.336617614173574541d-2/
   data aaint3(10)/ 0.530851163518892985d-2/
   data aaint3(11)/ 0.41371226458555081d-3/
   data aaint3(12)/-0.102490579926726266d-2/
   data aaint3(13)/-0.32508221672025853d-3/
   data aaint3(14)/ 0.8608660957169213d-4/
   data aaint3(15)/ 0.6671367298120775d-4/
   data aaint3(16)/ 0.449205999318095d-5/
   data aaint3(17)/-0.670427230958249d-5/
   data aaint3(18)/-0.196636570085009d-5/
   data aaint3(19)/ 0.22229677407226d-6/
   data aaint3(20)/ 0.22332222949137d-6/
   data aaint3(21)/ 0.2803313766457d-7/
   data aaint3(22)/-0.1155651663619d-7/
   data aaint3(23)/-0.433069821736d-8/
   data aaint3(24)/-0.6227777938d-10/
   data aaint3(25)/ 0.26432664903d-9/
   data aaint3(26)/ 0.5333881114d-10/
   data aaint3(27)/-0.522957269d-11/
   data aaint3(28)/-0.382229283d-11/
   data aaint3(29)/-0.40958233d-12/
   data aaint3(30)/ 0.11515622d-12/
   data aaint3(31)/ 0.3875766d-13/
   data aaint3(32)/ 0.140283d-14/
   data aaint3(33)/-0.141526d-14/
   data aaint3(34)/-0.28746d-15/
   data aaint3(35)/ 0.923d-17/
   data aaint3(36)/ 0.1224d-16/
   data aaint3(37)/ 0.157d-17/
   data aaint3(38)/-0.19d-18/
   data aaint3(39)/-0.8d-19/
   data aaint3(40)/-0.1d-19/
   data aaint4/1.99653305828522730048d0, &
              -0.187541177605417759d-2, &
              -0.15377536280305750d-3, &
              -0.1283112967682349d-4, &
              -0.108128481964162d-5, &
              -0.9182131174057d-7, &
              -0.784160590960d-8, &
              -0.67292453878d-9, &
              -0.5796325198d-10, &
              -0.501040991d-11, &
              -0.43420222d-12, &
              -0.3774305d-13, &
              -0.328473d-14, &
              -0.28700d-15, &
              -0.2502d-16, &
              -0.220d-17, &
              -0.19d-18, &
              -0.2d-19/
   data aaint5/1.13024602034465716133d0, &
              -0.464718064639872334d-2, &
              -0.35137413382693203d-3, &
              -0.2768117872545185d-4, &
              -0.222057452558107d-5, &
              -0.18089142365974d-6, &
              -0.1487613383373d-7, &
              -0.123515388168d-8, &
              -0.10310104257d-9, &
              -0.867493013d-11, &
              -0.73080054d-12, &
              -0.6223561d-13, &
              -0.525128d-14, &
              -0.45677d-15, &
              -0.3748d-16, &
              -0.356d-17, &
              -0.23d-18, &
              -0.4d-19/
   data forty1/ 41.0d0/
   data ninhun,fr996/ 900.0d0, 4996.0d0 /
   data pitim6/18.84955592153875943078d0/
   data rt2b3p/0.46065886596178063902d0/
   data airzer/0.35502805388781723926d0/
 !
 !   Machine-dependant constants (suitable for IEEE machines)
 !
   data nterm4,nterm5/15,15/

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < xneg1 ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'AIRY_AI_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  X too negative for accurate computation.'
     airy_ai_Integral = -two / three
     return
   else if ( x < -eight ) then
     z = - ( x + x ) * sqrt ( -x ) / three
     arg = z + piby4
     temp = nine * z * z
     t = ( fr996 - temp ) / ( ninhun + temp )
     gval = cheval ( nterm4, aaint4, t )
     hval = cheval ( nterm5, aaint5, t )
     temp = gval * cos ( arg ) + hval * sin ( arg ) / z
     airy_ai_Integral = rt2b3p * temp / sqrt ( z ) - two / three
   else if ( x <= -eps_DP )then
     t = -x / four - one
     airy_ai_Integral = x * cheval ( nterm3, aaint3, t )
   else if ( x < eps_DP ) then
     airy_ai_Integral = airzer * x
   else if ( x <= four ) then
     t = x / two - one
     airy_ai_Integral = cheval ( nterm1, aaint1, t ) * x
   else if ( x <= xhigh1_AI_INT ) then
     z = ( x + x ) * sqrt ( x ) / three
     temp = three * z
     t = ( forty1 - temp ) / ( nine + temp )
     temp = exp ( -z ) * cheval ( nterm2, aaint2, t ) / sqrt ( pitim6 * z )
     airy_ai_Integral = one / three - temp
   else
     airy_ai_Integral = one / three
   endif

   return
 end function airy_ai_Integral
 !******************************************************************
 !>>>airy_bi_Integral.f90
 function airy_bi_Integral(xvalue)

 !*****************************************************************************80
 !
 !! AIRY_BI_INT calculates the integral of the Airy Function Bi.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      AIRY_BI_INT(x) = Integral ( 0 <= t <= x ) Bi(t) dt
 !
 !    The program uses Chebyshev expansions, the coefficients of which
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: AIRY_BI_INT, the value of the FUNCTION.
 !
   implicit none

   real(DP),parameter :: xhigh1_BI_INT=104.587632d0
   real(kind = DP) :: abint1(0:36)
   real(kind = DP) :: abint2(0:37)
   real(kind = DP) :: abint3(0:37)
   real(kind = DP) :: abint4(0:20)
   real(kind = DP) :: abint5(0:20)
   real(kind = DP) :: airy_bi_Integral
   integer, parameter :: nterm1 = 33
   integer, parameter :: nterm2 = 30
   integer, parameter :: nterm3 = 34
   integer :: nterm4
   integer :: nterm5
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(DP) :: arg,birzer,f1,f2,ninhun,rt2b3p,t,temp, thr644,z
   data abint1(0)/  0.38683352445038543350d0/
   data abint1(1)/ -0.8823213550888908821d-1/
   data abint1(2)/  0.21463937440355429239d0/
   data abint1(3)/ -0.4205347375891315126d-1/
   data abint1(4)/  0.5932422547496086771d-1/
   data abint1(5)/ -0.840787081124270210d-2/
   data abint1(6)/  0.871824772778487955d-2/
   data abint1(7)/ -0.12191600199613455d-3/
   data abint1(8)/  0.44024821786023234d-3/
   data abint1(9)/  0.27894686666386678d-3/
   data abint1(10)/-0.7052804689785537d-4/
   data abint1(11)/ 0.5901080066770100d-4/
   data abint1(12)/-0.1370862587982142d-4/
   data abint1(13)/ 0.505962573749073d-5/
   data abint1(14)/-0.51598837766735d-6/
   data abint1(15)/ 0.397511312349d-8/
   data abint1(16)/ 0.9524985978055d-7/
   data abint1(17)/-0.3681435887321d-7/
   data abint1(18)/ 0.1248391688136d-7/
   data abint1(19)/-0.249097619137d-8/
   data abint1(20)/ 0.31775245551d-9/
   data abint1(21)/ 0.5434365270d-10/
   data abint1(22)/-0.4024566915d-10/
   data abint1(23)/ 0.1393855527d-10/
   data abint1(24)/-0.303817509d-11/
   data abint1(25)/ 0.40809511d-12/
   data abint1(26)/ 0.1634116d-13/
   data abint1(27)/-0.2683809d-13/
   data abint1(28)/ 0.896641d-14/
   data abint1(29)/-0.183089d-14/
   data abint1(30)/ 0.21333d-15/
   data abint1(31)/ 0.1108d-16/
   data abint1(32)/-0.1276d-16/
   data abint1(33)/ 0.363d-17/
   data abint1(34)/-0.62d-18/
   data abint1(35)/ 0.5d-19/
   data abint1(36)/ 0.1d-19/
   data abint2(0)/  2.04122078602516135181d0/
   data abint2(1)/  0.2124133918621221230d-1/
   data abint2(2)/  0.66617599766706276d-3/
   data abint2(3)/  0.3842047982808254d-4/
   data abint2(4)/  0.362310366020439d-5/
   data abint2(5)/  0.50351990115074d-6/
   data abint2(6)/  0.7961648702253d-7/
   data abint2(7)/  0.717808442336d-8/
   data abint2(8)/ -0.267770159104d-8/
   data abint2(9)/ -0.168489514699d-8/
   data abint2(10)/-0.36811757255d-9/
   data abint2(11)/ 0.4757128727d-10/
   data abint2(12)/ 0.5263621945d-10/
   data abint2(13)/ 0.778973500d-11/
   data abint2(14)/-0.460546143d-11/
   data abint2(15)/-0.183433736d-11/
   data abint2(16)/ 0.32191249d-12/
   data abint2(17)/ 0.29352060d-12/
   data abint2(18)/-0.1657935d-13/
   data abint2(19)/-0.4483808d-13/
   data abint2(20)/ 0.27907d-15/
   data abint2(21)/ 0.711921d-14/
   data abint2(22)/-0.1042d-16/
   data abint2(23)/-0.119591d-14/
   data abint2(24)/ 0.4606d-16/
   data abint2(25)/ 0.20884d-15/
   data abint2(26)/-0.2416d-16/
   data abint2(27)/-0.3638d-16/
   data abint2(28)/ 0.863d-17/
   data abint2(29)/ 0.591d-17/
   data abint2(30)/-0.256d-17/
   data abint2(31)/-0.77d-18/
   data abint2(32)/ 0.66d-18/
   data abint2(33)/ 0.3d-19/
   data abint2(34)/-0.15d-18/
   data abint2(35)/ 0.2d-19/
   data abint2(36)/ 0.3d-19/
   data abint2(37)/-0.1d-19/
   data abint3(0)/  0.31076961598640349251d0/
   data abint3(1)/ -0.27528845887452542718d0/
   data abint3(2)/  0.17355965706136543928d0/
   data abint3(3)/ -0.5544017909492843130d-1/
   data abint3(4)/ -0.2251265478295950941d-1/
   data abint3(5)/  0.4107347447812521894d-1/
   data abint3(6)/  0.984761275464262480d-2/
   data abint3(7)/ -0.1555618141666041932d-1/
   data abint3(8)/ -0.560871870730279234d-2/
   data abint3(9)/  0.246017783322230475d-2/
   data abint3(10)/ 0.165740392292336978d-2/
   data abint3(11)/-0.3277587501435402d-4/
   data abint3(12)/-0.24434680860514925d-3/
   data abint3(13)/-0.5035305196152321d-4/
   data abint3(14)/ 0.1630264722247854d-4/
   data abint3(15)/ 0.851914057780934d-5/
   data abint3(16)/ 0.29790363004664d-6/
   data abint3(17)/-0.64389707896401d-6/
   data abint3(18)/-0.15046988145803d-6/
   data abint3(19)/ 0.1587013535823d-7/
   data abint3(20)/ 0.1276766299622d-7/
   data abint3(21)/ 0.140578534199d-8/
   data abint3(22)/-0.46564739741d-9/
   data abint3(23)/-0.15682748791d-9/
   data abint3(24)/-0.403893560d-11/
   data abint3(25)/ 0.666708192d-11/
   data abint3(26)/ 0.128869380d-11/
   data abint3(27)/-0.6968663d-13/
   data abint3(28)/-0.6254319d-13/
   data abint3(29)/-0.718392d-14/
   data abint3(30)/ 0.115296d-14/
   data abint3(31)/ 0.42276d-15/
   data abint3(32)/ 0.2493d-16/
   data abint3(33)/-0.971d-17/
   data abint3(34)/-0.216d-17/
   data abint3(35)/-0.2d-19/
   data abint3(36)/ 0.6d-19/
   data abint3(37)/ 0.1d-19/
   data abint4(0)/  1.99507959313352047614d0/
   data abint4(1)/ -0.273736375970692738d-2/
   data abint4(2)/ -0.30897113081285850d-3/
   data abint4(3)/ -0.3550101982798577d-4/
   data abint4(4)/ -0.412179271520133d-5/
   data abint4(5)/ -0.48235892316833d-6/
   data abint4(6)/ -0.5678730727927d-7/
   data abint4(7)/ -0.671874810365d-8/
   data abint4(8)/ -0.79811649857d-9/
   data abint4(9)/ -0.9514271478d-10/
   data abint4(10)/-0.1137468966d-10/
   data abint4(11)/-0.136359969d-11/
   data abint4(12)/-0.16381418d-12/
   data abint4(13)/-0.1972575d-13/
   data abint4(14)/-0.237844d-14/
   data abint4(15)/-0.28752d-15/
   data abint4(16)/-0.3475d-16/
   data abint4(17)/-0.422d-17/
   data abint4(18)/-0.51d-18/
   data abint4(19)/-0.6d-19/
   data abint4(20)/-0.1d-19/
   data abint5(0)/  1.12672081961782566017d0/
   data abint5(1)/ -0.671405567525561198d-2/
   data abint5(2)/ -0.69812918017832969d-3/
   data abint5(3)/ -0.7561689886425276d-4/
   data abint5(4)/ -0.834985574510207d-5/
   data abint5(5)/ -0.93630298232480d-6/
   data abint5(6)/ -0.10608556296250d-6/
   data abint5(7)/ -0.1213128916741d-7/
   data abint5(8)/ -0.139631129765d-8/
   data abint5(9)/ -0.16178918054d-9/
   data abint5(10)/-0.1882307907d-10/
   data abint5(11)/-0.220272985d-11/
   data abint5(12)/-0.25816189d-12/
   data abint5(13)/-0.3047964d-13/
   data abint5(14)/-0.358370d-14/
   data abint5(15)/-0.42831d-15/
   data abint5(16)/-0.4993d-16/
   data abint5(17)/-0.617d-17/
   data abint5(18)/-0.68d-18/
   data abint5(19)/-0.10d-18/
   data abint5(20)/-0.1d-19/
   data ninhun,thr644/900.0d0 , 3644.0d0 /
   data rt2b3p/0.46065886596178063902d0/
   data birzer/0.61492662744600073515d0/
 !
 !   Machine-dependent parameters (suitable for IEEE machines)
 !
   data nterm4,nterm5/17,17/

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < xneg1 ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'AIRY_BI_INT - Warning!'
     if (write_errors==1) print'(a)', '  Argument is too negative for accurate computation.'
     airy_bi_Integral = zero
   else if ( x < -seven ) then
     z = - ( x + x ) * sqrt ( -x ) / three
     arg = z + piby4
     temp = nine * z * z
     t = ( thr644 - temp ) / ( ninhun + temp )
     f1 = cheval ( nterm4, abint4, t ) * sin ( arg )
     f2 = cheval ( nterm5, abint5, t ) * cos ( arg ) / z
     airy_bi_Integral = ( f2 - f1 ) * rt2b3p / sqrt ( z )
   else if ( x <= -eps_DP ) then
     t = - ( x + x ) / seven - one
     airy_bi_Integral = x * cheval ( nterm3, abint3, t )
   else if ( x < eps_DP ) then
     airy_bi_Integral = birzer * x
   else if ( x <= eight ) then
     t = x / four - one
     airy_bi_Integral = x * exp ( onept5 * x ) * cheval ( nterm1, abint1, t )
   else if ( x <= xhigh1_BI_INT ) then
     t = sixteen * sqrt ( eight / x ) / x - one
     z = ( x + x ) * sqrt ( x ) / three
     temp = rt2b3p * cheval ( nterm2, abint2, t ) / sqrt ( z )
     temp = z + log ( temp )
     airy_bi_Integral = exp ( temp )
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'AIRY_BI_INT - Warning!'
     if (write_errors==1) print'(a)', '  Argument is too large for accurate computation.'
     airy_bi_Integral = xmax
   endif

   return
 end function airy_bi_Integral
 !******************************************************************
 !>>>airy_gi.f90
 function airy_gi(xvalue)

 !*****************************************************************************80
 !
 !! AIRY_GI computes the modified Airy Function Gi(x).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      AIRY_GI(x) = Integral ( 0 <= t < infinity ) sin ( x*t+t^3/3) dt / pi
 !
 !    The approximation uses Chebyshev expansions with the coefficients
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: AIRY_GI, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: airy_gi
   integer, parameter :: nterm1 = 28
   integer, parameter :: nterm2 = 23
   integer, parameter :: nterm3 = 39
   integer :: nterm4
   integer :: nterm5
   integer :: nterm6
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: argip1(0:30),argip2(0:29),argin1(0:42), &
        arbin1(0:10),arbin2(0:11),arhin1(0:15), &
        bi,cheb1,cheb2,cosz,five14, &
        gizero,onebpi,one76,one024, &
        rtpiin,seven2,sinz,t,temp,twelhu,twent8, &
        xcube,xminus, &
        zeta
   data argip1(0)/  0.26585770795022745082d0/
   data argip1(1)/ -0.10500333097501922907d0/
   data argip1(2)/  0.841347475328454492d-2/
   data argip1(3)/  0.2021067387813439541d-1/
   data argip1(4)/ -0.1559576113863552234d-1/
   data argip1(5)/  0.564342939043256481d-2/
   data argip1(6)/ -0.59776844826655809d-3/
   data argip1(7)/ -0.42833850264867728d-3/
   data argip1(8)/  0.22605662380909027d-3/
   data argip1(9)/ -0.3608332945592260d-4/
   data argip1(10)/-0.785518988788901d-5/
   data argip1(11)/ 0.473252480746370d-5/
   data argip1(12)/-0.59743513977694d-6/
   data argip1(13)/-0.15917609165602d-6/
   data argip1(14)/ 0.6336129065570d-7/
   data argip1(15)/-0.276090232648d-8/
   data argip1(16)/-0.256064154085d-8/
   data argip1(17)/ 0.47798676856d-9/
   data argip1(18)/ 0.4488131863d-10/
   data argip1(19)/-0.2346508882d-10/
   data argip1(20)/ 0.76839085d-12/
   data argip1(21)/ 0.73227985d-12/
   data argip1(22)/-0.8513687d-13/
   data argip1(23)/-0.1630201d-13/
   data argip1(24)/ 0.356769d-14/
   data argip1(25)/ 0.25001d-15/
   data argip1(26)/-0.10859d-15/
   data argip1(27)/-0.158d-17/
   data argip1(28)/ 0.275d-17/
   data argip1(29)/-0.5d-19/
   data argip1(30)/-0.6d-19/
   data argip2(0)/  2.00473712275801486391d0/
   data argip2(1)/  0.294184139364406724d-2/
   data argip2(2)/  0.71369249006340167d-3/
   data argip2(3)/  0.17526563430502267d-3/
   data argip2(4)/  0.4359182094029882d-4/
   data argip2(5)/  0.1092626947604307d-4/
   data argip2(6)/  0.272382418399029d-5/
   data argip2(7)/  0.66230900947687d-6/
   data argip2(8)/  0.15425323370315d-6/
   data argip2(9)/  0.3418465242306d-7/
   data argip2(10)/ 0.728157724894d-8/
   data argip2(11)/ 0.151588525452d-8/
   data argip2(12)/ 0.30940048039d-9/
   data argip2(13)/ 0.6149672614d-10/
   data argip2(14)/ 0.1202877045d-10/
   data argip2(15)/ 0.233690586d-11/
   data argip2(16)/ 0.43778068d-12/
   data argip2(17)/ 0.7996447d-13/
   data argip2(18)/ 0.1494075d-13/
   data argip2(19)/ 0.246790d-14/
   data argip2(20)/ 0.37672d-15/
   data argip2(21)/ 0.7701d-16/
   data argip2(22)/ 0.354d-17/
   data argip2(23)/-0.49d-18/
   data argip2(24)/ 0.62d-18/
   data argip2(25)/-0.40d-18/
   data argip2(26)/-0.1d-19/
   data argip2(27)/ 0.2d-19/
   data argip2(28)/-0.3d-19/
   data argip2(29)/ 0.1d-19/
   data argin1(0)/ -0.20118965056732089130d0/
   data argin1(1)/ -0.7244175303324530499d-1/
   data argin1(2)/  0.4505018923894780120d-1/
   data argin1(3)/ -0.24221371122078791099d0/
   data argin1(4)/  0.2717884964361678294d-1/
   data argin1(5)/ -0.5729321004818179697d-1/
   data argin1(6)/ -0.18382107860337763587d0/
   data argin1(7)/  0.7751546082149475511d-1/
   data argin1(8)/  0.18386564733927560387d0/
   data argin1(9)/  0.2921504250185567173d-1/
   data argin1(10)/-0.6142294846788018811d-1/
   data argin1(11)/-0.2999312505794616238d-1/
   data argin1(12)/ 0.585937118327706636d-2/
   data argin1(13)/ 0.822221658497402529d-2/
   data argin1(14)/ 0.132579817166846893d-2/
   data argin1(15)/-0.96248310766565126d-3/
   data argin1(16)/-0.45065515998211807d-3/
   data argin1(17)/ 0.772423474325474d-5/
   data argin1(18)/ 0.5481874134758052d-4/
   data argin1(19)/ 0.1245898039742876d-4/
   data argin1(20)/-0.246196891092083d-5/
   data argin1(21)/-0.169154183545285d-5/
   data argin1(22)/-0.16769153169442d-6/
   data argin1(23)/ 0.9636509337672d-7/
   data argin1(24)/ 0.3253314928030d-7/
   data argin1(25)/ 0.5091804231d-10/
   data argin1(26)/-0.209180453553d-8/
   data argin1(27)/-0.41237387870d-9/
   data argin1(28)/ 0.4163338253d-10/
   data argin1(29)/ 0.3032532117d-10/
   data argin1(30)/ 0.340580529d-11/
   data argin1(31)/-0.88444592d-12/
   data argin1(32)/-0.31639612d-12/
   data argin1(33)/-0.1505076d-13/
   data argin1(34)/ 0.1104148d-13/
   data argin1(35)/ 0.246508d-14/
   data argin1(36)/-0.3107d-16/
   data argin1(37)/-0.9851d-16/
   data argin1(38)/-0.1453d-16/
   data argin1(39)/ 0.118d-17/
   data argin1(40)/ 0.67d-18/
   data argin1(41)/ 0.6d-19/
   data argin1(42)/-0.1d-19/
   data arbin1/1.99983763583586155980d0, &
              -0.8104660923669418d-4, &
               0.13475665984689d-6, &
              -0.70855847143d-9, &
               0.748184187d-11, &
              -0.12902774d-12, &
               0.322504d-14, &
              -0.10809d-15, &
               0.460d-17, &
              -0.24d-18, &
               0.1d-19/
   data arbin2/0.13872356453879120276d0, &
              -0.8239286225558228d-4, &
               0.26720919509866d-6, &
              -0.207423685368d-8, &
               0.2873392593d-10, &
              -0.60873521d-12, &
               0.1792489d-13, &
              -0.68760d-15, &
               0.3280d-16, &
              -0.188d-17, &
               0.13d-18, &
              -0.1d-19/
   data arhin1/1.99647720399779650525d0, &
              -0.187563779407173213d-2, &
              -0.12186470897787339d-3, &
              -0.814021609659287d-5, &
              -0.55050925953537d-6, &
              -0.3763008043303d-7, &
              -0.258858362365d-8, &
              -0.17931829265d-9, &
              -0.1245916873d-10, &
              -0.87171247d-12, &
              -0.6084943d-13, &
              -0.431178d-14, &
              -0.29787d-15, &
              -0.2210d-16, &
              -0.136d-17, &
              -0.14d-18/
   data twent8,seven2/ 28.0d0 , 72.0d0 /
   data one76,five14/ 176.0d0 , 514.0d0 /
   data one024,twelhu/ 1024.0d0, 1200.0d0 /
   data gizero/0.20497554248200024505d0/
   data onebpi/0.31830988618379067154d0/
   data rtpiin/0.56418958354775628695d0/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !
   data nterm4,nterm5,nterm6/9,10,14/

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < -xneg2m * xneg2m ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'AIRY_GI - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument too negative for accurate computation.'
     airy_gi = zero
   else if ( x <= xhhh3 ) then
     xminus = -x
     t = xminus * sqrt ( xminus )
     zeta = ( t + t ) / three
     temp = rtpiin / sqrt ( sqrt ( xminus ) )
     cosz = cos ( zeta + piby4 )
     sinz = sin ( zeta + piby4 ) / zeta
     xcube = x * x * x
     bi = ( cosz + sinz * five / seven2 ) * temp
     t = ( xcube + twelhu ) / ( one76 - xcube )
     airy_gi = bi + cheval ( nterm6, arhin1, t ) * onebpi / x
   else if ( x < minate ) then
     xminus = -x
     t = xminus * sqrt ( xminus )
     zeta = ( t + t ) / three
     temp = rtpiin / sqrt ( sqrt ( xminus ) )
     cosz = cos ( zeta + piby4 )
     sinz = sin ( zeta + piby4 ) / zeta
     xcube = x * x * x
     t = - ( one024 / ( xcube ) + one )
     cheb1 = cheval ( nterm4, arbin1, t )
     cheb2 = cheval ( nterm5, arbin2, t )
     bi = ( cosz * cheb1 + sinz * cheb2 ) * temp
     t = ( xcube + twelhu ) / ( one76 - xcube )
     airy_gi = bi + cheval ( nterm6, arhin1, t ) * onebpi / x
   else if ( x <= -eps_DP ) then
     t = -( x + four ) / four
     airy_gi = cheval ( nterm3, argin1, t )
   else if ( x < eps_DP ) then
     airy_gi = gizero
   else if ( x <= seven ) then
     t = ( nine * x - twent8 ) / ( x + twent8 )
     airy_gi = cheval ( nterm1, argip1, t )
   else if ( x <= xneg2m ) then
     xcube = x * x * x
     t = ( twelhu - xcube ) / ( five14 + xcube )
     airy_gi = onebpi * cheval ( nterm2, argip2, t ) / x
   else if ( x <= xhi11 ) then
     airy_gi = onebpi / x
   else
     airy_gi = zero
   endif

   return
 end function airy_gi
 !******************************************************************
 !>>>airy_hi.f90
 function airy_hi(xvalue)

 !*****************************************************************************80
 !
 !! AIRY_HI computes the modified Airy Function Hi(x).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      AIRY_HI(x) = Integral ( 0 <= t < infinity ) exp(x*t-t^3/3) dt / pi
 !
 !    The approximation uses Chebyshev expansions with the coefficients
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: AIRY_HI, the value of the FUNCTION.
 !
   implicit none

   real(DP),parameter :: xhigh1_HI=104.4175d0
   real(kind = DP) :: airy_hi
   integer, parameter :: nterm1 = 29
   integer, parameter :: nterm2 = 17
   integer, parameter :: nterm3 = 22
   integer :: nterm4
   integer :: nterm5
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arhip(0:31),arbip(0:23),argip1(0:29), arhin1(0:21),arhin2(0:15)
   real(DP) :: bi,five14,gi,hizero,lnrtpi, onebpi,one76,t,temp, thre43,twelhu,xcube,zeta
   data arhip(0)/ 1.24013562561762831114d0/
   data arhip(1)/ 0.64856341973926535804d0/
   data arhip(2)/ 0.55236252592114903246d0/
   data arhip(3)/ 0.20975122073857566794d0/
   data arhip(4)/ 0.12025669118052373568d0/
   data arhip(5)/ 0.3768224931095393785d-1/
   data arhip(6)/ 0.1651088671548071651d-1/
   data arhip(7)/ 0.455922755211570993d-2/
   data arhip(8)/ 0.161828480477635013d-2/
   data arhip(9)/ 0.40841282508126663d-3/
   data arhip(10)/0.12196479721394051d-3/
   data arhip(11)/0.2865064098657610d-4/
   data arhip(12)/0.742221556424344d-5/
   data arhip(13)/0.163536231932831d-5/
   data arhip(14)/0.37713908188749d-6/
   data arhip(15)/0.7815800336008d-7/
   data arhip(16)/0.1638447121370d-7/
   data arhip(17)/0.319857665992d-8/
   data arhip(18)/0.61933905307d-9/
   data arhip(19)/0.11411161191d-9/
   data arhip(20)/0.2064923454d-10/
   data arhip(21)/0.360018664d-11/
   data arhip(22)/0.61401849d-12/
   data arhip(23)/0.10162125d-12/
   data arhip(24)/0.1643701d-13/
   data arhip(25)/0.259084d-14/
   data arhip(26)/0.39931d-15/
   data arhip(27)/0.6014d-16/
   data arhip(28)/0.886d-17/
   data arhip(29)/0.128d-17/
   data arhip(30)/0.18d-18/
   data arhip(31)/0.3d-19/
   data arbip(0)/  2.00582138209759064905d0/
   data arbip(1)/  0.294478449170441549d-2/
   data arbip(2)/  0.3489754514775355d-4/
   data arbip(3)/  0.83389733374343d-6/
   data arbip(4)/  0.3136215471813d-7/
   data arbip(5)/  0.167865306015d-8/
   data arbip(6)/  0.12217934059d-9/
   data arbip(7)/  0.1191584139d-10/
   data arbip(8)/  0.154142553d-11/
   data arbip(9)/  0.24844455d-12/
   data arbip(10)/ 0.4213012d-13/
   data arbip(11)/ 0.505293d-14/
   data arbip(12)/-0.60032d-15/
   data arbip(13)/-0.65474d-15/
   data arbip(14)/-0.22364d-15/
   data arbip(15)/-0.3015d-16/
   data arbip(16)/ 0.959d-17/
   data arbip(17)/ 0.616d-17/
   data arbip(18)/ 0.97d-18/
   data arbip(19)/-0.37d-18/
   data arbip(20)/-0.21d-18/
   data arbip(21)/-0.1d-19/
   data arbip(22)/ 0.2d-19/
   data arbip(23)/ 0.1d-19/
   data argip1(0)/  2.00473712275801486391d0/
   data argip1(1)/  0.294184139364406724d-2/
   data argip1(2)/  0.71369249006340167d-3/
   data argip1(3)/  0.17526563430502267d-3/
   data argip1(4)/  0.4359182094029882d-4/
   data argip1(5)/  0.1092626947604307d-4/
   data argip1(6)/  0.272382418399029d-5/
   data argip1(7)/  0.66230900947687d-6/
   data argip1(8)/  0.15425323370315d-6/
   data argip1(9)/  0.3418465242306d-7/
   data argip1(10)/ 0.728157724894d-8/
   data argip1(11)/ 0.151588525452d-8/
   data argip1(12)/ 0.30940048039d-9/
   data argip1(13)/ 0.6149672614d-10/
   data argip1(14)/ 0.1202877045d-10/
   data argip1(15)/ 0.233690586d-11/
   data argip1(16)/ 0.43778068d-12/
   data argip1(17)/ 0.7996447d-13/
   data argip1(18)/ 0.1494075d-13/
   data argip1(19)/ 0.246790d-14/
   data argip1(20)/ 0.37672d-15/
   data argip1(21)/ 0.7701d-16/
   data argip1(22)/ 0.354d-17/
   data argip1(23)/-0.49d-18/
   data argip1(24)/ 0.62d-18/
   data argip1(25)/-0.40d-18/
   data argip1(26)/-0.1d-19/
   data argip1(27)/ 0.2d-19/
   data argip1(28)/-0.3d-19/
   data argip1(29)/ 0.1d-19/
   data arhin1(0)/  0.31481017206423404116d0/
   data arhin1(1)/ -0.16414499216588964341d0/
   data arhin1(2)/  0.6176651597730913071d-1/
   data arhin1(3)/ -0.1971881185935933028d-1/
   data arhin1(4)/  0.536902830023331343d-2/
   data arhin1(5)/ -0.124977068439663038d-2/
   data arhin1(6)/  0.24835515596994933d-3/
   data arhin1(7)/ -0.4187024096746630d-4/
   data arhin1(8)/  0.590945437979124d-5/
   data arhin1(9)/ -0.68063541184345d-6/
   data arhin1(10)/ 0.6072897629164d-7/
   data arhin1(11)/-0.367130349242d-8/
   data arhin1(12)/ 0.7078017552d-10/
   data arhin1(13)/ 0.1187894334d-10/
   data arhin1(14)/-0.120898723d-11/
   data arhin1(15)/ 0.1189656d-13/
   data arhin1(16)/ 0.594128d-14/
   data arhin1(17)/-0.32257d-15/
   data arhin1(18)/-0.2290d-16/
   data arhin1(19)/ 0.253d-17/
   data arhin1(20)/ 0.9d-19/
   data arhin1(21)/-0.2d-19/
   data arhin2/1.99647720399779650525d0, &
              -0.187563779407173213d-2, &
              -0.12186470897787339d-3, &
              -0.814021609659287d-5, &
              -0.55050925953537d-6, &
              -0.3763008043303d-7, &
              -0.258858362365d-8, &
              -0.17931829265d-9, &
              -0.1245916873d-10, &
              -0.87171247d-12, &
              -0.6084943d-13, &
              -0.431178d-14, &
              -0.29787d-15, &
              -0.2210d-16, &
              -0.136d-17, &
              -0.14d-18/
   data one76/ 176.0d0 /
   data thre43,five14,twelhu/ 343.0d0, 514.0d0, 1200.0d0 /
   data hizero/0.40995108496400049010d0/
   data lnrtpi/0.57236494292470008707d0/
   data onebpi/0.31830988618379067154d0/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !
   data nterm4,nterm5/19,14/

   x = xvalue

   if ( xhigh1_HI < x ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'AIRY_HI - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument too large.'
     airy_hi = xmax
     return
   endif
 !
 !  Code for x < 0.0
 !
   if ( x < zero ) then

     if ( x < minate ) then

       if ( x < xneg11 ) then
         airy_hi = zero
       else
         if ( x < xneg2 ) then
           temp = one
           airy_hi = - temp * onebpi / x
         else
           xcube = x * x * x
           t = ( xcube + twelhu ) / ( one76 - xcube )
           temp = cheval ( nterm5, arhin2, t )
           airy_hi = - temp * onebpi / x
         endif

       endif
     else
       if ( -eps_DP < x ) then
         airy_hi = hizero
       else
         t = ( four * x + twelve ) / ( x - twelve )
         airy_hi = cheval ( nterm4, arhin1, t )
       endif

     endif
 !
 !   Code for x >= 0.0
 !
   else

     if ( x <= seven ) then
       if ( x < eps_DP ) then
         airy_hi = hizero
       else
         t = ( x + x ) / seven - one
         temp = ( x + x + x ) / two
         airy_hi = exp ( temp ) * cheval ( nterm1, arhip, t )
       endif
     else
       xcube = x * x * x
       temp = sqrt ( xcube )
       zeta = ( temp + temp ) / three
       t = two * ( sqrt ( thre43 / xcube ) ) - one
       temp = cheval ( nterm2, arbip, t )
       temp = zeta + log ( temp ) - log ( x ) / four - lnrtpi
       bi = exp ( temp )
       t = ( twelhu - xcube ) / ( xcube + five14 )
       gi = cheval ( nterm3, argip1, t ) * onebpi / x
       airy_hi = bi - gi
     endif

   endif

   return
 end function airy_hi
 !******************************************************************
 !>>>arctan_Integral.f90
 function arctan_Integral(xvalue)

 !*****************************************************************************80
 !
 !! ARCTAN_INT calculates the inverse tangent integral.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      ARCTAN_INT(x) = Integral ( 0 <= t <= x ) arctan ( t ) / t dt
 !
 !    The approximation uses Chebyshev series with the coefficients
 !    given to an accuracy of 20D.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    24 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: ARCTAN_INT, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP), dimension ( 0:22 ) :: atnina = (/ &
      1.91040361296235937512d0, &
     -0.4176351437656746940d-1, &
      0.275392550786367434d-2, &
     -0.25051809526248881d-3, &
      0.2666981285121171d-4, &
     -0.311890514107001d-5, &
      0.38833853132249d-6, &
     -0.5057274584964d-7, &
      0.681225282949d-8, &
     -0.94212561654d-9, &
      0.13307878816d-9, &
     -0.1912678075d-10, &
      0.278912620d-11, &
     -0.41174820d-12, &
      0.6142987d-13, &
     -0.924929d-14, &
      0.140387d-14, &
     -0.21460d-15, &
      0.3301d-16, &
     -0.511d-17, &
      0.79d-18, &
     -0.12d-18, &
      0.2d-19 /)
   real(kind = DP) :: arctan_Integral
   integer :: ind
   integer, parameter :: nterms = 19
   real(kind = DP) :: t
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   ind = 1
   x = xvalue

   if ( x < zero ) then
     x = -x
     ind = -1
   endif

   if ( x < half*sceps_DP ) then
     arctan_Integral = x
   else if ( x <= one ) then
     t = x * x
     t =  ( t - half ) + ( t - half )
     arctan_Integral = x * cheval ( nterms, atnina, t )
   else if ( x <= xupper ) then
     t = one / ( x * x )
     t = ( t - half ) + ( t - half )
     arctan_Integral = log ( x ) / twobpi + cheval ( nterms, atnina, t ) / x
   else
     arctan_Integral = log ( x ) / twobpi
   endif

   if ( ind < 0 ) then
     arctan_Integral = -arctan_Integral
   endif

   return
 end function arctan_Integral
 !******************************************************************
 !>>>bessel_i0_Integral.f90
 function bessel_i0_Integral(xvalue)

 !*****************************************************************************80
 !
 !! BESSEL_I0_INT computes the integral of the modified Bessel Function I0(X).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      I0_INT(x) = Integral ( 0 <= t <= x ) I0(t) dt
 !
 !    The program uses Chebyshev expansions, the coefficients of
 !    which are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: BESSEL_I0_INT, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: ari01(0:28)
   real(kind = DP) :: ari0a(0:33)
   real(kind = DP) :: bessel_i0_Integral
   integer :: ind
   real(kind = DP), parameter :: lnr2pi = 0.91893853320467274178D+00
   integer, parameter :: nterm1 = 25
   integer, parameter :: nterm2 = 27
   real(kind = DP) :: t
   real(kind = DP) :: temp
   real(kind = DP), parameter :: thirt6 = 36.0D+00
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   data ari01(0)/  0.41227906926781516801d0/
   data ari01(1)/ -0.34336345150081519562d0/
   data ari01(2)/  0.22667588715751242585d0/
   data ari01(3)/ -0.12608164718742260032d0/
   data ari01(4)/  0.6012484628777990271d-1/
   data ari01(5)/ -0.2480120462913358248d-1/
   data ari01(6)/  0.892773389565563897d-2/
   data ari01(7)/ -0.283253729936696605d-2/
   data ari01(8)/  0.79891339041712994d-3/
   data ari01(9)/ -0.20053933660964890d-3/
   data ari01(10)/ 0.4416816783014313d-4/
   data ari01(11)/-0.822377042246068d-5/
   data ari01(12)/ 0.120059794219015d-5/
   data ari01(13)/-0.11350865004889d-6/
   data ari01(14)/ 0.69606014466d-9/
   data ari01(15)/ 0.180622772836d-8/
   data ari01(16)/-0.26039481370d-9/
   data ari01(17)/-0.166188103d-11/
   data ari01(18)/ 0.510500232d-11/
   data ari01(19)/-0.41515879d-12/
   data ari01(20)/-0.7368138d-13/
   data ari01(21)/ 0.1279323d-13/
   data ari01(22)/ 0.103247d-14/
   data ari01(23)/-0.30379d-15/
   data ari01(24)/-0.1789d-16/
   data ari01(25)/ 0.673d-17/
   data ari01(26)/ 0.44d-18/
   data ari01(27)/-0.14d-18/
   data ari01(28)/-0.1d-19/
   data ari0a(0)/  2.03739654571143287070d0/
   data ari0a(1)/  0.1917631647503310248d-1/
   data ari0a(2)/  0.49923334519288147d-3/
   data ari0a(3)/  0.2263187103659815d-4/
   data ari0a(4)/  0.158682108285561d-5/
   data ari0a(5)/  0.16507855636318d-6/
   data ari0a(6)/  0.2385058373640d-7/
   data ari0a(7)/  0.392985182304d-8/
   data ari0a(8)/  0.46042714199d-9/
   data ari0a(9)/ -0.7072558172d-10/
   data ari0a(10)/-0.6747183961d-10/
   data ari0a(11)/-0.2026962001d-10/
   data ari0a(12)/-0.87320338d-12/
   data ari0a(13)/ 0.175520014d-11/
   data ari0a(14)/ 0.60383944d-12/
   data ari0a(15)/-0.3977983d-13/
   data ari0a(16)/-0.8049048d-13/
   data ari0a(17)/-0.1158955d-13/
   data ari0a(18)/ 0.827318d-14/
   data ari0a(19)/ 0.282290d-14/
   data ari0a(20)/-0.77667d-15/
   data ari0a(21)/-0.48731d-15/
   data ari0a(22)/ 0.7279d-16/
   data ari0a(23)/ 0.7873d-16/
   data ari0a(24)/-0.785d-17/
   data ari0a(25)/-0.1281d-16/
   data ari0a(26)/ 0.121d-17/
   data ari0a(27)/ 0.214d-17/
   data ari0a(28)/-0.27d-18/
   data ari0a(29)/-0.36d-18/
   data ari0a(30)/ 0.7d-19/
   data ari0a(31)/ 0.6d-19/
   data ari0a(32)/-0.2d-19/
   data ari0a(33)/-0.1d-19/

   if (.not.setupMISC_done) call setupMISC
   ind = 1
   x = xvalue

   if ( xvalue < zero ) then
     ind = -1
     x = -x
   endif

   if ( x < sr3*two*sceps_DP ) then
     bessel_i0_Integral = x
   else if ( x <= ateen ) then
     t = ( three * x - ateen ) / ( x + ateen )
     bessel_i0_Integral = x * exp ( x ) * cheval(nterm1,ari01,t)
   else if ( x <= xhighP ) then
     t = ( thirt6 / x - half ) - half
     temp = x - half * log ( x ) - lnr2pi + log(cheval(nterm2,ari0a,t))
     bessel_i0_Integral = exp ( temp )
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'BESSEL_I0_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large.'
     bessel_i0_Integral = exp ( xhighP - lnr2pi - half * log ( xhighP ) )
   endif

   if ( ind == -1 ) then
     bessel_i0_Integral = -bessel_i0_Integral
   endif

   return
 end function bessel_i0_Integral
 !******************************************************************
 !>>>bessel_j0_Integral.f90
 function bessel_j0_Integral(xvalue)

 !*****************************************************************************80
 !
 !! BESSEL_J0_INT calculates the integral of the Bessel Function J0.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      J0_INT(x) = Integral ( 0 <= t <= x ) J0(t) dt
 !
 !    The code uses Chebyshev expansions whose coefficients are
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: BESSEL_J0_INT, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: bessel_j0_Integral
   integer :: ind
   integer, parameter :: nterm1 = 22
   integer, parameter :: nterm2 = 18
   integer, parameter :: nterm3 = 16
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arj01(0:23),arj0a1(0:21),arj0a2(0:18), &
        five12,one28,pib41,pib411,pib412, &
        pib42,rt2bpi,t,temp, &
        xmpi4
   data one28,five12/ 128.0d0 , 512d0 /
   data rt2bpi/0.79788456080286535588d0/
   data pib411,pib412/ 201.0d0 , 256.0d0/
   data pib42/0.24191339744830961566d-3/
   data arj01(0)/  0.38179279321690173518d0/
   data arj01(1)/ -0.21275636350505321870d0/
   data arj01(2)/  0.16754213407215794187d0/
   data arj01(3)/ -0.12853209772196398954d0/
   data arj01(4)/  0.10114405455778847013d0/
   data arj01(5)/ -0.9100795343201568859d-1/
   data arj01(6)/  0.6401345264656873103d-1/
   data arj01(7)/ -0.3066963029926754312d-1/
   data arj01(8)/  0.1030836525325064201d-1/
   data arj01(9)/ -0.255670650399956918d-2/
   data arj01(10)/ 0.48832755805798304d-3/
   data arj01(11)/-0.7424935126036077d-4/
   data arj01(12)/ 0.922260563730861d-5/
   data arj01(13)/-0.95522828307083d-6/
   data arj01(14)/ 0.8388355845986d-7/
   data arj01(15)/-0.633184488858d-8/
   data arj01(16)/ 0.41560504221d-9/
   data arj01(17)/-0.2395529307d-10/
   data arj01(18)/ 0.122286885d-11/
   data arj01(19)/-0.5569711d-13/
   data arj01(20)/ 0.227820d-14/
   data arj01(21)/-0.8417d-16/
   data arj01(22)/ 0.282d-17/
   data arj01(23)/-0.9d-19/
   data arj0a1(0)/  1.24030133037518970827d0/
   data arj0a1(1)/ -0.478125353632280693d-2/
   data arj0a1(2)/  0.6613148891706678d-4/
   data arj0a1(3)/ -0.186042740486349d-5/
   data arj0a1(4)/  0.8362735565080d-7/
   data arj0a1(5)/ -0.525857036731d-8/
   data arj0a1(6)/  0.42606363251d-9/
   data arj0a1(7)/ -0.4211761024d-10/
   data arj0a1(8)/  0.488946426d-11/
   data arj0a1(9)/ -0.64834929d-12/
   data arj0a1(10)/ 0.9617234d-13/
   data arj0a1(11)/-0.1570367d-13/
   data arj0a1(12)/ 0.278712d-14/
   data arj0a1(13)/-0.53222d-15/
   data arj0a1(14)/ 0.10844d-15/
   data arj0a1(15)/-0.2342d-16/
   data arj0a1(16)/ 0.533d-17/
   data arj0a1(17)/-0.127d-17/
   data arj0a1(18)/ 0.32d-18/
   data arj0a1(19)/-0.8d-19/
   data arj0a1(20)/ 0.2d-19/
   data arj0a1(21)/-0.1d-19/
   data arj0a2(0)/  1.99616096301341675339d0/
   data arj0a2(1)/ -0.190379819246668161d-2/
   data arj0a2(2)/  0.1539710927044226d-4/
   data arj0a2(3)/ -0.31145088328103d-6/
   data arj0a2(4)/  0.1110850971321d-7/
   data arj0a2(5)/ -0.58666787123d-9/
   data arj0a2(6)/  0.4139926949d-10/
   data arj0a2(7)/ -0.365398763d-11/
   data arj0a2(8)/  0.38557568d-12/
   data arj0a2(9)/ -0.4709800d-13/
   data arj0a2(10)/ 0.650220d-14/
   data arj0a2(11)/-0.99624d-15/
   data arj0a2(12)/ 0.16700d-15/
   data arj0a2(13)/-0.3028d-16/
   data arj0a2(14)/ 0.589d-17/
   data arj0a2(15)/-0.122d-17/
   data arj0a2(16)/ 0.27d-18/
   data arj0a2(17)/-0.6d-19/
   data arj0a2(18)/ 0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   x = xvalue
   ind = 1

   if ( x < zero ) then
     x = -x
     ind = -1
   endif

   if ( x < sr2*sr3*sceps_DP ) then
     bessel_j0_Integral = x
   else if ( x <= sixteen ) then
     t = x * x / one28 - one
     bessel_j0_Integral = x * cheval ( nterm1, arj01, t )
   else if ( x <= sorgo(2) ) then
     t = five12 / ( x * x ) - one
     pib41 = pib411 / pib412
     xmpi4 = ( x - pib41 ) - pib42
     temp = cos ( xmpi4 ) * cheval ( nterm2, arj0a1, t ) / x
     temp = temp - sin ( xmpi4) * cheval ( nterm3, arj0a2, t )
     bessel_j0_Integral = one - rt2bpi * temp / sqrt ( x )
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'BESSEL_J0_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large.'
     bessel_j0_Integral = one
   endif

   if ( ind == -1 ) then
     bessel_j0_Integral = -bessel_j0_Integral
   endif

   return
 end function bessel_j0_Integral
 !******************************************************************
 !>>>bessel_k0_Integral.f90
 function bessel_k0_Integral(xvalue)

 !*****************************************************************************80
 !
 !! BESSEL_K0_INT calculates the integral of the modified Bessel Function K0(X).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      K0_INT(x) = Integral ( 0 <= t <= x ) K0(t) dt
 !
 !    The code uses Chebyshev expansions, whose coefficients are
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: BESSEL_K0_INT, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: bessel_k0_Integral
   integer, parameter :: nterm1 = 14
   integer, parameter :: nterm2 = 14
   integer, parameter :: nterm3 = 23
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: ak0in1(0:15),ak0in2(0:15),ak0ina(0:27), &
        const1,const2,fval, &
        rt2bpi,t,temp
   data const1/1.11593151565841244881d0/
   data const2/-0.11593151565841244881d0/
   data rt2bpi/0.79788456080286535588d0/
   data ak0in1/16.79702714464710959477d0, &
                9.79134687676889407070d0, &
                2.80501316044337939300d0, &
                0.45615620531888502068d0, &
                0.4716224457074760784d-1, &
                0.335265148269698289d-2, &
                0.17335181193874727d-3, &
                0.679951889364702d-5, &
                0.20900268359924d-6, &
                0.516603846976d-8, &
                0.10485708331d-9, &
                0.177829320d-11, &
                0.2556844d-13, &
                0.31557d-15, &
                0.338d-17, &
                0.3d-19/
   data ak0in2/10.76266558227809174077d0, &
                5.62333479849997511550d0, &
                1.43543664879290867158d0, &
                0.21250410143743896043d0, &
                0.2036537393100009554d-1, &
                0.136023584095623632d-2, &
                0.6675388699209093d-4, &
                0.250430035707337d-5, &
                0.7406423741728d-7, &
                0.176974704314d-8, &
                0.3485775254d-10, &
                0.57544785d-12, &
                0.807481d-14, &
                0.9747d-16, &
                0.102d-17, &
                0.1d-19/
   data ak0ina(0)/  1.91172065445060453895d0/
   data ak0ina(1)/ -0.4183064565769581085d-1/
   data ak0ina(2)/  0.213352508068147486d-2/
   data ak0ina(3)/ -0.15859497284504181d-3/
   data ak0ina(4)/  0.1497624699858351d-4/
   data ak0ina(5)/ -0.167955955322241d-5/
   data ak0ina(6)/  0.21495472478804d-6/
   data ak0ina(7)/ -0.3058356654790d-7/
   data ak0ina(8)/  0.474946413343d-8/
   data ak0ina(9)/ -0.79424660432d-9/
   data ak0ina(10)/ 0.14156555325d-9/
   data ak0ina(11)/-0.2667825359d-10/
   data ak0ina(12)/ 0.528149717d-11/
   data ak0ina(13)/-0.109263199d-11/
   data ak0ina(14)/ 0.23518838d-12/
   data ak0ina(15)/-0.5247991d-13/
   data ak0ina(16)/ 0.1210191d-13/
   data ak0ina(17)/-0.287632d-14/
   data ak0ina(18)/ 0.70297d-15/
   data ak0ina(19)/-0.17631d-15/
   data ak0ina(20)/ 0.4530d-16/
   data ak0ina(21)/-0.1190d-16/
   data ak0ina(22)/ 0.319d-17/
   data ak0ina(23)/-0.87d-18/
   data ak0ina(24)/ 0.24d-18/
   data ak0ina(25)/-0.7d-19/
   data ak0ina(26)/ 0.2d-19/
   data ak0ina(27)/-0.1d-19/
 !
 !   Machine-dependent values (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'BESSEL_K0_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     bessel_k0_Integral = zero
   else if ( x == zero ) then
     bessel_k0_Integral = zero
   else if ( x < three*sceps_DP ) then
     bessel_k0_Integral = x * ( const1 - log ( x ) )
   else if ( x <= six ) then
     t = ( ( x * x ) / eighteen - half ) - half
     fval = ( const2 + log ( x ) ) * cheval ( nterm2, ak0in2, t )
     bessel_k0_Integral = x * ( cheval ( nterm1, ak0in1, t ) - fval )
   else if ( x < xhighL1 ) then
     fval = piby2
     t = ( twelve / x - half ) - half
     temp = exp ( -x ) * cheval ( nterm3, ak0ina, t )
     fval = fval - temp / ( sqrt ( x ) * rt2bpi )
     bessel_k0_Integral = fval
   else
     bessel_k0_Integral = piby2
   endif

   return
 end function bessel_k0_Integral
 !******************************************************************
 !>>>bessel_y0_Integral.f90
 function bessel_y0_Integral(xvalue)

 !*****************************************************************************80
 !
 !! BESSEL_Y0_INT calculates the integral of the Bessel Function Y0.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      Y0_INT(x) = Integral ( 0 <= t <= x ) Y0(t) dt
 !
 !    The code uses Chebyshev expansions whose coefficients are
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    23 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: BESSEL_Y0_INT, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: bessel_y0_Integral
   integer, parameter :: nterm1 = 22
   integer, parameter :: nterm2 = 22
   integer, parameter :: nterm3 = 17
   integer, parameter :: nterm4 = 15
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arj01(0:23),ary01(0:24),ary0a1(0:21), &
        ary0a2(0:18),five12,gal2m1,gamln2, &
        one28,pib41,pib411,pib412, &
        pib42,rt2bpi,t,temp, xmpi4
   data one28,five12/ 128.0d0 , 512.0d0 /
   data rt2bpi/0.79788456080286535588d0/
   data pib411,pib412/ 201.0d0, 256.0d0/
   data pib42/0.24191339744830961566d-3/
   data gal2m1/-1.11593151565841244881d0/
   data gamln2/-0.11593151565841244881d0/
   data arj01(0)/  0.38179279321690173518d0/
   data arj01(1)/ -0.21275636350505321870d0/
   data arj01(2)/  0.16754213407215794187d0/
   data arj01(3)/ -0.12853209772196398954d0/
   data arj01(4)/  0.10114405455778847013d0/
   data arj01(5)/ -0.9100795343201568859d-1/
   data arj01(6)/  0.6401345264656873103d-1/
   data arj01(7)/ -0.3066963029926754312d-1/
   data arj01(8)/  0.1030836525325064201d-1/
   data arj01(9)/ -0.255670650399956918d-2/
   data arj01(10)/ 0.48832755805798304d-3/
   data arj01(11)/-0.7424935126036077d-4/
   data arj01(12)/ 0.922260563730861d-5/
   data arj01(13)/-0.95522828307083d-6/
   data arj01(14)/ 0.8388355845986d-7/
   data arj01(15)/-0.633184488858d-8/
   data arj01(16)/ 0.41560504221d-9/
   data arj01(17)/-0.2395529307d-10/
   data arj01(18)/ 0.122286885d-11/
   data arj01(19)/-0.5569711d-13/
   data arj01(20)/ 0.227820d-14/
   data arj01(21)/-0.8417d-16/
   data arj01(22)/ 0.282d-17/
   data arj01(23)/-0.9d-19/
   data ary01(0)/  0.54492696302724365490d0/
   data ary01(1)/ -0.14957323588684782157d0/
   data ary01(2)/  0.11085634486254842337d0/
   data ary01(3)/ -0.9495330018683777109d-1/
   data ary01(4)/  0.6820817786991456963d-1/
   data ary01(5)/ -0.10324653383368200408d0/
   data ary01(6)/  0.10625703287534425491d0/
   data ary01(7)/ -0.6258367679961681990d-1/
   data ary01(8)/  0.2385645760338293285d-1/
   data ary01(9)/ -0.644864913015404481d-2/
   data ary01(10)/ 0.131287082891002331d-2/
   data ary01(11)/-0.20988088174989640d-3/
   data ary01(12)/ 0.2716042484138347d-4/
   data ary01(13)/-0.291199114014694d-5/
   data ary01(14)/ 0.26344333093795d-6/
   data ary01(15)/-0.2041172069780d-7/
   data ary01(16)/ 0.137124781317d-8/
   data ary01(17)/-0.8070680792d-10/
   data ary01(18)/ 0.419883057d-11/
   data ary01(19)/-0.19459104d-12/
   data ary01(20)/ 0.808782d-14/
   data ary01(21)/-0.30329d-15/
   data ary01(22)/ 0.1032d-16/
   data ary01(23)/-0.32d-18/
   data ary01(24)/ 0.1d-19/
   data ary0a1(0)/  1.24030133037518970827d0/
   data ary0a1(1)/ -0.478125353632280693d-2/
   data ary0a1(2)/  0.6613148891706678d-4/
   data ary0a1(3)/ -0.186042740486349d-5/
   data ary0a1(4)/  0.8362735565080d-7/
   data ary0a1(5)/ -0.525857036731d-8/
   data ary0a1(6)/  0.42606363251d-9/
   data ary0a1(7)/ -0.4211761024d-10/
   data ary0a1(8)/  0.488946426d-11/
   data ary0a1(9)/ -0.64834929d-12/
   data ary0a1(10)/ 0.9617234d-13/
   data ary0a1(11)/-0.1570367d-13/
   data ary0a1(12)/ 0.278712d-14/
   data ary0a1(13)/-0.53222d-15/
   data ary0a1(14)/ 0.10844d-15/
   data ary0a1(15)/-0.2342d-16/
   data ary0a1(16)/ 0.533d-17/
   data ary0a1(17)/-0.127d-17/
   data ary0a1(18)/ 0.32d-18/
   data ary0a1(19)/-0.8d-19/
   data ary0a1(20)/ 0.2d-19/
   data ary0a1(21)/-0.1d-19/
   data ary0a2(0)/  1.99616096301341675339d0/
   data ary0a2(1)/ -0.190379819246668161d-2/
   data ary0a2(2)/  0.1539710927044226d-4/
   data ary0a2(3)/ -0.31145088328103d-6/
   data ary0a2(4)/  0.1110850971321d-7/
   data ary0a2(5)/ -0.58666787123d-9/
   data ary0a2(6)/  0.4139926949d-10/
   data ary0a2(7)/ -0.365398763d-11/
   data ary0a2(8)/  0.38557568d-12/
   data ary0a2(9)/ -0.4709800d-13/
   data ary0a2(10)/ 0.650220d-14/
   data ary0a2(11)/-0.99624d-15/
   data ary0a2(12)/ 0.16700d-15/
   data ary0a2(13)/-0.3028d-16/
   data ary0a2(14)/ 0.589d-17/
   data ary0a2(15)/-0.122d-17/
   data ary0a2(16)/ 0.27d-18/
   data ary0a2(17)/-0.6d-19/
   data ary0a2(18)/ 0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'BESSEL_Y0_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     bessel_y0_Integral = zero
   else if ( x == zero ) then
     bessel_y0_Integral = zero
   else if ( x < three*sceps_DP*srhalf ) then
     bessel_y0_Integral = ( log ( x ) + gal2m1 ) * twobpi * x
   else if ( x <= sixteen ) then
     t = x * x / one28 - one
     temp = ( log ( x ) + gamln2 ) * cheval ( nterm1, arj01, t )
     temp = temp - cheval ( nterm2, ary01, t )
     bessel_y0_Integral = twobpi * x * temp
   else if ( x <= sorgo(2) ) then
     t = five12 / ( x * x ) - one
     pib41 = pib411 / pib412
     xmpi4 = ( x - pib41 ) - pib42
     temp = sin ( xmpi4 ) * cheval ( nterm3, ary0a1, t ) / x
     temp = temp + cos ( xmpi4 ) * cheval ( nterm4, ary0a2, t )
     bessel_y0_Integral = - rt2bpi * temp / sqrt ( x )
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'BESSEL_Y0_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument too large.'
     bessel_y0_Integral = zero
   endif

   return
 end function bessel_y0_Integral

 !******************************************************************
 !>>>clausen.f90
 function clausen(xvalue)

 !*****************************************************************************80
 !
 !! CLAUSEN calculates Clausen's integral.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      CLAUSEN(x) = Integral ( 0 <= t <= x ) -ln ( 2 * sin ( t / 2 ) ) dt
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: CLAUSEN, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: aclaus(0:15)
   real(kind = DP) :: clausen
   integer :: indx
   integer, parameter :: nterms = 13
   real(kind = DP) :: t
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: twopia,twopib
   data twopia,twopib/6.28125d0, 0.19353071795864769253d-2/
   data aclaus/2.14269436376668844709d0, &
               0.7233242812212579245d-1, &
               0.101642475021151164d-2, &
               0.3245250328531645d-4, &
               0.133315187571472d-5, &
               0.6213240591653d-7, &
               0.313004135337d-8, &
               0.16635723056d-9, &
               0.919659293d-11, &
               0.52400462d-12, &
               0.3058040d-13, &
               0.181969d-14, &
               0.11004d-15, &
               0.675d-17, &
               0.42d-18, &
               0.3d-19/
 !
 !  Set machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( sorgo(1) < abs(x) ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'CLAUSEN - Warning!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large for accurate computation.'
     clausen = zero
     return
   endif

   indx = 1
   if ( x < zero ) then
     x = -x
     indx = -1
   endif
 !
 !  Argument reduced using simulated extra precision
 !
   if ( pi2 < x ) then
     t = aint ( x / pi2 )
     x =  ( x - t * twopia ) - t * twopib
   endif

   if ( pi < x ) then
     x = ( twopia - x ) + twopib
     indx = -indx
   endif

   if ( x == zero ) then
     clausen = zero
   else if ( x < xsmallx ) then
     clausen = x * ( one - log ( x ) )
   else
     t = ( x * x ) / pisq - half
     t = t + t
     if ( one < t ) then
       t = one
     endif

     clausen = x * cheval ( nterms, aclaus, t ) - x * log ( x )

   endif

   if ( indx < 0 ) then
     clausen = -clausen
   endif

   return
 end function clausen
 !******************************************************************
 !>>>debye1.f90
 function debye1(xvalue)

 !*****************************************************************************80
 !
 !! DEBYE1 calculates the Debye Function of order 1.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      DEBYE1(x) = 1 / x * Integral ( 0 <= t <= x ) t / ( exp ( t ) - 1 ) dt
 !
 !    The code uses Chebyshev series whose coefficients
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: DEBYE1, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: adeb1(0:18)
   real(kind = DP) :: debye1
   integer :: i
   integer :: nexp
   integer, parameter :: nterms = 15
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: debinf,expmx, &
        rk,sum1,t,thirt6,xk
   data thirt6 /36.0d0 /
   data debinf/0.60792710185402662866d0/
   data adeb1/2.40065971903814101941d0, &
              0.19372130421893600885d0, &
             -0.623291245548957703d-2, &
              0.35111747702064800d-3, &
             -0.2282224667012310d-4, &
              0.158054678750300d-5, &
             -0.11353781970719d-6, &
              0.835833611875d-8, &
             -0.62644247872d-9, &
              0.4760334890d-10, &
             -0.365741540d-11, &
              0.28354310d-12, &
             -0.2214729d-13, &
              0.174092d-14, &
             -0.13759d-15, &
              0.1093d-16, &
             -0.87d-18, &
              0.7d-19, &
             -0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'DEBYE1 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     debye1 = zero
   else if ( x < two*sceps_DP ) then
     debye1 = ( ( x - nine ) * x + thirt6 ) / thirt6
   else if ( x <= four ) then
     t = ( ( x * x / eight ) - half ) - half
     debye1 = cheval ( nterms, adeb1, t ) - quart * x
   else

     debye1 = one / ( x * debinf )
     if ( x < xlim ) then
       expmx = exp ( -x )
       if ( xupper1 < x ) then
         debye1 = debye1 - expmx * ( one + one / x )
       else
         sum1 = zero
         rk = aint ( xlim / x )
         nexp = int ( rk )
         xk = rk * x
         do i = nexp, 1, -1
           t = ( one + one / xk ) / rk
           sum1 = sum1 * expmx + t
           rk = rk - one
           xk = xk - x
         end do
         debye1 = debye1 - sum1 * expmx
       endif
     endif
   endif

   return
 end function debye1
 !******************************************************************
 !>>>debye2.f90
 function debye2(xvalue)

 !*****************************************************************************80
 !
 !! DEBYE2 calculates the Debye Function of order 2.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      DEBYE2(x) = 2 / x^2 * Integral ( 0 <= t <= x ) t^2 / ( exp ( t ) - 1 ) dt
 !
 !    The code uses Chebyshev series whose coefficients
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    24 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: DEBYE2, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: debye2
   integer :: i
   integer :: nexp
   integer, parameter :: nterms = 17
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: adeb2(0:18),debinf,expmx, &
        rk,sum1,t,twent4,xk, xlim2
   data twent4/24.0d0/
   data debinf/4.80822761263837714160d0/
   data adeb2/2.59438102325707702826d0, &
              0.28633572045307198337d0, &
             -0.1020626561580467129d-1, &
              0.60491097753468435d-3, &
             -0.4052576589502104d-4, &
              0.286338263288107d-5, &
             -0.20863943030651d-6, &
              0.1552378758264d-7, &
             -0.117312800866d-8, &
              0.8973585888d-10, &
             -0.693176137d-11, &
              0.53980568d-12, &
             -0.4232405d-13, &
              0.333778d-14, &
             -0.26455d-15, &
              0.2106d-16, &
             -0.168d-17, &
              0.13d-18, &
             -0.1d-19/
 !
 !   Machine-dependent constants
 !
   data xlim2/2.1572317d154/
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'DEBYE2 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     debye2 = zero
   else if ( x < two*sceps_DP ) then
     debye2 = ( ( x - eight ) * x + twent4 ) / twent4
   else if ( x <= four ) then
     t = ( ( x * x / eight ) - half ) - half
     debye2 = cheval ( nterms, adeb2, t ) - x / three
   else if ( x <= xupper1 ) then

     expmx = exp ( -x )
     sum1 = zero
     rk = aint ( xlim / x )
     nexp = int ( rk )
     xk = rk * x
     do i = nexp, 1, -1
       t =  ( one + two / xk + two / ( xk * xk ) ) / rk
       sum1 = sum1 * expmx + t
       rk = rk - one
       xk = xk - x
     end do
     debye2 = debinf / ( x * x ) - two * sum1 * expmx

   else if ( x < xlim ) then

     expmx = exp ( -x )
     sum1 = ( ( x + two ) * x + two ) / ( x * x )
     debye2 = debinf / ( x * x ) - two * sum1 * expmx

   else if ( x <= xlim2 ) then
     debye2 = debinf / ( x * x )
   else
     debye2 = zero
   endif

   return
 end function debye2
 !******************************************************************
 !>>>debye3.f90
 function debye3(xvalue)

 !*****************************************************************************80
 !
 !! DEBYE3 calculates the Debye Function of order 3.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      DEBYE3(x) = 3 / x^3 * Integral ( 0 <= t <= x ) t^3 / ( exp ( t ) - 1 ) dt
 !
 !    The code uses Chebyshev series whose coefficients
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: DEBYE3, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: debye3
   integer :: i
   integer :: nexp
   integer, parameter :: nterms = 16
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: adeb3(0:18),debinf,expmx, &
        pt375,rk,sevp5,sum1,t, &
        xk,xki,xlim2
   data pt375/0.375d0/
   data sevp5/7.5d0 /
   data debinf/0.51329911273421675946d-1/
   data adeb3/2.70773706832744094526d0, &
              0.34006813521109175100d0, &
             -0.1294515018444086863d-1, &
              0.79637553801738164d-3, &
             -0.5463600095908238d-4, &
              0.392430195988049d-5, &
             -0.28940328235386d-6, &
              0.2173176139625d-7, &
             -0.165420999498d-8, &
              0.12727961892d-9, &
             -0.987963459d-11, &
              0.77250740d-12, &
             -0.6077972d-13, &
              0.480759d-14, &
             -0.38204d-15, &
              0.3048d-16, &
             -0.244d-17, &
              0.20d-18, &
             -0.2d-19/
 !
 !   Machine-dependent constants
 !
   data xlim2/0.9487163d103/
 !
   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'DEBYE3 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     debye3 = zero
     return
   endif

   if ( x < two*sceps_DP ) then
     debye3 = ( ( x - sevp5 ) * x + twenty ) / twenty
   else if ( x <= 4 ) then
     t = ( ( x * x / eight ) - half ) - half
     debye3 = cheval ( nterms, adeb3, t ) - pt375 * x
   else
 !
 !   Code for x > 4.0
 !
      if ( xlim2 < x ) then
         debye3 = zero
      else
         debye3 = one / ( debinf * x * x * x )
         if ( x < xlim ) then
            expmx = exp ( -x )
            if ( xupper1 < x ) then
               sum1 = ((( x + three ) * x + six ) * x + six ) / ( x * x * x )
            else
               sum1 = zero
               rk = aint ( xlim / x )
               nexp = int ( rk )
               xk = rk * x
               do i = nexp, 1, -1
                  xki = one / xk
                  t =  ((( six * xki + six ) * xki + three ) * xki + one ) / rk
                  sum1 = sum1 * expmx + t
                  rk = rk - one
                  xk = xk - x
               end do
            endif
            debye3 = debye3 - three * sum1 * expmx
         endif
      endif
   endif

   return
 end function debye3
 !******************************************************************
 !>>>debye4.f90
 function debye4(xvalue)

 !*****************************************************************************80
 !
 !! DEBYE4 calculates the Debye Function of order 4.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      DEBYE4(x) = 4 / x^4 * Integral ( 0 <= t <= x ) t^4 / ( exp ( t ) - 1 ) dt
 !
 !    The code uses Chebyshev series whose coefficients
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: DEBYE4, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: debye4
   integer :: i
   integer :: nexp
   integer, parameter :: nterms = 16
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: adeb4(0:18),debinf,expmx, &
        forty5,rk,sum1,t,twent4, &
        twopt5,xk,xki,xlim2
   data twopt5/2.5d0/
   data twent4,forty5 /24.0d0 , 45.0d0 /
   data debinf/99.54506449376351292781d0/
   data adeb4/2.78186941502052346008d0, &
              0.37497678352689286364d0, &
             -0.1494090739903158326d-1, &
              0.94567981143704274d-3, &
             -0.6613291613893255d-4, &
              0.481563298214449d-5, &
             -0.35880839587593d-6, &
              0.2716011874160d-7, &
             -0.208070991223d-8, &
              0.16093838692d-9, &
             -0.1254709791d-10, &
              0.98472647d-12, &
             -0.7772369d-13, &
              0.616483d-14, &
             -0.49107d-15, &
              0.3927d-16, &
             -0.315d-17, &
              0.25d-18, &
             -0.2d-19/
 !
 !   Machine-dependent constants
 !
   data xlim2/2.5826924d77/

   if (.not.setupMISC_done) call setupMISC
   x = xvalue
 !
 !   Check XVALUE >= 0.0
 !
   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'DEBYE4 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     debye4 = zero
     return
   endif

   if ( x < two*sceps_DP ) then
     debye4 = ( ( twopt5 * x - eighteen ) * x + forty5 ) / forty5
   else if ( x <= four ) then
     t = ( ( x * x / eight ) - half ) - half
     debye4 = cheval ( nterms, adeb4, t ) - ( x + x ) / five
   else
 !
 !   Code for x > 4.0
 !
      if ( xlim2 < x ) then
         debye4 = zero
      else
         t = x * x
         debye4 = ( debinf / t ) / t
         if ( x < xlim ) then
            expmx = exp ( -x )
            if ( xupper1 < x ) then
               sum1 = ( ( ( ( x + four ) * x + twelve ) * x + &
                     twent4 ) * x + twent4 ) / ( x * x * x * x )
            else
               sum1 = zero
               rk = aint ( xlim / x )
               nexp = int ( rk )
               xk = rk * x
               do i = nexp, 1, -1
                  xki = one / xk
                  t =  ( ( ( ( twent4 * xki + twent4 ) * xki + &
                       twelve ) * xki + four ) * xki + one ) / rk
                  sum1 = sum1 * expmx + t
                  rk = rk - one
                  xk = xk - x
               end do
            endif
            debye4 = debye4 - four * sum1 * expmx
         endif
      endif
   endif

   return
 end function debye4
 !******************************************************************
 !>>>exp3_Integral.f90
 function exp3_Integral(xvalue)

 !*****************************************************************************80
 !
 !! EXP3_INT calculates the integral of exp(-t^3).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      EXP3_INT(x) = Integral ( 0 <= t <= x ) exp ( -t^3 ) dt
 !
 !    The code uses Chebyshev expansions, whose coefficients are
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: EXP3_INT, the value of the FUNCTION.
 !
   implicit none

   real(DP),parameter :: cinque12=512.d0
   real(kind = DP) :: exp3_Integral
   integer, parameter :: nterm1 = 22
   integer, parameter :: nterm2 = 20
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: aexp3(0:24),aexp3a(0:24), &
          funinf,t, &
          xupper2
   data funinf/0.89297951156924921122d0/
   data aexp3(0)/  1.26919841422112601434d0/
   data aexp3(1)/ -0.24884644638414098226d0/
   data aexp3(2)/  0.8052622071723104125d-1/
   data aexp3(3)/ -0.2577273325196832934d-1/
   data aexp3(4)/  0.759987887307377429d-2/
   data aexp3(5)/ -0.203069558194040510d-2/
   data aexp3(6)/  0.49083458669932917d-3/
   data aexp3(7)/ -0.10768223914202077d-3/
   data aexp3(8)/  0.2155172626428984d-4/
   data aexp3(9)/ -0.395670513738429d-5/
   data aexp3(10)/ 0.66992409338956d-6/
   data aexp3(11)/-0.10513218080703d-6/
   data aexp3(12)/ 0.1536258019825d-7/
   data aexp3(13)/-0.209909603636d-8/
   data aexp3(14)/ 0.26921095381d-9/
   data aexp3(15)/-0.3251952422d-10/
   data aexp3(16)/ 0.371148157d-11/
   data aexp3(17)/-0.40136518d-12/
   data aexp3(18)/ 0.4123346d-13/
   data aexp3(19)/-0.403375d-14/
   data aexp3(20)/ 0.37658d-15/
   data aexp3(21)/-0.3362d-16/
   data aexp3(22)/ 0.288d-17/
   data aexp3(23)/-0.24d-18/
   data aexp3(24)/ 0.2d-19/
   data aexp3a(0)/  1.92704649550682737293d0/
   data aexp3a(1)/ -0.3492935652048138054d-1/
   data aexp3a(2)/  0.145033837189830093d-2/
   data aexp3a(3)/ -0.8925336718327903d-4/
   data aexp3a(4)/  0.705423921911838d-5/
   data aexp3a(5)/ -0.66717274547611d-6/
   data aexp3a(6)/  0.7242675899824d-7/
   data aexp3a(7)/ -0.878258256056d-8/
   data aexp3a(8)/  0.116722344278d-8/
   data aexp3a(9)/ -0.16766312812d-9/
   data aexp3a(10)/ 0.2575501577d-10/
   data aexp3a(11)/-0.419578881d-11/
   data aexp3a(12)/ 0.72010412d-12/
   data aexp3a(13)/-0.12949055d-12/
   data aexp3a(14)/ 0.2428703d-13/
   data aexp3a(15)/-0.473311d-14/
   data aexp3a(16)/ 0.95531d-15/
   data aexp3a(17)/-0.19914d-15/
   data aexp3a(18)/ 0.4277d-16/
   data aexp3a(19)/-0.944d-17/
   data aexp3a(20)/ 0.214d-17/
   data aexp3a(21)/-0.50d-18/
   data aexp3a(22)/ 0.12d-18/
   data aexp3a(23)/-0.3d-19/
   data aexp3a(24)/ 0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !
   data xupper2/3.3243018d0/

   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'EXP3_INT - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     exp3_Integral = zero
   else if ( x < cinque12*sceps_DP ) then
     exp3_Integral = x
   else if ( x <= two ) then
     t = ( ( x * x * x / four ) - half ) - half
     exp3_Integral = x * cheval ( nterm1, aexp3, t )
   else if ( x <= xupper2 ) then
     t = ( ( sixteen / ( x * x * x ) ) - half ) - half
     t = cheval ( nterm2, aexp3a, t )
     t = t * exp ( -x * x * x ) / ( three * x * x )
     exp3_Integral = funinf - t
   else
     exp3_Integral = funinf
   endif

   return
 end function exp3_Integral
 !******************************************************************
 !>>>goodwin.f90
 function goodwin(xvalue)

 !*****************************************************************************80
 !
 !! GOODWIN calculates the integral of exp(-t^2/(t+x)).
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      GOODWIN(x) = Integral ( 0 <= t < infinity ) exp ( -t^2 ) / ( t + x ) dt
 !
 !    The code uses Chebyshev expansions whose coefficients are
 !    given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: GOODWIN, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: goodwin
   integer, parameter :: nterm1 = 26
   integer, parameter :: nterm2 = 20
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: agost(0:28),agosta(0:23), &
        fval,gamby2,rtpib2, t
   data gamby2/0.28860783245076643030d0/
   data rtpib2/0.88622692545275801365d0/
   data agost(0)/  0.63106560560398446247d0/
   data agost(1)/  0.25051737793216708827d0/
   data agost(2)/ -0.28466205979018940757d0/
   data agost(3)/  0.8761587523948623552d-1/
   data agost(4)/  0.682602267221252724d-2/
   data agost(5)/ -0.1081129544192254677d-1/
   data agost(6)/  0.169101244117152176d-2/
   data agost(7)/  0.50272984622615186d-3/
   data agost(8)/ -0.18576687204100084d-3/
   data agost(9)/ -0.428703674168474d-5/
   data agost(10)/ 0.1009598903202905d-4/
   data agost(11)/-0.86529913517382d-6/
   data agost(12)/-0.34983874320734d-6/
   data agost(13)/ 0.6483278683494d-7/
   data agost(14)/ 0.757592498583d-8/
   data agost(15)/-0.277935424362d-8/
   data agost(16)/-0.4830235135d-10/
   data agost(17)/ 0.8663221283d-10/
   data agost(18)/-0.394339687d-11/
   data agost(19)/-0.209529625d-11/
   data agost(20)/ 0.21501759d-12/
   data agost(21)/ 0.3959015d-13/
   data agost(22)/-0.692279d-14/
   data agost(23)/-0.54829d-15/
   data agost(24)/ 0.17108d-15/
   data agost(25)/ 0.376d-17/
   data agost(26)/-0.349d-17/
   data agost(27)/ 0.7d-19/
   data agost(28)/ 0.6d-19/
   data agosta(0)/  1.81775467984718758767d0/
   data agosta(1)/ -0.9921146570744097467d-1/
   data agosta(2)/ -0.894058645254819243d-2/
   data agosta(3)/ -0.94955331277726785d-3/
   data agosta(4)/ -0.10971379966759665d-3/
   data agosta(5)/ -0.1346694539578590d-4/
   data agosta(6)/ -0.172749274308265d-5/
   data agosta(7)/ -0.22931380199498d-6/
   data agosta(8)/ -0.3127844178918d-7/
   data agosta(9)/ -0.436197973671d-8/
   data agosta(10)/-0.61958464743d-9/
   data agosta(11)/-0.8937991276d-10/
   data agosta(12)/-0.1306511094d-10/
   data agosta(13)/-0.193166876d-11/
   data agosta(14)/-0.28844270d-12/
   data agosta(15)/-0.4344796d-13/
   data agosta(16)/-0.659518d-14/
   data agosta(17)/-0.100801d-14/
   data agosta(18)/-0.15502d-15/
   data agosta(19)/-0.2397d-16/
   data agosta(20)/-0.373d-17/
   data agosta(21)/-0.58d-18/
   data agosta(22)/-0.9d-19/
   data agosta(23)/-0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   x = xvalue

   if ( x <= zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'GOODWIN - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X <= 0.'
     goodwin = zero
   else if ( x < halfeps ) then
     goodwin = - gamby2 - log ( x )
   else if ( x <= two ) then
     t = ( x - half ) - half
     goodwin = cheval ( nterm1, agost, t ) - exp ( -x * x ) * log ( x )
   else if ( x <= sorgo(4) ) then
     fval = rtpib2 / x
     t = ( six - x ) / ( two + x )
     goodwin = fval * cheval ( nterm2, agosta, t )
   else
     goodwin = rtpib2 / x
   endif

   return
 end function goodwin
 !******************************************************************
 !>>>i0ml0.f90
 function i0ml0(xvalue)

 !*****************************************************************************80
 !
 !! I0ML0 calculates the difference between the Bessel I0 and Struve L0 Functions.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      I0ML0(x) = I0(x) - L0(x)
 !
 !    I0(x) is the modified Bessel Function of the first kind of order 0,
 !    L0(x) is the modified Struve Function of order 0.
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to an accuracy of 20D.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: I0ML0, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: ai0l0(0:23)
   real(kind = DP) :: ai0l0a(0:23)
   real(kind = DP) :: i0ml0
   integer, parameter :: nterm1 = 21
   integer, parameter :: nterm2 = 21
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atehun, &
        forty,t,two88, &
        xsq
   data forty / 40.0d0 /
   data two88,atehun/ 288.0d0, 800.0d0 /
   data ai0l0(0)/  0.52468736791485599138d0/
   data ai0l0(1)/ -0.35612460699650586196d0/
   data ai0l0(2)/  0.20487202864009927687d0/
   data ai0l0(3)/ -0.10418640520402693629d0/
   data ai0l0(4)/  0.4634211095548429228d-1/
   data ai0l0(5)/ -0.1790587192403498630d-1/
   data ai0l0(6)/  0.597968695481143177d-2/
   data ai0l0(7)/ -0.171777547693565429d-2/
   data ai0l0(8)/  0.42204654469171422d-3/
   data ai0l0(9)/ -0.8796178522094125d-4/
   data ai0l0(10)/ 0.1535434234869223d-4/
   data ai0l0(11)/-0.219780769584743d-5/
   data ai0l0(12)/ 0.24820683936666d-6/
   data ai0l0(13)/-0.2032706035607d-7/
   data ai0l0(14)/ 0.90984198421d-9/
   data ai0l0(15)/ 0.2561793929d-10/
   data ai0l0(16)/-0.710609790d-11/
   data ai0l0(17)/ 0.32716960d-12/
   data ai0l0(18)/ 0.2300215d-13/
   data ai0l0(19)/-0.292109d-14/
   data ai0l0(20)/-0.3566d-16/
   data ai0l0(21)/ 0.1832d-16/
   data ai0l0(22)/-0.10d-18/
   data ai0l0(23)/-0.11d-18/
   data ai0l0a(0)/ 2.00326510241160643125d0/
   data ai0l0a(1)/ 0.195206851576492081d-2/
   data ai0l0a(2)/ 0.38239523569908328d-3/
   data ai0l0a(3)/ 0.7534280817054436d-4/
   data ai0l0a(4)/ 0.1495957655897078d-4/
   data ai0l0a(5)/ 0.299940531210557d-5/
   data ai0l0a(6)/ 0.60769604822459d-6/
   data ai0l0a(7)/ 0.12399495544506d-6/
   data ai0l0a(8)/ 0.2523262552649d-7/
   data ai0l0a(9)/ 0.504634857332d-8/
   data ai0l0a(10)/0.97913236230d-9/
   data ai0l0a(11)/0.18389115241d-9/
   data ai0l0a(12)/0.3376309278d-10/
   data ai0l0a(13)/0.611179703d-11/
   data ai0l0a(14)/0.108472972d-11/
   data ai0l0a(15)/0.18861271d-12/
   data ai0l0a(16)/0.3280345d-13/
   data ai0l0a(17)/0.565647d-14/
   data ai0l0a(18)/0.93300d-15/
   data ai0l0a(19)/0.15881d-15/
   data ai0l0a(20)/0.2791d-16/
   data ai0l0a(21)/0.389d-17/
   data ai0l0a(22)/0.70d-18/
   data ai0l0a(23)/0.16d-18/
 !
 !   MACHINE-DEPENDENT CONSTANTS (suitable for IEEE-arithmetic machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'I0ML0 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     i0ml0 = zero
   else if ( x < halfeps ) then
     i0ml0 = one
   else if ( x <= sixteen ) then
     t = ( six * x - forty ) / ( x + forty )
     i0ml0 = cheval ( nterm1, ai0l0, t )
   else if ( x <= sxhi ) then
     xsq = x * x
     t = ( atehun - xsq ) / ( two88 + xsq )
     i0ml0 = cheval ( nterm2, ai0l0a, t ) * twobpi / x
   else
     i0ml0 = twobpi / x
   endif

   return
 end function i0ml0
 !******************************************************************
 !>>>i1ml1.f90
 function i1ml1(xvalue)

 !*****************************************************************************80
 !
 !! I1ML1 calculates the difference between the Bessel I1 and Struve L1 Functions.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      I1ML1(x) = I1(x) - L1(x)
 !
 !    I1(x) is the modified Bessel Function of the first kind of order 1,
 !    L1(x) is the modified Struve Function of order 1.
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to an accuracy of 20D.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    29 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !    0 <= XVALUE is required.
 !
 !    Output, real(kind = DP) :: I1ML1, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: i1ml1
   integer, parameter :: nterm1 = 20
   integer, parameter :: nterm2 = 22
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: ai1l1(0:23),ai1l1a(0:25),atehun, &
        forty,t,two88, xsq
   data forty/  40.0d0 /
   data two88,atehun/ 288.0d0 , 800.0d0 /
   data ai1l1(0)/  0.67536369062350576137d0/
   data ai1l1(1)/ -0.38134971097266559040d0/
   data ai1l1(2)/  0.17452170775133943559d0/
   data ai1l1(3)/ -0.7062105887235025061d-1/
   data ai1l1(4)/  0.2517341413558803702d-1/
   data ai1l1(5)/ -0.787098561606423321d-2/
   data ai1l1(6)/  0.214814368651922006d-2/
   data ai1l1(7)/ -0.50862199717906236d-3/
   data ai1l1(8)/  0.10362608280442330d-3/
   data ai1l1(9)/ -0.1795447212057247d-4/
   data ai1l1(10)/ 0.259788274515414d-5/
   data ai1l1(11)/-0.30442406324667d-6/
   data ai1l1(12)/ 0.2720239894766d-7/
   data ai1l1(13)/-0.158126144190d-8/
   data ai1l1(14)/ 0.1816209172d-10/
   data ai1l1(15)/ 0.647967659d-11/
   data ai1l1(16)/-0.54113290d-12/
   data ai1l1(17)/-0.308311d-14/
   data ai1l1(18)/ 0.305638d-14/
   data ai1l1(19)/-0.9717d-16/
   data ai1l1(20)/-0.1422d-16/
   data ai1l1(21)/ 0.84d-18/
   data ai1l1(22)/ 0.7d-19/
   data ai1l1(23)/-0.1d-19/
   data ai1l1a(0)/  1.99679361896789136501d0/
   data ai1l1a(1)/ -0.190663261409686132d-2/
   data ai1l1a(2)/ -0.36094622410174481d-3/
   data ai1l1a(3)/ -0.6841847304599820d-4/
   data ai1l1a(4)/ -0.1299008228509426d-4/
   data ai1l1a(5)/ -0.247152188705765d-5/
   data ai1l1a(6)/ -0.47147839691972d-6/
   data ai1l1a(7)/ -0.9020819982592d-7/
   data ai1l1a(8)/ -0.1730458637504d-7/
   data ai1l1a(9)/ -0.332323670159d-8/
   data ai1l1a(10)/-0.63736421735d-9/
   data ai1l1a(11)/-0.12180239756d-9/
   data ai1l1a(12)/-0.2317346832d-10/
   data ai1l1a(13)/-0.439068833d-11/
   data ai1l1a(14)/-0.82847110d-12/
   data ai1l1a(15)/-0.15562249d-12/
   data ai1l1a(16)/-0.2913112d-13/
   data ai1l1a(17)/-0.543965d-14/
   data ai1l1a(18)/-0.101177d-14/
   data ai1l1a(19)/-0.18767d-15/
   data ai1l1a(20)/-0.3484d-16/
   data ai1l1a(21)/-0.643d-17/
   data ai1l1a(22)/-0.118d-17/
   data ai1l1a(23)/-0.22d-18/
   data ai1l1a(24)/-0.4d-19/
   data ai1l1a(25)/-0.1d-19/
 !
 !   MACHINE-DEPENDENT CONSTANTS (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'I1ML1 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     i1ml1 = zero
   else if ( x < eps_DP ) then
     i1ml1 = x / two
   else if ( x <= sixteen ) then
     t = ( six * x - forty ) / ( x + forty )
     i1ml1 = cheval ( nterm1, ai1l1, t ) * x / two
   else if ( x <= sxhi ) then
     xsq = x * x
     t = ( atehun - xsq ) / ( two88 + xsq )
     i1ml1 = cheval ( nterm2, ai1l1a, t ) * twobpi
   else
     i1ml1 = twobpi
   endif

   return
 end function i1ml1
 !******************************************************************
 !>>>lobachevsky.f90
 function lobachevsky(xvalue)

 !*****************************************************************************80
 !
 !! LOBACHEVSKY calculates the Lobachevsky Function.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      LOBACHEVSKY(x) = Integral ( 0 <= t <= x ) -ln ( abs ( cos ( t ) ) dt
 !
 !    The code uses Chebyshev expansions whose coefficients are given
 !    to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: LOBACHEVSKY, the value of the FUNCTION.
 !
   implicit none

   integer :: indpiy2
   integer :: indsgn
   integer :: npiy
   integer, parameter :: nterm1 = 13
   integer, parameter :: nterm2 = 9
   real(kind = DP) :: lobachevsky
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arlob1(0:15),arlob2(0:10), &
        fval,fval1,lbpb21,lbpb22,lobpiya,lobpiyb, &
        lobpiy1,lobpiy2,piyby2,piyby21,piyby22,piyby4,piy1,piy, &
        piy11,piy12,piy2,t,tcon,xcub, &
        xr
   data lobpiya,lobpiyb/ 1115.0d0 , 512.0d0 /
   data lobpiy2/-1.48284696397869499311d-4/
   data lbpb22/-7.41423481989347496556d-5/
   data piy11,piy12/ 201.0d0 , 64.0d0 /
   data piy2/9.67653589793238462643d-4/
   data piyby22/4.83826794896619231322d-4/
   data tcon/3.24227787655480868620d0/
   data arlob1/0.34464884953481300507d0, &
               0.584198357190277669d-2, &
               0.19175029694600330d-3, &
               0.787251606456769d-5, &
               0.36507477415804d-6, &
               0.1830287272680d-7, &
               0.96890333005d-9, &
               0.5339055444d-10, &
               0.303408025d-11, &
               0.17667875d-12, &
               0.1049393d-13, &
               0.63359d-15, &
               0.3878d-16, &
               0.240d-17, &
               0.15d-18, &
               0.1d-19/
   data arlob2/2.03459418036132851087d0, &
               0.1735185882027407681d-1, &
               0.5516280426090521d-4, &
               0.39781646276598d-6, &
               0.369018028918d-8, &
               0.3880409214d-10, &
               0.44069698d-12, &
               0.527674d-14, &
               0.6568d-16, &
               0.84d-18, &
               0.1d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = abs(xvalue)
   indsgn = 1
   if ( xvalue < zero ) then
     indsgn = -1
   endif

   if ( sorgo(1) < x ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'LOBACHEVSKY - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large.'
     lobachevsky = zero
     return
   endif
 !
 !  Reduce argument to [0,pi]
 !
   piy1 = piy11 / piy12
   piy = piy1 + piy2
   piyby2 = piy / two
   piyby21 = piy1 / two
   piyby4 = piyby2 / two
   npiy = int ( x / piy )
   xr = ( x - npiy * piy1 ) - npiy * piy2
 !
 !  Reduce argument to [0,pi/2]
 !
   indpiy2 = 0
   if ( piyby2 < xr ) then
     indpiy2 = 1
     xr = ( piy1 - xr ) + piy2
   endif
 !
 !  Code for argument in [0,pi/4]
 !
   if ( xr <= piyby4 ) then
      if ( xr < titty1 ) then
         fval = zero
      else
         xcub = xr * xr * xr
         if ( xr < titty2 ) then
           fval = xcub / six
         else
           t = ( tcon * xr * xr - half ) - half
           fval = xcub * cheval ( nterm1, arlob1, t )
         endif
      endif
   else
 !
 !  Code for argument in [pi/4,pi/2]
 !
      xr = ( piyby21 - xr ) + piyby22
      if ( xr == zero ) then
         fval1 = zero
      else
         if ( xr < titty3 ) then
           fval1 = xr * ( one - log ( xr ) )
         else
           t = ( tcon * xr * xr - half ) - half
           fval1 = xr * ( cheval ( nterm2, arlob2, t ) - log ( xr ) )
         endif
      endif
      lbpb21 = lobpiya / ( lobpiyb + lobpiyb )
      fval = ( lbpb21 - fval1 ) + lbpb22
   endif

   lobpiy1 = lobpiya / lobpiyb
 !
 !  Compute value for argument in [pi/2,pi]
 !
   if ( indpiy2 == 1 ) then
      fval = ( lobpiy1 - fval ) + lobpiy2
   endif

   if ( npiy <= 0 ) then
     lobachevsky = fval
   else
     lobachevsky = ( fval + npiy * lobpiy2 ) + npiy * lobpiy1
   endif

   if ( indsgn == -1 ) then
     lobachevsky = -lobachevsky
   endif

   return
 end function lobachevsky
 !******************************************************************
 !>>>stromgen.f90
 function stromgen(xvalue)

 !*****************************************************************************80
 !
 !! STROMGEN calculates Stromgen's integral.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      STROMGEN(X) = Integral ( 0 <= t <= X ) t^7 * exp(2*t) / (exp(t)-1)^3 dt
 !
 !    The code uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: STROMGEN, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: astrom(0:26)
   real(kind = DP) :: f15bp4
   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 23
   integer :: numexp
   real(kind = DP) :: one5ln
   real(kind = DP) :: pi4b3
   real(kind = DP) :: rk
   real(kind = DP) :: stromgen
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: sumexp,sum2,t,valinf, &
        xk,xk1
   data one5ln/ 0.4055d0 /
   data f15bp4/0.38497433455066256959d-1 /
   data pi4b3/1.29878788045336582982d2 /
   data valinf/196.51956920868988261257d0/
   data astrom(0)/  0.56556120872539155290d0/
   data astrom(1)/  0.4555731969101785525d-1/
   data astrom(2)/ -0.4039535875936869170d-1/
   data astrom(3)/ -0.133390572021486815d-2/
   data astrom(4)/  0.185862506250538030d-2/
   data astrom(5)/ -0.4685555868053659d-4/
   data astrom(6)/ -0.6343475643422949d-4/
   data astrom(7)/  0.572548708143200d-5/
   data astrom(8)/  0.159352812216822d-5/
   data astrom(9)/ -0.28884328431036d-6/
   data astrom(10)/-0.2446633604801d-7/
   data astrom(11)/ 0.1007250382374d-7/
   data astrom(12)/-0.12482986104d-9/
   data astrom(13)/-0.26300625283d-9/
   data astrom(14)/ 0.2490407578d-10/
   data astrom(15)/ 0.485454902d-11/
   data astrom(16)/-0.105378913d-11/
   data astrom(17)/-0.3604417d-13/
   data astrom(18)/ 0.2992078d-13/
   data astrom(19)/-0.163971d-14/
   data astrom(20)/-0.61061d-15/
   data astrom(21)/ 0.9335d-16/
   data astrom(22)/ 0.709d-17/
   data astrom(23)/-0.291d-17/
   data astrom(24)/ 0.8d-19/
   data astrom(25)/ 0.6d-19/
   data astrom(26)/-0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC

   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'STROMGEN - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     stromgen = zero
     return
   endif

   if ( x < titty0 ) then
     stromgen = zero
   else if ( x < eps_DP ) then
     stromgen = x**5 / pi4b3
   else if ( x <= four ) then
     t = ( ( x / two ) - half ) - half
     stromgen = x**5 * cheval ( nterms, astrom, t ) * f15bp4
   else
 !
 !  Code for x > 4.0
 !
     if ( sorgo(7) < x ) then
       sumexp = one
     else
       numexp = int ( lneps / ( one5ln - x ) ) + 1
       if ( 1 < numexp ) then
         t = exp ( -x )
       else
         t = one
       endif
       rk = zero
       do k1 = 1, numexp
         rk = rk + one
       end do
       sumexp = zero
       do k1 = 1, numexp
         sum2 = one
         xk = one / ( rk * x )
         xk1 = one
         do k2 = 1, 7
           sum2 = sum2 * xk1 * xk + one
           xk1 = xk1 + one
         end do
         sum2 = sum2 * ( rk + one ) / two
         sumexp = sumexp * t + sum2
         rk = rk - one
       end do

     endif

     t = seven * log ( x ) - x + log ( sumexp )

     if ( t < xhighL3 ) then
       stromgen = valinf
     else
       stromgen = valinf - exp ( t ) * f15bp4
     endif

   endif

   return
 end function stromgen
 !******************************************************************
 !>>>struve_h0.f90
 function struve_h0(xvalue)

 !*****************************************************************************80
 !
 !! STRUVE_H0 calculates the Struve Function of order 0.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      HO(x) = (2/pi) Integral ( 0 <= t <= pi/2 ) sin ( x * cos ( t ) ) dt
 !
 !    H0 also satisfies the second-order equation
 !
 !      x*D(Df) + Df + x * f = 2 * x / pi
 !
 !    The code uses Chebyshev expansions whose coefficients are
 !    given to 20D.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: STRUVE_H0, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: arrh0(0:19)
   real(kind = DP) :: arrh0a(0:20)
   real(kind = DP) :: ay0asp(0:12)
   real(kind = DP) :: ay0asq(0:13)
   integer :: indsgn
   integer, parameter :: nterm1 = 18
   integer, parameter :: nterm2 = 18
   integer, parameter :: nterm3 = 11
   integer, parameter :: nterm4 = 11
   real(kind = DP) :: struve_h0
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: h0as,rt2bpi,sixtp5,t,thr2p5, &
        two62,xmp4,xsq, &
        y0p,y0q,y0val
   data sixtp5,two62,thr2p5/60.5d0, 262.0d0, 302.5d0/
   data rt2bpi/0.79788456080286535588d0/
   data arrh0(0)/  0.28696487399013225740d0/
   data arrh0(1)/ -0.25405332681618352305d0/
   data arrh0(2)/  0.20774026739323894439d0/
   data arrh0(3)/ -0.20364029560386585140d0/
   data arrh0(4)/  0.12888469086866186016d0/
   data arrh0(5)/ -0.4825632815622261202d-1/
   data arrh0(6)/  0.1168629347569001242d-1/
   data arrh0(7)/ -0.198118135642418416d-2/
   data arrh0(8)/  0.24899138512421286d-3/
   data arrh0(9)/ -0.2418827913785950d-4/
   data arrh0(10)/ 0.187437547993431d-5/
   data arrh0(11)/-0.11873346074362d-6/
   data arrh0(12)/ 0.626984943346d-8/
   data arrh0(13)/-0.28045546793d-9/
   data arrh0(14)/ 0.1076941205d-10/
   data arrh0(15)/-0.35904793d-12/
   data arrh0(16)/ 0.1049447d-13/
   data arrh0(17)/-0.27119d-15/
   data arrh0(18)/ 0.624d-17/
   data arrh0(19)/-0.13d-18/
   data arrh0a(0)/  1.99291885751992305515d0/
   data arrh0a(1)/ -0.384232668701456887d-2/
   data arrh0a(2)/ -0.32871993712353050d-3/
   data arrh0a(3)/ -0.2941181203703409d-4/
   data arrh0a(4)/ -0.267315351987066d-5/
   data arrh0a(5)/ -0.24681031075013d-6/
   data arrh0a(6)/ -0.2295014861143d-7/
   data arrh0a(7)/ -0.215682231833d-8/
   data arrh0a(8)/ -0.20303506483d-9/
   data arrh0a(9)/ -0.1934575509d-10/
   data arrh0a(10)/-0.182773144d-11/
   data arrh0a(11)/-0.17768424d-12/
   data arrh0a(12)/-0.1643296d-13/
   data arrh0a(13)/-0.171569d-14/
   data arrh0a(14)/-0.13368d-15/
   data arrh0a(15)/-0.2077d-16/
   data arrh0a(16)/ 0.2d-19/
   data arrh0a(17)/-0.55d-18/
   data arrh0a(18)/ 0.10d-18/
   data arrh0a(19)/-0.4d-19/
   data arrh0a(20)/ 0.1d-19/
   data ay0asp/1.99944639402398271568d0, &
              -0.28650778647031958d-3, &
              -0.1005072797437620d-4, &
              -0.35835941002463d-6, &
              -0.1287965120531d-7, &
              -0.46609486636d-9, &
              -0.1693769454d-10, &
              -0.61852269d-12, &
              -0.2261841d-13, &
              -0.83268d-15, &
              -0.3042d-16, &
              -0.115d-17, &
              -0.4d-19/
   data ay0asq/1.99542681386828604092d0, &
              -0.236013192867514472d-2, &
              -0.7601538908502966d-4, &
              -0.256108871456343d-5, &
              -0.8750292185106d-7, &
              -0.304304212159d-8, &
              -0.10621428314d-9, &
              -0.377371479d-11, &
              -0.13213687d-12, &
              -0.488621d-14, &
              -0.15809d-15, &
              -0.762d-17, &
              -0.3d-19, &
              -0.3d-19/
 !
 !   MACHINE-DEPENDENT CONSTANTS (Suitable for IEEE-arithmetic machines)
 !

   x = xvalue
   indsgn = 1

   if ( x < zero ) then
     x = -x
     indsgn = -1
   endif

   if ( x < three*sceps_DP*srhalf ) then
     struve_h0 = twobpi * x
   else if ( x <= eleven ) then
     t = ( ( x * x ) / sixtp5 - half ) - half
     struve_h0 = twobpi * x * cheval ( nterm1, arrh0, t )
   else if ( x <= sorgo(1) ) then
     xsq = x * x
     t = ( two62 - xsq ) / ( twenty + xsq )
     y0p = cheval ( nterm3, ay0asp, t )
     y0q = cheval ( nterm4, ay0asq, t ) / ( eight * x )
     xmp4 = x - piby4
     y0val = y0p * sin ( xmp4 ) - y0q * cos ( xmp4 )
     y0val = y0val * rt2bpi / sqrt ( x )
     t = ( thr2p5 - xsq ) / ( sixtp5 + xsq )
     h0as = twobpi * cheval ( nterm2, arrh0a, t ) / x
     struve_h0 = y0val + h0as
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'STRUVE_H0 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large.'
     struve_h0 = zero
   endif

   if ( indsgn == -1 ) then
     struve_h0 = -struve_h0
   endif

   return
 end function struve_h0
 !******************************************************************
 !>>>struve_h1.f90
 function struve_h1(xvalue)

 !*****************************************************************************80
 !
 !! STRUVE_H1 calculates the Struve Function of order 1.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      H1(x) = 2*x/pi * Integral ( 0 <= t <= pi/2 )
 !        sin ( x * cos ( t ) )^2 * sin ( t ) dt
 !
 !    H1 also satisfies the second-order differential equation
 !
 !      x^2 * D^2 f  +  x * Df  +  (x^2 - 1)f  =  2x^2 / pi
 !
 !    The code uses Chebyshev expansions with the coefficients
 !    given to 20D.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: STRUVE_H1, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: arrh1(0:17)
   real(kind = DP) :: arrh1a(0:21)
   real(kind = DP) :: ay1asp(0:14)
   real(kind = DP) :: ay1asq(0:15)
   integer, parameter :: nterm1 = 15
   integer, parameter :: nterm2 = 17
   integer, parameter :: nterm3 = 12
   integer, parameter :: nterm4 = 13
   real(kind = DP) :: struve_h1
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: fortp5, &
        h1as,one82,rt2bpi,t,thpby4, &
        tw02p5, &
        xm3p4,xsq,y1p,y1q,y1val
   data fortp5,one82,tw02p5/40.5d0, 182.0d0 , 202.5d0/
   data rt2bpi/0.79788456080286535588d0/
   data thpby4/2.35619449019234492885d0/
   data arrh1/0.17319061083675439319d0, &
             -0.12606917591352672005d0, &
              0.7908576160495357500d-1, &
             -0.3196493222321870820d-1, &
              0.808040581404918834d-2, &
             -0.136000820693074148d-2, &
              0.16227148619889471d-3, &
             -0.1442352451485929d-4, &
              0.99219525734072d-6, &
             -0.5441628049180d-7, &
              0.243631662563d-8, &
             -0.9077071338d-10, &
              0.285926585d-11, &
             -0.7716975d-13, &
              0.180489d-14, &
             -0.3694d-16, &
              0.67d-18, &
             -0.1d-19/
   data arrh1a(0)/  2.01083504951473379407d0/
   data arrh1a(1)/  0.592218610036099903d-2/
   data arrh1a(2)/  0.55274322698414130d-3/
   data arrh1a(3)/  0.5269873856311036d-4/
   data arrh1a(4)/  0.506374522140969d-5/
   data arrh1a(5)/  0.49028736420678d-6/
   data arrh1a(6)/  0.4763540023525d-7/
   data arrh1a(7)/  0.465258652283d-8/
   data arrh1a(8)/  0.45465166081d-9/
   data arrh1a(9)/  0.4472462193d-10/
   data arrh1a(10)/ 0.437308292d-11/
   data arrh1a(11)/ 0.43568368d-12/
   data arrh1a(12)/ 0.4182190d-13/
   data arrh1a(13)/ 0.441044d-14/
   data arrh1a(14)/ 0.36391d-15/
   data arrh1a(15)/ 0.5558d-16/
   data arrh1a(16)/-0.4d-19/
   data arrh1a(17)/ 0.163d-17/
   data arrh1a(18)/-0.34d-18/
   data arrh1a(19)/ 0.13d-18/
   data arrh1a(20)/-0.4d-19/
   data arrh1a(21)/ 0.1d-19/
   data ay1asp/2.00135240045889396402d0, &
               0.71104241596461938d-3, &
               0.3665977028232449d-4, &
               0.191301568657728d-5, &
               0.10046911389777d-6, &
               0.530401742538d-8, &
               0.28100886176d-9, &
               0.1493886051d-10, &
               0.79578420d-12, &
               0.4252363d-13, &
               0.227195d-14, &
               0.12216d-15, &
               0.650d-17, &
               0.36d-18, &
               0.2d-19/
   data ay1asq/5.99065109477888189116d0, &
              -0.489593262336579635d-2, &
              -0.23238321307070626d-3, &
              -0.1144734723857679d-4, &
              -0.57169926189106d-6, &
              -0.2895516716917d-7, &
              -0.147513345636d-8, &
              -0.7596537378d-10, &
              -0.390658184d-11, &
              -0.20464654d-12, &
              -0.1042636d-13, &
              -0.57702d-15, &
              -0.2550d-16, &
              -0.210d-17, &
               0.2d-19, &
              -0.2d-19/
 !
 !   MACHINE-DEPENDENT CONSTANTS (Suitable for IEEE-arithmetic machines)
 !

   if (.not.setupMISC_done) call setupMISC

   x = abs(xvalue)

   if ( x < titty4 ) then
     struve_h1 = zero
   else if ( x < titty5 ) then
     xsq = x * x
     struve_h1 = twobpi * xsq
   else if ( x <= nine ) then
     xsq = x * x
     t = ( xsq / fortp5 - half ) - half
     struve_h1 = twobpi * xsq * cheval ( nterm1, arrh1, t )
   else if ( x <= sorgo(1) ) then
     xsq = x * x
     t = ( one82 - xsq ) / ( twenty + xsq )
     y1p = cheval ( nterm3, ay1asp, t )
     y1q = cheval ( nterm4, ay1asq, t ) / ( eight * x)
     xm3p4 = x - thpby4
     y1val = y1p * sin ( xm3p4 ) + y1q * cos ( xm3p4 )
     y1val = y1val * rt2bpi / sqrt ( x )
     t = ( tw02p5 - xsq ) / ( fortp5 + xsq )
     h1as = twobpi * cheval ( nterm2, arrh1a, t )
     struve_h1 = y1val + h1as
   else
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'STRUVE_H1 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument magnitude too large.'
     struve_h1 = zero
   endif

   return
 end function struve_h1
 !******************************************************************
 !>>>struve_l0.f90
 function struve_l0(xvalue)

 !*****************************************************************************80
 !
 !! STRUVE_L0 calculates the modified Struve Function of order 0.
 !
 !  Discussion:
 !
 !    This FUNCTION calculates the modified Struve Function of
 !    order 0, denoted L0(x), defined as the solution of the
 !    second-order equation
 !
 !      x*D(Df) + Df - x*f  =  2x/pi
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: STRUVE_L0, the value of the FUNCTION.
 !
   implicit none

   integer :: indsgn
   integer, parameter :: nterm1 = 25
   integer, parameter :: nterm2 = 14
   integer, parameter :: nterm3 = 21
   real(kind = DP) :: struve_l0
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arl0(0:27),arl0as(0:15),ai0ml0(0:23), &
        atehun,ch1,ch2,lnr2pi, &
        t,test,twent4,twent8,two88, xsq
   data twent4,twent8/24.0d0 , 28.0d0 /
   data two88,atehun/288.0d0 , 800.0d0/
   data lnr2pi/0.91893853320467274178d0/
   data arl0(0)/  0.42127458349979924863d0/
   data arl0(1)/ -0.33859536391220612188d0/
   data arl0(2)/  0.21898994812710716064d0/
   data arl0(3)/ -0.12349482820713185712d0/
   data arl0(4)/  0.6214209793866958440d-1/
   data arl0(5)/ -0.2817806028109547545d-1/
   data arl0(6)/  0.1157419676638091209d-1/
   data arl0(7)/ -0.431658574306921179d-2/
   data arl0(8)/  0.146142349907298329d-2/
   data arl0(9)/ -0.44794211805461478d-3/
   data arl0(10)/ 0.12364746105943761d-3/
   data arl0(11)/-0.3049028334797044d-4/
   data arl0(12)/ 0.663941401521146d-5/
   data arl0(13)/-0.125538357703889d-5/
   data arl0(14)/ 0.20073446451228d-6/
   data arl0(15)/-0.2588260170637d-7/
   data arl0(16)/ 0.241143742758d-8/
   data arl0(17)/-0.10159674352d-9/
   data arl0(18)/-0.1202430736d-10/
   data arl0(19)/ 0.262906137d-11/
   data arl0(20)/-0.15313190d-12/
   data arl0(21)/-0.1574760d-13/
   data arl0(22)/ 0.315635d-14/
   data arl0(23)/-0.4096d-16/
   data arl0(24)/-0.3620d-16/
   data arl0(25)/ 0.239d-17/
   data arl0(26)/ 0.36d-18/
   data arl0(27)/-0.4d-19/
   data arl0as(0)/  2.00861308235605888600d0/
   data arl0as(1)/  0.403737966500438470d-2/
   data arl0as(2)/ -0.25199480286580267d-3/
   data arl0as(3)/  0.1605736682811176d-4/
   data arl0as(4)/ -0.103692182473444d-5/
   data arl0as(5)/  0.6765578876305d-7/
   data arl0as(6)/ -0.444999906756d-8/
   data arl0as(7)/  0.29468889228d-9/
   data arl0as(8)/ -0.1962180522d-10/
   data arl0as(9)/  0.131330306d-11/
   data arl0as(10)/-0.8819190d-13/
   data arl0as(11)/ 0.595376d-14/
   data arl0as(12)/-0.40389d-15/
   data arl0as(13)/ 0.2651d-16/
   data arl0as(14)/-0.208d-17/
   data arl0as(15)/ 0.11d-18/
   data ai0ml0(0)/ 2.00326510241160643125d0/
   data ai0ml0(1)/ 0.195206851576492081d-2/
   data ai0ml0(2)/ 0.38239523569908328d-3/
   data ai0ml0(3)/ 0.7534280817054436d-4/
   data ai0ml0(4)/ 0.1495957655897078d-4/
   data ai0ml0(5)/ 0.299940531210557d-5/
   data ai0ml0(6)/ 0.60769604822459d-6/
   data ai0ml0(7)/ 0.12399495544506d-6/
   data ai0ml0(8)/ 0.2523262552649d-7/
   data ai0ml0(9)/ 0.504634857332d-8/
   data ai0ml0(10)/0.97913236230d-9/
   data ai0ml0(11)/0.18389115241d-9/
   data ai0ml0(12)/0.3376309278d-10/
   data ai0ml0(13)/0.611179703d-11/
   data ai0ml0(14)/0.108472972d-11/
   data ai0ml0(15)/0.18861271d-12/
   data ai0ml0(16)/0.3280345d-13/
   data ai0ml0(17)/0.565647d-14/
   data ai0ml0(18)/0.93300d-15/
   data ai0ml0(19)/0.15881d-15/
   data ai0ml0(20)/0.2791d-16/
   data ai0ml0(21)/0.389d-17/
   data ai0ml0(22)/0.70d-18/
   data ai0ml0(23)/0.16d-18/
 !
 !   MACHINE-DEPENDENT VALUES (Suitable for IEEE-arithmetic machines)
 !
   if (.not.setupMISC_done) call setupMISC

   x = xvalue
   indsgn = 1
   if ( x < zero ) then
     x = -x
     indsgn = -1
   endif

   if ( x < three*sceps_DP ) then
     struve_l0 = twobpi * x
   else if ( x <= sixteen ) then
     t = ( four * x - twent4 ) / ( x + twent4 )
     struve_l0 = twobpi * x * cheval ( nterm1, arl0, t ) * exp ( x )
   else
 !
 !   Code for |xvalue| > 16
 !
      if ( sorgo(8) < x ) then
         ch1 = one
      else
         t = ( x - twent8 ) / ( four - x )
         ch1 = cheval ( nterm2, arl0as, t )
      endif

      if ( sxhi1 < x ) then
         ch2 = one
      else
         xsq = x * x
         t = ( atehun - xsq ) / ( two88 + xsq )
         ch2 = cheval ( nterm3, ai0ml0, t )
      endif

      test = log ( ch1 ) - lnr2pi - log ( x ) / two + x

      if ( log ( xmax ) < test ) then
         if (write_errors==1) print'(a)', ' '
         if (write_errors==1) print'(a)', 'STRUVE_L0 - Fatal error!'
         if (write_errors==1) print'(a)', '  Argument would cause overflow.'
         struve_l0 = xmax
      else
         struve_l0 = exp ( test ) - twobpi * ch2 / x
      endif

   endif

   if ( indsgn == -1 ) then
     struve_l0 = -struve_l0
   endif

   return
 end function struve_l0
 !******************************************************************
 !>>>struve_l1.f90
 function struve_l1(xvalue)

 !*****************************************************************************80
 !
 !! STRUVE_L1 calculates the modified Struve Function of order 1.
 !
 !  Discussion:
 !
 !    This FUNCTION calculates the modified Struve Function of
 !    order 1, denoted L1(x), defined as the solution of
 !
 !      x*x*D(Df) + x*Df - (x*x+1)f = 2 * x * x / pi
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: STRUVE_L1, the value of the FUNCTION.
 !
   implicit none

   integer, parameter :: nterm1 = 24
   integer, parameter :: nterm2 = 13
   integer, parameter :: nterm3 = 22
   real(kind = DP) :: struve_l1
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: arl1(0:26),arl1as(0:16),ai1ml1(0:25), &
        atehun,ch1,ch2,lnr2pi, &
        pi3by2,t,test,thirty,twent4, &
        two88,xsq
   data twent4,thirty/24.0d0, 30.0d0/
   data two88,atehun/288.0d0, 800.0d0/
   data lnr2pi/0.91893853320467274178d0/
   data pi3by2/4.71238898038468985769d0/
   data arl1(0)/  0.38996027351229538208d0/
   data arl1(1)/ -0.33658096101975749366d0/
   data arl1(2)/  0.23012467912501645616d0/
   data arl1(3)/ -0.13121594007960832327d0/
   data arl1(4)/  0.6425922289912846518d-1/
   data arl1(5)/ -0.2750032950616635833d-1/
   data arl1(6)/  0.1040234148637208871d-1/
   data arl1(7)/ -0.350532294936388080d-2/
   data arl1(8)/  0.105748498421439717d-2/
   data arl1(9)/ -0.28609426403666558d-3/
   data arl1(10)/ 0.6925708785942208d-4/
   data arl1(11)/-0.1489693951122717d-4/
   data arl1(12)/ 0.281035582597128d-5/
   data arl1(13)/-0.45503879297776d-6/
   data arl1(14)/ 0.6090171561770d-7/
   data arl1(15)/-0.623543724808d-8/
   data arl1(16)/ 0.38430012067d-9/
   data arl1(17)/ 0.790543916d-11/
   data arl1(18)/-0.489824083d-11/
   data arl1(19)/ 0.46356884d-12/
   data arl1(20)/ 0.684205d-14/
   data arl1(21)/-0.569748d-14/
   data arl1(22)/ 0.35324d-15/
   data arl1(23)/ 0.4244d-16/
   data arl1(24)/-0.644d-17/
   data arl1(25)/-0.21d-18/
   data arl1(26)/ 0.9d-19/
   data arl1as(0)/  1.97540378441652356868d0/
   data arl1as(1)/ -0.1195130555088294181d-1/
   data arl1as(2)/  0.33639485269196046d-3/
   data arl1as(3)/ -0.1009115655481549d-4/
   data arl1as(4)/  0.30638951321998d-6/
   data arl1as(5)/ -0.953704370396d-8/
   data arl1as(6)/  0.29524735558d-9/
   data arl1as(7)/ -0.951078318d-11/
   data arl1as(8)/  0.28203667d-12/
   data arl1as(9)/ -0.1134175d-13/
   data arl1as(10)/ 0.147d-17/
   data arl1as(11)/-0.6232d-16/
   data arl1as(12)/-0.751d-17/
   data arl1as(13)/-0.17d-18/
   data arl1as(14)/ 0.51d-18/
   data arl1as(15)/ 0.23d-18/
   data arl1as(16)/ 0.5d-19/
   data ai1ml1(0)/  1.99679361896789136501d0/
   data ai1ml1(1)/ -0.190663261409686132d-2/
   data ai1ml1(2)/ -0.36094622410174481d-3/
   data ai1ml1(3)/ -0.6841847304599820d-4/
   data ai1ml1(4)/ -0.1299008228509426d-4/
   data ai1ml1(5)/ -0.247152188705765d-5/
   data ai1ml1(6)/ -0.47147839691972d-6/
   data ai1ml1(7)/ -0.9020819982592d-7/
   data ai1ml1(8)/ -0.1730458637504d-7/
   data ai1ml1(9)/ -0.332323670159d-8/
   data ai1ml1(10)/-0.63736421735d-9/
   data ai1ml1(11)/-0.12180239756d-9/
   data ai1ml1(12)/-0.2317346832d-10/
   data ai1ml1(13)/-0.439068833d-11/
   data ai1ml1(14)/-0.82847110d-12/
   data ai1ml1(15)/-0.15562249d-12/
   data ai1ml1(16)/-0.2913112d-13/
   data ai1ml1(17)/-0.543965d-14/
   data ai1ml1(18)/-0.101177d-14/
   data ai1ml1(19)/-0.18767d-15/
   data ai1ml1(20)/-0.3484d-16/
   data ai1ml1(21)/-0.643d-17/
   data ai1ml1(22)/-0.118d-17/
   data ai1ml1(23)/-0.22d-18/
   data ai1ml1(24)/-0.4d-19/
   data ai1ml1(25)/-0.1d-19/
 !
 !   MACHINE-DEPENDENT VALUES (Suitable for IEEE-arithmetic machines)
 !

   if (.not.setupMISC_done) call setupMISC

   x = abs(xvalue)

   if ( x <= titty6 ) then
     struve_l1 = zero
   else if ( x < sceps_DP*sr3*sr5 ) then
     xsq = x * x
     struve_l1 = xsq / pi3by2
   else if ( x <= sixteen ) then
     xsq = x * x
     t = ( four * x - twent4 ) / ( x + twent4 )
     struve_l1 = xsq * cheval ( nterm1, arl1, t ) * exp ( x ) / pi3by2
   else

     if ( sorgo(9) < x ) then
       ch1 = one
     else
       t = ( x - thirty ) / ( two - x )
       ch1 = cheval ( nterm2, arl1as, t )
     endif

     if ( sxhi1 < x ) then
       ch2 = one
     else
       xsq = x * x
       t = ( atehun - xsq ) / ( two88 + xsq )
       ch2 = cheval ( nterm3, ai1ml1, t )
     endif

     test = log ( ch1 ) - lnr2pi - log ( x ) / two + x

     if ( log ( xmax ) < test ) then
       if (write_errors==1) print'(a)', ' '
       if (write_errors==1) print'(a)', 'STRUVE_L1 - Fatal error!'
       if (write_errors==1) print'(a)', '  Argument would cause overflow.'
       struve_l1 = xmax
     else
       struve_l1 = exp ( test ) - twobpi * ch2
     endif

   endif

   return
 end function struve_l1
 !******************************************************************
 !>>>synch1.f90
 function synch1(xvalue)

 !*****************************************************************************80
 !
 !! SYNCH1 calculates the synchrotron radiation Function.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      SYNCH1(x) = x * Integral ( x <= t < infinity ) K(5/3)(t) dt
 !
 !    where K(5/3) is a modified Bessel Function of order 5/3.
 !
 !    The code uses Chebyshev expansions, the coefficients of which
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 September 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: SYNCH1, the value of the FUNCTION.
 !
   implicit none

   real(DP),parameter :: trep8=3.8_DP
   integer, parameter :: nterm1 = 12
   integer, parameter :: nterm2 = 10
   integer, parameter :: nterm3 = 21
   real(kind = DP) :: synch1
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: async1(0:13),async2(0:11),asynca(0:24), &
        cheb1,cheb2,conlow, &
        lnrtp2,pibrt3,t,xpowth
   data conlow/2.14952824153447863671d0/
   data pibrt3/1.81379936423421785059d0/
   data lnrtp2/0.22579135264472743236d0/
   data async1/30.36468298250107627340d0, &
               17.07939527740839457449d0, &
                4.56013213354507288887d0, &
                0.54928124673041997963d0, &
                0.3729760750693011724d-1, &
                0.161362430201041242d-2, &
                0.4819167721203707d-4, &
                0.105124252889384d-5, &
                0.1746385046697d-7, &
                0.22815486544d-9, &
                0.240443082d-11, &
                0.2086588d-13, &
                0.15167d-15, &
                0.94d-18/
   data async2/0.44907216235326608443d0, &
               0.8983536779941872179d-1, &
               0.810445737721512894d-2, &
               0.42617169910891619d-3, &
               0.1476096312707460d-4, &
               0.36286336153998d-6, &
               0.666348074984d-8, &
               0.9490771655d-10, &
               0.107912491d-11, &
               0.1002201d-13, &
               0.7745d-16, &
               0.51d-18/
   data asynca(0)/ 2.13293051613550009848d0/
   data asynca(1)/ 0.7413528649542002401d-1/
   data asynca(2)/ 0.869680999099641978d-2/
   data asynca(3)/ 0.117038262487756921d-2/
   data asynca(4)/ 0.16451057986191915d-3/
   data asynca(5)/ 0.2402010214206403d-4/
   data asynca(6)/ 0.358277563893885d-5/
   data asynca(7)/ 0.54477476269837d-6/
   data asynca(8)/ 0.8388028561957d-7/
   data asynca(9)/ 0.1306988268416d-7/
   data asynca(10)/0.205309907144d-8/
   data asynca(11)/0.32518753688d-9/
   data asynca(12)/0.5179140412d-10/
   data asynca(13)/0.830029881d-11/
   data asynca(14)/0.133527277d-11/
   data asynca(15)/0.21591498d-12/
   data asynca(16)/0.3499673d-13/
   data asynca(17)/0.569942d-14/
   data asynca(18)/0.92906d-15/
   data asynca(19)/0.15222d-15/
   data asynca(20)/0.2491d-16/
   data asynca(21)/0.411d-17/
   data asynca(22)/0.67d-18/
   data asynca(23)/0.11d-18/
   data asynca(24)/0.2d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'SYNCH1 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     synch1 = zero
   else if ( x < two*sceps_DP ) then
     xpowth = exp(log(x)*unter)
     synch1 = conlow * xpowth
!   else if ( x <= four ) then !corr.
   else if ( x <= trep8 ) then
     xpowth = exp(log(x)*unter)
     t = ( x * x / eight - half ) - half
     cheb1 = cheval ( nterm1, async1, t )
     cheb2 = cheval ( nterm2, async2, t )
     t = xpowth * cheb1 - xpowth**11 * cheb2
     synch1 = t - pibrt3 * x
   else if ( x <= loghuge_eps ) then
     t = ( twelve - x ) / ( x + four )
     cheb1 = cheval ( nterm3, asynca, t )
     t = lnrtp2 - x + log( sqrt( x ) * cheb1 )
     if ( t < logtiny ) then
       synch1 = zero
     else
       synch1 = exp(t)
     endif
   else
     synch1 = zero
   endif

   return
 end function synch1
 !******************************************************************
 !>>>synch2.f90
 function synch2(xvalue)

 !*****************************************************************************80
 !
 !! SYNCH2 calculates the synchrotron radiation Function.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      SYNCH2(x) = x * K(2/3)(x)
 !
 !    where K(2/3) is a modified Bessel Function of order 2/3.
 !
 !    The code uses Chebyshev expansions, the coefficients of which
 !    are given to 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: SYNCH2, the value of the FUNCTION.
 !
   implicit none

   integer, parameter :: nterm1 = 13
   integer, parameter :: nterm2 = 12
   integer, parameter :: nterm3 = 16
   real(kind = DP) :: synch2
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: asyn21(0:14),asyn22(0:13),asyn2a(0:18), &
        cheb1,cheb2,conlow, &
        lnrtp2,t,xpowth
   data conlow/1.07476412076723931836d0/
   data lnrtp2/0.22579135264472743236d0/
   data asyn21/38.61783992384308548014d0, &
               23.03771559496373459697d0, &
                5.38024998683357059676d0, &
                0.61567938069957107760d0, &
                0.4066880046688955843d-1, &
                0.172962745526484141d-2, &
                0.5106125883657699d-4, &
                0.110459595022012d-5, &
                0.1823553020649d-7, &
                0.23707698034d-9, &
                0.248872963d-11, &
                0.2152868d-13, &
                0.15607d-15, &
                0.96d-18, &
                0.1d-19/
   data asyn22/7.90631482706608042875d0, &
               3.13534636128534256841d0, &
               0.48548794774537145380d0, &
               0.3948166758272372337d-1, &
               0.196616223348088022d-2, &
               0.6590789322930420d-4, &
               0.158575613498559d-5, &
               0.2868653011233d-7, &
               0.40412023595d-9, &
               0.455684443d-11, &
               0.4204590d-13, &
               0.32326d-15, &
               0.210d-17, &
               0.1d-19/
   data asyn2a/2.02033709417071360032d0, &
               0.1095623712180740443d-1, &
               0.85423847301146755d-3, &
               0.7234302421328222d-4, &
               0.631244279626992d-5, &
               0.56481931411744d-6, &
               0.5128324801375d-7, &
               0.471965329145d-8, &
               0.43807442143d-9, &
               0.4102681493d-10, &
               0.386230721d-11, &
               0.36613228d-12, &
               0.3480232d-13, &
               0.333010d-14, &
               0.31856d-15, &
               0.3074d-16, &
               0.295d-17, &
               0.29d-18, &
               0.3d-19/
 !
 !   Machine-dependent constants (suitable for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'SYNCH2 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     synch2 = zero
   else if ( x < two*sceps_DP ) then
     xpowth = x ** ( one / three )
     synch2 = conlow * xpowth
   else if ( x <= four ) then
     xpowth = x ** ( one / three )
     t = ( x * x / eight - half ) - half
     cheb1 = cheval ( nterm1, asyn21, t )
     cheb2 = cheval ( nterm2, asyn22, t )
     synch2 = xpowth * cheb1 - xpowth**5 * cheb2
   else if ( x <= loghuge_eps ) then
     t = ( ten - x ) / ( x + two )
     cheb1 = cheval ( nterm3, asyn2a, t )
     t = lnrtp2 - x + log ( sqrt ( x ) * cheb1 )
     if ( t < logtiny ) then
       synch2 = zero
     else
       synch2 = exp ( t )
     endif
   else
     synch2 = zero
   endif

   return
 end function synch2
 !******************************************************************
 !>>>tran02.f90
 function tran02(xvalue)

 !*****************************************************************************80
 !
 !! TRAN02 calculates the transport integral of order 2.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN02(x) = Integral ( 0 <= t <= x ) t^2 exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN02, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer, parameter :: numjn = 2
   real(kind = DP) :: tran02
   real(kind = DP), parameter :: valinf = 0.32898681336964528729D+01
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t, &
        xk,xk1
   data rnumjn/ 2.0d0 /

   data atran/1.67176044643453850301d0, &
             -0.14773535994679448986d0, &
              0.1482138199469363384d-1, &
             -0.141953303263056126d-2, &
              0.13065413244157083d-3, &
             -0.1171557958675790d-4, &
              0.103334984457557d-5, &
             -0.9019113042227d-7, &
              0.781771698331d-8, &
             -0.67445656840d-9, &
              0.5799463945d-10, &
             -0.497476185d-11, &
              0.42596097d-12, &
             -0.3642189d-13, &
              0.311086d-14, &
             -0.26547d-15, &
              0.2264d-16, &
             -0.193d-17, &
              0.16d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !
   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN02 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran02 = zero
   else if ( x < two*sceps_DP ) then
     tran02 =  ( x ** ( numjn - 1 ) ) / ( rnumjn - one )
   else if ( x <= four ) then
     t = ( ( ( x * x ) / eight ) - half ) - half
     tran02 = ( x ** ( numjn - 1 ) ) * cheval ( nterms, atran, t )
   else

      if ( xhigh2T(1) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif

      t = rnumjn * log ( x ) - x + log ( sumexp )
      if ( t < xhighL3 ) then
         tran02 = valinf
      else
         tran02 = valinf - exp ( t )
      endif

   endif

   return
 end function tran02
 !******************************************************************
 !>>>tran03.f90
 function tran03(xvalue)

 !*****************************************************************************80
 !
 !! TRAN03 calculates the transport integral of order 3.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN03(x) = Integral ( 0 <= t <= x ) t^3 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN03, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: atran(0:19)
   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran03
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 3 , 3.0d0 /
   data valinf/0.72123414189575657124d1/
   data atran/0.76201254324387200657d0, &
             -0.10567438770505853250d0, &
              0.1197780848196578097d-1, &
             -0.121440152036983073d-2, &
              0.11550997693928547d-3, &
             -0.1058159921244229d-4, &
              0.94746633853018d-6, &
             -0.8362212128581d-7, &
              0.731090992775d-8, &
             -0.63505947788d-9, &
              0.5491182819d-10, &
             -0.473213954d-11, &
              0.40676948d-12, &
             -0.3489706d-13, &
              0.298923d-14, &
             -0.25574d-15, &
              0.2186d-16, &
             -0.187d-17, &
              0.16d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN03 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran03 = zero
   else if ( x < tittyx(1) ) then
     tran03 = zero
   else if ( x < two*sceps_DP ) then
     tran03 = ( x**( numjn - 1 ) ) / ( rnumjn - one )
   else if ( x <= four ) then
     t = ( ( ( x*x ) / eight ) - half ) - half
     tran03 = ( x**( numjn - 1 ) ) * cheval ( nterms, atran, t )
   else

      if ( xhigh2T(2) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif

      t = rnumjn * log ( x ) - x + log ( sumexp )

      if ( t < xhighL3 ) then
         tran03 = valinf
      else
         tran03 = valinf - exp ( t )
      endif

   endif

   return
 end function tran03
 !******************************************************************
 !>>>tran04.f90
 function tran04(xvalue)

 !*****************************************************************************80
 !
 !! TRAN04 calculates the transport integral of order 4.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN04(x) = Integral ( 0 <= t <= x ) t^4 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN04, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran04
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 4 , 4.0d0 /
   data valinf/0.25975757609067316596d2/
   data atran/0.48075709946151105786d0, &
             -0.8175378810321083956d-1, &
              0.1002700665975162973d-1, &
             -0.105993393598201507d-2, &
              0.10345062450304053d-3, &
             -0.964427054858991d-5, &
              0.87455444085147d-6, &
             -0.7793212079811d-7, &
              0.686498861410d-8, &
             -0.59995710764d-9, &
              0.5213662413d-10, &
             -0.451183819d-11, &
              0.38921592d-12, &
             -0.3349360d-13, &
              0.287667d-14, &
             -0.24668d-15, &
              0.2113d-16, &
             -0.181d-17, &
              0.15d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN04 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran04 = zero
     return
   endif
 !
 !   Code for x < =  4.0
 !
   if ( x <= four ) then
      if ( x < tittyx(2) ) then
         tran04 = zero
      else
         if ( x < two*sceps_DP ) then
            tran04 =  ( x ** ( numjn-1 ) ) / ( rnumjn - one )
         else
            t = ( ( ( x * x ) / eight ) - half ) - half
            tran04 = ( x ** ( numjn-1 ) ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4.0
 !
      if ( xhigh2T(3) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif

      t = rnumjn * log ( x ) - x + log ( sumexp )

      if ( t < xhighL3 ) then
         tran04 = valinf
      else
         tran04 = valinf - exp ( t )
      endif

   endif

   return
 end function tran04
 !******************************************************************
 !>>>tran05.f90
 function tran05(xvalue)

 !*****************************************************************************80
 !
 !! TRAN05 calculates the transport integral of order 5.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN05(x) = Integral ( 0 <= t <= x ) t^5 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN05, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran05
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 5 , 5.0d0 /
   data valinf/0.12443133061720439116d3/
   data atran/0.34777777713391078928d0, &
             -0.6645698897605042801d-1, &
              0.861107265688330882d-2, &
             -0.93966822237555384d-3, &
              0.9363248060815134d-4, &
             -0.885713193408328d-5, &
              0.81191498914503d-6, &
             -0.7295765423277d-7, &
              0.646971455045d-8, &
             -0.56849028255d-9, &
              0.4962559787d-10, &
             -0.431093996d-11, &
              0.37310094d-12, &
             -0.3219769d-13, &
              0.277220d-14, &
             -0.23824d-15, &
              0.2044d-16, &
             -0.175d-17, &
              0.15d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN05 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran05 = zero
     return
   endif
 !
 !   Code for x < =  4.0
 !
   if ( x <= four ) then
      if ( x < tittyx(3) ) then
         tran05 = zero
      else
         if ( x < two*sceps_DP ) then
            tran05 =  ( x ** ( numjn - 1 ) ) / ( rnumjn - one )
         else
            t = ( ( ( x * x ) / eight ) - half ) - half
            tran05 = ( x ** ( numjn-1 ) ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4.0
 !
      if ( xhigh2T(4) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x )  + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif
      t = rnumjn * log ( x ) - x + log ( sumexp )
      if ( t < xhighL3 ) then
         tran05 = valinf
      else
         tran05 = valinf - exp ( t )
      endif
   endif

   return
 end function tran05
 !******************************************************************
 !>>>tran06.f90
 function tran06(xvalue)

 !*****************************************************************************80
 !
 !! TRAN06 calculates the transport integral of order 6.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN06(x) = Integral ( 0 <= t <= x ) t^6 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN06, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran06
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 6 , 6.0d0 /
   data valinf/0.73248700462880338059d3/
   data atran/0.27127335397840008227d0, &
             -0.5588610553191453393d-1, &
              0.753919513290083056d-2, &
             -0.84351138579211219d-3, &
              0.8549098079676702d-4, &
             -0.818715493293098d-5, &
              0.75754240427986d-6, &
             -0.6857306541831d-7, &
              0.611700376031d-8, &
             -0.54012707024d-9, &
              0.4734306435d-10, &
             -0.412701055d-11, &
              0.35825603d-12, &
             -0.3099752d-13, &
              0.267501d-14, &
             -0.23036d-15, &
              0.1980d-16, &
             -0.170d-17, &
              0.15d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN06 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran06 = zero
     return
   endif
 !
 !   Code for x < =  4 .0
 !
   if ( x <= four ) then
      if ( x < tittyx(4) ) then
         tran06 = zero
      else
         if ( x < two*sceps_DP ) then
            tran06 =  ( x ** ( numjn-1 ) ) / ( rnumjn - one )
         else
            t =  ( ( ( x * x ) / eight ) - half ) - half
            tran06 = ( x ** ( numjn-1 )  ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4 .0
 !
      if ( xhigh2T(5) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( - x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif
      t = rnumjn * log ( x ) - x + log ( sumexp )
      if ( t < xhighL3 ) then
         tran06 = valinf
      else
         tran06 = valinf - exp ( t )
      endif
   endif

   return
 end function tran06
 !******************************************************************
 !>>>tran07.f90
 function tran07(xvalue)

 !*****************************************************************************80
 !
 !! TRAN07 calculates the transport integral of order 7.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN07(x) = Integral ( 0 <= t <= x ) t^7 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN07, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran07
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 7 , 7.0d0/
   data valinf/0.50820803580048910473d4/
   data atran/0.22189250734010404423d0, &
             -0.4816751061177993694d-1, &
              0.670092448103153629d-2, &
             -0.76495183443082557d-3, &
              0.7863485592348690d-4, &
             -0.761025180887504d-5, &
              0.70991696299917d-6, &
             -0.6468025624903d-7, &
              0.580039233960d-8, &
             -0.51443370149d-9, &
              0.4525944183d-10, &
             -0.395800363d-11, &
              0.34453785d-12, &
             -0.2988292d-13, &
              0.258434d-14, &
             -0.22297d-15, &
              0.1920d-16, &
             -0.165d-17, &
              0.14d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN07 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran07 = zero
     return
   endif
 !
 !   Code for x <= 4.0
 !
   if ( x <= four ) then
      if ( x < tittyx(5) ) then
         tran07 = zero
      else
         if ( x < two*sceps_DP ) then
            tran07 = ( x**(numjn-1) ) / ( rnumjn - one )
         else
            t = ( ( ( x * x ) / eight ) - half ) - half
            tran07 = ( x**(numjn-1) ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4.0
 !
      if ( xhigh2T(6) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif

      t = rnumjn * log ( x ) - x + log ( sumexp )

      if ( t < xhighL3 ) then
         tran07 = valinf
      else
         tran07 = valinf - exp ( t )
      endif

   endif

   return
 end function tran07
 !******************************************************************
 !>>>tran08.f90
 function tran08(xvalue)

 !*****************************************************************************80
 !
 !! TRAN08 calculates the transport integral of order 8.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN08(x) = Integral ( 0 <= t <= x ) t^8 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN08, the value of the FUNCTION.
 !
   implicit none

   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer :: numjn
   real(kind = DP) :: tran08
   real(kind = DP) :: x
   real(kind = DP),intent(IN) :: xvalue

   real(kind = DP) :: atran(0:19),rk, &
        rnumjn,sumexp,sum2,t,valinf, &
        xk,xk1
   data numjn,rnumjn/ 8, 8.0d0 /
   data valinf/0.40484399001901115764d5/
   data atran/0.18750695774043719233d0, &
             -0.4229527646093673337d-1, &
              0.602814856929065592d-2, &
             -0.69961054811814776d-3, &
              0.7278482421298789d-4, &
             -0.710846250050067d-5, &
              0.66786706890115d-6, &
             -0.6120157501844d-7, &
              0.551465264474d-8, &
             -0.49105307052d-9, &
              0.4335000869d-10, &
             -0.380218700d-11, &
              0.33182369d-12, &
             -0.2884512d-13, &
              0.249958d-14, &
             -0.21605d-15, &
              0.1863d-16, &
             -0.160d-17, &
              0.14d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN08 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran08 = zero
     return
   endif
 !
 !   Code for x < =  4.0
 !
   if ( x <= four ) then
      if ( x < tittyx(6) ) then
         tran08 = zero
      else
         if ( x < two*sceps_DP ) then
            tran08 = ( x ** ( numjn - 1 ) ) / ( rnumjn - one )
         else
            t = ( ( ( x * x ) / eight ) - half ) - half
            tran08 = ( x ** ( numjn - 1 ) ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4.0
 !
      if ( xhigh2T(7) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( - x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif
      t = rnumjn * log ( x ) - x + log ( sumexp )
      if ( t < xhighL3 ) then
         tran08 = valinf
      else
         tran08 = valinf - exp ( t )
      endif
   endif

   return
 end function tran08
 !******************************************************************
 !>>>tran09.f90
 function tran09(xvalue)

 !*****************************************************************************80
 !
 !! TRAN09 calculates the transport integral of order 9.
 !
 !  Discussion:
 !
 !    The Function is defined by:
 !
 !      TRAN09(x) = Integral ( 0 <= t <= x ) t^9 * exp(t) / ( exp(t) - 1 )^2 dt
 !
 !    The program uses a Chebyshev series, the coefficients of which are
 !    given to an accuracy of 20 decimal places.
 !
 !    This FUNCTION is set up to work on IEEE machines.
 !
 !  Modified:
 !
 !    07 August 2004
 !
 !  Author:
 !
 !    Allan McLeod,
 !    Department of Mathematics and Statistics,
 !    Paisley University, High Street, Paisley, Scotland, PA12BE
 !    macl_ms0@paisley.ac.uk
 !
 !  Reference:
 !
 !    Allan McLeod,
 !    Algorithm 757, MISCFUN: A software package to compute uncommon
 !      Special Functions,
 !    ACM Transactions on Mathematical Software,
 !    Volume 22, Number 3, September 1996, pages 288-301.
 !
 !  Parameters:
 !
 !    Input, real(kind = DP) :: XVALUE, the argument of the FUNCTION.
 !
 !    Output, real(kind = DP) :: TRAN09, the value of the FUNCTION.
 !
   implicit none

   real(kind = DP) :: atran(0:19)
   integer :: k1
   integer :: k2
   integer, parameter :: nterms = 17
   integer :: numexp
   integer, parameter :: numjn = 9
   real(kind = DP) :: rk
   real(kind = DP), parameter :: rnumjn = 9.0D+00
   real(kind = DP) :: sumexp
   real(kind = DP) :: sum2
   real(kind = DP) :: t
   real(kind = DP) :: tran09
   real(kind = DP), parameter :: valinf = 0.36360880558872871397d6
   real(kind = DP) :: x
   real(kind = DP) :: xk
   real(kind = DP) :: xk1
   real(kind = DP),intent(IN) :: xvalue

   data atran/0.16224049991949846835d0, &
             -0.3768351452195937773d-1, &
              0.547669715917719770d-2, &
             -0.64443945009449521d-3, &
              0.6773645285280983d-4, &
             -0.666813497582042d-5, &
              0.63047560019047d-6, &
             -0.5807478663611d-7, &
              0.525551305123d-8, &
             -0.46968861761d-9, &
              0.4159395065d-10, &
             -0.365808491d-11, &
              0.32000794d-12, &
             -0.2787651d-13, &
              0.242017d-14, &
             -0.20953d-15, &
              0.1810d-16, &
             -0.156d-17, &
              0.13d-18, &
             -0.1d-19/
 !
 !  Machine-dependent constants (for IEEE machines)
 !

   if (.not.setupMISC_done) call setupMISC
   x = xvalue

   if ( x < zero ) then
     if (write_errors==1) print'(a)', ' '
     if (write_errors==1) print'(a)', 'TRAN09 - Fatal error!'
     if (write_errors==1) print'(a)', '  Argument X < 0.'
     tran09 = zero
     return
   endif
 !
 !   Code for x < =  4.0
 !
   if ( x <= four ) then
      if ( x < tittyx(7) ) then
         tran09 = zero
      else
         if ( x < two*sceps_DP ) then
            tran09 = ( x ** ( numjn - 1 ) ) / ( rnumjn - one )
         else
            t = ( ( ( x * x ) / eight ) - half ) - half
            tran09 = ( x ** ( numjn - 1 ) ) * cheval ( nterms, atran, t )
         endif
      endif
   else
 !
 !  Code for x > 4.0
 !
      if ( xhigh2T(8) < x ) then
         sumexp = one
      else
         if ( x <= xhighL1 ) then
            numexp = int ( xhighL1 / x ) + 1
            t = exp ( -x )
         else
            numexp = 1
            t = one
         endif
         rk = zero
         do k1 = 1, numexp
            rk = rk + one
         end do
         sumexp = zero
         do k1 = 1, numexp
            sum2 = one
            xk = one / ( rk * x )
            xk1 = one
            do k2 = 1, numjn
               sum2 = sum2 * xk1 * xk + one
               xk1 = xk1 + one
            end do
            sumexp = sumexp * t + sum2
            rk = rk - one
         end do
      endif

      t = rnumjn * log ( x ) - x + log ( sumexp )

      if ( t < xhighL3 ) then
         tran09 = valinf
      else
         tran09 = valinf - exp ( t )
      endif

   endif

   return
 end function tran09
 !_________________________ TEST ALL
 subroutine test_all_miscfun()
 implicit none
 integer :: jtest,j,nd,iu
 real(DP) :: aerr,rerr,berr

 do jtest=1,numsub
   if (jtest==1) then
     nd=0
     do j=1,number_testvalues
       call abram0_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = abram0(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==2) then
     nd=0
     do j=1,number_testvalues
       call abram1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = abram1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==3) then
     nd=0
     do j=1,number_testvalues
       call abram2_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = abram2(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==4) then
     nd=0
     do j=1,number_testvalues
       call airy_ai_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = airy_ai_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==5) then
     nd=0
     do j=1,number_testvalues
       call airy_bi_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = airy_bi_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==6) then
     nd=0
     do j=1,number_testvalues
       call airy_gi_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = airy_gi(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==7) then
     nd=0
     do j=1,number_testvalues
       call airy_hi_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = airy_hi(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==8) then
     nd=0
     do j=1,number_testvalues
       call arctan_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = arctan_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==9) then
     nd=0
     do j=1,number_testvalues
       call bessel_i0_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = bessel_i0_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==10) then
     nd=0
     do j=1,number_testvalues
       call bessel_j0_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = bessel_j0_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==11) then
     nd=0
     do j=1,number_testvalues
       call bessel_k0_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = bessel_k0_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==12) then
     nd=0
     do j=1,number_testvalues
       call bessel_y0_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = bessel_y0_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==13) then
     nd=0
     do j=1,number_testvalues
       call clausen_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = clausen(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==14) then
     nd=0
     do j=1,number_testvalues
       call debye1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = debye1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==15) then
     nd=0
     do j=1,number_testvalues
       call debye2_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = debye2(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==16) then
     nd=0
     do j=1,number_testvalues
       call debye3_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = debye3(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==17) then
     nd=0
     do j=1,number_testvalues
       call debye4_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = debye4(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==18) then
     nd=0
     do j=1,number_testvalues
       call exp3_int_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = exp3_integral(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==19) then
     nd=0
     do j=1,number_testvalues
       call goodwin_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = goodwin(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==20) then
     nd=0
     do j=1,number_testvalues
       call i0ml0_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = i0ml0(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==21) then
     nd=0
     do j=1,number_testvalues
       call i1ml1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = i1ml1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==22) then
     nd=0
     do j=1,number_testvalues
       call lobachevsky_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = lobachevsky(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==23) then
     nd=0
     do j=1,number_testvalues
       call stromgen_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = stromgen(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==24) then
     nd=0
     do j=1,number_testvalues
       call struve_h0_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = struve_h0(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==25) then
     nd=0
     do j=1,number_testvalues
       call struve_h1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = struve_h1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==26) then
     nd=0
     do j=1,number_testvalues
       call struve_l0_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = struve_l0(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==27) then
     nd=0
     do j=1,number_testvalues
       call struve_l1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = struve_l1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==28) then
     nd=0
     do j=1,number_testvalues
       call synch1_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = synch1(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==29) then
     nd=0
     do j=1,number_testvalues
       call synch2_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = synch2(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==30) then
     nd=0
     do j=1,number_testvalues
       call tran02_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran02(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==31) then
     nd=0
     do j=1,number_testvalues
       call tran03_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran03(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==32) then
     nd=0
     do j=1,number_testvalues
       call tran04_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran04(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==33) then
     nd=0
     do j=1,number_testvalues
       call tran05_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran05(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==34) then
     nd=0
     do j=1,number_testvalues
       call tran06_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran06(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==35) then
     nd=0
     do j=1,number_testvalues
       call tran07_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran07(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==36) then
     nd=0
     do j=1,number_testvalues
       call tran08_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran08(xvalue = testval(1,j,jtest))
     enddo
   else if (jtest==37) then
     nd=0
     do j=1,number_testvalues
       call tran09_values(nd,testval(1,j,jtest),testval(2,j,jtest))
       testval(3,j,jtest) = tran09(xvalue = testval(1,j,jtest))
     enddo
   endif
 enddo

 iu=find_unit()
 open(iu,status='replace',file=file_test_miscfun)
 do jtest=1,numsub
   write(iu,'(a,a14)')'*** TESTING ',nasu(jtest)
   do j=1,number_testvalues
     aerr=abs(testval(2,j,jtest)-testval(3,j,jtest))
     berr=aerr/max(tiny_DP,abs(testval(2,j,jtest)))
     rerr=min(aerr,berr)/eps_DP
     if (rerr>twenty) then
       write(iu,'("ER",i3,5(1x,g28.20))')j,testval(1,j,jtest),testval(2,j,jtest),testval(3,j,jtest),rerr,aerr
     else if (rerr<=twenty.and.rerr>1) then
       write(iu,'("FG",i3,5(1x,g28.20))')j,testval(1,j,jtest),testval(2,j,jtest),testval(3,j,jtest),rerr,aerr
     else
       write(iu,'("OK",i3,3(1x,g28.20))')j,testval(1,j,jtest),testval(2,j,jtest),testval(3,j,jtest)
     endif
   enddo
 enddo
 close(iu)

end subroutine test_all_miscfun

end module miscfun_AC
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module ComVolFrac_CoreShell
use nano_deftyp
real(DP),parameter :: Pido=0.261799387799149436538553615273291907d0, & ! Pi/12
                      Pise=0.523598775598298873077107230546583814d0    ! Pi/6
contains
!************************************************************************************************
function SphereSphere(rc,d0,vcel)
implicit none
real(DP),intent(IN) :: rc,d0,vcel
real(DP) :: SphereSphere

if (d0<-eps_DP.or.d0>two*rc+eps_DP) then
  SphereSphere=zero
  return
endif
SphereSphere = CoreCore(rc,d0,vcel)


end function SphereSphere
!************************************************************************************************
function CoreCore(rc,d0,vcel)
implicit none
real(DP),intent(IN) :: rc,d0,vcel
real(DP) :: CoreCore
real(DP) :: d0s

if (d0<-eps_DP.or.d0>two*rc+eps_DP) then
  CoreCore=zero
  return
endif
d0s=max(d0,zero)
CoreCore = ((d0s-two*rc)**2)*(d0s+four*rc)*Pido/vcel


end function CoreCore
!************************************************************************************************
function ShellShell(rc,tsh,d0,vcel)
implicit none
real(DP),intent(IN) :: rc,d0,vcel,tsh
real(DP) :: ShellShell
real(DP) :: Rex,tworc,twoRex,sqd,d0s,d01,rcs,Rexs,rxsum,Pi6vc
logical :: thinsh

if (tsh<=eps_DP) then
  ShellShell=zero
  return
endif
  

Rex=rc+tsh
if (d0<-eps_DP.or.d0>two*Rex+eps_DP) then
  ShellShell=zero
  return
endif
tworc=two*rc; twoRex=two*Rex
thinsh = (tsh < tworc)
d0s=(max(d0,zero))**2
rcs=rc**2
Rexs=Rex**2
rxsum=Rex+rc
Pi6vc=Pise/vcel

if (thinsh) then
  if (d0>=-eps_DP.and.d0<tsh) then
    d01=max(d0,zero)
    ShellShell = (d01*(d0s-six*(Rex**2+rc**2))+eight*(Rex**3-rc**3))*Pi6vc
  else if (d0>=tsh.and.d0<tworc) then
    sqd=tsh*rxsum
    ShellShell = (sqd**2)*three*Pise/(vcel*d0)
  else if (d0>=tworc.and.d0<rxsum) then
    d01=one/d0
    sqd=tsh*rxsum
    ShellShell = ( -half*d0*d0s + rcs*(six*d0-eight*rc) + (sqd**2)*three*d01 )*Pi6vc
  else if (d0>=rxsum.and.d0<=twoRex) then
    ShellShell = ((d0-twoRex)**2)*(d0+four*Rex)*Pido/vcel
  endif
else
  if (d0>=-eps_DP.and.d0<tworc) then
    d01=max(d0,zero)
    ShellShell = (d01*(d0s-six*(Rex**2+rc**2))+eight*(Rex**3-rc**3))*Pi6vc
  else if (d0>=tworc.and.d0<tsh) then
    ShellShell = (d0*(half*d0s-six*Rexs)+eight*(Rex*Rexs-tworc*rcs))*Pi6vc
  else if (d0>=tsh.and.d0<rxsum) then
    d01=one/d0
    sqd=tsh*rxsum
    ShellShell = ( -half*d0*d0s + rcs*(six*d0-eight*rc) + (sqd**2)*three*d01 )*Pi6vc
  else if (d0>=rxsum.and.d0<=twoRex) then
    ShellShell = ((d0-twoRex)**2)*(d0+four*Rex)*Pido/vcel
  endif
endif
end function ShellShell
!************************************************************************************************
function CoreShell(rc,tsh,d0,vcel)
implicit none
real(DP),intent(IN) :: rc,d0,vcel,tsh
real(DP) :: CoreShell
real(DP) :: Rex,tworc,twoRex,sqd,d0s,d01,rcs,Rexs,rxsum,Pi6vc
logical :: thinsh

Rex=rc+tsh
if ((d0<eps_DP.or.d0>rc+Rex-eps_DP).or.(rc<eps_DP.or.tsh<eps_DP)) then
  CoreShell=zero
  return
endif
tworc=two*rc; twoRex=two*Rex
thinsh = (tsh < tworc)
d0s=(max(d0,zero))**2
rcs=rc**2
Rexs=Rex**2
rxsum=Rex+rc
Pi6vc=Pise/vcel

if (thinsh) then
  if (d0>=-eps_DP.and.d0<tsh) then
    d01=max(d0,zero)
    CoreShell = (d01*(-half*d0s+six*rcs))*Pi6vc
  else if (d0>=tsh.and.d0<tworc) then
    sqd=tsh*rxsum
    CoreShell = ( four*tsh*(Rexs+rcs+Rex*rc) + three*sqd*(-d0-half*sqd/d0) )*Pi6vc
  else if (d0>=tworc.and.d0<rxsum) then
    d01=one/d0
    sqd=tsh*rxsum
    CoreShell = ( d0*(half*d0s -three*(Rexs+rcs)) +four*(Rexs*Rex+rcs*rc)-three*half*((sqd**2)/d0) )*Pi6vc
  endif
else
  if (d0>=-eps_DP.and.d0<tworc) then
    d01=max(d0,zero)
    CoreShell = (d01*(-half*d0s+six*rcs))*Pi6vc
  else if (d0>=tworc.and.d0<tsh) then
    CoreShell = (eight*rc*rcs )*Pi6vc
  else if (d0>=tsh.and.d0<rxsum) then
    sqd=tsh*rxsum
    CoreShell = ( d0*(half*d0s -three*(Rexs+rcs)) +four*(Rexs*Rex+rcs*rc)-three*half*((sqd**2)/d0) )*Pi6vc
  endif
endif
end function CoreShell
!************************************************************************************************
function CS_All3(rc,tsh,d0,vcel)
implicit none
real(DP),intent(IN) :: rc,d0,vcel,tsh
real(DP) :: CS_All3(3)
real(DP) :: Rex,tworc,twoRex,sqd,d0s,d01,rcs,Rexs,rxsum,Pi6vc,Pi12vc
logical :: thinsh

Rex=rc+tsh
if (d0<-eps_DP.or.d0>two*Rex+eps_DP) then
  CS_All3=zero
  return
endif

Pi12vc=Pido/vcel
if (d0>two*rc+eps_DP) then
  CS_All3(1)=zero
else
  d0s=max(d0,zero)
  CS_All3(1) = ((d0s-two*rc)**2)*(d0s+four*rc)*Pi12vc
endif
if (tsh<=eps_DP) then
  CS_All3(2:3)=zero
  return
endif

tworc=two*rc; twoRex=two*Rex
thinsh = (tsh < tworc)
d0s=(max(d0,zero))**2
rcs=rc**2
Rexs=Rex**2
rxsum=Rex+rc
Pi6vc=Pise/vcel

if (thinsh) then
  if (d0>=-eps_DP.and.d0<tsh) then
    d01=max(d0,zero)
    CS_All3(2) = (d01*(-half*d0s+six*rcs))*Pi6vc
    CS_All3(3) = (d01*(d0s-six*(Rex**2+rc**2))+eight*(Rex**3-rc**3))*Pi6vc
  else if (d0>=tsh.and.d0<tworc) then
    sqd=tsh*rxsum
    CS_All3(2) = ( four*tsh*(Rexs+rcs+Rex*rc) + three*sqd*(-d0-half*sqd/d0) )*Pi6vc
    CS_All3(3) = (sqd**2)*three*Pise/(vcel*d0)
  else if (d0>=tworc.and.d0<rxsum) then
    d01=one/d0
    sqd=tsh*rxsum
    CS_All3(2) = ( d0*(half*d0s -three*(Rexs+rcs)) +four*(Rexs*Rex+rcs*rc)-three*half*((sqd**2)/d0) )*Pi6vc
    CS_All3(3) = ( -half*d0*d0s + rcs*(six*d0-eight*rc) + (sqd**2)*three*d01 )*Pi6vc
  else if (d0>=rxsum.and.d0<=twoRex) then
    CS_All3(2) = zero
    CS_All3(3) = ((d0-twoRex)**2)*(d0+four*Rex)*Pi12vc
  endif
else
  if (d0>=-eps_DP.and.d0<tworc) then
    d01=max(d0,zero)
    CS_All3(3) = (d01*(d0s-six*(Rex**2+rc**2))+eight*(Rex**3-rc**3))*Pi6vc
    CS_All3(2) = (d01*(-half*d0s+six*rcs))*Pi6vc
  else if (d0>=tworc.and.d0<tsh) then
    CS_All3(3) = (d0*(half*d0s-six*Rexs)+eight*(Rex*Rexs-tworc*rcs))*Pi6vc
    CS_All3(2) = (eight*rc*rcs )*Pi6vc
  else if (d0>=tsh.and.d0<rxsum) then
    d01=one/d0
    sqd=tsh*rxsum
    CS_All3(3) = ( -half*d0*d0s + rcs*(six*d0-eight*rc) + (sqd**2)*three*d01 )*Pi6vc
    CS_All3(2) = ( d0*(half*d0s -three*(Rexs+rcs)) +four*(Rexs*Rex+rcs*rc)-three*half*((sqd**2)/d0) )*Pi6vc
  else if (d0>=rxsum.and.d0<=twoRex) then
    CS_All3(3) = ((d0-twoRex)**2)*(d0+four*Rex)*Pi12vc
    CS_All3(2) = zero
  endif
endif
end function CS_All3
!************************************************************************************************

end module ComVolFrac_CoreShell
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module dQuadpack_AC
use nano_deftyp
private
public :: dqageAC

integer(I4B),parameter :: I1mach(16) = [ 5, 6, 7, 6, &
                                        32, 4, 2, 31, &
                                        2147483647, 2, 24, -125, &
                                        128, 53, -1021, 1024 ]
                                        
real(DP),parameter     :: tolin = 100.d0*eps_DP
integer(I4B),parameter :: keyval=3

contains

subroutine dgtsl(n,c,d,e,b,info)
!*********************************************************************72
!
!c DGTSL solves a general tridiagonal linear system.
!
!     on entry
!
!        n       integer
!                is the order of the tridiagonal matrix.
!
!        c       real(DP) ::(n)
!                is the subdiagonal of the tridiagonal matrix.
!                c(2) through c(n) should contain the subdiagonal.
!                on output c is destroyed.
!
!        d       real(DP) ::(n)
!                is the diagonal of the tridiagonal matrix.
!                on output d is destroyed.
!
!        e       real(DP) ::(n)
!                is the superdiagonal of the tridiagonal matrix.
!                e(1) through e(n-1) should contain the superdiagonal.
!                on output e is destroyed.
!
!        b       real(DP) ::(n)
!                is the right hand side vector.
!
!     on return
!
!        b       is the solution vector.
!
!        info    integer
!                = 0 normal value.
!                = k if the k-th element of the diagonal becomes
!                    exactly zero.  the subroutine returns when
!                    this is detected.
!
!     linpack. this version dated 08/14/78 .
!     jack dongarra, argonne national laboratory.
!
      integer(I4B) :: n,info
      real(DP) :: c(1),d(1),e(1),b(1)
      integer(I4B) :: k,kb,kp1,nm1,nm2
      real(DP) :: t
!     begin block permitting ...exits to 100
!
         info = 0
         c(1) = d(1)
         nm1 = n - 1
         if (nm1  <  1) go to 40
            d(1) = e(1)
            e(1) = 0.0d0
            e(n) = 0.0d0
!
            do 30 k = 1, nm1
               kp1 = k + 1
!
!              find the largest of the two rows
!
               if (dabs(c(kp1))  <  dabs(c(k))) go to 10
!
!                 interchange row
!
                  t = c(kp1)
                  c(kp1) = c(k)
                  c(k) = t
                  t = d(kp1)
                  d(kp1) = d(k)
                  d(k) = t
                  t = e(kp1)
                  e(kp1) = e(k)
                  e(k) = t
                  t = b(kp1)
                  b(kp1) = b(k)
                  b(k) = t
   10          continue
!
!              zero elements
!
               if (c(k)  /=  0.0d0) go to 20
                  info = k
!     ............exit
                  go to 100
   20          continue
               t = -c(kp1)/c(k)
               c(kp1) = d(kp1) + t*d(k)
               d(kp1) = e(kp1) + t*e(k)
               e(kp1) = 0.0d0
               b(kp1) = b(kp1) + t*b(k)
   30       continue
   40    continue
         if (c(n)  /=  0.0d0) go to 50
            info = n
         go to 90
   50    continue
!
!           back solve
!
            nm2 = n - 2
            b(n) = b(n)/c(n)
            if (n  ==  1) go to 80
               b(nm1) = (b(nm1) - d(nm1)*b(n))/c(nm1)
               if (nm2  <  1) go to 70
               do 60 kb = 1, nm2
                  k = nm2 - kb + 1
                  b(k) = (b(k) - d(k)*b(k+1) - e(k)*b(k+2))/c(k)
   60          continue
   70          continue
   80       continue
   90    continue
  100 continue
!
      return
end subroutine dgtsl
!*********************************************************************72
subroutine dqageAC(f,a,b, result_I)
implicit none
real(DP),intent(IN) :: a,b
real(DP),intent(OUT) :: result_I
! local
real(DP)                          :: abserr,epsabs,epsrel
integer(I4B)                      :: neval,ier,last,key,Climit
real(DP),dimension(:),allocatable :: alist,blist,rlist,elist
integer(I4B),dimension(:),allocatable :: iord

  INTERFACE
    FUNCTION F(X)
      USE nano_deftyp
      implicit none
      REAL(DP),INTENT(IN) :: X
      real(DP)            :: f
    END FUNCTION F
  END INTERFACE

epsabs=tolin
epsrel=tolin
key=keyval
Climit=2048
allocate(alist(Climit),blist(Climit),rlist(Climit),elist(Climit),iord(Climit))

call dqage(f,a,b,epsabs,epsrel,key,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,iord,last)

deallocate(alist,blist,rlist,elist)

end subroutine dqageAC
!*********************************************************************72
subroutine dqage(f,a,b,epsabs,epsrel,key,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,iord,last)
implicit none
!c DQAGE estimates a definite integral.
!
!***begin prologue  dqage
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a1
!***keywords  automatic integrator, general-purpose,
!             integrand examinator, globally adaptive,
!             gauss-kronrod
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral   i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            abs(i-reslt) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            key    - integer
!                     key for choice of local integration rule
!                     a gauss-kronrod pair is used with
!                          7 - 15 points if key < 2,
!                         10 - 21 points if key = 2,
!                         15 - 31 points if key = 3,
!                         20 - 41 points if key = 4,
!                         25 - 51 points if key = 5,
!                         30 - 61 points if key > 5.
!
!            Climit  - integer
!                     gives an upperbound on the number of subintervals
!                     in the partition of (a,b), Climit >= 1.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for result_I and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value
!                             of Climit.
!                             however, if this yields no improvement it
!                             is rather advised to analyze the integrand
!                             in order to determine the integration
!                             difficulties. if the position of a local
!                             difficulty can be determined(e.g.
!                             singularity, discontinuity within the
!                             interval) one will probably gain from
!                             splitting up the interval at this point
!                             and calling the integrator on the
!                             subranges. if possible, an appropriate
!                             special-purpose integrator should be used
!                             which is designed for handling the type of
!                             difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28),
!                             result_I, abserr, neval, last, rlist(1) ,
!                             elist(1) and iord(1) are set to zero.
!                             alist(1) and blist(1) are set to a and b
!                             respectively.
!
!            alist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the left
!                      end points of the subintervals in the partition
!                      of the given integration range (a,b)
!
!            blist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the right
!                      end points of the subintervals in the partition
!                      of the given integration range (a,b)
!
!            rlist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the
!                      integral approximations on the subintervals
!
!            elist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the moduli of the
!                      absolute error estimates on the subintervals
!
!            iord    - integer
!                      vector of dimension at least Climit, the first k
!                      elements of which are pointers to the
!                      error estimates over the subintervals,
!                      such that elist(iord(1)), ...,
!                      elist(iord(k)) form a decreasing sequence,
!                      with k = last if last <= (Climit/2+2), and
!                      k = Climit+1-last otherwise
!
!            last    - integer
!                      number of subintervals actually produced in the
!                      subdivision process
!
!***references  (none)
!***routines called  dqk15,dqk21,dqk31,
!                    dqk41,dqk51,dqk61,dqpsrt
!***end prologue  dqage
!
      real(DP) :: a,abserr,alist,area,area1,area12,area2,a1,a2,b,  &
        blist,b1,b2,dabs,defabs,defab1,defab2,dmax1,elist,epmach,&
        epsabs,epsrel,errbnd,errmax,error1,error2,erro12,errsum,f,      &
        resabs,result_I,rlist,uflow
      integer(I4B) :: ier,iord,iroff1,iroff2,k,key,keyf,last,Climit,maxerr,neval,&
        nrmax
!
      dimension alist(Climit),blist(Climit),elist(Climit),iord(Climit),     &
        rlist(Climit)
!
      external f
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                      (alist(i),blist(i))
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest
!                       error estimate
!           errmax    - elist(maxerr)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!
!
!           machine dependent constants
!           ---------------------------
!
!           epmach  is the largest relative spacing.
!           uflow  is the smallest positive magnitude.
!
!***first executable statement  dqage
      epmach = d1mach(4)
      uflow = d1mach(1)
!
!           test on validity of parameters
!           ------------------------------
!
      ier = 0
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      if (epsabs <= 0.0d+00 .and. epsrel < dmax1(0.5d+02*epmach,0.5d-28)) ier = 6
      if (ier == 6) go to 999
!
!           first approximation to the integral
!           -----------------------------------
!
      keyf = key
      if (key <= 0) keyf = 1
      if (key >= 7) keyf = 6
      neval = 0
      if (keyf == 1) call dqk15(f,a,b,result_I,abserr,defabs,resabs)
      if (keyf == 2) call dqk21(f,a,b,result_I,abserr,defabs,resabs)
      if (keyf == 3) call dqk31(f,a,b,result_I,abserr,defabs,resabs)
      if (keyf == 4) call dqk41(f,a,b,result_I,abserr,defabs,resabs)
      if (keyf == 5) call dqk51(f,a,b,result_I,abserr,defabs,resabs)
      if (keyf == 6) call dqk61(f,a,b,result_I,abserr,defabs,resabs)
      last = 1
      rlist(1) = result_I
      elist(1) = abserr
      iord(1) = 1
!
!           test on accuracy.
!
      errbnd = dmax1(epsabs,epsrel*dabs(result_I))
      if (abserr <= 0.5d+02*epmach*defabs.and.abserr > errbnd) ier = 2
      if (Climit == 1) ier = 1
      if (ier /= 0.or.(abserr <= errbnd.and.abserr /= resabs) .or. abserr == 0.0d+00) go to 60
!
!           initialization
!           --------------
!
!
      errmax = abserr
      maxerr = 1
      area = result_I
      errsum = abserr
      nrmax = 1
      iroff1 = 0
      iroff2 = 0
!
!           main do-loop
!           ------------
!
      do 30 last = 2,Climit
!
!           bisect the subinterval with the largest error estimate.
!
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        if (keyf == 1) call dqk15(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 2) call dqk21(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 3) call dqk31(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 4) call dqk41(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 5) call dqk51(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 6) call dqk61(f,a1,b1,area1,error1,resabs,defab1)
        if (keyf == 1) call dqk15(f,a2,b2,area2,error2,resabs,defab2)
        if (keyf == 2) call dqk21(f,a2,b2,area2,error2,resabs,defab2)
        if (keyf == 3) call dqk31(f,a2,b2,area2,error2,resabs,defab2)
        if (keyf == 4) call dqk41(f,a2,b2,area2,error2,resabs,defab2)
        if (keyf == 5) call dqk51(f,a2,b2,area2,error2,resabs,defab2)
        if (keyf == 6) call dqk61(f,a2,b2,area2,error2,resabs,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        neval = neval+1
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (defab1 == error1.or.defab2 == error2) go to 5
        if (dabs(rlist(maxerr)-area12) <= 0.1d-04*dabs(area12) .and.erro12 >= 0.99d+00*errmax) iroff1 = iroff1+1
        if (last > 10.and.erro12 > errmax) iroff2 = iroff2+1
    5   rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
        if (errsum <= errbnd) go to 8
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1 >= 6.or.iroff2 >= 20) ier = 2
!
!           set error flag in the case that the number of subintervals
!           equals Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03 * epmach)*(dabs(a2)+0.1d+04*uflow)) ier = 3
!
!           append the newly-created intervals to the list.
!
    8   if (error2 > error1) go to 10
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 20
   10   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with the largest error estimate (to be bisected next).
!
   20   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
        if (ier /= 0.or.errsum <= errbnd) go to 40
   30 continue
!
!           compute final result_I.
!           ---------------------
!
   40 result_I = 0.0d+00
      do 50 k=1,last
        result_I = result_I+rlist(k)
   50 continue
      abserr = errsum
   60 if (keyf /= 1) neval = (10*keyf+1)*(2*neval+1)
      if (keyf == 1) neval = 30*neval+15
  999 return
end subroutine dqage
!*********************************************************************72
subroutine dqag(f,a,b,epsabs,epsrel,key,result_I,abserr,neval,ier,Climit,lenw,last,iwork,work)
!
!c DQAG approximates an integral over a finite interval.
!
!***begin prologue  dqag
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a1
!***keywords  automatic integrator, general-purpose,
!             integrand examinator, globally adaptive,
!             gauss-kronrod
!***author  piessens,robert,appl. math. & progr. div - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I)le.max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!            f      - real(DP) ::
!                     function subprogam defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            epsabs - real(DP) ::
!                     absolute accoracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            key    - integer
!                     key for choice of local integration rule
!                     a gauss-kronrod pair is used with
!                       7 - 15 points if key < 2,
!                      10 - 21 points if key = 2,
!                      15 - 31 points if key = 3,
!                      20 - 41 points if key = 4,
!                      25 - 51 points if key = 5,
!                      30 - 61 points if key > 5.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for result_I and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!                      error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yield no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulaties.
!                             if the position of a local difficulty can
!                             be determined (i.e.singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or Climit < 1 or lenw < Climit*4.
!                             result_I, abserr, neval, last are set
!                             to zero.
!                             except when lenw is invalid, iwork(1),
!                             work(Climit*2+1) and work(Climit*3+1) are
!                             set to zero, work(1) is set to a and
!                             work(Climit+1) to b.
!
!         dimensioning parameters
!            Climit - integer
!                    dimensioning parameter for iwork
!                    Climit determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (a,b), Climit >= 1.
!                    if Climit < 1, the routine will end with ier = 6.
!
!            lenw  - integer
!                    dimensioning parameter for work
!                    lenw must be at least Climit*4.
!                    if lenw < Climit*4, the routine will end with
!                    ier = 6.
!
!            last  - integer
!                    on return, last equals the number of subintervals
!                    produced in the subdiviosion process, which
!                    determines the number of significant elements
!                    actually in the work arrays.
!
!         work arrays
!            iwork - integer
!                    vector of dimension at least Climit, the first k
!                    elements of which contain pointers to the error
!                    estimates over the subintervals, such that
!                    work(Climit*3+iwork(1)),... , work(Climit*3+iwork(k))
!                    form a decreasing sequence with k = last if
!                    last <= (Climit/2+2), and k = Climit+1-last otherwise
!
!            work  - real(DP) ::
!                    vector of dimension at least lenw
!                    on return
!                    work(1), ..., work(last) contain the left end
!                    points of the subintervals in the partition of
!                     (a,b),
!                    work(Climit+1), ..., work(Climit+last) contain the
!                     right end points,
!                    work(Climit*2+1), ..., work(Climit*2+last) contain
!                     the integral approximations over the subintervals,
!                    work(Climit*3+1), ..., work(Climit*3+last) contain
!                     the error estimates.
!
!***references  (none)
!***routines called  dqage,xerror
!***end prologue  dqag
      real(DP) :: a,abserr,b,epsabs,epsrel,f,result_I,work
      integer(I4B) :: ier,iwork,key,last,lenw,Climit,lvl,l1,l2,l3,neval
!
      dimension iwork(Climit),work(lenw)
!
      external f
!
!         check validity of lenw.
!
!***first executable statement  dqag
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (Climit < 1.or.lenw < Climit*4) go to 10
!
!         prepare call for dqage.
!
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
!
      call dqage(f,a,b,epsabs,epsrel,key,Climit,result_I,abserr,neval, ier,work(1),work(l1),work(l2),work(l3),iwork,last)
!
!         call error handler if necessary.
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqag ',26,ier,lvl)
      return
end subroutine dqag
!*********************************************************************72
subroutine dqagie(f,bound,inf,epsabs,epsrel,Climit,result_I,abserr, neval,ier,alist,blist,rlist,elist,iord,last)
!
!c DQAGIE estimates an integral over a semi-infinite or infinite interva
!
!***begin prologue  dqagie
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1,h2a4a1
!***keywords  automatic integrator, infinite intervals,
!             general-purpose, transformation, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math & progr. div - k.u.leuven
!           de doncker,elise,appl. math & progr. div - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            integral   i = integral of f over (bound,+infinity)
!            or i = integral of f over (-infinity,bound)
!            or i = integral of f over (-infinity,+infinity),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i))
!***description
!
! integration over infinite intervals
! standard fortran subroutine
!
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            bound  - real(DP) ::
!                     finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            inf    - real(DP) ::
!                     indicating the kind of integration range involved
!                     inf = 1 corresponds to  (bound,+infinity),
!                     inf = -1            to  (-infinity,bound),
!                     inf = 2             to (-infinity,+infinity).
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), Climit >= 1
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                   - ier > 0 abnormal termination of the routine. the
!                             estimates for result_I and error are less
!                             reliable. it is assumed that the requested
!                             accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however,if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties.
!                             if the position of a local difficulty can
!                             be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             result_I is the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28),
!                             result_I, abserr, neval, last, rlist(1),
!                             elist(1) and iord(1) are set to zero.
!                             alist(1) and blist(1) are set to 0
!                             and 1 respectively.
!
!            alist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            blist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            rlist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real(DP) ::
!                     vector of dimension at least Climit,  the first
!                     last elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension Climit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last <= (Climit/2+2), and k = Climit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced
!                     in the subdivision process
!
!***references  (none)
!***routines called  dqelg,dqk15i,dqpsrt
!***end prologue  dqagie
      real(DP) :: abseps,abserr,alist,area,area1,area12,area2,a1,  &
        a2,blist,boun,bound,b1,b2,correc,dabs,defabs,defab1,defab2,     &
        dmax1,dres,elist,epmach,epsabs,epsrel,erlarg,erlast,     &
        errbnd,errmax,error1,error2,erro12,errsum,ertest,f,oflow,resabs,&
        reseps,result_I,res3la,rlist,rlist2,small,uflow
      integer(I4B) :: id,ier,ierro,inf,iord,iroff1,iroff2,iroff3,jupbnd,k,ksgn, &
        ktmin,last,Climit,maxerr,neval,nres,nrmax,numrl2
      logical extrap,noext
!
      dimension alist(Climit),blist(Climit),elist(Climit),iord(Climit),     &
        res3la(3),rlist(Climit),rlist2(52)
!
      external f
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine dqelg.
!
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least (limexp+2),
!                       containing the part of the epsilon table
!                       wich is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements currently in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained, it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           small     - length of the smallest interval considered up
!                       to now, multiplied by 1.5
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation
!                       is no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  dqagie
       epmach = d1mach(4)
!
!           test on validity of parameters
!           -----------------------------
!
      ier = 0
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      alist(1) = 0.0d+00
      blist(1) = 0.1d+01
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      if (epsabs <= 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28)) ier = 6
       if (ier == 6) go to 999
!
!
!           first approximation to the integral
!           -----------------------------------
!
!           determine the interval to be mapped onto (0,1).
!           if inf = 2 the integral is computed as i = i1+i2, where
!           i1 = integral of f over (-infinity,0),
!           i2 = integral of f over (0,+infinity).
!
      boun = bound
      if (inf == 2) boun = 0.0d+00
      call dqk15i(f,boun,inf,0.0d+00,0.1d+01,result_I,abserr, defabs,resabs)
!
!           test on accuracy
!
      last = 1
      rlist(1) = result_I
      elist(1) = abserr
      iord(1) = 1
      dres = dabs(result_I)
      errbnd = dmax1(epsabs,epsrel*dres)
      if (abserr <= 1.0d+02*epmach*defabs.and.abserr > errbnd) ier = 2
      if (Climit == 1) ier = 1
      if (ier /= 0.or.(abserr <= errbnd.and.abserr /= resabs).or. abserr == 0.0d+00) go to 130
!
!           initialization
!           --------------
!
      uflow = d1mach(1)
      oflow = d1mach(2)
      rlist2(1) = result_I
      errmax = abserr
      maxerr = 1
      area = result_I
      errsum = abserr
      abserr = oflow
      nrmax = 1
      nres = 0
      ktmin = 0
      numrl2 = 2
      extrap = .false.
      noext = .false.
      ierro = 0
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      if (dres >= (0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
!
!           main do-loop
!           ------------
!
      do 90 last = 2,Climit
!
!           bisect the subinterval with nrmax-th largest error estimate.
!
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        call dqk15i(f,boun,inf,a1,b1,area1,error1,resabs,defab1)
        call dqk15i(f,boun,inf,a2,b2,area2,error2,resabs,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (defab1 == error1.or.defab2 == error2)go to 15
        if (dabs(rlist(maxerr)-area12) > 0.1d-04*dabs(area12) .or.erro12 < 0.99d+00*errmax) go to 10
        if (extrap) iroff2 = iroff2+1
        if (.not.extrap) iroff1 = iroff1+1
   10   if (last > 10.and.erro12 > errmax) iroff3 = iroff3+1
   15   rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1+iroff2 >= 10.or.iroff3 >= 20) ier = 2
        if (iroff2 >= 5) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at some points of the integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)* (dabs(a2)+0.1d+04*uflow)) ier = 4
!
!           append the newly-created intervals to the list.
!
        if (error2 > error1) go to 20
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 30
   20   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with nrmax-th largest error estimate (to be bisected next).
!
   30   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
        if (errsum <= errbnd) go to 115
        if (ier /= 0) go to 100
        if (last == 2) go to 80
        if (noext) go to 90
        erlarg = erlarg-erlast
        if (dabs(b1-a1) > small) erlarg = erlarg+erro12
        if (extrap) go to 40
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
        if (dabs(blist(maxerr)-alist(maxerr)) > small) go to 90
        extrap = .true.
        nrmax = 2
   40   if (ierro == 3.or.erlarg <= ertest) go to 60
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors over the
!           larger intervals (erlarg) and perform extrapolation.
!
        id = nrmax
        jupbnd = last
        if (last > (2+Climit/2)) jupbnd = Climit+3-last
        do 50 k = id,jupbnd
          maxerr = iord(nrmax)
          errmax = elist(maxerr)
          if (dabs(blist(maxerr)-alist(maxerr)) > small) go to 90
          nrmax = nrmax+1
   50   continue
!
!           perform extrapolation.
!
   60   numrl2 = numrl2+1
        rlist2(numrl2) = area
        call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if (ktmin > 5.and.abserr < 0.1d-02*errsum) ier = 5
        if (abseps >= abserr) go to 70
        ktmin = 0
        abserr = abseps
        result_I = reseps
        correc = erlarg
        ertest = dmax1(epsabs,epsrel*dabs(reseps))
        if (abserr <= ertest) go to 100
!
!            prepare bisection of the smallest interval.
!
   70   if (numrl2 == 1) noext = .true.
        if (ier == 5) go to 100
        maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        small = small*0.5d+00
        erlarg = errsum
        go to 90
   80   small = 0.375d+00
        erlarg = errsum
        ertest = errbnd
        rlist2(2) = area
   90 continue
!
!           set final result_I and error estimate.
!           ------------------------------------
!
  100 if (abserr == oflow) go to 115
      if ((ier+ierro) == 0) go to 110
      if (ierro == 3) abserr = abserr+correc
      if (ier == 0) ier = 3
      if (result_I /= 0.0d+00.and.area /= 0.0d+00)go to 105
      if (abserr > errsum)go to 115
      if (area == 0.0d+00) go to 130
      go to 110
  105 if (abserr/dabs(result_I) > errsum/dabs(area))go to 115
!
!           test on divergence
!
  110 if (ksgn == (-1).and.dmax1(dabs(result_I),dabs(area)) <= defabs*0.1d-01) go to 130
      if (0.1d-01 > (result_I/area).or.(result_I/area) > 0.1d+03.or.errsum > dabs(area)) ier = 6
      go to 130
!
!           compute global integral sum.
!
  115 result_I = 0.0d+00
      do 120 k = 1,last
        result_I = result_I+rlist(k)
  120 continue
      abserr = errsum
  130 neval = 30*last-15
      if (inf == 2) neval = 2*neval
      if (ier > 2) ier=ier-1
  999 return
end subroutine dqagie
!*********************************************************************72
subroutine dqagi(f,bound,inf,epsabs,epsrel,result_I,abserr,neval,ier,Climit,lenw,last,iwork,work)
!
!c DQAGI estimates an integral over a semi-infinite or infinite interval
!
!***begin prologue  dqagi
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1,h2a4a1
!***keywords  automatic integrator, infinite intervals,
!             general-purpose, transformation, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. -k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            integral   i = integral of f over (bound,+infinity)
!            or i = integral of f over (-infinity,bound)
!            or i = integral of f over (-infinity,+infinity)
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        integration over infinite intervals
!        standard fortran subroutine
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            bound  - real(DP) ::
!                     finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            inf    - integer
!                     indicating the kind of integration range involved
!                     inf = 1 corresponds to  (bound,+infinity),
!                     inf = -1            to  (-infinity,bound),
!                     inf = 2             to (-infinity,+infinity).
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                   - ier > 0 abnormal termination of the routine. the
!                             estimates for result_I and error are less
!                             reliable. it is assumed that the requested
!                             accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             result_I is the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                              or Climit < 1 or leniw < Climit*4.
!                             result_I, abserr, neval, last are set to
!                             zero. exept when Climit or leniw is
!                             invalid, iwork(1), work(Climit*2+1) and
!                             work(Climit*3+1) are set to zero, work(1)
!                             is set to a and work(Climit+1) to b.
!
!         dimensioning parameters
!            Climit - integer
!                    dimensioning parameter for iwork
!                    Climit determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (a,b), Climit >= 1.
!                    if Climit < 1, the routine will end with ier = 6.
!
!            lenw  - integer
!                    dimensioning parameter for work
!                    lenw must be at least Climit*4.
!                    if lenw < Climit*4, the routine will end
!                    with ier = 6.
!
!            last  - integer
!                    on return, last equals the number of subintervals
!                    produced in the subdivision process, which
!                    determines the number of significant elements
!                    actually in the work arrays.
!
!         work arrays
!            iwork - integer
!                    vector of dimension at least Climit, the first
!                    k elements of which contain pointers
!                    to the error estimates over the subintervals,
!                    such that work(Climit*3+iwork(1)),... ,
!                    work(Climit*3+iwork(k)) form a decreasing
!                    sequence, with k = last if last <= (Climit/2+2), and
!                    k = Climit+1-last otherwise
!
!            work  - real(DP) ::
!                    vector of dimension at least lenw
!                    on return
!                    work(1), ..., work(last) contain the left
!                     end points of the subintervals in the
!                     partition of (a,b),
!                    work(Climit+1), ..., work(Climit+last) contain
!                     the right end points,
!                    work(Climit*2+1), ...,work(Climit*2+last) contain the
!                     integral approximations over the subintervals,
!                    work(Climit*3+1), ..., work(Climit*3)
!                     contain the error estimates.
!***references  (none)
!***routines called  dqagie,xerror
!***end prologue  dqagi
!
      real(DP) :: abserr,bound,epsabs,epsrel,f,result_I,work
      integer(I4B) :: ier,inf,iwork,last,lenw,Climit,lvl,l1,l2,l3,neval
!
      dimension iwork(Climit),work(lenw)
!
      external f
!
!         check validity of Climit and lenw.
!
!***first executable statement  dqagi
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (Climit < 1.or.lenw < Climit*4) go to 10
!
!         prepare call for dqagie.
!
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
!
      call dqagie(f,bound,inf,epsabs,epsrel,Climit,result_I,abserr,neval,ier,work(1),work(l1),work(l2),work(l3),iwork,last)
!
!         call error handler if necessary.
!
       lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqagi',26,ier,lvl)
      return
end subroutine dqagi
!*********************************************************************72
subroutine dqagpe(f,a,b,npts2,points,epsabs,epsrel,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,pts,iord,level,ndin, &
                  last)
!
!c DQAGPE computes a definite integral.
!
!***begin prologue  dqagpe
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, general-purpose,
!             singularities at user specified points,
!             extrapolation, globally adaptive.
!***author  piessens,robert ,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f over (a,b), hopefully
!            satisfying following claim for accuracy abs(i-result_I) <= 
!            max(epsabs,epsrel*abs(i)). break points of the integration
!            interval, where local difficulties of the integrand may
!            occur(e.g. singularities,discontinuities),provided by user.
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            npts2  - integer
!                     number equal to two more than the number of
!                     user-supplied break points within the integration
!                     range, npts2 >= 2.
!                     if npts2 < 2, the routine will end with ier = 6.
!
!            points - real(DP) ::
!                     vector of dimension npts2, the first (npts2-2)
!                     elements of which are the user provided break
!                     points. if these points do not constitute an
!                     ascending sequence there will be an automatic
!                     sorting.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), Climit >= npts2
!                     if Climit < npts2, the routine will end with
!                     ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (i.e. singularity,
!                             discontinuity within the interval), it
!                             should be supplied to the routine as an
!                             element of the vector points. if necessary
!                             an appropriate special-purpose integrator
!                             must be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table. it is presumed that
!                             the requested tolerance cannot be
!                             achieved, and that the returned result_I is
!                             the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier > 0.
!                         = 6 the input is invalid because
!                             npts2 < 2 or
!                             break points are specified outside
!                             the integration range or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or Climit < npts2.
!                             result_I, abserr, neval, last, rlist(1),
!                             and elist(1) are set to zero. alist(1) and
!                             blist(1) are set to a and b respectively.
!
!            alist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the left end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            blist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the right end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            rlist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            pts    - real(DP) ::
!                     vector of dimension at least npts2, containing the
!                     integration Climits and the break points of the
!                     interval in ascending sequence.
!
!            level  - integer
!                     vector of dimension at least Climit, containing the
!                     subdivision levels of the subinterval, i.e. if
!                     (aa,bb) is a subinterval of (p1,p2) where p1 as
!                     well as p2 is a user-provided break point or
!                     integration Climit, then (aa,bb) has level l if
!                     abs(bb-aa) = abs(p2-p1)*2**(-l).
!
!            ndin   - integer
!                     vector of dimension at least npts2, after first
!                     integration over the intervals (pts(i)),pts(i+1),
!                     i = 0,1, ..., npts2-2, the error estimates over
!                     some of the intervals may have been increased
!                     artificially, in order to put their subdivision
!                     forward. if this happens for the subinterval
!                     numbered k, ndin(k) is put to 1, otherwise
!                     ndin(k) = 0.
!
!            iord   - integer
!                     vector of dimension at least Climit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last <= (Climit/2+2), and k = Climit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced in the
!                     subdivisions process
!
!***references  (none)
!***routines called  dqelg,dqk21,dqpsrt
!***end prologue  dqagpe
      real(DP) :: a,abseps,abserr,alist,area,area1,area12,area2,a1,&
        a2,b,blist,b1,b2,correc,dabs,defabs,defab1,defab2,dmax1,dmin1,  &
        dres,elist,epmach,epsabs,epsrel,erlarg,erlast,errbnd,    &
        errmax,error1,erro12,error2,errsum,ertest,f,oflow,points,pts,   &
        resa,resabs,reseps,result_I,res3la,rlist,rlist2,sign,temp,uflow
      integer(I4B) :: i,id,ier,ierro,ind1,ind2,iord,ip1,iroff1,iroff2,iroff3,j, &
        jlow,jupbnd,k,ksgn,ktmin,last,levcur,level,levmax,Climit,maxerr, &
        ndin,neval,nint,nintp1,npts,npts2,nres,nrmax,numrl2
      logical extrap,noext
!
!
      dimension alist(Climit),blist(Climit),elist(Climit),iord(Climit),     &
        level(Climit),ndin(npts2),points(npts2),pts(npts2),res3la(3),    &
        rlist(Climit),rlist2(52)
!
      external f
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine epsalg (rlist2 should be of dimension
!            (limexp+2) at least).
!
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least limexp+2
!                       containing the part of the epsilon table which
!                       is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements in rlist2. if an appropriate
!                       approximation to the compounded integral has
!                       been obtained, it is put in rlist2(numrl2) after
!                       numrl2 has been increased by one.
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine
!                       is attempting to perform extrapolation. i.e.
!                       before subdividing the smallest interval we
!                       try to decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation is
!                       no longer allowed (true-value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  dqagpe
      epmach = d1mach(4)
!
!            test on validity of parameters
!            -----------------------------
!
      ier = 0
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      level(1) = 0
      npts = npts2-2
      if (npts2 < 2.or.Climit <= npts.or.(epsabs <= 0.0d+00.and.  epsrel < dmax1(0.5d+02*epmach,0.5d-28))) ier = 6
      if (ier == 6) go to 999
!
!            if any break points are provided, sort them into an
!            ascending sequence.
!
      sign = 1.0d+00
      if (a > b) sign = -1.0d+00
      pts(1) = dmin1(a,b)
      if (npts == 0) go to 15
      do 10 i = 1,npts
        pts(i+1) = points(i)
   10 continue
   15 pts(npts+2) = dmax1(a,b)
      nint = npts+1
      a1 = pts(1)
      if (npts == 0) go to 40
      nintp1 = nint+1
      do 20 i = 1,nint
        ip1 = i+1
        do 20 j = ip1,nintp1
          if (pts(i) <= pts(j)) go to 20
          temp = pts(i)
          pts(i) = pts(j)
          pts(j) = temp
   20 continue
      if (pts(1) /= dmin1(a,b).or.pts(nintp1) /= dmax1(a,b)) ier = 6
      if (ier == 6) go to 999
!
!            compute first integral and error approximations.
!            ------------------------------------------------
!
   40 resabs = 0.0d+00
      do 50 i = 1,nint
        b1 = pts(i+1)
        call dqk21(f,a1,b1,area1,error1,defabs,resa)
        abserr = abserr+error1
        result_I = result_I+area1
        ndin(i) = 0
        if (error1 == resa.and.error1 /= 0.0d+00) ndin(i) = 1
        resabs = resabs+defabs
        level(i) = 0
        elist(i) = error1
        alist(i) = a1
        blist(i) = b1
        rlist(i) = area1
        iord(i) = i
        a1 = b1
   50 continue
      errsum = 0.0d+00
      do 55 i = 1,nint
        if (ndin(i) == 1) elist(i) = abserr
        errsum = errsum+elist(i)
   55 continue
!
!           test on accuracy.
!
      last = nint
      neval = 21*nint
      dres = dabs(result_I)
      errbnd = dmax1(epsabs,epsrel*dres)
      if (abserr <= 0.1d+03*epmach*resabs.and.abserr > errbnd) ier = 2
      if (nint == 1) go to 80
      do 70 i = 1,npts
        jlow = i+1
        ind1 = iord(i)
        do 60 j = jlow,nint
          ind2 = iord(j)
          if (elist(ind1) > elist(ind2)) go to 60
          ind1 = ind2
          k = j
   60   continue
        if (ind1 == iord(i)) go to 70
        iord(k) = iord(i)
        iord(i) = ind1
   70 continue
      if (Climit < npts2) ier = 1
   80 if (ier /= 0.or.abserr <= errbnd) go to 210
!
!           initialization
!           --------------
!
      rlist2(1) = result_I
      maxerr = iord(1)
      errmax = elist(maxerr)
      area = result_I
      nrmax = 1
      nres = 0
      numrl2 = 1
      ktmin = 0
      extrap = .false.
      noext = .false.
      erlarg = errsum
      ertest = errbnd
      levmax = 1
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ierro = 0
      uflow = d1mach(1)
      oflow = d1mach(2)
      abserr = oflow
      ksgn = -1
      if (dres >= (0.1d+01-0.5d+02*epmach)*resabs) ksgn = 1
!
!           main do-loop
!           ------------
!
      do 160 last = npts2,Climit
!
!           bisect the subinterval with the nrmax-th largest error
!           estimate.
!
        levcur = level(maxerr)+1
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        call dqk21(f,a1,b1,area1,error1,resa,defab1)
        call dqk21(f,a2,b2,area2,error2,resa,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        neval = neval+42
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (defab1 == error1.or.defab2 == error2) go to 95
        if (dabs(rlist(maxerr)-area12) > 0.1d-04*dabs(area12)  .or.erro12 < 0.99d+00*errmax) go to 90
        if (extrap) iroff2 = iroff2+1
        if (.not.extrap) iroff1 = iroff1+1
   90   if (last > 10.and.erro12 > errmax) iroff3 = iroff3+1
   95   level(maxerr) = levcur
        level(last) = levcur
        rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1+iroff2 >= 10.or.iroff3 >= 20) ier = 2
        if (iroff2 >= 5) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)*  (dabs(a2)+0.1d+04*uflow)) ier = 4
!
!           append the newly-created intervals to the list.
!
        if (error2 > error1) go to 100
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 110
  100   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with nrmax-th largest error estimate (to be bisected next).
!
  110   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
        if (errsum <= errbnd) go to 190
! ***jump out of do-loop
        if (ier /= 0) go to 170
        if (noext) go to 160
        erlarg = erlarg-erlast
        if (levcur+1 <= levmax) erlarg = erlarg+erro12
        if (extrap) go to 120
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
        if (level(maxerr)+1 <= levmax) go to 160
        extrap = .true.
        nrmax = 2
  120   if (ierro == 3.or.erlarg <= ertest) go to 140
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors over
!           the larger intervals (erlarg) and perform extrapolation.
!
        id = nrmax
        jupbnd = last
        if (last > (2+Climit/2)) jupbnd = Climit+3-last
        do 130 k = id,jupbnd
          maxerr = iord(nrmax)
          errmax = elist(maxerr)
! ***jump out of do-loop
          if (level(maxerr)+1 <= levmax) go to 160
          nrmax = nrmax+1
  130   continue
!
!           perform extrapolation.
!
  140   numrl2 = numrl2+1
        rlist2(numrl2) = area
        if (numrl2 <= 2) go to 155
        call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if (ktmin > 5.and.abserr < 0.1d-02*errsum) ier = 5
        if (abseps >= abserr) go to 150
        ktmin = 0
        abserr = abseps
        result_I = reseps
        correc = erlarg
        ertest = dmax1(epsabs,epsrel*dabs(reseps))
! ***jump out of do-loop
        if (abserr < ertest) go to 170
!
!           prepare bisection of the smallest interval.
!
  150   if (numrl2 == 1) noext = .true.
        if (ier >= 5) go to 170
  155   maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        levmax = levmax+1
        erlarg = errsum
  160 continue
!
!           set the final result_I.
!           ---------------------
!
!
  170 if (abserr == oflow) go to 190
      if ((ier+ierro) == 0) go to 180
      if (ierro == 3) abserr = abserr+correc
      if (ier == 0) ier = 3
      if (result_I /= 0.0d+00.and.area /= 0.0d+00)go to 175
      if (abserr > errsum)go to 190
      if (area == 0.0d+00) go to 210
      go to 180
  175 if (abserr/dabs(result_I) > errsum/dabs(area))go to 190
!
!           test on divergence.
!
  180 if (ksgn == (-1).and.dmax1(dabs(result_I),dabs(area)) <=  resabs*0.1d-01) go to 210
      if (0.1d-01 > (result_I/area).or.(result_I/area) > 0.1d+03.or.  errsum > dabs(area)) ier = 6
      go to 210
!
!           compute global integral sum.
!
  190 result_I = 0.0d+00
      do 200 k = 1,last
        result_I = result_I+rlist(k)
  200 continue
      abserr = errsum
  210 if (ier > 2) ier = ier-1
      result_I = result_I*sign
  999 return
end subroutine dqagpe
!*********************************************************************72
subroutine dqagp(f,a,b,npts2,points,epsabs,epsrel,result_I,abserr,   neval,ier,leniw,lenw,last,iwork,work)
!
!c DQAGP computes a definite integral.
!
!***begin prologue  dqagp
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, general-purpose,
!             singularities at user specified points,
!             extrapolation, globally adaptive
!***author  piessens,robert,appl. math. & progr. div - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            break points of the integration interval, where local
!            difficulties of the integrand may occur (e.g.
!            singularities, discontinuities), are provided by the user.
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            npts2  - integer
!                     number equal to two more than the number of
!                     user-supplied break points within the integration
!                     range, npts >= 2.
!                     if npts2 < 2, the routine will end with ier = 6.
!
!            points - real(DP) ::
!                     vector of dimension npts2, the first (npts2-2)
!                     elements of which are the user provided break
!                     points. if these points do not constitute an
!                     ascending sequence there will be an automatic
!                     sorting.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (i.e. singularity,
!                             discontinuity within the interval), it
!                             should be supplied to the routine as an
!                             element of the vector points. if necessary
!                             an appropriate special-purpose integrator
!                             must be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is presumed that the requested
!                             tolerance cannot be achieved, and that
!                             the returned result_I is the best which
!                             can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier > 0.
!                         = 6 the input is invalid because
!                             npts2 < 2 or
!                             break points are specified outside
!                             the integration range or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             result_I, abserr, neval, last are set to
!                             zero. exept when leniw or lenw or npts2 is
!                             invalid, iwork(1), iwork(Climit+1),
!                             work(Climit*2+1) and work(Climit*3+1)
!                             are set to zero.
!                             work(1) is set to a and work(Climit+1)
!                             to b (where Climit = (leniw-npts2)/2).
!
!         dimensioning parameters
!            leniw - integer
!                    dimensioning parameter for iwork
!                    leniw determines Climit = (leniw-npts2)/2,
!                    which is the maximum number of subintervals in the
!                    partition of the given integration interval (a,b),
!                    leniw >= (3*npts2-2).
!                    if leniw < (3*npts2-2), the routine will end with
!                    ier = 6.
!
!            lenw  - integer
!                    dimensioning parameter for work
!                    lenw must be at least leniw*2-npts2.
!                    if lenw < leniw*2-npts2, the routine will end
!                    with ier = 6.
!
!            last  - integer
!                    on return, last equals the number of subintervals
!                    produced in the subdivision process, which
!                    determines the number of significant elements
!                    actually in the work arrays.
!
!         work arrays
!            iwork - integer
!                    vector of dimension at least leniw. on return,
!                    the first k elements of which contain
!                    pointers to the error estimates over the
!                    subintervals, such that work(Climit*3+iwork(1)),...,
!                    work(Climit*3+iwork(k)) form a decreasing
!                    sequence, with k = last if last <= (Climit/2+2), and
!                    k = Climit+1-last otherwise
!                    iwork(Climit+1), ...,iwork(Climit+last) contain the
!                     subdivision levels of the subintervals, i.e.
!                     if (aa,bb) is a subinterval of (p1,p2)
!                     where p1 as well as p2 is a user-provided
!                     break point or integration Climit, then (aa,bb) has
!                     level l if abs(bb-aa) = abs(p2-p1)*2**(-l),
!                    iwork(Climit*2+1), ..., iwork(Climit*2+npts2) have
!                     no significance for the user,
!                    note that Climit = (leniw-npts2)/2.
!
!            work  - real(DP) ::
!                    vector of dimension at least lenw
!                    on return
!                    work(1), ..., work(last) contain the left
!                     end points of the subintervals in the
!                     partition of (a,b),
!                    work(Climit+1), ..., work(Climit+last) contain
!                     the right end points,
!                    work(Climit*2+1), ..., work(Climit*2+last) contain
!                     the integral approximations over the subintervals,
!                    work(Climit*3+1), ..., work(Climit*3+last)
!                     contain the corresponding error estimates,
!                    work(Climit*4+1), ..., work(Climit*4+npts2)
!                     contain the integration Climits and the
!                     break points sorted in an ascending sequence.
!                    note that Climit = (leniw-npts2)/2.
!
!***references  (none)
!***routines called  dqagpe,xerror
!***end prologue  dqagp
!
      real(DP) :: a,abserr,b,epsabs,epsrel,f,points,result_I,work
      integer(I4B) :: ier,iwork,last,leniw,lenw,Climit,lvl,l1,l2,l3,l4,neval,  npts2
!
      dimension iwork(leniw),points(npts2),work(lenw)
!
      external f
!
!         check validity of Climit and lenw.
!
!***first executable statement  dqagp
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (leniw < (3*npts2-2).or.lenw < (leniw*2-npts2).or.npts2 < 2)  go to 10
!
!         prepare call for dqagpe.
!
      Climit = (leniw-npts2)/2
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
      l4 = Climit+l3
!
      call dqagpe(f,a,b,npts2,points,epsabs,epsrel,Climit,result_I,abserr, &
        neval,ier,work(1),work(l1),work(l2),work(l3),work(l4), iwork(1),iwork(l1),iwork(l2),last)
!
!         call error handler if necessary.
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqagp',26,ier,lvl)
      return
end subroutine dqagp
!*********************************************************************72
subroutine dqagse(f,a,b,epsabs,epsrel,Climit,result_I,abserr,neval,   ier,alist,blist,rlist,elist,iord,last)
!
!c DQAGSE estimates the integral of a function.
!
!***begin prologue  dqagse
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a1
!***keywords  automatic integrator, general-purpose,
!             (end point) singularities, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upperbound on the number of subintervals
!                     in the partition of (a,b)
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                         = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more sub-
!                             divisions by increasing the value of Climit
!                             (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is detec-
!                             ted, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour
!                             occurs at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is presumed that the requested
!                             tolerance cannot be achieved, and that the
!                             returned result_I is the best which can be
!                             obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             epsabs <= 0 and
!                             epsrel < max(50*rel.mach.acc.,0.5d-28).
!                             result_I, abserr, neval, last, rlist(1),
!                             iord(1) and elist(1) are set to zero.
!                             alist(1) and blist(1) are set to a and b
!                             respectively.
!
!            alist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the left end points
!                     of the subintervals in the partition of the
!                     given integration range (a,b)
!
!            blist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the right end points
!                     of the subintervals in the partition of the given
!                     integration range (a,b)
!
!            rlist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension at least Climit, the first k
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that elist(iord(1)), ..., elist(iord(k))
!                     form a decreasing sequence, with k = last
!                     if last <= (Climit/2+2), and k = Climit+1-last
!                     otherwise
!
!            last   - integer
!                     number of subintervals actually produced in the
!                     subdivision process
!
!***references  (none)
!***routines called  dqelg,dqk21,dqpsrt
!***end prologue  dqagse
!
      real(DP) :: a,abseps,abserr,alist,area,area1,area12,area2,a1,a2,b,blist,b1,b2,correc,dabs,defabs,defab1,defab2,dmax1,&
        dres,elist,epmach,epsabs,epsrel,erlarg,erlast,errbnd,errmax,    &
        error1,error2,erro12,errsum,ertest,f,oflow,resabs,reseps,result_I,&
        res3la,rlist,rlist2,small,uflow
      integer(I4B) :: id,ier,ierro,iord,iroff1,iroff2,iroff3,jupbnd,k,ksgn,     &
        ktmin,last,Climit,maxerr,neval,nres,nrmax,numrl2
      logical extrap,noext
!
      dimension alist(Climit),blist(Climit),elist(Climit),iord(Climit),     &
       res3la(3),rlist(Climit),rlist2(52)
!
      external f
!
!            the dimension of rlist2 is determined by the value of
!            limexp in subroutine dqelg (rlist2 should be of dimension
!            (limexp+2) at least).
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least limexp+2 containing
!                       the part of the epsilon table which is still
!                       needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest error
!                       estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!                       (before that subdivision has taken place)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left interval
!           *****2    - variable for the right interval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements currently in rlist2. if an
!                       appropriate approximation to the compounded
!                       integral has been obtained it is put in
!                       rlist2(numrl2) after numrl2 has been increased
!                       by one.
!           small     - length of the smallest interval considered up
!                       to now, multiplied by 1.5
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine is
!                       attempting to perform extrapolation i.e. before
!                       subdividing the smallest interval we try to
!                       decrease the value of erlarg.
!           noext     - logical variable denoting that extrapolation
!                       is no longer allowed (true value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  dqagse
      epmach = d1mach(4)
!
!            test on validity of parameters
!            ------------------------------
      ier = 0
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      if (epsabs <= 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28))   ier = 6
      if (ier == 6) go to 999
!
!           first approximation to the integral
!           -----------------------------------
!
      uflow = d1mach(1)
      oflow = d1mach(2)
      ierro = 0
      call dqk21(f,a,b,result_I,abserr,defabs,resabs)
!
!           test on accuracy.
!
      dres = dabs(result_I)
      errbnd = dmax1(epsabs,epsrel*dres)
      last = 1
      rlist(1) = result_I
      elist(1) = abserr
      iord(1) = 1
      if (abserr <= 1.0d+02*epmach*defabs.and.abserr > errbnd) ier = 2
      if (Climit == 1) ier = 1
      if (ier /= 0.or.(abserr <= errbnd.and.abserr /= resabs).or.  abserr == 0.0d+00) go to 140
!
!           initialization
!           --------------
!
      rlist2(1) = result_I
      errmax = abserr
      maxerr = 1
      area = result_I
      errsum = abserr
      abserr = oflow
      nrmax = 1
      nres = 0
      numrl2 = 2
      ktmin = 0
      extrap = .false.
      noext = .false.
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      if (dres >= (0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
!
!           main do-loop
!           ------------
!
      do 90 last = 2,Climit
!
!           bisect the subinterval with the nrmax-th largest error
!           estimate.
!
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        call dqk21(f,a1,b1,area1,error1,resabs,defab1)
        call dqk21(f,a2,b2,area2,error2,resabs,defab2)
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (defab1 == error1.or.defab2 == error2) go to 15
        if (dabs(rlist(maxerr)-area12) > 0.1d-04*dabs(area12)  .or.erro12 < 0.99d+00*errmax) go to 10
        if (extrap) iroff2 = iroff2+1
        if (.not.extrap) iroff1 = iroff1+1
   10   if (last > 10.and.erro12 > errmax) iroff3 = iroff3+1
   15   rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1+iroff2 >= 10.or.iroff3 >= 20) ier = 2
        if (iroff2 >= 5) ierro = 3
!
!           set error flag in the case that the number of subintervals
!           equals Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)*  (dabs(a2)+0.1d+04*uflow)) ier = 4
!
!           append the newly-created intervals to the list.
!
        if (error2 > error1) go to 20
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 30
   20   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with nrmax-th largest error estimate (to be bisected next).
!
   30   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
        if (errsum <= errbnd) go to 115
! ***jump out of do-loop
        if (ier /= 0) go to 100
        if (last == 2) go to 80
        if (noext) go to 90
        erlarg = erlarg-erlast
        if (dabs(b1-a1) > small) erlarg = erlarg+erro12
        if (extrap) go to 40
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
        if (dabs(blist(maxerr)-alist(maxerr)) > small) go to 90
        extrap = .true.
        nrmax = 2
   40   if (ierro == 3.or.erlarg <= ertest) go to 60
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors over the
!           larger intervals (erlarg) and perform extrapolation.
!
        id = nrmax
        jupbnd = last
        if (last > (2+Climit/2)) jupbnd = Climit+3-last
        do 50 k = id,jupbnd
          maxerr = iord(nrmax)
          errmax = elist(maxerr)
! ***jump out of do-loop
          if (dabs(blist(maxerr)-alist(maxerr)) > small) go to 90
          nrmax = nrmax+1
   50   continue
!
!           perform extrapolation.
!
   60   numrl2 = numrl2+1
        rlist2(numrl2) = area
        call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if (ktmin > 5.and.abserr < 0.1d-02*errsum) ier = 5
        if (abseps >= abserr) go to 70
        ktmin = 0
        abserr = abseps
        result_I = reseps
        correc = erlarg
        ertest = dmax1(epsabs,epsrel*dabs(reseps))
! ***jump out of do-loop
        if (abserr <= ertest) go to 100
!
!           prepare bisection of the smallest interval.
!
   70   if (numrl2 == 1) noext = .true.
        if (ier == 5) go to 100
        maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        small = small*0.5d+00
        erlarg = errsum
        go to 90
   80   small = dabs(b-a)*0.375d+00
        erlarg = errsum
        ertest = errbnd
        rlist2(2) = area
   90 continue
!
!           set final result_I and error estimate.
!           ------------------------------------
!
  100 if (abserr == oflow) go to 115
      if (ier+ierro == 0) go to 110
      if (ierro == 3) abserr = abserr+correc
      if (ier == 0) ier = 3
      if (result_I /= 0.0d+00.and.area /= 0.0d+00) go to 105
      if (abserr > errsum) go to 115
      if (area == 0.0d+00) go to 130
      go to 110
  105 if (abserr/dabs(result_I) > errsum/dabs(area)) go to 115
!
!           test on divergence.
!
  110 if (ksgn == (-1).and.dmax1(dabs(result_I),dabs(area)) <= defabs*0.1d-01) go to 130
      if (0.1d-01 > (result_I/area).or.(result_I/area) > 0.1d+03 .or.errsum > dabs(area)) ier = 6
      go to 130
!
!           compute global integral sum.
!
  115 result_I = 0.0d+00
      do 120 k = 1,last
         result_I = result_I+rlist(k)
  120 continue
      abserr = errsum
  130 if (ier > 2) ier = ier-1
  140 neval = 42*last-21
  999 return
end  subroutine dqagse
subroutine dqags(f,a,b,epsabs,epsrel,result_I,abserr,neval,ier,   Climit,lenw,last,iwork,work)
!*********************************************************************72
!
!c DQAGS estimates the integral of a function.
!
!***begin prologue  dqags
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a1
!***keywords  automatic integrator, general-purpose,
!             (end-point) singularities, extrapolation,
!             globally adaptive
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & prog. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral  i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of a definite integral
!        standard fortran subroutine
!        real(DP) :: version
!
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more sub-
!                             divisions by increasing the value of Climit
!                             (and taking the according dimension
!                             adjustments into account. however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. if
!                             the position of a local difficulty can be
!                             determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is detec-
!                             ted, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour
!                             occurs at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table. it is presumed that
!                             the requested tolerance cannot be
!                             achieved, and that the returned result_I is
!                             the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28)
!                             or Climit < 1 or lenw < Climit*4.
!                             result_I, abserr, neval, last are set to
!                             zero.except when Climit or lenw is invalid,
!                             iwork(1), work(Climit*2+1) and
!                             work(Climit*3+1) are set to zero, work(1)
!                             is set to a and work(Climit+1) to b.
!
!         dimensioning parameters
!            Climit - integer
!                    dimensioning parameter for iwork
!                    Climit determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (a,b), Climit >= 1.
!                    if Climit < 1, the routine will end with ier = 6.
!
!            lenw  - integer
!                    dimensioning parameter for work
!                    lenw must be at least Climit*4.
!                    if lenw < Climit*4, the routine will end
!                    with ier = 6.
!
!            last  - integer
!                    on return, last equals the number of subintervals
!                    produced in the subdivision process, detemines the
!                    number of significant elements actually in the work
!                    arrays.
!
!         work arrays
!            iwork - integer
!                    vector of dimension at least Climit, the first k
!                    elements of which contain pointers
!                    to the error estimates over the subintervals
!                    such that work(Climit*3+iwork(1)),... ,
!                    work(Climit*3+iwork(k)) form a decreasing
!                    sequence, with k = last if last <= (Climit/2+2),
!                    and k = Climit+1-last otherwise
!
!            work  - real(DP) ::
!                    vector of dimension at least lenw
!                    on return
!                    work(1), ..., work(last) contain the left
!                     end-points of the subintervals in the
!                     partition of (a,b),
!                    work(Climit+1), ..., work(Climit+last) contain
!                     the right end-points,
!                    work(Climit*2+1), ..., work(Climit*2+last) contain
!                     the integral approximations over the subintervals,
!                    work(Climit*3+1), ..., work(Climit*3+last)
!                     contain the error estimates.
!
!***references  (none)
!***routines called  dqagse,xerror
!***end prologue  dqags
!
!
      real(DP) :: a,abserr,b,epsabs,epsrel,f,result_I,work
      integer(I4B) :: ier,iwork,last,lenw,Climit,lvl,l1,l2,l3,neval
!
      dimension iwork(Climit),work(lenw)
!
      external f
!
!         check validity of Climit and lenw.
!
!***first executable statement  dqags
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (Climit < 1.or.lenw < Climit*4) go to 10
!
!         prepare call for dqagse.
!
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
!
      call dqagse(f,a,b,epsabs,epsrel,Climit,result_I,abserr,neval,  ier,work(1),work(l1),work(l2),work(l3),iwork,last)
!
!         call error handler if necessary.
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqags',26,ier,lvl)
      return
end subroutine dqags
subroutine dqawce(f,a,b,c,epsabs,epsrel,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,iord,last)
!*********************************************************************72
!
!c DQAWCE computes a Cauchy principal value.
!
!***begin prologue  dqawce
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1,j4
!***keywords  automatic integrator, special-purpose,
!             cauchy principal value, clenshaw-curtis method
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***  purpose  the routine calculates an approximation result_I to a
!              cauchy principal value i = integral of f*w over (a,b)
!              (w(x) = 1/(x-c), (c /= a, c /= b), hopefully satisfying
!              following claim for accuracy
!              abs(i-result_I) <= max(epsabs,epsrel*abs(i))
!***description
!
!        computation of a cauchy principal value
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            c      - real(DP) ::
!                     parameter in the weight function, c /= a, c /= b
!                     if c = a or c = b, the routine will end with
!                     ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), Climit >= 1
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more sub-
!                             divisions by increasing the value of
!                             Climit. however, if this yields no
!                             improvement it is advised to analyze the
!                             the integrand, in order to determine the
!                             the integration difficulties. if the
!                             position of a local difficulty can be
!                             determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling
!                             appropriate integrators on the subranges.
!                         = 2 the occurrence of roundoff error is detec-
!                             ted, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour
!                             occurs at some interior points of
!                             the integration interval.
!                         = 6 the input is invalid, because
!                             c = a or c = b or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or Climit < 1.
!                             result_I, abserr, neval, rlist(1), elist(1),
!                             iord(1) and last are set to zero. alist(1)
!                             and blist(1) are set to a and b
!                             respectively.
!
!            alist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the left
!                      end points of the subintervals in the partition
!                      of the given integration range (a,b)
!
!            blist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the right
!                      end points of the subintervals in the partition
!                      of the given integration range (a,b)
!
!            rlist   - real(DP) ::
!                      vector of dimension at least Climit, the first
!                       last  elements of which are the integral
!                      approximations on the subintervals
!
!            elist   - real(DP) ::
!                      vector of dimension Climit, the first  last
!                      elements of which are the moduli of the absolute
!                      error estimates on the subintervals
!
!            iord    - integer
!                      vector of dimension at least Climit, the first k
!                      elements of which are pointers to the error
!                      estimates over the subintervals, so that
!                      elist(iord(1)), ..., elist(iord(k)) with k = last
!                      if last <= (Climit/2+2), and k = Climit+1-last
!                      otherwise, form a decreasing sequence
!
!            last    - integer
!                      number of subintervals actually produced in
!                      the subdivision process
!
!***references  (none)
!***routines called  dqc25c,dqpsrt
!***end prologue  dqawce
!
      real(DP) :: a,aa,abserr,alist,area,area1,area12,area2,a1,a2, &
        b,bb,blist,b1,b2,c,dabs,dmax1,elist,epmach,epsabs,epsrel,&
        errbnd,errmax,error1,erro12,error2,errsum,f,result_I,rlist,uflow
      integer(I4B) :: ier,iord,iroff1,iroff2,k,krule,last,Climit,maxerr,nev,     &
        neval,nrmax
!
      dimension alist(Climit),blist(Climit),rlist(Climit),elist(Climit),    &
        iord(Climit)
!
      external f
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest
!                       error estimate
!           errmax    - elist(maxerr)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqawce
      epmach = d1mach(4)
      uflow = d1mach(1)
!
!
!           test on validity of parameters
!           ------------------------------
!
      ier = 6
      neval = 0
      last = 0
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (c == a.or.c == b.or.(epsabs <= 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28))) go to 999
!
!           first approximation to the integral
!           -----------------------------------
!
      aa=a
      bb=b
      if (a <= b) go to 10
      aa=b
      bb=a
   10 ier=0
      krule = 1
      call dqc25c(f,aa,bb,c,result_I,abserr,krule,neval)
      last = 1
      rlist(1) = result_I
      elist(1) = abserr
      iord(1) = 1
      alist(1) = a
      blist(1) = b
!
!           test on accuracy
!
      errbnd = dmax1(epsabs,epsrel*dabs(result_I))
      if (Climit == 1) ier = 1
      if (abserr < dmin1(0.1d-01*dabs(result_I),errbnd).or.ier == 1) go to 70
!
!           initialization
!           --------------
!
      alist(1) = aa
      blist(1) = bb
      rlist(1) = result_I
      errmax = abserr
      maxerr = 1
      area = result_I
      errsum = abserr
      nrmax = 1
      iroff1 = 0
      iroff2 = 0
!
!           main do-loop
!           ------------
!
      do 40 last = 2,Climit
!
!           bisect the subinterval with nrmax-th largest
!           error estimate.
!
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        b2 = blist(maxerr)
        if (c <= b1.and.c > a1) b1 = 0.5d+00*(c+b2)
        if (c > b1.and.c < b2) b1 = 0.5d+00*(a1+c)
        a2 = b1
        krule = 2
        call dqc25c(f,a1,b1,c,area1,error1,krule,nev)
        neval = neval+nev
        call dqc25c(f,a2,b2,c,area2,error2,krule,nev)
        neval = neval+nev
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (dabs(rlist(maxerr)-area12) < 0.1d-04*dabs(area12).and.erro12 >= 0.99d+00*errmax.and.krule == 0) iroff1 = iroff1+1
        if (last > 10.and.erro12 > errmax.and.krule == 0)iroff2 = iroff2+1
        rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = dmax1(epsabs,epsrel*dabs(area))
        if (errsum <= errbnd) go to 15
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1 >= 6.and.iroff2 > 20) ier = 2
!
!           set error flag in the case that number of interval
!           bisections exceeds Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)*(dabs(a2)+0.1d+04*uflow)) ier = 3
!
!           append the newly-created intervals to the list.
!
   15   if (error2 > error1) go to 20
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 30
   20   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with nrmax-th largest error estimate (to be bisected next).
!
   30    call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
        if (ier /= 0.or.errsum <= errbnd) go to 50
   40 continue
!
!           compute final result_I.
!           ---------------------
!
   50 result_I = 0.0d+00
      do 60 k=1,last
        result_I = result_I+rlist(k)
   60 continue
      abserr = errsum
   70 if (aa == b) result_I=-result_I
  999 return
end  subroutine dqawce
subroutine dqawc(f,a,b,c,epsabs,epsrel,result_I,abserr,neval,ier,Climit,lenw,last,iwork,work)
!*********************************************************************72
!
!c DQAWC computes a Cauchy principal value.
!
!***begin prologue  dqawc
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1,j4
!***keywords  automatic integrator, special-purpose,
!             cauchy principal value,
!             clenshaw-curtis, globally adaptive
!***author  piessens,robert ,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a
!            cauchy principal value i = integral of f*w over (a,b)
!            (w(x) = 1/((x-c), c /= a, c /= b), hopefully satisfying
!            following claim for accuracy
!            abs(i-result_I) <= max(epsabe,epsrel*abs(i)).
!***description
!
!        computation of a cauchy principal value
!        standard fortran subroutine
!        real(DP) :: version
!
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     under limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            c      - parameter in the weight function, c /= a, c /= b.
!                     if c = a or c = b, the routine will end with
!                     ier = 6 .
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate or the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more sub-
!                             divisions by increasing the value of Climit
!                             (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties.
!                             if the position of a local difficulty
!                             can be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling
!                             appropriate integrators on the subranges.
!                         = 2 the occurrence of roundoff error is detec-
!                             ted, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 the input is invalid, because
!                             c = a or c = b or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or Climit < 1 or lenw < Climit*4.
!                             result_I, abserr, neval, last are set to
!                             zero. exept when lenw or Climit is invalid,
!                             iwork(1), work(Climit*2+1) and
!                             work(Climit*3+1) are set to zero, work(1)
!                             is set to a and work(Climit+1) to b.
!
!         dimensioning parameters
!            Climit - integer
!                    dimensioning parameter for iwork
!                    Climit determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (a,b), Climit >= 1.
!                    if Climit < 1, the routine will end with ier = 6.
!
!           lenw   - integer
!                    dimensioning parameter for work
!                    lenw must be at least Climit*4.
!                    if lenw < Climit*4, the routine will end with
!                    ier = 6.
!
!            last  - integer
!                    on return, last equals the number of subintervals
!                    produced in the subdivision process, which
!                    determines the number of significant elements
!                    actually in the work arrays.
!
!         work arrays
!            iwork - integer
!                    vector of dimension at least Climit, the first k
!                    elements of which contain pointers
!                    to the error estimates over the subintervals,
!                    such that work(Climit*3+iwork(1)), ... ,
!                    work(Climit*3+iwork(k)) form a decreasing
!                    sequence, with k = last if last <= (Climit/2+2),
!                    and k = Climit+1-last otherwise
!
!            work  - real(DP) ::
!                    vector of dimension at least lenw
!                    on return
!                    work(1), ..., work(last) contain the left
!                     end points of the subintervals in the
!                     partition of (a,b),
!                    work(Climit+1), ..., work(Climit+last) contain
!                     the right end points,
!                    work(Climit*2+1), ..., work(Climit*2+last) contain
!                     the integral approximations over the subintervals,
!                    work(Climit*3+1), ..., work(Climit*3+last)
!                     contain the error estimates.
!
!***references  (none)
!***routines called  dqawce,xerror
!***end prologue  dqawc
!
      real(DP) :: a,abserr,b,c,epsabs,epsrel,f,result_I,work
      integer(I4B) :: ier,iwork,last,lenw,Climit,lvl,l1,l2,l3,neval
!
      dimension iwork(Climit),work(lenw)
!
      external f
!
!         check validity of Climit and lenw.
!
!***first executable statement  dqawc
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (Climit < 1.or.lenw < Climit*4) go to 10
!
!         prepare call for dqawce.
!
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
      call dqawce(f,a,b,c,epsabs,epsrel,Climit,result_I,abserr,neval,ier,work(1),work(l1),work(l2),work(l3),iwork,last)
!
!         call error handler if necessary.
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqawc',26,ier,lvl)
      return
end  subroutine dqawc
subroutine dqawfe(f,a,omega,integr,epsabs,limlst,Climit,maxp1,result_I,abserr,neval,ier,rslst,erlst,ierlst,lst,alist,blist, &
                  rlist,elist,iord,nnlog,chebmo)
!*********************************************************************72
!
!c DQAWFE computes Fourier integrals.
!
!***begin prologue  dqawfe
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1
!***keywords  automatic integrator, special-purpose,
!             fourier integrals,
!             integration between zeros with dqawoe,
!             convergence acceleration with dqelg
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           dedoncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a
!            given fourier integal
!            i = integral of f(x)*w(x) over (a,infinity)
!            where w(x)=cos(omega*x) or w(x)=sin(omega*x),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= epsabs.
!***description
!
!        computation of fourier integrals
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to
!                     be declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            omega  - real(DP) ::
!                     parameter in the weight function
!
!            integr - integer
!                     indicates which weight function is used
!                     integr = 1      w(x) = cos(omega*x)
!                     integr = 2      w(x) = sin(omega*x)
!                     if integr /= 1.and.integr /= 2, the routine will
!                     end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested, epsabs > 0
!                     if epsabs <= 0, the routine will end with ier = 6.
!
!            limlst - integer
!                     limlst gives an upper bound on the number of
!                     cycles, limlst >= 1.
!                     if limlst < 3, the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subintervals
!                     allowed in the partition of each cycle, Climit >= 1
!                     each cycle, Climit >= 1.
!
!            maxp1  - integer
!                     gives an upper bound on the number of
!                     chebyshev moments which can be stored, i.e.
!                     for the intervals of lengths abs(b-a)*2**(-l),
!                     l=0,1, ..., maxp1-2, maxp1 >= 1
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral x
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - ier = 0 normal and reliable termination of
!                             the routine. it is assumed that the
!                             requested accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine. the
!                             estimates for integral and error are less
!                             reliable. it is assumed that the requested
!                             accuracy has not been achieved.
!            error messages
!                    if omega /= 0
!                     ier = 1 maximum number of  cycles  allowed
!                             has been achieved., i.e. of subintervals
!                             (a+(k-1)c,a+kc) where
!                             c = (2*int(abs(omega))+1)*pi/abs(omega),
!                             for k = 1, 2, ..., lst.
!                             one can allow more cycles by increasing
!                             the value of limlst (and taking the
!                             according dimension adjustments into
!                             account).
!                             examine the array iwork which contains
!                             the error flags on the cycles, in order to
!                             look for eventual local integration
!                             difficulties. if the position of a local
!                             difficulty can be determined (e.g.
!                             singularity, discontinuity within the
!                             interval) one will probably gain from
!                             splitting up the interval at this point
!                             and calling appropriate integrators on
!                             the subranges.
!                         = 4 the extrapolation table constructed for
!                             convergence acceleration of the series
!                             formed by the integral contributions over
!                             the cycles, does not converge to within
!                             the requested accuracy. as in the case of
!                             ier = 1, it is advised to examine the
!                             array iwork which contains the error
!                             flags on the cycles.
!                         = 6 the input is invalid because
!                             (integr /= 1 and integr /= 2) or
!                              epsabs <= 0 or limlst < 3.
!                              result_I, abserr, neval, lst are set
!                              to zero.
!                         = 7 bad integrand behaviour occurs within one
!                             or more of the cycles. location and type
!                             of the difficulty involved can be
!                             determined from the vector ierlst. here
!                             lst is the number of cycles actually
!                             needed (see below).
!                             ierlst(k) = 1 the maximum number of
!                                           subdivisions (= Climit) has
!                                           been achieved on the k th
!                                           cycle.
!                                       = 2 occurrence of roundoff error
!                                           is detected and prevents the
!                                           tolerance imposed on the
!                                           k th cycle, from being
!                                           achieved.
!                                       = 3 extremely bad integrand
!                                           behaviour occurs at some
!                                           points of the k th cycle.
!                                       = 4 the integration procedure
!                                           over the k th cycle does
!                                           not converge (to within the
!                                           required accuracy) due to
!                                           roundoff in the
!                                           extrapolation procedure
!                                           invoked on this cycle. it
!                                           is assumed that the result_I
!                                           on this interval is the
!                                           best which can be obtained.
!                                       = 5 the integral over the k th
!                                           cycle is probably divergent
!                                           or slowly convergent. it
!                                           must be noted that
!                                           divergence can occur with
!                                           any other value of
!                                           ierlst(k).
!                    if omega = 0 and integr = 1,
!                    the integral is calculated by means of dqagie
!                    and ier = ierlst(1) (with meaning as described
!                    for ierlst(k), k = 1).
!
!            rslst  - real(DP) ::
!                     vector of dimension at least limlst
!                     rslst(k) contains the integral contribution
!                     over the interval (a+(k-1)c,a+kc) where
!                     c = (2*int(abs(omega))+1)*pi/abs(omega),
!                     k = 1, 2, ..., lst.
!                     note that, if omega = 0, rslst(1) contains
!                     the value of the integral over (a,infinity).
!
!            erlst  - real(DP) ::
!                     vector of dimension at least limlst
!                     erlst(k) contains the error estimate corresponding
!                     with rslst(k).
!
!            ierlst - integer
!                     vector of dimension at least limlst
!                     ierlst(k) contains the error flag corresponding
!                     with rslst(k). for the meaning of the local error
!                     flags see description of output parameter ier.
!
!            lst    - integer
!                     number of subintervals needed for the integration
!                     if omega = 0 then lst is set to 1.
!
!            alist, blist, rlist, elist - real(DP) ::
!                     vector of dimension at least Climit,
!
!            iord, nnlog - integer
!                     vector of dimension at least Climit, providing
!                     space for the quantities needed in the subdivision
!                     process of each cycle
!
!            chebmo - real(DP) ::
!                     array of dimension at least (maxp1,25), providing
!                     space for the chebyshev moments needed within the
!                     cycles
!
!***references  (none)
!***routines called  dqagie,dqawoe,dqelg
!***end prologue  dqawfe
!
      real(DP) :: a,abseps,abserr,alist,blist,chebmo,correc,cycle, &
        c1,c2,dabs,dl,dla,dmax1,drl,elist,erlst,ep,eps,epsa,     &
        epsabs,errsum,f,fact,omega,p,pi,p1,psum,reseps,result_I,res3la,   &
        rlist,rslst,uflow
      integer(I4B) :: ier,ierlst,integr,iord,ktmin,l,last,lst,Climit,limlst,ll,  &
          maxp1,momcom,nev,neval,nnlog,nres,numrl2
!
      dimension alist(Climit),blist(Climit),chebmo(maxp1,25),elist(Climit),&
        erlst(limlst),ierlst(limlst),iord(Climit),nnlog(Climit),psum(52), &
        res3la(3),rlist(Climit),rslst(limlst)
!
      external f
!
!
!            the dimension of  psum  is determined by the value of
!            limexp in subroutine dqelg (psum must be of dimension
!            (limexp+2) at least).
!
!           list of major variables
!           -----------------------
!
!           c1, c2    - end points of subinterval (of length cycle)
!           cycle     - (2*int(abs(omega))+1)*pi/abs(omega)
!           psum      - vector of dimension at least (limexp+2)
!                       (see routine dqelg)
!                       psum contains the part of the epsilon table
!                       which is still needed for further computations.
!                       each element of psum is a partial sum of the
!                       series which should sum to the value of the
!                       integral.
!           errsum    - sum of error estimates over the subintervals,
!                       calculated cumulatively
!           epsa      - absolute tolerance requested over current
!                       subinterval
!           chebmo    - array containing the modified chebyshev
!                       moments (see also routine dqc25f)
!
      data p/0.9d+00/
      data pi / 3.14159265358979323846264338327950d0 /
!
!           test on validity of parameters
!           ------------------------------
!
!***first executable statement  dqawfe
      result_I = 0.0d+00
      abserr = 0.0d+00
      neval = 0
      lst = 0
      ier = 0
      if ((integr /= 1.and.integr /= 2).or.epsabs <= 0.0d+00.or.limlst < 3) ier = 6
      if (ier == 6) go to 999
      if (omega /= 0.0d+00) go to 10
!
!           integration by dqagie if omega is zero
!           --------------------------------------
!
      if (integr == 1) call dqagie(f,0.0d+00,1,epsabs,0.0d+00,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,iord,last)
      rslst(1) = result_I
      erlst(1) = abserr
      ierlst(1) = ier
      lst = 1
      go to 999
!
!           initializations
!           ---------------
!
   10 l = dabs(omega)
      dl = 2*l+1
      cycle = dl*pi/dabs(omega)
      ier = 0
      ktmin = 0
      neval = 0
      numrl2 = 0
      nres = 0
      c1 = a
      c2 = cycle+a
      p1 = 0.1d+01-p
      uflow = d1mach(1)
      eps = epsabs
      if (epsabs > uflow/p1) eps = epsabs*p1
      ep = eps
      fact = 0.1d+01
      correc = 0.0d+00
      abserr = 0.0d+00
      errsum = 0.0d+00
!
!           main do-loop
!           ------------
!
      do 50 lst = 1,limlst
!
!           integrate over current subinterval.
!
        dla = lst
        epsa = eps*fact
        call dqawoe(f,c1,c2,omega,integr,epsa,0.0d+00,Climit,lst,maxp1,rslst(lst),erlst(lst), &
                    nev,ierlst(lst),last,alist,blist,rlist,elist,iord,nnlog,momcom,chebmo)
        neval = neval+nev
        fact = fact*p
        errsum = errsum+erlst(lst)
        drl = 0.5d+02*dabs(rslst(lst))
!
!           test on accuracy with partial sum
!
        if ((errsum+drl) <= epsabs.and.lst >= 6) go to 80
        correc = dmax1(correc,erlst(lst))
        if (ierlst(lst) /= 0) eps = dmax1(ep,correc*p1)
        if (ierlst(lst) /= 0) ier = 7
        if (ier == 7.and.(errsum+drl) <= correc*0.1d+02.and.lst > 5) go to 80
        numrl2 = numrl2+1
        if (lst > 1) go to 20
        psum(1) = rslst(1)
        go to 40
   20   psum(numrl2) = psum(ll)+rslst(lst)
        if (lst == 2) go to 40
!
!           test on maximum number of subintervals
!
        if (lst == limlst) ier = 1
!
!           perform new extrapolation
!
        call dqelg(numrl2,psum,reseps,abseps,res3la,nres)
!
!           test whether extrapolated result_I is influenced by roundoff
!
        ktmin = ktmin+1
        if (ktmin >= 15.and.abserr <= 0.1d-02*(errsum+drl)) ier = 4
        if (abseps > abserr.and.lst /= 3) go to 30
        abserr = abseps
        result_I = reseps
        ktmin = 0
!
!           if ier is not 0, check whether direct result_I (partial sum)
!           or extrapolated result_I yields the best integral
!           approximation
!
        if ((abserr+0.1d+02*correc) <= epsabs.or.(abserr <= epsabs.and.0.1d+02*correc >= epsabs)) go to 60
   30   if (ier /= 0.and.ier /= 7) go to 60
   40   ll = numrl2
        c1 = c2
        c2 = c2+cycle
   50 continue
!
!         set final result_I and error estimate
!         -----------------------------------
!
   60 abserr = abserr+0.1d+02*correc
      if (ier == 0) go to 999
      if (result_I /= 0.0d+00.and.psum(numrl2) /= 0.0d+00) go to 70
      if (abserr > errsum) go to 80
      if (psum(numrl2) == 0.0d+00) go to 999
   70 if (abserr/dabs(result_I) > (errsum+drl)/dabs(psum(numrl2)))go to 80
      if (ier >= 1.and.ier /= 7) abserr = abserr+drl
      go to 999
   80 result_I = psum(numrl2)
      abserr = errsum+drl
  999 return
end  subroutine dqawfe
subroutine dqawf(f,a,omega,integr,epsabs,result_I,abserr,neval,ier,limlst,lst,leniw,maxp1,lenw,iwork,work)
!*********************************************************************72
!
!c DQAWF computes Fourier integrals over the interval [ A, +Infinity ).
!
!***begin prologue  dqawf
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a1
!***keywords  automatic integrator, special-purpose,fourier
!             integral, integration between zeros with dqawoe,
!             convergence acceleration with dqelg
!***author  piessens,robert ,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            fourier integral i=integral of f(x)*w(x) over (a,infinity)
!            where w(x) = cos(omega*x) or w(x) = sin(omega*x).
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= epsabs.
!***description
!
!        computation of fourier integrals
!        standard fortran subroutine
!        real(DP) :: version
!
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            omega  - real(DP) ::
!                     parameter in the integrand weight function
!
!            integr - integer
!                     indicates which of the weight functions is used
!                     integr = 1      w(x) = cos(omega*x)
!                     integr = 2      w(x) = sin(omega*x)
!                     if integr /= 1.and.integr /= 2, the routine
!                     will end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested, epsabs > 0.
!                     if epsabs <= 0, the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                    if omega /= 0
!                     ier = 1 maximum number of cycles allowed
!                             has been achieved, i.e. of subintervals
!                             (a+(k-1)c,a+kc) where
!                             c = (2*int(abs(omega))+1)*pi/abs(omega),
!                             for k = 1, 2, ..., lst.
!                             one can allow more cycles by increasing
!                             the value of limlst (and taking the
!                             according dimension adjustments into
!                             account). examine the array iwork which
!                             contains the error flags on the cycles, in
!                             order to look for eventual local
!                             integration difficulties.
!                             if the position of a local difficulty
!                             can be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling
!                             appropriate integrators on the subranges.
!                         = 4 the extrapolation table constructed for
!                             convergence accelaration of the series
!                             formed by the integral contributions over
!                             the cycles, does not converge to within
!                             the requested accuracy.
!                             as in the case of ier = 1, it is advised
!                             to examine the array iwork which contains
!                             the error flags on the cycles.
!                         = 6 the input is invalid because
!                             (integr /= 1 and integr /= 2) or
!                              epsabs <= 0 or limlst < 1 or
!                              leniw < (limlst+2) or maxp1 < 1 or
!                              lenw < (leniw*2+maxp1*25).
!                              result_I, abserr, neval, lst are set to
!                              zero.
!                         = 7 bad integrand behaviour occurs within
!                             one or more of the cycles. location and
!                             type of the difficulty involved can be
!                             determined from the first lst elements of
!                             vector iwork.  here lst is the number of
!                             cycles actually needed (see below).
!                             iwork(k) = 1 the maximum number of
!                                          subdivisions (=(leniw-limlst)
!                                          /2) has been achieved on the
!                                          k th cycle.
!                                      = 2 occurrence of roundoff error
!                                          is detected and prevents the
!                                          tolerance imposed on the k th
!                                          cycle, from being achieved
!                                          on this cycle.
!                                      = 3 extremely bad integrand
!                                          behaviour occurs at some
!                                          points of the k th cycle.
!                                      = 4 the integration procedure
!                                          over the k th cycle does
!                                          not converge (to within the
!                                          required accuracy) due to
!                                          roundoff in the extrapolation
!                                          procedure invoked on this
!                                          cycle. it is assumed that the
!                                          result_I on this interval is
!                                          the best which can be
!                                          obtained.
!                                      = 5 the integral over the k th
!                                          cycle is probably divergent
!                                          or slowly convergent. it must
!                                          be noted that divergence can
!                                          occur with any other value of
!                                          iwork(k).
!                    if omega = 0 and integr = 1,
!                    the integral is calculated by means of dqagie,
!                    and ier = iwork(1) (with meaning as described
!                    for iwork(k),k = 1).
!
!         dimensioning parameters
!            limlst - integer
!                     limlst gives an upper bound on the number of
!                     cycles, limlst >= 3.
!                     if limlst < 3, the routine will end with ier = 6.
!
!            lst    - integer
!                     on return, lst indicates the number of cycles
!                     actually needed for the integration.
!                     if omega = 0, then lst is set to 1.
!
!            leniw  - integer
!                     dimensioning parameter for iwork. on entry,
!                     (leniw-limlst)/2 equals the maximum number of
!                     subintervals allowed in the partition of each
!                     cycle, leniw >= (limlst+2).
!                     if leniw < (limlst+2), the routine will end with
!                     ier = 6.
!
!            maxp1  - integer
!                     maxp1 gives an upper bound on the number of
!                     chebyshev moments which can be stored, i.e. for
!                     the intervals of lengths abs(b-a)*2**(-l),
!                     l = 0,1, ..., maxp1-2, maxp1 >= 1.
!                     if maxp1 < 1, the routine will end with ier = 6.
!            lenw   - integer
!                     dimensioning parameter for work
!                     lenw must be at least leniw*2+maxp1*25.
!                     if lenw < (leniw*2+maxp1*25), the routine will
!                     end with ier = 6.
!
!         work arrays
!            iwork  - integer
!                     vector of dimension at least leniw
!                     on return, iwork(k) for k = 1, 2, ..., lst
!                     contain the error flags on the cycles.
!
!            work   - real(DP) ::
!                     vector of dimension at least
!                     on return,
!                     work(1), ..., work(lst) contain the integral
!                      approximations over the cycles,
!                     work(limlst+1), ..., work(limlst+lst) contain
!                      the error extimates over the cycles.
!                     further elements of work have no specific
!                     meaning for the user.
!
!***references  (none)
!***routines called  dqawfe,xerror
!***end prologue  dqawf
!
       real(DP) :: a,abserr,epsabs,f,omega,result_I,work
       integer(I4B) :: ier,integr,iwork,last,leniw,lenw,Climit,limlst,ll2,lvl,   &
        lst,l1,l2,l3,l4,l5,l6,maxp1,neval
!
       dimension iwork(leniw),work(lenw)
!
       external f
!
!         check validity of limlst, leniw, maxp1 and lenw.
!
!***first executable statement  dqawf
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (limlst < 3.or.leniw < (limlst+2).or.maxp1 < 1.or.lenw <(leniw*2+maxp1*25)) go to 10
!
!         prepare call for dqawfe
!
      Climit = (leniw-limlst)/2
      l1 = limlst+1
      l2 = limlst+l1
      l3 = Climit+l2
      l4 = Climit+l3
      l5 = Climit+l4
      l6 = Climit+l5
      ll2 = Climit+l1
      call dqawfe(f,a,omega,integr,epsabs,limlst,Climit,maxp1,result_I,abserr,neval,ier, &
                  work(1),work(l1),iwork(1),lst,work(l2),work(l3),work(l4),work(l5),iwork(l1),iwork(ll2),work(l6))
!
!         call error handler if necessary
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqawf',26,ier,lvl)
      return
end  subroutine dqawf
!*********************************************************************72
!
!c DQAWOE computes the integrals of oscillatory integrands.
!
!***begin prologue  dqawoe
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, special-purpose,
!             integrand with oscillatory cos or sin factor,
!             clenshaw-curtis method, (end point) singularities,
!             extrapolation, globally adaptive
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral
!            i = integral of f(x)*w(x) over (a,b)
!            where w(x) = cos(omega*x) or w(x)=sin(omega*x),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of oscillatory integrals
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            omega  - real(DP) ::
!                     parameter in the integrand weight function
!
!            integr - integer
!                     indicates which of the weight functions is to be
!                     used
!                     integr = 1      w(x) = cos(omega*x)
!                     integr = 2      w(x) = sin(omega*x)
!                     if integr /= 1 and integr /= 2, the routine
!                     will end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subdivisions
!                     in the partition of (a,b), Climit >= 1.
!
!            icall  - integer
!                     if dqawoe is to be used only once, icall must
!                     be set to 1.  assume that during this call, the
!                     chebyshev moments (for clenshaw-curtis integration
!                     of degree 24) have been computed for intervals of
!                     lenghts (abs(b-a))*2**(-l), l=0,1,2,...momcom-1.
!                     if icall > 1 this means that dqawoe has been
!                     called twice or more on intervals of the same
!                     length abs(b-a). the chebyshev moments already
!                     computed are then re-used in subsequent calls.
!                     if icall < 1, the routine will end with ier = 6.
!
!            maxp1  - integer
!                     gives an upper bound on the number of chebyshev
!                     moments which can be stored, i.e. for the
!                     intervals of lenghts abs(b-a)*2**(-l),
!                     l=0,1, ..., maxp1-2, maxp1 >= 1.
!                     if maxp1 < 1, the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the
!                             requested accuracy has been achieved.
!                   - ier > 0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand, in order to
!                             determine the integration difficulties.
!                             if the position of a local difficulty can
!                             be determined (e.g. singularity,
!                             discontinuity within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. if possible,
!                             an appropriate special-purpose integrator
!                             should be used which is designed for
!                             handling the type of difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table.
!                             it is presumed that the requested
!                             tolerance cannot be achieved due to
!                             roundoff in the extrapolation table,
!                             and that the returned result_I is the
!                             best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier > 0.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or (integr /= 1 and integr /= 2) or
!                             icall < 1 or maxp1 < 1.
!                             result_I, abserr, neval, last, rlist(1),
!                             elist(1), iord(1) and nnlog(1) are set
!                             to zero. alist(1) and blist(1) are set
!                             to a and b respectively.
!
!            last  -  integer
!                     on return, last equals the number of
!                     subintervals produces in the subdivision
!                     process, which determines the number of
!                     significant elements actually in the
!                     work arrays.
!            alist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the given integration range (a,b)
!
!            blist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the given integration range (a,b)
!
!            rlist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension at least Climit, the first k
!                     elements of which are pointers to the error
!                     estimates over the subintervals,
!                     such that elist(iord(1)), ...,
!                     elist(iord(k)) form a decreasing sequence, with
!                     k = last if last <= (Climit/2+2), and
!                     k = Climit+1-last otherwise.
!
!            nnlog  - integer
!                     vector of dimension at least Climit, containing the
!                     subdivision levels of the subintervals, i.e.
!                     iwork(i) = l means that the subinterval
!                     numbered i is of length abs(b-a)*2**(1-l)
!
!         on entry and return
!            momcom - integer
!                     indicating that the chebyshev moments
!                     have been computed for intervals of lengths
!                     (abs(b-a))*2**(-l), l=0,1,2, ..., momcom-1,
!                     momcom < maxp1
!
!            chebmo - real(DP) ::
!                     array of dimension (maxp1,25) containing the
!                     chebyshev moments
!
subroutine dqawoe (f,a,b,omega,integr,epsabs,epsrel,Climit,icall,  &
        maxp1,result_I,abserr,neval,ier,last,alist,blist,rlist,elist,iord,&
         nnlog,momcom,chebmo)
!***references  (none)
!***routines called  dqc25f,dqelg,dqpsrt
!***end prologue  dqawoe
!
      real(DP) :: a,abseps,abserr,alist,area,area1,area12,area2,a1,&
        a2,b,blist,b1,b2,chebmo,correc,dabs,defab1,defab2,defabs,dmax1, &
        domega,dres,elist,epmach,epsabs,epsrel,erlarg,erlast,    &
        errbnd,errmax,error1,erro12,error2,errsum,ertest,f,oflow,       &
        omega,resabs,reseps,result_I,res3la,rlist,rlist2,small,uflow,width
      integer(I4B) :: icall,id,ier,ierro,integr,iord,iroff1,iroff2,iroff3,      &
        jupbnd,k,ksgn,ktmin,last,Climit,maxerr,maxp1,momcom,nev,neval,   &
        nnlog,nres,nrmax,nrmom,numrl2
      logical extrap,noext,extall
!
      dimension alist(Climit),blist(Climit),rlist(Climit),elist(Climit),    &
        iord(Climit),rlist2(52),res3la(3),chebmo(maxp1,25),nnlog(Climit)
!
      external f
!
!            the dimension of rlist2 is determined by  the value of
!            limexp in subroutine dqelg (rlist2 should be of
!            dimension (limexp+2) at least).
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           rlist2    - array of dimension at least limexp+2
!                       containing the part of the epsilon table
!                       which is still needed for further computations
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest
!                       error estimate
!           errmax    - elist(maxerr)
!           erlast    - error on the interval currently subdivided
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!           nres      - number of calls to the extrapolation routine
!           numrl2    - number of elements in rlist2. if an appropriate
!                       approximation to the compounded integral has
!                       been obtained it is put in rlist2(numrl2) after
!                       numrl2 has been increased by one
!           small     - length of the smallest interval considered
!                       up to now, multiplied by 1.5
!           erlarg    - sum of the errors over the intervals larger
!                       than the smallest interval considered up to now
!           extrap    - logical variable denoting that the routine is
!                       attempting to perform extrapolation, i.e. before
!                       subdividing the smallest interval we try to
!                       decrease the value of erlarg
!           noext     - logical variable denoting that extrapolation
!                       is no longer allowed (true  value)
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!           oflow is the largest positive magnitude.
!
!***first executable statement  dqawoe
      epmach = d1mach(4)
!
!         test on validity of parameters
!         ------------------------------
!
      ier = 0
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      nnlog(1) = 0
      if ((integr /= 1.and.integr /= 2).or.(epsabs <= 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28)) &
           .or.icall < 1.or.maxp1 < 1) ier = 6
      if (ier == 6) go to 999
!
!           first approximation to the integral
!           -----------------------------------
!
      domega = dabs(omega)
      nrmom = 0
      if (icall > 1) go to 5
      momcom = 0
    5 call dqc25f(f,a,b,domega,integr,nrmom,maxp1,0,result_I,abserr,neval,defabs,resabs,momcom,chebmo)
!
!           test on accuracy.
!
      dres = dabs(result_I)
      errbnd = dmax1(epsabs,epsrel*dres)
      rlist(1) = result_I
      elist(1) = abserr
      iord(1) = 1
      if (abserr <= 0.1d+03*epmach*defabs.and.abserr > errbnd) ier = 2
      if (Climit == 1) ier = 1
      if (ier /= 0.or.abserr <= errbnd) go to 200
!
!           initializations
!           ---------------
!
      uflow = d1mach(1)
      oflow = d1mach(2)
      errmax = abserr
      maxerr = 1
      area = result_I
      errsum = abserr
      abserr = oflow
      nrmax = 1
      extrap = .false.
      noext = .false.
      ierro = 0
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ktmin = 0
      small = dabs(b-a)*0.75d+00
      nres = 0
      numrl2 = 0
      extall = .false.
      if (0.5d+00*dabs(b-a)*domega > 0.2d+01) go to 10
      numrl2 = 1
      extall = .true.
      rlist2(1) = result_I
   10 if (0.25d+00*dabs(b-a)*domega <= 0.2d+01) extall = .true.
      ksgn = -1
      if (dres >= (0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
!
!           main do-loop
!           ------------
!
      do 140 last = 2,Climit
!
!           bisect the subinterval with the nrmax-th largest
!           error estimate.
!
        nrmom = nnlog(maxerr)+1
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        call dqc25f(f,a1,b1,domega,integr,nrmom,maxp1,0,area1,error1,nev,resabs,defab1,momcom,chebmo)
        neval = neval+nev
        call dqc25f(f,a2,b2,domega,integr,nrmom,maxp1,1,area2,error2,nev,resabs,defab2,momcom,chebmo)
        neval = neval+nev
!
!           improve previous approximations to integral
!           and error and test for accuracy.
!
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (defab1 == error1.or.defab2 == error2) go to 25
        if (dabs(rlist(maxerr)-area12) > 0.1d-04*dabs(area12).or.erro12 < 0.99d+00*errmax) go to 20
        if (extrap) iroff2 = iroff2+1
        if (.not.extrap) iroff1 = iroff1+1
   20   if (last > 10.and.erro12 > errmax) iroff3 = iroff3+1
   25   rlist(maxerr) = area1
        rlist(last) = area2
        nnlog(maxerr) = nrmom
        nnlog(last) = nrmom
        errbnd = dmax1(epsabs,epsrel*dabs(area))
!
!           test for roundoff error and eventually set error flag.
!
        if (iroff1+iroff2 >= 10.or.iroff3 >= 20) ier = 2
        if (iroff2 >= 5) ierro = 3
!
!           set error flag in the case that the number of
!           subintervals equals Climit.
!
        if (last == Climit) ier = 1
!
!           set error flag in the case of bad integrand behaviour
!           at a point of the integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)*(dabs(a2)+0.1d+04*uflow)) ier = 4
!
!           append the newly-created intervals to the list.
!
        if (error2 > error1) go to 30
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 40
   30   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with nrmax-th largest error estimate (to bisected next).
!
   40   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
      if (errsum <= errbnd) go to 170
      if (ier /= 0) go to 150
        if (last == 2.and.extall) go to 120
        if (noext) go to 140
        if (.not.extall) go to 50
        erlarg = erlarg-erlast
        if (dabs(b1-a1) > small) erlarg = erlarg+erro12
        if (extrap) go to 70
!
!           test whether the interval to be bisected next is the
!           smallest interval.
!
   50   width = dabs(blist(maxerr)-alist(maxerr))
        if (width > small) go to 140
        if (extall) go to 60
!
!           test whether we can start with the extrapolation procedure
!           (we do this if we integrate over the next interval with
!           use of a gauss-kronrod rule - see subroutine dqc25f).
!
        small = small*0.5d+00
        if (0.25d+00*width*domega > 0.2d+01) go to 140
        extall = .true.
        go to 130
   60   extrap = .true.
        nrmax = 2
   70   if (ierro == 3.or.erlarg <= ertest) go to 90
!
!           the smallest interval has the largest error.
!           before bisecting decrease the sum of the errors over
!           the larger intervals (erlarg) and perform extrapolation.
!
        jupbnd = last
        if (last > (Climit/2+2)) jupbnd = Climit+3-last
        id = nrmax
        do 80 k = id,jupbnd
          maxerr = iord(nrmax)
          errmax = elist(maxerr)
          if (dabs(blist(maxerr)-alist(maxerr)) > small) go to 140
          nrmax = nrmax+1
   80   continue
!
!           perform extrapolation.
!
   90   numrl2 = numrl2+1
        rlist2(numrl2) = area
        if (numrl2 < 3) go to 110
        call dqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if (ktmin > 5.and.abserr < 0.1d-02*errsum) ier = 5
        if (abseps >= abserr) go to 100
        ktmin = 0
        abserr = abseps
        result_I = reseps
        correc = erlarg
        ertest = dmax1(epsabs,epsrel*dabs(reseps))
! ***jump out of do-loop
        if (abserr <= ertest) go to 150
!
!           prepare bisection of the smallest interval.
!
  100   if (numrl2 == 1) noext = .true.
        if (ier == 5) go to 150
  110   maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        small = small*0.5d+00
        erlarg = errsum
        go to 140
  120   small = small*0.5d+00
        numrl2 = numrl2+1
        rlist2(numrl2) = area
  130   ertest = errbnd
        erlarg = errsum
  140 continue
!
!           set the final result_I.
!           ---------------------
!
  150 if (abserr == oflow.or.nres == 0) go to 170
      if (ier+ierro == 0) go to 165
      if (ierro == 3) abserr = abserr+correc
      if (ier == 0) ier = 3
      if (result_I /= 0.0d+00.and.area /= 0.0d+00) go to 160
      if (abserr > errsum) go to 170
      if (area == 0.0d+00) go to 190
      go to 165
  160 if (abserr/dabs(result_I) > errsum/dabs(area)) go to 170
!
!           test on divergence.
!
  165 if (ksgn == (-1).and.dmax1(dabs(result_I),dabs(area)) <=defabs*0.1d-01) go to 190
      if (0.1d-01 > (result_I/area).or.(result_I/area) > 0.1d+03.or.errsum >= dabs(area)) ier = 6
      go to 190
!
!           compute global integral sum.
!
  170 result_I = 0.0d+00
      do 180 k=1,last
        result_I = result_I+rlist(k)
  180 continue
      abserr = errsum
  190 if (ier > 2) ier=ier-1
  200 if (integr == 2.and.omega < 0.0d+00) result_I=-result_I
  999 return
end  subroutine dqawoe
subroutine dqawo(f,a,b,omega,integr,epsabs,epsrel,result_I,abserr,neval,ier,leniw,maxp1,lenw,last,iwork,work)
!*********************************************************************72
!
!c DQAWO computes the integrals of oscillatory integrands.
!
!***begin prologue  dqawo
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, special-purpose,
!             integrand with oscillatory cos or sin factor,
!             clenshaw-curtis method, (end point) singularities,
!             extrapolation, globally adaptive
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i=integral of f(x)*w(x) over (a,b)
!            where w(x) = cos(omega*x)
!            or w(x) = sin(omega*x),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        computation of oscillatory integrals
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the function
!                     f(x).  the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration
!
!            omega  - real(DP) ::
!                     parameter in the integrand weight function
!
!            integr - integer
!                     indicates which of the weight functions is used
!                     integr = 1      w(x) = cos(omega*x)
!                     integr = 2      w(x) = sin(omega*x)
!                     if integr /= 1.and.integr /= 2, the routine will
!                     end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if epsabs <= 0 and
!                     epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of  integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                   - ier > 0 abnormal termination of the routine.
!                             the estimates for integral and error are
!                             less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             (= leniw/2) has been achieved. one can
!                             allow more subdivisions by increasing the
!                             value of leniw (and taking the according
!                             dimension adjustments into account).
!                             however, if this yields no improvement it
!                             is advised to analyze the integrand in
!                             order to determine the integration
!                             difficulties. if the position of a local
!                             difficulty can be determined (e.g.
!                             singularity, discontinuity within the
!                             interval) one will probably gain from
!                             splitting up the interval at this point
!                             and calling the integrator on the
!                             subranges. if possible, an appropriate
!                             special-purpose integrator should be used
!                             which is designed for handling the type of
!                             difficulty involved.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             the error may be under-estimated.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some interior points of the
!                             integration interval.
!                         = 4 the algorithm does not converge.
!                             roundoff error is detected in the
!                             extrapolation table. it is presumed that
!                             the requested tolerance cannot be achieved
!                             due to roundoff in the extrapolation
!                             table, and that the returned result_I is
!                             the best which can be obtained.
!                         = 5 the integral is probably divergent, or
!                             slowly convergent. it must be noted that
!                             divergence can occur with any other value
!                             of ier.
!                         = 6 the input is invalid, because
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or (integr /= 1 and integr /= 2),
!                             or leniw < 2 or maxp1 < 1 or
!                             lenw < leniw*2+maxp1*25.
!                             result_I, abserr, neval, last are set to
!                             zero. except when leniw, maxp1 or lenw are
!                             invalid, work(Climit*2+1), work(Climit*3+1),
!                             iwork(1), iwork(Climit+1) are set to zero,
!                             work(1) is set to a and work(Climit+1) to
!                             b.
!
!         dimensioning parameters
!            leniw  - integer
!                     dimensioning parameter for iwork.
!                     leniw/2 equals the maximum number of subintervals
!                     allowed in the partition of the given integration
!                     interval (a,b), leniw >= 2.
!                     if leniw < 2, the routine will end with ier = 6.
!
!            maxp1  - integer
!                     gives an upper bound on the number of chebyshev
!                     moments which can be stored, i.e. for the
!                     intervals of lengths abs(b-a)*2**(-l),
!                     l=0,1, ..., maxp1-2, maxp1 >= 1
!                     if maxp1 < 1, the routine will end with ier = 6.
!
!            lenw   - integer
!                     dimensioning parameter for work
!                     lenw must be at least leniw*2+maxp1*25.
!                     if lenw < (leniw*2+maxp1*25), the routine will
!                     end with ier = 6.
!
!            last   - integer
!                     on return, last equals the number of subintervals
!                     produced in the subdivision process, which
!                     determines the number of significant elements
!                     actually in the work arrays.
!
!         work arrays
!            iwork  - integer
!                     vector of dimension at least leniw
!                     on return, the first k elements of which contain
!                     pointers to the error estimates over the
!                     subintervals, such that work(Climit*3+iwork(1)), ..
!                     work(Climit*3+iwork(k)) form a decreasing
!                     sequence, with Climit = lenw/2 , and k = last
!                     if last <= (Climit/2+2), and k = Climit+1-last
!                     otherwise.
!                     furthermore, iwork(Climit+1), ..., iwork(Climit+
!                     last) indicate the subdivision levels of the
!                     subintervals, such that iwork(Climit+i) = l means
!                     that the subinterval numbered i is of length
!                     abs(b-a)*2**(1-l).
!
!            work   - real(DP) ::
!                     vector of dimension at least lenw
!                     on return
!                     work(1), ..., work(last) contain the left
!                      end points of the subintervals in the
!                      partition of (a,b),
!                     work(Climit+1), ..., work(Climit+last) contain
!                      the right end points,
!                     work(Climit*2+1), ..., work(Climit*2+last) contain
!                      the integral approximations over the
!                      subintervals,
!                     work(Climit*3+1), ..., work(Climit*3+last)
!                      contain the error estimates.
!                     work(Climit*4+1), ..., work(Climit*4+maxp1*25)
!                      provide space for storing the chebyshev moments.
!                     note that Climit = lenw/2.
!
!***references  (none)
!***routines called  dqawoe,xerror
!***end prologue  dqawo
!
       real(DP) :: a,abserr,b,epsabs,epsrel,f,omega,result_I,work
       integer(I4B) :: ier,integr,iwork,last,Climit,lenw,leniw,lvl,l1,l2,l3,l4,  &
        maxp1,momcom,neval
!
       dimension iwork(leniw),work(lenw)
!
       external f
!
!         check validity of leniw, maxp1 and lenw.
!
!***first executable statement  dqawo
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (leniw < 2.or.maxp1 < 1.or.lenw < (leniw*2+maxp1*25))        &
        go to 10
!
!         prepare call for dqawoe
!
      Climit = leniw/2
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
      l4 = Climit+l3
      call dqawoe(f,a,b,omega,integr,epsabs,epsrel,Climit,1,maxp1,result_I,&
         abserr,neval,ier,last,work(1),work(l1),work(l2),work(l3),      &
         iwork(1),iwork(l1),momcom,work(l4))
!
!         call error handler if necessary
!
      lvl = 0
   10 if (ier == 6) lvl = 0
      if (ier /= 0) call xerror('abnormal return from dqawo',26,ier,lvl)
      return
end subroutine dqawo
subroutine dqawse(f,a,b,alfa,beta,integr,epsabs,epsrel,Climit,result_I,abserr,neval,ier,alist,blist,rlist,elist,iord,last)
!*********************************************************************72
!
!c DQAWSE estimates integrals with algebraico-logarithmic endpoint singu
!
!***begin prologue  dqawse
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, special-purpose,
!             algebraico-logarithmic end point singularities,
!             clenshaw-curtis method
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f*w over (a,b),
!            (where w shows a singular behaviour at the end points,
!            see parameter integr).
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        integration of functions having algebraico-logarithmic
!        end point singularities
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration, b > a
!                     if b <= a, the routine will end with ier = 6.
!
!            alfa   - real(DP) ::
!                     parameter in the weight function, alfa > (-1)
!                     if alfa <= (-1), the routine will end with
!                     ier = 6.
!
!            beta   - real(DP) ::
!                     parameter in the weight function, beta > (-1)
!                     if beta <= (-1), the routine will end with
!                     ier = 6.
!
!            integr - integer
!                     indicates which weight function is to be used
!                     = 1  (x-a)**alfa*(b-x)**beta
!                     = 2  (x-a)**alfa*(b-x)**beta*log(x-a)
!                     = 3  (x-a)**alfa*(b-x)**beta*log(b-x)
!                     = 4  (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x)
!                     if integr < 1 or integr > 4, the routine
!                     will end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!            Climit  - integer
!                     gives an upper bound on the number of subintervals
!                     in the partition of (a,b), Climit >= 2
!                     if Climit < 2, the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for the integral and error
!                             are less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                         = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit. however, if this yields no
!                             improvement, it is advised to analyze the
!                             integrand in order to determine the
!                             integration difficulties which prevent the
!                             requested tolerance from being achieved.
!                             in case of a jump discontinuity or a local
!                             singularity of algebraico-logarithmic type
!                             at one or more interior points of the
!                             integration range, one should proceed by
!                             splitting up the interval at these
!                             points and calling the integrator on the
!                             subranges.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 the input is invalid, because
!                             b <= a or alfa <= (-1) or beta <= (-1), or
!                             integr < 1 or integr > 4, or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28),
!                             or Climit < 2.
!                             result_I, abserr, neval, rlist(1), elist(1),
!                             iord(1) and last are set to zero. alist(1)
!                             and blist(1) are set to a and b
!                             respectively.
!
!            alist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the given integration range (a,b)
!
!            blist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the given integration range (a,b)
!
!            rlist  - real(DP) ::
!                     vector of dimension at least Climit,the first
!                      last  elements of which are the integral
!                     approximations on the subintervals
!
!            elist  - real(DP) ::
!                     vector of dimension at least Climit, the first
!                      last  elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            iord   - integer
!                     vector of dimension at least Climit, the first k
!                     of which are pointers to the error
!                     estimates over the subintervals, so that
!                     elist(iord(1)), ..., elist(iord(k)) with k = last
!                     if last <= (Climit/2+2), and k = Climit+1-last
!                     otherwise form a decreasing sequence
!
!            last   - integer
!                     number of subintervals actually produced in
!                     the subdivision process
!
!***references  (none)
!***routines called  dqc25s,dqmomo,dqpsrt
!***end prologue  dqawse
!
      real(DP) :: a,abserr,alfa,alist,area,area1,area12,area2,a1,  &
        a2,b,beta,blist,b1,b2,centre,dabs,dmax1,elist,epmach,    &
        epsabs,epsrel,errbnd,errmax,error1,erro12,error2,errsum,f,      &
        resas1,resas2,result_I,rg,rh,ri,rj,rlist,uflow
      integer(I4B) :: ier,integr,iord,iroff1,iroff2,k,last,Climit,maxerr,nev,    &
        neval,nrmax
!
      external f
!
      dimension alist(Climit),blist(Climit),rlist(Climit),elist(Climit),    &
        iord(Climit),ri(25),rj(25),rh(25),rg(25)
!
!            list of major variables
!            -----------------------
!
!           alist     - list of left end points of all subintervals
!                       considered up to now
!           blist     - list of right end points of all subintervals
!                       considered up to now
!           rlist(i)  - approximation to the integral over
!                       (alist(i),blist(i))
!           elist(i)  - error estimate applying to rlist(i)
!           maxerr    - pointer to the interval with largest
!                       error estimate
!           errmax    - elist(maxerr)
!           area      - sum of the integrals over the subintervals
!           errsum    - sum of the errors over the subintervals
!           errbnd    - requested accuracy max(epsabs,epsrel*
!                       abs(result_I))
!           *****1    - variable for the left subinterval
!           *****2    - variable for the right subinterval
!           last      - index for subdivision
!
!
!            machine dependent constants
!            ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqawse
      epmach = d1mach(4)
      uflow = d1mach(1)
!
!           test on validity of parameters
!           ------------------------------
!
      ier = 6
      neval = 0
      last = 0
      rlist(1) = 0.0d+00
      elist(1) = 0.0d+00
      iord(1) = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (b <= a.or.(epsabs == 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28)).or. &
         alfa <= (-0.1d+01).or.beta <= (-0.1d+01).or.integr < 1.or.integr > 4.or.Climit < 2) go to 999
      ier = 0
!
!           compute the modified chebyshev moments.
!
      call dqmomo(alfa,beta,ri,rj,rg,rh,integr)
!
!           integrate over the intervals (a,(a+b)/2) and ((a+b)/2,b).
!
      centre = 0.5d+00*(b+a)
      call dqc25s(f,a,b,a,centre,alfa,beta,ri,rj,rg,rh,area1,           &
        error1,resas1,integr,nev)
      neval = nev
      call dqc25s(f,a,b,centre,b,alfa,beta,ri,rj,rg,rh,area2,           &
        error2,resas2,integr,nev)
      last = 2
      neval = neval+nev
      result_I = area1+area2
      abserr = error1+error2
!
!           test on accuracy.
!
      errbnd = dmax1(epsabs,epsrel*dabs(result_I))
!
!           initialization
!           --------------
!
      if (error2 > error1) go to 10
      alist(1) = a
      alist(2) = centre
      blist(1) = centre
      blist(2) = b
      rlist(1) = area1
      rlist(2) = area2
      elist(1) = error1
      elist(2) = error2
      go to 20
   10 alist(1) = centre
      alist(2) = a
      blist(1) = b
      blist(2) = centre
      rlist(1) = area2
      rlist(2) = area1
      elist(1) = error2
      elist(2) = error1
   20 iord(1) = 1
      iord(2) = 2
      if (Climit == 2) ier = 1
      if (abserr <= errbnd.or.ier == 1) go to 999
      errmax = elist(1)
      maxerr = 1
      nrmax = 1
      area = result_I
      errsum = abserr
      iroff1 = 0
      iroff2 = 0
!
!            main do-loop
!            ------------
!
      do 60 last = 3,Climit
!
!           bisect the subinterval with largest error estimate.
!
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
!
        call dqc25s(f,a,b,a1,b1,alfa,beta,ri,rj,rg,rh,area1,error1,resas1,integr,nev)
        neval = neval+nev
        call dqc25s(f,a,b,a2,b2,alfa,beta,ri,rj,rg,rh,area2,error2,resas2,integr,nev)
        neval = neval+nev
!
!           improve previous approximations integral and error
!           and test for accuracy.
!
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if (a == a1.or.b == b2) go to 30
        if (resas1 == error1.or.resas2 == error2) go to 30
!
!           test for roundoff error.
!
        if (dabs(rlist(maxerr)-area12) < 0.1d-04*dabs(area12).and.erro12 >= 0.99d+00*errmax) iroff1 = iroff1+1
        if (last > 10.and.erro12 > errmax) iroff2 = iroff2+1
   30   rlist(maxerr) = area1
        rlist(last) = area2
!
!           test on accuracy.
!
        errbnd = dmax1(epsabs,epsrel*dabs(area))
        if (errsum <= errbnd) go to 35
!
!           set error flag in the case that the number of interval
!           bisections exceeds Climit.
!
        if (last == Climit) ier = 1
!
!
!           set error flag in the case of roundoff error.
!
        if (iroff1 >= 6.or.iroff2 >= 20) ier = 2
!
!           set error flag in the case of bad integrand behaviour
!           at interior points of integration range.
!
        if (dmax1(dabs(a1),dabs(b2)) <= (0.1d+01+0.1d+03*epmach)*(dabs(a2)+0.1d+04*uflow)) ier = 3
!
!           append the newly-created intervals to the list.
!
   35   if (error2 > error1) go to 40
        alist(last) = a2
        blist(maxerr) = b1
        blist(last) = b2
        elist(maxerr) = error1
        elist(last) = error2
        go to 50
   40   alist(maxerr) = a2
        alist(last) = a1
        blist(last) = b1
        rlist(maxerr) = area2
        rlist(last) = area1
        elist(maxerr) = error2
        elist(last) = error1
!
!           call subroutine dqpsrt to maintain the descending ordering
!           in the list of error estimates and select the subinterval
!           with largest error estimate (to be bisected next).
!
   50   call dqpsrt(Climit,last,maxerr,errmax,elist,iord,nrmax)
! ***jump out of do-loop
        if (ier /= 0.or.errsum <= errbnd) go to 70
   60 continue
!
!           compute final result_I.
!           ---------------------
!
   70 result_I = 0.0d+00
      do 80 k=1,last
        result_I = result_I+rlist(k)
   80 continue
      abserr = errsum
  999 return
end subroutine dqawse
subroutine dqaws(f,a,b,alfa,beta,integr,epsabs,epsrel,result_I,&
      abserr,neval,ier,Climit,lenw,last,iwork,work)
!*********************************************************************72
!
!c DQAWS estimates integrals with algebraico-logarithmic endpoint singul
!
!***begin prologue  dqaws
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1
!***keywords  automatic integrator, special-purpose,
!             algebraico-logarithmic end-point singularities,
!             clenshaw-curtis, globally adaptive
!***author  piessens,robert,appl. math. & progr. div. -k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the routine calculates an approximation result_I to a given
!            definite integral i = integral of f*w over (a,b),
!            (where w shows a singular behaviour at the end points
!            see parameter integr).
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
!        integration of functions having algebraico-logarithmic
!        end point singularities
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!            f      - real(DP) ::
!                     function subprogram defining the integrand
!                     function f(x). the actual name for f needs to be
!                     declared e x t e r n a l in the driver program.
!
!            a      - real(DP) ::
!                     lower limit of integration
!
!            b      - real(DP) ::
!                     upper limit of integration, b > a
!                     if b <= a, the routine will end with ier = 6.
!
!            alfa   - real(DP) ::
!                     parameter in the integrand function, alfa > (-1)
!                     if alfa <= (-1), the routine will end with
!                     ier = 6.
!
!            beta   - real(DP) ::
!                     parameter in the integrand function, beta > (-1)
!                     if beta <= (-1), the routine will end with
!                     ier = 6.
!
!            integr - integer
!                     indicates which weight function is to be used
!                     = 1  (x-a)**alfa*(b-x)**beta
!                     = 2  (x-a)**alfa*(b-x)**beta*log(x-a)
!                     = 3  (x-a)**alfa*(b-x)**beta*log(b-x)
!                     = 4  (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x)
!                     if integr < 1 or integr > 4, the routine
!                     will end with ier = 6.
!
!            epsabs - real(DP) ::
!                     absolute accuracy requested
!            epsrel - real(DP) ::
!                     relative accuracy requested
!                     if  epsabs <= 0
!                     and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                     the routine will end with ier = 6.
!
!         on return
!            result_I - real(DP) ::
!                     approximation to the integral
!
!            abserr - real(DP) ::
!                     estimate of the modulus of the absolute error,
!                     which should equal or exceed abs(i-result_I)
!
!            neval  - integer
!                     number of integrand evaluations
!
!            ier    - integer
!                     ier = 0 normal and reliable termination of the
!                             routine. it is assumed that the requested
!                             accuracy has been achieved.
!                     ier > 0 abnormal termination of the routine
!                             the estimates for the integral and error
!                             are less reliable. it is assumed that the
!                             requested accuracy has not been achieved.
!            error messages
!                     ier = 1 maximum number of subdivisions allowed
!                             has been achieved. one can allow more
!                             subdivisions by increasing the value of
!                             Climit (and taking the according dimension
!                             adjustments into account). however, if
!                             this yields no improvement it is advised
!                             to analyze the integrand, in order to
!                             determine the integration difficulties
!                             which prevent the requested tolerance from
!                             being achieved. in case of a jump
!                             discontinuity or a local singularity
!                             of algebraico-logarithmic type at one or
!                             more interior points of the integration
!                             range, one should proceed by splitting up
!                             the interval at these points and calling
!                             the integrator on the subranges.
!                         = 2 the occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 the input is invalid, because
!                             b <= a or alfa <= (-1) or beta <= (-1) or
!                             or integr < 1 or integr > 4 or
!                             (epsabs <= 0 and
!                              epsrel < max(50*rel.mach.acc.,0.5d-28))
!                             or Climit < 2 or lenw < Climit*4.
!                             result_I, abserr, neval, last are set to
!                             zero. except when lenw or Climit is invalid
!                             iwork(1), work(Climit*2+1) and
!                             work(Climit*3+1) are set to zero, work(1)
!                             is set to a and work(Climit+1) to b.
!
!         dimensioning parameters
!            Climit  - integer
!                     dimensioning parameter for iwork
!                     Climit determines the maximum number of
!                     subintervals in the partition of the given
!                     integration interval (a,b), Climit >= 2.
!                     if Climit < 2, the routine will end with ier = 6.
!
!            lenw   - integer
!                     dimensioning parameter for work
!                     lenw must be at least Climit*4.
!                     if lenw < Climit*4, the routine will end
!                     with ier = 6.
!
!            last   - integer
!                     on return, last equals the number of
!                     subintervals produced in the subdivision process,
!                     which determines the significant number of
!                     elements actually in the work arrays.
!
!         work arrays
!            iwork  - integer
!                     vector of dimension Climit, the first k
!                     elements of which contain pointers
!                     to the error estimates over the subintervals,
!                     such that work(Climit*3+iwork(1)), ...,
!                     work(Climit*3+iwork(k)) form a decreasing
!                     sequence with k = last if last <= (Climit/2+2),
!                     and k = Climit+1-last otherwise
!
!            work   - real(DP) ::
!                     vector of dimension lenw
!                     on return
!                     work(1), ..., work(last) contain the left
!                      end points of the subintervals in the
!                      partition of (a,b),
!                     work(Climit+1), ..., work(Climit+last) contain
!                      the right end points,
!                     work(Climit*2+1), ..., work(Climit*2+last)
!                      contain the integral approximations over
!                      the subintervals,
!                     work(Climit*3+1), ..., work(Climit*3+last)
!                      contain the error estimates.
!
!***references  (none)
!***routines called  dqawse,xerror
!***end prologue  dqaws
!
      real(DP) :: a,abserr,alfa,b,beta,epsabs,epsrel,f,result_I,work
      integer(I4B) :: ier,integr,iwork,last,lenw,Climit,lvl,l1,l2,l3,neval
!
      dimension iwork(Climit),work(lenw)
!
      external f
!
!         check validity of Climit and lenw.
!
!***first executable statement  dqaws
      ier = 6
      neval = 0
      last = 0
      result_I = 0.0d+00
      abserr = 0.0d+00
      if (Climit < 2.or.lenw < Climit*4) go to 10
!
!         prepare call for dqawse.
!
      l1 = Climit+1
      l2 = Climit+l1
      l3 = Climit+l2
!
      call dqawse(f,a,b,alfa,beta,integr,epsabs,epsrel,Climit,result_I,abserr,neval,ier, &
                  work(1),work(l1),work(l2),work(l3),iwork,last)
!
!         call error handler if necessary.
!
      lvl = 0
   10 if (ier == 6) lvl = 1
      if (ier /= 0) call xerror('abnormal return from dqaws',26,ier,lvl)
      return
end subroutine dqaws
subroutine dqc25c(f,a,b,c,result_I,abserr,krul,neval)
!*********************************************************************72
!
!c DQC25C returns integration rules for Cauchy Principal Value integrals
!
!***begin prologue  dqc25c
!***date written   810101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a2,j4
!***keywords  25-point clenshaw-curtis integration
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f*w over (a,b) with
!            error estimate, where w(x) = 1/(x-c)
!***description
!
!        integration rules for the computation of cauchy
!        principal value integrals
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!           f      - real(DP) ::
!                    function subprogram defining the integrand function
!                    f(x). the actual name for f needs to be declared
!                    e x t e r n a l  in the driver program.
!
!           a      - real(DP) ::
!                    left end point of the integration interval
!
!           b      - real(DP) ::
!                    right end point of the integration interval, b > a
!
!           c      - real(DP) ::
!                    parameter in the weight function
!
!           result_I - real(DP) ::
!                    approximation to the integral
!                    result_I is computed by using a generalized
!                    clenshaw-curtis method if c lies within ten percent
!                    of the integration interval. in the other case the
!                    15-point kronrod rule obtained by optimal addition
!                    of abscissae to the 7-point gauss rule, is applied.
!
!           abserr - real(DP) ::
!                    estimate of the modulus of the absolute error,
!                    which should equal or exceed abs(i-result_I)
!
!           krul   - integer
!                    key which is decreased by 1 if the 15-point
!                    gauss-kronrod scheme has been used
!
!           neval  - integer
!                    number of integrand evaluations
!
!.......................................................................
!***references  (none)
!***routines called  dqcheb,dqk15w,dqwgtc
!***end prologue  dqc25c
!
      real(DP) :: a,abserr,ak22,amom0,amom1,amom2,b,c,cc,centr,cheb12,cheb24,dabs,&
                  dlog,f,fval,hlgth,p2,p3,p4,resabs,resasc,result_I,res12,res24,u,x
      integer(I4B) :: i,isym,k,kp,krul,neval
!
      dimension x(11),fval(25),cheb12(13),cheb24(25)
!
      external f
!
!           the vector x contains the values cos(k*pi/24),
!           k = 1, ..., 11, to be used for the chebyshev series
!           expansion of f
!
      data x(1) / 0.991444861373810411144557526928563d0 /
      data x(2) / 0.965925826289068286749743199728897d0 /
      data x(3) / 0.923879532511286756128183189396788d0 /
      data x(4) / 0.866025403784438646763723170752936d0 /
      data x(5) / 0.793353340291235164579776961501299d0 /
      data x(6) / 0.707106781186547524400844362104849d0 /
      data x(7) / 0.608761429008720639416097542898164d0 /
      data x(8) / 0.500000000000000000000000000000000d0 /
      data x(9) / 0.382683432365089771728459984030399d0 /
      data x(10) / 0.258819045102520762348898837624048d0 /
      data x(11) / 0.130526192220051591548406227895489d0 /
!
!           list of major variables
!           ----------------------
!           fval   - value of the function f at the points
!                    cos(k*pi/24),  k = 0, ..., 24
!           cheb12 - chebyshev series expansion coefficients,
!                    for the function f, of degree 12
!           cheb24 - chebyshev series expansion coefficients,
!                    for the function f, of degree 24
!           res12  - approximation to the integral corresponding
!                    to the use of cheb12
!           res24  - approximation to the integral corresponding
!                    to the use of cheb24
!           dqwgtc - external function subprogram defining
!                    the weight function
!           hlgth  - half-length of the interval
!           centr  - mid point of the interval
!
!
!           check the position of c.
!
!***first executable statement  dqc25c
      cc = (0.2d+01*c-b-a)/(b-a)
      if (dabs(cc) < 0.11d+01) go to 10
!
!           apply the 15-point gauss-kronrod scheme.
!
      krul = krul-1
      call dqk15w(f,dqwgtc,c,p2,p3,p4,kp,a,b,result_I,abserr,resabs,resasc)
      neval = 15
      if (resasc == abserr) krul = krul+1
      go to 50
!
!           use the generalized clenshaw-curtis method.
!
   10 hlgth = 0.5d+00*(b-a)
      centr = 0.5d+00*(b+a)
      neval = 25
      fval(1) = 0.5d+00*f(hlgth+centr)
      fval(13) = f(centr)
      fval(25) = 0.5d+00*f(centr-hlgth)
      do 20 i=2,12
        u = hlgth*x(i-1)
        isym = 26-i
        fval(i) = f(u+centr)
        fval(isym) = f(centr-u)
   20 continue
!
!           compute the chebyshev series expansion.
!
      call dqcheb(x,fval,cheb12,cheb24)
!
!           the modified chebyshev moments are computed by forward
!           recursion, using amom0 and amom1 as starting values.
!
      amom0 = dlog(dabs((0.1d+01-cc)/(0.1d+01+cc)))
      amom1 = 0.2d+01+cc*amom0
      res12 = cheb12(1)*amom0+cheb12(2)*amom1
      res24 = cheb24(1)*amom0+cheb24(2)*amom1
      do 30 k=3,13
        amom2 = 0.2d+01*cc*amom1-amom0
        ak22 = (k-2)*(k-2)
        if ((k/2)*2 == k) amom2 = amom2-0.4d+01/(ak22-0.1d+01)
        res12 = res12+cheb12(k)*amom2
        res24 = res24+cheb24(k)*amom2
        amom0 = amom1
        amom1 = amom2
   30 continue
      do 40 k=14,25
        amom2 = 0.2d+01*cc*amom1-amom0
        ak22 = (k-2)*(k-2)
        if ((k/2)*2 == k) amom2 = amom2-0.4d+01/(ak22-0.1d+01)
        res24 = res24+cheb24(k)*amom2
        amom0 = amom1
        amom1 = amom2
   40 continue
      result_I = res24
      abserr = dabs(res24-res12)
   50 return
end subroutine dqc25c
subroutine dqc25f(f,a,b,omega,integr,nrmom,maxp1,ksave,result_I,abserr,neval,resabs,resasc,momcom,chebmo)
!*********************************************************************72
!
!c DQC25F returns integration rules for functions with a COS or SIN fact
!
!***begin prologue  dqc25f
!***date written   810101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a2
!***keywords  integration rules for functions with cos or sin
!             factor, clenshaw-curtis, gauss-kronrod
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute the integral i=integral of f(x) over (a,b)
!            where w(x) = cos(omega*x) or w(x)=sin(omega*x) and to
!            compute j = integral of abs(f) over (a,b). for small value
!            of omega or small intervals (a,b) the 15-point gauss-kronro
!            rule is used. otherwise a generalized clenshaw-curtis
!            method is used.
!***description
!
!        integration rules for functions with cos or sin factor
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!         on entry
!           f      - real(DP) ::
!                    function subprogram defining the integrand
!                    function f(x). the actual name for f needs to
!                    be declared e x t e r n a l in the calling program.
!
!           a      - real(DP) ::
!                    lower limit of integration
!
!           b      - real(DP) ::
!                    upper limit of integration
!
!           omega  - real(DP) ::
!                    parameter in the weight function
!
!           integr - integer
!                    indicates which weight function is to be used
!                       integr = 1   w(x) = cos(omega*x)
!                       integr = 2   w(x) = sin(omega*x)
!
!           nrmom  - integer
!                    the length of interval (a,b) is equal to the length
!                    of the original integration interval divided by
!                    2**nrmom (we suppose that the routine is used in an
!                    adaptive integration process, otherwise set
!                    nrmom = 0). nrmom must be zero at the first call.
!
!           maxp1  - integer
!                    gives an upper bound on the number of chebyshev
!                    moments which can be stored, i.e. for the
!                    intervals of lengths abs(bb-aa)*2**(-l),
!                    l = 0,1,2, ..., maxp1-2.
!
!           ksave  - integer
!                    key which is one when the moments for the
!                    current interval have been computed
!
!         on return
!           result_I - real(DP) ::
!                    approximation to the integral i
!
!           abserr - real(DP) ::
!                    estimate of the modulus of the absolute
!                    error, which should equal or exceed abs(i-result_I)
!
!           neval  - integer
!                    number of integrand evaluations
!
!           resabs - real(DP) ::
!                    approximation to the integral j
!
!           resasc - real(DP) ::
!                    approximation to the integral of abs(f-i/(b-a))
!
!         on entry and return
!           momcom - integer
!                    for each interval length we need to compute the
!                    chebyshev moments. momcom counts the number of
!                    intervals for which these moments have already been
!                    computed. if nrmom < momcom or ksave = 1, the
!                    chebyshev moments for the interval (a,b) have
!                    already been computed and stored, otherwise we
!                    compute them and we increase momcom.
!
!           chebmo - real(DP) ::
!                    array of dimension at least (maxp1,25) containing
!                    the modified chebyshev moments for the first momcom
!                    momcom interval lengths
!
! ......................................................................
!***references  (none)
!***routines called  dgtsl,dqcheb,dqk15w,dqwgtf
!***end prologue  dqc25f
!
      real(DP) :: a,abserr,ac,an,an2,as,asap,ass,b,centr,chebmo,cheb12,cheb24,conc, &
                  cons,cospar,d,d1,d2,estc,ests,f,fval,&
                  hlgth,oflow,omega,parint,par2,par22,p2,p3,p4,resabs,resasc,resc12,resc24,ress12,ress24,result_I,sinpar,v,x
      integer(I4B) :: i,iers,integr,isym,j,k,ksave,m,momcom,neval,maxp1,noequ,noeq1,nrmom
!
      dimension chebmo(maxp1,25),cheb12(13),cheb24(25),d(25),d1(25),d2(25),fval(25),v(28),x(11)
!
      external f
!
!           the vector x contains the values cos(k*pi/24)
!           k = 1, ...,11, to be used for the chebyshev expansion of f
!
      data x(1) / 0.991444861373810411144557526928563d0 /
      data x(2) / 0.965925826289068286749743199728897d0 /
      data x(3) / 0.923879532511286756128183189396788d0 /
      data x(4) / 0.866025403784438646763723170752936d0 /
      data x(5) / 0.793353340291235164579776961501299d0 /
      data x(6) / 0.707106781186547524400844362104849d0 /
      data x(7) / 0.608761429008720639416097542898164d0 /
      data x(8) / 0.500000000000000000000000000000000d0 /
      data x(9) / 0.382683432365089771728459984030399d0 /
      data x(10) / 0.258819045102520762348898837624048d0 /
      data x(11) / 0.130526192220051591548406227895489d0 /
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the integration interval
!           hlgth  - half-length of the integration interval
!           fval   - value of the function f at the points
!                    (b-a)*0.5*cos(k*pi/12) + (b+a)*0.5, k = 0, ..., 24
!           cheb12 - coefficients of the chebyshev series expansion
!                    of degree 12, for the function f, in the
!                    interval (a,b)
!           cheb24 - coefficients of the chebyshev series expansion
!                    of degree 24, for the function f, in the
!                    interval (a,b)
!           resc12 - approximation to the integral of
!                    cos(0.5*(b-a)*omega*x)*f(0.5*(b-a)*x+0.5*(b+a))
!                    over (-1,+1), using the chebyshev series
!                    expansion of degree 12
!           resc24 - approximation to the same integral, using the
!                    chebyshev series expansion of degree 24
!           ress12 - the analogue of resc12 for the sine
!           ress24 - the analogue of resc24 for the sine
!
!
!           machine dependent constant
!           --------------------------
!
!           oflow is the largest positive magnitude.
!
!***first executable statement  dqc25f
      oflow = d1mach(2)
!
      centr = 0.5d+00*(b+a)
      hlgth = 0.5d+00*(b-a)
      parint = omega*hlgth
!
!           compute the integral using the 15-point gauss-kronrod
!           formula if the value of the parameter in the integrand
!           is small.
!
      if (dabs(parint) > 0.2d+01) go to 10
      call dqk15w(f,dqwgtf,omega,p2,p3,p4,integr,a,b,result_I,abserr,resabs,resasc)
      neval = 15
      go to 170
!
!           compute the integral using the generalized clenshaw-
!           curtis method.
!
   10 conc = hlgth*dcos(centr*omega)
      cons = hlgth*dsin(centr*omega)
      resasc = oflow
      neval = 25
!
!           check whether the chebyshev moments for this interval
!           have already been computed.
!
      if (nrmom < momcom.or.ksave == 1) go to 120
!
!           compute a new set of chebyshev moments.
!
      m = momcom+1
      par2 = parint*parint
      par22 = par2+0.2d+01
      sinpar = dsin(parint)
      cospar = dcos(parint)
!
!           compute the chebyshev moments with respect to cosine.
!
      v(1) = 0.2d+01*sinpar/parint
      v(2) = (0.8d+01*cospar+(par2+par2-0.8d+01)*sinpar/parint)/par2
      v(3) = (0.32d+02*(par2-0.12d+02)*cospar+(0.2d+01*((par2-0.80d+02)*par2+0.192d+03)*sinpar)/parint)/(par2*par2)
      ac = 0.8d+01*cospar
      as = 0.24d+02*parint*sinpar
      if (dabs(parint) > 0.24d+02) go to 30
!
!           compute the chebyshev moments as the solutions of a
!           boundary value problem with 1 initial value (v(3)) and 1
!           end value (computed using an asymptotic formula).
!
      noequ = 25
      noeq1 = noequ-1
      an = 0.6d+01
      do 20 k = 1,noeq1
        an2 = an*an
        d(k) = -0.2d+01*(an2-0.4d+01)*(par22-an2-an2)
        d2(k) = (an-0.1d+01)*(an-0.2d+01)*par2
        d1(k+1) = (an+0.3d+01)*(an+0.4d+01)*par2
        v(k+3) = as-(an2-0.4d+01)*ac
        an = an+0.2d+01
   20 continue
      an2 = an*an
      d(noequ) = -0.2d+01*(an2-0.4d+01)*(par22-an2-an2)
      v(noequ+3) = as-(an2-0.4d+01)*ac
      v(4) = v(4)-0.56d+02*par2*v(3)
      ass = parint*sinpar
      asap = (((((0.210d+03*par2-0.1d+01)*cospar-(0.105d+03*par2-0.63d+02)*ass)/ &
             an2-(0.1d+01-0.15d+02*par2)*cospar+0.15d+02*ass)/an2-cospar+0.3d+01*ass)/an2-cospar)/an2
      v(noequ+3) = v(noequ+3)-0.2d+01*asap*par2*(an-0.1d+01)*(an-0.2d+01)
!
!           solve the tridiagonal system by means of gaussian
!           elimination with partial pivoting.
!
!***        call to dgtsl must be replaced by call to
!***        real(DP) :: version of linpack routine sgtsl
!
      call dgtsl(noequ,d1,d,d2,v(4),iers)
      go to 50
!
!           compute the chebyshev moments by means of forward
!           recursion.
!
   30 an = 0.4d+01
      do 40 i = 4,13
        an2 = an*an
        v(i) = ((an2-0.4d+01)*(0.2d+01*(par22-an2-an2)*v(i-1)-ac)+as-par2*(an+0.1d+01)*(an+0.2d+01)*v(i-2))/&
               (par2*(an-0.1d+01)*(an-0.2d+01))
        an = an+0.2d+01
   40 continue
   50 do 60 j = 1,13
        chebmo(m,2*j-1) = v(j)
   60 continue
!
!           compute the chebyshev moments with respect to sine.
!
      v(1) = 0.2d+01*(sinpar-parint*cospar)/par2
      v(2) = (0.18d+02-0.48d+02/par2)*sinpar/par2+(-0.2d+01+0.48d+02/par2)*cospar/parint
      ac = -0.24d+02*parint*cospar
      as = -0.8d+01*sinpar
      if (dabs(parint) > 0.24d+02) go to 80
!
!           compute the chebyshev moments as the solutions of a boundary
!           value problem with 1 initial value (v(2)) and 1 end value
!           (computed using an asymptotic formula).
!
      an = 0.5d+01
      do 70 k = 1,noeq1
        an2 = an*an
        d(k) = -0.2d+01*(an2-0.4d+01)*(par22-an2-an2)
        d2(k) = (an-0.1d+01)*(an-0.2d+01)*par2
        d1(k+1) = (an+0.3d+01)*(an+0.4d+01)*par2
        v(k+2) = ac+(an2-0.4d+01)*as
        an = an+0.2d+01
   70 continue
      an2 = an*an
      d(noequ) = -0.2d+01*(an2-0.4d+01)*(par22-an2-an2)
      v(noequ+2) = ac+(an2-0.4d+01)*as
      v(3) = v(3)-0.42d+02*par2*v(2)
      ass = parint*cospar
      asap = (((((0.105d+03*par2-0.63d+02)*ass+(0.210d+03*par2-0.1d+01)*sinpar)/an2+(0.15d+02*par2-0.1d+01)*&
             sinpar-0.15d+02*ass)/an2-0.3d+01*ass-sinpar)/an2-sinpar)/an2
      v(noequ+2) = v(noequ+2)-0.2d+01*asap*par2*(an-0.1d+01)*(an-0.2d+01)
!
!           solve the tridiagonal system by means of gaussian
!           elimination with partial pivoting.
!
!***        call to dgtsl must be replaced by call to
!***        real(DP) :: version of linpack routine sgtsl
!
      call dgtsl(noequ,d1,d,d2,v(3),iers)
      go to 100
!
!           compute the chebyshev moments by means of forward recursion.
!
   80 an = 0.3d+01
      do 90 i = 3,12
        an2 = an*an
        v(i) = ((an2-0.4d+01)*(0.2d+01*(par22-an2-an2)*v(i-1)+as)+ac-par2*(an+0.1d+01)*(an+0.2d+01)*v(i-2))/&
               (par2*(an-0.1d+01)*(an-0.2d+01))
        an = an+0.2d+01
   90 continue
  100 do 110 j = 1,12
        chebmo(m,2*j) = v(j)
  110 continue
  120 if (nrmom < momcom) m = nrmom+1
       if (momcom < (maxp1-1).and.nrmom >= momcom) momcom = momcom+1
!
!           compute the coefficients of the chebyshev expansions
!           of degrees 12 and 24 of the function f.
!
      fval(1) = 0.5d+00*f(centr+hlgth)
      fval(13) = f(centr)
      fval(25) = 0.5d+00*f(centr-hlgth)
      do 130 i = 2,12
        isym = 26-i
        fval(i) = f(hlgth*x(i-1)+centr)
        fval(isym) = f(centr-hlgth*x(i-1))
  130 continue
      call dqcheb(x,fval,cheb12,cheb24)
!
!           compute the integral and error estimates.
!
      resc12 = cheb12(13)*chebmo(m,13)
      ress12 = 0.0d+00
      k = 11
      do 140 j = 1,6
        resc12 = resc12+cheb12(k)*chebmo(m,k)
        ress12 = ress12+cheb12(k+1)*chebmo(m,k+1)
        k = k-2
  140 continue
      resc24 = cheb24(25)*chebmo(m,25)
      ress24 = 0.0d+00
      resabs = dabs(cheb24(25))
      k = 23
      do 150 j = 1,12
        resc24 = resc24+cheb24(k)*chebmo(m,k)
        ress24 = ress24+cheb24(k+1)*chebmo(m,k+1)
        resabs = dabs(cheb24(k))+dabs(cheb24(k+1))
        k = k-2
  150 continue
      estc = dabs(resc24-resc12)
      ests = dabs(ress24-ress12)
      resabs = resabs*dabs(hlgth)
      if (integr == 2) go to 160
      result_I = conc*resc24-cons*ress24
      abserr = dabs(conc*estc)+dabs(cons*ests)
      go to 170
  160 result_I = conc*ress24+cons*resc24
      abserr = dabs(conc*ests)+dabs(cons*estc)
  170 return
end subroutine dqc25f
subroutine dqc25s(f,a,b,bl,br,alfa,beta,ri,rj,rg,rh,result_I,abserr,resasc,integr,nev)
!*********************************************************************72
!
!c DQC25S returns rules for algebraico-logarithmic end point singulariti
!
!***begin prologue  dqc25s
!***date written   810101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a2
!***keywords  25-point clenshaw-curtis integration
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f*w over (bl,br), with error
!            estimate, where the weight function w has a singular
!            behaviour of algebraico-logarithmic type at the points
!            a and/or b. (bl,br) is a part of (a,b).
!***description
!
!        integration rules for integrands having algebraico-logarithmic
!        end point singularities
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!           f      - real(DP) ::
!                    function subprogram defining the integrand
!                    f(x). the actual name for f needs to be declared
!                    e x t e r n a l  in the driver program.
!
!           a      - real(DP) ::
!                    left end point of the original interval
!
!           b      - real(DP) ::
!                    right end point of the original interval, b > a
!
!           bl     - real(DP) ::
!                    lower limit of integration, bl >= a
!
!           br     - real(DP) ::
!                    upper limit of integration, br <= b
!
!           alfa   - real(DP) ::
!                    parameter in the weight function
!
!           beta   - real(DP) ::
!                    parameter in the weight function
!
!           ri,rj,rg,rh - real(DP) ::
!                    modified chebyshev moments for the application
!                    of the generalized clenshaw-curtis
!                    method (computed in subroutine dqmomo)
!
!           result_I - real(DP) ::
!                    approximation to the integral
!                    result_I is computed by using a generalized
!                    clenshaw-curtis method if b1 = a or br = b.
!                    in all other cases the 15-point kronrod
!                    rule is applied, obtained by optimal addition of
!                    abscissae to the 7-point gauss rule.
!
!           abserr - real(DP) ::
!                    estimate of the modulus of the absolute error,
!                    which should equal or exceed abs(i-result_I)
!
!           resasc - real(DP) ::
!                    approximation to the integral of abs(f*w-i/(b-a))
!
!           integr - integer
!                    which determines the weight function
!                    = 1   w(x) = (x-a)**alfa*(b-x)**beta
!                    = 2   w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)
!                    = 3   w(x) = (x-a)**alfa*(b-x)**beta*log(b-x)
!                    = 4   w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)*
!                                 log(b-x)
!
!           nev    - integer
!                    number of integrand evaluations
!***references  (none)
!***routines called  dqcheb,dqk15w
!***end prologue  dqc25s
!
      real(DP) :: a,abserr,alfa,b,beta,bl,br,centr,cheb12,cheb24,dabs,dc,dlog,f,factor,&
                  fix,fval,hlgth,resabs,resasc,result_I,res12,  res24,rg,rh,ri,rj,u,x
      integer(I4B) :: i,integr,isym,nev
!
      dimension cheb12(13),cheb24(25),fval(25),rg(25),rh(25),ri(25),rj(25),x(11)
!
      external f
!
!           the vector x contains the values cos(k*pi/24)
!           k = 1, ..., 11, to be used for the computation of the
!           chebyshev series expansion of f.
!
      data x(1) / 0.991444861373810411144557526928563d0 /
      data x(2) / 0.965925826289068286749743199728897d0 /
      data x(3) / 0.923879532511286756128183189396788d0 /
      data x(4) / 0.866025403784438646763723170752936d0 /
      data x(5) / 0.793353340291235164579776961501299d0 /
      data x(6) / 0.707106781186547524400844362104849d0 /
      data x(7) / 0.608761429008720639416097542898164d0 /
      data x(8) / 0.500000000000000000000000000000000d0 /
      data x(9) / 0.382683432365089771728459984030399d0 /
      data x(10) / 0.258819045102520762348898837624048d0 /
      data x(11) / 0.130526192220051591548406227895489d0 /
!
!           list of major variables
!           -----------------------
!
!           fval   - value of the function f at the points
!                    (br-bl)*0.5*cos(k*pi/24)+(br+bl)*0.5
!                    k = 0, ..., 24
!           cheb12 - coefficients of the chebyshev series expansion
!                    of degree 12, for the function f, in the
!                    interval (bl,br)
!           cheb24 - coefficients of the chebyshev series expansion
!                    of degree 24, for the function f, in the
!                    interval (bl,br)
!           res12  - approximation to the integral obtained from cheb12
!           res24  - approximation to the integral obtained from cheb24
!           dqwgts - external function subprogram defining
!                    the four possible weight functions
!           hlgth  - half-length of the interval (bl,br)
!           centr  - mid point of the interval (bl,br)
!
!***first executable statement  dqc25s
      nev = 25
      if (bl == a.and.(alfa /= 0.0d+00.or.integr == 2.or.integr == 4))go to 10
      if (br == b.and.(beta /= 0.0d+00.or.integr == 3.or.integr == 4))go to 140
!
!           if a > bl and b < br, apply the 15-point gauss-kronrod
!           scheme.
!
!
      call dqk15w(f,dqwgts,a,b,alfa,beta,integr,bl,br,result_I,abserr,resabs,resasc)
      nev = 15
      go to 270
!
!           this part of the program is executed only if a = bl.
!           ----------------------------------------------------
!
!           compute the chebyshev series expansion of the
!           following function
!           f1 = (0.5*(b+b-br-a)-0.5*(br-a)*x)**beta
!                  *f(0.5*(br-a)*x+0.5*(br+a))
!
   10 hlgth = 0.5d+00*(br-bl)
      centr = 0.5d+00*(br+bl)
      fix = b-centr
      fval(1) = 0.5d+00*f(hlgth+centr)*(fix-hlgth)**beta
      fval(13) = f(centr)*(fix**beta)
      fval(25) = 0.5d+00*f(centr-hlgth)*(fix+hlgth)**beta
      do 20 i=2,12
        u = hlgth*x(i-1)
        isym = 26-i
        fval(i) = f(u+centr)*(fix-u)**beta
        fval(isym) = f(centr-u)*(fix+u)**beta
   20 continue
      factor = hlgth**(alfa+0.1d+01)
      result_I = 0.0d+00
      abserr = 0.0d+00
      res12 = 0.0d+00
      res24 = 0.0d+00
      if (integr > 2) go to 70
      call dqcheb(x,fval,cheb12,cheb24)
!
!           integr = 1  (or 2)
!
      do 30 i=1,13
        res12 = res12+cheb12(i)*ri(i)
        res24 = res24+cheb24(i)*ri(i)
   30 continue
      do 40 i=14,25
        res24 = res24+cheb24(i)*ri(i)
   40 continue
      if (integr == 1) go to 130
!
!           integr = 2
!
      dc = dlog(br-bl)
      result_I = res24*dc
      abserr = dabs((res24-res12)*dc)
      res12 = 0.0d+00
      res24 = 0.0d+00
      do 50 i=1,13
        res12 = res12+cheb12(i)*rg(i)
        res24 = res12+cheb24(i)*rg(i)
   50 continue
      do 60 i=14,25
        res24 = res24+cheb24(i)*rg(i)
   60 continue
      go to 130
!
!           compute the chebyshev series expansion of the
!           following function
!           f4 = f1*log(0.5*(b+b-br-a)-0.5*(br-a)*x)
!
   70 fval(1) = fval(1)*dlog(fix-hlgth)
      fval(13) = fval(13)*dlog(fix)
      fval(25) = fval(25)*dlog(fix+hlgth)
      do 80 i=2,12
        u = hlgth*x(i-1)
        isym = 26-i
        fval(i) = fval(i)*dlog(fix-u)
        fval(isym) = fval(isym)*dlog(fix+u)
   80 continue
      call dqcheb(x,fval,cheb12,cheb24)
!
!           integr = 3  (or 4)
!
      do 90 i=1,13
        res12 = res12+cheb12(i)*ri(i)
        res24 = res24+cheb24(i)*ri(i)
   90 continue
      do 100 i=14,25
        res24 = res24+cheb24(i)*ri(i)
  100 continue
      if (integr == 3) go to 130
!
!           integr = 4
!
      dc = dlog(br-bl)
      result_I = res24*dc
      abserr = dabs((res24-res12)*dc)
      res12 = 0.0d+00
      res24 = 0.0d+00
      do 110 i=1,13
        res12 = res12+cheb12(i)*rg(i)
        res24 = res24+cheb24(i)*rg(i)
  110 continue
      do 120 i=14,25
        res24 = res24+cheb24(i)*rg(i)
  120 continue
  130 result_I = (result_I+res24)*factor
      abserr = (abserr+dabs(res24-res12))*factor
      go to 270
!
!           this part of the program is executed only if b = br.
!           ----------------------------------------------------
!
!           compute the chebyshev series expansion of the
!           following function
!           f2 = (0.5*(b+bl-a-a)+0.5*(b-bl)*x)**alfa
!                *f(0.5*(b-bl)*x+0.5*(b+bl))
!
  140 hlgth = 0.5d+00*(br-bl)
      centr = 0.5d+00*(br+bl)
      fix = centr-a
      fval(1) = 0.5d+00*f(hlgth+centr)*(fix+hlgth)**alfa
      fval(13) = f(centr)*(fix**alfa)
      fval(25) = 0.5d+00*f(centr-hlgth)*(fix-hlgth)**alfa
      do 150 i=2,12
        u = hlgth*x(i-1)
        isym = 26-i
        fval(i) = f(u+centr)*(fix+u)**alfa
        fval(isym) = f(centr-u)*(fix-u)**alfa
  150 continue
      factor = hlgth**(beta+0.1d+01)
      result_I = 0.0d+00
      abserr = 0.0d+00
      res12 = 0.0d+00
      res24 = 0.0d+00
      if (integr == 2.or.integr == 4) go to 200
!
!           integr = 1  (or 3)
!
      call dqcheb(x,fval,cheb12,cheb24)
      do 160 i=1,13
        res12 = res12+cheb12(i)*rj(i)
        res24 = res24+cheb24(i)*rj(i)
  160 continue
      do 170 i=14,25
        res24 = res24+cheb24(i)*rj(i)
  170 continue
      if (integr == 1) go to 260
!
!           integr = 3
!
      dc = dlog(br-bl)
      result_I = res24*dc
      abserr = dabs((res24-res12)*dc)
      res12 = 0.0d+00
      res24 = 0.0d+00
      do 180 i=1,13
        res12 = res12+cheb12(i)*rh(i)
        res24 = res24+cheb24(i)*rh(i)
  180 continue
      do 190 i=14,25
        res24 = res24+cheb24(i)*rh(i)
  190 continue
      go to 260
!
!           compute the chebyshev series expansion of the
!           following function
!           f3 = f2*log(0.5*(b-bl)*x+0.5*(b+bl-a-a))
!
  200 fval(1) = fval(1)*dlog(hlgth+fix)
      fval(13) = fval(13)*dlog(fix)
      fval(25) = fval(25)*dlog(fix-hlgth)
      do 210 i=2,12
        u = hlgth*x(i-1)
        isym = 26-i
        fval(i) = fval(i)*dlog(u+fix)
        fval(isym) = fval(isym)*dlog(fix-u)
  210 continue
      call dqcheb(x,fval,cheb12,cheb24)
!
!           integr = 2  (or 4)
!
      do 220 i=1,13
        res12 = res12+cheb12(i)*rj(i)
        res24 = res24+cheb24(i)*rj(i)
  220 continue
      do 230 i=14,25
        res24 = res24+cheb24(i)*rj(i)
  230 continue
      if (integr == 2) go to 260
      dc = dlog(br-bl)
      result_I = res24*dc
      abserr = dabs((res24-res12)*dc)
      res12 = 0.0d+00
      res24 = 0.0d+00
!
!           integr = 4
!
      do 240 i=1,13
        res12 = res12+cheb12(i)*rh(i)
        res24 = res24+cheb24(i)*rh(i)
  240 continue
      do 250 i=14,25
        res24 = res24+cheb24(i)*rh(i)
  250 continue
  260 result_I = (result_I+res24)*factor
      abserr = (abserr+dabs(res24-res12))*factor
  270 return
end subroutine dqc25s
subroutine dqcheb(x,fval,cheb12,cheb24)
!*********************************************************************72
!
!c DQCHEB computes the Chebyshev series expansion.
!
!***begin prologue  dqcheb
!***refer to  dqc25c,dqc25f,dqc25s
!***routines called  (none)
!***revision date  830518   (yymmdd)
!***keywords  chebyshev series expansion, fast fourier transform
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  this routine computes the chebyshev series expansion
!            of degrees 12 and 24 of a function using a
!            fast fourier transform method
!            f(x) = sum(k=1,..,13) (cheb12(k)*t(k-1,x)),
!            f(x) = sum(k=1,..,25) (cheb24(k)*t(k-1,x)),
!            where t(k,x) is the chebyshev polynomial of degree k.
!***description
!
!        chebyshev series expansion
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!          on entry
!           x      - real(DP) ::
!                    vector of dimension 11 containing the
!                    values cos(k*pi/24), k = 1, ..., 11
!
!           fval   - real(DP) ::
!                    vector of dimension 25 containing the
!                    function values at the points
!                    (b+a+(b-a)*cos(k*pi/24))/2, k = 0, ...,24,
!                    where (a,b) is the approximation interval.
!                    fval(1) and fval(25) are divided by two
!                    (these values are destroyed at output).
!
!          on return
!           cheb12 - real(DP) ::
!                    vector of dimension 13 containing the
!                    chebyshev coefficients for degree 12
!
!           cheb24 - real(DP) ::
!                    vector of dimension 25 containing the
!                    chebyshev coefficients for degree 24
!
!***end prologue  dqcheb
!
      real(DP) :: alam,alam1,alam2,cheb12,cheb24,fval,part1,part2,part3,v,x
      integer(I4B) :: i,j
!
      dimension cheb12(13),cheb24(25),fval(25),v(12),x(11)
!
!***first executable statement  dqcheb
      do 10 i=1,12
        j = 26-i
        v(i) = fval(i)-fval(j)
        fval(i) = fval(i)+fval(j)
   10 continue
      alam1 = v(1)-v(9)
      alam2 = x(6)*(v(3)-v(7)-v(11))
      cheb12(4) = alam1+alam2
      cheb12(10) = alam1-alam2
      alam1 = v(2)-v(8)-v(10)
      alam2 = v(4)-v(6)-v(12)
      alam = x(3)*alam1+x(9)*alam2
      cheb24(4) = cheb12(4)+alam
      cheb24(22) = cheb12(4)-alam
      alam = x(9)*alam1-x(3)*alam2
      cheb24(10) = cheb12(10)+alam
      cheb24(16) = cheb12(10)-alam
      part1 = x(4)*v(5)
      part2 = x(8)*v(9)
      part3 = x(6)*v(7)
      alam1 = v(1)+part1+part2
      alam2 = x(2)*v(3)+part3+x(10)*v(11)
      cheb12(2) = alam1+alam2
      cheb12(12) = alam1-alam2
      alam = x(1)*v(2)+x(3)*v(4)+x(5)*v(6)+x(7)*v(8)+x(9)*v(10)+x(11)*v(12)
      cheb24(2) = cheb12(2)+alam
      cheb24(24) = cheb12(2)-alam
      alam = x(11)*v(2)-x(9)*v(4)+x(7)*v(6)-x(5)*v(8)+x(3)*v(10)-x(1)*v(12)
      cheb24(12) = cheb12(12)+alam
      cheb24(14) = cheb12(12)-alam
      alam1 = v(1)-part1+part2
      alam2 = x(10)*v(3)-part3+x(2)*v(11)
      cheb12(6) = alam1+alam2
      cheb12(8) = alam1-alam2
      alam = x(5)*v(2)-x(9)*v(4)-x(1)*v(6)-x(11)*v(8)+x(3)*v(10)+x(7)*v(12)
      cheb24(6) = cheb12(6)+alam
      cheb24(20) = cheb12(6)-alam
      alam = x(7)*v(2)-x(3)*v(4)-x(11)*v(6)+x(1)*v(8)-x(9)*v(10)-x(5)*v(12)
      cheb24(8) = cheb12(8)+alam
      cheb24(18) = cheb12(8)-alam
      do 20 i=1,6
        j = 14-i
        v(i) = fval(i)-fval(j)
        fval(i) = fval(i)+fval(j)
   20 continue
      alam1 = v(1)+x(8)*v(5)
      alam2 = x(4)*v(3)
      cheb12(3) = alam1+alam2
      cheb12(11) = alam1-alam2
      cheb12(7) = v(1)-v(5)
      alam = x(2)*v(2)+x(6)*v(4)+x(10)*v(6)
      cheb24(3) = cheb12(3)+alam
      cheb24(23) = cheb12(3)-alam
      alam = x(6)*(v(2)-v(4)-v(6))
      cheb24(7) = cheb12(7)+alam
      cheb24(19) = cheb12(7)-alam
      alam = x(10)*v(2)-x(6)*v(4)+x(2)*v(6)
      cheb24(11) = cheb12(11)+alam
      cheb24(15) = cheb12(11)-alam
      do 30 i=1,3
        j = 8-i
        v(i) = fval(i)-fval(j)
        fval(i) = fval(i)+fval(j)
   30 continue
      cheb12(5) = v(1)+x(8)*v(3)
      cheb12(9) = fval(1)-x(8)*fval(3)
      alam = x(4)*v(2)
      cheb24(5) = cheb12(5)+alam
      cheb24(21) = cheb12(5)-alam
      alam = x(8)*fval(2)-fval(4)
      cheb24(9) = cheb12(9)+alam
      cheb24(17) = cheb12(9)-alam
      cheb12(1) = fval(1)+fval(3)
      alam = fval(2)+fval(4)
      cheb24(1) = cheb12(1)+alam
      cheb24(25) = cheb12(1)-alam
      cheb12(13) = v(1)-v(3)
      cheb24(13) = cheb12(13)
      alam = 0.1d+01/0.6d+01
      do 40 i=2,12
        cheb12(i) = cheb12(i)*alam
   40 continue
      alam = 0.5d+00*alam
      cheb12(1) = cheb12(1)*alam
      cheb12(13) = cheb12(13)*alam
      do 50 i=2,24
        cheb24(i) = cheb24(i)*alam
   50 continue
      cheb24(1) = 0.5d+00*alam*cheb24(1)
      cheb24(25) = 0.5d+00*alam*cheb24(25)
      return
end subroutine dqcheb
subroutine dqelg(n,epstab,result_I,abserr,res3la,nres)
!*********************************************************************72
!
!c DQELG carries out the Epsilon extrapolation algorithm.
!
!***begin prologue  dqelg
!***refer to  dqagie,dqagoe,dqagpe,dqagse
!***revision date  830518   (yymmdd)
!***keywords  epsilon algorithm, convergence acceleration,
!             extrapolation
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math & progr. div. - k.u.leuven
!***purpose  the routine determines the Climit of a given sequence of
!            approximations, by means of the epsilon algorithm of
!            p.wynn. an estimate of the absolute error is also given.
!            the condensed epsilon table is computed. only those
!            elements needed for the computation of the next diagonal
!            are preserved.
!***description
!
!           epsilon algorithm
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!              n      - integer
!                       epstab(n) contains the new element in the
!                       first column of the epsilon table.
!
!              epstab - real(DP) ::
!                       vector of dimension 52 containing the elements
!                       of the two lower diagonals of the triangular
!                       epsilon table. the elements are numbered
!                       starting at the right-hand corner of the
!                       triangle.
!
!              result_I - real(DP) ::
!                       result_Iing approximation to the integral
!
!              abserr - real(DP) ::
!                       estimate of the absolute error computed from
!                       result_I and the 3 previous result_Is
!
!              res3la - real(DP) ::
!                       vector of dimension 3 containing the last 3
!                       result_Is
!
!              nres   - integer
!                       number of calls to the routine
!                       (should be zero at first call)
!
!***end prologue  dqelg
!
      real(DP) :: abserr,dabs,delta1,delta2,delta3,dmax1,epmach,epsinf,epstab,&
                  error,err1,err2,err3,e0,e1,e1abs,e2,e3,oflow,res,result_I,res3la,ss,tol1,tol2,tol3
      integer(I4B) :: i,ib,ib2,ie,indx,k1,k2,k3,limexp,n,newelm,nres,num
      dimension epstab(52),res3la(3)
!
!           list of major variables
!           -----------------------
!
!           e0     - the 4 elements on which the computation of a new
!           e1       element in the epsilon table is based
!           e2
!           e3                 e0
!                        e3    e1    new
!                              e2
!           newelm - number of elements to be computed in the new
!                    diagonal
!           error  - error = abs(e1-e0)+abs(e2-e1)+abs(new-e2)
!           result_I - the element in the new diagonal with least value
!                    of error
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           oflow is the largest positive magnitude.
!           limexp is the maximum number of elements the epsilon
!           table can contain. if this number is reached, the upper
!           diagonal of the epsilon table is deleted.
!
!***first executable statement  dqelg
      epmach = d1mach(4)
      oflow = d1mach(2)
      nres = nres+1
      abserr = oflow
      result_I = epstab(n)
      if (n < 3) go to 100
      limexp = 50
      epstab(n+2) = epstab(n)
      newelm = (n-1)/2
      epstab(n) = oflow
      num = n
      k1 = n
      do 40 i = 1,newelm
        k2 = k1-1
        k3 = k1-2
        res = epstab(k1+2)
        e0 = epstab(k3)
        e1 = epstab(k2)
        e2 = res
        e1abs = dabs(e1)
        delta2 = e2-e1
        err2 = dabs(delta2)
        tol2 = dmax1(dabs(e2),e1abs)*epmach
        delta3 = e1-e0
        err3 = dabs(delta3)
        tol3 = dmax1(e1abs,dabs(e0))*epmach
        if (err2 > tol2.or.err3 > tol3) go to 10
!
!           if e0, e1 and e2 are equal to within machine
!           accuracy, convergence is assumed.
!           result_I = e2
!           abserr = abs(e1-e0)+abs(e2-e1)
!
        result_I = res
        abserr = err2+err3
! ***jump out of do-loop
        go to 100
   10   e3 = epstab(k1)
        epstab(k1) = e1
        delta1 = e1-e3
        err1 = dabs(delta1)
        tol1 = dmax1(e1abs,dabs(e3))*epmach
!
!           if two elements are very close to each other, omit
!           a part of the table by adjusting the value of n
!
        if (err1 <= tol1.or.err2 <= tol2.or.err3 <= tol3) go to 20
        ss = 0.1d+01/delta1+0.1d+01/delta2-0.1d+01/delta3
        epsinf = dabs(ss*e1)
!
!           test to detect irregular behaviour in the table, and
!           eventually omit a part of the table adjusting the value
!           of n.
!
        if (epsinf > 0.1d-03) go to 30
   20   n = i+i-1
! ***jump out of do-loop
        go to 50
!
!           compute a new element and eventually adjust
!           the value of result_I.
!
   30   res = e1+0.1d+01/ss
        epstab(k1) = res
        k1 = k1-2
        error = err2+dabs(res-e2)+err3
        if (error > abserr) go to 40
        abserr = error
        result_I = res
   40 continue
!
!           shift the table.
!
   50 if (n == limexp) n = 2*(limexp/2)-1
      ib = 1
      if ((num/2)*2 == num) ib = 2
      ie = newelm+1
      do 60 i=1,ie
        ib2 = ib+2
        epstab(ib) = epstab(ib2)
        ib = ib2
   60 continue
      if (num == n) go to 80
      indx = num-n+1
      do 70 i = 1,n
        epstab(i)= epstab(indx)
        indx = indx+1
   70 continue
   80 if (nres >= 4) go to 90
      res3la(nres) = result_I
      abserr = oflow
      go to 100
!
!           compute error estimate
!
   90 abserr = dabs(result_I-res3la(3))+dabs(result_I-res3la(2))+dabs(result_I-res3la(1))
      res3la(1) = res3la(2)
      res3la(2) = res3la(3)
      res3la(3) = result_I
  100 abserr = dmax1(abserr,0.5d+01*epmach*dabs(result_I))
      return
end subroutine dqelg
subroutine dqk15(f,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK15 carries out a 15 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk15
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  15-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 15-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the7-point gauss rule(resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***end prologue  dqk15
!
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,&
                  fval2,fv1,fv2,hlgth,resabs,resasc,  resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(7),fv2(7),wg(4),wgk(8),xgk(8)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 15-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 7-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 7-point gauss rule
!
!           wgk    - weights of the 15-point kronrod rule
!
!           wg     - weights of the 7-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.129484966168869693270611432679082d0 /
      data wg  (  2) / 0.279705391489276667901467771423780d0 /
      data wg  (  3) / 0.381830050505118944950369775488975d0 /
      data wg  (  4) / 0.417959183673469387755102040816327d0 /
!
      data xgk (  1) / 0.991455371120812639206854697526329d0 /
      data xgk (  2) / 0.949107912342758524526189684047851d0 /
      data xgk (  3) / 0.864864423359769072789712788640926d0 /
      data xgk (  4) / 0.741531185599394439863864773280788d0 /
      data xgk (  5) / 0.586087235467691130294144838258730d0 /
      data xgk (  6) / 0.405845151377397166906606412076961d0 /
      data xgk (  7) / 0.207784955007898467600689403773245d0 /
      data xgk (  8) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.022935322010529224963732008058970d0 /
      data wgk (  2) / 0.063092092629978553290700663189204d0 /
      data wgk (  3) / 0.104790010322250183839876322541518d0 /
      data wgk (  4) / 0.140653259715525918745189590510238d0 /
      data wgk (  5) / 0.169004726639267902826583426598550d0 /
      data wgk (  6) / 0.190350578064785409913256402421014d0 /
      data wgk (  7) / 0.204432940075298892414161999234649d0 /
      data wgk (  8) / 0.209482141084727828012999174891714d0 /
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result_I of the 7-point gauss formula
!           resk   - result_I of the 15-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk15
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 15-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      fc = f(centr)
      resg = fc*wg(4)
      resk = fc*wgk(8)
      resabs = dabs(resk)
      do 10 j=1,3
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,4
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(8)*dabs(fc-reskh)
      do 20 j=1,7
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk15
subroutine dqk15i(f,boun,inf,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK15I applies a 15 point Gauss-Kronrod quadrature on an infinite int
!
!***begin prologue  dqk15i
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a3a2,h2a4a2
!***keywords  15-point transformed gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  the original (infinite integration range is mapped
!            onto the interval (0,1) and (a,b) is a part of (0,1).
!            it is the purpose to compute
!            i = integral of transformed integrand over (a,b),
!            j = integral of abs(transformed integrand) over (a,b).
!***description
!
!           integration rule
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       fuction subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              boun   - real(DP) ::
!                       finite bound of original integration
!                       range (set to zero if inf = +2)
!
!              inf    - integer
!                       if inf = -1, the original interval is
!                                   (-infinity,bound),
!                       if inf = +1, the original interval is
!                                   (bound,+infinity),
!                       if inf = +2, the original interval is
!                                   (-infinity,+infinity) and
!                       the integral is computed as the sum of two
!                       integrals, one over (-infinity,0) and one over
!                       (0,+infinity).
!
!              a      - real(DP) ::
!                       lower limit for integration over subrange
!                       of (0,1)
!
!              b      - real(DP) ::
!                       upper limit for integration over subrange
!                       of (0,1)
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 15-point
!                       kronrod rule(resk) obtained by optimal addition
!                       of abscissae to the 7-point gauss rule(resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should equal or exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integral of
!                       abs((transformed integrand)-i/(b-a)) over (a,b)
!
!***references  (none)
!***end prologue  dqk15i
!
      real(DP) :: a,absc,absc1,absc2,abserr,b,boun,centr,dabs,dinf,&
                  dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth,resabs,resasc,resg,resk,reskh,&
                  result_I,tabsc1,tabsc2,uflow,wg,wgk,xgk
      integer(I4B) :: inf,j
      external f
!
      dimension fv1(7),fv2(7),xgk(8),wgk(8),wg(8)
!
!           the abscissae and weights are supplied for the interval
!           (-1,1).  because of symmetry only the positive abscissae and
!           their corresponding weights are given.
!
!           xgk    - abscissae of the 15-point kronrod rule
!                    xgk(2), xgk(4), ... abscissae of the 7-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 7-point gauss rule
!
!           wgk    - weights of the 15-point kronrod rule
!
!           wg     - weights of the 7-point gauss rule, corresponding
!                    to the abscissae xgk(2), xgk(4), ...
!                    wg(1), wg(3), ... are set to zero.
!
      data wg(1) / 0.0d0 /
      data wg(2) / 0.129484966168869693270611432679082d0 /
      data wg(3) / 0.0d0 /
      data wg(4) / 0.279705391489276667901467771423780d0 /
      data wg(5) / 0.0d0 /
      data wg(6) / 0.381830050505118944950369775488975d0 /
      data wg(7) / 0.0d0 /
      data wg(8) / 0.417959183673469387755102040816327d0 /
!
      data xgk(1) / 0.991455371120812639206854697526329d0 /
      data xgk(2) / 0.949107912342758524526189684047851d0 /
      data xgk(3) / 0.864864423359769072789712788640926d0 /
      data xgk(4) / 0.741531185599394439863864773280788d0 /
      data xgk(5) / 0.586087235467691130294144838258730d0 /
      data xgk(6) / 0.405845151377397166906606412076961d0 /
      data xgk(7) / 0.207784955007898467600689403773245d0 /
      data xgk(8) / 0.000000000000000000000000000000000d0 /
!
      data wgk(1) / 0.022935322010529224963732008058970d0 /
      data wgk(2) / 0.063092092629978553290700663189204d0 /
      data wgk(3) / 0.104790010322250183839876322541518d0 /
      data wgk(4) / 0.140653259715525918745189590510238d0 /
      data wgk(5) / 0.169004726639267902826583426598550d0 /
      data wgk(6) / 0.190350578064785409913256402421014d0 /
      data wgk(7) / 0.204432940075298892414161999234649d0 /
      data wgk(8) / 0.209482141084727828012999174891714d0 /
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc*  - abscissa
!           tabsc* - transformed abscissa
!           fval*  - function value
!           resg   - result_I of the 7-point gauss formula
!           resk   - result_I of the 15-point kronrod formula
!           reskh  - approximation to the mean value of the transformed
!                    integrand over (a,b), i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk15i
      epmach = d1mach(4)
      uflow = d1mach(1)
      dinf = min0(1,inf)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      tabsc1 = boun+dinf*(0.1d+01-centr)/centr
      fval1 = f(tabsc1)
      if (inf == 2) fval1 = fval1+f(-tabsc1)
      fc = (fval1/centr)/centr
!
!           compute the 15-point kronrod approximation to
!           the integral, and estimate the error.
!
      resg = wg(8)*fc
      resk = wgk(8)*fc
      resabs = dabs(resk)
      do 10 j=1,7
        absc = hlgth*xgk(j)
        absc1 = centr-absc
        absc2 = centr+absc
        tabsc1 = boun+dinf*(0.1d+01-absc1)/absc1
        tabsc2 = boun+dinf*(0.1d+01-absc2)/absc2
        fval1 = f(tabsc1)
        fval2 = f(tabsc2)
        if (inf == 2) fval1 = fval1+f(-tabsc1)
        if (inf == 2) fval2 = fval2+f(-tabsc2)
        fval1 = (fval1/absc1)/absc1
        fval2 = (fval2/absc2)/absc2
        fv1(j) = fval1
        fv2(j) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(j)*fsum
        resabs = resabs+wgk(j)*(dabs(fval1)+dabs(fval2))
   10 continue
      reskh = resk*0.5d+00
      resasc = wgk(8)*dabs(fc-reskh)
      do 20 j=1,7
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resasc = resasc*hlgth
      resabs = resabs*hlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.d0) abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk15i
!*********************************************************************72
subroutine dqk15w(f,w,p1,p2,p3,p4,kp,a,b,result_I,abserr,resabs,resasc)
!
!c DQK15W applies a 15 point Gauss-Kronrod rule for a weighted integrand
!
!***begin prologue  dqk15w
!***date written   810101   (yymmdd)
!***revision date  830518   (mmddyy)
!***category no.  h2a2a2
!***keywords  15-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f*w over (a,b), with error
!                           estimate
!                       j = integral of abs(f*w) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!             on entry
!              f      - real(DP) ::
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the driver program.
!
!              w      - real(DP) ::
!                       function subprogram defining the integrand
!                       weight function w(x). the actual name for w
!                       needs to be declared e x t e r n a l in the
!                       calling program.
!
!              p1, p2, p3, p4 - real(DP) ::
!                       parameters in the weight function
!
!              kp     - integer
!                       key for indicating the type of weight function
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 15-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 7-point gauss rule (resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should equal or exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral of abs(f)
!
!              resasc - real(DP) ::
!                       approximation to the integral of abs(f-i/(b-a))
!
!
!***references  (none)
!***end prologue  dqk15w
!
      real(DP) :: a,absc,absc1,absc2,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,&
                  epmach,f,fc,fsum,fval1,fval2,fv1,fv2,hlgth,p1,p2,p3,p4,resabs,resasc,&
                  resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1,kp
      external f

  INTERFACE
  real(DP) function w(xx,r1,r2,r3,r4,ii1)
      use nano_deftyp
      real(DP) :: xx,r1,r2,r3,r4
      integer(I4B) :: ii1
  end function w
  END INTERFACE
!
      dimension fv1(7),fv2(7),xgk(8),wgk(8),wg(4)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 15-point gauss-kronrod rule
!                    xgk(2), xgk(4), ... abscissae of the 7-point
!                    gauss rule
!                    xgk(1), xgk(3), ... abscissae which are optimally
!                    added to the 7-point gauss rule
!
!           wgk    - weights of the 15-point gauss-kronrod rule
!
!           wg     - weights of the 7-point gauss rule
!
      data xgk(1),xgk(2),xgk(3),xgk(4),xgk(5),xgk(6),xgk(7),xgk(8)/0.9914553711208126d+00,&
      0.9491079123427585d+00,0.8648644233597691d+00,     0.7415311855993944d+00,0.5860872354676911d+00,&
      0.4058451513773972d+00,0.2077849550078985d+00,     0.0000000000000000d+00/
!
      data wgk(1),wgk(2),wgk(3),wgk(4),wgk(5),wgk(6),wgk(7),wgk(8)/0.2293532201052922d-01,&
      0.6309209262997855d-01,0.1047900103222502d+00,     0.1406532597155259d+00,0.1690047266392679d+00,&
      0.1903505780647854d+00,0.2044329400752989d+00,     0.2094821410847278d+00/
!
      data wg(1),wg(2),wg(3),wg(4)/0.1294849661688697d+00,    0.2797053914892767d+00,0.3818300505051889d+00,&
      0.4179591836734694d+00/
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc*  - abscissa
!           fval*  - function value
!           resg   - result_I of the 7-point gauss formula
!           resk   - result_I of the 15-point kronrod formula
!           reskh  - approximation to the mean value of f*w over (a,b),
!                    i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk15w
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 15-point kronrod approximation to the
!           integral, and estimate the error.
!
      fc = f(centr)*w(centr,p1,p2,p3,p4,kp)
      resg = wg(4)*fc
      resk = wgk(8)*fc
      resabs = dabs(resk)
      do 10 j=1,3
        jtw = j*2
        absc = hlgth*xgk(jtw)
        absc1 = centr-absc
        absc2 = centr+absc
        fval1 = f(absc1)*w(absc1,p1,p2,p3,p4,kp)
        fval2 = f(absc2)*w(absc2,p1,p2,p3,p4,kp)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j=1,4
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        absc1 = centr-absc
        absc2 = centr+absc
        fval1 = f(absc1)*w(absc1,p1,p2,p3,p4,kp)
        fval2 = f(absc2)*w(absc2,p1,p2,p3,p4,kp)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(8)*dabs(fc-reskh)
      do 20 j=1,7
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk15w
!*********************************************************************72
subroutine dqk21(f,a,b,result_I,abserr,resabs,resasc)
!
!c DQK21 carries out a 21 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk21
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  21-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the driver program.
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 21-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 10-point gauss rule (resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***end prologue  dqk21
!
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,&
                  hlgth,resabs,resasc,resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(10),fv2(10),wg(5),wgk(11),xgk(11)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 21-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 10-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 10-point gauss rule
!
!           wgk    - weights of the 21-point kronrod rule
!
!           wg     - weights of the 10-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.066671344308688137593568809893332d0 /
      data wg  (  2) / 0.149451349150580593145776339657697d0 /
      data wg  (  3) / 0.219086362515982043995534934228163d0 /
      data wg  (  4) / 0.269266719309996355091226921569469d0 /
      data wg  (  5) / 0.295524224714752870173892994651338d0 /
!
      data xgk (  1) / 0.995657163025808080735527280689003d0 /
      data xgk (  2) / 0.973906528517171720077964012084452d0 /
      data xgk (  3) / 0.930157491355708226001207180059508d0 /
      data xgk (  4) / 0.865063366688984510732096688423493d0 /
      data xgk (  5) / 0.780817726586416897063717578345042d0 /
      data xgk (  6) / 0.679409568299024406234327365114874d0 /
      data xgk (  7) / 0.562757134668604683339000099272694d0 /
      data xgk (  8) / 0.433395394129247190799265943165784d0 /
      data xgk (  9) / 0.294392862701460198131126603103866d0 /
      data xgk ( 10) / 0.148874338981631210884826001129720d0 /
      data xgk ( 11) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.011694638867371874278064396062192d0 /
      data wgk (  2) / 0.032558162307964727478818972459390d0 /
      data wgk (  3) / 0.054755896574351996031381300244580d0 /
      data wgk (  4) / 0.075039674810919952767043140916190d0 /
      data wgk (  5) / 0.093125454583697605535065465083366d0 /
      data wgk (  6) / 0.109387158802297641899210590325805d0 /
      data wgk (  7) / 0.123491976262065851077958109831074d0 /
      data wgk (  8) / 0.134709217311473325928054001771707d0 /
      data wgk (  9) / 0.142775938577060080797094273138717d0 /
      data wgk ( 10) / 0.147739104901338491374841515972068d0 /
      data wgk ( 11) / 0.149445554002916905664936468389821d0 /
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result_I of the 10-point gauss formula
!           resk   - result_I of the 21-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk21
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 21-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      resg = 0.0d+00
      fc = f(centr)
      resk = wgk(11)*fc
      resabs = dabs(resk)
      do 10 j=1,5
        jtw = 2*j
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,5
        jtwm1 = 2*j-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(11)*dabs(fc-reskh)
      do 20 j=1,10
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk21
subroutine dqk31(f,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK31 carries out a 31 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk31
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  31-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 31-point
!                       gauss-kronrod rule (resk), obtained by optimal
!                       addition of abscissae to the 15-point gauss
!                       rule (resg).
!
!              abserr - double precison
!                       estimate of the modulus of the modulus,
!                       which should not exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***end prologue  dqk31
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,&
                  hlgth,resabs,resasc,resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(15),fv2(15),xgk(16),wgk(16),wg(8)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 31-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 15-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 15-point gauss rule
!
!           wgk    - weights of the 31-point kronrod rule
!
!           wg     - weights of the 15-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.030753241996117268354628393577204d0 /
      data wg  (  2) / 0.070366047488108124709267416450667d0 /
      data wg  (  3) / 0.107159220467171935011869546685869d0 /
      data wg  (  4) / 0.139570677926154314447804794511028d0 /
      data wg  (  5) / 0.166269205816993933553200860481209d0 /
      data wg  (  6) / 0.186161000015562211026800561866423d0 /
      data wg  (  7) / 0.198431485327111576456118326443839d0 /
      data wg  (  8) / 0.202578241925561272880620199967519d0 /
!
      data xgk (  1) / 0.998002298693397060285172840152271d0 /
      data xgk (  2) / 0.987992518020485428489565718586613d0 /
      data xgk (  3) / 0.967739075679139134257347978784337d0 /
      data xgk (  4) / 0.937273392400705904307758947710209d0 /
      data xgk (  5) / 0.897264532344081900882509656454496d0 /
      data xgk (  6) / 0.848206583410427216200648320774217d0 /
      data xgk (  7) / 0.790418501442465932967649294817947d0 /
      data xgk (  8) / 0.724417731360170047416186054613938d0 /
      data xgk (  9) / 0.650996741297416970533735895313275d0 /
      data xgk ( 10) / 0.570972172608538847537226737253911d0 /
      data xgk ( 11) / 0.485081863640239680693655740232351d0 /
      data xgk ( 12) / 0.394151347077563369897207370981045d0 /
      data xgk ( 13) / 0.299180007153168812166780024266389d0 /
      data xgk ( 14) / 0.201194093997434522300628303394596d0 /
      data xgk ( 15) / 0.101142066918717499027074231447392d0 /
      data xgk ( 16) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.005377479872923348987792051430128d0 /
      data wgk (  2) / 0.015007947329316122538374763075807d0 /
      data wgk (  3) / 0.025460847326715320186874001019653d0 /
      data wgk (  4) / 0.035346360791375846222037948478360d0 /
      data wgk (  5) / 0.044589751324764876608227299373280d0 /
      data wgk (  6) / 0.053481524690928087265343147239430d0 /
      data wgk (  7) / 0.062009567800670640285139230960803d0 /
      data wgk (  8) / 0.069854121318728258709520077099147d0 /
      data wgk (  9) / 0.076849680757720378894432777482659d0 /
      data wgk ( 10) / 0.083080502823133021038289247286104d0 /
      data wgk ( 11) / 0.088564443056211770647275443693774d0 /
      data wgk ( 12) / 0.093126598170825321225486872747346d0 /
      data wgk ( 13) / 0.096642726983623678505179907627589d0 /
      data wgk ( 14) / 0.099173598721791959332393173484603d0 /
      data wgk ( 15) / 0.100769845523875595044946662617570d0 /
      data wgk ( 16) / 0.101330007014791549017374792767493d0 /
!
!
!           list of major variables
!           -----------------------
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result_I of the 15-point gauss formula
!           resk   - result_I of the 31-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!***first executable statement  dqk31
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 31-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      fc = f(centr)
      resg = wg(8)*fc
      resk = wgk(16)*fc
      resabs = dabs(resk)
      do 10 j=1,7
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,8
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(16)*dabs(fc-reskh)
      do 20 j=1,15
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk31
subroutine dqk41(f,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK41 carries out a 41 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk41
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  41-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b), with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       function subprogram defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 41-point
!                       gauss-kronrod rule (resk) obtained by optimal
!                       addition of abscissae to the 20-point gauss
!                       rule (resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integal of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***end prologue  dqk41
!
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,&
                  hlgth,resabs,resasc,resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(20),fv2(20),xgk(21),wgk(21),wg(10)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 41-point gauss-kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 20-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 20-point gauss rule
!
!           wgk    - weights of the 41-point gauss-kronrod rule
!
!           wg     - weights of the 20-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.017614007139152118311861962351853d0 /
      data wg  (  2) / 0.040601429800386941331039952274932d0 /
      data wg  (  3) / 0.062672048334109063569506535187042d0 /
      data wg  (  4) / 0.083276741576704748724758143222046d0 /
      data wg  (  5) / 0.101930119817240435036750135480350d0 /
      data wg  (  6) / 0.118194531961518417312377377711382d0 /
      data wg  (  7) / 0.131688638449176626898494499748163d0 /
      data wg  (  8) / 0.142096109318382051329298325067165d0 /
      data wg  (  9) / 0.149172986472603746787828737001969d0 /
      data wg  ( 10) / 0.152753387130725850698084331955098d0 /
!
      data xgk (  1) / 0.998859031588277663838315576545863d0 /
      data xgk (  2) / 0.993128599185094924786122388471320d0 /
      data xgk (  3) / 0.981507877450250259193342994720217d0 /
      data xgk (  4) / 0.963971927277913791267666131197277d0 /
      data xgk (  5) / 0.940822633831754753519982722212443d0 /
      data xgk (  6) / 0.912234428251325905867752441203298d0 /
      data xgk (  7) / 0.878276811252281976077442995113078d0 /
      data xgk (  8) / 0.839116971822218823394529061701521d0 /
      data xgk (  9) / 0.795041428837551198350638833272788d0 /
      data xgk ( 10) / 0.746331906460150792614305070355642d0 /
      data xgk ( 11) / 0.693237656334751384805490711845932d0 /
      data xgk ( 12) / 0.636053680726515025452836696226286d0 /
      data xgk ( 13) / 0.575140446819710315342946036586425d0 /
      data xgk ( 14) / 0.510867001950827098004364050955251d0 /
      data xgk ( 15) / 0.443593175238725103199992213492640d0 /
      data xgk ( 16) / 0.373706088715419560672548177024927d0 /
      data xgk ( 17) / 0.301627868114913004320555356858592d0 /
      data xgk ( 18) / 0.227785851141645078080496195368575d0 /
      data xgk ( 19) / 0.152605465240922675505220241022678d0 /
      data xgk ( 20) / 0.076526521133497333754640409398838d0 /
      data xgk ( 21) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.003073583718520531501218293246031d0 /
      data wgk (  2) / 0.008600269855642942198661787950102d0 /
      data wgk (  3) / 0.014626169256971252983787960308868d0 /
      data wgk (  4) / 0.020388373461266523598010231432755d0 /
      data wgk (  5) / 0.025882133604951158834505067096153d0 /
      data wgk (  6) / 0.031287306777032798958543119323801d0 /
      data wgk (  7) / 0.036600169758200798030557240707211d0 /
      data wgk (  8) / 0.041668873327973686263788305936895d0 /
      data wgk (  9) / 0.046434821867497674720231880926108d0 /
      data wgk ( 10) / 0.050944573923728691932707670050345d0 /
      data wgk ( 11) / 0.055195105348285994744832372419777d0 /
      data wgk ( 12) / 0.059111400880639572374967220648594d0 /
      data wgk ( 13) / 0.062653237554781168025870122174255d0 /
      data wgk ( 14) / 0.065834597133618422111563556969398d0 /
      data wgk ( 15) / 0.068648672928521619345623411885368d0 /
      data wgk ( 16) / 0.071054423553444068305790361723210d0 /
      data wgk ( 17) / 0.073030690332786667495189417658913d0 /
      data wgk ( 18) / 0.074582875400499188986581418362488d0 /
      data wgk ( 19) / 0.075704497684556674659542775376617d0 /
      data wgk ( 20) / 0.076377867672080736705502835038061d0 /
      data wgk ( 21) / 0.076600711917999656445049901530102d0 /
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result_I of the 20-point gauss formula
!           resk   - result_I of the 41-point kronrod formula
!           reskh  - approximation to mean value of f over (a,b), i.e.
!                    to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk41
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 41-point gauss-kronrod approximation to
!           the integral, and estimate the absolute error.
!
      resg = 0.0d+00
      fc = f(centr)
      resk = wgk(21)*fc
      resabs = dabs(resk)
      do 10 j=1,10
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,10
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(21)*dabs(fc-reskh)
      do 20 j=1,20
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk41
subroutine dqk51(f,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK51 carries out a 51 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk51
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  51-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of abs(f) over (a,b)
!***description
!
!           integration rules
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters
!            on entry
!              f      - real(DP) ::
!                       function subroutine defining the integrand
!                       function f(x). the actual name for f needs to be
!                       declared e x t e r n a l in the calling program.
!
!              a      - real(DP) ::
!                       lower limit of integration
!
!              b      - real(DP) ::
!                       upper limit of integration
!
!            on return
!              result_I - real(DP) ::
!                       approximation to the integral i
!                       result_I is computed by applying the 51-point
!                       kronrod rule (resk) obtained by optimal addition
!                       of abscissae to the 25-point gauss rule (resg).
!
!              abserr - real(DP) ::
!                       estimate of the modulus of the absolute error,
!                       which should not exceed abs(i-result_I)
!
!              resabs - real(DP) ::
!                       approximation to the integral j
!
!              resasc - real(DP) ::
!                       approximation to the integral of abs(f-i/(b-a))
!                       over (a,b)
!
!***references  (none)
!***end prologue  dqk51
!
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,&
                  hlgth,resabs,resasc,resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(25),fv2(25),xgk(26),wgk(26),wg(13)
!
!           the abscissae and weights are given for the interval (-1,1).
!           because of symmetry only the positive abscissae and their
!           corresponding weights are given.
!
!           xgk    - abscissae of the 51-point kronrod rule
!                    xgk(2), xgk(4), ...  abscissae of the 25-point
!                    gauss rule
!                    xgk(1), xgk(3), ...  abscissae which are optimally
!                    added to the 25-point gauss rule
!
!           wgk    - weights of the 51-point kronrod rule
!
!           wg     - weights of the 25-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.011393798501026287947902964113235d0 /
      data wg  (  2) / 0.026354986615032137261901815295299d0 /
      data wg  (  3) / 0.040939156701306312655623487711646d0 /
      data wg  (  4) / 0.054904695975835191925936891540473d0 /
      data wg  (  5) / 0.068038333812356917207187185656708d0 /
      data wg  (  6) / 0.080140700335001018013234959669111d0 /
      data wg  (  7) / 0.091028261982963649811497220702892d0 /
      data wg  (  8) / 0.100535949067050644202206890392686d0 /
      data wg  (  9) / 0.108519624474263653116093957050117d0 /
      data wg  ( 10) / 0.114858259145711648339325545869556d0 /
      data wg  ( 11) / 0.119455763535784772228178126512901d0 /
      data wg  ( 12) / 0.122242442990310041688959518945852d0 /
      data wg  ( 13) / 0.123176053726715451203902873079050d0 /
!
      data xgk (  1) / 0.999262104992609834193457486540341d0 /
      data xgk (  2) / 0.995556969790498097908784946893902d0 /
      data xgk (  3) / 0.988035794534077247637331014577406d0 /
      data xgk (  4) / 0.976663921459517511498315386479594d0 /
      data xgk (  5) / 0.961614986425842512418130033660167d0 /
      data xgk (  6) / 0.942974571228974339414011169658471d0 /
      data xgk (  7) / 0.920747115281701561746346084546331d0 /
      data xgk (  8) / 0.894991997878275368851042006782805d0 /
      data xgk (  9) / 0.865847065293275595448996969588340d0 /
      data xgk ( 10) / 0.833442628760834001421021108693570d0 /
      data xgk ( 11) / 0.797873797998500059410410904994307d0 /
      data xgk ( 12) / 0.759259263037357630577282865204361d0 /
      data xgk ( 13) / 0.717766406813084388186654079773298d0 /
      data xgk ( 14) / 0.673566368473468364485120633247622d0 /
      data xgk ( 15) / 0.626810099010317412788122681624518d0 /
      data xgk ( 16) / 0.577662930241222967723689841612654d0 /
      data xgk ( 17) / 0.526325284334719182599623778158010d0 /
      data xgk ( 18) / 0.473002731445714960522182115009192d0 /
      data xgk ( 19) / 0.417885382193037748851814394594572d0 /
      data xgk ( 20) / 0.361172305809387837735821730127641d0 /
      data xgk ( 21) / 0.303089538931107830167478909980339d0 /
      data xgk ( 22) / 0.243866883720988432045190362797452d0 /
      data xgk ( 23) / 0.183718939421048892015969888759528d0 /
      data xgk ( 24) / 0.122864692610710396387359818808037d0 /
      data xgk ( 25) / 0.061544483005685078886546392366797d0 /
      data xgk ( 26) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.001987383892330315926507851882843d0 /
      data wgk (  2) / 0.005561932135356713758040236901066d0 /
      data wgk (  3) / 0.009473973386174151607207710523655d0 /
      data wgk (  4) / 0.013236229195571674813656405846976d0 /
      data wgk (  5) / 0.016847817709128298231516667536336d0 /
      data wgk (  6) / 0.020435371145882835456568292235939d0 /
      data wgk (  7) / 0.024009945606953216220092489164881d0 /
      data wgk (  8) / 0.027475317587851737802948455517811d0 /
      data wgk (  9) / 0.030792300167387488891109020215229d0 /
      data wgk ( 10) / 0.034002130274329337836748795229551d0 /
      data wgk ( 11) / 0.037116271483415543560330625367620d0 /
      data wgk ( 12) / 0.040083825504032382074839284467076d0 /
      data wgk ( 13) / 0.042872845020170049476895792439495d0 /
      data wgk ( 14) / 0.045502913049921788909870584752660d0 /
      data wgk ( 15) / 0.047982537138836713906392255756915d0 /
      data wgk ( 16) / 0.050277679080715671963325259433440d0 /
      data wgk ( 17) / 0.052362885806407475864366712137873d0 /
      data wgk ( 18) / 0.054251129888545490144543370459876d0 /
      data wgk ( 19) / 0.055950811220412317308240686382747d0 /
      data wgk ( 20) / 0.057437116361567832853582693939506d0 /
      data wgk ( 21) / 0.058689680022394207961974175856788d0 /
      data wgk ( 22) / 0.059720340324174059979099291932562d0 /
      data wgk ( 23) / 0.060539455376045862945360267517565d0 /
      data wgk ( 24) / 0.061128509717053048305859030416293d0 /
      data wgk ( 25) / 0.061471189871425316661544131965264d0 /
!       note: wgk (26) was calculated from the values of wgk(1..25)
      data wgk ( 26) / 0.061580818067832935078759824240066d0 /
!
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           absc   - abscissa
!           fval*  - function value
!           resg   - result_I of the 25-point gauss formula
!           resk   - result_I of the 51-point kronrod formula
!           reskh  - approximation to the mean value of f over (a,b),
!                    i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqk51
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(a+b)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 51-point kronrod approximation to
!           the integral, and estimate the absolute error.
!
      fc = f(centr)
      resg = wg(13)*fc
      resk = wgk(26)*fc
      resabs = dabs(resk)
      do 10 j=1,12
        jtw = j*2
        absc = hlgth*xgk(jtw)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j = 1,13
        jtwm1 = j*2-1
        absc = hlgth*xgk(jtwm1)
        fval1 = f(centr-absc)
        fval2 = f(centr+absc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15 continue
      reskh = resk*0.5d+00
      resasc = wgk(26)*dabs(fc-reskh)
      do 20 j=1,25
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk51
subroutine dqk61(f,a,b,result_I,abserr,resabs,resasc)
!*********************************************************************72
!
!c DQK61 carries out a 61 point Gauss-Kronrod quadrature rule.
!
!***begin prologue  dqk61
!***date written   800101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a1a2
!***keywords  61-point gauss-kronrod rules
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  to compute i = integral of f over (a,b) with error
!                           estimate
!                       j = integral of dabs(f) over (a,b)
!***description
!
!        integration rule
!        standard fortran subroutine
!        real(DP) :: version
!
!
!        parameters
!         on entry
!           f      - real(DP) ::
!                    function subprogram defining the integrand
!                    function f(x). the actual name for f needs to be
!                    declared e x t e r n a l in the calling program.
!
!           a      - real(DP) ::
!                    lower limit of integration
!
!           b      - real(DP) ::
!                    upper limit of integration
!
!         on return
!           result_I - real(DP) ::
!                    approximation to the integral i
!                    result_I is computed by applying the 61-point
!                    kronrod rule (resk) obtained by optimal addition of
!                    abscissae to the 30-point gauss rule (resg).
!
!           abserr - real(DP) ::
!                    estimate of the modulus of the absolute error,
!                    which should equal or exceed dabs(i-result_I)
!
!           resabs - real(DP) ::
!                    approximation to the integral j
!
!           resasc - real(DP) ::
!                    approximation to the integral of dabs(f-i/(b-a))
!
!
!***references  (none)
!***end prologue  dqk61
!
      real(DP) :: a,dabsc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,f,fc,fsum,fval1,fval2,fv1,fv2,&
                  hlgth,resabs,resasc,resg,resk,reskh,result_I,uflow,wg,wgk,xgk
      integer(I4B) :: j,jtw,jtwm1
      external f
!
      dimension fv1(30),fv2(30),xgk(31),wgk(31),wg(15)
!
!           the abscissae and weights are given for the
!           interval (-1,1). because of symmetry only the positive
!           abscissae and their corresponding weights are given.
!
!           xgk   - abscissae of the 61-point kronrod rule
!                   xgk(2), xgk(4)  ... abscissae of the 30-point
!                   gauss rule
!                   xgk(1), xgk(3)  ... optimally added abscissae
!                   to the 30-point gauss rule
!
!           wgk   - weights of the 61-point kronrod rule
!
!           wg    - weigths of the 30-point gauss rule
!
!
! gauss quadrature weights and kronron quadrature abscissae and weights
! as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
! bell labs, nov. 1981.
!
      data wg  (  1) / 0.007968192496166605615465883474674d0 /
      data wg  (  2) / 0.018466468311090959142302131912047d0 /
      data wg  (  3) / 0.028784707883323369349719179611292d0 /
      data wg  (  4) / 0.038799192569627049596801936446348d0 /
      data wg  (  5) / 0.048402672830594052902938140422808d0 /
      data wg  (  6) / 0.057493156217619066481721689402056d0 /
      data wg  (  7) / 0.065974229882180495128128515115962d0 /
      data wg  (  8) / 0.073755974737705206268243850022191d0 /
      data wg  (  9) / 0.080755895229420215354694938460530d0 /
      data wg  ( 10) / 0.086899787201082979802387530715126d0 /
      data wg  ( 11) / 0.092122522237786128717632707087619d0 /
      data wg  ( 12) / 0.096368737174644259639468626351810d0 /
      data wg  ( 13) / 0.099593420586795267062780282103569d0 /
      data wg  ( 14) / 0.101762389748405504596428952168554d0 /
      data wg  ( 15) / 0.102852652893558840341285636705415d0 /
!
      data xgk (  1) / 0.999484410050490637571325895705811d0 /
      data xgk (  2) / 0.996893484074649540271630050918695d0 /
      data xgk (  3) / 0.991630996870404594858628366109486d0 /
      data xgk (  4) / 0.983668123279747209970032581605663d0 /
      data xgk (  5) / 0.973116322501126268374693868423707d0 /
      data xgk (  6) / 0.960021864968307512216871025581798d0 /
      data xgk (  7) / 0.944374444748559979415831324037439d0 /
      data xgk (  8) / 0.926200047429274325879324277080474d0 /
      data xgk (  9) / 0.905573307699907798546522558925958d0 /
      data xgk ( 10) / 0.882560535792052681543116462530226d0 /
      data xgk ( 11) / 0.857205233546061098958658510658944d0 /
      data xgk ( 12) / 0.829565762382768397442898119732502d0 /
      data xgk ( 13) / 0.799727835821839083013668942322683d0 /
      data xgk ( 14) / 0.767777432104826194917977340974503d0 /
      data xgk ( 15) / 0.733790062453226804726171131369528d0 /
      data xgk ( 16) / 0.697850494793315796932292388026640d0 /
      data xgk ( 17) / 0.660061064126626961370053668149271d0 /
      data xgk ( 18) / 0.620526182989242861140477556431189d0 /
      data xgk ( 19) / 0.579345235826361691756024932172540d0 /
      data xgk ( 20) / 0.536624148142019899264169793311073d0 /
      data xgk ( 21) / 0.492480467861778574993693061207709d0 /
      data xgk ( 22) / 0.447033769538089176780609900322854d0 /
      data xgk ( 23) / 0.400401254830394392535476211542661d0 /
      data xgk ( 24) / 0.352704725530878113471037207089374d0 /
      data xgk ( 25) / 0.304073202273625077372677107199257d0 /
      data xgk ( 26) / 0.254636926167889846439805129817805d0 /
      data xgk ( 27) / 0.204525116682309891438957671002025d0 /
      data xgk ( 28) / 0.153869913608583546963794672743256d0 /
      data xgk ( 29) / 0.102806937966737030147096751318001d0 /
      data xgk ( 30) / 0.051471842555317695833025213166723d0 /
      data xgk ( 31) / 0.000000000000000000000000000000000d0 /
!
      data wgk (  1) / 0.001389013698677007624551591226760d0 /
      data wgk (  2) / 0.003890461127099884051267201844516d0 /
      data wgk (  3) / 0.006630703915931292173319826369750d0 /
      data wgk (  4) / 0.009273279659517763428441146892024d0 /
      data wgk (  5) / 0.011823015253496341742232898853251d0 /
      data wgk (  6) / 0.014369729507045804812451432443580d0 /
      data wgk (  7) / 0.016920889189053272627572289420322d0 /
      data wgk (  8) / 0.019414141193942381173408951050128d0 /
      data wgk (  9) / 0.021828035821609192297167485738339d0 /
      data wgk ( 10) / 0.024191162078080601365686370725232d0 /
      data wgk ( 11) / 0.026509954882333101610601709335075d0 /
      data wgk ( 12) / 0.028754048765041292843978785354334d0 /
      data wgk ( 13) / 0.030907257562387762472884252943092d0 /
      data wgk ( 14) / 0.032981447057483726031814191016854d0 /
      data wgk ( 15) / 0.034979338028060024137499670731468d0 /
      data wgk ( 16) / 0.036882364651821229223911065617136d0 /
      data wgk ( 17) / 0.038678945624727592950348651532281d0 /
      data wgk ( 18) / 0.040374538951535959111995279752468d0 /
      data wgk ( 19) / 0.041969810215164246147147541285970d0 /
      data wgk ( 20) / 0.043452539701356069316831728117073d0 /
      data wgk ( 21) / 0.044814800133162663192355551616723d0 /
      data wgk ( 22) / 0.046059238271006988116271735559374d0 /
      data wgk ( 23) / 0.047185546569299153945261478181099d0 /
      data wgk ( 24) / 0.048185861757087129140779492298305d0 /
      data wgk ( 25) / 0.049055434555029778887528165367238d0 /
      data wgk ( 26) / 0.049795683427074206357811569379942d0 /
      data wgk ( 27) / 0.050405921402782346840893085653585d0 /
      data wgk ( 28) / 0.050881795898749606492297473049805d0 /
      data wgk ( 29) / 0.051221547849258772170656282604944d0 /
      data wgk ( 30) / 0.051426128537459025933862879215781d0 /
      data wgk ( 31) / 0.051494729429451567558340433647099d0 /
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the interval
!           hlgth  - half-length of the interval
!           dabsc  - abscissa
!           fval*  - function value
!           resg   - result_I of the 30-point gauss rule
!           resk   - result_I of the 61-point kronrod rule
!           reskh  - approximation to the mean value of f
!                    over (a,b), i.e. to i/(b-a)
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
      epmach = d1mach(4)
      uflow = d1mach(1)
!
      centr = 0.5d+00*(b+a)
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
!
!           compute the 61-point kronrod approximation to the
!           integral, and estimate the absolute error.
!
!***first executable statement  dqk61
      resg = 0.0d+00
      fc = f(centr)
      resk = wgk(31)*fc
      resabs = dabs(resk)
      do 10 j=1,15
        jtw = j*2
        dabsc = hlgth*xgk(jtw)
        fval1 = f(centr-dabsc)
        fval2 = f(centr+dabsc)
        fv1(jtw) = fval1
        fv2(jtw) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(jtw)*fsum
        resabs = resabs+wgk(jtw)*(dabs(fval1)+dabs(fval2))
   10 continue
      do 15 j=1,15
        jtwm1 = j*2-1
        dabsc = hlgth*xgk(jtwm1)
        fval1 = f(centr-dabsc)
        fval2 = f(centr+dabsc)
        fv1(jtwm1) = fval1
        fv2(jtwm1) = fval2
        fsum = fval1+fval2
        resk = resk+wgk(jtwm1)*fsum
        resabs = resabs+wgk(jtwm1)*(dabs(fval1)+dabs(fval2))
   15   continue
      reskh = resk*0.5d+00
      resasc = wgk(31)*dabs(fc-reskh)
      do 20 j=1,30
        resasc = resasc+wgk(j)*(dabs(fv1(j)-reskh)+dabs(fv2(j)-reskh))
   20 continue
      result_I = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      return
end subroutine dqk61
subroutine dqmomo(alfa,beta,ri,rj,rg,rh,integr)
!*********************************************************************72
!
!c DQMOMO computes modified Chebyshev moments.
!
!***begin prologue  dqmomo
!***date written   820101   (yymmdd)
!***revision date  830518   (yymmdd)
!***category no.  h2a2a1,c3a2
!***keywords  modified chebyshev moments
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  this routine computes modified chebsyshev moments. the k-th
!            modified chebyshev moment is defined as the integral over
!            (-1,1) of w(x)*t(k,x), where t(k,x) is the chebyshev
!            polynomial of degree k.
!***description
!
!        modified chebyshev moments
!        standard fortran subroutine
!        real(DP) :: version
!
!        parameters
!           alfa   - real(DP) ::
!                    parameter in the weight function w(x), alfa > (-1)
!
!           beta   - real(DP) ::
!                    parameter in the weight function w(x), beta > (-1)
!
!           ri     - real(DP) ::
!                    vector of dimension 25
!                    ri(k) is the integral over (-1,1) of
!                    (1+x)**alfa*t(k-1,x), k = 1, ..., 25.
!
!           rj     - real(DP) ::
!                    vector of dimension 25
!                    rj(k) is the integral over (-1,1) of
!                    (1-x)**beta*t(k-1,x), k = 1, ..., 25.
!
!           rg     - real(DP) ::
!                    vector of dimension 25
!                    rg(k) is the integral over (-1,1) of
!                    (1+x)**alfa*log((1+x)/2)*t(k-1,x), k = 1, ..., 25.
!
!           rh     - real(DP) ::
!                    vector of dimension 25
!                    rh(k) is the integral over (-1,1) of
!                    (1-x)**beta*log((1-x)/2)*t(k-1,x), k = 1, ..., 25.
!
!           integr - integer
!                    input parameter indicating the modified
!                    moments to be computed
!                    integr = 1 compute ri, rj
!                           = 2 compute ri, rj, rg
!                           = 3 compute ri, rj, rh
!                           = 4 compute ri, rj, rg, rh
!
!***references  (none)
!***routines called  (none)
!***end prologue  dqmomo
!
      real(DP) :: alfa,alfp1,alfp2,an,anm1,beta,betp1,betp2,ralf,rbet,rg,rh,ri,rj
      integer(I4B) :: i,im1,integr
!
      dimension rg(25),rh(25),ri(25),rj(25)
!
!
!***first executable statement  dqmomo
      alfp1 = alfa+0.1d+01
      betp1 = beta+0.1d+01
      alfp2 = alfa+0.2d+01
      betp2 = beta+0.2d+01
      ralf = 0.2d+01**alfp1
      rbet = 0.2d+01**betp1
!
!           compute ri, rj using a forward recurrence relation.
!
      ri(1) = ralf/alfp1
      rj(1) = rbet/betp1
      ri(2) = ri(1)*alfa/alfp2
      rj(2) = rj(1)*beta/betp2
      an = 0.2d+01
      anm1 = 0.1d+01
      do 20 i=3,25
        ri(i) = -(ralf+an*(an-alfp2)*ri(i-1))/(anm1*(an+alfp1))
        rj(i) = -(rbet+an*(an-betp2)*rj(i-1))/(anm1*(an+betp1))
        anm1 = an
        an = an+0.1d+01
   20 continue
      if (integr == 1) go to 70
      if (integr == 3) go to 40
!
!           compute rg using a forward recurrence relation.
!
      rg(1) = -ri(1)/alfp1
      rg(2) = -(ralf+ralf)/(alfp2*alfp2)-rg(1)
      an = 0.2d+01
      anm1 = 0.1d+01
      im1 = 2
      do 30 i=3,25
        rg(i) = -(an*(an-alfp2)*rg(im1)-an*ri(im1)+anm1*ri(i))/(anm1*(an+alfp1))
        anm1 = an
        an = an+0.1d+01
        im1 = i
   30 continue
      if (integr == 2) go to 70
!
!           compute rh using a forward recurrence relation.
!
   40 rh(1) = -rj(1)/betp1
      rh(2) = -(rbet+rbet)/(betp2*betp2)-rh(1)
      an = 0.2d+01
      anm1 = 0.1d+01
      im1 = 2
      do 50 i=3,25
        rh(i) = -(an*(an-betp2)*rh(im1)-an*rj(im1)+anm1*rj(i))/(anm1*(an+betp1))
        anm1 = an
        an = an+0.1d+01
        im1 = i
   50 continue
      do 60 i=2,25,2
        rh(i) = -rh(i)
   60 continue
   70 do 80 i=2,25,2
        rj(i) = -rj(i)
   80 continue
   90 return
end subroutine dqmomo
subroutine dqng(f,a,b,epsabs,epsrel,result_I,abserr,neval,ier)
!*********************************************************************72
!
!c DQNG estimates an integral, using non-adaptive integration.
!
!***begin prologue  dqng
!***date written   800101   (yymmdd)
!***revision date  810101   (yymmdd)
!***category no.  h2a1a1
!***keywords  automatic integrator, smooth integrand,
!             non-adaptive, gauss-kronrod(patterson)
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl math & progr. div. - k.u.leuven
!           kahaner,david,nbs - modified (2/82)
!***purpose  the routine calculates an approximation result_I to a
!            given definite integral i = integral of f over (a,b),
!            hopefully satisfying following claim for accuracy
!            abs(i-result_I) <= max(epsabs,epsrel*abs(i)).
!***description
!
! non-adaptive integration
! standard fortran subroutine
! real(DP) :: version
!
!           f      - real(DP) ::
!                    function subprogram defining the integrand function
!                    f(x). the actual name for f needs to be declared
!                    e x t e r n a l in the driver program.
!
!           a      - real(DP) ::
!                    lower limit of integration
!
!           b      - real(DP) ::
!                    upper limit of integration
!
!           epsabs - real(DP) ::
!                    absolute accuracy requested
!           epsrel - real(DP) ::
!                    relative accuracy requested
!                    if  epsabs <= 0
!                    and epsrel < max(50*rel.mach.acc.,0.5d-28),
!                    the routine will end with ier = 6.
!
!         on return
!           result_I - real(DP) ::
!                    approximation to the integral i
!                    result_I is obtained by applying the 21-point
!                    gauss-kronrod rule (res21) obtained by optimal
!                    addition of abscissae to the 10-point gauss rule
!                    (res10), or by applying the 43-point rule (res43)
!                    obtained by optimal addition of abscissae to the
!                    21-point gauss-kronrod rule, or by applying the
!                    87-point rule (res87) obtained by optimal addition
!                    of abscissae to the 43-point rule.
!
!           abserr - real(DP) ::
!                    estimate of the modulus of the absolute error,
!                    which should equal or exceed abs(i-result_I)
!
!           neval  - integer
!                    number of integrand evaluations
!
!           ier    - ier = 0 normal and reliable termination of the
!                            routine. it is assumed that the requested
!                            accuracy has been achieved.
!                    ier > 0 abnormal termination of the routine. it is
!                            assumed that the requested accuracy has
!                            not been achieved.
!           error messages
!                    ier = 1 the maximum number of steps has been
!                            executed. the integral is probably too
!                            difficult to be calculated by dqng.
!                        = 6 the input is invalid, because
!                            epsabs <= 0 and
!                            epsrel < max(50*rel.mach.acc.,0.5d-28).
!                            result_I, abserr and neval are set to zero.
!
!***references  (none)
!***end prologue  dqng
!
      real(DP) :: a,absc,abserr,b,centr,dabs,dhlgth,dmax1,dmin1,epmach,epsabs,&
                  epsrel,f,fcentr,fval,fval1,fval2,fv1,fv2,fv3,fv4,hlgth,result_I,res10,res21,res43,res87,&
                  resabs,resasc,reskh,savfun,uflow,w10,w21a,w21b,w43a,w43b,w87a,w87b,x1,x2,x3,x4
      integer(I4B) :: ier,ipx,k,l,neval
      external f
!
      dimension fv1(5),fv2(5),fv3(5),fv4(5),x1(5),x2(5),x3(11),x4(22),w10(5),w21a(5),w21b(6),w43a(10),w43b(12),&
                w87a(21),w87b(23),savfun(21)
!
!           the following data statements contain the
!           abscissae and weights of the integration rules used.
!
!           x1      abscissae common to the 10-, 21-, 43- and 87-
!                   point rule
!           x2      abscissae common to the 21-, 43- and 87-point rule
!           x3      abscissae common to the 43- and 87-point rule
!           x4      abscissae of the 87-point rule
!           w10     weights of the 10-point formula
!           w21a    weights of the 21-point formula for abscissae x1
!           w21b    weights of the 21-point formula for abscissae x2
!           w43a    weights of the 43-point formula for abscissae x1, x3
!           w43b    weights of the 43-point formula for abscissae x3
!           w87a    weights of the 87-point formula for abscissae x1,
!                   x2, x3
!           w87b    weights of the 87-point formula for abscissae x4
!
!
! gauss-kronrod-patterson quadrature coefficients for use in
! quadpack routine qng.  these coefficients were calculated with
! 101 decimal digit arithmetic by l. w. fullerton, bell labs, nov 1981.
!
      data x1    (  1) / 0.973906528517171720077964012084452d0 /
      data x1    (  2) / 0.865063366688984510732096688423493d0 /
      data x1    (  3) / 0.679409568299024406234327365114874d0 /
      data x1    (  4) / 0.433395394129247190799265943165784d0 /
      data x1    (  5) / 0.148874338981631210884826001129720d0 /
      data w10   (  1) / 0.066671344308688137593568809893332d0 /
      data w10   (  2) / 0.149451349150580593145776339657697d0 /
      data w10   (  3) / 0.219086362515982043995534934228163d0 /
      data w10   (  4) / 0.269266719309996355091226921569469d0 /
      data w10   (  5) / 0.295524224714752870173892994651338d0 /
!
      data x2    (  1) / 0.995657163025808080735527280689003d0 /
      data x2    (  2) / 0.930157491355708226001207180059508d0 /
      data x2    (  3) / 0.780817726586416897063717578345042d0 /
      data x2    (  4) / 0.562757134668604683339000099272694d0 /
      data x2    (  5) / 0.294392862701460198131126603103866d0 /
      data w21a  (  1) / 0.032558162307964727478818972459390d0 /
      data w21a  (  2) / 0.075039674810919952767043140916190d0 /
      data w21a  (  3) / 0.109387158802297641899210590325805d0 /
      data w21a  (  4) / 0.134709217311473325928054001771707d0 /
      data w21a  (  5) / 0.147739104901338491374841515972068d0 /
      data w21b  (  1) / 0.011694638867371874278064396062192d0 /
      data w21b  (  2) / 0.054755896574351996031381300244580d0 /
      data w21b  (  3) / 0.093125454583697605535065465083366d0 /
      data w21b  (  4) / 0.123491976262065851077958109831074d0 /
      data w21b  (  5) / 0.142775938577060080797094273138717d0 /
      data w21b  (  6) / 0.149445554002916905664936468389821d0 /
!
      data x3    (  1) / 0.999333360901932081394099323919911d0 /
      data x3    (  2) / 0.987433402908088869795961478381209d0 /
      data x3    (  3) / 0.954807934814266299257919200290473d0 /
      data x3    (  4) / 0.900148695748328293625099494069092d0 /
      data x3    (  5) / 0.825198314983114150847066732588520d0 /
      data x3    (  6) / 0.732148388989304982612354848755461d0 /
      data x3    (  7) / 0.622847970537725238641159120344323d0 /
      data x3    (  8) / 0.499479574071056499952214885499755d0 /
      data x3    (  9) / 0.364901661346580768043989548502644d0 /
      data x3    ( 10) / 0.222254919776601296498260928066212d0 /
      data x3    ( 11) / 0.074650617461383322043914435796506d0 /
      data w43a  (  1) / 0.016296734289666564924281974617663d0 /
      data w43a  (  2) / 0.037522876120869501461613795898115d0 /
      data w43a  (  3) / 0.054694902058255442147212685465005d0 /
      data w43a  (  4) / 0.067355414609478086075553166302174d0 /
      data w43a  (  5) / 0.073870199632393953432140695251367d0 /
      data w43a  (  6) / 0.005768556059769796184184327908655d0 /
      data w43a  (  7) / 0.027371890593248842081276069289151d0 /
      data w43a  (  8) / 0.046560826910428830743339154433824d0 /
      data w43a  (  9) / 0.061744995201442564496240336030883d0 /
      data w43a  ( 10) / 0.071387267268693397768559114425516d0 /
      data w43b  (  1) / 0.001844477640212414100389106552965d0 /
      data w43b  (  2) / 0.010798689585891651740465406741293d0 /
      data w43b  (  3) / 0.021895363867795428102523123075149d0 /
      data w43b  (  4) / 0.032597463975345689443882222526137d0 /
      data w43b  (  5) / 0.042163137935191811847627924327955d0 /
      data w43b  (  6) / 0.050741939600184577780189020092084d0 /
      data w43b  (  7) / 0.058379395542619248375475369330206d0 /
      data w43b  (  8) / 0.064746404951445885544689259517511d0 /
      data w43b  (  9) / 0.069566197912356484528633315038405d0 /
      data w43b  ( 10) / 0.072824441471833208150939535192842d0 /
      data w43b  ( 11) / 0.074507751014175118273571813842889d0 /
      data w43b  ( 12) / 0.074722147517403005594425168280423d0 /
!
      data x4    (  1) / 0.999902977262729234490529830591582d0 /
      data x4    (  2) / 0.997989895986678745427496322365960d0 /
      data x4    (  3) / 0.992175497860687222808523352251425d0 /
      data x4    (  4) / 0.981358163572712773571916941623894d0 /
      data x4    (  5) / 0.965057623858384619128284110607926d0 /
      data x4    (  6) / 0.943167613133670596816416634507426d0 /
      data x4    (  7) / 0.915806414685507209591826430720050d0 /
      data x4    (  8) / 0.883221657771316501372117548744163d0 /
      data x4    (  9) / 0.845710748462415666605902011504855d0 /
      data x4    ( 10) / 0.803557658035230982788739474980964d0 /
      data x4    ( 11) / 0.757005730685495558328942793432020d0 /
      data x4    ( 12) / 0.706273209787321819824094274740840d0 /
      data x4    ( 13) / 0.651589466501177922534422205016736d0 /
      data x4    ( 14) / 0.593223374057961088875273770349144d0 /
      data x4    ( 15) / 0.531493605970831932285268948562671d0 /
      data x4    ( 16) / 0.466763623042022844871966781659270d0 /
      data x4    ( 17) / 0.399424847859218804732101665817923d0 /
      data x4    ( 18) / 0.329874877106188288265053371824597d0 /
      data x4    ( 19) / 0.258503559202161551802280975429025d0 /
      data x4    ( 20) / 0.185695396568346652015917141167606d0 /
      data x4    ( 21) / 0.111842213179907468172398359241362d0 /
      data x4    ( 22) / 0.037352123394619870814998165437704d0 /
      data w87a  (  1) / 0.008148377384149172900002878448190d0 /
      data w87a  (  2) / 0.018761438201562822243935059003794d0 /
      data w87a  (  3) / 0.027347451050052286161582829741283d0 /
      data w87a  (  4) / 0.033677707311637930046581056957588d0 /
      data w87a  (  5) / 0.036935099820427907614589586742499d0 /
      data w87a  (  6) / 0.002884872430211530501334156248695d0 /
      data w87a  (  7) / 0.013685946022712701888950035273128d0 /
      data w87a  (  8) / 0.023280413502888311123409291030404d0 /
      data w87a  (  9) / 0.030872497611713358675466394126442d0 /
      data w87a  ( 10) / 0.035693633639418770719351355457044d0 /
      data w87a  ( 11) / 0.000915283345202241360843392549948d0 /
      data w87a  ( 12) / 0.005399280219300471367738743391053d0 /
      data w87a  ( 13) / 0.010947679601118931134327826856808d0 /
      data w87a  ( 14) / 0.016298731696787335262665703223280d0 /
      data w87a  ( 15) / 0.021081568889203835112433060188190d0 /
      data w87a  ( 16) / 0.025370969769253827243467999831710d0 /
      data w87a  ( 17) / 0.029189697756475752501446154084920d0 /
      data w87a  ( 18) / 0.032373202467202789685788194889595d0 /
      data w87a  ( 19) / 0.034783098950365142750781997949596d0 /
      data w87a  ( 20) / 0.036412220731351787562801163687577d0 /
      data w87a  ( 21) / 0.037253875503047708539592001191226d0 /
      data w87b  (  1) / 0.000274145563762072350016527092881d0 /
      data w87b  (  2) / 0.001807124155057942948341311753254d0 /
      data w87b  (  3) / 0.004096869282759164864458070683480d0 /
      data w87b  (  4) / 0.006758290051847378699816577897424d0 /
      data w87b  (  5) / 0.009549957672201646536053581325377d0 /
      data w87b  (  6) / 0.012329447652244853694626639963780d0 /
      data w87b  (  7) / 0.015010447346388952376697286041943d0 /
      data w87b  (  8) / 0.017548967986243191099665352925900d0 /
      data w87b  (  9) / 0.019938037786440888202278192730714d0 /
      data w87b  ( 10) / 0.022194935961012286796332102959499d0 /
      data w87b  ( 11) / 0.024339147126000805470360647041454d0 /
      data w87b  ( 12) / 0.026374505414839207241503786552615d0 /
      data w87b  ( 13) / 0.028286910788771200659968002987960d0 /
      data w87b  ( 14) / 0.030052581128092695322521110347341d0 /
      data w87b  ( 15) / 0.031646751371439929404586051078883d0 /
      data w87b  ( 16) / 0.033050413419978503290785944862689d0 /
      data w87b  ( 17) / 0.034255099704226061787082821046821d0 /
      data w87b  ( 18) / 0.035262412660156681033782717998428d0 /
      data w87b  ( 19) / 0.036076989622888701185500318003895d0 /
      data w87b  ( 20) / 0.036698604498456094498018047441094d0 /
      data w87b  ( 21) / 0.037120549269832576114119958413599d0 /
      data w87b  ( 22) / 0.037334228751935040321235449094698d0 /
      data w87b  ( 23) / 0.037361073762679023410321241766599d0 /
!
!           list of major variables
!           -----------------------
!
!           centr  - mid point of the integration interval
!           hlgth  - half-length of the integration interval
!           fcentr - function value at mid point
!           absc   - abscissa
!           fval   - function value
!           savfun - array of function values which have already been
!                    computed
!           res10  - 10-point gauss result_I
!           res21  - 21-point kronrod result_I
!           res43  - 43-point result_I
!           res87  - 87-point result_I
!           resabs - approximation to the integral of abs(f)
!           resasc - approximation to the integral of abs(f-i/(b-a))
!
!           machine dependent constants
!           ---------------------------
!
!           epmach is the largest relative spacing.
!           uflow is the smallest positive magnitude.
!
!***first executable statement  dqng
      epmach = d1mach(4)
      uflow = d1mach(1)
!
!           test on validity of parameters
!           ------------------------------
!
      result_I = 0.0d+00
      abserr = 0.0d+00
      neval = 0
      ier = 6
      if (epsabs <= 0.0d+00.and.epsrel < dmax1(0.5d+02*epmach,0.5d-28))go to 80
      hlgth = 0.5d+00*(b-a)
      dhlgth = dabs(hlgth)
      centr = 0.5d+00*(b+a)
      fcentr = f(centr)
      neval = 21
      ier = 1
!
!          compute the integral using the 10- and 21-point formula.
!
      do 70 l = 1,3
      go to (5,25,45),l
    5 res10 = 0.0d+00
      res21 = w21b(6)*fcentr
      resabs = w21b(6)*dabs(fcentr)
      do 10 k=1,5
        absc = hlgth*x1(k)
        fval1 = f(centr+absc)
        fval2 = f(centr-absc)
        fval = fval1+fval2
        res10 = res10+w10(k)*fval
        res21 = res21+w21a(k)*fval
        resabs = resabs+w21a(k)*(dabs(fval1)+dabs(fval2))
        savfun(k) = fval
        fv1(k) = fval1
        fv2(k) = fval2
   10 continue
      ipx = 5
      do 15 k=1,5
        ipx = ipx+1
        absc = hlgth*x2(k)
        fval1 = f(centr+absc)
        fval2 = f(centr-absc)
        fval = fval1+fval2
        res21 = res21+w21b(k)*fval
        resabs = resabs+w21b(k)*(dabs(fval1)+dabs(fval2))
        savfun(ipx) = fval
        fv3(k) = fval1
        fv4(k) = fval2
   15 continue
!
!          test for convergence.
!
      result_I = res21*hlgth
      resabs = resabs*dhlgth
      reskh = 0.5d+00*res21
      resasc = w21b(6)*dabs(fcentr-reskh)
      do 20 k = 1,5
        resasc = resasc+w21a(k)*(dabs(fv1(k)-reskh)+dabs(fv2(k)-reskh))+w21b(k)*(dabs(fv3(k)-reskh)+dabs(fv4(k)-reskh))
   20 continue
      abserr = dabs((res21-res10)*hlgth)
      resasc = resasc*dhlgth
      go to 65
!
!          compute the integral using the 43-point formula.
!
   25 res43 = w43b(12)*fcentr
      neval = 43
      do 30 k=1,10
        res43 = res43+savfun(k)*w43a(k)
   30 continue
      do 40 k=1,11
        ipx = ipx+1
        absc = hlgth*x3(k)
        fval = f(absc+centr)+f(centr-absc)
        res43 = res43+fval*w43b(k)
        savfun(ipx) = fval
   40 continue
!
!          test for convergence.
!
      result_I = res43*hlgth
      abserr = dabs((res43-res21)*hlgth)
      go to 65
!
!          compute the integral using the 87-point formula.
!
   45 res87 = w87b(23)*fcentr
      neval = 87
      do 50 k=1,21
        res87 = res87+savfun(k)*w87a(k)
   50 continue
      do 60 k=1,22
        absc = hlgth*x4(k)
        res87 = res87+w87b(k)*(f(absc+centr)+f(centr-absc))
   60 continue
      result_I = res87*hlgth
      abserr = dabs((res87-res43)*hlgth)
   65 if (resasc /= 0.0d+00.and.abserr /= 0.0d+00)abserr = resasc*dmin1(0.1d+01,(0.2d+03*abserr/resasc)**1.5d+00)
      if (resabs > uflow/(0.5d+02*epmach)) abserr = dmax1((epmach*0.5d+02)*resabs,abserr)
      if (abserr <= dmax1(epsabs,epsrel*dabs(result_I))) ier = 0
! ***jump out of do-loop
      if (ier == 0) go to 999
   70 continue
   80 call xerror('abnormal return from dqng ',26,ier,0)
  999 return
end subroutine dqng
subroutine dqpsrt(Climit,last,maxerr,ermax,elist,iord,nrmax)
!*********************************************************************72
!
!c DQPSRT maintains the order of a list of local error estimates.
!
!***begin prologue  dqpsrt
!***refer to  dqage,dqagie,dqagpe,dqawse
!***routines called  (none)
!***revision date  810101   (yymmdd)
!***keywords  sequential sorting
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  this routine maintains the descending ordering in the
!            list of the local error estimated result_Iing from the
!            interval subdivision process. at each call two error
!            estimates are inserted using the sequential search
!            method, top-down for the largest error estimate and
!            bottom-up for the smallest error estimate.
!***description
!
!           ordering routine
!           standard fortran subroutine
!           real(DP) :: version
!
!           parameters (meaning at output)
!              Climit  - integer
!                       maximum number of error estimates the list
!                       can contain
!
!              last   - integer
!                       number of error estimates currently in the list
!
!              maxerr - integer
!                       maxerr points to the nrmax-th largest error
!                       estimate currently in the list
!
!              ermax  - real(DP) ::
!                       nrmax-th largest error estimate
!                       ermax = elist(maxerr)
!
!              elist  - real(DP) ::
!                       vector of dimension last containing
!                       the error estimates
!
!              iord   - integer
!                       vector of dimension last, the first k elements
!                       of which contain pointers to the error
!                       estimates, such that
!                       elist(iord(1)),...,  elist(iord(k))
!                       form a decreasing sequence, with
!                       k = last if last <= (Climit/2+2), and
!                       k = Climit+1-last otherwise
!
!              nrmax  - integer
!                       maxerr = iord(nrmax)
!
!***end prologue  dqpsrt
!
      real(DP) :: elist,ermax,errmax,errmin
      integer(I4B) :: i,ibeg,ido,iord,isucc,j,jbnd,jupbn,k,last,Climit,maxerr,nrmax
      dimension elist(last),iord(last)
!
!           check whether the list contains more than
!           two error estimates.
!
!***first executable statement  dqpsrt
      if (last > 2) go to 10
      iord(1) = 1
      iord(2) = 2
      go to 90
!
!           this part of the routine is only executed if, due to a
!           difficult integrand, subdivision increased the error
!           estimate. in the normal case the insert procedure should
!           start after the nrmax-th largest error estimate.
!
   10 errmax = elist(maxerr)
      if (nrmax == 1) go to 30
      ido = nrmax-1
      do 20 i = 1,ido
        isucc = iord(nrmax-1)
! ***jump out of do-loop
        if (errmax <= elist(isucc)) go to 30
        iord(nrmax) = isucc
        nrmax = nrmax-1
   20    continue
!
!           compute the number of elements in the list to be maintained
!           in descending order. this number depends on the number of
!           subdivisions still allowed.
!
   30 jupbn = last
      if (last > (Climit/2+2)) jupbn = Climit+3-last
      errmin = elist(last)
!
!           insert errmax by traversing the list top-down,
!           starting comparison from the element elist(iord(nrmax+1)).
!
      jbnd = jupbn-1
      ibeg = nrmax+1
      if (ibeg > jbnd) go to 50
      do 40 i=ibeg,jbnd
        isucc = iord(i)
! ***jump out of do-loop
        if (errmax >= elist(isucc)) go to 60
        iord(i-1) = isucc
   40 continue
   50 iord(jbnd) = maxerr
      iord(jupbn) = last
      go to 90
!
!           insert errmin by traversing the list bottom-up.
!
   60 iord(i-1) = maxerr
      k = jbnd
      do 70 j=i,jbnd
        isucc = iord(k)
! ***jump out of do-loop
        if (errmin < elist(isucc)) go to 80
        iord(k+1) = isucc
        k = k-1
   70 continue
      iord(i) = last
      go to 90
   80 iord(k+1) = last
!
!           set maxerr and ermax.
!
   90 maxerr = iord(nrmax)
      ermax = elist(maxerr)
      return
end subroutine dqpsrt
!*********************************************************************72
real(DP) function dqwgtc(x,c,p2,p3,p4,kp)
!
!c DQWGTC defines the weight function used by DQC25C.
!
!***begin prologue  dqwgtc
!***refer to dqk15w
!***routines called  (none)
!***revision date  810101   (yymmdd)
!***keywords  weight function, cauchy principal value
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  this function subprogram is used together with the
!            routine qawc and defines the weight function.
!***end prologue  dqwgtc
!
      real(DP) :: c,p2,p3,p4,x
      integer(I4B) :: kp
!***first executable statement  dqwgtc
      dqwgtc = 1.d0/(x-c)
      return
end function dqwgtc
!*********************************************************************72
real(DP) function dqwgtf(x,omega,p2,p3,p4,integr)
!
!c DQWGTF defines the weight functions used by DQC25F.
!
!***begin prologue  dqwgtf
!***refer to   dqk15w
!***routines called  (none)
!***revision date 810101   (yymmdd)
!***keywords  cos or sin in weight function
!***author  piessens,robert, appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. * progr. div. - k.u.leuven
!***end prologue  dqwgtf
!
      real(DP) :: omega,p2,p3,p4,x
      integer(I4B) :: integr
!***first executable statement  dqwgtf
  if ( integr == 1 ) then
    dqwgtf = cos ( omega * x )
  else if ( integr == 2 ) then
    dqwgtf = sin ( omega * x )
  end if
end function dqwgtf
!*********************************************************************72
real(DP) function dqwgts(x,a,b,alfa,beta,integr)
!
!c DQWGTS defines the weight functions used by DQC25S.
!
!***begin prologue  dqwgts
!***refer to dqk15w
!***routines called  (none)
!***revision date  810101   (yymmdd)
!***keywords  weight function, algebraico-logarithmic
!             end-point singularities
!***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
!           de doncker,elise,appl. math. & progr. div. - k.u.leuven
!***purpose  this function subprogram is used together with the
!            routine dqaws and defines the weight function.
!***end prologue  dqwgts
!
      real(DP) :: a,alfa,b,beta,bmx,x,xma
      integer(I4B) :: integr
!***first executable statement  dqwgts
  xma = x-a
  bmx = b-x
  dqwgts = xma**alfa*bmx**beta

  if ( integr == 2 ) then
    dqwgts = dqwgts * log ( xma )
  else if ( integr == 3 ) then
    dqwgts = dqwgts * log ( bmx )
  else if ( integr == 4 ) then
    dqwgts = dqwgts * log ( xma ) * log ( bmx )
  end if

end function dqwgts
!*********************************************************************72
subroutine XERROR (XMESS, NMESS, NERR, LEVEL)
!
!c XERROR replaces the SLATEC XERROR routine.
!
      CHARACTER(len=*),intent(IN) :: XMESS
      IF (LEVEL >= 1) THEN
        IERR=I1MACH(3)
        WRITE(IERR,'(1X,A)') trim(XMESS)
        WRITE(IERR,'('' ERROR NUMBER = '',I5,'', MESSAGE LEVEL = '',I5)') NERR,LEVEL
      endif
END subroutine XERROR
!*********************************************************************72
end module dQuadpack_AC
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module specfun_AC
 use nano_deftyp
 use SPECIAL_GAMMAERF
 use morespec
 use SPECIAL_CISI
 use strangef_V
 use FRESNEL
 use miscfun_AC
 use use_libcerf
 use ComVolFrac_CoreShell
 use dQuadpack_AC
!__________ Calling all special functions


private
public :: dqageAC
public :: CoreCore, CoreShell, ShellShell, SphereSphere
public :: Spec_GAMMA,Spec_LNGAMMA,Spec_PSI,Spec_ERF,Spec_ERFC,Spec_ERFCX,Spec_DAW

public :: Spec_BesselJ0,Spec_BesselJ1
public :: Spec_BesselY0,Spec_BesselY1
public :: Spec_BesselK0,Spec_BesselK1,Spec_BesselK0_Expx,Spec_BesslK1_Expx
public :: Spec_BesselI0,Spec_BesselI1,Spec_BesselI0_Expmx,Spec_BesslI1_Expmx

public :: Spec_Ei,Spec_E1,Spec_Ei_Expmx
public :: Spec_Si, Spec_Ci, gamma_EulerMascheroni

public :: sqpi_ea2_erfca,cacio
public :: Abramowitz, Transport_Integral, Synchrotron_Radiation, Debye_Function, &
          Airy_Ai_Integral, Airy_Bi_Integral, Airy_Gi, Airy_Hi, Arctan_Integral, &
          Bessel_i0_Integral, Bessel_j0_Integral, Bessel_k0_Integral, Bessel_y0_Integral, &
          Clausen, Exp3_Integral, goodwin, BesselI_minus_StruveL, Lobachevsky, Stromgen, &
          Struve_h0, Struve_h1, Struve_l0, Struve_l1
public :: FresnelC,FresnelS,FresnelF,FresnelG
public :: T_Cheb,U_Cheb,Tcheb,Tchebv,Ucheb,Uchebv
public :: coshy,sinhy,tanhy,erf0,erf00

public :: test_all_miscfun, file_test_miscfun
public :: Voigt_F, w_of_z_F, cdawson_F, Erfcx_F, Erfi_F, Im_w_of_x_F, Dawson_F, cerf_F, cerfc_F, cerfcx_F, cerfi_F
 
INTERFACE T_cheb
  module procedure Tcheb, TchebV
END INTERFACE
INTERFACE U_cheb
  module procedure Ucheb, UchebV
END INTERFACE

contains


!***********************************************
 FUNCTION Tcheb(dum, a,b,c,x)
  IMPLICIT NONE
  type(DBarr0),intent(IN)            :: dum
  REAL(DP), INTENT(IN)               :: x
  REAL(DP), OPTIONAL,INTENT(IN)      :: a,b
  REAL(DP), DIMENSION(:), INTENT(IN) :: c

  REAL(DP)                           :: Tcheb
  REAL(DP), DIMENSION(size(c))       :: cfr

  INTEGER(I4B)    :: j,m
  REAL(DP)        :: d,dd,sv,y,y2,crout
  LOGICAL         :: abyes

  IF ( (PRESENT(a).and.(.not.(PRESENT(b)))) .or. (PRESENT(b).and.(.not.(PRESENT(a)))) ) &
     STOP 'Tcheb : a,b must be simultaneously present or not'
  abyes = (PRESENT(a).and.PRESENT(b))
  IF (abyes) THEN
    crout = (x-a)*(x-b)
  ELSE
    crout = (x-one)*(x+one)
  ENDIF

  if (crout > eps_DP) STOP 'x not in range in Tcheb'

  m=size(c)
  if (m==0) then
    Tcheb=zero
    return
  else if (m==1) then
    Tcheb=c(1)
    return
  endif
  cfr = zero
  cfr(m) = one
  IF (abyes) THEN
    y=MIN(one,MAX(-one,(two*x-a-b)/(b-a)))
  ELSE
    y=x
  ENDIF

  IF (MAXVAL(ABS(cfr-c)) < eps_DP) THEN
    Tcheb = cos(REAL(m-1,DP)*ACOS(y))
  ELSE
    d=zero
    dd=zero
    y2=two*y
    do j=m,2,-1
      sv=d
      d=y2*d-dd+c(j)
      dd=sv
    end do
    Tcheb=y*d-dd+c(1)
  ENDIF

 END FUNCTION Tcheb
!***********************************************
 FUNCTION TchebV(dum, a,b,c,x)
  IMPLICIT NONE
  type(DBarr1),intent(IN)            :: dum
  REAL(DP), DIMENSION(:), INTENT(IN) :: c,x
  REAL(DP),OPTIONAL, INTENT(IN)      :: a,b

  REAL(DP), DIMENSION(size(x))       :: TchebV
  REAL(DP), DIMENSION(size(c))       :: cfr

  INTEGER(I4B)                 :: j,m
  REAL(DP), DIMENSION(size(x)) :: d,dd,sv,y,y2,crout
  LOGICAL                      :: abyes

  IF ( (PRESENT(a).and.(.not.(PRESENT(b)))) .or. (PRESENT(b).and.(.not.(PRESENT(a)))) ) &
     STOP 'TchebV : a,b must be simultaneously present or not'
  abyes = (PRESENT(a).and.PRESENT(b))
  IF (abyes) THEN
    crout = (x-a)*(x-b)
  ELSE
    crout = (x-one)*(x+one)
  ENDIF
  if (any(crout > eps_DP)) STOP 'x not in range in TchebV'

  m=size(c)
  if (m==0) then
    TchebV=zero
    return
  else if (m==1) then
    TchebV=c(1)
    return
  endif
  cfr = zero
  cfr(m) = one
  IF (abyes) THEN
    y=MIN(one,MAX(-one,(two*x-a-b)/(b-a)))
  ELSE
    y=x
  ENDIF

  IF (MAXVAL(ABS(cfr-c)) < eps_DP) THEN
    TchebV = cos(REAL(m-1,DP)*ACOS(y))
  ELSE
    d=zero
    dd=zero
    y2=two*y
    do j=m,2,-1
      sv=d
      d=y2*d-dd+c(j)
      dd=sv
    end do
    TchebV=y*d-dd+c(1)
  ENDIF

 END FUNCTION TchebV
!***********************************************
 FUNCTION Ucheb(dum, a,b,c,x)
  IMPLICIT NONE
  type(DBarr0),intent(IN)            :: dum
  REAL(DP), INTENT(IN)               :: x
  REAL(DP), OPTIONAL,INTENT(IN)      :: a,b
  REAL(DP), DIMENSION(:), INTENT(IN) :: c

  REAL(DP)                           :: Ucheb

  INTEGER(I4B)    :: j,m
  REAL(DP)        :: d,dd,sv,y,y2,crout
  LOGICAL         :: abyes

  IF ( (PRESENT(a).and.(.not.(PRESENT(b)))) .or. (PRESENT(b).and.(.not.(PRESENT(a)))) ) &
     STOP 'Ucheb : a,b must be simultaneously present or not'
  abyes = (PRESENT(a).and.PRESENT(b))
  IF (abyes) THEN
    crout = (x-a)*(x-b)
  ELSE
    crout = (x-one)*(x+one)
  ENDIF

  if (crout > sceps_DP) STOP 'x not in range in Ucheb'

  m=size(c)
  if (m==0) then
    Ucheb=zero
    return
  else if (m==1) then
    Ucheb=c(1)
    return
  endif
  IF (abyes) THEN
    y=MIN(one,MAX(-one,(two*x-a-b)/(b-a)))
  ELSE
    y=MIN(one,MAX(-one,x))
  ENDIF

  d=zero
  dd=zero
  y2=two*y
  do j=m,2,-1
    sv=d
    d=y2*d-dd+c(j)
    dd=sv
  end do
  Ucheb=y2*d-dd+c(1)

 END FUNCTION Ucheb
!***********************************************
 FUNCTION UchebV(dum, a,b,c,x)
  IMPLICIT NONE
  type(DBarr1),intent(IN)            :: dum
  REAL(DP), DIMENSION(:), INTENT(IN) :: c,x
  REAL(DP), OPTIONAL,INTENT(IN)      :: a,b

  REAL(DP), DIMENSION(size(x))       :: UchebV

  INTEGER(I4B)                 :: j,m
  REAL(DP), DIMENSION(size(x)) :: d,dd,sv,y,y2,crout
  LOGICAL                      :: abyes

  IF ( (PRESENT(a).and.(.not.(PRESENT(b)))) .or. (PRESENT(b).and.(.not.(PRESENT(a)))) ) &
     STOP 'UchebV : a,b must be simultaneously present or not'
  abyes = (PRESENT(a).and.PRESENT(b))
  IF (abyes) THEN
    crout = (x-a)*(x-b)
  ELSE
    crout = (x-one)*(x+one)
  ENDIF
  if (any(crout > sceps_DP)) STOP 'x not in range in UchebV'

  m=size(c)
  if (m==0) then
    UchebV=zero
    return
  else if (m==1) then
    UchebV=c(1)
    return
  endif
  IF (abyes) THEN
    y=MIN(one,MAX(-one,(two*x-a-b)/(b-a)))
  ELSE
    y=MIN(one,MAX(-one,x))
  ENDIF

  
  d=zero
  dd=zero
  y2=two*y
  do j=m,2,-1
    sv=d
    d=y2*d-dd+c(j)
    dd=sv
  enddo
  UchebV=y2*d-dd+c(1)

 END FUNCTION UchebV
!***********************************************
FUNCTION erf0(x,ioptc) result(erf_val)
  IMPLICIT NONE
  REAL(DP), INTENT(IN) :: x
  INTEGER,intent(IN)   :: ioptc ! ioptc = 0: evaluate erf
                                ! ioptc = 1: evaluate erfc
  REAL(DP)             :: erf_val

  if (ioptc==0) then
    erf_val=Spec_ERF(x)
  else if (ioptc==1) then
    erf_val=Spec_ERFC(x)
  else
    erf_val=zero
  endif
!! Local variables
!  REAL(DP)             :: x2,xab,xss,ap,kang,summ,fpmin,an,b,c,d,h,aaa
!  INTEGER(I4B) :: n
!  REAL(DP), PARAMETER :: one = 1.0_DP, half = 0.5_DP, &
!                         zero = 0.0_DP, oneh=1.5_DP, two=2.0_DP, fortwo=42.0_DP, &
!                         coof=1.12837916709551257389615890312154517_DP, &
!                         coo3=-unter,coo5=0.1_DP,coo7=one/fortwo, &
!                         vali0=.865301442621613526493447691055258608e-2_DP, &
!                         vali1=5.80501868319345330018125827038722663_DP, &
!                         valiX=1.22474487139158904909864203735294570_DP, &
!                         lngh=.572364942924700087071713675676529356_DP
!
!  x2=x*x
!  xab=abs(x)
!  xss=sign(one,x)
!  IF (xab<=vali0) then
!    erf_val=coof*x*(one+x2*(coo3+x2*(coo5+x2*coo7)))
!  ELSE IF (xab>vali0.and.xab<=valiX) then
!    ap=half
!    summ=two
!    kang=summ
!    do
!      ap=ap+one
!      kang=kang*x2/ap
!      summ=summ+kang
!      if (abs(kang) < abs(summ)*eps_DP) exit
!    enddo
!    erf_val=xss*summ*exp(-x2+log(xab)-lngh)
!  ELSE IF (xab>valiX.and.xab<vali1) then
!    FPMIN=tiny(x)/eps_DP
!    b=x2+half
!    c=one/FPMIN
!    d=one/b
!    h=d
!    aaa=zero
!    do
!      aaa=aaa+one
!      an=-aaa*(aaa-half)
!      b=b+two
!      d=an*d+b
!      if (abs(d) < FPMIN) d=FPMIN
!      c=b+an/c
!      if (abs(c) < FPMIN) c=FPMIN
!      d=one/d
!      kang=d*c
!      h=h*kang
!      if (abs(kang-one) <= eps_DP) exit
!    enddo
!    erf_val=xss*(one-exp(-x2+log(xab)-lngh)*h)
!  ELSE IF (xab>=vali1) then
!    erf_val=xss
!  ENDIF
!  IF (ioptc == 1) erf_val = one - erf_val
END FUNCTION erf0
!***********************************************
 FUNCTION erf00(x,ioptc) result(erf_val)

  IMPLICIT NONE
  REAL(DP), INTENT(IN) :: x
  INTEGER,intent(IN)   :: ioptc ! ioptc = 0: evaluate erf
                                ! ioptc = 1: evaluate erfc
  REAL(DP)             :: erf_val

  if (ioptc==0) then
    erf_val=Spec_ERF(x)
  else if (ioptc==1) then
    erf_val=Spec_ERFC(x)
  else
    erf_val=zero
  endif

!! Local variables
!  REAL(DP), PARAMETER :: c = .564189583547756_DP, one = 1.0_DP, half = 0.5_DP, &
!                         zero = 0.0_DP, fpuuuu = .448499897858964522278768435295409485_DP, &
!                         fpvvvv = 3.99163354067340958012032576905187621_DP
!  REAL(DP), PARAMETER ::  &
!           a(5) = (/ .771058495001320D-04, -.133733772997339D-02, &
!                     .323076579225834D-01,  .479137145607681D-01, &
!                     .128379167095513D+00 /),  &
!           b(3) = (/ .301048631703895D-02,  .538971687740286D-01,  &
!                     .375795757275549D+00 /),  &
!           p(8) = (/ -1.36864857382717D-07, 5.64195517478974D-01,  &
!                      7.21175825088309D+00, 4.31622272220567D+01,  &
!                      1.52989285046940D+02, 3.39320816734344D+02,  &
!                      4.51918953711873D+02, 3.00459261020162D+02 /), &
!           q(8) = (/  1.00000000000000D+00, 1.27827273196294D+01,  &
!                      7.70001529352295D+01, 2.77585444743988D+02,  &
!                      6.38980264465631D+02, 9.31354094850610D+02,  &
!                      7.90950925327898D+02, 3.00459260956983D+02 /), &
!           r(5) = (/  2.10144126479064D+00, 2.62370141675169D+01,  &
!                      2.13688200555087D+01, 4.65807828718470D+00,  &
!                      2.82094791773523D-01 /),  &
!           s(4) = (/  9.41537750555460D+01, 1.87114811799590D+02,  &
!                      9.90191814623914D+01, 1.80124575948747D+01 /)
!  REAL(DP) :: ax, bot, t, top, x2
!
!  ax = ABS(x)
!
!  IF (ax <= fpuuuu) THEN
!    t = x*x
!    top = ((((a(1)*t + a(2))*t + a(3))*t + a(4))*t + a(5)) + one
!    bot = ((b(1)*t + b(2))*t + b(3))*t + one
!    erf_val = x*(top/bot)
!  ELSE IF (ax > fpuuuu .and. ax <= fpvvvv) THEN
!    top = ((((((p(1)*ax + p(2))*ax + p(3))*ax + p(4))*ax + p(5))*ax  &
!          + p(6))*ax + p(7))*ax + p(8)
!    bot = ((((((q(1)*ax + q(2))*ax + q(3))*ax + q(4))*ax + q(5))*ax  &
!          + q(6))*ax + q(7))*ax + q(8)
!    erf_val = half + (half - EXP(-x*x)*top/bot)
!    IF (x < zero) erf_val = -erf_val
!  ELSE IF (ax > fpvvvv .and. ax < 5.8_DP) THEN
!    x2 = x*x
!    t = one / x2
!    top = (((r(1)*t + r(2))*t + r(3))*t + r(4))*t + r(5)
!    bot = (((s(1)*t + s(2))*t + s(3))*t + s(4))*t + one
!    erf_val = (c - top/(x2*bot)) / ax
!    erf_val = half + (half - EXP(-x2)*erf_val)
!    IF (x < zero) erf_val = -erf_val
!  ELSE IF (ax > 5.8_DP) THEN
!    erf_val = SIGN(one, x)
!  ENDIF
!  IF (ioptc == 1) erf_val = one - erf_val

END FUNCTION erf00

 FUNCTION COSHY(x)
  IMPLICIT NONE
  REAL(CP),intent(IN)  :: x
  REAL(CP)             :: coshy
  REAL(CP)             :: xexp,xexpi

  xexp = exp(x)
  xexpi = 1.0_DP/xexp
  coshy = 0.5_DP*(xexp+xexpi)

 END FUNCTION COSHY

 FUNCTION SINHY(x)
  IMPLICIT NONE
  REAL(CP),intent(IN)  :: x
  REAL(CP)             :: sinhy
  REAL(CP)             :: xexp,xexpi

  xexp = exp(x)
  xexpi = 1.0_DP/xexp
  sinhy = 0.5_DP*(xexp-xexpi)

 END FUNCTION SINHY

 FUNCTION TANHY(x)
  IMPLICIT NONE
  REAL(CP),intent(IN)  :: x
  REAL(CP)             :: tanhy
  REAL(CP)             :: xexp,xexpi

  xexp = exp(x)
  xexpi = 1.0_DP/xexp
  tanhy = (xexp-xexpi)/(xexp+xexpi)

 END FUNCTION TANHY
end module specfun_AC
!_____________________________________________________________________________________________________
