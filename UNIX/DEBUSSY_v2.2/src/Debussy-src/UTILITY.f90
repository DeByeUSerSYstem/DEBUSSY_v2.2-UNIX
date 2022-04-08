module chebapprox
use nano_deftyp
use specfun_AC
real(DP),allocatable,save :: cheba_x(:),cheba_xAB(:),cheba_F(:),cheba_T(:,:),cheba_c(:)
real(DP),save :: xNi,xNi2,a_lb,b_ub
integer(I4B),save :: N_ord_cheba
logical,save :: chebapp_setup_done = .false.
logical,save :: chebapp_func_done = .false.

INTERFACE chebapp
  module procedure eval_cheba_S, eval_cheba_V
END INTERFACE

contains
!***********************************************************
subroutine fill_chebax_odd(a,b,M)
implicit none
real(DP),intent(IN)     :: a,b
integer(I4B),intent(IN) :: M
integer(I4B) :: l

a_lb=a; b_ub=b
N_ord_cheba = 2*M+1
print*,M,N_ord_cheba
xNi=one/real(N_ord_cheba,DP)
xNi2=two*xNi
if (allocated(cheba_x)) deallocate(cheba_x)
allocate(cheba_x(N_ord_cheba))
if (allocated(cheba_xAB)) deallocate(cheba_xAB)
allocate(cheba_xAB(N_ord_cheba))
if (allocated(cheba_F)) deallocate(cheba_F)
allocate(cheba_F(N_ord_cheba))
if (allocated(cheba_c)) deallocate(cheba_c)
allocate(cheba_c(0:N_ord_cheba-1))
if (allocated(cheba_T)) deallocate(cheba_T)
allocate(cheba_T(N_ord_cheba,0:N_ord_cheba-1))
cheba_x = [(-Cos(pi_over_2*real(2*l - 1,DP)*xNi), l=1,N_ord_cheba)]
where(abs(cheba_x)<sceps_DP) cheba_x=zero
cheba_T(:,0)=one
cheba_T(:,1)=cheba_x
do l=2,N_ord_cheba-1
  cheba_T(:,l) = two*cheba_x*cheba_T(:,l-1)-cheba_T(:,l-2)
enddo
cheba_xAB = half*( (b-a)*cheba_x + a+b )

chebapp_setup_done = .true.

end subroutine fill_chebax_odd
!***********************************************************
subroutine fill_cheba_coe()
implicit none
integer(I4B) :: j

if (.not.chebapp_setup_done) stop 'please call fill_chebax_odd(a,b,M) first!'

do j=0,N_ord_cheba-1
  cheba_c(j) = sum(cheba_F*cheba_T(:,j))
enddo
cheba_c(0) = xNi*cheba_c(0)
cheba_c(1:N_ord_cheba-1) = xNi2*cheba_c(1:N_ord_cheba-1)

chebapp_func_done = .true.

end subroutine fill_cheba_coe
!***********************************************************
function eval_cheba_S(x)
implicit none
real(DP),intent(IN)     :: x
real(DP) :: eval_cheba_S

if (.not.chebapp_func_done) stop 'please call fill_cheba_coe() first!'

eval_cheba_S = T_cheb(dum = scal_arg, a=a_lb,b=b_ub,c=cheba_c,x=x)

end function eval_cheba_S
!***********************************************************
function eval_cheba_V(x)
implicit none
real(DP),dimension(:),intent(IN)     :: x
real(DP),dimension(size(x)) :: eval_cheba_V

if (.not.chebapp_func_done) stop 'please call fill_cheba_coe() first!'

eval_cheba_V = T_cheb(dum = vec1_arg, a=a_lb,b=b_ub,c=cheba_c,x=x)

end function eval_cheba_V
!***********************************************************
end module chebapprox
!_______________________________________________________________________________________________________

!_______________________________________________________________________________
module chebapp_g
use nano_deftyp
use specfun_AC
private
public :: cheb_X,cheb_C,cheb_ev

type(DBarr1),save :: stpd

!T_cheb(dum = stpd, c = cheb_Y_sav(1:n), x=Xcal(1:m))
! evaluates SUM_{k=1...n} c_k T_{k-1} ( x_p ), forall p = 1..m

contains
! USAGE
! evaluate cheb_X for the selected degree;
! evaluate your function (cheb_Y, externally) at points cheb_X;
! evaluate cheb_C using the cheb_X and cheb_Y;
! evaluate the approximation cheb_ev on the evaluation grid x_ev, using the cheb_C
!*******************************************************************************
function cheb_X(Nco)
implicit none
integer(I4B),intent(IN)  :: Nco
real(DP), dimension(Nco) :: cheb_X
integer(I4B) :: m
real(DP) :: z,zz

z=Pi*half/real(Nco,DP)
do m=1,Nco
  zz = two * real(m,DP) - one
  cheb_X(m) = -cos(zz*z)
enddo

end function cheb_X
!*******************************************************************************
function cheb_C(Nco,cheb_Y,cheb_Xv)
implicit none
integer(I4B),intent(IN)  :: Nco
real(DP), dimension(Nco),intent(IN) :: cheb_Y,cheb_Xv
real(DP), dimension(Nco) :: cheb_C
real(DP),dimension(Nco,Nco)  :: chebmat
integer(I4B) :: m
real(DP) :: z,zz

z=one/real(Nco,DP)
cheb_C(1)=z*sum(cheb_Y)

chebmat(:,1) = one
chebmat(:,2) = cheb_Xv
do m=3,Nco
  chebmat(:,m) = two * cheb_Xv * chebmat(:,m-1) - chebmat(:,m-2)
enddo

z=half*z
cheb_C(2:Nco) = z * matmul( cheb_Y,chebmat(:,2:Nco) )

end function cheb_C
!*******************************************************************************
!*******************************************************************************
function cheb_ev(Nco,Nev,cheb_Co,x_ev)
implicit none
integer(I4B),intent(IN)  :: Nco,Nev
real(DP), dimension(Nco),intent(IN) :: cheb_Co
real(DP), dimension(Nev),intent(IN) :: x_ev
real(DP), dimension(Nev) :: cheb_ev

!print*,x_ev(1),x_ev(size(x_ev))
cheb_ev(:) = T_cheb( dum = stpd, c = cheb_Co, x = x_ev )

end function cheb_ev
!*******************************************************************************


end module chebapp_g
!_______________________________________________________________________________
!_______________________________________________________________________________________________________
module INSTRUM_FUNNY
use nano_deftyp
use specfun_AC

integer(I4B),parameter :: ncircd180=36,ncircd=ncircd180*2
real(DP),parameter     :: awcircd=360.d0/real(ncircd,DP)

!real(DP), save :: capillary_D, wobbling_R, detector_R
real(DP), save :: capillary_R_deg, wobbling_R_deg
real(DP), parameter :: wobbling_R_deg_tol = 1.d-6

contains
!***********************************************************
function capillary_profile_V(tt,ttc)
implicit none
real(DP),intent(IN) :: ttc
real(DP),dimension(:),intent(IN) :: tt
real(DP),dimension(size(tt)) :: capillary_profile_V
real(DP) :: ttcx,wei,sscl1
integer(I4B) :: i,j,ndg

capillary_profile_V = zero
if (wobbling_R_deg < wobbling_R_deg_tol) then
  where(abs(tt-ttc)<capillary_R_deg) capillary_profile_V = sqrt(max(zero, (capillary_R_deg-(tt-ttc)) &
                                                                        * (capillary_R_deg+(tt-ttc)) ))
else
  do j=1,ncircd180
    wei=half
    if (j>1.and.j<ncircd180) wei=one
    ttcx = ttc+wobbling_R_deg*cos(j*degrees_to_radians*awcircd)
    where(abs(tt-ttcx)<capillary_R_deg) capillary_profile_V = capillary_profile_V + &
                                                  wei*sqrt(max(zero, (capillary_R_deg-(tt-ttcx)) &
                                                                   * (capillary_R_deg+(tt-ttcx)) ))
  enddo
endif
sscl1 = sum(capillary_profile_V)
sscl1 = one/max(sscl1,eps_DP)
capillary_profile_V = capillary_profile_V*sscl1

end function capillary_profile_V
end module INSTRUM_FUNNY

!***********************************************************
