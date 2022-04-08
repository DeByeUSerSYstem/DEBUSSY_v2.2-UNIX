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
module for_nlopt

integer(4),parameter :: NLOPT_GN_DIRECT                   = 0
integer(4),parameter :: NLOPT_GN_DIRECT_L                 = 1
integer(4),parameter :: NLOPT_GN_DIRECT_L_RAND            = 2
integer(4),parameter :: NLOPT_GN_DIRECT_NOSCAL            = 3
integer(4),parameter :: NLOPT_GN_DIRECT_L_NOSCAL          = 4
integer(4),parameter :: NLOPT_GN_DIRECT_L_RAND_NOSCAL     = 5
integer(4),parameter :: NLOPT_GN_ORIG_DIRECT              = 6
integer(4),parameter :: NLOPT_GN_ORIG_DIRECT_L            = 7
integer(4),parameter :: NLOPT_GD_STOGO                    = 8
integer(4),parameter :: NLOPT_GD_STOGO_RAND               = 9
integer(4),parameter :: NLOPT_LD_LBFGS_NOCEDAL            = 10
integer(4),parameter :: NLOPT_LD_LBFGS                    = 11
integer(4),parameter :: NLOPT_LN_PRAXIS                   = 12
integer(4),parameter :: NLOPT_LD_VAR1                     = 13
integer(4),parameter :: NLOPT_LD_VAR2                     = 14
integer(4),parameter :: NLOPT_LD_TNEWTON                  = 15
integer(4),parameter :: NLOPT_LD_TNEWTON_RESTART          = 16
integer(4),parameter :: NLOPT_LD_TNEWTON_PRECOND          = 17
integer(4),parameter :: NLOPT_LD_TNEWTON_PRECOND_RESTART  = 18
integer(4),parameter :: NLOPT_GN_CRS2_LM                  = 19
integer(4),parameter :: NLOPT_GN_MLSL                     = 20
integer(4),parameter :: NLOPT_GD_MLSL                     = 21
integer(4),parameter :: NLOPT_GN_MLSL_LDS                 = 22
integer(4),parameter :: NLOPT_GD_MLSL_LDS                 = 23
integer(4),parameter :: NLOPT_LD_MMA                      = 24
integer(4),parameter :: NLOPT_LN_COBYLA                   = 25
integer(4),parameter :: NLOPT_LN_NEWUOA                   = 26
integer(4),parameter :: NLOPT_LN_NEWUOA_BOUND             = 27
integer(4),parameter :: NLOPT_LN_NELDERMEAD               = 28
integer(4),parameter :: NLOPT_LN_SBPLX                    = 29
integer(4),parameter :: NLOPT_LN_AUGLAG                   = 30
integer(4),parameter :: NLOPT_LD_AUGLAG                   = 31
integer(4),parameter :: NLOPT_LN_AUGLAG_EQ                = 32
integer(4),parameter :: NLOPT_LD_AUGLAG_EQ                = 33
integer(4),parameter :: NLOPT_LN_BOBYQA                   = 34
integer(4),parameter :: NLOPT_GN_ISRES                    = 35
integer(4),parameter :: NLOPT_AUGLAG                      = 36
integer(4),parameter :: NLOPT_AUGLAG_EQ                   = 37
integer(4),parameter :: NLOPT_G_MLSL                      = 38
integer(4),parameter :: NLOPT_G_MLSL_LDS                  = 39
integer(4),parameter :: NLOPT_LD_SLSQP                    = 40
integer(4),parameter :: NLOPT_FAILURE                     = -1
integer(4),parameter :: NLOPT_INVALID_ARGS                = -2
integer(4),parameter :: NLOPT_OUT_OF_MEMORY               = -3
integer(4),parameter :: NLOPT_ROUNDOFF_LIMITED            = -4
integer(4),parameter :: NLOPT_FORCED_STOP                 = -5
integer(4),parameter :: NLOPT_SUCCESS                     = 1
integer(4),parameter :: NLOPT_STOPVAL_REACHED             = 2
integer(4),parameter :: NLOPT_FTOL_REACHED                = 3
integer(4),parameter :: NLOPT_XTOL_REACHED                = 4
integer(4),parameter :: NLOPT_MAXEVAL_REACHED             = 5
integer(4),parameter :: NLOPT_MAXTIME_REACHED             = 6
real(8),save ::  dum

end module for_nlopt
!___________________________________________________________________________________________________
module diff_chi2
use nano_deftyp

real(DP),save             :: chi2_0
real(DP),allocatable,save :: cal_0(:),wei_0(:),obs_0(:),diff_0(:),dcal_0(:)
integer(I4B)              :: npu

  contains

!*************************************************************
subroutine setup_DCHI(cal,obs,wei)
implicit none
real(DP),dimension(:),intent(IN) :: cal,obs,wei
integer(I4B)     :: n

 n = size(obs)
 if (ALLOCATED(cal_0)) deallocate(cal_0,dcal_0,wei_0,obs_0,diff_0)
 allocate(cal_0(n),wei_0(n),obs_0(n),diff_0(n),dcal_0(n))
 cal_0 = cal
 obs_0 = obs
 wei_0 = wei
 diff_0 = cal-obs
 dcal_0 = 0.0_DP
 npu=n

 chi2_0 = sum(diff_0*diff_0*wei_0)/real(n,DP)

end subroutine setup_DCHI
!*************************************************************
function eval_DCHI(calnew)
implicit none
real(DP),dimension(:),intent(IN) :: calnew
real(DP)         :: eval_DCHI

 dcal_0 = calnew-cal_0

 eval_DCHI = SUM(wei_0*dcal_0*(dcal_0+2.0_DP*diff_0))/REAL(npu,DP)

end function eval_DCHI
!*************************************************************
subroutine RESET_DCHI
implicit none

IF (allocated(cal_0)) deallocate(cal_0,wei_0,obs_0,diff_0,dcal_0)

end subroutine RESET_DCHI
  
end module diff_chi2
!___________________________________________________________________________________________________
module stray_cats
use nano_deftyp
use linalg_tools

contains

subroutine FCNV_GH(FCN,Npar,Par_000, fun0,grad0,hess0, ddpar,Par_Magn)

  IMPLICIT NONE
  REAL(CP),intent(INOUT)       :: Par_000(Npar)
  REAL(CP),optional,intent(IN) :: ddpar(2),Par_Magn(Npar)
  REAL(CP),intent(OUT)         :: fun0,grad0(Npar),hess0(Npar,Npar)
  real(CP)                     :: diaco(Npar),ParSk(Npar)
  INTEGER(I4B),intent(IN)      :: Npar
                                  
  REAL(CP)                     :: chi2_0,gof_0,a,b,std
  INTEGER(I4B)                 :: i,i1,i2,i3,i2b,kk,kk1,kk2,Noffd,Npgrid,Nvx,is,is2,iw,iw2,iagr


  REAL(CP),allocatable         :: dx(:,:),dy(:),dyc(:),mls(:,:),bls(:),mlsi(:,:),xls1(:)
  REAL(CP)                     :: wrel(2),fom,sy1,sy2,s2,s3,s4,ff

  INTERFACE
    SUBROUTINE FCN(N,P,f)
    USE nano_deftyp
    IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
    INTEGER(I4B), INTENT(IN)  :: N
    REAL(CP),INTENT(IN)       :: P(N)
    REAL(CP),INTENT(OUT)      :: f
    END SUBROUTINE FCN
  END INTERFACE


  call FCN(Npar,Par_000,fun0)
  if (.not. PRESENT(ddpar)) then
    wrel = [0.1d0,1.d0]*s4eps_DP
  else if (PRESENT(ddpar)) then
    wrel = ddpar
  endif
  if (.not. PRESENT(Par_Magn)) then
    ParSk = abs(Par_000)
  else if (PRESENT(Par_Magn)) then
    ParSk = abs(Par_Magn)
  endif
  

!_______ Memorize starting point
  chi2_0 = fun0
  print'("chi2_0",1x,g24.16)', chi2_0 
  print'(a)', 'param/stdev'
  print*, ' Npar = ',Npar

  Noffd=Npar*(Npar-1)
  Noffd=Noffd/2
  if (Npar==1) then
    Npgrid=4
    Nvx=1 + 2*Npar
  else
    Npgrid=8*Noffd+4*Npar
    Nvx= 2*Npar + Noffd
  endif
  allocate(dx(Npar,Npgrid),dy(Npgrid),dyc(Npgrid))
  dx=zero; dy=zero
  
  if (Npar==1) then
    kk=0
    do is=-1,1,2
      do iw=1,2
        kk=kk+1
        dx(1,kk)=wrel(iw)*is*ParSk(1)
        call FCN(Npar,dx(:,kk)+Par_000,ff)
        dy(kk) = ff
      enddo
    enddo
    dy=dy-fun0
    sy1=sum(dy(1:kk)*dx(1,kk))
    sy2=sum(dy(1:kk)*dx(1,kk)*dx(1,kk))
    s2=sum(dx(1,1:kk)**2)
    s3=sum(dx(1,1:kk)**3)
    s4=sum(dx(1,1:kk)**4)
    grad0(1)=(s4*sy1-s3*sy2)/(s2*s4-s3*s3)
    hess0(1,1)=two*(s2*sy2-s3*sy1)/(s2*s4-s3*s3)
  else
    allocate(mls(Nvx,Nvx),bls(Nvx),mlsi(Nvx,Nvx),xls1(Nvx))
    mls=zero;bls=zero
    kk=0
    do i1=1,Npar
      do is=-1,1,2
        do iw=1,2
          kk=kk+1
          dx(i1,kk)=wrel(iw)*is*ParSk(i1)
          call FCN(Npar,dx(:,kk)+Par_000,ff)
          dy(kk) = ff
        enddo
      enddo
    enddo
    do i1=1,Npar-1
      do i2=i1+1,Npar
        do is=-1,1,2
          do is2=-1,1,2
            do iw=1,2
              kk=kk+1
              dx(i1,kk)=wrel(iw)*is*ParSk(i1)
              dx(i2,kk)=wrel(iw)*is2*ParSk(i2)
              call FCN(Npar,dx(:,kk)+Par_000,ff)
              dy(kk) = ff
            enddo
          enddo
        enddo
      enddo
    enddo
    dy = dy - fun0
    print*,'# Number of function evaluations done / planned : ', kk,Npgrid
    if (ANY(Isnan(dx))) print*,'NAN in dx'
    if (ANY(Isnan(dy))) print*,'NAN in dy'
    kk1=0
    do i1=1,Npar
      kk1=kk1+1
      bls(kk1)=sum(dy(:)*dx(i1,:))
      kk2=0
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:))
      enddo
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum(dx(i1,:)*(dx(i2,:)**2))
      enddo
      do i2=1,Npar-1;do i3=i2+1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i3,:))
      enddo;enddo
    enddo
    print*,'# Number of variables set / planned : ', kk2,Nvx
    do i1=1,Npar
      kk1=kk1+1
      bls(kk1)=half*sum(dy(:)*(dx(i1,:)**2))
      kk2=0
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum((dx(i1,:)**2)*dx(i2,:))
      enddo
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=unqua*sum((dx(i1,:)**2)*(dx(i2,:)**2))
      enddo
      do i2=1,Npar-1;do i3=i2+1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum((dx(i1,:)**2)*dx(i2,:)*dx(i3,:))
      enddo;enddo
    enddo
    do i1=1,Npar-1
      do i2=i1+1,Npar
        kk1=kk1+1
        bls(kk1)=sum(dy(:)*dx(i1,:)*dx(i2,:))
        kk2=0
        do i2b=1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i2b,:))
        enddo
        do i2b=1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=half*sum(dx(i1,:)*dx(i2,:)*(dx(i2b,:)**2))
        enddo
        do i2b=1,Npar-1;do i3=i2b+1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i2b,:)*dx(i3,:))
        enddo;enddo
      enddo
    enddo
    mlsi = PSEUDO_INV(A=mls,tol=sceps_DP,posdef=0)
    if (ANY(ISNAN(bls))) print*, ' NAN in bls'
    if (ANY(ISNAN(mlsi))) print*, ' NAN in mlsi'
    xls1=matmul(mlsi,bls)
    if (ANY(ISNAN(xls1))) print*, ' NAN in xls1'
    grad0=xls1(1:Npar)
    diaco=xls1(Npar+1:2*Npar)
!    print*,'max/min diaco: ',maxval(diaco),minval(diaco)
    kk=2*Npar
    do i2=1,Npar-1
      hess0(i2,i2)=diaco(i2)
      do i3=i2+1,Npar
        kk=kk+1
        hess0(i2,i3)=xls1(kk)
        hess0(i3,i2)=hess0(i2,i3)
      enddo
    enddo
    hess0=two*hess0
  endif
  deallocate(dx,dy,mls,bls,mlsi,xls1)

end subroutine FCNV_GH
!****************************************************************************************************
subroutine NEWTON_NUM(FCN,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Par_fin,Func_value,Check_conv, ddpar,Par_Magn, Par_Err,Par_Corl)
   IMPLICIT NONE
   REAL(CP), PARAMETER          :: R_coe=2.0_dp, C_coe=one/R_coe
   INTEGER(I4B),intent(IN)      :: Npar
   REAL(CP),intent(IN)          :: Par_ini(Npar), Par_LB(Npar), Par_UB(Npar), Func_Tol
   REAL(CP),intent(OUT)         :: Par_fin(Npar), Func_value
   REAL(CP),optional,intent(IN) :: ddpar(2),Par_Magn(Npar)
   REAL(CP),optional,intent(OUT):: Par_Err(Npar),Par_Corl(Npar,Npar)
   
   REAL(CP)                     :: ddpar1(2),Par_Magn1(Npar)
   REAL(CP)                     :: par_curr(Npar),par_new(Npar), eigval(Npar),eigvec(Npar,Npar), &
                                   fun0,grad0(Npar), hess0(Npar,Npar), dpar(Npar),iHess0(Npar,Npar), &
                                   eivgra(Npar),eigvalaig(Npar),dbou(Npar)
   REAL(CP)                     :: dlam_min,dlam_add,fun1,shiftn,gradn,minei,dfun,tolcomp,s2,s3,s4,bestf,ccr(3)
   INTEGER(I4B)                 :: valfun, Npar2
   INTEGER(I4B)                 :: i,i2, iacco
   LOGICAL, INTENT(OUT)         :: Check_Conv
   LOGICAL :: exit1(2),critc(3),critpos,notstuckatbou(Npar)

   INTERFACE
     SUBROUTINE FCN(N,P,chisquared)
        USE nano_deftyp

        IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
        INTEGER(I4B), INTENT(IN)  :: N
        REAL(CP),INTENT(IN)       :: P(N)
        REAL(CP),INTENT(OUT)      :: chisquared
     END SUBROUTINE FCN
   END INTERFACE

  Check_Conv = ALL([ PRESENT(Par_Err),PRESENT(Par_Corl) ])
  if (.not.Check_Conv) then
    print*,'NEWTON_NUM : Par_Err, Par_Corl must be BOTH SIMULTANEOUSLY present or not present!!'
    stop 'NEWTON_NUM : Par_Err, Par_Corl must be BOTH SIMULTANEOUSLY present or not present!!'
  endif
  Check_Conv = .false.
  if (.not. PRESENT(ddpar)) then
    ddpar1 = [0.1d0,1.d0]*s4eps_DP
  else if (PRESENT(ddpar)) then
    ddpar1 = ddpar
  endif
  if (.not. PRESENT(Par_Magn)) then
    Par_Magn1 = half * max( abs(Par_UB-Par_LB), abs(Par_UB+Par_LB) )
  else if (PRESENT(Par_Magn)) then
    Par_Magn1 = abs(Par_Magn)
  endif


  tolcomp = Npar*eps_DP
  par_curr=max(Par_LB,min(Par_UB,Par_ini))
  bestf=99.d99
  iacco=0
  do
    call FCNV_GH(FCN=FCN,Npar=Npar,Par_000=Par_curr, fun0=fun0,grad0=grad0,hess0=hess0, ddpar=ddpar1, Par_Magn=Par_Magn1)
    if (bestf>fun0) then
      iacco=1
      print*,'best ',fun0,par_curr
      call flush()
      bestf=fun0
    else
      iacco=0
    endif
    call DIAGONALY(a_in=hess0, d=eigval, z=eigvec)
    minei=minval(eigval)
    dlam_min=max(sceps_DP, -minei)
    dlam_add=sceps_DP * C_coe
    eivgra=matmul(transpose(eigvec),grad0)
    do
      dlam_add=dlam_add*R_coe
      eigvalaig = -eivgra/(eigval+dlam_min+dlam_add)
      dpar=zero
      do i=1,Npar
        dpar=dpar+eigvec(:,i)*eigvalaig(i)
      enddo
      par_new=par_curr+dpar
      exit1(1) = ALL(par_new<=Par_UB .and. par_new>=Par_LB)
      if (.not.exit1(1)) cycle
      call FCN(Npar,par_new,fun1)
      exit1(2) = (fun1<fun0)
      if (ALL(exit1)) then
        par_curr=par_new
        dfun=fun0-fun1
        where (dpar > zero) 
          dbou = par_UB-par_curr
        elsewhere
          dbou = par_curr-par_LB
        end where
        notstuckatbou = (dbou > tolcomp)
        shiftn=sum(dpar**2,mask=notstuckatbou)
        gradn=sum(grad0**2,mask=notstuckatbou)
        exit
      endif
    enddo
    ccr=[shiftn / tolcomp, gradn / tolcomp, dfun / Func_Tol]
    critc(1) = (shiftn <= tolcomp)
    critc(2) = .true.!(gradn <= 100.d0*tolcomp)
    critc(3) = (dfun <= Func_Tol)
    if (iacco==1) print*,'crit: ',ccr,minei
    critpos = .true.
    IF (ALL(dbou>tolcomp)) critpos = (minei > -eps_DP)
    if (.not.critpos) cycle
    if (ALL(critc)) then
      Check_conv=.true.
      Func_value=fun1
      Par_fin = par_curr
      if (PRESENT(Par_Err)) then
        iHess0 = PSEUDO_INV(A=hess0,tol=sceps_DP,posdef=1)
        Par_Err = fun1 * [( sqrt(max(zero,iHess0(i,i))), i=1,Npar )]
        Par_Corl=zero
        do i2=1,Npar
          Par_Corl(i2,i2)=one
        enddo
        do i=1,Npar
          if (Par_Err(i)<eps_DP) cycle
          do i2=1,Npar
            if (Par_Err(i2)<eps_DP) cycle
            if (i2==i) cycle
            Par_Corl(i,i2)=iHess0(i,i2)/(Par_Err(i2)*Par_Err(i))
          enddo
        enddo
      endif
      exit
    endif
  enddo
end subroutine NEWTON_NUM
!****************************************************************************************************
end module stray_cats
!___________________________________________________________________________________________________


module POLYTOPE
use for_nlopt
use nano_deftyp
use stray_cats
use diff_chi2 
use linalg_tools
use specfun_AC
   INTEGER(I4B),save                 :: stopanyway=1
   INTEGER(I4B),save                 :: icycle
   REAL(DP),parameter                :: target_E_O = 0.0009_DP
   integer(I4B),save :: diag_modus = 1
   REAL(CP), PARAMETER               :: deltafr = 0.0005_DP, deltaxr = 0.00001_DP, dectol=-0.05_DP*deltafr
   REAL(DP),save                     :: start_temp = 1.0_DP
   REAL(CP),save                     :: deltafr1=deltafr , deltaxr1=deltaxr , dectol1=dectol
   REAL(DP),allocatable,save         :: StDev_Vec(:),ParVal_Vec(:),Correl_Matx(:,:)
   

CONTAINS
!***************

!*******************************************************************************
!*******************************************************************************
subroutine NLOPT_NELDMEA(FCN_NLOPT,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_value,Check_conv)
use nano_deftyp
use for_nlopt
IMPLICIT NONE
INTEGER(I4B),intent(IN)      :: Npar
REAL(CP),intent(IN)          :: Par_ini(Npar), Par_LB(Npar), Par_UB(Npar), Func_Tol
REAL(CP),intent(OUT)         :: Par_fin(Npar), Func_value
INTEGER(I4B),intent(INOUT)   :: Max_Func
logical, intent(OUT) :: Check_conv

real(DP) :: grad(Npar)
integer(8) :: opt  
!real(DP) ::  dum
integer(I4B) :: ires 

external FCN_NLOPT

!INTERFACE
!  subroutine FCN_NLOPT(v, np, p, grad, need_gradient, dumm)
!  INTEGER(4),INTENT(IN)              :: np, need_gradient
!  REAL(8),DIMENSION(np),INTENT(IN)    :: p
!  REAL(8),INTENT(OUT)                 :: v, grad(np)
!  REAL(8),INTENT(INOUT)               :: dumm
!  END SUBROUTINE FCN_NLOPT
!END INTERFACE

opt = 0

call nlo_create(opt, NLOPT_LN_NELDERMEAD, Npar) 

print*,'NELMEA: Value of opt after call nlo_create : ',opt

CALL FCN_NLOPT(Func_value,Npar,Par_ini,grad,0,dum)
!print*,Par_ini,Func_value,opt

!call nlo_get_lower_bounds(ires, opt, Par_LB) 

call nlo_set_lower_bounds( ires, opt, Par_LB)
call nlo_set_upper_bounds( ires, opt, Par_UB)
call nlo_set_min_objective(ires, opt, FCN_NLOPT, 0)

call nlo_optimize(ires, opt, Par_ini, Func_value)

call nlo_destroy(opt) 
Check_conv=.true.

end subroutine NLOPT_NELDMEA
!*******************************************************************************
subroutine NLOPT_BOBYQA(FCN_NLOPT,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_value,Check_conv)
use nano_deftyp
use for_nlopt
IMPLICIT NONE
INTEGER(I4B),intent(IN)      :: Npar
REAL(CP),intent(IN)          :: Par_ini(Npar), Par_LB(Npar), Par_UB(Npar), Func_Tol
REAL(CP),intent(OUT)         :: Par_fin(Npar), Func_value
INTEGER(I4B),intent(INOUT)   :: Max_Func
logical, intent(OUT) :: Check_conv

real(DP) :: grad(Npar)
integer(8) :: opt  
integer(I4B) :: ires 

!external myfunc
external FCN_NLOPT

!INTERFACE
!  subroutine FCN_NLOPT(v, np, p, grad, need_gradient, dumm)
!  INTEGER(4),INTENT(IN)              :: np, need_gradient
!  REAL(8),DIMENSION(np),INTENT(IN)    :: p
!  REAL(8),INTENT(OUT)                 :: v, grad(np)
!  REAL(8),INTENT(INOUT)               :: dumm
!  END SUBROUTINE FCN_NLOPT
!END INTERFACE

print*, ' ----------------------------------------------------------------'
print*, '>>>>>>   ENTERING BOBYQUA'
print*, ' ----------------------------------------------------------------'

opt = 0
dum=zero

call nlo_create(opt, NLOPT_LN_BOBYQA, Npar) 

print*,'BOBYQA: Value of opt after call nlo_create : ',opt

CALL FCN_NLOPT(Func_value,Npar,Par_ini,grad,0,dum)
print*,Par_ini
print*,Func_value,opt
print*,1111
!call nlo_get_lower_bounds(ires, opt, Par_LB) 
!print*,2222

call nlo_set_lower_bounds(ires, opt, Par_LB)
print*,3333
call nlo_set_upper_bounds(ires, opt, Par_UB)
print*,4444
call nlo_set_min_objective(ires, opt, FCN_NLOPT,0)
print*,5555,opt
Par_fin=Par_ini
call nlo_set_xtol_rel(ires, opt, 1.D-4)
call nlo_optimize(ires, opt, Par_fin, Func_value)
print*,9999

call nlo_destroy(opt) 
Check_conv=.true.

end subroutine NLOPT_BOBYQA
!*******************************************************************************
!**********************************************************************************************************************************
!**********************************************************************************************************************************
 subroutine SimAnn(FCN,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_value,Check_conv,Npara,IPTOP, &
                      Starting_T, Target_Min_F, Cooling_Factor)
!________________________ Monte Carlo search
   IMPLICIT NONE
   INTEGER(I4B),intent(IN)      :: Npar
   REAL(CP),intent(IN)          :: Par_ini(Npar), Par_LB(Npar), Par_UB(Npar), Func_Tol
   REAL(CP),intent(OUT)         :: Par_fin(Npar), Func_value
   INTEGER(I4B),intent(INOUT)   :: Max_Func
   INTEGER(I4B),optional,intent(IN)      :: Npara
   REAL(CP),dimension(:,:),optional,intent(in) :: IPTOP
   REAL(CP),optional,intent(IN)      :: Starting_T, Target_Min_F, Cooling_Factor
   LOGICAL, INTENT(OUT)         :: Check_Conv
   
   real(CP) :: Starting_Temp, Target_Min_Func, Cooling_Factor_1,Cooling_Factor_2, best_init, worst_init, ave_init, std_init, &
               Temp, eee, eeei, iTemp, redrange, Ynew,z,zc, &
               ave,std,slope, sumk, sumk2, invdet, misis(2,2),yv(2),sv(2),xfx
   
   REAL(CP)                     :: Xrand(Npar), Xrand2(Npar), Xtoss(Npar),Xnew(Npar),XPoly(Npar,2*Npar), YPoly(2*Npar), &
                                   Xcen(Npar), Xr(Npar), Xe(Npar), Xsh(Npar), relvar(Npar),pave(Npar), range_width(Npar), &
                                   search_range(2,Npar),Xc(Npar), Xold(Npar), Ycen, Yr,Ye,Yc, Ymed, Yd2, dxl, dxu, cfr_con
   REAL(CP)                     :: xNpar,xNpar2,xiNpar,xiNpar2,cfr_con2,vrand,dYnew
   REAL(CP), PARAMETER          :: R_coe=1.0_dp, C_coe=0.5_dp, E_coe=2.0_dp
   INTEGER(I4B)                 :: valfun, Npar2,iexit,ntstep,ncypstep,kbyt,kTemp,kacc
   INTEGER(I4B)                 :: i, iacco, ibest(1), iworst(1), cptmax,kok
   logical                      :: accept_it,is_best,is_good

   INTERFACE
     SUBROUTINE FCN(N,P,chisquared)
        USE nano_deftyp

        IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
        INTEGER(I4B), INTENT(IN)  :: N
        REAL(CP),INTENT(IN)       :: P(N)
        REAL(CP),INTENT(OUT)      :: chisquared
     END SUBROUTINE FCN
   END INTERFACE
  
 
  cptmax=Npar**2
  Check_Conv=.false.
  eee=exp(one)
  eeei=exp(-one)
  Target_Min_Func=one ! for GOF
  IF (PRESENT(Target_Min_F)) then
    Target_Min_Func=Target_Min_F
  ENDIF
  Cooling_Factor_1=0.5d0
  IF (PRESENT(Cooling_Factor)) then
    Cooling_Factor_1=MIN(Cooling_Factor,0.9999d0)
  ENDIF
  Cooling_Factor_2=0.8d0
   
  iacco=0                                            
  IF (present(Npara).and.present(IPTOP)) then
    IF (size(IPTOP,1)/=Npar .or. size(IPTOP,2)/=Npara) STOP 'Simul_Ann : sizing of Initial Set!'
    iacco = size(IPTOP,1) !MIN(Npara,10*Npar-1)                     
  ENDIF

! calculate  2*Npar sets of Npar parameters and, for each of them, the corresponding function value (CHISQ)
  Npar2 = 2*Npar
  sumk  = half*REAL(Npar2*(Npar2+1),DP)
  sumk2 = unses*REAL(Npar2*(Npar2+1)*(2*Npar2+1),DP)
  xNpar2 = REAL(Npar2,DP)
  xNpar = REAL(Npar,DP)
  invdet=one/(sumk2*xNpar2-sumk*sumk)
  misis(1,:)=[xNpar2,-sumk]
  misis(2,:)=[-sumk, sumk2]
  misis=misis*invdet
  
  xiNpar2 = one/xNpar2
  xiNpar = one/xNpar
  xfx=one+sqrt(xiNpar2)

! check initial parameters with respect to bounds
  
  redrange = eeei * xiNpar
  range_width = (Par_UB(:) - Par_LB(:))*redrange
  search_range(1,:) = Par_ini-half*range_width
  search_range(2,:) = Par_ini+half*range_width
  
  where (search_range(1,:) < Par_LB)
    search_range(1,:)=Par_LB
    search_range(2,:) = Par_LB+range_width
  end where
  where (search_range(2,:) > Par_UB)
    search_range(2,:)=Par_UB
    search_range(1,:) = Par_UB-range_width
  end where
  
  call FCN(Npar,Xpoly(:,1),Ypoly(1))
! continue with given point set (if any)
  if (present(Npara).and.present(IPTOP)) then
     Xpoly(:,1:iacco) = IPTOP(:,1:iacco)
     do i=1,iacco
       call FCN(Npar,Xpoly(:,i),Ypoly(i))
     enddo
  else
    iacco=0
  endif
! finish - do up to Npar2 with random points
  do i=iacco+1,Npar2-1
    call RANDOM_NUMBER(Xrand)
    Xpoly(:,i) = search_range(1,:) + Xrand(:)*range_width
    call FCN(Npar,Xpoly(:,i),Ypoly(i))
  enddo
  Xpoly(:,Npar2) = MIN(MAX(Par_ini,Par_LB),Par_UB)
  call FCN(Npar,Xpoly(:,Npar2),Ypoly(Npar2))
  print*,'Starting Y, X : ',Ypoly(Npar2),Xpoly(:,Npar2)
  do i=1,Npar2
    print*,'Initial set Y ',i,Ypoly(i)
  enddo

  if (PRESENT(Starting_T)) then
    Starting_Temp=Starting_T
  ELSE
    best_init = minval(Ypoly(1:Npar2))
    worst_init= maxval(Ypoly(1:Npar2))
    ave_init  = sum(Ypoly(1:Npar2))*xiNpar2
    std_init  = sqrt( sum((Ypoly(1:Npar2)-ave_init)**2)*xiNpar2 )
    print*, 'Random start: best, worst, ave, std = ',best_init,worst_init,ave_init,std_init
    Starting_Temp = (best_init-one)*eeei*eeei !ave_init !max(half*std_init, unter*(worst_init-best_init))
  ENDIF
  ntstep = ceiling(log(Func_Tol/Starting_Temp)/log(Cooling_Factor_1))
  print*, 'Simann: starting T, #T steps = ',Starting_Temp,ntstep
  ncypstep=nint(Max_Func/real(ntstep,DP))
  print*,ntstep,ncypstep
  call flush()
! __________ INITIALIZE OUPUT ...

  ibest = minloc(Ypoly)
  if (ibest(1)<Npar2) then
  ! exchange if needed ...
    Xrand=Xpoly(:,ibest(1))
    Xpoly(:,ibest(1)) = Xpoly(:,Npar2)
    Xpoly(:,Npar2) = Xrand
    Func_value = Ypoly(ibest(1))
    Ypoly(ibest(1)) = Ypoly(Npar2)
    Ypoly(Npar2) = Func_value
  endif
  
  Par_fin = Xpoly(:,Npar2)
  Func_value = Ypoly(Npar2)
  ibest = Npar2
  print*, 'Simann: starting Y = ',Func_value
  
  valfun=0
  kTemp=1
  Temp = Starting_Temp
  Xe=max(eps_DP,(Par_UB-Par_LB)*half)
  KOOLANDTHEGANG: DO
    iTemp = one / Temp
    print*, 'Simann: beginning T = ',Temp,Ypoly(Npar2)
    kbyt=0
    kacc=0
    BARBECUE: do
      kbyt=kbyt+1
      Xcen = Xpoly(:,Npar2)
      Ycen = Ypoly(Npar2)
      print*,'Cycle ',kbyt,' func. = ',Ycen
      do
        call RANDOM_NUMBER(Xrand)
        kok=count(Xrand<xiNpar)
        if (kok>0) exit
      enddo
      call RANDOM_NUMBER(Xrand2)
      call RANDOM_NUMBER(vrand)
      Xnew=Xcen
      where (Xrand<xiNpar) Xnew=search_range(1,:)+Xrand2*range_width
      call FCN(Npar,Xnew,Ynew)
      valfun=valfun+1
      dYnew = Ynew-Ycen
      is_good = (dYnew<=zero)
      accept_it = is_good.or.((dYnew>zero).and.(vrand < exp(-iTemp*dYnew)))
      
! _____ Decision
      if (accept_it) then
        kacc=kacc+1
        print*, 'Accepting : ',Ynew,Ycen,' @ T = ',Temp,kTemp;  call flush()
        iworst = MAXLOC(Ypoly)
        if (iworst(1)<Npar2) then
          Xpoly(:,iworst(1):Npar2-1) = Xpoly(:,iworst(1)+1:Npar2)
          Ypoly(iworst(1):Npar2-1) = Ypoly(iworst(1)+1:Npar2)
        endif
        Xpoly(:,Npar2) = Xnew
        Ypoly(Npar2)   = Ynew
        if (Ynew<Func_value) then
          Par_fin    = Xnew
          Func_value = Ynew
          ibest = Npar2
        else
          ibest = MINLOC(Ypoly)
        endif
        
! ______ Search range shift (if accepted)
        search_range(1,:) = Xnew-half*range_width
        search_range(2,:) = Xnew+half*range_width
        where (search_range(1,:) < Par_LB)
          search_range(1,:)=Par_LB
          search_range(2,:) = Par_LB+range_width
        end where
        where (search_range(2,:) > Par_UB)
          search_range(2,:)=Par_UB
          search_range(1,:) = Par_UB-range_width
        end where
      endif
      if (.not.accept_it) cycle
! ______ Exit criteria
      if (valfun>Max_Func) then
        print*,'Function Evaluations Reached Maximum: ',valfun,' : EXIT'
        EXIT KOOLANDTHEGANG
      endif
      ave=sum(Ypoly)*xiNpar2
      std  = sqrt( sum((Ypoly-ave)**2)*xiNpar2 )
      print*,'  @ T = ',kTemp,Temp,': av/sd = ',ave,std
      if (kacc>Npar2.and.std <=Temp) then
        iexit=0
        exit BARBECUE
      endif
      if (kacc>Npar2.and.ave-one < half*Temp) then
        iexit=1
        exit BARBECUE
      endif
      if (kacc>Npar2.and.kbyt>cptmax) then
        iexit=2
        exit BARBECUE
      endif
      
      iworst = MAXLOC(Ypoly)
      if (kacc>Npar2.and.Ypoly(iworst(1)) - Func_value < Temp) then
        iexit=3
        exit BARBECUE
      endif
      
      pave=[(sum(Xpoly(i,:))*xiNpar2,i=1,Npar)]
      relvar=[(sum((Xpoly(i,:)-pave)**2)*xiNpar2,i=1,Npar)]/pave**2
      IF (kacc>Npar2.and.ALL(relvar<sceps_DP)) then
        iexit=4
        exit BARBECUE
      endif
    enddo BARBECUE
    print*,'Exit T',Temp,Func_value,' exit mode = ',iexit
! ___ Update T    
    kTemp=kTemp+1
    Temp = Temp * Cooling_Factor_1
    range_width = range_width * Cooling_Factor_2
    print*,'Changing T to ',Temp,kTemp
    
    search_range(1,:) = Par_fin-half*range_width
    search_range(2,:) = Par_fin+half*range_width
    where (search_range(1,:) < Par_LB)
      search_range(1,:)=Par_LB
      search_range(2,:) = Par_LB+range_width
    end where
    where (search_range(2,:) > Par_UB)
      search_range(2,:)=Par_UB
       search_range(1,:) = Par_UB-range_width
    end where
! ___ Check T 
    if (Temp <= Func_Tol) EXIT KOOLANDTHEGANG
  enddo KOOLANDTHEGANG
  
  Check_Conv=.true.

end subroutine SimAnn
!**********************************************************************************************************************************
 subroutine POLYTO(FCN,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_value,Check_conv,Npara,IPTOP)
   IMPLICIT NONE
   INTEGER(I4B),intent(IN)      :: Npar
   REAL(CP),intent(IN)          :: Par_ini(Npar), Par_LB(Npar), Par_UB(Npar), Func_Tol
   REAL(CP),intent(OUT)         :: Par_fin(Npar), Func_value
   INTEGER(I4B),intent(INOUT)   :: Max_Func
   INTEGER(I4B),optional,intent(IN)      :: Npara
   REAL(CP),dimension(:,:),optional,intent(in) :: IPTOP
   
   REAL(CP)                     :: Xrand(Npar), XPoly(Npar,2*Npar), YPoly(2*Npar), Xcen(Npar), Xr(Npar), Xe(Npar),&
                                   Xc(Npar), Xold(Npar), Ycen, Yr,Ye,Yc, Ymed, Yd2, dxl, dxu, cfr_con
   REAL(CP)                     :: xNpar2,xiNpar2,cfr_con2
   REAL(CP), PARAMETER          :: R_coe=1.0_dp, C_coe=0.5_dp, E_coe=2.0_dp
   INTEGER(I4B)                 :: valfun, Npar2
   INTEGER(I4B)                 :: i, iacco
   LOGICAL, INTENT(OUT)         :: Check_Conv

   INTERFACE
     SUBROUTINE FCN(N,P,chisquared)
        USE nano_deftyp

        IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
        INTEGER(I4B), INTENT(IN)  :: N
        REAL(CP),INTENT(IN)       :: P(N)
        REAL(CP),INTENT(OUT)      :: chisquared
     END SUBROUTINE FCN
   END INTERFACE

   iacco=0
   IF (present(Npara).and.present(IPTOP)) then
     IF (size(IPTOP,1)/=Npar .or. size(IPTOP,2)/=Npara) STOP 'POLYTO:sizing!'
     iacco = MIN(Npara,2*Npar-1)
   ENDIF

! calculate  2*Npar sets of Npar parameters and, for each of them, the corresponding function value (CHISQ)
  Npar2 = 2*Npar
  xNpar2 = REAL(Npar2,DP)
  xiNpar2 = 1.0_DP/xNpar2
  Xpoly(:,1) = Par_ini(:)

! check initial parameters with respect to bounds
  do i=1,Npar
     Xpoly(i,1) = MAX(Xpoly(i,1),Par_LB(i)+eps_DP)
     Xpoly(i,1) = MIN(Xpoly(i,1),Par_UB(i)-eps_DP)
  enddo
  call FCN(Npar,Xpoly(:,1),Ypoly(1))
  do i=2,iacco+1,1
     Xpoly(:,i) = IPTOP(:,i-1)
     call FCN(Npar,Xpoly(:,i),Ypoly(i))
  enddo
  do i=iacco+2,Npar2,1
     call RANDOM_NUMBER(Xrand)
     Xpoly(:,i) = Par_LB(:) + Xrand(:)*(Par_UB(:) - Par_LB(:) )
     call FCN(Npar,Xpoly(:,i),Ypoly(i))
  enddo

  valfun = 0
  icycle = 0
  Check_Conv = .False.
  MAIN_LOOP: do
      call SORT_POLYTOPE(Ypoly,Xpoly)                                   ! sorts Xpoly/Ypoly in increasing order of function values
      Xcen(:) = (/(SUM( Xpoly(i,1:Npar2-1)),i=1,Npar)/) / (Npar2 - 1)   ! find the centroid of all points but the last (max value)
!
      Xr(:) = Xcen(:) + R_coe * ( Xcen(:) - Xpoly(:,Npar2) )            ! reflection point
      Xold = Xcen
      call Check_Bounds(Npar,Xr,Xold,Par_LB,Par_UB)
      call FCN(Npar,Xr,Yr)
      valfun = valfun + 1
      IF (valfun > Max_Func) EXIT MAIN_LOOP
      IF (Yr <= Ypoly(1) ) THEN 
          Xe(:) = Xcen(:) + E_coe * ( Xr(:) - Xcen(:) )                 ! expansion point
          Xold = Xcen
          call Check_Bounds(Npar,Xe,Xold,Par_LB,Par_UB)
          call FCN(Npar,Xe,Ye)
          valfun = valfun + 1
          IF (valfun > Max_Func) EXIT MAIN_LOOP
          IF (Ye <= Ypoly(1) ) THEN 
              Xpoly(:,Npar2) = Xe
              Ypoly(Npar2) = Ye
          ELSE
              Xpoly(:,Npar2) = Xr
              Ypoly(Npar2) = Yr
          ENDIF
      ELSE 
          IF (Yr > Ypoly(Npar2-1) ) THEN 
              IF (Yr <= Ypoly(Npar2) ) THEN 
                  Xpoly(:,Npar2) = Xr
                  Ypoly(Npar2) = Yr
              ENDIF
              Xc(:) = Xcen(:) * (1.0_DP - C_coe) + C_coe * Xpoly(:,Npar2)   ! contraction point
              Xold = Xcen 
              call Check_Bounds(Npar,Xc,Xold,Par_LB,Par_UB)
              call FCN(Npar,Xc,Yc)
              valfun = valfun + 1
              IF (valfun > Max_Func) EXIT MAIN_LOOP
              IF (Yc <= Ypoly(Npar2) ) THEN 
                  Xpoly(:,Npar2) = Xc
                  Ypoly(Npar2) = Yc
              ELSE
                  do i=2,Npar2                                               ! contraction of all points
                     Xpoly(:,i) = Xpoly(:,1)  + C_coe * ( Xpoly(:,i) - Xpoly(:,1) )
                     call FCN(Npar,Xpoly(:,i),Ypoly(i))
                     valfun = valfun + 1
                     IF (valfun > Max_Func) EXIT MAIN_LOOP
                  enddo
               ENDIF
          ELSE
               Xpoly(:,Npar2) = Xr
               Ypoly(Npar2) = Yr
          ENDIF
      ENDIF
      icycle = icycle + 1
      IF (MODULO(icycle,Npar) == 0) THEN
! check for convergence
         call SORT_POLYTOPE(Ypoly,Xpoly)
         Ymed = SUM(Ypoly)

         Yd2 = sqrt(SUM(Ypoly*Ypoly)*xNpar2)     
         Yd2 = sqrt(ABS(Yd2-Ymed)*(Yd2+Ymed))
         Ymed = Ymed * xiNpar2

         cfr_con = MAX(ABS(Ypoly(Npar2) - Ypoly(1))/ABS(Ymed), Yd2)
         cfr_con2 = zero
         do i=2,Npar2
           cfr_con2 = MAX(cfr_con2,maxval(abs(Xpoly(:,1)-Xpoly(:,i))))
         enddo
         IF ( cfr_con <= Func_Tol ) THEN
            Check_Conv = .True.
            EXIT MAIN_LOOP
         ENDIF

      ENDIF
      
  enddo MAIN_LOOP 
  
  call SORT_POLYTOPE(Ypoly,Xpoly)                                   ! sorts Xpoly/Ypoly in increasing order of function values
  Par_fin(:) = Xpoly(:,1)
  Func_value = Ypoly(1)      
  Max_Func = valfun

 end subroutine POLYTO

!**********************************************************************************************************************************
 subroutine SORT_POLYTOPE(v1,v2)
   IMPLICIT NONE
   REAL(CP),intent(INOUT)  :: v1(:),v2(:,:)  !! v1(m) = chi^2 values
                                             !! v2(npar,m) = polytope
                                             !! npar is the number of parameters
                                             !! m is the number of points
   real(CP)                :: x,sx(size(v2,1))
   integer                 :: KKK,Ndym,Ndymm1,I,Ip1,Iflag,npar,m

!!!!! SORTS IN ASCENDING (increasing) ORDER OF v1 both vectors

   npar = SIZE(v2,1)
   m = SIZE(v1,1)
   IF (m /= SIZE(v2,2)) STOP 'SORT_POLYTOPE : wrong dim.!'


   KKK=m
   Iflag=1
   DO
     IF (Iflag == 0) exit
     Ndym=KKK
     Iflag=0
     Ndymm1=Ndym-1
     DO I=1,Ndymm1
       Ip1=I+1
       IF (v1(i)>v1(Ip1)) THEN
         x=v1(I)
         v1(I)=v1(Ip1)
         v1(Ip1)=x
         sx=v2(:,I)
         v2(:,I)=v2(:,Ip1)
         v2(:,Ip1)=sx
         KKK=I
         Iflag=1
       ENDIF
     ENDDO
   ENDDO

 END subroutine SORT_POLYTOPE

!*************
 subroutine Check_Bounds(Npar,Xp,Xcen,Par_LB,Par_UB)
   IMPLICIT NONE
   INTEGER(I4B),intent(IN)      :: Npar
   REAL(CP),intent(IN)          :: Xcen(Npar), Par_LB(Npar), Par_UB(Npar)
   REAL(CP),intent(INOUT)       :: Xp(Npar)
   REAL(CP)                     :: damp_fact, damp_min, shift_lim, shift_par(Npar), eps_shift,xrn
   INTEGER(I4B)                 :: i

   IF (ALL(Xp >= Par_LB+eps_CP) .and. ALL(Xp <= Par_UB-eps_CP)) RETURN

   damp_fact = one
   damp_min = one
   shift_par = Xp - Xcen
   do i=1,Npar
      eps_shift = MAX(abs(Xp(i)),one)*eps_DP
      IF ( Xp(i) < Par_LB(i)+eps_CP .and. shift_par(i) < -eps_shift) THEN
           shift_lim = Par_LB(i)+eps_CP - Xcen(i)
           damp_fact = abs(shift_lim/shift_par(i))
           damp_min = MIN(damp_min, damp_fact)
      ELSE IF ( Xp(i) > Par_UB(i)-eps_CP .and. shift_par(i) > eps_shift) THEN
           shift_lim = Xcen(i) - Par_UB(i)-eps_CP
           damp_fact = abs(shift_lim/shift_par(i))
           damp_min = MIN(damp_min, damp_fact)
      ELSE IF ( abs(shift_par(i)) < eps_shift) THEN
           shift_par(i) = 0.0_DP
      ENDIF
   enddo
   call RANDOM_NUMBER(xrn)
   damp_min=damp_min*(0.9d0+0.09d0*xrn)
   Xp = Xcen + shift_par*damp_min
   Xp = MIN(MAX(Xp,Par_LB+eps_CP),Par_UB-eps_CP)
   
 END subroutine Check_Bounds
!******************************************************************************************
  subroutine POLYTO_STD(FCNV,Npar,Npun,Iset,Par_000,cal000,obs000,wei000,Par_LB,Par_UB,refstd)

   IMPLICIT NONE
   REAL(CP),intent(INOUT)       :: Par_000(Npar), Par_LB(Npar), Par_UB(Npar),cal000(Npun)
   REAL(CP),intent(IN)          :: obs000(Npun),wei000(Npun)
   real(CP)                     :: covmat(Npar,Npar),diaco(Npar)
   INTEGER(I4B),intent(IN)      :: Npar,Npun, Iset
   
   REAL(CP)                     :: Par_var(Npar), Par_LBX(Npar), Par_UBX(Npar),relat(Npar), &
                                   calnew(Npun), Xrand(Npar), &
                                   Xold(Npar), Deltax_log(Npar),goosh(2,Npar), &
                                   GEV(Npar),EEV(Npar),EEV2(Npar),evk(Npar,Npar), devar(Npar)
                                   
   REAL(CP)                     :: est,stande, dchi, adchir
   INTEGER(I4B)                 :: i,kpf,iu,iss,iuA, NUPH,NUPH2,keps,iubu, flag_mooh,refstd


   REAL(CP),allocatable         :: xp4h(:,:),vf4h(:),dd4h(:),ww4h(:)
   REAL(CP)                     :: est3(3),zok,zoka
   

   INTERFACE
     SUBROUTINE FCNV(N,P,Np,calc_in_p)
     USE nano_deftyp
     IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
     INTEGER(I4B), INTENT(IN)  :: N,Np
     REAL(CP),INTENT(IN)       :: P(N)
     REAL(CP),INTENT(OUT)      :: calc_in_p(Np)
     END SUBROUTINE FCNV
   END INTERFACE


!_______ Memorize starting point   
   Xold = Par_000
   call setup_DCHI(cal000,obs000,wei000)
   print'("chi2_0",1x,g24.16)', chi2_0 
   print'(a)', 'param/stdev'
   print*, ' Npar = ',Npar


   kpf=0
   do i=1,Npar
      zoka=zero
      est = zero
      est3 = zero
      Par_var=zero
      Par_var(i)=ten*eps_DP*abs(Par_000(i))
      call FCNV(Npar,Par_var+Par_000,Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=abs(DCHI)
      est3(1)=est3(1) + abs(DCHI)/abs(Par_var(i)**2)
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      Par_var(i)=-ten*eps_DP*abs(Par_000(i))
      print*, 'test1 ', zok,zoka              !!RF1306
      call FCNV(Npar,max(Par_LB,min(Par_UB,Par_var+Par_000)),Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=zok+abs(DCHI)
      zok = zok*half/abs(Par_var(i)**2)
      zoka=zoka+zok*unter
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      est3(1)=est3(1) + abs(DCHI)/abs(Par_var(i)**2)
      print*, 'test2 ', zok,zoka              !!RF1306

      Par_var(i)=sceps_DP*abs(Par_000(i))
      call FCNV(Npar,Par_var+Par_000,Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=abs(DCHI)
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      est3(2)=est3(2) + abs(DCHI)/abs(Par_var(i)**2)

      Par_var(i)=-sceps_DP*abs(Par_000(i))
      print*, 'test3 ', zok,zoka              !!RF1306
      call FCNV(Npar,Par_var+Par_000,Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=zok+abs(DCHI)
      zok = zok*half/abs(Par_var(i)**2)
      zoka=zoka+zok*unter
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      est3(2)=est3(2) + abs(DCHI)/abs(Par_var(i)**2)


      Par_var(i)=s4eps_DP*abs(Par_000(i))
      print*, 'test4 ', zok,zoka              !!RF1306
      call FCNV(Npar,Par_var+Par_000,Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=abs(DCHI)
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      est3(3)=est3(3) + abs(DCHI)/abs(Par_var(i)**2)

      Par_var(i)=-s4eps_DP*abs(Par_000(i))
      print*, 'test5 ', zok,zoka              !!RF1306
      call FCNV(Npar,Par_var+Par_000,Npun,calnew)
!     call FCNV(Npar,max(Par_LB,min(Par_UB,Par_var+Par_000)),Npun,calnew)
      DCHI = eval_DCHI(calnew)
      zok=zok+abs(DCHI)
      zok = zok*half/abs(Par_var(i)**2)
      zoka=zoka+zok*unter
      est=est+abs(DCHI)/abs(Par_var(i)**2)
      est3(3)=est3(3) + abs(DCHI)/abs(Par_var(i)**2)

      est = est * half
      stande = sqrt(chi2_0)/sqrt(est)
      est3=sqrt(chi2_0)/sqrt(est3)
      zoka=sqrt(chi2_0)/sqrt(zoka)
      print*, 'test6 ', zok,zoka              !!RF1306
      !write(88,'(i4,1x,g24.16,5(1x,g16.8))'),i, Par_000(i), stande,est3, DCHI
      write(refstd,'(i4,1x,g24.16,6(1x,g16.8))')i, Par_000(i), stande,est3,zoka, DCHI 

    enddo

   call RESET_DCHI

 end subroutine POLYTO_STD
!******************************************************************************************
  subroutine POLYTO_STD2(FCNV,Npar,Npun,Iset,Par_000,cal000,obs000,wei000,Par_LB,Par_UB,refstd)

   IMPLICIT NONE
   REAL(CP),intent(INOUT)       :: Par_000(Npar), Par_LB(Npar), Par_UB(Npar),cal000(Npun)
   REAL(CP),intent(IN)          :: obs000(Npun),wei000(Npun)
   real(CP)                     :: covmat(Npar,Npar),diaco(Npar)
   INTEGER(I4B),intent(IN)      :: Npar,Npun, Iset
   
   REAL(CP)                     :: Par_var(Npar), Par_LBX(Npar), Par_UBX(Npar),relat(Npar), &
                                   calnew(Npun), Xrand(Npar), &
                                   Xold(Npar), Deltax_log(Npar),goosh(2,Npar), &
                                   Gradient_F(Npar),Full_Hessian(Npar,Npar),Gerr(Npar),hint(Npar),x0pp(Npar), &
                                   EEV(Npar),EEV2(Npar),evk(Npar,Npar), devar(Npar),Inv_Hessian(Npar,Npar)
                                   
   REAL(CP)                     :: est,stande,stande2, dchi, adchir
   INTEGER(I4B)                 :: i,i2,kpf,iu,iss,iuA, NUPH,NUPH2,keps,iubu, flag_mooh,refstd


   REAL(CP),allocatable         :: xp4h(:,:),vf4h(:),dd4h(:),ww4h(:)
   REAL(CP)                     :: est3(3),gof,zoka
   

   INTERFACE
     SUBROUTINE FCNV(N,P,Np,calc_in_p)
     USE nano_deftyp
     IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
     INTEGER(I4B), INTENT(IN)  :: N,Np
     REAL(CP),INTENT(IN)       :: P(N)
     REAL(CP),INTENT(OUT)      :: calc_in_p(Np)
     END SUBROUTINE FCNV
   END INTERFACE


!_______ Memorize starting point   
   Xold = Par_000
   call setup_DCHI(cal000,obs000,wei000)
   print'("chi2_0",1x,g24.16)', chi2_0 
   print'(a)', 'param/stdev'
   print*, ' Npar = ',Npar


   call GHDCHI2( FCNV,Npun,Gradient_F,Par_000,npar,Par_UB,Par_LB,Gerr,hint,x0pp,Full_Hessian)
   Inv_Hessian = PSEUDO_INV(A=Full_Hessian,tol=s4eps_DP,posdef=1)

   kpf=0
   gof=sqrt(chi2_0*Npun/(real(Npun-Npar,DP)))
   do i=1,Npar
     stande = gof*sqrt(Inv_Hessian(i,i))
     write(refstd,'(i4,1x,g24.16,6(1x,g16.8))')i, Par_000(i), hint(i), stande
   enddo
   Xrand = [(one/sqrt(Inv_Hessian(i,i)),i=1,Npar)]
   do i=1,Npar
     stande = one/sqrt(Inv_Hessian(i,i))
     do i2=i,Npar
       stande2 = Inv_Hessian(i,i2)*stande*Xrand(i2)
       write(refstd,'(2i4,1x,f23.16)')i, i2,  stande2
     enddo
   enddo

   call RESET_DCHI

 end subroutine POLYTO_STD2
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!*************************************************************
Subroutine GHDCHI2( FCNV,npun,Gradient_F,x,n,ub,lb,err,hint,x0,Full_Hessian)
implicit none
INTEGER(I4B),parameter ::  NTAB = 10
real(DP),parameter :: fak = 1.d-5
INTEGER(I4B),intent(IN) ::  n,npun
REAL(DP),dimension(n),intent(OUT) :: hint,x0,err,Gradient_F
REAL(DP),dimension(n,n),optional,intent(OUT) :: Full_Hessian
REAL(DP),dimension(n),intent(IN) :: x,ub,lb
REAL(DP),dimension(npun) :: ycal

REAL(DP) :: xpp(n),xmm(n),h(n),ypp,ymm,ycc,xcc(n),xpm(n),xmp(n),ymp,ypm
real(DP),parameter ::  CON=1.4d0, CON2=CON*CON, ICON=one/CON, BIG=1.d30, SAFE=two
!EXTERNAL eval_DCHI
INTEGER(I4B) ::  i,ivar,j,ivar2
REAL(DP) :: errt,fac,hh,a(NTAB,NTAB),hhi,hhih,hhr,hhr2,t00,t0,t1

! Returns the derivative of a function eval_DCHI at a point x by Ridders' method of polynomial
! extrapolation. The value h is input as an estimated initial stepsize; it need not be small,
! but rather should be an increment in x over which eval_DCHI changes substantially. An estimate
! of the error in the derivative is returned as err.
! Parameters: Stepsize is decreased by CON at each iteration. Max size of tableau is set by
! NTAB. Return when error is SAFE worse than the best so far.


   INTERFACE
     SUBROUTINE FCNV(N,P,Np,calc_in_p)
     USE nano_deftyp
     IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
     INTEGER(I4B), INTENT(IN)  :: N,Np
     REAL(CP),INTENT(IN)       :: P(N)
     REAL(CP),INTENT(OUT)      :: calc_in_p(Np)
     END SUBROUTINE FCNV
   END INTERFACE


h = max(fak,abs(ub-lb)*fak,abs(x)*fak)

!if (minval(abs(h))<eps_DP) stop 'h(:) must be > epsilon_DP in Gradient_F'
print*,'Parameter Dimension ',n
call CPU_TIME(t0)
do ivar=1,n
  if (h(ivar)<sceps_DP) then
    Gradient_F(ivar)=zero
    hint(ivar)=zero
    err(ivar)=zero
    if (PRESENT(Full_Hessian)) then
      Full_Hessian(:,ivar)=zero
      Full_Hessian(ivar,:)=zero
    endif
    cycle
  endif
  hh=h(ivar)
  xpp=x
  xpp(ivar)=min(x(ivar)+hh,ub(ivar))
  xmm=x
  xmm(ivar)=max(x(ivar)-hh,lb(ivar))
  x0(ivar)=half*(xpp(ivar)+xmm(ivar))
  hhr = half*(xpp(ivar)-xmm(ivar))
  hint(ivar)=hhr
  hhi=one/hhr
  hhih=half*hhi
  call FCNV(n,xpp,npun,ycal)
  ypp = eval_DCHI(ycal)
  call FCNV(n,xmm,npun,ycal)
  ymm = eval_DCHI(ycal)
  a(1,1)=(ypp-ymm)*hhih
  err(ivar)=BIG
  Gerundio:do i=2,NTAB
    !__ Successive columns in the Neville tableau will go to smaller
    !__ and higher orders of extrapolation.
    hh=hh*ICON
    xpp=x; xpp(ivar)=min(x(ivar)+hh,ub(ivar))
    xmm=x; xmm(ivar)=max(x(ivar)-hh,lb(ivar))
    x0(ivar)=half*(xpp(ivar)+xmm(ivar))
    hhr = half*(xpp(ivar)-xmm(ivar))
    hint(ivar)=hhr
    hhi=one/hhr
    hhih=half*hhi
    call FCNV(n,xpp,npun,ycal)
    ypp = eval_DCHI(ycal)
    call FCNV(n,xmm,npun,ycal)
    ymm = eval_DCHI(ycal)
    a(1,i)=(ypp-ymm)*hhih !__ Try new, smaller stepsize.
    fac=CON2
    do j=2,i !__ Compute extrapolations of various orders, requiring no new function evaluations.
      a(j,i)=(a(j-1,i)*fac-a(j-1,i-1))/(fac-one) 
      fac=CON2*fac
      errt=max(abs(a(j,i)-a(j-1,i)),abs(a(j,i)-a(j-1,i-1)))
  !__ The error strategy is to compare each new extrapolation to one order lower, both at
  !__ the present stepsize and the previous one.
      if (errt <= err(ivar)) then !__ If error is decreased, save the improved answer.
        err(ivar)=errt
        Gradient_F(ivar)=a(j,i)
      endif
    enddo
    if (abs(a(i,i)-a(i-1,i-1)) >= SAFE*err(ivar)) then
      if (PRESENT(Full_Hessian)) then
        ycc=zero
        if (abs(x0(ivar)-x(ivar))>ten*eps_DP) then
          xcc=x
          xcc(ivar)=x0(ivar)
     !     ycc=eval_DCHI(xcc)
          call FCNV(n,xcc,npun,ycal)
          ycc = eval_DCHI(ycal)
        endif
        Full_Hessian(ivar,ivar) = (ypp+ymm-two*ycc)*hhih*hhih
      endif
      exit Gerundio
    !__ If higher order is worse by a significant factor SAFE, then quit early.
    endif
  enddo Gerundio
enddo
call CPU_TIME(t1)
print'(a,f18.6,a)','Gradient done in ',t1-t0,' sec.'
t00=t0
t0=t1
if (.not.PRESENT(Full_Hessian)) return
do ivar=1,n-1
  if (h(ivar)<sceps_DP) cycle
  hhr=hint(ivar)
  do ivar2=ivar+1,n
    if (h(ivar2)<sceps_DP) cycle
    hhr2=hint(ivar2)
    xpp=x; xmm=x; xpm=x; xmp=x
    xpp(ivar)=x(ivar)+hhr; xpp(ivar2)=x(ivar2)+hhr2
    xmm(ivar)=x(ivar)-hhr; xmm(ivar2)=x(ivar2)-hhr2
    xpm(ivar)=x(ivar)+hhr; xpm(ivar2)=x(ivar2)-hhr2
    xmp(ivar)=x(ivar)-hhr; xmp(ivar2)=x(ivar2)+hhr2
    call FCNV(n,xpp,npun,ycal)
    ypp = eval_DCHI(ycal)
    call FCNV(n,xmm,npun,ycal)
    ymm = eval_DCHI(ycal)
 !   ypp=eval_DCHI(xpp)
 !   ymm=eval_DCHI(xmm)
    call FCNV(n,xpm,npun,ycal)
    ypm = eval_DCHI(ycal)
    call FCNV(n,xmp,npun,ycal)
    ymp = eval_DCHI(ycal)
    !ypm=eval_DCHI(xpm)
    !ymp=eval_DCHI(xmp)
    Full_Hessian(ivar,ivar2)=((ypp-ypm)-(ymp-ymm))*unqua/(hhr*hhr2)
    Full_Hessian(ivar2,ivar) = Full_Hessian(ivar,ivar2)
  enddo
enddo
call CPU_TIME(t1)
print'(a,f18.6,a)','Hessian done in ',t1-t0,' sec.'
print'(a,f18.6,a)','Total time ',t1-t00,' sec.'
END Subroutine GHDCHI2
!****************************************************************************************************
 subroutine SalernoReggio_STD(FCNV,Npar,Npun,Iset,Par_000,cal000,obs000,wei000,Par_LB,Par_UB,iu_STD)
   IMPLICIT NONE
   REAL(CP),intent(INOUT)       :: Par_000(Npar), Par_LB(Npar), Par_UB(Npar),cal000(Npun)
   REAL(CP),intent(IN)          :: obs000(Npun),wei000(Npun)
   real(CP)                     :: covmat(Npar,Npar),diaco(Npar)
   INTEGER(I4B),intent(IN)      :: Npar,Npun, Iset, iu_STD
   
   REAL(CP)                     :: Par_var(Npar), Par_LBX(Npar), Par_UBX(Npar),relat(Npar), &
                                   calnew(Npun), Xrand(Npar), &
                                   Xold(Npar), Deltax_log(Npar),goosh(2,Npar)
                                   
   REAL(CP)                     :: xxx, dxx,adxx, dchi,adchir,redno,predi,zz,Fcova,stoll, &
                                   Ycen, Yr,Ye,Yc, Ymed, Yd2, dxl, dxu, cfr_con, &
                                   deltayup,deltaylo,chi2_old,dz,zmax,zmin,znew,z
                                   
   INTEGER(I4B)                 :: valfun, Nevalpo, isconv,irand(Npar)
   INTEGER(I4B)                 :: i,kpf,iu,iss,iuA, NUPH,NUPH2


   REAL(CP),allocatable        :: xp4h(:,:),vf4h(:),dd4h(:),ww4h(:)
!   REAL(CP), PARAMETER :: deltafr = 0.0005_DP, deltaxr = 0.00001_DP, dectol=-0.05_DP*deltafr
   

INTERFACE
 SUBROUTINE FCNV(N,P,Np,calc_in_p)
  USE nano_deftyp

  IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
  INTEGER(I4B), INTENT(IN)  :: N,Np
  REAL(CP),INTENT(IN)       :: P(N)
  REAL(CP),INTENT(OUT)      :: calc_in_p(Np)
 END SUBROUTINE FCNV
END INTERFACE


   if (ALLOCATED(StDev_Vec)) deallocate(StDev_Vec)
   if (ALLOCATED(ParVal_Vec)) deallocate(ParVal_Vec)
   if (ALLOCATED(Correl_Matx)) deallocate(Correl_Matx)
   allocate(StDev_Vec(Npar),ParVal_Vec(Npar),Correl_Matx(Npar,Npar))

   deltafr1 = deltafr
   deltaxr1 = deltaxr
   dectol1  = -hundred*sceps_DP !dectol
   deltayup=1.0e-3_DP
   deltaylo=1.0e-6_DP
   
!_______ Memorize starting point   
   Xold = Par_000
   call setup_DCHI(cal000,obs000,wei000)
   chi2_old=chi2_0

   Nevalpo = 4*Npar*Npar+4*Npar
   allocate(xp4h(Npar,Nevalpo),vf4h(Nevalpo),dd4h(Nevalpo),ww4h(Nevalpo))

   iuA = FIND_UNIT()
   open(iuA,status='replace',file='test_stdev.out')

!______ Restarting point
1963 continue

   goosh=zero

   do i=1,Npar
     xxx = MAX(ABS(Par_UB(i)-Par_LB(i)),ABS(Par_000(i)))
     Deltax_log(i) = log10(xxx)-four
     Par_LBX(i) = log10(max(Par_000(i)-Par_LB(i),eps_DP)) - Deltax_log(i)
     Par_UBX(i) = log10(max(Par_UB(i)-Par_000(i),eps_DP)) - Deltax_log(i)
!     xxx=deltaxr*xxx
!     Par_LBX(i) = max(Par_LB(i),min(Par_UB(i),Par_000(i)-xxx))
!     Par_UBX(i) = max(Par_LB(i),min(Par_UB(i),Par_000(i)+xxx))
!     relat(i) = xxx
   enddo
   
1964 continue
   call setup_DCHI(cal000,obs000,wei000)
   write(iuA,'(1x,g24.16)') chi2_0 
   call FLUSH(iuA)

   kpf=0

   do i=1,Npar
   ! up loop
     zmax = Par_UBX(i)
     zmin = -two
     z = min(zero,zmax)
     do
       dxx = exp(logar10*(Deltax_log(i)+z))
       Par_var    = zero
       Par_var(i) = dxx
       call FCNV(Npar,Par_var+Par_000,Npun,calnew)
       DCHI = eval_DCHI(calnew)
       adchir = DCHI/chi2_0
       
!________ recheck that we have to shift the minimum
!       IF (adchir<dectol1) then
!         Par_000= Par_000 + Par_var
!         cal000 = calnew
!         call RESET_DCHI
!         write(iuA,'(1x,a,i6,7(1x,g24.16))')'Shifting 1 to ',i,Par_000(i)-Par_var(i),Par_var(i),Par_000(i),&
!                                                             chi2_0,DCHI,chi2_0+DCHI,adchir
!         goto 1964
!       endif
       
       adchir = abs(adchir)
       IF (adchir <= deltayup .and. adchir>=deltaylo) THEN
       ! accept and exit
         kpf = kpf+1
         xp4h(:,kpf) = Par_var
         goosh(1,i) = dxx
         vf4h(kpf)   = DCHI
         dd4h(kpf)   = abs(dxx)
         ww4h(kpf)   = 1.0_DP/dd4h(kpf)
         write(iuA,'(1x,2i5,5g24.16)') kpf, i, DCHI, deltafr, dxx, Par_LB(i), Par_UB(i)
         call FLUSH(iuA)
         kpf = kpf+1
         xp4h(:,kpf) = Par_var*half
         call FCNV(Npar,Par_var*half+Par_000,Npun,calnew)
         DCHI = eval_DCHI(calnew)
         vf4h(kpf)   = DCHI
         dd4h(kpf)   = abs(dxx)
         ww4h(kpf)   = 1.0_DP/dd4h(kpf)
         write(iuA,'(1x,2i5,5g24.16)') kpf, i, DCHI, deltafr, half*dxx, Par_LB(i), Par_UB(i)
         call FLUSH(iuA)
         EXIT
       ELSE IF (adchir > deltayup) THEN
       ! reduce
         znew=max(zmin,z-0.2_DP)
         dz=abs(znew-z)
         if (dz<sceps_DP) then
           goosh(1,i) = exp(logar10*(Deltax_log(i)+znew))
           EXIT
         endif
         z=znew
         cycle
       ELSE IF (adchir < deltaylo) THEN
       ! increase
         znew=min(zmax,z+0.2_DP)
         dz=abs(znew-z)
         if (dz<sceps_DP) then
           goosh(1,i) = exp(logar10*(Deltax_log(i)+znew))
           EXIT
         endif
         z=znew
         cycle
       ENDIF
     enddo
   enddo

   print*, 'Evaluating STD: End Loop I'
  
  
  
   do i=1,Npar
   ! down loop
     zmax = Par_LBX(i)
     zmin = -two
     z = min(zero,zmax)
     do
       dxx = exp(logar10*(Deltax_log(i)+z))
       Par_var    = zero
       Par_var(i) = -dxx
       call FCNV(Npar,Par_var+Par_000,Npun,calnew)
       DCHI = eval_DCHI(calnew)
       adchir = DCHI/chi2_0
       
!________ recheck that we have to shift the minimum
!       IF (adchir<dectol1) then
!         Par_000= Par_000 + Par_var
!         cal000 = calnew
!         call RESET_DCHI
!         !write(iuA,'(1x,a,36(1x,g42.16))')'Shifting to ',Par_000,chi2_0,adchir
!         write(iuA,'(1x,a,i6,6(1x,g24.16))')'Shifting 2 to ',i,Par_000(i)-Par_var(i),Par_var(i),Par_000(i),&
!                                                             chi2_0,DCHI,chi2_0+DCHI,adchir
!         goto 1964
!       endif
       
       adchir = abs(adchir)
       IF (adchir <= deltayup .and. adchir>=deltaylo) THEN
       ! accept and exit
         kpf = kpf+1
         xp4h(:,kpf) = Par_var
         goosh(2,i) = -dxx
         vf4h(kpf)   = DCHI
         dd4h(kpf)   = abs(dxx)
         ww4h(kpf)   = 1.0_DP/dd4h(kpf)
         write(iuA,'(1x,2i5,5g24.16)') kpf, i, adchir, deltafr, -dxx, Par_LB(i), Par_UB(i)
         call FLUSH(iuA)
         kpf = kpf+1
         xp4h(:,kpf) = Par_var*half
         call FCNV(Npar,Par_var*half+Par_000,Npun,calnew)
         DCHI = eval_DCHI(calnew)
         vf4h(kpf)   = DCHI
         dd4h(kpf)   = abs(dxx)
         ww4h(kpf)   = 1.0_DP/dd4h(kpf)
         write(iuA,'(1x,2i5,5g24.16)') kpf, i, adchir, deltafr, -half*dxx, Par_LB(i), Par_UB(i)
         call FLUSH(iuA)
         EXIT
       ELSE IF (adchir > deltayup) THEN
       ! reduce
         znew=max(zmin,z-0.2_DP)
         dz=abs(znew-z)
         if (dz<sceps_DP) then
           goosh(2,i) = -exp(logar10*(Deltax_log(i)+znew))
           EXIT
         endif
         z=znew
         cycle
       ELSE IF (adchir < deltaylo) THEN
       ! increase
         znew=min(zmax,z+0.2_DP)
         dz=abs(znew-z)
         if (dz<sceps_DP) then
           goosh(2,i) = -exp(logar10*(Deltax_log(i)+znew))
           EXIT
         endif
         z=znew
         cycle
       ENDIF
     enddo
   enddo

   print*, 'Evaluating STD: End Loop II'

   do
     if (kpf==Nevalpo) EXIT
     call RANDOM_NUMBER(Xrand)
     irand=min(2,max(1,CEILING(two*Xrand)))
     call RANDOM_NUMBER(Xrand)
     do i=1,Npar
       Par_var(i) = Xrand(i) * goosh(irand(i),i)
     enddo
!!!!
     call FCNV(Npar,Par_var+Par_000,Npun,calnew)
     DCHI = eval_DCHI(calnew)
     adchir = DCHI/chi2_0
!     IF (adchir<dectol1) then
!       Par_000= Par_000 + Par_var
!       cal000 = calnew
!       call RESET_DCHI
!       !write(iuA,'(1x,a,6(1x,g42.16))')'Shifting to ',Par_000,chi2_0
!       
!         write(iuA,'(1x,a,i6,3(1x,g24.16))')'Shifting R to ',i,&
!                                                             chi2_0,DCHI,chi2_0+DCHI,adchir
!       goto 1964
!     endif
!     
     IF (adchir <= deltayup .and. adchir>=deltaylo) THEN
      ! accept and exit
       kpf = kpf+1
       xp4h(:,kpf) = Par_var
       vf4h(kpf)   = DCHI
       dd4h(kpf)   = sqrt(sum(Par_var**2))
       ww4h(kpf)   = one/dd4h(kpf)
       write(iuA,'(1x,2i5,g24.16)') kpf, Nevalpo, adchir
       call FLUSH(iuA)
       if (kpf<Nevalpo) then
         kpf = kpf+1
         xp4h(:,kpf) = Par_var*half
         call FCNV(Npar,Par_var*half+Par_000,Npun,calnew)
         DCHI = eval_DCHI(calnew)
         vf4h(kpf)   = DCHI
         dd4h(kpf)   = abs(dxx)
         ww4h(kpf)   = one/dd4h(kpf)
         write(iuA,'(1x,2i5,g24.16)') kpf, Nevalpo, adchir
         call FLUSH(iuA)
       endif
     ENDIF
   enddo
!!!!

!   iu_STD = FIND_UNIT()
!   open(iu_STD,status='replace',file='for_stan_dev.out')
   write(iu_STD,*) Nevalpo,Npar, Npun 
   write(iu_STD,'(1x,g24.16)') chi2_0 
   do kpf = 1,Nevalpo
     write(iu_STD,'(3(1x,g24.16))')vf4h(kpf),dd4h(kpf),ww4h(kpf)
     do i=1,Npar
       write(iu_STD,'(1x,g24.16)')xp4h(i,kpf)
     enddo
   enddo
   
   NUPH2=Npar*(Npar+3)
   NUPH=NUPH2/2
   isconv=0
   stoll=sceps_DP
   DO
     call Matriciana(Np=Npar,Nev=Nevalpo,NU=NUPH, &
          vf=vf4h,dd=dd4h,ww=ww4h,xp=xp4h,UPPB=Par_UB-Par_000,LOWB=Par_LB-Par_000, &
          isconv=isconv,newp=Par_var,littvar=deltafr*chi2_0, &
          cova=covmat,pred=predi,sthresh=stoll,nowe=1)
     if (isconv==1 .or. stopanyway==1) then
       IF (isconv==1) then
         write(iu_STD,*)' *** SPLENDID - convergence reached'
       ELSE
         write(iu_STD,*)' *** WARNING - the supposed minimum might not be so'
       ENDIF

       !call FCNV(Npar,Par_000,Npun,calnew)
       call FCNV(Npar,Par_var+Par_000,Npun,calnew)
       DCHI = eval_DCHI(calnew)
       if (isconv/=1) then
         zz=abs(DCHI-predi)/max(abs(DCHI),eps_DP)
         if (zz<0.1_DP) then 
           write(iu_STD,*)' *** NEGLECT WARNING - the minimum is OK'
         else  
           write(iu_STD,*)' *** '
         endif
       else  
         write(iu_STD,*)' *** '
       endif
       write(iu_STD,*)'STANDARD DEVIATIONS BLOCK'
       write(iu_STD,*)Nevalpo,Npar, Npun
     !  Par_000=Par_000+Par_var
       ParVal_Vec = Par_000
     !  chi2_0=chi2_0+DCHI
       write(iu_STD,'(1x,g24.16)') chi2_0
       Fcova=sqrt(chi2_0*REAL(Npun,DP)/REAL(Npun-Npar,DP))
       write(iu_STD,'(1x,g24.16)') Fcova
       do i=1,Npar
         diaco(i)=sqrt(covmat(i,i))
       enddo
       if (isconv==1) then
         ! we are in a true minimum with all sacraments
         do i=1,Npar
           StDev_Vec(i) = Fcova*diaco(i)
           write(iu_STD,'(i6,2(1x,g24.16))')i,Par_000(i),StDev_Vec(i)
         enddo
       else
         ! we are NOT in a true minimum with all sacraments but we are out of patience
         do i=1,Npar
           StDev_Vec(i) = Fcova*diaco(i)
           write(iu_STD,'(i6,2(1x,g24.16))')i,Par_000(i),StDev_Vec(i)
         enddo
       endif
       do i=1,Npar
         covmat(:,i)=covmat(:,i)/(diaco(i)*diaco)
       enddo
       Correl_Matx = covmat
       write(iu_STD,*)'CORRELATION MATRIX BLOCK'
       write(iu_STD,*)Npar,Npar
       do i=1,Npar
         write(iu_STD,*) covmat(i,:)
       enddo
       exit
     else
       Par_000= Par_000 + Par_var
       call FCNV(Npar,Par_000,Npun,calnew)
       cal000 = calnew
       call RESET_DCHI
       do kpf=1,NUPH2
         call FCNV(Npar,xp4h(:,kpf)+Par_000,Npun,calnew)
         DCHI = eval_DCHI(calnew)
         vf4h(kpf) = DCHI
         xxx=SUM(Par_var*Par_var)
         dd4h(kpf) = sqrt(xxx)
         ww4h(kpf)   = one/sqrt(max(sceps_DP,xxx))
       enddo
       Nevalpo=NUPH2
     endif
   ENDDO
!   close(iu_STD)

! clean-up
   deallocate(xp4h,vf4h,dd4h,ww4h)
   call RESET_DCHI

 end subroutine SalernoReggio_STD
!****************************************************************************************************
subroutine Matriciana(Np,Nev,NU,vf,dd,ww,xp,UPPB,LOWB, isconv,newp,littvar,cova,pred,sthresh,nowe)
implicit none
integer(I4B),intent(IN) :: Np,Nev,NU
integer(I4B),optional,intent(IN) :: nowe
integer(I4B),intent(OUT):: isconv
real(DP),intent(IN) :: vf(Nev),dd(Nev),ww(Nev),UPPB(Np),LOWB(Np),littvar,sthresh
real(DP),intent(INOUT) :: xp(Np,Nev)
real(DP),intent(OUT) :: newp(Np),cova(Np,Np),pred
real(DP)  :: Als(Nev,NU),Bls(Nev),Xls(NU),GxN
real(DP)  :: Hx(Np,Np),Gx(Np),F0, HEV(Np), HEV2(Np),HET(Np,Np),dxsh(Np),vera(Np),vera2(Np),GxE(Np)
real(DP)  :: eigmin,eigmax,eigtol,eiinv,GcN,dxshN,cosam,spicc,xlvera,spicc2,sfx,sfx2
integer(I4B) :: i,j,k,posdef,weiit,nu2

sfx=1000.0_DP
sfx2=sfx*sfx
weiit=1
if (PRESENT(nowe)) then
  if (nowe==1) weiit=0
endif
k=Np*(Np+3)
k=k/2

if (k/=NU) stop 'Matriciana :: NU /= 1+Np*(Np+3)/2 '
nu2=2*nu
Bls=vf
if (weiit==1) Bls=Bls*ww
Als=zero
Als(:,1:Np)=transpose(xp)*sfx
Als(:,Np+1:2*Np)=Als(:,1:Np)**2
k=2*Np
do i=1,Np-1
  do j=i+1,Np
    k=k+1
    Als(:,k) = xp(i,:)*xp(j,:)*sfx2
  enddo
enddo
if (weiit==1) then
  do i=1,NU
    Als(:,i)=Als(:,i)*ww
  enddo
endif
call SING_VAL_LSSOLVE(a=Als,b=Bls,thresh=sceps_DP,x=Xls)
Gx=Xls(1:Np)*sfx
print*,'Gx',Gx
do i=1,Np
  Hx(i,i)=two*Xls(Np+i)*sfx2
enddo
k=2*Np
do i=1,Np-1
  do j=i+1,Np
    k=k+1
    Hx(i,j) = Xls(k)*sfx2
    Hx(j,i) = Hx(i,j)
  enddo
enddo
call DIAGONALY(a_in=Hx, d=HEV, z=HET)
print*,'HEV',HEV
eigmin=MINVAL(HEV)
eigmax=MAXVAL(ABS(HEV))
eigtol=sthresh*eigmax
k=0
if (eigmin>=eigtol) THEN
  posdef=1
  print*,'Matriciana: posi. def. fitted Hessian ', posdef,eigmax,eigmin,eigtol
else if (abs(eigmin)<eigtol) THEN
  posdef=0
  print*,'Matriciana: singular   fitted Hessian ', posdef,eigmax,eigmin,eigtol
else if (eigmin<-eigtol) THEN
  posdef=-1
  print*,'Matriciana: indefinite fitted Hessian ', posdef,eigmax,eigmin,eigtol
endif

if (posdef<1) HEV2=HEV+(eigtol-eigmin)

GxE = matmul(Gx,HET)
dxsh = zero
cova = zero
do i=1,Np
  eiinv=zero
  if (HEV(i)>=eigtol) eiinv = one/HEV(i)
  cova = cova + SPECTRAL_COMP(HET(:,i),eiinv)
  eiinv = GxE(i)/HEV2(i)
  dxsh = dxsh - eiinv * HET(:,i)
enddo
dxsh = MAX(LOWB,MIN(UPPB,dxsh))

pred = sum(dxsh*(Gx+half*matmul(Hx,dxsh)))
print*,'Predicted decrease :: ',pred
dxshN=sqrt(sum(dxsh*dxsh))
GxN=sqrt(sum(Gx*Gx))
cosam = abs(one-sum(dxsh*Gx)/(dxshN*GxN))
isconv=0
print*,'Matriciana: three_criteria / 2be<1 ',dxshN,GxN,cosam,MIN(dxshN,GxN,cosam)/sceps_DP
if (posdef>=0 .and. MIN(dxshN,GxN,cosam)<sceps_DP) then
  isconv=1
  return
endif
newp=zero
IF (isconv==0) then
!___ prepare new pointset
  newp=dxsh
  xp=zero
  k=1
  vera=Gx/GxN
  spicc=sqrt(littvar*two/sum(vera*vera*HEV))
  vera2=vera
  vera=MAX(LOWB,MIN(UPPB,vera))
  xlvera=SQRT(sum(vera*vera))
  if (xlvera>sceps_DP) then
    k=k+1
    xp(:,k)=vera
  endif
  vera=MAX(LOWB,MIN(UPPB,-vera2))
  xlvera=SQRT(sum(vera*vera))
  if (xlvera>sceps_DP) then
    k=k+1
    xp(:,k)=vera
  endif
  do i=2,Np+1
    spicc=sqrt(littvar*two/HEV(i-1))
    spicc2=spicc
    spicc=MAX(LOWB(i-1),MIN(UPPB(i-1),spicc))
    xlvera = abs(spicc)
    if (xlvera>sceps_DP) then
      k=k+1
      xp(i-1,k)=spicc
    endif
    spicc=MAX(LOWB(i-1),MIN(UPPB(i-1),-spicc2))
    xlvera = abs(spicc)
    if (xlvera>sceps_DP) then
      k=k+1
      xp(i-1,k)=spicc
    endif
  enddo
  do
    if (k>=NU2) exit
    call RANDOM_NUMBER(vera)
    spicc=one/sqrt(sum(vera*vera))
    vera=vera*spicc
    spicc=sqrt(littvar*two/sum(vera*vera*HEV))
    vera=MAX(LOWB,MIN(UPPB,vera*spicc))
    xlvera=SQRT(sum(vera*vera))
    if (xlvera>sceps_DP) then
      k=k+1
      xp(:,k)=vera
    endif
  enddo
endif

end subroutine Matriciana
!****************************************************************************************************
subroutine FusseKeFusse(FCNV,Npar,Npun,Iset,Par_000,cal000,obs000,wei000,Par_LB,Par_UB,refstd)

  IMPLICIT NONE
  REAL(CP),intent(INOUT)       :: Par_000(Npar), Par_LB(Npar), Par_UB(Npar),cal000(Npun)
  REAL(CP),intent(IN)          :: obs000(Npun),wei000(Npun)
  real(CP)                     :: covmat(Npar,Npar),covmati(Npar,Npar),diaco(Npar), grad(Npar)
  INTEGER(I4B),intent(IN)      :: Npar,Npun, Iset,refstd
  
  REAL(CP)                     :: calnew(Npun)
                                  
  REAL(CP)                     :: chi2_0,gof_0,a,b,std
  INTEGER(I4B)                 :: i,i1,i2,i3,i2b,kk,kk1,kk2,Noffd,Npgrid,Nvx,is,is2,iw,iw2,iagr


  REAL(CP),allocatable         :: dx(:,:),dy(:),dyc(:),mls(:,:),bls(:),mlsi(:,:),xls1(:)
  REAL(CP)                     :: wrel(2),fom
  character(3)                 :: a_iset

  INTERFACE
    SUBROUTINE FCNV(N,P,Np,calc_in_p)
    USE nano_deftyp
    IMPLICIT REAL(CP)(a-h,o-z),INTEGER(I4B)(i-n)
    INTEGER(I4B), INTENT(IN)  :: N,Np
    REAL(CP),INTENT(IN)       :: P(N)
    REAL(CP),INTENT(OUT)      :: calc_in_p(Np)
    END SUBROUTINE FCNV
  END INTERFACE


   if (ALLOCATED(StDev_Vec)) deallocate(StDev_Vec)
   if (ALLOCATED(ParVal_Vec)) deallocate(ParVal_Vec)
   if (ALLOCATED(Correl_Matx)) deallocate(Correl_Matx)
   allocate(StDev_Vec(Npar),ParVal_Vec(Npar),Correl_Matx(Npar,Npar))
  ParVal_Vec=Par_000
  wrel=[0.1d0,1.d0]*s4eps_DP
  write(a_iset,'(i3.3)')Iset
  write(refstd,'("# Parameter value - gradient - est.st.dev. for dtataset ",a)')a_iset

!_______ Memorize starting point   
  call setup_DCHI(cal000,obs000,wei000)
  gof_0=sqrt(chi2_0/(Npun-Npar))
  print'("chi2_0",1x,g24.16)', chi2_0 
  print'(a)', 'param/stdev'
  print*, ' Npar = ',Npar

  Noffd=Npar*(Npar-1)
  Noffd=Noffd/2
  if (Npar==1) then
    Npgrid=4
    Nvx=1 + 2*Npar
  else
    Npgrid=8*Noffd+4*Npar
    Nvx= 2*Npar + Noffd
  endif
  allocate(dx(Npar,Npgrid),dy(Npgrid),dyc(Npgrid))
  dx=zero; dy=zero
  
  if (Npar==1) then
    kk=0
    do is=-1,1,2
      do iw=1,2
        kk=kk+1
        dx(1,kk)=wrel(iw)*is*Par_000(1)
        call FCNV(Npar,dx(:,kk)+Par_000,Npun,calnew)
        dy(kk) = eval_DCHI(calnew)
      enddo
    enddo
    a=sum(dy*dx(1,:))/sum(dx(1,:)**2)
    b=max(eps_DP,two*sum(dy*(dx(1,:)**2))/sum(dx(1,:)**4))
    std = sqrt(chi2_0/(Npun-Npar))*sqrt(one/b)
    write(refstd,'(2i4,1x,g24.16,2(1x,g16.8))')iset,1, Par_000(1), a, std
  else
    allocate(mls(Nvx,Nvx),bls(Nvx),mlsi(Nvx,Nvx),xls1(Nvx))
    mls=zero;bls=zero
    kk=0
    do i1=1,Npar
      do is=-1,1,2
        do iw=1,2
          kk=kk+1
          dx(i1,kk)=wrel(iw)*is*Par_000(i1)
          call FCNV(Npar,dx(:,kk)+Par_000,Npun,calnew)
          dy(kk) = eval_DCHI(calnew)
        enddo
      enddo
    enddo
    do i1=1,Npar-1
      do i2=i1+1,Npar
        do is=-1,1,2
          do is2=-1,1,2
            do iw=1,2
              kk=kk+1
              dx(i1,kk)=wrel(iw)*is*Par_000(i1)
              dx(i2,kk)=wrel(iw)*is2*Par_000(i2)
              call FCNV(Npar,dx(:,kk)+Par_000,Npun,calnew)
              dy(kk) = eval_DCHI(calnew)
            enddo
          enddo
        enddo
      enddo
    enddo
    print*,'# Number of function evaluations done / planned : ', kk,Npgrid
    if (ANY(Isnan(dx))) print*,'NAN in dx'
    if (ANY(Isnan(dy))) print*,'NAN in dy'
    kk1=0
    do i1=1,Npar
      kk1=kk1+1
      bls(kk1)=sum(dy(:)*dx(i1,:))
      kk2=0
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:))
      enddo
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum(dx(i1,:)*(dx(i2,:)**2))
      enddo
      do i2=1,Npar-1;do i3=i2+1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i3,:))
      enddo;enddo
    enddo
    print*,'# Number of variables set / planned : ', kk2,Nvx
    do i1=1,Npar
      kk1=kk1+1
      bls(kk1)=half*sum(dy(:)*(dx(i1,:)**2))
      kk2=0
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum((dx(i1,:)**2)*dx(i2,:))
      enddo
      do i2=1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=unqua*sum((dx(i1,:)**2)*(dx(i2,:)**2))
      enddo
      do i2=1,Npar-1;do i3=i2+1,Npar
        kk2=kk2+1
        mls(kk1,kk2)=half*sum((dx(i1,:)**2)*dx(i2,:)*dx(i3,:))
      enddo;enddo
    enddo
    do i1=1,Npar-1
      do i2=i1+1,Npar
        kk1=kk1+1
        bls(kk1)=sum(dy(:)*dx(i1,:)*dx(i2,:))
        kk2=0
        do i2b=1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i2b,:))
        enddo
        do i2b=1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=half*sum(dx(i1,:)*dx(i2,:)*(dx(i2b,:)**2))
        enddo
        do i2b=1,Npar-1;do i3=i2b+1,Npar
          kk2=kk2+1
          mls(kk1,kk2)=sum(dx(i1,:)*dx(i2,:)*dx(i2b,:)*dx(i3,:))
        enddo;enddo
      enddo
    enddo
    mlsi = PSEUDO_INV(A=mls,tol=sceps_DP,posdef=0)
    if (ANY(ISNAN(bls))) print*, ' NAN in bls'
    if (ANY(ISNAN(mlsi))) print*, ' NAN in mlsi'
    xls1=matmul(mlsi,bls)
    if (ANY(ISNAN(xls1))) print*, ' NAN in xls1'
    grad=xls1(1:Npar)
    diaco=xls1(Npar+1:2*Npar)
    print*,'max/min diaco: ',maxval(diaco),minval(diaco)
    kk=2*Npar
    do i2=1,Npar
      covmati(i2,i2)=diaco(i2)
      do i3=i2+1,Npar
        kk=kk+1
        covmati(i2,i3)=xls1(kk)
        covmati(i3,i2)=covmati(i2,i3)
      enddo
    enddo
    do i2=1, Npgrid
      dyc(i2) = sum(grad*dx(:,i2)) + half*sum(dx(:,i2)*matmul(covmati,dx(:,i2)))
    enddo
    fom = sqrt(sum((dyc-dy)**2)/sum((dy)**2))
    iagr=find_unit()
    open(iagr,status='replace',file='stdev_fit_'//a_iset//'.out')
    write(iagr,*)'# Fit quality % = ',100.d0*fom
    do i2=1, Npgrid
      write(iagr,'(i4,4(1x,g12.6))') i2,sqrt(sum(dx(:,i2)**2)), dy(i2),dyc(i2),dy(i2)-dyc(i2)
    enddo
    close(iagr)
    
    if (ANY(ISNAN(covmati))) print*, ' NAN in covmati'
    covmat = PSEUDO_INV(A=covmati,tol=s4eps_DP,posdef=diag_modus)
    if (ANY(ISNAN(covmat))) print*, ' NAN in covmat'
    diaco=[(sqrt(covmat(i2,i2)),i2=1,Npar)]
    do i2=1,Npar
      covmat(:,i2)=covmat(:,i2)/(diaco*diaco(i2))
    enddo
    do i2=1,Npar
      write(refstd,'(2i4,1x,g24.16,2(1x,g16.8))')iset,i2, Par_000(i2), grad(i2),diaco(i2)
    enddo
    do i2=1,Npar
      write(refstd,'(2i4,1x,g24.16,100(1x,g16.8))')iset,i2, Par_000(i2), covmat(i2,i2:Npar)
    enddo
  endif
  StDev_Vec = diaco
  Correl_Matx = covmat

  call RESET_DCHI
  deallocate(dx,dy,mls,bls,mlsi,xls1)

end subroutine FusseKeFusse
!****************************************************************************************************
 

end module POLYTOPE
!_______________________________________________________________________________
module calc_hkl

use nano_deftyp
use HELPINPUT

contains

subroutine hkl_gen(file_inp)
implicit none

character(len=*), intent(IN):: file_inp
character(700)           :: pwd,phase,rline,dbstring,phase_path,inifil,ps,phase_name,dwafil,file
character(700)           :: smp_name,sgstring,shap,grpath,sampto,phase_string,filegnuL,dbpath,wlenght
character(10)            :: sg_char, prot,dbind,struc
real(DP), dimension(6)   :: cell=0.0d0,cell1=0.0d0
real(DP)                 :: wave,dmin,d,tth, hh, kk, lk,thmax,thmin,trad,delta,wave1,wave2,wave3
real(DP)                 :: al, be, c,ga, V, ar, br, cr, cosalr,cosber, cosgar
real(DP)                 :: a11, a12, a13, a21, a22, a23, a31, a32, a33,str_MF
real(DP), allocatable    :: dhkl(:),two_theta(:), sum(:), tmp(:,:), t(:),mtx(:,:)
integer(I4B),allocatable :: hkl(:,:)
integer(I4B)             :: h, k, l ,ll, j,iloginp,ilogout, lpwd,lpha,iux,ind,ldwa, kstr,lf,iii,jj,nline,wl
integer(I4B)             :: lphase,lsmp,lpath,lgrp,sg,samp,delta1,lsg,lfgnuL,istr=0,ii,i,ind1,iost,iux1,iu,isam_read,io
logical                  :: ini_file_exists=.false.,dwa=.false.,diffra=.false., prototype=.false.


call GET_PWD(pwd=pwd,lpwd=lpwd)
isam_read=0

!if (len_trim(file_inp) > 13) then
  if (file_inp(1:14)=='diffractor.inp') then 
    dwafil='#'
 ! endif   
 else     
  read(file_inp, '(a)') file
  file=trim(adjustl(file))
  lf=len_trim(file) 
  dwafil=file(1:lf)
 endif

dwafil=trim(adjustl(dwafil))
lpha=len_trim(dwafil)

iux=FIND_UNIT()
INQUIRE(FILE= dwafil(1:lpha), EXIST=ini_file_exists)
if (ini_file_exists) then
  dwa=.true.
  open(iux,status='old',file= dwafil(1:lpha) ,action='read')
  READ_IN: do 
    read(iux, '(a)',end = 10) rline
    rline=trim(adjustl(rline))
    lpha=len_trim(rline)  
    if(rline(1:4)=='rang') read(rline(5:lpha),*) thmin, thmax
    if(rline(1:4)=='wave') read(rline(5:lpha),'(a)') wlenght
    if(rline(1:1)=='%') istr=istr+1 
  enddo READ_IN 
  10 rewind(iux) 

else

  INQUIRE(FILE=pwd(1:lpwd)//'diffractor.inp', EXIST=ini_file_exists)
  if (ini_file_exists) then
    iux1=FIND_UNIT()
    diffra=.true.
    istr=1
    open(iux,status='old',file= pwd(1:lpwd)//'diffractor.inp' ,action='read')
    READ_IN4: do 
      read(iux1, '(a)',end = 11) rline
      rline=trim(adjustl(rline))
      lpha=len_trim(rline) 
      if (rline(1:4)=='WLEN') then
        read(rline(5:lpha),*,iostat=io) wave,isam_read
        if (io/=0) read(rline(5:lpha),*) wave
      endif
      if (rline(1:4)=='TWOT') read(rline(5:lpha),*) thmin, thmax
      if (rline(1:4)=='PATH') read(rline(5:lpha), '(a)') dbstring
      if (rline(1:4)=='FILE') read(rline(5:lpha), '(a)') smp_name
    enddo READ_IN4
    11 close(iux1)
  else
    print*, 'ERROR! '//dwafil(1:lpha)//' and diffractor.inp are both missing. hkl gen STOPS!'
    STOP
  endif
  
endif
if (verbose) print*,' get_hkl: istr = ',istr
do iii=1, istr 
  if (dwa) then
    prototype=.false.
    cell=0.0d0 
    write(struc, '(i1.1)') iii
    READ_IN2: do 
      read(iux, '(a)',end = 15) rline
      rline=trim(adjustl(rline))
      lpha=len_trim(rline) 
      if (rline(1:2)=='%'//struc(1:1)) then 
        read(rline(2:lpha), *) kstr, phase_name
        if (verbose) print*,  kstr, phase_name
        READ_IN3: do
          read(iux, '(a)',end = 15) rline
          rline=trim(adjustl(rline))
          lpha=len_trim(rline) 
          if (rline(1:1)=='%' .and. rline(2:2)/=struc(1:1)) exit READ_IN3 
          if (rline(1:2)=='db') read(rline(3:lpha), '(a)') dbpath
          if (rline(1:4)=='shap') read(rline(5:lpha), '(a)') shap
          if (rline(1:4)=='prot') read(rline(5:lpha),'(a)') prot
          if (rline(1:4)=='cell') read(rline(5:lpha),*) cell(1:6)          
        enddo READ_IN3 
      else 
        cycle READ_IN2
      endif  
    enddo READ_IN2
    15 close(iux)   

    ind=0
    wlenght=trim(adjustl(wlenght))
    wl=len_trim(wlenght)
    ind= index(wlenght(1:wl), ' ') 
    if (ind==0) read(wlenght(1:wl), *) wave
    if (ind/=0) then 
      read(wlenght(1:ind-1),*) wave1 
      ind1=index(wlenght(1:wl), ' ', .true.)
      read(wlenght(ind1+1:wl), *) wave3
      read(wlenght(ind+1:ind1-1),*) wave2
      wave=(wave1+wave2*wave3)/(1+wave3)
    endif
   
    dbpath=trim(adjustl(dbpath))
    lpha=len_trim(dbpath)
    ind=index(dbpath(1:lpha), ' ')
    read(dbpath(1:ind-1), '(a)') dbind
    read(dbpath(ind+1:lpha), '(a)') dbstring 
    
    prot= trim(adjustl(prot))
    if (prot(1:3) =='yes' .and. cell(1) /=0.0d0 ) prototype=.true.
       
    ind=index(dwafil(1:lpha), '.')
    dwafil=dwafil(1:ind-1)
  endif
 

  dwafil=trim(adjustl(dwafil))
  ldwa=len_trim(dwafil)
  dbstring=trim(adjustl(dbstring))
  lpha=len_trim(dbstring)


  if (dwa) then
    write(phase_string, '(i2.2)') kstr
    phase_name=trim(adjustl(phase_name))
    lphase=len_trim(phase_name)

   
     
    if (verbose) print*, dbstring(1:lpha)
    shap=trim(adjustl(shap))
    SHAPE: select case (shap(1:3))
    case ('SPH') SHAPE
      dbstring=dbstring(1:lpha)//'001_SPH.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    case ('QBE') SHAPE
      dbstring=dbstring(1:lpha)//'001_QBE.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)  
    case ('CSH') SHAPE
      dbstring=dbstring(1:lpha)//'_k001_s001_CSH.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    case ('PAR') SHAPE
      dbstring=dbstring(1:lpha)//'_a001_c001_PAR.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    case ('HEX') SHAPE
      dbstring=dbstring(1:lpha)//'_a001_c001_HEX.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    case ('CYL') SHAPE
      dbstring=dbstring(1:lpha)//'_a001_c001_CYL.smp_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    case default SHAPE
      STOP
    end select SHAPE 
  else
    if (diffra) then 
      smp_name= trim(adjustl(smp_name))
      lsmp=len_trim(smp_name)
      ind=index(smp_name(1:lsmp), '_r')
      if (ind==0) ind=index(smp_name(1:lsmp), '_a')
      if (ind==0) ind=index(smp_name(1:lsmp), '_k')
      phase_name=smp_name(1:ind-1)
      phase_name=trim(adjustl(phase_name))
      lphase=len_trim(phase_name)
    
      if (isam_read>0) then
        write(sampto, '(i3.3)') isam_read
      else
        trad=(thmax/two)*degrees_to_radians
        delta=(wave/(four*sin(trad)))*0.8d0
!         delta1=nint(delta*1000.0d0)
!         if (delta1<30) delta1=30
!         if (delta1>960) delta1=960
!         samp=delta1-mod(delta1,30)
        samp=30 * max(1,min(32,FLOOR(delta/0.03d0)))
        write(sampto, '(i3.3)') samp
      endif
      dbstring=dbstring(1:lpha)//sampto(1:3)//'A'//separator//smp_name(1:lsmp)//'_INFO'
      dbstring=trim(adjustl(dbstring))
      lpha=len_trim(dbstring)
    endif
  endif  


  if (verbose) print*, dbstring(1:lpha)
  if (verbose) print*, phase_name(1:lphase)
  if (verbose) print*, phase_string(1:2)

!! Now read the smp_INFO  
  iu=FIND_UNIT() 
  open(iu,status='old',file=dbstring(1:lpha)  ,action='read', iostat=iost)
  if (iost /= 0) then
    print*, dbstring(1:lpha)//' non found! The Program stops!'
    STOP
  endif
  READ_INFO: do 
    read(iu, '(a)',end = 12) rline
    rline=trim(adjustl(rline))
  enddo READ_INFO
  12 close(iu)

  if (.not. prototype) then 
    read(rline, '(6(g14.8),1x,a)', iostat=iost) cell(1:6), sgstring
    if (iost /= 0) RETURN
  else 
    read(rline, '(6(g14.8),1x,a)', iostat=iost) cell1(1:6), sgstring
    if (iost /= 0) RETURN
  endif    

  sgstring=trim(adjustl(sgstring))
  lpha=len_trim(sgstring)
  ind=index(sgstring(1:lpha),'[')
  ind1=index(sgstring(1:lpha),']')
  read(sgstring(ind+1:ind1-1),'(a)')sg_char
  sg_char=trim(adjustl(sg_char))
  lsg=len_trim(sg_char)
  read(sgstring(1:ind-1),*,iostat=iost) sg
  if (iost /= 0) RETURN
 
  if (dwa) then
    nline=0
    dbind=trim(adjustl(dbind))
    SHA: select case (dbind(1:2))  
    case ('03') SHA
      filegnuL=phase_name(1:lphase)//phase_string(1:2)//'_plot1D.mtx'
      filegnuL=trim(adjustl(filegnuL))
      lfgnuL=len_trim(filegnuL)
      if (verbose) print*, filegnuL(1:lfgnuL)
      open(iu,status='old',file=filegnuL(1:lfgnuL)  ,action='read', iostat=iost)
      if (iost /= 0) then
        print*, filegnuL//' not found! hkl_gen stops!'
        STOP
      endif
      READ_MTX: do
        read(iu, fmt = '(a)', end = 13) rline
        rline = trim(adjustl(rline))
        if (rline(1:1) == '#') then 
          cycle READ_MTX
        else
          nline = nline +1 
        endif
      enddo READ_MTX
      13 rewind(iu)
      if (verbose) print*, 'NLINE', nline
      read (iu, fmt = '(a)') rline
      read (iu, fmt = '(a)') rline
      allocate(mtx(1:10,1:nline) )
      str_MF=zero
      do i = 1, nline
        read(iu,*) mtx(:,i) 
        if (mtx(3,i) /= 0.0) then 
          str_MF =str_MF+ mtx(9,i)* mtx(3,i)
        endif
      enddo
      cell(1:3)=cell(1:3)*str_MF
      deallocate(mtx)
      close(iu) 
   
    case ('04') SHA
      filegnuL=phase_name(1:lphase)//phase_string(1:2)//'_plot2D.mtx'
      filegnuL=trim(adjustl(filegnuL))
      lfgnuL=len_trim(filegnuL)
      open(iu,status='old',file=filegnuL(1:lfgnuL)  ,action='read', iostat=iost)
      if (iost /= 0) then
        print*, filegnuL//' not found! hkl_gen stops!'
        STOP
      endif
      READ_MTX2: do
        read(iu, fmt = '(a)', end = 14) rline
        rline = trim(adjustl(rline))
        if (rline(1:1) == '#') then 
          cycle READ_MTX2
        else
          nline = nline +1 
        endif
      enddo READ_MTX2
      14 rewind(iu)
      read (iu, fmt = '(a)') rline
      read (iu, fmt = '(a)') rline
      allocate(mtx(1:10,1:nline) )
      str_MF=zero
      do i = 1, nline
        read(iu,*) mtx(:,i) 
        if (mtx(3,i) /= 0.0) then 
          str_MF =str_MF+ mtx(10,i)* mtx(4,i)
        endif
      enddo
      cell(1:3)=cell(1:3)*str_MF
      deallocate(mtx)
      close(iu)   
    end select SHA

  endif


  if (wave<0.4 .and. thmax>130.0d0) write(*,*) 'The hkl calculation may take some time ...'

  
  dmin=wave/two
  
  al=cell(4)*(pi/180.0d0)
  be=cell(5)*(pi/180.0d0)
  ga=cell(6)*(pi/180.0d0)

   

  a11=(cell(2)**2)*(cell(3)**2)*(sin(al))**2
  a12=cell(1)*cell(2)*(cell(3)**2)*(cos(al)*cos(be)-cos(ga))
  a13=cell(1)*(cell(2)**2)*cell(3)*(cos(ga)*cos(al)-cos(be))
  a22=cell(1)**2*cell(3)**2*(sin(be))**2
  a23=cell(1)**2*cell(2)*cell(3)*(cos(be)*cos(ga)-cos(al))
  a33=(cell(1)**2)*(cell(2)**2)*((sin(ga))**2)
   !   if (verbose) print*,' CELL ', cell(1), cell(2), cell(3), al, be, ga  

  V=cell(1)*cell(2)*cell(3)*sqrt(1-(cos(al))**2-(cos(be))**2-(cos(ga))**2+2*cos(al)*cos(be)*cos(ga))


  allocate(hkl(1:3,1:10**6))
  ll=0
!!cubic


  do h=50, -50,-1
  do k=50,-50,-1
  do l=50,-50, -1
    ii=ii+1
    d=V*(1/(sqrt((a11*h**2)+(a22*k**2)+(a33*l**2)+(two*a12*h*k)+(two*a23*k*l)+(two*a13*h*l))))
    tth=two*((asin((wave/(2*d))))*radians_to_degrees)
    if (d<dmin) cycle
    if (tth>thmax .or. tth<thmin) cycle
    if (h+k+l/=0) then 
   !! select case lattice centering     
      SGR: select case(sg)
!      case(225) SGR
!              if ((k==0 .and. l==0 .and. mod(h,2)/=0)  ) cycle 
!              if (h==0 .and.  mod(k,2)/=0 ) cycle 
!              if (h==0 .and.  mod(l,2)/=0 ) cycle 
!              if (mod((h+k),2)/=0 ) cycle 
!              if (mod((h+l),2)/=0 ) cycle 
!              if (mod((k+l),2)/=0 ) cycle 
!                   ll=ll+1
!                   hkl(:,ll)=(/h,k,l/)
      case(227) SGR
            if ((k==0 .and. l==0 .and. mod(h,4)/=0)  ) cycle 
            if ((h==0 .and. k==0 .and. mod(l,4)/=0)  ) cycle 
            if ((h==0 .and. l==0 .and. mod(k,4)/=0)  ) cycle 
            
            if (h==0 .and.  mod((k+l),4)/=0 ) cycle
            if (k==0 .and.  mod((h+l),4)/=0 ) cycle 
            if (l==0 .and.  mod((h+k),4)/=0 ) cycle 
            
            if (h==0 .and.  mod(l,2)/=0 ) cycle 
            if (k==0 .and.  mod(l,2)/=0 ) cycle 
            if (k==0 .and.  mod(h,2)/=0 ) cycle
            if (h==0 .and.  mod(k,2)/=0 ) cycle 
            if (l==0 .and.  mod(k,2)/=0 ) cycle
            if (l==0 .and.  mod(h,2)/=0 ) cycle
             
            if (h==k .and.  mod((h+l),2)/=0 ) cycle 
            if (k==l .and.  mod((k+l),2)/=0 ) cycle
            if (h==l .and.  mod((h+k),2)/=0 ) cycle
            if (mod((h+k),2)/=0 ) cycle
            if (mod((h+l),2)/=0 ) cycle
            if (mod((k+l),2)/=0 ) cycle
            ll=ll+1
            hkl(:,ll)=(/h,k,l/)   
      case(141) SGR
            if ((mod(h+k+l,2)/=0)  ) cycle 
            if ((l==0 .and. mod(h,2)/=0)  ) cycle 
            if ((l==0 .and. mod(k,2)/=0)  ) cycle 
            if ((h==0 .and. mod(k+l,2)/=0)  ) cycle 
            if ((h==k .and. mod(2*h+l,4)/=0)  ) cycle
            if ((h==0 .and.k==0 .and. mod(l,4)/=0)  ) cycle 
            if ((k==0 .and.l==0 .and. mod(h,2)/=0)  ) cycle 
            if ((k==h .and.l==0 .and. mod(h,2)/=0)  ) cycle 
            if ((h==k .and.l==0 .and. mod(h,2)/=0)  ) cycle            
            if ((h==-k .and. k<0 .or. h<0)) cycle 
            ll=ll+1
            hkl(:,ll)=(/h,k,l/)          
!      case(62) SGR
!              if ((h==0 .and. k==0 .and. mod(l,2)/=0)  ) cycle 
!              if ((h==0 .and. l==0 .and. mod(k,2)/=0)  ) cycle 
!              if ((k==0 .and. l==0 .and. mod(h,2)/=0)  ) cycle
!              if ((l==0 .and.  mod(h,2)/=0)  ) cycle
!              if ((h==0 .and.  mod((k+l),2)/=0)  ) cycle
!                   ll=ll+1
!                   hkl(:,ll)=(/h,k,l/)                       
      case default SGR          
            RETURN
      end select SGR  
    endif  
  enddo
  enddo
  enddo
  

!write(*,*) ' '
!write(*,'(a,6f15.10)') 'Cell parameters       = ', cell(1:6)
 !write(*,*)             'Space Group           = ', sg_char(1:lsg), sg
 !write(*,'(a,f15.10)')  'Working Wavelength (Å)= ', wave
 !write(*,'(a,2f15.10)') '2theta range          = ',thmin, thmax
 !write(*,*) ' '


  allocate(sum(1:ll))
  allocate(tmp(1:7, 1:ll), t(1:7))
 
! allocate(dhkl(1:jj),two_theta(1:jj))

  do i =1, ll 
    hh=real(hkl(1,i), DP)
    kk=real(hkl(2,i), DP)
    lk=real(hkl(3,i), DP)
    if(hh<0) hh=hh-0.1d0
    if(kk<0) kk=kk-0.1d0
    if(lk<0) lk=lk-0.1d0
    sum(i)= abs(hh)+ abs(kk) + abs(lk)
    tmp(1:3,i)=real(hkl(:,i),DP)
    tmp(4,i)=abs(sum(i))
    h= hkl(1,i)
    k= hkl(2,i)
    l= hkl(3,i)
    tmp(5,i)=sqrt(1/((1/V**2)*((a11*h**2)+(a22*k**2)+(a33*l**2)+(two*a12*h*k)+(two*a23*k*l)+(two*a13*h*l))))
   !  if (h==1.0 .and. k==1.0 .and. l==1.0) print*, tmp(5,i)
    tmp(6,i)=two*((asin((wave/(2*tmp(5,i)))))*radians_to_degrees)
    tmp(7,i)=1.0d0
  enddo
   
  if (cell(1)==cell(2) .or. cell(2)==cell(3)) then 
    do i = ll-1, 1, -1
      do j = 1, i
        if (tmp(4,j) > tmp(4,j+1)) then
          t(1:7) = tmp(1:7,j)
          tmp(1:7,j)=tmp(1:7,j+1)
          tmp(1:7,j+1)=t(1:7)
        endif
      enddo
    enddo     
  endif  
   
  
  deallocate(t, sum, hkl)
 
 
 
!  do i=1,ll 
!    print*, tmp(:,i)
! enddo
 !
!STOP
 
 
!  
!  
!    
  do i =1,ll
    do j=ll, i+1, -1
      if ((tmp(5,i))<(tmp(5,j))+sceps_DP .and. (tmp(5,i))>(tmp(5,j))-sceps_DP) then 
        if(nint(tmp(4,i))==nint(tmp(4,j))) then
          if (nint(abs(tmp(1,i)))==nint(abs(tmp(1,j))) .or. nint(abs(tmp(1,i)))==nint(abs(tmp(2,j))) &
          .or. nint(abs(tmp(1,i)))==nint(abs(tmp(3,j)))) then
            if (nint(abs(tmp(2,i)))==nint(abs(tmp(1,j))) .or. nint(abs(tmp(2,i)))==nint(abs(tmp(2,j))) &
            .or. nint(abs(tmp(2,i)))==nint(abs(tmp(3,j)))) then
              if (nint(abs(tmp(3,i)))==nint(abs(tmp(1,j))) .or. nint(abs(tmp(3,i)))==nint(abs(tmp(2,j))) &
              .or. nint(abs(tmp(3,i)))==nint(abs(tmp(3,j)))) then
                tmp(7,j)=2.0d0
              else 
                cycle
              endif
            endif
          endif
        endif
      endif 
    enddo
  enddo


!     
!   ! 
 !do i=1, ll
!    if (verbose) print*, tmp(:,i)
!  enddo
!!  
!STOP
!  
  allocate(hkl(1:3,1:ll), dhkl(1:ll),two_theta(1:ll))
!
  jj=0
  do i =1,ll
    if (tmp(7,i)==1.0d0)  then
      jj=jj+1
      hkl(:,jj)= nint(tmp(1:3,i))
      dhkl(jj)=tmp(5,i)
      two_theta(jj)=tmp(6,i)
    endif
  enddo 

  deallocate(tmp)
!  do i=1, jj
!   if (verbose) print*,   hkl(:,i), dhkl(i), two_theta(i)
!  enddo
!  

  ilogout=FIND_UNIT()

  open(ilogout,status='replace',file=pwd(1:lpwd)//phase_name(1:lphase)//'.hkl',action='readwrite',iostat=iost)
  if (verbose) print*,'Opening hkl file: '//pwd(1:lpwd)//phase_name(1:lphase)//'.hkl'
  if (iost/=0) then
    print*, 'ERROR opening ', pwd(1:lpwd)//phase_name(1:lphase)//'.hkl'
  endif
 
  write(ilogout, '(a,f8.5,a)')'#     h      k      l     d-spacing     2θ (λ=',wave,'Å)'
  do i=1, jj
    write(ilogout, '(i6,2i7,2f15.8)') hkl(:,i),dhkl(i),two_theta(i)
 ! write(ilogout, '(i6,2i7,f10.6)') hkl(:,i),dhkl(i)!,two_theta(i)
  enddo  
  close(ilogout)
  deallocate(hkl, dhkl, two_theta)

enddo

end subroutine hkl_gen
end module calc_hkl
!_______________________________________________________________________________
