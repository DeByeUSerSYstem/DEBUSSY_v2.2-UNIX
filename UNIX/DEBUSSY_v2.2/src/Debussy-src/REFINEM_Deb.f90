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
module REFINE_TOOLS
  use nano_deftyp
  use LINALG_TOOLS
  use specfun_AC
  use CALC_WSPACE
  
  integer(I4B),save :: kouncyp = 0
  logical,save            :: ref_fin=.false., sim_sum=.false.    !!RF 18.05.12
  INTEGER(I4B),save       :: reffinu, reffinu1, simsum    !! FB mar17 simsum
  INTEGER(I4B),save       :: diag_modus_covm


contains

 SUBROUTINE refy_SCAL
   IMPLICIT NONE
   INTEGER(I4B)              :: P_bound, Iset, NSFirst, NSCurr, NSall, NPall
   INTEGER(I4B),pointer      :: Mn_obs(:)=>NULL()

   TYPE(bivec),DIMENSION(:),ALLOCATABLE   :: zeta(:),Obse(:)
   TYPE(trivec),DIMENSION(:),ALLOCATABLE  :: B(:),C(:)
   REAL(DP),ALLOCATABLE                   :: sigma(:),s(:)

   REAL(DP),ALLOCATABLE      :: wow(:),onemat(:,:),onevec(:),onesol(:),E_onesol(:),ESSE0(:),Amc(:,:),Bmc(:),VARCOV_lin(:,:), &
                                Sxxx(:),W_setpha(:),COV_onesol(:,:),auvec(:),eee(:),accsetP(:,:)
   REAL(DP),ALLOCATABLE      :: onepar(:),onepar0(:),onejac(:,:),onehess(:,:),oneihess(:,:),onegrad(:),oneV(:,:),oneW(:)
   INTEGER(I4B),ALLOCATABLE  :: oneflag(:), p_not0(:), m_not0(:)
   INTEGER(I2B),ALLOCATABLE  :: wcold(:)

   INTEGER(I4B)              :: i,n,kp,j,msb,N4gof,numvarx,milk(1),nnot0,kccc
   REAL(DP)                  :: kisq,kisqr,wrd,avar,k_gof,sivsq,valin,valin2,zup,ZZZ_set

   IF (ASSOCIATED(Mn_obs)) NULLIFY(Mn_obs)
   illogik = .false.


!____________________ Cut corners for dataless simulations
   if (SIM_NODATA_W) then
     do Iset=1,NSET_W
       Scales(Iset)%SETscal = one
       Scales(Iset)%STRscal = one
       IF (NAMO_W>0) Scales(Iset)%AMOscal = zero
       Scales(Iset)%BKGscal = zero
     enddo
     return
   endif


   Mn_obs => NDATA_W
   n=NSET_W
   P_bound = Scales(1)%ns_cons

   !!!!!!!!! START NEW PART
   if (ALLOCATED(accsetP)) deallocate(accsetP)
   allocate(accsetP(size(Scales(1)%STRscal),0:2))
   accsetP = zero
   do Iset = 1, NSET_W
     NSCurr = Scales(Iset)%ntot
     if (Iset==1) NSFirst=NSCurr
     if (ALLOCATED(wow)) deallocate(wow)
     if (ALLOCATED(onemat)) deallocate(onemat)
     if (ALLOCATED(onevec)) deallocate(onevec)
     if (ALLOCATED(oneflag)) deallocate(oneflag)
     if (ALLOCATED(onesol)) deallocate(onesol)
     if (ALLOCATED(E_onesol)) deallocate(E_onesol)
     if (ALLOCATED(COV_onesol)) deallocate(COV_onesol)
     if (ALLOCATED(oneW)) deallocate(oneW)
     if (ALLOCATED(oneV)) deallocate(oneV)
     if (ALLOCATED(Sxxx)) deallocate(Sxxx)
     if (ALLOCATED(p_not0)) deallocate(p_not0)
     if (ALLOCATED(m_not0)) deallocate(m_not0)
     if (ALLOCATED(VARCOV_lin)) deallocate(VARCOV_lin)
     if (ALLOCATED(W_setpha)) deallocate(W_setpha)
     if (ALLOCATED(auvec)) deallocate(auvec)
     if (ALLOCATED(eee)) deallocate(eee)
 
     ALLOCATE(wow(Mn_obs(Iset)), &
          onemat(Mn_obs(Iset),NSCurr), &
          onevec(Mn_obs(Iset)), &
          oneflag(NSCurr), &
          onesol(NSCurr), &
          E_onesol(NSCurr), &
          COV_onesol(NSCurr,NScurr), &
          oneW(NSCurr), &
          Sxxx(NSCurr), &
          p_not0(NSCurr), &
          m_not0(NSCurr), &
          oneV(NSCurr,NSCurr), &
          VARCOV_lin(NSCurr,NSCurr), &
          W_setpha(NSTR_W), &
          auvec(size(Scales(1)%STRscal)),eee(size(Scales(1)%STRscal)))

     wow = SQRT(OBS_DATA_W(Iset)%wdata)      
     onevec = wow * OBS_DATA_W(Iset)%vdata
     onesol = zero
     do i=1,NSTR_W
       onemat(:,i+BACKGROUND(Iset)%DIMBACK+NAMO_W) = wow * CALPHA_W(Iset,i)%vdata
     enddo
     do i=1,NAMO_W
       onemat(:,i+BACKGROUND(Iset)%DIMBACK) = wow * AMORPHOUS(Iset)%amo_scat_sep(:,i)
     enddo
     do i=1,BACKGROUND(Iset)%DIMBACK
       onemat(:,i) = wow * BACKGROUND(Iset)%lin_bkgr_sep(:,i)
     enddo
     
!i=find_unit()
!open(i,status='replace',file='debug.xxx')
!do kccc=1,size(wow)
!  write(i,*)OBS_DATA_W(Iset)%t2data(kccc),OBS_DATA_W(Iset)%vdata(kccc),BACKGROUND(Iset)%lin_bkgr_sep(kccc,1),&
!            onemat(kccc,:),onevec(kccc),&
!            wow(kccc)
!enddo
!close(i)
  
     oneflag = 0
!       oneflag(BACKGROUND(Iset)%DIMBACK+1:BACKGROUND(Iset)%DIMBACK+NSTR_W+NAMO_W) = 1

     CALL SING_VAL_LSpre(a=onemat,b=onevec,thresh1=sceps_DP,x=onesol,num_effvar=numvarx, Wxio=oneW,Vxo=oneV,Sxo=Sxxx, &
     Pnot0=p_not0,Mnot0=m_not0)
     nnot0 = sum(P_not0)
!print*,'Debug: SING_VAL_LSpre onesol',onesol
!     call  SING_VAL_LSSOLVE(a=onemat,b=onevec,thresh=sceps_DP,x=onesol)
 !   print*,'Debug: SING_VAL_Lsolve onesol',onesol
     wrd = DOT_PRODUCT(onevec,onevec)
     msb = BACKGROUND(Iset)%DIMBACK
  
     N4gof = numvarx
     onevec = -onevec+MATMUL(onemat,onesol)
     kisq = DOT_PRODUCT(onevec,onevec)
     Scales(Iset)%WD_SQSUM = kisq
     Scales(Iset)%N_OB_PT  = real(Mn_obs(Iset),DP)
     kisqr = kisq/REAL(Mn_obs(Iset)-numvarx,DP)
     k_gof = sqrt(kisqr)
     
     do i=1,nnot0
       oneV(i,1:nnot0)=oneV(i,1:nnot0)*Sxxx(i)
     enddo
     !!!!!!!! check places...
     VARCOV_lin=zero
     do i=1,numvarx
       sivsq=oneW(i)**2
       VARCOV_lin(1:nnot0,1:nnot0) = VARCOV_lin(1:nnot0,1:nnot0) + SPECTRAL_COMP(oneV(1:nnot0,i),sivsq)
     enddo
     VARCOV_lin(1:nnot0,1:nnot0) = VARCOV_lin(1:nnot0,1:nnot0) * kisqr
     E_onesol = zero
     E_onesol(m_not0(1:nnot0)) = [(sqrt(max(zero,VARCOV_lin(i,i))), i=1,nnot0)]
     COV_onesol=zero
     do i=1,nnot0
       COV_onesol(m_not0(1:nnot0),m_not0(i)) = VARCOV_lin(1:nnot0,i)
     enddo
     !__ finqui ok

     print'(1x,a,i4,a,g16.10,"; wR = ",f16.9," %; GoF = ",f16.9,3i6, a,i6)',&
       'SET # ',Iset,' : chi^2 = ', kisqr, 100.0_DP*sqrt(kisq/wrd), k_gof, Mn_obs(Iset),N4gof,numvarx, &
            ';  cycle  = ',kouncyp !!RF print CYCLE number
  
     IF (ref_fin) THEN
       write(reffinu,'(1x,a,i4,a,g16.10,"; wR = ",f16.9," %; GoF = ",f16.9,3i6, a,i6)')&
           'SET # ',Iset,' : chi^2 = ', kisqr, 100.0_DP*sqrt(kisq/wrd), sqrt(kisqr),Mn_obs(Iset),N4gof,numvarx, &
            ';  cycle  = ',kouncyp !!RF print CYCLE number
       write(reffinu1,'(1x,a,i4,a,g16.10,"; wR = ",f16.9," %; GoF = ",f16.9,3i6, a,i6)')&
           'SET # ',Iset,' : chi^2 = ', kisqr, 100.0_DP*sqrt(kisq/wrd), sqrt(kisqr),Mn_obs(Iset),N4gof,numvarx, &
            ';  cycle  = ',kouncyp !!RF print CYCLE number
     ENDIF
    if (sim_sum) then 
     write(simsum,'(1x,a,i4,a,g16.10,"; wR = ",f16.9," %; GoF = ",f16.9,3i6, a,i6)')&
           'SET # ',Iset,' : chi^2 = ', kisqr, 100.0_DP*sqrt(kisq/wrd), sqrt(kisqr),Mn_obs(Iset),N4gof,numvarx, &
            ';  cycle  = ',kouncyp !!RF print CYCLE number
   ! close(simsum)    
    endif      


     Scales(Iset)%ALLscal = onesol
     Scales(Iset)%STRscal = onesol(1+BACKGROUND(Iset)%DIMBACK+NAMO_W:NSTR_W+BACKGROUND(Iset)%DIMBACK+NAMO_W)
     IF (NAMO_W>0) Scales(Iset)%AMOscal = onesol(BACKGROUND(Iset)%DIMBACK+1:BACKGROUND(Iset)%DIMBACK+NAMO_W)
     Scales(Iset)%BKGscal = onesol(1:BACKGROUND(Iset)%DIMBACK)
     
! print*,'Debug: onesol  ',onesol
!print*,'Debug: allscal ',Scales(Iset)%ALLscal
!print*,'Debug: strscal ',Scales(Iset)%STRscal
!print*,'Debug: bkgscal ',Scales(Iset)%BKGscal
     
     Scales(Iset)%E_ALLscal = E_onesol
     Scales(Iset)%E_STRscal = E_onesol(1+BACKGROUND(Iset)%DIMBACK+NAMO_W:NSTR_W+BACKGROUND(Iset)%DIMBACK+NAMO_W)
     IF (NAMO_W>0) Scales(Iset)%E_AMOscal = E_onesol(BACKGROUND(Iset)%DIMBACK+1:BACKGROUND(Iset)%DIMBACK+NAMO_W)
     Scales(Iset)%E_BKGscal = E_onesol(1:BACKGROUND(Iset)%DIMBACK)
     Scales(Iset)%COVm_STRscal = COV_onesol(1+BACKGROUND(Iset)%DIMBACK+NAMO_W:NSTR_W+BACKGROUND(Iset)%DIMBACK+NAMO_W, &
                                            1+BACKGROUND(Iset)%DIMBACK+NAMO_W:NSTR_W+BACKGROUND(Iset)%DIMBACK+NAMO_W)
     
     
     !______________ Find out set scale
!     kccc=0
!     IF (Iset==1) then
!        Scales(Iset)%SETscal = one
!        Scales(Iset)%E_SETscal = zero
!     ELSE
!       valin=sum(Scales(Iset)%STRscal/Scales(1)%STRscal)/NSTR_W
!       do
!         kccc=kccc+1
!         valin2=valin**2
!         W_setpha = one / ( Scales(1)%E_STRscal**2 + valin * ( Scales(Iset)%E_STRscal**2) )
!         Scales(Iset)%SETscal = sum(Scales(1)%STRscal*Scales(Iset)%STRscal*W_setpha) / &
!                                sum((Scales(Iset)%STRscal**2)*W_setpha)
!         if (ABS(Scales(Iset)%SETscal-valin) < sceps_DP*abs(valin)) exit
!         valin=Scales(Iset)%SETscal
!       enddo
!       valin=sqrt(max(zero, sum( W_setpha * (( Scales(1)%STRscal - Scales(Iset)%SETscal * Scales(Iset)%STRscal )**2) ) / &
!                            max(one,one*(NSTR_W-1)) ))
!       valin2 = one / max(eps_DP,sqrt(max(zero, sum( W_setpha * (Scales(Iset)%STRscal**2) ) )))
!       Scales(Iset)%E_SETscal = valin*valin2
!     ENDIF
     
     kccc=0
     ZZZ_set=SUM(Scales(Iset)%STRscal)
     zup = one/ZZZ_set
     Scales(Iset)%SETscal = ZZZ_set
     Scales(Iset)%E_SETscal = SQRT(MAX(zero, SUM( Scales(Iset)%COVm_STRscal ) ))
     print'(a,i2.2,a,f15.8,a,f15.8,a,i12)','Set # ',Iset,' : Total Scale = ',Scales(Iset)%SETscal,' +/- ',&
           Scales(Iset)%E_SETscal !, ' CYCLES = ',kccc
     
     do i=1,size(Scales(Iset)%STRscal)
       auvec = -Scales(Iset)%STRscal * zup * zup
       auvec(i) = auvec(i) + zup
       eee(i) = sqrt(max(zero, SUM(auvec * MATMUL(Scales(Iset)%COVm_STRscal,auvec)) ))
     enddo
     Scales(Iset)%STRscal = Scales(Iset)%STRscal * zup
     Scales(Iset)%E_STRscal = eee
     !____ errors to weights
     eee=one/(max(eps_DP,eee)**2)
     accsetP(:,0) = accsetP(:,0) + eee
     accsetP(:,1) = accsetP(:,1) + eee*Scales(Iset)%STRscal
     accsetP(:,2) = accsetP(:,2) + eee*(Scales(Iset)%STRscal**2)
     
     Scales(Iset)%ALLscal = Scales(Iset)%ALLscal * zup
     Scales(Iset)%ALLscal(1+BACKGROUND(Iset)%DIMBACK+NAMO_W:NSTR_W+BACKGROUND(Iset)%DIMBACK+NAMO_W) = Scales(Iset)%STRscal
     Scales(Iset)%E_ALLscal = Scales(Iset)%E_ALLscal * zup
    !_____ simplified treatment for all other scales
     IF (NAMO_W>0) then
       Scales(Iset)%AMOscal = Scales(Iset)%AMOscal * zup
       Scales(Iset)%E_AMOscal = Scales(Iset)%E_AMOscal * zup
     endif
     Scales(Iset)%BKGscal = Scales(Iset)%BKGscal * zup
     Scales(Iset)%E_BKGscal = Scales(Iset)%E_BKGscal * zup
     
     DEALLOCATE(wow, oneflag, onemat, onevec, onesol, COV_onesol)
     DEALLOCATE(E_onesol,oneW,oneV,Sxxx,p_not0,m_not0,VARCOV_lin,auvec,eee)
   enddo
   if (NSET_W>1) then
     do Iset = 1, NSET_W
       Scales(Iset)%STRscal = accsetP(:,1) / accsetP(:,0)
       Scales(Iset)%E_STRscal = sqrt(max(zero, ( accsetP(:,2) - accsetP(:,1)*Scales(Iset)%STRscal ) / &
                                               ((NSET_W-1)*accsetP(:,0)) )) 
     enddo
   endif
     !!!!!!!!! END NEW PART

   NULLIFY(Mn_obs)
   deallocate(accsetP)

 end SUBROUTINE refy_SCAL

end module REFINE_TOOLS
!___________________________________________________________________________________________________
module REFINEMENT

 use INPUT_VARIABLES, only: ALL_PHA_INFO, CALC_RPDF, CALC_FLAG
 use LINALG_TOOLS, only : Brutal_sincTransf
 use REFINE_TOOLS
 use POLYTOPE
 use STRATEGY
 use CALCFUN_DB2

 real(DP),save     :: timepolyt,t0polyt,v_overall_best=1.0e20_DP
 real(CP),allocatable,save  :: zwischen(:,:)
 
 character(128),save :: path_output_files
 integer(I4B),save   :: lpath_output_files,kpolfin=0,Fstdev_mode=0
 integer(I4B),allocatable,save   :: lcut_structure_name(:),lstructure_name(:)
 integer(I4B),allocatable,save   :: lcut_data_filename(:),ldata_filename(:)
 integer(I4B),save :: istr0,kset0,Nallp_thisphase,Nallp_all
 real(CP),allocatable,save  :: Pall_refined(:)
 integer(I4B),allocatable,save :: point_par(:),grenzpha(:,:)
 
 real(DP), Allocatable,save   :: diam_ave(:,:), diam_sig(:,:), CLU_DIAM(:,:), rod_ave_ab(:,:), &
                                 rod_sig_ab(:,:), rod_ave_c(:,:), rod_sig_c(:,:), DISTRU_ROD(:,:,:), &
                                 rod_ave(:,:), rod_sig(:,:), mass_ave(:,:), mass_sig(:,:), CLU_VOL(:,:),CLU_MAS(:,:), &
                                 cell_nu(:), vol_ave(:,:), vol_sig(:,:), E_CLU_MAS(:,:), &
                                 DISTRM(:,:),E_DISTRM(:,:), E_OCCB(:,:,:), projND_ab(:),projMD_ab(:),projND_c(:),projMD_c(:), &
                                 CLU_SIZES(:,:,:),massfrac_struc(:),CLU_MAS_1(:,:), CLU_MAS_2(:,:),&
                                 DISTRM_1(:,:), DISTRM_2(:,:),ndisto(:),mdisto(:),diamclo(:),dis1d(:,:), &
                                 CompDeri(:,:),JacMat(:,:), Jacmat_II(:,:)

  type,public :: multicolpha
     INTEGER(I4B)         :: ncolpha,dimcolpha
     real(DP),allocatable :: mucol(:,:),E_mucol(:,:),J_mucol(:,:,:)
  END TYPE multicolpha
  type(multicolpha),allocatable,save :: allcolout(:)

CONTAINS

!***********************************************************************************
function Err_FofPars(Funny,nfunny,npin,point_fullpset,fullpset,nfull,Jaco)
implicit none
integer(I4B),intent(IN) :: npin,nfull,nfunny
real(DP),intent(IN) :: fullpset(nfull)
integer(I4B),intent(IN) :: point_fullpset(npin)
real(DP),optional,intent(OUT) :: Jaco(npin,nfunny)
real(DP),dimension(nfunny) :: Err_FofPars
real(DP) :: grpin(nfunny,npin),yp(nfunny),ym(nfunny),pps(npin),pps1(npin),dxx,h_dxx,dprel
integer(I4B) :: ifunny,ipin

INTERFACE
  subroutine FUNNY(M,V,N,P)
     USE nano_deftyp
     IMPLICIT REAL(DP)(a-h,o-z),INTEGER(I4B)(i-n)
     INTEGER(I4B), INTENT(IN)  :: M,N
     REAL(DP),INTENT(IN)       :: P(N)
     REAL(DP),INTENT(OUT)      :: V(M)
  END subroutine FUNNY
END INTERFACE

grpin=zero
dprel=s4eps_DP*sqrt(s4eps_DP)
do ipin=1,npin
  pps=fullpset(point_fullpset(1:npin))
  dxx=min(one,max(abs(pps(ipin)),0.01d0))*dprel
  h_dxx=half/dxx
  pps(ipin)=pps(ipin)+dxx
  call FUNNY(nfunny,yp,npin,pps)
  pps=fullpset(point_fullpset(1:npin))
  pps(ipin)=pps(ipin)-dxx
  call FUNNY(nfunny,ym,npin,pps)
  grpin(:,ipin)=(yp-ym)*h_dxx
enddo
if (PRESENT(Jaco)) then
  Jaco=TRANSPOSE(grpin)
endif
do ifunny=1,nfunny
  pps=grpin(ifunny,:)*StDev_Vec(point_fullpset(1:npin))
  pps1=matmul(Correl_Matx(point_fullpset(1:npin),point_fullpset(1:npin)),pps)
  Err_FofPars(ifunny) = sqrt(max(zero, sum(pps*pps1) ))
enddo
end function Err_FofPars
!***********************************************************************************
!***********************************************************************************
subroutine Err_FofPars_COL(fullpset,nfull)
implicit none
integer(I4B),intent(IN) :: nfull
real(DP),intent(IN) :: fullpset(nfull)
real(DP) :: pps(nfull),dxx,h_dxx,dprel,xfack,oxfack,xerr, chi2dum
integer(I4B) :: istr,ipx,n00x,n01x,kk,ksp,ii

!_________ Values
do istr=1,NSTR_W
  n00x=ncut_0_off(1,istr)
  n01x=ncut_0_off(2,istr)
  allcolout(istr)%J_mucol = zero
  xfack=sum(DISTRU(n00x:n01x,istr,1))
  oxfack=one/xfack
  allcolout(istr)%mucol(:,1) = DISTRU(n00x:n01x,istr,1) * oxfack
  allcolout(istr)%mucol(:,2) = DISTRU(n00x:n01x,istr,2)
  kk=2
  do ksp=1,nano_iav(istr)%struk(n00x)%numspat
    kk=kk+1
    allcolout(istr)%mucol(:,kk) = [(nano_iav(istr)%struk(ii)%occusite(ksp),ii=n00x,n01x)]
    kk=kk+1
    allcolout(istr)%mucol(:,kk) = [(nano_iav(istr)%struk(ii)%DebyeWallerB(ksp),ii=n00x,n01x)]
  enddo
enddo
!_________ Jacobian
dprel=s4eps_DP*sqrt(s4eps_DP)
do ipx=1,nfull
  pps=fullpset(1:nfull)
  dxx=min(one,max(abs(pps(ipx)),0.01d0))*dprel
  pps(ipx)=pps(ipx)-dxx
  call FUNNY_ALLCOL(N=nfull,P=pps,isg=0,ipv=ipx,dpv=dxx)
  pps=fullpset(1:nfull)
  pps(ipx)=pps(ipx)+dxx
  call FUNNY_ALLCOL(N=nfull,P=pps,isg=1,ipv=ipx,dpv=dxx)
enddo
pps=fullpset(1:nfull)
!_________ Errors
do istr=1,NSTR_W
  n00x=ncut_0_off(1,istr)
  n01x=ncut_0_off(2,istr)
  do ii=1,n01x-n00x+1
    do kk=1,(nano_iav(istr)%struk(n00x)%numspat+1)*2
      pps = allcolout(istr)%J_mucol(ii,1:nfull,kk) * StDev_Vec(1:nfull)
      xerr=sum(pps*matmul(Correl_Matx,pps))
      allcolout(istr)%E_mucol(ii,kk) = sqrt(max(zero, xerr ))
    enddo
  enddo
enddo
! just for resetting...
call FCN_POLYTOPE(Np=nfull,P=fullpset,v=chi2dum)
print*,'Err_FofPars_COL: dummy chi2 FINAL-RESET = ',chi2dum

end subroutine Err_FofPars_COL
!***********************************************************************************
function Err_FofPars_Jac(nfunny,fullpset,nfull,Jaco)
implicit none
integer(I4B),intent(IN) :: nfull,nfunny
real(DP),intent(IN) :: fullpset(nfull)
real(DP),intent(IN) :: Jaco(nfull,nfunny)
real(DP),dimension(nfunny) :: Err_FofPars_Jac
real(DP) :: pps(nfull),eexx
integer(I4B) :: ifunny

do ifunny=1,nfunny
  pps=Jaco(:,ifunny)*StDev_Vec
  eexx=sum(pps*matmul(Correl_Matx,pps))
  Err_FofPars_Jac(ifunny) = sqrt(max(zero, eexx ))
enddo
end function Err_FofPars_Jac
!***********************************************************************************

subroutine PREPPATH()
implicit none
integer(I4B) :: istr,iset,ll,ii

if (allocated(lcut_structure_name)) deallocate(lcut_structure_name)
if (allocated(lstructure_name)) deallocate(lstructure_name)
allocate(lcut_structure_name(NSTR_W),lstructure_name(NSTR_W))
if (allocated(lcut_data_filename)) deallocate(lcut_data_filename)
if (allocated(ldata_filename)) deallocate(ldata_filename)
allocate(lcut_data_filename(NSET_W),ldata_filename(NSET_W))
do istr=1,NSTR_W
  lstructure_name(istr)=len_trim(structure_name_w(istr))
  lcut_structure_name(istr)=index(TRIM(structure_name_w(istr)(:)), separator, .true.) + 1
enddo
do iset=1,NSET_W
  ldata_filename(iset)=len_trim(data_filename(iset))
  lcut_data_filename(iset)=index(TRIM(data_filename(iset)(:)), separator, .true.) + 1
enddo

!___defaults

!path_output_files='.'//separator
!lpath_output_files=2
 path_output_files=''

ll=len_trim(OUTPUT_FILE_W)
if (ll>0) then
  ii=index(OUTPUT_FILE_W(1:ll),separator,.true.)
  if (ii>0) then
    path_output_files=OUTPUT_FILE_W(1:ii)
    lpath_output_files=ii
  endif
endif

!___
end subroutine PREPPATH
!********** ********** ********** ********** ********** ********** ********** ********** 
function AVESTD_DISTR(w,v,isnormalized)
implicit none
real(DP),dimension(:),intent(IN) :: w,v
integer(I4B),intent(IN) :: isnormalized
real(DP),dimension(2) :: AVESTD_DISTR
real(DP) :: iswei,a1,a2

if (size(w)/=size(v)) stop 'AVESTD_DISTR: wrong dimension input'
if (isnormalized/=1) then
  iswei=one/sum(w)
  a1=sum(w*v)*iswei
  a2=sum(w*(v**2))*iswei
else
  a1=sum(w*v)
  a2=sum(w*(v**2))
endif
AVESTD_DISTR=[a1,sqrt(max(zero,a2-a1**2))]

end function AVESTD_DISTR
!********** ********** ********** ********** ********** ********** ********** ********** 
function AVESTD_DISTR_2D(w,v,isnormalized)
implicit none
real(DP),dimension(:),intent(IN) :: w
real(DP),dimension(:,:),intent(IN) :: v
integer(I4B),intent(IN) :: isnormalized
real(DP),dimension(5) :: AVESTD_DISTR_2D
real(DP) :: iswei,a1(2),a2(3)
integer(I4B) :: k,klin(2),m

m=size(w)
if (2/=size(v,1).or.m/=size(v,2)) stop 'AVESTD_DISTR_2D: wrong dimension input'
if (isnormalized/=1) then
  iswei=one/sum(w)
  a1=zero
  a2=zero
  do k=1,m
	a1 = a1+v(:,k)*w(k)
	a2 = a2+[v(1,k)**2,v(2,k)**2,v(1,k)*v(2,k)]*w(k)
  enddo
  a1=a1*iswei
  a2=a2*iswei
else
  a1=zero
  a2=zero
  do k=1,m
    a1 = a1+v(:,k)*w(k)
    a2 = a2+[v(1,k)**2,v(2,k)**2,v(1,k)*v(2,k)]*w(k)
  enddo
endif
AVESTD_DISTR_2D(1:2)=a1
AVESTD_DISTR_2D(3)=max(zero,a2(1)-a1(1)**2)
AVESTD_DISTR_2D(4)=max(zero,a2(2)-a1(2)**2)
AVESTD_DISTR_2D(5)=a2(3)-a1(2)*a1(1)

end function AVESTD_DISTR_2D
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine DO_REFINE
  implicit none
  INTEGER(I4B)         :: kstage, krepeat

  call SET_STARTING_TEMP()
  call PREPPATH()
  diag_modus = diag_modus_covm
  krepeat =  2
  rep_strat:DO
      krepeat = krepeat + 1
      DO kstage=1,NSTAG
!_______ operazioni iniziali dello stage kstage-esimo
         call STAGE_assoc_PAR(kstage)

         IF (ref_stage(kstage)%stage_type == 'N') THEN
            continue
         ELSE IF (ref_stage(kstage)%stage_type == 'C') THEN
            call stage_POLYTO(kstage, krepeat)   ! pronta, da finire (far dipendere func_tol e maxfcn da kstage, krepeat)
         ELSE IF (ref_stage(kstage)%stage_type == 'A') THEN
            call stage_ANNEAL(kstage, krepeat)   ! pronta, da finire (far dipendere func_tol e maxfcn da kstage, krepeat)
         ELSE IF (ref_stage(kstage)%stage_type == 'B') THEN
            call stage_BOBYQA(kstage, krepeat)   ! pronta, da finire (far dipendere func_tol e maxfcn da kstage, krepeat)
         ELSE IF (ref_stage(kstage)%stage_type == 'M') THEN
            call stage_NELDERMEAD(kstage, krepeat)   ! pronta, da finire (far dipendere func_tol e maxfcn da kstage, krepeat)
         ELSE
           STOP 'ERROR! Wrong Refining Procedure in file .ref'
         ENDIF
!_______ e qua?
      ENDDO



      IF (krepeat>2) EXIT rep_strat
  ENDDO rep_strat
 end subroutine DO_REFINE
 !********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine stage_funerr(npain,pain,point_pain)
  ! used only to recalculate stdev.s of par.s
  implicit none
  INTEGER(I4B),intent(IN)          :: npain
  REAL(CP),intent(IN)              :: pain(npain)
  INTEGER(I4B),intent(IN)          :: point_pain(npain)
  
  INTEGER(I4B)                         :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  
     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )
     
     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     auxout = 0
     Par_ini(point_pain(1:npain)) = pain
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)

     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)

 end subroutine stage_funerr ! used only to recalculate stdev.s of par.s
!********** ********** ********** ********** ********** ********** ********** ********** 
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine stage_POLYTO(kstage, krepeat)
  use calc_hkl
  implicit none
  INTEGER(I4B),INTENT(IN)              :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd,lf
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  character(len=2)  :: stageNR

     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )
     
     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     Par_LB2 = Par_ini*bound_down
     Par_UB2 = Par_ini*bound_up
     IF (kstage<0 .and. krepeat<0) STOP 'Negative cycling?'  ! Silly check 
     Func_Tol = sum(NDATA_W(1:NSET_W))*sceps_DP  !! default;farla dipendere da kstage,krepeat --> da file .ref
     Max_Func = 1000000000   !! default;farla dipendere da kstage,krepeat --> da file .ref
     if (ref_stage(kstage)%main_crit > eps_DP) Func_Tol = ref_stage(kstage)%main_crit
     auxout = 1
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)
     call STAGE_POLYT_OUT(0,kstage,Func_value)
     auxout = 0

!     if (Fstdev_mode == 1) return

     IF (kpolfin == 0) then
       call POLYTO(FCN_POLYTOPE,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_Value,Check_conv)
       call CPU_TIME(timepolyt);timepolyt = timepolyt-t0polyt
       print*,' '
       print*,'  STAGE # = ',kstage                  !! FB 09.11.2014 
       print*,'  # of cycles = ', kouncyp
       print*,'  Max Function / Tolerance = ', Max_Func, Func_Tol
       print*,'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
          
       print*,' '
       ref_fin=.True.
         reffinu=FIND_UNIT()
         open(reffinu, status='replace', file=path_output_files(1:lpath_output_files)// &
                                    REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref.sum',action='WRITE')
         write(reffinu,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu,*)' '
               write(reffinu,*)'  # of cycles = ', kouncyp
               write(reffinu,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu,*)' '
           
         reffinu1=FIND_UNIT()  !! FB 09.11.2014 one ref.sum for each stage 
         write(stageNR(1:2),'(i2.2)') kstage     
         open(reffinu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref_F_'//stageNR(1:2)//'.sum',action='WRITE')
         write(reffinu1,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu1,*)' '
               write(reffinu1,*)'  STAGE # = ',kstage
               write(reffinu1,*)'  # of cycles = ', kouncyp
               write(reffinu1,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu1,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
         write(reffinu1,*)'  ' 
          

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(krepeat,kstage,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
    
        !lf=len_trim(main_name)
        !call hkl_gen (main_name(1:lf))

     else if (kpolfin == 1) then

         Par_fin = Par_ini
         DO Iset=1,NSET_W
            write(wwiset,'(i3.3)')Iset
            allocate(cal000(1:NDATA_W(Iset)),obs000(1:NDATA_W(Iset)),wei000(1:NDATA_W(Iset)))
            cal000 = CALTOT_W(Iset)%vdata
            obs000 = OBS_DATA_W(Iset)%vdata
            wei000 = OBS_DATA_W(Iset)%wdata
            npunti=NDATA_W(Iset)

            refstd=FIND_UNIT()                                            !!RF 07062012
            OPEN(refstd,status='replace',file=path_output_files(1:lpath_output_files)//&
                    'refinement_set'//wwiset//'.std',action='WRITE')
            write(refstd,('(1x,a)')) '#______        REFINEMENT   STD FILE       ______#'
            write(refstd,*)          '#                                                #'
!            call POLYTO_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call POLYTO_STD2(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call SalernoReggio_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
            call FusseKeFusse(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB2,Par_UB2, &
                              refstd)
            close(refstd)
         ENDDO

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(99,99,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
         DEALLOCATE(cal000,obs000,wei000)
     endif


     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)

 end subroutine stage_POLYTO
!********** ********** ********** ********** ********** ********** ********** **********
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine stage_NEWTNUM(kstage, krepeat)
  implicit none
  INTEGER(I4B),INTENT(IN)              :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  character(len=2)  :: stageNR
  
     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )
     
     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     Par_LB2 = Par_ini*bound_down
     Par_UB2 = Par_ini*bound_up
     IF (kstage<0 .and. krepeat<0) STOP 'Negative cycling?'  ! Silly check 
     Func_Tol = sum(NDATA_W(1:NSET_W))*sceps_DP  !! default;farla dipendere da kstage,krepeat --> da file .ref
     Max_Func = 1000000000   !! default;farla dipendere da kstage,krepeat --> da file .ref
     if (ref_stage(kstage)%main_crit > eps_DP) Func_Tol = ref_stage(kstage)%main_crit
     auxout = 1
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)
     call STAGE_POLYT_OUT(0,kstage,Func_value)
     auxout = 0

     IF (kpolfin == 0) then
         call NEWTON_NUM(FCN=FCN_POLYTOPE,Npar=Npar,Par_ini=Par_ini,Par_LB=Par_LB,Par_UB=Par_UB,Func_Tol=Func_Tol, &
                         Par_fin=Par_fin,Func_value=Func_value,Check_conv=Check_conv)!, &
    !                     Par_Err=StDev_Vec,Par_Corl=Correl_Matx)
    !    call POLYTO(    FCN_POLYTOPE,Npar,Par_ini,Par_LB,Par_UB,Func_Tol,Max_Func,Par_fin,Func_Value,Check_conv)

         call CPU_TIME(timepolyt);timepolyt = timepolyt-t0polyt
          print*,' '
          print*,'  STAGE # = ',kstage                  !! FB 09.11.2014 + RF
          print*,'  # of cycles = ', kouncyp
          print*,'  Max Function / Tolerance = ', Max_Func, Func_Tol
          print*,'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
          
          print*,' '
          ref_fin=.True.
          
          IF (ref_fin) THEN          !!RF 18.05.12
             reffinu=FIND_UNIT()
             open(reffinu, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref.sum',action='WRITE')
               write(reffinu,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu,*)' '
               write(reffinu,*)'  # of cycles = ', kouncyp
               write(reffinu,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu,*)' '
             !close(reffinu)
             reffinu1=FIND_UNIT()  !! FB 09.11.2014 one ref.sum for each stage  
             write(stageNR(1:2),'(i2.2)') kstage   
             open(reffinu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref_F_'//stageNR(1:2)//'.sum',action='WRITE')
               write(reffinu1,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu1,*)' '
               write(reffinu1,*)'  STAGE # = ',kstage
               write(reffinu1,*)'  # of cycles = ', kouncyp
               write(reffinu1,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu1,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu1,*)'  ' 
          ENDIF
          

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(krepeat,kstage,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)

     else if (kpolfin == 1) then

         Par_fin = Par_ini   
         DO Iset=1,NSET_W
            write(wwiset,'(i3.3)')Iset
            allocate(cal000(1:NDATA_W(Iset)),obs000(1:NDATA_W(Iset)),wei000(1:NDATA_W(Iset)))
            cal000 = CALTOT_W(Iset)%vdata
            obs000 = OBS_DATA_W(Iset)%vdata
            wei000 = OBS_DATA_W(Iset)%wdata
            npunti=NDATA_W(Iset)

            refstd=FIND_UNIT()                                            !!RF 07062012
            OPEN(refstd,status='replace',file=path_output_files(1:lpath_output_files)//&
                    'refinement_set'//wwiset//'.std',action='WRITE')
            write(refstd,('(1x,a)')) '#______        REFINEMENT   STD FILE       ______#'
            write(refstd,*)          '#                                                #'
!            call POLYTO_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call POLYTO_STD2(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call SalernoReggio_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
            call FusseKeFusse(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB2,Par_UB2, &
                              refstd)
            close(refstd)
         ENDDO

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(99,99,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
         DEALLOCATE(cal000,obs000,wei000)
     endif


     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)

 end subroutine stage_NEWTNUM
!********** ********** ********** ********** ********** ********** ********** **********
 subroutine stage_ANNEAL(kstage, krepeat)
  implicit none
  INTEGER(I4B),INTENT(IN)              :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  character(len=2)  :: stageNR

     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )

     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))
     

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     Par_LB2 = Par_ini*bound_down
     Par_UB2 = Par_ini*bound_up
     IF (kstage<0 .and. krepeat<0) STOP 'Negative cycling?'  
     Func_Tol = sum(NDATA_W(1:NSET_W))*sceps_DP  
     Max_Func = 1000000000   
     if (ref_stage(kstage)%main_crit > eps_DP) Func_Tol = ref_stage(kstage)%main_crit 
     auxout = 1
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)
     call STAGE_POLYT_OUT(0,kstage,Func_value)
     auxout = 0

     IF (kpolfin == 0) then
         call SIMANN(FCN=FCN_POLYTOPE,Npar=Npar,Par_ini=Par_ini,Par_LB=Par_LB,Par_UB=Par_UB,Func_Tol=Func_Tol, &
                     Max_Func=Max_Func,Par_fin=Par_fin,Func_Value=Func_Value,Check_conv=Check_conv, &
                     Target_Min_F=one, Cooling_Factor=0.7d0)
         call CPU_TIME(timepolyt);timepolyt = timepolyt-t0polyt
          print*,' '
          print*,'  STAGE # = ',kstage                  !! FB 09.11.2014 
          print*,'  # of cycles = ', kouncyp
          print*,'  Max Function / Tolerance = ', Max_Func, Func_Tol
          print*,'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
          
          print*,' '
          ref_fin=.True.
          
          IF (ref_fin) THEN          !!RF 18.05.12
             reffinu=FIND_UNIT()
             open(reffinu, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref.sum',action='WRITE')
               write(reffinu,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu,*)' '
               write(reffinu,*)'  # of cycles = ', kouncyp
               write(reffinu,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu,*)' '
             !close(reffinu)
             reffinu1=FIND_UNIT()  !! FB 09.11.2014 one ref.sum for each stage  
             write(stageNR(1:2),'(i2.2)') kstage   
             open(reffinu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref_F_'//stageNR(1:2)//'.sum',action='WRITE')
               write(reffinu1,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu1,*)' '
               write(reffinu1,*)'  STAGE # = ',kstage
               write(reffinu1,*)'  # of cycles = ', kouncyp
               write(reffinu1,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu1,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu1,*)'  ' 
          ENDIF
          

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(krepeat,kstage,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)

     else if (kpolfin == 1) then

         Par_fin = Par_ini   
         DO Iset=1,NSET_W
            write(wwiset,'(i3.3)')Iset
            allocate(cal000(1:NDATA_W(Iset)),obs000(1:NDATA_W(Iset)),wei000(1:NDATA_W(Iset)))
            cal000 = CALTOT_W(Iset)%vdata
            obs000 = OBS_DATA_W(Iset)%vdata
            wei000 = OBS_DATA_W(Iset)%wdata
            npunti=NDATA_W(Iset)

            refstd=FIND_UNIT()                                            !!RF 07062012
            OPEN(refstd,status='replace',file=path_output_files(1:lpath_output_files)//&
                    'refinement_set'//wwiset//'.std',action='WRITE')
            write(refstd,('(1x,a)')) '#______        REFINEMENT   STD FILE       ______#'
            write(refstd,*)          '#                                                #'
!            call POLYTO_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call POLYTO_STD2(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call SalernoReggio_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
            call FusseKeFusse(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB2,Par_UB2, &
                              refstd)
            close(refstd)
         ENDDO

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(99,99,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
         DEALLOCATE(cal000,obs000,wei000)
     endif


     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)
 end subroutine stage_ANNEAL
!********** ********** ********** ********** ********** ********** ********** **********
 subroutine stage_BOBYQA(kstage, krepeat)
  implicit none
  INTEGER(I4B),INTENT(IN)              :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  character(len=2)  :: stageNR

     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )

     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))
     

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     Par_LB2 = Par_ini*bound_down
     Par_UB2 = Par_ini*bound_up
     IF (kstage<0 .and. krepeat<0) STOP 'Negative cycling?'  
     Func_Tol = sum(NDATA_W(1:NSET_W))*sceps_DP  
     Max_Func = 1000000000   
     if (ref_stage(kstage)%main_crit > eps_DP) Func_Tol = ref_stage(kstage)%main_crit 
     auxout = 1
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)
     call STAGE_POLYT_OUT(0,kstage,Func_value)
     auxout = 0

     IF (kpolfin == 0) then
         call NLOPT_BOBYQA(FCN_NLOPT=FCN_nlopt_LN,Npar=Npar,Par_ini=Par_ini,Par_LB=Par_LB,Par_UB=Par_UB,Func_Tol=Func_Tol, &
                     Max_Func=Max_Func,Par_fin=Par_fin,Func_Value=Func_Value,Check_conv=Check_conv)
         call CPU_TIME(timepolyt);timepolyt = timepolyt-t0polyt
          print*,' '
          print*,'  STAGE # = ',kstage                  !! FB 09.11.2014
          print*,'  # of cycles = ', kouncyp
          print*,'  Max Function / Tolerance = ', Max_Func, Func_Tol
          print*,'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
          
          print*,' '
          ref_fin=.True.
          
          IF (ref_fin) THEN          !!RF 18.05.12
             reffinu=FIND_UNIT()
             open(reffinu, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref.sum',action='WRITE')
               write(reffinu,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu,*)' '
               write(reffinu,*)'  # of cycles = ', kouncyp
               write(reffinu,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu,*)' '
             !close(reffinu)
              reffinu1=FIND_UNIT()  !!FB 09.11.2014 
              write(stageNR(1:2),'(i2.2)') kstage    
              open(reffinu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref_F_'//stageNR(1:2)//'.sum',action='WRITE')
               write(reffinu1,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu1,*)' '
               write(reffinu1,*)'  STAGE # = ',kstage
               write(reffinu1,*)'  # of cycles = ', kouncyp
               write(reffinu1,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu1,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu1,*)'  ' 
          ENDIF
          

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(krepeat,kstage,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)

     else if (kpolfin == 1) then

         Par_fin = Par_ini   
         DO Iset=1,NSET_W
            write(wwiset,'(i3.3)')Iset
            allocate(cal000(1:NDATA_W(Iset)),obs000(1:NDATA_W(Iset)),wei000(1:NDATA_W(Iset)))
            cal000 = CALTOT_W(Iset)%vdata
            obs000 = OBS_DATA_W(Iset)%vdata
            wei000 = OBS_DATA_W(Iset)%wdata
            npunti=NDATA_W(Iset)

            refstd=FIND_UNIT()                                            !!RF 07062012
            OPEN(refstd,status='replace',file=path_output_files(1:lpath_output_files)//&
                    'refinement_set'//wwiset//'.std',action='WRITE')
            write(refstd,('(1x,a)')) '#______        REFINEMENT   STD FILE       ______#'
            write(refstd,*)          '#                                                #'
!            call POLYTO_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call POLYTO_STD2(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call SalernoReggio_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
            call FusseKeFusse(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB2,Par_UB2, &
                              refstd)
            close(refstd)
         ENDDO

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(99,99,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
         DEALLOCATE(cal000,obs000,wei000)
     endif


     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)
 end subroutine stage_BOBYQA
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine stage_NELDERMEAD(kstage, krepeat)
  implicit none
  INTEGER(I4B),INTENT(IN)              :: kstage, krepeat
  INTEGER(I4B)                         :: Max_Func, Npar, kstr, i, Nparz, Iset, npunti,refstd
  REAL(CP)                             :: Func_Value,Func_tol, bound_up,bound_down
  REAL(CP), ALLOCATABLE                :: Par_ini(:), Par_fin(:), Par_LB(:), Par_UB(:), Par_LB2(:), Par_UB2(:)
  REAL(CP), ALLOCATABLE                :: cal000(:), obs000(:), wei000(:)
  LOGICAL                              :: Check_conv
  character(len=3)  :: wwiset
  character(len=2)  :: stageNR

     bound_up=one+1.1d0*s4eps_DP; bound_down=one-1.1d0*s4eps_DP
     kouncyp = 0
     call CPU_TIME(t0polyt)
     call SETUP_CYCLE_START
     i = NSTR_W
     Npar = SUM((/(SUM( PARAPHAS(kstr)%nano_doit(:) ),kstr=1,i)/))
     Npar = Npar + SUM( PARAGLOB%nano_doit(:) )

     IF (ALLOCATED(Par_ini)) DEALLOCATE(Par_ini)
     IF (ALLOCATED(Par_fin)) DEALLOCATE(Par_fin)
     IF (ALLOCATED(Par_LB)) DEALLOCATE(Par_LB)
     IF (ALLOCATED(Par_UB)) DEALLOCATE(Par_UB)
     IF (ALLOCATED(Par_LB2)) DEALLOCATE(Par_LB2)
     IF (ALLOCATED(Par_UB2)) DEALLOCATE(Par_UB2)
     IF (ALLOCATED(zwischen)) DEALLOCATE(zwischen)

     Nparz = 2*Npar-1
     ALLOCATE(Par_ini(Npar), Par_fin(Npar), Par_LB(Npar), Par_UB(Npar), Par_LB2(Npar), Par_UB2(Npar), zwischen(Npar,Nparz))
     

     call SETUP_POLYTO_PAR(nparstage=Npar,parstage=Par_ini,Low_B=Par_LB,UP_B=Par_UB)
     Par_LB2 = Par_ini*bound_down
     Par_UB2 = Par_ini*bound_up
     IF (kstage<0 .and. krepeat<0) STOP 'Negative cycling?'  
     Func_Tol = sum(NDATA_W(1:NSET_W))*sceps_DP  
     Max_Func = 1000000000   
     if (ref_stage(kstage)%main_crit > eps_DP) Func_Tol = ref_stage(kstage)%main_crit 
     auxout = 1
     call FCN_POLYTOPE(Npar,Par_ini,Func_value)
     call STAGE_POLYT_OUT(0,kstage,Func_value)
     auxout = 0

     IF (kpolfin == 0) then
         call NLOPT_NELDMEA(FCN_NLOPT=FCN_nlopt_LN,Npar=Npar,Par_ini=Par_ini,Par_LB=Par_LB,Par_UB=Par_UB,Func_Tol=Func_Tol, &
                     Max_Func=Max_Func,Par_fin=Par_fin,Func_Value=Func_Value,Check_conv=Check_conv)
         call CPU_TIME(timepolyt);timepolyt = timepolyt-t0polyt
          print*,' '
          print*,  ' STAGE #  =   ',kstage                  !! FB 09.11.2014
          print*,'  # of cycles = ', kouncyp
          print*,'  Max Function / Tolerance = ', Max_Func, Func_Tol
          print*,'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
          
          print*,' '
          ref_fin=.True.
          
          IF (ref_fin) THEN          !!RF 18.05.12
             reffinu=FIND_UNIT()
             open(reffinu, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref.sum',action='WRITE')
               write(reffinu,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu,*)' '
               write(reffinu,*)'  # of cycles = ', kouncyp
               write(reffinu,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu,*)' '
             !close(reffinu)
              reffinu1=FIND_UNIT()       !!FB 09.11.2014 
              write(stageNR(1:2),'(i2.2)') kstage     
              open(reffinu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                      REFINEMENT_FILE(1:index(REFINEMENT_FILE,'.',.true.)-1)//&
                                      '_ref_F_'//stageNR(1:2)//'.sum',action='WRITE')
               write(reffinu1,('(1x,a)')) '_____   '//TRIM(REFINEMENT_FILE)//'     SUMMARY LOGFILE   _____'
               write(reffinu1,*)' '
               write(reffinu1,*)'  STAGE # = ',kstage
               write(reffinu1,*)'  # of cycles = ', kouncyp
               write(reffinu1,*)'  Max Function / Tolerance = ', Max_Func, Func_Tol
               write(reffinu1,*)'  CPU Time: Total / Per cyle = ', timepolyt,timepolyt/kouncyp
               write(reffinu1,*)'  ' 
          ENDIF
          

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(krepeat,kstage,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)

     else if (kpolfin == 1) then

         Par_fin = Par_ini   
         DO Iset=1,NSET_W
            write(wwiset,'(i3.3)')Iset
            allocate(cal000(1:NDATA_W(Iset)),obs000(1:NDATA_W(Iset)),wei000(1:NDATA_W(Iset)))
            cal000 = CALTOT_W(Iset)%vdata
            obs000 = OBS_DATA_W(Iset)%vdata
            wei000 = OBS_DATA_W(Iset)%wdata
            npunti=NDATA_W(Iset)

            refstd=FIND_UNIT()                                            !!RF 07062012
            OPEN(refstd,status='replace',file=path_output_files(1:lpath_output_files)//&
                    'refinement_set'//wwiset//'.std',action='WRITE')
            write(refstd,('(1x,a)')) '#______        REFINEMENT   STD FILE       ______#'
            write(refstd,*)          '#                                                #'
!            call POLYTO_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call POLYTO_STD2(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
!            call SalernoReggio_STD(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB,Par_UB,refstd)
            call FusseKeFusse(FCNV_POLYTOPE,Npar,npunti,Iset,Par_fin,cal000,obs000,wei000,Par_LB2,Par_UB2, &
                              refstd)
            close(refstd)
         ENDDO

         auxout = 1
         call FCN_POLYTOPE(Npar,Par_fin,Func_value)
         call STAGE_POLYT_OUT(99,99,Func_value)
         auxout = 0
         call SAVE_POLYTO_PAR(Npar,Par_fin)
         DEALLOCATE(cal000,obs000,wei000)
     endif


     DEALLOCATE(Par_ini, Par_fin, Par_LB, Par_UB, Par_LB2, Par_UB2, zwischen)
 end subroutine stage_NELDERMEAD
!********** ********** ********** ********** ********** ********** ********** ********** 
!********** ********** ********** ********** ********** ********** ********** ********** 
 SUBROUTINE GRENZ_SET(spsend)
   implicit none
   INTEGER(I4B)                                :: npsend,i
   INTEGER(I4B),intent(OUT)                    :: spsend

    IF (ALLOCATED(grenzpha)) DEALLOCATE(grenzpha)
    ALLOCATE(grenzpha(2,NSTR_W+1))
    grenzpha = -1
    spsend = 0
    DO i=1,NSTR_W
      IF (DB_INDEX_W(i) > 5) CYCLE
      npsend = SUM(PARAPHAS(i)%nano_doit)
      grenzpha(:,i)=[spsend,spsend+npsend]
      spsend = spsend+npsend
    ENDDO
!____________________ AMORPH as last
    npsend = SUM(PARAGLOB%nano_doit)
    grenzpha(:,NSTR_W+1)=[spsend,spsend+npsend]
    spsend = spsend+npsend

 END  SUBROUTINE GRENZ_SET
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine SETUP_POLYTO_PAR(nparstage,parstage,Low_B,UP_B) 
  implicit none
  INTEGER(I4B),INTENT(IN)                     :: nparstage
  REAL(CP),DIMENSION(nparstage),INTENT(OUT)   :: parstage
  REAL(CP),DIMENSION(nparstage),optional,INTENT(OUT)   :: Low_B,UP_B
  INTEGER(I4B)                                :: kstr, k1, k2

     k1=0
     do kstr=1,NSTR_W
       k2 = SUM(PARAPHAS(kstr)%nano_doit(:))
       parstage(k1+1:k1+k2) = PARAPHAS(kstr)%nano_par0(PARAPHAS(kstr)%nano_mask(1:k2))
       if (PRESENT(Low_B)) then
         Low_B(k1+1:k1+k2)    = PARAPHAS(kstr)%nano_parLO(PARAPHAS(kstr)%nano_mask(1:k2))
       endif
       if (PRESENT(UP_B)) then
         UP_B(k1+1:k1+k2)     = PARAPHAS(kstr)%nano_parUP(PARAPHAS(kstr)%nano_mask(1:k2))
       endif
       k1 = k1 + k2
     enddo
     k2 = SUM(PARAGLOB%nano_doit(:))
     parstage(k1+1:k1+k2) = PARAGLOB%nano_par0(PARAGLOB%nano_mask(1:k2))
     if (PRESENT(Low_B)) then
       Low_B(k1+1:k1+k2)    = PARAGLOB%nano_parLO(PARAGLOB%nano_mask(1:k2))
     endif
     if (PRESENT(UP_B)) then
       UP_B(k1+1:k1+k2)     = PARAGLOB%nano_parUP(PARAGLOB%nano_mask(1:k2))
     endif
     k1 = k1 + k2

 end subroutine SETUP_POLYTO_PAR
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine SAVE_POLYTO_PAR(nparstage,parstage) 
  implicit none
  INTEGER(I4B),INTENT(IN)                     :: nparstage
  REAL(CP),DIMENSION(nparstage),INTENT(IN)    :: parstage
  INTEGER(I4B)                                :: kstr, k1, k2

     k1=0
     do kstr=1,NSTR_W
        PARAPHAS(kstr)%nano_par0 = PARAPHAS(kstr)%nano_parcurr
        PARAPHAS(kstr)%nano_par0E = PARAPHAS(kstr)%nano_parcurrE
        k2 = SUM(PARAPHAS(kstr)%nano_doit(:))
        k1 = k1 + k2
     enddo
     PARAGLOB%nano_par0 = PARAGLOB%nano_parcurr
     PARAGLOB%nano_par0E = PARAGLOB%nano_parcurrE
     k2 = SUM(PARAGLOB%nano_doit(:))
     k1 = k1 + k2


 end subroutine SAVE_POLYTO_PAR
!********** ********** ********** ********** ********** ********** ********** ********** 
function endlength(a,l)
  implicit none
  character(len=*),intent(IN) :: a
  integer(I4B),intent(IN)     :: l
  integer(I4B) :: endlength,l1

  l1=min(l,len_trim(a))
  endlength = l1-index(a(1:l1),'.',.true.)+1
end function endlength
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine STAGE_POLYT_OUT(nrstage,kstage,v)
  implicit none
  INTEGER(I4B),INTENT(IN)                     :: nrstage,kstage
  REAL(CP),INTENT(IN)                         :: v
  INTEGER(I4B)                                :: kstr, k1, k2, kup, ku1,lns,lns1,kset,accorc,jat,jr
  character(14)                               :: foust
  character(2)                                :: astru
  character(5)                                :: AtoNum
  real(DP)  :: rawpdf(10000,3)=zero
  logical :: make_pdf
  
  make_pdf=.false.
  foust(1:5)='stage'
  write(foust(6:9),'(2i2.2)')nrstage,kstage
  if (nrstage==99.and.kstage==99) foust(6:9)='Best'
  if (nrstage==0) foust(6:7)='I_'
  
  if (nrstage==0) return
  
  if (nrstage>0 .and. nrstage /= 99) then
    foust(6:7)='F_'
  endif
  make_pdf = ((nrstage>0 .and. nrstage /= 99).or.(CALC_RPDF==1))

  kstr=0
  AtoNum(1:3) = 'ATO'
  do kstr=1,NSTR_W
    ku1=FIND_UNIT()
    write(astru,'(i2.2)')kstr
    accorc = endlength( TRIM(structure_name_w(kstr)), lstructure_name(kstr) )
    lns=lstructure_name(kstr)-accorc
    lns1=lcut_structure_name(kstr)
    OPEN(ku1,status='replace', &
         file=path_output_files(1:lpath_output_files)//&
              trim(structure_name_w(kstr))//astru//'_'//foust(6:9)//'.par',&
         action='WRITE')
    write(ku1,'("STcod",8x,i2)') PARAPHAS(kstr)%str_cod
    write(ku1,'("VALn1",8x,i2)') PARAPHAS(kstr)%n1

    jat = 0
    do k2=1,PARAPHAS(kstr)%Numero
      IF ( mod((k2-NumParPha),NumPar_at) == 1 ) THEN
        jat = jat +1
        write(AtoNum(4:5),'(i2.2)') jat
        write(ku1,'(a5,8x,2(i2,3x))') AtoNum,PARAPHAS(kstr)%law_O(jat),PARAPHAS(kstr)%law_B(jat)
      ENDIF
      if ((trim(PARAPHAS(kstr)%nano_names(k2))=='STR_i'.or.trim(PARAPHAS(kstr)%nano_names(k2))=='STR_1').and. &
          (PARAPHAS(kstr)%str_cod > 3)) then
        write(ku1,'(a,4x,3(1x,g24.17),i2)')trim(PARAPHAS(kstr)%nano_names(k2)), &
                  PARAPHAS(kstr)%nano_parLO(k2)*Downscale_Par,&
                  PARAPHAS(kstr)%nano_parcurrE(k2)*Downscale_Par,&
                  PARAPHAS(kstr)%nano_parUP(k2)*Downscale_Par, PARAPHAS(kstr)%nano_parref(k2)
      else
        write(ku1,'(a,4x,3(1x,g24.17),i2)')trim(PARAPHAS(kstr)%nano_names(k2)), &
                  PARAPHAS(kstr)%nano_parLO(k2),&
                  PARAPHAS(kstr)%nano_parcurrE(k2),&
                  PARAPHAS(kstr)%nano_parUP(k2), PARAPHAS(kstr)%nano_parref(k2)
      endif
    enddo
    close(ku1)
  enddo
  IF (DO_AMORPH_W) then
    kstr = 0
    ku1=FIND_UNIT()
    write(astru,'(i2.2)')kstr
    OPEN(ku1,status='replace',file='Amorphous_'//foust(6:9)//'.par',action='WRITE')
    write(ku1,'(a,3i3)')' AMORPHOUS ',PARAGLOB%Numero
    do k2=1,PARAGLOB%Numero
      write(ku1,'(a,4x,3(1x,g24.17)," 1")')trim(PARAGLOB%nano_names(k2)), &
                   PARAGLOB%nano_parLO(k2),&
                   PARAGLOB%nano_parcurrE(k2),&
                   PARAGLOB%nano_parUP(k2)
    enddo
    close(ku1)
  ENDIF
  !!************ Output clc.s
  foust(10:13) =  '.cal'
  do kset=1,NSET_W
    k1=find_unit()
    accorc = endlength( TRIM(data_filename(kset)), ldata_filename(kset) )
    lns=ldata_filename(kset)-accorc
    lns1=lcut_data_filename(kset)
    open(k1,status='replace',file=path_output_files(1:lpath_output_files)//&
                                   data_filename(kset)(lns1:lns)//'_'//foust(6:13),action='WRITE')
    write(k1,*)'# SET.rep.kstage.chi2 ',kset,nrstage,kstage,kouncyp,v  !! mar17 print ncycle
    write(k1,*)'# T2 Obs Cal [Cal_1 .. Cal_n] Bkg Blank'
    if (make_pdf) then
      call write_cal_fileX(kset=kset,iu=k1,difpdf=rawpdf)
      kup=find_unit()
      open(kup,status='replace',file=path_output_files(1:lpath_output_files)//&
                                     data_filename(kset)(lns1:lns)//'_'//foust(6:9)//'.rpdf',action='WRITE')
      write(kup,*)'# SET.rep.kstage.chi2 ',kset,nrstage,kstage,v
      write(kup,*)'# r pdf_Obs pdf_Cal pdf_Dif'
      do jr=1,10000
        write(kup,'(f8.2,3(1x,g15.9))') jr*0.01d0, rawpdf(jr,:)
      enddo
      close(kup)
    else
      call write_cal_fileX(kset=kset,iu=k1)
    endif
    close(k1)
  enddo

  !!************ Output amo.s
  IF (DO_AMORPH_W) then
    foust(10:13) =  '.cam'
    do kset=1,NSET_W
      k1=find_unit()
      accorc = endlength( TRIM(data_filename(kset)), ldata_filename(kset) )
      lns=ldata_filename(kset)-accorc
      lns1=lcut_data_filename(kset)
      open(k1,status='replace',file=path_output_files(1:lpath_output_files)//&
!                                   data_filename(kset)(lns1:lns)//'_'//foust(6:13),action='WRITE')
        data_filename(kset)(:index(data_filename(kset),'.',.true.)-1)//'_'//foust(6:13),action='WRITE')                        
       write(k1,*)'# AMORPHOUS COEFFICIENTS '
       call write_dumm_fileX(kset=kset,iu=k1,u0=PARAGLOB%nano_parcurrE(1),u1=PARAGLOB%nano_parcurrE(2))
       close(k1)
    enddo
  endif

  !!************ Output weight.s
  
  if (ANY(DB_INDEX_W>1)) then 
    foust(10:13) =  '.dis'
    k1=find_unit()
    accorc = endlength( TRIM(main_name), len_trim(main_name) )
    lns=len_trim(main_name)-accorc
    open(k1,status='replace',file=path_output_files(1:lpath_output_files)//&
                             main_name(1:lns)//'_'//foust(6:13),action='WRITE')
    call write_varia_fileX(kset=1,iu=k1)
    close(k1)
  endif
  
 end subroutine STAGE_POLYT_OUT
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine FCN_nlopt_LN(v, np, p, grad, need_gradient, dumm)
  INTEGER(I4B),INTENT(IN)              :: np, need_gradient
  REAL(CP),DIMENSION(np),INTENT(IN)    :: p
  REAL(CP),INTENT(OUT)                 :: v, grad(np)
  REAL(CP),INTENT(INOUT)               :: dumm
  
  print*, '  FCN_nlopt_LN ', p, grad
  if (need_gradient.ne.0) then
    grad = 0.0d0
  endif
  call FCN_POLYTOPE(np,p,v)

 end subroutine FCN_nlopt_LN
!********** ********** ********** ********** ********** ********** ********** ********** 
 subroutine FCN_POLYTOPE(np,p,v)
  INTEGER(I4B),INTENT(IN)              :: np
  REAL(CP),DIMENSION(np),INTENT(IN)    :: p
  REAL(CP),INTENT(OUT)                 :: v
  REAL(CP)              :: timecy
  INTEGER(I4B)          :: icon,i

  kouncyp = kouncyp + 1
  call CPU_TIME(timecy)
  
  call LOOP_CALC(np,p,icon)
  if (icon==0) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif
  call RENORM_CAL

  call refy_SCAL
!_____________________ NEGATIVE BKG or AMO : exit w/hi value
  if (illogik) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif

  call PUT_TOGETHER
!_____________________ NEGATIVE BKG or AMO : exit w/hi value
  if (illogik) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif

  call CHISQ_TOT(v)
  if (v<v_overall_best.and.kpolfin == 0) then
    v_overall_best = v
    call STAGE_POLYT_OUT(99,99,v)
  endif
  call CPU_TIME(timepolyt)
  timecy = timepolyt-timecy
  timepolyt = timepolyt-t0polyt

 end subroutine FCN_POLYTOPE
!***********************************************************************************************************************
 subroutine FCNV_POLYTOPE(np,p,npu,v_calc)
  INTEGER(I4B),INTENT(IN)              :: np, npu
  REAL(CP),DIMENSION(np),INTENT(IN)    :: p
  REAL(CP),INTENT(OUT)                 :: v_calc(npu)
  REAL(CP)              :: timecy, v
  INTEGER(I4B)          :: icon,i
  INTEGER(I4B),PARAMETER :: ise=1

  kouncyp = kouncyp + 1
  call CPU_TIME(timecy)
  call LOOP_CALC(np,p,icon)
  if (icon==0) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif
  call RENORM_CAL

  call refy_SCAL
!_____________________ NEGATIVE BKG or AMO : exit w/hi value
  if (illogik) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif

  call PUT_TOGETHER
!_____________________ NEGATIVE BKG or AMO : exit w/hi value
  if (illogik) then
    call RANDOM_NUMBER(v)
    v = MAX(NINT(100.0_DP*start_temp),kouncyp)*exp(v)
    return
  endif

  call CHISQ_TOT(v)
  if (v<v_overall_best) then
    v_overall_best = v
    call STAGE_POLYT_OUT(99,99,v)
  endif
  if (v>100.0_DP) then
    print'(a,1x,g22.16,i10,10(1x,g22.16))', 'FCNV_POLYTOPE - Chi^2H=',&
      v,kouncyp,timepolyt,timecy,(BACKGROUND(i)%min_of_B,i=1,NSET_W),&
      (AMORPHOUS(i)%min_of_A,i=1,NSET_W)
  else
    print'(a,1x,g22.16,i10,10(1x,g22.16))', 'FCNV_POLYTOPE - Chi^2 =',&
      v,kouncyp,timepolyt,timecy,(BACKGROUND(i)%min_of_B,i=1,NSET_W),&
      (AMORPHOUS(i)%min_of_A,i=1,NSET_W)
  endif

  v_calc = CALTOT_W(ise)%vdata


 end subroutine FCNV_POLYTOPE
!********** ********** ********** ********** ********** ********** ********** ********** 

 SUBROUTINE RENORM_CAL

  implicit none
  real(DP)      :: ual
  INTEGER(I4B)  :: i,j, ia


  do i=1,NSET_W
    do j=1,NSTR_W
      ual = sqrt(max(zero,SUM((CALPHA_W(i,j)%vdata**2)*OBS_DATA_W(i)%wdata)))
            
!      do ia=1,NDATA_W(i)
!        print*, 'RENORM_CAL = ', OBS_DATA_W(i)%t2data(ia),OBS_DATA_W(i)%vdata(ia), CALPHA_W(i,j)%vdata(ia)
!    enddo
      
       if (ual>=eps_DP) then
        ual = Scales(i)%NAT_Scale/ual
      else
        ual=zero
      endif
      CALPHA_W(i,j)%vdata=CALPHA_W(i,j)%vdata*ual
      Scales(i)%ALLscal(j) = ual
      Scales(i)%STRscal_W(j)=ual
    enddo
  enddo

 END SUBROUTINE RENORM_CAL
!********** ********** ********** ********** ********** ********** ********** ********** 
 SUBROUTINE LOOP_CALC(nparstage,parstage,ungar)  !!! HIGH_LEVEL SUBROUTINE

   implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
   INTEGER(I4B),OPTIONAL,INTENT(IN)              :: nparstage    ! nparstage = n. par. da raffinare
   REAL(CP),OPTIONAL,DIMENSION(:),INTENT(IN)     :: parstage
   INTEGER(I4B),intent(OUT)                      :: ungar
   REAL(CP),ALLOCATABLE,DIMENSION(:)             :: psend
   INTEGER(I4B)                                  :: npsend,spsend

   
   IF ( (PRESENT(nparstage) .and.(.not.PRESENT(parstage))) .or. &
        (PRESENT(parstage) .and.(.not.PRESENT(nparstage))) ) STOP 'LOOP_CALC: error in input'

   IF (PRESENT(nparstage)) THEN
     IF (size(parstage)/=nparstage)  STOP 'LOOP_CALC: error in input'

     IF (ALLOCATED(psend)) DEALLOCATE(PSEND)
     ALLOCATE(PSEND(nparstage))
     spsend = 0
     STRULOOP:DO i=1,NSTR_W
       IF (DB_INDEX_W(i) > 5) CYCLE

       IF (DB_INDEX_W(i) == 1) THEN
         DO j=1,NSET_W
           call CONCK_DB2(Curr_Str=i,Curr_Set=j,iconsist=ungar)
           if (ungar==0) RETURN
           call NANO_CALC_DBX(Curr_Str=i,Curr_Set=j)
         ENDDO
       ELSE
         psend = zero
         npsend          = SUM(PARAPHAS(i)%nano_doit)
         psend(1:npsend) = parstage(spsend+1:spsend+npsend)
       
         DO j=1,NSET_W
           call CONCK_DB2(npin=npsend,pin=psend(1:npsend),Curr_Str=i,Curr_Set=j,iconsist=ungar)
           if (ungar==0) RETURN
           call NANO_CALC_DBX(npin=npsend,pin=psend(1:npsend),Curr_Str=i,Curr_Set=j)
         ENDDO

         spsend = spsend+npsend
       
       ENDIF   

     ENDDO STRULOOP
!____________________ AMORPH as last
     psend = zero
     npsend          = SUM(PARAGLOB%nano_doit)
     psend(1:npsend) = parstage(spsend+1:spsend+npsend)

     IF (DO_AMORPH_W) THEN
       DO j=1,NSET_W
         call NANO_UPDATE_AMO(npin=npsend,pin=psend(1:npsend),Curr_Set=j)
       ENDDO
     ENDIF

     spsend = spsend+npsend

     IF (illogik) then
       DO i=1,NSTR_W
         DO j=1,NSET_W
           call RANDOM_NUMBER(CALPHA_W(j,i)%vdata(:))
         ENDDO
       ENDDO
     ENDIF

     kount_calc=kount_calc+1

     IF (ALLOCATED(psend)) DEALLOCATE(PSEND)

   ELSE

     STRULOOP2:DO i=1,NSTR_W

!       IF (DB_INDEX_W(i) < 1 .or. DB_INDEX_W(i) > 4) CYCLE
       IF (DB_INDEX_W(i) > 5) CYCLE
       DO j=1,NSET_W
         call CONCK_DB2(Curr_Str=i,Curr_Set=j,iconsist=ungar)
         if (ungar==0) RETURN
         call NANO_CALC_DBX(Curr_Str=i,Curr_Set=j)
         if (illogik) exit STRULOOP2
       ENDDO
     ENDDO STRULOOP2
     IF (DO_AMORPH_W) THEN
       DO j=1,NSET_W
         call NANO_UPDATE_AMO(Curr_Set=j)
       ENDDO
     ENDIF
     IF (illogik) then
       DO i=1,NSTR_W
         DO j=1,NSET_W
           call RANDOM_NUMBER(CALPHA_W(j,i)%vdata(:))
         ENDDO
       ENDDO
     ENDIF

   ENDIF

 END  SUBROUTINE LOOP_CALC

!********** ********** ********** ********** ********** ********** ********** ********** 
 SUBROUTINE PUT_TOGETHER
!____ To be called after filling up Scales, e.g. by calling refy_SCAL
   IMPLICIT NONE
   INTEGER(I4B)  :: i,ii,Iset,auxn2,haveam
   REAL(DP)      :: sigma,suscam,newN

   DO Iset = 1, NSET_W

     CALTOT_W(Iset)%vdata = zero
     BACKGROUND(Iset)%lin_bkgr_tot = zero
     BACKGROUND(Iset)%lin_bkgr_blank = zero
     IF (DO_AMORPH_W) THEN
       AMORPHOUS(Iset)%amo_scat_tot =  zero
       AMORPHOUS(Iset)%min_of_A = zero
     ENDIF

     sigma = Scales(Iset)%SETscal

     if (NSET_W>1) print'(1x,a,i5,2(a,f15.8))','Set ',Iset,' : SCALE = ',sigma,' +/- ',Scales(Iset)%E_SETscal
     print'(1x,a,10(1x,g12.6))', 'Scales for Icalc ',Scales(Iset)%STRscal
     print'(1x,a,10(1x,g12.6))', 'Scales for Iback ',Scales(Iset)%BKGscal
     
     ! FB mar17 simsum  
     if (sim_sum) then 
                write(simsum,('(1x,a,i5,2(a,f15.8))'))'Set ',Iset,' : SCALE = ',sigma,' +/- ',Scales(Iset)%E_SETscal
        write(simsum,('(1x,a)')) 'Scales for Icalc :'
        do i=1,NSTR_W
          write(simsum,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%STRscal(i),Scales(Iset)%E_STRscal(i)
        enddo
        write(simsum,('(1x,a)')) 'Scales for Iback :'
        do i=1,size(Scales(Iset)%BKGscal)
          write(simsum,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%BKGscal(i),Scales(Iset)%E_BKGscal(i)
        enddo
        write(simsum,*)' '
     close(simsum)     
     endif
     

     IF (ref_fin) THEN
        write(reffinu,('(1x,a,i5,2(a,f15.8))'))'Set ',Iset,' : SCALE = ',sigma,' +/- ',Scales(Iset)%E_SETscal
        write(reffinu,('(1x,a)')) 'Scales for Icalc :'
        do i=1,NSTR_W
          write(reffinu,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%STRscal(i),Scales(Iset)%E_STRscal(i)
        enddo
        write(reffinu,('(1x,a)')) 'Scales for Iback :'
        do i=1,size(Scales(Iset)%BKGscal)
          write(reffinu,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%BKGscal(i),Scales(Iset)%E_BKGscal(i)
        enddo
        write(reffinu,*)' '
        close(reffinu)
        
        write(reffinu1,('(1x,a,i5,2(a,f15.8))'))'Set ',Iset,' : SCALE = ',sigma,' +/- ',Scales(Iset)%E_SETscal
        write(reffinu1,('(1x,a)')) 'Scales for Icalc :'
        do i=1,NSTR_W
          write(reffinu1,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%STRscal(i),Scales(Iset)%E_STRscal(i)
        enddo
        write(reffinu1,('(1x,a)')) 'Scales for Iback :'
        do i=1,size(Scales(Iset)%BKGscal)
          write(reffinu1,'(i4,1x,g15.8," +- ",1x,g15.8)')i,Scales(Iset)%BKGscal(i),Scales(Iset)%E_BKGscal(i)
        enddo
        write(reffinu1,*)' '
     ENDIF

     do i=1,NSTR_W
       CALTOT_W(Iset)%vdata = CALTOT_W(Iset)%vdata + Scales(Iset)%STRscal(i) * CALPHA_W(Iset,i)%vdata
     enddo
     CALTOT_W(Iset)%vdata = CALTOT_W(Iset)%vdata * sigma
!__________ 

     IF (DO_AMORPH_W) THEN
       haveam = 1
       suscam = zero
       do i=1,NAMO_W
         if (ABS(Scales(Iset)%AMOscal(i)) <= eps_DP) Scales(Iset)%AMOscal(i) = zero
         suscam = suscam + ABS(Scales(Iset)%AMOscal(i)*AMORPHOUS(Iset)%addbase(i))
       enddo
       newN = zero
       if (suscam < NAMO_W*eps_DP) then 
         haveam = 0
       endif
       if (haveam == 1) then
         do i=1,NAMO_W
           AMORPHOUS(Iset)%amo_scat_tot = AMORPHOUS(Iset)%amo_scat_tot  &
                                        + Scales(Iset)%AMOscal(i)*AMORPHOUS(Iset)%amo_scat_sep(:,i)
         enddo
         AMORPHOUS(Iset)%min_of_A = MINVAL(AMORPHOUS(Iset)%amo_scat_tot)
         AMORPHOUS(Iset)%amo_scat_tot = AMORPHOUS(Iset)%amo_scat_tot-AMORPHOUS(Iset)%min_of_A
         BACKGROUND(Iset)%lin_bkgr_tot = AMORPHOUS(Iset)%min_of_A
       endif
     endif
     
     do i=1,BACKGROUND(Iset)%DIMBACK
       BACKGROUND(Iset)%lin_bkgr_tot = BACKGROUND(Iset)%lin_bkgr_tot &
                                     + Scales(Iset)%BKGscal(i) * BACKGROUND(Iset)%lin_bkgr_sep(:,i)
     enddo
     BACKGROUND(Iset)%lin_bkgr_tot = BACKGROUND(Iset)%lin_bkgr_tot * sigma
     if (BACKGROUND(Iset)%DIMBACK > BACKGROUND(Iset)%nbpol) then
       do i=BACKGROUND(Iset)%nbpol+1,BACKGROUND(Iset)%DIMBACK
         BACKGROUND(Iset)%lin_bkgr_blank = BACKGROUND(Iset)%lin_bkgr_blank &
                                         + Scales(Iset)%BKGscal(i) * BACKGROUND(Iset)%lin_bkgr_sep(:,i)
       enddo
       BACKGROUND(Iset)%lin_bkgr_blank = BACKGROUND(Iset)%lin_bkgr_blank * sigma
     endif
     BACKGROUND(Iset)%min_of_B = MINVAL(BACKGROUND(Iset)%lin_bkgr_tot)
     IF (MINVAL(BACKGROUND(Iset)%lin_bkgr_tot)<-eps_DP) THEN
       print*,'%%% PROBLEM ',MINVAL(BACKGROUND(Iset)%lin_bkgr_tot)
!       if (NSET_W==1) BACKGROUND(Iset)%lin_bkgr_tot = MAX(zero,BACKGROUND(Iset)%lin_bkgr_tot)
     ENDIF
!!!!!!!!!!!!!!!!!! NO SOFT-PENALTY 
     CALTOT_W(Iset)%vdata = CALTOT_W(Iset)%vdata + BACKGROUND(Iset)%lin_bkgr_tot
     IF (DO_AMORPH_W) CALTOT_W(Iset)%vdata = CALTOT_W(Iset)%vdata + AMORPHOUS(Iset)%amo_scat_tot
!!!!!!!!!!!!!!!!!! this line is just for beauty
   ENDDO


 END SUBROUTINE PUT_TOGETHER
!********** ********** ********** ********** ********** ********** ********** ********** 
 SUBROUTINE CHISQ_TOT(chi2)

!____ To be called after PUT_TOGETHER

   IMPLICIT NONE
   REAL(CP),intent(OUT)      :: chi2
   INTEGER(I4B)              :: i,ii,Iset,gat(1)
   REAL(DP)                  :: sigma,canor,xamb
   REAL(DP),allocatable      :: micio(:)

   chi2 = zero
   canor= zero
   if (allocated(micio)) deallocate(micio)
   DO Iset = 1, NSET_W
!     allocate(micio(NDATA_W(Iset)))
     chi2 = chi2 + SUM(OBS_DATA_W(Iset)%wdata * (CALTOT_W(Iset)%vdata - OBS_DATA_W(Iset)%vdata)**2)
!____________ Background can be negative in case of subtracted data!
!             IF with a flag?
!     micio = MIN(zero,BACKGROUND(Iset)%lin_bkgr_tot)
!     IF (DO_AMORPH_W) micio = MIN(zero,BACKGROUND(Iset)%lin_bkgr_tot+AMORPHOUS(Iset)%amo_scat_tot)
!     chi2=chi2+0.01_DP*SUM(micio*micio*OBS_DATA_W(Iset)%wdata)
!____________ 
     canor = canor+REAL(NDATA_W(Iset),DP)
!     deallocate(micio)
   ENDDO
   chi2 = chi2/canor
!!print*,'DEBUG: chi^2 CHISQ_TOT= ',chi2


 END SUBROUTINE CHISQ_TOT
!********** ********** ********** ********** ********** ********** ********** ********** 
 SUBROUTINE SET_STARTING_TEMP

!____ To be called after PUT_TOGETHER

   IMPLICIT NONE
   INTEGER(I4B)              :: i,ii,Iset
   REAL(DP)                  :: sigma,canor,normob

   normob = zero
   canor= zero
   DO Iset = 1, NSET_W
     normob = normob + SUM(OBS_DATA_W(Iset)%wdata * (OBS_DATA_W(Iset)%vdata**2))
     canor = canor+REAL(size(OBS_DATA_W(Iset)%wdata),DP)
   ENDDO
   start_temp = target_E_O*normob/canor


 END SUBROUTINE SET_STARTING_TEMP
!********** ********** ********** ********** ********** ********** ********** ********** 
SUBROUTINE write_cal_fileX(kset,iu, difpdf)

  implicit none
  INTEGER(I4B),INTENT(IN)         :: kset, iu
  real(DP),dimension(10000,3),optional,intent(OUT)   :: difpdf
  INTEGER(I4B)                    :: i, jj, n0, nn, n1, n2, nn2
  character(512)                  :: wline
  real(DP),dimension(10000)   :: r_pdf
  

  wline = ''
  n1 = 16
  n2 = n1*2
  if (PRESENT(difpdf)) then
    difpdf = zero
    r_pdf = [(0.01d0*i,i=1,10000)]
  endif

      do jj=1,NDATA_W(kset)
         n0 = 1
         nn2 = n1
         nn=nn2
         IF (XDATA_TYPE_W(kset) == 'A') THEN
             write(wline(n0:nn),'(1x,f15.8)') Obs_data_W(kset)%t2data(jj)
         ELSE
             write(wline(n0:nn),'(1x,g15.9)') Obs_data_W(kset)%qdata(jj,1)
         ENDIF
         n0 = nn+1
         nn2 = nn+n2
         nn=nn2
         write(wline(n0:nn),'(2(1x,g15.9))') Obs_data_W(kset)%vdata(jj), CALTOT_W(kset)%vdata(jj)
         IF (CALC_FLAG==1) then
           do i=1,NSTR_W
             IF (DB_INDEX_W(i) < 1 .or. DB_INDEX_W(i) > 5) CYCLE
             n0 = nn+1
             nn2 = nn+n1
             nn=nn2
             write(wline(n0:nn),'(1x,g15.9)') CALPHA_W(kset,i)%vdata(jj)*Scales(kset)%STRscal(i)*Scales(kset)%SETscal 
           enddo
         endif
         n0 = nn+1
         nn2 = nn+n2
         nn=nn2
         write(wline(n0:nn),'(2(1x,g15.9))') BACKGROUND(kset)%lin_bkgr_tot(jj),BACKGROUND(kset)%lin_bkgr_blank(jj)
         write(iu,'(a)') trim(adjustl(wline))
         wline=''
      enddo

  if (PRESENT(difpdf)) then
    call Brutal_sincTransf(Y=Obs_data_W(kset)%vdata(:),R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,1),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
    call Brutal_sincTransf(Y = CALTOT_W(kset)%vdata(:),R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,2),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
    call Brutal_sincTransf(Y = BACKGROUND(kset)%lin_bkgr_tot(:)+BACKGROUND(kset)%lin_bkgr_blank(:), &
                           R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,3),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
  endif

 END SUBROUTINE write_cal_fileX
!********** ********** ********** ********** ********** ********** ********** **********  
! subroutine Brutal_sincTransf(Y,R,tt,F,isetin,xlamw)
!implicit none
!real(DP),dimension(:),intent(IN)  :: Y,R,tt
!integer(I4B),optional,intent(IN) :: isetin
!real(DP),intent(IN)              :: xlamw
!real(DP),dimension(:),intent(OUT) :: F
!real(DP),dimension(size(tt)) :: dtt,dq,q,xlamw
!integer(I4B) :: n,m,j,ks
!
!ks=1
!if (PRESENT(isetin)) then
!  ks=isetin
!endif
!n=size(tt)
!print*,'********** ',n,size(Y),size(R)
!dtt(1)=tt(2)-tt(1)
!do j=2,n-1
!  dtt(j)=(tt(j+1)-tt(j-1))*half
!enddo
!dtt(n)=tt(n)-tt(n-1)
!
!q=two*SIN(tt*duet2r)/xlamw
!dq=dtt*duet2r*two*COS(tt*duet2r)/xlamw
!
!F=zero
!do j=1,n
!  F=F+Y(j)*q(j)*dq(j)*sin(pi2*q(j)*R)
!enddo
!F=F*eight*Pi
!
!end subroutine Brutal_sincTransf
!********** ********** ********** ********** ********** ********** ********** **********  
Subroutine FUNNY_DISTRU(M,V,N,P)
implicit none
integer(I4B),intent(IN) :: M,N
real(DP),intent(IN) :: P(N)
real(DP),intent(OUT) :: V(M)
real(DP),dimension(:),allocatable :: Pallv
real(DP) :: xfack,chi2dum

if (allocated(Pallv)) deallocate(Pallv)
allocate(Pallv(Nallp_all))
Pallv=Pall_refined
Pallv(point_par(1:N)) = P
call FCN_POLYTOPE(Np=Nallp_all,P=Pallv,v=chi2dum)
!print*,'FUNNY_DISTRU: dummy chi2 = ',chi2dum

xfack=sum(DISTRU(ncut_0_off(1,istr0):ncut_0_off(2,istr0),istr0,1))
V = DISTRU(ncut_0_off(1,istr0):ncut_0_off(2,istr0),istr0,1)/xfack


end Subroutine FUNNY_DISTRU
!********** ********** ********** ********** ********** ********** ********** **********  
Subroutine FUNNY_DISTRU2(M,V,N,P)
implicit none
integer(I4B),intent(IN) :: M,N
real(DP),intent(IN) :: P(N)
real(DP),intent(OUT) :: V(M)
real(DP),dimension(:),allocatable :: Pallv
real(DP) :: xfack,chi2dum

if (allocated(Pallv)) deallocate(Pallv)
allocate(Pallv(Nallp_all))
Pallv=Pall_refined
Pallv(point_par(1:N)) = P
call FCN_POLYTOPE(Np=Nallp_all,P=Pallv,v=chi2dum)
!print*,'FUNNY_DISTRU2: dummy chi2 = ',chi2dum

xfack=sum(DISTRU(ncut_0_off(1,istr0):ncut_0_off(2,istr0),istr0,1))
V = DISTRU(ncut_0_off(1,istr0):ncut_0_off(2,istr0),istr0,1)/xfack


end Subroutine FUNNY_DISTRU2
!********** ********** ********** ********** ********** ********** ********** **********  
!********** ********** ********** ********** ********** ********** ********** **********  
Subroutine FUNNY_ALLCOL(N,P,isg,ipv,dpv)
implicit none
integer(I4B),intent(IN) :: N,isg,ipv
real(DP),intent(IN) :: P(N),dpv
real(DP) :: xfack,chi2dum,oxfack,o2dpv
integer(I4B) :: istr,ip,ksp,n00x,n01x,kk,ii

! N = np_std_all

call FCN_POLYTOPE(Np=N,P=P,v=chi2dum)
!print*,'FUNNY_ALLCOL: dummy chi2 = ',chi2dum
o2dpv=half/dpv

do istr=1,NSTR_W
  n00x=ncut_0_off(1,istr)
  n01x=ncut_0_off(2,istr)
  if (isg==0) then
    xfack=sum(DISTRU(n00x:n01x,istr,1))
    oxfack=one/xfack
    allcolout(istr)%J_mucol(:,ipv,1) = -DISTRU(n00x:n01x,istr,1) * oxfack
    allcolout(istr)%J_mucol(:,ipv,2) = -DISTRU(n00x:n01x,istr,2)
    kk=2
    do ksp=1,nano_iav(istr)%struk(n00x)%numspat
      kk=kk+1
      allcolout(istr)%J_mucol(:,ipv,kk) = -[(nano_iav(istr)%struk(ii)%occusite(ksp),ii=n00x,n01x)]
      kk=kk+1
      allcolout(istr)%J_mucol(:,ipv,kk) = -[(nano_iav(istr)%struk(ii)%DebyeWallerB(ksp),ii=n00x,n01x)]
    enddo
  else if (isg==1) then
    xfack=sum(DISTRU(n00x:n01x,istr,1))
    oxfack=one/xfack
    allcolout(istr)%J_mucol(:,ipv,1) = allcolout(istr)%J_mucol(:,ipv,1) + DISTRU(n00x:n01x,istr,1) * oxfack
    allcolout(istr)%J_mucol(:,ipv,2) = allcolout(istr)%J_mucol(:,ipv,2) + DISTRU(n00x:n01x,istr,2)
    kk=2
    do ksp=1,nano_iav(istr)%struk(n00x)%numspat
      kk=kk+1
      allcolout(istr)%J_mucol(:,ipv,kk) = allcolout(istr)%J_mucol(:,ipv,kk) + &
                                          [(nano_iav(istr)%struk(ii)%occusite(ksp),ii=n00x,n01x)]
      kk=kk+1
      allcolout(istr)%J_mucol(:,ipv,kk) = allcolout(istr)%J_mucol(:,ipv,kk) + &
                                          [(nano_iav(istr)%struk(ii)%DebyeWallerB(ksp),ii=n00x,n01x)]
    enddo
    allcolout(istr)%J_mucol(:,ipv,1:kk) = allcolout(istr)%J_mucol(:,ipv,:) * o2dpv
  endif
enddo

end Subroutine FUNNY_ALLCOL
!********** ********** ********** ********** ********** ********** ********** ********** 
SUBROUTINE write_varia_fileX(kset,iu)

  implicit none
  INTEGER(I4B),INTENT(IN)         :: kset, iu
  INTEGER(I4B)                    :: i, jj, n0, nn, k, ll, klin(2), n, n_2nd, maxn_ab, istr,maxn_c, iu1, iu2, iu1e,&
                                     jats,lns,lns1,inm,iugr,ksp,kasp,iuxxx, np_std,minab,minc,maxab,maxc,mins,maxs, &
                                     sk,skk,isk,iskk,ck,sk0,sk1,maxk,np_fun,kk, matisse(5,0:6,2) , kk2, &
                                     ipas1,ipas2,n_pas,toallo,np_std_all, ncol_str
                                     !___ matisse=matrix index str(e)in, locating the 1st and last str. par. 
                                     !___ look at calcfun_DBX for the corect entries!
  INTEGER(I4B)                    :: n00x,n01x,ii
                                     
  INTEGER(I4B), Allocatable       :: kk1(:),nsto(:),nso(:),nupar_pha(:),nupar_at_pha(:), nuat_pha(:)
  character(132)                  :: sgline
  character(14)                   :: filegnuL,filegnuS
  character(2)                    :: astr,spat
  character(1024)                  :: hlix
  character(512)                  :: specstr,wrtstr,specstr1,specstrl,E_specstr,E_specstr1
  real(DP)  :: jack(2,100),john(2),sumck(2),cak, a_bulk, Delta_R, &
               singa, thr_Psize, sum1d_ab, sum1d_c, xnd, xfack, Zfack, &
               xxx1,xxx2,xxx0,avestd(2),avestd_2D(5,2),corrcoe,corrphi, eigval(2),eigvec(2,2),eigang,&
               nf1,mf1,dc1,nfx,mfx,dcx,oxfack
               

  if (ALLOCATED(allcolout)) then
    do istr=1,size(allcolout)
      if (ALLOCATED(allcolout(istr)%mucol)) deallocate(allcolout(istr)%mucol)
      if (ALLOCATED(allcolout(istr)%E_mucol)) deallocate(allcolout(istr)%E_mucol)
      if (ALLOCATED(allcolout(istr)%J_mucol)) deallocate(allcolout(istr)%J_mucol)
    enddo
    deallocate(allcolout)
  endif
  allocate(allcolout(NSTR_W))
  np_std_all = 0
  do istr=1,NSTR_W
    np_std_all = np_std_all + SUM(PARAPHAS(istr)%nano_doit)
  enddo
  do istr=1,NSTR_W
    ncol_str = 2 + 2 * nano_iav(istr)%struk(ncut_0_off(1,istr))%numspat
    allcolout(istr)%ncolpha = ncol_str
    np_fun=ncut_0_off(2,istr)-ncut_0_off(1,istr)+1
    allcolout(istr)%dimcolpha = np_fun
    allocate(allcolout(istr)%mucol(np_fun,ncol_str),allcolout(istr)%E_mucol(np_fun,ncol_str), &
             allcolout(istr)%J_mucol(np_fun,np_std_all,ncol_str))
             
             allcolout(istr)%J_mucol=zero; allcolout(istr)%mucol=zero; allcolout(istr)%E_mucol = zero
  enddo
  if (ALL(DB_INDEX_W==1)) return
  Fstdev_mode = 1


  sgline = ' '

  jack=zero
  john=zero  
  thr_Psize = 0.0001_DP

  IF (Allocated(diam_ave)) Deallocate(diam_ave)
  IF (Allocated(diam_sig)) Deallocate(diam_sig)
  IF (Allocated(mass_ave)) Deallocate(mass_ave)
  IF (Allocated(mass_sig)) Deallocate(mass_sig)
  IF (Allocated(vol_ave)) Deallocate(vol_ave)
  IF (Allocated(vol_sig)) Deallocate(vol_sig)
  
  IF (Allocated(CLU_DIAM)) Deallocate(CLU_DIAM)
  IF (Allocated(CLU_VOL)) Deallocate(CLU_VOL)
  IF (Allocated(CLU_MAS)) Deallocate(CLU_MAS)
  IF (Allocated(E_CLU_MAS)) Deallocate(E_CLU_MAS)
  IF (Allocated(CLU_SIZES)) Deallocate(CLU_SIZES)
  IF (Allocated(DISTRM)) Deallocate(DISTRM)
  IF (Allocated(E_DISTRM)) Deallocate(E_DISTRM)
  IF (Allocated(CLU_MAS_1)) Deallocate(CLU_MAS_1)
  IF (Allocated(DISTRM_1)) Deallocate(DISTRM_1)
  IF (Allocated(CLU_MAS_2)) Deallocate(CLU_MAS_2)
  IF (Allocated(DISTRM_2)) Deallocate(DISTRM_2)

  
  IF (Allocated(DISTRU_ROD)) Deallocate(DISTRU_ROD)
  IF (Allocated(rod_ave)) Deallocate(rod_ave)
  IF (Allocated(rod_sig)) Deallocate(rod_sig)
  IF (Allocated(massfrac_struc)) Deallocate(massfrac_struc)
  
  Allocate(diam_ave(1:NSTR_W,2), diam_sig(1:NSTR_W,2), mass_ave(1:NSTR_W,2), mass_sig(1:NSTR_W,2), &
           CLU_DIAM(1:NLARGEST1,1:NSTR_W), CLU_VOL(1:NLARGEST1,1:NSTR_W), CLU_MAS(1:NLARGEST1,1:NSTR_W), &
           E_CLU_MAS(1:NLARGEST1,1:NSTR_W), &
           DISTRM(1:NLARGEST1,1:NSTR_W), E_DISTRM(1:NLARGEST1,1:NSTR_W), vol_ave(1:NSTR_W,2), vol_sig(1:NSTR_W,2), &
           massfrac_struc(1:NSTR_W),CLU_SIZES(2,1:NLARGEST1,1:NSTR_W))
  IF (Allocated(nupar_pha)) Deallocate(nupar_pha)
  IF (Allocated(nupar_at_pha)) Deallocate(nupar_at_pha)
  IF (Allocated(nuat_pha)) Deallocate(nuat_pha)
  
  ALLOCATE(nupar_pha(NSTR_W),nupar_at_pha(NSTR_W), nuat_pha(NSTR_W))
  do i=1,NSTR_W
    nupar_pha(i) = SUM(PARAPHAS(i)%nano_doit)
  enddo
  
  DISTRM = zero
  E_DISTRM = zero
  massfrac_struc = zero
  
  IF(ANY(DB_INDEX_W == 4)) THEN
     maxn_ab = maxval(nano_iav(:)%dimstruk1)
     maxn_c = maxval(nano_iav(:)%dimstruk2)
     if (allocated(rod_ave_ab)) deallocate(rod_ave_ab)
     if (allocated(rod_sig_ab)) deallocate(rod_sig_ab)
     if (allocated(rod_ave_c)) deallocate(rod_ave_c)
     if (allocated(rod_sig_c)) deallocate(rod_sig_c)
     if (allocated(DISTRU_ROD)) deallocate(DISTRU_ROD)
     if (allocated(rod_ave)) deallocate(rod_ave)
     if (allocated(rod_sig)) deallocate(rod_sig)
     if (allocated(projND_ab)) deallocate(projND_ab)
     if (allocated(projMD_ab)) deallocate(projMD_ab)
     if (allocated(projND_c)) deallocate(projND_c)
     if (allocated(projMD_c)) deallocate(projMD_c)
     if (allocated(CLU_MAS_1)) deallocate(CLU_MAS_1)
     if (allocated(CLU_MAS_2)) deallocate(CLU_MAS_2)
     if (allocated(DISTRM_1)) deallocate(DISTRM_1)
     if (allocated(DISTRM_2)) deallocate(DISTRM_2)
     Allocate(rod_ave_ab(2,NSTR_W), &
              rod_sig_ab(2,NSTR_W), &
              rod_ave_c(2,NSTR_W), &
              rod_sig_c(2,NSTR_W), &
              DISTRU_ROD(maxn_ab,maxn_c,NSTR_W), &
              rod_ave(NSTR_W,2), &
              rod_sig(NSTR_W,2), &
              projND_ab(maxn_ab),projMD_ab(maxn_ab),projND_c(maxn_c),projMD_c(maxn_c))
              rod_ave = zero ; rod_sig = zero
              rod_ave_ab = zero; rod_sig_ab = zero
              rod_ave_c = zero; rod_sig_c = zero
              projND_ab=zero; projMD_ab=zero; projND_c=zero; projMD_c=zero
  ENDIF
  
  IF(ANY(DB_INDEX_W == 5)) THEN
     maxn_ab = maxval(nano_iav(:)%dimstruk1)
     maxn_c = maxval(nano_iav(:)%dimstruk2)
     if (allocated(rod_ave_ab)) deallocate(rod_ave_ab)
     if (allocated(rod_sig_ab)) deallocate(rod_sig_ab)
     if (allocated(rod_ave_c)) deallocate(rod_ave_c)
     if (allocated(rod_sig_c)) deallocate(rod_sig_c)
     if (allocated(DISTRU_ROD)) deallocate(DISTRU_ROD)
     if (allocated(rod_ave)) deallocate(rod_ave)
     if (allocated(rod_sig)) deallocate(rod_sig)
     if (allocated(projND_ab)) deallocate(projND_ab)
     if (allocated(projMD_ab)) deallocate(projMD_ab)
     if (allocated(projND_c)) deallocate(projND_c)
     if (allocated(projMD_c)) deallocate(projMD_c)
     if (allocated(CLU_MAS_1)) deallocate(CLU_MAS_1)
     if (allocated(CLU_MAS_2)) deallocate(CLU_MAS_2)
     if (allocated(DISTRM_1)) deallocate(DISTRM_1)
     if (allocated(DISTRM_2)) deallocate(DISTRM_2)
     Allocate(rod_ave_ab(2,NSTR_W), &
              rod_sig_ab(2,NSTR_W), &
              rod_ave_c(2,NSTR_W), &
              rod_sig_c(2,NSTR_W), &
              DISTRU_ROD(maxn_ab,maxn_c,NSTR_W), &
              rod_ave(NSTR_W,2), &
              rod_sig(NSTR_W,2), &
              projND_ab(maxn_ab),projMD_ab(maxn_ab),projND_c(maxn_c),projMD_c(maxn_c),&
              CLU_MAS_1(1:NLARGEST1,1:NSTR_W), CLU_MAS_2(1:NLARGEST1,1:NSTR_W),&
              DISTRM_1(1:NLARGEST1,1:NSTR_W), DISTRM_2(1:NLARGEST1,1:NSTR_W))
              rod_ave = zero ; rod_sig = zero
              rod_ave_ab = zero; rod_sig_ab = zero
              rod_ave_c = zero; rod_sig_c = zero
              projND_ab=zero; projMD_ab=zero; projND_c=zero; projMD_c=zero
  ENDIF
  
  !___ Calculate volumes and diameters and masses

  CLU_DIAM = zero
  CLU_VOL = zero
  CLU_MAS = zero
  E_CLU_MAS = zero
 
  call GRENZ_SET(spsend=Nallp_all)
  if (allocated(Pall_refined)) deallocate(Pall_refined)
  allocate(Pall_refined(Nallp_all))
  call SETUP_POLYTO_PAR( nparstage=Nallp_all, parstage=Pall_refined )
  
  
  
!  matisse = 0
!  do kk=2,3
!    matisse(kk,0,:)=[3,6]
!  enddo
!  do kk=4,5
!    matisse(kk,0,:)=[6,9]
!  enddo
!  
!  do kk=2,3
!    matisse(kk,1,:)=[3,3]
!  enddo
!  do kk=4,5
!    matisse(kk,1,:)=[6,6]
!  enddo
!  
!  do kk=2,3
!    matisse(kk,2,:)=[3,4]
!  enddo
!  do kk=4,5
!    matisse(kk,2,:)=[6,7]
!  enddo
!  do kk=2,3
!    matisse(kk,3,:)=[3,5]
!   enddo
!  do kk=4,5
!    matisse(kk,3,:)=[6,8]
!  enddo
!  do kk2=4,6
!    do kk=2,3
!      matisse(kk,kk2,:)=[3,4]
!    enddo
!    do kk=4,5
!      matisse(kk,kk2,:)=[6,7]
!    enddo
!  enddo
 
  if (kpolfin == 1) then
      call Err_FofPars_COL(fullpset=Pall_refined,nfull=Nallp_all)
  else
    do istr=1,NSTR_W
      n00x=ncut_0_off(1,istr)
      n01x=ncut_0_off(2,istr)
      allcolout(istr)%J_mucol = zero
      xfack=sum(DISTRU(n00x:n01x,istr,1))
      oxfack=one/xfack
      allcolout(istr)%mucol(:,1) = DISTRU(n00x:n01x,istr,1) * oxfack
      allcolout(istr)%mucol(:,2) = DISTRU(n00x:n01x,istr,2)
      kk=2
      do ksp=1,nano_iav(istr)%struk(n00x)%numspat
        kk=kk+1
        allcolout(istr)%mucol(:,kk) = [(nano_iav(istr)%struk(ii)%occusite(ksp),ii=n00x,n01x)]
        kk=kk+1
        allcolout(istr)%mucol(:,kk) = [(nano_iav(istr)%struk(ii)%DebyeWallerB(ksp),ii=n00x,n01x)]
      enddo
    enddo
  endif
  
  do istr=1,NSTR_W
    
    np_fun=ncut_0_off(2,istr)-ncut_0_off(1,istr)+1
    IF (DB_INDEX(istr) == 2) THEN
    
      a_bulk = CELL_P_W(1,istr)
      do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
        xnd = a_bulk* (k + half) * sd_conv(ITYPE_DB_W(istr)) 
        if (ITYPE_DB_W(istr) == 4) xnd = a_bulk*sd_conv(ITYPE_DB_W(istr)) * (k**3 + 1.5d0*k**2 + 0.25d0*k + 13.d0/8.d0)**unter
        CLU_DIAM(k,istr) = 2.d0 * 0.1d0 * xnd
        CLU_VOL(k,istr) = unses * pi * (CLU_DIAM(k,istr)**3)
        CLU_MAS(k,istr) = nano_iav(istr)%struk(k)%natclu * atwei(nano_iav(istr)%struk(k)%Z_at(1))
        CLU_SIZES(:,k,istr) = CLU_DIAM(k,istr)
      enddo
    ELSE IF (DB_INDEX(istr) == 3) THEN
      do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
        CLU_SIZES(1,k,istr) = nano_iav(istr)%struk(k)%act_diam(1)
        CLU_SIZES(2,k,istr) = CLU_SIZES(1,k,istr)
        CLU_DIAM(k,istr) = nano_iav(istr)%struk(k)%act_diam(1)
        ! CLU_VOL(k,istr) = (CLU_DIAM(k,istr)**three)*pi*unses
        CLU_VOL(k,istr) = (nano_iav(istr)%struk(k)%celvolr)*(nano_iav(istr)%struk(k)%ncelpha(1))/1000
        CLU_MAS(k,istr)=zero
        do kasp=1,nano_iav(istr)%struk(k)%natspglo
            CLU_MAS(k,istr) = CLU_MAS(k,istr)+atwei(nano_iav(istr)%struk(k)%specZ(kasp))*&
                                    nano_iav(istr)%struk(k)%xnat(kasp)*&
                                    nano_iav(istr)%struk(k)%occusite(kasp)
        enddo
      enddo
    ELSE IF (DB_INDEX(istr) == 4) THEN
      do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
        klin = nano_iav(istr)%post_office_I(:,k)
        n = klin(1)
        n_2nd = klin(2)
        CLU_SIZES(:,k,istr) = nano_iav(istr)%struk(k)%act_diam(:)
        ! CLU_VOL(k,istr) = pi*unqua*(CLU_SIZES(1,k,istr)**two)*CLU_SIZES(2,k,istr)
        CLU_VOL(k,istr) = (nano_iav(istr)%struk(k)%celvolr)*(nano_iav(istr)%struk(k)%ncelpha(1))/1000
        CLU_DIAM(k,istr) = (CLU_VOL(k,istr)*six/pi)**unter
        CLU_MAS(k,istr)=zero
        do kasp=1,nano_iav(istr)%struk(k)%natspglo
            CLU_MAS(k,istr) = CLU_MAS(k,istr)+atwei(nano_iav(istr)%struk(k)%specZ(kasp))*&
                                    nano_iav(istr)%struk(k)%xnat(kasp)*&
                                    nano_iav(istr)%struk(k)%occusite(kasp)
        enddo
      enddo
    ELSE IF (DB_INDEX(istr) == 5) THEN    
      do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
        klin = nano_iav(istr)%post_office_I(:,k)
        n = klin(1)
        n_2nd = klin(2)
        CLU_SIZES(:,k,istr) = nano_iav(istr)%struk(k)%act_diam(:)
        CLU_DIAM(k,istr) = sum(nano_iav(istr)%struk(k)%act_diam(:))
        ! CLU_VOL(k,istr) = (CLU_DIAM(k,istr)**three)*pi*unses
        CLU_VOL(k,istr) = (nano_iav(istr)%struk(k)%celvolr)*(nano_iav(istr)%struk(k)%ncelpha(1) + &
                           nano_iav(istr)%struk(k)%ncelpha(2))/1000
        CLU_MAS(k,istr)=zero
        CLU_MAS_1(k,istr)=zero
        CLU_MAS_2(k,istr)=zero
        do kasp=1,nano_iav(istr)%struk(k)%natspglo
            CLU_MAS(k,istr) = CLU_MAS(k,istr)+atwei(nano_iav(istr)%struk(k)%specZ(kasp))*&
                  nano_iav(istr)%struk(k)%xnat(kasp)*nano_iav(istr)%struk(k)%occusite(kasp)
            CLU_MAS_1(k,istr) = CLU_MAS_1(k,istr)+atwei(nano_iav(istr)%struk(k)%specZ(kasp))*&
                  nano_iav(istr)%struk(k)%specXN(kasp,1)*nano_iav(istr)%struk(k)%occusite(kasp)
            CLU_MAS_2(k,istr) = CLU_MAS_2(k,istr)+atwei(nano_iav(istr)%struk(k)%specZ(kasp))*&
                 nano_iav(istr)%struk(k)%specXN(kasp,2)*nano_iav(istr)%struk(k)%occusite(kasp)
        enddo
      enddo  
    
    endif
    
!__________ OP. 1 - distribution
    ! (re) normalze the number distributon
    ! elim/subst.
!!     xfack=sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1))
!!     DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1) = DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)/xfack
    ! rescale the scales 
    
!___________ do as function... OCT14
!    Nallp_thisphase = SUM(PARAPHAS(istr)%nano_doit)
!    if (allocated(point_par)) deallocate(point_par)
!    allocate(point_par(Nallp_thisphase))
!    point_par=0
!    IF (DB_INDEX(istr) >= 4) THEN
!      point_par(1:5)=[1,2,3,4,5]+grenzpha(1,istr)
!      np_std=5
!    ELSE IF (DB_INDEX(istr) == 3.or.DB_INDEX(istr) == 2) THEN
!      point_par(1:2)=[1,2]+grenzpha(1,istr)
!      np_std=2
!    ENDIF
!    np_fun=ncut_0_off(2,istr)-ncut_0_off(1,istr)+1
!    call FUNNY_DISTRU(M=ncut_0_off(2,istr)-ncut_0_off(1,istr)+1, &
!                 V=DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1),&
!                 N=np_std,P=Pall_refined(point_par(1:np_std)))
    istr0=istr;kset0=kset
    if (kpolfin == 1) then
!      E_DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1) = &
!        Err_FofPars( Funny = FUNNY_DISTRU, nfunny = np_fun, &
!                     npin = np_std, point_fullpset = point_par(1:np_std), &
!                     fullpset = Pall_refined, nfull = Nallp_all, &
!                     Jaco = JacMat)
      E_DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1) = allcolout(istr)%E_mucol(:,1)
      E_DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,2) = allcolout(istr)%E_mucol(:,2)
    endif

!___________ END do as function... OCT14
 
    
    Scales(kset)%ALLscal(istr) = Scales(kset)%STRscal_W(istr)*Scales(kset)%STRscal(istr)

  
    ! evaluate the average masses
    xxx0=zero
    do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
      xxx0 = xxx0+ DISTRU(k,istr,1)*CLU_MAS(k,istr) !nano_iav(istr)%struk(k)%clu_mass
    enddo
  
   
    massfrac_struc(istr) = xxx0 * Scales(kset)%ALLscal(istr) ! altro calcolo ...



    
    DISTRM(:,istr) = DISTRU(:,istr,1)*CLU_MAS(:,istr)
    xfack=sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    Zfack=one/xfack
    DISTRM(:,istr)=DISTRM(:,istr)*Zfack  ! 1/xfack = Z
    if (kpolfin == 1) then
      IF (ALLOCATED(JacMat)) deallocate(JacMat); allocate(JacMat(Nallp_all,np_fun))
      IF (ALLOCATED(JacMat_II)) deallocate(JacMat_II); allocate(JacMat_II(Nallp_all,np_fun))
      JacMat = TRANSPOSE( allcolout(istr)%J_mucol(:,1:Nallp_all,1) )
      do k=1,np_fun
        kk=k+ncut_0_off(1,istr)-1
        do i=1,Nallp_all
          JacMat_II(i,k) = Zfack*CLU_MAS(kk,istr) * &
                           ( JacMat(i,k) - Zfack * DISTRU(kk,istr,1) * &
                           sum(JacMat(i,:)*CLU_MAS(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)) )
        enddo
      enddo
      E_DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr) = &
        Err_FofPars_Jac( nfunny = np_fun, &
                         fullpset = Pall_refined, nfull = Nallp_all, &
                         Jaco = JacMat_II)
    endif
!______________________________ STRAIN CURVE gia' fatto

!    Nallp_thisphase = SUM(PARAPHAS(istr)%nano_doit)
!    if (allocated(point_par)) deallocate(point_par)
!    allocate(point_par(Nallp_thisphase))
!    point_par=0
!    ipas1 = matisse(DB_INDEX(istr),PARAPHAS(istr)%str_cod,1)+grenzpha(1,istr)
!    ipas2 = matisse(DB_INDEX(istr),PARAPHAS(istr)%str_cod,2)+grenzpha(1,istr)
!    n_pas = ipas2-ipas1+1
!    point_par(1:n_pas)=([(i,i=ipas1,ipas2)])
!    np_std=n_pas
!    istr0=istr;kset0=kset
!    np_fun=ncut_0_off(2,istr)-ncut_0_off(1,istr)+1
!    call FUNNY_DISTRU2(M=np_fun, &
!                 V=DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1),&
!                 N=np_std,P=Pall_refined(point_par(1:np_std)))
!    if (kpolfin == 1) then
!      E_DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,2) = &
!        Err_FofPars( Funny = FUNNY_DISTRU2, nfunny = np_fun, &
!                     npin = np_std, point_fullpset = point_par(1:np_std), &
!                     fullpset = Pall_refined, nfull = Nallp_all)
!    endif
!    

    IF (DB_INDEX(istr) == 5) THEN
      DISTRM_1(:,istr) = DISTRU(:,istr,1)*CLU_MAS_1(:,istr)
      DISTRM_1(:,istr)=DISTRM_1(:,istr)*Zfack
      DISTRM_2(:,istr) = DISTRU(:,istr,1)*CLU_MAS_2(:,istr)
      DISTRM_2(:,istr)=DISTRM_2(:,istr)*Zfack
    ENDIF

    !!!_________distributions check-file --  to be deleted
    !iuxxx = FIND_UNIT()
    !open(iuxxx, status='replace', file='check_dists.mtx',action='WRITE')
    !do k=ncut_0_off(1,istr),ncut_0_off(2,istr)
    !    write(iuxxx,'(2i6,4(3x,g15.8))') n, n_2nd, DISTRU(k,istr,1), DISTRM(k,istr),DISTRM_1(k,istr),DISTRM_2(k,istr)
    !enddo
    !close(iuxxx)
    !!!_______delete

    mass_ave(istr,1) = sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1) * &
                           CLU_MAS(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)*(CLU_MAS(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    mass_sig(istr,1) = sqrt(max(zero, xfack-mass_ave(istr,1)**2))
    
    mass_ave(istr,2) = sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)* &
                       CLU_MAS(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)*(CLU_MAS(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    mass_sig(istr,2) = sqrt(max(zero, xfack-mass_ave(istr,2)**2))
    
    diam_ave(istr,1) = sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)* &
                           CLU_DIAM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)*(CLU_DIAM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    diam_sig(istr,1) = sqrt(max(zero, xfack-diam_ave(istr,1)**2))
    
    diam_ave(istr,2) = sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)* &
                           CLU_DIAM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)*(CLU_DIAM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    diam_sig(istr,2) = sqrt(max(zero, xfack-diam_ave(istr,2)**2))
    
    
    
    vol_ave(istr,1) = sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)* &
                          CLU_vol(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1)*(CLU_vol(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    vol_sig(istr,1) = sqrt(max(zero, xfack-vol_ave(istr,1)**2))
    
    vol_ave(istr,2) = sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)* &
                          CLU_vol(ncut_0_off(1,istr):ncut_0_off(2,istr),istr))
    xfack=sum(DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)*(CLU_vol(ncut_0_off(1,istr):ncut_0_off(2,istr),istr)**2))
    vol_sig(istr,2) = sqrt(max(zero, xfack-vol_ave(istr,2)**2))
    
    ! univariate outputs
    write(iu,*)
    write(iu,*) 'STRUCTURE # ',istr, ' ***  DataBase-Code : ',DB_INDEX_W(istr)
    write(iu,*)
    write(iu,*) '*** Univariate structure statistics ***'
    write(iu,*)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Mass / Number Distribution ',mass_ave(istr,1),' +/- ',mass_sig(istr,1)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Mass / Mass Distribution   ',mass_ave(istr,2),' +/- ',mass_sig(istr,2)
    write(iu,*)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Diameter / Number Distribution ',diam_ave(istr,1),' +/- ',diam_sig(istr,1)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Diameter / Mass Distribution   ',diam_ave(istr,2),' +/- ',diam_sig(istr,2)
    write(iu,*)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Volume / Number Distribution ',vol_ave(istr,1),' +/- ',vol_sig(istr,1)
    write(iu,'(a,2x,g12.6,a,2x,g12.6)') 'Average/std Volume / Mass Distribution   ',vol_ave(istr,2),' +/- ',vol_sig(istr,2)
    write(iu,*)
    
    
    write(astr,'(i2.2)') istr
    IF (DB_INDEX_W(istr) == 4 .or. DB_INDEX_W(istr) == 5) THEN
        filegnuL(1:13) = astr(1:2)//'_plot2D.mtx'
    ELSE
        filegnuL(1:13) = astr(1:2)//'_plot1D.mtx'
    ENDIF
    iu1 = FIND_UNIT()
    lns1=lcut_structure_name(istr)
    lns=lstructure_name(istr)-4
    hlix=TRIM(path_output_files(1:lpath_output_files)//TRIM(structure_name_w(istr)(:))// &
        filegnuL(1:13))
    open(iu1, status='replace', file = trim(hlix), action='WRITE')

    if (kpolfin == 1) then
      iu1e = FIND_UNIT()
      open(iu1e, status='replace', file=path_output_files(1:lpath_output_files)// &
                         trim(structure_name_w(istr)(:))//filegnuL(1:13)//'e',action='WRITE')
    endif
     
    IF (DB_INDEX_W(istr) < 4)  THEN
        write(iu1,'("# Univariate distribution")')
        if (kpolfin == 1) write(iu1e,'("# Univariate distribution - Standard deviations")')
        specstr=''  
        do ksp=1,nano_iav(istr)%struk(1)%numspat
           write(spat(1:2),'(i2.2)') ksp
           specstrl="   OCC_ATOM_"//spat//"    Biso_ATOM_"//spat//"[A^2]"
           write(specstr(len_trim(specstr)+1:500),'(a)') trim(specstrl)
        enddo
        wrtstr='# n_shell     Number_Frac.       Mass_Frac.       Diam[nm]       Volume[nm^3]       Mass[amu]&
               &   Base_Diam.(e.c.)[nm]   Height[nm]       Lattice_Expan.[]'//TRIM(specstr)
        write(iu1,'(a)') TRIM(wrtstr)
        if (kpolfin == 1) write(iu1e,'(a)') TRIM(wrtstr)
   
       i=istr
         
       do k=ncut_0_off(1,i),ncut_0_off(2,i)
          specstr=""
          specstr1=''
          do ksp=1,nano_iav(istr)%struk(k)%numspat
            write(specstr1(1:30),'(2g15.8)') allcolout(istr)%mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1:2+ksp*2)
        !    write(specstr(len_trim(specstr)+6:500),'(a)') trim(specstr1)
            specstr=trim(trim(specstr)//'      '//trim(adjustl(specstr1)))
          enddo 
          write(iu1,'(i6,8(3x,g15.8),a)') k, DISTRU(k,i,1), DISTRM(k,i),CLU_DIAM(k,i),CLU_VOL(k,i),CLU_MAS(k,i), &
                                      CLU_SIZES(:,k,i), DISTRU(k,i,2:2),TRIM(specstr)
          if (kpolfin == 1) then
            E_specstr=""
            E_specstr1=''
            E_CLU_MAS(k,i) = zero
            do ksp=1,nano_iav(istr)%struk(k)%numspat
              write(E_specstr1(1:30),'(2g15.8)') allcolout(istr)%E_mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1:2+ksp*2)
     !         write(E_specstr(len_trim(E_specstr)+6:500),'(a)') trim(E_specstr1)
              E_specstr=trim(trim(E_specstr)//'      '//trim(adjustl(E_specstr1)))
              E_CLU_MAS(k,i) = E_CLU_MAS(k,i)+( atwei(nano_iav(i)%struk(k)%specZ(ksp))*&
                                    nano_iav(i)%struk(k)%xnat(kasp)*&
                                    allcolout(i)%E_mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1) )**2
            enddo
            E_CLU_MAS(k,i) = sqrt(E_CLU_MAS(k,i))
            write(iu1e,'(i6,8(3x,g15.8),a)') k, E_DISTRU(k,i,1), E_DISTRM(k,i),zero,zero,E_CLU_MAS(k,i), &
                                             zero,zero, E_DISTRU(k,i,2),TRIM(E_specstr)
          endif
       enddo
        close(iu1)
        if (kpolfin == 1) close(iu1e)
        
    ELSE IF (DB_INDEX_W(istr) == 4 .or. DB_INDEX_W(istr) == 5)  THEN
                                   
       write(iu1,'("# Bivariate distribution")')
       if (kpolfin == 1) write(iu1e,'("# Bivariate distribution - Standard deviations")')
       !write(iu1,'("# n_ab   n_c   Number_Frac.    Mass_Frac.    Diam(e.s.)[nm]   Volume[nm^3]    Mass[amu]  ", &
       !        & "  Base_Diam.(e.c.)[nm]    Height[nm]     Lattice_Expan.[]     Biso[Angstroem^2] ")')

        specstr=''  
        do ksp=1,nano_iav(istr)%struk(1)%numspat
           write(spat(1:2),'(i2.2)') ksp
           specstrl="   OCC_ATOM_"//spat//"    Biso_ATOM_"//spat//"[A^2]"
           write(specstr(len_trim(specstr)+1:500),'(a)') trim(specstrl)
        enddo
        wrtstr='# n_ab   n_c     Number_Frac.       Mass_Frac.       Diam[nm]       Volume[nm^3]       Mass[amu]&
               &   Base_Diam.(e.c.)[nm]   Height[nm]       Lattice_Expan.[]'//TRIM(specstr)
        write(iu1,'(a)') TRIM(wrtstr)
        if (kpolfin == 1) write(iu1e,'(a)') TRIM(wrtstr)
    
       projND_ab=zero; projMD_ab=zero; projND_c=zero; projMD_c=zero
    
       DISTRU_ROD(:,:,istr) = zero
       i=istr
       do k=ncut_0_off(1,i),ncut_0_off(2,i)
    
          klin = nano_iav(i)%post_office_I(:,k)
          n = klin(1)
          n_2nd = klin(2)
          DISTRU_ROD(n,n_2nd,i) = DISTRU(k,i,1)

          specstr=""
          specstr1=''
          do ksp=1,nano_iav(istr)%struk(k)%numspat
            write(specstr1(1:30),'(2g15.8)') allcolout(istr)%mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1:2+ksp*2)
   !          write(specstr(len_trim(specstr)+6:500),'(a)') trim(specstr1)
            specstr=trim(trim(specstr)//'      '//trim(adjustl(specstr1)))
          enddo 
          write(iu1,'(2i6,8(3x,g15.8),a)') n, n_2nd, DISTRU(k,i,1), DISTRM(k,i),CLU_DIAM(k,i),CLU_VOL(k,i),CLU_MAS(k,i),&
                                      CLU_SIZES(:,k,i), DISTRU(k,i,2:2),TRIM(specstr)
          if (kpolfin == 1) then
            E_specstr=""
            E_specstr1=''
            E_CLU_MAS(k,i) = zero
            do ksp=1,nano_iav(istr)%struk(k)%numspat
              write(E_specstr1(1:30),'(2g15.8)') allcolout(istr)%E_mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1:2+ksp*2)
     !         write(E_specstr(len_trim(E_specstr)+6:500),'(a)') trim(E_specstr1)
              E_specstr=trim(trim(E_specstr)//'      '//trim(adjustl(E_specstr1)))
              E_CLU_MAS(k,i) = E_CLU_MAS(k,i)+( atwei(nano_iav(i)%struk(k)%specZ(ksp))*&
                                    nano_iav(i)%struk(k)%xnat(kasp)*&
                                    allcolout(i)%E_mucol(k-ncut_0_off(1,i)+1, 2+(ksp-1)*2+1) )**2
            enddo
            E_CLU_MAS(k,i) = sqrt(E_CLU_MAS(k,i))
            write(iu1e,'(2i6,8(3x,g15.8),a)') n, n_2nd, E_DISTRU(k,i,1), E_DISTRM(k,i),zero,zero,E_CLU_MAS(k,i),&
                                              zero,zero, E_DISTRU(k,i,2),TRIM(E_specstr)
          endif

          projND_ab(n)=projND_ab(n)+DISTRU(k,i,1)
          projND_c(n_2nd)=projND_c(n_2nd)+DISTRU(k,i,1)
          projMD_ab(n)=projMD_ab(n)+DISTRM(k,i)
          projMD_c(n_2nd)=projMD_c(n_2nd)+DISTRM(k,i)
       enddo
   
       close(iu1)
       if (kpolfin == 1) close(iu1e)

       projND_ab=projND_ab/sum(projND_ab)
       projND_c=projND_c/sum(projND_c)
       projMD_ab=projMD_ab/sum(projMD_ab)
       projMD_c=projMD_c/sum(projMD_c)
    
       avestd=AVESTD_DISTR(w=projND_ab(1:nano_iav(i)%dimstruk1), &
                        v=CLU_SIZES(1,nano_iav(istr)%post_office(1:nano_iav(i)%dimstruk1,1),i), isnormalized=1)
       rod_ave_ab(1,istr) = avestd(1)
       rod_sig_ab(1,istr) = avestd(2)
       avestd=AVESTD_DISTR(w=projMD_ab(1:nano_iav(i)%dimstruk1), &
                        v=CLU_SIZES(1,nano_iav(istr)%post_office(1:nano_iav(i)%dimstruk1,1),i), isnormalized=1)
       rod_ave_ab(2,istr) = avestd(1)
       rod_sig_ab(2,istr) = avestd(2)
    
       avestd=AVESTD_DISTR(w=projND_c(1:nano_iav(i)%dimstruk2), &
                        v=CLU_SIZES(2,nano_iav(istr)%post_office(1,1:nano_iav(i)%dimstruk2),i), isnormalized=1)
       rod_ave_c(1,istr) = avestd(1)
       rod_sig_c(1,istr) = avestd(2)
       avestd=AVESTD_DISTR(w=projMD_c(1:nano_iav(i)%dimstruk2), &
                        v=CLU_SIZES(2,nano_iav(istr)%post_office(1,1:nano_iav(i)%dimstruk2),i), isnormalized=1)
       rod_ave_c(2,istr) = avestd(1)
       rod_sig_c(2,istr) = avestd(2)
    
       avestd_2D(:,1)=AVESTD_DISTR_2D(w=DISTRU(ncut_0_off(1,istr):ncut_0_off(2,istr),istr,1),&
                                   v=CLU_SIZES(:,ncut_0_off(1,istr):ncut_0_off(2,istr),istr),isnormalized=0)
       avestd_2D(:,2)=AVESTD_DISTR_2D(w=DISTRM(ncut_0_off(1,istr):ncut_0_off(2,istr),istr),&
                                   v=CLU_SIZES(:,ncut_0_off(1,istr):ncut_0_off(2,istr),istr),isnormalized=0)
    
       write(iu,*)
       write(iu,*) '#*********   PROJECTED SIZE DISTRIBUTIONS   *********'
       write(iu,*)
       write(iu,'(a,2x,g12.6,a,2x,g12.6)') &
              '#  Number Distribution:   < L > along ab : ',rod_ave_ab(1,istr), &
              ' +/- ',rod_sig_ab(1,istr)
       write(iu,'(a,2x,g12.6,a,2x,g12.6)') &
              '#  Mass   Distribution:   < L > along ab : ',rod_ave_ab(2,istr),' +/- ',rod_sig_ab(2,istr) 
       write(iu,*)
       xxx0=one/maxval(projND_ab)
       xxx2=0.005d0
       write(iu,*) '# D (eq. circle)        N Distr          M Distr'
       do n=1,nano_iav(istr)%dimstruk1
          n_2nd=1
          k=nano_iav(istr)%post_office(n,n_2nd)
          xxx1=projND_ab(n)*xxx0
          if (xxx1<xxx2) cycle
          write(iu,'(i4,3(2x,g14.8))')n,CLU_SIZES(1,k,i),projND_ab(n),projMD_ab(n)
       enddo
       write(iu,*)
       write(iu,'(a,2x,g12.6,a,2x,g12.6)') &
             '#  Number Distribution:   < L > along c : ',rod_ave_c(1,istr),' +/- ',rod_sig_c(1,istr)
       write(iu,'(a,2x,g12.6,a,2x,g12.6)') &
              '#  Mass   Distribution:   < L > along c : ',rod_ave_c(2,istr),' +/- ',rod_sig_c(2,istr)
       write(iu,*)
       xxx0=one/maxval(projND_c)
       xxx2=0.005d0
       write(iu,*) '#         L             N-Distr         M-Distr'
       do n_2nd=1,nano_iav(istr)%dimstruk2
          n=1
          k=nano_iav(istr)%post_office(n,n_2nd)
          xxx1=projND_c(n_2nd)*xxx0
          if (xxx1<xxx2) cycle
          write(iu,'(i4,3(2x,g14.8))')n_2nd,CLU_SIZES(2,k,i),projND_c(n_2nd),projMD_c(n_2nd)
       enddo
    
       write(iu,*)
       write(iu,'(a)') '#*********   BIDIMENSIONAL SIZE DISTRIBUTIONS   *********'
       write(iu,'(a)') '#   Coordinates: '
       write(iu,'(a)') '#   x == ab-plane growth direction, step = diameter of circle of equal area  '
       write(iu,'(a)') '#   y == c-axis growth direction, step = c  '
       write(iu,*)
       write(iu,'(a)') '#      AVERAGE POINT: '
       write(iu,*)
       write(iu,'(a)') ' Definitions: '
       write(iu,*)
       write(iu,'(a)') ' Ave   == [ Average_x, Average_y ] '
       write(iu,*)
       write(iu,'(a)') '#      COVARIANCE MATRIX: '
       write(iu,*)
       write(iu,'(a)') '            |   Variance_{x,x}   Covariance_{x,y}  |'
       write(iu,'(a)') '   Cov  ==  |                                      |'
       write(iu,'(a)') '            |  Covariance_{y,x}   Variance_{y,y}   |'
       write(iu,*)
       write(iu,*)
       write(iu,'(a)') '#      CORRELATION COEFFICIENT, ANGLE: '
       write(iu,*)
       write(iu,'(a)') '      c_corr   ==  Covariance_{x,y} / Sqrt[Variance_{x,x}*Variance_{y,y}];'
       write(iu,'(a)') '      phi_corr ==  ArcCos[c_corr]'
       write(iu,*)
 
      do inm=1,2
        if (inm==1) then
          write(iu,'(a)') '# 1) _____________________________  NUMBER DISTRIBUTION  ___________________________'
        else
          write(iu,'(a)') '# 2) _____________________________   MASS DISTRIBUTION  ____________________________'
        endif
        write(iu,*)
        write(iu,*)
        write(iu,'(a,g16.10,a,g16.10,a)') ' Ave   = [ ',avestd_2D(1,inm),', ',avestd_2D(2,inm),' ] '
        write(iu,*)
        write(iu,'(a,g16.10,a,g16.10,a)') '            |  ',avestd_2D(3,inm),'  ',avestd_2D(5,inm),'  |'
        write(iu,'(a)')                          '   Cov  =   |                                      |'
        write(iu,'(a,g16.10,a,g16.10,a)') '            |  ',avestd_2D(5,inm),'  ',avestd_2D(4,inm),'  |'
        write(iu,*)
        corrcoe=avestd_2D(5,inm)/sqrt(avestd_2D(3,inm)*avestd_2D(4,inm))
        corrphi=radians_to_degrees * acos(max(-one,min(one,corrcoe)))
        write(iu,'(1x,a,g16.10,a,g16.10,a)')'      c_corr   = ',corrcoe,' ; phi_corr  = ',corrphi,' deg '
        write(iu,*)
        write(iu,'(a)') ' Eigenvariances / Eigenvectors ( Cov ) : '
        xxx0=sqrt(max(zero,avestd_2D(3,inm)**2+avestd_2D(4,inm)**2-two*avestd_2D(3,inm)*avestd_2D(4,inm)* &
                    (one-two*(corrcoe**2))))
        eigval=half*[ avestd_2D(3,inm) + avestd_2D(4,inm) - xxx0, avestd_2D(3,inm) + avestd_2D(4,inm) + xxx0]
        eigang = ATAN2( two*avestd_2D(5,inm), avestd_2D(3,inm) - avestd_2D(4,inm) - xxx0)
      
        eigvec(:,1)=[ cos(eigang),sin(eigang)]
        eigvec(:,2)=[-sin(eigang),cos(eigang)]
        write(iu,*)
        write(iu,'(a,3(g12.6,a))') ' V_1, e_1 :  ',eigval(1),'  [ ',eigvec(1,1),', ',eigvec(2,1),' ]'
        write(iu,'(a,3(g12.6,a))') ' V_2, e_2 :  ',eigval(2),'  [ ',eigvec(1,2),', ',eigvec(2,2),' ]'  
        write(iu,*)
        write(iu,*)
        write(iu,*)
       filegnuL(1:12) = astr(1:2)//'_Dis2D.vec'
        iugr=find_unit()
        open(iugr,status='replace',file=path_output_files(1:lpath_output_files)// &
                                     trim(structure_name_w(istr))//filegnuL(1:12))
        write(iugr,'(a)') '# Number Distribution vectors  Mass Distribution vectors' 
        write(iugr,'(4(2x,g12.6))')avestd_2D(1:2,1:2)
        write(iugr,'(4(2x,g12.6))')avestd_2D(1:2,1)+sqrt(abs(eigval(1)))*eigvec(:,1), &
                                   avestd_2D(1:2,2)+sqrt(abs(eigval(1)))*eigvec(:,1)
        write(iugr,'(4(2x,g12.6))')avestd_2D(1:2,1:2)
        write(iugr,'(4(2x,g12.6))')avestd_2D(1:2,1)+sqrt(abs(eigval(2)))*eigvec(:,2), &
                                   avestd_2D(1:2,2)+sqrt(abs(eigval(2)))*eigvec(:,2)
        close(iugr)

      enddo
    ENDIF  
        !!___writing DB05 1D distr. of the total diameter !!RF 30.09.2014
    write(astr,'(i2.2)') istr
    IF (DB_INDEX_W(istr) == 5) THEN
      filegnuL(1:13) = astr(1:2)//'_plot1D.mtx'
      iu1 = FIND_UNIT()
      lns1=lcut_structure_name(istr)
      lns=lstructure_name(istr)-4
      open(iu1, status='replace', file=path_output_files(1:lpath_output_files)// &
                                  trim(structure_name_w(istr))//filegnuL(1:13),action='WRITE')
    
      write(iu1,'("# Univariate distribution")')
      specstr=''  
      do ksp=1,nano_iav(istr)%struk(1)%numspat
        write(spat(1:2),'(i2.2)') ksp
        specstrl="   OCC_ATOM_"//spat//"    Biso_ATOM_"//spat//"[A^2]"
        write(specstr(len_trim(specstr)+1:500),'(a)') trim(specstrl)
      enddo
      wrtstr='# n_shell_tot Number_Frac.       Mass_Frac.       Diam[nm]'
      write(iu1,'(a)') TRIM(wrtstr)

      i=istr
      minab = minval(nano_iav(i)%post_office_I(1,:))
      minc = minval(nano_iav(i)%post_office_I(2,:))
      maxab = maxval(nano_iav(i)%post_office_I(1,:))
      maxc = maxval(nano_iav(i)%post_office_I(2,:))
      mins = minab+minc
      maxs = maxab+maxc
      maxk = maxab*maxc
      Allocate(kk1(ncut_0_off(2,i)),nsto(ncut_0_off(2,i)),ndisto(ncut_0_off(2,i)),mdisto(ncut_0_off(2,i)),&
               diamclo(ncut_0_off(2,i)),dis1d(3,maxab+maxc-1),nso(maxab+maxc-1))
      ck=0
      do sk = mins,maxs
        do skk = 1,min(maxc,sk)
          if (sk-skk>0 .and. sk-skk<=maxab) then
            if (sk-skk== maxab) then
              isk=skk
              iskk=sk-skk-maxab
            else
              isk=skk
              iskk=sk-skk
            endif
            ck=ck+1
            kk1(ck) = isk*maxab-iskk
            nsto(ck) = nano_iav(i)%post_office_I(1,kk1(ck))+nano_iav(i)%post_office_I(2,kk1(ck))
            ndisto(ck) = DISTRU(kk1(ck),i,1)
            mdisto(ck) = DISTRM(kk1(ck),i)
            diamclo(ck) = CLU_DIAM(kk1(ck),i)
          endif
        enddo
      enddo

      ck=0
      do k=ncut_0_off(1,i),ncut_0_off(2,i)
        if (k==1) then
          ck=ck+1
          nso(ck) = nsto(k)
          dis1d(1,ck) = ndisto(k)
          dis1d(2,ck) = mdisto(k)
          dis1d(3,ck) = diamclo(k)
        else
          if (nsto(k)-nsto(k-1)>0) then
            ck=ck+1
            nso(ck) = nsto(k)
            dis1d(1,ck) = ndisto(k)
            dis1d(2,ck) = mdisto(k)
            dis1d(3,ck) = diamclo(k)
          else if (nsto(k)-nsto(k-1)==0) then
            !   dis1d(1,ck-1) = dis1d(1,ck-1)+ndisto(k)
            !   dis1d(2,ck-1) = dis1d(2,ck-1)+mdisto(k)
              dis1d(1,ck) = dis1d(1,ck)+ndisto(k)
              dis1d(2,ck) = dis1d(2,ck)+mdisto(k)
          endif
        endif                        
      enddo
      do k=1,ck
        write(iu1,'(i6,3(3x,g15.8))') nso(k), dis1d(:,k)
      enddo
      close(iu1)
      deallocate(kk1,nso,nsto,ndisto,mdisto,dis1d,diamclo)
    ENDIF
    !!___end
     
  enddo
   xxx0=100.d0/sum(massfrac_struc(1:NSTR_W))
  write(iu,'(a)')' '
  write(iu,'(a)')' #******************** GLOBAL MASS FRACTIONS OF DIFFERENT STRUCTURES ********************'
  write(iu,'(a)')' '
  write(iu,'(a)')' Str.#    Mass frac. % '
  do istr=1,NSTR_W
    write(iu,*)
    write(iu,'(i4,3x,f13.4)') istr, xxx0*massfrac_struc(istr)
  enddo
  
 
 END SUBROUTINE write_varia_fileX
!********** ********** ********** ********** ********** ********** ********** ********** 
SUBROUTINE write_dumm_fileX(kset,iu,u0,u1)

  implicit none
  INTEGER(I4B),INTENT(IN)         :: kset, iu
  real(DP),INTENT(IN)             :: u0,u1
  INTEGER(I4B)                :: i, jj,j,k,nn
  real(DP),allocatable        :: Pma1(:,:),yve1(:),ave1(:),cvek(:),nvek(:),cleofe(:,:)
  real(DP)                    :: ukon,dx,x,x0,domenico,salvatore,calogero,smaragdo

  salvatore=sqrt(-2.0_DP*log(eps_DP))

  if (allocated(Pma1)) deallocate(Pma1)
  if (allocated(yve1)) deallocate(yve1)
  if (allocated(ave1)) deallocate(ave1)
  if (allocated(cvek)) deallocate(cvek)
  if (allocated(nvek)) deallocate(nvek)
  allocate(Pma1(NAMO_W,NAMO_W),yve1(NAMO_W),ave1(NAMO_W),cvek(NAMO_W),nvek(NAMO_W))
  i=kset

  Pma1=AMORPHOUS(i)%precondy
  yve1=AMORPHOUS(i)%yadd
  ave1=AMORPHOUS(i)%addbase
  cvek=Scales(i)%AMOscal

  nvek = matmul(Pma1,cvek*ave1)
  ukon = sum(cvek*ave1*yve1)
  write(iu,*)'# AMORPHOUS Coefficients: j, (j+j0)*Delta_Amor, NEW_COE[j], OLD_COE[j], ADD_C '
  write(iu,*)'# The I_amo in output is calc. as I_amo[k] = sum_j(Anew[k,j]*NEW_COE[j]) '
  write(iu,*)'# May be calc. also as            I_amo[k] = sum_j(Aold[k,j]*OLD_COE[j]) + ADD_C '
  DO j=1,NAMO_W
    jj=j+AMORPHOUS(i)%j000A
    write(iu,'("#",i5,4(1x,g24.16))')j,jj*Delta_Amor,cvek(j),nvek(j),ukon
  ENDDO
  write(iu,*)'# Direct space ddf P[u]: u, P[u], Pbroad[u] :'

  nn=200
  if (allocated(cleofe)) deallocate(cleofe)
  allocate(cleofe(nn,2))
  cleofe=zero
  dx=(1.1_DP*AMO_DCORR_W)/nn
  do j=1,NAMO_W
    jj=j+AMORPHOUS(i)%j000A
    x0=jj*Delta_Amor
    domenico=u0+real(jj,DP)*u1
    smaragdo=domenico+Delta_Amor*2.7_DP
    calogero=LOG(MAX(eps_DP,nvek(j)))
    do k=1,nn
      x=abs(dx*k-x0)/domenico
      if (x>salvatore) cycle
      cleofe(k,1)=cleofe(k,1)+exp(calogero-0.5_DP*x*x)
    ENDDO
    do k=1,nn
      x=abs(dx*k-x0)/smaragdo
      if (x>salvatore) cycle
      cleofe(k,2)=cleofe(k,2)+exp(calogero-0.5_DP*x*x)
    ENDDO
  enddo
  do k=1,nn
    write(iu,'(3(1x,g14.7))')k*dx,cleofe(k,:)
  enddo

  if (allocated(cleofe)) deallocate(cleofe)
  if (allocated(Pma1)) deallocate(Pma1)
  if (allocated(yve1)) deallocate(yve1)
  if (allocated(ave1)) deallocate(ave1)
  if (allocated(cvek)) deallocate(cvek)
  if (allocated(nvek)) deallocate(nvek)
 END SUBROUTINE write_dumm_fileX
!********** ********** ********** ********** ********** ********** ********** ********** 
SUBROUTINE write_dumm_fileY(kset,iu,u0,u1)

  implicit none
  INTEGER(I4B),INTENT(IN)         :: kset, iu
  real(DP),INTENT(IN)             :: u0,u1
  INTEGER(I4B)                :: i, jj,j,k,nn
  real(DP),allocatable        :: Pma1(:,:),yve1(:),ave1(:),cvek(:),nvek(:),cleofe(:,:)
  real(DP)                    :: ukon,dx,x,x0,domenico,salvatore,calogero,smaragdo

  nn = NDATA_W(kset)
  write(iu,*)'# ',nn
  do i=1,nn
    write(iu,*)Obs_data_W(kset)%t2data(i), &
               Obs_data_W(kset)%qdata(i,1),Obs_data_W(kset)%vdata(i),Obs_data_W(kset)%wdata(i),  &
               (CALPHA_W(kset,j)%vdata(i),j=1,NSTR_W)
  enddo
  IF (DO_AMORPH_W.and.NAMO_W>0) then
    write(iu,*)'# ',DELTA_AMOR,FLOOR((AMO_DCORR0_W)/Delta_Amor),u0,u1,LAMBDAS_W(1,kset)
    write(iu,*)'# ',(scales(kset)%AMOSCAL(j),j=1,NAMO_W)
    do i=1,nn
      write(iu,*)amorphous(kset)%amo_scat_tot(i)
    enddo
  endif
 END SUBROUTINE write_dumm_fileY
!********** ********** ********** ********** ********** ********** ********** ********** 
end module REFINEMENT
!___________________________________________________________________________________________________
