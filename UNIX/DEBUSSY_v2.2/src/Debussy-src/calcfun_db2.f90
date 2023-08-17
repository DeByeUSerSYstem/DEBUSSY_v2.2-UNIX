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
MODULE POLYSUB
 USE nano_deftyp

 PRIVATE
 PUBLIC  :: cenlar2avsd, avsd2cenlar, cenlar2avsd_old, avsd2cenlar_old

 INTERFACE cenlar2avsd
   module procedure cenlar2avsd1, cenlar2avsd2
 END INTERFACE

 INTERFACE avsd2cenlar
   module procedure avsd2cenlar1, avsd2cenlar2
 END INTERFACE

 INTERFACE cenlar2avsd_old
   module procedure cenlar2avsd1_old, cenlar2avsd2_old
 END INTERFACE

 INTERFACE avsd2cenlar_old
   module procedure avsd2cenlar1_old, avsd2cenlar2_old
 END INTERFACE

CONTAINS
 SUBROUTINE cenlar2avsd1(xn0,xw,av,sd)
   REAL(SP),intent(IN)  :: xn0,xw
   REAL(SP),intent(OUT) :: av,sd
   REAL(DP)             :: w_sq

   w_sq = xw*xw
   av   = xn0 * exp(half * w_sq)
   sd   = av * sqrt(exp(w_sq)-one)

 END SUBROUTINE cenlar2avsd1
 
 SUBROUTINE avsd2cenlar1(xn0,xw,av,sd)
   REAL(SP),intent(IN)  :: av,sd
   REAL(SP),intent(OUT) :: xn0,xw
   REAL(DP)             :: qsar,w_sq

   
   qsar = one + sd*sd/(av*av)
   w_sq = LOG(qsar)
   xw   = sqrt(w_sq)
   xn0  = av*exp(-half*w_sq)

 END SUBROUTINE avsd2cenlar1

 SUBROUTINE cenlar2avsd2(xn0,xw,av,sd)
   REAL(DP),intent(IN)  :: xn0,xw
   REAL(DP),intent(OUT) :: av,sd
   REAL(DP)             :: w_sq

   w_sq = xw*xw
   av   = xn0 * exp(half * w_sq)
   sd   = av * sqrt(exp(w_sq)-one)

 END SUBROUTINE cenlar2avsd2

 SUBROUTINE avsd2cenlar2(xn0,xw,av,sd)
   REAL(DP),intent(IN)  :: av,sd
   REAL(DP),intent(OUT) :: xn0,xw
   REAL(DP)             :: qsar,w_sq

   qsar = one + sd*sd/(av*av)
   w_sq = LOG(qsar)
   xw   = sqrt(w_sq)
   xn0  = av*exp(-half*w_sq)

 END SUBROUTINE avsd2cenlar2
 

 SUBROUTINE cenlar2avsd1_old(xn0,xw,av,sd)
   REAL(SP),intent(IN)  :: xn0,xw
   REAL(SP),intent(OUT) :: av,sd
   REAL(DP)             :: w_sq

   w_sq = xw*xw
   av   = xn0 * exp(1.5_DP * w_sq)
   sd   = av * sqrt(exp(w_sq)-one)

 END SUBROUTINE cenlar2avsd1_old

 SUBROUTINE avsd2cenlar1_old(xn0,xw,av,sd)
   REAL(SP),intent(IN)  :: av,sd
   REAL(SP),intent(OUT) :: xn0,xw
   REAL(DP)             :: qsar,w_sq

   
   qsar = one + sd*sd/(av*av)
   w_sq = LOG(qsar)
   xw   = sqrt(w_sq)
   xn0  = av*exp(-1.5_DP*w_sq)

 END SUBROUTINE avsd2cenlar1_old

 SUBROUTINE cenlar2avsd2_old(xn0,xw,av,sd)
   REAL(DP),intent(IN)  :: xn0,xw
   REAL(DP),intent(OUT) :: av,sd
   REAL(DP)             :: w_sq

   w_sq = xw*xw
   av   = xn0 * exp(1.5_DP * w_sq)
   sd   = av * sqrt(exp(w_sq)-one)

 END SUBROUTINE cenlar2avsd2_old

 SUBROUTINE avsd2cenlar2_old(xn0,xw,av,sd)
   REAL(DP),intent(IN)  :: av,sd
   REAL(DP),intent(OUT) :: xn0,xw
   REAL(DP)             :: qsar,w_sq

   qsar = one + sd*sd/(av*av)
   w_sq = LOG(qsar)
   xw   = sqrt(w_sq)
   xn0  = av*exp(-1.5_DP*w_sq)

 END SUBROUTINE avsd2cenlar2_old

END MODULE POLYSUB
!___________________________________________________________________________________________________
MODULE SERVICE
 USE nano_deftyp
 use LINALG_TOOLS
 use CALC_WSPACE,only: xlogan, diam_logar
 USE POLYSUB
 
 integer(I4B), save :: MIN_Ncut0=10000

 CONTAINS

!______________________________________________________________________________________________________________________
 SUBROUTINE COLON_AS_CL(avLN,sdLN,ln_n00,ln_wid,n1,n2,n1in)
   IMPLICIT NONE
   REAL(CP),intent(IN)      :: avLN,sdLN
   REAL(CP),intent(OUT)     :: ln_n00,ln_wid
   INTEGER(I4B),intent(OUT),optional :: n1,n2
   INTEGER(I4B),intent(IN),optional :: n1in

   Call avsd2cenlar(ln_n00,ln_wid,avLN,sdLN)

   if (present(n1).and.present(n2)) then
     if (.not.PRESENT(n1in)) then
       n1 = MAX(1,FLOOR(ln_n00))
     else if (PRESENT(n1in)) then
       if (n1in < 0) then
         n1 = MAX(1,FLOOR(ln_n00))
       else
         n1=n1in
       endif
     endif
     n2=n1+1
   endif

 END SUBROUTINE COLON_AS_CL
!__________________________________________________________________________________________________________
 SUBROUTINE COSTR_EX2IN(n1,n2,Omega,Xi,xn0,xw,y1,y2,Delta,alp4,numcase,Etol, straflag)
   IMPLICIT NONE
   REAL(CP),intent(IN)      :: Omega,Xi,xw
   REAL(CP),intent(INOUT)   :: xn0
   INTEGER(I4B),intent(IN)  :: n1,n2
   INTEGER(I4B),optional,intent(IN)  :: straflag
   REAL(CP),intent(OUT)     :: y1,y2,Delta,alp4(4),Etol
   INTEGER(I4B),intent(OUT) :: numcase
   REAL(DP)                 :: xnu,xnuxw,EXLIM,dOX,dOXa,dOXs,dOXal,exn1,exn2
   INTEGER(I4B)  :: strain_fun

   
   strain_fun=0
   if (present(straflag)) then
     strain_fun=straflag
   endif
   
   if (strain_fun==1) then
     y1=Omega
     y2=Omega
     Delta=zero
     alp4=zero
     xn0 = REAL(n1+n2,DP)*half
     numcase=0
     Etol= -log(two*eps_DP)
     return
   endif

   if (strain_fun==2) then
     y1 = Xi
     y2 = Omega
     Delta=zero
     alp4=zero
     xn0 = REAL(n1+n2,DP)*half
     numcase=0
     Etol= -log(two*eps_DP)
     return
   endif
   
!______ xw is supposed positive; ex1val = 1/(1+exp((1-xn0)/xw))
   IF (xw < sceps_CP) STOP 'COSTR_EX2IN: too small w'

   EXLIM = -log(eps_CP)
   xnu = one-xn0
   xnuxw = xnu/xw
   dOX  = Omega-Xi
   dOXa = ABS(dOX)
   IF (dOXa > eps_CP) THEN
     dOXal= LOG(dOXa)
     dOXs = SIGN(one,dOX)

     IF (xnuxw>EXLIM) THEN
       Delta  = dOXs * EXP(dOXal + xnuxw)
     ELSE IF (xnuxw<-EXLIM) THEN
       Delta  = dOX
     ELSE
       Delta = dOX * (one+EXP(xnuxw))
     ENDIF

     xnu = REAL(n1,DP)-xn0
     xnuxw = xnu/xw
     IF (xnuxw>EXLIM) THEN
       exn1 = zero
     ELSE IF (xnuxw<-EXLIM) THEN
       exn1 = one
     ELSE
       exn1 = one / (one+EXP(xnuxw))
     ENDIF
     xnu = REAL(n2,DP)-xn0
     xnuxw = xnu/xw
     IF (xnuxw>EXLIM) THEN
       exn2 = zero
     ELSE IF (xnuxw<-EXLIM) THEN
       exn2 = one
     ELSE
       exn2 = one / (one+EXP(xnuxw))
     ENDIF
     y1 = Omega - Delta * exn1
     y2 = Omega - Delta * exn2

     alp4(1:2) = (/ Omega - y1, Omega - y2/)
     alp4(3:4) = Delta - alp4(1:2)
     alp4 = ABS(alp4)

     Call CHECKCASE(y1,y2,Omega,Delta,numcase,Etol)
   ELSE
     y1 = Omega
     y2 = Omega
     Delta = zero
     alp4  = zero
     xn0 = REAL(n1+n2,DP)*half
     Call CHECKCASE(y1,y2,Omega,Delta,numcase,Etol)
   ENDIF

 END SUBROUTINE COSTR_EX2IN
!______________________________________________________________________________________________________________________
 SUBROUTINE COSTR_IN2EX(n1,n2,Omega,Xi,xn0,xw,y1,y2,Delta,numcase,Etol, straflag)
   IMPLICIT NONE
   REAL(CP),intent(OUT)     :: Xi,xn0,xw,Etol
   INTEGER(I4B),intent(IN)  :: n1,n2
   INTEGER(I4B),optional,intent(IN)  :: straflag
   REAL(CP),intent(IN)      :: Omega,y1,y2,Delta
   INTEGER(I4B),intent(OUT) :: numcase
   REAL(DP)                 :: sulo,xnu,xnuxw,EXLIM,exn1,exn2,alp4(4)
   REAL(DP)                 :: heps,za,ta,cex, taza,taza2,taza3,tam1
   REAL(DP)                 :: ciap,prend, xdn1
   INTEGER(I4B)  :: strain_fun

   
   strain_fun=0
   if (present(straflag)) then
     strain_fun=straflag
   endif

   if (strain_fun==1) then
     Xi=Omega
     alp4=zero
     xn0 = REAL(n1+n2,DP)*half
     xw  = one
     numcase=0
     Etol= -log(two*eps_DP)
     return
   endif

   if (strain_fun==2) then
     Xi    = y1
     alp4=zero
     xn0 = REAL(n1+n2,DP)*half
     xw  = one
     numcase=0
     Etol= -log(two*eps_DP)
     return
   endif
   
   
   alp4 = abs((/Omega-y1,Omega-y2,Delta-Omega+y1,Delta-Omega+y2/))

   Call CHECKCASE(y1,y2,Omega,Delta,numcase,Etol)

   SELECT CASE(numcase)
   CASE(0)
     xw  = half
     xn0 = REAL(n1,DP)+half
     Xi  = Omega
   CASE(1)
     xw  = half/Etol
     xn0 = REAL(n2,DP)+half
     Xi  = Omega-Delta
   CASE(2)
     xw  = half/Etol
     xn0 = REAL(n1,DP)-half
     IF (n1==1) THEN
       Xi  = y1
     ELSE
       Xi  = Omega-Delta
     ENDIF
   CASE(3)
     sulo= log(alp4(4)/alp4(2))
     xw  = one/ABS(sulo+Etol)
     xn0 = REAL(n1,DP)+Etol*xw
     Xi  = Omega-Delta
   CASE(4)
     sulo= log(alp4(3)/alp4(1))
     xw  = one/ABS(sulo-Etol)
     xn0 = REAL(n2,DP)-Etol*xw
     IF (n1==1) THEN
       Xi  = y1
     ELSE
       Xi  = Omega-Delta
     ENDIF
   CASE(5)
     IF (n1==1) THEN
       Xi = y1
     ELSE
       heps = exp(-Etol)
       za = ABS(Omega-y1)
       ta = ABS(Delta-Omega+y1)
       taza = ta/za
       taza2 = taza*taza
       taza3 = taza2*taza
       tam1 = one/ta
       prend = -one+taza3
       ciap = one
       xdn1 = REAL(1-n1,DP)
       cex = ABS(Delta)*xdn1/(za*ta)
       Xi = Omega-Delta*za/(za+ta*EXP(cex*heps))
     ENDIF
     cex = MAX(heps, tam1 * (one+taza) * heps)
     xw  = one/cex
     xn0 = REAL(n1,DP) - log(taza) * xw
   CASE(6)
     sulo = log(alp4(1))+log(alp4(4))-log(alp4(2))-log(alp4(3))
     xw = one/sulo
     xn0 = (REAL(n2,DP)*(log(alp4(1)) - log(alp4(3))) + REAL(n1,DP)*(log(alp4(4)) - log(alp4(2))))*xw
     
     xnu = one-xn0
     xnuxw = xnu/xw
     IF (xnuxw>Etol) THEN
       exn1 = zero
     ELSE IF (xnuxw<-Etol) THEN
       exn1 = one
     ELSE
       exn1 = one / (one+EXP(xnuxw))
     ENDIF
     Xi = Omega-Delta*exn1
   CASE DEFAULT
     STOP 'COSTR_IN2EX : CASE DEFAULT'
   END SELECT

 END SUBROUTINE COSTR_IN2EX
!______________________________________________________________________________________________________________________
 SUBROUTINE CHECKCASE(y1,y2,Omega,Delta,numcase,Etol)
   IMPLICIT NONE
   REAL(CP),intent(IN)      :: y1,y2,Omega,Delta
   INTEGER(I4B),intent(OUT) :: numcase
   REAL(CP),intent(OUT)     :: Etol
   REAL(DP)                 :: tol,aaa,aaaD,alp4(4),aw4(4),aw4D(4),abD

   tol = 2.0_DP*eps_CP
   Etol = -log(tol)

   alp4 = (/Omega - y1, Omega - y2, Delta - Omega + y1, Delta - Omega + y2/)
   aw4 = ABS(alp4)
   abD = abs(Delta)
   aw4D= abs(aw4-abD)

   numcase = -1

   aaa = MAXVAL(aw4)
   IF (MIN(abD,aaa) <= tol) THEN
     numcase = 0
     RETURN
   ENDIF

   aaa = MINVAL(aw4)
   IF (aaa > tol) THEN
     numcase = 6
     RETURN
   ENDIF

   IF ((ALL(aw4(3:4) <= tol)) .and. (ALL(aw4D(1:2) <= tol))) THEN
     numcase = 1
   ELSE IF ((ALL(aw4(1:2) <= tol)) .and. (ALL(aw4D(3:4) <= tol))) THEN
     numcase = 2
   ELSE
     IF (aw4D(1) <= tol .and. aw4(3) <= tol) THEN
       numcase = 3
     ELSE IF (aw4D(4) <= tol .and. aw4(2) <= tol) THEN
       numcase = 4
     ENDIF
   ENDIF

   IF (numcase >= 0) RETURN
   IF (aaa > tol .and. abs(y1-y2) <= tol) numcase = 5
   IF (numcase < 0) STOP 'CHECKCASE: incongruent sitation!'

 END SUBROUTINE CHECKCASE
!______________________________________________________________________________________________________________________
 SUBROUTINE MAKE_DAmat(n1,n2,Omega,Xi,xn0,xw,y1,y2,Delta,A_mat,DA_mat,D2A_mat,Ncutoff,Ncut0,Etol,numcase)
  IMPLICIT NONE
  REAL(CP),intent(IN)                   :: Omega,Xi,xn0,xw,y1,y2,Delta,Etol
  REAL(CP),dimension(:),intent(OUT)     :: A_mat
  REAL(CP),dimension(:,:),intent(OUT)   :: dA_mat
  REAL(CP),dimension(:,:),intent(OUT)   :: d2A_mat
  INTEGER(I4B),intent(IN)               :: n1,n2,Ncutoff,Ncut0,numcase
!****LOCAL
  REAL(DP)                          :: heps,za,ta,cex, hepsta,hepsta2, taza,taza2,taza3,tam1
  REAL(DP)                          :: valAMAT,NORMAL_DAMAT(4),NORMAL_D2AMAT(10),ciap,prend, xdn1
  INTEGER(I4B)                      :: n

  A_mat = zero
  DA_mat = zero
  D2A_mat = zero
  DO n=Ncut0,Ncutoff
!____ Easy cases
!
    IF (n==n1) THEN
      A_mat(n) = y1
      DA_mat(:,n) = (/zero,one,zero,zero/)
      D2A_mat(:,n)= zero
      CYCLE
    ELSE IF (n==n2) THEN
      A_mat(n) = y2
      DA_mat(:,n) = (/zero,zero,one,zero/)
      D2A_mat(:,n)= zero
      CYCLE
    ENDIF

    IF (numcase==5) THEN
      heps = EXP(-Etol)
      za = ABS(Omega-y1)
      ta = ABS(Delta-Omega+y1)
      taza = ta/za
      taza2 = taza*taza
      taza3 = taza2*taza
      tam1 = one/ta
      prend = -one+taza3
      ciap = one
    ENDIF

!____ Normal cases
!
    SELECT CASE(numcase)
    CASE(0)
      A_mat(n) = Omega
      DA_mat(:,n) = (/one,zero,zero,zero/)
      D2A_mat(:,n)= zero
    CASE(1)
      IF (n<n1) THEN
        A_mat(n) = Omega-Delta
        DA_mat(:,n) = (/one,-one,zero,zero/)
        D2A_mat(:,n)= zero
      ELSE
        Call MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,NORMAL_DAMAT,NORMAL_D2AMAT,valAMAT)
        A_mat(n) = valAMAT
        DA_mat(:,n) = NORMAL_DAMAT
        D2A_mat(:,n) = NORMAL_D2AMAT
      ENDIF
    CASE(2)
      IF (n>n2) THEN
        A_mat(n) = Omega
        DA_mat(:,n) = (/one,zero,zero,zero/)
        D2A_mat(:,n)= zero
      ELSE
        Call MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,NORMAL_DAMAT,NORMAL_D2AMAT,valAMAT)
        A_mat(n) = valAMAT
        DA_mat(:,n) = NORMAL_DAMAT
        D2A_mat(:,n) = NORMAL_D2AMAT
      ENDIF
    CASE(3)
      IF (n<n1) THEN
        A_mat(n) = Omega-Delta
        DA_mat(:,n) = (/one,-one,zero,zero/)
        D2A_mat(:,n)= zero
      ELSE
        Call MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,NORMAL_DAMAT,NORMAL_D2AMAT,valAMAT)
        A_mat(n) = valAMAT
        DA_mat(:,n) = NORMAL_DAMAT
        D2A_mat(:,n) = NORMAL_D2AMAT
      ENDIF
    CASE(4)
      IF (n>n2) THEN
        A_mat(n) = Omega
        DA_mat(:,n) = (/one,zero,zero,zero/)
        D2A_mat(:,n)= zero
      ELSE
        Call MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,NORMAL_DAMAT,NORMAL_D2AMAT,valAMAT)
        A_mat(n) = valAMAT
        DA_mat(:,n) = NORMAL_DAMAT
        D2A_mat(:,n) = NORMAL_D2AMAT
      ENDIF
    CASE(5)
      xdn1 = REAL(n-n1,DP)
      cex = ABS(Delta)*xdn1/(za*ta)
      A_mat(n) = Omega-Delta*za/(za+ta*EXP(cex*heps))
      hepsta = heps*xdn1/ta
      hepsta2= hepsta*hepsta
      DA_mat(1:2,n) = (/half * (one+taza2) * hepsta2, half * hepsta2/)
      DA_mat(3:4,n) = (/one-DA_mat(1,n), zero/)
      hepsta2 = hepsta2 * tam1

      D2A_mat(:,n) = hepsta2 * (/ -prend, -ciap, prend,  zero, ciap, ciap, zero, -prend, zero, zero/)
    CASE(6)
      Call MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,NORMAL_DAMAT,NORMAL_D2AMAT,valAMAT)
      A_mat(n) = valAMAT
      DA_mat(:,n) = NORMAL_DAMAT
      D2A_mat(:,n) = NORMAL_D2AMAT
    END SELECT
  ENDDO

 END SUBROUTINE MAKE_DAmat
!______________________________________________________________________________________________________________________
 SUBROUTINE MK_NORMAL_DST(n,n1,n2,Omega,Xi,y1,y2,Delta,xn0,xw,Etol,GRASS,HERB,FUN)
  IMPLICIT NONE
  REAL(CP),intent(IN)               :: Omega,xn0,xw,Delta,Etol,Xi,y1,y2
  REAL(CP),DIMENSION(4),INTENT(OUT) :: GRASS
  REAL(CP),DIMENSION(10),INTENT(OUT):: HERB
  REAL(CP),INTENT(OUT)              :: FUN
  INTEGER(I4B),intent(IN)           :: n,n1,n2

  REAL(DP)                          :: xn,eex,epsloc,Ve(4),Vee(10),Ex
  REAL(DP)                          :: Ha,Ka,Ga,La, alpha1,alpha2,beta1,beta2,gama1,gama2, &
                                       beta1s,beta2s,xdn1,xdn2, &
                                       cop1,cop2,thief1,thief2,cash1,cash2
  INTEGER(I4B)                      :: hugex,zeroex,ss,nmn1,nmn2

  hugex = 0
  zeroex= 0
  xn=REAL(n,DP)-xn0
  ss = SIGN(one,xn)*SIGN(one,xw)
  IF (xw > eps_CP) THEN
    eex = xn/xw
    IF (abs(eex)>Etol) THEN
      IF (ss==1) THEN
        hugex = 1
        zeroex= 0
      ELSE
        hugex = 0
        zeroex= 1
      ENDIF
    ENDIF
  ELSE
    IF (abs(xn)<eps_CP) THEN
      eex = zero
    ELSE
      IF (ss==1) THEN
        hugex = 1
        zeroex= 0
      ELSE
        hugex = 0
        zeroex= 1
      ENDIF
    ENDIF
  ENDIF
  nmn1 = n-n1
  nmn2 = n-n2
  IF (hugex==0.and.zeroex==0) THEN
    Ex = exp(eex)
    Ha = -one/(one+Ex)
    IF (n==1) THEN
      FUN = Xi
    ELSE
      FUN = Omega + Delta * Ha
    ENDIF
    La = (Ex * Ha) * Ha
    Ka = Delta * La
    Ga = Ha * (Ka * (Ex-one))

    Ve = (/one, Ha, zero, zero/)

    alpha1 = Omega-y1
    alpha2 = Omega-y2
    beta1  = Delta-alpha1
    beta2  = Delta-alpha2

    beta1s = beta1*beta1
    beta2s = beta2*beta2
    xdn1   = REAL(nmn1,DP)
    xdn2   = REAL(nmn2,DP)
    cash1  = xdn2/beta1s
    cash2  = xdn1/beta2s
    gama1  = Delta-2.0_DP*alpha1
    gama2  = Delta-2.0_DP*alpha2
    cop1   = xdn2/beta1
    cop2   = xdn1/beta2
    thief1 = cash1*Delta*gama1/(alpha1*alpha1)
    thief2 = cash2*Delta*gama2/(alpha2*alpha2)
    
    GRASS(2:4) = (/  -cop1+cop2, &
       -Delta *(cop1/alpha1), &
        Delta *(cop2/alpha2) /)
    GRASS(1) = -GRASS(3)-GRASS(4)

    Vee = zero
    Vee(5:7) = GRASS(2:4) * La

    HERB(1:4) = (/-thief1+thief2, -cash1+cash2, thief1, -thief2 /)
    HERB(5) = -HERB(2)
    HERB(6:10) = (/cash1, -cash2, -thief1, zero, thief2 /)

    GRASS  = Ve  + Ka * GRASS
    HERB = Vee + Ga * HERB

  ELSE IF (hugex==1.and.zeroex==0) THEN
    FUN = Omega
    GRASS  = (/one, zero, zero, zero/)
    HERB = zero
  ELSE IF (hugex==0.and.zeroex==1) THEN
    FUN = Omega - Delta
    GRASS  = (/one, -one, zero, zero/)
    HERB = zero
  ENDIF

 END SUBROUTINE MK_NORMAL_DST
!______________________________________________________________________________________________________________________
! SUBROUTINE MAKE_SizeDistr(ln_n00,ln_wid,V_mat, Ncutoff,Ncut0,N2USE_CURR_STR, donormalize)
!  IMPLICIT NONE
!  REAL(CP),intent(IN)               :: ln_n00,ln_wid
!  REAL(CP),dimension(:),intent(OUT) :: V_mat
!  INTEGER(I4B),intent(OUT)          :: Ncutoff,Ncut0
!  INTEGER(I4B),intent(IN)           :: N2USE_CURR_STR
!  INTEGER(I4B),intent(IN),optional  :: donormalize
!  REAL(DP)                          :: xlnw2,xlnw2_7,xlnw2_s,Cni,filnw,xlx00,vix,vix2,Deltah,sumal
!  INTEGER(I4B)                      :: iuu,j,n
!
!  V_mat = zero
!  xlnw2 = ln_wid*ln_wid
!  xlnw2_7= xlnw2 * 7.0_DP
!  xlnw2_s= one/(ln_wid*sr2)
!  Cni = one / (ln_wid*Pi2sqrt)
!  filnw=half/xlnw2
!  xlx00 = log(ln_n00)
!
!  Ncutoff = N2USE_CURR_STR
!  Ncut0 = 1
!
!!__________________________________ No cutting the tails
!!  iuu = MAX(Ncut0,FLOOR(ln_n00))
!!  do n=iuu,Ncut0,-1
!!    j=n
!!    vix = (xlogan(n)-xlx00-xlnw2_7)*xlnw2_s
!!    vix = half*(one+ERF0(vix,0))
!!    IF (vix<skip_tail) EXIT
!!  enddo
!!  Ncut0 = MAX(j,Ncut0)
!!
!!  iuu = MAX(Ncut0,CEILING(ln_n00))
!!  do n=iuu,Ncutoff
!!    j=n
!!    vix = (xlogan(n)-xlx00-xlnw2_7)*xlnw2_s
!!    vix = half*(one-ERF0(vix,0))
!!    IF (vix<skip_tail) EXIT
!!  enddo
!!  Ncutoff = MIN(j,Ncutoff)
!!  Ncut0 = MIN(Ncut0,Ncutoff)
!!  MIN_Ncut0 = min(Ncut0,MIN_Ncut0)
!!  Ncut0=MIN_Ncut0
!!__________________________________ No cutting the tails 
!
!  do n = Ncut0, Ncutoff
!    Deltah = xlogan(n)-xlx00
!    V_mat(n) = Cni * exp(-filnw*Deltah*Deltah) / real(n,DP)
!  enddo
!  if (present(donormalize)) then
!    if (donormalize==1) then
!      sumal=max(hundred*tiny_DP,sum(V_mat(Ncut0:Ncutoff)))
!      V_mat(Ncut0:Ncutoff)=V_mat(Ncut0:Ncutoff)/sumal
!    endif
!  endif
!
! END SUBROUTINE MAKE_SizeDistr

!______________________________________________________________________________________________________________________

 SUBROUTINE MAKE_SizeDistr2d(ln_n00,ln_wid, V_mat, Ncutoff,Ncut0,icurr_str, N2USE_CURR_STR, &
                        ln_n00_2nd,ln_wid_2nd,eigenangle,    N2USE_CURR_STR_2nd, K_DISTRU2D)
  IMPLICIT NONE
  REAL(CP),intent(IN)                     :: ln_n00,ln_wid
  REAL(CP),dimension(:),intent(OUT)       :: V_mat
  INTEGER(I4B),intent(OUT)                :: Ncutoff,Ncut0
  INTEGER(I4B),intent(IN)                 :: N2USE_CURR_STR,icurr_str
!_____________ OPTIONALS for 2d case
  REAL(CP),intent(IN),optional            :: ln_n00_2nd,ln_wid_2nd,eigenangle
  INTEGER(I4B),intent(IN),optional        :: N2USE_CURR_STR_2nd
  INTEGER(I4B),dimension(:,:),intent(IN),optional        :: K_DISTRU2D
  REAL(CP),dimension(:,:), allocatable    :: V_matd2, dummyden
!__________________________________ LOCAL VAR.S
  REAL(DP)                          :: xlnw2,xlnw2_7,xlnw2_s,Cni,filnw,xlx00,vix,vix2,Deltah,lobigg,losceps,Vmatmin
  INTEGER(I4B)                      :: iuu,j,n,nnx,nny,i2,i1
  REAL(DP)                          :: xlnw2_2nd, xlnw2_7_2nd, xlnw2_s_2nd, Cni_2nd, filnw_2nd, xlx00_2nd, &
                                       vix_2nd, vix2_2nd, Deltah_2nd, Cni_both, ln_Cni_both, dddx,dddxm,&
                                       trame(2,2),tramet(2,2),metm(2,2),vsys0(2),vsyse(2),vc0(2),vce(2),auv(2)
  REAL(DP)                          :: tan_eigenangle,cos_eigenangle,sin_eigenangle,xxxp,ivc0(2,2), &
                                       eigenangle_rad,sum_Vmatel
  REAL(DP)                          :: Vmatmax,Vmatthr,Vmatscal,nnpr
  INTEGER(I4B)                      :: iu, j_2nd, n_2nd, n2use_A, klin(2)
  LOGICAL  :: is1d

!______ 1-D case
is1d = (.not.PRESENT(ln_n00_2nd))
is1d=is1d.or.(.not.PRESENT(ln_wid_2nd))
is1d=is1d.or.(.not.PRESENT(eigenangle))
is1d=is1d.or.(.not.PRESENT(N2USE_CURR_STR_2nd))
Ncutoff = N2USE_CURR_STR
Ncut0 = 1


IF (ALLOCATED(V_matd2)) DEALLOCATE(V_matd2)
IF (ALLOCATED(dummyden)) DEALLOCATE(dummyden)
if (is1d) then
  ALLOCATE(V_matd2(1:N2USE_CURR_STR,1),dummyden(1:N2USE_CURR_STR,1))
else
  ALLOCATE(V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd),dummyden(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd))
endif
lobigg=-log(tiny_DP)
losceps=-log(sceps_DP)
V_mat = zero 

if (is1d) then
    xlnw2 = ln_wid*ln_wid
    xlnw2_7= xlnw2 * seven
    xlnw2_s= one/(ln_wid*sr2)
    Cni = one / (ln_wid*Pi2sqrt)
    filnw=0.5d0/xlnw2
    xlx00 = log(ln_n00)
    
    V_matd2=zero
    do n = 1,N2USE_CURR_STR
      Deltah=diam_logar(icurr_str,n,1)-xlx00
      V_matd2(n,1) = filnw*Deltah*Deltah
      dummyden(n,1) = exp(-diam_logar(icurr_str,n,1)) !one/real(n,DP)
    enddo
    dddxm=minval(V_matd2(1:N2USE_CURR_STR,1:1))
    V_matd2(1:N2USE_CURR_STR,1:1)=V_matd2(1:N2USE_CURR_STR,1:1)-dddxm
    where (V_matd2(1:N2USE_CURR_STR,1:1) < losceps) 
       V_matd2(1:N2USE_CURR_STR,1:1)=exp(-V_matd2(1:N2USE_CURR_STR,1:1)) &
                                                     * dummyden(1:N2USE_CURR_STR,1:1) 
    elsewhere
      V_matd2(1:N2USE_CURR_STR,1:1)=zero
    end where
    sum_Vmatel=sum(V_matd2, mask=V_matd2>=sceps_DP)
    where (V_matd2>=sceps_DP) 
      V_matd2=V_matd2/sum_Vmatel
    elsewhere
      V_matd2=zero
    end where
    
    V_mat(1:N2USE_CURR_STR) = V_matd2(1:N2USE_CURR_STR,1)
    DEALLOCATE(V_matd2,dummyden)
    
    return
endif
!______________________ real case 2-D
V_matd2 = zero

xlnw2 = ln_wid*ln_wid
xlnw2_7= xlnw2 * seven
xlnw2_s= one/(ln_wid*sr2)
filnw=half/xlnw2
xlx00 = log(ln_n00)

xlnw2_2nd = ln_wid_2nd*ln_wid_2nd
xlnw2_7_2nd= xlnw2_2nd * seven
xlnw2_s_2nd= one/(ln_wid_2nd*sr2)
filnw_2nd=half/xlnw2_2nd
xlx00_2nd = log(ln_n00_2nd)

eigenangle_rad = eigenangle*degrees_to_radians
sin_eigenangle=sin(eigenangle_rad)
cos_eigenangle=cos(eigenangle_rad)

trame(1,:)=[ cos_eigenangle,-sin_eigenangle]
trame(2,:)=[ sin_eigenangle, cos_eigenangle]

tramet=transpose(trame)
metm=zero
metm(1,1)=filnw
metm(2,2)=filnw_2nd
metm=matmul(tramet,matmul(metm,trame))

V_matd2=zero
do n = 1,N2USE_CURR_STR
  do n_2nd = 1, N2USE_CURR_STR_2nd
    j=ONE_FM_TWO(n,n_2nd,N2USE_CURR_STR,N2USE_CURR_STR_2nd)
    nnpr=exp(-diam_logar(icurr_str,j,1)-diam_logar(icurr_str,j,2))
    vsys0(:)=diam_logar(icurr_str,j,:)-[xlx00,xlx00_2nd]
    V_matd2(n,n_2nd) = sum(vsys0*matmul(metm,vsys0))
    dummyden(n,n_2nd) = nnpr
  enddo
enddo
dddxm=minval(V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd))
V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)=V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)-dddxm
where (V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd) < losceps) 
  V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)=exp(-V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)) &
                                                 * dummyden(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)
elsewhere
  V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd)=zero
end where
sum_Vmatel=sum(V_matd2,&
               mask=V_matd2>=sceps_DP)
where (V_matd2>=sceps_DP) 
  V_matd2=V_matd2/sum_Vmatel
elsewhere
  V_matd2=zero
end where

!print*, ' CHECK SUM V_matd2 = ', SUM(V_matd2), maxval(V_matd2),minval(V_matd2)

Vmatmax=maxval(V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd))
Vmatmin=minval(V_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd))

n2use_A = N2USE_CURR_STR*N2USE_CURR_STR_2nd

do j=1,n2use_A
   klin(:) = K_DISTRU2D(:,j)
   n = klin(1)
   n_2nd = klin(2)
   V_mat(j) = V_matd2(n,n_2nd)
enddo

!print*, ' CHECK SUM V_mat = ', SUM(V_mat), maxval(V_mat),minval(V_mat)
DEALLOCATE(V_matd2,dummyden)
Ncutoff = n2use_A

END SUBROUTINE MAKE_SizeDistr2d
!______________________________________________________________________________________________________________________
subroutine FILL_VAR_BO(na,npa,nclu,valBpar,valOpar,MTX_B,MTX_O,VVV_O,VVV_B,lawB,lawO,diamcl)
implicit none
integer(I4B),intent(IN) :: na,npa,nclu,lawB(na),lawO(na)
real(DP),intent(IN)     :: valBpar(3,na),valOpar(3,na),diamcl(nclu)
real(DP),intent(OUT)    :: MTX_B(npa,nclu),MTX_O(npa,nclu),VVV_O(na,nclu),VVV_B(na,nclu)
real(DP) :: bbb1,bbb2,ooo1,ooo2
integer(I4B) :: ia1,ia2,ipair,iclu

do iclu=1,nclu
  ipair=0
  do ia1=1,na
    if (lawB(ia1)==1) then
      bbb1 = valBpar(1,ia1)
    else if (lawB(ia1)==2) then
      bbb1 = valBpar(1,ia1) + (valBpar(2,ia1)-valBpar(1,ia1))*exp(-diamcl(iclu)/valBpar(3,ia1))
    endif
    VVV_B(ia1,iclu) = bbb1
    if (lawO(ia1)==1) then
      ooo1 = valOpar(1,ia1)
    else if (lawO(ia1)==2) then
      ooo1 = valOpar(1,ia1) + (valOpar(2,ia1)-valOpar(1,ia1))*exp(-diamcl(iclu)/valOpar(3,ia1))
    endif
    VVV_O(ia1,iclu) = ooo1
    do ia2=ia1,na
      ipair=ipair+1
      if (lawB(ia2)==1) then
        bbb2 = valBpar(1,ia2)
      else if (lawB(ia2)==2) then
        bbb2 = valBpar(1,ia2) + (valBpar(2,ia2)-valBpar(1,ia2))*exp(-diamcl(iclu)/valBpar(3,ia2))
      endif
      if (lawO(ia2)==1) then
        ooo2 = valOpar(1,ia2)
      else if (lawO(ia2)==2) then
        ooo2 = valOpar(1,ia2) + (valOpar(2,ia2)-valOpar(1,ia2))*exp(-diamcl(iclu)/valOpar(3,ia2))
      endif
      MTX_B(ipair,iclu) = half*(bbb1+bbb2)
      MTX_O(ipair,iclu) = ooo1*ooo2
    enddo
  enddo
enddo

end subroutine FILL_VAR_BO
!***************************************************************************************************
 SUBROUTINE MAKE_Bmat(B0,B1,B_mat, Ncutoff,Ncut0,n2use_i)
  IMPLICIT NONE
  REAL(CP),intent(IN)               :: B0, B1
  REAL(CP),dimension(:),intent(OUT) :: B_mat
  INTEGER(I4B),intent(IN)           :: Ncutoff,Ncut0,n2use_i
  REAL(DP)                          :: c1,c0,nx,nxi
  INTEGER(I4B)                      :: n

! if B0 = 1 and B1=B0=1 then B_mat(:) = 1
! in this case you see just what you put in the .pha

  B_mat = 0.d0
  nxi = REAL(n2use_i-1,DP)
  nx = one/nxi
  c1 = (B1-B0)*nx
  c0 = (nxi*B0+B0-B1)*nx


  do n = Ncut0, Ncutoff
    B_mat(n) = C0 + C1 * n
  enddo

 END SUBROUTINE MAKE_Bmat
 !______________________________________________________________________________________________________________________
 SUBROUTINE MAKE_Bmat2d(B0,B1,B_mat, Ncutoff,Ncut0,n2use_i,B0_2nd,B1_2nd,Ncutoff_2nd,Ncut0_2nd,n2use_2nd)
  IMPLICIT NONE
  REAL(CP),intent(IN)               :: B0, B1
  REAL(CP),dimension(:),intent(OUT) :: B_mat
  INTEGER(I4B),intent(IN)           :: Ncutoff,Ncut0,n2use_i
  REAL(DP)                          :: c1,c0,nx,nxi
  INTEGER(I4B)                      :: n, n_2nd, j
!_____________ OPTIONALS for 2d case
  REAL(CP),intent(IN),OPTIONAL               :: B0_2nd, B1_2nd
  INTEGER(I4B),intent(IN),OPTIONAL           :: Ncutoff_2nd,Ncut0_2nd,n2use_2nd
  REAL(DP),dimension(:,:),allocatable          :: B_matd2
  LOGICAL  :: b01d
  
! if B0 = 1 and B1=B0=1 and [B0_2nd = 1 and B1_2nd=B0_2nd=1] then B_mat(:) = 1
! in this case you see just what you put in the .pha

 b01d = (.not.PRESENT(B0_2nd))
 IF (b01d) THEN
    B_mat = 0.d0
    nxi = REAL(n2use_i-1,DP)
    nx = one/nxi
    c1 = (B1-B0)*nx
    c0 = (nxi*B0+B0-B1)*nx


    do n = Ncut0, Ncutoff
      B_mat(n) = C0 + C1 * n
    enddo
  ELSE
    IF (ALLOCATED(B_matd2)) DEALLOCATE(B_matd2)
    ALLOCATE(B_matd2(1:n2use_i,1:n2use_2nd))
    B_matd2 = 0.d0
    nxi = REAL(n2use_i-1,DP)
    nx = one/nxi
    c1 = (B1-B0)*nx
    c0 = (nxi*B0+B0-B1)*nx
    
    do n = 1, n2use_i
       B_matd2(n,1) = C0 + C1 * n
    enddo
    
    nxi = REAL(n2use_2nd-1,DP)
    nx = one/nxi
    c1 = (B1_2nd-B0_2nd)*nx
    c0 = (nxi*B0_2nd+B0_2nd-B1_2nd)*nx
    
    do n = 1, n2use_i
       do n_2nd=2,n2use_2nd
         B_matd2(n,n_2nd) = B_matd2(n,1) + C1 * (n_2nd-1)
       enddo
    enddo
    
    B_matd2=max(B_matd2,sceps_DP)
    
    do n=1,n2use_i
       do n_2nd=1,n2use_2nd
          j=ONE_FM_TWO(n,n_2nd,n2use_i,n2use_2nd)
          B_mat(j) = B_matd2(n,n_2nd)
       enddo
    enddo
    IF (ALLOCATED(B_matd2)) DEALLOCATE(B_matd2)
  ENDIF

 END SUBROUTINE MAKE_Bmat2d
!______________________________________________________________________________________________________________________
 SUBROUTINE MAKE_DBmat(B0,B1,B_mat,DB_mat, Ncutoff,Ncut0,n2use_i)
  IMPLICIT NONE
  REAL(CP),intent(IN)                 :: B0, B1
  REAL(CP),dimension(:),intent(OUT)   :: B_mat
  REAL(CP),dimension(:,:),intent(OUT) :: DB_mat
  INTEGER(I4B),intent(IN)             :: Ncutoff,Ncut0,n2use_i
  REAL(DP)                            :: c1,c0,nx
  INTEGER(I4B)                        :: n

  B_mat = 0.d0
  nx = one/REAL(n2use_i-1,DP)
  c1 = (B1-B0)*nx
  c0 = (n2use_i*B0-B1)*nx

  do n = Ncut0, Ncutoff
    B_mat(n) = C0 + C1 * n
    DB_mat(:,n) = (/-n+n2use_i, n-1/)*nx
  enddo

 END SUBROUTINE MAKE_DBmat
!______________________________________________________________________________________________________________________
FUNCTION NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
  IMPLICIT NONE
  REAL(CP),intent(IN)               :: Omega,xn0,xw,Delta,Etol
  REAL(CP)                          :: NORMAL_AMAT
  INTEGER(I4B),intent(IN)           :: n
  REAL(DP)                          :: xn,eex,epsloc
  INTEGER(I4B)                      :: hugex,zeroex,ss

  hugex = 0
  zeroex= 0
  epsloc = EXP(-Etol)
  xn=REAL(n,DP)-xn0
  ss = SIGN(one,xn)*SIGN(one,xw)
  IF (xw > epsloc) THEN
    eex = xn/xw
    IF (abs(eex)>Etol) THEN
      IF (ss==1) THEN
    hugex = 1
    zeroex= 0
      ELSE
    hugex = 0
    zeroex= 1
      ENDIF
    ENDIF
  ELSE
    IF (abs(xn)<epsloc) THEN
      eex = zero
    ELSE
      IF (ss==1) THEN
        hugex = 1
        zeroex= 0
      ELSE
        hugex = 0
        zeroex= 1
      ENDIF
    ENDIF
  ENDIF
  IF (hugex==0.and.zeroex==0) THEN
    normal_amat = Omega - Delta / (one+exp(eex))
  ELSE IF (hugex==1.and.zeroex==0) THEN
    normal_amat = Omega
  ELSE IF (hugex==0.and.zeroex==1) THEN
    normal_amat = Omega - Delta
  ENDIF

END FUNCTION NORMAL_AMAT
!______________________________________________________________________________________________________________________
 SUBROUTINE MAKE_Amat(n1,n2,Omega,Xi,xn0,xw,y1,y2,Delta,A_mat,Ncutoff,Ncut0,Etol,numcase, straflag)
  IMPLICIT NONE
  REAL(CP),intent(IN)               :: Omega,Xi,xn0,xw,y1,y2,Delta,Etol
  REAL(CP),dimension(:),intent(OUT) :: A_mat
  INTEGER(I4B),intent(IN)           :: n1,n2,Ncutoff,Ncut0,numcase
  INTEGER(I4B),optional,intent(IN)  :: straflag
  REAL(DP)                          :: xn,eex,heps,za,ta,cex
  INTEGER(I4B)                      :: i,j,k,hugex,zeroex,ss,n
  INTEGER(I4B)  :: strain_fun

  strain_fun=0
  if (present(straflag)) then
    strain_fun=straflag
  endif
  A_mat = zero
  
  if (strain_fun==1) then
    A_mat(Ncut0:Ncutoff) = Omega
    return
  endif
  if (strain_fun==2) then
    DO n=Ncut0,Ncutoff
      IF (n==n1) THEN
        A_mat(n) = y1
        CYCLE
      ELSE IF (n==n2) THEN
        A_mat(n) = Omega
        CYCLE
      ENDIF
      A_mat(n) = Omega+(Omega-y1)*REAL(n-n1-1,DP)
    enddo
    return
  endif
  
  DO n=Ncut0,Ncutoff
!____ Easy cases
!
    IF (n==n1) THEN
      A_mat(n) = y1
      CYCLE
    ELSE IF (n==n2) THEN
      A_mat(n) = y2
      CYCLE
    ENDIF
!____ Less easy cases
!
    IF (numcase /= 5 .and. n==1) THEN
      A_mat(n) = Xi
      CYCLE
    ENDIF
!____ Normal cases
!
    SELECT CASE(numcase)
    CASE(0)
      A_mat(n) = Omega
    CASE(1)
      IF (n<n1) THEN
        A_mat(n) = Omega-Delta
      ELSE
        A_mat(n) = NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
      ENDIF
    CASE(2)
      IF (n>n2) THEN
        A_mat(n) = Omega
      ELSE
        A_mat(n) = NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
      ENDIF
    CASE(3)
      IF (n<n1) THEN
        A_mat(n) = Omega-Delta
      ELSE
        A_mat(n) = NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
      ENDIF
    CASE(4)
      IF (n>n2) THEN
        A_mat(n) = Omega
      ELSE
        A_mat(n) = NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
      ENDIF
    CASE(5)
      heps = EXP(-Etol)
      za = ABS(Omega-y1)
      ta = ABS(Delta-Omega+y1)
      cex = ABS(Delta)*REAL(n-n1,DP)/(za*ta)
      A_mat(n) = Omega-Delta*za/(za+ta*EXP(cex*heps))
    CASE(6)
      A_mat(n) = NORMAL_AMAT(n,Omega,Delta,xn0,xw,Etol)
    END SELECT

  ENDDO

 END SUBROUTINE MAKE_Amat
!______________________________________________________________________________________________________________________
 SUBROUTINE MAKE_Amat2d(y1,y2, A_mat, N2USE_CURR_STR, y1_2nd, N2USE_CURR_STR_2nd)
 
 IMPLICIT NONE
 REAL(CP),intent(IN)                     :: y1,y2
 REAL(CP),dimension(:),intent(OUT)       :: A_mat
 INTEGER(I4B),intent(IN)                 :: N2USE_CURR_STR
 INTEGER(I4B),intent(IN),optional        :: N2USE_CURR_STR_2nd
!_____________ OPTIONALS for 2d case
 REAL(CP),intent(IN),optional            :: y1_2nd
 REAL(CP),dimension(:,:), allocatable    :: A_matd2
!__________________________________ LOCAL VAR.S
 REAL(DP)                          :: Delta,Delta_2nd,c1,a1,c2,a2
 INTEGER(I4B)                      :: n,n_2nd, j
 logical :: is1d
 
 is1d = (.not.PRESENT(N2USE_CURR_STR_2nd))
 is1d=is1d.or.(.not.PRESENT(y1_2nd))
 if (is1d) then
   Delta=y1-y2
   do n = 1,N2USE_CURR_STR
     c1=three*half/(half+real(n,DP))
     a1=y2+Delta*c1
     A_mat(n) = a1
   enddo
   return
 endif
 IF (ALLOCATED(A_matd2)) DEALLOCATE(A_matd2)
 ALLOCATE(A_matd2(1:N2USE_CURR_STR,1:N2USE_CURR_STR_2nd))
 
  A_matd2=one
 
 
 !...
 Delta     = y1     - y2
 Delta_2nd = y1_2nd - y2
 do n = 1,N2USE_CURR_STR
   c1=three*half/(half+real(n,DP))
   a1=y2+Delta*c1
   do n_2nd = 1, N2USE_CURR_STR_2nd
     c2=three*half/(half+real(n_2nd,DP))
     a2=y2+Delta_2nd*c2
     A_matd2(n,n_2nd) = exp(unter*(two*log(a1)+log(a2)))
   enddo
 enddo
 
 do n=1,N2USE_CURR_STR
    do n_2nd=1,N2USE_CURR_STR_2nd
          j=ONE_FM_TWO(n,n_2nd,N2USE_CURR_STR,N2USE_CURR_STR_2nd)
          A_mat(j) = A_matd2(n,n_2nd)
    enddo
 enddo
 
 DEALLOCATE(A_matd2)
 
END SUBROUTINE MAKE_Amat2d
!*******************************************************************************
subroutine Find_AlphaBeta_Strain(D1t,D2t,yv1,yv2,alpha_str,beta_str)
implicit none
integer(I4B),intent(IN) :: D1t, D2t
real(DP),intent(IN)  :: yv1,yv2
real(DP),intent(OUT)  :: alpha_str,beta_str
real(DP) :: dy,dd,dnm

dy=yv2-yv1
dd=D2t-D1t
dnm=one/(-D1t*dy + dd*yv1)

beta_str = -one + yv1 + D2t*dy*yv1 * dnm
alpha_str = D1t*D2t*dy * dnm


end subroutine Find_AlphaBeta_Strain

END MODULE SERVICE
!_______________________________________________________________________________________________

MODULE CALCFUN_DB2
 use LINALG_TOOLS
 USE NANO_TYPES
 USE CALC_WSPACE
 use SERVICE
 use calcdp_sam

 real(DP),save :: ckseq(3)
 integer(I4B),save  :: auxout = 1

contains

 SUBROUTINE NANO_UPDATE_AMO(npin,pin,Curr_Set)

   implicit real(DP)(a-h,o-z),integer(I4B)(i-n)

   INTEGER(I4B),INTENT(IN)            :: Curr_Set
   INTEGER(I4B),OPTIONAL,INTENT(IN)   :: npin           ! npin = n. par. da raffinare
   REAL(CP),OPTIONAL,INTENT(IN)       :: pin(:)

!_____ Definire Curr_Str = I of current   

   IF (PRESENT(npin) .and.(.not.PRESENT(pin))) STOP 'NANO_UPDATE_AMO: error 1'
   IF (PRESENT(pin) .and.(.not.PRESENT(npin))) STOP 'NANO_UPDATE_AMO: error 2'
   IF (PRESENT(pin) ) THEN
       IF (size(pin)/=npin) STOP 'NANO_UPDATE_AMO: error 3'
   ENDIF
   IF (PRESENT(npin)) THEN
     IF (npin /= SUM(PARAGLOB%nano_doit)) &
       STOP 'Error[NANO_UPDATE_AMO] : npin /= SUM(PARAGLOB%nano_doit) !!? '
   ENDIF

   PARAGLOB%nano_parcurr = PARAGLOB%nano_par0
   PARAGLOB%nano_par0E = PARAGLOB%nano_par0
   IF (PRESENT(npin)) THEN
     IF (npin>0) PARAGLOB%nano_parcurr(PARAGLOB%nano_mask(1:npin)) = pin(1:npin)
   ENDIF
   PARAGLOB%nano_parcurrE = PARAGLOB%nano_parcurr

   amo_u0  = PARAGLOB%nano_parcurr(1)
   amo_u1  = PARAGLOB%nano_parcurr(2)

   call REFRESH_AMOR(i=Curr_Set,u0=amo_u0,u1=amo_u1)

 END SUBROUTINE NANO_UPDATE_AMO

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 SUBROUTINE NANO_CALC_DBX(npin,pin,Curr_Str,Curr_Set)

   implicit real(DP)(a-h,o-z),integer(I4B)(i-n)

   INTEGER(I4B),INTENT(IN)            :: Curr_Str,Curr_Set      
   INTEGER(I4B),OPTIONAL,INTENT(IN)   :: npin           ! npin = n. par. da raffinare
   REAL(CP),OPTIONAL,INTENT(IN)       :: pin(:)

   REAL(DP)               :: alp4(4)
   REAL(DP)               :: ln_n00,ln_wid, ln_n00_2nd,ln_wid_2nd, dA_A,sig,gau_fwhm,nk,dxxx
   LOGICAL                :: refresh_Umat
   LOGICAL                :: refresh_Tmat
   character(132)         :: funga 
   INTEGER(I4B)           :: klin(2)
   REAL(DP)               :: DWpars(NumPar_DW,100),OKpars(NumPar_Oc,100), cal_last, cal_one
   
  
  REAL(DP), allocatable  :: micr_cal(:)
  logical, allocatable   :: MICR_READY(:)
 integer(I4B),allocatable,save  :: K_DISTRU2D(:,:)

   illogik = .false.

!_____ Definire Curr_Str = I of current   

  IF (PRESENT(npin) .and.(.not.PRESENT(pin))) STOP 'NANO_CALC_DBX: error 1'
  IF (PRESENT(pin) .and.(.not.PRESENT(npin))) STOP 'NANO_CALC_DBX: error 2'
  IF (PRESENT(pin) ) THEN
      IF (size(pin)/=npin) STOP 'NANO_CALC_DBX: error 3'
  ENDIF
  IF (PRESENT(npin)) THEN
    IF (npin /= SUM(PARAPHAS(Curr_Str)%nano_doit)) &
      STOP 'Error[NANO_CALC_DBX] : npin /= SUM(PARAPHAS(Curr_Str)%nano_doit) !!? '
  ENDIF

  refresh_Umat = .true.
!   refresh_Tmat = .true.

  CALPHA_W(CURR_SET,CURR_STR)%vdata = zero

  PARAPHAS(CURR_STR)%nano_parcurr = PARAPHAS(CURR_STR)%nano_par0
  
  IF (PRESENT(npin)) THEN
    IF (npin>0) THEN
      PARAPHAS(CURR_STR)%nano_parcurr(PARAPHAS(CURR_STR)%nano_mask(1:npin)) = pin(1:npin)
      refresh_Umat = ( (kount_calc<=2) .or. ANY(PARAPHAS(Curr_Str)%nano_doit(6:9)==1) )
    ENDIF
  ENDIF

!
!_______ Chapter 1 : SIZE DISTRIBUTION par.s
!
  avLN  = PARAPHAS(CURR_STR)%nano_parcurr(1)
  sdLN  = PARAPHAS(CURR_STR)%nano_parcurr(2)
  av2LN=zero
  sd2LN=zero
  phiLN=zero
  N2USE_CURR_STR = N2USE_W(CURR_STR)
!  IF (DB_INDEX_W(CURR_STR) == 4) THEN  
  IF (DB_INDEX_W(CURR_STR) == 4 .or. DB_INDEX_W(CURR_STR) == 5) THEN
      av2LN = PARAPHAS(CURR_STR)%nano_parcurr(3)
      sd2LN = PARAPHAS(CURR_STR)%nano_parcurr(4)
      phiLN = PARAPHAS(CURR_STR)%nano_parcurr(5)
      N2USE_CURR_STR =  N2USE_ab(CURR_STR,2)
      N2USE_CURR_STR_2nd = N2USE_c(CURR_STR,2)
  ENDIF
  if (ANY([ISNAN(avLN),ISNAN(sdLN),ISNAN(av2LN),ISNAN(sd2LN),ISNAN(phiLN)])) then
    print*,'found_NaN in avLN,av2LN,sdLN,sd2LN,phiLN'
  endif
!
!_______ Chapter 2 : LATTICE EXPANSION par.s
!  
  Omega = PARAPHAS(CURR_STR)%nano_parcurr(6) ! value at infinity, for all curves (for str_cod=0,1,2)!
  
  IF (PARAPHAS(CURR_STR)%str_cod == 0) THEN
      y1    = PARAPHAS(CURR_STR)%nano_parcurr(7)
      y2    = PARAPHAS(CURR_STR)%nano_parcurr(8)
      Delta = PARAPHAS(CURR_STR)%nano_parcurr(9)
      ckseq = (/Omega-Delta-y1,y1-y2,y2-Omega/)
      where(abs(ckseq) < eps_DP) ckseq = zero
      illogik = .not.(ALL(ckseq>=zero).or.ALL(ckseq<=zero))
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 1) THEN
      y1    = Omega 
      y2    = Omega 
      Delta = zero
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 2) THEN
      !______________ INVERSE_LINEAR strain : s(n) = Omega+(Xi-Omega)/(2*n+1)
      y2 = Omega
      y1 = PARAPHAS(CURR_STR)%nano_parcurr(7)
      Delta = zero ! no use
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 3) THEN
      !______________ INVERSE_LINEAR strain for rods: 
 !                                            s(n) = Omega-3*Delta/(2*n+1)
 !                                            s(n_2nd) = Omega_2nd-3*Delta_2nd/(2*n_2nd+1)
      y2 = Omega
      y1 = PARAPHAS(CURR_STR)%nano_parcurr(7)
      y1_2nd = PARAPHAS(CURR_STR)%nano_parcurr(8)
  ELSE IF (PARAPHAS(CURR_STR)%str_cod > 3 .and. PARAPHAS(CURR_STR)%str_cod <= 6) THEN
      alpha_str = Omega * Downscale_Par
      beta_str  = PARAPHAS(CURR_STR)%nano_parcurr(7) * Downscale_Par
      IF (MICRO_FLAG(CURR_STR)==1) then 
           dA_A=PARAPHAS(CURR_STR)%nano_parcurrE(9)
      ENDIF
  ENDIF
  
  !______ Penalty ...
  if (illogik) then
    call RANDOM_NUMBER(CALPHA_W(CURR_SET,CURR_STR)%vdata(:))
    if (auxout==1) print*,'ILLOGIK from calcfun !!!',illogik,ckseq
    return
  endif
  if (ANY([ISNAN(y1),ISNAN(y2),ISNAN(Delta),ISNAN(Omega)])) then
    print*,'found_NaN in y1,y2,Delta,Omega'
  endif

  PARAPHAS(CURR_STR)%nano_parcurrE = PARAPHAS(CURR_STR)%nano_parcurr

  IF (DB_INDEX_W(CURR_STR) /= 1) call COLON_AS_CL(avLN=avLN,sdLN=sdLN,ln_n00=ln_n00,ln_wid=ln_wid)
  
  IF (DB_INDEX_W(CURR_STR) == 4 .or. DB_INDEX_W(CURR_STR) == 5) & 
      call COLON_AS_CL(avLN=av2LN,sdLN=sd2LN,ln_n00=ln_n00_2nd,ln_wid=ln_wid_2nd)
  
  Ncutoff=N2USE_W(CURR_STR); Ncut0=1

  IF (PARAPHAS(CURR_STR)%str_cod == 0) THEN
    IF (PRESENT(npin)) THEN
      call COSTR_IN2EX(PARAPHAS(CURR_STR)%n1,PARAPHAS(CURR_STR)%n2,Omega,Xi,xn0,xw,y1,y2,Delta,numcase,Etol, &
           straflag=PARAPHAS(CURR_STR)%str_cod)
      PARAPHAS(CURR_STR)%nano_parcurrE(6:9) = (/Omega, Xi, xn0, xw/)
    ELSE
      Xi    = PARAPHAS(CURR_STR)%nano_par0E(7)
      xn0   = PARAPHAS(CURR_STR)%nano_par0E(8)
      xw    = PARAPHAS(CURR_STR)%nano_par0E(9)
    ENDIF
    call CHECKCASE(y1,y2,Omega,Delta,numcase,Etol)
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 1) THEN
  !!! microstrain 
  IF (MICRO_FLAG(CURR_STR)==1) then 
    dA_A=PARAPHAS(CURR_STR)%nano_parcurrE(9)
    IF (PRESENT(npin)) THEN
     PARAPHAS(CURR_STR)%nano_parcurrE(6:9) = (/Omega, Omega, real(Ncutoff+Ncut0,DP)*half,dA_A/)
     ELSE
      Xi    = PARAPHAS(CURR_STR)%nano_par0E(6)
      xn0   = real(Ncutoff+Ncut0,DP)*half
      xw    = dA_A
    ENDIF
    !!
  ELSE  
    IF (PRESENT(npin)) THEN
      PARAPHAS(CURR_STR)%nano_parcurrE(6:9) = (/Omega, Omega, real(Ncutoff+Ncut0,DP)*half, one/)
    ELSE
      Xi    = PARAPHAS(CURR_STR)%nano_par0E(6)
      xn0   = real(Ncutoff+Ncut0,DP)*half
      xw    = one
    ENDIF
  ENDIF
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 2) THEN
     !______________ INVERSE LINEAR strain
    IF (PRESENT(npin)) THEN
      Xi = y1
      xn0   = real(Ncutoff+Ncut0,DP)*half
      xw    = one
      PARAPHAS(CURR_STR)%nano_parcurrE(6:9) = (/Omega, Xi, xn0, xw/)
    ELSE
      Xi    = PARAPHAS(CURR_STR)%nano_par0E(7)  ! value at 0
      xn0   = real(Ncutoff+Ncut0,DP)*half
      xw    = one
    ENDIF
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 3) THEN
     !______________ INVERSE LINEAR strain in 2D : (n,2_nd)
    IF (PRESENT(npin)) THEN
      Xi = y1
      xn0   = y1_2nd
      xw    = zero
      PARAPHAS(CURR_STR)%nano_parcurrE(6:9) = (/Omega, Xi, xn0, xw/)
    ELSE
      Xi    = PARAPHAS(CURR_STR)%nano_par0E(7)  ! value at 0
      xn0   = PARAPHAS(CURR_STR)%nano_par0E(8)
      xw    = zero
    ENDIF
  ENDIF
!_______________ MAKE V_MAT --> size distribution

  IF (DB_INDEX_W(CURR_STR) == 1 ) THEN
      V_mat = one
      Ncut0 = 1
      Ncutoff = 1
  ELSE IF (DB_INDEX_W(CURR_STR) == 2 .or. DB_INDEX_W(CURR_STR) == 3) THEN
      call MAKE_SizeDistr2d(ln_n00=ln_n00,ln_wid=ln_wid,V_mat=V_mat,Ncutoff=Ncutoff,Ncut0=Ncut0, &
                            icurr_str=CURR_STR,N2USE_CURR_STR = N2USE_W(CURR_STR) )
  ELSE IF (DB_INDEX_W(CURR_STR) == 4 .or. DB_INDEX_W(CURR_STR) == 5) THEN
      IF (allocated(K_DISTRU2D)) deallocate(K_DISTRU2D)
      ALLOCATE( K_DISTRU2D(2,nano_iav(CURR_STR)%dimstruk))
      do n=1,nano_iav((CURR_STR))%dimstruk
        K_DISTRU2D(:,n) = nano_iav(CURR_STR)%post_office_I(:,n)
      enddo
!_______________ MAKE V_MAT2D --> size distribution
      call MAKE_SizeDistr2d(ln_n00=ln_n00,ln_wid=ln_wid,V_mat=V_mat,Ncutoff=Ncutoff,Ncut0=Ncut0, &
                            icurr_str=CURR_STR,N2USE_CURR_STR=N2USE_CURR_STR, &
                            ln_n00_2nd=ln_n00_2nd,ln_wid_2nd=ln_wid_2nd, &
                            eigenangle=phiLN,N2USE_CURR_STR_2nd=N2USE_CURR_STR_2nd,K_DISTRU2D=K_DISTRU2D)
  ENDIF
  if (ANY(ISNAN(V_mat))) then
    print*,'found_NaN V_mat'
  endif
  
 !_______________ MAKE A_MAT / A_MAT2D --> strain distribution
  
  IF (PARAPHAS(CURR_STR)%str_cod == 0) THEN
      call MAKE_Amat(PARAPHAS(CURR_STR)%n1,PARAPHAS(CURR_STR)%n2,Omega,Xi,xn0,xw,y1,y2,Delta, &
                     A_mat,Ncutoff,Ncut0,Etol,numcase)
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 1) THEN
      A_mat(Ncut0:Ncutoff) = Omega
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 2) THEN
      hog=real(PARAPHAS(CURR_STR)%n1*2+1,DP)
      A_mat(Ncut0:Ncutoff) = y2+half*(y2-y1)*hog*(one-(hog+two)/(/(REAL(2*n+1,DP),n=Ncut0,Ncutoff)/))   
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 3) THEN
      IF (DB_INDEX_W(CURR_STR) == 4) THEN  !!!! the case 'str_cod=3 & DB_INDEX=5' is not allowed
         call MAKE_Amat2d(y1,y2, A_mat, N2USE_CURR_STR, y1_2nd, N2USE_CURR_STR_2nd)
      ELSE
         call MAKE_Amat2d(y1,y2, A_mat, N2USE_CURR_STR)
      ENDIF
!!!!!!!!!! AC 28.5.2014
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 4) THEN
    A_mat(Ncut0:Ncutoff) = [( (one - alpha_str/( exp(diam_logar(CURR_STR,n,1)) &
                              + alpha_str))*(one + beta_str) , n=Ncut0,Ncutoff )]
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 5) THEN
    A_mat(Ncut0:Ncutoff) = [( &
       (one - alpha_str/( &
       ( half*exp(diam_logar(CURR_STR,n,1)+diam_logar(CURR_STR,n,2)) / &
         ( exp(diam_logar(CURR_STR,n,1)) + two*exp(diam_logar(CURR_STR,n,2)) ) ) &
                              + alpha_str)) &
                              *(one + beta_str) &
                              , n=Ncut0,Ncutoff )]
  ELSE IF (PARAPHAS(CURR_STR)%str_cod == 6) THEN
    A_mat(Ncut0:Ncutoff) = [( (one - alpha_str/( exp(diam_logar(CURR_STR,n,1)) + exp(diam_logar(CURR_STR,n,2)) &
                              + alpha_str))*(one + beta_str) , n=Ncut0,Ncutoff )]
                              
  ENDIF
  if (ANY(ISNAN(A_mat(Ncut0:Ncutoff)))) then
    print*,'found_NaN A_mat'
  endif

 !_______________ FILL_VAR_BO --> B, Occ distribution

   do ia1=1,NSP_AT_W(CURR_STR)
     OKpars(:,ia1) = PARAPHAS(CURR_STR)%nano_parcurr(NumParPha+NumPar_at*(ia1-1)+1:&
                                                     NumParPha+NumPar_at*(ia1-1)+NumPar_Oc)
     DWpars(:,ia1) = PARAPHAS(CURR_STR)%nano_parcurr(NumParPha+NumPar_at*(ia1-1)+NumPar_Oc+1:&
                                                     NumParPha+NumPar_at*(ia1-1)+NumPar_Oc+NumPar_DW)
   enddo
   
 ! put occ.s in occusite
   IF (DB_INDEX_W(CURR_STR) > 2) THEN
     call FILL_VAR_BO(na=NSP_AT_W(CURR_STR), npa=NPAIR_AT_W(Curr_Str), &
                    nclu=OccMT(CURR_STR)%nclu_dim, &
                    valBpar=DWpars(:,1:NSP_AT_W(CURR_STR)), valOpar=OKpars(:,1:NSP_AT_W(CURR_STR)), &
                    MTX_B=DWalMT(CURR_STR)%MTvalue(:,:),MTX_O=OccMT(CURR_STR)%MTvalue(:,:), &
                    VVV_O=OccMT(CURR_STR)%VVvalue(:,:),VVV_B=DWalMT(CURR_STR)%VVvalue(:,:), &
                    lawB=PARAPHAS(CURR_STR)%law_B(:),lawO=PARAPHAS(CURR_STR)%law_O(:),diamcl=OccMT(CURR_STR)%DiamClu(:))
   
     do i=Ncut0,Ncutoff
       nano_iav(CURR_STR)%struk(i)%occusite = OccMT(CURR_STR)%VVvalue(:,i)
       nano_iav(CURR_STR)%struk(i)%DebyeWallerB = DWalMT(CURR_STR)%VVvalue(:,i)
! now also in occupair
       ipair=0
       do ia1=1,NSP_AT_W(CURR_STR)
         do ia2=ia1,NSP_AT_W(CURR_STR)
           ipair=ipair+1
           nano_iav(CURR_STR)%struk(i)%occupair(ipair)=OccMT(CURR_STR)%MTvalue(ipair,i)
         enddo
       enddo
     enddo
   ELSE IF (DB_INDEX_W(CURR_STR) == 2) THEN
     do i=Ncut0,Ncutoff
       nano_iav(CURR_STR)%struk(i)%occusite = one
       nano_iav(CURR_STR)%struk(i)%DebyeWallerB = DWpars(1,1)
! now also in occupair
       ipair=0
       do ia1=1,NSP_AT_W(CURR_STR)
         do ia2=ia1,NSP_AT_W(CURR_STR)
           ipair=ipair+1
           nano_iav(CURR_STR)%struk(i)%occupair(ipair) = one
           nano_iav(CURR_STR)%struk(i)%DebyeWallerB = DWpars(1,1)
         enddo
       enddo
     enddo
   ELSE IF (DB_INDEX_W(CURR_STR) == 1) THEN
     do i=Ncut0,Ncutoff
       nano_iav(CURR_STR)%struk(i)%occusite = OKpars(1,1:NSP_AT_W(CURR_STR))
       nano_iav(CURR_STR)%struk(i)%DebyeWallerB = DWpars(1,1:NSP_AT_W(CURR_STR))
! now also in occupair
       ipair=0
       do ia1=1,NSP_AT_W(CURR_STR)
         do ia2=ia1,NSP_AT_W(CURR_STR)
           ipair=ipair+1
           nano_iav(CURR_STR)%struk(i)%occupair(ipair) = OKpars(1,ia1)*OKpars(1,ia2)
           nano_iav(CURR_STR)%struk(i)%DebyeWallerB = half*(DWpars(1,ia1)+DWpars(1,ia2))
         enddo
       enddo
     enddo
   ENDIF
   

!__________Prep done!

  i = CURR_STR

  IF (refresh_Umat) THEN
    call U_FILLER(Curr_Str=Curr_Str,Curr_Set=Curr_Set)
  ENDIF

  tolvvv = s4eps_DP*maxval(V_mat(1:nano_iav(CURR_STR)%dimstruk))

  IF (ILAMBDA_W(CURR_SET)>1) then
    do i=Ncut0,Ncutoff
      if (V_mat(i) < tolvvv) cycle ! -- control
      do iwl=1,ILAMBDA_W(CURR_SET)
        xli = one/(lambdas_W(iwl,CURR_SET)**2)
        do ipair=1,NPAIR_AT_W(Curr_Str)
          CALPHA_W(CURR_SET,CURR_STR)%vdata(:) = CALPHA_W(CURR_SET,CURR_STR)%vdata(:) &
                                               + xli * CALPHA_W(CURR_SET,CURR_STR)%ascaf(:,ipair,iwl) & 
                        * (DebyeWaller(CURR_SET,NDATA_W(CURR_SET),iwl,DWalMT(CURR_STR)%MTvalue(ipair,i)) &
                        * WS_PHA_SET(CURR_STR,CURR_SET)%Umat(:,i,iwl,ipair) + WS_PHA_SET(CURR_STR,CURR_SET)%cotes(i,ipair)) &
                        * V_mat(i) * nano_iav(CURR_STR)%struk(i)%occupair(ipair)
        enddo
        do iat=1,NSP_AT_W(Curr_Str)
          CALPHA_W(CURR_SET,CURR_STR)%vdata(:) = CALPHA_W(CURR_SET,CURR_STR)%vdata(:) &
                                               + (xli * CALPHA_W(CURR_SET,CURR_STR)%incoh(:,iat,iwl)) & 
                                               * V_mat(i) * nano_iav(CURR_STR)%struk(i)%xnat(iat) &
                                                          * nano_iav(CURR_STR)%struk(i)%occusite(iat)
        enddo
      enddo
    enddo
  ELSE
    do i=Ncut0,Ncutoff
      if (V_mat(i) < tolvvv) cycle ! -- control
      do ipair=1,NPAIR_AT_W(Curr_Str)
        CALPHA_W(CURR_SET,CURR_STR)%vdata(:) = CALPHA_W(CURR_SET,CURR_STR)%vdata(:) &
                                               + CALPHA_W(CURR_SET,CURR_STR)%ascaf(:,ipair,1) & 
                   * (DebyeWaller(CURR_SET,NDATA_W(CURR_SET),1,DWalMT(CURR_STR)%MTvalue(ipair,i)) &
                   * WS_PHA_SET(CURR_STR,CURR_SET)%Umat(:,i,1,ipair) + WS_PHA_SET(CURR_STR,CURR_SET)%cotes(i,ipair)) &
                   * V_mat(i) * nano_iav(CURR_STR)%struk(i)%occupair(ipair)
!                print*,'debug: ',i,ipair,minval(CALPHA_W(CURR_SET,CURR_STR)%ascaf(:,ipair,1)),&
!                                         maxval(CALPHA_W(CURR_SET,CURR_STR)%ascaf(:,ipair,1)),&
!                                         sum(CALPHA_W(CURR_SET,CURR_STR)%ascaf(:,ipair,1))/NDATA_W(CURR_SET)
      enddo
      
      do iat=1,NSP_AT_W(Curr_Str)
        CALPHA_W(CURR_SET,CURR_STR)%vdata(:) = CALPHA_W(CURR_SET,CURR_STR)%vdata(:) &
                                             + CALPHA_W(CURR_SET,CURR_STR)%incoh(:,iat,1) & 
                                             * V_mat(i) * nano_iav(CURR_STR)%struk(i)%xnat(iat) &
                                                        * nano_iav(CURR_STR)%struk(i)%occusite(iat)
      enddo
    enddo
    
  ENDIF
  if (ANY(abs(BACKGROUND(CURR_SET)%LORCORR2(:)-one)>sceps_DP)) &
      CALPHA_W(CURR_SET,CURR_STR)%vdata(:) = CALPHA_W(CURR_SET,CURR_STR)%vdata(:) &
                                           * BACKGROUND(CURR_SET)%LORCORR2(:)  ! this is 1/cos(theta) for deltaq=const, 
                                                                               !         1 for delta2theta=const
  if (ANY(ISNAN(CALPHA_W(CURR_SET,CURR_STR)%vdata(:)))) then
    print*,'NaN in CALC',CURR_SET,CURR_STR
    where( ISNAN(CALPHA_W(CURR_SET,CURR_STR)%vdata(:)) ) CALPHA_W(CURR_SET,CURR_STR)%vdata(:)=zero
  endif


!! micr ISO 4eps tan(theta)
!  dA_A=PARAPHAS(CURR_STR)%nano_parcurrE(9) 
   if (MICRO_FLAG(CURR_STR)==1) then 
     if (ALLOCATED(micr_cal)) deallocate(micr_cal)
     allocate(micr_cal(1:NDATA_W(CURR_SET)))
   micr_cal=zero
  dxxx= Obs_data_W(CURR_SET)%t2data(2)-Obs_data_W(CURR_SET)%t2data(1)
  do i =1, NDATA_W(CURR_SET)
      gau_fwhm = four*dA_A*tan(degrees_to_radians/two*Obs_data_W(CURR_SET)%t2data(i))/degrees_to_radians 
      sig = gau_fwhm/(two*sqrt(two*log(two))) 
      nk = 1/sqrt(two*pi)/sig        
    do j =1, NDATA_W(CURR_SET)
     if (abs(Obs_data_W(CURR_SET)%t2data(i)-Obs_data_W(CURR_SET)%t2data(j))<=sig*three) then 
		micr_cal(i) = micr_cal(i) + CALPHA_W(CURR_SET,CURR_STR)%vdata(j)*nk*exp(-((Obs_data_W(CURR_SET)%t2data(i)- &
       Obs_data_W(CURR_SET)%t2data(j)))**2/(two*sig**2))*dxxx
          else  
        cycle
      endif
     enddo
   enddo
    do i =1, NDATA_W(CURR_SET)	
      gau_fwhm = four*dA_A*tan(degrees_to_radians/two*Obs_data_W(CURR_SET)%t2data(i))/degrees_to_radians 
      sig = gau_fwhm/(two*sqrt(two*log(two)))  
     if ((Obs_data_W(CURR_SET)%t2data(i)-(sig*one))>Obs_data_W(CURR_SET)%t2data(1) .and. &
       (Obs_data_W(CURR_SET)%t2data(i)+(sig*one))<Obs_data_W(CURR_SET)%t2data(NDATA_W(CURR_SET))) then 
        CALPHA_W(CURR_SET,CURR_STR)%vdata(i)=micr_cal(i)
      else
      cycle
     endif   
    enddo
endif  
!!

    DISTRU(:,CURR_STR,:)=zero
    do n=Ncut0,Ncutoff
     if (V_mat(n) < s4eps_DP*maxval(V_mat(1:nano_iav(CURR_STR)%dimstruk))) cycle
       DISTRU(n,CURR_STR,1)=V_mat(n)
       DISTRU(n,CURR_STR,2)=A_mat(n)
       DISTRU(n,CURR_STR,3)=SUM(nano_iav(CURR_STR)%struk(n)%DebyeWallerB(:))/SIZE(nano_iav(CURR_STR)%struk(n)%DebyeWallerB(:))
    enddo
    ncut_0_off(1,CURR_STR)=Ncut0
    ncut_0_off(2,CURR_STR)=Ncutoff

END SUBROUTINE NANO_CALC_DBX
!_______________________________________________________________________________
!_______________________________________________________________________________
SUBROUTINE U_FILLER(Curr_Str,Curr_Set)
 ! He calculaates the separate patterns saving them in Umat
   INTEGER(I4B),INTENT(IN)            :: Curr_Str,Curr_Set 
   INTEGER(I4B)    :: klin(2)
   REAL(DP)      :: vv, dthr,  wxni, wthr, duePiqas, uuu2, uuu, sinc_uuu, cos_uuu, uacc
   INTEGER(I4B)  :: ipair, k,iwl,n,iaaa, iset

   ! WS_PHA_SET(CURR_STR,CURR_SET)%Umat(:,:,:,:) = zero
    if (.not. isalloc_dsd1) call ALLOC_DSD1(NSET_W,NDATA_W(1:NSET_W))
!________ Constant terms
    dthr = s4eps_DP*maxval(V_mat(1:nano_iav(Curr_Str)%dimstruk))
    do n=1,nano_iav(Curr_Str)%dimstruk
      WS_PHA_SET(CURR_STR,CURR_SET)%cotes(n,1:NPAIR_AT_W(Curr_Str)) = &
           nano_iav(Curr_Str)%struk(n)%termcon_allP(1:NPAIR_AT_W(Curr_Str))
      if (V_mat(n) < dthr) WS_PHA_SET(CURR_STR,CURR_SET)%Umat(:,n,:,:) = zero
    enddo
    
!________ calculate big matrix U_mat
    wav:do iwl=1,ILAMBDA_W(CURR_SET)
      sizex:do n=1,nano_iav(Curr_Str)%dimstruk
        if (V_mat(n) < dthr) cycle sizex
        vv = A_mat(n)                         ! vv is s, the strain
!________ calculate correction function C_G(q)

        call FILL_DSD1(i=CURR_SET, qapi2=CALPHA_W(CURR_SET,CURR_STR)%twoPi_q_a(1:NDATA_W(CURR_SET),iwl), &
                       dilat=vv, gsig=nano_iav(Curr_Str)%struk(n)%widg )
!_________ Debye function evaluation
        atp:do ipair=1,NPAIR_AT_W(Curr_Str)
        
           WS_PHA_SET(CURR_STR,CURR_SET)%Umat(1:NDATA_W(CURR_SET),n,iwl,ipair) = &
                 NSCATT(i=Curr_Set, &
                 qapi2=CALPHA_W(CURR_SET,CURR_STR)%twoPi_q_a(1:NDATA_W(CURR_SET),iwl), &
                 dilat=vv, gsig=nano_iav(Curr_Str)%struk(n)%widg, delta=nano_iav(Curr_Str)%struk(n)%Delta, &
                 wsam=nano_iav(Curr_Str)%struk(n)%pseudomult(1:nano_iav(Curr_Str)%struk(n)%esse,ipair))
 
        enddo atp
      enddo sizex
    enddo wav
    
end SUBROUTINE U_FILLER
!_______________________________________________________________________________
!_______________________________________________________________________________
SUBROUTINE U_FILLER_old(Curr_Str,Curr_Set)
 ! He calculaates the separate patterns saving them in Umat
   INTEGER(I4B),INTENT(IN)            :: Curr_Str,Curr_Set      
   real(DP)               :: sceps_SR2 
   REAL(DP),allocatable   :: C_G_q(:)
   INTEGER(I4B)    :: klin(2)
   REAL(DP)      :: vv, wxni, wthr, duePiqas, uuu2, uuu, sinc_uuu, cos_uuu, uacc
   INTEGER(I4B)  :: ipair, k,iwl,n,iaaa

    sceps_SR2 = sceps_DP*sr2

    allocate(C_G_q(NDATA_W(CURR_SET)))
    C_G_q = one

    WS_PHA_SET(CURR_STR,CURR_SET)%Umat(:,:,:,:) = zero
    
!________ calculate big matrix U_mat
    wav:do iwl=1,ILAMBDA_W(CURR_SET)
      sizex:do n=1,nano_iav(Curr_Str)%dimstruk
         if (V_mat(n) < s4eps_DP*maxval(V_mat(1:nano_iav(Curr_Str)%dimstruk))) cycle
         vv = A_mat(n)                         ! vv is s, the strain
!________ calculate correction function C_G(q)

        wxni = nano_iav(Curr_Str)%struk(n)%widg
        wthr = sceps_SR2/real(wxni,DP)
        CORRf:do k=1,NDATA_W(CURR_SET)
          duePiqas = CALPHA_W(CURR_SET,CURR_STR)%twoPi_q_a(k,iwl) * vv  ! 2*Pi*q*a * s
          IF (duePiqas < wthr) CYCLE CORRf
          uuu2 = duePiqas * wxni
          C_G_q(k) = EXP( half * (uuu2**2) )
        enddo CORRf

!_________ Debye function evaluation

        skatt:do k=1,NDATA_W(CURR_SET)

          duePiqas = vv * CALPHA_W(CURR_SET,CURR_STR)%twoPi_q_a(k,iwl)  ! 2*Pi*q*a * s

          uuu = duePiqas*nano_iav(Curr_Str)%struk(n)%Delta
          sinc_uuu = sin(uuu)/uuu
          cos_uuu = cos(uuu)
          atp:do ipair=1,NPAIR_AT_W(Curr_Str)
!_________  If it is a pair of equal atoms, we must know it and add the constant term. Otherwise not.
            uacc = sinc_uuu * C_G_q(k) &
                   * U_cheb(dum=scal_arg, &
                            c = nano_iav(Curr_Str)%struk(n)%pseudomult(1:nano_iav(Curr_Str)%struk(n)%esse,ipair), &
                            x = cos_uuu)
            WS_PHA_SET(CURR_STR,CURR_SET)%Umat(k,n,iwl,ipair) = uacc
            WS_PHA_SET(CURR_STR,CURR_SET)%cotes(n,ipair) = nano_iav(Curr_Str)%struk(n)%termcon_allP(ipair)
 
          enddo atp
        enddo skatt
      enddo sizex
    enddo wav
    deallocate(C_G_q)
    
end SUBROUTINE U_FILLER_old
!_______________________________________________________________________________
function CK_EQP(j,N)
implicit none
integer(I4B),intent(IN) :: j,N
integer(I4B) :: CK_EQP,ko
real(DP) :: x

CK_EQP = 0
x = REAL(4*N*(N+1)+9-8*j,DP)
x = half*( REAL(2*N+3,DP)-sqrt(x) )
ko = nint(x)
IF (abs(x-REAL(ko,DP))<sceps_DP) CK_EQP = ko

end function CK_EQP
!_______________________________________________________________________________

 SUBROUTINE CONCK_DB2(npin,pin,Curr_Str,Curr_Set,iconsist)

   implicit real(DP)(a-h,o-z),integer(I4B)(i-n)

   INTEGER(I4B),INTENT(IN)            :: Curr_Str,Curr_Set      
   INTEGER(I4B),OPTIONAL,INTENT(IN)   :: npin           ! npin = n. par. da raffinare
   REAL(CP),OPTIONAL,INTENT(IN)       :: pin(:)
   INTEGER(I4B),intent(out)           :: iconsist

   REAL(DP)               :: alp4(4)
   REAL(DP),allocatable   :: C_G_q(:)
   REAL(DP)               :: ln_n00,ln_wid

   illogik = .false.
   iconsist=1
!_____ Definire Curr_Str = I of current   

   IF (PRESENT(npin) .and.(.not.PRESENT(pin))) STOP 'CONCK_DB2: error 1'
   IF (PRESENT(pin) .and.(.not.PRESENT(npin))) STOP 'CONCK_DB2: error 2'
   IF (PRESENT(pin) ) THEN
       IF (size(pin)/=npin) STOP 'CONCK_DB2: error 3'
   ENDIF
   IF (PRESENT(npin)) THEN
     IF (npin /= SUM(PARAPHAS(Curr_Str)%nano_doit)) &
       STOP 'Error[CONCK_DB2] : npin /= SUM(PARAPHAS(Curr_Str)%nano_doit) !!? '
   ENDIF

   PARAPHAS(CURR_STR)%nano_parcurr = PARAPHAS(CURR_STR)%nano_par0
   IF (PRESENT(npin)) THEN
     IF (npin>0) THEN
       PARAPHAS(CURR_STR)%nano_parcurr(PARAPHAS(CURR_STR)%nano_mask(1:npin)) = pin(1:npin)
     ENDIF
   ENDIF
   if (PARAPHAS(CURR_STR)%str_cod /= 0) return
   
   avLN  = PARAPHAS(CURR_STR)%nano_parcurr(1)
   sdLN  = PARAPHAS(CURR_STR)%nano_parcurr(2)
   Omega = PARAPHAS(CURR_STR)%nano_parcurr(6)
   IF (PARAPHAS(CURR_STR)%str_cod == 0) THEN
       y1    = PARAPHAS(CURR_STR)%nano_parcurr(7)
       y2    = PARAPHAS(CURR_STR)%nano_parcurr(8)
       Delta = PARAPHAS(CURR_STR)%nano_parcurr(9)
       where(abs(ckseq) < eps_DP) ckseq = zero
       ckseq = (/Omega-Delta-y1,y1-y2,y2-Omega/)
       illogik = .not.(ALL(ckseq>=zero).or.ALL(ckseq<=zero))
   ELSE IF (PARAPHAS(CURR_STR)%str_cod == 1) THEN
       y1    = PARAPHAS(CURR_STR)%nano_parcurr(6)
       y2    = PARAPHAS(CURR_STR)%nano_parcurr(6)
       Delta = zero
       
   ENDIF
   Btherm0 = PARAPHAS(CURR_STR)%nano_parcurr(10)
   Btherm1 = PARAPHAS(CURR_STR)%nano_parcurr(11)

   
   if (illogik) then
     iconsist = 0
     return
   endif

 END SUBROUTINE CONCK_DB2
!_______________________________________________________________________________
FUNCTION CALCSCATHE3(CURR_SET,Np,Nw,B)

integer(I4B),intent(IN)               :: CURR_SET,Np,Nw
real(CP),DIMENSION(:),intent(IN)      :: B
real(CP),DIMENSION(Np,SIZE(B),Nw)     :: CALCSCATHE3
integer(I4B)                          :: i

DO i=1,Nw
!___ Debye-Waller ONLY
  CALCSCATHE3(:,:,i) = exp(OUTERP(CALTOT_W(CURR_SET)%qshdata(:,i),-B))
ENDDO

end FUNCTION CALCSCATHE3
!_______________________________________________________________________________
! ACRF may 2014
!FUNCTION CALCSCATHE4(CURR_SET,CURR_STR,Np,Nw,B,ipair)
!implicit none
!integer(I4B),intent(IN)               :: CURR_SET,curr_str,Np,Nw,ipair
!real(CP),DIMENSION(:),intent(IN)      :: B
!real(CP),DIMENSION(Np,SIZE(B),Nw)     :: CALCSCATHE4
!integer(I4B)                          :: i
!
!DO i=1,Nw
!!___ Debye-Waller ONLY
!  CALCSCATHE4(:,:,i) = exp(OUTERP(CALTOT_W(CURR_SET)%qshdata(:,i), &
!                                  -B*ALL_PHA_INFO_W(curr_str)%pha_PAIR_averBTH(ipair) ))
!ENDDO
!
!end FUNCTION CALCSCATHE4
!_______________________________________________________________________________
FUNCTION DebyeWaller(CURR_SET,Np,iwl,Bpair_half)
implicit none
integer(I4B),intent(IN)     :: CURR_SET,Np,iwl
real(CP),intent(IN)         :: Bpair_half
real(CP),DIMENSION(Np)  :: DebyeWaller

!___ Debye-Waller ONLY
  DebyeWaller(:) = exp(-Bpair_half*CALTOT_W(CURR_SET)%qshdata(:,iwl))

end FUNCTION DebyeWaller
!_______________________________________________________________________________
 SUBROUTINE SETUP_CYCLE_START
   IMPLICIT NONE
   INTEGER(I4B)  :: iCURR,numcase
   REAL(DP)      :: ln_n00,ln_wid,avLN,sdLN,Omega,Xi,xn0,xw,alp4(4),Etol,Delta,y1,y2


   PARAGLOB%nano_par0 = PARAGLOB%nano_par0E
   PARAGLOB%nano_parcurrE = PARAGLOB%nano_par0E
   PARAGLOB%nano_parcurr = PARAGLOB%nano_par0

   do iCURR = 1,NSTR_W
     IF (DB_INDEX_W(iCURR) == 1) cycle
     PARAPHAS(iCURR)%nano_par0 = PARAPHAS(iCURR)%nano_par0E
     PARAPHAS(iCURR)%nano_parcurrE = PARAPHAS(iCURR)%nano_par0E
!____________ evaluate internal
     avLN = PARAPHAS(iCURR)%nano_par0E(1)
     sdLN = PARAPHAS(iCURR)%nano_par0E(2)

     call COLON_AS_CL(avLN=avLN,sdLN=sdLN,ln_n00=ln_n00,ln_wid=ln_wid,n1=PARAPHAS(iCURR)%n1,n2=PARAPHAS(iCURR)%n2, &
     
                 n1in=PARAPHAS(iCURR)%n1)

   IF (PARAPHAS(iCURR)%str_cod == 0)THEN

     Omega = PARAPHAS(iCURR)%nano_par0E(6)
     Xi    = PARAPHAS(iCURR)%nano_par0E(7)
     xn0   = PARAPHAS(iCURR)%nano_par0E(8)
     xw    = PARAPHAS(iCURR)%nano_par0E(9)
     call COSTR_EX2IN(PARAPHAS(iCURR)%n1,PARAPHAS(iCURR)%n2,Omega,Xi,xn0,xw,y1,y2,Delta,alp4,numcase,Etol, &
           straflag=PARAPHAS(iCURR)%str_cod)
     PARAPHAS(iCURR)%nano_par0(6:9) = (/Omega, y1, y2, Delta/)
     PARAPHAS(iCURR)%nano_parcurr = PARAPHAS(iCURR)%nano_par0
   ENDIF
   enddo
   kount_calc = 0
 END SUBROUTINE SETUP_CYCLE_START

 SUBROUTINE SETUP_CYCLE_END
   IMPLICIT NONE
   INTEGER(I4B)  :: iCURR,numcase
   REAL(DP)      :: ln_n00,ln_wid,avLN,sdLN,Omega,Xi,xn0,xw,alp4(4),Etol,Delta,y1,y2

   PARAGLOB%nano_par0E = PARAGLOB%nano_par0

   do iCURR = 1,NSTR_W
   
     PARAPHAS(iCURR)%nano_par0E = PARAPHAS(iCURR)%nano_par0
!_________ first, update E param.s
     avLN = PARAPHAS(iCURR)%nano_par0(1)
     sdLN = PARAPHAS(iCURR)%nano_par0(2)

     Omega = PARAPHAS(iCURR)%nano_par0(6)
     y1    = PARAPHAS(iCURR)%nano_par0(7)
     y2    = PARAPHAS(iCURR)%nano_par0(8)
     Delta = PARAPHAS(iCURR)%nano_par0(9)
     call COSTR_IN2EX(PARAPHAS(iCURR)%n1,PARAPHAS(iCURR)%n2,Omega,Xi,xn0,xw,y1,y2,Delta,numcase,Etol, &
           straflag=PARAPHAS(iCURR)%str_cod)
     PARAPHAS(iCURR)%nano_par0E(6:9) = (/Omega, Xi, xn0, xw/)
!_________ E are OK now, setup I par.s with new n1,n2
     IF (DB_INDEX_W(iCURR) > 1) call COLON_AS_CL(avLN,sdLN,ln_n00,ln_wid,PARAPHAS(iCURR)%n1,PARAPHAS(iCURR)%n2)
     Omega = PARAPHAS(iCURR)%nano_par0E(6)
     Xi    = PARAPHAS(iCURR)%nano_par0E(7)
     xn0   = PARAPHAS(iCURR)%nano_par0E(8)
     xw    = PARAPHAS(iCURR)%nano_par0E(9)
     call COSTR_EX2IN(PARAPHAS(iCURR)%n1,PARAPHAS(iCURR)%n2,Omega,Xi,xn0,xw,y1,y2,Delta,alp4,numcase,Etol, &
           straflag=PARAPHAS(iCURR)%str_cod)
     PARAPHAS(iCURR)%nano_par0(6:9) = (/Omega, y1, y2, Delta/)
   enddo
 END SUBROUTINE SETUP_CYCLE_END


END MODULE CALCFUN_DB2
!_______________________________________________________________________________

