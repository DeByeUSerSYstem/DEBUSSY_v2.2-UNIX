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
Module FAST_PLACE
 use nano_deftyp
private
public :: placer

contains

subroutine PLACER(x,vec,nplace,iseq,toldo)
!____ VEC(1:n) sorted in ascending order
!     FIND nplace : x>=vec(nplace) .and. x<vec(nplace+1)
!     IF x<vec(1) nplace=0; IF x>=vec(n) nplace=n
!     IF x=vec(nplace) iseq=1; IF x>vec(nplace) iseq=0

  IMPLICIT NONE
  REAL(CP),intent(IN)              :: x,toldo
  REAL(CP),intent(IN),DIMENSION(:) :: vec
  INTEGER(I4B),intent(OUT)         :: nplace,iseq
  REAL(CP)                  :: dex
  INTEGER(I4B)              :: n,i0,i1,ic

  n    = size(vec)
  iseq = 0

  dex = vec(1)-x
  IF (dex >= toldo) THEN
    nplace=0
    RETURN
  ENDIF
  IF (dex > -toldo) THEN
    iseq=1
    nplace=1
    RETURN
  ENDIF
  dex = x-vec(n)
  IF (dex >= toldo) THEN
    nplace=n
    RETURN
  ENDIF
  IF (dex > -toldo) THEN
    iseq=1
    nplace=n
    RETURN
  ENDIF
!_____ now, x > vec(1) .and. x < vec(n)
  IF (n==2) THEN
    nplace=1
    RETURN
  ENDIF

  i0=1
  i1=n
  do
!_____ now, x > vec(i0) .and. x < vec(i1)
    ic=(i0+i1)/2
    IF (ic==i0) EXIT
    dex = x-vec(ic)
    IF (dex >= toldo) THEN
      i0=ic
    ELSE IF (dex <= -toldo) THEN
      i1=ic
    ELSE
      iseq = 1
      EXIT
    ENDIF
  enddo
  nplace = ic

end subroutine PLACER

END MODULE FAST_PLACE
!_____________________________________________________________________________________________________
module pre_linalto
use nano_deftyp
!________ from .inc
use F95_LAPACK, only : LA_SYEVR, LA_GESVD!,LA_GESDD

private

public :: SING_VAL_DECOMP, DIAGONALY, outerp, pythag


contains

!***********************************************
 SUBROUTINE SING_VAL_DECOMP(a0,U,W,V)
! Decomposes a0(m,n) into U(m,n)*DIAG(W(n))*V^T(n,n)
! Columns of U are orthonormal;
! Columns and rows of V are orthonormal.

  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)  :: a0
  REAL(DP), DIMENSION(:,:), INTENT(OUT) :: U
  REAL(DP), DIMENSION(:), INTENT(OUT)   :: W
  REAL(DP), DIMENSION(:,:), INTENT(OUT) :: V
! From here to Alternatively (no LAPACK)
  REAL(DP), DIMENSION(SIZE(a0,1),SIZE(a0,2))    :: a
  INTEGER(I4B)  :: m,n,info
  
  m=size(a0,1)
  n=size(a0,2)

  IF (m /= size(U,1) .or. n /= size(U,2) .or. n /= size(V,1) .or. n /= size(V,2) .or. n /= size(W)) &
    STOP 'SING_VAL_DECOMP: input dimensions wrong'
  if (n==0) then
    return
  endif
  if (n==1) then
    W(1) = sqrt(sum(a0*a0))
    U=a0/W(1)
    V=one
    return
  endif
  
  a=a0
  call LA_GESVD(A=a, S=W, U=U, VT=v, INFO=info)
  !,WW=ww)
  if (info>0) print*,'LA_GESVD error - INFO = ',info
  v = transpose(v)
!--- Alternatively (no LAPACK)
!  call SING_VAL_DECOMP_intern(a0,U,W,V)

end SUBROUTINE SING_VAL_DECOMP
!***********************************************
 SUBROUTINE DIAGONALY(a_in, d, z, lpri)
!*_______ DIAGONALIZE A REAL SYMMETRIC MATRIX
!*_______ INPUT   : matrix a_in(:,:)
!*_______ INPUT   : integer lpri --> print level (OPTIONAL)
!*_______ OUTPUT  : vector d(:)    eigenvalues of a_in
!*_______ OUTPUT  : matrix z(:,:)  columns(eigenvecors of a_in)

  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)              :: a_in
  REAL(DP), DIMENSION(:),   INTENT(OUT)             :: d
  REAL(DP), DIMENSION(:,:), OPTIONAL, INTENT(OUT)   :: z
  INTEGER,                  OPTIONAL, INTENT(IN)    :: lpri
! From here to Alternatively (no LAPACK)
  REAL(DP), DIMENSION(SIZE(a_in,1),SIZE(a_in,2))    :: a
  INTEGER(I4B)  :: lpr, info, n, Nfound
  lpr = 0
  IF (present(lpri)) lpr = lpri
  
  n = size(a_in,1)
  IF (n/=size(a_in,2) .or. n/=size(d)) STOP 'DIAGONALY_extern: A_IN , D ill-dimensioned'
  a = a_in
  a = a_in
  IF (present(z)) THEN
    IF (n/=size(z,1) .or. n/=size(z,2)) STOP 'DIAGONALY_extern: Z ill-dimensioned'
  ENDIF
  call LA_SYEVR(A=a, W=d, JOBZ='V', M=Nfound, INFO=info)
  if (lpr==1) then
    print*,'Found ',Nfound,' eigenvalues of ',n,' : status ',info
  endif
  if (PRESENT(z)) then
    z=a
  endif
!--- Alternatively (no LAPACK)
! if (PRESENT(z).and.PRESENT(lpri)) then
!   call DIAGONALY_intern(a_in=a_in, d=d, z=z, lpri=lpri)
! else if ((.not.PRESENT(z)).and.PRESENT(lpri)) then
!   call DIAGONALY_intern(a_in=a_in, d=d, lpri=lpri)
! else if (PRESENT(z).and.(.not.PRESENT(lpri))) then
!   call DIAGONALY_intern(a_in=a_in, d=d, z=z)
! else if ((.not.PRESENT(z)).and.(.not.PRESENT(lpri))) then
!   call DIAGONALY_intern(a_in=a_in, d=d)
! endif
  
end SUBROUTINE DIAGONALY
!***********************************************
 FUNCTION outerp(a1,a2)
  IMPLICIT NONE
  REAL(DP), DIMENSION(:), INTENT(IN)    :: a1,a2
  REAL(DP), DIMENSION(size(a1),size(a2)):: outerp
  INTEGER(I4B)                          :: i

  do i=1,size(a1,1)
    outerp(i,:) = a1(i)*a2(:)
  enddo
 END FUNCTION outerp
!***********************************************
!***********************************************
 FUNCTION pythag(a,b)
  IMPLICIT NONE
  REAL(DP), INTENT(IN) :: a,b
  REAL(DP)             :: pythag
  REAL(DP)             :: absa,absb,rat

  absa=abs(a)
  absb=abs(b)
  if (absa > absb) then
    rat = absb/absa
    pythag=absa*sqrt(one+rat*rat)
  else
    if (absb < eps_DP) then
      pythag=MAX(zero,absa,absb)
    else
      rat = absa/absb
      pythag=absb*sqrt(one+rat*rat)
    end if
  end if
 END FUNCTION pythag
!***********************************************
 SUBROUTINE DIAGONALY_intern(a_in, d, z, lpri)
!*_______ DIAGONALIZE A REAL SYMMETRIC MATRIX
!*_______ INPUT   : matrix a_in(:,:)
!*_______ INPUT   : integer lpri --> print level (OPTIONAL)
!*_______ OUTPUT  : vector d(:)    eigenvalues of a_in
!*_______ OUTPUT  : matrix z(:,:)  columns(eigenvecors of a_in)

  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)              :: a_in
  REAL(DP), DIMENSION(:),   INTENT(OUT)             :: d
  REAL(DP), DIMENSION(:,:), OPTIONAL, INTENT(OUT)   :: z
  INTEGER,                  OPTIONAL, INTENT(IN)    :: lpri

  REAL(DP), DIMENSION(SIZE(a_in,1),SIZE(a_in,2))    :: a
  REAL(DP), DIMENSION(SIZE(a_in,1))                 :: e,gg,ff

  INTEGER(I4B)  :: i,j,l,m,n,lpr, iter
  REAL(DP)      :: f,g,h,hh,scale, b,c,dd,p,r,s

  lpr = 0
  IF (present(lpri)) lpr = lpri

  n=size(a_in,1)
  IF (n/=size(a_in,2) .or. n/=size(d)) STOP 'tridy: dimensions!'
  a = a_in
  IF (present(z)) THEN
    IF (n/=size(z,1) .or. n/=size(z,2)) STOP 'tridy: dimensions Z!'
  ENDIF

!!!!!!!!!!!! PART 1 :: DIRECT TRIDIAGONALIZATION

  do i=n,2,-1
    l=i-1
    h=zero
    if (l > 1) then
      scale=sum(abs(a(i,1:l)))
      if (abs(scale) < eps_DP) then
        e(i)=a(i,l)
      else
        a(i,1:l)=a(i,1:l)/scale
        h=sum(a(i,1:l)**2)
        f=a(i,l)
        g=-sign(sqrt(h),f)
        e(i)=scale*g
        h=h-f*g
        a(i,l)=f-g
        if (present(z)) a(1:l,i)=a(i,1:l)/h
        do j=1,l
          e(j)=(dot_product(a(j,1:j),a(i,1:j)) &
          +dot_product(a(j+1:l,j),a(i,j+1:l)))/h
        end do
        f=dot_product(e(1:l),a(i,1:l))
        hh=f/(h+h)
        e(1:l)=e(1:l)-hh*a(i,1:l)
        do j=1,l
          a(j,1:j)=a(j,1:j)-a(i,j)*e(1:j)-e(j)*a(i,1:j)
        end do
      end if
    else
      e(i)=a(i,l)
    end if
    d(i)=h
  end do
  if (present(z)) d(1)=zero
  e(1)=zero
  do i=1,n
    if (present(z)) then
      l=i-1
      if (abs(d(i)) > eps_DP) then
        gg(1:l)=matmul(a(i,1:l),a(1:l,1:l))
        a(1:l,1:l)=a(1:l,1:l)-outerp(a(1:l,i),gg(1:l))
      end if
      d(i)=a(i,i)
      a(i,i)=one
      a(i,1:l)=zero
      a(1:l,i)=zero
    else
      d(i)=a(i,i)
    end if
  end do

!!!!!!!! PART 2 : ITERATIVE DIAGONALIZATION

  IF (present(z)) z = a

  e(:)=eoshift(e(:),1)
  do l=1,n
    iter=0
    iterate: do
      do m=l,n-1
        dd=abs(d(m))+abs(d(m+1))
        if (abs(e(m)) < eps_DP*ABS(dd)) exit
      end do
      if (m == l) exit iterate
      if (iter == 3000) STOP 'too many iterations in tqli'
      iter=iter+1
      g = (d(l+1) - d(l)) * 0.5_DP / e(l)
      r=pythag(g,one)
      g=d(m)-d(l)+e(l)/(g+sign(r,g))
      s=one
      c=one
      p=zero
      do i=m-1,l,-1
        f=s*e(i)
        b=c*e(i)
        r=pythag(f,g)
        e(i+1)=r
        if (abs(r) < eps_DP) then
          d(i+1)=d(i+1)-p
          e(m)=zero
          cycle iterate
        end if
        s=f/r
        c=g/r
        g=d(i+1)-p
        r=(d(i)-g)*s+two*c*b
        p=s*r
        d(i+1)=g+p
        g=c*r-b
        if (present(z)) then
          ff(1:n)=z(1:n,i+1)
          z(1:n,i+1)=s*z(1:n,i)+c*ff(1:n)
          z(1:n,i)=c*z(1:n,i)-s*ff(1:n)
        end if
      end do
      d(l)=d(l)-p
      e(l)=g
      e(m)=zero
    end do iterate
    IF (lpr > 0) PRINT'(i5,a,i4,a)',l,'-th eigenvalue found in',iter,' iterations.'
  end do

 END SUBROUTINE DIAGONALY_intern
!***********************************************
 SUBROUTINE SING_VAL_DECOMP_intern(a0,U,W,V)
! Decomposes a0(m,n) into U(m,n)*DIAG(W(n))*V^T(n,n)
! Columns of U are orthonormal;
! Columns and rows of V are orthonormal.

  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)  :: a0
  REAL(DP), DIMENSION(:,:), INTENT(OUT) :: U
  REAL(DP), DIMENSION(:), INTENT(OUT)   :: W
  REAL(DP), DIMENSION(:,:), INTENT(OUT) :: V

  INTEGER(I4B),parameter                :: itmax = 3000
  INTEGER(I4B)                          :: i,its,j,k,l,m,n,nm,ll
  INTEGER(I4B)                          :: KKK,Ndym,Ip1,Iflag
  REAL(DP)                              :: Unorm,c,f,g,h,s,sclfk,x,y,z
  REAL(DP), DIMENSION(size(U,1))        :: Vauxm
  REAL(DP), DIMENSION(size(U,2))        :: Wauxn,Vauxn


  m=size(a0,1)
  n=size(a0,2)

  IF (m /= size(U,1) .or. n /= size(U,2) .or. n /= size(V,1) .or. n /= size(V,2) .or. n /= size(W)) &
    STOP 'SING_VAL_DECOMP: dim'
  if (n==0) then
    return
  endif
  if (n==1) then
    W(1) = sqrt(sum(a0*a0))
    U=a0/W(1)
    V=one
    return
  endif

  U = a0

  g = zero
  sclfk = zero
  do i = 1,n
    l = i+1
    Wauxn(i) = sclfk*g
    g = zero
    sclfk = zero
    IF (i <=  m) then
      sclfk = sum(abs(U(i:m,i)))
      IF (ABS(sclfk) > eps_DP) then
        U(i:m,i) = U(i:m,i)/sclfk
        s = dot_product(U(i:m,i),U(i:m,i))
        f = U(i,i)
        g = -sign(sqrt(s),f)
        h = f*g-s
        U(i,i) = f-g
        Vauxn(l:n) = matmul(U(i:m,i),U(i:m,l:n))/h
        U(i:m,l:n) = U(i:m,l:n)+OUTERP(U(i:m,i),Vauxn(l:n))
        U(i:m,i) = sclfk*U(i:m,i)
      ENDIF
    ENDIF
    W(i) = sclfk*g
    g = zero
    sclfk = zero
    IF ((i <= m) .and. (i /= n)) then
      sclfk = sum(abs(U(i,l:n)))
      IF (ABS(sclfk) > eps_DP) then
        U(i,l:n) = U(i,l:n)/sclfk
        s = dot_product(U(i,l:n),U(i,l:n))
        f = U(i,l)
        g = -sign(sqrt(s),f)
        h = f*g-s
        U(i,l) = f-g
        Wauxn(l:n) = U(i,l:n)/h
        Vauxm(l:m) = matmul(U(l:m,l:n),U(i,l:n))
        U(l:m,l:n) = U(l:m,l:n)+OUTERP(Vauxm(l:m),Wauxn(l:n))
        U(i,l:n) = sclfk*U(i,l:n)
      ENDIF
    ENDIF
  enddo
  Unorm = maxval(abs(W)+abs(Wauxn))
  do i = n,1,-1
    IF (i < n) then
      IF (ABS(g) > eps_DP) then
        V(l:n,i) = (U(i,l:n)/U(i,l))/g
        Vauxn(l:n) = matmul(U(i,l:n),V(l:n,l:n))
        V(l:n,l:n) = V(l:n,l:n)+OUTERP(V(l:n,i),Vauxn(l:n))
      ENDIF
      V(i,l:n) = zero
      V(l:n,i) = zero
    ENDIF
    V(i,i) = one
    g = Wauxn(i)
    l = i
  enddo
  do i = min(m,n),1,-1
    l = i+1
    g = W(i)
    U(i,l:n) = zero
    IF (ABS(g) > eps_DP) then
      g = one/g
      Vauxn(l:n) = (matmul(U(l:m,i),U(l:m,l:n))/U(i,i))*g
      U(i:m,l:n) = U(i:m,l:n)+OUTERP(U(i:m,i),Vauxn(l:n))
      U(i:m,i) = U(i:m,i)*g
    else
      U(i:m,i) = zero
    ENDIF
    U(i,i) = U(i,i)+one
  enddo
  do k = n,1,-1
    do its = 1,itmax
      do l = k,1,-1
        ll=l
        nm = l-1
        IF (abs(Wauxn(l)) < abs(Unorm*eps_DP)) exit
        IF (nm>0) then
          IF (abs(W(nm)) < abs(Unorm*eps_DP)) then
            c = zero
            s = one
            do i = l,k
              f = s*Wauxn(i)
              Wauxn(i) = c*Wauxn(i)
              IF (abs(f) < abs(Unorm*eps_DP)) exit
              g = W(i)
              h = pythag(f,g)
              W(i) = h
              h = one/h
              c =  (g*h)
              s = -(f*h)
              Vauxm(1:m) = U(1:m,nm)
              U(1:m,nm) = U(1:m,nm)*c+U(1:m,i)*s
              U(1:m,i) = -Vauxm(1:m)*s+U(1:m,i)*c
            enddo
            exit
          ENDIF
        Endif
      enddo
      l=ll
      z = W(k)
      IF (l  == k) then
        IF (z < zero) then
          W(k) = -z
          V(1:n,k) = -V(1:n,k)
        ENDIF
        exit
      ENDIF
      IF (its > itmax) STOP 'SING_VAL_DECOMP: iter.'
IF (l<1) then
  print*,'k,its,l,n ',k,its,l,n
  stop 'SING_VAL_DECOMP: error l < 1'
endif
      x = W(l)
      nm = k-1
      y = W(nm)
      g = Wauxn(nm)
      h = Wauxn(k)
      f = ((y-z)*(y+z)+(g-h)*(g+h))/(two*h*y)
      g = pythag(f,one)
      f = ((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
      c = one
      s = one
      do j = l,nm
        i = j+1
        g = Wauxn(i)
        y = W(i)
        h = s*g
        g = c*g
        z = pythag(f,h)
        Wauxn(j) = z
        c = f/z
        s = h/z
        f =  (x*c)+(g*s)
        g = -(x*s)+(g*c)
        h = y*s
        y = y*c
        Vauxn(1:n) = V(1:n,j)
        V(1:n,j) = V(1:n,j)*c+V(1:n,i)*s
        V(1:n,i) = -Vauxn(1:n)*s+V(1:n,i)*c
        z = pythag(f,h)
        W(j) = z
        IF (ABS(z) > eps_DP) then
          z = one/z
          c = f*z
          s = h*z
        ENDIF
        f =  (c*g)+(s*y)
        x = -(s*g)+(c*y)
        Vauxm(1:m) = U(1:m,j)
        U(1:m,j) = U(1:m,j)*c+U(1:m,i)*s
        U(1:m,i) = -Vauxm(1:m)*s+U(1:m,i)*c
      enddo
      Wauxn(l) = zero
      Wauxn(k) = f
      W(k) = x
    enddo
  enddo
!______ SORT SVs in decreasing order

  KKK=n
  Iflag=1
  DO
    IF (Iflag == 0) exit
    Ndym=KKK
    Iflag=0
    DO I=1,Ndym-1
      Ip1=I+1
      IF (W(I)<W(Ip1)) THEN
        x      = W(I)
        W(I)   = W(Ip1)
        W(Ip1) = x
        Vauxm    = U(:,I)
        U(:,I)   = U(:,Ip1)
        U(:,Ip1) = Vauxm
        Vauxn    = V(:,I)
        V(:,I)   = V(:,Ip1)
        V(:,Ip1) = Vauxn
        KKK=I
        Iflag=1
      ENDIF
    ENDDO
  ENDDO


 END SUBROUTINE SING_VAL_DECOMP_intern
!***********************************************

end module pre_linalto
!_____________________________________________________________________________________________________
module LINALG_TOOLS
  use special_types
  use specfun_AC
  use Silicon_Darwin
  use FAST_PLACE
  use pre_linalto

contains


!********** ********** ********** ********** ********** ********** ********** **********  
 subroutine Brutal_sincTransf(Y,R,tt,F,isetin,xlamw)
implicit none
real(DP),dimension(:),intent(IN)  :: Y,R,tt
integer(I4B),optional,intent(IN) :: isetin
real(DP),intent(IN)              :: xlamw
real(DP),dimension(:),intent(OUT) :: F
real(DP),dimension(size(tt)) :: dtt,dq,q
integer(I4B) :: n,m,j,ks

ks=1
if (PRESENT(isetin)) then
  ks=isetin
endif
n=size(tt)
 if (verbose) print*,'********** ',n,size(Y),size(R)
dtt(1)=tt(2)-tt(1)
do j=2,n-1
  dtt(j)=(tt(j+1)-tt(j-1))*half
enddo
dtt(n)=tt(n)-tt(n-1)

q=two*SIN(tt*duet2r)/xlamw
dq=dtt*duet2r*two*COS(tt*duet2r)/xlamw

F=zero
do j=1,n
  F=F+Y(j)*q(j)*dq(j)*sin(pi2*q(j)*R)
enddo
F=F*eight*Pi

end subroutine Brutal_sincTransf
!***********************************************
 FUNCTION spectral_comp(vec,val)
  IMPLICIT NONE
  REAL(DP), DIMENSION(:), INTENT(IN)      :: vec
  REAL(DP),               INTENT(IN)      :: val
  REAL(DP), DIMENSION(size(vec),size(vec)):: spectral_comp
  INTEGER(I4B)                            :: i

  do i=1,size(vec)
    spectral_comp(i,:) = val*vec(i)*vec(:)
  enddo

 END FUNCTION spectral_comp
!***********************************************
 RECURSIVE SUBROUTINE FAKK(n,u)
   INTEGER,intent(IN)  :: n
   REAL(CP),intent(OUT):: u
   REAL(CP)            :: x

   IF (n<=1) THEN
      u=one
   ELSE IF (n==2) THEN
      u=two
   ELSE IF (n==3) THEN
      u=6._DP
   ELSE IF (n>3 .and. n<=12) THEN
      CALL FAKK(n-1,u)
      u = n*u
   ELSE IF (n>12) THEN
      x = LOG(REAL(n,DP))
      CALL FAKK(n-1,u)
      u = EXP(x+LOG(u))
   ENDIF

 end SUBROUTINE FAKK
!***********************************************
 function PSEUDO_INV(A,tol,posdef)
  implicit none
  real(CP),dimension(:,:),intent(IN)  :: A
  real(CP),intent(IN)                 :: tol
  integer(I4B),optional,intent(IN)    :: posdef

  real(CP),dimension(size(A,1),size(A,2))  :: PSEUDO_INV

  integer(I4B)                             :: i,n,j
  real(CP),dimension(size(A,1),size(A,2))  :: eivec,eivin
  real(CP),dimension(size(A,1))            :: eival
  real(CP)                                 :: eitol,eiinv,eimad,eimin

    n = size(A,1)
    IF (size(A,2) /= n) STOP 'pseudo_inv : non-square!'
    eivin = A
    eimad = MAX(one,MAXVAL(ABS([(A(i,i),i=1,n)]))) * tol
    do i=1,n
      eivin(i,i) = eivin(i,i)+eimad
    enddo

    call DIAGONALY(eivin, eival, eivec)

    eival = eival-eimad
    eitol = maxval(abs(eival))*tol
    eimin = minval(eival)
    IF (PRESENT(posdef)) THEN
      IF (posdef==2 .and. eimin < eitol) then
        eival=eival+eitol-eimin
      endif
    ENDIF

    PSEUDO_INV = zero
    recon:do i=1,n
      IF (abs(eival(i)) < eitol) CYCLE recon
      IF (PRESENT(posdef)) THEN
        IF (posdef==1 .and. eival(i) < eitol-eps_DP) CYCLE recon
      ENDIF
      eiinv = one/eival(i)
      PSEUDO_INV = PSEUDO_INV + SPECTRAL_COMP(eivec(:,i),eiinv)
    enddo recon

 end function PSEUDO_INV
!***********************************************
 function MP_PSEUDO_INV(A,tol)
  implicit none
  real(CP),dimension(:,:),intent(IN)  :: A
  real(CP),intent(IN)                 :: tol

  real(CP),dimension(size(A,2),size(A,1))  :: MP_PSEUDO_INV
  real(CP),dimension(size(A,1),size(A,2))  :: U
  real(CP),dimension(size(A,2),size(A,2))  :: V
  real(CP),dimension(size(A,2))            :: W

  integer(I4B)                             :: m,n,i,j,n0
  real(CP)                                 :: Wtol

    m = size(A,1)
    n = size(A,2)
    MP_PSEUDO_INV = zero
    call SING_VAL_DECOMP(a0=A,U=U,W=W,V=V)
    Wtol=maxval(abs(W))*tol
    WHERE(W<Wtol)
      W=zero
    ELSEWHERE
      W=one/W
    END WHERE
    n0=0
    do i=n,1,-1
      if (W(i)>eps_DP) then
        n0=i
        exit
      endif
    enddo
    if (n0==0) return
    recon:do i=1,n
      MP_PSEUDO_INV(i,:)=matmul(U(:,:n0),V(i,:n0)*W(:n0))
    enddo recon

 end function MP_PSEUDO_INV
 SUBROUTINE orthogonalize_C(A)
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: A

  REAL(DP), DIMENSION(size(A,1),size(A,2)):: U
  REAL(DP), DIMENSION(size(A,2))          :: W
  REAL(DP), DIMENSION(size(A,2),size(A,2)):: V
  INTEGER(I4B)                            :: i,n

  n=size(A,2)
  call SING_VAL_DECOMP(A,U,W,V)

  do i=1,n
    A(:,i) = U(:,i) * W(i)
  enddo

 END SUBROUTINE orthogonalize_C
!***********************************************
 SUBROUTINE orthonormalize_C(A,xx,Wo,Vo)
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: A
  REAL(DP), OPTIONAL, INTENT(IN)          :: xx
  REAL(DP), DIMENSION(size(A,2)),OPTIONAL, INTENT(OUT)          :: Wo
  REAL(DP), DIMENSION(size(A,2),size(A,2)),OPTIONAL, INTENT(OUT):: Vo

  REAL(DP), DIMENSION(size(A,1),size(A,2)):: U
  REAL(DP), DIMENSION(size(A,2))          :: W
  REAL(DP), DIMENSION(size(A,2),size(A,2)):: V
  REAL(DP)  :: xxi

! call SING_VAL_DECOMP(A,A,W,V)
  call SING_VAL_DECOMP(A,U,W,V)
  A = U
  if (PRESENT(Wo)) then
    Wo=W
  endif
  if (PRESENT(xx)) then
    A=A*xx
    if (PRESENT(Wo)) then
      xxi = one/xx
      Wo=Wo*xxi
    endif
  endif
  IF (PRESENT(Vo)) then
    Vo=V
  endif

 END SUBROUTINE orthonormalize_C
!***********************************************
 subroutine SING_VAL_LSSOLVE(a,b,thresh,x, Uo,Wo,Wio,Vo,rankw)

  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)    :: a
  REAL(DP), DIMENSION(:), INTENT(IN)      :: b
  REAL(DP), DIMENSION(:), INTENT(OUT)     :: x
  REAL(DP), INTENT(IN)                    :: thresh

  REAL(DP), DIMENSION(size(a,1),size(a,2)),OPTIONAL,intent(OUT) :: Uo
  REAL(DP), DIMENSION(size(a,2),size(a,2)),OPTIONAL,intent(OUT) :: Vo
  REAL(DP), DIMENSION(size(a,2)),OPTIONAL,intent(OUT)           :: Wo,Wio
  INTEGER(I4B),OPTIONAL,intent(OUT)                             :: rankw

  REAL(DP), DIMENSION(size(a,1),size(a,2)) :: U
  REAL(DP), DIMENSION(size(a,2),size(a,2)) :: V
  REAL(DP), DIMENSION(size(a,2))           :: W,Wi

  INTEGER(I4B)                            :: i,m,n,nw
  REAL(DP)                                :: www

  n=size(a,1)  ! Nobs
  m=size(a,2)  ! Npar

  IF (m /= size(x) .or. n /= size(b)) STOP 'SING_VAL_LSSOLVE: dim'

  call SING_VAL_DECOMP(a,U,W,V)

  IF (present(Uo)) Uo=U
  IF (present(Vo)) Vo=V

  Wi = zero
  www = thresh * MAXVAL(W)
  WHERE (W>www) Wi = one/W
  IF (present(Wio)) Wio=Wi
  nw = COUNT(W>www)
  IF (present(Wo)) then
    Wo(:nw)=W(:nw)
    Wo(nw+1:m) = zero
  endif

  IF (present(rankw)) rankw = nw

  x = matmul(V(:,:nw), Wi(:nw) * matmul(b,U(:,1:nw)) )

 end subroutine SING_VAL_LSSOLVE
!***********************************************
 subroutine SING_VAL_ROWSPACE(a,thresh_in,V,rankw,Ufull,Wfull,Ured,Wred)

!________ A is m X n
!________ the first rankw col.s of V are a base for the row-space of a
!________ the last n-rankw col.s of V are a base for the null-space of a
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)    :: a
  REAL(DP), optional,INTENT(IN)           :: thresh_in

  REAL(DP), DIMENSION(size(a,2),size(a,2)),intent(OUT),optional :: V
  REAL(DP), DIMENSION(size(a,1),size(a,1)),intent(OUT),optional :: Ured
  REAL(DP), DIMENSION(size(a,1)),intent(OUT),optional :: Wred
  REAL(DP), DIMENSION(size(a,1),size(a,2)),intent(OUT),optional :: Ufull
  REAL(DP), DIMENSION(size(a,2)),intent(OUT),optional :: Wfull
  INTEGER(I4B),intent(OUT)                             :: rankw

  REAL(DP), DIMENSION(size(a,1),size(a,2)) :: U
  REAL(DP), DIMENSION(size(a,2))           :: W
  REAL(DP)                                 :: www
  integer(I4B)  :: m,n,mm

  m=size(a,1)
  n=size(a,2)
  call SING_VAL_DECOMP(a0=a,U=U,W=W,V=V)

  IF (present(thresh_in)) then
    www = thresh_in * MAXVAL(W)
  ELSE
    www = sceps_DP * MAXVAL(W) * sqrt(real(size(a,2),DP))
  ENDIF
  rankw = COUNT(W>www)

  IF (present(Wfull)) then
    Wfull = zero
    Wfull(1:rankw)=W(1:rankw)
  ENDIF
  IF (present(Ufull)) then
    Ufull=U
  ENDIF

  IF (present(Wred)) then
    mm=min(m,n,rankw)
    Wred=zero
    Wred(1:mm)=W(1:mm)
  ENDIF
  IF (present(Ured)) then
    if (m<=n) then
      Ured=U(1:m,1:m)
    else if (m>n) then
      Ured=zero
      Ured(:,1:n)=U(:,1:n)
    endif
  ENDIF

 end subroutine SING_VAL_ROWSPACE
!***********************************************
!***********************************************
 subroutine SING_VAL_LSPRE(a,b,thresh1,x, num_effvar, Wxio,Vxo,Sxo,Pnot0,Mnot0)
!!!!!!!!! Make possible using Nbc=0 or Nac=0
!_______________________________________________________________________________________________
! EXPECTED :: a :: (Nobs X Npar) == (nob * nvr) matrix
!                  of Npar, the 1st Nbc are "polynomial bkg. terms" and/or a blank
!                           the next Nac terms are "amorphous bkg. terms"
!                           the last are "calculated intensity terms"
!
!_______________________________________________________________________________________________
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)    :: a
  REAL(DP), DIMENSION(:), INTENT(IN)      :: b
  REAL(DP), DIMENSION(:), INTENT(OUT)     :: x
  REAL(DP),optional, INTENT(IN)           :: thresh1
  INTEGER(I4B), OPTIONAL, INTENT(OUT)     :: num_effvar
  INTEGER(I4B), DIMENSION(SIZE(a,2)),optional, INTENT(OUT)         :: Pnot0,Mnot0
  REAL(DP), DIMENSION(SIZE(a,2)),optional, INTENT(OUT)         :: Wxio,Sxo
  REAL(DP), DIMENSION(SIZE(a,2),SIZE(a,2)),optional, INTENT(OUT) :: Vxo
!___ Local
  INTEGER(I4B) :: m,n,nob,nvr,NVR_wk0,neff,i,ii
  real(DP)     :: tolnorm, toln2, thrSVD
  INTEGER(I4B),dimension(SIZE(a,2)) :: valid_col,valid_mask
  real(DP),dimension(SIZE(a,2)) :: xnorm,XNORMI
  real(DP),allocatable :: Awk(:,:),Bwk(:),Xwk(:)
  INTEGER(I4B),dimension(SIZE(a,2)) :: nanflag,nanind
  REAL(DP), DIMENSION(SIZE(a,2))           :: WWio
  REAL(DP), DIMENSION(SIZE(a,2),SIZE(a,2)) :: VVo
  

  n=size(a,1)  ! Nobs
  nob=n
  m=size(a,2)  ! Npar
  nvr=m
  IF (m /= size(x) .or. n /= size(b) ) STOP 'SING_VAL_LSPRE: dim'

  nanflag=0
  !_____ discard accidentally null col.s
  valid_col = 1
  XNORMI = one
  do i=1,nvr
    if (ANY(ISNAN(a(:,i)))) then
      xnorm(i)=zero
      nanflag(i)=1
    else
      xnorm(i) = SUM(a(:,i)**2)
    endif
  enddo
  if (ANY(nanflag==1)) then
    ii=0
    nanind=0
    do i=1,nvr
      if (nanflag(i)==1) then
        ii=ii+1
        nanind(ii) = i
      endif
    enddo
    print*,'SING_VAL_LSPRE: NaN found',nanind
  endif
  tolnorm =eps_DP*REAL(nob,DP)
  toln2 = eps_DP*(REAL(nvr,DP))
  where (xnorm < tolnorm)
    valid_col = 0
    xnorm     = zero
    xnormi    = zero
  elsewhere
    xnorm  = sqrt(xnorm)
    xnormi = one/xnorm
  end where

!_____ prepare compact arrays

  NVR_wk0 = SUM(valid_col)
  call MAKE_MASK(valid_col,valid_mask)
  if (present(Pnot0)) then
    Pnot0 = valid_col
  endif
  if (present(Mnot0)) then
    Mnot0 = valid_mask
  endif
  if (present(num_effvar)) then
    num_effvar = NVR_wk0
  endif
  thrSVD = sceps_DP*sqrt(REAL(NVR_wk0,DP))
  if (present(thresh1)) then
    thrSVD=thresh1
  endif

!______ prepare fixed mat/vec for grad. eval.
  if (ALLOCATED(Awk)) DEALLOCATE(Awk)
  if (ALLOCATED(Bwk)) DEALLOCATE(Bwk)
  if (ALLOCATED(Xwk)) DEALLOCATE(Xwk)
  ALLOCATE(Awk(nob,NVR_wk0),Bwk(nob),Xwk(NVR_wk0))

  Bwk = b
  do i=1,NVR_wk0
    ii = valid_mask(i)
    Awk(:,i) = a(:,ii)*xnormi(ii)
  enddo  
  call SING_VAL_LSSOLVE(a=Awk,b=Bwk,thresh=thrSVD,x=Xwk,rankw=neff,Wio=WWio(1:NVR_wk0),Vo=VVo(1:NVR_wk0,1:NVR_wk0))
  if (present(num_effvar)) then
    num_effvar = neff
  endif
  if (present(Sxo)) then
    Sxo=zero
    !where (valid_col==1) Sxo=xnormi
    Sxo(1:NVR_wk0) = xnormi(valid_mask(1:NVR_wk0))
  endif
  if (present(Wxio)) then
    Wxio=zero
    Wxio(1:neff) = WWio(1:neff)
  endif
  if (present(Vxo)) then
    Vxo(1:NVR_wk0,1:NVR_wk0) = VVo
  endif
  
!________ backtransforming solution
  x = zero
  x(valid_mask(1:NVR_wk0)) = Xwk*xnormi(valid_mask(1:NVR_wk0))

  if (ALLOCATED(Awk)) DEALLOCATE(Awk)
  if (ALLOCATED(Bwk)) DEALLOCATE(Bwk)
  if (ALLOCATED(Xwk)) DEALLOCATE(Xwk)

 end subroutine SING_VAL_LSPRE
!***********************************************
!***********************************************
 subroutine SING_VAL_LScon(a,b,thresh1,x,req_pos, num_effvar, Nbc,Nac,iprint1,MODEinit1,COLD,COLDlc,inconA,inconB)
!!!!!!!!! Make possible using Nbc=0 or Nac=0
!_______________________________________________________________________________________________
! EXPECTED :: a :: (Nobs X Npar) == (nob * nvr) matrix
!                  of Npar, the 1st Nbc are "polynomial bkg. terms" and/or a blank
!                           the next Nac terms are "amorphous bkg. terms"
!                           the last are "calculated intensity terms"
!______________ IT IS IMPLICITLY ASSUMED THAT both the total "polynomial bkg." (l.c. of A cols. 1:Nbc)
!                                             and the total "polynomial + amorphous bkg." (l.c. of A cols. 1:Nbc+Nac)
!               SHOULD BE POSITIVE --> this generates many positivity constraints (2 * nob)
! EXPECTED :: req_pos :: flag (1) of the A columns which should have POSITIVE COEFFICIENTS
!             this generates COUNT(req_pos==1) positivity constraints
!
!_______________________________________________________________________________________________
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)    :: a
  REAL(DP), DIMENSION(:), INTENT(IN)      :: b
  INTEGER(I4B), DIMENSION(:), INTENT(IN)  :: req_pos
  INTEGER(I2B), DIMENSION(:), INTENT(INOUT),optional  :: COLD,COLDlc
  REAL(DP), DIMENSION(:), INTENT(OUT)     :: x
  REAL(DP),optional, INTENT(IN)           :: thresh1
  INTEGER(I4B), optional, INTENT(IN)      :: iprint1,MODEinit1
  INTEGER(I4B), INTENT(IN)                :: Nbc,Nac
  INTEGER(I4B), OPTIONAL, INTENT(OUT)     :: num_effvar

  REAL(DP), DIMENSION(:,:),OPTIONAL,INTENT(IN)    :: inconA
  REAL(DP), DIMENSION(:),OPTIONAL,INTENT(IN)      :: inconB

  INTEGER(I4B), DIMENSION(size(a,2))      :: free_flag,free_mask,free_flag0,free_mask0,solsign,&
                                             pos_mask,pos_flag,acflag,acmask,complflag,complmask
  INTEGER(I4B), allocatable               :: LCflag(:),LCmask(:)

  REAL(DP), DIMENSION(size(a,2))          :: carlo
  REAL(DP), DIMENSION(size(a,2),size(a,2)):: luigi
  REAL(DP), DIMENSION(size(a,1),size(a,2)):: Jmat0

  REAL(DP), DIMENSION(size(a,2))          :: gloc,dx_long
  REAL(DP), DIMENSION(size(a,1))          :: cwk,cwk0,diffwk

  REAL(DP), allocatable                   :: lagra(:),gammawk(:),cavol(:),viola(:)
  REAL(DP), allocatable                   :: Jwk(:,:),xwk(:),dxwk(:),Awk(:,:), &
                                             Twk(:,:),Rwk(:,:),Swk(:),Zwk(:,:), &
                                             Ywk(:,:),Uwk(:,:),Wwk(:),Vwk(:,:), &
                                             Mwk(:,:),Kwk(:,:),Lwk(:),Nwk(:,:)

  INTEGER(I4B)                            :: nvr,nob,NVR_wk,NVR_wk0,Rsvd,Nbc1,Nac1, &
                                             IPRINT
  real(DP)                                :: tolnorm,toln2,&
                                             thresh,threshS,lmac,xami,xmu,xdd,tolLwk,apiv,halbou
  INTEGER(I4B)                            :: m,n,wdone,i,ii,j,numcb,kdel,nacwk,solly,kokok,MODEinit,imark
  INTEGER(I4B)                            :: RANGE_dim,NULL_dim,nineq,nineq_ac,rank_M,ncompl,RSV_DP,jpiv,reason_up
  REAL(DP), DIMENSION(size(a,2))          :: XNORM,XNORMI
  real(DP)                                :: valnew,valold,valolda0,emegl,emegl1,curry,curryo,critc


  n=size(a,1)  ! Nobs
  m=size(a,2)  ! Npar
  IF (PRESENT(inconA).and..not.PRESENT(inconB)) STOP 'SING_VAL_LScon: inconA,inconB both or none'
  IF (m /= size(x) .or. n /= size(b) .or. m /= size(req_pos) ) STOP 'SING_VAL_LScon: dim'

  IF (present(COLD)) THEN
    IF (m /= size(COLD) ) STOP 'SING_VAL_LScon: <COLD> dim.'
    IF (maxval(COLD) > 1 .or. minval(COLD) < 0) &
       STOP 'SING_VAL_LScon: <COLD> out of [0,1]'
  ENDIF

  nob=n
  nvr=m
  nineq=0
  IF(PRESENT(inconA)) then
    nineq=size(inconA,1)
    IF (modulo(nineq,2)/=0) STOP ' Even n. of l.c. (LB-UB pairs) needed'
    ALLOCATE(viola(nineq))
    IF (nvr /= size(inconA,2) .or. nineq /= size(inconB) ) STOP 'SING_VAL_LScon: dim con'
    ALLOCATE(LCflag(nineq),LCmask(nineq))
    IF (present(COLDlc)) THEN
      IF (nineq /= size(COLDlc) ) &
        STOP 'SING_VAL_LScon: <COLDlc> dim.'
      IF (maxval(COLDlc) > 1 .or. minval(COLDlc) < 0) &
        STOP 'SING_VAL_LScon: <COLDlc> out of [0,1]'
    ENDIF
  else
    nineq=0
    ALLOCATE(viola(nineq))
    ALLOCATE(LCflag(nineq),LCmask(nineq))
  ENDIF
  IF (PRESENT(thresh1)) then
    thresh = thresh1
    threshS= thresh1*sqrt(thresh1)
  ELSE
    thresh = sceps_DP
    threshS= sceps_DP*s4eps_DP
  ENDIF
  IF (PRESENT(iprint1)) then
    iprint = iprint1
  ELSE
    iprint = 0
  ENDIF

  !_____ discard accidentally null col.s
  free_flag0 = 1
  XNORMI = one
  do i=1,m
    xnorm(i) = SUM(a(:,i)*a(:,i))
  enddo
  tolnorm =eps_DP*REAL(nob,DP)
  toln2 = eps_DP*(REAL(nvr,DP))
  where (xnorm < tolnorm)
    free_flag0 = 0
    xnorm      = zero
  elsewhere
    xnorm  = sqrt(xnorm)
    xnormi = one/xnorm
  end where

!_____ prepare compact arrays

  Nbc1 = SUM(free_flag0(1:Nbc))
  Nac1 = SUM(free_flag0(Nbc+1:Nbc+Nac))
  NVR_wk0 = count(free_flag0==1)
  call MAKE_MASK(free_flag0,free_mask0)
  free_flag = free_flag0
  free_mask = free_mask0

  numcb = count(req_pos(free_mask0(1:NVR_wk0))==1)

  pos_flag = req_pos * free_flag0
  call MAKE_MASK(pos_flag,pos_mask)

!______ prepare fixed mat/vec for grad. eval.
  if (ALLOCATED(Jwk)) DEALLOCATE(Jwk)
  if (ALLOCATED(Uwk)) DEALLOCATE(Uwk)
  if (ALLOCATED(Wwk)) DEALLOCATE(Wwk)
  if (ALLOCATED(Vwk)) DEALLOCATE(Vwk)
  ALLOCATE(Jwk(nob,NVR_wk0),Uwk(nob,NVR_wk0),Wwk(NVR_wk0),Vwk(NVR_wk0,NVR_wk0))
  do i=1,nvr
    Jmat0(:,i)=a(:,i)*xnormi(i)
  enddo
  do i=1,NVR_wk0
    Jwk(:,i) = a(:,free_mask0(i))*xnormi(free_mask0(i))
!Jmat0(:,free_mask0(i))
  enddo
  call SING_VAL_ROWSPACE(a=Jwk,thresh_in=threshS,V=Vwk,rankw=RSV_DP,Ufull=Uwk,Wfull=Wwk)
  carlo = matmul(b,Uwk)
  luigi = zero
  do i=1,RSV_DP
    luigi(i,:) = Wwk(i)*Vwk(:,i)
  enddo

!______ INITIALIZE: see what
  MODEinit=1
  IF (present(MODEinit1)) MODEinit=MODEinit1
  IF (.not. present(COLD)) then
    IF (MODEinit==0) then
      acflag=pos_flag
      x=zero
    ELSE IF (MODEinit==1) then
      acflag = 0
      x=zero
      x(pos_mask(1:numcb))=one
    ENDIF
  ELSE
    acflag = COLD*pos_flag
    x=zero
    where(acflag==0) x=one
  ENDIF
!_______ INITIALIZATION at feas. point
  solsign=0
  IF (.not. present(COLDlc)) then
    IF (MODEinit==0) then
!_____________________ LOCK all odd lin. cs. (corr. to LBs)
      LCflag=0
      LCflag(1:nineq-1:2)=1
      do i=1,nineq/2
        ii = 2*i-1
        jpiv=0
        apiv=zero
        do j=1,nvr
          IF (free_flag(j)==0 .or. acflag(j)==1 .or. req_pos(j)==1 .or. solsign(j)==1) cycle
          IF (ABS(inconA(ii,j)*xnormi(j))>apiv) then
            jpiv=j
            apiv=ABS(inconA(ii,j)*xnormi(j))
          endif
        enddo
        IF (apiv<eps_DP) stop 'inconsistent CS matrix'
        x(jpiv) = (inconB(ii)-sum(inconA(ii,1:jpiv-1)*x(1:jpiv-1)*xnormi(1:jpiv-1)) &
                   -sum(inconA(ii,jpiv+1:)*x(jpiv+1:)*xnormi(1+jpiv:)))/(xnormi(jpiv)*inconA(ii,jpiv))
        solsign(jpiv) = 1
      enddo
!___ 
      do i=1,nineq
        apiv=sum(inconA(i,:)*xnormi(:)*x(:))-inconB(i)
      enddo
    ELSE
      LCflag = 0
      x=zero
      x(pos_mask(1:numcb))=one
      do i=1,nineq/2
!_____________________ FREE all lin. cs. (lock to (LB+UB)/2)
        ii = 2*i-1
!_____________________ this selects the LB eq.s
        jpiv=0
        apiv=zero
        do j=1,nvr
          IF (free_flag(j)==0 .or. acflag(j)==1 .or. req_pos(j)==1 .or. solsign(j)==1) cycle
          IF (ABS(inconA(ii,j))>apiv) then
            jpiv=j
            apiv=ABS(inconA(ii,j))
          endif
        enddo
        IF (apiv<eps_DP) stop 'inconsistent CS matrix'
        halbou = 0.5_DP*(inconB(ii)-inconB(ii+1))
        x(jpiv) = (halbou-sum(inconA(ii,1:jpiv-1)*x(1:jpiv-1)*xnormi(1:jpiv-1)) &
                   -sum(inconA(ii,jpiv+1:)*x(jpiv+1:)*xnormi(1+jpiv:)))/(xnormi(jpiv)*inconA(ii,jpiv))
        solsign(jpiv) = 1
      enddo
      do i=1,nineq
        apiv=sum(inconA(i,:)*xnormi(:)*x(:))-inconB(i)
      enddo
    ENDIF
  ELSE
    STOP 'COLDlc not implemented yet'
  ENDIF

!_____ now Rsvd is the # of significant var.s
  wdone=0
  kokok=0
  cwk=matmul(Jwk,x(free_mask0(1:NVR_wk0)))
  valnew=sum(cwk*(cwk-two*b))/REAL(nob,DP)
  valolda0=sum(b*b)/real(nob,DP)
  curryo=two*(valnew+valolda0)
  reason_up=0
!________________________________ MAIN-LOOP
  REPEATref:do
!________________________________ MAIN-LOOP

!!!!!!!!! SETUP ACTIVE POSITIVITY BOUNDS
    IF (PRESENT(inconA)) then
      viola=-matmul(inconA,x*xnormi)+inconB
    else
      viola=zero
    endif

    kokok=kokok+1
    if (nineq>0.and.minval(viola)>sceps_DP) then
       print*,'stopping/infeasible',kokok,valnew
       stop 'infeasible'
    endif

!!!!!!!!! SETUP ACTIVE POS.CONSTRAINTS
    nacwk = count(acflag==1)
    call MAKE_MASK(acflag,acmask)

    complflag = (1-acflag)*free_flag0
    ncompl=count(complflag==1)
    call MAKE_MASK(complflag,complmask)

    free_flag=free_flag0
    where(acflag==1) free_flag=0
    NVR_wk = count(free_flag==1)
    call MAKE_MASK(free_flag,free_mask)
!_________ for good measure...
    x(acmask(1:nacwk))=zero
    cwk0=matmul(a,x*xnormi)
    valold = sum(cwk0*(cwk0-two*b))/REAL(nob,DP)
    cwk0 = cwk0-b
    curry=sum(cwk0*cwk0)/real(nob,DP)
!___________ Emergency exit
    critc = abs(curry-curryo)/curry
    IF (reason_up==1.and.critc<toln2) then
      print*,'emergency/stuck at ',kokok,curry,curryo,critc
      exit REPEATref
    ENDIF
    curryo=curry

!!!!!!!!! SETUP ACTIVE LIN.CONSTRAINTS
    nineq_ac = count(LCflag==1)
    LCmask=0
    call MAKE_MASK(LCflag,LCmask)

    if (ALLOCATED(Jwk)) DEALLOCATE(Jwk)
    if (ALLOCATED(xwk)) DEALLOCATE(xwk)
    if (ALLOCATED(dxwk)) DEALLOCATE(dxwk)
    if (ALLOCATED(Uwk)) DEALLOCATE(Uwk)
    if (ALLOCATED(Wwk)) DEALLOCATE(Wwk)
    if (ALLOCATED(Vwk)) DEALLOCATE(Vwk)

    if (ALLOCATED(Awk)) DEALLOCATE(Awk)
    if (ALLOCATED(gammawk)) DEALLOCATE(gammawk)
    if (ALLOCATED(cavol)) DEALLOCATE(cavol)
    if (ALLOCATED(Twk)) DEALLOCATE(Twk)
    if (ALLOCATED(Rwk)) DEALLOCATE(Rwk)
    if (ALLOCATED(Swk)) DEALLOCATE(Swk)
    if (ALLOCATED(lagra)) DEALLOCATE(lagra)


    ALLOCATE(Jwk(nob,NVR_wk),xwk(NVR_wk),dxwk(NVR_wk),cavol(NVR_wk), &
             Uwk(nob,NVR_wk),Wwk(NVR_wk),Vwk(NVR_wk,NVR_wk),gammawk(NVR_wk))

    xwk = x(free_mask(1:NVR_wk))
    do i=1,NVR_wk
      Jwk(:,i) = a(:,free_mask(i))*xnormi(free_mask(i))
    enddo
    where (ABS(Jwk)<eps_DP) Jwk=zero
    diffwk = b-matmul(Jwk,xwk)
!_____ DECOMPOSE J
    call SING_VAL_ROWSPACE(a=Jwk,thresh_in=threshS,V=Vwk,rankw=Rsvd,Ufull=Uwk,Wfull=Wwk)
    gammawk = matmul(diffwk,Uwk)
    cavol=zero
    gloc=zero

    IF (nineq_ac==0) then
      RANGE_dim=0
      NULL_dim=NVR_wk
      rank_M=Rsvd
      dxwk = matmul(Vwk(:,1:Rsvd), gammawk(1:Rsvd)/Wwk(1:Rsvd))
    ELSE IF (nineq_ac>0) then
      ALLOCATE( Awk(nineq_ac,NVR_wk), &
                Twk(NVR_wk,NVR_wk),Rwk(nineq_ac,nineq_ac),Swk(nineq_ac),lagra(nineq_ac))
      do i=1,NVR_wk
        Awk(1:nineq_ac,i) = inconA(LCmask(1:nineq_ac),free_mask(i))*xnormi(free_mask(i))
      enddo
      where (ABS(Awk)<eps_DP) Awk=zero
      call SING_VAL_ROWSPACE(a=Awk,thresh_in=threshS,V=Twk,rankw=RANGE_dim,Ured=Rwk,Wred=Swk)
      NULL_dim=NVR_wk-RANGE_dim
      rank_M=NULL_dim
      if (ALLOCATED(Zwk)) DEALLOCATE(Zwk)
      if (ALLOCATED(Ywk)) DEALLOCATE(Ywk)
      if (ALLOCATED(Mwk)) DEALLOCATE(Mwk)
      if (ALLOCATED(Kwk)) DEALLOCATE(Kwk)
      if (ALLOCATED(Lwk)) DEALLOCATE(Lwk)
      if (ALLOCATED(Nwk)) DEALLOCATE(Nwk)
      IF (RANGE_dim>0) then
        ALLOCATE(Ywk(NVR_wk,RANGE_dim))
        Ywk=Twk(:,1:RANGE_dim)
      endif
      IF (NULL_dim>0) then
        ALLOCATE(Zwk(NVR_wk,NULL_dim),Mwk(Rsvd,NULL_dim),Kwk(Rsvd,NULL_dim),Lwk(NULL_dim),Nwk(NULL_dim,NULL_dim))
        Zwk=zero
        Zwk=Twk(:,1+RANGE_dim:NVR_wk)
        Mwk=zero
        do i=1,Rsvd
          Mwk(i,1:NULL_dim) = Wwk(i)*matmul(Vwk(1:NVR_wk,i),Zwk(1:NVR_wk,1:NULL_dim))
        enddo
        call SING_VAL_ROWSPACE(a=Mwk,thresh_in=threshS,V=Nwk,rankw=rank_M,Ufull=Kwk,Wfull=Lwk)
        cavol(1:Rsvd) = matmul(Kwk(:,1:rank_M),matmul(gammawk(1:Rsvd),Kwk(1:Rsvd,1:rank_M)))
      else
        print*,'NULL_dim=0!',nineq_ac,NULL_dim
      endif
      cavol = cavol-gammawk
      gloc(free_mask(1:NVR_wk)) = two*matmul(Vwk(:,1:Rsvd),Wwk(1:Rsvd)*cavol(1:Rsvd))
      lagra = matmul(Rwk(:,:RANGE_dim),matmul(gloc(free_mask(1:NVR_wk)),Ywk(:,1:RANGE_dim))/Swk(1:RANGE_dim))
      dxwk = matmul(Zwk,matmul(Nwk(:,1:rank_M), &
                    matmul(gammawk(1:Rsvd),Kwk(1:Rsvd,1:rank_M))/Lwk(1:rank_M) ))
    ENDIF

    where(abs(dxwk)<eps_DP) dxwk=zero
    IF (PRESENT(num_effvar)) THEN
      num_effvar = Rsvd-RANGE_dim
    ENDIF

!______ now dxwk and dx_lon are calculated as the shift vector:

    dx_long=zero
    dx_long(free_mask(1:NVR_wk)) = dxwk
    where(abs(dx_long)<eps_DP) dx_long=zero

!_____ check for adding active CS...

    xmu=one
    solly=0
    imark=0
    CHECKADD:do i=1,NVR_wk
      ii=free_mask(i)
      IF (acflag(ii)==1) cycle
      if (req_pos(ii)==0.or.(req_pos(ii)==1.and.dx_long(ii)>-toln2)) cycle
      xdd=dx_long(ii)
      if (abs(xdd)<toln2) cycle !__ because of no variation
      xdd = -x(ii)/xdd
      if (xmu>xdd+toln2) then
!______ mark a variable to be blocked
        imark=ii
        xmu=min(xmu,xdd)
      endif
      IF (xmu<toln2) then
        acflag(ii) = 1
        x(ii) = zero
        solly=1
        exit CHECKADD
      ENDIF
    enddo CHECKADD
    IF (solly==1) then
      reason_up=0
      cycle REPEATref
    endif
!_______ a variable is to be possibly blocked
    IF (imark>0) acflag(imark) = -1
!___ piece on cslin inconA, inconB
    imark=0
    CHECKADD2:do i=1,nineq
      IF (LCflag(i)==1) cycle
!_____ change of constraint value
      xdd = sum(inconA(i,free_mask(1:NVR_wk))*xnormi(free_mask(1:NVR_wk))*dxwk)
      if (xdd>-toln2) cycle
      xdd = viola(i)/xdd
      if (xmu-toln2>xdd) then
        imark=i
        xmu=min(xmu,xdd)
      endif
      IF (xmu<toln2) then
        LCflag(i) = 1
        solly=1
        exit CHECKADD2
      ENDIF
    enddo CHECKADD2
    IF (solly==1) then
      reason_up=7
      cycle REPEATref
    ENDIF
    IF (imark>0) then
!__________ acflag reset
      acflag=max(acflag,0)
      LCflag(imark) = -1
    endif
!_____ No more CS to add, proceed
    gloc(:) = two*matmul((carlo+matmul(luigi,x+dx_long)),luigi(:,:))
    xwk = xwk + xmu*dxwk
    where (ABS(xwk)<eps_DP) xwk=zero
    x(free_mask(1:NVR_wk))=x(free_mask(1:NVR_wk))+xmu*dxwk
    where (ABS(x)<eps_DP) x=zero
    IF (ANY(acflag==-1) .or. ANY(LCflag==-1)) then
      where(acflag==-1) acflag=1
      where(LCflag==-1) LCflag=1
      reason_up=2
      cycle REPEATref
    ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CONVERGENCE/EXIT check
!_________ Proper conv. check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (abs(one-xmu)<toln2) then
      cwk = matmul(Jwk,xwk)
      valnew = sum(cwk*(cwk-two*b))/REAL(nob,DP)
      emegl1 = -(valnew-valold)
      emegl = two*sum(dxwk*matmul(Vwk(:,1:Rsvd),Wwk(1:Rsvd)*gammawk(1:Rsvd)))

if (emegl<-sceps_DP.or.emegl1<-sceps_DP) then
  print*,'increase! 1 ',valold+valolda0,valnew+valolda0, valnew-valold, reason_up,nineq_ac,nacwk
  print*,'increase! 2 ',emegl,emegl1,Rsvd
endif

      lmac=one
      IF (nineq_ac>0) lmac=minval(lagra)
      IF (lmac>-toln2) lmac = minval(gloc,MASK=(acflag==1))
      IF ((lmac>-toln2) .and. (emegl < sceps_DP)) then
        EXIT REPEATref
      endif
    ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CONVERGENCE/EXIT check done:
!_________ NO CONVERGENCE...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!_________ Check if any cs. may be deleted
    kdel=0
    do i=NVR_wk0,1,-1
      ii = free_mask0(i)
      if (acflag(ii)==0) cycle
      if (gloc(ii)<-toln2*three) then
        acflag(ii)=0
        kdel=kdel+1
        reason_up = 3
        cycle REPEATref
      endif
    enddo
    do i=1,nineq
      IF (LCflag(i)==0) cycle
      ii=sum(LCflag(1:i))
      if (lagra(ii)<=-toln2*three) then
        LCflag(i)=0
        kdel=kdel+1
        reason_up = 4
        cycle REPEATref
      endif
    enddo
    IF (kdel>0) then
      reason_up = 5
      cycle REPEATref
    ENDIF
!________ DO NOT Check if any bound-CS have to be added 

    reason_up=6

  enddo REPEATref

!_____ renormalize solution
  x = x*xnormi
  if (PRESENT(COLD)) COLD = acflag

  if (ALLOCATED(Jwk)) DEALLOCATE(Jwk)
  if (ALLOCATED(xwk)) DEALLOCATE(xwk)
  if (ALLOCATED(dxwk)) DEALLOCATE(dxwk)
  if (ALLOCATED(Uwk)) DEALLOCATE(Uwk)
  if (ALLOCATED(Wwk)) DEALLOCATE(Wwk)
  if (ALLOCATED(Vwk)) DEALLOCATE(Vwk)

  if (ALLOCATED(Twk)) DEALLOCATE(Twk)
  if (ALLOCATED(Swk)) DEALLOCATE(Swk)
  if (ALLOCATED(Rwk)) DEALLOCATE(Rwk)
  if (ALLOCATED(Zwk)) DEALLOCATE(Zwk)
  if (ALLOCATED(Ywk)) DEALLOCATE(Ywk)
  if (ALLOCATED(Mwk)) DEALLOCATE(Mwk)
  if (ALLOCATED(Kwk)) DEALLOCATE(Kwk)
  if (ALLOCATED(Lwk)) DEALLOCATE(Lwk)
  if (ALLOCATED(Nwk)) DEALLOCATE(Nwk)
  if (ALLOCATED(Awk)) DEALLOCATE(Awk)
  if (ALLOCATED(cavol)) DEALLOCATE(cavol)
  if (ALLOCATED(lagra)) DEALLOCATE(lagra)
  if (ALLOCATED(gammawk)) DEALLOCATE(gammawk)
  IF(ALLOCATED(LCflag)) DEALLOCATE(LCflag)
  IF(ALLOCATED(LCmask)) DEALLOCATE(LCmask)
  IF(ALLOCATED(viola)) DEALLOCATE(viola)

 end subroutine SING_VAL_LScon
!***********************************************
!***********************************************
 subroutine SING_VAL_LSPOS(a,b,thresh1,x,req_pos, num_effvar, Nbc,Nac,iprint1,flaus,COLD)
!!!!!!!!! Make possible using Nbc=0 or Nac=0
!_______________________________________________________________________________________________
! EXPECTED :: a :: (Nobs X Npar) == (nob * nvr) matrix
!                  of Npar, the 1st Nbc are "polynomial bkg. terms" and/or a blank
!                           the next Nac terms are "amorphous bkg. terms"
!                           the last are "calculated intensity terms"
!______________ IT IS IMPLICITLY ASSUMED THAT both the total "polynomial bkg." (l.c. of A cols. 1:Nbc)
!                                             and the total "polynomial + amorphous bkg." (l.c. of A cols. 1:Nbc+Nac)
!               SHOULD BE POSITIVE --> this generates many positivity constraints (2 * nob)
! EXPECTED :: req_pos :: flag (1) of the A columns which should have POSITIVE COEFFICIENTS
!             this generates COUNT(req_pos==1) positivity constraints
!
!_______________________________________________________________________________________________
  IMPLICIT NONE
  REAL(DP), DIMENSION(:,:), INTENT(IN)    :: a
  REAL(DP), DIMENSION(:), INTENT(IN)      :: b
  INTEGER(I4B), DIMENSION(:), INTENT(IN)  :: req_pos
  INTEGER(I2B), DIMENSION(:), INTENT(INOUT),optional  :: COLD
  REAL(DP), DIMENSION(:), INTENT(OUT)     :: x
  REAL(DP),optional, INTENT(IN)           :: thresh1
  INTEGER(I4B), optional, INTENT(IN)      :: iprint1,flaus
  INTEGER(I4B), INTENT(IN)                :: Nbc,Nac
  INTEGER(I4B), OPTIONAL, INTENT(OUT)     :: num_effvar

  INTEGER(I4B), DIMENSION(size(a,2))      :: free_flag,free_mask,free_flag0,free_mask0,solsign,&
                                             pos_mask,pos_flag,acflag,acmask
  REAL(DP), DIMENSION(size(a,2))          :: gwk,gloc,dx
  REAL(DP), DIMENSION(size(a,1))          :: cwk
  REAL(DP), DIMENSION(size(a,2),size(a,2)):: hwk

  REAL(DP), allocatable                   :: awk(:,:),xwk(:)

  INTEGER(I4B)                            :: k,nvr,nob,nwk,nwk0,Rsvd,numcon,numlbc,Nbc1,Nac1,nsubp, &
                                             ido_feas,nclong,NACTIVEC,IFAIL,IPRINT,NUMCON_EFF,NUMCON_DISC
  real(DP)                                :: tolnorm,toln2,xs1,xs2,xs3,www,conad,tol_ALINE,canor,tol_vio,fin_fun,&
                                             thresh, gmfree,lmac,xami,xmu,xdd
  INTEGER(I4B)                            :: m,n,wdone,idelc,iaddc,i,ii,j,jj,numcb,mlk(1),kdel,nacwk,solly,kokok,iflaus
  REAL(DP), DIMENSION(size(a,2))          :: XNORM,XNORMI
  real(DP)                                :: val0,valk,valka,valka0,emegl


  n=size(a,1)  ! Nobs
  m=size(a,2)  ! Npar
  IF (m /= size(x) .or. n /= size(b) .or. m /= size(req_pos) ) STOP 'SING_VAL_LSPOS: dim'
  IF (present(COLD)) THEN
    IF (m /= size(COLD) ) THEN
        STOP 'SING_VAL_LSPOS: <COLD> dim.'
    ENDIF
    IF (maxval(COLD) > 1 .or. minval(COLD) < 0) THEN 
        STOP 'SING_VAL_LSPOS: <COLD> out of [0,1]'
    ENDIF
  ENDIF

  nob=n
  nvr=m
  IF (PRESENT(thresh1)) then
    thresh = thresh1
  ELSE
    thresh = sceps_DP
  ENDIF
  IF (PRESENT(iprint1)) then
    iprint = iprint1
  ELSE
    iprint = 0
  ENDIF

  !_____ discard accidentally null col.s
  free_flag0 = 1
  XNORMI = one
  do i=1,m
    xnorm(i) = SUM(a(:,i)*a(:,i))
  enddo
  tolnorm = eps_DP*REAL(nob,DP)
  toln2 = eps_DP*(REAL(nvr,DP))
  where (xnorm < tolnorm)
    free_flag0 = 0
    xnorm      = zero
  elsewhere
    xnorm  = sqrt(xnorm)
    xnormi = one/xnorm
  end where

!_____ prepare compact arrays

  Nbc1 = SUM(free_flag0(1:Nbc))
  Nac1 = SUM(free_flag0(Nbc+1:Nbc+Nac))
  nwk0 = count(free_flag0==1)
  call MAKE_MASK(free_flag0,free_mask0)
  free_flag = free_flag0
  free_mask = free_mask0

  numcb = count(req_pos(free_mask0(1:nwk0))==1)

  pos_flag = req_pos * free_flag0
  call MAKE_MASK(pos_flag,pos_mask)

!____ gwk is the gradient,hwk the half hessian in the taylor exp. of f=||a.x-b||=b.b-2.bT.a.x+xT.aT.a.x
!____ i.e. gwk = -2.bT.a ; hwk = aT.a

  gwk=zero
  hwk=zero
  do i=1,nwk0
    ii = free_mask0(i)
    xs1 = xnormi(ii)*two
    gwk(ii) = -xs1*sum(b*a(:,ii))
    do j=1,i
      jj = free_mask0(j)
      hwk(ii,jj) = sum(a(:,ii)*a(:,jj))*xs1*xnormi(jj)
      if (j<i) hwk(jj,ii)=hwk(ii,jj)
    enddo
  enddo

!_____ copy onto compact arrays, simultaneously normalizing
!______ INIT.:lock all
  iflaus=1
  IF (present(flaus)) iflaus=flaus
  IF (.not. present(COLD)) then
    IF (iflaus==0) then
      acflag=pos_flag
      x=zero
    ELSE
      acflag = 0
      x=zero
      x(pos_mask(1:numcb))=one
    ENDIF
  ELSE
    acflag = COLD*pos_flag
    x=zero
    where(acflag==0) x=one
  ENDIF
!_____ now Rsvd is the # of significant var.s
  wdone=0
  kokok=0
  val0=huge(one)
  valka0=sum(b*b)/real(nob,DP)
  REPEATref:do

!!!!!!!!! SETUP
    kokok=kokok+1
    nacwk = count(acflag==1)
    call MAKE_MASK(acflag,acmask)
    free_flag=free_flag0
    where(acflag==1) free_flag=0
    nwk = count(free_flag==1)
    call MAKE_MASK(free_flag,free_mask)

    if (ALLOCATED(awk)) DEALLOCATE(awk)
    if (ALLOCATED(xwk)) DEALLOCATE(xwk)
    ALLOCATE(awk(nob,nwk),xwk(nwk))
!
    do i=1,nwk
      awk(:,i) = a(:,free_mask(i))*xnormi(free_mask(i))
    enddo
    where (ABS(awk)<eps_DP) awk=zero
!_____ solve unconstrained L.S. problem
    call SING_VAL_LSSOLVE(a=awk,b=b,thresh=thresh,x=xwk,rankw=Rsvd)
    IF (PRESENT(num_effvar)) THEN
      num_effvar = Rsvd
    ENDIF
    gloc = zero

    dx=zero
    dx(free_mask(1:nwk)) = xwk
!_____ KIFS!
    where(abs(dx)<eps_DP) dx=zero
    xami = one
    do i=1,nwk
      ii=free_mask(i)
      if (req_pos(ii)==0) cycle
      xami=min(xami,dx(ii))
    enddo
    xmu=one
    solly=0
    if (xami>=-eps_DP) then
      x=dx
    else
      do i=1,nwk
        ii=free_mask(i)
        if (req_pos(ii)==0.or.(req_pos(ii)==1.and.dx(ii)>-eps_DP)) cycle
        xdd=dx(ii)-x(ii)
        if (abs(xdd)<eps_DP) cycle
! because of no variation
        IF (abs(x(ii))<sceps_DP) then
          acflag(ii) = 1
          solly=1
          cycle REPEATref
        ELSE
          xmu=min(xmu,-x(ii)/xdd)
          IF (xmu<toln2) then
            acflag(ii) = 1
            solly=1
            cycle REPEATref
          ENDIF
        ENDIF
      enddo
      IF (solly==1) cycle REPEATref
      x(free_mask(1:nwk))=x(free_mask(1:nwk))+xmu*(dx(free_mask(1:nwk))-x(free_mask(1:nwk)))
      where (ABS(x)<eps_DP) x=zero
      xwk = x(free_mask(1:nwk))
    endif
    gloc = gwk+matmul(hwk(:,free_mask(1:nwk)),xwk)
    cwk = matmul(awk,xwk)
    valk=val0
    val0 = sum(cwk*(cwk-two*b))/REAL(nob,DP)

    emegl = (val0-valk)/valka0

!_________ Proper conv. check
    gmfree=zero
    lmac=one
    do i=1,nvr
      if (acflag(i)==0.and.free_flag0(i)==1) gmfree = max(gmfree,abs(gloc(i)))
      if (acflag(i)==1) lmac = min(lmac,gloc(i))
    enddo

    IF ((lmac>-toln2.or.emegl>-nob*eps_DP).and.abs(one-xmu)<eps_DP) EXIT REPEATref

!_________ Check if any cs. may be deleted
    kdel=0
    do i=nwk0,1,-1
      ii = free_mask0(i)
      if (acflag(ii)==0) cycle
      if (gloc(ii)<-toln2) then
        acflag(ii)=0
        kdel=kdel+1
        exit
      endif
    enddo
    IF (kdel>0) cycle REPEATref

!________ Check if any CS have to be added
    solsign=1
    where(x<-toln2) solsign=-1
    where(abs(x)<=toln2) solsign=0
    do i=1,nwk0
      ii = free_mask0(i)
      if (free_flag(ii)==0 .or. req_pos(ii) == 0 .or. solsign(ii) == 1) CYCLE
      if (solsign(ii)==0.and.gloc(i)>-toln2) acflag(ii)=1
!________ ... add one at a time!
    enddo

  enddo REPEATref

!_____ renormalize solution
  x = x*xnormi
  if (PRESENT(COLD)) COLD = acflag
  if (ALLOCATED(awk)) DEALLOCATE(awk)
  if (ALLOCATED(xwk)) DEALLOCATE(xwk)

 end subroutine SING_VAL_LSPOS
!***********************************************
 SUBROUTINE BILIN_LS_REF(&
            Obse, C, B, sigma, ss, zeta, &
            N, P, Q, Nrp, kisq,wrd,verbos)
   IMPLICIT NONE
   INTEGER(I4B),INTENT(IN)                  :: N, P, Nrp
   INTEGER(I4B), DIMENSION(N),INTENT(IN)    :: Q
   TYPE(bivec),  DIMENSION(N),INTENT(IN)    :: Obse
   TYPE(trivec), DIMENSION(N),INTENT(IN)    :: B, C
   TYPE(bivec),  DIMENSION(N),INTENT(OUT)   :: zeta
   REAL(DP), INTENT(OUT)                    :: sigma(N), ss(P), kisq
   REAL(DP),OPTIONAL, INTENT(OUT)           :: wrd
   INTEGER(I4B),OPTIONAL,INTENT(IN)         :: verbos

   ! INIT. VAL. of ss,sigma,zeta = 1

   TYPE(bivec),DIMENSION(N)  :: ESS,ZED,zeta0,DIF
   REAL(DP)                  :: sigma0(N), ss0(P)
   TYPE(bivec),DIMENSION(N)  :: dESS,dZED,dzeta,dDIF
   REAL(DP)                  :: dsigma(N), dss(P)
   REAL(DP)                  :: crut,chis_i,dchis_r,ddchis_r
   REAL(DP)                  :: time00,time0,time1
   INTEGER(I4B)              :: i,m,k,ndo(N),ndos(N+1),Qs(N+1),ndom,kup,klo

   REAL(DP),ALLOCATABLE      :: sol1(:),mat1(:,:),Ovec1(:)
   REAL(DP),ALLOCATABLE      :: sol2(:),mat2(:,:),Ovec2(:)
   INTEGER(I4B),ALLOCATABLE  :: rep1(:),rep2(:)
   INTEGER(I4B)              :: Notot,Npar1,Npar2,verbosei

   call CPU_TIME(time00)
   verbosei = 0
   IF (PRESENT(verbos)) THEN
     verbosei=verbos
   ENDIF

   Npar1 = P+SUM(Q)
   Npar2 = N-1+SUM(Q)
   ndo   = (/(size(Obse(m)%BU),m=1,N)/)
   Notot = SUM(ndo)
   ndos(1) = 0
   Qs(1)   = 0
   do m=2,N+1
     ndos(m) = SUM(ndo(:m-1))
     Qs(m)   = SUM(Q(:m-1))
   enddo

   ALLOCATE(Ovec1(Notot),sol1(Npar1),mat1(Notot,Npar1),rep1(Npar1), &
            Ovec2(Notot),sol2(Npar2),mat2(Notot,Npar2),rep2(Npar2))
   IF (PRESENT(wrd)) wrd = zero
   do m=1,N

     IF (ASSOCIATED(zeta(m)%BU)) NULLIFY(zeta(m)%BU)
     ALLOCATE(zeta(m)%BU(Q(m)))

     IF (ASSOCIATED(zeta0(m)%BU)) NULLIFY(zeta0(m)%BU)
     ALLOCATE(zeta0(m)%BU(Q(m)))
     zeta0(m)%BU = one

     IF (ASSOCIATED(dzeta(m)%BU)) NULLIFY(dzeta(m)%BU)
     ALLOCATE(dzeta(m)%BU(Q(m)))

     Ovec1(ndos(m)+1:ndos(m+1)) = Obse(m)%BU
     IF (PRESENT(wrd)) wrd = wrd + DOT_PRODUCT(Obse(m)%BU,Obse(m)%BU)

     IF (ASSOCIATED(ESS(m)%BU)) NULLIFY(ESS(m)%BU)
     ALLOCATE(ESS(m)%BU(ndo(m)))

     IF (ASSOCIATED(dESS(m)%BU)) NULLIFY(dESS(m)%BU)
     ALLOCATE(dESS(m)%BU(ndo(m)))

     IF (ASSOCIATED(DIF(m)%BU)) NULLIFY(DIF(m)%BU)
     ALLOCATE(DIF(m)%BU(ndo(m)))

     IF (ASSOCIATED(dDIF(m)%BU)) NULLIFY(dDIF(m)%BU)
     ALLOCATE(dDIF(m)%BU(ndo(m)))

     IF (ASSOCIATED(ZED(m)%BU)) NULLIFY(ZED(m)%BU)
     ALLOCATE(ZED(m)%BU(ndo(m)))
     ZED(m)%BU = one

     IF (ASSOCIATED(dZED(m)%BU)) NULLIFY(dZED(m)%BU)
     ALLOCATE(dZED(m)%BU(ndo(m)))

   enddo

   sigma  = one
   ss     = one
   sigma0 = one
   ss0    = one

   toggling:DO
     call CPU_TIME(time0)
     !________ FIX sigma(2:N), refine ss,zeta
     IF (verbosei == 3) THEN
       print'(1x,a,20(1x,g12.6))','SETS0: ',sigma0
       print'(1x,a,40(1x,g12.6))','STRS0: ',ss0
     ENDIF
     chis_i = zero
     do m=1,N
       ESS(m)%BU = matmul(C(m)%TU,ss0)
       ZED(m)%BU = matmul(B(m)%TU,zeta0(m)%BU)
       DIF(m)%BU = sigma0(m)*ESS(m)%BU + (ZED(m)%BU - Obse(m)%BU)
       chis_i = chis_i + DOT_PRODUCT(DIF(m)%BU,DIF(m)%BU)
     enddo

     IF (verbosei >= 1) print'(1x,a,1x,g14.8)','*** Chi^2_init. ',chis_i / Notot

     rep1 = 0
     rep1(1:Nrp) = 1

     mat1 = zero
     do m=1,N
       mat1(ndos(m)+1:ndos(m+1),1:P) = sigma0(m) * C(m)%TU
       do k=1,Q(m)
         mat1(ndos(m)+1:ndos(m+1),P+Qs(m)+k) = B(m)%TU(:,k)
       enddo
     enddo

!    Mod. 07 Sep 2016
!     CALL SING_VAL_LSPOS(a=mat1,b=Ovec1,thresh1=sceps_DP,x=sol1,req_pos=rep1,Nbc=Npar1-Nrp,Nac=Nrp)
     CALL SING_VAL_LSpre(a=mat1,b=Ovec1,thresh1=sceps_DP,x=sol1)

     ss = sol1(1:P)
     do m=1,N
       zeta(m)%BU = sol1(P+Qs(m)+1:P+Qs(m+1))
     enddo

     IF (verbosei == 3) THEN
       print'(1x,a,20(1x,g12.6))','SETSx: ',sigma0
       print'(1x,a,40(1x,g12.6))','STRSx: ',ss
     ENDIF

     !________ FIX ss, refine sigma(2:N),zeta
     do m=1,N
       dESS(m)%BU = matmul(C(m)%TU,ss)
     enddo
     Ovec2 = Ovec1
     Ovec2(ndos(1)+1:ndos(2)) = Ovec1(ndos(1)+1:ndos(2))-sigma0(1)*dESS(1)%BU

     rep2 = 0
     rep2(1:N-1) = 1

     mat2 = zero
     do m=2,N
       mat2(ndos(m)+1:ndos(m+1),m-1) = dESS(m)%BU
     enddo

     do m=1,N
       do k=1,Q(m)
         mat2(ndos(m)+1:ndos(m+1),N-1+Qs(m)+k) = B(m)%TU(:,k)
       enddo
     enddo

!    Mod. 07 Sep 2016
!     CALL SING_VAL_LSPOS(a=mat2,b=Ovec2,thresh1=sceps_DP,x=sol2,req_pos=rep2,Nbc=Npar2-N+1,Nac=N-1)
     CALL SING_VAL_LSpre(a=mat2,b=Ovec2,thresh1=sceps_DP,x=sol2)

     sigma(2:N) = sol2(1:N-1)
     dsigma = sigma-sigma0
     dss = ss-ss0
     do m=1,N
       zeta(m)%BU = sol2(N-1+Qs(m)+1:N-1+Qs(m+1))
       dzeta(m)%BU = zeta(m)%BU - zeta0(m)%BU
     enddo

     do m=1,N

       dESS(m)%BU = matmul(C(m)%TU,dss)

       dZED(m)%BU = matmul(B(m)%TU,dzeta(m)%BU)

       dDIF(m)%BU =  dsigma(m) *  ESS(m)%BU &
                   +  sigma(m) * dESS(m)%BU + dZED(m)%BU
     enddo

     dchis_r = zero
     do m=1,N
       ddchis_r = SUM(dDIF(m)%BU * (two*DIF(m)%BU + dDIF(m)%BU))
       dchis_r = dchis_r + ddchis_r
     enddo

     kisq = chis_i+dchis_r    ! new chi^2
     dchis_r = dchis_r/chis_i ! relative variation
     IF (verbosei == 3) THEN
       print'(1x,a,20(1x,g12.6))','SETSf: ',sigma
       print'(1x,a,40(1x,g12.6))','STRSf: ',ss
     ENDIF
     IF (verbosei >= 2) THEN
       print'(1x,a,20(1x,g12.6))','DIFFf: ',dsigma
       print'(1x,a,40(1x,g12.6))','DIFFf: ',dss
     ENDIF
     call CPU_TIME(time1)
     IF (verbosei >= 1) print'(1x,a,2(1x,g14.8))','*** Chi^2_fin., DChi^2_Rel. ', kisq / Notot, dchis_r
     IF (verbosei==2) print*,'TOGGLING CYCLE TIME seconds ',time1-time0

!***** EVALUATE EXIT CONDITIONS
     crut = ABS(dchis_r/max(one,abs(chis_i)))
     IF (crut < sceps_DP) EXIT toggling
     crut = MAX(maxval(abs( dsigma/max(one,abs(sigma)) )),maxval(abs( dss/max(one,abs(ss)) )))
     do m=1,N
       crut = MAX(crut,MAXVAL(ABS( dzeta(m)%BU/max(one,abs(zeta(m)%BU)) )))
     enddo

     IF (crut < sceps_DP) EXIT toggling
!***** DONE WITH EXIT CONDITIONS
!***** UPDATE INITIAL VALUES
     ss0    = ss
     sigma0 = sigma
     do m=1,N
       zeta0(m)%BU  = zeta(m)%BU
     enddo
   ENDDO toggling

   DEALLOCATE(Ovec1,sol1,mat1,rep1, &
              Ovec2,sol2,mat2,rep2)
   do m=1,N
     IF (ASSOCIATED(zeta0(m)%BU)) NULLIFY(zeta0(m)%BU)
     IF (ASSOCIATED(dzeta(m)%BU)) NULLIFY(dzeta(m)%BU)
     IF (ASSOCIATED(ESS(m)%BU)) NULLIFY(ESS(m)%BU)
     IF (ASSOCIATED(dESS(m)%BU)) NULLIFY(dESS(m)%BU)
     IF (ASSOCIATED(DIF(m)%BU)) NULLIFY(DIF(m)%BU)
     IF (ASSOCIATED(dDIF(m)%BU)) NULLIFY(dDIF(m)%BU)
     IF (ASSOCIATED(ZED(m)%BU)) NULLIFY(ZED(m)%BU)
     IF (ASSOCIATED(dZED(m)%BU)) NULLIFY(dZED(m)%BU)
   enddo
   call CPU_TIME(time1)
   IF (verbosei>=1) print*,'BILIN_LS_REF: TOTAL TIME s. ',time1-time00

 END SUBROUTINE BILIN_LS_REF
!*************************************************************************
SUBROUTINE Cspline_evalD2(x,y,yp1,ypn,y2)
implicit none
REAL(DP),intent(IN),optional :: yp1,ypn
REAL(DP),intent(IN),dimension(:) :: x,y
REAL(DP),intent(OUT),dimension(size(x)) :: y2
integer(I4B) :: i,k,n
REAL(DP) ::  p,qn,sig,un
REAL(DP),dimension(size(x)) ::  u

n=size(x)
if (size(y)/=n) stop 'Cspline_evalD2: unaligned x,y'
if (.not.present(yp1)) then
  y2(1)=zero
  u(1)=zero
else
  y2(1)=-half
  u(1)=(three/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
endif
do i=2,n-1
  sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
  p=sig*y2(i-1)+two
  y2(i)=(sig-one)/p
  u(i)=(six*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) / &
       (x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
enddo
if (.not.present(ypn)) then
  qn=zero
  un=zero
else
  qn=half
  un=(three/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
endif
y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+one)
do k=n-1,1,-1
  y2(k)=y2(k)*y2(k+1)+u(k)
enddo
END SUBROUTINE Cspline_evalD2
!*************************************************************************
SUBROUTINE Cspline_go(xa,ya,y2a,x,y)
implicit none
REAL(DP),intent(IN),dimension(:) :: xa,ya,y2a
REAL(DP),intent(IN) :: x
REAL(DP),intent(OUT) :: y
REAL(DP) :: a,b,h
integer(I4B) :: k,khi,klo,n

n=size(xa)
if (size(ya)/=n.or.size(y2a)/=n) stop 'Cspline_go: unaligned x,y'
klo=1
khi=n
do
  if (khi-klo<=1) exit
  k=(khi+klo)/2
  if(xa(k)>x)then
    khi=k
  else
    klo=k
  endif
enddo
h=xa(khi)-xa(klo)
if (h<eps_DP) stop 'bad x input in Cspline_go'
a=(xa(khi)-x)/h
b=(x-xa(klo))/h
y=a*ya(klo)+b*ya(khi)+(a*(a*a-one)*y2a(klo)+b*(b*b-one)*y2a(khi))*(h*h)*unses

END SUBROUTINE Cspline_go
!*************************************************************************
SUBROUTINE Cspline_INTERP(xa,ya,x,y)
implicit none
REAL(DP),intent(IN),dimension(:) :: xa,ya
REAL(DP),intent(IN) :: x
REAL(DP),intent(OUT) :: y
REAL(DP),dimension(size(xa)) :: y2a

call Cspline_evalD2(x=xa, y=ya, y2=y2a)
call Cspline_go(xa=xa,ya=ya,y2a=y2a,x=x,y=y)

END SUBROUTINE Cspline_INTERP
!*************************************************************************
subroutine Cspline_FT(x,y,y2,qv,Fq1,Fq2)
implicit none
real(DP),dimension(:),intent(IN) :: x,y,y2,qv
real(DP),dimension(size(qv)),intent(OUT) :: Fq1,Fq2
real(DP),dimension(2) :: VFA,VFD,VFR,VFL,au2,au20, FF1L,FF1R,FF2L,FF2R
real(DP) :: q,qpi2,iqp2s,xj,Dxj,yj,Dyj,y2j,Dy2j,Dxjs,Cj,DCj,Sj,DSj,v,u,aa,bb,qpi2s
integer(I4B) :: j,i,n,m

n=size(x)
m=size(qv)
Fq1=zero; Fq2=zero
do i=1,m
  q=qv(i)
  qpi2=q*pi2
  qpi2s=qpi2**2
  iqp2s=one/(qpi2s)
  au20=[qpi2s*unter,one]*qpi2s  ! [ 1/(48*Pi^4*q^4), 1/(4*Pi^2*q^2) ]
  do j=1,n-1
    xj=x(j)
    Cj=cos(xj*qpi2)
    Sj=sin(xj*qpi2)
    Dxj=x(j+1)-xj
    bb=Dxj*qpi2
    u=cos(bb)-one
    v=sin(bb)
    DCj=Cj*u-Sj*v
    DSj=Sj*u+Cj*v
    Dxjs=Dxj*Dxj
    yj=y(j)
    Dyj=y(j+1)-yj
    y2j=y2(j)
    Dy2j=y2(j+1)-y2j
    VFD = half*au20*[Dy2j,Dyj]
    VFL = au20*[y2j,yj]
    VFA = VFD+VFL
    VFR = VFD+VFA
    aa=Dxjs*qpi2s*half
    FF1L = [three*(DCj+ bb*Sj) + aa*(three*Cj + DCj) , -DCj - bb*Sj]
    FF1R = [-three*(DCj + bb*(DSj + Sj)) + aa*(three*Cj + two*DCj) , DCj + bb*(DSj + Sj)]
    Fq1(i) = Fq1(i) + sum(FF1L*VFL) + sum(FF1R*VFR)
    FF2L = [three*(DSj - Cj*bb) + aa*(DSj + three*Sj), -DSj + Cj*bb]
    FF2R = [three*(-DSj + (Cj + DCj)*bb) + aa*(DSj*two + three*Sj), DSj - (Cj + DCj)*bb]
    Fq2(i) = Fq2(i) + sum(FF2L*VFL) + sum(FF2R*VFR)
  enddo
enddo

end subroutine Cspline_FT
!*************************************************************************
subroutine find_ranges_IRF(dtt,ttmax, Gsig, Lwid, &
                           CapD_R, WobR_R, H_rec,N_rec,delta_rec, &
                           U_dir,N_dir,delta_dir)
implicit none
real(DP),intent(IN) :: dtt,ttmax, Gsig, Lwid, CapD_R, WobR_R
real(DP),intent(OUT) :: H_rec,delta_rec, U_dir,delta_dir
integer(I4B),intent(OUT) :: N_rec,N_dir
integer(I4B) :: nu

H_rec = min(sqrt(-two*log(eps_DP))/(pi*Gsig), -log(eps_DP)/(pi2*Lwid) )

U_dir = max(CapD_R*half+WobR_R, ttmax, Lwid/s4eps_DP, 12.5d0*Gsig)

delta_rec = half/U_dir
N_rec = ceiling(H_rec/delta_rec)
delta_rec=H_rec/REAL(N_rec,DP)
delta_dir = min(dtt,half/H_rec)
N_dir = ceiling(U_dir/delta_dir)
delta_dir=U_dir/REAL(N_dir,DP)

!print*,N_rec,N_dir

end subroutine find_ranges_IRF
!*************************************************************************
end module LINALG_TOOLS
!_____________________________________________________________________________________________________
module calcdp_sam
USE nano_deftyp
use LINALG_TOOLS

type,public :: dataset_arr
  integer(I4B) :: dimen_vec
  real(DP),DIMENSION(:),allocatable :: vec_dat
END TYPE dataset_arr

type(dataset_arr), allocatable, save :: dsd1(:)

logical,save  :: isalloc_dsd1=.false.

contains
!*******************************************************
subroutine ALLOC_DSD1(n,innd)
implicit none
integer(I4B),intent(IN) :: n
integer(I4B),intent(IN) :: innd(n)
integer(I4B) :: i

call DEALLOC_DSD1()
allocate(dsd1(n))
do i=1,n
  dsd1(i)%dimen_vec = innd(i)
  allocate(dsd1(i)%vec_dat(innd(i)))
enddo
isalloc_dsd1 = .true.

end subroutine ALLOC_DSD1
!*******************************************************
subroutine DEALLOC_DSD1()
implicit none
integer(I4B) :: i

if (.not.allocated(dsd1)) return
do i=1,size(dsd1)
  deallocate(dsd1(i)%vec_dat)
enddo
deallocate(dsd1)
isalloc_dsd1 = .false.

end subroutine DEALLOC_DSD1
!*******************************************************
subroutine FILL_DSD1(i,qapi2,dilat,gsig)
!___________ Evaluates correction factor exp(+1/2 * (2*Pi*q*sigma*s)**2)
!___________ Input : qapi2 = 2*Pi*q(:)
!___________       : dilat = s (dilatation strain); gsig = sigma
implicit none
integer(I4B),intent(IN) :: i
real(DP),dimension(:),intent(IN) :: qapi2
real(DP),intent(IN)              :: dilat,gsig
real(DP) :: cog

if (.not.isalloc_dsd1) stop 'FILL_DSD1: Not allocated dsd1'
if (size(qapi2) /= dsd1(i)%dimen_vec) stop 'FILL_DSD1: Unmatching dimensions'

cog = srhalf*gsig*dilat
dsd1(i)%vec_dat(:) = exp( ((cog * qapi2)**2) )

end subroutine FILL_DSD1
!*******************************************************
function NSCATT(i,qapi2,dilat,gsig,delta,wsam)
implicit none
integer(I4B),intent(IN) :: i
real(DP),dimension(:),intent(IN) :: qapi2
real(DP),intent(IN)              :: dilat,gsig,delta
real(DP),dimension(:),intent(IN) :: wsam
real(DP),dimension(size(qapi2))  :: NSCATT
real(DP),dimension(size(qapi2))  :: uaux

uaux = (dilat*delta) * qapi2
NSCATT = dsd1(i)%vec_dat * (sin(uaux) / uaux) * U_cheb(dum=vec1_arg, c = wsam, x = cos(uaux))

end function NSCATT

end module calcdp_sam
