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
MODULE NANO_TYPES
 use SPECIAL_TYPES
 use input_data 

 INTEGER(I4B),parameter                     :: NPH_DB2 = 4
 INTEGER(I4B),parameter                     :: nIKOSA=50,nCUBOK=50,nDEKAH=50, nDEKAB=43, &
                                               NLARGEST = MAX(nIKOSA,nCUBOK,nDEKAH, nDEKAB)
 INTEGER(I4B),save                          :: nmaxsize, nactsize, NLARGEST1 
 CHARACTER(5),dimension(NPH_DB2),save       :: finp_names
 INTEGER(I4B),dimension(NPH_DB2),save       :: NCLUST_DB2
 INTEGER(I4B),dimension(:),allocatable,save :: NCLUST_DB1
 INTEGER(I4B),dimension(:),allocatable,save :: NCLUST_DB3
 INTEGER(I4B),dimension(:),allocatable,save :: NCLUST_DB4
 INTEGER(I4B),dimension(:),allocatable,save :: NCLUST_DB5

 TYPE(nanoiav_mult),ALLOCATABLE,save     :: nano_iav(:)
 
 REAL(DP),ALLOCATABLE,save          :: wintr(:,:),sintr(:,:)
 INTEGER(I4B),ALLOCATABLE,save      :: nano_nat(:,:)

 DATA NCLUST_DB2/nCUBOK,nIKOSA,nDEKAH,nDEKAB/

 contains

 SUBROUTINE TO_DB_READERS
   IMPLICIT NONE
   INTEGER(I4B)                               :: i,j,iki,n,nki

   nactsize = 0
   NLARGEST1 = MAXVAL(n2read)
   do i=1,nstr
!     if (DB_INDEX(i) > 4) cycle
      if (DB_INDEX(i) > 5) cycle
      if (DB_INDEX(i) == 1) n2read(i) = 1
      if (DB_INDEX(i) == 2) n2read(i) = NCLUST_DB2(ITYPE_DB(i))
      if (DB_INDEX(i) == 4 .or. DB_INDEX(i) == 5) then
          n2read(i) = n2read_ab(i,2)*n2read_c(i,2)
      endif
      nactsize = max(nactsize,n2read(i))
   enddo

   NLARGEST1 = MAX(NLARGEST1,NLARGEST)
   NLARGEST1 = MAX(NLARGEST1,nactsize)

   IF (ALLOCATED(nano_iav)) THEN
     n = SIZE(nano_iav)
     do i=1,n
       nki=nano_iav(i)%dimstruk
       do iki=1,nki
         CALL destroy_iavS(nano_iav(i)%struk(iki))
       enddo
     enddo
   ENDIF

   ALLOCATE(nano_iav(nstr))
   do j=1,nstr
     ALLOCATE(nano_iav(j)%struk(n2read(j)))
     nano_iav(j)%dimstruk = n2read(j)
   enddo

   do j=1,nstr
     do i=1,nano_iav(j)%dimstruk
       nano_iav(j)%struk(i)%numspat= 1
       nano_iav(j)%struk(i)%numpair= 1
       nano_iav(j)%struk(i)%esse   = 0
       nano_iav(j)%struk(i)%niav   = 0
       nano_iav(j)%struk(i)%natclu = 0
       nano_iav(j)%struk(i)%rho    = 0.0_DP
       nano_iav(j)%struk(i)%delta  = 0.0_DP
       nano_iav(j)%struk(i)%widg   = 0.0_DP
       nano_iav(j)%struk(i)%cnorg  = 0.0_DP
       nano_iav(j)%struk(i)%clu_mass  = 0.0_DP
     enddo
   enddo

 END SUBROUTINE TO_DB_READERS

END MODULE NANO_TYPES
!_____________________________________________________________________________________________________
module DB2_read
 USE NANO_TYPES
 use INPUT_DATA, only: PATH_NAME,nstr, DB_INDEX, ITYPE_DB,n2read
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: DB2_READER
 
contains

!***********************************************************************************************
 SUBROUTINE DB2_READER

   IMPLICIT NONE
   INTEGER(I4B)                               :: i,ii,iki,n,nki, i_astat, ndist,nat, nox,kox,ll, j,jj, ilogerr, iloginp, &
                                                 ixdist,cknat,kline, jjj, nasp
   CHARACTER(2)                               :: anumsh
   CHARACTER(1024)                            :: rline,nanofina
   character(128), dimension(NPH_DB2)         :: NANOPATH_DB2
   REAL(CP)                                   :: xdumm,xdold,xdist,xmult,dx,rat


   nmaxsize = MAX(nIKOSA,nCUBOK,nDEKAH,nDEKAB)

   finp_names = (/'CUBOK', 'IKOSA', 'DEKAH', 'DEKAB'/)

   ilogerr = FIND_UNIT()
   OPEN(ilogerr,status='replace',file='DB2_read.err')   !!!! Vedere

   NANOPATH_DB2 = '' 
   do i=1,nstr
      if (DB_INDEX(i) /= 2) cycle
      ll = LEN_TRIM(NANOPATH_DB2(ITYPE_DB(i)))
      if (ll == 0) THEN
          NANOPATH_DB2(ITYPE_DB(i)) = PATH_NAME(i)   
      else
          print'(1x,a,i5)','too many PATH for structure %',i
          STOP
      endif
   enddo

   iki=0
   jj = NPH_DB1
   
   do ii=1,nstr
     if (DB_INDEX(ii) /= 2) cycle
     iki=ITYPE_DB(ii)
     if (iki>NPH_DB2) stop 'big mistake: iki>NPH_DB2)'
     jj = jj + 1
     do i=1,nano_iav(jj)%dimstruk 
       write(anumsh,'(i2.2)') i
       nanofina = TRIM(ADJUSTL(nanopath_DB2(iki)))//finp_names(iki)(:)//anumsh//'.nano'
       iloginp  = FIND_UNIT()
       open(iloginp,status='old',action='read',file=TRIM(nanofina))

!___________________ READ HEADER-
       read(iloginp,'(a)') rline
       read(rline,*,iostat=i_astat) nano_iav(jj)%struk(i)%esse,  &
                                    nano_iav(jj)%struk(i)%natclu,&
                                    nano_iav(jj)%struk(i)%widg,  &
                                    nano_iav(jj)%struk(i)%Delta, &
                                    nano_iav(jj)%struk(i)%cnorg   ! THIS IS 1/(rho*sqrt(2*Pi))
       nano_iav(jj)%struk(i)%rho = nano_iav(jj)%struk(i)%widg / nano_iav(jj)%struk(i)%Delta
       
       allocate(nano_iav(jj)%struk(i)%xnat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%Z_at(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%nat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occusite(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%DebyeWallerB(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occumatr(nano_iav(jj)%struk(i)%numspat,nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%ndi(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%zappa(2,nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%summul(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%occupair(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%termcon(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%termcon_allP(nano_iav(jj)%struk(i)%numpair))
!                nano_iav(jj)%struk(i)%poi_eq(nano_iav(jj)%struk(i)%numspat))
       nasp = nano_iav(jj)%struk(i)%numspat
       nano_iav(jj)%struk(i)%termcon = real(nano_iav(jj)%struk(i)%natclu,DP)
       !new
       nano_iav(jj)%struk(i)%occupair = 1.0d0
       nano_iav(jj)%struk(i)%occusite = 1.0d0
!       nano_iav(jj)%struk(i)%poi_eq = FILL_POI_EQ( nano_iav(jj)%struk(i)%numspat, nano_iav(jj)%struk(i)%numpair )
       nano_iav(jj)%struk(i)%termcon_allP = zero
       nano_iav(jj)%struk(i)%termcon_allP(Hot_stuff(nasp,1:nasp)) = nano_iav(jj)%struk(i)%termcon
       !new
       nano_iav(jj)%struk(i)%Z_at = Z_ATOM(1,jj)
       nano_iav(jj)%struk(i)%nat = nano_iav(jj)%struk(i)%natclu
       nano_iav(jj)%struk(i)%summul = 0.5_DP*real(nano_iav(jj)%struk(i)%natclu*(nano_iav(jj)%struk(i)%natclu-1),DP)
       nano_iav(jj)%struk(i)%zappa(:,1) = (/Z_ATOM(1,jj),Z_ATOM(1,jj)/)
       nano_iav(jj)%struk(i)%ndi = NINT(nano_iav(jj)%struk(i)%summul)
       
       jjj = 1
       nano_iav(jj)%struk(i)%xnat(jjj) = real(nano_iav(jj)%struk(i)%nat(jjj),DP)
         nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
         nano_iav(jj)%struk(i)%xnat(jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))  

!__________________ COUNT DISTANCES (precautional)
       kline=0
       do
         read(iloginp,*,end=1111,err=1111)ixdist,xdold
         kline=kline+1
       enddo
       1111 rewind(iloginp)
       read(iloginp,'(a)') rline
       IF (kline /= nano_iav(jj)%struk(i)%esse) nano_iav(jj)%struk(i)%esse = kline

!__________________ READ DISTANCES

       ALLOCATE(nano_iav(jj)%struk(i)%pseudomult(0:nano_iav(jj)%struk(i)%esse,nano_iav(jj)%struk(i)%numpair),stat=i_astat)
       IF (i_astat /= 0) THEN
         WRITE(ilogerr,*)' FATAL-Allocation of TYPE(nanoiav) failed ***',i,jj,nano_iav(jj)%struk(i)%esse
         STOP
       ENDIF

       nano_iav(jj)%struk(i)%pseudomult(0,1) = 0.0_DP
       do j=1,nano_iav(jj)%struk(i)%esse
         read(iloginp,*) ixdist,xdumm
         nano_iav(jj)%struk(i)%pseudomult(j,1) = xdumm/REAL(j,DP)
         IF (ixdist /= j) then
           WRITE(ilogerr,'(a,a)')' FATAL-trouble reading file ',TRIM(nanofina)
           STOP
         ENDIF
       enddo
       close(iloginp)

       if (iki< 3) then
         cknat = 1+((i*(11+i*(15+i*10)))/3)
       else if (iki==3) then
         cknat = 1+((i*(16+i*(15+i*5)))/6)
       else if (iki==4) then
         cknat = 1+((i*(-14+i*(15+i*5)))/6)
       endif
       IF (cknat /= nano_iav(jj)%struk(i)%natclu) THEN
         write(ilogerr,'(1x,a,2i4,2i10)') 'Incorrect n. of atoms', i,jj,cknat,nano_iav(jj)%struk(i)%natclu
         STOP 'Incorrect n. of atoms'
       endif
     enddo
   enddo

 END SUBROUTINE DB2_READER

END module DB2_read
!_______________________________________________________________________________
module DB1_read
 USE NANO_TYPES
 use INPUT_DATA, only: PATH_NAME,nstr, DB_INDEX, ITYPE_DB, ALL_PHA_INFO, n2read
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: DB1_READER  !, nmax_sampl,sampl_step


 contains


 SUBROUTINE DB1_READER

   IMPLICIT NONE
   INTEGER(I4B)                               :: i,iki,n,nki, i_astat, ndist,nat, nox,kox,ll, j,jj,jjj, &
                                                 ilogerr, iloginp, ixdist,cknat,kline,iloginp2,k,k2,&
                                                 lrline,ios,jjj2,kou, nasp,nrpha,ngdim,&
                                                 celcen,natspglo,kr,ind
   INTEGER(I4B)                               :: kdb1
   CHARACTER(2)                               :: anumsh
   CHARACTER(1024)                             :: rline,nanofina,nanofina2
   character(256),dimension(:), allocatable   :: NANOPATH_DB1
   REAL(CP)                                   :: xdumm,xdold,xdist,xmult,dx,rat,celly,xocc(100),xocc2(100),xrat,&
                                                 celvol,celvolr
   REAL(CP),dimension(:), allocatable         :: rescamul
   REAL(DP),dimension(:), allocatable         :: size_abx,ncelpha,specXN
   INTEGER(I4B),dimension(:), allocatable     :: specind,specZ,specN


   if (ALLOCATED(NANOPATH_DB1)) deallocate(NANOPATH_DB1)
   ALLOCATE(NANOPATH_DB1(NPH_DB1))
   NANOPATH_DB1 = ''
   if (ALLOCATED(NCLUST_DB1)) deallocate(NCLUST_DB1)
   ALLOCATE(NCLUST_DB1(NPH_DB1))

   NCLUST_DB1 = 0

   ilogerr = FIND_UNIT()
   OPEN(ilogerr,status='replace',file='DB1_read.err')   

   NANOPATH_DB1 = ''
   kdb1=0
   nactsize = 0
   do i=1,nstr
      if (DB_INDEX(i) /= 1) cycle
      kdb1=kdb1+1
      ll = LEN_TRIM(NANOPATH_DB1(kdb1))
      if (ll == 0) THEN
          NANOPATH_DB1(kdb1) = PATH_NAME(i)   
      else
          print'(1x,a,i5)','too many PATH for structure %',i
          STOP
      endif
      NCLUST_DB1(kdb1) = n2read(i)
   enddo

   jj = 0
   
   iki = 0 
   do j=1,nstr
     if (DB_INDEX(j) /= 1) cycle
     iki = iki + 1
     jj = jj + 1
     if (iki > NPH_DB1) stop 'big mistake: jj>NPH_DB1)'
     do i=1,n2read(j)
       nanofina  = TRIM(ADJUSTL(nanopath_DB1(iki)))//'.smp'
       nanofina2 = TRIM(ADJUSTL(nanopath_DB1(iki)))//'.smp_INFO'
       iloginp  = FIND_UNIT() 
       open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB1 file: ',trim(nanofina)
           STOP
       endif
       iloginp2  = FIND_UNIT()
       open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB1 file: ',trim(nanofina2)
           STOP
       endif
       read(iloginp2,*)nano_iav(jj)%struk(i)%esse, nano_iav(jj)%struk(i)%numspat, &
                       nano_iav(jj)%struk(i)%numpair, celly, nano_iav(jj)%struk(i)%rho, &
                       nano_iav(jj)%struk(i)%delta
!       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR1',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
!       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
        
       allocate(nano_iav(jj)%struk(i)%xnat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%Z_at(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%nat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occusite(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%DebyeWallerB(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occumatr(nano_iav(jj)%struk(i)%numspat,nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%ndi(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%zappa(2,nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%summul(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%occupair(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%termcon(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%termcon_allP(nano_iav(jj)%struk(i)%numpair))
!                nano_iav(jj)%struk(i)%poi_eq(nano_iav(jj)%struk(i)%numspat))
       nasp = nano_iav(jj)%struk(i)%numspat
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%Z_at(jjj),nano_iav(jj)%struk(i)%nat(jjj),nano_iav(jj)%struk(i)%xnat(jjj),&
                         nano_iav(jj)%struk(i)%termcon(jjj)
!         xocc(jjj) = nano_iav(jj)%struk(i)%xnat(jjj) / nano_iav(jj)%struk(i)%nat(jjj)
!         nano_iav(jj)%struk(i)%xnat(jjj) = nano_iav(jj)%struk(i)%nat(jjj) 
!         xocc(jjj) = 1.0d0/xocc(jjj)
!         nano_iav(jj)%struk(i)%termcon(jjj) = nano_iav(jj)%struk(i)%termcon(jjj) * (xocc(jjj)**2)
       enddo
       nano_iav(jj)%struk(i)%natclu = MAXVAL(nano_iav(jj)%struk(i)%nat)
       
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         nano_iav(jj)%struk(i)%occusite(jjj) = nano_iav(jj)%struk(i)%xnat(jjj) / nano_iav(jj)%struk(i)%nat(jjj)
       enddo
       
       !new
!       nano_iav(jj)%struk(i)%poi_eq = FILL_POI_EQ( nano_iav(jj)%struk(i)%numspat, nano_iav(jj)%struk(i)%numpair )
       nano_iav(jj)%struk(i)%termcon_allP = zero
       nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp)) = nano_iav(jj)%struk(i)%termcon
       !new
       
       do jjj=1,nano_iav(jj)%struk(i)%numspat
    !!     nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
    !!       nano_iav(jj)%struk(i)%nat(jjj)*ALL_PHA_INFO(j)%pha_xyzbo(5,jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))
          nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
            nano_iav(jj)%struk(i)%nat(jjj)*nano_iav(jj)%struk(i)%occusite(jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))
!         ALL_PHA_INFO(j)%pha_xyzbo(5,jjj) = nano_iav(jj)%struk(i)%occusite(jjj)
       enddo

       do jjj=1,nano_iav(jj)%struk(i)%numpair
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%ndi(jjj),nano_iav(jj)%struk(i)%zappa(:,jjj),nano_iav(jj)%struk(i)%summul(jjj)
       enddo
       read(iloginp2,'(a)')rline
       rline=trim(adjustl(rline))
       read(rline(5:),*) nano_iav(jj)%struk(i)%qtop
       
       kou=0
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         do jjj2=jjj,nano_iav(jj)%struk(i)%numspat
           kou=kou+1
           nano_iav(jj)%struk(i)%occupair(kou) = nano_iav(jj)%struk(i)%occusite(jjj) * nano_iav(jj)%struk(i)%occusite(jjj2)
           nano_iav(jj)%struk(i)%occumatr(jjj,jjj2) = kou
           nano_iav(jj)%struk(i)%occumatr(jjj2,jjj) = kou
         enddo
       enddo
       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%termcon_allP
           nano_iav(jj)%struk(i)%termcon = nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp))
         endif
       endif
       !new
       
       jjj=0
       do k=1,nano_iav(jj)%struk(i)%numspat
         do k2=k,nano_iav(jj)%struk(i)%numspat
           jjj=jjj+1
           ! 23.02.2015 nano_iav(jj)%struk(i)%zappa(:,jjj) = (/ALL_PHA_INFO(j)%pha_Z(k),ALL_PHA_INFO(j)%pha_Z(k2)/)
           nano_iav(jj)%struk(i)%zappa(:,jjj) = [ nano_iav(jj)%struk(i)%Z_at(k),  nano_iav(jj)%struk(i)%Z_at(k2) ]
         enddo
       enddo
       
       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%nrpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ngdim
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%size_abx(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%size_abx
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%act_diam(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%act_diam
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%ncelpha(nano_iav(jj)%struk(i)%nrpha))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ncelpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%celcen,nano_iav(jj)%struk(i)%celvol,nano_iav(jj)%struk(i)%celvolr
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%natspglo
         endif
       endif       
       allocate(nano_iav(jj)%struk(i)%specind(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specZ(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha),&
                nano_iav(jj)%struk(i)%specXN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha))
       do kr=1,nano_iav(jj)%struk(i)%natspglo
         read(iloginp2,'(a)',iostat=ios)rline
         if (ios==0) then
           rline=trim(adjustl(rline))
           lrline=len_trim(rline)
           if (lrline>0) then
             read(rline,*) nano_iav(jj)%struk(i)%specind(kr),nano_iav(jj)%struk(i)%specZ(kr),&
                           nano_iav(jj)%struk(i)%specN(kr,:),nano_iav(jj)%struk(i)%specXN(kr,:)
           endif
         endif
       enddo       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%abcabg_db
         endif
       endif

       close(iloginp2)
       
       ALL_PHA_INFO(jj)%acell_PROT = nano_iav(jj)%struk(i)%abcabg_db(1)
       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR1',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       read(iloginp,*)
       allocate(nano_iav(jj)%struk(i)%pseudomult(nano_iav(jj)%struk(i)%esse,nano_iav(jj)%struk(i)%numpair))
       do k=1,nano_iav(jj)%struk(i)%numpair
         do k2=1,nano_iav(jj)%struk(i)%esse
           read(iloginp,*)xdumm
!           nano_iav(jj)%struk(i)%pseudomult(k2,k) = rescamul(k)*xdumm/REAL(k2,DP) ! again we read the mu(k)/k
           nano_iav(jj)%struk(i)%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
         enddo
       enddo
       close(iloginp)
     enddo
   enddo
 END SUBROUTINE DB1_READER

END module DB1_read
!_______________________________________________________________________________
module DB3_read
 USE NANO_TYPES
 use INPUT_DATA, only: PATH_NAME,nstr, DB_INDEX, ITYPE_DB, ALL_PHA_INFO, n2read
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: DB3_READER

 contains

 SUBROUTINE DB3_READER

   IMPLICIT NONE
   INTEGER(I4B)                               :: i,iki,n,nki, i_astat, ndist,nat, nox,kox,ll, j,jj,jjj, &
                                                 ilogerr, iloginp, ixdist,cknat,kline,iloginp2,k,k2,&
                                                 lrline,ios,jjj2,kou, iiaa, nasp, i_sph,nrpha,ngdim,&
                                                 celcen,natspglo,kr,ind
   INTEGER(I4B)                               :: kdb3,lenan3
   CHARACTER(3)                               :: anumsh
   CHARACTER(1024)                            :: rline,nanofina,nanofina2
   character(256),dimension(:), allocatable   :: NANOPATH_DB3
   REAL(CP)                                   :: xdumm,xdold,xdist,xmult,dx,rat,celly,xocc(100),xocc2(100),xrat,&
                                                 celvol,celvolr
   REAL(CP),dimension(:), allocatable         :: rescamul
   REAL(DP),dimension(:), allocatable         :: size_abx,ncelpha,specXN
   INTEGER(I4B),dimension(:), allocatable     :: specind,specZ,specN

   if (ALLOCATED(NANOPATH_DB3)) deallocate(NANOPATH_DB3)
   ALLOCATE(NANOPATH_DB3(NPH_DB3))
   NANOPATH_DB3 = ''
   if (ALLOCATED(NCLUST_DB3)) deallocate(NCLUST_DB3)
   ALLOCATE(NCLUST_DB3(NPH_DB3))

   NCLUST_DB3 = 0

   ilogerr = FIND_UNIT()
   OPEN(ilogerr,status='replace',file='DB3_read.err')   

   NANOPATH_DB3 = ''
   kdb3=0
   nactsize = 0
   do i=1,nstr
      if (DB_INDEX(i) /= 3) cycle
      kdb3=kdb3+1
      ll = LEN_TRIM(NANOPATH_DB3(kdb3))
      if (ll == 0) THEN
          NANOPATH_DB3(kdb3) = PATH_NAME(i)   
      else
          print'(1x,a,i5)','too many PATH for structure %',i
          STOP
      endif
      NCLUST_DB3(kdb3) = n2read(i)
   enddo


   jj = NPH_DB2_USED + NPH_DB1
   
   iki = 0 
   do j=1,nstr
     if (DB_INDEX(j) /= 3) cycle
     iki = iki + 1
     jj = jj + 1
     if (iki > NPH_DB3) stop 'big mistake: jj>NPH_DB3)'

     i=1
     i_sph = 0
       lenan3 = 3
       write(anumsh,'(i3.3)') i
       nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh//'.smp'
       iloginp  = FIND_UNIT() 
       open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
       if (ios /= 0) then
           nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh//'_SPH.smp'
           iloginp  = FIND_UNIT() 
           open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
           i_sph = 1           
           if (ios /= 0) then 
               nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh//'_QBE.smp'
               iloginp  = FIND_UNIT() 
               open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
               i_sph = 2          
               if (ios /= 0) then
                  i_sph = 0
                  write(anumsh(1:2),'(i2.2)') i
                  nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:2)//'.smp'
                  iloginp  = FIND_UNIT() 
                  open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
                  if (ios /= 0) then
                     print*, 'Error opening DB3 file 1: ',trim(nanofina)
                     STOP
                  ELSE     
                     lenan3 = 2
                  endif
               endif
           endif
       endif
       close(iloginp)
 
     do i=1,n2read(j)
       if (lenan3 == 3) then
          write(anumsh(1:lenan3),'(i3.3)') i
       ELSE
          write(anumsh(1:lenan3),'(i2.2)') i
       endif 

       if (i_sph == 0) THEN
          nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'.smp'
          nanofina2 = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'.smp_INFO'
       else
         if (i_sph == 1) THEN
          nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'_SPH.smp'
          nanofina2 = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'_SPH.smp_INFO'
         else 
          nanofina  = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'_QBE.smp'
          nanofina2 = TRIM(ADJUSTL(nanopath_DB3(iki)))//anumsh(1:lenan3)//'_QBE.smp_INFO'
         endif
       endif
       iloginp  = FIND_UNIT()
       open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB3 file 2: ',trim(nanofina)
           STOP
       endif
       iloginp2  = FIND_UNIT()
       open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB3 file: ',trim(nanofina2)
           STOP
       endif
       read(iloginp2,*)nano_iav(jj)%struk(i)%esse, nano_iav(jj)%struk(i)%numspat, &
                       nano_iav(jj)%struk(i)%numpair, celly, nano_iav(jj)%struk(i)%rho, &
                       nano_iav(jj)%struk(i)%delta
!       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR3',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
!       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
        
       allocate(nano_iav(jj)%struk(i)%xnat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%Z_at(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%nat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occusite(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%DebyeWallerB(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occumatr(nano_iav(jj)%struk(i)%numspat,nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%ndi(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%zappa(2,nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%summul(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%occupair(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%termcon(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%termcon_allP(nano_iav(jj)%struk(i)%numpair))
!                nano_iav(jj)%struk(i)%poi_eq(nano_iav(jj)%struk(i)%numspat))
       nasp = nano_iav(jj)%struk(i)%numspat
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%Z_at(jjj),nano_iav(jj)%struk(i)%nat(jjj),nano_iav(jj)%struk(i)%xnat(jjj),&
                         nano_iav(jj)%struk(i)%termcon(jjj)
       enddo
       nano_iav(jj)%struk(i)%natclu = MAXVAL(nano_iav(jj)%struk(i)%nat)
       
       !new
!       nano_iav(jj)%struk(i)%poi_eq = FILL_POI_EQ( nano_iav(jj)%struk(i)%numspat, nano_iav(jj)%struk(i)%numpair )
       nano_iav(jj)%struk(i)%termcon_allP = zero
       nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp)) = nano_iav(jj)%struk(i)%termcon
       !new
       
!       do jjj=1,nano_iav(jj)%struk(i)%numspat
!          nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
!          nano_iav(jj)%struk(i)%nat(jjj)*ALL_PHA_INFO(j)%pha_xyzbo(5,jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))
!       enddo

   
       do jjj=1,nano_iav(jj)%struk(i)%numpair
        read(iloginp2,*)kox,nano_iav(jj)%struk(i)%ndi(jjj),nano_iav(jj)%struk(i)%zappa(:,jjj),&
                nano_iav(jj)%struk(i)%summul(jjj)
       enddo   
       
       read(iloginp2,'(a)')rline
       rline=trim(adjustl(rline))
       read(rline(5:),*) nano_iav(jj)%struk(i)%qtop
       
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         nano_iav(jj)%struk(i)%occusite(jjj) = nano_iav(jj)%struk(i)%xnat(jjj) / nano_iav(jj)%struk(i)%nat(jjj)
       enddo
       kou=0
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         do jjj2=jjj,nano_iav(jj)%struk(i)%numspat
           kou=kou+1
           nano_iav(jj)%struk(i)%occupair(kou) = nano_iav(jj)%struk(i)%occusite(jjj) * nano_iav(jj)%struk(i)%occusite(jjj2)
           nano_iav(jj)%struk(i)%occumatr(jjj,jjj2) = kou
           nano_iav(jj)%struk(i)%occumatr(jjj2,jjj) = kou
         enddo
       enddo  


       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%termcon_allP
           nano_iav(jj)%struk(i)%termcon = nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp))
         endif
       endif
       !new
       
       jjj=0
       do k=1,nano_iav(jj)%struk(i)%numspat
         do k2=k,nano_iav(jj)%struk(i)%numspat
           jjj=jjj+1
           ! 23.02.2015 nano_iav(jj)%struk(i)%zappa(:,jjj) = (/ALL_PHA_INFO(j)%pha_Z(k),ALL_PHA_INFO(j)%pha_Z(k2)/)
           nano_iav(jj)%struk(i)%zappa(:,jjj) = [ nano_iav(jj)%struk(i)%Z_at(k), nano_iav(jj)%struk(i)%Z_at(k2) ]
         enddo
       enddo
       
       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%nrpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ngdim
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%size_abx(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%size_abx
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%act_diam(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%act_diam
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%ncelpha(nano_iav(jj)%struk(i)%nrpha))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ncelpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%celcen,nano_iav(jj)%struk(i)%celvol,nano_iav(jj)%struk(i)%celvolr
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%natspglo
         endif
       endif       
       allocate(nano_iav(jj)%struk(i)%specind(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specZ(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha),&
                nano_iav(jj)%struk(i)%specXN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha))
       do kr=1,nano_iav(jj)%struk(i)%natspglo
         read(iloginp2,'(a)',iostat=ios)rline
         if (ios==0) then
           rline=trim(adjustl(rline))
           lrline=len_trim(rline)
           if (lrline>0) then
             read(rline,*) nano_iav(jj)%struk(i)%specind(kr),nano_iav(jj)%struk(i)%specZ(kr),&
                           nano_iav(jj)%struk(i)%specN(kr,:),nano_iav(jj)%struk(i)%specXN(kr,:)
           endif
         endif
       enddo       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%abcabg_db
         endif
       endif
       
       close(iloginp2)
       
       ALL_PHA_INFO(jj)%acell_PROT = nano_iav(jj)%struk(i)%abcabg_db(1)
       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR3',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       
       read(iloginp,*)
       allocate(nano_iav(jj)%struk(i)%pseudomult(nano_iav(jj)%struk(i)%esse,nano_iav(jj)%struk(i)%numpair))
       do k=1,nano_iav(jj)%struk(i)%numpair
         do k2=1,nano_iav(jj)%struk(i)%esse
           read(iloginp,*)xdumm
!           nano_iav(jj)%struk(i)%pseudomult(k2,k) = rescamul(k)*xdumm/REAL(k2,DP) ! again we read the mu(k)/k
           nano_iav(jj)%struk(i)%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
         enddo
       enddo
       close(iloginp)
     enddo
   enddo
 END SUBROUTINE DB3_READER

END module DB3_read

!_______________________________________________________________________________
module DB4_read
 USE NANO_TYPES
 use INPUT_DATA, only: PATH_NAME,nstr, DB_INDEX, ITYPE_DB, ALL_PHA_INFO, n2read_ab, n2read_c
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: DB4_READER 

contains
 
 SUBROUTINE DB4_READER

   IMPLICIT NONE
   INTEGER(I4B)                               :: i,iki,n,nki, i_astat, ndist,nat, nox,kox,ll, j,jj,jjj, &
                                                 ilogerr, iloginp, ixdist,cknat,kline,iloginp2,k,k2,kk,ndimax,&
                                                 lrline,ios,jjj2,kou, nasp,nrpha,ngdim,celcen,natspglo,kr,ind
   INTEGER(I4B)                               :: kdb4, klin(2)
   CHARACTER(5)                               :: anumsh,cnumsh
   CHARACTER(4)                               :: clushap
   CHARACTER(1024)                             :: rline,nanofina,nanofina2
   character(256),dimension(:), allocatable   :: NANOPATH_DB4
   REAL(CP)                                   :: xdumm,xdold,xdist,xmult,dx,rat,celly,xocc(100),xocc2(100),xrat,&
                                                 celvol,celvolr
   REAL(CP),dimension(:), allocatable         :: rescamul
   REAL(DP),dimension(:), allocatable         :: size_abx,ncelpha,specXN
   INTEGER(I4B),dimension(:), allocatable     :: specind,specZ,specN



   if (ALLOCATED(NANOPATH_DB4)) deallocate(NANOPATH_DB4)
   ALLOCATE(NANOPATH_DB4(NPH_DB4))
   NANOPATH_DB4 = ''
   if (ALLOCATED(NCLUST_DB4)) deallocate(NCLUST_DB4)
   ALLOCATE(NCLUST_DB4(NPH_DB4))

   NCLUST_DB4 = 0

   ilogerr = FIND_UNIT()
   OPEN(ilogerr,status='replace',file='DB4_read.err')   

   NANOPATH_DB4 = ''
   kdb4=0
   nactsize = 0
   do i=1,nstr
      if (DB_INDEX(i) /= 4) cycle
      kdb4=kdb4+1
      NANOPATH_DB4(kdb4) = PATH_NAME(i)  
   enddo


   anumsh(1:2)='_a'
   cnumsh(1:2)='_c'
   clushap(1:1)='_'
   
   jj = NPH_DB1 + NPH_DB2_USED + NPH_DB3
   iki=0
   do j=1,nstr
     if (DB_INDEX(j) /= 4) cycle
     iki = iki + 1
     jj = jj + 1
     if (iki > NPH_DB4) stop 'big mistake: jj>NPH_DB4)'
     nano_iav(jj)%dimstruk1 = n2read_ab(j,2)
     nano_iav(jj)%dimstruk2 = n2read_c(j,2)
     ALLOCATE(nano_iav(jj)%post_office(nano_iav(jj)%dimstruk1,nano_iav(jj)%dimstruk2), &
              nano_iav(jj)%post_office_I(2,nano_iav(jj)%dimstruk))
     do i=1,n2read(j)
       klin = TWO_FM_ONE(i,n2read_ab(j,2),n2read_c(j,2))
       if (klin(1) < n2read_ab(j,1) .or. klin(2) < n2read_c(j,1)) then
           cycle
       endif    
       if (klin(1) > n2read_ab(j,2) .or. klin(2) > n2read_c(j,2)) then
           STOP 'ERROR in DB4, deriving two indices from one'
       endif
       write(anumsh(3:5),'(i3.3)') klin(1)
       write(cnumsh(3:5),'(i3.3)') klin(2)
       clushap(2:4) = ALL_PHA_INFO(j)%clushape
       nanofina  = TRIM(ADJUSTL(nanopath_DB4(iki)))//anumsh//cnumsh//clushap//'.smp'
       nanofina2 = TRIM(ADJUSTL(nanopath_DB4(iki)))//anumsh//cnumsh//clushap//'.smp_INFO'
       
       if (ONE_FM_TWO(klin(1),klin(2),n2read_ab(j,2),n2read_c(j,2)) /= i) then
           print*, ' k, l indices : ', klin,i,&
                    ONE_FM_TWO(klin(1),klin(2),n2read_ab(j,2),n2read_c(j,2))
           STOP 'NEW ERROR in DB4, reading 2 indices'
       endif
       
       nano_iav(jj)%post_office_I(:,i) = klin
       nano_iav(jj)%post_office(klin(1),klin(2)) = i
      
       iloginp  = FIND_UNIT()
       open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB4 file: ',trim(nanofina)
           STOP
       endif
       iloginp2  = FIND_UNIT()
       open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB4 file: ',trim(nanofina2)
           STOP
       endif
       read(iloginp2,*)nano_iav(jj)%struk(i)%esse, nano_iav(jj)%struk(i)%numspat, &
                       nano_iav(jj)%struk(i)%numpair, celly, nano_iav(jj)%struk(i)%rho, &
                       nano_iav(jj)%struk(i)%delta
!       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR4',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
!       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       allocate(nano_iav(jj)%struk(i)%xnat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%Z_at(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%nat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occusite(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%DebyeWallerB(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occumatr(nano_iav(jj)%struk(i)%numspat,nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%ndi(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%zappa(2,nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%summul(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%occupair(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%termcon(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%termcon_allP(nano_iav(jj)%struk(i)%numpair))
!                nano_iav(jj)%struk(i)%poi_eq(nano_iav(jj)%struk(i)%numspat))

       nasp = nano_iav(jj)%struk(i)%numspat
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%Z_at(jjj),nano_iav(jj)%struk(i)%nat(jjj),nano_iav(jj)%struk(i)%xnat(jjj),&
                         nano_iav(jj)%struk(i)%termcon(jjj)
       enddo
       nano_iav(jj)%struk(i)%natclu = MAXVAL(nano_iav(jj)%struk(i)%nat)
       
!       nano_iav(jj)%struk(i)%poi_eq = FILL_POI_EQ( nano_iav(jj)%struk(i)%numspat, nano_iav(jj)%struk(i)%numpair )
       nano_iav(jj)%struk(i)%termcon_allP = zero
       nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp)) = nano_iav(jj)%struk(i)%termcon
       !new
       
!       do jjj=1,nano_iav(jj)%struk(i)%numspat
!         nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
!           nano_iav(jj)%struk(i)%nat(jjj)*ALL_PHA_INFO(j)%pha_xyzbo(5,jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))
!       enddo
       
       do jjj=1,nano_iav(jj)%struk(i)%numpair
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%ndi(jjj),nano_iav(jj)%struk(i)%zappa(:,jjj),nano_iav(jj)%struk(i)%summul(jjj)
       enddo
       read(iloginp2,'(a)')rline
       rline=trim(adjustl(rline))
       read(rline(5:),*) nano_iav(jj)%struk(i)%qtop

       do jjj=1,nano_iav(jj)%struk(i)%numspat
         nano_iav(jj)%struk(i)%occusite(jjj) = nano_iav(jj)%struk(i)%xnat(jjj) / nano_iav(jj)%struk(i)%nat(jjj)
       enddo
       kou=0
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         do jjj2=jjj,nano_iav(jj)%struk(i)%numspat
           kou=kou+1
           nano_iav(jj)%struk(i)%occupair(kou) = nano_iav(jj)%struk(i)%occusite(jjj) * nano_iav(jj)%struk(i)%occusite(jjj2)
           nano_iav(jj)%struk(i)%occumatr(jjj,jjj2) = kou
           nano_iav(jj)%struk(i)%occumatr(jjj2,jjj) = kou
         enddo
       enddo

       !new
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%termcon_allP
           nano_iav(jj)%struk(i)%termcon = nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp))
         endif
       endif
       
       jjj=0
       do k=1,nano_iav(jj)%struk(i)%numspat
         do k2=k,nano_iav(jj)%struk(i)%numspat
           jjj=jjj+1
           ! 23.02.2015 nano_iav(jj)%struk(i)%zappa(:,jjj) = (/ALL_PHA_INFO(j)%pha_Z(k),ALL_PHA_INFO(j)%pha_Z(k2)/)
           nano_iav(jj)%struk(i)%zappa(:,jjj) = [ nano_iav(jj)%struk(i)%Z_at(k), nano_iav(jj)%struk(i)%Z_at(k2) ]
         enddo
       enddo

       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%nrpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ngdim
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%size_abx(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%size_abx
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%act_diam(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%act_diam
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%ncelpha(nano_iav(jj)%struk(i)%nrpha))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ncelpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%celcen,nano_iav(jj)%struk(i)%celvol,nano_iav(jj)%struk(i)%celvolr
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%natspglo
         endif
       endif       
       allocate(nano_iav(jj)%struk(i)%specind(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specZ(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha),&
                nano_iav(jj)%struk(i)%specXN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha))
       do kr=1,nano_iav(jj)%struk(i)%natspglo
         read(iloginp2,'(a)',iostat=ios)rline
         if (ios==0) then
           rline=trim(adjustl(rline))
           lrline=len_trim(rline)
           if (lrline>0) then
             read(rline,*) nano_iav(jj)%struk(i)%specind(kr),nano_iav(jj)%struk(i)%specZ(kr),&
                           nano_iav(jj)%struk(i)%specN(kr,:),nano_iav(jj)%struk(i)%specXN(kr,:)
           endif
         endif
       enddo
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%abcabg_db
         endif
       endif

       close(iloginp2)
       
       ALL_PHA_INFO(jj)%acell_PROT = nano_iav(jj)%struk(i)%abcabg_db(1)
       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR4',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       read(iloginp,*)
       allocate(nano_iav(jj)%struk(i)%pseudomult(nano_iav(jj)%struk(i)%esse,nano_iav(jj)%struk(i)%numpair))
       do k=1,nano_iav(jj)%struk(i)%numpair
         do k2=1,nano_iav(jj)%struk(i)%esse
           read(iloginp,*)xdumm
           nano_iav(jj)%struk(i)%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
         enddo
       enddo
       close(iloginp)
     enddo
   enddo

 END SUBROUTINE DB4_READER

END module DB4_read
!_____________________________________________________________________________________________________________________________________________________
module DB5_read
 USE NANO_TYPES
 use INPUT_DATA, only: PATH_NAME,nstr, DB_INDEX, ITYPE_DB, ALL_PHA_INFO, n2read_ab, n2read_c
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: DB5_READER 

contains
 
 SUBROUTINE DB5_READER

   IMPLICIT NONE
   INTEGER(I4B)                               :: i,iki,n,nki, i_astat, ndist,nat, nox,kox,ll, j,jj,jjj, &
                                                 ilogerr, iloginp, ixdist,cknat,kline,iloginp2,k,k2,kk,ndimax,&
                                                 lrline,ios,jjj2,kou, nasp,nrpha,ngdim,celcen,natspglo,kr,ind
   INTEGER(I4B)                               :: kdb5, klin(2)
   CHARACTER(5)                               :: anumsh,cnumsh
   CHARACTER(4)                               :: clushap
   CHARACTER(1024)                            :: rline,nanofina,nanofina2
   character(256),dimension(:), allocatable   :: NANOPATH_DB5
   REAL(CP)                                   :: xdumm,xdold,xdist,xmult,dx,rat,celly,xocc(100),xocc2(100),xrat,&
                                                 celvol,celvolr
   REAL(CP),dimension(:), allocatable         :: rescamul
   REAL(DP),dimension(:), allocatable         :: size_abx,ncelpha,specXN
   INTEGER(I4B),dimension(:), allocatable     :: specind,specZ,specN

   if (ALLOCATED(NANOPATH_DB5)) deallocate(NANOPATH_DB5)
   ALLOCATE(NANOPATH_DB5(NPH_DB5))
   NANOPATH_DB5 = ''
   if (ALLOCATED(NCLUST_DB5)) deallocate(NCLUST_DB5)
   ALLOCATE(NCLUST_DB5(NPH_DB5))

   NCLUST_DB5 = 0

   ilogerr = FIND_UNIT()
   OPEN(ilogerr,status='replace',file='DB5_read.err')   

   NANOPATH_DB5 = ''
   kdb5=0
   nactsize = 0
   do i=1,nstr
      if (DB_INDEX(i) /= 5) cycle
      kdb5=kdb5+1
      NANOPATH_DB5(kdb5) = PATH_NAME(i)  
   enddo


   anumsh(1:2)='_k'
   cnumsh(1:2)='_s'
   clushap(1:1)='_'
   
   jj = NPH_DB1 + NPH_DB2_USED + NPH_DB3 + NPH_DB4
   iki=0
   do j=1,nstr
     if (DB_INDEX(j) /= 5) cycle
     iki = iki + 1
     jj = jj + 1
     if (iki > NPH_DB5) stop 'big mistake: jj>NPH_DB5)'
     nano_iav(jj)%dimstruk1 = n2read_ab(j,2)
     nano_iav(jj)%dimstruk2 = n2read_c(j,2)
     ALLOCATE(nano_iav(jj)%post_office(nano_iav(jj)%dimstruk1,nano_iav(jj)%dimstruk2), &
              nano_iav(jj)%post_office_I(2,nano_iav(jj)%dimstruk))
     do i=1,n2read(j)
       klin = TWO_FM_ONE(i,n2read_ab(j,2),n2read_c(j,2))
       if (klin(1) < n2read_ab(j,1) .or. klin(2) < n2read_c(j,1)) then
           cycle
       endif    
       if (klin(1) > n2read_ab(j,2) .or. klin(2) > n2read_c(j,2)) then
           STOP 'ERROR in DB5, deriving two indices from one'
       endif
       write(anumsh(3:5),'(i3.3)') klin(1)
       write(cnumsh(3:5),'(i3.3)') klin(2)
       clushap(2:4) = ALL_PHA_INFO(j)%clushape
       nanofina  = TRIM(ADJUSTL(nanopath_DB5(iki)))//anumsh//cnumsh//clushap//'.smp'
       nanofina2 = TRIM(ADJUSTL(nanopath_DB5(iki)))//anumsh//cnumsh//clushap//'.smp_INFO'
       
       if (ONE_FM_TWO(klin(1),klin(2),n2read_ab(j,2),n2read_c(j,2)) /= i) then
           print*, ' k, l indices : ', klin,i,&
                    ONE_FM_TWO(klin(1),klin(2),n2read_ab(j,2),n2read_c(j,2))
           STOP 'NEW ERROR in DB5, reading 2 indices'
       endif
       
       nano_iav(jj)%post_office_I(:,i) = klin
       nano_iav(jj)%post_office(klin(1),klin(2)) = i
       iloginp  = FIND_UNIT()
       open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB4 file: ',trim(nanofina)
           STOP
       endif
       iloginp2  = FIND_UNIT()
       open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
       if (ios /= 0) then
           print*, 'Error opening DB5 file: ',trim(nanofina2)
           STOP
       endif
       read(iloginp2,*)nano_iav(jj)%struk(i)%esse, nano_iav(jj)%struk(i)%numspat, &
                       nano_iav(jj)%struk(i)%numpair, celly, nano_iav(jj)%struk(i)%rho, &
                       nano_iav(jj)%struk(i)%delta
!       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR5',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
!       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
!       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       allocate(nano_iav(jj)%struk(i)%xnat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%Z_at(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%nat(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occusite(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%DebyeWallerB(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%occumatr(nano_iav(jj)%struk(i)%numspat,nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%ndi(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%zappa(2,nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%summul(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%occupair(nano_iav(jj)%struk(i)%numpair), &
                nano_iav(jj)%struk(i)%termcon(nano_iav(jj)%struk(i)%numspat), &
                nano_iav(jj)%struk(i)%termcon_allP(nano_iav(jj)%struk(i)%numpair))
!                nano_iav(jj)%struk(i)%poi_eq(nano_iav(jj)%struk(i)%numspat))

       nasp = nano_iav(jj)%struk(i)%numspat
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%Z_at(jjj),nano_iav(jj)%struk(i)%nat(jjj),nano_iav(jj)%struk(i)%xnat(jjj),&
                         nano_iav(jj)%struk(i)%termcon(jjj)
       enddo
       nano_iav(jj)%struk(i)%natclu = MAXVAL(nano_iav(jj)%struk(i)%nat)
       
!       nano_iav(jj)%struk(i)%poi_eq = FILL_POI_EQ( nano_iav(jj)%struk(i)%numspat, nano_iav(jj)%struk(i)%numpair )
       nano_iav(jj)%struk(i)%termcon_allP = zero
       nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp)) = nano_iav(jj)%struk(i)%termcon
       !new
       
!       do jjj=1,nano_iav(jj)%struk(i)%numspat
!         nano_iav(jj)%struk(i)%clu_mass = nano_iav(jj)%struk(i)%clu_mass + &
!           nano_iav(jj)%struk(i)%nat(jjj)*ALL_PHA_INFO(j)%pha_xyzbo(5,jjj)*atwei(nano_iav(jj)%struk(i)%Z_at(jjj))
!       enddo
       
       do jjj=1,nano_iav(jj)%struk(i)%numpair
         read(iloginp2,*)kox,nano_iav(jj)%struk(i)%ndi(jjj),nano_iav(jj)%struk(i)%zappa(:,jjj),nano_iav(jj)%struk(i)%summul(jjj)
       enddo
       read(iloginp2,'(a)')rline
       rline=trim(adjustl(rline))
       read(rline(5:),*) nano_iav(jj)%struk(i)%qtop

       do jjj=1,nano_iav(jj)%struk(i)%numspat
         nano_iav(jj)%struk(i)%occusite(jjj) = nano_iav(jj)%struk(i)%xnat(jjj) / nano_iav(jj)%struk(i)%nat(jjj)
       enddo
       kou=0
       do jjj=1,nano_iav(jj)%struk(i)%numspat
         do jjj2=jjj,nano_iav(jj)%struk(i)%numspat
           kou=kou+1
           nano_iav(jj)%struk(i)%occupair(kou) = nano_iav(jj)%struk(i)%occusite(jjj) * nano_iav(jj)%struk(i)%occusite(jjj2)
           nano_iav(jj)%struk(i)%occumatr(jjj,jjj2) = kou
           nano_iav(jj)%struk(i)%occumatr(jjj2,jjj) = kou
         enddo
       enddo

       !new
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%termcon_allP
           nano_iav(jj)%struk(i)%termcon = nano_iav(jj)%struk(i)%termcon_allP(Hot_Stuff(nasp,1:nasp))
         endif
       endif
       
       jjj=0
       do k=1,nano_iav(jj)%struk(i)%numspat
         do k2=k,nano_iav(jj)%struk(i)%numspat
           jjj=jjj+1
           ! 23.02.2015 nano_iav(jj)%struk(i)%zappa(:,jjj) = (/ALL_PHA_INFO(j)%pha_Z(k),ALL_PHA_INFO(j)%pha_Z(k2)/)
           nano_iav(jj)%struk(i)%zappa(:,jjj) = [ nano_iav(jj)%struk(i)%Z_at(k), nano_iav(jj)%struk(i)%Z_at(k2) ]
         enddo
       enddo
       
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%nrpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ngdim
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%size_abx(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%size_abx
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%act_diam(nano_iav(jj)%struk(i)%ngdim))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%act_diam
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           allocate(nano_iav(jj)%struk(i)%ncelpha(nano_iav(jj)%struk(i)%nrpha))
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%ncelpha
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%celcen,nano_iav(jj)%struk(i)%celvol,nano_iav(jj)%struk(i)%celvolr
         endif
       endif
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           ind = index(rline(1:lrline),' ')
           read(rline(ind:lrline),*) nano_iav(jj)%struk(i)%natspglo
         endif
       endif       
       allocate(nano_iav(jj)%struk(i)%specind(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specZ(nano_iav(jj)%struk(i)%natspglo),&
                nano_iav(jj)%struk(i)%specN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha),&
                nano_iav(jj)%struk(i)%specXN(nano_iav(jj)%struk(i)%natspglo, nano_iav(jj)%struk(i)%nrpha))
       do kr=1,nano_iav(jj)%struk(i)%natspglo
         read(iloginp2,'(a)',iostat=ios)rline
         if (ios==0) then
           rline=trim(adjustl(rline))
           lrline=len_trim(rline)
           if (lrline>0) then
             read(rline,*) nano_iav(jj)%struk(i)%specind(kr),nano_iav(jj)%struk(i)%specZ(kr),&
                           nano_iav(jj)%struk(i)%specN(kr,:),nano_iav(jj)%struk(i)%specXN(kr,:)
            endif
         endif
       enddo
       read(iloginp2,'(a)',iostat=ios)rline
       if (ios==0) then
         rline=trim(adjustl(rline))
         lrline=len_trim(rline)
         if (lrline>0) then
           read(rline(1:lrline),*) nano_iav(jj)%struk(i)%abcabg_db
         endif
       endif

       close(iloginp2)
       
       ALL_PHA_INFO(jj)%acell_PROT=nano_iav(jj)%struk(i)%abcabg_db(1)
       celly = ALL_PHA_INFO(jj)%acell_PROT
!       print*,'############## DCR5',nano_iav(jj)%struk(i)%delta,celly,nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%delta = nano_iav(jj)%struk(i)%delta/celly
       nano_iav(jj)%struk(i)%widg = nano_iav(jj)%struk(i)%delta*nano_iav(jj)%struk(i)%rho
       celly = ALL_PHA_INFO(jj)%cepa(1)/celly   ! scale factor for dist.s
       
       read(iloginp,*)
       allocate(nano_iav(jj)%struk(i)%pseudomult(nano_iav(jj)%struk(i)%esse,nano_iav(jj)%struk(i)%numpair))
       do k=1,nano_iav(jj)%struk(i)%numpair
         do k2=1,nano_iav(jj)%struk(i)%esse
           read(iloginp,*)xdumm
           nano_iav(jj)%struk(i)%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
         enddo
       enddo
       close(iloginp)
     enddo
   enddo

 END SUBROUTINE DB5_READER

END module DB5_read
!_____________________________________________________________________________________________________________________________________________________
module read_onesam
 USE NANO_TYPES
 use atomix, only: atwei
 PRIVATE
 PUBLIC :: THE_READER, THE_READER_EL, nano_iav1
 TYPE(nanoiav),save     :: nano_iav1

contains
 
SUBROUTINE THE_READER(fn)
IMPLICIT NONE
character(len=*),intent(IN) :: fn
INTEGER(I4B)                               :: i,n, j,jj,jjj,kox, &
                                              ilogerr, iloginp,iloginp2,k,k2,kk,lrline,ios,jjj2,kou, nasp
CHARACTER(512)                             :: rline,nanofina,nanofina2
REAL(CP)                                   :: xdumm,celly,xocc(100)


  ilogerr = FIND_UNIT()
  OPEN(ilogerr,status='replace',file='DB4_read.err')
  nanofina  = TRIM(ADJUSTL(fn))
  nanofina2 = TRIM(ADJUSTL(fn))//'_INFO'
     
  iloginp  = FIND_UNIT()
  open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
  if (ios /= 0) then
    print*, 'THE_READER: Error opening file: ',trim(nanofina)
    STOP
  endif
  iloginp2  = FIND_UNIT()
  open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
  if (ios /= 0) then
    print*, 'THE_READER: Error opening file: ',trim(nanofina2)
    STOP
  endif
  read(iloginp2,*)nano_iav1%esse, nano_iav1%numspat, &
                  nano_iav1%numpair, celly, nano_iav1%rho, &
                  nano_iav1%delta
  nano_iav1%widg = nano_iav1%delta*nano_iav1%rho
  
  allocate(nano_iav1%xnat(nano_iav1%numspat), &
           nano_iav1%Z_at(nano_iav1%numspat), &
           nano_iav1%nat(nano_iav1%numspat), &
           nano_iav1%occusite(nano_iav1%numspat), &
           nano_iav1%occumatr(nano_iav1%numspat,nano_iav1%numspat), &
           nano_iav1%ndi(nano_iav1%numpair), &
           nano_iav1%zappa(2,nano_iav1%numpair), &
           nano_iav1%summul(nano_iav1%numpair), &
           nano_iav1%occupair(nano_iav1%numpair), &
           nano_iav1%termcon(nano_iav1%numspat), &
           nano_iav1%termcon_allP(nano_iav1%numpair))
!           nano_iav1%poi_eq(nano_iav1%numspat))

  nasp = nano_iav1%numspat
  do jjj=1,nano_iav1%numspat
    read(iloginp2,*)kox,nano_iav1%Z_at(jjj),nano_iav1%nat(jjj),nano_iav1%xnat(jjj),&
                    nano_iav1%termcon(jjj)    
    xocc(jjj) = nano_iav1%xnat(jjj) / nano_iav1%nat(jjj)
    nano_iav1%occusite(jjj) = xocc(jjj)
  enddo
  nano_iav1%natclu = MAXVAL(nano_iav1%nat)
  
!  nano_iav1%poi_eq = FILL_POI_EQ( nano_iav1%numspat, nano_iav1%numpair )
  nano_iav1%termcon_allP = zero
  nano_iav1%termcon_allP(Hot_Stuff(nasp,1:nasp)) = nano_iav1%termcon
  !new
  
  do jjj=1,nano_iav1%numspat
    nano_iav1%clu_mass = nano_iav1%clu_mass + &
     nano_iav1%nat(jjj)*xocc(jjj)*atwei(nano_iav1%Z_at(jjj))
  enddo
  
  do jjj=1,nano_iav1%numpair
    read(iloginp2,*)kox,nano_iav1%ndi(jjj),nano_iav1%zappa(:,jjj),nano_iav1%summul(jjj)
  enddo
  read(iloginp2,'(a)')rline
  rline=trim(adjustl(rline))
  lrline=len_trim(rline)
  read(rline(5:lrline),*) nano_iav1%qtop
  kou=0
  do jjj=1,nano_iav1%numspat
    do jjj2=jjj,nano_iav1%numspat
      kou=kou+1
      nano_iav1%occupair(kou) = nano_iav1%occusite(jjj) * nano_iav1%occusite(jjj2)
      nano_iav1%occumatr(jjj,jjj2) = kou
      nano_iav1%occumatr(jjj2,jjj) = kou
    enddo
  enddo
  !new
  read(iloginp2,'(a)',iostat=ios)rline
  if (ios==0) then
    rline=trim(adjustl(rline))
    lrline=len_trim(rline)
    if (lrline>0) then
      read(rline(1:lrline),*) nano_iav1%termcon_allP
      nano_iav1%termcon_allP = nano_iav1%termcon_allP
      nano_iav1%termcon = nano_iav1%termcon_allP(Hot_Stuff(nasp,1:nasp))
    endif
  endif
  close(iloginp2)
  
  read(iloginp,*)
  allocate(nano_iav1%pseudomult(nano_iav1%esse,nano_iav1%numpair))
  do k=1,nano_iav1%numpair
    do k2=1,nano_iav1%esse
      read(iloginp,*)xdumm
      nano_iav1%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
    enddo
  enddo
  close(iloginp)

 END SUBROUTINE THE_READER
 
!**************************************************************************************************
 
SUBROUTINE THE_READER_EL(fn,eliav1)
IMPLICIT NONE
character(len=*),intent(IN) :: fn
TYPE(nanoiav),intent(INOUT)     :: eliav1
INTEGER(I4B)                               :: i,n, j,jj,jjj,kox, &
                                              ilogerr, iloginp,iloginp2,k,k2,kk,lrline,ios,jjj2,kou, nasp
CHARACTER(512)                             :: rline,nanofina,nanofina2
REAL(CP)                                   :: xdumm,celly,xocc(100)


  call destroy_iavS(rp=eliav1)

  ilogerr = FIND_UNIT()
  OPEN(ilogerr,status='replace',file='DB4_read.err')
  nanofina  = TRIM(ADJUSTL(fn))
  nanofina2 = TRIM(ADJUSTL(fn))//'_INFO'
     
  iloginp  = FIND_UNIT()
  open(iloginp,status='old',action='read',file=TRIM(nanofina),iostat=ios)
  if (ios /= 0) then
    print*, 'THE_READER: Error opening file: ',trim(nanofina)
    STOP
  endif
  iloginp2  = FIND_UNIT()
  open(iloginp2,status='old',action='read',file=TRIM(nanofina2),iostat=ios)
  if (ios /= 0) then
    print*, 'THE_READER: Error opening file: ',trim(nanofina2)
    STOP
  endif
  read(iloginp2,*)eliav1%esse, eliav1%numspat, &
                  eliav1%numpair, celly, eliav1%rho, &
                  eliav1%delta
  eliav1%widg = eliav1%delta*eliav1%rho
  
  allocate(eliav1%xnat(eliav1%numspat), &
           eliav1%Z_at(eliav1%numspat), &
           eliav1%nat(eliav1%numspat), &
           eliav1%occusite(eliav1%numspat), &
           eliav1%occumatr(eliav1%numspat,eliav1%numspat), &
           eliav1%ndi(eliav1%numpair), &
           eliav1%zappa(2,eliav1%numpair), &
           eliav1%summul(eliav1%numpair), &
           eliav1%occupair(eliav1%numpair), &
           eliav1%termcon(eliav1%numspat), &
           eliav1%termcon_allP(eliav1%numpair))
!           eliav1%poi_eq(eliav1%numspat))

  nasp = eliav1%numspat
  do jjj=1,eliav1%numspat
    read(iloginp2,*)kox,eliav1%Z_at(jjj),eliav1%nat(jjj),eliav1%xnat(jjj),&
                    eliav1%termcon(jjj)    
    xocc(jjj) = eliav1%xnat(jjj) / eliav1%nat(jjj)
    eliav1%occusite(jjj) = xocc(jjj)
  enddo
  eliav1%natclu = MAXVAL(eliav1%nat)
  
 ! eliav1%poi_eq = FILL_POI_EQ( eliav1%numspat, eliav1%numpair )
  eliav1%termcon_allP = zero
  eliav1%termcon_allP(Hot_Stuff(nasp,1:nasp)) = eliav1%termcon
  !new
  
  do jjj=1,eliav1%numspat
    eliav1%clu_mass = eliav1%clu_mass + &
     eliav1%nat(jjj)*xocc(jjj)*atwei(eliav1%Z_at(jjj))
  enddo
  
  do jjj=1,eliav1%numpair
    read(iloginp2,*)kox,eliav1%ndi(jjj),eliav1%zappa(:,jjj),eliav1%summul(jjj)
  enddo
  read(iloginp2,'(a)')rline
  rline=trim(adjustl(rline))
  lrline=len_trim(rline)
  read(rline(5:lrline),*) eliav1%qtop
  kou=0
  do jjj=1,eliav1%numspat
    do jjj2=jjj,eliav1%numspat
      kou=kou+1
      eliav1%occupair(kou) = eliav1%occusite(jjj) * eliav1%occusite(jjj2)
      eliav1%occumatr(jjj,jjj2) = kou
      eliav1%occumatr(jjj2,jjj) = kou
    enddo
  enddo
  !new
  read(iloginp2,'(a)',iostat=ios)rline
  if (ios==0) then
    rline=trim(adjustl(rline))
    lrline=len_trim(rline)
    if (lrline>0) then
      read(rline(1:lrline),*) eliav1%termcon_allP
      eliav1%termcon_allP = eliav1%termcon_allP
      eliav1%termcon = eliav1%termcon_allP(Hot_Stuff(nasp,1:nasp))
    endif
  endif
  close(iloginp2)
  
  read(iloginp,*)
  allocate(eliav1%pseudomult(eliav1%esse,eliav1%numpair))
  do k=1,eliav1%numpair
    do k2=1,eliav1%esse
      read(iloginp,*)xdumm
      eliav1%pseudomult(k2,k) = xdumm/REAL(k2,DP) ! again we read the mu(k)/k
    enddo
  enddo
  close(iloginp)
  close(ilogerr)

 END SUBROUTINE THE_READER_EL
 end module read_onesam

!_________________________________________________________________________________________________________________
