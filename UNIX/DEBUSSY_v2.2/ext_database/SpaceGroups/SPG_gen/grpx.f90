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
MODULE multran
    use nano_deftyp
    REAL(CP),parameter   :: hiden=1.0e6_CP,eqthresh = 1.0_CP/hiden, dnm=8.0_CP*9.0_CP*5.0_CP*7.0_CP*11.0_CP

    TYPE,public :: transf
      INTEGER(I4B),dimension(6,6)   ::  ps_rot
      REAL(CP),dimension(6)         ::  transl
    END TYPE transf

    INTERFACE ASSIGNMENT(=)
      MODULE PROCEDURE assi
    END INTERFACE
    INTERFACE OPERATOR(*)
      MODULE PROCEDURE multi
    END INTERFACE
    INTERFACE OPERATOR(-)
      MODULE PROCEDURE minu
    END INTERFACE

CONTAINS

    SUBROUTINE assi(b,a)
      TYPE(transf),INTENT(INOUT)  :: b
      TYPE(transf),INTENT(IN)     :: a
      REAL(CP)                    :: v(6)

      v = MODULO(a%transl,1.0_CP)
      WHERE (abs(v) < eqthresh) v=0.0_CP
      WHERE (abs(v-1.0_CP) < eqthresh) v=0.0_CP
      b%ps_rot = a%ps_rot
      b%transl = v
    END SUBROUTINE assi

    FUNCTION multi(a,b)
      TYPE(transf)               :: multi
      TYPE(transf), INTENT(IN)   :: a,b
      REAL(CP)                   :: vv(6)

      multi%ps_rot = MATMUL(a%ps_rot,b%ps_rot)
      vv = a%transl + REAL(MATMUL(a%ps_rot,NINT(dnm * b%transl)),CP)/dnm
      multi%transl = vv - REAL(FLOOR(vv+eqthresh),CP)
    END FUNCTION multi

    FUNCTION minu(a,b)
      TYPE(transf)               :: minu
      TYPE(transf), INTENT(IN)   :: a,b

      minu%ps_rot = a%ps_rot - b%ps_rot
      minu%transl = a%transl - b%transl
      WHERE (abs(minu%transl) < eqthresh) minu%transl=0.0_CP
    END FUNCTION minu

END MODULE multran
!_______________________________________________________________________________
 MODULE grpdef
    USE multran
    implicit none

    INTEGER(I4B),public,parameter  :: ord_grp_max = 192,ngen_max=8,norb_max=16,sp_dim_max=6
    INTEGER(I4B),public,save       :: sp_dim,ord_grp,n_gen,ord_gen(ngen_max)
    TYPE(transf),public,save       :: grp(ord_grp_max),gen(ngen_max),orbg(ngen_max,norb_max)
    TYPE(transf),PUBLIC,save       :: null,ident

    DATA null%ps_rot/36*0/
    DATA null%transl/6*0.0_CP/
    DATA ident%ps_rot/1,6*0,1,6*0,1,6*0,1,6*0,1,6*0,1/
    DATA ident%transl/6*0.0_CP/

 CONTAINS

!_______________________________________________________________________________
  SUBROUTINE STOREL(tostore)

    TYPE(transf),intent(IN)     :: tostore
    TYPE(transf)           :: diff
    INTEGER(I4B)           :: ii

    DO ii=1,ord_grp
       diff = grp(ii)-tostore
       IF (ISITZERO(diff)) RETURN
    ENDDO
    ord_grp=ord_grp+1
    grp(ord_grp) = tostore

  END SUBROUTINE STOREL
!_______________________________________________________________________________
  SUBROUTINE ORBITG(genx,i)

    TYPE(transf),INTENT(IN)    :: genx
    INTEGER(I4B),INTENT(IN)    :: i
    TYPE(transf)        :: diff,enew
    INTEGER(I4B)        :: j,keep

!_________________________ STORE A NEW ELEMENT IN THE i-TH GENERATOR'S ORBIT

    orbg(i,1)=ident
    ord_gen(i)=1

    diff=genx-ident
    IF (ISITZERO(diff)) RETURN
    orbg(i,2)=genx
    ord_gen(i)=2

    CYCGROUP:do
      enew = genx*orbg(i,ord_gen(i))
      keep=1
      do j=1,ord_gen(i)
        diff = enew-orbg(i,j)
        if (ISITZERO(diff)) THEN
          keep=0
          exit
        endif
      enddo
      if (keep==1) THEN
        ord_gen(i)=ord_gen(i)+1
        orbg(i,ord_gen(i))=enew
      else
        exit CYCGROUP
      endif
    enddo CYCGROUP
!______ Save in grp

    do j=1,ord_gen(i)
      CALL STOREL(orbg(i,j))
    enddo

  END SUBROUTINE ORBITG
!_______________________________________________________________________________
   FUNCTION ISITZERO(entr)
     TYPE(transf),INTENT(IN)    :: entr
     real(CP),dimension(6)      :: tra6
     LOGICAL(LGT)               :: isitzero

     tra6(1:sp_dim) = entr%transl(1:sp_dim)
     where(ABS(tra6(1:sp_dim)-REAL(NINT(tra6(1:sp_dim))))<eqthresh) tra6(1:sp_dim)=0.0_CP
     tra6(1:sp_dim) = modulo(tra6(1:sp_dim),1.0_CP)
     isitzero = (ALL(entr%ps_rot(1:sp_dim,1:sp_dim)==0) .AND. MAXVAL(ABS(tra6(1:sp_dim))) < eqthresh)

   END FUNCTION ISITZERO
!_______________________________________________________________________________
  FUNCTION TORATIO(ve)
    real(CP),dimension(:),INTENT(IN) :: ve
    real(CP),dimension(size(ve))     :: TORATIO

    TORATIO = REAL(NINT(ve*dnm),CP)/dnm

  END FUNCTION TORATIO
!_______________________________________________________________________________
  SUBROUTINE MKGR

  TYPE(transf)               :: tryto,tryto2,diff,tryto3
  INTEGER                    :: igr,isb,ig,ord_grp_0,new,kg,kc
  LOGICAL                    :: commute

    DO ig=1,n_gen
      ord_grp_0=ord_grp
      DO isb=1,ord_gen(ig)
        DO igr=2,ord_grp_0
          tryto=orbg(ig,isb)*grp(igr)
          CALL STOREL(tryto)
          tryto2=(grp(igr)*orbg(ig,isb))
          diff=tryto-tryto2
          commute=ISITZERO(diff)
          IF (.NOT.commute) CALL STOREL(tryto2)
        ENDDO
      ENDDO
    ENDDO

    kc=0
    new=1
    DO
      IF (new==0) EXIT
      kc=kc+1
      ord_grp_0=ord_grp
      new=0
      kg=1
      DO
        IF (kg >= ord_grp_0) EXIT
        kg=kg+1
        tryto=grp(kg)
        igr=1
        DO
          IF (igr >= ord_grp_0) EXIT
          igr=igr+1
          tryto2=tryto*grp(igr)
          CALL STOREL(tryto2)
          if (ord_grp.gt.ord_grp_0) then
            new=1
            !ord_grp_0 = ord_grp
          endif
          tryto3=grp(igr)*tryto
          diff=tryto2-tryto3
          commute = ISITZERO(diff)
          IF (.NOT.commute) then
            CALL STOREL(tryto3)
            if (ord_grp.gt.ord_grp_0) then
              new=1
              !ord_grp_0 = ord_grp
            endif
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE MKGR
 END MODULE grpdef
!_______________________________________________________________________________
  Program Group
    USE grpdef

    implicit none
    INTEGER(I4B)           :: mat(6,6),i2,ldire,lname,gtyp,lfn,lenlin,iout,iunit, ierr
    REAL(CP)               :: vec(6)
    CHARACTER(132),save    :: wline,rline,grpname,dire,direc,genf, outgrp
    Logical                :: DB_exist
    
!____ INPUT
    
 iunit = FIND_UNIT()
 
  OPEN(UNIT=iunit,status='old', &
       form='formatted',access='sequential', &
       file=trim('grp.ini'),action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file:  grp.ini'
        STOP
     ENDIF
   READ(iunit,'(a)',end=10, iostat=ierr) genf
     IF (ierr /=0) THEN
       print*, ' Error reading input file:  grp.ini'
       STOP
     ENDIF 
    
 10 genf=TRIM(ADJUSTL(genf))
    lfn=LEN_TRIM(genf)
!______ Append extension if needed
    IF (genf(lfn-3:lfn) /= '.gen') genf(lfn+1:lfn+4) = '.gen'
    genf=TRIM(ADJUSTL(genf))
    lfn=LEN_TRIM(genf)
    
   OPEN(UNIT=iunit,status='old', &
       form='formatted',access='sequential', &
       file=trim(genf),action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file: ',  genf
        STOP
     ENDIF
   READ(iunit,'(a)',end=20, iostat=ierr) rline
     IF (ierr /=0) THEN
       print*, ' Error reading input file:',  rline
       STOP
     ENDIF 
    
20  call CLEAN_line(rline)
    dire=TRIM(ADJUSTL(rline(1:6)))
    ldire=len_trim(dire)
    grpname=TRIM(ADJUSTL(rline(7:)))
    lname=len_trim(grpname)
    READ(iunit,*) sp_dim,n_gen
    IF (n_gen>ngen_max.or.sp_dim>sp_dim_max) THEN
        print*, 'too large space_dimension or n_generators!', n_gen, sp_dim
        STOP 
    ENDIF

    ord_gen=0
    ord_grp=0
    
!___________________ STORE IDENTITY

    CALL STOREL(ident)

    DO i2=1,n_gen
       gen(i2)%ps_rot = 0
       gen(i2)%transl = 0.0_CP
       READ(iunit,*)   mat(1:sp_dim,1:sp_dim), vec(1:sp_dim)
       gen(i2)%ps_rot(1:sp_dim,1:sp_dim) = TRANSPOSE(mat(1:sp_dim,1:sp_dim))
       gen(i2)%transl(1:sp_dim)          = TORATIO(vec(1:sp_dim))
       
       CALL ORBITG(gen(i2),i2)
    ENDDO
    CLOSE(iunit)

    IF (n_gen > 1) CALL MKGR

    IF (ord_grp > ord_grp_max) THEN
        print*, 'n_Operators too large!' , ord_grp
        STOP 
    ENDIF

!_________________________ OUTPUT

    iunit=FIND_UNIT()
    outgrp = genf(1:lfn-4)//'.grp'
    
    OPEN(UNIT=iunit,status='replace', &
       form='formatted',access='sequential', &
       file=trim(outgrp),action='WRITE', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening output file: ',  outgrp
        STOP
     ENDIF
    
    
    write(iunit,'(a6,a)') dire,trim(grpname)
    write(iunit,*) ord_grp,sp_dim

    lenlin = 12*6
    wline=''

    DO iout=1,ord_grp
      IF (iout>ord_grp) EXIT
      do i2=1,sp_dim
        IF (i2>sp_dim) exit
        write(wline(1:lenlin),'(6i12)')grp(iout)%ps_rot(i2:i2,1:sp_dim)
        write(iunit,'(a)')trim(wline)
      enddo
      write(wline(1:lenlin),'(6g12.6)')grp(iout)%transl(1:sp_dim)
      write(iunit,'(a)')trim(wline)
    ENDDO

    CLOSE(iunit)


  END Program Group


!===============================================================================
