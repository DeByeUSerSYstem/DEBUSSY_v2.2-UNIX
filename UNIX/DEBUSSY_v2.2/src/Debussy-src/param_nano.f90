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
MODULE PARAM_ASREAD
use nano_types
use struct_asread
use data_asread
use input_variables, only : ALL_PHA_INFO
use LINALG_TOOLS,only  : erf0


 INTEGER(I4B), PARAMETER                 :: npar0_ref(5)=[9,9,9,9,9]
 integer(I4B), PARAMETER                 :: npar_x_ato=6


!**************
CONTAINS
!**************

 SUBROUTINE READ_PARAM_DATA
   IMPLICIT NONE
   integer(I4B)                             :: i,iu,astat,j,ll,lbuf,ntoread, ncmax, kk, &
                                               ja, jat, na
   CHARACTER(132)                           :: rline
   character(5)                             :: ident
   character(127)                           :: buffer
   REAL(DP)                                 :: n0_in, n0max, w_in, wmax


   IF (ALLOCATED(Param_data)) DEALLOCATE (Param_data)
   ALLOCATE(Param_data(0:NSTR))

   Param_data(0:NSTR)%strain_cod = 0
   Param_data(0:NSTR)%strain_n1 = -4423614

!________ 1st the global part
   i=0
   IF (DO_AMORPH) then

     iu = FIND_UNIT()
     OPEN(UNIT=iu,status='old', &
     form='formatted',access='sequential', &
     file=TRIM(PARAMETER_FILE(i)),action='READ',iostat=astat)
     IF (astat /=0) THEN
       print'(1x,a,a,a,i5)','Error opening file ',TRIM(PARAMETER_FILE(i)),' structure %',i
       STOP
     ENDIF
     Param_data(i)%npar_ref = 2  !___ including B and B1 of amo
   ELSE
     Param_data(i)%npar_ref = 0  !___ including B and B1 of amo
   ENDIF

   ALLOCATE( Param_data(i)%flag_ref(Param_data(i)%npar_ref), &
             Param_data(i)%PhasePar(3,Param_data(i)%npar_ref), &
             Param_data(i)%NamePar(Param_data(i)%npar_ref) )

   IF (DO_AMORPH) then
     j = 0
     PARAM_LOOP0: do
        read(iu,'(a)',end=10) rline
        call clean_line(rline)
        rline=trim(adjustl(rline))
        IF (rline(1:1)=='!' .or. rline(1:1)=='>' .or. len_trim(rline) == 0) CYCLE PARAM_LOOP0
        ll = len_trim(rline)
        IF (ll<5) THEN
           PRINT*, 'Too short line in parameter file :', PARAMETER_FILE(i)
           STOP
        ENDIF
        j = j + 1
        ident = rline(1:5)
        buffer= trim(adjustl(rline(6:)))
        lbuf = LEN_TRIM(buffer)
        call PARAM_READ(rline(:ll),ident,buffer(:lbuf),lbuf,i)
     enddo PARAM_LOOP0

10   close(iu)
     IF (j > Param_data(i)%npar_ref) THEN
        print*,j, ' /= ',Param_data(i)%npar_ref,i
        print*, 'AMORPH: Wrong number of parameters in parameter file ', PARAMETER_FILE(i)
        STOP
     ENDIF

     call CHECK_PARAM_LIM(i)

   ENDIF
   FILE_PARAM_LOOP: DO i=1,nstr
!       print*, i, ' ciao1 ', trim(PARAMETER_FILE(i))
       iu = FIND_UNIT()
       OPEN(UNIT=iu,status='old', &
           form='formatted',access='sequential', &
           file=TRIM(PARAMETER_FILE(i)),action='READ',iostat=astat)
       IF (astat /=0) THEN
         print'(1x,a,a,a,i5)','Error opening file ',TRIM(PARAMETER_FILE(i)),' structure %',i
         STOP
       ENDIF

       Param_data(i)%npar_ref = npar0_ref(DB_INDEX(i))
    !    ACRF may 2014
    !    IF (DB_INDEX(i) == 2) Nsp_at(i) = 1
    !    ALL_PHA_INFO(i)%numat_asy_cell = 1 
    !   Param_data(i)%npar_ref = Param_data(i)%npar_ref + ALL_PHA_INFO(i)%numat_asy_cell*npar_x_ato                     
       Param_data(i)%npar_ref = Param_data(i)%npar_ref + Nsp_at(i)*npar_x_ato                     
       ALLOCATE( Param_data(i)%flag_ref(Param_data(i)%npar_ref), &
                 Param_data(i)%PhasePar(3,Param_data(i)%npar_ref), &
                 Param_data(i)%NamePar(Param_data(i)%npar_ref), &
                 Param_data(i)%NameAto(Nsp_at(i)), &
                 Param_data(i)%law_refOB(Nsp_at(i),2) )

       Param_data(i)%PhasePar(1,:) = zero
       Param_data(i)%PhasePar(2,:) = half
       Param_data(i)%PhasePar(3,:) = one
       Param_data(i)%flag_ref = 0
       IF (Param_data(i)%npar_ref > npar0_ref(DB_INDEX(i))) THEN
         na = npar0_ref(DB_INDEX(i))
         Param_data(i)%flag_ref(na+1:Param_data(i)%npar_ref) = 1
         do ja=1,Nsp_at(i)
           Param_data(i)%NameAto(ja) = 'ATO**'
           write(Param_data(i)%NameAto(ja)(4:5),'(i2.2)') ja
           Param_data(i)%law_refOB(ja,:) = 1
           kk = na+(ja-1)*npar_x_ato
           Param_data(i)%NamePar(kk+1) = 'OKK_I'
           Param_data(i)%NamePar(kk+2) = 'OKK_0'
           Param_data(i)%NamePar(kk+3) = 'OKK_L'
           Param_data(i)%NamePar(kk+4) = 'BTH_I'
           Param_data(i)%NamePar(kk+5) = 'BTH_0'
           Param_data(i)%NamePar(kk+6) = 'BTH_L'
           IF (DB_INDEX(i) /= 2) THEN
    !    ACRF may 2014: killing softly ALL+PHA_INFO
              Param_data(i)%PhasePar(2,kk+1) = one   ! ALL_PHA_INFO(i)%pha_xyzbo(5,ja)
              Param_data(i)%PhasePar(2,kk+2) = one   ! ALL_PHA_INFO(i)%pha_xyzbo(5,ja)
              Param_data(i)%PhasePar(2,kk+3) = one
              Param_data(i)%PhasePar(2,kk+4) = 0.1d0  ! ALL_PHA_INFO(i)%pha_xyzbo(4,ja)
              Param_data(i)%PhasePar(2,kk+5) = 0.1d0  ! ALL_PHA_INFO(i)%pha_xyzbo(4,ja)
              Param_data(i)%PhasePar(2,kk+6) = one
           ELSE
              Param_data(i)%PhasePar(2,kk+1:kk+3) = one
              Param_data(i)%PhasePar(2,kk+4:kk+6) = one
           ENDIF     
         enddo
       ENDIF


!_______ Store parameters information
       j = 0
       ja = 0
       jat = 0
       PARAM_LOOP: do
         read(iu,'(a)',end=1) rline
         call clean_line(rline)
         rline=trim(adjustl(rline))
         IF (rline(1:1)=='!' .or. rline(1:1)=='>' .or. len_trim(rline) == 0) CYCLE PARAM_LOOP
         ll = len_trim(rline)
         IF (ll<5) THEN
            PRINT*, 'Too short line in parameter file :', PARAMETER_FILE(i)
            STOP
         ENDIF
         j = j + 1
         ident = rline(1:5)
         IF (ident == 'STcod') j = j-1
         IF (ident == 'VALn1') j = j-1
         IF (ident(1:3) == 'ATO') then
             ja = 1
             jat = jat + 1
             if (jat > Nsp_at(i)) then
                print*, 'ERROR: too many atoms in .par file, structure % ',i
                STOP
             endif
         else
             ja = 0
         endif
         buffer= trim(adjustl(rline(6:)))
         lbuf = LEN_TRIM(buffer)
         IF (j <= npar0_ref(DB_INDEX(i)) .or. ja==1) THEN
            call PARAM_READ(rline(:ll),ident,buffer(:lbuf),lbuf,i)
         ELSE
            call ATOMPAR_READ(rline(:ll),ident,buffer(:lbuf),lbuf,i,jat)
         ENDIF
       enddo PARAM_LOOP

1      close(iu)

       IF (j-jat > Param_data(i)%npar_ref) THEN
         print*, ' Wrong number of parameters in parameter file ', PARAMETER_FILE(i)
         STOP
       ENDIF

       call CHECK_PARAM_LIM(i)

       n0_in = Param_data(i)%PhasePar(2,1)
       w_in = Param_data(i)%PhasePar(2,2)
       n0max = Param_data(i)%PhasePar(3,1)
       wmax = Param_data(i)%PhasePar(3,2)
       ntoread = N2READ(i)
       NCMAX = ntoread
!!bug off       call SET_N2USE(i,n0_in,w_in,n0max,wmax,ntoread,NCMAX)
       N2USE(i) = NCMAX
!       IF (DB_INDEX(i) == 4) THEN
! DB_INDEX(5) refers to core-shell NPs with ab=k and c=s, k and s indices are used for core and shell, respectively
        IF (DB_INDEX(i) == 4 .or. DB_INDEX(i) == 5) THEN
            N2USE_ab = N2READ_ab
            N2USE_c = N2READ_c
            N2USE(i) = N2READ(i)
       ENDIF
   enddo FILE_PARAM_LOOP

     RETURN


 END SUBROUTINE READ_PARAM_DATA
!___________________________________________________________________________________________________
 SUBROUTINE PARAM_READ(rline,ident,buffer,lbuf,i)
  INTEGER, intent(IN)        :: lbuf,i
  INTEGER(I4B)               :: jj,j
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(5),intent(IN)    :: ident
  CHARACTER(5)               :: ident2
  CHARACTER(lbuf),intent(IN) :: buffer

  ident2 = ident
  if (ident(1:3) == 'ATO') then
      ident2(4:5) = '**'
      read(ident(4:5),'(i2.2)') j
  endif

  parameters: SELECT CASE(ident2)

        CASE ('AmoB0') parameters
          IF (i/=0) then
             print*,'trying to read global par. * AmoB0 * in structure',i
             STOP
          endif
          Param_data(i)%NamePar(1) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,1),Param_data(i)%flag_ref(1)

        CASE ('AmoB1') parameters
          IF (i/=0) then
             print*,'trying to read global par. * AmoB1 * in structure',i
             STOP
          endif
          Param_data(i)%NamePar(2) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,2),Param_data(i)%flag_ref(2)

        CASE ('STcod') parameters
          read(buffer(:lbuf),*) Param_data(i)%strain_cod
          IF (DB_INDEX(i) == 1) THEN
              Param_data(i)%strain_cod = 1
          ELSE IF (DB_INDEX(i) == 2) THEN
              Param_data(i)%strain_cod = min(2,max(0,Param_data(i)%strain_cod))
          ELSE IF (DB_INDEX(i) == 3) THEN
              Param_data(i)%strain_cod = min(4,max(1,Param_data(i)%strain_cod))
              if (Param_data(i)%strain_cod==3) then !upgrade
                Param_data(i)%strain_cod=4
              endif
          ELSE IF (DB_INDEX(i) == 4) THEN
              Param_data(i)%strain_cod = min(5,max(1,Param_data(i)%strain_cod))
              IF (Param_data(i)%strain_cod == 2) then !upgrade
                Param_data(i)%strain_cod = 3
              endif
              IF (Param_data(i)%strain_cod == 4) then !upgrade
                Param_data(i)%strain_cod = 5
              endif
          ELSE IF (DB_INDEX(i) == 5) THEN
              Param_data(i)%strain_cod = min(6,max(1,Param_data(i)%strain_cod))
              IF (Param_data(i)%strain_cod > 1 .and. Param_data(i)%strain_cod < 6) then !upgrade
                Param_data(i)%strain_cod = 6
              endif
          ENDIF

        CASE ('VALn1') parameters
          read(buffer(:lbuf),*) Param_data(i)%strain_n1

        CASE ('AV_LN') parameters
          Param_data(i)%NamePar(1) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,1),Param_data(i)%flag_ref(1)

        CASE ('SD_LN') parameters
          Param_data(i)%NamePar(2) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,2),Param_data(i)%flag_ref(2)

        CASE ('AV1LN') parameters
          Param_data(i)%NamePar(1) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,1),Param_data(i)%flag_ref(1)

        CASE ('SD1LN') parameters
          Param_data(i)%NamePar(2) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,2),Param_data(i)%flag_ref(2)

        CASE ('AV2LN') parameters
          Param_data(i)%NamePar(3) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,3),Param_data(i)%flag_ref(3)

        CASE ('SD2LN') parameters
          Param_data(i)%NamePar(4) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,4),Param_data(i)%flag_ref(4)

        CASE ('PHILN') parameters
          Param_data(i)%NamePar(5) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,5),Param_data(i)%flag_ref(5)

        CASE ('STR_i') parameters
          Param_data(i)%NamePar(6) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,6),Param_data(i)%flag_ref(6)
          if (Param_data(i)%strain_cod > 3) &
            Param_data(i)%PhasePar(:,6) = Param_data(i)%PhasePar(:,6) * Upscale_Par

        CASE ('STR_1') parameters
          Param_data(i)%NamePar(7) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,7),Param_data(i)%flag_ref(7)
          if (Param_data(i)%strain_cod > 3) &
            Param_data(i)%PhasePar(:,7) = Param_data(i)%PhasePar(:,7) * Upscale_Par

        CASE ('STR_C') parameters
          Param_data(i)%NamePar(8) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,8),Param_data(i)%flag_ref(8)

        CASE ('STR_W') parameters
          Param_data(i)%NamePar(9) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,9),Param_data(i)%flag_ref(9)
          !micr
         CASE ('EPS_W') parameters
          Param_data(i)%NamePar(9) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,9),Param_data(i)%flag_ref(9)  
		 !
        CASE ('ATO**') parameters
          Param_data(i)%NameAto(j) = ident
          read(buffer(:lbuf),*) Param_data(i)%law_refOB(j,:)

        CASE Default parameters
            STOP 'ERROR in PARAMETER FILE'

  END SELECT parameters

  RETURN

 END SUBROUTINE PARAM_READ
!___________________________________________________________________________________________________
 SUBROUTINE ATOMPAR_READ(rline,ident,buffer,lbuf,i,jat)
  INTEGER, intent(IN)        :: lbuf,i, jat
  INTEGER(I4B)               :: jj,j
!  INTEGER(I4B),PARAMETER     :: npar_x_ato=6
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(5),intent(IN)    :: ident
  CHARACTER(5)               :: ident2
  CHARACTER(lbuf),intent(IN) :: buffer

  ident2 = ident

  atompar: SELECT CASE(ident2)


        CASE ('OKK_I') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 1
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE ('OKK_0') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 2
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE ('OKK_L') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 3
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE ('BTH_I') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 4
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE ('BTH_0') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 5
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE ('BTH_L') atompar
          jj = npar0_ref(DB_INDEX(i))+ npar_x_ato*(jat-1) + 6
          Param_data(i)%NamePar(jj) = ident
          read(buffer(:lbuf),*) Param_data(i)%PhasePar(:,jj),Param_data(i)%flag_ref(jj)

        CASE Default atompar
            STOP 'ERROR in PARAMETER FILE'

  END SELECT atompar

  RETURN

 END SUBROUTINE ATOMPAR_READ
!___________________________________________________________________________________________________
 SUBROUTINE CHECK_PARAM_LIM(i)
  INTEGER(I4B), intent(IN)        :: i
  INTEGER(I4B)                    :: j
  REAL(DP)                        :: dxl,dxu,xl,xu,xv

   do j=1,Param_data(i)%npar_ref
    IF (i>0 .and. Param_data(i)%NamePar(j)(1:3) == 'STR') then
      CYCLE
    ENDIF
    xl = Param_data(i)%PhasePar(1,j)
    xu = Param_data(i)%PhasePar(3,j)
    xv = Param_data(i)%PhasePar(2,j)
    IF (xl>xu-eps_SP) THEN
      print*,'FATAL: upper limit less than lower. Parameter: ', Param_data(i)%NamePar(j),' phase: ',i
      print*,'FATAL: upper limit less than lower. Parameter: ', Param_data(i)%PhasePar(:,j)
      STOP
    ENDIF

    dxl = xv - xl - sceps_CP
    dxu = xu - xv - sceps_CP

    IF (dxl>0.0_DP .and. dxu>0.0_DP) CYCLE

    IF (PARAM_LIM(i) == 0) THEN
      print*,'FATAL: variable out of limits :', Param_data(i)%NamePar(j),' phase: ',i, &
      'Lower    value    Upper: ', Param_data(i)%PhasePar(:,j)
      STOP
    ELSE IF (PARAM_LIM(i) == 1) THEN
      IF (dxl < 0.0_DP) xv = xl+sceps_CP
      IF (dxu < 0.0_DP) xv = xu-sceps_CP
    ELSE IF (PARAM_LIM(i) == 2) THEN
      IF (dxl < 0.0_DP) xl = xv-sceps_CP
      IF (dxu < 0.0_DP) xu = xv+sceps_CP
    ENDIF
    Param_data(i)%PhasePar(1,j) = xl
    Param_data(i)%PhasePar(2,j) = xv
    Param_data(i)%PhasePar(3,j) = xu
  enddo
  IF (i == 0) return
!  IF (Param_data(i)%npar_ref > npar0_ref(DB_INDEX(i))) THEN
!      Param_data(i)%PhasePar(1,npar0_ref(DB_INDEX(i))+1: Param_data(i)%npar_ref) = &
!      max(0.d0,Param_data(i)%PhasePar(1,npar0_ref(DB_INDEX(i))+1: Param_data(i)%npar_ref))
!      Param_data(i)%PhasePar(3,npar0_ref(DB_INDEX(i))+1: Param_data(i)%npar_ref) = &
!      min(1.d0,Param_data(i)%PhasePar(3,npar0_ref(DB_INDEX(i))+1: Param_data(i)%npar_ref))
!  ENDIF

 end SUBROUTINE CHECK_PARAM_LIM
!___________________________________________________________________________________________________
 SUBROUTINE SET_N2USE(i,n0_in,w_in,n0max,wmax,ntoread, NCMAX)
  implicit none
  INTEGER(I4B), intent(IN)        :: i, ntoread
  REAL(DP), intent(IN)            :: n0_in, n0max, w_in, wmax
  INTEGER(I4B), intent(OUT)       :: NCMAX
  REAL(DP)                        :: n0,w,w2t7,wtsr2i,vix,xlx00
  integer(I4B)                    :: iuu,nuu,juu,Ncut   !!!!,NCMAX

  n0= n0_in
  w = w_in
  NCMAX = ntoread

  Ncut = ntoread
  w2t7 = w*w*7.0_DP
  wtsr2i= 1.0_DP/(w*sr2)
  xlx00 = log(n0)
  iuu = CEILING(n0)
  do nuu=iuu,Ncut
    juu=nuu
    vix = (log(REAL(nuu,DP))-xlx00-w2t7)*wtsr2i
    vix = 0.5d0*(1.0_DP-ERF0(vix,0))
    IF (vix<skip_tail) EXIT
  enddo
  Ncut = MIN(juu,Ncut)
  NCMAX = Ncut

  w = wmax

  Ncut = ntoread
  w2t7 = w*w*7.0_DP
  wtsr2i= 1.0_DP/(w*sr2)
  do nuu=iuu,Ncut
    juu=nuu
    vix = (log(REAL(nuu,DP))-xlx00-w2t7)*wtsr2i
    vix = 0.5d0*(1.0_DP-ERF0(vix,0))
    IF (vix<skip_tail) EXIT
  enddo
  Ncut = MIN(juu,Ncut)
  NCMAX = MAX(Ncut,NCMAX)

  n0= n0max
  w = w_in

  Ncut = ntoread
  w2t7 = w*w*seven
  wtsr2i= 1.0_DP/(w*sr2)
  xlx00 = log(n0)
  iuu = CEILING(n0)
  do nuu=iuu,Ncut
    juu=nuu
    vix = (log(REAL(nuu,DP))-xlx00-w2t7)*wtsr2i
    vix = 0.5d0*(1.0_DP-ERF0(vix,0))
    IF (vix<skip_tail) EXIT
  enddo
  Ncut = MIN(juu,Ncut)
  NCMAX = MAX(Ncut,NCMAX)

 end SUBROUTINE SET_N2USE
!___________________________________________________________________________________________________

 SUBROUTINE make_par_file(natok,istcod,ivaln1, par_val,finampar)
!____________ TODO: NOT for the "structure #0" for the moment
   IMPLICIT NONE
   integer(I4B),intent(IN)            :: natok,istcod,ivaln1
   real(DP),dimension(:,:),intent(IN) :: par_val
   CHARACTER(132),intent(IN)          :: finampar

   integer(I4B)                             :: i,iu,astat
   CHARACTER(256)                           :: rline,check_par
   character(5),dimension(13)               :: par_name
   logical                                  :: par_exist

DATA par_name / 'AV1LN','SD1LN','AV2LN','SD2LN','PHILN','STR_i','STR_1','STR_C','STR_W','BTH_0','BTH_1','BTK_1', &
                'OKK**'/
    RETURN

 END SUBROUTINE make_par_file
!___________________________________________________________________________________________________
 SUBROUTINE PARAM_WRITE(i,iu,par_name,par_val)
  INTEGER(I4B), intent(IN)               :: i,iu
  CHARACTER(5),dimension(16),intent(IN)  :: par_name
  REAL(CP),dimension(3,16),intent(IN)    :: par_val
  REAL(CP),dimension(3)                  :: xyz_val
  INTEGER(I4B)                           :: j, flag_ref
  CHARACTER(132)                         :: wline
  CHARACTER(5)                           :: ident

  flag_ref = 1
  do j=1,npar0_ref(DB_INDEX(i))
      wline = ' '
      wline(1:5) = par_name(j)                                    ! Parameter names
      write(wline(10:60),'(3g15.6,2x,i2)') par_val(:,j),flag_ref    ! Parameter values
      write(iu,'(a)') trim(wline)
  enddo
  IF (DB_INDEX(i) == 1) THEN
      ident(1:5) = 'XYZ  '
      do j=1,Struk_data(i)%Npar_free
         wline = ' '
         write(ident(4:5),'(i2.2)') j
         wline(1:5) = ident
         xyz_val(2) = Struk_data(i)%Start_par(j)
         xyz_val(1) = xyz_val(2) * 0.95_CP
         xyz_val(3) = xyz_val(2) * 1.05_CP
         write(wline(10:60),'(3g15.6,2x,i2)') xyz_val,flag_ref   ! Parameter values
         write(iu,'(a)') trim(wline)
      enddo
  ENDIF

  RETURN

 END SUBROUTINE PARAM_WRITE
!___________________________________________________________________________________________________
 SUBROUTINE make_ref_file
   IMPLICIT NONE
   integer(I4B) , PARAMETER                 :: NSTAG = 3
   integer(I4B)                             :: iu, i, j, jj, flag_fix, flag_ref, j1, j2
   CHARACTER(132)                           :: rline, check_ref, rliner
   CHARACTER(132),dimension(5)              :: comm_lin
   CHARACTER(132),dimension(NSTAG)          :: stage_lin
   CHARACTER(1)                             :: comm
   logical                                  :: ref_exist

   ref_exist = .false.
   flag_fix = 0
   flag_ref = 1
   comm_lin(1) = '! 1 2 3 4 5 6 7 8; 9 10 11 12 13 14 15 16; 16+1 .. 16+Npar_free'
   comm_lin(2) = '! 1 2 3 4 5 6 7 8'
   comm_lin(3) = '! 1 2 3 4 5 6 7 8 9 10 11 12; 12+1 .. 12+Npar_free'
   comm_lin(4) = comm_lin(3)
   comm_lin(5) = comm_lin(3)
   stage_lin(1) = '  1 1 0 0 0 0 1 1'
   stage_lin(2) = '  0 0 1 1 1 1 0 0'
   stage_lin(3) = '  1 1 1 1 1 1 1 1'

   check_ref = trim(REFINEMENT_FILE)
   INQUIRE(file=check_ref,exist=ref_exist)
   IF (ref_exist) THEN
       print'(1x,a,a,a,/,a)','WARNING! refinement file ',TRIM(REFINEMENT_FILE),' exists. '// &
       ' Do you want to overwrite? (YES/NO)'
       read(*,'(a)') rline
       call clean_line(rline)
       rline=trim(adjustl(rline))
       if (rline(1:2) == 'NO' ) RETURN
   ENDIF
   iu = FIND_UNIT()
   OPEN(UNIT=iu,status='replace', &
           file=TRIM(REFINEMENT_FILE),action='write')
   rline = ' '
   rline(1:20) = ' Number of stages # '
   write(rline(22:),'(i2)') NSTAG
   write(iu,'(a)') trim(rline)                         ! 1st relevant line

   STAGE_WRITE: do i=1,NSTAG
      rline = ' '
      write(rline,'(a8,1x,i2,a8)') ' stage #',i,' COMPLEX'
      write(iu,'(a)') trim(rline)                      ! 2nd relevant line
      STRUC_WRITE: do j=1,NSTR
         rline = ' '
         if (j < 10) then
             write(rline,'(a2,i1)') ' %',j
         else
             write(rline,'(a2,i2)') ' %',j
         endif
         write(iu,'(a)') trim(rline)
         rline = ' '
         write(iu,'(a)') trim(comm_lin(DB_INDEX(J)))
         if (DB_INDEX(j) == 1) then
             j1 = 1
             do jj=1,npar0_ref(2)
                j2 = j1+2*jj
                write(rline(j2:j2),'(i1)') flag_ref
             enddo
             j2 = j2 + 1
             rline(j2:j2) = ';'
             j2 = j2 + 2
             write(rline(j2:j2),'(i1)') flag_fix
             j1 = j2 + 2
             do jj=npar0_ref(2)+2,npar0_ref(1)
                j2 = j1+1
                write(rline(j2:j2),'(i1)') flag_fix
                j1=j2+2
             enddo
             IF (Struk_data(j)%Npar_free > 0) THEN
                 j2 = j2 + 1
                 rline(j2:j2) = ';'
                 j1 = 43
                 do jj=1,Struk_data(j)%Npar_free
                     j2 = j1+2
                     write(rline(j1:j2),'(2x,i1)') flag_fix
                     j1 = j2
                 enddo
             ENDIF
             write(iu,'(a)') trim(rline)                   ! from 3rd to (2+nstr)-th relevant lines
         else if (DB_INDEX(j) == 2) then
             write(iu,'(a)') trim(stage_lin(i))            ! from 3rd to (2+nstr)-th relevant lines
         endif
      enddo STRUC_WRITE
      rline = ' %amo'
      write(iu,'(a)') trim(rline)
      IF (DO_AMORPH) THEN
          write(iu,'(2x,i1)') flag_ref
      ELSE
          write(iu,'(2x,i1)') flag_fix
      ENDIF
      rline = ' '
      rliner = ' '
      j1 = 1
      comm = ','
      SETS_WRITE: do j=1,NSET
         if (j == NSET) comm = ' '
         j2 = j1+1
         rline(j1:j2) = ' #'
         j1 = j2+1
         if (j < 10) then
             write(rline(j1:j1),'(i1)') j
             write(rliner(j1:j1),'(i1)') flag_ref
             j2 = j1
         else
             write(rline(j1:j1+1),'(i2)') j
             write(rliner(j1+1:j1+1),'(i1)') flag_ref
             j2 = j1+1
         endif
         rline(j2+1:j2+1) = comm
         j1 = j2+2
      enddo SETS_WRITE
      write(iu,'(a)') trim(rline)
      write(iu,'(a)') trim(rliner)

   enddo STAGE_WRITE
   close(iu)

 END SUBROUTINE make_ref_file
!___________________________________________________________________________________________________

END MODULE PARAM_ASREAD

