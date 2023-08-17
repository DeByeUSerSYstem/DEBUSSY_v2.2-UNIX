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
module input_data
 use input_variables
 use ATOMIX, only: symb_of_Z, n_elements

!**************
CONTAINS
!**************

subroutine input_main

  implicit real(CP)(a-h,o-z),integer(I4B)(i-n)
  logical     :: reading_dataset, reading_structure, reading_amo
  INTEGER(I4B):: section

  character(256) :: rline
  character(4)   :: ident
  character(256) :: buffer

  real(CP)    :: r3(3)
  INTEGER(I4B):: i4(4), islash

  main_name = TRIM(ADJUSTL(main_name))
  
  NPH_DB1=0
  NPH_DB2_USED=0
  NPH_DB3=0
  NPH_DB4=0
  NPH_DB5=0

  iu = FIND_UNIT()
      OPEN(UNIT=iu,status='old', file=trim(main_name),action='READ', iostat=ioerr)
      IF (ioerr /=0) then
         print*, 'error opening input file: ', trim(main_name)
      STOP 
      ENDIF

!!!!!!!!!!!!!!!!!!! COUNT datasets and structures
  kset = 0
  kstr = 0
  klin = 0
  do
    read(iu,'(a)',end=1,err=11)rline
    call clean_line(rline)
    rline=trim(adjustl(rline))
    IF (rline(1:1)=='#') kset = kset + 1
    IF (rline(1:1)=='%') then
       IF (rline(2:2)/='0') kstr = kstr + 1
    ENDIF
    klin = klin + 1
  enddo
11 STOP 'error reading input file!'
1 nset = kset
  nstr = kstr
  rewind(iu)

!________ Allocate & initialize arrays

  call ALLOCCO
  call INIVALS

  section = 0
  kset = 0
  kstr = 0
  DB_INDEX = 0
  ITYPE_DB = 0


  MAINREAD: do ilin=1,klin
!!!!!!!!!!!!!!!!!!!! READ NEW LINE; CHECK COMMENTS; TAKE LENGTHS

    read(iu,'(a)',end=2)rline
    call clean_line(rline)

    rline=trim(adjustl(rline))
    IF (rline(1:1)=='!' .or. rline(1:1)=='>' .or. len_trim(rline) == 0) CYCLE MAINREAD

    ll = len_trim(rline)
    IF (ll<5) THEN
      PRINT*, 'Too short line in input file at line:', ilin
      STOP
    ENDIF

    ident = rline(1:4)
    buffer= trim(adjustl(rline(5:)))
    lbuf = LEN_TRIM(buffer)
    

    IF (ident=='****') THEN !______ changing section
      IF (section==1) THEN
        !_________________ Management of last DATASET
        IF (kset>0) call CHECK_DATASET(kset)      !!! Completeness of information, last check
      ELSE IF (section==2) THEN
        !_________________ Management of last structure
        IF (kstr==nstr) call CHECK_STRUCTURE(kstr)   !!! Completeness of information, last check
!        if (kstr==nstr .and. DB_INDEX(kstr) >= 3) THEN
        ! check DB_INDEX order 
       if (kstr>1) then
         if (DB_INDEX(kstr)<DB_INDEX(kstr-1))  then 
            print*, 'ERROR: db0x have to be listed in increasing order of x! The Program stops'
            STOP
          endif
        endif

        if (kstr==nstr .and. DB_INDEX(kstr) /= 2) THEN
!!           call PHA_READER(kstr)
!!           call PHA_PROT_COMP(kstr)
           if (DB_INDEX(kstr) == 1) THEN
               NPH_DB1=NPH_DB1+1
           else if (DB_INDEX(kstr) == 3) THEN
               NPH_DB3=NPH_DB3+1
           else if (DB_INDEX(kstr) == 4) THEN
               NPH_DB4=NPH_DB4+1
           else if (DB_INDEX(kstr) == 5) THEN
               NPH_DB5=NPH_DB5+1
           endif
        endif
      ENDIF
      section = section + 1
      CYCLE MAINREAD
    ENDIF

    IF (lbuf == 128) THEN
      print*,'WARNING: long buffer at line ', ilin,': '
      print*,buffer
    ENDIF

    IF (section==1) THEN
      reading_dataset   = .true.
      reading_structure = .false.
      reading_amo       = .false.
    ELSE IF (section==2) THEN
      IF (kstr == 0) THEN
        IF (ident(2:2)=='0') THEN
          reading_dataset   = .false.
          reading_structure = .false.
          reading_amo       = .true.
        ELSE IF (ident(2:2)=='1') THEN
          reading_dataset   = .false.
          reading_structure = .true.
          reading_amo       = .false.
        ENDIF
      ENDIF
    ELSE IF (section==3) THEN
      reading_dataset   = .false.
      reading_structure = .false.
      reading_amo       = .false.
    ELSE
      print*, 'SECTION NUMBER',section,' NOT IMPLEMENTED - fatal'
      STOP 
    ENDIF

    IF (ident(1:1)=='#') THEN
      IF (kset>0) call CHECK_DATASET(kset)   !!! Completeness of information check
      kset = kset + 1
      TECHNIQUE(kset)      = buffer(:lbuf)
      IF (TRIM(TECHNIQUE(kset))/='XRD' .and. TRIM(TECHNIQUE(kset))/='NPD' .and. &
          TRIM(TECHNIQUE(kset))/='EPD') THEN
      print*, 'Dataset # ', i2, 'Incorrect TECHNIQUE ',TRIM(TECHNIQUE(kset))
      STOP 
      ENDIF
    !! Neutrons and Electrons not allowed [for now] in the distributed version
      IF (TRIM(TECHNIQUE(kset))=='NPD' .or. TRIM(TECHNIQUE(kset))=='EPD') THEN 
        print*, TRIM(TECHNIQUE(kset)), ' still under development! The program stops'
      STOP
      ENDIF
      cycle MAINREAD
    ELSE IF (ident(1:1)=='%') THEN
      IF (reading_structure) THEN
          IF (kstr>0) THEN
             call CHECK_STRUCTURE(kstr)   !!! Completeness of information check
             if (DB_INDEX(kstr) == 2) THEN
                 NPH_DB2_USED=NPH_DB2_USED+1
             else if (DB_INDEX(kstr) /= 2) THEN    
!!                 call PHA_READER(kstr)
!!                 call PHA_PROT_COMP(kstr)
                 if (DB_INDEX(kstr) == 1) THEN
                    NPH_DB1=NPH_DB1+1
                 else if (DB_INDEX(kstr) == 3) THEN
                    NPH_DB3=NPH_DB3+1
                 else if (DB_INDEX(kstr) == 4) THEN
                    NPH_DB4=NPH_DB4+1
                 else if (DB_INDEX(kstr) == 5) THEN
                    NPH_DB5=NPH_DB5+1
                 endif
             endif
          endif
          kstr = kstr + 1
          STRUCTURE_NAME(kstr) = trim(adjustl(buffer(:lbuf)))
          !______________________________________________________________
          !_____ Stripping off a path if present
          !_____ The path is currently lost; if we want to use it, 
          !_____ we need to save it into another variable
          !______________________________________________________________
          islash = INDEX(trim(STRUCTURE_NAME(kstr)), separator, .true.)
          if (islash > 0) then
            STRUCTURE_NAME(kstr) = trim(adjustl(STRUCTURE_NAME(kstr)(islash+1:)))
          endif
          !______________________________________________________________
          PROTOTYPE_PHA_FILE(kstr) = trim(adjustl(STRUCTURE_NAME(kstr)))
      ELSE IF (reading_amo) THEN
         print'(1x,a)','Amorphous '
         DO_AMORPH = .true.
      ENDIF
      cycle MAINREAD
    ENDIF

!!!!!!!!!!! START READING MEANINGFUL THINGS

  IF (reading_dataset.and.TRIM(TECHNIQUE(kset))=='XRD') THEN

      IF (buffer(:lbuf) == TRIM(TECHNIQUE(kset)) ) THEN
          print'(1x,a,i3.3,a)','Dataset #',kset,': TECHNIQUE = X-Ray Powder Diffraction  '
      ENDIF

      call ULOOP(rline(:ll),ident,buffer(:lbuf),lbuf,kset,ilin)
      
  ELSE IF (reading_dataset.and.TRIM(TECHNIQUE(kset))=='NPD') THEN

      IF (buffer(:lbuf) == TRIM(TECHNIQUE(kset)) ) THEN
          print'(1x,a,i3.3,a)','Dataset #',kset,': TECHNIQUE = Neutron Powder Diffraction  '
      ENDIF

      call ULOOP(rline(:ll),ident,buffer(:lbuf),lbuf,kset,ilin)
      
  ELSE IF (reading_dataset.and.TRIM(TECHNIQUE(kset))=='EPD') THEN

      IF (buffer(:lbuf) == TRIM(TECHNIQUE(kset)) ) THEN
          print'(1x,a,i3.3,a)','Dataset #',kset,': TECHNIQUE = Electron Powder Diffraction  '
      ENDIF
    
      call ULOOP(rline(:ll),ident,buffer(:lbuf),lbuf,kset,ilin)    

    ELSE IF (reading_dataset.and.TRIM(TECHNIQUE(kset))=='SAXS') THEN
      print'(1x,a,i3.3,a)','Dataset #',kset,': TECHNIQUE = SAXS '
      print*, 'Sorry - SAXS  still not implemented. I stop.'
      STOP
    ELSE IF (reading_dataset.and.TRIM(TECHNIQUE(kset))=='EXAFS') THEN
      print'(1x,a,i3.3,a)','Dataset #',kset,': TECHNIQUE = EXAFS'
      print*, 'Sorry - EXAFS still not implemented. I stop.'
      STOP
    ELSE IF (reading_amo) THEN
      IF (DO_AMORPH) call ALOOP(rline(:ll),ident,buffer(:lbuf),lbuf,ilin)
      CYCLE MAINREAD
    ELSE IF (reading_structure) THEN
      IF (ident(1:1)=='%') THEN
         print'(1x,a,i3.3,a)','Structure #',kstr,':   ', STRUCTURE_NAME(kstr)
      ENDIF

      ilst = scan(STRUCTURE_NAME(kstr), '.', .true.) - 1
      IF (ilst == 0) THEN
        !print*, 'Strange database ',kstr,' '//trim(STRUCTURE_NAME(kstr))
        !STOP
        ilst=len_trim(STRUCTURE_NAME(kstr))+1
      ENDIF

      IF (STRUCTURE_NAME(kstr)(1:ilst) == 'cubo' ) ITYPE_DB(kstr) = 1
      IF (STRUCTURE_NAME(kstr)(1:ilst) == 'icos' ) ITYPE_DB(kstr) = 2
      IF (STRUCTURE_NAME(kstr)(1:ilst) == 'deca' ) ITYPE_DB(kstr) = 3
      IF (STRUCTURE_NAME(kstr)(1:ilst) == 'decb' ) ITYPE_DB(kstr) = 4
         
      call VLOOP(rline(:ll),ident,buffer(:lbuf),lbuf,kstr,ilin)
      
!!      IF (DB_INDEX(kstr) /= 2 .and. index(STRUCTURE_NAME(kstr), 'pha') > 0) THEN
      IF (DB_INDEX(kstr) /= 2) THEN
         IF (DB_INDEX(kstr) == 1) THEN
           ITYPE_DB(kstr) = NPH_DB1
         ELSE IF (DB_INDEX(kstr) == 3) THEN
           ITYPE_DB(kstr) = NPH_DB3
         ELSE IF (DB_INDEX(kstr) == 4) THEN
           ITYPE_DB(kstr) = NPH_DB4
         ELSE IF (DB_INDEX(kstr) == 5) THEN
           ITYPE_DB(kstr) = NPH_DB5
         ENDIF
         ALL_PHA_INFO(kstr)%PHA_FILE(:)=''
         if (len_trim(STRUCTURE_NAME(kstr))>ilst) then
           ALL_PHA_INFO(kstr)%PHA_FILE(1:len_trim(STRUCTURE_NAME(kstr)))=trim(STRUCTURE_NAME(kstr))
         else
           ALL_PHA_INFO(kstr)%PHA_FILE(1:ilst-1)=STRUCTURE_NAME(kstr)(1:ilst-1)
         endif
      ENDIF
      IF (DB_INDEX(kstr) == 3  .or. DB_INDEX(kstr) == 5) ALL_PHA_INFO(kstr)%skipme = .true.

    ELSE IF ((.not.reading_structure).and.(.not.reading_dataset)) THEN

      call ZLOOP(rline(:ll),ident,buffer(:lbuf),lbuf,ilin)

    ENDIF

  enddo MAINREAD

! check for correctness of background information 
  NSET_BACK = 0 
  IF (ALL(LEN_TRIM(BLANK_FILENAME) == 0) .and. ALL(YOUNG_NC == 0) .and. ALL(CHEB_NC == 0)) THEN
     print*, 'Sorry - Background information not given. The program stops.'
     STOP
  ENDIF
  IF (NSET == 1) THEN
     NSET_BACK = 1 
  ELSE IF (NSET >= 2) THEN
     IF (ALL(LEN_TRIM(BLANK_FILENAME(2:NSET)) == 0) .and. ALL(YOUNG_NC(2:NSET) == 0) .and. ALL(CHEB_NC(2:NSET) == 0)) THEN
        NSET_BACK = 1 
        BLANK_FILENAME(2:NSET) = BLANK_FILENAME(1)
        YOUNG_NC(2:NSET) = YOUNG_NC(1)
        CHEB_NC(2:NSET) =  CHEB_NC(1)
     ELSE
        NSET_BACK = NSET 
        do kset=1,NSET
           IF (LEN_TRIM(BLANK_FILENAME(kset)) == 0 .and. YOUNG_NC(kset) == 0 .and. CHEB_NC(kset) == 0) THEN
               print*, 'WARNING! - Incorrect background info for the',kset,' dataset. Info are reset.'
               BLANK_FILENAME(kset) = BLANK_FILENAME(1)
               YOUNG_NC(kset) = YOUNG_NC(1)
               CHEB_NC(kset) =  CHEB_NC(1)
           ENDIF
        enddo
     ENDIF
  ENDIF

2 close(iu)

end subroutine input_main
!_______________________________________________________________________________________________
subroutine ALLOCCO
implicit none
integer(I4B)  :: j

  IF (ALLOCATED(TECHNIQUE)) DEALLOCATE(TECHNIQUE)
  IF (ALLOCATED(RADIATION)) DEALLOCATE(RADIATION)
  IF (ALLOCATED(DATA_FILENAME)) DEALLOCATE(DATA_FILENAME)
  IF (ALLOCATED(BLANK_FILENAME)) DEALLOCATE(BLANK_FILENAME)
  IF (ALLOCATED(BLANK_NCOMPS)) DEALLOCATE(BLANK_NCOMPS)
  IF (ALLOCATED(POROD_BKG)) DEALLOCATE(POROD_BKG)
  IF (ALLOCATED(GEOM)) DEALLOCATE(GEOM)
  IF (ALLOCATED(XDATA_TYPE)) DEALLOCATE(XDATA_TYPE)
  IF (ALLOCATED(INST_FLAG)) DEALLOCATE(INST_FLAG)
  IF (ALLOCATED(INST_6P_var)) DEALLOCATE(INST_6P_var)
  IF (ALLOCATED(INST_5P_con)) DEALLOCATE(INST_5P_con)
  
  ALLOCATE(INST_FLAG(nset),INST_6P_var(nset,7),INST_5P_con(nset,5))
  INST_FLAG=0
  INST_6P_var=0.d0
  INST_5P_con=0.d0
  

  ALLOCATE(TECHNIQUE(nset), RADIATION(nset), DATA_FILENAME(nset), BLANK_FILENAME(nset), &
           BLANK_NCOMPS(nset), POROD_BKG(nset), GEOM(nset), XDATA_TYPE(nset))

  IF (ALLOCATED(NDATA)) deallocate(NDATA)
  IF (ALLOCATED(ANGRANGE)) deallocate(ANGRANGE)
  IF (ALLOCATED(N_EVERY)) deallocate(N_EVERY)

  ALLOCATE(NDATA(nset), ANGRANGE(3,nset), N_EVERY(nset))
  N_EVERY=1
  
!! microstrain
  IF (ALLOCATED(MICRO_FLAG)) deallocate(MICRO_FLAG)
  ALLOCATE(MICRO_FLAG(nstr)) 
  MICRO_FLAG = 0

  IF (ALLOCATED(STRUCTURE_NAME)) DEALLOCATE(STRUCTURE_NAME)
  IF (ALLOCATED(PATH_NAME))      DEALLOCATE(PATH_NAME)
  IF (ALLOCATED(PROTOTYPE_PHA_FILE)) DEALLOCATE(PROTOTYPE_PHA_FILE)
  IF (ALLOCATED(PARAMETER_FILE)) DEALLOCATE(PARAMETER_FILE)
  IF (ALLOCATED(DB_INDEX))       DEALLOCATE(DB_INDEX)
  IF (ALLOCATED(ITYPE_DB))       DEALLOCATE(ITYPE_DB)
  IF (ALLOCATED(PARAM_LIM))      DEALLOCATE(PARAM_LIM)
  IF (ALLOCATED(N2USE))          DEALLOCATE(N2USE)
  IF (ALLOCATED(N2USE_ab))       DEALLOCATE(N2USE_ab)
  IF (ALLOCATED(N2USE_c))        DEALLOCATE(N2USE_c)
  IF (ALLOCATED(ATOM))           DEALLOCATE(ATOM)
  IF (ALLOCATED(Z_ATOM))         DEALLOCATE(Z_ATOM)
  IF (ALLOCATED(Nsp_at))         DEALLOCATE(Nsp_at)
  IF (ALLOCATED(NPAIR_AT))       DEALLOCATE(NPAIR_AT)
  IF (ALLOCATED(CELL_P))         DEALLOCATE(CELL_P)
  IF (ALLOCATED(n2read))         DEALLOCATE(n2read)
  IF (ALLOCATED(n2read_ab))      DEALLOCATE(n2read_ab)
  IF (ALLOCATED(n2read_c))       DEALLOCATE(n2read_c)
  IF (ALLOCATED(IND_SPACE_GROUP))DEALLOCATE(IND_SPACE_GROUP)
  IF (ALLOCATED(NATCEL_PER_SPEC)) DEALLOCATE(NATCEL_PER_SPEC)
  IF (ALLOCATED(PROTOTYPING))    DEALLOCATE(PROTOTYPING)

  ALLOCATE(STRUCTURE_NAME(nstr), PATH_NAME(0:nstr), PARAMETER_FILE(0:nstr), DB_INDEX(nstr), ATOM(NSpecMax,nstr), &
           Z_ATOM(NspecMax,nstr),IND_SPACE_GROUP(3,nstr), CELL_P(6,nstr), Nsp_at(nstr), NPAIR_AT(nstr), &
           ITYPE_DB(nstr), PARAM_LIM(0:nstr), N2USE(nstr), PROTOTYPE_PHA_FILE(nstr), n2read(nstr), &
           n2read_ab(nstr,2), n2read_c(nstr,2), N2USE_ab(nstr,2), N2USE_c(nstr,2), NATCEL_PER_SPEC(NSpecMax,nstr), &
           PROTOTYPING(nstr))
  NATCEL_PER_SPEC=0
  PROTOTYPING=.false.
  Nsp_at=0
  IF (ALLOCATED(ILAMBDA)) DEALLOCATE(ILAMBDA)
  IF (ALLOCATED(LAMBDAS)) DEALLOCATE(LAMBDAS)
  IF (ALLOCATED(POLARIZB)) DEALLOCATE(POLARIZB)
  IF (ALLOCATED(FULLPOL)) DEALLOCATE(FULLPOL)
  IF (ALLOCATED(WAVE_ERR)) DEALLOCATE(WAVE_ERR)
  IF (ALLOCATED(CORR_ABS)) DEALLOCATE(CORR_ABS)
  IF (ALLOCATED(MONO_POSIT)) DEALLOCATE(MONO_POSIT)
  IF (ALLOCATED(COBRA)) deallocate(COBRA)

  ALLOCATE(ILAMBDA(nset), LAMBDAS(0:2,nset), CORR_ABS(nset), WAVE_ERR(nset), &
           MONO_POSIT(nset),COBRA(nset),POLARIZB(2,nset),FULLPOL(nset))

!!! Private var.s

  IF (ALLOCATED(FFORM)) deallocate(FFORM)
  IF (ALLOCATED(NSKIP_HEAD)) deallocate(NSKIP_HEAD)
  IF (ALLOCATED(NSKIP_FOOT)) deallocate(NSKIP_FOOT)
  IF (ALLOCATED(CHEB_NC)) DEALLOCATE(CHEB_NC)
  IF (ALLOCATED(YOUNG_NC)) DEALLOCATE(YOUNG_NC)

  ALLOCATE(FFORM(nset), NSKIP_HEAD(nset), NSKIP_FOOT(nset), CHEB_NC(nset), YOUNG_NC(nset))

  IF (ALLOCATED(ALL_PHA_INFO)) THEN
    do j=1,size(ALL_PHA_INFO)
      NULLIFY(ALL_PHA_INFO(j)%pha_xyzbo)
      NULLIFY(ALL_PHA_INFO(j)%pha_Z)
      NULLIFY(ALL_PHA_INFO(j)%pha_Ion)
      NULLIFY(ALL_PHA_INFO(j)%pha_MASSNUMBER)
      NULLIFY(ALL_PHA_INFO(j)%pha_SYMB)
      NULLIFY(ALL_PHA_INFO(j)%pha_PAIR_averBTH)
      NULLIFY(ALL_PHA_INFO(j)%pha_PAIR_prodOKK)
    enddo
    DEALLOCATE(ALL_PHA_INFO)
  endif
  ALLOCATE(ALL_PHA_INFO(nstr))

end subroutine ALLOCCO
!_______________________________________________________________________________________________
subroutine INIVALS

!!!!!!!!!!! Datasets section

  NSKIP_HEAD(:) = 0;      NSKIP_FOOT(:) = 0;      FFORM(:) = 0;  NDATA(:) = 0
  DATA_FILENAME = ''
  ANGRANGE  = -99.e99_DP
  BLANK_FILENAME = ''
  BLANK_NCOMPS = 1
  POROD_BKG = 0
  CHEB_NC = 0
  YOUNG_NC = 0
  ILAMBDA = 1;  LAMBDAS(0,:) = 0.0_DP;  LAMBDAS(1,:) = 1.54056_DP;  LAMBDAS(2,:) = 0.0_DP
  POLARIZB(1,:) = one; POLARIZB(2,:) =  zero; FULLPOL(:) = .false.
  WAVE_ERR = zero
  RADIATION = 'S'
  MONO_POSIT = 0;  COBRA      = 0.0_DP
  GEOM  = 'reflecBB';  CORR_ABS = 0.0_DP
  DATA_FILENAME = ''
  XDATA_TYPE = 'A'

!!!!!!!!!!! Structure models section

  PATH_NAME = 'NOPATH'
  PROTOTYPE_PHA_FILE = ''
  ATOM      = ''
  IND_SPACE_GROUP = 0
  CELL = 0.0_DP
  DO_AMORPH = .false.
  AMO_DCORR = 0.0_DP
  AMO_DCORR0 = 0.0_DP
  QMAX_D = 0.0_DP
  PARAMETER_FILE = ''
  PARAM_LIM = 1
  N2USE = 0
  PROTOTYPING=.false.

!!!!!!!!!!! Output section

  SIMUL_FLAG = -1
  CALC_FLAG  = 0
  CALC_RPDF  = 0
  REFINEMENT_FILE = ''
  MAKEFIL_FLAG    = 0   
  OUTPUT_FILE = ''

end subroutine INIVALS
!_______________________________________________________________________________________________
subroutine ULOOP(rline,ident,buffer,lbuf,kset,i)
  INTEGER, intent(IN)        :: lbuf,kset,i
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(4),intent(IN)    :: ident
  CHARACTER(lbuf),intent(IN) :: buffer

  INTEGER(I4B):: astat, astat2, astat3, astat4
  real(CP)    :: r3(3),cphip,sphip,omk2
  INTEGER(I4B):: i4(4)


      dataset: SELECT CASE(ident)

      CASE ('data') dataset

        DATA_FILENAME(kset) = buffer(:lbuf)

      CASE ('xtyp') dataset

        XDATA_TYPE(kset) = buffer(:lbuf)

      CASE ('form') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) i4
        IF (astat /= 0) THEN
          READ(buffer(:lbuf),*,IOSTAT=astat2) i4(:3)
          IF (astat2 /= 0) THEN
            READ(buffer(:lbuf),*,IOSTAT=astat3) i4(:2)
            IF (astat3 /= 0) THEN
              READ(buffer(:lbuf),*,IOSTAT=astat4) i4(:1)
              IF (astat4 /= 0) THEN
                print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
                STOP
              ELSE
                FFORM(kset) = i4(1)
              ENDIF
            ELSE
              FFORM(kset) = i4(1)
              NDATA(kset) = i4(2)
            ENDIF
          ELSE
            FFORM(kset) = i4(1)
            NSKIP_HEAD(kset) = i4(2)
            NSKIP_FOOT(kset) = i4(3)
          ENDIF
        ELSE
          FFORM(kset) = i4(1)
          NDATA(kset) = i4(2)
          NSKIP_HEAD(kset) = i4(3)
          NSKIP_FOOT(kset) = i4(4)
        ENDIF
          
      CASE ('rang') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) ANGRANGE(:,kset),N_EVERY(kset)
        IF (astat /= 0) THEN
          READ(buffer(:lbuf),*,IOSTAT=astat2) ANGRANGE(:,kset)
          IF (astat2 /= 0) THEN
            print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
            STOP
          ENDIF
          N_EVERY(kset)=1
        ENDIF

      CASE ('blnk') dataset

        BLANK_FILENAME(kset) = buffer(:lbuf)
        
      CASE ('blnc') dataset

         READ(buffer(:lbuf),*,IOSTAT=astat) BLANK_NCOMPS(kset),POROD_BKG(kset)
         IF (astat /= 0) THEN
           READ(buffer(:lbuf),*,IOSTAT=astat2) BLANK_NCOMPS(kset)
           IF (astat2 /= 0) THEN
             print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
             STOP
           endif
         ENDIF
         POROD_BKG(kset)=min(1,max(0,POROD_BKG(kset)))
         BLANK_NCOMPS(kset)=BLANK_NCOMPS(kset)+POROD_BKG(kset)
      CASE ('cheb') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) CHEB_NC(kset)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF

      CASE ('youn') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) YOUNG_NC(kset)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF

      CASE ('wave') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) r3
        IF (astat /= 0) THEN
          READ(buffer(:lbuf),*,IOSTAT=astat2) r3(:1)
          IF (astat2 /= 0) THEN
            print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
            STOP
          ELSE
            ilambda(kset) = 1
            lambdas(0:ilambda(kset),kset) = (/1.0_DP, r3(1) /)
          ENDIF
        ELSE
          ilambda(kset) = 2
          lambdas(0:ilambda(kset),kset) = (/r3(3), r3(1), r3(2) /)
        ENDIF

      CASE ('esdw') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) WAVE_ERR(kset)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF

      CASE ('beam') dataset

        READ(buffer(:lbuf),'(a1)',IOSTAT=astat) RADIATION(kset)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF
        
        if (RADIATION(kset)=='S') then
          POLARIZB(1,:) = zero; POLARIZB(2,:) =  zero
        endif
        
      CASE ('mono') dataset
        if (.not.FULLPOL(kset)) then
          READ(buffer(:lbuf),*,IOSTAT=astat) MONO_POSIT(kset),COBRA(kset)
          IF (astat /= 0) THEN
             print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
             STOP
          ENDIF
          POLARIZB(1:2,kset) = [COBRA(kset),zero] ! case lab-source; reset for case synchro
        endif

      CASE ('pola') dataset ! prevails
                            ! reading : b/a, phi[deg], product of cosines of all deflection angles

        READ(buffer(:lbuf),*,IOSTAT=astat) r3(1:3)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF
        if (abs(r3(2))<sceps_DP) then
          POLARIZB(1:2,kset) = [r3(1)*abs(r3(3)),zero]
          !POLARIZB(1:2,kset) = [r3(1),r3(2)]
        else
          cphip = cos(degrees_to_radians*r3(2))
          sphip = sin(degrees_to_radians*r3(2))
          omk2 = one-r3(3)**2
          POLARIZB(1:2,kset) = [r3(1)*sqrt( (one-(cphip**2) * omk2) / (one-(sphip**2) * omk2) ), &
                                radians_to_degrees*acos(cphip/sqrt(cphip**2+(sphip*r3(3))**2))]
        endif
        MONO_POSIT(kset) = 1
        COBRA(kset) = r3(3)
        FULLPOL(kset)=.true. !this disables reading kwd 'mono'

      CASE ('geom') dataset

        READ(buffer(:lbuf),*,IOSTAT=astat) GEOM(kset),CORR_ABS(kset)
        IF (astat /= 0) THEN
           print'(1x,a,i3,":",/,a)','Something may be wrong at line ',i,rline
           STOP
        ENDIF

      CASE ('inst') dataset
        READ(buffer(:lbuf),*,IOSTAT=astat) INST_FLAG(kset), INST_6P_var(kset,:),INST_5P_con(kset,:)
        IF (astat /= 0) THEN
          INST_FLAG(kset) = 0
        ENDIF

      CASE DEFAULT dataset

        print*,'Something may be wrong at line ',i
        STOP

      END SELECT dataset

end subroutine ULOOP
!_______________________________________________________________________________________________!_______________________________________________________________________________________________
subroutine ALOOP(rline,ident,buffer,lbuf,i)
  IMPLICIT NONE
  INTEGER, intent(IN)        :: lbuf,i
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(4),intent(IN)    :: ident
  CHARACTER(lbuf),intent(IN) :: buffer
  INTEGER   :: arstat

  print*, 'siamo in ALOOP  ', ident

  amorphous : SELECT CASE(ident)

      CASE ('dcor') amorphous

      READ(buffer(:lbuf),*,iostat=arstat) AMO_DCORR0, AMO_DCORR 
      if (arstat/=0) then
        READ(buffer(:lbuf),*) AMO_DCORR
        AMO_DCORR0 = one
      endif 

      CASE ('path') amorphous

        PATH_NAME(0) = buffer(:lbuf)

      CASE ('parx') amorphous

        PARAMETER_FILE(0) = buffer(:lbuf)

      CASE DEFAULT amorphous

        print*,'Something may be wrong at line ',i
        STOP

      END SELECT amorphous 

end subroutine ALOOP
!_______________________________________________________________________________________________________________________
subroutine VLOOP(rline,ident,buffer,lbuf,kstr,i)
  IMPLICIT NONE
  INTEGER, intent(IN)        :: lbuf,kstr,i
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(4),intent(IN)    :: ident
  CHARACTER(lbuf),intent(IN) :: buffer

  INTEGER(I4B):: astat, astat2, astat3, astat4, k, ieq, ivirg, lastp, luat, coat, &
                copair,check_n2read_ab,check_n2read_c

      structure: SELECT CASE(ident)

      CASE ('db01') structure

        PATH_NAME(kstr) = buffer(:lbuf)
        DB_INDEX(kstr) = 1
        
      CASE ('db02') structure

        PATH_NAME(kstr) = buffer(:lbuf)
        DB_INDEX(kstr) = 2
        
      CASE ('db03') structure

        PATH_NAME(kstr) = buffer(:lbuf)
        DB_INDEX(kstr) = 3
      
      CASE ('db04') structure

        PATH_NAME(kstr) = buffer(:lbuf)
        DB_INDEX(kstr) = 4
        
      CASE ('db05') structure

        PATH_NAME(kstr) = buffer(:lbuf)
        DB_INDEX(kstr) = 5
        
      CASE ('prot') structure

        PROTOTYPE_PHA_FILE(kstr) = buffer(:lbuf)
        call lowcase(PROTOTYPE_PHA_FILE(kstr))
        PROTOTYPING(kstr) = (PROTOTYPE_PHA_FILE(kstr)(1:3)=='yes')
        !_________________________ anything else - no prot
        
      CASE ('nclu') structure

        read(buffer(:lbuf),*,iostat=astat) n2read(kstr)
        !prova
        if (DB_INDEX(kstr)/=3) then
          print*, 'ERROR: db03 deals only with SPH or QBE (please check the ''shap'' or the db code)!'
          STOP
        endif

      CASE ('nrod') structure

        read(buffer(:lbuf),*,iostat=astat) n2read_ab(kstr,:),n2read_c(kstr,:)
        if (DB_INDEX(kstr)/=4) then
           print*, 'ERROR: db04 deals only with PAR,CYL or HEX (please check the ''shap'' or the db code)!'
           STOP
         endif
        
      CASE ('cosh') structure

        read(buffer(:lbuf),*,iostat=astat) n2read_ab(kstr,:),n2read_c(kstr,:)  
        if (DB_INDEX(kstr)/=5) then
            print*, 'ERROR: db05 deals only with COSH (please check the ''shap'' or the db code)!'
            STOP
          endif

      CASE ('dmax') structure

        read(buffer(:lbuf),*) ALL_PHA_INFO(kstr)%diamMAX
        
      CASE ('shap') structure

        read(buffer(:lbuf),*) ALL_PHA_INFO(kstr)%clushape  

      CASE ('chem') structure

        IF (DB_INDEX(kstr) /= 2) THEN
          if (PROTOTYPING(kstr)) then
            lastp = 0
            coat = 0
            do 
              ieq   = SCAN(buffer(lastp+1:lbuf),'=')
              IF (ieq >0) THEN
                coat=coat+1
                lastp = lastp+ieq
              ELSE
                EXIT
              ENDIF
              IF (coat>NSpecMax) THEN
                 print*, 'Too many atomic species : MAX =',NSpecMax
                 STOP
              ENDIF
            enddo
            Nsp_at(kstr) = coat
            copair = coat*(coat+1)
            NPAIR_AT(kstr) = copair/2
  
            lastp = 0
            do k=1,coat ! <= NSpecMax
              
              ieq   = SCAN(buffer(lastp+1:lbuf),'=') + lastp
              IF (ieq == 0) then
                print*,'NO = , NO ATOM'
                STOP
              endif
              ivirg = SCAN(buffer(lastp+1:lbuf),',') + lastp
              IF (ivirg == lastp) THEN
                  ivirg = lbuf + 1
              ENDIF
                ATOM(k,kstr) = '  '
                luat = LEN_TRIM(adjustl(buffer(ieq+1:ivirg-1)))
                ATOM(k,kstr)(:luat) = TRIM(adjustl(buffer(ieq+1:ivirg-1)))
                lastp = ivirg
  
            enddo
          else
            Nsp_at(kstr) = -99999
          endif
        ELSE IF (DB_INDEX(kstr) == 2) THEN
          Nsp_at(kstr) = 1
          NPAIR_AT(kstr) = 1
          ATOM(1,kstr) = '  '
          ATOM(1,kstr)(:lbuf) = buffer(:lbuf)
        ENDIF
    
!!__AC RF 11.07.2015
!      CASE ('natc') structure
!
!        read(buffer(:lbuf),*) NATCEL_PER_SPEC(1:Nsp_at(kstr),kstr)

      CASE ('spgn') structure

        READ(buffer(:lbuf),*) IND_SPACE_GROUP(1,kstr)

      CASE ('cell') structure

        if (PROTOTYPING(kstr)) then
          READ(buffer(:lbuf),*) cell_p(:,kstr)
        else
          cell_p(:,kstr) = -999.d0
        endif
        
      CASE ('parx') structure

        PARAMETER_FILE(kstr) = buffer(:lbuf)
!        print*,'Accepting ',TRIM(PARAMETER_FILE(kstr))

     !! micr
          CASE ('micr') structure ! FB microstrain calc
                            ! reading : FLAG, epsilon 

        READ(buffer(:lbuf),*) MICRO_FLAG(kstr)



      CASE ('limi') structure

        READ(buffer(:lbuf),*) PARAM_LIM(kstr)

      CASE DEFAULT structure

        print*,'Something may be wrong at line ',i, ': ', ident//' '//buffer(:lbuf)
!        STOP

      END SELECT structure

        if (ALL_PHA_INFO(kstr)%clushape=='SPH' .and. n2read(kstr)<=0) then  
            print*, 'ERROR for structure % ',kstr,': SPH shape requires NCLU > 0! The Program stops'
            STOP
        endif
       if (ALL_PHA_INFO(kstr)%clushape=='QBE' .and. n2read(kstr)<=0) then  
            print*, 'ERROR for structure % ',kstr,': QBE shape requires NCLU > 0! The Program stops'
            STOP
        endif
        
        check_n2read_ab=n2read_ab(kstr,1)*n2read_ab(kstr,2)
        check_n2read_c=n2read_c(kstr,1)*n2read_c(kstr,2)
        
        
        if (ALL_PHA_INFO(kstr)%clushape=='PAR' .and. (check_n2read_ab<=0 .or. check_n2read_c<=0)) then  
            print*, 'ERROR for structure % ',kstr,': PAR shape requires 4 nr > 0 in NROD! The Program stops'
            STOP
        endif
        
         if (ALL_PHA_INFO(kstr)%clushape=='HEX' .and. (check_n2read_ab<=0 .or. check_n2read_c<=0)) then  
            print*, 'ERROR for structure % ',kstr,': HEX shape requires 4 nr > 0 in NROD! The Program stops'
            STOP
        endif
        
         if (ALL_PHA_INFO(kstr)%clushape=='CYL' .and. (check_n2read_ab<=0 .or. check_n2read_c<=0)) then  
            print*, 'ERROR for structure % ',kstr,': CYL shape requires 4 nr > 0 in NROD! The Program stops'
            STOP
        endif
end subroutine VLOOP
!_______________________________________________________________________________________________
subroutine ZLOOP(rline,ident,buffer,lbuf,i)
  INTEGER, intent(IN)        :: lbuf,i
  CHARACTER(*),intent(IN)    :: rline
  CHARACTER(4),intent(IN)    :: ident
  CHARACTER(lbuf),intent(IN) :: buffer

  INTEGER(I4B)     :: ls, ll, j
  CHARACTER(132)   :: lline

      refout: SELECT CASE(ident)
      CASE ('simu') refout
        READ(buffer(:lbuf),*) SIMUL_FLAG
      CASE ('calm') refout
        READ(buffer(:lbuf),*) CALC_FLAG
      CASE ('rpdf') refout
        READ(buffer(:lbuf),*) CALC_RPDF
      CASE ('rfil') refout
        REFINEMENT_FILE = buffer(:lbuf)
      CASE ('outs') refout
        OUTPUT_FILE = buffer(:lbuf)
      CASE ('make') refout
        lline = buffer(:lbuf)
            ls = SCAN(lline,',') 
            ll = len_trim(lline)
            call LOWCASE(lline(1:ll))
            IF (ls == 0) THEN
                IF (trim(adjustl(lline(:ll))) == 'par') THEN
                    MAKEFIL_FLAG = 1
                ELSE IF (trim(adjustl(lline(:ll))) == 'ref') THEN
                    MAKEFIL_FLAG = 2
                ENDIF
            ELSE IF (ls > 1) THEN
                IF ( (trim(adjustl(lline(1:ls-1))) == 'par' .and. trim(adjustl(lline(ls+1:ll))) == 'ref') .or. &
                     (trim(adjustl(lline(1:ls-1))) == 'ref' .and. trim(adjustl(lline(ls+1:ll))) == 'par') ) THEN
                    MAKEFIL_FLAG = 3
                ENDIF
            ENDIF
      CASE DEFAULT refout
        print*,'Something may be wrong at line ',i
        STOP
      END SELECT refout

end subroutine ZLOOP
!_______________________________________________________________________________________________
subroutine CHECK_DATASET(kset)
  IMPLICIT NONE
  CHARACTER(132), DIMENSION(30)         :: line_err 
  INTEGER(I4B), intent(IN)                   :: kset
  INTEGER                               :: ier,ll, i,j

  line_err = ' '
  ier = 0
  i = 0
  IF (len_trim(DATA_FILENAME(kset)) == 0) THEN
      ier = -1
      i = i + 1
      line_err(i) = 'DATA_FILENAME missed'
  ENDIF
  IF (FFORM(kset) < 1 .or. FFORM(kset) > 4) THEN
      ier = -1
      i = i + 1
      write(line_err(i),'(a,i3)') 'illegal FFORM value:',FFORM(kset)
  ENDIF
  IF (FFORM(kset) >= 1 .and. FFORM(kset) <= 3) THEN
!     IF (maxval(abs(ANGRANGE(:,kset))) < eps_DP .or. &
!         any(ANGRANGE(:,kset) < 0.0_DP) .or. &
!         ANGRANGE(1,kset) > ANGRANGE(2,kset) ) THEN
!         ier = -1
!         i = i + 1
!         write(line_err(i),'(a,3(1x,g12.6))') 'illegal ANGRANGE values:',ANGRANGE(:,kset)
!     ENDIF
     IF (ANGRANGE(2,kset) < ANGRANGE(1,kset) .or. &
         ANGRANGE(3,kset) < eps_DP) THEN
         ier = -1
         i = i + 1
         write(line_err(i),'(a,3(1x,g12.6))') 'illegal ANGRANGE values:',ANGRANGE(:,kset)
     ENDIF
  ENDIF
  ll = len_trim(BLANK_FILENAME(kset))
  IF (ll == 0 .and. cheb_nc(kset) == 0 .and. young_nc(kset) == 0 ) THEN
      ier = -1
      i = i + 1
      line_err(i) = ' Background information not given '
  ENDIF
  IF (ILAMBDA(kset) == 0 ) THEN
      ier = -1
      i = i + 1
      line_err(i) = ' WAVELENGHT missed '
  ENDIF
  IF (ILAMBDA(kset) == 2 .and. LAMBDAS(0,kset) < eps_DP) THEN
      ier = -1
      i = i + 1
      write(line_err(i),'(a,i3)') 'illegal two wavelengths intensity ratio:',LAMBDAS(0,kset)
  ENDIF
  IF (WAVE_ERR(kset) < -eps_DP) THEN
      ier = -1
      i = i + 1
      write(line_err(i),'(a,i3)') 'negative wavelength_error :',WAVE_ERR(kset)
  ELSE IF ( abs(WAVE_ERR(kset)) < -eps_DP) THEN
      WAVE_ERR(kset) = 0.0_DP
  ENDIF
  IF ( .not. (RADIATION(kset) == 'X' .or.  RADIATION(kset) == 'S' .or. &
              RADIATION(kset) == 'N' .or.  RADIATION(kset) == 'E') ) THEN
      ier = -1
      i = i + 1
      write(line_err(i),'(a)') 'illegal radiation type : '//RADIATION(kset)
  ENDIF
 !! Neutrons and Electrons not allowed [for now] in the distributed version
  IF (RADIATION(kset) == 'N' .or.  RADIATION(kset) == 'E') THEN 
    print*, RADIATION(kset), ' data modeling still under development! The program stops'
  STOP
  ENDIF
  IF ( .not. (GEOM(kset) == 'transmDS' .or.  GEOM(kset) == 'transmFP' .or. &
              GEOM(kset) == 'reflecBB' .or.  GEOM(kset) == 'thinfilm') ) THEN
      ier = -1
      i = i + 1
      write(line_err(i),'(a)') 'illegal Geometry : '//GEOM(kset)
  ENDIF

  IF (ier < 0 ) THEN
      print'(1x,a,i3.3,(/,1x,a))','INPUT ERROR in Datasets section, for Datasets #',kset
      do j=1,i
         print'(1x,a)',line_err(j)
      enddo
      STOP
  ENDIF

  return

end subroutine CHECK_DATASET
!_______________________________________________________________________________________________
subroutine CHECK_STRUCTURE(kstr)
  IMPLICIT NONE
  CHARACTER(132), DIMENSION(30)         :: line_err 
  CHARACTER(132)                        :: check_path
  INTEGER(I4B), intent(IN)              :: kstr
  INTEGER                               :: ier,ll, i,j,k, Z_e
  LOGICAL                               :: path_exists
  real(DP)  :: cellp_def(6)=[one,one,one,90.d0,90.d0,90.d0]

  line_err = ''
  check_path = ''
  ier = 0
  i = 0
  path_exists = .True.


  IF (DB_INDEX(kstr) == 2) THEN
    IF ( TRIM(PATH_NAME(kstr)) /= 'NOPATH') THEN
       ll = len_trim(PATH_NAME(kstr))
       IF (ll > 0) THEN
           check_path = PATH_NAME(kstr)(1:ll-1)
           INQUIRE(file=check_path,exist=path_exists)
           IF (.not. path_exists) THEN
               ier = -1
               i = i + 1
               line_err(i) = PATH_NAME(kstr)(1:ll)//': PATH_NAME missed or wrong'
           ENDIF
       ENDIF
    ENDIF
  ENDIF


  IF (kstr>0) then

    do k=1,Nsp_at(kstr)
      Z_e = -1
      ll = len_trim(ATOM(k,kstr))
      
      do j=0,n_elements
        IF (ATOM(k,kstr)(1:ll) /= TRIM(symb_of_Z(j))) CYCLE
        Z_e = j
        EXIT
      enddo
      IF (Z_e < 0.or.Z_e>n_elements) THEN
        if (PROTOTYPING(kstr)) then
          ier = -1
        else
          ier=111
        endif
        i = i + 1
        write(line_err(i),'(a,1x,a)') 'Unknown element ', ATOM(k,kstr)(1:ll)
      ELSE
        Z_ATOM(k,kstr) = Z_e
      ENDIF
    enddo
    IF (DB_INDEX(kstr) > 2) THEN
      IF (IND_SPACE_GROUP(1,kstr) <= 0 .or.IND_SPACE_GROUP(1,kstr) > 230) THEN
        ier = 230
        i = i + 1
        write(line_err(i),'(a,i3)') 'wrong IND_SPACE_GROUP :',IND_SPACE_GROUP(1,kstr)
      ENDIF
    ENDIF
    IF (DB_INDEX(kstr) == 2) THEN
      do j=1,6
        IF (CELL_P(j,kstr) < eps_DP) THEN
          ier = 1618
          i = i + 1
          write(line_err(i),'(a,i3)') 'wrong cell parameter: ',CELL_P(j,kstr)
        ENDIF
      enddo
      if (ier==1618) CELL_P(:,kstr)=cellp_def
    ENDIF
  ENDIF
  IF (len_trim(PARAMETER_FILE(kstr)) == 0) THEN
      ier = -1
      i = i + 1
      line_err(i) = 'PARAMETER_FILE missed'
  ENDIF

  IF (ier < 0 ) THEN
      print'(1x,a,i3.3,(/,1x,a))','INPUT ERROR in Structure section, for structure %',kstr
      do j=1,i
         print'(1x,a)',line_err(j)
      enddo
      STOP 'FATAL INPUT ERROR in Structure section - stopping'
  ELSE IF (ier > 0 .and. verbose) THEN
      print'(1x,a,i3.3,(/,1x,a))','INPUT WARNING in Structure section, for structure %',kstr
      do j=1,i
         print'(1x,a)',line_err(j)
      enddo
      print*,'No problem anyway, continuing...'
  ENDIF

  return

end subroutine CHECK_STRUCTURE
!_______________________________________________________________________________________________
subroutine PHA_READER(jstr)
implicit none
integer(I4B),intent(IN)       :: jstr
integer(I4B)    :: iu,ll,kat,iat,ll2,iendsym,firstch,lastch,j,jj,isch,sign_ion,abs_ion,jpair
character(132)  :: rl,rl2
integer(I4B)  :: ithis,isnt,ibegsy,iendsy,isnewtype,iidum
real(DP)      :: dumxyz(5)

ALL_PHA_INFO(jstr)%skipme=.false.
ll=len_trim(ALL_PHA_INFO(jstr)%PHA_FILE)

iu=FIND_UNIT()
open(iu,status='old',action = 'read',err=1,file=ALL_PHA_INFO(jstr)%PHA_FILE(:ll))
goto 2
1 print *, 'Not found .PHA file: '//ALL_PHA_INFO(jstr)%PHA_FILE(:ll)
! ACRF may 2014: non-stop
!  stop 'Not found .PHA file! '
2 continue

kat=0
isnewtype=0
do
  read(iu,'(a)',end=3,err=3)rl
  rl=trim(adjustl(rl))
  call clean_line(rl)
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0) cycle
  if (rl(1:1)=='>') cycle
  if (rl(1:5)=='Title') cycle
  if (rl(1:5)=='Cell ') then
     read(rl(6:ll),*) ALL_PHA_INFO(jstr)%cepa(1:6)
     CELL_P(:,jstr) = ALL_PHA_INFO(jstr)%cepa(1:6)
  endif
  if (rl(1:5)=='Space') ALL_PHA_INFO(jstr)%SPAGRO = trim(adjustl(rl(6:ll)))
  if (rl(1:5)=='Coord') then
    do j=6,ll
      if (rl(j:j)/=' ') then
        ibegsy=j
        exit
      endif
    enddo
    iendsy=ibegsy
    do j=ibegsy+1,ll
      if (rl(j:j)==' ') then
        iendsy=j-1
        exit
      endif
    enddo
    read(rl(iendsy+1:ll),*,iostat=isnt)ithis,dumxyz
    if (isnt/=0) then
      kat=kat+1
    else
      isnewtype=1
      kat=max(kat,ithis)
    endif
  endif
  
enddo
3 rewind(iu)

!___________________ Completely useless but I like it...
!IF (abs(ALL_PHA_INFO(jstr)%cepa(1)-CELL_P(1,jstr))>abs(ALL_PHA_INFO(jstr)%cepa(1))*sceps_DP) then
!   print*,'Warning : cell_P(1) not equal to a in .pha',ALL_PHA_INFO(jstr)%cepa(1),CELL_P(1,jstr)
!   print*,' cell_P(1) =',CELL_P(1,jstr)
!   print*,'cell in .pha',ALL_PHA_INFO(jstr)%cepa(1)
! !23.02.2015!      STOP 'wrong : cell_P(1) not equal to a in .pha'
!endif
!do j=1,kat
!   IF (NATCEL_PER_SPEC(j,jstr) == 0) THEN
!       print*, ' Error: # of atoms in cell not supplied for structure: ', jstr
! !23.02.2015!      STOP 
!   ENDIF
!enddo

ALL_PHA_INFO(jstr)%numat_asy_cell = kat
allocate(ALL_PHA_INFO(jstr)%pha_xyzbo(5,kat), &
         ALL_PHA_INFO(jstr)%pha_Z(kat), &
         ALL_PHA_INFO(jstr)%pha_Ion(kat), &
         ALL_PHA_INFO(jstr)%pha_MASSNUMBER(kat), &
         ALL_PHA_INFO(jstr)%pha_SYMB(kat), &
         ALL_PHA_INFO(jstr)%pha_PAIR_averBTH(NPAIR_AT(jstr)), &
         ALL_PHA_INFO(jstr)%pha_PAIR_prodOKK(NPAIR_AT(jstr))  )
         
!         ALL_PHA_INFO(jstr)%pha_MASSNUMBER(1:kat) = NATCEL_PER_SPEC(1:kat,jstr)
iat=0
do
  read(iu,'(a)',end=33,err=33)rl
  rl=trim(adjustl(rl))
  call clean_line(rl)
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (rl(1:5)=='Coord') then

    rl2=trim(adjustl(rl(6:ll)))
    ll2=len_trim(rl2)
    iendsym = index(rl2(1:ll2),' ')-1
    read(rl2(iendsym+1:ll2),*,iostat=isnt)ithis,dumxyz
    if (isnt/=0) then
      iat=iat+1
    else
      iat=ithis
    endif
    do j=iendsym,1,-1
      isch=0
      do jj=1,26
        if (rl2(j:j)==UPCA(jj:jj) .or. rl2(j:j)==LOCA(jj:jj)) then
          isch=1
          exit
        endif
      enddo
      if (isch==0) cycle
      lastch = j
      exit
    enddo

    do j=1,iendsym
      isch=0
      do jj=1,26
        if (rl2(j:j)==UPCA(jj:jj) .or. rl2(j:j)==LOCA(jj:jj)) then
          isch=1
          exit
        endif
      enddo
      if (isch==0) cycle
      firstch = j
      exit
    enddo

    ALL_PHA_INFO(jstr)%pha_SYMB(iat)(1:1+lastch-firstch)=rl2(firstch:lastch)
    if (lastch==firstch) ALL_PHA_INFO(jstr)%pha_SYMB(iat)(lastch+1:2)=' '

    ALL_PHA_INFO(jstr)%pha_Z(iat) = 0
    do j=0,n_elements
      if (ALL_PHA_INFO(jstr)%pha_SYMB(iat)(1:1+lastch-firstch)==trim(symb_of_Z(j)(:))) then
        ALL_PHA_INFO(jstr)%pha_Z(iat) = j
        exit
      endif
    enddo

    ALL_PHA_INFO(jstr)%pha_Ion(iat) = 0
    sign_ion=1
    abs_ion=0
    if (lastch<iendsym) then
!! Syntax type K+, Cl-, Ca+2, P-3
      if (rl2(iendsym:iendsym)=='+') then
        sign_ion=1
      else if (rl2(iendsym:iendsym)=='+') then
        sign_ion=1
      endif
      IF (iendsym-lastch==1) then
        abs_ion = 1
      else
        read(rl2(lastch+1:iendsym-1),*)abs_ion
      endif
    endif
    ALL_PHA_INFO(jstr)%pha_ion(iat) = sign_ion*abs_ion
    if (isnewtype==0) then
      read(rl2(iendsym+1:ll2),*)ALL_PHA_INFO(jstr)%pha_xyzbo(:,iat)
    else
      read(rl2(iendsym+1:ll2),*)iidum,ALL_PHA_INFO(jstr)%pha_xyzbo(:,iat)
    endif
  endif
enddo
33 close(iu)

!!!! VERIFICATION OF SPACE GROUP

ll=len_trim(ALL_PHA_INFO(jstr)%SPAGRO)

!!!! VERIFICATION OF ATOMS

if (ALL_PHA_INFO(jstr)%numat_asy_cell /= Nsp_AT(jstr)) THEN
  print'("PHA_READER : # atoms NOT OK; #DWA, #PHA = ",2i6)',Nsp_AT(jstr),ALL_PHA_INFO(jstr)%numat_asy_cell
  stop "PHA_READER died"
endif
do j=1,Nsp_AT(jstr)
  if (ALL_PHA_INFO(jstr)%pha_Z(j) /= Z_ATOM(j,jstr)) THEN
    print'("PHA_READER : ",i2,"-th ATOM NOT OK; Z_DWA, Z_PHA = ",2i6)',j,Z_ATOM(j,jstr),ALL_PHA_INFO(jstr)%pha_Z(j)
    stop "PHA_READER died"
  endif
enddo

jpair=0
do j=1,Nsp_AT(jstr)
  do jj=j,Nsp_AT(jstr)
    jpair=jpair+1
    ALL_PHA_INFO(jstr)%pha_PAIR_averBTH(jpair) = half * (ALL_PHA_INFO(jstr)%pha_xyzbo(4,j)+ALL_PHA_INFO(jstr)%pha_xyzbo(4,jj))
    ALL_PHA_INFO(jstr)%pha_PAIR_prodOKK(jpair) = ALL_PHA_INFO(jstr)%pha_xyzbo(5,j) * ALL_PHA_INFO(jstr)%pha_xyzbo(5,jj)
  enddo
enddo

end subroutine PHA_READER
!_______________________________________________________________________________________________

END module input_data

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
