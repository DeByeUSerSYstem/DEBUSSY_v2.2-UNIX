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
 PROGRAM NANO

 use special_types
 use linalg_tools
 use input_data
 use data_asread
 use struct_asread
 use DB1_READ
 use DB2_READ
 use DB3_READ
 use DB4_READ
 use DB5_READ
 use param_asread
 use atomix, only: symb_of_Z
 use OBS_WSPACE
 use BACKCO
 use CALC_WSPACE
 use CALCFUN_DB2
 use REFINE_TOOLS
 use strategy
 use REFINEMENT

 IMPLICIT NONE
 Integer (I4B)   :: iudwa, iodwa,lfndwa  
 character(512)  :: fndwa
 !! FB cputime
 real(DP)		:: t0,t1


 call def_eps
 
 fndwa='';lfndwa=0
 CALL GETARG(1, fndwa)
 fndwa=trim(adjustl(fndwa))
 lfndwa=len_trim(fndwa)
 if (lfndwa==0) then
   iudwa = FIND_UNIT()
   OPEN(UNIT=iudwa,status='old', file='debussy.inp',action='READ', iostat=iodwa)
   IF (iodwa /=0) STOP ' File not found: debussy.inp '
   read (iudwa,'(a)') fndwa
   close(iudwa)
   call clean_line(fndwa)
   fndwa = trim(adjustl(fndwa))
   lfndwa=len_trim(fndwa)
 endif
 
 print*, '                                         ------------------------------------'
 print*,'                                                 DebUsSy Suite v2.2   '
 print*, '                                         ------------------------------------'
 print*,' '
 print*, ' DebUsSy Program is running on Input File: ',fndwa(1:lfndwa)
 print*,' '
 !!FB cputime
 call CPU_TIME(t0)
 main_name = ''
! main_name = '.'//separator//trim(fndwa)
 main_name = trim(fndwa)
 CALL DO_INPUT

 IF (SIMUL_FLAG == 1) THEN
         print*, ' '
         print*, '                  *********** SIMULATION SECTION *********** '
         print*, ' '
     kpolfin = -1
     CALL DO_SIMUL
     call DO_OUTPUT
     print*, ' '
     print*, ' ******* DebUsSy Simulation DONE!  ******* '

  ELSE IF (SIMUL_FLAG == 0) THEN
     kpolfin = 0
     CALL DO_INIT
     IF (len_trim(REFINEMENT_FILE) > 0) THEN
         call read_ref_file
         print*, ' '
         print*, '                  *********** REFINEMENT SECTION *********** '
         print*, ' '
         print*, ' ----------------- '
         print*, ' Refinement file = ',REFINEMENT_FILE
         print*, ' ----------------- '
         print*, ' '
         print*, '  Number of stages # ', size(ref_stage)
         print*, ' ------------------------------------------------------ '
     CALL DO_REFINE
     call DO_OUTPUT   !!! OUTPUT_FILE is now used also for SIMUL_FLAG = 1
     !call system('gnuplot < plot1.gnu')
     
     !!FB test time
     call CPU_TIME(t1)
     write(*,*)  '------->   CPU time = ',t1-t0,' s'
     print*, ' '
     print*, ' ******* DebUsSy Refinement DONE!  ******* '

        ELSE
             print*, 'SIMULATION or REFINEMENT must be specified in .dwa file!'
     ENDIF

  ELSE IF (SIMUL_FLAG <0) THEN
     kpolfin = 1
     diag_modus_covm = min(2,abs(SIMUL_FLAG))
     !print*, 'kpolfin ',kpolfin
     CALL DO_INIT
     IF (len_trim(REFINEMENT_FILE) > 0) THEN
         call read_ref_file
         print*, ' '
         print*, '                  *********** STANDARD ERRORS SECTION *********** '
         print*, ' '
         print*, ' ----------------- '
         print*, ' Refinement file = ',REFINEMENT_FILE
         print*, ' ----------------- '
         print*, ' '
         print*, '  Number of stages # ', size(ref_stage)
         print*, ' ------------------------------------------------------ '
     CALL DO_REFINE
     call DO_OUTPUT   !!! OUTPUT_FILE is now used also for SIMUL_FLAG = 1
     !call system('gnuplot < plot1.gnu')
     
     print*, ' '
     print*, ' ******* DebUsSy Standard Errors Calculation DONE!  ******* '


        ELSE
             print*, 'SIMULATION or REFINEMENT or STANDARD ERRORS must be specified in .dwa file!'
     ENDIF

 ENDIF
 
 

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 contains 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 subroutine DO_INPUT
  implicit none
  INTEGER(I4B)         :: kset, kstr, j, jj, k, ll, nasy, ja,zja,ja2,zja2,jjp
  character(100)            :: fmt
  logical                   :: sim_sum=.false. ! FB mar17 *sim.sum

  call input_main
  call read_obs_data

  print*, '                     *********** DATASETS SECTION *********** '
  print*, ' '
  print*, 'Number of input datasets = ',nset
  do kset=1,nset
    print*, ' ------------------------------------------------------ '
    print*, ' Input data for  dataset # ',kset
    print*, ' Data File : ',DATA_FILENAME(kset)
    print*, ' Number of Points #',NDATA(kset)
    print*, ' Angular  Range ',ANGRANGE(:,kset)
    print*, ' Wavelengths : ',ilambda,lambdas(0:ilambda(kset),kset)
    print*, ' Radiation Type :',RADIATION(kset)
    print*, ' Geometry :',GEOM(kset)
  enddo
  print*, ' ------------------------------------------------------ '

  print*, ' '
  print*, '                     *********** STRUCTURES SECTION *********** '
  print*, ' '
  print*, 'Number of input Structure Types = ',nstr

  IF (ANY(DB_INDEX == 1) ) call read_struk_data
  IF (ANY(DB_INDEX >= 3) ) call read_struk_data
  
  CALL TO_DB_READERS
  
  IF (ANY(DB_INDEX == 1) ) call DB1_READER
  IF (ANY(DB_INDEX == 2) ) call DB2_READER
  IF (ANY(DB_INDEX == 3) ) call DB3_READER
  IF (ANY(DB_INDEX == 4) ) call DB4_READER
  IF (ANY(DB_INDEX == 5) ) call DB5_READER
  do kstr=1,nstr
    if (DB_INDEX(kstr) == 1) PROTOTYPING(kstr) = .false.
    if (.not. PROTOTYPING(kstr)) then
      print'("Original phase # ",i3)',kstr
      ALL_PHA_INFO(kstr)%numat_asy_cell = nano_iav(kstr)%struk(1)%numspat
      Nsp_at(kstr) = nano_iav(kstr)%struk(1)%numspat
      NPAIR_AT(kstr)=(Nsp_at(kstr)*(Nsp_at(kstr)+1)) / 2
      CELL_P(:,kstr) = nano_iav(kstr)%struk(1)%abcabg_db(:)
    else if (PROTOTYPING(kstr)) then
      print'("Modified phase # ",i3)',kstr
    !___________ if a different cell has been read in the "cell" kwd of .dwa
      if ( ALL(CELL_P(:,kstr)>1.d0) ) then
        do j=1,size(nano_iav(kstr)%struk)
          nano_iav(kstr)%struk(j)%abcabg_db(:) = CELL_P(:,kstr)
        enddo
        print'("Modified CELL : ",6(1x,f10.6))',CELL_P(:,kstr)
      else
        CELL_P(:,kstr) = nano_iav(kstr)%struk(1)%abcabg_db(:)
        print'("Original CELL : ",6(1x,f10.6))',CELL_P(:,kstr)
      endif
    !___________ if a different composition has been read in the "chem" kwd of .dwa
      if ( Nsp_at(kstr) > 0 .and. Nsp_at(kstr) <= NSpecMax ) then
        print'("Modified CHEM : ",10(1x,a2))',ATOM(1:Nsp_at(kstr),kstr)
        do j=1,size(nano_iav(kstr)%struk)
          nano_iav(kstr)%struk(j)%numspat = Nsp_at(kstr)
        enddo
        ALL_PHA_INFO(kstr)%numat_asy_cell = Nsp_at(kstr)
        NPAIR_AT(kstr)=(Nsp_at(kstr)*(Nsp_at(kstr)+1)) / 2
        jjp=0
        do ja=1,Nsp_at(kstr)
          zja=Z_OF_SYMB(ATOM(ja,kstr))
          do j=1,size(nano_iav(kstr)%struk)
            nano_iav(kstr)%struk(j)%Z_at(ja) = zja
          enddo
          do ja2=ja,Nsp_at(kstr)
            zja2=Z_OF_SYMB(ATOM(ja2,kstr))
            jjp=jjp+1
            do j=1,size(nano_iav(kstr)%struk)
              nano_iav(kstr)%struk(j)%zappa(:,jjp) = [ zja,zja2 ]
            enddo
          enddo
        enddo
      else
        print'("Original CHEM : ",10(1x,a2))',(symb_of_Z(nano_iav(kstr)%struk(1)%Z_at(ja)),ja=1,Nsp_at(kstr))
        Nsp_at(kstr) = nano_iav(kstr)%struk(1)%numspat
        ALL_PHA_INFO(kstr)%numat_asy_cell = Nsp_at(kstr)
        NPAIR_AT(kstr)=(Nsp_at(kstr)*(Nsp_at(kstr)+1)) / 2
      endif
    !____________ if none of the two then what? just another monday?
    endif
  enddo
  
  call read_param_data
  
  IF (DO_AMORPH) THEN
    kstr=0
    print*, ' ------------------------------------------------------ '
    print*, ' Input structure type % ',kstr
    print*, ' Structure type ',' AMORPHOUS PART'
    print*, ' ----------------- '
    print*, ' Parameter file : ',TRIM(PATH_NAME(kstr))//PARAMETER_FILE(kstr)
    print*, ' ----------------- '
    print*, ' Parameter       Lower Bound      Value         Upper Bound         Ref-Flag '
!     do j=1,Param_data(kstr)%npar_ref     ! AG 10.05.2012 modified structure of .par file
    do j=1,npar0_ref(DB_INDEX(kstr))   
      print'(2x,a6,8x,3(g15.8,2x),i8)', Param_data(kstr)%NamePar(j),Param_data(kstr)%PhasePar(:,j),&
                                      Param_data(kstr)%flag_ref(j)
    enddo
  ENDIF

  do kstr=1,nstr
    print*, ' ------------------------------------------------------ '
    print*, ' Input Structure type % ',kstr
    print*, ' Structure Name : ',STRUCTURE_NAME(kstr)
    print'(a20,6(2x,f10.6))', ' Cell parameters : ',(CELL_P(jj,kstr),jj=1,6)
    
    nasy = ALL_PHA_INFO(kstr)%numat_asy_cell
    
    IF (DB_INDEX(kstr) /= 2) THEN
      write(fmt,*) nasy
      fmt=trim(adjustl(fmt))
      ll = len_trim(fmt)
      print'(1x,a,2x,'//fmt(1:ll)//'(1x,i2))', &
             ' Atomic species in cell [Z]          : ', nano_iav(kstr)%struk(1)%Z_at(1:Nsp_at(kstr))
      print'(1x,a,2x,'//fmt(1:ll)//'(1x,a2))', &
             ' Atomic species in cell [symbol]     : ', (symb_of_Z(nano_iav(kstr)%struk(1)%Z_at(jj)),jj=1,Nsp_at(kstr))
      print'(1x,a,2x,'//fmt(1:ll)//'(1x,i12))', &
    ' # of atoms per each species in cell : ', nano_iav(kstr)%struk(1)%nat(1:Nsp_at(kstr))
    
    ENDIF   
    print*, ' Database Code : ',DB_INDEX(kstr)
    print*, ' Database PATH : ',trim(PATH_NAME(kstr))
    print*, ' Database # of Files : ',n2read(kstr)
    IF (DB_INDEX(kstr) == 1) THEN
        print*, ' Number of atoms in the Cluster/Molecule = ', sum(nano_iav(kstr)%struk(1)%nat(1:Nsp_at(kstr)))
        print*, ' Number of atomic species =', Nsp_at(kstr)
    ENDIF
    print*, ' '
    print*, ' Parameter file : ',trim(PARAMETER_FILE(kstr))
    print*, '                           ----------------- '
    print*, ' Parameter       Lower Bound      Value         Upper Bound         Ref-Flag '
    
    do j=1,npar0_ref(DB_INDEX(kstr)) 
       print'(2x,a6,8x,3(g15.8,2x),i8)', Param_data(kstr)%NamePar(j),Param_data(kstr)%PhasePar(:,j),&
                                      Param_data(kstr)%flag_ref(j)
    enddo
    do jj=1,ALL_PHA_INFO(kstr)%numat_asy_cell
         if (ANY(Param_data(kstr)%law_refOB(jj,:) < 1)) Param_data(kstr)%law_refOB(jj,:) = 1
         print'(2x,a6,8x,2(i8))', Param_data(kstr)%NameAto(jj), Param_data(kstr)%law_refOB(jj,:)
       do k=1,npar_x_ato
         j = npar0_ref(DB_INDEX(kstr)) + npar_x_ato*(jj-1) + k 
         print'(2x,a6,8x,3(g15.8,2x),i8)', Param_data(kstr)%NamePar(j),Param_data(kstr)%PhasePar(:,j),&
                                      Param_data(kstr)%flag_ref(j)
       enddo
    enddo   
  enddo

 end subroutine DO_INPUT

 subroutine DO_SIMUL
  IMPLICIT NONE
  integer(I4B) :: iok

 call DO_INIT
 !FB mar17 simsum 
 simsum=FIND_UNIT()
 open(simsum, status='replace', file=path_output_files(1:lpath_output_files)//trim(OUTPUT_FILE_W)//'_sim.sum', &
 action='WRITE')
 write(simsum,('(1x,a)')) '_____   '//TRIM(OUTPUT_FILE_W)//'     SUMMARY LOGFILE   _____'
 write(simsum,*) ' '
 sim_sum=.true. 
 
  call LOOP_CALC(ungar=iok)
  if (iok==0) then
    print*,'inconsistent simulation parameters!!!'
    stop
  endif
  call DO_FIRST_ICAL
 end subroutine DO_SIMUL

 subroutine DO_INIT
  IMPLICIT NONE

  call assoc_OBS
  
  call prepare_BACK
  call assoc_CAL
  call SETUP_CYCLE_START


 end subroutine DO_INIT

 subroutine assoc_obs
!____ Aliasing data vectors for flexibility

  IMPLICIT NONE

    IF (ASSOCIATED(ALL_PHA_INFO_W)) NULLIFY(ALL_PHA_INFO_W)
    ALL_PHA_INFO_W => ALL_PHA_INFO
    IF (ASSOCIATED(OBS_DATA_W)) NULLIFY(OBS_DATA_W)
    OBS_DATA_W => Obs_data
    IF (ASSOCIATED(IRF_CURVES_W)) NULLIFY(IRF_CURVES_W)
    IRF_CURVES_W => IRF_Curves
    IF (ASSOCIATED(INST_FLAG_W)) NULLIFY(INST_FLAG_W)
    INST_FLAG_W => INST_FLAG
    IF (ASSOCIATED(NDATA_W)) NULLIFY(NDATA_W)
    NDATA_W   => NDATA
    IF (ASSOCIATED(N2USE_W)) NULLIFY(N2USE_W)
    N2USE_W   => N2USE
    IF (ASSOCIATED(ILAMBDA_W)) NULLIFY(ILAMBDA_W)
    ILAMBDA_W => ILAMBDA
    IF (ASSOCIATED(MONO_POSIT_W)) NULLIFY(MONO_POSIT_W)
    MONO_POSIT_W => MONO_POSIT
    IF (ASSOCIATED(COBRA_W)) NULLIFY(COBRA_W)
    COBRA_W => COBRA
    IF (ASSOCIATED(LAMBDAS_W)) NULLIFY(LAMBDAS_W)
    LAMBDAS_W => LAMBDAS
    IF (ASSOCIATED(POLARIZB_W)) NULLIFY(POLARIZB_W)
    POLARIZB_W => POLARIZB
    IF (ASSOCIATED(CELL_P_W)) NULLIFY(CELL_P_W)
    CELL_P_W => CELL_P
    IF (ASSOCIATED(DB_INDEX_W)) NULLIFY(DB_INDEX_W)
    DB_INDEX_W => DB_INDEX
    IF (ASSOCIATED(ITYPE_DB_W)) NULLIFY(ITYPE_DB_W)
    ITYPE_DB_W => ITYPE_DB
    IF (ASSOCIATED(Z_ATOM_W)) NULLIFY(Z_ATOM_W)
    Z_ATOM_W => Z_ATOM
    IF (ASSOCIATED(NSP_AT_W)) NULLIFY(NSP_AT_W)
    NSP_AT_W => NSP_AT
    IF (ASSOCIATED(NPAIR_AT_W)) NULLIFY(NPAIR_AT_W)
    NPAIR_AT_W => NPAIR_AT

    IF (ASSOCIATED(REFINEMENT_FILE_W)) NULLIFY(REFINEMENT_FILE_W)
    REFINEMENT_FILE_W => REFINEMENT_FILE
    IF (ASSOCIATED(OUTPUT_FILE_W)) NULLIFY(OUTPUT_FILE_W)
    OUTPUT_FILE_W => OUTPUT_FILE
    IF (ASSOCIATED(NSPECMAX_W)) NULLIFY(NSPECMAX_W)
    NSPECMAX_W => NSPECMAX
    IF (ASSOCIATED(NSTR_W)) NULLIFY(NSTR_W)
    NSTR_W => NSTR
    IF (ASSOCIATED(NSET_W)) NULLIFY(NSET_W)
    NSET_W => NSET
    IF (ASSOCIATED(NAMO_W)) NULLIFY(NAMO_W)
    NAMO_W => NAMO
    IF (ASSOCIATED(NSET_BACK_W)) NULLIFY(NSET_BACK_W)
    NSET_BACK_W => NSET_BACK
    IF (ASSOCIATED(TECHNIQUE_W)) NULLIFY(TECHNIQUE_W)
    TECHNIQUE_W => TECHNIQUE
    IF (ASSOCIATED(CHEB_NC_W)) NULLIFY(CHEB_NC_W)
 if (.not. allocated(CHEB_NC)) then
   print*,'ACHTUNG: CHEB_NC is not allocated! '
 else
    CHEB_NC_W => CHEB_NC
 endif
    IF (ASSOCIATED(YOUNG_NC_W)) NULLIFY(YOUNG_NC_W)
    YOUNG_NC_W => YOUNG_NC
    IF (ASSOCIATED(BLANK_FILENAME_W)) NULLIFY(BLANK_FILENAME_W)
    BLANK_FILENAME_W => BLANK_FILENAME
    IF (ASSOCIATED(BLANK_NCOMPS_W)) NULLIFY(BLANK_NCOMPS_W)
    BLANK_NCOMPS_W => BLANK_NCOMPS
    IF (ASSOCIATED(STRUCTURE_NAME_W)) NULLIFY(STRUCTURE_NAME_W)
    STRUCTURE_NAME_W => STRUCTURE_NAME
    IF (ASSOCIATED(POROD_BKG_W)) NULLIFY(POROD_BKG_W)
    POROD_BKG_W => POROD_BKG
    IF (ASSOCIATED(XDATA_TYPE_W)) NULLIFY(XDATA_TYPE_W)
    XDATA_TYPE_W => XDATA_TYPE
    IF (ASSOCIATED(RADIATION_W)) NULLIFY(RADIATION_W)
    RADIATION_W => RADIATION
    IF (ASSOCIATED(QMAX_D_W)) NULLIFY(QMAX_D_W)
    QMAX_D_W => QMAX_D
    IF (ASSOCIATED(AMO_DCORR_W)) NULLIFY(AMO_DCORR_W)
    AMO_DCORR_W => AMO_DCORR
    IF (ASSOCIATED(AMO_DCORR0_W)) NULLIFY(AMO_DCORR0_W)
    AMO_DCORR0_W => AMO_DCORR0
    IF (ASSOCIATED(DO_AMORPH_W)) NULLIFY(DO_AMORPH_W)
    DO_AMORPH_W => DO_AMORPH
    IF (ASSOCIATED(SIM_NODATA_W)) NULLIFY(SIM_NODATA_W)
    SIM_NODATA_W => SIM_NODATA

    OBS_WSPACE_DONE = .true.

  end subroutine assoc_obs


 subroutine DO_FIRST_ICAL
  implicit none
  real(DP)                  :: v
  
  call RENORM_CAL
 
  call conv_IRF 
    
  call refy_SCAL

  call PUT_TOGETHER
!  call CHISQ_TOT(v)

 end subroutine DO_FIRST_ICAL
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 subroutine DO_OUTPUT
 use calc_hkl
    
  implicit none
  INTEGER(I4B)                       :: i, iu, iur, iu1, lns,lf
  CHARACTER(5)                       :: line_set
  CHARACTER(512), DIMENSION(NSET)    :: outcal
  CHARACTER(512)                     :: outdis
  
  IF (SIMUL_FLAG == 0) RETURN

! writing .cal files
       call PREPPATH()

  outcal = ' '
  DO i=1,nset
     write(line_set(1:2),'(i2.2)') i
     outcal(i) = trim(OUTPUT_FILE_W)//'_'//trim(RADIATION_W(i))//'PD#'//line_set(1:2)//'.cal'
     
     lns = len_trim(outcal(i))
     iu = FIND_UNIT()
     OPEN(UNIT=iu,status='replace', &
           file=TRIM(outcal(i)),action='write')
     iur=0
     if (CALC_RPDF==1) then
       iur= FIND_UNIT()
       OPEN(UNIT=iur,status='replace', &
           file=TRIM(outcal(i))//'.rpdf',action='write')
     endif
     call write_cal_file(i,iu,iur)
     close(iu)
     if (CALC_RPDF==1) close(iur)
  ENDDO
  IF (nset == 1) call SYSTEM(trim(cp_command)//' '//trim(outcal(1))//' stage_best.cal')
  
  if (ANY(DB_INDEX_W>1)) then
     line_set(1:4) ='Best'
     iu1=find_unit()
     outdis = trim(OUTPUT_FILE_W)//'_'//line_set(1:4)//'.dis'
    open(iu1,status='replace',file=TRIM(outdis),action='WRITE')
    call write_varia_fileX(kset=1,iu=iu1)
    close(iu1)
  endif
        !lf=len_trim(main_name)
        !call hkl_gen (main_name(1:lf))  
     

 end subroutine DO_OUTPUT
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 SUBROUTINE write_cal_file(kset,iu,iur)

  implicit none
  INTEGER(I4B),INTENT(IN)         :: kset, iu, iur
  INTEGER(I4B)                    :: i, jj, n0, nn
  character(256)                  :: wline
  real(DP)        :: r_pdf(10000),difpdf(10000,3)

  wline = ' '
  IF (CALC_FLAG == 0) THEN
      IF ( XDATA_TYPE_W(kset) == 'A') THEN
          do jj=1,NDATA_W(kset)
             write(iu,'(3g13.6)') Obs_data_W(kset)%t2data(jj), Obs_data_W(kset)%vdata(jj), CALTOT_W(kset)%vdata(jj)
          enddo
      ELSE
          do jj=1,NDATA_W(kset)
             write(iu,'(3g13.6)') Obs_data_W(kset)%qdata(jj,1), Obs_data_W(kset)%vdata(jj), CALTOT_W(kset)%vdata(jj)
          enddo
      ENDIF
  ELSE
      do jj=1,NDATA_W(kset)
         n0 = 1
         nn = 16
         IF (XDATA_TYPE_W(kset) == 'A') THEN
             write(wline(n0:nn),'(1x,f15.8)') Obs_data_W(kset)%t2data(jj)
         ELSE
             write(wline(n0:nn),'(1x,f15.8)') Obs_data_W(kset)%qdata(jj,1)
         ENDIF
         n0 = nn+1
         nn = nn+32
         write(wline(n0:nn),'(2(1x,g15.9))') Obs_data_W(kset)%vdata(jj), CALTOT_W(kset)%vdata(jj)
         
         IF (CALC_FLAG==1) then
           do i=1,NSTR_W
             n0 = nn+1
             nn = nn+16
             write(wline(n0:nn),'(1x,g15.9)') CALPHA_W(kset,i)%vdata(jj)*Scales(kset)%STRscal(i)*Scales(kset)%SETscal 
           enddo
         endif
         n0 = nn+1
         IF (DO_AMORPH_W) THEN
           nn = nn+48
           write(wline(n0:nn),'(3(1x,g15.9))') BACKGROUND(kset)%lin_bkgr_tot(jj), &
                                               BACKGROUND(kset)%lin_bkgr_blank(jj), AMORPHOUS(kset)%amo_scat_tot(jj)
         ELSE
           nn = nn+32
           write(wline(n0:nn),'(2(1x,g15.9))') BACKGROUND(kset)%lin_bkgr_tot(jj), BACKGROUND(kset)%lin_bkgr_blank(jj)
         ENDIF
         write(iu,'(a)') trim(wline)
      enddo
  ENDIF
  if (CALC_RPDF==1) then
    r_pdf=[(jj*0.01d0,jj=1,10000)]
    call Brutal_sincTransf(Y=Obs_data_W(kset)%vdata(:),R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,1),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
    call Brutal_sincTransf(Y = CALTOT_W(kset)%vdata(:),R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,2),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
    call Brutal_sincTransf(Y = BACKGROUND(kset)%lin_bkgr_tot(:)+BACKGROUND(kset)%lin_bkgr_blank(:), &
                           R=r_pdf,tt=Obs_data_W(kset)%t2data(:),F=difpdf(:,3),isetin=kset, &
                           xlamw=LAMBDAS_W(1,kset))
    do jj=1,10000
      write(iur,'(f8.2,3(1x,g15.9))')r_pdf(jj),difpdf(jj,:)
    enddo
  endif
 END SUBROUTINE write_cal_file
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 END PROGRAM NANO
