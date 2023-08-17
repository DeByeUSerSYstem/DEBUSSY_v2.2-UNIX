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
MODULE DATA_ASREAD
use INPUT_DATA
use rhapsody_in_blue
use specfun_AC

TYPE(data_SETS),DIMENSION(:),POINTER        :: Obs_data => NULL()
TYPE(IRF_SETS),DIMENSION(:),POINTER         :: IRF_Curves => NULL()

!**************
CONTAINS
!**************



SUBROUTINE READ_OBS_DATA
integer(I4B)        :: i,iu,astat,j, knt, knt2, nn, iwl,lrlc,muck_step
character(len=1024) :: rlc
REAL(DP),allocatable :: wks(:,:),wksf4(:,:)
real(DP)  :: check_step,adiff_step,xckrs,ggg_step,xye(3)
integer(I4B) :: kntx,nskip,iskip,intx,astat2,nwksf4


ALLOCATE(Obs_data(NSET),IRF_Curves(NSET))

DO i=1,NSET
  IF (TRIM(TECHNIQUE(i))/='XRD' .and. TRIM(TECHNIQUE(i))/='NPD' .and. &
      TRIM(TECHNIQUE(i))/='EPD') CYCLE
  if (TRIM(DATA_FILENAME(i))=='none') then
    SIM_NODATA = .true.
    NDATA(i) = 1 + nint( (ANGRANGE(2,i) - ANGRANGE(1,i))/ANGRANGE(3,i) )
    ALLOCATE(Obs_data(i)%vdata(NDATA(i)), &
    Obs_data(i)%wdata(NDATA(i)), &
    Obs_data(i)%t2data(NDATA(i)), &
    Obs_data(i)%qdata(NDATA(i),ilambda(i)) )
    
    Obs_data(i)%vdata=1.d6
    Obs_data(i)%wdata=1.d-6
    Obs_data(i)%t2data=[(ANGRANGE(1,i)+ANGRANGE(3,i)*real(j,DP),j=0,NDATA(i)-1)]
    Obs_data(i)%num_data = NDATA(i)
    Obs_data(i)%step_data_t2 = ANGRANGE(3,i)
    do iwl = 1,ilambda(i)
      Obs_data(i)%qdata(:,iwl) = (two/lambdas(iwl,i)) * [( sin(duet2r*Obs_data(i)%t2data(j)), j=1,NDATA(i) )] 
    enddo
    QMAX_D = MAX(QMAX_D,MAXVAL(Obs_data(i)%qdata))
    
    IF (INST_FLAG(i) > 0) then
      ALLOCATE(IRF_Curves(i)%G_sigma_sqrd(NDATA(i)), &
               IRF_Curves(i)%L_w(NDATA(i)), &
               IRF_Curves(i)%Triang(NDATA(i)) )
      IRF_Curves(i)%G_sigma_sqrd = zero; IRF_Curves(i)%L_w = zero; IRF_Curves(i)%Triang = zero
      call wid_gett(opt=INST_FLAG(i), arr6p=INST_6P_var(i,1:6), ttarr=Obs_data(i)%t2data, &
                    sig2arr = IRF_Curves(i)%G_sigma_sqrd, wlarr=IRF_Curves(i)%L_w)
      where (abs(Obs_data(i)%t2data-90.d0) > sceps_DP)
        IRF_Curves(i)%Triang = INST_6P_var(i,7) / tan(Obs_data(i)%t2data*degrees_to_radians)
      elsewhere
        IRF_Curves(i)%Triang = zero
      end where
    ENDIF

    cycle
  endif
  
  IF (FFORM(i) == 4) then
    if (ALLOCATED(wksf4)) deallocate(wksf4)
    call read_xye(filen=TRIM(DATA_FILENAME(i)), nhead=NSKIP_HEAD(i), ntail=NSKIP_FOOT(i), &
                  ttmin=ANGRANGE(1,i), ttmax=ANGRANGE(2,i), ttstep=ANGRANGE(3,i), p_skip=N_EVERY(i), &
                  npr=nwksf4, ppr=wksf4 )
    NDATA(i) = nwksf4
    check_step = ANGRANGE(3,i) * real(N_EVERY(i),DP)
    
    ALLOCATE(Obs_data(i)%vdata(NDATA(i)), &
    Obs_data(i)%wdata(NDATA(i)), &
    Obs_data(i)%t2data(NDATA(i)), &
    Obs_data(i)%qdata(NDATA(i),ilambda(i)) )
    
    Obs_data(i)%vdata=wksf4(2,:)
    Obs_data(i)%wdata=(one/wksf4(3,:))**2
    Obs_data(i)%t2data=wksf4(1,:)
    Obs_data(i)%num_data = NDATA(i)
    Obs_data(i)%step_data_t2 = check_step
    do iwl = 1,ilambda(i)
      Obs_data(i)%qdata(:,iwl) = (two/lambdas(iwl,i)) * [( sin(duet2r*Obs_data(i)%t2data(j)), j=1,NDATA(i) )] 
    enddo
    QMAX_D = MAX(QMAX_D,MAXVAL(Obs_data(i)%qdata))
    
    print'(a,i4,a)','FORM 4 DATASET # ',i,' :: '
    print'(3(a,1x,f20.12,/))','READING DATA: detected angular step Dtt0 = ',ANGRANGE(3,i),&
                              '              decimated every n          = ',real(N_EVERY(i),DP), &
                              '              decimated step Dtt1        = ',check_step
    
    
    IF (INST_FLAG(i) > 0) then
      ALLOCATE(IRF_Curves(i)%G_sigma_sqrd(NDATA(i)), &
               IRF_Curves(i)%L_w(NDATA(i)), &
               IRF_Curves(i)%Triang(NDATA(i)) )
      IRF_Curves(i)%G_sigma_sqrd = zero; IRF_Curves(i)%L_w = zero; IRF_Curves(i)%Triang = zero
      call wid_gett(opt=INST_FLAG(i), arr6p=INST_6P_var(i,1:6), ttarr=Obs_data(i)%t2data, &
                    sig2arr = IRF_Curves(i)%G_sigma_sqrd, wlarr=IRF_Curves(i)%L_w)
      where (abs(Obs_data(i)%t2data-90.d0) > sceps_DP)
        IRF_Curves(i)%Triang = INST_6P_var(i,7) / tan(Obs_data(i)%t2data*degrees_to_radians)
      elsewhere
        IRF_Curves(i)%Triang = zero
      end where
    ENDIF
    
    cycle
  endif
  
  ! the following only for FFORM = 1, 2, 3
  
  iu = FIND_UNIT()
  OPEN(UNIT=iu,status='old', &
        form='formatted',access='sequential', &
        file=TRIM(DATA_FILENAME(i)),action='READ',iostat=astat)
  IF (astat /=0) THEN
    print'(1x,a,a,a,i5)','Error opening file ',TRIM(DATA_FILENAME(i)),' dataset #',i
    STOP
  ENDIF


!_______ Only for synchrotron data, decimation possible, otherwise read all points
  IF (FFORM(i) /= 4) then
    N_EVERY(i)=1
    nskip=0
  endif
!_______ Establish n. of observations

  knt = 0
  IF (NDATA(i) == 0) THEN
    do
      read(iu,*,iostat=astat)rlc
      if (astat/=0) exit
      rlc=trim(adjustl(rlc))
      lrlc=len_trim(rlc)
      if (lrlc==0) cycle
      knt = knt+1
    enddo
    rewind(iu)
    NDATA(i) = knt - NSKIP_FOOT(i) - NSKIP_HEAD(i)
  ENDIF
  IF (NDATA(i) <=0 ) THEN
      print*, 'No data to read for dataset #',i
      print*, ' REASONS:  knt,ndata   ,nskip_foot   ,nskip_head   ',&
      knt,ndata(i),nskip_foot(i),nskip_head(i)
      STOP
  ENDIF

!_______ Header skipping
  DO j=1,NSKIP_HEAD(i)
    read(iu,*)
  ENDDO

  IF (FFORM(i) == 1) THEN
    nn = 1
  ELSE IF (FFORM(i) == 2) THEN
    nn = 2
  ELSE IF (FFORM(i) == 3) THEN
    nn = 2
  ENDIF

  IF (ALLOCATED(wks)) DEALLOCATE(wks)
  ALLOCATE(wks(nn,NDATA(i) ))
  
  READ(iu,*,iostat=astat) wks
  IF (astat /=0) THEN
    print'(1x,a,i7,a,i1,a,a,a,i3)','Error reading ',NDATA(i),' data of form ',FFORM(i),&
    ' from file ',TRIM(DATA_FILENAME(i)),' dataset #',i
    STOP 'Error reading data ...'
  ENDIF
  close(iu)

!!! Apply cutoff from ANGRANGE when possible

  IF ((FFORM(i) == 2 .or. FFORM(i) == 4) .and. ANY(ABS(ANGRANGE(:,i))>180.d0)) THEN
    print*, 'PROBLEM: File format #',fform,': angular range (2theta_min, 2theta_max, 2theta_step) missing. STOP'
    print*, 'Please fill them in after the keyword "rang" in the .dwa file'
    stop 'Missing angular range'
  ENDIF
  knt = 1
  knt2 = NDATA(i)
  IF ((FFORM(i) == 2 .or. FFORM(i) == 4)) THEN
    knt = 0
    knt2 = NDATA(i) + 1
    do j=1,NDATA(i)
      IF (wks(1,j) < ANGRANGE(1,i)) knt  = j
      IF (wks(1,j) > ANGRANGE(2,i)) THEN
        knt2 = j
        EXIT
      ENDIF
    enddo
    knt = MAX(1,knt+1)
    knt2= MIN(knt2-1,NDATA(i))

    NDATA(i) = knt2 - knt + 1
  ELSE IF (FFORM(i) == 1 .or. FFORM(i) == 3) THEN
    nnn = 1 + nint( (ANGRANGE(2,i) - ANGRANGE(1,i))/ANGRANGE(3,i) )
    IF (nnn /= NDATA(i)) THEN
      print*, ' Mismatching between NDATA and ANGRANGE values for dataset # ',i
      STOP
    ENDIF
  ENDIF
  
  ALLOCATE(Obs_data(i)%vdata(NDATA(i)), &
           Obs_data(i)%wdata(NDATA(i)), &
           Obs_data(i)%t2data(NDATA(i)), &
           Obs_data(i)%qdata(NDATA(i),ilambda(i)) )

  IF (FFORM(i) == 1) THEN ! only I (counts)

    Obs_data(i)%vdata = wks(1,:)+one   ! Mighell's correction
    Obs_data(i)%wdata = one/max(eps_DP, (wks(1,:) + min(one,wks(1,:))))
    Obs_data(i)%t2data = ANGRANGE(1,i) + [ (real(j,DP), j = 0, NDATA(i) - 1) ] * ANGRANGE(3,i)
    Obs_data(i)%num_data = NDATA(i)
    Obs_data(i)%step_data_t2 = ANGRANGE(3,i)

  ELSE IF (FFORM(i) == 2) THEN ! only angle, I (counts)
    
    check_step = evaluate_step_array(xv=wks(1,knt:knt2))
    muck_step  = NINT(check_step/ANGRANGE(3,i))
    print'(a,i4,a)','DATASET # ',i,' :: '
    print'(3(a,1x,f20.12,/))','READING DATA: given angular step Dtt0 = ',ANGRANGE(3,i),&
                              '              file readout step  Dtt1 = ',check_step,&
                              '              ratio Dtt1 / Dtt0       = ',check_step/ANGRANGE(3,i)
    xckrs = check_step/REAL(muck_step,DP)
    adiff_step = ABS( ANGRANGE(3,i) - xckrs ) / ANGRANGE(3,i)
    if (adiff_step > 0.01d0 ) then
      print*,'WARNING :: file readout step  Dtt1 is NOT an integer multiple of the given angular step by over 1% !!'
    endif
    ANGRANGE(3,i) = xckrs
    
    Obs_data(i)%vdata = wks(2,knt:knt2)+one   ! Mighell's correction
    Obs_data(i)%wdata = one/max(eps_DP, (wks(2,knt:knt2) + min(one,wks(2,knt:knt2))))
    Obs_data(i)%t2data = NINT(wks(1,knt:knt2)/ANGRANGE(3,i))*ANGRANGE(3,i)
    Obs_data(i)%num_data = NDATA(i)
    Obs_data(i)%step_data_t2 = ANGRANGE(3,i)
    !
  ELSE IF (FFORM(i) == 3) THEN ! only I, sigma_I

    Obs_data(i)%vdata = wks(1,:)
    Obs_data(i)%wdata = one/(wks(2,:)**2)
    Obs_data(i)%t2data = ANGRANGE(1,i) + [ (real(j,DP), j = 0, NDATA(i) - 1) ] * ANGRANGE(3,i)
    Obs_data(i)%num_data = NDATA(i)
    Obs_data(i)%step_data_t2 = ANGRANGE(3,i)
    
  ENDIF
  
  do iwl = 1,ilambda(i)
    do j=1,NDATA(i)
      Obs_data(i)%qdata(j,iwl) = two * sin(duet2r*ABS(Obs_data(i)%t2data(j))) / lambdas(iwl,i)
    enddo
  enddo
  QMAX_D = MAX(QMAX_D,MAXVAL(Obs_data(i)%qdata))
  
  IF (INST_FLAG(i) > 0) then
    ALLOCATE(IRF_Curves(i)%G_sigma_sqrd(NDATA(i)), &
             IRF_Curves(i)%L_w(NDATA(i)), &
             IRF_Curves(i)%Triang(NDATA(i)) )
    IRF_Curves(i)%G_sigma_sqrd = zero; IRF_Curves(i)%L_w = zero; IRF_Curves(i)%Triang = zero
    call wid_gett(opt=INST_FLAG(i), arr6p=INST_6P_var(i,1:6), ttarr=Obs_data(i)%t2data, &
                  sig2arr = IRF_Curves(i)%G_sigma_sqrd, wlarr=IRF_Curves(i)%L_w)
    where (abs(Obs_data(i)%t2data-90.d0) > sceps_DP)
      IRF_Curves(i)%Triang = INST_6P_var(i,7) / tan(Obs_data(i)%t2data*degrees_to_radians)
    elsewhere
      IRF_Curves(i)%Triang = zero
    end where
  ENDIF

  deallocate(wks)
ENDDO

END SUBROUTINE READ_OBS_DATA


END MODULE DATA_ASREAD
!---------------------------------------------------------------------------------
