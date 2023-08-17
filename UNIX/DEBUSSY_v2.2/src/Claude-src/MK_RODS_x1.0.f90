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
program karn_evil_9
use nano_deftyp
use ATOMIX
use helpinput

implicit real(DP)(a-h,o-z),integer(I4B)(i-n)

character(9),parameter :: file_inp0 = 'clumk.ini'
character(512)            :: pwd,rlx,inpath, file_inp,clu2make
character(512)            :: RL,namestr,finu
character(2)              :: ch2
character(3)              :: clushape,ff1
character(8)              :: pearsy
integer(I4B),parameter    :: ttmaxx0=20000,mayy=200
real(DP),allocatable      :: atx(:,:,:),ato(:,:),atb(:,:)
integer(I4B),allocatable  :: atz(:,:),nabys(:),zabys(:),placyl(:,:),katin(:)
real(DP), allocatable     :: cooall(:,:,:)
real(DP)                  :: DV(3,3),aux(3),auxl(3),auxl0(2),Diam_max_nm,MTxy(2,2),cellCM(3),DCL2MK(2),&
                            alpha,beta,ar,br,cr,cosalr,cosber,cosgar
integer(I4B)              :: kuk(1),NCL2MK(2),ttmaxx,lat_plane_extreme(2,2)
integer(I4B)              :: lpwd,lrlx,linpath,jlower_ab,jupper_ab,jlower_c,jupper_c
Logical                   :: DB_exist,do_allsizes

call def_eps
call CPU_TIME(t0)


DB_exist = .true.
do_allsizes=.true.




print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_RODS Program    '
print*, ' '


!___ learn what is the currrent directory ($PWD)

call GET_PWD(pwd=pwd,lpwd=lpwd)

! reading name root of input file and its path

 iu99 = FIND_UNIT()

  OPEN(UNIT=iu99,status='old', &
       form='formatted',access='sequential', &
       file=file_inp0,action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file: ', file_inp0
       STOP
     ENDIF


!!!!! READING .cel file

read(iu99,'(a)') rlx
rlx=trim(adjustl(rlx))
lrlx=len_trim(rlx)
jj=0
do j=lrlx,1,-1
  if (rlx(j:j)==separator) then
    jj=j
    exit
  endif
enddo
lroot=jj-1
namestr=''
namestr(1:lrlx-jj)=rlx(jj+1:lrlx)
Lnamestr=lrlx-jj
linpath=max(0,lroot)
inpath=''
if (linpath>0) THEN
  inpath(1:linpath+1)=rlx(1:linpath+1)
  file_inp=inpath(1:linpath+1)//namestr(1:Lnamestr)
else
  file_inp=namestr(1:Lnamestr)
endif
namestr = namestr(1:Lnamestr-4)//'_'
Lnamestr = Lnamestr-3
!if (inpath(linpath:linpath)/=separator.and.linpath>0) then
!!__RF 06.06.2014
if (linpath>0) then
  if (inpath(linpath:linpath)/=separator) then
    linpath=linpath+1
    inpath(linpath:linpath)=separator
  endif
endif
!linpath   = SCAN(rlx(1:lrlx),'/',.true.)       
!inpath = trim(adjustl(rlx(1:linpath)))
!
!namestr = trim(adjustl(rlx(linpath+1:lrlx-4)))//'_'
!Lnamestr = len_trim(namestr)!
!print'(a)',' INPATH:   '//inpath(1:linpath)
!print'(a)',' NAMESTR:  '//namestr(1:Lnamestr)
 write(*,*) ' Input File               = '//trim(file_inp)



!!!!!!!!!!!!!! SHAPE/SIZE
itra=0
read(iu99,'(a)')RL
RL=trim(adjustl(RL))
LRL=len_trim(RL)
if (rl(1:1)=='D') then
  read(RL(2:LRL),*,IOSTAT=ias1)DCL2MK
  IF (ias1 /= 0) THEN
    read(RL(2:LRL),*)DCL2MK(1)
    DCL2MK(2)=DCL2MK(1)
  ENDIF
  DCL2MK=max(DCL2MK,zero)
  itra=1
else if (rl(1:1)=='N') then
  read(RL(2:LRL),*,IOSTAT=ias1)NCL2MK
  IF (ias1 /= 0) THEN
    read(RL(2:LRL),*)NCL2MK(1)
    NCL2MK(2)=NCL2MK(1)
  ENDIF
  NCL2MK=max(NCL2MK,0)
  itra=2
endif
if (itra==0) then
  print*,' ERROR! Could not read RODs size - stopping'
  stop 'Could not read RODs size - stopping'
endif

IF (verbose) print*, 'NCL2MK =', NCL2MK

clushape='PAR'
read(iu99,'(a)',iostat=ioa) rl
if (ioa==0) then
  rl=trim(adjustl(rl))
  lrl=len_trim(rl)
  if (lrl>=3) then
    if (rl(1:3)=='CYL') clushape='CYL'
    if (rl(1:3)=='HEX') clushape='HEX'
  endif
endif
read(iu99,'(a)',iostat=ioa)RL
  if (iostat /=0) THEN
    do_allsizes = .true.
  endif
RL=trim(adjustl(RL))
LRL=len_trim(RL)
if (rl(1:4)=='TODO') then
 read(RL(5:LRL),*,IOSTAT=ias1)clu2make
    call LOWCASE(clu2make)
    if (clu2make(1:12)=='largest_only') then
      do_allsizes = .false.
    else if (clu2make(1:12)=='all_clusters') then
      do_allsizes = .true.
    else
      print*,'TODO instructon unclear (valid : all_clusters, largest_only). Stop'
      STOP 
  endif  
endif     
      
close(iu99)

!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path

iu=find_unit()

  OPEN(UNIT=iu,status='old', &
       form='formatted',access='sequential', &
       file=file_inp,action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file: ', file_inp
       STOP
     ENDIF


rl=''
read(iu,'(a)') rl
call clean_line(rl)
rl=trim(adjustl(rl))
ll=len_trim(rl)
iep=index(rl(1:ll),' ')
pearsy=rl(1:iep-1)
lpearsy=iep-1
read(rl(iep:ll),*)a,b,c,alpha_DEG,beta_DEG,gamma_DEG,Nspa

if (abs(alpha_DEG-90.d0)>sceps_DP.or.abs(beta_DEG-90.d0)>sceps_DP)  then
    if (clushape=='CYL' .or. clushape=='HEX') then 
    print*,'For monoclinic and triclinic lattice, we''re dealing only with PAR shapes! Bye Bye ...'
    STOP 
  endif 
endif

IF (verbose) print*,'selected shape is '//clushape


gmm=gamma_DEG*degrees_to_radians
if ((abs(gamma_DEG)-90.d0)<sceps_DP) then
  sg=one
  cg=zero
  c2g=-one
else
  sg=sin(gmm)
  cg=cos(gmm)
  c2g=cos(two*gmm)
endif
bsg=b*sg
bcg=b*cg
DV=zero
DV(1,1:2)=(/a,bcg/); DV(2,2)=bsg; DV(3,3)=c

!! FB general matrix for all crystal systems ("OBL")

if (abs(alpha_DEG-90.d0)>sceps_DP.or.abs(beta_DEG-90.d0)>sceps_DP) then 
    alpha=alpha_DEG*degrees_to_radians
    beta=beta_DEG*degrees_to_radians
    !volce=c*cell_base_area
    volce=a*b*c*sqrt(1-(cos(alpha))**2-(cos(beta))**2-(cos(gmm))**2+2*cos(alpha)*cos(beta)*cos(gmm))

    ar=(b*c*sin(alpha))/volce
    br=(a*c*sin(beta))/volce
    cr=(a*b*sin(gmm))/volce
    cosalr=(cos(beta)*cos(gmm)-cos(alpha))/(sin(beta)*sin(gmm))
    cosber=(cos(alpha)*cos(gmm)-cos(beta))/(sin(alpha)*sin(gmm))
    cosgar=(cos(alpha)*cos(beta)-cos(gmm))/(sin(alpha)*sin(beta))
    


    DV(1,1)=a*sin(beta)
    DV(1,2)=-b*cosgar*sin(alpha)

    DV(2,2)=1/br

    DV(3,1)=a*cos(beta)
    DV(3,2)=b*cos(alpha) 
    DV(3,3)=c

endif

WHERE(ABS(DV)<10*eps_DP) DV=zero


MTxy(1,:)=[a**2,   a*b*cg]
MTxy(2,:)=[a*b*cg, b**2  ]

   

cell_base_area=a*bsg

if (itra==1) then                             ! D to N, mode = PAR
    base_area_max=pi*unqua*((DCL2MK(1)*ten)**2)
    select case (clushape)
    case('PAR')
       side_base_max=sqrt(base_area_max/cell_base_area)
       NCL2MK(1)=ceiling(side_base_max)
    case('HEX')
       xxx_base_max=base_area_max/cell_base_area
       NCL2MK(1)=ceiling((-three+sqrt(max(nine,-three+twelve*xxx_base_max)))/six)
    case('CYL')
       r0c=sqrt(cell_base_area/pi)
       NCL2MK(1)=max(0,-1+ceiling(DCL2MK(1)*ten/(r0c*two)))
    case default
       stop 'Unknown shape'
    end select
    NCL2MK(2)=ceiling(DCL2MK(2)*ten/c)
else if (itra==2) then                         ! N to D
    DCL2MK(2)=c*NCL2MK(2)
    select case (clushape)
    case('PAR')
       base_area_max=cell_base_area*(NCL2MK(1)**2)
    case('HEX')
       base_area_max=cell_base_area*(1+3*NCL2MK(1)*(NCL2MK(1)+1))
    case('CYL')
       base_area_max=cell_base_area*((1+NCL2MK(1))**2)
    case default
       stop 'Unknown shape'
    end select
    DCL2MK(1)=two*SQRT(base_area_max/pi)+0.0001d0
    DC2LMK=DC2LMK/ten
endif

igoon=1
if (clushape=='PAR') then
  if (minval(NCL2MK)==0) igoon=0
else 
  if (NCL2MK(1)<0.or.NCL2MK(2)<1) igoon=0
endif
if (igoon==0) then
    print*,' ERROR! ROD size not available! Stop.'
    stop ' ERROR! ROD size not available! Stop.'
endif


 write(*,*) ' Selected shape           = '//clushape(1:3)
 
 if (itra==2)write(*,*) ' Cluster limits supplied  = ', NCL2MK(1:2)
 if (itra==1) write(*,*)' Cluster limits supplied  = ', DCL2MK(1:2), ' nm'
 

allocate(katin(Nspa),nabys(Nspa),zabys(Nspa))
read(iu,*)nabys
read(iu,*)zabys

!  [re]evaluate Diam_max_nm , alpha=beta=90!!!
!volce=c*cell_base_area
deltar_cyl=sqrt(cell_base_area/pi)



nmabys=maxval(nabys)
allocate(atx(3,nmabys,Nspa),ato(nmabys,Nspa),atb(nmabys,Nspa))
atx=zero; ato=zero; atb=one
do i=1,Nspa
  do j=1,nabys(i)
    read(iu,'(a)')rl
    rl=trim(adjustl(rl))
    ch2=rl(1:2)
    kz=0
    do k=1,n_elements
      if (ch2==symb_of_Z(k)(1:2)) then
        kz=k
        exit
      endif
    enddo
    if (kz/=zabys(i)) stop 'ERROR! unsorted species!'
    read(rl(3:),*)atx(:,j,i),ato(j,i),atb(j,i)
  enddo
enddo

    

close(iu)
cellCM=zero
cellM=zero
do i=1,Nspa;do j=1,nabys(i)
  cellCM=cellCM + zabys(i)*ato(j,i)*atx(:,j,i)
  cellM=cellM + zabys(i)*ato(j,i)
enddo;enddo
cellCM=cellCM/cellM
do i=1,Nspa;do j=1,nabys(i)
   atx(:,j,i)=atx(:,j,i)-cellCM
enddo;enddo
where(abs(atx)<sceps_DP) atx=zero

     write(*,*) ' Nr. of Atom Species      = ', Nspa
     write(*,*) ' Nr. of Atom in unit cell = ',  nabys(:)
 print*, ' '
 write(*,'("    Unit Cell parameters    ",6g12.6)') a,b,c,alpha_DEG,beta_DEG,gamma_DEG 

print*,' '
 print*,'Direct Space Crystal-to-orthonormal coordinates matrix: '
    print'(3(3(1x,g12.6),/))',(DV(ix,:),ix=1,3)
     print*, ' '

 call SYSTEM(trim(mkdir_command)//' XYZ'//separator//' > tmp.out 2> tmp.err')
 call SYSTEM(trim(delete_command)//' tmp.out tmp.err')
safe_half=half+sceps_DP
select case (clushape)
case ('PAR')
  ! parallelepipedic, n*a,n*b,m*c
  napcel=sum(nabys(1:Nspa))
  
!! 26.12.2016 largest_only
if (do_allsizes) then 
     jlower_ab=0
     jupper_ab=NCL2MK(1)-1
  else
    jlower_ab=NCL2MK(1)-1
    jupper_ab=NCL2MK(1)-1
endif    

   
  
  DO NL_ab=jlower_ab,jupper_ab
    call CPU_TIME(t1)
   write(ff1, '(i3.3)') NL_ab+1
  write(*,*) 'Building '//clushape(1:3)//' Clusters with Nab '//ff1(1:3)//' --> CPU time = ',t1-t0,' s'

   lat_plane_extreme(:,1)=[0,NL_ab];lat_plane_extreme(:,2)=[0,NL_ab]
   nplanepoints = (NL_ab+1)**2
   M1=0
   finu=''
   write(finu(1:4),'("a",i3.3)')NL_ab+1
   !iu4=find_unit()
!   open(iu4,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:4)//'_'//clushape(1:3)//'.plp')
!   write(iu4,*)'# ',nplanepoints,lat_plane_extreme(:,1),lat_plane_extreme(:,2)
!   do ii1=M1,NL_ab
!     do i2=M1,NL_ab
!       write(iu4,*)ii1,i2
!     enddo
!   enddo
!   close(iu4)
   DO NL_c=0,NCL2MK(2)-1
    nlatpoints = nplanepoints * (NL_c+1)
    finu=''
    write(finu(1:9),'("a",i3.3,"_c",i3.3)')NL_ab+1, NL_c+1
 
    if (allocated(cooall)) deallocate(cooall)
    allocate(cooall(4,Nspa,maxval(nabys)*nlatpoints))
    cooall=zero
  
    katin=0
    do ii1=M1,NL_ab
      do i2=M1,NL_ab
        do i3=0,NL_c
          do ia=1,Nspa
            do ja=1,nabys(ia)
              aux=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
              keep=1
              ooo=ato(ja,ia)
              oooc=one+sceps_DP-ooo
              if (ooo<=safe_half) then
                do jj=1,katin(ia)
                  if (cooall(4,ia,jj) > oooc) cycle
                  xmd=minval(abs( aux - cooall(:3,ia,jj) ))
                  if (xmd<sceps_DP) then
                    keep=0
                    j2add=jj
                    exit
                  endif
                enddo
              endif
              if (keep==1) then
                katin(ia)=katin(ia)+1
                cooall(:3,ia,katin(ia))=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
                cooall(4,ia,katin(ia))=ato(ja,ia) 
              else
                cooall(4,ia,j2add)=cooall(4,ia,j2add)+ooo
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
    
!!_____RF 18.06.14 [killing .xyz and renaming _new.xyz in .xyz]
!    iu2=find_unit()
!    open(iu2,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
!    write(iu2,*)Nspa,1,1.d0
!    do ia=1,Nspa
!      write(iu2,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!      do ja=1,katin(ia)
!        write(iu2,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
!      enddo
!    enddo
!    close(iu2)
    
!    iu3b=find_unit()
!    open(iu3b,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz_INFO')
!    write(iu3b,*)Nspa,1,1.d0
!    do ia=1,Nspa
!      write(iu3b,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!    enddo
!    write(iu3b,*) clushape, NL_ab,NL_c
!    write(iu3b,*) a,b,c,alpha_DEG,beta_DEG,gamma_DEG,napcel
!    write(iu3b,*) volce,c,cell_base_area, nlatpoints, nlatpoints*volce
!    write(iu3b,*) nplanepoints, lat_plane_extreme(:,1), lat_plane_extreme(:,2), &
!                  (lat_plane_extreme(2,ix)-lat_plane_extreme(1,ix),ix=1,2)
!    close(iu3b)
    
    iu3=find_unit()
!    open(iu3,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz')
    open(iu3,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
    write(iu3,*)sum(katin(1:Nspa))
    write(iu3,'(a)')namestr(1:Lnamestr)//finu(1:9)//' !xyzob'
    do ia=1,Nspa
      do ja=1,katin(ia)
        write(iu3,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
      enddo
    enddo
    close(iu3)
    
   enddo;enddo
  
case ('HEX')

!! 26.12.2016 largest_only
if (do_allsizes) then 
     jlower_ab=0
     jupper_ab=NCL2MK(1)-1
  else
    jlower_ab=NCL2MK(1)-1
    jupper_ab=NCL2MK(1)-1
endif   

  ! hexagonal, n*a,n*b,m*c
  napcel=sum(nabys(1:Nspa))
   
!  DO NL_ab=0,NCL2MK(1)-1

   DO NL_ab=jlower_ab,jupper_ab
  call CPU_TIME(t1)
   write(ff1, '(i3.3)') NL_ab
  write(*,*) 'Building '//clushape(1:3)//' Clusters with Nab '//ff1(1:3)//' --> CPU time = ',t1-t0,' s'

  
   lat_plane_extreme(:,1)=[-NL_ab,NL_ab];lat_plane_extreme(:,2)=[-NL_ab,NL_ab]
   nplanepoints=1+3*NL_ab*(NL_ab+1)
   M1=-NL_ab
   finu=''
   write(finu(1:4),'("a",i3.3)')NL_ab+1
  ! iu4=find_unit()
!   open(iu4,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:4)//'_'//clushape(1:3)//'.plp')
!
!   write(iu4,*)'# ',nplanepoints,lat_plane_extreme(:,1),lat_plane_extreme(:,2)
!   do ii1=M1,NL_ab
!     do i2=M1,NL_ab
!       if (abs(ii1-i2)>NL_ab) cycle
!       write(iu4,*)ii1,i2
!     enddo
!   enddo
!   close(iu4)
   DO NL_c=0,NCL2MK(2)-1
    nlatpoints = nplanepoints * (NL_c+1)
    finu=''
    write(finu(1:9),'("a",i3.3,"_c",i3.3)')NL_ab+1, NL_c+1
    if (allocated(cooall)) deallocate(cooall)
    allocate(cooall(4,Nspa,maxval(nabys)*nlatpoints))
    cooall=zero
  
    katin=0
    do ii1=M1,NL_ab
      do i2=M1,NL_ab
        if (abs(ii1-i2)>NL_ab) cycle
        do i3=0,NL_c
          do ia=1,Nspa
            do ja=1,nabys(ia)
              aux=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
              keep=1
              ooo=ato(ja,ia)
              oooc=one+sceps_DP-ooo
              if (ooo<=safe_half) then
                do jj=1,katin(ia)
                  if (cooall(4,ia,jj) > oooc) cycle
                  xmd=minval(abs( aux - cooall(:3,ia,jj) ))
                  if (xmd<sceps_DP) then
                    keep=0
                    j2add=jj
                    exit
                  endif
                enddo
              endif
              if (keep==1) then
                katin(ia)=katin(ia)+1
                cooall(:3,ia,katin(ia))=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
                cooall(4,ia,katin(ia))=ato(ja,ia)
              else
                cooall(4,ia,j2add)=cooall(4,ia,j2add)+ooo
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
    
!!_____RF 18.06.14 [killing .xyz and renaming _new.xyz in .xyz]
!    iu2=find_unit()
!    open(iu2,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
!    write(iu2,*)Nspa,1,1.d0
!    do ia=1,Nspa
!      write(iu2,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!      do ja=1,katin(ia)
!        write(iu2,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
!      enddo
!    enddo
!    close(iu2)
    
!    iu3b=find_unit()
!    open(iu3b,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz_INFO')
!    write(iu3b,*)Nspa,1,1.d0
!    do ia=1,Nspa
!      write(iu3b,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!    enddo
!    write(iu3b,*) clushape, NL_ab,NL_c
!    write(iu3b,*) a,b,c,alpha_DEG,beta_DEG,gamma_DEG,napcel
!    write(iu3b,*) volce,c,cell_base_area, nlatpoints, nlatpoints*volce
!    write(iu3b,*) nplanepoints, lat_plane_extreme(:,1), lat_plane_extreme(:,2), &
!                  (lat_plane_extreme(2,ix)-lat_plane_extreme(1,ix),ix=1,2)
!    close(iu3b)
 
    iu3=find_unit()
!    open(iu3,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz')
    open(iu3,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
    write(iu3,*)sum(katin(1:Nspa))
    write(iu3,'(a)')namestr(1:Lnamestr)//finu(1:9)//' !xyzob'
    do ia=1,Nspa
      do ja=1,katin(ia)
        write(iu3,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
      enddo
    enddo
    close(iu3)
    
  enddo;enddo
  
case ('CYL')
  ! cylinder, radius=n*sqrt(a*b*cos(gamma)/pi),height=m*c
  napcel=sum(nabys(1:Nspa))
  asq=a**2
  bsq=b**2
  ab4=a**4+b**4
  eigv_min=sqrt(half*(asq+bsq-sqrt(ab4+two*asq*bsq*c2g)))
  nhu=huge(1_I4B)
  DO NL_ab=1,NCL2MK(1)
    call CPU_TIME(t1)
   write(ff1, '(i3.3)') NL_ab
  write(*,*) 'Building '//clushape(1:3)//' Clusters with Nab '//ff1(1:3)//' --> CPU time = ',t1-t0,' s'

    lat_plane_extreme(:,1)=[nhu,-nhu];lat_plane_extreme(:,2)=[nhu,-nhu]
    radius=NL_ab*deltar_cyl*(one+eps_DP)
    ell0=radius/eigv_min
    M0=ceiling(ell0)
    if (allocated(placyl)) deallocate(placyl)
    allocate(placyl(-M0:M0,-M0:M0))
    placyl=0
    nplanepoints=0
    do ii1=-M0,M0; do i2=-M0,M0
      auxl0=real([ii1,i2],DP)
      xltrue=sqrt(sum(auxl0*matmul(MTxy,auxl0)))
      if (xltrue>radius) cycle
      placyl(ii1,i2)=1
      nplanepoints=nplanepoints+1
      lat_plane_extreme(1,1)=min(ii1,lat_plane_extreme(1,1))
      lat_plane_extreme(2,1)=max(ii1,lat_plane_extreme(2,1))
      lat_plane_extreme(1,2)=min( i2,lat_plane_extreme(1,2))
      lat_plane_extreme(2,2)=max( i2,lat_plane_extreme(2,2))
    enddo;enddo
    finu=''
    write(finu(1:4),'("a",i3.3)')NL_ab+1
!    iu4=find_unit()
!    open(iu4,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:4)//'_'//clushape(1:3)//'.plp')
!    write(iu4,*)'# ',nplanepoints,lat_plane_extreme(:,1),lat_plane_extreme(:,2)
!    do ii1=-M0,M0; do i2=-M0,M0
!      if (placyl(ii1,i2)==0) cycle
!      write(iu4,*)ii1,i2
!    enddo;enddo
!    close(iu4)

    DO NL_c=0,NCL2MK(2)-1
      
      nlatpoints = nplanepoints * (NL_c+1)
      finu=''
      write(finu(1:9),'("a",i3.3,"_c",i3.3)')NL_ab, NL_c+1
      if (allocated(cooall)) deallocate(cooall)
      allocate(cooall(4,Nspa,maxval(nabys)*nlatpoints))
      cooall=zero
  
      katin=0
      do ii1=-M0,M0
        do i2=-M0,M0
          if (placyl(ii1,i2)==0) cycle
          do i3=0,NL_c
            do ia=1,Nspa
              do ja=1,nabys(ia)
                aux=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
                keep=1
                ooo=ato(ja,ia)
                oooc=one+sceps_DP-ooo
                if (ooo<=safe_half) then
                  do jj=1,katin(ia)
                    if (cooall(4,ia,jj) > oooc) cycle
                    xmd=minval(abs( aux - cooall(:3,ia,jj) ))
                    if (xmd<sceps_DP) then
                      keep=0
                      j2add=jj
                      exit
                    endif
                  enddo
                endif
                if (keep==1) then
                  katin(ia)=katin(ia)+1
                  cooall(:3,ia,katin(ia))=matmul(DV,atx(:,ja,ia)+[ii1,i2,i3])
                  cooall(4,ia,katin(ia))=ato(ja,ia)
                else
                  cooall(4,ia,j2add)=cooall(4,ia,j2add)+ooo
                endif
              enddo
            enddo
          enddo
        enddo
      enddo
    
!!_____RF 18.06.14 [killing .xyz and renaming _new.xyz in .xyz]
!      iu2=find_unit()
!      open(iu2,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
!      write(iu2,*)Nspa,1,1.d0
!      do ia=1,Nspa
!        write(iu2,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!        do ja=1,katin(ia)
!          write(iu2,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
!        enddo
!      enddo
!      close(iu2)
    
!      iu3b=find_unit()
!      open(iu3b,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz_INFO')
!      write(iu3b,*)Nspa,1,1.d0
!      do ia=1,Nspa
!        write(iu3b,*)zabys(ia),katin(ia),sum(cooall(4,ia,1:katin(ia)))
!      enddo
!      write(iu3b,*) clushape, NL_ab,NL_c
!      write(iu3b,*) a,b,c,alpha_DEG,beta_DEG,gamma_DEG,napcel
!      write(iu3b,*) volce,c,cell_base_area, nlatpoints, nlatpoints*volce
!      write(iu3b,*) nplanepoints, lat_plane_extreme(:,1), lat_plane_extreme(:,2), &
!                    (lat_plane_extreme(2,ix)-lat_plane_extreme(1,ix),ix=1,2)
!      close(iu3b)
    
      iu3=find_unit()
!      open(iu3,status='replace', file=trim(inpath)//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'_new.xyz')
      open(iu3,status='replace', file=trim(inpath)//'XYZ'//separator//namestr(1:Lnamestr)//finu(1:9)//'_'//clushape(1:3)//'.xyz')
      write(iu3,*)sum(katin(1:Nspa))
      write(iu3,'(a)')namestr(1:Lnamestr)//finu(1:9)//' !xyzob'
      do ia=1,Nspa
        do ja=1,katin(ia)
          write(iu3,'(a2,5(1x,g16.10))')symb_of_z(zabys(ia)),cooall(:,ia,ja)
        enddo
      enddo
      close(iu3)
    
    enddo
  enddo


case default
  continue
end select

!call system ('rm '//trim(inpath)//separator//'XYZ'*plp') 

call CPU_TIME(tend)
write(*,*) ' '
write(*,*) ' Total CPU time =', tend-t0, 's'
write(*,*) ' '
print*, '******* JOB XYZ-'//clushape//' RODS DONE! *******'

end program karn_evil_9
