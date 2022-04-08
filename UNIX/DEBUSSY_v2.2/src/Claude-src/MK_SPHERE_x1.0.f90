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
!_______________________________________________________________________________
program clu_spheres
use nano_deftyp
use ATOMIX
use fast_PLACE
use GURKEN_2012
use helpinput
use Sultans_Of_Swing

implicit real(DP)(a-h,o-z),integer(I4B)(i-n)

integer(I4B),parameter    :: ttmaxx=20000,mayy=200
real(DP),allocatable      :: biga(:,:,:,:),radat(:), &
                             xwish(:),xnabys(:),mulat(:,:),mulats(:,:), &
                             stoi_cell(:,:),stoi_clus(:,:)
integer(I4B),allocatable  :: atz(:,:),nabys(:),zabys(:),wish(:),mulsit(:,:)
integer(I4B)              :: ttmaxx1
real(DP)                  :: DV(3,3),alpha,beta1,ar,br,cr,cosalr,cosber,cosgar,volce
character(132)            :: rlx, RL
character(256)            :: inpath, namestr, file_inp
character(2)              :: ch2, Pears4
character(4)              :: PearsS
character(8)              :: finu
character(1)              :: strukt_D, Pears3,sglet1=' '
character(12)             :: finu2
character(132)             :: filn1,filn2, rline
integer(I4B)              :: kuk(1),sgnum1=-1
type(GURK),allocatable    :: cetriol(:,:)
Logical                   :: occs_are_one = .true.
call def_eps



print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_SPHERE Program    '
print*, ' '


KMAX=0
file_inp = 'clumkS.ini'
ilog_inp= find_unit()

  OPEN(UNIT=ilog_inp,status='old', &
       form='formatted',access='sequential', &
       file=file_inp,action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file: ', file_inp
       STOP
     ENDIF
     
!!!!! READING .cel filename

 read(ilog_inp,'(a)') rlx
 rlx=trim(adjustl(rlx))
 lrlx=len_trim(rlx)


!!!!!!!!!!!!!! SHAPE/SIZE

read(ilog_inp,'(a)') RL
RL=trim(adjustl(RL))
LRL=len_trim(RL)
if (RL(1:1)=='D') then
  read(RL(2:LRL),*,IOSTAT=ias1) Diammax
  IF (ias1 /= 0 .or. Diammax <= zero) THEN
      print*, 'ERROR: radius of clusters non supplied. STOP'
      Stop 'ERROR: radius of clusters non supplied. STOP'
  ELSE
! Converting Diammax (max diameter in nm) in RRmax (max radius in Ang.)  
      RRmax = Diammax*5.0_DP      
  ENDIF 
 else if (RL(1:1)=='N') then
  read(RL(2:LRL),*,IOSTAT=ias1) KMAX
  IF (ias1 /= 0 .or. KMAX <= zero) THEN
      print*, 'ERROR: radius of clusters non supplied. STOP'
      Stop 'ERROR: radius of clusters non supplied. STOP'
  ELSE
! Converting Diammax (max diameter in nm) in RRmax (max radius in Ang.)  
      KMAX = -KMAX     
  ENDIF 
endif

!print*, 'N_max, R_max =', N_max, RRmax, KMAX

read(ilog_inp,'(a)',iostat=io) RL
if (io==0) then
  RL=trim(adjustl(RL))
  if (RL(1:4)=='OCC1') then  ! yes, no; 1 0
    rline=trim(adjustl(rl(5:)))
    if (((rline(1:1)=='y').or. &
        (rline(1:1)=='Y')).or. &
        (rline(1:1)=='1')) then
      occs_are_one = .true.
    else if (((rline(1:1)=='n').or. &
        (rline(1:1)=='N')).or. &
        (rline(1:1)=='0')) then
      occs_are_one = .false.
    endif
  endif
endif
close(ilog_inp)
rline=''
!--------
!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path

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

iucel=find_unit()
!_____________________________________ open .cel file
  OPEN(UNIT=iucel,status='old', &
       form='formatted',access='sequential', &
       file=file_inp,action='READ', iostat=ierr)
     IF (ierr /=0) THEN
        print*, ' Error opening input file: ', file_inp
       STOP
     ENDIF

linpath   = SCAN(rlx(1:lrlx),separator,.true.)       
inpath = trim(adjustl(rlx(1:linpath)))

namestr = trim(adjustl(rlx(linpath+1:lrlx-4)))//'_'
Lnamestr = len_trim(namestr)

!--------

 rl=''
read(iucel,'(a)') rl
call clean_line(rl)
rl=trim(adjustl(rl))
ll=len_trim(rl)
iep=index(rl(1:ll),' ')
!  pearsy=rl(1:iep-1)
lpearsy=iep-1

IF (lpearsy <= 4) THEN
    PearsS = rl(1:lpearsy)
ELSE
    PearsS = rl(1:4)
ENDIF

lpee=len_trim(PearsS)

read(rl(iep:ll),*)a,b,c,alpha_DEG,beta_DEG,gamma_DEG,Nspa
if (verbose) print*, PearsS(1:lpee),a,b,c,alpha_DEG,beta_DEG,gamma_DEG,Nspa


call Pearson_to_SpGroup (PearsS(1:lpee), sgnum1, sglet1)


if (sgnum1 == -1 .and. sglet1 == ' ') then
 print'(a)','Pearson symbol given = '//PearsS(1:lpee)
 call LOWCASE(PearsS)
! IF (PearsS(1:1) /= 'c' .and. PearsS(1:1) /= 'h' .and. PearsS(1:1) /= 't') THEN
   !  STOP 'ERROR! Wrong Pearson Symbol - Sorry, only c,h,t structures are managed !!! '
! ENDIF
!nuni=1
 IF (PearsS(1:1) == 'm') THEN
     IF (PearsS(2:2) == 'c') THEN
         nuni = 2
     ELSE IF (PearsS(2:2) == 'p') THEN
         nuni = 1
     ENDIF
 ENDIF


 IF (PearsS(1:1) == 'c') THEN
     IF (PearsS(2:2) == 'f') THEN
         nuni = 4
     ELSE IF (PearsS(2:2) == 'i') THEN
         nuni = 2
     ELSE IF (PearsS(2:2) == 'p') THEN
         nuni = 1
     ENDIF
 ENDIF
 IF (PearsS(1:1) == 't') THEN
     IF (PearsS(2:2) == 'i') THEN
         nuni = 2
     ELSE IF (PearsS(2:2) == 'p') THEN
         nuni = 1
     ENDIF
 ENDIF
 IF (PearsS(1:1) == 'h') THEN
     IF (PearsS(2:2) == 'i') THEN
         nuni = 2
     ELSE IF (PearsS(2:2) == 'p') THEN
         nuni = 1
     ELSE IF (PearsS(2:2) == 'r') THEN
         nuni = 1
     ENDIF
 ENDIF
else


   nspgroup=sgnum1
   if (nspgroup < 1 .or. nspgroup > 230) then
     centering_type='P'
     print*,'Readallo_cely warning: Pearson symbol/space group not read correctly, assuming P centering'
     nspgroup=1
   else
     call Find_The_SpGroup(sgnum=nspgroup, sglet=sglet1)
     ind=index(spgroup_IDENT, "[")
     print*,'Space Group number, setting, centering : ',nspgroup,' ',sglet1,' ',spgroup_IDENT(ind+1:ind+1)
     ncenters_cell = GET_CENTERING_MULT(centering_type)
     nuni=ncenters_cell
  endif   
endif
!_______ Estimate n. at. 

volu_cel = COGEO(alpha_DEG,beta_DEG,gamma_DEG)*a*b*c
aeq_cel =exp(unter*log(volu_cel))

! print*, volu_cel
volce=volu_cel

 XK = (6.0_DP/(nuni*Pi))**unter
 volk0 = quter*Pi
 r0 = 0.5_DP*XK*(volce**unter)

IF (Kmax < 0) THEN
    RRmax=(-KMAX*r0) 
    KMAX = ceiling(-1+(RRmax)/(XK*r0)) 
    !print*, RRmax
   ! KMAX = ceiling(-1+(RRmax)/(XK*r0))  
ELSE
   KMAX= (ceiling(RRmax/r0))
   RRmax=(KMAX*r0) 
 !  KMAX = ceiling(-1+(RRmax)/(XK*r0))
ENDIF  
 if (verbose) print*,'r0,KMAX, RRmax, XK, nuni',  r0, KMAX, RRmax, XK, nuni
RRmax_a=RRmax !*aeq_cel
volu_clus_max = quter*Pi*RRmax_a*RRmax_a*RRmax_a
NU_CEL_MAX = ceiling(1.2_DP*volu_clus_max/volu_cel)
allocate(nabys(Nspa),xnabys(Nspa),zabys(Nspa),wish(Nspa),xwish(Nspa), &
         stoi_cell(2,Nspa),stoi_clus(2,Nspa))
         
! nabys(:) = Nr. of atoms for specie 1,  Nr. of atoms for specie 2, ...
! zabys(:) = Z for specie 1,  Z for specie 2, ....
! wish(:) =  Nr. of atoms of specie 1 for cluster 1, ....

read(iucel,*) nabys
read(iucel,*) zabys

nmabys = maxval(nabys)
ttmaxx1 = nmabys*NU_CEL_MAX

 IF (verbose) print*,ttmaxx,ttmaxx1,nmabys,NU_CEL_MAX

! RRmax = largest cluster's radius (in Angstrom)
! PearsS = Pearson's symbol
! Nat = total number of atoms in unit cell
! Nspa = Nr. of atomic species

 maur=ttmaxx1*Nspa
 
 allocate(radat(maur),mulsit(ttmaxx1,Nspa),mulat(ttmaxx1,Nspa),mulats(ttmaxx1,Nspa), &
         cetriol(ttmaxx1,Nspa))
 do i=1,Nspa
    do j=1,ttmaxx1
       if (ALLOCATED(cetriol(j,i)%ACOO)) deallocate(cetriol(j,i)%ACOO)
    enddo
 enddo

 do i=1,Nspa
    do j=1,ttmaxx1
      cetriol(j,i)%ZETA=zabys(i)
    enddo
 enddo
 allocate(atx(3,nmabys,Nspa),ato(nmabys,Nspa))
 atx=0.0_DP; ato=0.0_DP
 stoi_cell=0.0_DP

 do i=1,Nspa
    xnabys(i)=0.0_DP
    do j=1,nabys(i)
       read(iucel,'(a)') rl       ! reading atom info: symb(char(2)) x y z occ [ occ can be 1.0 or less]
       rl=trim(adjustl(rl))
       ch2=rl(1:2)
       kz=0
       do k=1,n_elements
          if (ch2==symb_of_Z(k)(1:2)) then
              kz=k
              exit
          endif
       enddo
       if (kz/=zabys(i)) stop 'unsorted species!'
       read(rl(3:),*) atx(:,j,i),ato(j,i),bbb
       if (occs_are_one) ato(j,i)=sign(one,ato(j,i))
       stoi_cell(1,i) = stoi_cell(1,i)+ato(j,i)
       xnabys(i)=xnabys(i)+ato(j,i)
    enddo
    ofrak=100*xnabys(i)/nabys(i)
  
    IF (verbose) print'(a,i2,a,i2,a,1x,g12.4,i4,1x,g13.4)', &
       ' SPECIES ',i,' Z=',zabys(i),': ATOMS/SITE/AV.OCCUP. = ',xnabys(i),nabys(i),ofrak,'%'
 enddo
 
 close(iucel)
 ssst=sum(stoi_cell(1,:))
 stoi_cell(2,:)=stoi_cell(1,:)/ssst

 IF (verbose) THEN
     print*,'*** Cell Composition [Z, N. at., frac. at.]:'
     do i=1,Nspa
        print'(i3,2(1x,g18.8))',zabys(i),stoi_cell(:,i)
     enddo
 ENDIF


 
 Nat = SUM(nabys)
 xNat = (SUM(ato(:,:)))
 Nat1=nint(xnat)
 IF (verbose) print*, 'Nat (atom list) & (sum of occupancies) = ', Nat, Nat1, xnat
 
 !if (Nat < 10) then 
!     write(Pears3(1:1),'(i1)') Nat1
!     IF (verbose) print*, ' Pears3  = ', Pears3
!     IF (PearsS(3:3) /= Pears3(1:1)) THEN
!         IF (verbose) print*, 'WARNING!  # of atoms in unit cell not in agreement with Pearson Symbol '
!     ENDIF
! else
!     write(Pears4(1:2),'(i2)') Nat1
!     IF (PearsS(3:4) /= Pears4(1:2)) THEN
!         IF (verbose) print*, 'WARNING!  # of atoms in unit cell not in agreement with Pearson Symbol '
!     ENDIF
! endif
!
! IF (Nspa == 1) THEN
!     IF (verbose) print*, 'Strukturbericht Designation: A --> monoatomic structure'
!     strukt_D = 'A'
! ELSE IF (Nspa == 2) THEN
!     IF (Nabys(1) == Nabys(2)) THEN
!        IF (verbose) &
!        print*, 'Strukturbericht Designation: B --> diatomic structure with 1:1 ratio of atom types'
!        strukt_D = 'B'
!     ELSE
!        IF (verbose) &
!        print*, 'Strukturbericht Designation: C --> diatomic structure with 2:1 ratio of atom types'
!        strukt_D = 'C'
!     ENDIF
! ELSE IF (Nspa > 2) THEN
!     IF (verbose) print*, 'WARNING, Strukturbericht Designation not assigned !!! '
!      strukt_D = 'Z'
! ENDIF
 
  NatC_spe = Nat/nuni


    gmm=gamma_DEG*degrees_to_radians
    alpha=alpha_DEG*degrees_to_radians
    beta1=beta_DEG*degrees_to_radians
    !volce=a*b*c*sqrt(1-(cos(alpha))**2-(cos(beta))**2-(cos(gmm))**2+2*cos(alpha)*cos(beta)*cos(gmm))

! print*, volce
    ar=(b*c*sin(alpha))/volce
    br=(a*c*sin(beta1))/volce
    cr=(a*b*sin(gmm))/volce
    cosalr=(cos(beta1)*cos(gmm)-cos(alpha))/(sin(beta1)*sin(gmm))
    cosber=(cos(alpha)*cos(gmm)-cos(beta1))/(sin(alpha)*sin(gmm))
    cosgar=(cos(alpha)*cos(beta1)-cos(gmm))/(sin(alpha)*sin(beta1))
    


    DV(1,1)=a*sin(beta1)
    DV(1,2)=-b*cosgar*sin(alpha)

    DV(2,2)=1/br

    DV(3,1)=a*cos(beta1)
    DV(3,2)=b*cos(alpha) 
    DV(3,3)=c

! gmm=gamma_DEG*Pi/180.0_DP
 sg=sin(gmm)
! cg=cos(gmm)
! if(abs(sg) < eps_DP) sg=0.0_DP
! if(abs(cg) < eps_DP) cg=0.0_DP
 asg=a*sg
! acg=a*cg
! DV=0.0_DP
! DV(1,1:2)=(/a,acg/); DV(2,2)=asg; DV(3,3)=c
! IF (verbose) print'(3(3(1x,g12.6),/))',DV
     
     WHERE(ABS(DV)<10*eps_DP) DV=zero
 
    print*,' '
    print*,'Direct Space Crystal-to-orthonormal coordinates matrix: '
    print'(3(3(1x,g12.6),/))',(DV(ix,:),ix=1,3)
    print*, ' '

 
 
 !print*, ' kmax, r0 = ',KMAX, r0
 call SYSTEM(trim(mkdir_command)//' XYZ'//separator//' > tmp.out 2> tmp.err')
 call SYSTEM(trim(delete_command)//' tmp.out tmp.err')
 CLUSTER: do k=0,KMAX*3
    Rmax = (k+1)*r0
    if (Rmax>RRmax) exit
    do ia=1,Nspa
       wish(ia) = nint(quter*Pi*Rmax*Rmax*Rmax/volce)*nabys(ia)
       IF (verbose) print*,'k,wish2',k,ia,wish(ia)
       wish(ia) = ((k+1)**3)*nabys(ia)/nuni
       xwish(ia) = real((k+1)**3,DP)*xnabys(ia)/real(nuni,DP)
       IF (verbose) print*,'k,wish1',k,ia,wish(ia),xwish(ia)
    enddo
    print*, ' Building cluster # ',k+1, ' - Diameter (nm): ',2*Rmax/10.0_DP, ' - # of atoms: ',wish
    if (k == 0) then
        Rmax=2.0_DP*Rmax  
    else if (k == 1) then
        Rmax=1.25_DP*Rmax
    else if (k > 1) then
        Rmax=1.1_DP*Rmax
    endif
    
    MM1=ceiling(Rmax/a) 
    MM2=ceiling(Rmax/c) 
    MM3=ceiling(Rmax/asg) 
    
    IF (verbose) THEN
        print*,'MM1',MM1,Rmax,a
        print*,'MM2',MM2,Rmax,c
        print*,'MM3',MM3,Rmax,asg
    ENDIF
    
    MMx=MM1
    if (MM2>MMx) MMx=MM2
    if (MM3>MMx) MMx=MM3
    MMx=MMx+1
    ndidi=0
    radat=0.0_DP
    mulat=0.0_DP
    mulsit=0
    mulats=0.0_DP
    yoll=sceps_DP
    
    do ii1=-MMx,MMx
     do i2=-MMx,MMx
      do i3=-MMx,MMx
       do ia=1,Nspa
          CARLO:do ja=1,nabys(ia)
              aux=atx(:,ja,ia)+real((/ii1,i2,i3/),DP)
              aux=matmul(DV,aux)
              dx=sqrt(sum(aux*aux))
              IF (dx>Rmax) cycle CARLO
              IF (ndidi<1) THEN
                 ndidi = 1
                 radat(ndidi) = dx
                 mulat(ndidi,ia) = ato(ja,ia)
                 mulsit(ndidi,ia) = 1
                 cycle CARLO
              ENDIF
              call PLACER(dx,radat(1:ndidi),n2place,isiteq,yoll)
              IF (isiteq==1) THEN
                 mulat(n2place,ia) = mulat(n2place,ia)+ato(ja,ia)
                 mulsit(n2place,ia) = mulsit(n2place,ia)+1
              ELSE
                 IF (ndidi==ttmaxx1) THEN
                    write(1,*)'nhu not enough! ',ttmaxx1
                    stop 'nhu not enough! '
                 ENDIF
                 radat(n2place+2:ndidi+1) = radat(n2place+1:ndidi)
                 mulat(n2place+2:ndidi+1,:) = mulat(n2place+1:ndidi,:)
                 mulsit(n2place+2:ndidi+1,:) = mulsit(n2place+1:ndidi,:)
                 radat(n2place+1) = dx
                 mulat(n2place+1,:) = 0.0_DP
                 mulsit(n2place+1,:) = 0
                 mulat(n2place+1,ia) = ato(ja,ia)
                 mulsit(n2place+1,ia) = 1
                 ndidi=ndidi+1
              ENDIF
         enddo CARLO
       enddo
      enddo
     enddo
    enddo
    where(abs(mulat)<=yoll) mulat = 0.0_DP
    
 if (verbose) print*, 'ndidi = ', ndidi,ttmaxx1
         
!________ SPHERICAL CLUSTER IS BUILT... TRIMMING IT
    ndidi0=0
    do ja=1,ndidi
       do ia=1,Nspa
          mulats(ja,ia)=sum(mulat(1:ja,ia))
          cetriol(ja,ia)%NATO = mulsit(ja,ia) 
          !NINT(mulat(ja,ia))
          cetriol(ja,ia)%focc = 0
          IF (abs(mulat(ja,ia))>yoll) then
             cetriol(ja,ia)%OKKU = 1.0_DP
             cetriol(ja,ia)%OKKU_init = mulat(ja,ia)/real(mulsit(ja,ia),DP)
          else IF (abs(mulat(ja,ia))<=yoll) then
             cetriol(ja,ia)%OKKU = 0.0_DP
             cetriol(ja,ia)%OKKU_init = 0.0_DP
          endif
          if (ALLOCATED(cetriol(ja,ia)%ACOO)) deallocate(cetriol(ja,ia)%ACOO)
          ALLOCATE(cetriol(ja,ia)%ACOO(4,cetriol(ja,ia)%NATO))
          if (cetriol(ja,ia)%NATO>0) cetriol(ja,ia)%ACOO(:,:)=0.0_DP
       enddo
       uth=minval(mulats(ja,:)-xwish)
       if (uth>=-yoll) then
          ndidi0=ja
          exit
       endif
    enddo
    if (ndidi0==0) STOP 'Problems!! # of distances = 0 !!!'

    if (ANY(mulats(ndidi0,:)-xwish>yoll)) then
        do ia=1,Nspa
           excess=mulats(ndidi0,ia)-xwish(ia)
           IF (abs(excess) <= yoll) cycle
           CHALO:do ja=ndidi0,1,-1
                IF (abs(mulat(ja,ia))<=yoll) cycle CHALO
                dexc = excess - mulat(ja,ia)
                if (dexc<-yoll) then
                   cetriol(ja,ia)%OKKU = -dexc/mulat(ja,ia)
                   exit CHALO
                else if (dexc>yoll) then
                   cetriol(ja,ia)%OKKU = 0.0_DP
                   excess=dexc
                else if (abs(dexc)<=yoll) then
                   cetriol(ja,ia)%OKKU = 0.0_DP
                   exit CHALO
                endif
           enddo CHALO
        enddo
    endif

! trimming done... now save coord.s

    do ii1=-MMx,MMx
     do i2=-MMx,MMx
      do i3=-MMx,MMx
       do ia=1,Nspa
         CARLO2:do ja=1,nabys(ia)
           aux=atx(:,ja,ia)+real((/ii1,i2,i3/),DP)
           aux=matmul(DV,aux)
           dx=sqrt(sum(aux*aux))
           IF (dx>radat(ndidi0)+yoll) cycle CARLO2
           kuk=MINLOC(ABS(radat(1:ndidi0)-dx))
           jm=kuk(1)
           cetriol(jm,ia)%focc = cetriol(jm,ia)%focc+1
        !!!   print*,ii1,i2,i3,ia,ja,jm,size(cetriol(jm,ia)%ACOO(:,:),2),cetriol(jm,ia)%focc
           cetriol(jm,ia)%ACOO(1:3,cetriol(jm,ia)%focc) = aux
           cetriol(jm,ia)%ACOO(4,cetriol(jm,ia)%focc) = ato(ja,ia)
         enddo CARLO2
       enddo
      enddo
     enddo
    enddo

  
  
    iu=find_unit()
    open(iu,status='replace',file='XYZ'//separator//'show.shell')
    write(iu,*) ndidi0,Nspa
    do i=1,ndidi0
      xuxx = 0.0_DP
      do ia1=1,Nspa
        do ia2=ia1+1,Nspa
          aadd=mulats(i,ia2)-mulats(i,ia1)
          if (abs(aadd)<yoll) cycle
          if (abs(aadd)>abs(xuxx)) xuxx=aadd
        enddo
      enddo
      write(iu,'(1x,g26.18,7(1x,g11.3))')radat(i),(mulat(i,:)),(mulats(i,:)),xuxx
    enddo
    close(iu)

    iu = find_unit()
    
    finu(1:1) = 'r'
    write(finu(2:8),'(i3.3,a4)') k+1,'.xyz'
    filn1 = namestr(1:Lnamestr)//finu(1:8)
       
    filn1='XYZ'//separator//TRIM(filn1)
    
    finu2(1:12) = finu(1:4)//'_SPH.xyz'
    filn2 = namestr(1:Lnamestr)//finu2(1:12)
    filn2='XYZ'//separator//TRIM(filn2)
    
    
   OPEN(iu,status='replace',file=TRIM(filn1))
    
    iu2 = find_unit()
    OPEN(iu2,status='replace',file=TRIM(filn2))
    
    numline = SUM(cetriol(:,:)%NATO)
    numline = 0

    ipocc=1

    a_latt_cioc=1.0_DP
     write(iu,'(2i6,1x,g8.2)') Nspa, ipocc, a_latt_cioc
    stoi_clus=0.0_DP
    do ia=1,Nspa
       izz=zabys(ia)
       ch2=symb_of_Z(izz)(1:2)
   !_____ Sum first
       inax=0
       sum_of_occu = 0.0_DP
       do ja=1,ndidi0
          nn=cetriol(ja,ia)%NATO
          oo=cetriol(ja,ia)%OKKU*cetriol(ja,ia)%OKKU_init
          IF (nn<1 .or. oo<yoll) CYCLE
          oo1=cetriol(ja,ia)%OKKU
          do ii=1,nn
             oo = cetriol(ja,ia)%ACOO(4,ii)*oo1
             sum_of_occu=sum_of_occu+oo
          enddo
          inax=inax+nn
       enddo
       IF (verbose) print*,'WRITING ATOM # ',ia,izz,ch2(1:2),inax,sum_of_occu
      ! write(iu,'(2i8,g24.12)')izz,inax,sum_of_occu
       stoi_clus(1,ia)=sum_of_occu
       inax=0
       sum_of_occu=0.0_DP
       do ja=1,ndidi0
          nn=cetriol(ja,ia)%NATO
          oo=cetriol(ja,ia)%OKKU*cetriol(ja,ia)%OKKU_init
          if (nn<1 .or. oo<yoll) CYCLE
          oo1=cetriol(ja,ia)%OKKU
          do ii=1,nn
             oo = cetriol(ja,ia)%ACOO(4,ii)*oo1
             write(iu,'(a2,2x,4(1x,g24.16))')ch2,cetriol(ja,ia)%ACOO(1:3,ii),oo
             numline = numline + 1
             sum_of_occu=sum_of_occu+oo
          enddo
          inax=inax+nn
       enddo
       IF (verbose) print*,'WRITTEN ATOM # ',ia,izz,ch2(1:2),inax,sum_of_occu
    enddo

    ssst=sum(stoi_clus(1,:))
    stoi_clus(2,:)=stoi_clus(1,:)/ssst
    IF (verbose) THEN
        print*,'*** Cluster Composition [Z, N. at., frac. at.]:'
        do ia=1,Nspa
           print'(i3,2(1x,g20.8))',zabys(ia),stoi_clus(:,ia)
        enddo
    ENDIF
 

  rewind(iu)
  write(iu2,'(i8)') numline   ! AG 19.05.10
  write(iu2,'(a)') '! '//filn2
!  read(iu,'(a)') rline
  read(iu,'(a)') rline
  do kant = 1,numline
     read(iu,'(a)') rline
     write(iu2,'(a)') rline
  enddo
  
  close(iu)
  
!  if (isystem==3) then
!  call system('del '//TRIM(filn1))
! else
  call system(delete_command//'  '//TRIM(filn1))
! endif

  close(iu2)
  
   enddo CLUSTER

print*, ' '
print*, '******* JOB XYZ-SPHERE DONE! *******'

    
end program clu_spheres
