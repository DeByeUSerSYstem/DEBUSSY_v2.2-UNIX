program Paint_It_Black
use nano_deftyp
use ATOMIX
use paper_blood
use GEODIS
use fast_PLACE
use GURKEN_2012
use Sultans_Of_Swing
use INPUT_CLUMKFILE
use specfun_AC

implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
character(10),parameter :: file_inp0 = 'qbemkQ.ini'
real(DP) :: DVds(3,3),DVrs(3,3),LVds(3),Diam_max_nm,MTds(3,3),cellCM(3),eigMT(3)
integer(I4B) :: NSPH2MK,NCL2MK(2),allocOK, lobo(3),upbo(3), Ncube1(3), Ncube2(3),mmax(3),mmax2(3)
real(DP)     :: DSPH2MK,DCL2MK(2)
real(DP) :: parapar(9),wl_tt(2)
real(DP) :: aux_latvec(3),aux_Dvec(3),xyzP(3)
integer(I4B) :: dr_step=1
character(3) :: clushape
character(512) :: pwd,rlx,inpath, file_inp,namestr, rl
character(8) :: pearsy
Logical      :: DB_exist, isordnot, orthoXY, eqatpair, zerolay, &
                isorigin, isoriginP, isoriginL, output_xyz, &
                nonzerowid, allsizes, nzw0
character(8)  :: para_model = 'WelbIsot'
logical :: occs_are_one = .true.

real(DP),allocatable :: Cube1(:,:,:),Cube1r(:,:,:),Cube2(:,:,:)
integer(I4B),allocatable  :: Cube1i(:,:,:)
real(DP),allocatable      :: radat(:), mulat(:),mulats(:)
real(DP),allocatable      :: patte_len(:,:),patte_invmul(:,:)
integer(I4B) :: aux1(3),aux2(3)
real(DP) :: Raux1(3),Raux2(3)
character(len=8),dimension(:),allocatable :: finu
real(DP),allocatable,save     :: xnat0(:,:),summul0(:,:),termcon0(:,:)
integer(I4B),allocatable,save :: nat0(:,:),ndi0(:,:)
character(888):: fxyz
character(2)  :: sya

call def_eps

allocate(numcells_byphase(1))

DB_exist = .true.

call CPU_TIME(ttot0)
call GET_PWD(pwd=pwd,lpwd=lpwd)


print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_QBE Program    '
print*, ' '



call openread_clumkX(clumkfn=file_inp0, &
                     rlx=rlx,lrlx=lrlx, &
                     itra=itra,nclu=NCL2MK,dclu=DCL2MK, &
                     clusha=clushape, &
                     parapar=parapar,smoothing_width=smowid, wlenttmax=wl_tt, &
                     do_allsizes=allsizes, sample_allsteps=multiple_steps, &
                     step_base = dr_step, para_model=para_model,occ1=occs_are_one, do_the_xyz=output_xyz)
if (clushape /= 'QBE') then
  print*,'Shape given is '//clushape//' but I deal only with shape QBE...'
  stop 'I deal only with shape QBE...'
endif
working_wavelength=wl_tt(1)
working_ttmax=wl_tt(2)
if (working_wavelength == zero) working_wavelength=wl_ourqmax
NCL2MK(1)=NCL2MK(1)-1
NSPH2MK=NCL2MK(1)
DSPH2MK=DCL2MK(1)

!___________________________________________ Paths and filename of .cel/.cely
file_inp=''
file_inp(1:lrlx)=rlx(1:lrlx)
jj=0
do j=lrlx,1,-1
  if (rlx(j:j)==separator) then
    jj=j
    exit
  endif
enddo
lroot=max(0,jj-1)
namestr=''
namestr(1:lrlx-jj)=rlx(jj+1:lrlx)
Lnamestr=lrlx-jj
linpath=max(0,lroot)
inpath=''
if (linpath>0) THEN
  inpath(1:linpath+1)=rlx(1:linpath+1)
!  file_inp=inpath(1:linpath+1)//namestr(1:Lnamestr)
!else
!  file_inp=namestr(1:Lnamestr)
endif
idot=index(namestr(1:Lnamestr),'.',.true.)
namestr = namestr(1:idot-1)//'_'
Lnamestr = idot
!!!!if (inpath(linpath:linpath)/=separator.and.linpath>0) then
if (linpath>0) then
  if (inpath(linpath:linpath) /=separator) then
    linpath=linpath+1
    inpath(linpath:linpath)=separator
  endif
endif
if (verbose) print'(a)',' INPATH:   '//inpath(1:linpath)
if (verbose) print'(a)',' NAMESTR:  '//namestr(1:Lnamestr)
print'(a)',' Input File          = '//trim(file_inp)

!___________________________________________ Paths and filename of .cel/.cely DONE


!_________________________ PARACRYSTALLINE
IF (para_model == 'WelbAnys') then
  rcorra=parapar(1)
  scorra=parapar(2)
  rcorrb=parapar(3)
  scorrb=parapar(4)
  cocoxy=parapar(5)
  sigmaxa=parapar(6)
  sigmaxb=parapar(7)
  B000_c=parapar(8)
  sigma_min_sph=parapar(9)
!!!!!!!!!!!
  var_targ = (sigmaxa**2+sigmaxb**2)
  r_targ=20.d0
  slope_var = var_targ/r_targ
!!!!!!!!!!!
  variance_min = sigma_min_sph**2
  corr_coe = max(-one+s4eps_DP,min(one-s4eps_DP,cocoxy))
  sigma_c_sq = B000_c/(pi2*pi2)
  max_intrinsic_sigma = max(zero, sigmaxa,sigmaxb)
  nzw0 = (max_intrinsic_sigma > scepsDP)
else IF (para_model == 'WelbIsot') then
  max_intrinsic_sigma=parapar(1) ! limit variance is  2 * max_intrinsic_sigma**2
  decorr_len = parapar(2)

!!!!!!!!!!!
  var_targ = two*(max_intrinsic_sigma**2)
  r_targ=decorr_len
  slope_var = var_targ/r_targ
!!!!!!!!!!!
  variance_min = zero
  corr_coe = zero
  nzw0 = (max_intrinsic_sigma > scepsDP)
endif

print'(a23,g15.8)', ' Working Wavelength  = ',working_wavelength
print'(a23,g15.8)', ' 2-Theta Max         = ',working_ttmax


!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path
call READALLO_CELY(cely_fn_V=[trim(file_inp)],lcely_fn_V=[len_trim(file_inp)], &
                   ncl2mk=ncl2mk,dcl2mk=dcl2mk,itra=itra,geom=clushape, &
                   diace=Delta_Lqb,cdx=cdx,c_lattice=c, occ1=occs_are_one )
volum_cell=(Delta_Lqb**3)

if (verbose) print*, 'NCL2MK =', NCL2MK
if (verbose) print*, 'DCL2MK =', DCL2MK !, Delta_Lqb*(NCL2MK+1)*0.2d0
if (verbose) print*, 'clushape = ',clushape

iup=find_unit()
open(iup,status='replace',file=trim(file_inp)//'.pat')
write(iup,*)n_at_pair_V(1),sum(Celty(1)%nat),Celty(1)%nat
do ipa=1,n_at_pair_V(1)
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  sopair = sum(patte(4,1:numpatv(ipa,1),ipa,1))!*two
!  if (is1==is2) sopair=sopair-Celty(1)%nat(is1)
  write(iup,*)is1,is2,Celty(1)%nat(is1),Celty(1)%nat(is2)
  write(iup,*)izz1,izz2,numpatv(ipa,1),sopair
  ss1=0.d0;ss2=0.d0
  do jx=1,numpatv(ipa,1)
    zzz=maxval(abs(patte(1:3,jx,ipa,1)))
    fk=2.d0
    if (zzz<sceps_DP.and.is1==is2) fk=1.d0
    ss1=ss1+patte(4,jx,ipa,1);ss2=ss2+patte(4,jx,ipa,1)*fk
    write(iup,*) patte(1:4,jx,ipa,1),patte(4,jx,ipa,1)*fk
  enddo
  write(iup,*)'                SUM ',ss1,ss2
enddo
close(iup)



abcabg_loc=abcabgy
sampled_folder='DISTANCES'; lsampled_folder=9
basepath_sampled=inpath(1:linpath); lbasepath_sampled=linpath

DVds=zero
ttrig=( cos(degrees_to_radians*abcabgy(4))-cos(degrees_to_radians*abcabgy(5))*cos(degrees_to_radians*abcabgy(6)) ) / &
           sin(degrees_to_radians*abcabgy(6))
DVds(1,1:3)=[abcabgy(1), abcabgy(2)*cos(degrees_to_radians*abcabgy(6)), abcabgy(3)*cos(degrees_to_radians*abcabgy(5))]
DVds(2,2:3)=[abcabgy(2)*sin(degrees_to_radians*abcabgy(6)), abcabgy(3) * ttrig ]
DVds(3,3) = abcabgy(3) * sqrt( -(ttrig**2) + (sin(degrees_to_radians*abcabgy(5)))**2 )
where(abs(DVds)<100.d0*eps_DP) DVds=zero
print*,'  '
print*,'Crystal-to-Orthonormal Direct Space Coordinates Transformation Matrix : '
print*,DVds(1,:)
print*,DVds(2,:)
print*,DVds(3,:)   
DVrs=zero
DVrs(1,1)=one/DVds(1,1)
DVrs(2,2)=one/DVds(2,2)
DVrs(3,3)=one/DVds(3,3)
DVrs(2,1)=-DVrs(1,1)*DVrs(2,2)*DVds(1,2)
DVrs(3,1)= DVrs(1,1)*DVrs(2,2)*DVrs(3,3)*(DVds(1,2)*DVds(2,3)-DVds(1,3)*DVds(2,2))
DVrs(3,2)=-DVrs(3,3)*DVrs(2,2)*DVds(2,3)
do i=1,3
  LVds(i)=sqrt(sum(DVrs(i:3,i)**2))
enddo


MTds(1,:) = [abcabgy(1)**2, abcabgy(1)*abcabgy(2)*cos(degrees_to_radians*abcabgy(6)), &
                           abcabgy(1)*abcabgy(3)*cos(degrees_to_radians*abcabgy(5))]
MTds(2,:) = [abcabgy(2)*abcabgy(1)*cos(degrees_to_radians*abcabgy(6)), abcabgy(2)**2, &
                           abcabgy(2)*abcabgy(3)*cos(degrees_to_radians*abcabgy(4))]
MTds(3,:) = [abcabgy(3)*abcabgy(1)*cos(degrees_to_radians*abcabgy(5)),  &
                           abcabgy(3)*abcabgy(2)*cos(degrees_to_radians*abcabgy(4)), &
                           abcabgy(3)**2]

eigMT = EigMTDS(al_C=abcabgy(4),be_C=abcabgy(5),ga_C=abcabgy(6), a_C=abcabgy(1),b_C=abcabgy(2),c_C=abcabgy(3))
eigMT = sqrt(eigMT)
if (verbose) print*,'Metric tensor eigenvalues square roots: ',eigMT
Ncube1 = 1+ceiling((half+real(NCL2MK(1)+1,DP))*Delta_Lqb*half*LVds)

NCube2 = NCube1 * 2

allocate(Cube1(-Ncube1(1):Ncube1(1),-Ncube1(2):Ncube1(2),-Ncube1(3):Ncube1(3)),stat = allocOK)
if (allocOK/=0) then
  print*,'Cannot allocate Cube1, too many nodes! ',2*Ncube1+1,product(2*Ncube1+1)
  stop 'Cannot allocate Cube1, too many nodes! '
endif
Cube1=zero

allocate(Cube1r(-Ncube1(1):Ncube1(1),-Ncube1(2):Ncube1(2),-Ncube1(3):Ncube1(3)),stat = allocOK)
if (allocOK/=0) then
  print*,'Cannot allocate Cube1r, too many nodes! ',2*Ncube1+1,product(2*Ncube1+1)
  stop 'Cannot allocate Cube1r, too many nodes! '
endif
Cube1r=zero

allocate(Cube1i(-Ncube1(1):Ncube1(1),-Ncube1(2):Ncube1(2),-Ncube1(3):Ncube1(3)),stat = allocOK)
if (allocOK/=0) then
  print*,'Cannot allocate Cube1i, too many nodes! ',2*Ncube1+1,product(2*Ncube1+1)
  stop 'Cannot allocate Cube1i, too many nodes! '
endif
Cube1i=0

allocate(Cube2(-Ncube2(1):Ncube2(1),-Ncube2(2):Ncube2(2),0:Ncube2(3)),stat = allocOK)
if (allocOK/=0) then
  print*,'Cannot allocate Cube2, too many nodes! ',[2,2,1]*Ncube2+1,(Ncube2(3)+1)*(2*Ncube2(1)+1)*(2*Ncube2(2)+1)
  stop 'Cannot allocate Cube2, too many nodes! '
endif
Cube2=zero
RRmax_a = half*DCL2MK(1)*ten + maxval(eigMT)
xran = 88.d0
volu_clus_max = quter*Pi*RRmax_a*RRmax_a*RRmax_a
NU_CEL_MAX = size(Cube2) !max(10,ceiling(xran*volu_clus_max/cell_volume))
if (verbose) print*,'Max. volume, cell volume ',volu_clus_max,cell_volume, xran
if (verbose) print*,'NU_CEL_MAX = ',NU_CEL_MAX,volu_clus_max/cell_volume,NU_CEL_MAX/(volu_clus_max/cell_volume)

allocate(radat(NU_CEL_MAX),mulat(NU_CEL_MAX),mulats(0:NU_CEL_MAX))

allocate(finu(0:NCL2MK(1)))
DO j_abc=0,NCL2MK(1)
  finu(j_abc)(:)=''
  write(finu(j_abc)(1:8),'("r",i3.3,"_",a3)')j_abc+1,clushape
enddo

call CPU_TIME(ts0)
do i1=-NCube1(1),NCube1(1)
  aux1(1)=i1
  do i2=-NCube1(2),NCube1(2)
    aux1(2)=i2
    do i3=-NCube1(3),NCube1(3)
      aux1(3)=i3
      Raux1 = matmul(MTds,aux1)
      ddd = maxval(abs(Raux1))
      Cube1r(i1,i2,i3)=ddd
    enddo
  enddo
enddo
call CPU_TIME(ts1)
if (verbose) print*,'Time for Cube1r: ',ts1-ts0

diamax = (one+real(NCL2MK(1)+1,DP))*Delta_Lqb * sr3  !ten*DCL2MK(1)+two*maxval(eigMT)
dimens = max(2*nbeta,2*N1cont)+CEILING(diamax/Deltas)
dimemax=maxval(dimens(Ndelta1:Ndelta2))
if (verbose) print*,Ndelta1,Ndelta2,Deltas(Ndelta1:Ndelta2),dimens(Ndelta1:Ndelta2),dimemax
if (allocated(samv)) deallocate(samv)
allocate(samv(dimemax,n_at_pair_V(1),Ndelta1:Ndelta2,1))
samv=zero


!_____________ ALLOCATE LOCAL VECTORS
 
 if (allocated(nat0)) deallocate(nat0)
 if (allocated(xnat0)) deallocate(xnat0)
 if (allocated(ndi0)) deallocate(ndi0)
 if (allocated(summul0)) deallocate(summul0)
 if (allocated(termcon0)) deallocate(termcon0)
 if (allocated(patte_len)) deallocate(patte_len)
 if (allocated(patte_invmul)) deallocate(patte_invmul)
 mmmp=maxval(numpatv(1:n_at_pair_V(1),1))
 allocate(nat0(1:n_sp_atom_V(1),0:NCL2MK(1)),xnat0(1:n_sp_atom_V(1),0:NCL2MK(1)),& 
          ndi0(1:n_at_pair_V(1),0:NCL2MK(1)),&
          summul0(1:n_at_pair_V(1),0:NCL2MK(1)), termcon0(n_at_pair_V(1),0:NCL2MK(1)),&
          patte_len(mmmp,n_at_pair_V(1)),patte_invmul(mmmp,n_at_pair_V(1)))
patte_len=zero
patte_invmul=zero
do ipa=1,n_at_pair_V(1)
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  do ix=1,numpatv(ipa,1)
    patte_len(ix,ipa) = sum(patte(1:3,ix,ipa,1)*matmul(MTds,patte(1:3,ix,ipa,1)))
    fk=two
    if (abs(patte_len(ix,ipa))<sceps_DP.and.is1==is2) fk=one
    patte_invmul(ix,ipa) = fk
  enddo
enddo

do ksph=0,NCL2MK(1)
  summul0(:,ksph)=zero
  termcon0(:,ksph)=zero
  do k=1,n_at_pair_V(1)
    ndi0(k,ksph)=8*((ksph+1)**3)*numpatv(k,1) ! approximately
  enddo
enddo
do k=1,n_at_pair_V(1)
  ipa=k
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  if (verbose) then
    print*,'******* PAIR ',k
    print*,is1,is2
    print*,izz1,izz2
    print*,Celty(1)%nat(is1),Celty(1)%nat(is2)
    print*,sum(Celty(1)%ato(1:Celty(1)%nat(is1),is1)),sum(Celty(1)%ato(1:Celty(1)%nat(is2),is2))
    print*,sum(Celty(1)%atx(:,1:Celty(1)%nat(is1),is1))/Celty(1)%nat(is1), &
         sum(Celty(1)%atx(:,1:Celty(1)%nat(is2),is2))/Celty(1)%nat(is2)
  endif       
enddo


yoll=sceps_DP
ndidi=0
radat=zero
mulat=zero
mulats=zero
Cube1i = 0
call CPU_TIME(ts0)
do i1=-Ncube1(1),Ncube1(1)
  aux1(1)=i1
  do i2=-Ncube1(2),Ncube1(2)
    aux1(2)=i2
    do i3=-Ncube1(3),Ncube1(3)
      aux1(3)=i3
      ddd = Cube1r(i1,i2,i3)
      IF (ndidi<1) THEN
        ndidi = 1
        radat(ndidi) = ddd
        mulat(ndidi) = one
        cycle
      ENDIF
      
      call PLACER(ddd,radat(1:ndidi),n2place,isiteq,yoll)
      
      IF (isiteq==1) THEN
        mulat(n2place) = mulat(n2place)+one
      ELSE
        IF (ndidi==NU_CEL_MAX) THEN
          print*,'NU_CEL_MAX not enough! ',NU_CEL_MAX
          stop 'NU_CEL_MAX not enough! '
        ENDIF
        radat(n2place+2:ndidi+1) = radat(n2place+1:ndidi)
        mulat(n2place+2:ndidi+1) = mulat(n2place+1:ndidi)
        radat(n2place+1) = ddd
        mulat(n2place+1) = one
        ndidi=ndidi+1
      ENDIF
    enddo
  enddo
enddo
call CPU_TIME(ts1)
if (verbose) then
  print*,'Time for radat/mulat: ',ts1-ts0
  print*,'NU_CEL_MAX = ',NU_CEL_MAX,ndidi,volu_clus_max/cell_volume,NU_CEL_MAX/(volu_clus_max/cell_volume)
  print*,'Allocating check ',ndidi,NU_CEL_MAX,size(Cube1)
endif  
where(abs(mulat)<=yoll) mulat = zero
Cube1i=0
do i=1,ndidi
  ddd=radat(i)
  where(abs(Cube1r-ddd)<yoll) Cube1i=i
enddo
n00=count(Cube1i==0)
if (n00>0) stop 'Unfilled index box'
mulats(0)=zero
do ja=1,ndidi
  mulats(ja)=mulats(ja-1)+mulat(ja)
enddo

Cube1=zero
!Cube1Former=zero
jlower=NCL2MK(1)
jupper=NCL2MK(1)

if (allsizes) then
  jlower=0
  if (modulo(NCL2MK(1),dr_step)>0) then
    jupper=(NCL2MK(1)/dr_step)*dr_step
  endif
endif
!print*,'Doing j_ab from ',jlower,' to ',jupper,' by ',dr_step

print*,'  '
print*,'Building Cubic Clusters from Edge ',jlower+1,' to Edge ',jupper+1,' by step ',dr_step
print*,'  '
print*,' Edge Length (nm)     edge #      edge-max    # of Atoms       CPU Time (sec)    '
print*,'  '


call CPU_TIME(tstart)
t0=tstart
DO ksph=jlower,jupper,dr_step
  radius = real(ksph+1,DP)*Delta_Lqb*half
  radius2 = radius**2+sceps_DP
  diameter = two*radius
  nlap = (ksph+1)**3
  xlap = real(nlap,DP)
  mmax = min( NCube1, 1+ceiling((half+real(ksph+1,DP))*Delta_Lqb*half*LVds) )
  xwish = real((ksph+1)**3,DP) / real(ncenters_cell,DP) ! # of whole unit cells
  
  Rmax=two*radius  
  Rmax2=Rmax**2
  
  
  Cube1=zero
  Cube2=zero; samv=zero
  
  ndidi0=0
  do ja=1,ndidi
    if (abs(mulats(ja)-xwish) <= sceps_DP) then
      ndidi0=ja
      xlast=one
      exit
    else if (mulats(ja)-xwish > sceps_DP) then
      ndidi0=ja
      excess=mulats(ja)-xwish
      xlast = (xwish-mulats(ja-1))/mulat(ja)
      exit
    endif
  enddo
  where (Cube1i==ndidi0) 
    Cube1 = xlast
  elsewhere (Cube1i>ndidi0) 
    Cube1 = zero
  elsewhere (Cube1i<ndidi0) 
    Cube1 = one
  end where
  if (verbose) &
    print*,'Cutting: ',ksph,ndidi,ndidi0,mulats(ndidi0-1), mulat(ndidi0),xlast, mulats(ndidi0-1) + mulat(ndidi0)*xlast
!_____________ Make dist  
  ! Shell occulpancy
!  where (Cube1Former>yoll) Cube1 = Cube1 - Cube1Former  
  
  xlast2 = xlast**2
  c000a1 = mulats(ndidi0-1)
  c000b1 = mulat(ndidi0)
  Cube2(0,0,0) = c000a1 + c000b1*xlast2
  
  call CPU_TIME(t1)
!  print*,'*** ',two*radius/ten,ksph,NCL2MK(1),c000a1,c000b1,xlast,Cube2(0,0,0),t1-t0,t1-tstart
  t0=t1
  mmax2=2*mmax
  do i3=0,mmax2(3)
    aux1(3)=i3
    ii2=-mmax2(2)
    if (i3==0) ii2=0
    lobo(3)=max(-mmax(3),-mmax(3)-i3)
    upbo(3)=min( mmax(3), mmax(3)-i3)
    do i2=ii2,mmax2(2)
      aux1(2)=i2
      ii1=-mmax2(1)
      if (i3==0.and.i2==0) ii1=1
      lobo(2)=max(-mmax(2),-mmax(2)-i2)
      upbo(2)=min( mmax(2), mmax(2)-i2)
      do i1=ii1,mmax2(1)
        aux1(1)=i1
        iimax=maxval(abs(aux1))
        lobo(1)=max(-mmax(1),-mmax(1)-i1)
        upbo(1)=min( mmax(1), mmax(1)-i1)
        soccp=zero
        do j1=lobo(1),upbo(1)
          do j2=lobo(2),upbo(2)
            do j3=lobo(3),upbo(3)
              ooo1=Cube1(j1,j2,j3)
              if (abs(ooo1)<=yoll) cycle
              ooo2=Cube1(j1+i1,j2+i2,j3+i3)
              if (abs(ooo2)>yoll) soccp=soccp+ooo1*ooo2
 !             ooo3=Cube1Former(j1+i1,j2+i2,j3+i3)
 !             if (abs(ooo3)>yoll) soccp=soccp+two*ooo1*ooo3
            enddo
          enddo
        enddo
        Cube2(i1,i2,i3)=Cube2(i1,i2,i3)+soccp
      enddo
    enddo
  enddo
  call CPU_TIME(t222)
  if (verbose) print*,'Lattice multiplicity evaluation time ',ksph,t222-t0
  
  
  !ready for next
!  Cube1Former = min(one,max(zero, Cube1))
!  Cube1Former = min(one,max(zero,Cube1Former + Cube1))
  nlatpoi_clu=count(Cube1>yoll)
  xlatpoi_clu=c000a1 + c000b1*xlast
  do k=1,n_sp_atom_V(1)
    if (verbose) print*,'cdb',k,n_sp_atom_V(1),ksph,Celty(1)%nat(k),Celty(1)%Z_at(k) !, nlatpoi_clu, xlatpoi_clu
    xnat0(k,ksph)=xlatpoi_clu * sum(Celty(1)%ato(1:Celty(1)%nat(k),k))
    nat0(k,ksph)=nlatpoi_clu * Celty(1)%nat(k)
  enddo
  
  print'(g15.5,2(3x,i8),5x,i8,8x,g15.5)',two*radius/ten,ksph+1,jupper+1,NINT(sum(xnat0(:,ksph))),t1-tstart
!  print*,'*** ',two*radius/ten,ksph+1,NCL2MK(1),c000a1,c000b1,xlast,Cube2(0,0,0),t1-t0,t1-tstart
  
  if (verbose) then
    SU2=sum(Cube2)
    print*,'Corr.sum : ',ksph,nlatpoi_clu,sum(Cube1),SU2,Cube2(0,0,0),SU2-Cube2(0,0,0)
  endif
  if (output_xyz) then
    lfile_inp=len_trim(file_inp)
    fxyz=''
    fxyz(1:lfile_inp-4)=trim(file_inp)
!    write(fxyz(lfile_inp-3:lfile_inp+4),'("_",i3.3,".xyz")')ksph
    write(fxyz(lfile_inp-3:lfile_inp+8),'("_",i3.3,"_QBE.xyz")')ksph+1
    iux=find_unit()
    open(iux,status='replace',file=fxyz(1:lfile_inp)//'_QBE_LAT'//fxyz(lfile_inp+1:lfile_inp+8))
    write(iux,*) nlatpoi_clu
    write(iux,*) fxyz(1:lfile_inp-4),ksph
    iuxa=find_unit()
    open(iuxa,status='replace',file=fxyz(1:lfile_inp+8))
    write(iuxa,*) nlatpoi_clu*sum(Celty(1)%nat)
    write(iuxa,*) fxyz(1:lfile_inp-4),ksph
    
    do i1=-Ncube1(1),Ncube1(1)
      aux1(1)=i1
      do i2=-Ncube1(2),Ncube1(2)
        aux1(2)=i2
        do i3=-Ncube1(3),Ncube1(3)
          aux1(3)=i3
          ooo=Cube1(i1,i2,i3)
          if (abs(ooo)<=yoll) cycle
          xyzP=matmul(DVds,aux1)
          if (abs(ooo)>one-yoll) then
            write(iux,'("Si ",3(1x,g14.8),1x,g12.6)')xyzP,ooo
          else
            write(iux,'("Np ",3(1x,g14.8),1x,g12.6)')xyzP,ooo
          endif
        enddo
      enddo
    enddo
    close(iux)
    do ias=1,n_sp_atom_V(1)
      izz=Celty(1)%Z_at(ias)
      sya = symb_of_Z(izz)
      nn=Celty(1)%nat(ias)
      do i1=-Ncube1(1),Ncube1(1)
        aux1(1)=i1
        do i2=-Ncube1(2),Ncube1(2)
          aux1(2)=i2
          do i3=-Ncube1(3),Ncube1(3)
            aux1(3)=i3
            ooo=Cube1(i1,i2,i3)
            if (abs(ooo)<=yoll) cycle
            xyzP=matmul(DVds,aux1)
            do ii=1,nn
              write(iuxa,'(a2,2x,3(1x,g14.8),1x,g12.6)')sya,xyzP+matmul(DVds,Celty(1)%atx(:,ii,ias)), &
                                                        ooo*Celty(1)%ato(ii,ias)
            enddo
          enddo
        enddo
      enddo
    enddo
    close(iuxa)
  endif  
  do i3=0,mmax2(3)
    aux1(3)=i3
    Raux1(3)=real(i3,DP)
    ii2=-mmax2(2)
    if (i3==0) ii2=0
    lobo(3)=max(-mmax(3),-mmax(3)-i3)
    upbo(3)=min( mmax(3), mmax(3)-i3)
    do i2=ii2,mmax2(2)
      Raux1(2)=real(i2,DP)
      aux1(2)=i2
      ii1=-mmax2(1)
      if (i3==0.and.i2==0) ii1=0
      lobo(2)=max(-mmax(2),-mmax(2)-i2)
      upbo(2)=min( mmax(2), mmax(2)-i2)
      iabsum=iabsum
      do i1=ii1,mmax2(1)
        aux1(1)=i1
        Raux1(1)=real(i1,DP)
        ooo_lat=Cube2(i1,i2,i3)
        if (abs(ooo_lat)<yoll) cycle
        aux_latvec=matmul(MTds,Raux1)
        d0lat=sum(Raux1*aux_latvec)
        aux_latvec=aux_latvec*two
        isoriginL = ALL(aux1==0)
        fffmuL=two
        if (isoriginL) fffmuL=one
        iiimuL=nint(fffmuL)
!            
        DO ipa=1,n_at_pair_V(1)
          is1=Celty(1)%zappa(1,ipa)
          is2=Celty(1)%zappa(2,ipa)
          do jx=1,numpatv(ipa,1)
            ooo_this = patte(4,jx,ipa,1) * ooo_Lat
            if (abs(ooo_this)<sceps_DP) cycle
            fffmuP = patte_invmul(jx,ipa)
            isoriginP = (nint(fffmuP)==1)
            ooo_this = ooo_this * max(fffmuP,fffmuL)
            d000a=d0lat + patte_len(jx,ipa)
            d000b=sum(aux_latvec*patte(1:3,jx,ipa,1))

            full_dis_sq1 = d000a + d000b
            
            d0s=sqrt(max(zero,full_dis_sq1))
            if (d0s<Celty(1)%minallowdist(ipa)) then            ! too short distance, set it to 0
               termcon0(ipa,ksph) = termcon0(ipa,ksph) + ooo_this
            else
              call SAM_ONE_TOALL(d0=d0s)
              summul0(ipa,ksph)=summul0(ipa,ksph)+ooo_this
              do ksam=Ndelta1,Ndelta2
                jsum1=max(1,centersum(ksam)-N1cont)
                jrr1=jsum1-centersum(ksam)
                jsum2=min(dimemax,centersum(ksam)+N1cont)
                jrr2=jsum2-centersum(ksam)
                samv(jsum1:jsum2,ipa,ksam,1) = samv(jsum1:jsum2,ipa,ksam,1) + &
                                                 for_samv_P(jrr1:jrr2,ksam) * ooo_this
                if (bnd_N(2,ksam) <= 0) cycle
                samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,1) = &
                  samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,1) + &
                     for_samv_N(bnd_N(1,ksam):bnd_N(2,ksam),ksam) * ooo_this
              enddo
            endif
    !______________________ difference equals sum if one is zero
    
            if (isoriginL.or.isoriginP) cycle
            
            full_dis_sq1 = d000a - d000b
            
            d0s=sqrt(max(zero,full_dis_sq1))
!            if (ksph==0) print*,'in',2,ipa,d0s,ooo_this,ooo_lat,patte(4,jx,ipa,1)
            if (d0s<Celty(1)%minallowdist(ipa)) then            ! too short distance, set it to 0
               termcon0(ipa,ksph) = termcon0(ipa,ksph)+ooo_this
            else
              call SAM_ONE_TOALL(d0=d0s)
              summul0(ipa,ksph)=summul0(ipa,ksph)+ooo_this
              do ksam=Ndelta1,Ndelta2
                jsum1=max(1,centersum(ksam)-N1cont)
                jrr1=jsum1-centersum(ksam)
                jsum2=min(dimemax,centersum(ksam)+N1cont)
                jrr2=jsum2-centersum(ksam)
                samv(jsum1:jsum2,ipa,ksam,1) = samv(jsum1:jsum2,ipa,ksam,1) + &
                                                 for_samv_P(jrr1:jrr2,ksam) * ooo_this
                if (bnd_N(2,ksam) <= 0) cycle
                samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,1) = &
                  samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,1) + &
                    for_samv_N(bnd_N(1,ksam):bnd_N(2,ksam),ksam) * ooo_this
              enddo
            endif
          enddo
        enddo
        
      enddo
    enddo
  enddo
  
!  Celty(1)%nat=nat0(:,ksph)
!  Celty(1)%xnat=xnat0(:,ksph)
  Celty(1)%ndi=ndi0(:,ksph)
  Celty(1)%summul=summul0(:,ksph)
  Celty(1)%termcon_all  = termcon0(:,ksph)
  Celty(1)%termcon_ineq = termcon0(Celty(1)%point_neqpair(:),ksph)
  Celty(1)%termcon      = termcon0(Celty(1)%point_eqpair(:),ksph)
  
  Z_at_glo = Celty(1)%Z_at
  nat_glo = nat0(:,ksph)
  zappa_glo = Celty(1)%zappa
  ndi_glo = ndi0(:,ksph)
  xnat_glo = xnat0(:,ksph)
  termcon_glo = termcon0(Celty(1)%point_eqpair(:),ksph)
  termcon_all_glo = termcon0(:,ksph)
  termcon_ineq_glo = termcon0(Celty(1)%point_neqpair(:),ksph)
  summul_glo = summul0(:,ksph)
  Size_Abscissa(1) = real(ksph+1,DP)
  numcells_byphase = (ksph+1)**3
  
  Actual_Diam = growing_diam(ksph,:)

  call OUTSAMP(finumb=namestr(1:lnamestr)//finu(ksph)(1:8)//'.smp', Bpre=zero)
  !if (ksph==0) stop
enddo
iucon=find_unit()
open(iucon,status='replace',file='check_termcon.out')
write(iucon,*)'#',NCL2MK(1),n_sp_atom_V(1),n_at_pair_V(1)
do ksph=0,NCL2MK(1)
  write(iucon,*)ksph,nat0(:,ksph),xnat0(:,ksph)
  write(iucon,*)' A   ',summul0(:,ksph)
  write(iucon,*)' B  ',termcon0(:,ksph)
  write(iucon,*)' C  ',termcon0(:,ksph)+summul0(:,ksph),xnat0(:,ksph)**2
  write(iucon,*)' D  ',Celty(1)%point_neqpair, Celty(1)%point_eqpair
enddo
close(iucon)

if (output_xyz) then
print*,'  '
  print*,  '******* JOB XYZ-'//clushape//' DONE! *******'
  print*,  ' '
endif
print*,'  '
print*, '******* JOB SMP-'//clushape//' DONE! *******'

end program Paint_It_Black
