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
character(10),parameter :: file_inp0 = 'setmkQ.ini'
real(DP) :: DVds(3,3),DVrs(3,3),LVds(3),Diam_max_nm,MTds(3,3),cellCM(3),eigMT(3)
integer(I4B) :: NCL2MK(2),allocOK, lobo(3),upbo(3), Ncube1(3), Ncube2(3),mmax(3),mmax2(3)
real(DP)     :: DCL2MK(2)
real(DP) :: parapar(9),wl_tt(2)
real(DP) :: aux01(3),aux02(3),aux_latvec(3),aux_Dvec(3),xyzP(3)
integer(I4B) :: dr_step=1
character(3) :: clushape
character(512) :: pwd,rlx,xyzlatset,inpath, file_inp_cel,namestr, rl
character(8) :: pearsy
Logical      :: DB_exist, isordnot, orthoXY, eqatpair, zerolay, &
                isorigin, isoriginP, isoriginL, output_xyz, &
                nonzerowid, allsizes, nzw0
real(DP),allocatable,save :: vecs(:,:),muvec(:), vdiff(:,:),mudiff(:),vdiff2(:,:),mudiff2(:),rdiff2(:)
character(8)  :: para_model = 'WelbIsot'
logical :: occs_are_one = .true.

real(DP),allocatable :: Cube1(:,:,:),Cube1r(:,:,:),Cube2(:,:,:)
integer(I4B),allocatable  :: Cube1i(:,:,:)
real(DP),allocatable      :: radat(:), mulat(:),mulats(:)
real(DP),allocatable      :: patte_len(:,:),patte_invmul(:,:)
!integer(I4B) :: aux1(3),aux2(3)
real(DP) :: Raux1(3),Raux2(3)
character(len=8),dimension(:),allocatable :: finu
real(DP),allocatable,save     :: xnat0(:,:),summul0(:,:),termcon0(:,:)
integer(I4B),allocatable,save :: nat0(:,:),ndi0(:,:)
character(888):: fxyz
character(10) :: thenumb='0123456789'
logical :: fndnmb

!call def_eps

DB_exist = .true.

call CPU_TIME(ttot0)
call GET_PWD(pwd=pwd,lpwd=lpwd)




print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_LATSET Program    '
print*, ' '




call openread_clumkX(clumkfn=file_inp0, &
                     rlx=rlx,lrlx=lrlx, &
                     itra=itra,nclu=NCL2MK,dclu=DCL2MK, &
                     clusha=clushape, &
                     parapar=parapar,smoothing_width=smowid, wlenttmax=wl_tt, &
                     do_allsizes=allsizes, sample_allsteps=multiple_steps, &
                     step_base = dr_step, para_model=para_model,occ1=occs_are_one, do_the_xyz=output_xyz, &
                     rlx_2nd=xyzlatset,lrlx_2nd=lxyzlatset,density_gcm3_set=dens_phase_gcm3)
if (clushape /= 'SET') then
  print*,'Shape given is '//clushape//' but I deal only with shape SET. Bye bye...'
  stop 'I deal only with shape SET. Bye bye...'
endif

working_wavelength=wl_tt(1)
working_ttmax=wl_tt(2)
if (working_wavelength == zero) working_wavelength=wl_ourqmax

call cpu_time(t0)
!_________________ Open & read set of translations from file
!________________ 0. Compose filename
nnn=NCL2MK(1)
iclu_ind=1
if (nnn>0) then
  iclu_ind=nnn
  RUSH:do i=1,lxyzlatset-2
    fndnmb=.true.
    do j=0,2
      isnu=0
      do k=1,10
        if (xyzlatset(i+j:i+j)==thenumb(k:k)) then
          isnu=1
          exit
        endif
      enddo
      fndnmb=fndnmb.and.(isnu==1)
    enddo
    if (fndnmb) then
      write(xyzlatset(i:i+2),'(i3.3)') nnn
      exit RUSH
    endif
  enddo RUSH
endif
print'(a)','Translation set filename: '//xyzlatset(1:lxyzlatset)
!________________ 1. Count
nvec=0
iuset=find_unit()
open(iuset,status='old',action='read',file=xyzlatset(1:lxyzlatset))
do
  read(iuset,*,iostat=io) aux01,x
  if (io/=0) exit
  nvec=nvec+1
enddo
rewind(iuset)
ndistUB = nvec*(nvec-1)
ndistUB = ndistUB/2
print*,'Nr. of translations in set: ',nvec
print*,'Nr. of max. distances within set: ',ndistUB
allocate(vecs(3,nvec),muvec(nvec), vdiff(3,0:ndistUB),mudiff(0:ndistUB))
!________________ 2. Read
kvec=0
do
  read(iuset,*,iostat=io) aux01,x
  if (io/=0) exit
  kvec=kvec+1
  vecs(:,kvec)=aux01;  muvec(kvec)=x
enddo
close(iuset)
!____________ Distances between points of translation set 
call cpu_time(t1)
print'(a,1x,f15.4)','Reading time translation set: ',t1-t0
print*,s4eps_DP,sceps_DP
call cpu_time(t0)
!_________ Zero distance
vdiff     = zero
mudiff(0) = sum(muvec**2)
print*,'INI mudiff(0) ',mudiff(0),sum(muvec),sum(muvec**2)
ndifv=0
do ivec1=1,nvec-1
  aux01 = vecs(:,ivec1)
  xmu1=muvec(ivec1)
  if (abs(xmu1)<sceps_DP) cycle
  do ivec2=ivec1+1,nvec
    aux02 = vecs(:,ivec2)-aux01
    xmu2 = muvec(ivec2)
    if (abs(xmu2)<sceps_DP) cycle
    ooo=xmu1*xmu2
    ! skip 'empty' pair
    if (abs(ooo) < sceps_DP) CYCLE
    where(abs(aux02)<sceps_DP) aux02=zero
    ! skip inversion equivalents, keep to upper subspace
    if (abs(aux02(3))>sceps_DP) then
      chosig=sign(one,aux02(3))
    else if (abs(aux02(3))<=sceps_DP) then
      if (abs(aux02(2))>sceps_DP) then
        chosig=sign(one,aux02(2))
      else if (abs(aux02(2))<=sceps_DP) then
        if (abs(aux02(1))>sceps_DP) then
          chosig=sign(one,aux02(1))
        else if (abs(aux02(1)) <= sceps_DP) then
        !___ Accidentally null vector
          chosig=zero
        endif
      endif
    endif
    if (abs(chosig)<sceps_DP) then
        !___ Accidentally null vector : counted in 0 distances
      
      mudiff(0) = mudiff(0) + ooo
     ! print*,'Adding:',mudiff(0)
      cycle
    else
      aux02=aux02*chosig
    endif
    
    isnew=1
    jadd=0
    do jck=1,ndifv
      xxx=maxval(abs( aux02 - vdiff(1:3,jck) ))
      if (xxx<s4eps_DP) then
        isnew=0
        jadd=jck
        exit
      endif
    enddo
    if (isnew==1) then
      ndifv=ndifv+1
      vdiff(:,ndifv) = aux02
      mudiff(ndifv)  = ooo
    else if (isnew==0) then
      mudiff(jadd) = mudiff(jadd) + ooo
    endif
  enddo
enddo
call cpu_time(t1)
print'(a,1x,f15.4)','Distance evaluation time translation set: ',t1-t0
print*,'Nr. of actual distances within set: ',ndifv
print'(a,/,2(3(1x,g20.14),/))',' Sum(Occ)    (Sum(Occ))^2    Sum(Occ^2)    mu_0    sum(mu_j>0)    :  check',&
  sum(muvec), sum(muvec)**2, sum(muvec**2), &
  mudiff(0), sum(mudiff(1:ndifv)),   sum(muvec)**2-(mudiff(0)+two*sum(mudiff(1:ndifv)))

allocate(vdiff2(3,0:ndifv),mudiff2(0:ndifv),rdiff2(0:ndifv))
vdiff2  = vdiff(:,0:ndifv)
mudiff2 = mudiff(0:ndifv)
deallocate(vdiff); deallocate(mudiff)
do i=0,ndifv
  rdiff2(i) = sum(vdiff2(:,i)**2)
enddo
!___________________________________________ Paths and filename of .cel/.cely
file_inp_cel=''
file_inp_cel(1:lrlx)=rlx(1:lrlx)
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
print'(a)',' INPATH:   '//inpath(1:linpath)
print'(a)',' NAMESTR:  '//namestr(1:Lnamestr)
print'(a)',' FILE_INP: '//trim(file_inp_cel)
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

print*, 'Working Wavelength = ',working_wavelength
print*, '2*Theta Max = ',working_ttmax

!!!!!!!!!!!!!!!!!!!! DONE reading name root of input file and its path
call READALLO_CELY(cely_fn_V=[trim(file_inp_cel)],lcely_fn_V=[len_trim(file_inp_cel)], &
                   ncl2mk=ncl2mk,dcl2mk=dcl2mk,itra=itra,geom=clushape, &
                   diace=Delta_R,cdx=cdx,c_lattice=c, occ1=occs_are_one )
!print*, 'NCL2MK =', NCL2MK
!print*, 'DCL2MK =', DCL2MK, Delta_R*(NCL2MK+1)*0.2d0
!print*, 'clushape = ',clushape


iup=find_unit()
open(iup,status='replace',file=trim(file_inp_cel)//'.pat')
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
print'(a,/,6(1x,f24.14))','a,b,c, alpha, beta, gamma :',abcabgy

DVds=zero
ttrig=( cos(degrees_to_radians*abcabgy(4))-cos(degrees_to_radians*abcabgy(5))*cos(degrees_to_radians*abcabgy(6)) ) / &
           sin(degrees_to_radians*abcabgy(6))
DVds(1,1:3)=[abcabgy(1), abcabgy(2)*cos(degrees_to_radians*abcabgy(6)), abcabgy(3)*cos(degrees_to_radians*abcabgy(5))]
DVds(2,2:3)=[abcabgy(2)*sin(degrees_to_radians*abcabgy(6)), abcabgy(3) * ttrig ]
DVds(3,3) = abcabgy(3) * sqrt( -(ttrig**2) + (sin(degrees_to_radians*abcabgy(5)))**2 )
where(abs(DVds)<100.d0*eps_DP) DVds=zero
print*,'DVds : '
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
print*,'Metric tensor eigenvalues square roots: ',eigMT

NU_CEL_MAX=ndifv


allocate(finu(1))
DO j_abc=1,1
  finu(j_abc)(:)=''
  write(finu(j_abc)(1:8),'("r",i3.3,"_",a3)')iclu_ind,clushape
enddo

!_____________ ALLOCATE LOCAL VECTORS
 
 if (allocated(nat0)) deallocate(nat0)
 if (allocated(xnat0)) deallocate(xnat0)
 if (allocated(ndi0)) deallocate(ndi0)
 if (allocated(summul0)) deallocate(summul0)
 if (allocated(termcon0)) deallocate(termcon0)
 if (allocated(patte_len)) deallocate(patte_len)
 if (allocated(patte_invmul)) deallocate(patte_invmul)
 mmmp=maxval(numpatv(1:n_at_pair_V(1),1))
 allocate(nat0(1:n_sp_atom_V(1),NCL2MK(1)),xnat0(1:n_sp_atom_V(1),NCL2MK(1)),& 
          ndi0(1:n_at_pair_V(1),NCL2MK(1)),&
          summul0(1:n_at_pair_V(1),NCL2MK(1)), termcon0(n_at_pair_V(1),NCL2MK(1)),&
          patte_len(mmmp,n_at_pair_V(1)),patte_invmul(mmmp,n_at_pair_V(1)))
patte_len=zero
patte_maxlen=zero
patte_invmul=zero
do ipa=1,n_at_pair_V(1)
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  do ix=1,numpatv(ipa,1)
    patte_len(ix,ipa) = sum(patte(1:3,ix,ipa,1)*matmul(MTds,patte(1:3,ix,ipa,1)))
    patte_maxlen=max(patte_maxlen,patte_len(ix,ipa))
    fk=two
    if (abs(patte_len(ix,ipa))<sceps_DP.and.is1==is2) fk=one
    patte_invmul(ix,ipa) = fk
  enddo
enddo
patte_maxlen=two*sqrt(patte_maxlen)

diamax = sqrt(maxval(rdiff2)) + patte_maxlen*two+two
dimens = max(nbeta,N1cont)+CEILING(diamax/Deltas)
dimemax=maxval(dimens(Ndelta1:Ndelta2))
print*,'Diameter:',maxval(rdiff2),sqrt(maxval(rdiff2)),patte_maxlen,three,dimens
print*,'samv',Ndelta1,Ndelta2,Deltas(Ndelta1:Ndelta2),dimens(Ndelta1:Ndelta2),dimemax
if (allocated(samv)) deallocate(samv)
allocate(samv(dimemax,n_at_pair_V(1),Ndelta1:Ndelta2,1))
samv=zero



ksph=1
summul0(:,ksph)=zero
termcon0(:,ksph)=zero
do k=1,n_at_pair_V(1)
  ndi0(k,ksph)=ndifv*numpatv(k,1) ! approximately
enddo

do k=1,n_at_pair_V(1)
  print*,'******* PAIR ',k
  ipa=k
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  print*,is1,is2
  print*,izz1,izz2
  print*,Celty(1)%nat(is1),Celty(1)%nat(is2)
  print*,sum(Celty(1)%ato(1:Celty(1)%nat(is1),is1)),sum(Celty(1)%ato(1:Celty(1)%nat(is2),is2))
  print*,sum(Celty(1)%atx(:,1:Celty(1)%nat(is1),is1))/Celty(1)%nat(is1), &
         sum(Celty(1)%atx(:,1:Celty(1)%nat(is2),is2))/Celty(1)%nat(is2)
enddo

! finqi

jlower=1
jupper=1
dr_step=1
print*,'Doing j_ab from ',jlower,' to ',jupper,' by ',dr_step


!finqi
toll=0.0001d0

call CPU_TIME(tstart)
t0=tstart
DO ksph=jlower,jupper,dr_step
  radius = half*diamax
  radius2 = radius**2+sceps_DP
  diameter = two*radius
  nlap = nvec
  xlap = sum(muvec)
  samv=zero
  
  nlatpoi_clu=nlap
  xlatpoi_clu=xlap
  do k=1,n_sp_atom_V(1)
    print*,'cdb',k,n_sp_atom_V(1),ksph,Celty(1)%nat(k),Celty(1)%Z_at(k)
    xnat0(k,ksph) = xlatpoi_clu * sum(Celty(1)%ato(1:Celty(1)%nat(k),k))
    nat0(k,ksph)  = nlatpoi_clu * Celty(1)%nat(k)
  enddo
  if (verbose) then
    SU2=sum(mudiff2)
    print*,'Corr.sum : ',ksph,nlatpoi_clu,xlatpoi_clu,SU2,mudiff2(0),SU2-mudiff2(0)
  endif
  if (output_xyz) then
    lfile_inp_cel=len_trim(file_inp_cel)
    fxyz=''
    idot=index(file_inp_cel(1:lfile_inp_cel),'.',.true.)
    if (idot==0) then
      idot=lfile_inp_cel+1
    endif
    fxyz=''
    fxyz(1:idot-1)=file_inp_cel(1:idot-1)
    write(fxyz(idot:idot+7),'("_",i3.3,".xyz")')ksph
    lfxyz=idot+7
    iux=find_unit()
    open(iux,status='replace',file=fxyz(1:lfxyz))
    write(iux,*) sum(nat0(1:n_sp_atom_V(1),ksph))
    write(iux,*) fxyz(1:lfxyz),ksph
    do ias=1,n_sp_atom_V(1)
      izz=Celty(1)%Z_at(ias)
      do ivec=1,nvec
        aux01=vecs(:,ivec)
        oool=muvec(ivec)
        do ik=1,Celty(1)%nat(ias)
          aux02=aux01+Celty(1)%atx(:,ik,ias)
          ooo=Celty(1)%ato(ik,ias) * oool
          if (ooo<=toll) cycle
          if (ooo>one-yoll) then
            write(iux,'(a2,1x,3(1x,g14.8),1x,g12.6)')symb_of_Z(izz),aux02,one
          else
            write(iux,'(a2,1x,3(1x,g14.8),1x,g12.6)')symb_of_z(izz),aux02,ooo
          endif
        enddo
      enddo
    enddo
    close(iux)
  endif
  do ilad=0,ndifv
    ooo_lat=mudiff2(ilad)
    if (ooo_lat<toll) cycle
    aux_latvec=vdiff2(:,ilad) * two
    d0lats=rdiff2(ilad)
    isoriginL = (d0lats<sceps_DP)
    fffmuL=two
    if (isoriginL) fffmuL=one
    iiimuL=nint(fffmuL)
!            
    DO ipa=1,n_at_pair_V(1)
      is1=Celty(1)%zappa(1,ipa)
      is2=Celty(1)%zappa(2,ipa)
      rfk=one
 !     if (is1/=is2) rfk=four
      do jx=1,numpatv(ipa,1)
        ooo_this = patte(4,jx,ipa,1) * ooo_Lat * rfk
        if (abs(ooo_this)<sceps_DP) cycle
        fffmuP = patte_invmul(jx,ipa)
        isoriginP = (nint(fffmuP)==1)
        ooo_this = ooo_this * max(fffmuP,fffmuL)
        d000a=d0lats + patte_len(jx,ipa)
        d000b=sum(aux_latvec*patte(1:3,jx,ipa,1))

        full_dis_sq1 = d000a + d000b
        
        d0s=sqrt(max(zero,full_dis_sq1))
        if (d0s<Celty(1)%minallowdist(ipa)-0.2d0) then            ! too short distance, set it to 0
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

        if ((isoriginL.or.isoriginP)) cycle
        
        full_dis_sq1 = d000a - d000b
        
        d0s=sqrt(max(zero,full_dis_sq1))
!            if (ksph==0) print*,'in',2,ipa,d0s,ooo_this,ooo_lat,patte(4,jx,ipa,1)
        if (d0s<Celty(1)%minallowdist(ipa)-0.2d0) then            ! too short distance, set it to 0
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


  ncenters_cell=nvec
  cell_volume_red=cell_volume
  allocate(numcells_byphase(1))
  numcells_byphase=1

  xna_all = sum(xnat_glo)
  wmol=sum(atwei(Z_at_glo(:))*xnat_glo)
  xnum_density = xna_all * (dens_phase_gcm3/wmol) * N_A * 1.0d-24
  vol_A3 = xna_all / xnum_density
  Size_Abscissa(1)=one
  Actual_Diam(1) = exp(unter*log(six*vol_A3/pi)) / ten



  call OUTSAMP(finumb=namestr(1:lnamestr)//finu(ksph)(1:8)//'.smp', Bpre=zero)
enddo
iucon=find_unit()
open(iucon,status='replace',file='check_termcon.out')
write(iucon,*)'#',NCL2MK(1),n_sp_atom_V(1),n_at_pair_V(1)
do ksph=1,NCL2MK(1)
  write(iucon,*)ksph,nat0(:,ksph),xnat0(:,ksph)
  write(iucon,*)' A   ',summul0(:,ksph)
  write(iucon,*)' B  ',termcon0(:,ksph)
  write(iucon,*)' C  ',termcon0(:,ksph)+summul0(:,ksph),xnat0(:,ksph)**2
  write(iucon,*)' D  ',Celty(1)%point_neqpair, Celty(1)%point_eqpair
enddo
close(iucon)

print*, '******* JOB SMP MULTIPLE TRANSLATED CLUSTER DONE! *******'

end program Paint_It_Black
