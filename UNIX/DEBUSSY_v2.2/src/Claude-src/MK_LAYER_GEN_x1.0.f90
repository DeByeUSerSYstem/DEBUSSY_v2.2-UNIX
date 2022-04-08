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
use paper_blood
use GEODIS
use Sultans_Of_Swing
use helpinput
use paracry_corr
use INPUT_CLUMKFILE

implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
character(10),parameter :: file_inp0 = 'clumkQ.ini'
integer(I4B),allocatable  :: Z_multipl(:,:)
real(DP) :: DV(3,3),Diam_max_nm,MTxy(2,2),cellCM(3)
real(DP) :: parapar(9),wl_tt(2),smowid
integer(I4B) :: NCL2MK(2)
real(DP)     :: DCL2MK(2)
character(3) :: clushape
character(512) :: pwd,rlx,inpath, file_inp,namestr, rl
character(8) :: pearsy
integer(I4B) :: lpwd,lrlx,linpath, ab_step
character(len=13),dimension(:,:),allocatable :: finu
character(2) :: ch2
Logical      :: DB_exist, isordnot, orthoXY, eqatpair, zerolay, isorigin, &
                nonzerowid, allsizes, nzw0, output_xyz, cond_double

real(DP) :: aux_latvec(2),aux_disvec(2),ava2(2), distvec2D_cry(2),aux_ppp(3)

real(DP), allocatable :: vertdissq(:,:,:,:)

real(DP),allocatable,save     :: xnat0(:,:),summul0(:,:),termcon0(:,:)
integer(I4B),allocatable,save :: nat0(:,:),ndi0(:,:)
integer(I4B),parameter :: ilap = 0
character(8)  :: para_model = 'WelbAnys'
logical :: occs_are_one = .true.


call def_eps
icel=1
DB_exist = .true.


!___ learn what is the currrent directory ($PWD)

print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_LAYER Program    '
print*, ' '

call CPU_TIME(ttot0)
call GET_PWD(pwd=pwd,lpwd=lpwd)
num_grow_dir = 2

call openread_clumkX(clumkfn=file_inp0, &
                     rlx=rlx,lrlx=lrlx, &
                     itra=itra,nclu=NCL2MK,dclu=DCL2MK, &
                     clusha=clushape, &
                     parapar=parapar,smoothing_width=smowid, wlenttmax=wl_tt, &
                     do_allsizes=allsizes, sample_allsteps=multiple_steps, &
                     step_base = ab_step, para_model=para_model,occ1=occs_are_one, do_the_xyz=output_xyz)
working_wavelength=wl_tt(1)
working_ttmax=wl_tt(2)
if (working_wavelength == zero) working_wavelength=wl_ourqmax
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
iupp=find_unit()
open(iupp,status='replace',file=namestr(1:Lnamestr)//'000.pat')

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
                   diace=diace,cdx=cdx,c_lattice=c, occ1=occs_are_one )
if (verbose) print*, 'NCL2MK =', NCL2MK
if (verbose) print*, 'DCL2MK =', DCL2MK
if (verbose) print*, 'clushape = ',clushape
abcabg_loc=abcabgy
if (verbose) print*,'abcabgy = ',abcabgy
DV=zero
DV(1:2,1:2)=planeDV
DV(3,3)=c
MTxy = matmul(transpose(planeDV),planeDV)
sampled_folder='DISTANCES'; lsampled_folder=9
basepath_sampled=inpath(1:linpath); lbasepath_sampled=linpath



!_____________ ALLOCATE LOCAL VECTORS
 
 if (allocated(nat0)) deallocate(nat0)
 if (allocated(xnat0)) deallocate(xnat0)
 if (allocated(ndi0)) deallocate(ndi0)
 if (allocated(summul0)) deallocate(summul0)
 if (allocated(termcon0)) deallocate(termcon0)
 allocate(nat0(1:Celty(icel)%n_sp_atom,NCL2MK(2)),xnat0(1:Celty(icel)%n_sp_atom,NCL2MK(2)), & 
          ndi0(1:Celty(icel)%n_at_pair,NCL2MK(2)),&
          summul0(1:Celty(icel)%n_at_pair,NCL2MK(2)), termcon0(Celty(icel)%n_at_pair,NCL2MK(2)))

!_____________ rock'n'roll

safe_half=half+sceps_DP
napcel=sum(Celty(icel)%nat(1:Celty(icel)%n_sp_atom))
if (verbose) print*,'Atoms/cell:',napcel
if (verbose) print*,'Start filling layers with lattice points'
call CPU_TIME(t0)
!____ fill single-layers with lattice nodes
call FILLSQUARE(Nbeg=1,Nend=NCL2MK(1),geo=clushape,mt22=MTxy,dv22=DV(1:2,1:2), smoowid1=smowid)
call CPU_TIME(t1)
if (verbose) print'(a,1x,f20.3," s")','Done - filled layers with lattice points in',t1-t0
!______________________ SKIP!
if (verbose) print*,'Now start dressing lattice layers with Patterson vectors'
!call CPU_TIME(t0)
!!____ fill single-layers with Patterson multiplicities
!call DRESSLAYERS()
!call CPU_TIME(t1)

!print'(a,1x,f20.3," s")','Done - dressed lattice layers with Patterson vectors in',t1-t0


print*,' '
if (allsizes) then 
    print*,' N_ab = 1 ... ',NCL2MK(1),'; N_c = 1 ... ',NCL2MK(2)
else 
 if (.not. allsizes) then
  print*,' N_ab = ',NCL2MK(1),'; N_c = 1 ... ',NCL2MK(2)
 endif 
endif  
print*,' '
print*,'                          Start evaluating distances...'
print*,' '

call CPU_TIME(t0)
if (verbose) print*, 'CPU(Time) = ',t0

if (allocated(Z_multipl)) deallocate(Z_multipl)
allocate(Z_multipl(NCL2MK(2),0:NCL2MK(2)-1))
Z_multipl=0
do i=1,NCL2MK(2)
  Z_multipl(i,0:i-1)=i-[(j,j=0,i-1)]
  if (i>1) Z_multipl(i,1:i-1) = 2*Z_multipl(i,1:i-1)
enddo

allocate(finu(NCL2MK(1),NCL2MK(2)))
DO j_ab=1,NCL2MK(1); DO j_c=1,NCL2MK(2)
  finu(j_ab,j_c)(:)=''
  write(finu(j_ab,j_c)(1:13),'("a",i3.3,"_c",i3.3,"_",a3)')j_ab, j_c,clushape
enddo; enddo



do ipai=1,Celty(icel)%n_at_pair
  sssp=zero
!  Celty(icel)%minallowdist(ipai)=Celty(icel)%minallowdist(ipai)*0.75d0
  do jz=1,ThePatVec(ipai)%num_Zs
    do ixy=1,ThePatVec(ipai)%patbyz(jz)%num_XYs
      aux_ppp(1:2)=ThePatVec(ipai)%patbyz(jz)%xydist(1:2,ixy)
      aux_ppp(3)=ThePatVec(ipai)%patbyz(jz)%Zval
      aux_ppp=matmul(DV,aux_ppp)
      xlaux=sqrt(sum(aux_ppp**2))
      write(iupp,'(2i2,3(1x,f16.8),2(1x,g16.8))')ThePatVec(ipai)%I1I2, ThePatVec(ipai)%patbyz(jz)%xydist(1:2,ixy), &
      ThePatVec(ipai)%patbyz(jz)%Zval,ThePatVec(ipai)%patbyz(jz)%patmu(ixy), xlaux/Celty(icel)%minallowdist(ipai)
      fk=two
      zdx=max(maxval(abs(ThePatVec(ipai)%patbyz(jz)%xydist(1:2,ixy))),abs(ThePatVec(ipai)%patbyz(jz)%Zval))
      if (zdx<sceps_DP.and.ThePatVec(ipai)%I1I2(1)==ThePatVec(ipai)%I1I2(2)) fk=one
      sssp=sssp+fk*ThePatVec(ipai)%patbyz(jz)%patmu(ixy)
    enddo
  enddo
  write(iupp,*)'Pair ',ipai,ThePatVec(ipai)%I1I2,sssp
enddo
close(iupp)

iupp=find_unit()
open(iupp,status='replace',file=namestr(1:Lnamestr)//'00B.pat')
write(iupp,*)n_at_pair_V(1),sum(Celty(1)%nat),Celty(1)%nat
do ipa=1,n_at_pair_V(1)
  is1=Celty(1)%zappa(1,ipa)
  is2=Celty(1)%zappa(2,ipa)
  izz1=Celty(1)%Z_at(is1)
  izz2=Celty(1)%Z_at(is2)
  sopair = sum(patte(4,1:numpatv(ipa,1),ipa,1))!*two
!  if (is1==is2) sopair=sopair-Celty(1)%nat(is1)
  write(iupp,*)is1,is2,Celty(1)%nat(is1),Celty(1)%nat(is2)
  write(iupp,*)izz1,izz2,numpatv(ipa,1),sopair
  ss1=0.d0;ss2=0.d0
  do jx=1,numpatv(ipa,1)
    zzz=maxval(abs(patte(1:3,jx,ipa,1)))
    fk=2.d0
    if (zzz<sceps_DP.and.is1==is2) fk=1.d0
    ss1=ss1+patte(4,jx,ipa,1);ss2=ss2+patte(4,jx,ipa,1)*fk
    write(iupp,*) patte(1:4,jx,ipa,1),patte(4,jx,ipa,1)*fk
  enddo
  write(iupp,*)'                SUM ',ss1,ss2,c,para_model
enddo
!iupp=find_unit()
!open(iupp,status='replace',file=namestr(1:Lnamestr)//'00C.ppp')
!kppp=0

nmzd=0
DO ipa=1,Celty(icel)%n_at_pair
  nmzd=max(nmzd,ThePatVec(ipa)%num_Zs)
enddo
ALLOCATE(vertdissq(2,NCL2MK(2),nmzd,Celty(icel)%n_at_pair))
vertdissq=zero
DO ipa=1,Celty(icel)%n_at_pair
  nzd=ThePatVec(ipa)%num_Zs
  do jz=1,nzd
    zz0=ThePatVec(ipa)%patbyz(jz)%Zval * c
    zzmi=ThePatVec(ipa)%patbyz(jz)%Zmul_inv
    izzm=nint(zzmi)
    do js=1,izzm
      do lz=0,NCL2MK(2)-1
        cz=lz*c+updown(js)*zz0
        vertdissq(js,lz+1,jz,ipa)=cz**2
      enddo
    enddo
  enddo
enddo

call FILL_GV()

juniq=NCL2MK(1)
jupper=NCL2MK(1)

if (allsizes) then
  juniq=ab_step
  if (modulo(NCL2MK(1),ab_step)>0) then
    jupper=(NCL2MK(1)/ab_step)*ab_step
  endif
endif
if (verbose) print*,'Doing j_ab from ',juniq,' to ',jupper,' by ',ab_step
ncelltypes=1
allocate(numcells_byphase(ncelltypes))


write(iupp,*)'DV 1: ',DV(1,:)
write(iupp,*)'DV 2: ',DV(2,:)
write(iupp,*)'DV 3: ',DV(3,:)
close(iupp)
BASE:DO jl_AB=juniq,jupper,ab_step
  
  if (clushape=='PAR') then
    diaclu=real(jl_AB+1,DP) * diace
  ELSE if (clushape=='CYL') then
    diaclu=real(2*jl_AB-1,DP) * diace
  ELSE if (clushape=='HEX') then
    diaclu=real(2*jl_AB-1,DP) * diace
  endif
  diamax = sqrt(cdx + diaclu**2)
  dimens = max(nbeta,N1cont)+CEILING(diamax/Deltas)
  dimemax=maxval(dimens)
  if (allocated(samv)) deallocate(samv)
  allocate(samv(dimemax,n_at_pair_V(1),Ndelta1:Ndelta2,NCL2MK(2)))
  samv=zero
  ! done
  num_lat_nodes_plane=TheLayers%layers_grid(jl_AB)%NumLatPoi(1)
  xnum_lat_nodes_plane=TheLayers%layers_grid(jl_AB)%XNumLatPoi(1)
  
!________prelim. :: sums of charges / excluding the z-multipl.

  if (verbose) then
     print*,'Celty%* sizes: ',size(Celty(icel)%nat),size(Celty(icel)%xnat),size(Celty(icel)%ato,1),size(Celty(icel)%ato,2)
     print*,'Celty(1)%nat : ',Celty(icel)%nat
  endif   
  do k=1,n_sp_atom_V(1)
    natk=Celty(icel)%nat(k)
    !print*,'Loop:', k,natk
    Celty(icel)%xnat(k)=sum( [(Celty(icel)%ato(i, k),i=1,natk)] )
  enddo
  summul0=zero
  termcon0=zero
  do mz=1,NCL2MK(2)
    do k=1,n_sp_atom_V(1)
      natk=Celty(icel)%nat(k)
      nat0(k,mz) = mz*natk*num_lat_nodes_plane
      xnat0(k,mz)= mz * sum( Celty(icel)%ato(1:natk,k) ) * xnum_lat_nodes_plane
  !    termcon0(k,mz) = mz*sum( Celty(icel)%ato(1:natk, k)**2 ) * xnum_lat_nodes_plane
    enddo
    do k=1,n_at_pair_V(1)
      ndi0(k,mz)=(num_lat_nodes_plane*8)*numpatv(k,1)*mz ! approximately
    enddo
  enddo
  xnld2do= zero
  do j2=TheLayers%layers_grid(jl_AB)%allod(1,2),TheLayers%layers_grid(jl_AB)%allod(2,2)
    do j1=TheLayers%layers_grid(jl_AB)%allod(1,1),TheLayers%layers_grid(jl_AB)%allod(2,1)
      xnld2do=xnld2do+min(one,sum(TheLayers%layers_grid(jl_AB)%Dsqr(j1,j2)%occ_LAT(:)))
    enddo
  enddo
  xnld2do=one/xnld2do
  xdone=zero
  ishowed = 0
  call CPU_TIME(t_starting)
  do j2=TheLayers%layers_grid(jl_AB)%allod(1,2),TheLayers%layers_grid(jl_AB)%allod(2,2)
    aux_latvec(2)=DV(2,2)*j2
    do j1=TheLayers%layers_grid(jl_AB)%allod(1,1),TheLayers%layers_grid(jl_AB)%allod(2,1)
      ooo_Lattice = sum(TheLayers%layers_grid(jl_AB)%Dsqr(j1,j2)%occ_LAT(:))
      if (ooo_Lattice<s4eps_DP) cycle
      aux_latvec(1) = DV(1,1)*j1
      if (.not.orthoXY) aux_latvec(1)=aux_latvec(1)+DV(1,2)*j2
      sqlen_latvec=sum(aux_latvec(1:2)**2)
      
      !________________ Progress evaluation / show progress
      xdone = xdone+xnld2do * min(one,ooo_Lattice)
      iidoi=nint(100.d0*xdone)
      if (iidoi>ishowed) then
        call CPU_TIME(t_now)
        t_passed=0.001d0*real(nint((t_now-t_starting)*1.d3),DP)
        t_1pc=t_passed/real(iidoi,DP)
        t_rem=t_1pc*real(100-iidoi,DP)
        if (verbose) print'(a,i4,a,f16.3,a,f16.6,a,3f11.1)','Done ',iidoi,' %; Time = ',t_passed,'; time/% = ',t_1pc, &
               '; time remaining [s/m/h]= ',t_rem,t_rem/60.d0,t_rem/3600.d0
        ishowed=iidoi
      endif
      !________________ Progress evaluation / show progress done
      
      DO ipa=1,Celty(icel)%n_at_pair
        is1=Celty(icel)%zappa(1,ipa)
        is2=Celty(icel)%zappa(2,ipa)
        izz1=Celty(icel)%Z_at(is1)
        izz2=Celty(icel)%Z_at(is2)
        rrzz12=real(izz1,DP)/real(izz2,DP)
        rrzz21=one/rrzz12
        eqatpair=(is1==is2)
        ifks=1
        if (is1/=is2) ifks=2
        do jz=1,ThePatVec(ipa)%num_Zs
          zeta_value=ThePatVec(ipa)%patbyz(jz)%Zval
          zerolay=(abs(zeta_value)<sceps_DP)
          izzm=nint(ThePatVec(ipa)%patbyz(jz)%Zmul_inv)
          do jxy=1,ThePatVec(ipa)%patbyz(jz)%num_XYs
            ooo_this = ThePatVec(ipa)%patbyz(jz)%patmu(jxy) * ooo_Lattice
            
            if (abs(ooo_this)<sceps_DP) cycle
            
            ddd_cell = ThePatVec(ipa)%patbyz(jz)%scalds(jxy)
            isorigin = (zerolay .and. (ddd_cell<sceps_DP))
            cond_double = (isorigin .and. (.not.eqatpair))
            ava2(1)=ThePatVec(ipa)%patbyz(jz)%xydist(1,jxy)*DV(1,1)
            if (.not.orthoXY) ava2(1)=ava2(1)+DV(1,2)*ThePatVec(ipa)%patbyz(jz)%xydist(2,jxy)
            ava2(2)=ThePatVec(ipa)%patbyz(jz)%xydist(2,jxy)*DV(2,2)
            if (nzw0) then
!__________ evaluate broadening along a
              distvec2D_cry = [j1,j2] + ThePatVec(ipa)%patbyz(jz)%xydist_SUB(:,jxy)
!              if (.not.is_cely) then
!                distvec2D_cry = [j1,j2]+ThePatVec(ipa)%patbyz(jz)%xydist(1:2,jxy)
!              else
!                i2isv = ThePatVec(ipa)%patbyz(jz)%pat_InterSubuPoint(jxy)
!                distvec2D_cry = [j1,j2]+subu_cendis(1:2,i2isv)
!              endif
              if (para_model=='WelbAnys') then
                sigma_a_sq = variance_rs(r=rcorra,s=scorra,sigmax=sigmaxa,xa=distvec2D_cry(1),yb=distvec2D_cry(2))
                sigma_b_sq = variance_rs(r=rcorrb,s=scorrb,sigmax=sigmaxb,xa=distvec2D_cry(1),yb=distvec2D_cry(2))
                phi_ab=zero
                xxx=aux_latvec(1) + ava2(1)
                yyy=aux_latvec(2) + ava2(2)
                if (max(abs(xxx), abs(yyy))>eps_DP) phi_ab = atan2( yyy, xxx )
                nonzerowid = (nzw0 .and. max(sigma_a_sq,sigma_b_sq)>sceps_DP) 
                call EST_SD0([xxx,yyy,phi_ab],sigma_a_sq,sigma_b_sq,nonzerowid)
              else if (para_model=='WelbIsot') then
                dddxy_subu=sum([MTxy(1,1),MTxy(2,2)]*(distvec2D_cry**2))
                if (MTxy(1,2)>sceps_DP) dddxy_subu = dddxy_subu+two*MTxy(1,2)*distvec2D_cry(1)*distvec2D_cry(2)
                nonzerowid=.true.
              endif
            endif
            plane_dis_sqA = sqlen_latvec+ddd_cell
            plane_dis_sqB = two*sum(aux_latvec*ava2)
            plane_dis_sq1 = plane_dis_sqA + plane_dis_sqB
            
            do jC=1,NCL2MK(2)
              do js=1,1
                vv=vertdissq(js,jC,jz,ipa)
                d0s=sqrt(max(zero,vv+plane_dis_sq1))
                if (d0s<Celty(icel)%minallowdist(ipa)) then
                ! too short distance, set it to 0
                  do jc2=jC,NCL2MK(2)
                    zzm = Z_multipl(jC2,jC-1) * ooo_this
                    termcon0(ipa,jC2)=termcon0(ipa,jC2)+zzm
                  enddo
                else
                ! normal distance, accumulate
                  if (nonzerowid) then
                    if (para_model=='WelbAnys') then
                      call CORREC_SD0(vertdissq(js,jC,jz,ipa),d0scorr,sigma_intr,nonzerowid,d0s)
                    else if (para_model=='WelbIsot') then
                      d0s_SUBU = sqrt(max(zero,vv+dddxy_subu))
                      d0scorr = d0s
                      sigma_intr=sqrt(var_targ*(one-exp(-d0s_SUBU/decorr_len)))
                    endif
!                    kppp=kppp+1
!                    write(iupp,*)kppp,d0s,d0scorr
                    call SAM_ONE_TOALL(d0=d0scorr, sigma_intrinsic=sigma_intr)
                  else
                    call SAM_ONE_TOALL(d0=d0s)
                  endif
                  do jc2=jC,NCL2MK(2)
                    zzm = Z_multipl(jC2,jC-1) * ooo_this 
                    summul0(ipa,jC2)=summul0(ipa,jC2)+zzm
                    do ksam=Ndelta1,Ndelta2
                      jsum1=max(1,centersum(ksam)-N1cont)
                      jrr1=jsum1-centersum(ksam)
                      jsum2=min(dimemax,centersum(ksam)+N1cont)
                      jrr2=jsum2-centersum(ksam)
                      samv(jsum1:jsum2,ipa,ksam,jC2) = samv(jsum1:jsum2,ipa,ksam,jC2) + &
                                                       for_samv_P(jrr1:jrr2,ksam)*zzm
                      if (bnd_N(2,ksam) <= 0) cycle
                      samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,jC2) = &
                        samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,jC2) + for_samv_N(bnd_N(1,ksam):bnd_N(2,ksam),ksam)*zzm
                    enddo
                  enddo
                endif
              enddo
            enddo
            if (isorigin.and.eqatpair) cycle
            if (nzw0) then
              distvec2D_cry = [j1,j2] - ThePatVec(ipa)%patbyz(jz)%xydist_SUB(:,jxy)
              if (para_model=='WelbAnys') then
                sigma_a_sq = variance_rs(r=rcorra,s=scorra,sigmax=sigmaxa,xa=distvec2D_cry(1),yb=distvec2D_cry(2))
                sigma_b_sq = variance_rs(r=rcorrb,s=scorrb,sigmax=sigmaxb,xa=distvec2D_cry(1),yb=distvec2D_cry(2))
                phi_ab=zero
                xxx=aux_latvec(1) - ava2(1)
                yyy=aux_latvec(2) - ava2(2)
                if (max(abs(xxx), abs(yyy))>eps_DP) phi_ab = atan2( yyy, xxx )
                nonzerowid = (max_intrinsic_sigma>sceps_DP.or.max(sigma_a_sq,sigma_b_sq)>sceps_DP) 
                call EST_SD0([xxx,yyy,phi_ab],sigma_a_sq,sigma_b_sq,nonzerowid)
              else if (para_model=='WelbIsot') then
                dddxy_subu=sum([MTxy(1,1),MTxy(2,2)]*(distvec2D_cry**2))
                if (MTxy(1,2)>sceps_DP) dddxy_subu = dddxy_subu+two*MTxy(1,2)*distvec2D_cry(1)*distvec2D_cry(2)
                nonzerowid=.true.
              endif
            endif
            plane_dis_sq2 = plane_dis_sqA - plane_dis_sqB
            
            do jC=1,NCL2MK(2) ! separation +1
              do js=izzm,izzm
                vv=vertdissq(js,jC,jz,ipa)
                
                d0s=sqrt(max(zero,vv+plane_dis_sq2))
                if (d0s<Celty(icel)%minallowdist(ipa)) then
                ! too short distance, set it to 0
                  do jc2=jC,NCL2MK(2)
                    zzm = Z_multipl(jC2,jC-1) * ooo_this 
                    termcon0(ipa,jC2)=termcon0(ipa,jC2)+zzm
                  enddo
                else
                ! normal distance, accumulate
                  if (nonzerowid) then
                    if (para_model=='WelbAnys') then
                      call CORREC_SD0(vertdissq(js,jC,jz,ipa),d0scorr,sigma_intr,nonzerowid, d0s)
                    else if (para_model=='WelbIsot') then
                      d0s_SUBU = sqrt(max(zero,vv+dddxy_subu))
                      d0scorr = d0s
                      sigma_intr=sqrt(var_targ*(one-exp(-d0s_SUBU/decorr_len)))
                    endif
!                    kppp=kppp+1
!                    write(iupp,*)kppp,d0s,d0scorr
                    call SAM_ONE_TOALL(d0=d0scorr, sigma_intrinsic=sigma_intr)
                  else
                    call SAM_ONE_TOALL(d0=d0s)
                  endif
                  do jc2=jC,NCL2MK(2) ! height
                    zzm = Z_multipl(jC2,jC-1) * ooo_this 
                    summul0(ipa,jC2)=summul0(ipa,jC2)+zzm
                    do ksam=Ndelta1,Ndelta2
                      jsum1=max(1,centersum(ksam)-N1cont)
                      jrr1=jsum1-centersum(ksam)
                      jsum2=min(dimemax,centersum(ksam)+N1cont)
                      jrr2=jsum2-centersum(ksam)
                      samv(jsum1:jsum2,ipa,ksam,jC2) = samv(jsum1:jsum2,ipa,ksam,jC2) + &
                                                       for_samv_P(jrr1:jrr2,ksam)*zzm
                      if (bnd_N(2,ksam) <= 0) cycle
                      samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,jC2) = &
                        samv(bnd_N(1,ksam):bnd_N(2,ksam),ipa,ksam,jC2) + for_samv_N(bnd_N(1,ksam):bnd_N(2,ksam),ksam)*zzm
                    enddo
                  enddo
                endif
              enddo
            enddo
            
          enddo
        enddo 
      enddo
    
    enddo
  enddo
  do izc=1,NCL2MK(2)
!    Celty(icel)%nat=nat0(:,izc)
!    Celty(icel)%xnat=xnat0(:,izc)
!    Celty(icel)%ndi=ndi0(:,izc)
!    Celty(icel)%summul=summul0(:,izc)
!    Celty(icel)%termcon_all  = termcon0(:,izc)
!    Celty(icel)%termcon_ineq = termcon0(Celty(icel)%point_neqpair(:),izc)
!    Celty(icel)%termcon      = termcon0(Celty(icel)%point_eqpair(:),izc)
    Z_at_glo = Celty(icel)%Z_at
    zappa_glo = Celty(icel)%zappa
    nat_glo = nat0(:,izc)
    ndi_glo = ndi0(:,izc)
    xnat_glo = xnat0(:,izc)
    termcon_glo = termcon0(Celty(icel)%point_eqpair(:),izc)
    termcon_all_glo = termcon0(:,izc)
    termcon_ineq_glo = termcon0(Celty(icel)%point_neqpair(:),izc)
    summul_glo = summul0(:,izc)
    if (verbose) then
      print*,'ZZ nat',jl_AB,izc,allocated(nat_glo),nat_glo
      print*,'ZZxnat',jl_AB,izc,allocated(xnat_glo),xnat_glo
      print*,'ZZtcon',jl_AB,izc,allocated(termcon_glo),termcon_glo
      print*,'ZZtcoA',jl_AB,izc,allocated(termcon_all_glo),termcon_all_glo
      print*,'ZZtcoI',jl_AB,izc,allocated(termcon_ineq_glo),termcon_ineq_glo
      print*,'ZZsmul',jl_AB,izc,allocated(summul_glo),summul_glo
    endif  
    numcells_byphase(1)=izc*xnum_lat_nodes_plane
    Size_Abscissa=real([jl_AB,izc],DP)
    Actual_Diam = [growing_diam(jl_AB,1),growing_diam(izc,2)]
    call OUTSAMP(isam=izc,finumb=namestr(1:lnamestr)//finu(jl_AB,izc)(1:13)//'.smp')
  enddo
  call CPU_TIME(t1)
  print*,'Done all clusters with N_ab # ',jl_AB,' up to N_c # ',izc-1,' - CPU(Time) : ',t1-t0, ' sec.'
  t0=t1
enddo BASE
print*, ' '
call CPU_TIME(ttot)
print*,'Total CPU(Time) : ',ttot-ttot0, ' sec.'

print*, ' '
print*, '******* JOB SMP-'//clushape//' DONE! *******'

end program karn_evil_9
