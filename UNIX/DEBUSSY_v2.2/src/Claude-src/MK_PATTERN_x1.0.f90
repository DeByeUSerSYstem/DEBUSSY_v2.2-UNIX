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
program no_woman_no_cry
use nano_deftyp
use specfun_ac
use atomix
use calcdp_sam
use SPECIAL_TYPES
use read_onesam
use calc_hkl
implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
integer(I4B),parameter :: sam1=1,sam2=32
real(DP),parameter     :: delta0=0.03_DP,eta=0.8_DP


real(DP),allocatable     :: q(:),tt(:),aux1(:),Ic(:),Ic000(:),sI(:),aux2(:,:),aux3(:),sink(:),kos(:),occat(:),baa(:),xnat(:), &
                            samy(:,:),fp_fpp(:,:), divifac(:), &
                            LPF(:),aux4(:),neusl(:),conterm(:),summul(:),fofa(:,:),tefa(:,:),uaux(:),conterm_all(:), &
                            avff2i(:), avff1i(:),fofa_inc(:,:),I_Menke(:,:),I_Menke1(:)
integer(I4B),allocatable :: zaa(:),zaa0(:),nat(:), ndi(:),zappa(:,:),coupling_index(:), point_eqp(:), point_neqp(:),&
                            flag_ano(:)
real(DP)     :: sams(sam1:sam2),qsmx(sam1:sam2),plono(2),fpfpp_EPDL97(2),pola(2)
real(DP)     :: scatcen(0:3),read6(6),ilo(1),ano2(2)
real(DP),allocatable     :: xyzcoo(:,:,:),xyzocc(:,:)
integer(I4B),allocatable :: cosp(:),xyz_nasp(:),uasp(:)
integer(I4B) :: xray_0_neutron_1=0,xray_0_electron_1=0,outall,which_sam_step,do_sofq,ind,lrl
integer(I4B) :: xyz_count_at(0:118)=0,xyz_Z_at(0:118)=0,gaddr(0:118)
character(512) :: rl, waff,step,path,kalamar,kalamari,rrll,lamp,rline,prova, inp_hkl, stob, xyz_file=''
character(len=3) :: div_sofq_flag
character(len=1) :: radtyp=' '
character(len=2) :: asy='  '
integer(I4B) :: I_do_compton = 0
character(len=33) :: mark_knopfler
type(nanoiav)  :: scal_niav
Logical                 ::  got0, calc_sam_step, add_compton_x, get_the_hkls


print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running MK_PATTERN Program    '
print*, ' '

call DEF_EPS

get_the_hkls = .false.
add_compton_x = .false.
xray_0_neutron_1=0
xray_0_electron_1=0
sams=delta0*(/(i,i=sam1,sam2)/)
qsmx=0.5_DP*eta/sams
which_sam_step=0
imode=0
tt_or_q=0
nato=0
iu=find_unit()
outall=0
igive_nscl=0
do_SofQ=0
div_sofq_flag='f2a'
open(iu,status='old',file='diffractor.inp',action='read', iostat=ierr)
    IF (ierr /=0) THEN
        print*, ' Error opening input file: diffractor.inp '
       STOP
     ENDIF
do
  read(iu,'(a)',end=1,err=1)rl
  call CLEAN_line(rl)
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  if (ll==0.or.rl(1:1)=='!'.or.rl(1:1)=='>'.or.rl(1:1)=='%') cycle
  if (rl(1:4)=='STEP' .or. rl(1:4)=='VARX') then
    step=trim(adjustl(rl(5:ll)))
    if (len_trim(step)==0) print*, 'variable X missing, twotheta will be assumed [default] !'
   ! step=trim(adjustl(step))
    lstep=len_trim(step)
    if (verbose) print'("*",a,"*")',step(1:lstep)
    if (step(1:lstep)=='twotheta') imode=0
    if (step(1:lstep)=='q') imode=1
  else if (rl(1:4)=='HKLS') then
    stob=trim(adjustl(rl(5:ll)))
    get_the_hkls = (stob(1:1)=='y'.or.(stob(1:1)=='Y'.or.(stob(1:1)=='1'))) 
  else if (rl(1:4)=='WLEN') then
    read(rl(5:ll),*,iostat=iost)wavel,which_sam_step
    if (iost/=0) then
      read(rl(5:ll),*)wavel
    endif
  else if (rl(1:4)=='RAYS') then
    radtyp=trim(adjustl(rl(5:ll)))
    call LOWCASE(radtyp)
    !! Neutrons and Electrons not allowed [for now] in the distributed version
    if (radtyp=='e' .or. radtyp=='n') then 
        print*, radtyp, ' data modeling still under development! The program stops'
        STOP
    endif    
    if (.not.(ANY(['e','x','n']==radtyp))) then
      radtyp=' '
    endif
  else if (rl(1:4)=='XRNT') then
    read(rl(5:ll),*)xray_0_neutron_1
  else if (rl(1:4)=='SOFQ') then
    read(rl(5:ll),*)do_SofQ
  else if (rl(1:4)=='DIVI') then
    div_sofq_flag = trim(adjustl(rl(5:ll)))
  else if (rl(1:4)=='ELEC') then
    read(rl(5:ll),*)xray_0_electron_1  
  else if (rl(1:4)=='TWOT' .or. rl(1:4)=='RANG' .or. rl(1:4)=='QRAN') then
    read(rl(5:ll),*,iostat=iost)tt0,tt1,dtt
    if (rl(1:4)=='QRAN') tt_or_q=1
    if(iost/=0) then
      write(*,*) 'ERROR: wrong RANG = [min, max, step] values supplied! Program stops!'
      STOP
    endif  
  else if (rl(1:4)=='IMAX') then
    read(rl(5:ll),*)xmaxi
  else if (rl(1:4)=='LONO') then
    read(rl(5:ll),*)plono
  else if (rl(1:4)=='OUTP') then
    read(rl(5:ll),*)outall
  else if (rl(1:5)=='MENKE') then
    read(rl(6:ll),*)xyz_file
  else if (rl(1:4)=='PATH') then
    path=trim(adjustl(rl(5:ll)))
    lpath=len_trim(path)
    if (verbose) print'("*",a,"*")',path(1:lpath)
  else if (rl(1:4)=='FILE') then
    waff=trim(adjustl(rl(5:ll)))
    lwaff=len_trim(waff)
    if (verbose) print'("*",a,"*")',waff(1:lwaff)
  else if (rl(1:4)=='NATO') then
    read(rl(5:ll),*)nato
    natod=(nato*(nato+1))/2
    allocate(zaa(nato),zaa0(nato),baa(nato),fp_fpp(2,nato),neusl(nato),conterm(nato),summul(natod),coupling_index(nato), &
             conterm_all(natod),occat(nato),flag_ano(nato))
    neusl=zero
    conterm=zero
    conterm_all=zero
    zaa=1
    baa=0.05_DP
    occat=one
    fp_fpp=zero
    flag_ano=0
  else if (rl(1:4)=='ZELE') then
    if (nato==0) then
      print*,' ERROR: line NATO must be before line ZELE!'
      stop 'line NATO must be before line ZELE!'
    endif
        read(rl(5:ll),*, iostat=iost)zaa(1:nato)
            if(iost/=0) then
                write(*,*) 'ERROR: wrong number of ZELE supplied! Program stops!'
                STOP
            endif
                
       

  else if (rl(1:4)=='BATO') then
    if (nato==0) then
      print*,' ERROR: line NATO must be before line BATO!'
      stop 'line NATO must be before line BATO!'
    endif
    read(rl(5:ll),*,iostat=iost)baa(1:nato)
        if(iost/=0) then
          write(*,*) 'ERROR: wrong number of BATO supplied! Program stops!'
            STOP
        endif
    
  else if (rl(1:4)=='ATOC') then
    if (nato==0) then
      print*,' ERROR: line NATO must be before line ATOC!'
      stop 'line NATO must be before line BATO!'
    endif
    read(rl(5:ll),*,iostat=iost)occat(1:nato)
        if(iost/=0) then
          write(*,*) 'ERROR: wrong number of ATOC supplied! Program stops!'
            STOP
        endif
  
  else if (rl(1:4)=='FPRI') then
    if (nato==0) then
      print*,' ERROR: line NATO must be before line FPRI!'
      stop 'line NATO must be before line FPRI!'
    endif
    read(rl(5:ll),*,iostat=iost)fp_fpp(1,1:nato)
      if(iost/=0) then
          write(*,*) 'ERROR: wrong number of FPRI supplied! Program stops!'
            STOP
        endif
  
  else if (rl(1:4)=='FDPR') then
    if (nato==0) then
      print*,' ERROR: line NATO must be before line F1F2!'
      stop 'line NATO must be before line F1F2!'
    endif
    read(rl(5:ll),*,iostat=iost)fp_fpp(2,1:nato)
        if(iost/=0) then
          write(*,*) 'ERROR: wrong number of FDPR supplied! Program stops!'
          STOP
        endif
    
  else if (rl(1:4)=='POLA') then
    read(rl(5:ll),*)pola
    if(iost/=0) then
      write(*,*) 'ERROR: wrong value of POLA supplied! Assuming defaults 0.0 0.0'
      pola(1) = zero
      pola(2) = zero 
    endif
  
  else if (rl(1:4)=='CMPT') then
    read(rl(5:ll),*,iostat=iost) I_do_compton
    if(iost/=0) then
      write(*,*) 'ERROR: wrong value of CMPT supplied! Program stops!'
      STOP
    endif
    add_compton_x=(I_do_compton == 1)
    
  else if (rl(1:4)=='NSCL') then
    read(rl(5:ll),*,iostat=iost)neusl(1:nato)
    if(iost/=0) then
      write(*,*) 'ERROR: wrong number of NSCL supplied! Program stops!'
      STOP
    endif
    igive_nscl=1
  endif
enddo

1 close(iu)
if (wavel==0.0d0) then 
  print*, 'ERROR:  wavelength not given in input! Program stops!'
  STOP
endif 
 
if (tt1==0.0d0) then
  print*, 'ERROR: 2θ min, max and step not given in input! Program stops!'
  STOP
endif

print*,' '
print'(" SMP Cluster Pathname: ",a)', path(1:lpath)
print*,' '
print'(" SMP Cluster Filename: ",a)', waff(1:lwaff)
print*,' '

lrty=len_trim(radtyp)
if (lrty==0) then
  if (xray_0_neutron_1==0 .and. xray_0_electron_1==0) then
    radtyp='x'
  else if (xray_0_neutron_1==1 .and. xray_0_electron_1==0) then
    radtyp='n'
  else if (xray_0_neutron_1==0 .and. xray_0_electron_1==1) then
    radtyp='e'
  else if (xray_0_neutron_1==1 .and. xray_0_electron_1==1) then
    stop 'AMBIGUOUS RADIATION CHOSEN (n or e)'
  endif
endif

 if (radtyp=='x') then
     print*,'Radiation used: X-RAY '
 else if (radtyp=='n') then
     print*,'Radiation used: NEUTRON '
 else if (radtyp=='e') then
     print*,'Radiation used: ELECTRON '
 endif
print*,' '

if (igive_nscl==0) then
  neusl=bcohr(zaa(1:nato))
endif

print*,'   Atom #       Z         Sof        Debye-Waller      fprime/nsclen/-      fdprime/-/-  '
print*,' '
do j=1,nato
  if (radtyp=='x' .and. MAXVAL(ABS(fp_fpp(:,j)))<sceps_DP) then
    fp_fpp(:,j) = Anomalous_X(Z_e=zaa(j), wavelength=wavel)
  endif
enddo
do j=1,nato
  if (radtyp=='x') then
    print'(2x,i4,8x,i4,4(1x,f15.8))',j,zaa(j),occat(j),baa(j),fp_fpp(:,j)
    fp_fpp(:,j)=fp_fpp(:,j)*occat(j)
  else if (radtyp=='n') then
    print'(2x,i4,8x,i4,4(1x,f15.8))',j,zaa(j),occat(j),baa(j),neusl(j),zero
  else if (radtyp=='e') then
    print'(2x,i4,8x,i4,4(1x,f15.8))',j,zaa(j),occat(j),baa(j),zero,zero
  endif
enddo
print*,' '

npq=1+NINT((tt1-tt0)/dtt)
allocate(q(npq),tt(npq),sink(npq),kos(npq),aux1(npq),Ic(npq),Ic000(npq),sI(npq),aux2(npq,nato),aux3(npq),aux4(npq),LPF(npq), &
         uaux(npq),fofa(npq,nato),tefa(npq,nato),avff1i(npq),avff2i(npq),divifac(npq),fofa_inc(npq,nato))
fofa_inc=zero
if (imode==0) then
  dtt=(tt1-tt0)/(npq-1)
  tt=dtt*(/(i,i=0,npq-1)/)+tt0
  q=two*sin(duet2r*tt)/wavel
  q1=maxval(q)
  q0=minval(q)
  print*, ' '
  print'(a,g12.5)', ' Pattern simulation with constant 2-theta step: ', dtt
  print*, '  ttmin ', tt0, '    ttmax ', tt1
  print*, ' '
else if (imode==1) then
  if (tt_or_q==0) then
    q0=two*sin(duet2r*tt0)/wavel
    q1=two*sin(duet2r*tt1)/wavel
   ! print*, 
    ! q1=max(q1,qsmx(1)) !!__RF 09.03.2018
    dq=(q1-q0)/(npq-1)
    q=dq*(/(i,i=0,npq-1)/)+q0
    tt=asin(0.5_DP*wavel*q)/duet2r
  else if (tt_or_q==1) then
    q0=tt0
    q1=tt1
    ! q1=max(q1,qsmx(1)) !!__RF 09.03.2018
    dq=dtt
    q=dq*(/(i,i=0,npq-1)/)+q0
    tt=asin(0.5_DP*wavel*q)/duet2r
  endif
  print*, ' '
  print'(a,g12.5)', ' Pattern simulation with constant q step: ', dq
  print*, '  qmin ', q0, '    qmax ', q1
  print*, ' '
endif

  if (imode==0) then
    LPF = one
  else if (imode==1) then
    LPF = one/cos(tt*duet2r)
  endif
 


  if (which_sam_step>0) then
    isam=which_sam_step/30
    if (q1>qsmx(isam)) then
        stop 'Too high q for the chosen step.'
    endif
  else
    isam=sam1
    do jsm=sam1,sam2
      if (verbose) print*,jsm,qsmx(jsm),sams(jsm),q1
      if (qsmx(jsm)<q1) exit
      isam=jsm
    enddo
  endif

if (verbose) print'("SAMPLING STEP:",i4,1x,g16.4)',isam,sams(isam)  
print'(" Sampling Step: "g12.3)',sams(isam)
print*,' '


if (verbose) print'(a)','now the name'
if (verbose) print'("*",a,"*",i8)',path(1:lpath),lpath
 kalamar=''
 kalamari=''
 kalamar(1:lpath) = path(1:lpath)
if (verbose) print'("*",a,"*")',trim(kalamar)
if (kalamar(lpath-5:lpath)=='SAMPTO') then
  write(kalamar(lpath+1:lpath+5),'(i3.3,"A",a1)')nint(1000.0_DP*sams(isam)),separator
  if (verbose) print'("*",a,"*")',trim(kalamar)
  kalamar(lpath+6:lpath+5+lwaff) = waff(1:lwaff)
  if (verbose) print'("*",a,"*")',trim(kalamar)
  kalamari(1:lpath+5+lwaff) = kalamar(1:lpath+5+lwaff)
  if (verbose) print'("*",a,"*")',trim(kalamari)
  kalamari(1+lpath+5+lwaff:5+lpath+5+lwaff) = '_INFO'
  if (verbose) print'("*",a,"*")',trim(kalamari)
else
  write(kalamar(lpath+1:lpath+11),'("SAMPTO",i3.3,"A",a1)')nint(1000.0_DP*sams(isam)),separator
  if (verbose) print'("*",a,"*")',trim(kalamar)
  kalamar(lpath+12:lpath+11+lwaff) = waff(1:lwaff)
  if (verbose) print'("*",a,"*")',trim(kalamar)
  kalamari(1:lpath+11+lwaff) = kalamar(1:lpath+11+lwaff)
  if (verbose) print'("*",a,"*")',trim(kalamari)
  kalamari(1+lpath+11+lwaff:5+lpath+11+lwaff) = '_INFO'
  if (verbose) print'("*",a,"*")',trim(kalamari)
endif
 iu=find_unit()
 if (verbose)print'(a,a)','OPENING FILE: ',trim(kalamar)
 open(iu,status='old',file=trim(kalamar),action='read',iostat=ierr)
    IF (ierr /=0) THEN
        print*, ' Error opening file: ',trim(kalamar)
       STOP
     ENDIF
 read(iu,*)
 iui=find_unit()
 open(iui,status='old',file=trim(kalamari),action='read',iostat=ierr)
    IF (ierr /=0) THEN
        print*, ' Error opening file: ',trim(kalamari)
       STOP
     ENDIF
 read(iui,'(a)')rl
 rl=trim(adjustl(rl))
 ll=len_trim(rl)

 read(rl(1:ll),*,iostat=io1)ndimens,n_sp_atom, n_at_pair, a_latt_cioc, rho, Delta, addBw
 if (io1/=0) then
   read(rl(1:ll),*)ndimens,n_sp_atom, n_at_pair, a_latt_cioc, rho, Delta
   addBw=zero
 endif
 baa=max(zero,baa+addBw)
 if (n_sp_atom/=nato) then
   print*,'ERROR: number of atomic species not correctly defined.',n_sp_atom,nato
   stop 'number of atomic species not correctly defined.'
 endif
 sss=abs(Delta-sams(isam))
 if (sss>sceps_DP) then
   print*,'ERROR: mishandled Delta/=sams(isam)! ',Delta,sams(isam)
   stop 'mishandled Delta/=sams(isam)!'
 endif


 do j=1,nato
   if (radtyp=='x') then
     if (verbose) print*,'Radiation used: X-RAY '
     if (zaa(j)>0) then
       flag_ano(j)=1
       fofa(:,j) = occat(j)*FormFact(q=q,Z_e=zaa(j), radiation_type='x')
       fofa_inc(:,j) = occat(j)*COMP_COMPTON_S(q=q, Z_e=zaa(j), wavelength=wavel, beam_pol_ang_ecc=pola)
       tefa(:,j) = exp(-unqua*q*q*baa(j))
       aux2(:,j) = (fofa(:,j)+fp_fpp(1,j))*tefa(:,j)
       if (verbose) print*,'Atom, Z, B, (USING fpri_Fdpri) ',j,zaa(j),baa(j)
     else
       aux2(:,j) = zero
       tefa(:,j) = one
       fofa(:,j) = zero
       fofa_inc(:,j) = zero
     endif
   else if (radtyp=='n') then
     if (verbose) print*,'Radiation used: NEUTRON ',j,neusl(j),baa(j)
     fofa(:,j) = occat(j)*neusl(j) !FormFact(q=q,Z_e=zaa(j),radiation_type='n')
     fofa_inc(:,j) = sqrt(sig_inc(zaa(j))*100.d0/(four*pi))
     tefa(:,j) = exp(-unqua*q*q*baa(j))
     aux2(:,j) = fofa(:,j)*tefa(:,j)
   else if (radtyp=='e') then
     if (verbose) print*,'Radiation used: ELECTRON '
     fofa(:,j) = occat(j)*FormFact(q=q,Z_e=zaa(j),radiation_type='e')
     tefa(:,j) = exp(-unqua*q*q*baa(j))
     aux2(:,j) = fofa(:,j)*tefa(:,j)
   endif
   if (verbose) print*,'done form factor', maxval(aux2(:,j)),minval(aux2(:,j))
   j1=j;j2=j
    IF (verbose) print*,radtyp//'-fofa:',j1,minval(fofa(:,j1)),maxval(fofa(:,j1)),j2,minval(fofa(:,j2)),maxval(fofa(:,j2))
    IF (verbose) print*,radtyp//'-aux2:',j1,minval(aux2(:,j1)),maxval(aux2(:,j1)),j2,minval(aux2(:,j2)),maxval(aux2(:,j2))
    IF (verbose) print*,radtyp//'-tefa:',j1,minval(tefa(:,j1)),maxval(tefa(:,j1)),j2,minval(tefa(:,j2)),maxval(tefa(:,j2))
    IF (verbose) print*,radtyp//'-tefa:',j1,minval(fofa_inc(:,j1)),maxval(fofa_inc(:,j1)),j2,minval(fofa_inc(:,j2)),&
           maxval(fofa_inc(:,j2))
 enddo
 
 IF (verbose) print*,'Allocating things',nato,n_at_pair,ndimens

 allocate(nat(nato),xnat(nato), ndi(n_at_pair),zappa(2,n_at_pair),samy(ndimens,n_at_pair), &
          point_eqp(nato),point_neqp(n_at_pair-nato))
 IF (verbose) print*,'Allocated things',nato,n_at_pair,ndimens


 
 iflag_conterm = 1
 do k=1,n_sp_atom
   read(iui,'(a)')rrll
   rrll=trim(adjustl(rrll))
   nll=len_trim(rrll)
   read(rrll(1:nll),*,iostat=io1234)kx,zaa0(k),nat(k),xnat(k),conterm(k)
   if (io1234/=0) then
     read(rrll(1:nll),*)kx,zaa0(k),nat(k),xnat(k)
     iflag_conterm = 0
     conterm(k)=zero
   endif
 enddo
 IF (verbose) print*,'iflag_conterm ',iflag_conterm
 
 avff1i=zero
 cdiv1=zero
 do j=1,nato
   avff1i=avff1i+xnat(j)*fofa(:,j)
   cdiv1=cdiv1+xnat(j)
 enddo
 xnattotal=cdiv1
 print*, 'Total # atoms: ',xnattotal
 cnu=one/cdiv1
 avff1i = avff1i*cnu ! <b>
 avff1i= sign(one,avff1i)/max(eps_DP**2,abs(avff1i)) ! 1/<b>
 avff1i= (avff1i**2) * cnu                           ! (1/<b>)^2*1/N
 if (verbose) print*,maxval(avff1i),minval(avff1i),sum(avff1i)/size(avff1i) 
 
 
 kk0=0
 kk1=0
 do kk=1,n_at_pair
   if (iflag_conterm==1) then
     read(iui,*)kkx,ndi(kk),zappa(:,kk),summul(kk)
   else
     read(iui,*)kkx,ndi(kk),zappa(:,kk)
     summul(kk)=zero
   endif
   if (zappa(1,kk)==zappa(2,kk)) then
     kk0=kk0+1
     coupling_index(kk0) = kk
     point_eqp(zappa(1,kk)) = kk
   else
     kk1=kk1+1
     point_neqp(kk1) = kk
   endif
 enddo
 read(iui,*)
 iflag_conterm_all=1
 read(iui,*,iostat=i2222)conterm_all
 if (i2222/=0) then
   conterm_all=zero
   iflag_conterm_all=0
   if (iflag_conterm/=0) conterm_all(point_eqp(1:n_sp_atom))=conterm
 endif
 close(iui)
 
 if (verbose) then
   print*,'Zero distance terms: ',iflag_conterm_all,iflag_conterm
   print'(15(i10,12x))',(kk,kk=1,n_at_pair)
   print'(15(2x,2i9))',(zappa(:,kk),kk=1,n_at_pair)
   print'(15(1x,g21.15))',conterm_all
 endif
 mul_cont=4
 
 avff2i=zero
 cdiv=zero
 do kk=1,n_at_pair
   j1=zappa(1,kk)
   j2=zappa(2,kk)
   avff2i=avff2i+conterm_all(kk)*fofa(:,j1)*fofa(:,j2)
   cdiv=cdiv+conterm_all(kk)
 enddo
 xnat_all = sum(xnat)
 avff2i= (cdiv/xnat_all)*sign(one,avff2i)/max(eps_DP**2,abs(avff2i))

 do kk=1,n_at_pair
   do js=1,ndimens
     read(iu,*)samyx
     if (iflag_conterm==0) then
       summul(kk) = summul(kk) + samyx
     endif
     samy(js,kk)=samyx/js
   enddo
 enddo
 close(iu)
if (iflag_conterm==0) then
  do k=1,n_sp_atom
    kk=coupling_index(k)
    conterm(k)=xnat(k)*xnat(k)-summul(kk)
    conterm_all(point_eqp(k))=conterm(k)
  enddo
endif

if (iflag_conterm_all==1) then
  dct=maxval(abs(conterm_all(point_eqp(1:n_sp_atom))-conterm))
  if (dct>s4eps_DP) then
    print*, 'Constant terms misaligned or wrong!'
    print*,point_eqp
    print*,point_neqp
    print*,conterm_all(point_eqp)
    print*,conterm
    stop 'Constant terms misaligned or wrong!'
  endif
endif

IF (verbose) THEN
   do k=1,n_sp_atom
      print*,'Nonnapapera: SUM_OCC**2, SUM_OCC ',k,conterm(k),xnat(k)
   enddo
ENDIF

got0 = (minval(q)<eps_DP)
divifac0 = sum(xnat(1:nato))

if (do_SofQ==0) then
  IF (verbose) print*,'Including the constant term ',iflag_conterm_all,iflag_conterm
  IF (verbose) print*,Pi2,rho,sams(isam)
  ronald = Pi2*rho*sams(isam)
  ronald = half*ronald*ronald
  Ic = zero
  Ic000 = zero
  divifac = one
  aux4=exp(ronald*q*q)
  aux3 = Pi2*q*sams(isam)
  kos=cos(aux3)
  
  sink=aux4*sincf(aux3)
!   if (.not.got0) then
!     sink = aux4*sin(aux3)/aux3
!   else
!     where (abs(q)<eps_DP)
!       sink = aux4
!     elsewhere
!       sink= aux4*sin(aux3)/aux3
!     end where
!   endif
  IF (verbose) print*,'sinc_C: ',maxval(sink),minval(sink),maxval(abs(sink)),minval(abs(sink))
  do kk=1,n_at_pair
    j1=zappa(1,kk)
    j2=zappa(2,kk)
    if (abs(conterm_all(kk)) > sceps_DP) then
      if (verbose) print*,'kk,conterm_all(kk) ',kk,conterm_all(kk)
      if (max(flag_ano(j1),flag_ano(j2))>0) then
        Ic000=Ic000+conterm_all(kk)*( (fofa(:,j1)+fp_fpp(1,j1))*(fofa(:,j2)+fp_fpp(1,j2))+fp_fpp(2,j1)*fp_fpp(2,j2) )
      else
        Ic000=Ic000+conterm_all(kk)*fofa(:,j1)*fofa(:,j2)
      endif
    endif
    if (max(flag_ano(j1),flag_ano(j2))>0) then
      aux1 = (aux2(:,j1)*aux2(:,j2)+tefa(:,j1)*tefa(:,j2)*fp_fpp(2,j1)*fp_fpp(2,j2))*sink
    else
      aux1 = aux2(:,j1)*aux2(:,j2)*sink
    endif
    Ic = Ic + aux1 * U_cheb(dum=vec1_arg, c=samy(:,kk),x=kos)
    IF (verbose) print*,'fofa:',j1,minval(fofa(:,j1)),maxval(fofa(:,j1)),j2,minval(fofa(:,j2)),maxval(fofa(:,j2))
    IF (verbose) print*,'aux2:',j1,minval(aux2(:,j1)),maxval(aux2(:,j1)),j2,minval(aux2(:,j2)),maxval(aux2(:,j2))
    IF (verbose) print*,'tefa:',j1,minval(tefa(:,j1)),maxval(tefa(:,j1)),j2,minval(tefa(:,j2)),maxval(tefa(:,j2))
  enddo
  
 IF (add_compton_x .and. radtyp=='x') then 
  do i=1,nato
     Ic=Ic+fofa_inc(:,i)*xnat(i)
   enddo 
  ENDIF
  
  Ic = Ic+Ic000
  if (maxval(abs(lpf-one))>sceps_DP) Ic=Ic*lpf

  if (xmaxi>0.00000001_DP) then
    ack = xmaxi/maxval(Ic)
    Ic=Ic*ack
    Ic000=Ic000*ack
  else
    ack=one
  endif
  IF (verbose) print*,'Ical_min, Ical_max, I0_min, I0_max ',minval(Ic),maxval(Ic),minval(Ic000),maxval(Ic000)


else if (do_SofQ==1) then
  ack=one
  print*,'Evaluating S(q) - not including the constant term and scaling by '//div_sofq_flag
  IF (verbose)  print*,iflag_conterm_all,iflag_conterm
  
  IF (verbose) print*,Pi2,rho,sams(isam)
  idivf=0
  if (div_sofq_flag(1:1)=='f'.or.div_sofq_flag(1:1)=='b') then
    idivf=1
  else if (div_sofq_flag(1:1)=='Z') then
    idivf=2
  endif
  if (idivf<1.or.idivf>2) then
    print*,'Error - S(q) division flag [DIVI] must be one of Z2a, f2a, b2a, Za2, fa2, ba2; it is ',div_sofq_flag
    stop 'Error - S(q) division flag [DIVI] must be one of Z2a, f2a, b2a, Za2, fa2, ba2; it is NOT!'
  endif
  i_sa_vs_as = 0
  if (div_sofq_flag(2:3)=='a2') then
    i_sa_vs_as=1
  else if (div_sofq_flag(2:3)=='2a') then
    i_sa_vs_as=2
  endif
  if (i_sa_vs_as<1.or.i_sa_vs_as>2) then
    print*,'Error - S(q) division flag [DIVI] must be one of Z2a, f2a, b2a, Za2, fa2, ba2; it is ',div_sofq_flag
    stop 'Error - S(q) division flag [DIVI] must be one of Z2a, f2a, b2a, Za2, fa2, ba2; it is NOT!'
  endif
  
  divifac = zero
  divifac0= zero
  do ja=1,nato
    xnat_occ=xnat(ja)*occat(ja)
    if (zaa(ja)==0) cycle
    if (radtyp=='x') then
      if (verbose) print*,'Radiation used: X-RAY '
      if (i_sa_vs_as==1) then ! to be checked case of square averaged
        if (idivf==1) then
          divifac = divifac + xnat_occ*(fofa(:,ja)**2)
        else if (idivf==2) then
          divifac = divifac + xnat_occ*real(Zaa(ja)**2,DP)
        endif
      else if (i_sa_vs_as==2) then
        if (idivf==1) then
          divifac=divifac+xnat_occ*(fofa(:,ja))
        else if (idivf==2) then
          divifac=divifac+xnat_occ*real(Zaa(ja),DP)
        endif
      endif
    else if (radtyp=='e') then
      if (verbose) print*,'Radiation used: ELECTRON '
      if (i_sa_vs_as==1) then
        if (idivf==1) then
          divifac=divifac+xnat_occ*(fofa(:,ja)**2)
        else if (idivf==2) then
          divifac=divifac+xnat_occ*real(Zaa(ja)**2,DP)
        endif
      else if (i_sa_vs_as==2) then
        if (idivf==1) then
          divifac=divifac+xnat_occ*(fofa(:,ja))
        else if (idivf==2) then
          divifac=divifac+xnat_occ*real(Zaa(ja),DP)
        endif
      endif
    else if (radtyp=='n') then
      if (verbose) print*,'Radiation used: NEUTRON ',j,neusl(j),baa(j)
      if (i_sa_vs_as==1) then
        divifac=divifac+xnat_occ*(fofa(:,ja)**2)
      else if (i_sa_vs_as==2) then
        divifac=divifac+xnat_occ*(fofa(:,ja))
      endif
    endif
    divifac0=divifac0+xnat_occ
  enddo
  odivifac0=one/divifac0
!  divifac=divifac*odivifac0
  if (i_sa_vs_as==2) then
    divifac=(divifac*odivifac0)**2
  endif
  
  Ic = zero
  Ic000 = zero
  ronald = Pi2*rho*sams(isam)
  ronald = half*ronald*ronald
  aux4=exp(ronald*q*q)
  aux3 = Pi2*q*sams(isam)
  kos=cos(aux3)
  
  sink = aux4*sincf(aux3)
!   if (.not.got0) then
!     sink = aux4*sin(aux3)/aux3
!   else
!     sink(1) = aux4(1)
!     sink(2:)= aux4(2:)*sin(aux3(2:))/aux3(2:)
!   endif
  IF (verbose) print*,'sinc_C: ',maxval(sink),minval(sink),maxval(abs(sink)),minval(abs(sink))
  do kk=1,n_at_pair
    j1=zappa(1,kk)
    j2=zappa(2,kk)
    occ_pair=occat(j1)*occat(j2)
    if (abs(conterm_all(kk)) > sceps_DP) then
      if (max(flag_ano(j1),flag_ano(j2))>0) then
        Ic000=Ic000+conterm_all(kk)*( (fofa(:,j1)+fp_fpp(1,j1))*(fofa(:,j2)+fp_fpp(1,j2))+fp_fpp(2,j1)*fp_fpp(2,j2) ) &
                    * occ_pair
      else
        Ic000=Ic000+conterm_all(kk)*fofa(:,j1)*fofa(:,j2) &
                    * occ_pair
      endif
    endif
    if (max(flag_ano(j1),flag_ano(j2))>0) then
      aux1 = (aux2(:,j1)*aux2(:,j2)+tefa(:,j1)*tefa(:,j2)*fp_fpp(2,j1)*fp_fpp(2,j2))*sink &
                    * occ_pair
    else
      aux1 = aux2(:,j1)*aux2(:,j2)*sink &
                    * occ_pair
    endif
    Ic = Ic + aux1 * U_cheb(dum=vec1_arg, c=samy(:,kk),x=kos)
  enddo

   IF (add_compton_x .and. radtyp=='x') then 
  do i=1,nato
     Ic=Ic+(fofa_inc(:,i)*xnat(i))*ack  !total Compton  = matmul(fofa_inc(:,:),xnat(:)) * ack [ ack is the plotting scale ]
   enddo 
  ENDIF

endif

IF (verbose) print*,'Ical_min, Ical_max, conterm, xnat ',minval(Ic),maxval(Ic),conterm,xnat
iuu=find_unit()
lkal=len_trim(kalamar)
lasts=INDEX(kalamar(1:lkal),separator,.true.)
if (lasts==lkal) lasts=0
mark_knopfler=''
lmark_knopfler=0
if (radtyp=='x') then
  mark_knopfler='_X'
  lmark_knopfler=lmark_knopfler+2
else if (radtyp=='e') then
  mark_knopfler='_E'
  lmark_knopfler=lmark_knopfler+2
else if (radtyp=='n') then
  mark_knopfler='_N'
  lmark_knopfler=lmark_knopfler+2
endif

if (do_SofQ==0) then
  mark_knopfler(lmark_knopfler+1:lmark_knopfler+5)='_Iexp'
  lmark_knopfler=lmark_knopfler+5
else if (do_SofQ==1) then
  mark_knopfler(lmark_knopfler+1:lmark_knopfler+5)='_Sofq'
  lmark_knopfler=lmark_knopfler+5
  mark_knopfler(lmark_knopfler+1:lmark_knopfler+4)='_'//div_sofq_flag(1:3)
  lmark_knopfler=lmark_knopfler+4
endif
i=0
lll=lmark_knopfler
do
  if (i>lll) exit
  i=i+1
  if (mark_knopfler(i:i)==' ') then
    mark_knopfler(i:lll-1)=mark_knopfler(i+1:lll)
    lll=lll-1
  endif
enddo


!!!___here Menke correction to be in output as additional column

gaddr=-1
xyz_file=trim(adjustl(xyz_file))
lfxyz=len_trim(xyz_file)
if (lfxyz>0) then !_________ there is a given .xyz file...
  allocate(I_Menke(npq,2),I_Menke1(npq))
  I_Menke=zero; I_Menke1=zero
  iuxyz=find_unit()
  scatcen=zero
  open(iuxyz,status='old',action='read',file=xyz_file(1:lfxyz))
  read(iuxyz,*)n_at_xyz
  read(iuxyz,*)
  
  read(iuxyz,'(a)')rl
  call CLEAN_LINE(rl)
  rl=trim(adjustl(rl))
  lrl=len_trim(rl)
  asy=rl(1:2)
  do i=0,n_elements
    IF (asy /= symb_of_Z(i)) CYCLE
    iZa = i
    EXIT
  enddo
  islab=0
  read(rl(3:lrl),*,iostat=io6)read6
  if (io6==0) then
    nuli=6
    islab=1
  else
    read(rl(3:lrl),*,iostat=io5)read6(1:5)
    if (io5==0) then
      nuli=5
    else
      read(rl(3:lrl),*,iostat=io4)read6(1:4)
      if (io4==0) then
        nuli=4
      else
        read(rl(3:lrl),*,iostat=io3)read6(1:3)
        if (io3==0) then
          nuli=3
        else
          print*,'INVALID .xyz file (less than 3 coordinates per atom)'
          stop 'INVALID .xyz file (less than 3 coordinates per atom)'
        endif
      endif
    endif
  endif
  rewind(iuxyz)
  allocate(cosp(nato),xyz_nasp(nato),uasp(nato))
  xyz_nasp=0
  read(iuxyz,*)
  read(iuxyz,*)
  nfsp=0
  n_all_at = sum(nat)
  do j=1,n_all_at
    read(iuxyz,'(a)')rl
    call CLEAN_LINE(rl)
    rl=trim(adjustl(rl))
    lrl=len_trim(rl)
    asy=rl(1:2)
    read(rl(3:lrl),*)read6(1:nuli)
    do i=0,n_elements
      IF (asy /= symb_of_Z(i)) CYCLE
      iZa = i
      EXIT
    enddo
    if (islab==0) then
      if (nfsp>0) then
        iiza=minval(abs(iZa-uasp(1:nfsp)))
      else
        iiza=1
      endif
      if (iiza>0) then
        nfsp=nfsp+1
        uasp(nfsp)=iZa
        ksp=nfsp
      else
        ilo=minloc(abs(iZa-uasp(1:nfsp)))
        ksp=ilo(1)
      endif
    else
      ksp=NINT(read6(6))
      uasp(ksp)=iZa
    endif
    xyz_nasp(ksp)=xyz_nasp(ksp)+1
  enddo
  rewind(iuxyz)
  read(iuxyz,*)
  read(iuxyz,*)

  uasp = zaa  !!! overriding the Z ...
  naallo=maxval(xyz_nasp)
  allocate(xyzcoo(3,naallo,nato),xyzocc(naallo,nato))
  xyzocc = zero

  cosp=0
  do j=1,n_all_at
    read(iuxyz,'(a)')rl
    call CLEAN_LINE(rl)
    rl=trim(adjustl(rl))
    lrl=len_trim(rl)
    asy=rl(1:2)
    do i=0,n_elements
      IF (asy /= symb_of_Z(i)) CYCLE
      iZa = i
      EXIT
    enddo
    
    if (islab==0) then
      ilo=minloc(abs(iZa-uasp(1:nfsp)))
      ksp=ilo(1)
    else
      ksp=NINT(read6(6))
    endif
    cosp(ksp)=cosp(ksp)+1
    
    if (radtyp=='x'.or.radtyp=='e') then
      ano2=Anomalous_X(Z_e=uasp(ksp), wavelength=wavel)
      wskatr=real(uasp(ksp),DP)+ano2(1)
      wskati=ano2(2)
    else if (radtyp=='n') then
      wskati=zero
      wskatr=neusl(ksp)
    endif
    wskata=sqrt(wskatr**2+wskati**2)
    
    read(rl(3:lrl),*)read6(1:nuli)
    xyzcoo(:,cosp(ksp),ksp) = read6(1:3)
    if (nuli>=5) then
      xyzocc(cosp(ksp),ksp) = read6(5)
    else if (nuli==4) then
      xyzocc(cosp(ksp),ksp) = read6(4)
    else if (nuli==3) then
      xyzocc(cosp(ksp),ksp) = one
    endif
    scatcen(0)=scatcen(0) + xyzocc(cosp(ksp),ksp)*wskata
    scatcen(1:3)=scatcen(1:3) + xyzocc(cosp(ksp),ksp)*xyzcoo(:,cosp(ksp),ksp)*wskata
    
  enddo
  close(iuxyz)
  print*,'sc',scatcen
  scatcen(1:3)=scatcen(1:3)/scatcen(0)
  print*,'sc',scatcen
  do ia=1,nato
    do i2=1,xyz_nasp(ia)
      xyzcoo(:,i2,ia)=xyzcoo(:,i2,ia)-scatcen(1:3)
    enddo
  enddo
  
  
  do ia=1,nato
    do i2=1,xyz_nasp(ia)
      xl=pi2*sqrt(sum(xyzcoo(:,i2,ia)**2))
      sink=sincf(xl*q)
      I_Menke(:,1)=I_Menke(:,1)+ occat(ia) * xyzocc(i2,ia)*( fofa(:,ia)+fp_fpp(1,ia) )*sink
      I_Menke(:,2)=I_Menke(:,2)+ occat(ia) * xyzocc(i2,ia)*fp_fpp(2,ia)*sink
    enddo
  enddo
  if (ANY(ISNAN(I_Menke))) print*,'here'
  !!!! The occupancies occat read from the diffractor.inp are MULTIPLIED ON TOP of those read from the .xyz 
  !!!! as well as those implied in the .smp file
!   I_Menke(:,1) = sqrt(I_Menke(:,1)*I_Menke(:,1)+I_Menke(:,2)*I_Menke(:,2))
  I_Menke1(:) = I_Menke(:,1)*I_Menke(:,1)+I_Menke(:,2)*I_Menke(:,2)
  
endif

open(iuu,status='replace',file=kalamar(lasts+1:lkal-4)//mark_knopfler(1:lmark_knopfler)//'.tqi')

write(iuu,*)'#',npq,maxval(Ic),xnattotal,tt0,tt1,dtt,wavel,divifac0


if (lfxyz>0) then !___ with Menke

  if (do_SofQ==1) then
    IF (outall==1) then
      do i=1,npq
!        write(iuu,*)tt(i),q(i),divifac0*Ic(i)/divifac(i),Ic000(i),divifac(i),LPF(i),kos(i),aux4(i),aux2(i,:),&
!                    fofa(i,:),tefa(i,:),I_Menke(i,1)
        write(iuu,*)tt(i),q(i),Ic(i)/divifac(i),Ic000(i),divifac(i),LPF(i),kos(i),aux4(i),aux2(i,:),&
                    fofa(i,:),tefa(i,:),&
                    SUM(fofa_inc(i,:)*xnat), & !!! total Compton  = matmul(fofa_inc(:,:),xnat(:)) * ack [ ack is the plotting scale ]
                    I_Menke1(i)
      enddo
    ELSE
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i)/divifac(i),Ic000(i),divifac(i),I_Menke1(i)
      enddo
    endif
  else if (do_SofQ==0) then
    IF (outall==1) then
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i),Ic000(i),1,LPF(i),kos(i),aux4(i),aux2(i,:),fofa(i,:),tefa(i,:),&
                    SUM(fofa_inc(i,:)*xnat)*ack, & !!! total Compton  = matmul(fofa_inc(:,:),xnat(:)) * ack [ ack is the plotting scale ]
                    I_Menke1(i)*ack 
      enddo 
    ELSE
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i),Ic000(i),1,I_Menke1(i)
      enddo
    endif
  endif
  
else !___ no Menke

  if (do_SofQ==1) then
    IF (outall==1) then
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i)/divifac(i),Ic000(i),divifac(i),LPF(i),kos(i),aux4(i),aux2(i,:),&
                    fofa(i,:),tefa(i,:)
      enddo
    ELSE
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i)/divifac(i),Ic000(i),divifac(i)
      enddo
    endif
  else if (do_SofQ==0) then
    IF (outall==1) then
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i),Ic000(i),1,LPF(i),kos(i),aux4(i),aux2(i,:),fofa(i,:),tefa(i,:),fofa_inc(i,:)
      enddo
    ELSE
      do i=1,npq
        write(iuu,*)tt(i),q(i),Ic(i),Ic000(i),1
      enddo
    endif
  endif
  
endif
close(iuu)


  
print*, ' '
print*, '******* JOB PATTERN DONE! *******'

!if (.not.get_the_hkls) stop 'Finished (no HKL requested)'

 !  inp_hkl(1:14)='diffractor.inp'
!   ll=len_trim(inp_hkl)
!   call hkl_gen(inp_hkl(1:ll))

end program no_woman_no_cry
