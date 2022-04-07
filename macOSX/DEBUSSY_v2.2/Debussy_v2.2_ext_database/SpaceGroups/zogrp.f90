module grps
use nano_deftyp
private
public :: primat,outgen, N_SPG_MATRX, N_SPG_VECT

integer(I4B),parameter :: nmsg=14,ntsg=11
integer(I4B),parameter :: N_SPG_MATRX = nmsg, N_SPG_VECT = ntsg
character(len=nmsg),parameter :: sgmlett='abcdefghijklmn'
character(len=ntsg),parameter :: sgtlett='ABCDEFGOXYZ'

integer(I4B),parameter :: mg14(3,3,nmsg)=reshape([ 1, 0, 0, 0, 1, 0, 0, 0, 1, &
                                                  -1, 0, 0, 0,-1, 0, 0, 0, 1, &
                                                  -1, 0, 0, 0, 1, 0, 0, 0,-1, &
                                                   0, 1, 0, 0, 0, 1, 1, 0, 0, &
                                                   0, 1, 0, 1, 0, 0, 0, 0,-1, &
                                                   0,-1, 0,-1, 0, 0, 0, 0,-1, &
                                                   0, 1, 0,-1, 0, 0, 0, 0, 1, &
                                                  -1, 0, 0, 0,-1, 0, 0, 0,-1, &
                                                   1, 0, 0, 0, 1, 0, 0, 0,-1, &
                                                   1, 0, 0, 0,-1, 0, 0, 0, 1, &
                                                   0,-1, 0,-1, 0, 0, 0, 0, 1, &
                                                   0, 1, 0, 1, 0, 0, 0, 0, 1, &
                                                   0,-1, 0, 1, 0, 0, 0, 0,-1, &
                                                   0, 1, 0,-1,-1, 0, 0, 0, 1], [3,3,nmsg])
real(DP),dimension(ntsg),parameter :: tg11=[unses, unqua, unter, half, duter, one-unqua, &
                                            five*unses, zero, -three/eight, -unqua, -half*unqua]

contains
!******************************************************
subroutine primat
implicit none
integer(I4B) :: i,j

do i=1,nmsg
  print*,'  ***  ',i,'  ***'
  do j=1,3
    print'(3i6)',mg14(j,:,i)
  print*,' '
  enddo
enddo
do i=1,ntsg
  print*,i,tg11(i)
enddo
end subroutine primat
!******************************************************
subroutine wmvfgen(iu,mm,tt,iperm,orig,mperm)
implicit none
integer(I4B),intent(IN) :: iu,mm(3,3),iperm(3),mperm(3,3)
real(DP),intent(IN) :: tt(3),orig(3)
real(DP) :: aux3(3)
integer(I4B) :: i,j,mmm(3,3)

!write(iu,*)iperm
mmm = matmul(mperm,matmul(mm,transpose(mperm)))
do j=1,3
  write(iu,'(3i6)') mmm(j,:)
enddo
aux3 = tt-(matmul(mm,orig)-orig)
where (aux3<=-0.001d0) aux3=aux3+one
where (aux3>= 0.999d0) aux3=aux3-one
write(iu,'(3(1x,f15.8))')aux3(iperm(1:3))
end subroutine wmvfgen
!******************************************************
function vec3r(a3)
implicit none
character(len=3),intent(IN) :: a3
real(DP) :: vec3r(3)
integer(I4B) :: i,j

vec3r=zero
do j=1,3
  do i=1,ntsg
    if (sgtlett(i:i)==a3(j:j)) then
      vec3r(j)=tg11(i)
      exit
    endif
  enddo
enddo

end function vec3r
!******************************************************
function matindx(a1)
implicit none
character(len=1),intent(IN) :: a1
integer(I4B) :: matindx
integer(I4B) :: i

matindx=0
do i=1,nmsg
  if (sgmlett(i:i)==a1(1:1)) then
    matindx=i
    exit
  endif
enddo

end function matindx
!******************************************************
subroutine OUTGEN
implicit none
integer(I4B) :: i,j,iu0,iuo,ip1,ip2,ll,lfng,lgen,lnamgr,iinv,ngen,ngen1,&
                imat,iii,ior,iax,Norig,Nuniq
logical :: twoax,twoor
character(len=16) :: fng
character(len=3) :: Xtra
character(len=113) :: rl,genstr,namgr
real(DP) :: ori(3,2),trx(3)
integer(I4B) :: perm(3,2),mperm(3,3,2)

perm(:,1)=[1,2,3]
perm(:,2)=[3,1,2]
ori(:,1)=[zero,zero,zero]
mperm=reshape([1, 0,0,0, 1, 0,0,0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0], [3,3,2])


twoax=.false.; twoor=.false.
iu0=find_unit()
open(iu0,status='old',action='read',file='spgsymgen.txt')
do i=1,230
  read(iu0,'(a)') rl
  rl=trim(adjustl(rl))
  ll=len_trim(rl)
  twoax=(i>=3.and.i<=15)
  Xtra=''
  Nuniq=1
  Norig=1
  if (twoax) then 
    Xtra='_Ub'
    Nuniq=2
  endif
  ip1=INDEX(rl(1:ll),'--')+3
  ip2=INDEX(rl(1:ll),')')+2
  genstr=''
  genstr=trim(adjustl(rl(ip2:ll))); lgen=len_trim(genstr)
  twoor=(.not.(genstr(lgen:lgen)=='0'))
  if (twoor) then 
    Xtra='_o1'
    Norig=2
    ori(:,2) = vec3r(a3=genstr(lgen-2:lgen))
  endif
  
  do ior=1,Norig
    if (ior==2) Xtra(3:3)='2'
    do iax=1,Nuniq
      if (iax==2) Xtra(3:3)='c'
      write(fng,'("SG_Nr_",i3.3,a,".gen")') i,trim(Xtra)
      lfng=len_trim(fng)
      namgr=''
      namgr(1:lfng-4)=fng(1:lfng-4)
      namgr(lfng-4+1:lfng-4+1+(ip2-2)-ip1+1) = ' '//rl(ip1:ip2-2)
      lnamgr=lfng-4+1+(ip2-2)-ip1+1
      iuo=find_unit()
      open(iuo,status='replace',file=fng)
      write(iuo,'(a)')'Space '//namgr(1:lnamgr)
      read(genstr(1:2),'(2i1)')iinv,ngen
      ngen1=ngen+iinv
      write(iuo,'(2i6)')3,ngen1
!      write(iuo,*)' M ',8
      if (iinv==1) then
        call wmvfgen(iu=iuo,mm=mg14(:,:,8),tt=[zero,zero,zero],&
                     iperm=perm(:,iax),orig=ori(:,ior),mperm=mperm(:,:,iax))
      endif
      do j=1,ngen
        iii=2+(j-1)*4+1
        imat=matindx(a1=genstr(iii:iii))
!      write(iuo,*)' M ',imat,iii,genstr(iii:iii)
        trx=vec3r(a3=genstr(iii+1:iii+3))
        call wmvfgen(iu=iuo,mm=mg14(:,:,imat),tt=trx,iperm=perm(:,iax),orig=ori(:,ior), &
                     mperm=mperm(:,:,iax))
      enddo
      close(iuo)
    enddo
  enddo
enddo
close(iu0)
end subroutine OUTGEN

end module grps
program ugo
use grps
!call primat
call outgen
end 