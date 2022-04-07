program OutCenter
use nano_deftyp
implicit real(DP)(a-h,o-z),integer(I4B)(i-n)
real(DP) :: tr0(3)
integer(I4B) :: mx(3,3),ide(3,3)
character(len=99) :: sgfn,rl
character(len=10) :: sgsy

ide=reshape([1,0,0,0,1,0,0,0,1],[3,3])

iu=find_unit()
open(iu,status='old',action='read',file='Index_SG.txt')
iu2=find_unit()
open(iu2,status='replace',file='SG_Centering.txt')
indg0=0
do
  read(iu,'(a)',iostat=io) sgfn
  if (io/=0) exit
  sgfn=trim(adjustl(sgfn))
  lsgfn=len_trim(sgfn)
  if (lsgfn==0) cycle
  read(sgfn(15:17),*) indg
!  if (indg==indg0) cycle
  indg0=indg
  iug=find_unit()
  open(iug,status='old',action='read',file=trim(sgfn))
  read(iug,'(a)')rl
  rl=trim(adjustl(rl(7:)))
  ll=len_trim(rl)
  ibr=index(rl(1:ll),'(')-2
  isp=index(rl(1:ll),' ')+1
  sgsy=''
  sgsy=trim(adjustl(rl(isp:ibr)))
  read(iug,*)ng,nsd
  kt=0
  do ig=1,ng
    read(iug,*)mx,tr0
    ii=maxval(abs(mx-ide))
    if (ii/=0) cycle
    kt=kt+1
  enddo
  close(iug)
  write(iu2,'(2i6,3x,a10,3x,a)')indg,kt,sgsy,trim(sgfn)
enddo


end program OutCenter