program REF_EPDL97
implicit real(8)(a-h,o-z),integer(4)(i-n)
integer(4),parameter :: n_at=100
integer(4) :: l2l(3),l2la(3),lfn(4),kv(n_at,4)=0,donei(4)
character(len=80),dimension(3) :: r2l
character(len=80),dimension(4) :: fn
real(8) :: factx(4)=[2.d-8,2.d-8,1.d3,1.d3]
logical :: cond,cond1

fn(1)='BYAT/Z_000_F_coh_EPDL97.dat'
fn(2)='BYAT/Z_000_S_incoh_EPDL97.dat'
fn(3)='BYAT/Z_000_Fdpr_anom_EPDL97.dat'
fn(4)='BYAT/Z_000_Fpr_anom_EPDL97.dat'
do j=1,4
  lfn(j)=len_trim(trim(adjustl(fn(j)(:))))
enddo
iwz=index(fn(1)(1:lfn(1)),'000')

open(1,status='old',action='read',file='epdl97-1.all.txt')
cond=.false.
ZLOOP:do kz=1,n_at
  donei=0
  do j=1,4
    write(fn(j)(iwz:iwz+2),'(i3.3)')kz
    open(10+j,status='replace',file=fn(j)(1:lfn(j)))
  enddo
  kz000=kz*1000
  do
    read(1,'(a)')r2l(1)(:)
    r2l(1)(:)=trim(r2l(1)(:))
    l2l(1) = len_trim(r2l(1)(:))
    l2la(1) = len_trim(trim(adjustl(r2l(1)(:))))
    if (l2l(1)==72 .and. (l2la(1)==1 .and. trim(adjustl(r2l(1)(:)))=='1')) then
      cond=.false.
      if (all(donei==1)) exit
      do ine=2,3
        read(1,'(a)')r2l(ine)(:)
        r2l(ine)(:)=trim(r2l(ine)(:))
        l2l(ine) = len_trim(r2l(ine)(:))
        r2l(ine)(:)=trim(adjustl(r2l(ine)(:)))
        l2la(ine) = len_trim(r2l(ine)(:))
      enddo
      ii=-999
      irz=index(r2l(2)(1:l2la(2)),' ')
      read(r2l(2)(1:irz-1),*,iostat=ioi)ii
      ii2=-999
      irz=index(r2l(3)(1:l2la(3)),' ')
      read(r2l(3)(1:irz-1),*,iostat=ioi2)ii2
      cond1=(ioi==0.and.ioi2==0)
      cond=.false.
      cond = (cond1.and.((ii==kz000).and.(ii2>=93941.and.ii2<=93944)))
      if (cond) then
        icond=ii2-93940
        donei(icond)=1
      endif
    else
      if (cond) then
        read(r2l(1)(1:l2l(1)),'(2e11.4)')x,y
        write(icond+10,*)factx(icond)*x,y
        kv(kz,icond)=kv(kz,icond)+1
      else
        cycle
      endif
    endif
  enddo
  
  do j=1,4
    close(10+j)
  enddo
enddo ZLOOP
close(1)

open(1,status='replace',file='EPDL97_NV.dat')
do kz=1,n_at
  write(1,'(i4,4i8)')kz,kv(kz,:)
enddo
close(1)

end program REF_EPDL97