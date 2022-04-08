program ckggg
implicit real(8)(a-h,o-z),integer(4)(i-n)
integer(4) :: mg(3,3,192,2),mr(3,3)
real(8)    :: tg(3,192,2),tr(3)
character(88),dimension(2) :: ng1,ng2


open(1,status='old',action='read',file='tock1.in')
read(1,*) ngrps1
ng1='';ng2=''
do i=1,ngrps1
  read(1,'(a)')ng1(i)(:)
enddo
read(1,*) ngrps2
do i=1,ngrps2
  read(1,'(a)')ng2(i)(:)
enddo
close(1)

do ig1=1,ngrps1
  do ig2=1,ngrps2
    open(11,status='old',action='read',file=trim(ng1(ig1)(:)))
    open(12,status='old',action='read',file=trim(ng2(ig2)(:)))
    read(11,*);read(12,*)
    read(11,*)ne1,id1;read(12,*)ne2,id2
    if (id1/=id2) then
      print*,trim(ng1(ig1)(:)),' /= ',trim(ng2(ig2)(:)),' by dim'
      close(11)
      close(12)
      cycle
    endif
    do ie1=1,ne1
      read(11,*)mr,tr
      mg(:,:,ie1,1) = transpose(mr)
      tg(:,ie1,1)=tr
      read(12,*)mr,tr
      mg(:,:,ie1,2) = transpose(mr)
      tg(:,ie1,2)=tr
    enddo
    close(11)
    close(12)
    iseq=1
    do ie1=1,ne1
      mr=mg(:,:,ie1,1)
      tr=tg(:,ie1,1)
      isin=0
      do ie2=1,ne1
        iii=maxval(abs(mr-mg(:,:,ie2,2)))
        if (iii>0) cycle
        ddd=maxval(abs(modulo(0.001d0+tr-tg(:,ie2,2),1.d0)-0.001d0))
        if (ddd>0.001d0) cycle
        isin=1
        exit
      enddo
      if (isin==0) then
        iseq=0
        print*, 'Not found ', ie1
        exit
      endif
    enddo
    if (iseq==0) then
      print*,trim(ng1(ig1)(:)),' /= ',trim(ng2(ig2)(:))
    else
      print*,trim(ng1(ig1)(:)),' == ',trim(ng2(ig2)(:))
    endif
  enddo
enddo







end