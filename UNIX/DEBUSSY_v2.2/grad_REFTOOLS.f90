
module calc_grad

use nano_deftyp
use HELPINPUT
use INPUT_DATA

contains

subroutine grad_gen(STR_1,STR_i,STR_C,avLN)
implicit none

character(500)           :: pwd,rline,path_db
character(256)           :: nodfile,inifil,file_out
character(20)            :: title, cellname(2)
character(2),allocatable :: symb_SPH(:),symb_k(:),symb(:),symb_s(:)
character(3)             :: k_core_out,  k_shell_out, ff3,k_core_in,k_shell_in
character(4)             :: pearsy
real(DP),intent(IN)      :: STR_1, STR_i,STR_C,avLN
real(CP),allocatable     :: dist(:), coord_orth_k(:,:), okk_k(:)
real(CP),allocatable     :: nodes_SPH(:,:),okkN_SPH(:),cell(:),coord_node(:,:), tmp(:,:), t(:)
real(CP),allocatable     ::  coord(:,:), okk(:), coord_orth_s(:,:), okk_s(:)
real(CP)                 :: xmax,xmin,ymax,ymin,zmax,zmin,xmean,ymean,zmean,radii,actual_diam,s
real(CP)                 :: a,b,c,alg,beg,gag,al,be,ga,ss,ave, aa(3)
real(CP)                 :: cell_volume_red,cell_volume,ck(3), csh(3),a0,a1,scale
integer(I4B)             :: ll,iux,lpwd, lc(2),lout,j,iunod,ilogout, nnod,lnod,l,iost,ierr, i,nstr=1,ind,lp
integer(I4B)             :: kk,jj,nspx_k,nspx_s,nspgroup,ncenters_cell,n1,n2,iogr,iugr,lpearsy, NCL2MK(2),natc(2)

a0=STR_1/100.0d0 ; a1=STR_i/100.0d0; ss=STR_C/100.d0

 print*, '********TEST', 'a0 =', a0, 'a1=', a1,  ' s=', ss, 'avLN =', avLN

call GET_PWD(pwd=pwd,lpwd=lpwd)


  ind=index(PATH_NAME(nstr), 'DISTANCES')
  path_db=PATH_NAME(nstr)(1:ind-1)
  path_db=trim(adjustl(path_db))
  lp=len_trim(path_db)


inifil='coshmkQ.ini'

  call system ('cp '//path_db(1:lp)//inifil//'  . > tmp.out 2> tmp.err')

iux=FIND_UNIT()


   open(iux,status='old',file= pwd(1:lpwd)//inifil ,action='read', iostat=ierr)
   IF (ierr /=0) THEN
         print*, ' Error opening INPUT file:  '//pwd(1:lpwd)//inifil
         STOP
     ENDIF
     
 READ_CSH: do
    read(unit = iux, fmt = '(a)') rline
         rline = trim(adjustl(rline))
         ll = len_trim(rline)
             if (ll == 0) cycle
             if  (rline(1:8) == '! N. at.') exit
        enddo READ_CSH
        
    read (iux, '(a)') rline
    read (iux, '(a)') cellname(1)
    cellname(1)=trim(adjustl(cellname(1)))
    lc(1)=len_trim(cellname(1))
    
    read (iux, '(a)') rline
    read (iux, '(a)') rline
    read (iux, '(a)') cellname(2)
    cellname(2)=trim(adjustl(cellname(2)))
    lc(2)=len_trim(cellname(2))
  close(iux) 
  
  
  NCL2MK(1)=n2read_ab(nstr,2)
  NCL2MK(2)=n2read_c(nstr,2)
    
if (verbose)  write(*,*) 'Cell names = ', cellname(1)(1:lc(1)),  cellname(2)(1:lc(2))
 write(*,*) 'Ncore= ', NCL2MK(1), ' Nshell= ', NCL2MK(2)
 write(*,*) 'a0 = ', a0, 'a1 = ',a1,' s=', ss

     open(iux,status='old',file=path_db(1:lp)//cellname(1)(1:lc(1)),action='read',iostat=iost)
     if (iost/=0) then
       print*, 'ERROR opening ', path_db(1:lp)//cellname(1)(1:lc(1))
     endif
   
    read(iux,*) pearsy,a,b,c,alg,beg,gag,nspx_k
     write(*,*) 'Cell parameters =',a,b,c

    close(iux)

 !! reading cel.orth files 
   open(iux,status='old',file=path_db(1:lp)//cellname(1)(1:lc(1))//'.xyz',action='read',iostat=iost)
     if (iost/=0) then
       print*, 'ERROR opening ', path_db(1:lp)//cellname(1)(1:lc(1))//'.xyz'
     endif
      read(iux,*) natc(1)
      read(iux,*) rline
      allocate(symb_k(1:natc(1)), coord_orth_k(1:3, 1:natc(1)), okk_k(1:natc(1)))
      do i=1,   natc(1)
        read(iux,*)  symb_k(i), coord_orth_k(1:3, i), okk_k(i)
        print*, 'coord orth K  ', symb_k(i), coord_orth_k(1:3, i), okk_k(i)
      enddo  
    close(iux)
    
    
      open(iux,status='old',file=path_db(1:lp)//cellname(2)(1:lc(2))//'.xyz',action='read',iostat=iost)
     if (iost/=0) then
       print*, 'ERROR opening ', path_db(1:lp)//cellname(2)(1:lc(2))//'.xyz'
     endif
      read(iux,*) natc(2)
      read(iux,*) rline
      allocate(symb_s(1:natc(2)), coord_orth_s(1:3, 1:natc(2)), okk_s(1:natc(2)))
       do i=1,   natc(2)
        read(iux,*)  symb_s(i), coord_orth_s(1:3, i), okk_s(i)
         print*, 'coord orth S  ',  symb_s(i), coord_orth_s(1:3, i), okk_s(i)
      enddo  
    close(iux)
      
      
      

     ave=avLN*ten
     scale=ave/ss

 !! ratio between avLN and s should be keep constant along the DB 
 !! in order to keep constant the ratio between the core and the expanded shell    



do kk  = 1, NCL2MK(1)
   do jj  = 1, NCL2MK(2)   
   
  if ((kk-1)==0 .or. (jj-1)==0) then 
     cycle 
  else
   

  write(k_core_in, '(i3.3)') kk  !num_shell_max core inp
  write(k_core_out, '(i3.3)') (kk -1)  !num_shell_max core out
  
  write(k_shell_in, '(i3.3)') jj !num_shell_max shell inp
  write(k_shell_out, '(i3.3)') (jj-1) !num_shell_max shell out
  
  write (ff3, '(i3.3)')(kk-1)+(jj-1)

  write(*,'(a)') '   NCL2MK   core    '//k_core_in(1:3)//' '//k_core_out(1:3)
  write(*,'(a)') '   NCL2MK   shell  '//k_shell_in(1:3)//' '//k_shell_out(1:3)

 nodfile=cellname(1)(1:lc(1)-4)//'_'//cellname(2)(1:lc(2)-4)//'_LAT_k'//k_core_in(1:3)//'_s'//k_shell_in(1:3)//'.xyz'
 nodfile= trim(adjustl(nodfile))
 lnod= len_trim(nodfile)
   
file_out=cellname(1)(1:lc(1)-4)//'_'//cellname(2)(1:lc(2)-4)//'_GRAD__k'//k_core_out(1:3)//'_s'//k_shell_out(1:3)//'.xyz'
file_out=trim(adjustl(file_out))
lout=len_trim(file_out)
     
 write(*,'(a)') '       Input file:       '//nodfile(1:lnod)
 write(*,'(a)') '       Output file:      '//file_out(1:lout)
  

   call system ('cp '//path_db(1:lp)//'XYZ'//separator//nodfile(1:lnod)//'  . > tmp.out 2> tmp.err')
   

 iunod=FIND_UNIT()

open(iunod,status='old',file=pwd(1:lpwd)//nodfile(1:lnod),action='read',iostat=iost)
   if (iost/=0) then
   print*, 'ERROR opening ', pwd(1:lpwd)//nodfile(1:lnod)
   endif
 
 read(iunod,*) nnod
 read(iunod,'(a)') title


  allocate(nodes_SPH(1:3,nnod),symb_SPH(1:nnod),okkN_SPH(nnod))


l = 0
 
   do i = 1,nnod
           read(iunod,*) symb_SPH(i), nodes_SPH(1:3,i),okkN_SPH(i)
    enddo
    close(iunod)  

     xmax = maxval(nodes_SPH(1,:))
    xmin = minval(nodes_SPH(1,:))
 
     ymax = maxval(nodes_SPH(2,:))
    ymin = minval(nodes_SPH(2,:))
 
    zmax = maxval(nodes_SPH(3,:))
    zmin = minval(nodes_SPH(3,:))
    
    xmean = (xmax+xmin)/2
    ymean = (ymax+ymin)/2
    zmean = (zmax+zmin)/2
     
     
  allocate(dist(1:nnod))
 do j = 1,nnod
   dist(j) = sqrt((nodes_SPH(1,j)-xmean)**2+(nodes_SPH(2,j)-ymean)**2+(nodes_SPH(3,j)-zmean)**2)
 enddo

actual_diam=maxval(dist)
actual_diam=actual_diam
s=actual_diam/scale

write(*,*) 'Actual Diam = ', actual_diam*two, 'Å ', '  s size dep = ', s


     allocate(coord_node(1:3,nnod), cell(1:nnod))

 open(iux,status='replace',file=pwd(1:lpwd)//'size_dep_law_N'//ff3(1:3)//'.txt',action='readwrite',iostat=iost)
   if (iost/=0) then
     print*, 'ERROR opening ', pwd(1:lpwd)//cellname(1)(1:lc(1))
   endif 

 do j = 1,nnod


   cell(j)=a1-(a1-a0)*two/pi*ATAN(((actual_diam)-dist(j))/s)
   ! print*, dist(j), cell(j)
    coord_node(1, j)=(nodes_SPH(1,j)/a)*cell(j)
    coord_node(2, j)=(nodes_SPH(2,j)/b)*cell(j)
    coord_node(3, j)=(nodes_SPH(3,j)/c)*cell(j) 
    
  !  print*, 'O', coord_node(1:3, j) 
  !  print*, 'N', nodes_SPH(1:3, j)  
     
 enddo
 
 
 !! ordering of the cell parameters for printing the size dep law
  
 allocate(tmp(1:2, 1:nnod))
  do j = 1,nnod
     tmp(1, j) =   dist(j)
     tmp(2, j) =   cell(j)
  enddo
  
  allocate(t(1:2))
 do i = nnod-1, 1, -1
   do j = 1, i
         if (tmp(1,j) > tmp(1,j+1)) then
            t(1:2) = tmp(1:2,j)
            tmp(1:2,j)=tmp(1:2,j+1)
            tmp(1:2,j+1)=t(1:2)
          endif
     enddo
   enddo     
  
 deallocate(t)

 
 do j = 1,nnod
     write(iux,*) tmp(1:2, j)
 enddo
deallocate (tmp)

allocate(symb (1:(nnod*natc(1))), coord(1:3, 1:(nnod*natc(1))), okk (1:(nnod*(natc(1)))))

ll=0
   do  i = 1, nnod
    if  ( symb_SPH(i) == 'Al' .or. symb_SPH(i) == 'Cl') then 
     do j=1,natc(1) 
       ll=ll+1
       symb(ll) = symb_k(j)
       aa(1)=(coord_orth_k(1,j)/a)*cell(i)
       aa(2)=(coord_orth_k(2,j)/b)*cell(i)
       aa(3)=(coord_orth_k(3,j)/c)*cell(i) 
       coord(1:3, ll) = coord_node(1:3,i) + aa(1:3)
       okk(ll) = okkN_SPH(i)*okk_k(j)
     enddo
    else 
     if  ( symb_SPH(i) == 'Si' .or. symb_SPH(i) == 'Np') then 
     do j=1,natc(2) 
       ll=ll+1
       symb(ll) = symb_s(j)
       aa(1)=(coord_orth_s(1,j)/a)*cell(i)
       aa(2)=(coord_orth_s(2,j)/b)*cell(i)
       aa(3)=(coord_orth_s(3,j)/c)*cell(i) 
       coord(1:3, ll) = coord_node(1:3,i) + aa(1:3)
       okk(ll) = okkN_SPH(i)*okk_s(j)
     enddo
     endif
    endif 
   enddo
   
   
 if (ll /= (nnod*natc(1))) then 
    print*, 'ERRROR!!', ll, (nnod*natc(1))
    STOP
 endif   

   ilogout=FIND_UNIT()

open(ilogout,status='replace',file=pwd(1:lpwd)//file_out(1:lout),action='readwrite',iostat=iost)
   if (iost/=0) then
   print*, 'ERROR opening ', pwd(1:lpwd)//file_out(1:lout)
   endif
 
    
 write(ilogout,*) ll
 write(ilogout,'(a)') title
 
   do  i = 1, ll
     write(ilogout,'(a2,4f13.6)') symb(i), coord(1:3,i), okk(i)
   enddo
 
 
 close(iux)
 close(ilogout)
 

  deallocate(nodes_SPH,symb_SPH,okkN_SPH,cell,dist,coord_node,okk,coord, symb )

  call system ('rm '//nodfile(1:lnod)) 
  
   endif
 enddo
enddo
  call system ('rm *pat > tmp.out 2> tmp.err')
  deallocate (symb_k, coord_orth_k, okk_k, symb_s, coord_orth_s, okk_s)

end subroutine grad_gen
end module calc_grad
