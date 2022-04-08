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
program mkpha

use HELPINPUT

implicit none 
character(512) :: file_inp, outpath,line_inp, line_path, inpath, file_out, rline_log
character(512) :: phase_name,phase_name_blank,rline,wyck, a_char,b_char, z_form,&                                                
                  c_car, alpha_car,beta_car,gamma_car, gamma_par, sym_line,&
                  sg_symb,lat_type, trig_sg, cell_line, file_log,linea, symm,&
                  atom_label,angle_line, angle,xyz,x,dw, b,U_line,axes,occ,type_line, &
                  b_line,atom,atom_lb,y,z,z_atom,sof,coord,u,occ_blank,occ_line,&
                  Uiso,spgr_line,spgr_type, biso_char, input, cif_name
real(4) ::  Uiso_real, sof_real, Biso
real(4), dimension(6) :: cell
real(4), parameter :: unter_SP = 0.3333,duter_SP=0.6666
real(8), parameter :: unter_DP = 0.333333333333333333333333333333333333, &
                      duter_DP=0.666666666666666666666666666666666667
real(8), parameter :: eight_pisqa  = 8*pisqa
character(5)  :: kw
character(2) :: al, label
character(1) :: aa,ab,j,k,cc,cd,ce
real(4), dimension(3) :: coord_real
character(50),dimension(3) :: fract, par, par_blank
character(50),dimension(6) :: cella, cell_par, cell_par_blank
character(1),dimension(10) :: nmbrs 
data nmbrs/'0','1','2','3','4','5','6','7','8','9'/
character(1),dimension(26) :: chara 
data chara/'a','b','c','d','e','f','g','h','i',&
           'j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'/
character(1),dimension(26):: elem 
data elem/'A','B','C','D','E','F','G','H','I',&
                  'J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
integer :: lc_sg,ind2,lc_sym,lcell,laxe,lap,lbp, lcp,lang,lalp, lbet, lgam,lsg, mult,&
       l_sym, lac, lbc, lcc,lcalpha, lcbeta, lcgamma,iloginp,&
       lat,lln,llchara,i,ilogout,ind4,ind5,ind5_bis,ind6,nn,lb, &
       ind7,ind7_bis,ind8,ind9,lU_line,ch, N_flag,lxyz,b_count, n_b,& 
       linee,fract_pos,hyd_pos,symm_pos,therm_pos,wyck_pos,mass,loc,ind10_bis,&
       ind6_bis,lxx,lx,l_y,lyy,lz,lzz,fract_pos_y,lp,lpou,lfout,B_pos, l_anis, &
       lb_line,lzat,ind10,l_z,lcoord,calc_flag_pos,locc,ind11,fract_pos_z,   scratch2,&
       N,ind,rind,iost,u_pos,llb,ind1,ind3,fract_pos_x,nat, sel,dat,lspg,lspgrt, n_anis, &
       occ_pos,n_el,ll,label_pos,type_pos,ind9_bis,lu,ind_bis, lx_coord, z_int, scratch1,&
       logfile, lUanis, llU, lU11, lU22, lU33, Zatom, mx, Zline, Nsp, lpth,ls
character(50) :: title
character(50) :: cella2 
character(6) :: space 
character(1) :: b1
character(2), dimension(100) :: sym 
data sym/'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne','Na',&
                'Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co',&
                'Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr', 'Rb','Sr','Y ','Zr','Nb','Mo','Tc',&
                'Ru','Rh','Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr',&
                'Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W ','Re',&
                'Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa',&
                'U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm'/
real(4),dimension(100) :: ch2 = (/(i, i = 1, 100)/)
real(4),allocatable,dimension(:,:) :: cord_inp
real(4),allocatable,dimension(:,:) :: cord_out
real(4),allocatable,dimension(:) :: x_cord, y_cord, z_cord, b_cord,sof_cord, atoN, Natom
character(2),dimension(:),allocatable :: label2, label3
character(5),allocatable,dimension(:) :: cord2
character(5) :: cord
real(4), dimension(:), allocatable :: xam 
integer, dimension(:), allocatable :: spec
character(15) :: sg, sgS
real(4), allocatable, dimension(:) :: Uiso_calc, Biso_calc
real(4), dimension(:,:), allocatable :: U_real
character(50) :: Uanis_line_blank, U22_line, Uanis, Uanis_blank, U33_line
character(10), dimension(3) :: U_char 
character(3) :: flag


 linee = 2 
 label_pos = 0
 type_pos = 0
 fract_pos_x = 0
 fract_pos_y = 0
 fract_pos_z = 0
 symm_pos = 0
 wyck_pos = 0
 occ_pos = 0
 u_pos = 0
 therm_pos = 0
 hyd_pos = 0
 b_pos = 0
 l_anis = 0
 b_count = 0
 n_b = 0



CALL GET_COMMAND(line_inp)
line_inp = trim(adjustl(adjustr(line_inp)))

ll = len_trim(line_inp)

!ind = index(line_inp(1:ll),' ',.true.)
!flag = line_inp(ind+1:ll)
!flag = trim(adjustl(flag))
!print*, 'flag=', flag
ind1= index(line_inp(1:ll),' ')
line_path = line_inp(ind1+1:ll)
line_path = trim(adjustl(line_path))
!print*, 'line_path=', line_path
lpth = len_trim(line_path)

ind2 = index(line_path(1:lpth),'.cif')

read(line_path(1:ind2-1),'(a)') file_inp
file_inp = trim(adjustl(file_inp))
inpath =  trim(adjustl(adjustr(file_inp)))//'.cif'
!print*,'input=',  inpath

read(line_path(ind2+5:lpth), '(a)') outpath
outpath = trim(adjustl(outpath))
!print*, 'out=', outpath
lp = len_trim(file_inp)
 !!scan file name, take from sep to '.'
 !rind=index(file_inp(1:lp), '\',.true.)
 !print*,'rind ',rind
 ind = index(file_inp(1:lp),separator,.true.)
 cif_name = file_inp(ind+1:lp)

! print*,'Compound type'
! read*, flag

input = trim(adjustl(adjustr(cif_name)))//'.cif'
!  print*, 'input ='//input

!file_log = trim(adjustl(adjustr(cif_name)))//'.log'
ind= INDEX(outpath(1:lpth),'.pha')
file_log = outpath(1:ind-1)
file_log = (trim(adjustl(adjustr(file_log))))//'.log'
 ! print*, 'logfile= ',file_log

logfile = FIND_UNIT()
open(unit = logfile, status = 'replace', file = file_log, access ='sequential',  action = 'readwrite')



iloginp = FIND_UNIT()

 open(unit = iloginp, status = 'old', file = inpath, form = 'formatted',&
              access = 'sequential', action = 'read', iostat = iost)
  if (iost /= 0) then
  write(logfile,*) ' Error opening file '//input//'!'
  STOP   
 endif


scratch1 = FIND_UNIT()

open(unit = scratch1, status = 'scratch', access ='sequential', action = 'readwrite')

 READ_IN: do
    read(iloginp,'(a)', end = 10) rline
      rline = trim(adjustl(rline))
     ll = len_trim(rline)
    if (rline(1:1) == '#'.or.ll == 0) cycle READ_IN
     kw = rline(1:5)

 task1_info: select case(kw)
       case ('data_') task1_info
           ind = index(rline(1:ll),'_')
               read(rline(ind+1:ll),'(a)') phase_name_blank
               if (len_trim(phase_name_blank) == 0) then
                  phase_name = 'test_cif'
               else
               phase_name = trim(adjustl(phase_name_blank))
               endif

   case ('_cell') task1_info
      read(rline(7:ll), '(a)') cell_line
        cell_line = trim(adjustl(cell_line))
          lcell = len_trim(cell_line)

   task4_info: select case (cell_line(1:3))
              case ('len') task4_info
               ind2 = index(cell_line(1:lcell),'_')
                axes = cell_line(ind2+1:lcell)
                axes = trim(adjustl(axes)) 
               laxe = len_trim(axes)

      task2_info: select case (axes(1:1))
        case('a') task2_info
           ind4 = index(axes(1:laxe),' ')
            cell_par_blank(1) = axes(ind4+1:laxe)
             cell_par(1) = trim(adjustl(cell_par_blank(1)))
               lap = len_trim(cell_par(1))
               ind5 = index(cell_par(1)(1:lap),'(')
              if (ind5 == 0) then
               cella(1) = trim(adjustl(cell_par(1)))
               else
              cella(1) = cell_par(1)(1:ind5-1)
               endif
            read(cella(1),*) cell(1)
    
      case('b') task2_info
           ind4 = index(axes(1:laxe),' ')
            cell_par_blank(2) = axes(ind4+1:laxe)
             cell_par(2) = trim(adjustl(cell_par_blank(2)))
               lbp = len_trim(cell_par(2))
               ind5 = index(cell_par(2)(1:lbp),'(')
              if (ind5 == 0) then
               cella(2) = trim(adjustl(cell_par(2)))
               else
              cella(2) = cell_par(2)(1:ind5-1)
               endif
            read(cella(2),*) cell(2)
      
          case('c') task2_info
           ind4 = index(axes(1:laxe),' ')
            cell_par_blank(3) = axes(ind4+1:laxe)
             cell_par(3) = trim(adjustl(cell_par_blank(3)))
               lcp = len_trim(cell_par(3))
               ind5 = index(cell_par(3)(1:lcp),'(')
              if (ind5 == 0) then
               cella(3) = trim(adjustl(cell_par(3)))
               else
              cella(3) = cell_par(3)(1:ind5-1)
               endif
           read(cella(3),*) cell(3)
      end select task2_info

      case ('ang') task4_info
               ind2 = index(cell_line(1:lcell),'_')
                angle = cell_line(ind2+1:lcell)
                angle = trim(adjustl(angle))
               lang = len_trim(angle)

         task5_info: select case (angle(1:4))
           case('alph') task5_info
             ind4 = index(angle(1:lang),' ')
              cell_par_blank(4) = angle(ind4+1:lang)
               cell_par(4) = trim(adjustl(cell_par_blank(4)))
               lalp = len_trim(cell_par(4))
               ind5 = index(cell_par(4)(1:lalp),'(')
              if (ind5 == 0) then
               cella(4) = trim(adjustl(cell_par(4)))
               else
               cella(4) = cell_par(4)(1:ind5-1)
               endif
             read(cella(4),*) cell(4)

         case('beta') task5_info
             ind4 = index(angle(1:lang),' ')
              cell_par_blank(5) = angle(ind4+1:lang)
               cell_par(5) = trim(adjustl(cell_par_blank(5)))
               lbet = len_trim(cell_par(5))
               ind5 = index(cell_par(5)(1:lbet),'(')
              if (ind5 == 0) then
               cella(5) = trim(adjustl(cell_par(5)))
               else
              cella(5) = cell_par(5)(1:ind5-1)
               endif
            read(cella(5), *) cell(5)

         case('gamm') task5_info
             ind4 = index(angle(1:lang),' ')
              cell_par_blank(6) = angle(ind4+1:lang)
               cell_par(6) = trim(adjustl(cell_par_blank(6)))
               lgam = len_trim(cell_par(6))
               ind5 = index(cell_par(6)(1:lgam),'(')
              if (ind5 == 0) then
               cella(6) = trim(adjustl(cell_par(6)))
               else
              cella(6) = cell_par(6)(1:ind5-1)
               endif
           read(cella(6),*) cell(6)
     
        end select task5_info


      end select task4_info

   case('_spac') task1_info
     spgr_line = rline(14:ll)
     spgr_line = trim(adjustl(spgr_line))
      lspg = len_trim(spgr_line)
       if (spgr_line(1:4) == 'name') then
        ind = index(spgr_line(1:lspg), '_')
         spgr_type = spgr_line(ind+1:lspg)
          spgr_type = trim(adjustl(spgr_type))
          lspgrt = len_trim(spgr_type)
           if (spgr_type(1:2) == 'H-') then
            ind1= index(spgr_type(1:ll), '''')
             sg_symb  = spgr_type(ind1+1:lspgrt)
              sg_symb = trim(adjustl(sg_symb))
               ind2 = index(sg_symb,'''')
                sg = sg_symb(1:ind2-1)
                 sg = trim(adjustl(sg))
                !  print*, sg
               lsg = len_trim(sg)
                ind3 = index (sg(1:lsg), ' ', .true.)
                  sgS = sg(ind3+1:lsg)
                !    print*, sgS
                   if (sgS == 'S') sg = sg(1:ind3-1)
                  

              ind3=index(sg(1:lsg),'R')
               if (ind3/=0) then
                 if (cell(4)/=90.0.or.cell(5)/=90.0.or.cell(6)/=120.0) then
                   write(logfile,*) 'WARNING, wrong setting in Rombohedral space group!'
                 endif
                  ind4=index(sg(1:lsg),'H') 
                   if (ind4/=0) sg=sg(1:ind4-1)
                     sg = trim(adjustl(sg))
                    if (verbose) print*, sg
                endif  
         if (sg == 'P n n n') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'P b a n') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'P m m n') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'C c c a') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'F d d d') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'P 4/n') write(logfile,*) 'Check origin setting in cif file!'
	 	 if (sg == 'P 42/n') write(logfile,*) 'Check origin setting in cif file!'
		 if (sg == 'P 4/n b m') write(logfile,*) 'Check origin setting in cif file!'
		 if (sg == 'P 4/n n c') write(logfile,*) 'Check origin setting in cif file!'
	     if (sg == 'P 4/n m m') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'P 4/n c c') write(logfile,*) 'Check origin setting in cif file!' 
          if (sg == 'P 42/n b c') write(logfile,*) 'Check origin setting in cif file!' 
		  if (sg == 'P 42/n n m') write(logfile,*) 'Check origin setting in cif file!' 
	 	  if (sg == 'P 42/n m c') write(logfile,*) 'Check origin setting in cif file!'
 		  if (sg == 'I 41/a m d') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'I 41/a c d') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'P n -3') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'F d -3') write(logfile,*) 'Check origin setting in cif file!'
          if (sg == 'P n -3 n') write(logfile,*) 'Check origin setting in cif file!'
          if (sg == 'P n -3 m') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'F d -3 m') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'F d -3 c') write(logfile,*) 'Check origin setting in cif file!'
             endif
             endif          


    case ('_symm') task1_info
     ind = index(rline,'_',.true.)
      sym_line = rline(ind+1:ll)
       sym_line = trim(adjustl(sym_line))  
        l_sym = len_trim(sym_line)
         if (sym_line(1:2) == 'H-') then 
         ind1 = index(sym_line(1:l_sym),'''')
          sg_symb = sym_line(ind1+1:l_sym)
           sg_symb = trim(adjustl(sg_symb))
            ind2 = index(sg_symb,'''')
             sg = sg_symb(1:ind2-1)
              sg = trim(adjustl(sg))
          if (verbose)   print*, sg
                lsg = len_trim(sg)
             ind3 = index (sg(1:lsg), ' ', .true.)
                  sgS = sg(ind3+1:lsg)
                 if (verbose) print*, sgS
                   if (sgS == 'S') sg = sg(1:ind3-1)

 
             ind3=index(sg(1:lsg),'R')
               if (ind3/=0) then
                 if (cell(4)/=90.0.or.cell(5)/=90.0.or.cell(6)/=120.0) then
                   write(logfile,*) 'WARNING, wrong setting in Rombohedral space group!'
                  endif
                  ind4=index(sg(1:lsg),'H') 
                   if (ind4/=0) sg=sg(1:ind4-1)
                     sg = trim(adjustl(sg))
                  ! print*, sg 
                 endif
     if (sg == 'P n n n') write(logfile,*) 'Check origin setting in cif file!'
     if (sg == 'P b a n') write(logfile,*) 'Check origin setting in cif file!'
     if (sg == 'P m m n') write(logfile,*) 'Check origin setting in cif file!'
     if (sg == 'C c c a') write(logfile,*) 'Check origin setting in cif file!'
     if (sg == 'F d d d') write(logfile,*) 'Check origin setting in cif file!'
     if (sg == 'P 4/n') write(logfile,*) 'Check origin setting in cif file!'
	  if (sg == 'P 42/n') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'P 4/n b m') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'P 4/n n c') write(logfile,*) 'Check origin setting in cif file!'
	          if (sg == 'P 4/n m m') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'P 4/n c c') write(logfile,*) 'Check origin setting in cif file!' 
     if (sg == 'P 42/n b c') write(logfile,*) 'Check origin setting in cif file!' 
		  if (sg == 'P 42/n n m') write(logfile,*) 'Check origin setting in cif file!' 
	 	  if (sg == 'P 42/n m c') write(logfile,*) 'Check origin setting in cif file!'
 		  if (sg == 'I 41/a m d') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'I 41/a c d') write(logfile,*) 'Check origin setting in cif file!'
         if (sg == 'P n -3') write(logfile,*) 'Check origin setting in cif file!'
		  if (sg == 'F d -3') write(logfile,*) 'Check origin setting in cif file!'
          if (sg == 'P n -3 n') write(logfile,*) 'Check origin setting in cif file!'
          if (sg == 'P n -3 m') write(logfile,*) 'Check origin setting in cif file!'
        if (sg == 'F d -3 m') write(logfile,*) 'Check origin setting in cif file!'
           if (sg == 'F d -3 c') write(logfile,*) 'Check origin setting in cif file!'
                 endif
  
  case default task1_info
     if (rline(1:ll)=='_atom_site_label') then 
     goto 11
    endif
   end select   task1_info 
enddo READ_IN 


11  READ_FLAG: do
      read(unit =iloginp, fmt ='(a)', end = 10) rline
        rline = trim(adjustl(rline))
   count_flag: select case(rline(12:13)) 
        case('la') count_flag
          label_pos = linee
          linee = linee +1
         !   print*, rline
        case('ty') count_flag
           type_pos = linee
           linee = linee +1
         !    print*, rline
        case('fr') count_flag
         if (rline(18:18) == 'x') then
          fract_pos_x = linee
          linee = linee +1
         !  print*, rline
         endif
         if (rline(18:18) == 'y') then
          fract_pos_y = linee
          linee = linee +1
         !   print*, rline
          endif
        if (rline(18:18) == 'z') then
          fract_pos_z = linee
          linee = linee +1
        !  print*, rline
        endif          
  
        case('sy') count_flag    
          symm_pos = linee
          linee = linee +1
      !  print*, rline
 
        case('Wy') count_flag 
          wyck_pos = linee
          linee = linee +1        
      !     print*, rline
       case('oc') count_flag
         occ_pos  =  linee 
         linee = linee +1
       !    print*, rline
       case ('U_') count_flag
        u_pos = linee
        linee = linee +1 
        !   print*, rline
        case ('th') count_flag
         therm_pos = linee
         linee = linee +1
         !  print*, rline    
       case ('at') count_flag           
        hyd_pos = linee
        linee = linee +1
          !  print*, rline
       case ('ca') count_flag 
        calc_flag_pos = linee
        linee = linee +1
         !  print*, rline
       case ('B_') count_flag
        B_pos = linee
        linee = linee +1
          ! print*, rline

      case default count_flag
       if (rline(1:5) /= '_atom') then
       goto 12
      endif
     end select count_flag
   enddo READ_FLAG

12      mass = linee-1
  !print*,'valore_max = ', mass    

  write(scratch1,'(a)') 'Title '//trim(adjustl(phase_name))
if (cell(1) /= 0) then
write(scratch1, '(a6,6f12.5)')'Cell  ',cell
else
write(logfile,*) 'ERROR in reading cell parameters in cif file!'
STOP
endif
write(scratch1,'(a)') 'Space '//trim(adjustl(sg))

backspace(iloginp)

  READ_ATOM:  do   
   read(iloginp,'(a)', end = 10) rline
    rline = trim(adjustl(rline))
    ll = len_trim(rline)
! print*, rline
!   if  (ll == 0.or.rline(1:1) =='#'.or.rline(1:1)== '_') cycle READ_ATOM
   if (rline(1:ll) == '_atom_site_aniso_U_23') then
    goto 40
  endif
  if (rline(1:5) == 'data_') then
    goto 10
  endif
 READ_ELEMENT: do ch = 1, 26
   if (rline(1:1) == elem(ch)) then 
    read(rline(1:ll), fmt = '(a)') atom
     atom = trim(adjustl(atom))
 !   print*, 'atom= '//atom
     lat = len_trim(atom)
      ind1 = index(atom,' ')
      if (type_pos == 0) then
         read(atom(1:ll), fmt = '(a)') atom_lb
  !  print*, 'atom_lb='//atom_lb
          
      else 
  
    read(atom(ind1+1:lat), fmt= '(a)') atom_lb
     atom_lb = trim(adjustl(atom_lb))
    
    endif
   atom_lb = trim(adjustl(atom_lb))
     llb = len_trim(atom_lb)
     al = atom_lb(1:2)
  !    print*, 'al ='//al
     aa = al(2:2)
  !   print*, 'al(2:2) ='//aa
   READ_NMBRS:  do i = 1, 9
     if (aa == nmbrs(i)) then
       j = ' '
      
      exit
      else
     j = al(2:2)
       
       endif
     enddo READ_NMBRS
    if (len_trim(al)==0) cycle READ_ELEMENT

 !print*, 'symb = '//al(1:1)//j

! se presente Wyckoff_symbol, salta
    if (symm_pos /= 0) then
     if (wyck_pos /= 0) then 
  !  print*, symm_pos, wyck_pos
     ind = index (atom_lb(1:llb),' ')
   !  print*,  'atom_lb='//atom_lb(ind+1:llb)
     read(atom_lb(ind+1:llb),'(a)') symm
     ls = len_trim(symm)
     symm = trim(adjustl(symm))
      read(symm(1:ls), *) mult
 !    print*, 'mult=',mult
     ind = index(symm(1:ls),' ')
     read(symm(ind+1:ls),'(a)') wyck
     wyck = trim(adjustl(wyck))
     ll = len_trim(wyck)
      ind = index(wyck(1:ll),' ')   
    read(wyck(ind+1:ll),'(a)') xyz
     xyz = trim(adjustl(xyz))
     lxyz = len_trim(xyz)
! print*, xyz
    
! USELESS...
!
!     READ_CHARA: do i = 1, 26
!  if (cc == chara(i).or.cd == chara(i).or.ce == chara(i)) then
!      ind2 = index(coord(1:lcoord),chara(i))
!         if (ind2 /= 0) then
!         goto 5
!        endif
!      endif
!       enddo READ_CHARA
!      if (cc == '?'.or.cd == '?'.or.ce == '?') then
!         ind2 = index(coord(1:lcoord),'?')
!           if (ind2 /= 0) then
!         goto 5
!        endif
!      endif
!
!5       xyz_blank = coord(ind2+1:lcoord)
!        xyz = trim(adjustl(xyz_blank))
!        lxyz = len_trim(xyz)
!    print*, 'xyz ='//xyz

   else
      if (wyck_pos == 0) then
        ind = index(atom_lb(1:llb), ' ')
        coord = atom_lb (ind+1:llb)
         coord = trim(adjustl(coord))

           lcoord = len_trim(coord)
         ind2= index(coord(1:lcoord),' ')
        xyz = coord(ind2+1:lcoord)
         xyz = trim(adjustl(xyz))
         lxyz = len_trim(xyz)
         
   if (verbose) print*, 'xyz ='//xyz
      endif
     endif
   endif
   
      if (symm_pos == 0) then 
  !  print*,'atom_lb ='//atom_lb
       ind2 = index(atom_lb(1:llb), ' ')
       xyz = atom_lb (ind2+1:llb) 
     
       xyz = trim(adjustl(xyz))
       lxyz = len_trim(xyz)
 ! print*, 'xyz ='//xyz
     endif

! lettura coord x
       par(1) = xyz(1:lxyz)
      ! print*, par(1)
       lxx = len_trim(par_blank(1))
       ind5 = index(par(1)(1:lxx), ' ')
       x = par(1) (1:ind5-1)
      ! print*, x
       lx = len_trim(x)
       ind5_bis = index(x(1:lx), '(')
      if (ind5_bis == 0) then
       fract(1) = x(1:lx)
      else
       fract(1) = x(1:ind5_bis-1)
      endif
    if (len_trim(fract(1)) ==0) cycle READ_ELEMENT

    read(fract(1),*, iostat = iost) coord_real(1)
        if (coord_real(1)> unter_SP - sceps_SP .and. coord_real(1)< unter_SP + sceps_SP) then 
             write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                        'they are approximate to the closer special position!'
            coord_real(1)= unter_DP
         endif  
         if (coord_real(1)> duter_SP - sceps_SP .and. coord_real(1)< duter_SP + sceps_SP) then 
            write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                        'they are approximate to the closer special position!'
            coord_real(1)= duter_DP
         endif  
   !  print*, coord_real(1)
     if (iost /= 0) then
         coord_real(1) = 1.0000
         write (logfile,*) ' Error reading atoms''coordinates, set default value 1.00!'
     endif    
! lettura y
      par_blank(2) = par(1)(ind5+1:lxx)
      par(2) = trim(adjustl(par_blank(2)))
      lyy = len_trim(par(2))
      ind6 = index(par(2)(1:lyy), ' ')
      y = par(2)(1:ind6-1)
      l_y = len_trim(y)
      ind6_bis = index(y(1:l_y), '(')
     if (ind6_bis == 0) then
      fract(2) = y(1:l_y)
     else
      fract(2) = y(1:ind6_bis-1)
     endif
      if (len_trim(fract(2)) == 0) cycle READ_ELEMENT
     read(fract(2), *, iostat = iost) coord_real(2)
      if (coord_real(2)> unter_SP - sceps_SP .and. coord_real(2)< unter_SP + sceps_SP) then 
          write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                     'they are approximate to the closer special position!'
         coord_real(2)= unter_DP
      endif  
       if (coord_real(2)> duter_SP - sceps_SP .and. coord_real(2)< duter_SP + sceps_SP) then 
            write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                 'they are approximate to the closer special position!'
            coord_real(2)= duter_DP
         endif
   !  print*, coord_real(2)
       if (iost /= 0) then
         coord_real(2) = 1.0000
        write (logfile,*) ' Error reading atoms''coordinates, set default value 1.00!'
      endif    
! lettura z
     par_blank(3) = par(2)(ind6+1:lyy)
     par(3) = trim(adjustl(par_blank(3)))
     lz = len_trim(par(3))
     ind7 = index(par(3)(1:lz), ' ')
     z = par(3)(1:ind7-1)
     l_z = len_trim(z)
    if (len_trim(z) == 0) then
     z_atom = par(3)(1:lz)
    else
     z_atom = z(1:lz)
    endif
     lzat = len_trim(z_atom)
     ind7_bis = index(z_atom(1:lzat), '(')
    if (ind7_bis == 0) then
     fract(3) = z_atom
    else
     fract(3) = z_atom(1:ind7_bis-1)
    endif
   if (len_trim(fract(3)) == 0) cycle READ_ELEMENT
      read(fract(3), *, iostat= iost) coord_real(3)
            if (coord_real(3)> unter_SP - sceps_SP .and. coord_real(3)< unter_SP + sceps_SP) then 
          write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                     'they are approximate to the closer special position!'
         coord_real(2)= unter_DP
      endif  
       if (coord_real(3)> duter_SP - sceps_SP .and. coord_real(3)< duter_SP + sceps_SP) then 
            write (logfile,*) 'Warning! The coordinates in the cif file have a low accuracy ',&
                 'they are approximate to the closer special position!'
            coord_real(2)= duter_DP
         endif
    !  print*, coord_real(3)
      if (iost /= 0) then
        coord_real(3) = 1.00
       write (logfile,*) ' Error reading atoms''coordinates, set default value 1.00!'
     endif 

task7_info:  select case (u_pos)
     case (0) task7_info
    if (b_pos == 0) then
          Uiso = '1.0'
      
     read(Uiso,*) Uiso_real
!      write(logfile,*)'WARNING! '//rline(1:3)//' Uiso/anis missing in cif set to '//Uiso 
    else 
     if (b_pos /= 0) then   
       goto 50
     endif
    endif
!   write(ilogout,*) 'Uiso ='//trim(adjustl(Uiso)) 
     if (occ_pos == 0) then
      sof = '1.0'
     read(sof, *) sof_real
    endif
     if (occ_pos /= 0) then 
       if (occ_pos == (fract_pos_z+1)) then 
       lzz = len_trim(par(3)) 
       ind = index(par(3)(1:lzz), ' ')
       read (par(3)(ind+1:lzz), '(a)') occ_blank
       locc = len_trim(occ_blank)
     if (occ_pos /= mass) then
      ind11 = index (occ_blank(1:locc),' ')
      occ = occ_blank (1:ind11-1)
!    print*, 'occ ='//occ
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
      if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
      read(sof,*, iostat= iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
      else
       if (ind10_bis == 0) then
       sof = occ 
      read(sof,*, iostat= iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
       endif
       endif
       endif
     if (occ_pos == mass) then
      occ = occ_blank
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
     if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
      read(sof,*, iostat = iost) sof_real
       if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
     else
       if (ind10_bis == 0) then
       sof = occ
      sof = occ(1:ind10_bis-1)
     endif
     endif
     endif
endif
endif
   write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Uiso_real,sof_real
if (iost /= 0) cycle READ_ELEMENT 

   
 case (1:) task7_info
    if (therm_pos /= 0) then
    ind8 = index (xyz(1:lxyz),'U')
     u_line = xyz (ind8:lxyz)
     u_line = trim(adjustl(u_line)) 
     lu_line = len_trim(u_line)
    if (u_line(1:4) == 'Uiso'.or.u_line(1:4) == 'Uequ') then
     ind9 = index(xyz(1:ind8-2),' ',.true.) 
     u = xyz(ind9+1:ind8-1)
     u = trim(adjustl(u))
     lu = len_trim(u)
     ind9_bis = index(u(1:lu),'(')
       if (ind9_bis == 0) then
         Uiso = u
      ! write(ilogout,*)' Uiso='//trim(adjustl(Uiso))
     read(Uiso,*, iostat = iost) Uiso_real
          if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' Uiso, set default value 1.00!'
       endif
        else
         Uiso = u (1:ind9_bis-1)
   !   write(ilogout,*)' Uiso='//trim(adjustl(Uiso))
        read(Uiso,*, iostat = iost) Uiso_real
        if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' Uiso, set default value 1.00!'
       endif
       endif
    else
     if (u_line(1:4) == 'Uani') then
       Uiso = '-1.0'
       l_anis = l_anis +1
!    write(ilogout,*)' Uiso='//trim(adjustl(Uiso))
    read(Uiso,*, iostat = iost) Uiso_real
        if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' Uiso, set default value 1.00!'
       endif
!      print*, 'Calcolare U iso from Uij'     
      endif
     endif
  if (Uiso_real == 0.00) then
    Uiso_real = 1.000
  endif 
  if (occ_pos == 0) then
   sof = '1.0'
  read(sof,*, iostat = iost) sof_real
    if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
  endif
if (occ_pos /= 0) then
    if (occ_pos == (therm_pos+1)) then
      ind10 = index(u_line(1:lu_line),' ') 
      occ_blank = (u_line(ind10+1:lu_line)) 
      locc = len_trim(occ_blank)
     if (occ_pos /= mass) then
      ind11 = index (occ_blank(1:locc),' ')           
      occ = occ_blank (1:ind11-1)
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
      if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
    read(sof,*,iostat = iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
     else
       if (ind10_bis == 0) then
       sof = occ
      read(sof,*, iostat= iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
     endif
    endif
  endif 
     if (occ_pos == mass) then
      occ = occ_blank
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
     if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
     read(sof,*,iostat=iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
     else
       if (ind10_bis == 0) then
       sof = occ
     read(sof,*,iostat= iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
       endif
     endif
    endif
endif
endif
else
  if (therm_pos == 0) then
   if (u_pos == mass) then
     ind = index (rline(1:ll), ' ', .true.)  
      u_line = rline(ind+1:ll) 
       u_line = trim(adjustl(u_line))
        lu_line = len_trim(u_line)
        ind_bis = index (u_line(1:lu_line), '(')
         if (ind_bis == 0) then
           Uiso = u_line
        read(Uiso,*,iostat=iost) Uiso_real
            if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' Uiso, set default value 1.00!'
       endif
          else
           if (ind_bis /= 0) then
             Uiso = u_line (1:ind_bis-1)
           read(Uiso,*,iostat=iost) Uiso_real  
              if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' Uiso, set default value 1.00!'
       endif  
          endif
        endif
 if (occ_pos == 0) then
   sof = '1.0'
 read(sof,*) sof_real
 endif
 if (occ_pos /= 0) then 
       if (occ_pos == (fract_pos_z+1)) then 
       lzz = len_trim(par(3)) 
       ind = index(par(3)(1:lzz), ' ')
       read (par(3)(ind+1:lzz), '(a)') occ_blank
       locc = len_trim(occ_blank)
      ind11 = index (occ_blank(1:locc),' ')
      occ = occ_blank (1:ind11-1)
 if (verbose)      print*, 'occ ='//occ
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
      if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
      read(sof,*,iostat = iost) sof_real
            if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
      else
       if (ind10_bis == 0) then
       sof = occ 
      read(sof,*,iostat= iost) sof_real
        if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
      
       endif
      endif
     endif
    endif
   endif
   if (u_pos /= mass) then
   if (u_pos == (fract_pos_z+1)) then 
    lzz = len_trim(par(3))
     ind = index(par(3)(1:lzz), ' ')
      read (par(3)(ind+1:lzz), '(a)') u_line
     u_line = trim(adjustl(u_line))
     lu_line = len_trim(u_line)
     ind11 = index (u_line(1:lu_line),' ')
     u = u_line (1:ind11-1)
     lu = len_trim(u)
     ind10_bis = index (u(1:lu),'(')
     if (ind10_bis /= 0) then
       Uiso = u(1:ind10_bis-1)
        read(Uiso,*,iostat= iost) Uiso_real 
        if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
       else
       if (ind10_bis == 0) then
       Uiso = u
         read(Uiso,*,iostat=iost) Uiso_real
            if (iost /= 0) then
            Uiso_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif  
       endif
     endif
    endif
   if (occ_pos /= 0) then
    if (occ_pos == (fract_pos_z+1)) then   
     lzz = len_trim(par(3))
     ind = index(par(3)(1:lzz), ' ')
     read (par(3)(ind+1:lzz), '(a)') occ_blank
     locc = len_trim(occ_blank)
    endif
     if (occ_pos /= mass) then
      ind11 = index (occ_blank(1:locc),' ')
      occ = occ_blank (1:ind11-1)
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
     if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
     read(sof,*,iostat=iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
      else
       if (ind10_bis == 0) then
       sof = occ
        read(sof,*,iostat=iost) sof_real
           if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
       endif
     endif
   endif
    if (occ_pos == mass) then
      ind = index (rline(1:ll), ' ', .true.)
      occ_blank = rline(ind+1:ll)
       occ = trim(adjustl(occ_blank))
        loc = len_trim(occ)
        ind_bis = index (occ(1:loc), '(')
         if (ind_bis == 0) then
           sof = occ
           read(sof,*,iostat=iost) sof_real
        if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
          else
           if (ind_bis /= 0) then
             sof = occ (ind_bis+1:loc)
            read(sof,*,iostat=iost) sof_real
      if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
           endif
        endif
    endif 
endif
endif
 endif   
endif
if (Uiso_real /= 1.0) then
Biso = Uiso_real*eight_pisqa
write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Biso,sof_real
else
if (Uiso_real == 1.0) then
 write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Uiso_real,sof_real
endif
endif
end select task7_info
!print*, b_pos, fract_pos_z

50 if (b_pos == (fract_pos_z+1)) then 
      lzz = len_trim(par(3)) 
       ind = index(par(3)(1:lzz), ' ')
       read (par(3)(ind+1:lzz), '(a)') b_line
       b_line = trim(adjustl(b_line))
         lb_line = len_trim(b_line)
      ! print*, b_line
       ind11 = index (b_line(1:lb_line),' ')
       b = b_line (1:ind11-1)
       lb = len_trim(b)  
       ind = index (b(1:lb),'(')
         if (ind == 0) then 
       Biso_char = b
      read(Biso_char,*,iostat = iost) Biso
            if (iost /= 0) then
            Biso= 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
        else
        Biso_char = b(1:ind-1)
          read(Biso_char,*) Biso
       !   print*, Biso
        endif  
      if (occ_pos == (b_pos+1)) then
      lb_line = len_trim(b_line)
       ind = index(b_line(1:lb_line), ' ')
       read (b_line(ind+1:lb_line), '(a)') occ_blank
       locc = len_trim(occ_blank)
       ind11 = index (occ_blank(1:locc),' ')
        occ= occ_blank(1:ind11-1)
         locc = len_trim(occ)
           occ = trim(adjustl(occ))
          ind = index(occ(1:locc),'(')
          if (ind == 0) then 
          sof = occ
      read(sof,*,iostat=iost) sof_real
       if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
   !   print*, sof_real
           else 
            sof = occ(1:ind-1)
            read(sof,*,iostat=iost) sof_real
        if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
    !  print*, sof_real
      endif
      write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Biso,sof_real
      endif
    endif
    
  if (b_pos /= 0.and.b_pos == mass) then
    ind = index(rline(1:ll),' ',.true.)
     b_line = rline(ind+1:ll)
      b_line = trim(adjustl(b_line))
    if (verbose)   print*, b_line
     lb_line = len_trim(b_line)
       ind10_bis = index (b_line(1:lb_line),'(')
        if (ind10_bis /= 0) then
       Biso_char = b_line(1:ind10_bis-1)
      read(Biso_char,*) Biso
      else
       if (ind10_bis == 0) then
       Biso_char = b_line 
      read(Biso_char,*) Biso
       endif
       endif
    if (occ_pos == 0) then 
      sof = '1.0'
     read(sof, *) sof_real
      endif 
    if (occ_pos /= 0) then
      if (occ_pos == (fract_pos_z+1)) then 
       lzz = len_trim(par(3)) 
       ind = index(par(3)(1:lzz), ' ')
       read (par(3)(ind+1:lzz), '(a)') occ_blank
       locc = len_trim(occ_blank)
       ind11 = index (occ_blank(1:locc),' ')
      occ = occ_blank (1:ind11-1)
 if (verbose) print*, 'occ ='//occ
      loc = len_trim(occ)
      ind10_bis = index (occ(1:loc),'(')
      if (ind10_bis /= 0) then
       sof = occ(1:ind10_bis-1)
      read(sof,*,iostat= iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
      else
       if (ind10_bis == 0) then
       sof = occ 
      read(sof,*,iostat=iost) sof_real
         if (iost /= 0) then
            sof_real = 1.000
         write (logfile,*) ' Error reading atoms'' sof, set default value 1.00!'
       endif
       endif
       endif
      endif
     endif
write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Biso,sof_real
endif
!write(scratch1,'(a8,a5,5f15.7)', iostat = iost)'Coord   ',al(1:1)//j,coord_real,Biso,sof_real
endif
enddo READ_ELEMENT
enddo READ_ATOM 

if (rline(1:5) == 'data_') then
goto 10
endif

40 n_anis = l_anis
if (verbose) print*, n_anis

allocate (Biso_calc(n_anis), Uiso_calc(n_anis))
allocate (U_real(3,n_anis))

 READ_ATOMS:  do i = 1, n_anis 
   read(iloginp,'(a)', end = 10) rline
    rline = trim(adjustl(rline))
!  print*, rline(1:ll)
    ll = len_trim(rline)
if (verbose) print*, rline
  READ_ELEMENTS: do ch = 1, 26
   if (rline(1:1) == elem(ch)) then
if (verbose)    print*, rline
!  READ_ANIS: do i = 1, nat
   read(rline(1:ll),'(a)') atom
    atom = trim(adjustl(atom))
   lat = len_trim(atom)
     ind1 = index(atom,' ')
    read(atom(ind1+1:lat),'(a)') atom_lb
   atom_lb = trim(adjustl(atom_lb))
if (verbose)  print*, 'atom_lb '//atom_lb 
   llb = len_trim(atom_lb)
   ind2 = index(atom_lb(1:llb), ' ')
   U_char(1) = atom_lb(1:ind2-1)
!  print*,'Uanis '//U_char(1)
   read(U_char(1),*,iostat=iost) U_real(1,i) 
       if (iost /= 0) then
            U_real(1,i) = 1.000
         write (logfile,*) ' Error reading atoms'' U11, set default value 1.00!'
       endif
!   print*,'U11=', U_real(1,i)
     U22_line =  atom_lb(ind2+1:ll)
     lU22 = len_trim(U22_line)
if (verbose)   print*, 'U22_line'//U22_line
     ind3 = index(U22_line(1:lU22), ' ') 
     U_char(2) = U22_line(1:ind3-1) 
   read(U_char(2), *,iostat=iost) U_real(2,i)
      if (iost /= 0) then
            U_real(2,i) = 1.000
         write (logfile,*) ' Error reading atoms'' U22, set default value 1.00!'
       endif
 !  print*, 'U22 =',U_real(2,i)
     U33_line = U22_line(ind3+1:lU22) 
     lU33 = len_trim(U33_line)
     ind4 = index(U33_line(1:lU33), ' ')     
     U_char(3) = U33_line(1:ind4-1)
    read(U_char(3), *,iostat=iost) U_real(3,i)
      if (iost /= 0) then
            U_real(3,i) = 1.000
         write (logfile,*) ' Error reading atoms'' U33, set default value 1.00!'
       endif
 ! print*,'U33 =', U_real(3,i)    
   Uiso_calc(i) = (U_real(1,i)+U_real(2,i)+U_real(3,i))/3
!  print*, 'Uiso=', Uiso_calc(i)
   Biso_calc(i) = eight_pisqa*Uiso_calc(i) 
 if (verbose) print*,'Biso=', Biso_calc(i)
!  enddo READ_ANIS
endif
enddo READ_ELEMENTS
enddo READ_ATOMS

!do i = 1, n_anis
!   print*, Biso_calc(i)
!enddo

10 close (iloginp)

ilogout = FIND_UNIT()
file_out = trim(adjustl(adjustr(outpath)))
lfout = len_trim(file_out)
file_out=file_out(1:lfout)
! print*, 'output='//file_out

!print*,lfout,file_out

open(ilogout, status = 'replace', form = 'formatted',&
     access = 'sequential', action = 'readwrite', file = file_out, iostat = iost)
      if (iost /= 0) then
        write(logfile,*) 'Opening  '//file_out//' file ERROR!'
       endif

rewind(scratch1)

  linee = 0             
   read(scratch1,'(a)') title
   read(scratch1,'(a6,6f12.5)') cella2,cell
   read(scratch1,'(a6,a15)') space,sg

 READ_SC: do  
  read(scratch1, '(a5,6x,a2,5f15.7)', end = 18) cord,label,coord_real,Biso,sof_real
   linee = linee+1
if (verbose) print*, cord,label, coord_real,Biso,sof_real
 enddo READ_SC
18  nat = linee
if (verbose) print*, nat  

 allocate(x_cord(nat),y_cord(nat),z_cord(nat),b_cord(nat),sof_cord(nat),&
                       label2(nat),atoN(nat),cord2(nat),label3(nat))
 allocate(cord_inp(nat,6), cord_out(nat,6))


 rewind(scratch1)
scratch2 = FIND_UNIT()
open(unit = scratch2, status = 'scratch', access ='sequential', action = 'readwrite')
  read(scratch1,'(a)') title
  read(scratch1,'(a6,6f12.5)') cella2,cell
  read(scratch1,'(a6,a15)') space,sg
 
  Zline = 1
  do i = 1, nat
  read(scratch1,'(a5,6x,a2,5f15.7)', end = 15) cord2(i),label2(i),x_cord(i), y_cord(i),&
                                               z_cord(i), b_cord(i),sof_cord(i)
    if (b_cord(i) == -78.9568405) then
      b_cord(i) = Biso_calc(i)
 if (verbose)  print*, b_cord(i)
   endif
       if (b_cord(i) .gt. -70.0000 .and. b_cord(i) .lt. 0.000) then
         b_cord(i) = 0.00001
    if (verbose)   print*, b_cord(i)
     endif

   if (b_cord(i) == 1. .or. b_cord(i) == 4.) then
      b_count = b_count +1
 if (verbose)  print*, b_cord(i)
   endif


   do  Zatom = 1, 100
    if (label2(i) == sym(Zatom)) then
   atoN(i) =  ch2(Zatom)
    endif
    enddo
      cord_inp(i,1) = x_cord(i)
      cord_inp(i,2) = y_cord(i)
      cord_inp(i,3) = z_cord(i)
      cord_inp(i,4) = b_cord(i)
      cord_inp(i,5) = sof_cord(i)
      cord_inp(i,6)=  atoN(i)
   !   print*, atoN(i)
   if (i>1) then  
      if (atoN(i) /=  atoN(i-1)) then
     Zline = Zline + 1
    endif
  endif
enddo   

 n_b = b_count
if (verbose)  print*, n_b
     if (n_b > 0) then
        write(logfile,*) 'Uiso/anis missing in cif file set to a default value!'
     endif
 
   allocate(xam(Zline))
    write(ilogout,'(a)') title
    write(ilogout,'(a6,6f12.5)') cella2,cell
    write(ilogout,'(a6,a15)') space,sg

  do mx  = 1, Zline
     xam(mx) = maxval(atoN)
    if (xam(mx) == 0) exit
   do i = 1,nat
     if (cord_inp(i,6) == xam(mx)) then 
      cord_out(i,1) = cord_inp(i,1)
      cord_out(i,2) = cord_inp(i,2) 
      cord_out(i,3) = cord_inp(i,3)
      cord_out(i,4) = cord_inp(i,4)
      cord_out(i,5) = cord_inp(i,5)
      cord_out(i,6) = cord_inp(i,6)
    do  Zatom = 1, 100
      if (cord_out(i,6) == ch2(Zatom)) then
       label3(i) = sym(Zatom)
    endif
   enddo
!   print*, cord2(i),label3(i), cord_out(i,1), cord_out(i,2),&
!                                       cord_out(i,3), cord_out(i,4), cord_out(i,5) 
     write(scratch2,'(a5,a2,5f15.7)') cord2(i),label3(i), cord_out(i,1), cord_out(i,2),&
                                       cord_out(i,3), cord_out(i,4), cord_out(i,5)
     cord_inp(i,6) = 0.0
    atoN(i) = 0.0
  endif
 enddo
enddo
allocate(spec(nat))
rewind (scratch2) 
Nsp = 1
do i = 1, nat
 !print*,i,  nat
  read(scratch2, '(a5,a2, 5f15.7)', end =20 ) cord2(i),label3(i), cord_out(i,1), cord_out(i,2),&
                                       cord_out(i,3), cord_out(i,4), cord_out(i,5)
 !  print*, cord2(i),label3(i)
  if (i >1) then 
      if(label3(i) /= label3(i-1)) then
          Nsp = Nsp +1
            ! print*, label3(i)
        endif
     endif
       spec(i) = Nsp
!   print*, spec(i), Nsp
! print*, cord2(i),label3(i), sp(i), cord_out(i,1), cord_out(i,2),&
!                                            cord_out(i,3), cord_out(i,4), cord_out(i,5)
  write(ilogout, '(a5,6x,a2,i5,5f15.7)')  cord2(i),label3(i), spec(i), cord_out(i,1), cord_out(i,2),&
                                            cord_out(i,3), cord_out(i,4), cord_out(i,5)
enddo

15 close(scratch1)
20 close(scratch2)
     close(ilogout) 
   !read(ilogout,'(a)', iostat = iost) rline_pha
   !  if (iost /= 0) then
  !     write(logfile, '(a)') 
  ! close (ilogout)

rewind(logfile)
 read(logfile,'(a)', iostat = iost) rline_log
!print*, rline_log
 if (iost /= 0) then
 close (logfile, status="delete")
else
 close(logfile)
endif
 
end program mkpha
