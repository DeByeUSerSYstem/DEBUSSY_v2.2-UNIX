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
program DB_PHA_CLU_info

use HELPINPUT

implicit none 

character(512)         :: rline, pline, phase, file_inp, grp, sg_line, Pha_name, sampling_how,file_PS,&
                          todo, output_xyz, cel_name, pha, phase_path, pwd, shapeC, sg_sym, &
                          grp_path, PS_inp,model_line, no_atosp_char, sg_blank, ddb,inp_path
character(5)           :: orig
character(10)          :: okk
character(50)          :: path_xyz
character(16)          :: Pears_symb,PS
character(3)           :: sg_char
character(1)           :: cell_constr_method='P'
character(4)           :: kw, kw2
character(4)           :: ident
character(8)           :: model
real(DP)               :: D_max_SPH, D_max_PAR, L_max_PAR, N_max_SPH, xmass_density_gcm3, redmindis, &
                          N1_max_PAR, N2_max_PAR, th2max, wave, rcorra, scorra,rcorrb, &
                          scorrb, cocoxy, sigmaxa,sigmaxb, B000_c, sigma_min_sph, para_coeff, &
                          max_intrinsic_sigma, decorr_len, cellpar(3), N1_PAR, N2_PAR, CELL_XYZ(3)
real(DP)               :: default_xmass_density_gcm3 = zero, default_redmindis = zero
integer(I4B)           :: sg_nr, sgr, ll, ind, no_atosp, iloginp, iloginp1,ilogout, ilogout5, iost,sg_pos, lm, lml,&
                          orig_pos, ind3, inds, llp, lpwd, ilogout3, i,iorspg, sc, ilogout4,ind1,lsg_blank,linp, lo, &
                          ind4, ilogpha, ilogout1, ilogout2, llph, pha_pos,lsg_line,AT_CELL_XYZ, lpha,lpath,lddb,linpath
logical     :: grp_exist=.false., phase_exist= .false., todo_exist=.false., para_exist=.false., spg_variant=.false., oricel_xyz,&
              occ_exist=.false., PS_exist=.false., mol_exist=.false.!!__AC RF 11.06.2015 [verbose now in local_system.inc], verbose=.false.





print*, '                                         ------------------------------------'
print*,'                                                 DebUsSy Suite v2.2   '
print*, '                                         ------------------------------------'
print*, ' '
print*,'     Running DB_PHASE Program    '
print*, ' '



!  file_inp = 'DB_PHA_CLU_info.inp'
  iloginp = FIND_UNIT()

 call GET_PWD(pwd=pwd,lpwd=lpwd) 


 call getarg(1,ddb)
 ddb=trim(adjustl(ddb))
 lddb=len_trim(ddb)
 
!print*, ddb(1:lddb)
   ind=index(ddb(1:lddb), separator,.true.)
 if (ind==0) then
   file_inp=ddb(1:lddb)
   file_inp=trim(adjustl(file_inp))
   linp=len_trim(file_inp)
 else
    read(ddb(ind+1:lddb),'(a)') file_inp
    file_inp=trim(adjustl(file_inp))
    linp=len_trim(file_inp)
    read(ddb(1:ind),'(a)') inp_path
    inp_path=trim(adjustl(inp_path))
    linpath=len_trim(inp_path)
 endif

 
 open(unit = iloginp, status = 'old', file = ddb(1:lddb), form = 'formatted', &
       access = 'sequential', action = 'read', iostat = iost)
 if (iost /= 0) then
  print*, ' Error opening file '//ddb(1:lddb)//'! Program stops!'
 STOP   
 endif
 
 AT_CELL_XYZ = 0
 oricel_xyz=.true.
 CELL_XYZ = zero

 READ_IN: do
    read(unit = iloginp, fmt = '(a)', end = 10) rline
    if (rline(1:1) == '!'.or.rline(1:1) == '#'.or.len_trim(rline) == 0) cycle READ_IN
    ll = len_trim(rline)
    if (ll<5) then
      print*, ' INPUT WARNING: Too short line at '//rline(1:ll)//'!  Program stops!'
      STOP
    endif
 
    kw = rline(1:4)
    call lowcase(kw)

 


 task1_info: select case(kw)
       case ('phas')  task1_info
           ind = index(rline(1:ll), ' ',.true.)
           inds = index(rline(ind:ll),separator)
           if (inds == 0) then
               read(rline(ind+1:ll),*) phase
               phase = trim(adjustl(phase))
               phase_path = pwd(1:lpwd)//trim(adjustl(phase))
            else
               read(rline(ind+1:ll),fmt = '(a)') phase_path
               phase_path = trim(adjustl(phase_path))
            endif
              llp = len_trim(trim(phase_path))
              ind3 = SCAN(phase_path(1:llp),separator,.true.) 
              Pha_name = phase_path(ind3+1:llp) 
 
             inquire(file = phase_path, exist = phase_exist)
               if (.not.phase_exist) then
                 print*, ' Error! '//trim(adjustl(phase_path))//' file not found! Program stops!' 
                 STOP
               else
                  if (verbose) print*, 'File '//trim(adjustl(phase_path))//' found'
               endif
               Pha_name = trim(adjustl(Pha_name))
               lpha = len_trim(Pha_name)
               ind4= index(Pha_name(1:lpha), '.xyz')
               if (ind4/=0) mol_exist = .true.
                   
        
                     
           
           
       case ('spac') task1_info
        if (.not. mol_exist) then
         ind = index(rline(1:ll),' ',.true.)
         sg_line = trim(adjustl(rline(ind+1:ll)))
         lsg_line= len_trim(sg_line)
         call lowcase(sg_line(1:1))
         if (sg_line(1:1) == 'u'.or.sg_line(1:1)=='o'.or.sg_line(1:1)=='s') then
            read (sg_line(1:lsg_line), '(a)') orig
            orig = trim(adjustl(orig))
            lo=len_trim(orig)
             if (orig(1:1) == 'U'.OR. orig(1:1) == 'u') then
               call upcase(orig(1:1)) 
               call lowcase(orig(2:2))
                 else
               if (orig(1:1) == 'O'.OR.orig(1:1) == 'o') then
               call lowcase(orig(1:1))
                else
                if  (orig(1:1) == 's'.OR. orig(1:1) == 'S') then
               call upcase(orig(2:2)) 
             !  call lowcase(orig(3:lo))
                endif
               endif
              endif
              
              read (rline(1:ind-1), '(a)') sg_blank
               sg_blank= (trim(adjustl(sg_blank)))
               lsg_blank = len_trim(sg_blank)
            ind1 = index(sg_blank(1:lsg_blank), ' ',.true.)
            read (sg_blank(ind1+1:lsg_blank),*) sg_char
              sg_char = (trim(adjustl(sg_char)))
          read(sg_char(1:3),*,iostat=iorspg) sg_nr
            if (iorspg/=0.or.(iorspg==0.and.(sg_nr < 1.or.sg_nr > 230))) then
             print*, 'Error! Wrong SG number supplied in input! Program stops!'  
              STOP
             endif 
          else 
            read (sg_line(1:lsg_line),*) sg_char
            sg_char = (trim(adjustl(sg_char)))
            read(sg_char(1:3),*,iostat=iorspg) sg_nr
            if (iorspg/=0.or.(iorspg==0.and.(sg_nr < 1.or.sg_nr > 230))) then
             print*, 'Error! Wrong SG number supplied in input! Program stops!' 
             STOP
             endif
            endif
          write(sg_char, fmt = '(i3.3)') sg_nr
          else
           cycle READ_IN
         endif
     
        
         origin: select case(orig(1:1))
         case ('U') origin
           grp = 'SG_Nr_'//trim(adjustl(sg_char))//'_'//orig(1:lo)//'.grp'
           inquire(file = trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp)), exist = grp_exist)
           if (.not.grp_exist) then
             print*, ' ERROR! Wrong combination SG/unique axis supplied in input! Program stops!'
             STOP
           else
             if (verbose) print*, 'File '//trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp))//' found'
           endif

         case ('o') origin
           grp = 'SG_Nr_'//trim(adjustl(sg_char))//'_'//orig(1:lo)//'.grp'
           inquire(file = trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp)), exist = grp_exist)
           if (.not.grp_exist) then
             print*, ' ERROR! Wrong combination SG/origin supplied in input! Program stops!'
             STOP
           else
             if (verbose) print*, 'File '//trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp))//' found'
           endif       
   
         case ('s') origin
           grp = 'SG_Nr_'//trim(adjustl(sg_char))//'_'//orig(2:lo)//'.grp'
           inquire(file = trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp)), exist = grp_exist)
           if (.not.grp_exist) then
             print*, ' ERROR! Wrong combination SG/setting supplied in input! Program stops!'
             STOP
           else
             if (verbose) print*, 'File '//trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp))//' found'
           endif
   
           

   
         case default origin
           grp = 'SG_Nr_'//trim(adjustl(sg_char))//'.grp'
           inquire(file = trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp)), exist = grp_exist)
           if (.not.grp_exist) then
             print*, ' ERROR! Wrong combination SG supplied in input! Program stops!'
             STOP
           else
             if (verbose) print*,'File '//trim(path_SpaceGroups)//'SPG_grp/'//trim(adjustl(grp))//' found'
           endif
         end select origin
    
       case ('atom') task1_info
       if (.not. mol_exist) then
          ind = INDEX(rline(1:ll), ' ', .true.)
          read(rline(ind+1:ll),*, iostat = iost) no_atosp
          if (iost /= 0) then 
            print*, 'Error in number of atomic species supplied! Program stops!'
            STOP
          endif
        else
          cycle READ_IN
        endif

       case ('cell') task1_info
       if (.not. mol_exist) then
         ind = INDEX(rline(1:ll), '.')
          if (ind == 0) then
            print*, 'Error in cell origin supplied! Program stops!'
            STOP
           else
          read(rline(ind-1:ll),*, iostat = iost) CELL_XYZ
             if (iost /= 0) then
            read(rline(ind-1:ll),*, iostat = iost) AT_CELL_XYZ
            if (iost /= 0) then
              print*, 'Error in cell origin supplied! Program stops!'
              STOP
            endif
            oricel_xyz=.false.
          else
            oricel_xyz=.true.
          endif
          endif
        else
          cycle READ_IN
        endif

       case ('cons') task1_info
         if (ll>6) then
           cell_constr_method=rline(ll:ll)
         else
           cell_constr_method='P'
         endif
         
       case ('pear') task1_info
        if (.not. mol_exist) then
        ind = INDEX(rline(1:ll), ':', .true.)
        read(rline(ind+1:ll),'(a)', iostat = iost) PS
            PS=trim(adjustl(PS))
             if (PS(1:1) == 'a'.or.PS(1:1) == 'm'.or.PS(1:1) == 't'.or.PS(1:1) == 'o' &
               .or.PS(1:1) == 'h'.or.PS(1:1) == 'c') then 
                 PS_exist = .true.
                 read(PS, '(a)') Pears_symb
                 Pears_symb = trim(adjustl(Pears_symb))   
                 else 
                      PS_exist = .false.
               endif
          else
            cycle READ_IN
         endif
        
       
       case ('shap') task1_info 
         if (.not. mol_exist) then    
          ind = INDEX(rline(1:ll), ' ',.true.)
          read(rline(ind+1:ll),fmt ='(a)', iostat = iost) shapeC
          shapeC = trim(adjustl(shapeC))
            call UPCASE (shapeC)
            if (shapeC /= 'SPH'.AND.shapeC /= 'PAR'.AND.shapeC /= 'HEX' &
                 .AND.shapeC /= 'CYL'.AND.shapeC /= 'QBE') then
             print*, 'Error in cluster shape supplied! Program stops!' 
             STOP
           endif 
        else 
          cycle READ_IN
        endif
           
         case ('diam') task1_info
              ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),*, iostat = iost) D_max_SPH      
              if (iost /= 0) cycle READ_IN
 
            case ('n_ma') task1_info
             ind = INDEX(rline(1:ll), ' ',.true.)
             read(rline(ind+1:ll),*, iostat = iost) N_max_SPH
              if (iost /= 0)  cycle READ_IN

          case ('d_ma') task1_info
              ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),*, iostat = iost) D_max_PAR
              if (iost /= 0) cycle READ_IN
       
         case ('l_ma') task1_info
            ind = INDEX(rline(1:ll), ' ',.true.)
            read(rline(ind+1:ll),*, iostat = iost) L_max_PAR
              if (iost /= 0) cycle READ_IN
  
         case ('n1_m') task1_info
            ind = INDEX(rline(1:ll), ' ',.true.)
            read(rline(ind+1:ll),*, iostat = iost) N1_max_PAR 
              if (iost /= 0) cycle READ_IN

         case ('n2_m') task1_info    
            ind = INDEX(rline(1:ll), ' ',.true.)
            read(rline(ind+1:ll),*, iostat = iost) N2_max_PAR
             if (iost /= 0)  cycle READ_IN
           
            case ('todo') task1_info
             if (.not. mol_exist) then
                ind = INDEX(rline(1:ll),' ')
              read(rline(ind+1:ll), fmt ='(a)', iostat = iost) todo
                if (iost /= 0)  cycle READ_IN
                 todo = trim(adjustl(todo))
              call lowcase(todo)
               if (todo /= 'largest_only'.AND.todo /= 'all_clusters'.AND.todo /= 'all_clusters 4') then
                print*, 'ERROR in cluster building method supplied! Program stops!'
                STOP
                else 
                  todo_exist = .true.
              endif
           else
              cycle READ_IN
          endif

        case('occ1') task1_info
          if (.not. mol_exist) then
           ind = INDEX(rline(1:ll), ' ',.true.)
             read(rline(ind+1:ll),'(a)',iostat=iost) okk
             okk = trim(adjustl(okk))
              if (iost /= 0)  cycle READ_IN
              call lowcase(okk)
               if (okk /= 'y'.AND.okk /= 'n'.AND.okk /= '1' .AND. okk /= '0') then
                print*, 'ERROR in OCC1 flag supplied! Please type y or n. Program stops!'
                STOP
                else 
                 occ_exist = .true.
              endif
            else
             cycle READ_IN
            endif
                 

          case ('para') task1_info
             ind = INDEX(rline(1:ll), ' ')
             para_exist = .true.
             read(rline(ind+1:ll),*, iostat = iost) model
             lm = len_trim(model)     
        
                     
              
             para_model : select case(model(1:5))
                 case ('WelbA') para_model
                 read(rline(ind+1:ll), *, iostat=iost) model, rcorra, scorra, rcorrb, scorrb,&
                       cocoxy, sigmaxa, sigmaxb, B000_c, sigma_min_sph
                   if (iost /= 0) then
                     print*, 'Error in paracrystalline model coefficients supplied! Program stops!'
                     STOP
                   endif
                 
                case ('WelbI') para_model                    
                  read(rline(ind+1:ll), *, iostat=iost) model, max_intrinsic_sigma, decorr_len
                   if (iost /= 0) then
                     print*, 'Error in paracrystalline model coefficients supplied! Program stops!'
                     STOP
                   endif

                   case default para_model
                    print*, 'Error in paracrystalline model supplied! Program stops!' 
                    STOP
                   end select para_model

          case('samp') task1_info
              ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),fmt = '(a)') sampling_how
             sampling_how = trim(adjustl(sampling_how))
             call lowcase(sampling_how)
               if (sampling_how /= 'one' .AND. sampling_how /= 'all') then
                print*, 'ERROR in sampling method supplied! Program stops!'
                STOP
              endif
            
          case('wave') task1_info
             ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),*, iostat = iost) wave
              if (iost /= 0) then
                print*, 'Error in wavelenght number format supplied! Program stops!'
                STOP
              endif
 
          case('2-th') task1_info
             ind = INDEX(rline(1:ll), ' ', .true.)
             read(rline(ind+1:ll),*, iostat = iost) th2max
             if (iost /= 0) then
                print*, 'Error in 2-theta number fomat supplied! Program stops!'
                STOP
              endif
    
                               
         case ('redu') task1_info
          if (mol_exist) then
              ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),*, iostat = iost) redmindis
              if (iost /= 0) redmindis = default_redmindis
         else 
           cycle READ_IN
        endif
              
         case ('dens') task1_info
          if (mol_exist) then
              ind = INDEX(rline(1:ll), ' ',.true.)
              read(rline(ind+1:ll),*, iostat = iost) xmass_density_gcm3
              if (iost /= 0) xmass_density_gcm3  = default_xmass_density_gcm3 
          else
            cycle READ_IN
          endif    
      
         
         case default task1_info
          print*,  'INPUT ERROR : wrong line at '//rline(1:ll)
          STOP
   end select   task1_info
  enddo READ_IN

 10 close(iloginp)
 
iloginp1 = FIND_UNIT()
 ! PS calculation   
 if (.not.PS_exist) then
   open(unit = iloginp1, status = 'old', file = trim(path_SpaceGroups)//'SG_Centering_PS.txt', &
        form = 'formatted',&
         access = 'sequential', action = 'read')
    read (unit = iloginp1, fmt = '(a)') rline
    READ_SP: do 
    read(unit = iloginp1, fmt = '(2i6,a13,a27,a6)', end = 12) sgr,sc, sg_sym, grp_path, PS
     if (sgr == sg_nr) then 
        read(PS, '(a6)') Pears_symb
        Pears_symb = trim(adjustl(Pears_symb))//'##'
       endif
    enddo READ_SP
12    close(iloginp1)
 endif
 
 
 ! mkcell.ini with fields *.pha/*.grp/nat_sp/origin/pears_symb
 ilogout = FIND_UNIT()
 if (.not.mol_exist) then
 open(unit = ilogout, status = 'replace', form = 'formatted', &
     access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'mkcell.ini', iostat = iost)
 if (iost /= 0) then 
   print*, 'Opening mkcell.ini file ERROR! Program stops!'
   STOP
 else
   if (oricel_xyz) then
     write(unit = ilogout, fmt = '(a/a/i10/3f14.6/a/a)') (trim(adjustl(Pha_name))), 'SPG_grp/'//(trim(adjustl(grp))), &
          no_atosp, CELL_XYZ,'constr  '//cell_constr_method, (trim(adjustl(Pears_symb)))
   else
     write(unit = ilogout, fmt = '(a/a/i5.1/"Atom0",i4/a/a)') (trim(adjustl(Pha_name))), 'SPG_grp/'//(trim(adjustl(grp))), &
          no_atosp, AT_CELL_XYZ,'constr  '//cell_constr_method, (trim(adjustl(Pears_symb)))
   endif
 endif
endif
   llph = len_trim(adjustl(Pha_name))
   pha_pos = index(Pha_name,'.')
    pha = trim(adjustl(Pha_name(1:pha_pos)))
   cel_name = (trim(adjustl(pha)))//'cel'
close(ilogout)

 ilogpha = FIND_UNIT()
 if (.not.mol_exist) then
   phase_path = trim(adjustl(phase_path))
   open(unit = ilogpha, status = 'old', form = 'formatted', &
     access = 'sequential', action = 'readwrite', file = phase_path, iostat = iost)
     if (iost /= 0) then
       print*, 'ERROR in opening '//phase_path//' Program stops!'
       STOP
      endif
     read_pha : do
       read(unit = ilogpha, fmt = '(a)', end = 20) pline
         if (pline(1:1) == '!'.or.pline(1:1) == '#'.or.len_trim(pline) == 0) cycle READ_PHA
          ident = pline(1:4)
           call lowcase (ident)
          llp = len_trim(adjustl(pline))
           task2_info : select case (ident)
               case ('cell') task2_info
                 ind = index(pline(1:llp),' ')
                 read(pline(ind+1:llp), fmt = *, iostat = iost) cellpar
                 if (iost /= 0) then
                   print*, 'ERROR in reading cell parameters from '//phase_path//'!'
                   print*,'***>'//pline(ind+1:llp)//'<*** ',cellpar
                 endif
               end select task2_info 
         enddo read_pha
    20 close(ilogpha)
endif


  ilogout5 =  FIND_UNIT()
    if (mol_exist) then
      open(unit = ilogout5, status = 'replace', form = 'formatted', &
      access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'molmkd.ini', iostat = iost)
        if (iost /= 0) then
          print*, 'Opening molmkd.ini file ERROR!'
          STOP
        else
        if (verbose) print*, 'molmkd.ini file created'
        phase_path = trim(adjustl(phase_path))
        lpath = len_trim(phase_path)
         write (unit = ilogout5, fmt ='(a)',iostat = iost) phase_path(1:lpath)
           if (iost /= 0) then
                 print*, 'ERROR in finding xyz path!'  
              endif
         write (unit = ilogout5, fmt = '(a3, 1x, f10.8, f10.5)', iostat = iost)  sampling_how, wave, th2max
             if (iost /= 0) then
                 print*, 'ERROR in finding sampling method, wavelenght and 2-theta max!'  
              endif
         write (ilogout5,'(a5,f10.8)') 'dens ', xmass_density_gcm3 
         write (ilogout5,'(a5,f10.8)') 'redm ', redmindis
        endif
      close(ilogout5)  
    endif 

 
if (.not.mol_exist) then
 ilogout1 =  FIND_UNIT()  
  cluster: select case (shapeC)
       case ('SPH') cluster
       open(unit = ilogout1, status = 'replace', form = 'formatted', &
           access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'sphmkQ.ini', iostat = iost)
          if (iost /= 0) then
            print*, 'Opening sphmkQ.ini file ERROR!'
            STOP
          else
         write (unit = ilogout1, fmt = '(a)', iostat = iost) trim(adjustl(cel_name)) 
            if (iost /= 0) then
              print*, 'ERROR in writing *.cel name!'
            endif 
        if (D_max_SPH > 0.0) then
           write (unit = ilogout1, fmt = '(a2,f10.4)') 'D ', D_max_SPH
         else
          if (N_max_SPH > 0) then
           write (unit = ilogout1, fmt = '(a2,i5)') 'N ', nint(N_max_SPH)  
          else
           if (D_max_SPH <= 0.0 .AND. N_max_SPH <= 1) then
             print*, 'ERROR in cluster limits supplied! Program stops!'
             STOP 
          endif   
        endif
        endif
          write (unit = ilogout1, fmt = '(a3)') shapeC
          if (todo_exist) then
            write (unit = ilogout1, fmt = '(a5, a20)') 'TODO ', todo
          endif
          
    if (occ_exist) then
         write (unit = ilogout1, fmt ='(a6, a1)') 'OCC1  ', okk
         else 
          write (unit = ilogout1, fmt ='(a7)') 'OCC1  n'
        endif
        
         
         if (para_exist) then
           print*, 'WARNING! Paracrystalline models not yet implemented in MK_BALL run'
         endif
            write (unit = ilogout1, fmt = '(a5, a3, 1x, f10.8, f10.5)', iostat = iost) 'SAMP ', &
                                                     sampling_how, wave, th2max
              if (iost /= 0) then
                print*, 'ERROR in finding sampling method, wavelenght and 2-theta max!'
               STOP
              else
                if (verbose) print*,  'sphmkQ.ini file created'
            endif

         endif
       close(ilogout1)      
   
   
   
   
 ilogout3 = FIND_UNIT()
        open(unit = ilogout3, status = 'replace', form = 'formatted', &
      access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'clumkS.ini', iostat = iost)
        if (iost /= 0) then
          print*, 'Opening clumkS.ini file ERROR! Program stops!'
          STOP
          else
            if (verbose) print*, 'clumkS.ini file created'
        endif
          write(unit = ilogout3, fmt = '(a)', iostat = iost)  trim(adjustl(cel_name))
           if (iost /= 0) then
             print*, 'ERROR in writing *.cel name! Program stops!'
             STOP
           endif
        if (D_max_SPH > 0.0) then
           write (unit = ilogout3, fmt = '(a2,f10.4)') 'D ', D_max_SPH
         else
          if (N_max_SPH > 0) then
           write (unit = ilogout3, fmt = '(a2,i5)') 'N ', nint(N_max_SPH)  
          else
           if (D_max_SPH <= 0.0 .AND. N_max_SPH <= 1) then
             print*, 'ERROR in cluster limits supplied! Program stops!'
             STOP 
          endif   
        endif
        endif
          write (unit = ilogout3, fmt = '(a3)') shapeC
      close(ilogout3)
         


  ilogout4 =  FIND_UNIT()  
   
   case ('QBE') cluster
       open(unit = ilogout4, status = 'replace', form = 'formatted', &
           access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'qbemkQ.ini', iostat = iost)
        if (iost /= 0) then
            print*, 'Opening qbemkQ.ini file ERROR! Program stops!'
            STOP
          else
         write (unit = ilogout4, fmt = '(a)', iostat = iost) trim(adjustl(cel_name)) 
            if (iost /= 0) then
              print*, 'ERROR in writing *.cel name! Program stops!'
              STOP
            endif 
         
         if (D_max_SPH > 0.0) then
           write (unit = ilogout4, fmt = '(a2,f10.4)') 'D ', D_max_SPH
         else
            
          if (N_max_SPH > 0) then
           write (unit = ilogout4, fmt = '(a2,i5)') 'N ', nint(N_max_SPH)  
          else
           if (D_max_SPH <= 0.0 .AND.  N_max_SPH <= 1) then
             print*, 'ERROR in cluster limits supplied! Program stops!'
             STOP 
          endif   
        endif
       endif 
        
        
        
          write (unit = ilogout4, fmt = '(a3)') shapeC
          if (todo_exist) then
            write (unit = ilogout4, fmt = '(a5, a20)') 'TODO ', todo
          endif
          
     if (occ_exist) then
         write (unit = ilogout4, fmt ='(a6, a1)') 'OCC1  ', okk
         else 
          write (unit = ilogout4, fmt ='(a7)') 'OCC1  n'
        endif
        
        
        
        
         if (para_exist) then
           print*, 'WARNING! Paracrystalline models not yet implemented in MK_BALL run'
         endif
            write (unit = ilogout4, fmt = '(a5, a3, 1x, f10.8, f10.5)', iostat = iost) 'SAMP ', &
                                                     sampling_how, wave, th2max
              if (iost /= 0) then
                print*, 'ERROR in finding sampling method, wavelenght and 2-theta max!'
               STOP
              else
                if (verbose) print*,  'qbemkQ.ini file created'
            endif

         endif
       close(ilogout4)      
     
     ilogout2 = FIND_UNIT()

     case ('PAR','HEX','CYL') cluster
      open(unit = ilogout2, status = 'replace', form = 'formatted', &
      access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'clumkQ.ini', iostat = iost)
        if (iost /= 0) then
          print*, 'Opening clumkQ.ini file ERROR! Program stops!'
          STOP
        else
         write(unit = ilogout2, fmt = '(a)', iostat = iost)  trim(adjustl(cel_name))
           if (iost /= 0) then
             print*, 'ERROR in writing *.cel name! Program stops!'
            STOP
           endif 
       if (D_max_PAR > 0.0 .AND. L_max_PAR > 0.0) then
          ! N1_PAR = (D_max_PAR*10.0d0)/cellpar(1)
          ! N2_PAR = (L_max_PAR*10.0d0)/cellpar(3)
         write (unit = ilogout2,fmt ='(a2,2(1x,f10.4))') 'D ', D_max_PAR, L_max_PAR
       else
        if (N1_max_PAR > 0.AND.N2_max_PAR > 0) then
          write (unit = ilogout2,fmt ='(a2,2(1x,i5))') 'N ',nint(N1_max_PAR), nint(N2_max_PAR)
         else
         if (D_max_PAR <= 0.0 .AND. L_max_PAR <= 0.0 .AND. N1_max_PAR <= 0.0 .AND. N2_max_PAR <= 0.0) then 
           print*, 'ERROR in cluster limits supplied! Program stops!' 
           STOP
         endif 
       endif
      endif
        write (unit = ilogout2,fmt='(a3)') shapeC
       if (todo_exist) then
         write (unit = ilogout2, fmt ='(a5, a20)') 'TODO ', todo
        endif
        
    if (occ_exist) then
         write (unit = ilogout2, fmt ='(a6, a1)') 'OCC1  ', okk
         else 
          write (unit = ilogout2, fmt ='(a7)') 'OCC1  n'
        endif
        
      
      if (para_exist) then
          paracry : select case(model)
               case ('WelbAnys') paracry
                 write(unit = ilogout2, fmt = '(a5, 9f8.4)', iostat = iost) 'PARA ', rcorra, scorra,&
                      rcorrb, scorrb, cocoxy, sigmaxa, sigmaxb, B000_c, sigma_min_sph
                 if (iost /= 0) then
                   print*, 'ERROR in paracrystalline model supplied!'
                endif
                case ('WelbIsot') paracry
                  write (unit = ilogout2, fmt = '(a14, 2f8.4, 7f5.1)', iostat = iost) 'PARA WelbIsot ', &
                        max_intrinsic_sigma, decorr_len, 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0
                   if (iost /= 0) then
                     print*, 'ERROR in paracrystalline model supplied!'
                         endif
                case default paracry
                   print*, 'ERROR in paracrystalline model supplied!'

                end select paracry      
             endif
         write (unit = ilogout2, fmt = '(a5, a3, 1x, f10.8, f10.5)', iostat = iost) 'SAMP ', sampling_how, wave, th2max
             if (iost /= 0) then
                 print*, 'ERROR in finding sampling method, wavelenght and 2-theta max! Program stops!'  
              STOP
                else
                 if (verbose) print*, 'clumkQ.ini file created'
              endif
            endif

      close(ilogout2)
     
 ilogout3 = FIND_UNIT()
        open(unit = ilogout3, status = 'replace', form = 'formatted', &
      access = 'sequential', action = 'readwrite', file = inp_path(1:linpath)//'clumk.ini', iostat = iost)
        if (iost /= 0) then
          print*, 'Opening clumk.ini file ERROR! Program stops!'
          STOP
          else
            if (verbose) print*, 'clumk.ini file created'
        endif
          write(unit = ilogout3, fmt = '(a)', iostat = iost)  trim(adjustl(cel_name))
           if (iost /= 0) then
             print*, 'ERROR in writing *.cel name! Program stops!'
             STOP
           endif
        if (D_max_PAR > 0.0 .AND. L_max_PAR > 0.0) then
          ! N1_PAR = (D_max_PAR*10.0d0)/cellpar(1)
          ! N2_PAR = (L_max_PAR*10.0d0)/cellpar(3)
         write (unit = ilogout3,fmt ='(a2,2(1x,f10.4))') 'D ', D_max_PAR, L_max_PAR
        else
           if (N1_max_PAR > 0 .AND. N2_max_PAR > 0) then
            write (unit = ilogout3, fmt ='(a2,2(1x,i5))') 'N ', nint(N1_max_PAR), nint(N2_max_PAR)
          else
             print*, 'Error in cluster sizes supplied Program stops!'
             STOP
           endif
         endif
           write (unit = ilogout3, fmt= '(a3)') shapeC

      close(ilogout3)

     case default cluster
       print*, 'ERROR in cluster shape supplied! Program stops!'
       STOP
   end select cluster
endif

close(ilogout)
 end program DB_PHA_CLU_info  
