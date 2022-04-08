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
MODULE STRUCT_ASREAD
use input_data

 TYPE(Coord_AT),DIMENSION(:),ALLOCATABLE    :: Struk_data

!**************
CONTAINS
!**************

 SUBROUTINE READ_STRUK_DATA
   IMPLICIT NONE
   integer                                  :: i,iu,astat,j,jj,ilin,ch0, chx, nspg
   CHARACTER(132)                           :: PATH_STRUCT, rline


   IF (ALLOCATED(Struk_data)) DEALLOCATE (Struk_data)
   ALLOCATE(Struk_data(NSTR))


    RETURN
2   print'(1x,a,a,a,i5)','ERROR reading starting free parameters, file: ',TRIM(STRUCTURE_NAME(i)),&
                                 ' structure %'
           STOP
3   print'(1x,a,a,a,i5)','ERROR reading Space Group matrix, file : ',TRIM(STRUCTURE_NAME(i)),&
                                 ' structure %'
           STOP

 END SUBROUTINE READ_STRUK_DATA
 

END MODULE STRUCT_ASREAD
! !---------------------------------------------------------------------------------
