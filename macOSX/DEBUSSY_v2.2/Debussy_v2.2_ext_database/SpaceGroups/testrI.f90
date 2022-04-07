program testreadg
implicit real(8)(a-h,o-z),integer(4)(i-n)
character(len=10) :: sgname
character(len=24) :: sgfile,sgfile2
character(256) :: rl
character(256),save :: path_SpaceGroups= &
"/Users/acervellino/000000-NANO/EXPERIM/ADVANCED_NOTES/TESTING/00000-VDIS-ZM/ext_database/SpaceGroups/"


open(1,status='old',action='READ', form='formatted',access='sequential',&
           file=trim(path_SpaceGroups)//'SG_Centering.txt')
read(1,*)
do
  read(1,*,iostat=io)n1,n2,sgname!,sgfile,sgfile2
  if (io/=0) exit
!  print'(2i8,4x,">",a,"< >",a,"<   ***    >",a,"<")',n1,n2,trim(sgname),trim(sgfile),trim(sgfile2)
  print'(2i8,5x,a)',n1,n2,trim(sgname)
enddo

end