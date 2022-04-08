
!___ predefined syntax of some system commands

character(1),dimension(3),parameter :: separators=['/','/','\']
character(7),dimension(3),parameter :: opsystems      = ['MacOSX ', 'Linux  ', 'Windows']
character(5),dimension(3),parameter :: delete_commands= ['rm -f',   'rm -f',   'del  ']
character(3),dimension(3),parameter :: ls_commands    = ['ls ',     'ls ',     'dir']
character(4),dimension(3),parameter :: cp_commands    = ['cp  ',    'cp  ',    'copy']
character(5),dimension(3),parameter :: mkdir_commands = ['mkdir',   'mkdir',   'md   ']

!_ Specify operating system giving the appropriate value to the variable isystem (below) 
!_ according to the following scheme:
!______ 1 = Mac OS X
!______ 2 = Linux
!______ 3 = Windows

integer,parameter :: isystem = 1 

logical,save  :: verbose = .false.

