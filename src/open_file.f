      integer function open_file(filename, iform, filestat)
!***********************************************************************
! $Id: open_file.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
!***********************************************************************
!  Copyright, 2002, 2004,  The  Regents of the University of California.
!  This  program  was  prepared by  the  Regents  of the  University  of
!  California  at Los Alamos National Laboratory (the  University) under
!  contract No. W-7405-ENG-36 with the U. S. Department of Energy (DOE).
!  All rights in the program are reserved by the DOE and the University.
!  Permission is  granted to the  public to copy  and  use this software
!  without  charge,  provided that this  Notice  and  any  statement  of
!  authorship are reproduced on all copies. Neither the U. S. Government
!  nor  the  University  makes any  warranty,  express  or  implied,  or
!  assumes any liability or responsibility for the use of this software.
!***********************************************************************
!  
!   PURPOSE
!  
!   To find an unused unit number and open the file specified.
!
!   Developed originally for FEHM.
!  
!**********************************************************************

      use comunits, only : error_unit_number
      implicit none

      character(*) filename, filestat 
      logical used
      integer iform
      integer irfile

c     Find an unused unit number, open the file if specified
      used = .true.
      irfile = 10
      do while(used)
         inquire(unit = irfile, opened = used)
         if(.not.used) then
            
c     Open file to read
            used = .false.
            select case (iform)
            case (1)
               open(irfile, file = filename, form = 'FORMATTED',
     &              err = 100, status = filestat)
            case (2)
               open(irfile, file = filename, form = 'UNFORMATTED',
     &              err = 100, status = filestat)
            case (3)
               open(irfile, file = filename, form = 'BINARY',
     &              err = 100, status = filestat)
            case default
               open(irfile, file = filename, status = filestat)
            end select
            open_file = irfile
         else
c     Unit number used, try the next one
            used = .true.
            irfile = irfile + 1
         end if
      end do

      return

 100  write (error_unit_number, *) 'ERROR opening ', filename
      write (error_unit_number, *) 'STOPPING execution'
      stop

      end

