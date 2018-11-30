      subroutine read_file_info()
!***********************************************************************
! $Id: read_file_info.f,v 1.1 2006/05/17 15:23:24 zvd Exp $
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
!     PSEUDOCODE
!
!     1. Read file information
!
!     OPEN control file (plumecalc.files) for file name information
!     OPEN info file (plumecalc.err) for warnings and error messages
!     READ names of files and OPEN each file for input/output
!     - grid file
!     - stor file
!     - sptr output file (particle tracking results)
!     - rock property file
!     - simulation control file
!     - output file
!     - type curve data file
!
!***********************************************************************

      use comgrid, only : lda, iflux
      use comunits
      implicit none

      integer open_file, iform, isptr, ctrl_unit_number
      integer i, iargc, nargc, len, imsg(2), msg(2), nwds
      real*8 xmsg(2)
      character*32  cmsg(2)
      character*7   file_stat
      character*12  file_format
      character*200 ctrl_file
      character*200 err_file
      character*200 grid_file
      character*200 stor_file
      character*200 sptr_file
      character*200 rock_file
      character*200 sim_file
      character*200 output_file
      character*200 tcurve_file
      character*200 flux_file
      character*200 cmdline
      character*200 input_msg
      logical ex

!********************* Executable lines begin here************
      iform = 1
      ex = .false.
      cmdline = ''
! Initialize file names
      ctrl_file = ''
      err_file = ''
      grid_file = ''
      flux_file = ''
      stor_file = ''
      sptr_file = ''
      rock_file = ''
      sim_file = ''
      output_file = ''
      tcurve_file = ''

! Have command line arguments been entered
      nargc = iargc()
      if (nargc .ge. 1) then
! First argument will be the name of the control file
         call getarg(1, cmdline)
         len = len_trim(cmdline)
         inquire (file = cmdline(1:len), exist = ex)
         if (ex)  then
            ctrl_file = cmdline(1:len)
         else
! Use default control file name
            ctrl_file = 'plumecalc.files'
         end if
         if (nargc .ge. 2) then
! Second argument will be the name of the error/information file
            call getarg(2, cmdline)
            len = len_trim(cmdline)
            err_file = cmdline(1:len)
         else
! Use default error/information file name
            err_file = 'plumecalc.err'
         end if
      else
! Use default names for control and error/information files
         ctrl_file = 'plumecalc.files'
         err_file = 'plumecalc.err'
       end if
       if (ctrl_file .eq. 'plumecalc.files') then
          inquire (file = ctrl_file, exist = ex)
          if (.not. ex) then
 200         write(6,*) trim(ctrl_file), ' Does not exist -- ',
     &            'Enter name of control file'
             read(5,'(200a)') ctrl_file
             inquire (file = ctrl_file, exist = ex)
             if (.not. ex) goto 200
          end if
       end if

!     OPEN control file for file name information
      file_stat = 'OLD'
      ctrl_unit_number = open_file(ctrl_file, iform, file_stat)

!     OPEN file for warnings and error messages
      file_stat = 'UNKNOWN'
      error_unit_number = open_file(err_file, iform, file_stat)

!     READ names of files from control file and 
!     OPEN each file for input/output

      file_stat = 'OLD'
!     - grid file
      read(ctrl_unit_number,'(a200)') grid_file
      grid_unit_number = open_file(grid_file, iform, file_stat)

!     - stor file
      read(ctrl_unit_number,'(a12)') file_format
      select case (file_format(1:3))
      case ('asc','for')
         lda = 1
      case ('unf')
         lda = 2
      case default
         lda = 1
         backspace ctrl_unit_number
      end select
      read(ctrl_unit_number,'(a200)') stor_file
      stor_unit_number = open_file(stor_file, lda, file_stat)

!     - flux file
      read(ctrl_unit_number,'(a200)') input_msg
      if (input_msg(1:4) .eq. 'flux') then
         read(ctrl_unit_number,'(a12)') file_format
         select case (file_format(1:3))
         case ('asc','for')
            iflux = 1
         case ('unf','bin')
            iflux = 3
         case default
            iflux = 1
            backspace ctrl_unit_number
         end select
         read(ctrl_unit_number,'(a200)') flux_file
         flux_unit_number = open_file(flux_file, iflux, file_stat)
      else
         flux_unit_number = 0
         backspace ctrl_unit_number
      end if

!     - sptr output file (particle tracking results) 
      read(ctrl_unit_number,'(a200)') input_msg
      call parse_string(input_msg, imsg, msg, xmsg, cmsg, nwds)
!     If the input is an integer 
!        (this is the number of sptr files to be read)
      if (msg(1) .eq. 1) then
         sptr_num = imsg(1)
      else
         backspace ctrl_unit_number
         sptr_num = 1
      end if
      allocate (sptr_unit_number(sptr_num), sptr_bin(sptr_num))
      do i = 1, sptr_num
!        name may be preceeded by line with file format 
         read(ctrl_unit_number,'(a12)') file_format
         select case (file_format(1:3))
         case ('asc','for')
            sptr_bin(i) = .false.
            isptr = 1
         case ('bin')
            sptr_bin(i) = .true.
            isptr = 3
         case ('unf')
            sptr_bin(i) = .true.
            isptr = 2
         case default
            sptr_bin(i) = .false.
            isptr = 1
            backspace ctrl_unit_number
         end select
         read(ctrl_unit_number,'(a200)') sptr_file
         sptr_unit_number(i) = open_file(sptr_file, isptr, file_stat)
      end do

!     - rock property file
      read(ctrl_unit_number,'(a200)') rock_file
      rock_unit_number = open_file(rock_file, iform, file_stat)

!     - simulation control file
      read(ctrl_unit_number,'(a200)') sim_file
      sim_unit_number = open_file(sim_file, iform, file_stat)

!     - output file
      file_stat = 'UNKNOWN'
      read(ctrl_unit_number,'(a200)') output_file
      output_unit_number = open_file(output_file, iform, file_stat)

!     - type curve data file
      file_stat = 'OLD'
      read(ctrl_unit_number,'(a200)', end = 100) tcurve_file
 100  if (tcurve_file .ne. '') then
         tcurve_unit_number = open_file(tcurve_file, iform, file_stat)
      else
         tcurve_unit_number = 0
      end if

      close ( ctrl_unit_number )
      return
      end
