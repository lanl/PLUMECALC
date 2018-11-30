      subroutine read_mdot_info()
!***********************************************************************
! $Id: read_mdot_info.f,v 1.2 2006/07/25 14:34:31 zvd Exp $
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
!     DEFINITION of variables
!
!     Identifier      Type       Description
!
!     n_sources       integer    number of individual solute sources
!     npart           integer    number of particles
!     start_no        integer    the particle number of the first 
!                                  particle in this source, array of 
!                                  size n_sources
!     n_part_source   integer    the number of particles associated with
!                                  each source, array of size n_sources
!     n_size_mdot     integer    total size of mdot and time arrays
!     mdot            real*8     array of mass flux values for all 
!                                  sources in the time, mdot curves, 
!                                  array of size n_size_mdot
!     time_mdot       real*8     array of times associated with the 
!                                  time, mdot curves, array of size 
!                                  n_size_mdot
!     
!***********************************************************************
!     PSEUDOCODE
!
!     6. Read source term information
!     
!     FOR each source
!        Open file
!        count the number of points by reading through the file
!        Close file
!     ENDFOR each source
!
!     Compute total size of mass flux and time arrays
!
!     ALLOCATE size for the arrays
!
!     FOR each Source
!        For each point in time, mass flux curve
!        read in a store each time and mass flux
!     ENDFOR each source
!
!***********************************************************************

      use comunits, only : sim_unit_number
      use commdot
      implicit none

      logical done, null1
      integer i, j, j1, previous_start, max_column
      integer current_unit_number
      integer counter
      integer open_file
      integer, allocatable :: header_lines(:)
      character*80 current_line
      character*250 current_file
      real*8, allocatable :: xjunk(:)

!*************Executable statements start here **************

      counter = 0
      allocate (header_lines(n_sources))
      header_lines = 0
!     FOR each source
      do i = 1, n_sources
         read(sim_unit_number,'(a250)') current_file
!        OPEN file
         current_unit_number = open_file(current_file, 1, 'OLD')
         done = .false.
         end_no_mdot(i) = counter
         do while (.not. done)
            read(current_unit_number,'(a80)',end=1000)
     2           current_line
            if (.not. null1(current_line)) then
               if (current_line(1:1) .eq. '#') then
!        count the number of header or comment lines at start of file
                  header_lines(i) = header_lines(i) + 1
               else
!        count the number of points by reading through the file
                  end_no_mdot(i) = end_no_mdot(i) + 1
                  counter = counter + 1
               end if
            end if
         end do
 1000    continue
!        CLOSE file
         close (current_unit_number)
      end do
!     ENDFOR each source
!     Compute total size of mass flux and time arrays
      n_size_mdot = end_no_mdot(n_sources)
! Find maximum column used for flux values
      do i = 1, n_sources
         max_column = max (1, column_number(i), max_column)
      end do
      
!     ALLOCATE size for the arrays
      allocate(mdot(n_size_mdot))
      allocate(time_mdot(n_size_mdot))
      allocate(xjunk(max_column))

      do i = 1, n_sources
         backspace sim_unit_number
      end do
      previous_start = 1
!     FOR each Source
      do i = 1, n_sources
         read(sim_unit_number,'(a250)') current_file
         current_unit_number = open_file(current_file, 1, 'OLD')
!        FOR each point in time, mass flux curve
         do j = 1, header_lines(i)
!           READ over the header/comment lines
            read(current_unit_number,'(a80)') current_line
         end do
         do j=previous_start, end_no_mdot(i)
!           READ in a store each time and mass flux
            read(current_unit_number,*)time_mdot(j),
     2           (xjunk(j1), j1=1,column_number(i)-1),
     3           mdot(j)
         end do
!        ENDFOR
         previous_start = end_no_mdot(i)+1
         close (current_unit_number)
      end do
!     ENDFOR each source

      deallocate(xjunk)

      return
      end subroutine read_mdot_info
