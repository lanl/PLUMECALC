      subroutine read_sim_info()
!***********************************************************************
! $Id: read_sim_info.f,v 1.2 2006/09/20 19:48:06 zvd Exp $
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
!     3. Read simulation parameter information
!
!     READ the number of sources
!     ALLOCATE arrays
!     READ the starting particle number for each source
!     READ the column number for each mdot curve
!     READ parameter denoting format of stor file (lda)
!
!     Compute the number of particles for each source
!        (assumes contiguous particle numbering such that
!         particle number for the first source goes from
!         1 to Np1, next is Np1+1 to Np2, etc.)
!
!***********************************************************************
!     Identifier      Type     Description
!
!     sim_unit_number integer file unit number for simulation input file
!     n_sources       integer  number of mass flux sources
!     end_no_mdot     integer  for each source, the index in the array 
!                                of the last entry in the mdot array for
!                                that source, array of size n_sources 
!     start_no        integer  starting particle number for each source,
!                                array of size n_sources
!     n_part_source  integer   number of particles associated with each 
!                                source, array of size n_sources
!***********************************************************************

      use comunits, only : sim_unit_number
      use commdot, only : n_sources, end_no_mdot, column_number
      use comparttr, only : npart, start_no, n_part_source, kdecay,
     2     end_no, step_no, cmin, cthreshold
      use comgrid, only : lda
      implicit none

      integer i
      integer current_unit_number
      integer imsg(3), msg(3), nwds
      real*8  xmsg(3)
      character*2   dummy2
      character*32  cmsg(3)
      character*80  dummy_string
      character*100 current_file
 
      read(sim_unit_number,'(a80)') dummy_string
      call parse_string(dummy_string, imsg, msg, xmsg, cmsg, nwds)
      n_sources = imsg(1)
      kdecay = xmsg(2)
      if (nwds .gt. 2) then
         cthreshold = xmsg(3)
      else
         cthreshold = cmin
      end if

!     ALLOCATE arrays

      allocate(start_no(n_sources))
      allocate(end_no(n_sources))
      allocate(step_no(n_sources))
      allocate(n_part_source(n_sources))
      allocate(column_number(n_sources))
      allocate(end_no_mdot(n_sources))
      start_no = 1
      end_no = 1
      step_no = 1
      n_part_source = 0
      column_number = 1
      end_no_mdot = 0

!     READ the starting particle number for each source
      read(sim_unit_number,'(a2)') dummy2

      if(dummy2 .eq. 'do') then
         do i=1,n_sources
            read(sim_unit_number,*)start_no(i),end_no(i),step_no(i)
         end do
      else
         backspace sim_unit_number
         read(sim_unit_number,*)(start_no(i),i=1,n_sources)
         do i = 1, n_sources-1
            end_no(i) = start_no(i+1)-1
         end do
         end_no(n_sources) = npart
      end if

!     READ the column number for each mdot curve
      read(sim_unit_number,*)(column_number(i),i=1,n_sources)

!     Compute the number of particles for each source
!        (assumes contiguous particle numbering such that
!         particle number for the first source goes from
!         1 to Np1, next is Np1+1 to Np2, etc.)
      if(dummy2 .eq. 'do') then
         do i = 1, n_sources
            n_part_source(i) = end_no(i)-start_no(i)+1
         end do
      else
         do i = 1, n_sources-1
            n_part_source(i) = start_no(i+1)-start_no(i)
         end do
         n_part_source(n_sources) = npart-start_no(n_sources)+1
      end if

      end subroutine read_sim_info
