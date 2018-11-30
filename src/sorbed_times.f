      subroutine sorbed_times
!***********************************************************************
! $Id: sorbed_times.f,v 1.1 2006/05/17 15:23:25 zvd Exp $
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
!     PSEUDOCODE
!
!     FOR each cell
!        If the retardation factor is not 1
!           Set flag to denote corrections needed
!           Exit loop over each cell
!        ENDIF the retardation factor is not 1
!     ENDFOR each cell
!
!     IF corrections are needed
!        FOR each particle
!           FOR each segment of its travel path
!              Compute new exit time from this cell
!           ENDFOR each segment of its travel path
!        ENDFOR each particle
!     ENDIF corrections are needed
!
!***********************************************************************

      use comparttr, only : npart, n_cells, time_packed, cell_packed
      use comgrid, only : n_grid_points
      use comrock, only : rfac
      implicit none

      logical corrections
      integer i
      integer j
      integer istart
      integer iend
      real*8 time_current
      real*8 time_original_previous
      real*8 delta_time

c***************** Begin executable statements here**********

      corrections = .false.

!     FOR each cell
      check_loop: do i = 1, n_grid_points
!        If the retardation factor is not 1
         if(rfac(i).ne.1.) then
!           Set flag to denote corrections needed
            corrections = .true.
!           Exit loop over each cell
            exit check_loop
         end if
!        ENDIF the retardation factor is not 1
      end do check_loop
!     ENDFOR each cell

!     IF corrections are needed
      if(corrections) then

         istart = 1
!        FOR each particle
         do i = 1, npart
            iend = istart + n_cells(i)-1
            time_current = 0.
            time_original_previous = 0.
!           FOR each segment of its travel path
            do j = istart, iend
!              Compute new exit time from this cell
               delta_time = time_packed(j)-time_original_previous
               time_current = time_current + 
     2              rfac(cell_packed(j))*delta_time
               time_original_previous = time_packed(j)
               time_packed(j) = time_current
            end do
!           ENDFOR each segment of its travel path
            istart = iend + 1
         end do
!        ENDFOR each particle
      end if
!     ENDIF corrections are needed

      return
      end subroutine sorbed_times
