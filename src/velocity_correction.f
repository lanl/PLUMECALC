      subroutine velocity_correction
!***********************************************************************
! Copyright 2006 Los Alamos National Security, LLC  All rights reserved
! Unless otherwise indicated,  this information has been authored by an 
! employee or employees of the Los Alamos National Security, LLC (LANS),
! operator of the  Los  Alamos National  Laboratory  under Contract  No.
! DE-AC52-06NA25396  with  the U. S. Department  of  Energy.  The  U. S.
! Government   has   rights  to  use,  reproduce,  and  distribute  this
! information.  The  public may copy  and  use this  information without
! charge, provided that this  Notice and any statement of authorship are
! reproduced on all copies.  Neither  the  Government nor LANS makes any
! warranty,   express   or   implied,   or   assumes  any  liability  or
! responsibility for the use of this information.       
!***********************************************************************
!
!     PSEUDOCODE
!
!     FOR each cell
!        If the velocity correction is not 1
!           Set flag to denote corrections needed
!           Exit loop over each cell
!        ENDIF the  factor is not 1
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
! Based on sorbed_times subroutine

      use comparttr, only : npart, n_cells, time_packed, cell_packed
      use comgrid, only : n_grid_points
      use comrock, only : vcf
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
!        If the velocity correction factor is not 1
         if(vcf(i).ne.1.) then
!           Set flag to denote corrections needed
            corrections = .true.
!           Exit loop over each cell
            exit check_loop
         end if
!        ENDIF the velocity correction factor is not 1
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
     2              vcf(cell_packed(j))*delta_time
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
      end subroutine velocity_correction
