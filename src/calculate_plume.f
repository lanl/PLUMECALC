      subroutine calculate_plume(current_time)
!***********************************************************************
! $Id: calculate_plume.f,v 1.3 2007/02/14 20:42:03 zvd Exp $
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
!     7. Perform convolution calculation at each time
!
!   FOR each source
!
!     FOR each particle in this source
!        FOR each stay in a given cell
!           IF time of calculation is less than exit time for the cell
!              Compute tau_out and tau_in
!              call mdot_integral to compute solute mass contrib.
!              Divide by number of particles for this source
!           ELSE the stay in this cell straddles the time
!              Compute tau_in, set tau_out to 0
!              call mdot_integral to compute solute mass contrib.
!              Divide by number of particles for this source
!              exit loop for this particle
!           ENDIF time of calculation is less than exit time for the cell
!        FOR each stay in a given cell
!     ENDFOR each particle in this source
!
!   ENDFOR each source
!
!   FOR each cell
!     perform correction for porosity and cell size
!   ENDFOR each cell
!
!***********************************************************************

      use comrock, only : ps, rfac, diffusion_model, itrc_diff,
     2     matrix_por, use_matrix
      use comgrid, only : sx1
      use commdot, only : n_sources, time_mdot, mdot, end_no_mdot
      use comparttr, only : start_no, npart, cell_index,
     2     n_cells, time_packed, cell_packed, n_part_source,
     3     concentration, cell_path, n_touched_cells, touched_cells,
     4     kdecay, end_no, step_no, cmin
      use comsim, only : out_cell
      implicit none

      integer isource
      integer istart
      integer iend
      integer istep
      integer jstart
      integer jend
      integer i
      integer j
      integer icell
      integer iarraystart
      integer iarrayend
      integer arraylen
      integer cellno
      real*8 current_time
      real*8 tau_in
      real*8 tau_out
      real*8 time_in
      real*8 result
      real*8 integrate_curve
      real*8 sxliter

!****************Begin executable statements here ******************

!   FOR each source
      do isource = 1, n_sources
         istart = start_no(isource)
         iend = end_no(isource)
         istep = step_no(isource)

         if(isource.eq.1) then
            iarraystart = 1
         else
            iarraystart = end_no_mdot(isource-1)+1
         end if
         iarrayend = end_no_mdot(isource)
         arraylen = iarrayend-iarraystart+1
!     FOR each particle in this source
         do i = istart, iend, istep

            jstart = cell_index(i)
            jend = cell_index(i)+n_cells(i)-1
            time_in = 0.

!        FOR each stay in a given cell
            partloop: do j = jstart, jend

!           IF time of calculation is less than exit time for the cell
               if(current_time.gt.time_packed(j)) then
!              Compute tau_out and tau_in
                  tau_out = current_time - time_packed(j)
                  tau_in = current_time - time_in
                  icell = cell_path(cell_packed(j))

                  if(out_cell(cell_packed(j)).eq.1) then 
!              call mdot_integral to compute solute mass contrib.
                     result = integrate_curve(tau_out, tau_in, 
     2                    current_time,
     2                    arraylen, time_mdot(iarraystart:iarrayend), 
     3                    mdot(iarraystart:iarrayend), kdecay)
!              Divide by number of particles for this source
                     concentration(icell) = concentration(icell) +
     2                    result/n_part_source(isource)
                  end if
                  time_in = time_packed(j)
!           ELSE the stay in this cell straddles the time
               else
!              Compute tau_in, set tau_out to 0
                  tau_out = 0.
                  tau_in = current_time - time_in
                  icell = cell_path(cell_packed(j))

                  if(out_cell(cell_packed(j)).eq.1) then
!              call mdot_integral to compute solute mass contrib.
                     result = integrate_curve(tau_out, tau_in, 
     2                    current_time,
     2                    arraylen, time_mdot(iarraystart:iarrayend), 
     3                    mdot(iarraystart:iarrayend),kdecay)
!              Divide by number of particles for this source
                     concentration(icell) = concentration(icell) +
     2                    result/n_part_source(isource)
                  end if
!              exit loop for this particle
                  exit partloop
               end if
!           ENDIF time of calculation is less than exit time for the cell
            end do partloop
!        EndFor each stay in a given cell


         end do
!     ENDFOR each particle in this source

      end do
!   ENDFOR each source

!     Only ones being written out will have nonzero concs.
!   FOR each cell
      do i = 1, n_touched_cells
         cellno = touched_cells(i)
c s kelkar 11/20/06
c change units of the cell voulme from m^3 to l so that the 
c answer will be in moles/l instead of moles/m^3
         sxliter=sx1(cellno)*1000.
!     perform correction for porosity and cell size
         if (diffusion_model .and. use_matrix) then
            if (itrc_diff(i).ne.0) then
               concentration(i) = concentration(i)/
     2              (matrix_por(itrc_diff(i))*sxliter*rfac(cellno))
            else
               concentration(i) = concentration(i)/
     2              (ps(cellno)*sxliter*rfac(cellno))
            end if
         else
            concentration(i) = concentration(i)/
     2           (ps(cellno)*sxliter*rfac(cellno))
         end if
         if (concentration(i) .lt. cmin) concentration(i) = 0.
      end do
!   ENDFOR each cell

      return
      end subroutine calculate_plume
