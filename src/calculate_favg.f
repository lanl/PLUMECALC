      subroutine calculate_favg(previous_time,current_time)
!***********************************************************************
! $Id: calculate_favg.f,v 1.2 2006/08/30 17:30:20 zvd Exp $
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
!        ENDFOR each stay in a given cell
!     ENDFOR each particle in this source
!
!   ENDFOR each source
!
!   FOR each cell
!     perform correction for porosity and cell size
!   ENDFOR each cell
!***********************************************************************

      use comgrid, only : sx1
      use commdot, only : n_sources, time_mdot, mdot, end_no_mdot
      use comparttr, only : start_no, npart, cell_index,
     2     n_cells, time_packed, cell_packed, n_part_source,
     3     cell_path, kdecay, end_no, step_no, cfavg, cmin,
     4     conc_total, zone_vol
      use comrock, only : itrc_diff, matrix_por, ps
      use comsim, only : nfavgzones, index_favg, water_flux, alt_string
      use comunits, only : error_unit_number, flux_unit_number
      implicit none

      integer isource
      integer istart
      integer iend
      integer istep
      integer jstart
      integer jend
      integer i
      integer j
      integer iarraystart
      integer iarrayend
      integer arraylen
      real*8 current_time
      real*8 previous_time
      real*8 tau1
      real*8 tau2
      real*8 result
      real*8 integrate_curve
      real*8 kdecayeq0
      real*8 ps_total
      logical :: warning = .false.
      parameter(kdecayeq0=0.)

      save warning

!****************Begin executable statements here ******************

      cfavg = 0.
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
!      FOR each particle in this source
         do i = istart, iend, istep

            jstart = cell_index(i)
            jend = cell_index(i)+n_cells(i)-1

!         FOR each stay in a given cell
            partloop: do j = jstart, jend
!         IF time of calculation is less than exit time for the cell
               if(current_time.gt.time_packed(j)) then
!            IF the cell is one of the ones in a favg zone
                  if(index_favg(cell_packed(j)).ne.0) then
!               Compute limits of integration
                     tau1 = max(0.d0,previous_time - time_packed(j))
                     tau2 = current_time - time_packed(j)
!               call integrate_curve to compute solute mass contrib.
                     result = integrate_curve(tau1, tau2, 
     2                    current_time,
     2                    arraylen, time_mdot(iarraystart:iarrayend), 
     3                    mdot(iarraystart:iarrayend), kdecayeq0)
!               Divide by number of particles for this source
                     cfavg(index_favg(cell_packed(j))) = 
     2                    cfavg(index_favg(cell_packed(j))) +
     3                    exp(-kdecay*time_packed(j))*
     4                    result/n_part_source(isource)
!     We don't exit loop here because even though the particle is exiting
!     a node designated as a flux averaged zone, this doesn't have to
!     mean it is exiting the system. If it actually exits, there won't
!     be any more segments, so we will exit anyway (i.e. j=jend)
                  end if
!              ENDIF the cell is one of the ones in a favg zone
               else
!     Particle has not left this cell yet, exit loop, go to next particle
                  exit partloop
               end if
!           ENDIF time of calculation is less than exit time for the cell
            end do partloop
!        ENDFOR each stay in a given cell


         end do
!     ENDFOR each particle in this source

      end do
!   ENDFOR each source

!     Only ones being written out will have nonzero concs.

!   FOR each favg zone
      do i = 1, nfavgzones
!     perform correction for flow rate from the zone
         if (water_flux(i) .ne. 0.) then
            cfavg(i) = cfavg(i)/
     2           (water_flux(i)*(current_time-previous_time))
            if (cfavg(i) .lt. cmin) cfavg(i) = 0.
         else
            if (alt_string .ne. 'alt') then
               if (cfavg(i) .gt. 0.) then
                  if (.not. warning) write (error_unit_number, 9)
                  cfavg(i) = -1. * cfavg(i)
               end if
            end if
         end if
         if (itrc_diff(i).ne.0) then
            ps_total = ps(i) + (1.0d0 - ps(i)) * 
     2           matrix_por(itrc_diff(i))
         else
            ps_total = ps(i)
         end if
         if (flux_unit_number .ne. 0) then
            conc_total(i) = cfavg(i) * sx1(i) * ps_total * 1000.
         else if (alt_string .eq. 'alt') then
c This value is really undefined for the alternate output
c the following is just a place holder
            conc_total(i) = cfavg(i)
         else
            conc_total(i) = cfavg(i) * zone_vol(i) * ps_total * 1000.
         end if
      end do
!   ENDFOR each favg zone
 9    format ('Flux of 0. found for cell with solute',
     &     'output will be negative total moles')
      return
      end subroutine calculate_favg
