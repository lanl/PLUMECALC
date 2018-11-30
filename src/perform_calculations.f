      subroutine perform_calculations()
!***********************************************************************
! $Id: perform_calculations.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
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
!     FOR each time at which a calculation is being performed
!        calculate concentration at this time
!        WRITE results to output file
!     ENDFOR each time at which a calculation is being performed
!
!     9. Perform convolution calculation at each time
c
!   For each source
c
!     For each particle in this source
!        For each stay in a given cell
!           If time of calculation is less than exit time for the cell
!              Compute tau_out and tau_in
!              call mdot_integral to compute solute mass contrib.
!              Divide by number of particles for this source
!           Else the stay in this cell straddles the time
!              Compute tau_in, set tau_out to 0
!              call mdot_integral to compute solute mass contrib.
!              Divide by number of particles for this source
!              exit loop for this particle
!           EndIf time of calculation is less than exit time for the cell
!     Endfor each particle in this source
c
!   Endfor each source
c
!     8a. mdot_integral: Perform integration of mass flux for this particle
c
!     For each time in mdot curve, starting w/ the second
c
!        If tau_out is less than or equal to this time
c
!           Record position in mdot where integration begins
!           Compute interpolated value of mdot
c
!           Exit loop for each time in mdot curve
c
!        Endif tau_out is less than or equal to this time
c
!     EndFor each time in mdot curve
c
!     For each time in mdot curve, starting with the one found for tau_out
!        If tau_in is less than or equal to this time
!           Compute interpolated value of mdot
!           Add to integrated result this contribution
!           Exit loop for each time in mdot
!        Else tau_in is past this time
!           Add to integrated result this contribution
!        Endif tau_in is less than or equal to this time
!     Endfor each time in mdot curve, starting with the one found for tau_out
c
c
!***********************************************************************

      use comsim, only : ntimes, out_times, conc_string, delta_time
      use comparttr, only : concentration, time_packed, cell_packed,
     2     n_packed, cfavg
      implicit none

      integer i
      real*8 current_time
      logical first
      real*8 previous_time

c*****************     Executable statements begin here

      previous_time = 0.
!     FOR each time at which a calculation is being performed
      do i = 1, ntimes
!        calculate concentration at this time
         if(conc_string .eq. 'favg') then
            cfavg = 0.
            current_time = out_times(i)
            if (delta_time .gt. 0.) then
! User specified output times are being used
               previous_time = max (0.d0, current_time - delta_time)
            end if
            call calculate_favg(previous_time,current_time)
            previous_time = current_time
         else
            concentration = 0.
            current_time = out_times(i)
            call calculate_plume(current_time)
         end if
         if(i.eq.1) then
            first = .true.
         else
            first = .false.
         end if
!        WRITE results to output file
         call output_results(first,current_time)
      end do
!     ENDFOR each time at which a calculation is being performed

      return
      end subroutine perform_calculations
