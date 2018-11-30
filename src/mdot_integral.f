      subroutine mdot_integral(current_time,istart,iend)
!***********************************************************************
! Copyright 2010 Los Alamos National Security, LLC  All rights reserved
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
!**********************************************************************
!
! PURPOSE
!
! To compute the solute that has been introduced into the system by
! integrating the mdor source data.
!
!**********************************************************************
!
! Initial implementation: 22-Nov-10, Programmer: Zora Dash
!
!**********************************************************************

      use commdot, only : n_sources, time_mdot, mdot, end_no_mdot, 
     2     mdot_total, mdot_out_file, mdot_out_unit
      use comparttr, only : kdecay

      implicit none

      integer open_file
      integer isource
      integer istart
      integer iend
      integer arraylen
      real*8 current_time
      real*8 time_in
      real*8 :: last_time = 0.
      real*8 tau_in
      real*8 tau_out
      real*8 result
      real*8 integrate_curve

      save last_time
      
   !****************Begin executable statements here ******************

      if (.not. allocated(mdot_total)) then
         mdot_out_unit = open_file(mdot_out_file, 1, 'unknown')
         if (kdecay .ne. 0.) then
            allocate (mdot_total(n_sources + 1, 2))
            write (mdot_out_unit, 200)
         else
            allocate (mdot_total(n_sources + 1, 1))
            write (mdot_out_unit, 100) 
         end if
         mdot_total = 0.
      end if
      
!   FOR each source
      write (mdot_out_unit, 110) current_time
      do isource = 1, n_sources
         arraylen = iend - istart + 1
         tau_in = last_time
         tau_out = current_time
         result = integrate_curve(tau_in, tau_out, current_time,
     2        arraylen, time_mdot(istart:iend), mdot(istart:iend), 
     3        0.d0)
         mdot_total(isource, 1) = result
         mdot_total(n_sources + 1, 1) = mdot_total(n_sources + 1, 1) +
     2        mdot_total(isource, 1)
         if (kdecay .ne. 0.) then
            result = integrate_curve(tau_in, tau_out, current_time,
     2           arraylen, time_mdot(istart:iend), mdot(istart:iend), 
     3           kdecay)
            mdot_total(isource, 2) = mdot_total(isource, 1) - result
            mdot_total(n_sources + 1, 2) = mdot_total(n_sources + 1, 2)
     2        + mdot_total(isource, 2)
            write (mdot_out_unit, 120) isource, mdot_total(isource, 1),
     2           mdot_total(isource, 2)
         else
            write (mdot_out_unit, 120) isource, mdot_total(isource, 1)
         end if
      end do
      if (kdecay .ne. 0.) then
         write (mdot_out_unit, 120) n_sources + 1,
     2        mdot_total(n_sources + 1, 1), mdot_total(n_sources + 1, 2)
      else
         write (mdot_out_unit, 120) n_sources + 1,
     2        mdot_total(n_sources + 1, 1)
      end if
      last_time = current_time

 100  format ('variables = "Source" "Cumulative Moles"')
 110  format ('zone t = "Time = ', g16.9, '"')
 120  format (i3, 2(2x, g16.9))
 200  format ('variables = "Source" "Cumulative Moles" ',
     2    '"Cumulative Decay"')

      end subroutine mdot_integral
