      subroutine mdot_integral(current_time,isource,istart,iend)
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
      real*8 r1, r2
      real*8, allocatable :: mdot_last(:,:)

      save last_time, mdot_last
      
   !****************Begin executable statements here ******************

      if (.not. allocated(mdot_total)) then
         mdot_out_unit = open_file(mdot_out_file, 1, 'unknown')
         if (kdecay .ne. 0.) then
            allocate (mdot_total(n_sources + 1, 2))
            allocate (mdot_last(n_sources + 1, 2))
            write (mdot_out_unit, 200)
         else
            allocate (mdot_total(n_sources + 1, 1))
            allocate (mdot_last(n_sources + 1, 1))
            write (mdot_out_unit, 100) 
         end if
         mdot_last = 0.
      end if
      
!   FOR each source
      if (isource .eq. 1) then
         write (mdot_out_unit, 110) current_time
         mdot_total = 0.
      end if
      arraylen = iend - istart + 1
      tau_in = 0.
      tau_out = current_time
      result = integrate_curve(tau_in, tau_out, current_time,
     2     arraylen, time_mdot(istart:iend), mdot(istart:iend), 
     3     0.d0)
      mdot_total(isource, 1) = result
      mdot_total(n_sources + 1, 1) = mdot_total(n_sources + 1, 1) +
     2     mdot_total(isource, 1)
      r1 = mdot_total(isource, 1) - mdot_last(isource, 1)
      if (kdecay .ne. 0.) then
         result = integrate_curve(tau_in, tau_out, current_time,
     2        arraylen, time_mdot(istart:iend), mdot(istart:iend), 
     3        kdecay)         
         mdot_total(isource, 2) = mdot_total(isource, 1) - result
         mdot_total(n_sources + 1, 2) = mdot_total(n_sources + 1, 2)
     2        + mdot_total(isource, 2)
         r2 = mdot_total(isource, 2) - mdot_last(isource, 2)
         write (mdot_out_unit, 220) isource, r1, 
     2        mdot_total(isource, 1), r2, mdot_total(isource, 2), result
      else
         write (mdot_out_unit, 120) isource, r1, mdot_total(isource, 1) 
      end if
      if (isource .eq. n_sources) then
         r1 = mdot_total(n_sources + 1, 1) - mdot_last(n_sources + 1, 1)
         if (kdecay .ne. 0.) then
            r2 = mdot_total(n_sources + 1, 2) - 
     2           mdot_last(n_sources + 1, 2)
            result = mdot_total(n_sources + 1, 1) - 
     2           mdot_total(n_sources + 1, 2)
            write (mdot_out_unit, 230) r1, mdot_total(n_sources + 1, 1),
     2           r2, mdot_total(n_sources + 1, 2), result
         else
            write (mdot_out_unit, 130) r1, mdot_total(n_sources + 1, 1)
         end if
         mdot_last = mdot_total
      end if

 100  format ('variables = "Source #" "Moles Input" "Cumulative Moles"')
 110  format ('zone t = "Time = ', g16.9, 'days"')
 120  format (9x, i7, 2(2x, g16.9))
 130  format ('text t = "Total ', 2(2x, g16.9), '"') 
 200  format ('variables = "Source" "Moles Input" "Cumulative Moles" ',
     2    '"Moles Decayed" "Cumulative Decayed" "Cumulative UnDecayed"')
 220  format (9x, i7, 5(2x, g16.9))
 230  format ('text t = "Total ', 5(2x, g16.9), '"')

      end subroutine mdot_integral
