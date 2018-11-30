      subroutine time_delay (transflag, ith,cur_node,
     2     par1v, par2v, par3v, fm,
     2     ret_factor, rseed, tau_zero, concv, timev)
!***********************************************************************
! $Id: time_delay.f,v 1.1 2006/05/17 15:23:25 zvd Exp $
!***********************************************************************
!  Copyright, 2004,  The  Regents  of the  University of California.
!  This program was prepared by the Regents of the University of 
!  California at Los Alamos National Laboratory (the University) under  
!  contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). 
!  All rights in the program are reserved by the DOE and the University. 
!  Permission is granted to the public to copy and use this software 
!  without charge, provided that this Notice and any statement of 
!  authorship are reproduced on all copies. Neither the U.S. Government 
!  nor the University makes any warranty, express or implied, or 
!  assumes any liability or responsibility for the use of this software.
!**********************************************************************
!
! PURPOSE
!
! To calculate particle time delay using type curves. 
! (such as Sudicky and Frind)
!
!**********************************************************************
!
! Initial implementation: 05-MAY-99, Programmer: Zora Dash
!     for FEHM Version 2.10 [10086-STN-2.10-00]
! 
!**********************************************************************

      use comunits, only : error_unit_number
      use compfrac
      implicit none

      real*8  par1v, par2v, par3v, tau_zero, timev, concv, timedelay
      real*8 interp
      real*8 ret_factor
      real ran_sp
      integer fm
      integer rseed
      integer transflag
      real inverf, fact_term, concvr
      integer ith, cur_node
      integer ierr, iptty


      ierr = error_unit_number
      iptty = 6

      if(transflag.eq.1.or.transflag.eq.2) then
         
         timev = par2v*tau_zero
      else

! Generate random concentration value
         concv = ran_sp (rseed)


         if(par1v.lt.0.) then
c     Error function solution - infinite fracture spacing

            concvr = concv
            timedelay = ret_factor + 
     2           par2v*tau_zero/(par1v**2*inverf(concvr)**2)
            timev = timedelay*tau_zero
         else
            if (.not. pfrac_read) then
               write (ierr, 10) 
!               write (iout, 10) 
               if (iptty .gt. 0) write (iptty, 10) 
               stop
            else
! Find time delay
               timedelay = interp (ith, cur_node,
     2              par1v, par2v, par3v, fm, concv)
! Compute adjusted time
               timev = tau_zero * (timedelay + ret_factor)
            end if
         end if
      end if
 10   format ("Dispersion type curve data not input")
      end

