      real*8 function interp4 (a1, a2, av, b1, b2, bv, t1, t2, t3, t4)
!***********************************************************************
! $Id: interp4.f,v 1.1 2006/05/17 15:23:22 zvd Exp $
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
!
!**********************************************************************
!
! Initial implementation: 05-MAY-99, Programmer: Zora Dash
!     for FEHM Version 2.10 [10086-STN-2.10-00]
!
!**********************************************************************
! Return the interpolated time value

      implicit none

      real*8 a1, a2, av, b1, b2, bv, t1, t2, t3, t4
      real*8 d1, d2, d3, d4, totald

      d1 = dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2)

      if (d1 .eq. 0) then
         interp4 = t1
         return
      end if

      d2 =  dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2)

      if (d2 .eq. 0) then
         interp4 = t2
         return
      end if
      
      d3 =  dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2)

      if (d3 .eq. 0) then
         interp4 = t3
         return
      end if

      d4 =  dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2)

      if (d4 .eq. 0) then
         interp4 = t4
         return
      end if

      d1 = 1./d1
      d2 = 1./d2
      d3 = 1./d3
      d4 = 1./d4

      totald = 1. /(d1 + d2 + d3 +d4)

      interp4 = totald * (d1*t1 + d2*t2 + d3*t3 + d4*t4)

      return
      end
