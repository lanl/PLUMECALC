      real*8 function interp8 (a1, a2, av, b1, b2, bv, c1, c2, cv,
     .     t1, t2, t3, t4, t5, t6, t7, t8)
!***********************************************************************
! $Id: interp8.f,v 1.1 2006/05/17 15:23:22 zvd Exp $
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
! Initial implementation: 13-NOV-02, Programmer: Zora Dash
!     for FEHM Version 2.10 [10086-STN-2.10-00]
! 
!**********************************************************************
! Return the interpolated time value

      implicit none

      real*8 a1, a2, av, b1, b2, bv, c1, c2, cv
      real*8 t1, t2, t3, t4, t5, t6, t7, t8
      real*8 d1, d2, d3, d4, d5, d6, d7, d8, totald

      d1 = dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2 + 
     .     (dlog10 (c1) - dlog10(cv))**2)

      d2 = dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2 + 
     .     (dlog10 (c2) - dlog10(cv))**2)

      d3 = dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2 + 
     .     (dlog10 (c1) - dlog10(cv))**2)

      d4 = dsqrt( (dlog10 (a1) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2 + 
     .     (dlog10 (c2) - dlog10(cv))**2)

      d5 = dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2 + 
     .     (dlog10 (c1) - dlog10(cv))**2)

      d6 = dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b1) - dlog10(bv))**2 + 
     .     (dlog10 (c2) - dlog10(cv))**2)

      d7 = dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2 + 
     .     (dlog10 (c1) - dlog10(cv))**2)

      d8 = dsqrt( (dlog10 (a2) - dlog10(av))**2 +
     .     (dlog10 (b2) - dlog10(bv))**2 + 
     .     (dlog10 (c2) - dlog10(cv))**2)

      if (d1 .eq. 0) then
         interp8 = t1
         return
      end if

      if (d2 .eq. 0) then
         interp8 = t2
         return
      end if

      if (d3 .eq. 0) then
         interp8 = t3
         return
      end if

      if (d4 .eq. 0) then
         interp8 = t4
         return
      end if

      if (d5 .eq. 0) then
         interp8 = t5
         return
      end if

      if (d6 .eq. 0) then
         interp8 = t6
         return
      end if

      if (d7 .eq. 0) then
         interp8 = t7
         return
      end if

      if (d8 .eq. 0) then
         interp8 = t8
         return
      end if

      d1 = 1./d1
      d2 = 1./d2
      d3 = 1./d3
      d4 = 1./d4
      d5 = 1./d5
      d6 = 1./d6
      d7 = 1./d7
      d8 = 1./d8

      totald = 1. /(d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8)

      interp8 = totald * (d1*t1 + d2*t2 + d3*t3 + d4*t4
     .     + d5*t5 + d6*t6 + d7*t7 + d8*t8)

      return
      end
