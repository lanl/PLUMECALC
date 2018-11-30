      real*8 function interp2 (a1, a2, av, t1, t2)
!***********************************************************************
! $Id: interp2.f,v 1.1 2006/05/17 15:23:22 zvd Exp $
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

      real*8 a1, a2, av, t1, t2, adj

      adj = (dlog10 (av) - dlog10 (a1)) / (dlog10 (a2) - dlog10 (a1))
      interp2 = t1 + adj * (t2 - t1)
      return
      end
