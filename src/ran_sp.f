      function ran_sp( k )
!***********************************************************************
! $Id: ran_sp.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
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
! To generate a pseudorandom number with a uniform distribution
! between 0 and 1.
!
!**********************************************************************
!
! $Id: ran_sp.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
!
! Initial implementation: Oct. 2, 1991, Programmer: BB. Robinson
!     for FRACNET
!
!**********************************************************************
!
! INTERFACES
!
! Formal Calling Parameters
!
! Identifier            Type  Use  Description
!
! k                      I    I/O  Random number seed used to
!                                  generate random number - a new
!                                  seed is computed in the process.
!
!**********************************************************************
!
! FUNCTIONAL DESCRIPTION
!
! This function computes a random number by first performing a
! remainder calculation using the mod function.  Then, it performs a
! floating point calculation dividing by the real value of the
! argument used in the mod calculation, thereby resulting in a value
! between 0 an 1 dominated by roundoff error.
!
!**********************************************************************
!
! ASSUMPTIONS AND LIMITATIONS
!
! It is assumed that the integer k passed to the function is a
! 6-digit integer.
!
!**********************************************************************
!
! REFERENCES
!
! FRACNET SRS
!
! Carnahan, B., H. A. Luther, and J. O. Wilkes, Applied Numerical
! Methods, John Wiley and Sons, Inc., NY (1969).
!
!**********************************************************************
! PSEUDOCODE
! 
! BEGIN ran_sp
! 
! Compute new value for k using remainder fun!tion mod
! Generate new random number
!
! END ran_sp
!**********************************************************************

      implicit none

      integer k
      real ran_sp

      k=mod(k*125,2796203)
      ran_sp=real(k)/2796203.0

      return
      end

