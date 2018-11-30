      subroutine indices (low, high, datav, datar, numr) 
!***********************************************************************
! $Id: indices.f,v 1.1 2006/05/17 15:23:21 zvd Exp $
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
!   PURPOSE
!   
!   Find indices of appropriate sigma and omega curves to be used in 
!   interpolation (log value method).
!
!   Developed originally for FEHM.
!    
!***********************************************************************
 
      implicit none

      real*8 datav, datar(*)
      integer low, high, numr, midpoint

C Find the T vs C curve indices

      if (datav .le. datar(1)) then
         low = 1
         high = low
         return
      else if (datav .ge. datar(numr)) then
         low = numr
         high = low
         return
      else
         low = 1
         high = numr
 100     midpoint = (low + high) / 2
         if (datav .eq. datar(midpoint)) then
            low = midpoint
            high = low
            return
         else if (datav .lt. datar(midpoint)) then
            high = midpoint
         else
            low = midpoint
         endif
         if (high - low .eq. 1) return
         goto 100
      endif

      end
