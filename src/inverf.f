      function inverf(x)
!***********************************************************************
! $Id: inverf.f,v 1.1 2006/05/17 15:23:22 zvd Exp $
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
!***********************************************************************
!
!  PURPOSE
!
!  This subroutine calculates the inverse of the error function
!  for a value of x between 0 and 1.  It does so using linear 
!  interpolation between chosen values of the erf(x) curve.
!
!***********************************************************************
!
! Initial implementation: 03/16/95, Programmer: B. Robinson
!     for FEHM V1.0
!
!***********************************************************************

      implicit none

      integer i
      real inverf,ef(8),a(8),b(8), x
      data ef/.2227,.42839,.60386,.79691,.91031,.97635,.99532,1./
      data a/.8980691,.972337,-1.255877,-1.033911,-0.8452046,
     +     -0.6909501,-0.5685161,-0.4299497/
      data b/0.,-1.653945e-2,9.494722e-2,0.1842109,0.3148551,0.476399,
     +     0.6754972,0.9983227/

      if(x.lt.0.) x=-x
      if(x.gt.1.) then
         write(6,110)
 110     format(1x,'value of x greater than 1')
         return
      end if
      do i=1,2
         if(x.lt.ef(i)) then
            inverf=a(i)*x+b(i)
            return
         end if
      enddo
      do i=3,7
         if(x.lt.ef(i)) goto 11
      enddo
      i=8
 11   inverf=a(i)*alog10(1.-x)+b(i)

      return
      end
