      real*8 function integrate_curve(lower, upper, tau, 
     2     nsize, x, y, kdecay)
!***********************************************************************
! $Id: integrate_curve.f,v 1.1 2006/05/17 15:23:21 zvd Exp $
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
!     PSEUDOCODE
!
!     FOR each time in curve, starting w/ the second
!
!        IF lower limit is less than or equal to this x
!
!           Record position in array where integration begins
!           Compute interpolated value of y
!
!           EXIT loop
!
!        ENDIF lower limit is less than or equal to this x
!
!     ENDFOR each time in curve
!
!     FOR each x in curve, starting with the one found for x
!        IF upper limit is less than or equal to this x
!           Compute interpolated value of y
!           Add to integrated result this contribution
!           Exit loop for each x in curve
!        ELSE upper limit is past this x
!           Add to integrated result this contribution
!        ENDIF upper limit is less than or equal to this x
!     ENDFOR each x in curve
!***********************************************************************

      implicit none
      real*8 lower
      real*8 upper
      integer nsize
      real*8 x(nsize)
      real*8 y(nsize)
      real*8 kdecay
      real*8 tin
      real*8 tout
      real*8 alpha
      real*8 beta
      real*8 term1
      real*8 term2
      real*8 term3
      real*8 exp1
      real*8 exp2
      
      integer i
      integer ilower
      real*8 xlower
      real*8 ylower
      real*8 yupper
      real*8 tau
      real*8 epsilon_decay
      parameter (epsilon_decay = 1.d-7)

c****************Begin executable statements here ******************

      integrate_curve = 0.
!     FOR each x in curve, starting w/ the second
      lower_search: do i = 2, nsize
!        IF lower limit is less than or equal to this x
         if(lower.le.x(i)) then
!           Record position in array where integration begins
            ilower = i
!           Compute interpolated value of y
            ylower = y(i-1)+(y(i)-y(i-1))*(lower-x(i-1))/(x(i)-x(i-1))
!           EXIT loop
            exit lower_search
         end if
!        ENDIF lower limit is less than or equal to this x
      end do lower_search
!     ENDFOR each time in curve

      xlower = lower
!     FOR each x in curve, starting with the one found for x
      integration: do i = ilower, nsize
!        IF upper limit is less than or equal to this x
         if(upper.le.x(i)) then
!           Compute interpolated value of y
            yupper = y(i-1)+(y(i)-y(i-1))*(upper-x(i-1))/(x(i)-x(i-1))
!           Add to integrated result this contribution
            if(upper-xlower.gt.1.e-10) then
               if(kdecay.lt.epsilon_decay) then
                  beta = (yupper-ylower)/(upper-xlower)
                  alpha = ylower-beta*xlower
                  if (kdecay .eq. 0) then
                     integrate_curve = integrate_curve +
     2                    alpha*(upper-xlower)+
     3                    0.5*beta*(upper**2-xlower**2)
                  else
                     integrate_curve = integrate_curve +
     2                    (alpha*(upper-xlower)+
     3                    0.5*beta*(upper**2-xlower**2)) *
     4                    exp(-kdecay*(tau - (upper+xlower)/2))
                  end if
               else
                  beta = (yupper-ylower)/(upper-xlower)
c                  term1 = (yupper-beta/kdecay)/kdecay
c                  term2 = (ylower-beta/kdecay)/kdecay
                  exp1 = exp(-kdecay*(tau-upper))
                  exp2 = exp(-kdecay*(tau-xlower))
                  term1 = yupper * exp1
                  term2 = ylower * exp2
                  term3 = beta/kdecay * (exp1 - exp2)
c                  integrate_curve = integrate_curve +
c     2                 term1*exp(-kdecay*(tau-upper))-
c     2                 term2*exp(-kdecay*(tau-xlower))
                  integrate_curve = integrate_curve + (term1 - term2 -
     2                 term3) / kdecay
               end if
            end if
!           EXIT loop for each x in curve
            exit integration
!        ELSE upper limit is past this x
         else
!           Add to integrated result this contribution
            if(x(i)-xlower.gt.1.e-10) then
               if(kdecay.lt.epsilon_decay) then
                  beta = (y(i)-ylower)/(x(i)-xlower)
                  alpha = ylower-beta*xlower
                  if (kdecay .eq. 0.) then
                     integrate_curve = integrate_curve +
     2                    alpha*(x(i)-xlower)+
     2                    0.5*beta*(x(i)**2-xlower**2)
                  else
                     integrate_curve = integrate_curve +
     2                    (alpha*(x(i)-xlower)+
     3                    0.5*beta*(x(i)**2-xlower**2)) *
     4                    exp(-kdecay*(tau - (x(i)+xlower)/2))
                  end if
               else
                  beta = (y(i)-ylower)/(x(i)-xlower)
c                  term1 = (y(i)-beta/kdecay)/kdecay
c                  term2 = (ylower-beta/kdecay)/kdecay
                  exp1 = exp(-kdecay*(tau-x(i)))
                  exp2 = exp(-kdecay*(tau-xlower))
                  term1 = y(i) * exp1
                  term2 = ylower * exp2
                  term3 = beta/kdecay * (exp1 - exp2)
c                  integrate_curve = integrate_curve +
c     2                 term1*exp(-kdecay*(tau-x(i)))-
c     2                 term2*exp(-kdecay*(tau-xlower))
                  integrate_curve = integrate_curve + (term1 - term2 -
     2                 term3) / kdecay
               end if
            end if
            xlower = x(i)
            ylower = y(i)
         end if
!        ENDIF upper limit is less than or equal to this x
      end do integration
!     ENDFOR each x in curve

      return
      end function integrate_curve
