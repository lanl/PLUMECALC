      real *8 function ftime (iparam1, jparam2, kparam3, fm, concv)
!***********************************************************************
! $Id: ftime.f,v 1.1 2006/05/17 15:23:21 zvd Exp $
!**********************************************************************
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
!   PURPOSE
!   
!   To calculate particle time delay using type curves.
!
!   Developed originally for FEHM.
!  
!***********************************************************************

      use compfrac
      implicit none

      real*8 concv, cadj, ctime, concv_sc
      integer fm, iparam1, jparam2, kparam3, low, high, midpoint, numv
      
      numv = nump (iparam1, jparam2, kparam3, fm)
      if (numparams .le. 2) then
! Don't scale for Sudicky-Frind curves
         concv_sc = concv
      else
         concv_sc = concv*conc(iparam1, jparam2, kparam3, fm, numv)
      endif
      if (concv_sc .le. conc(iparam1, jparam2, kparam3, fm, 1)) then
         low = 1
         high = 1
         ftime = dtime(iparam1, jparam2, kparam3, fm, 1)
         if(numparams.le.2) then
            ftime = dlog10(ftime)
         end if
         return
      else if (concv_sc .ge. conc(iparam1, jparam2, kparam3, fm, numv)) 
     .        then
         low = numv
         high = numv
         ftime = dtime(iparam1, jparam2, kparam3, fm, numv)
         if(numparams.le.2) then
            ftime = dlog10(ftime)
         end if
         return
      else
         low = 1
         high = numv
 20      midpoint = (low + high) / 2
         if (concv_sc .eq. 
     .        conc(iparam1, jparam2, kparam3, fm, midpoint)) 
     .        then
            low = midpoint
            high = low
            ftime = dtime(iparam1, jparam2, kparam3, fm, 
     .           midpoint)
            if(numparams.le.2) then
               ftime = dlog10(ftime)
            end if
            return
         else if (concv_sc .lt. 
     .           conc(iparam1, jparam2, kparam3, fm, midpoint)) then
            high = midpoint
         else
            low = midpoint
         end if
         if (high - low .eq. 1) goto 30
         goto 20
      endif

 30   continue
      cadj = (concv_sc - conc(iparam1, jparam2, kparam3, fm, low)) / 
     .     (conc(iparam1, jparam2, kparam3, fm, high) - 
     .     conc(iparam1, jparam2, kparam3, fm, low))
      if(numparams.le.2) then
         ctime = dlog10 (dtime(iparam1, jparam2, kparam3, fm, low)) + 
     .        cadj * (dlog10(dtime(iparam1, jparam2, kparam3, fm, high))
     .        - dlog10 (dtime(iparam1, jparam2, kparam3, fm, low)))
         ftime = ctime
      else
         ctime = dtime(iparam1, jparam2, kparam3, fm, low) + 
     .        cadj * (dtime(iparam1, jparam2, kparam3, fm, high) -
     .        dtime(iparam1, jparam2, kparam3, fm, low))
         ftime = ctime
      end if

      return
      end

