      subroutine sort_values(n, nsize, indx, value)
!***********************************************************************
! $Id: sort_values.f,v 1.1 2006/05/17 15:23:25 zvd Exp $
!**********************************************************************
!
! PURPOSE
!
! To sort parameter values to facillitate finding of closest transfer
! function curve.
!
!**********************************************************************
!
! REVISION HISTORY
!
! Modified from:
! FEHM Version 2.20 [10086-STN-2.20-00] 
! Initial implementation: 15-DEC-02, Programmer: Bruce Robinson
!
!**********************************************************************
!
!     Indexing and Ranking algorithm for sorting, from Numerical Recipes
!     Press, W. H., B. P. Flannery, S. A. Teukolsky, and W. T.
!     Vetterling, 1986, Numerical Recipes. The Art of Scientific
!     Computing, Cambridge University Press, Cambridge, pp. 232-234.

      implicit none
      integer nsize
      integer n, j, l, ir, indxt, i
      integer indx(nsize)
      real(8) value(nsize), q

      do j = 1, n
         indx(j) = j
      end do
      l = n/2+1
      ir=n
 10   continue
      if(l.gt.1) then
         l=l-1
         indxt=indx(l)
         q=value(indxt)
      else
         indxt=indx(ir)
         q=value(indxt)
         indx(ir)=indx(1)
         ir=ir-1
         if(ir.eq.1) then
            indx(1)=indxt
            return
         end if
      end if
      i=l
      j=l+l
 20   if(j.le.ir) then
         if(j.lt.ir) then
            if(value(indx(j)).lt.value(indx(j+1))) j=j+1
         end if
         if(q.lt.value(indx(j))) then
            indx(i)=indx(j)
            i=j
            j=j+j
         else
            j=ir+1
         end if
         goto 20
      end if
      indx(i)=indxt
      goto 10
      end
