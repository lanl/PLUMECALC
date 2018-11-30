      subroutine unique_elements(i, j, n, nu)
!
!     Given an integer vector i(n), sort the vector
!     in ascending order and then fill the integer vector
!     j(nu) with the ascending unique entries of j(n).
!
!     Input:  i ,n
!     Output: j, nu
!
      implicit none
      integer n, nu, k, iflag
      integer i(n), j(n)
!
!     Fill j with i
!      
      do k = 1, n
         j(k) = i(k)
      enddo
!      
!     Sort and reorder j in ascending order
!      
      iflag = 1
      call isort(j,j,n,iflag)
!
!     Compact the entries in j so that only the unique
!     entries are kept.
!
      nu = 1
      do k = 2, n
        if(j(k) .ne. j(k-1))then
           nu = nu + 1
           j(nu) = j(k)
        endif
      enddo
!
!     Fill the extra space at the end, if it exists, with
!     a value 1 smaller than the smallest j(1) entry.
      if(nu+1 .lt. n)then
      do k = nu+1, n
        j(k) = j(1) - 1
      enddo
      endif
      
      return
      end subroutine unique_elements
