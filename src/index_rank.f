      subroutine index_rank(n_value,indx,irank)
      implicit none
      integer n_value, i
      integer indx(n_value), irank(n_value)
      do i = 1, n_value
         irank(indx(i)) = i
      enddo
      return
      end
