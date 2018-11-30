      subroutine vector_to_sg_vector_r8(r,r_sg)
!
      use comparttr_sg
      use comgrid,    only : n_grid_points
      use comgrid_sg, only : n_grid_points_sg
!
      implicit none
!
      real*8 r(n_grid_points), r_sg(n_grid_points_sg)
      integer ii, i, j, k, icount
!
!     loop over all cells, refined or not filling r_sg with r values     
!
      icount = 1
      do ii = 1, n_grid_points
       if(ip_touched_cells(ii) .ne. 0)then
        k=1
        do while (k .le. nxyz(ip_touched_cells(ii),3))
        j=1
        do while (j .le. nxyz(ip_touched_cells(ii),2))
        i=1
        do while (i .le. nxyz(ip_touched_cells(ii),1))
        r_sg(icount) = r(ii)
        icount = icount + 1
        i = i + 1
        enddo
        j = j + 1
        enddo
        k = k + 1
        enddo
       else
        r_sg(icount) = r(ii)
        icount = icount + 1
       endif
      enddo
!
!     End loop over all cells ip_touched_cells      
!
      return
      end

      subroutine vector_to_sg_vector_i4(i4,i4_sg)
!
      use comparttr_sg
      use comgrid,    only : n_grid_points
      use comgrid_sg, only : n_grid_points_sg
!
      implicit none
!
      integer i4(n_grid_points), i4_sg(n_grid_points_sg)
      integer ii, i, j, k, icount

!
!     loop over all cells, refined or not filling i_sg with i values     
!
      icount = 1
      do ii = 1, n_grid_points
       if(ip_touched_cells(ii) .ne. 0)then
        k=1
        do while (k .le. nxyz(ip_touched_cells(ii),3))
        j=1
        do while (j .le. nxyz(ip_touched_cells(ii),2))
        i=1
        do while (i .le. nxyz(ip_touched_cells(ii),1))
        i4_sg(icount) = i4(ii)
        icount = icount + 1
        i = i + 1
        enddo
        j = j + 1
        enddo
        k = k + 1
        enddo
       else
        i4_sg(icount) = i4(ii)
        icount = icount + 1
       endif
      enddo
!
!     End loop over all cells ip_touched_cells      
!
      return
      end
      subroutine vector_to_scale_sg_vector_r8(r,r_sg)
!
      use comparttr_sg
      use comgrid,    only : n_grid_points
      use comgrid_sg, only : n_grid_points_sg
!
      implicit none
!
      real*8 r(n_grid_points), r_sg(n_grid_points_sg)
      real*8  scale_factor
      integer ii, i, j, k, icount

!
!     loop over all cells, refined or not filling r_sg with r values
!     Scale values by 1/nx*ny*nz of the subgrid cell.    
!
      icount = 1
      do ii = 1, n_grid_points
       if(ip_touched_cells(ii) .ne. 0)then
        k=1
        do while (k .le. nxyz(ip_touched_cells(ii),3))
        j=1
        do while (j .le. nxyz(ip_touched_cells(ii),2))
        i=1
        do while (i .le. nxyz(ip_touched_cells(ii),1))
        
        scale_factor = 1.0d0/(nxyz(ip_touched_cells(ii),1)*
     1                        nxyz(ip_touched_cells(ii),2)*
     2                        nxyz(ip_touched_cells(ii),3))
        r_sg(icount) = scale_factor*r(ii)
        icount = icount + 1
        i = i + 1
        enddo
        j = j + 1
        enddo
        k = k + 1
        enddo
       else
        r_sg(icount) = r(ii)
        icount = icount + 1
       endif
      enddo
!
!     End loop over all cells ip_touched_cells      
!
      return
      end

