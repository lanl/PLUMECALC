      subroutine fill_ip_cell_to_cell_sg()
!
      use comgrid
      use comgrid_sg
      use comparttr
      use comparttr_sg
      use comunits
!
      implicit none
!
      integer ii, i, j, k, icell_sg
!
!     Create a pointer from unrefined cells numbers into refined cell numbers
!
!     Given an unrefined cell number N, ip_cell_to_cell_sg(N) will give
!     the sg cell number of sg the 1,1,1 subgrid cell.
!
      allocate(ip_cell_to_cell_sg(n_grid_points))
      ip_cell_to_cell_sg=0

      ip_cell_to_cell_sg(1)=1
      do ii = 1, n_grid_points-1
        icell_sg = 0
        if(ip_touched_cells(ii) .ne. 0)then
          k=1
          do while (k .le. nxyz(ip_touched_cells(ii),3))
            j=1
            do while (j .le. nxyz(ip_touched_cells(ii),2))
              i=1
              do while (i .le. nxyz(ip_touched_cells(ii),1))
                icell_sg = icell_sg + 1
                i = i + 1
              enddo
              j = j + 1
            enddo
            k = k + 1
          enddo
        else
          icell_sg = icell_sg + 1
        endif
        ip_cell_to_cell_sg(ii+1)=ip_cell_to_cell_sg(ii)+icell_sg
      enddo
      return
      end subroutine fill_ip_cell_to_cell_sg
