      subroutine fill_id_parent_sg()

      use comgrid_sg  ,  only : n_grid_points_sg
      use comgrid     ,  only : n_grid_points
      use comunits    ,  only : error_unit_number
      use comparttr_sg

      implicit none

      integer ii, i, j, k, icell_sg

      integer local_debug
      data    local_debug / 0 /
      
      allocate(id_parent_sg(n_grid_points_sg))
      id_parent_sg=0

      icell_sg = 0
      do ii = 1, n_grid_points
        if(ip_touched_cells(ii) .ne. 0)then
          k = 1
          do while (k .le. nxyz(ip_touched_cells(ii),3))
            j = 1
            do while (j .le. nxyz(ip_touched_cells(ii),2))
              i = 1
              do while (i .le. nxyz(ip_touched_cells(ii),1))
                icell_sg = icell_sg + 1
                i = i + 1
                id_parent_sg(icell_sg) = ii
              enddo
              j = j + 1
            enddo
            k = k + 1
          enddo
        else
          icell_sg = icell_sg + 1
          id_parent_sg(icell_sg) = ii
        endif
      enddo


      if(local_debug .ne. 0)then
      icell_sg = 0
      do ii = 1, n_grid_points
        if(ip_touched_cells(ii) .ne. 0)then
          k = 1
          do while (k .le. nxyz(ip_touched_cells(ii),3))
            j = 1
            do while (j .le. nxyz(ip_touched_cells(ii),2))
              i = 1
              do while (i .le. nxyz(ip_touched_cells(ii),1))
                icell_sg = icell_sg + 1
                i = i + 1
                write(error_unit_number,*)
     1          '  touched id_parent_sg(icell_sg) ',ii,icell_sg
              enddo
              j = j + 1
            enddo
            k = k + 1
          enddo
        else
          icell_sg = icell_sg + 1
                write(error_unit_number,*)
     1          'untouched id_parent_sg(icell_sg) ',ii,icell_sg
        endif
      enddo
      endif
      
      return
      end subroutine fill_id_parent_sg
