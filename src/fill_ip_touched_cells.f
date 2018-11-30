      subroutine fill_ip_touched_cells()

      use comparttr_sg
      use comparttr
      use comgrid, only  : n_grid_points
      use comunits, only : error_unit_number

      implicit none

      integer  k
      
      if(n_touched_cells .gt. n_grid_points)then
         write(error_unit_number,*  )'ERROR: fill_ip_touched_cells'
         write(error_unit_number,100)n_touched_cells, n_grid_points
  100    format ('n_touched_cells = ',i8,' n_grid_points = ', i8)
         write(error_unit_number,*  )'ERROR: fill_ip_touched_cells'
      endif
       
      do k = 1, n_grid_points
        ip_touched_cells(k) = 0
      enddo
      
      do k = 1, n_touched_cells
        ip_touched_cells(touched_cells(k)) = k
      enddo
      
      return
      end subroutine fill_ip_touched_cells
