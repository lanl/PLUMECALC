      subroutine fill_ip_touched_cells_sg()

      use comparttr   ,  only : n_touched_cells
      use comparttr_sg
      
      implicit none
      
      integer icell
      
      ip_touched_cells_sg(1) = 0
      do icell = 2, n_touched_cells
         ip_touched_cells_sg(icell) = 
     1          (nxyz(icell-1,1) *
     2           nxyz(icell-1,2) *
     3           nxyz(icell-1,3))+ 
     4           ip_touched_cells_sg(icell-1)
      enddo
      
      n_touched_cells_sg_max = 
     1          (nxyz(n_touched_cells,1) *
     2           nxyz(n_touched_cells,2) *
     3           nxyz(n_touched_cells,3))+
     4           ip_touched_cells_sg(n_touched_cells)
      return
      end subroutine fill_ip_touched_cells_sg
