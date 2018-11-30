      subroutine fill_touched_cells_sg
c s kelkar May 8, 06
c fill the arrays cell_path_sg and touched_cells_sg
c needed for computing concen
c******************************************************************** 
!     cell_path_sg       integer  array denoting whether a particle has 
!                                passed through the grid cell, array of
!                                size n_grid_points_sg 
!     n_touched_cells_sg integer  number of cells that particles have
!                                passed through
!     touched_cells_sg   integer  array containing the condensed version 
!                                 of cell_path, i.e. an array of only 
!                                 those cells that have particles 
!                                 passing through them, array of size 
!                                 n_touched_cells_sg 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      use comparttr
      use comparttr_sg
      use comgrid_sg

      implicit none

      integer i
      integer temp_c

      temp_c=0
      do i = 1, n_grid_points_sg
      if(cell_path_sg(i).gt.0) temp_c=temp_c+1
      end do

      do i = 1, n_packed_sg
!         Add one to the array denoting particles passing through a cell
         cell_path_sg(cell_packed_sg(i)) = 
     1        cell_path_sg(cell_packed_sg(i))+1
      end do

      n_touched_cells_sg = 0
      do i = 1, n_grid_points_sg
         if(cell_path_sg(i).ne.0) then
!           Increment counter of number of cells touched by 1
            n_touched_cells_sg = n_touched_cells_sg + 1
!           Store index of cell in cell array
            cell_path_sg(i) = n_touched_cells_sg
         end if
      end do

      do i = 1, n_grid_points_sg
         if(cell_path_sg(i).ne.0) then
!           Store cell number in indexing array
            touched_cells_sg(cell_path_sg(i)) = i
         end if
      end do

      return
      
      end
      
c...................................................................



