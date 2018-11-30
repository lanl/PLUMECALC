      function sg_ijk_to_sg_cell(i,j,k,icell)
      use comparttr_sg
!
      implicit none
!
      integer i, j, k, icell
      integer sg_ijk_to_sg_cell
!
      sg_ijk_to_sg_cell = 
     1   ((i-1)+1) +
     2   ((j-1)*nxyz(icell,1)) +
     3   ((k-1)*nxyz(icell,1)*nxyz(icell,2)) +
     4     ip_touched_cells_sg(icell)
      return
      end
