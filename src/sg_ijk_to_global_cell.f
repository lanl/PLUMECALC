      function sg_ijk_to_global_cell(i,j,k,icell)
      use comparttr_sg
!
      implicit none
!
      integer i, j, k, icell
      integer sg_ijk_to_global_cell
!
      if(ip_touched_cells(icell) .ne. 0)then
        sg_ijk_to_global_cell = 
     1   ((i-1)+1) +
     2   ((j-1)*nxyz(ip_touched_cells(icell),1)) +
     3   ((k-1)*nxyz(ip_touched_cells(icell),1)*
     4          nxyz(ip_touched_cells(icell),2)) +
     5          (ip_cell_to_cell_sg(icell)-1)
     
      else
        sg_ijk_to_global_cell = ip_cell_to_cell_sg(icell)
      endif
      return
      end
