      subroutine allocmem_subgrid()
c allocate some subgrid arrays
c note that n_touched_cells_sg_max is set = # of sg cells in
c fill_ip_touched_cells_sg.f, since we wont know its real value
c until all the calculations are done. Simillarly, 
c n_packed_sg_max= n_touched_cells_sg_max*$particles as a priliminary
c guess at this point. Later, all these arryas are copied into
c smaller arrays and deallocated.
      
      use comunits
      use comparttr
      use comparttr_sg
      use comgrid_sg
      
      implicit none

c      integer n_packed_sg_max,n_packed_sg_tmp
      integer n_packed_sg_ini

      n_packed_sg_ini=n_packed
      n_packed_sg_max = n_touched_cells_sg_max*npart
c      n_packed_sg_tmp = n_packed_sg_max * size_fac_initial

      n_packed_sg_tmp = n_packed_sg_ini

c      allocate(x_sg(n_grid_points_sg))
c      allocate(y_sg(n_grid_points_sg))
c      allocate(z_sg(n_grid_points_sg))

      allocate (x_packed_sg(n_packed_sg_tmp))
      allocate (y_packed_sg(n_packed_sg_tmp))
      allocate (z_packed_sg(n_packed_sg_tmp))
      allocate (time_packed_sg(n_packed_sg_tmp))
      allocate (adv_packed_sg(n_packed_sg_tmp))
      allocate (cell_packed_sg(n_packed_sg_tmp))

      allocate (n_cells_sg(npart))
      allocate (cell_index_sg(npart))
      allocate (cell_path_sg(n_grid_points_sg))
      allocate (touched_cells_sg(n_touched_cells_sg_max))

      allocate(concentration_sg(n_touched_cells_sg_max))
      allocate(conc_mobile_sg(n_touched_cells_sg_max))
      allocate(conc_total_sg(n_touched_cells_sg_max))
      allocate(izonef_sg(n_grid_points_sg))
c      allocate(id_parent_sg(n_grid_points_sg))
      
      allocate(water_flux_sg      (n_grid_points_sg))

      x_packed_sg=0.
      y_packed_sg=0.
      z_packed_sg=0.
      time_packed_sg=0.

      cell_packed_sg=0
      n_cells_sg=0
      cell_index_sg=0
      cell_path_sg=0
      touched_cells_sg=0

      concentration_sg=0.
      conc_mobile_sg=0.
      conc_total_sg=0.
      izonef_sg=0

      water_flux_sg=0.

      return
      end subroutine allocmem_subgrid
