      subroutine initialize_sg( )
!
      use comgrid
      use comgrid_sg
      use comparttr
      use comparttr_sg
      use comunits, only : error_unit_number
!
      implicit none
!
      integer local_debug
      data    local_debug / 0 /
!
      if(local_debug .ne. 0)then
         write(error_unit_number, *) 'Enter:initialize_sg '
      endif
!
      allocate(corn               (n_grid_points,3))
      allocate(dxyz               (n_grid_points,3))
      allocate(ip_touched_cells   (n_grid_points))
      allocate(scale_xyz          (n_touched_cells,3))
      allocate(nxyz               (n_touched_cells,3))
      allocate(ip_touched_cells_sg(n_touched_cells))

      allocate(ggg                (n_grid_points,-3:3))
c      allocate(ps_trac                (n_grid_points))

      corn=0.
      dxyz=0.
      ip_touched_cells=0
      scale_xyz=0.
      nxyz=0
      ip_touched_cells_sg=0
      ggg=0.
c      ps_trac = 1.

!
!     Set default values
!
      scale_xyz = 1.0d20
      nxyz      = 1
      
      if(local_debug .ne. 0)then
         write(error_unit_number, *) 'Exit: initialize_sg'
      endif

      return
      end subroutine initialize_sg
      
