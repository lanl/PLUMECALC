      module comparttr_sg
!***********************************************************************
!  Copyright, 2002, 2004,  The  Regents of the University of California.
!  This  program  was  prepared by  the  Regents  of the  University  of
!  California  at Los Alamos National Laboratory (the  University) under
!  contract No. W-7405-ENG-36 with the U. S. Department of Energy (DOE).
!  All rights in the program are reserved by the DOE and the University.
!  Permission is  granted to the  public to copy  and  use this software
!  without  charge,  provided that this  Notice  and  any  statement  of
!  authorship are reproduced on all copies. Neither the U. S. Government
!  nor  the  University  makes any  warranty,  express  or  implied,  or
!  assumes any liability or responsibility for the use of this software.
!***********************************************************************
!     Module containing particle tracking arrays and variables
!
!     Identifier      Type     Description
!
!     if_subgrid      integer  = 0 No subgrid calculations
!                              = 1 Perform subgrid calculations
!
!     input_flag_sg   integer  = 1 Input scale_factor, compute nxyz
!                              = 2 Input nxyz, do not compute nxyz
!
!     n_packed_sg     integer  total number of subgrid cells all particles pass
!                                through
!     n_cells_sg      integer  number ofsubgrid cells that each particle passed
!                                through during simulation, array of 
!                                size npart 
!     cell_packed_sg  integer  array of subgrid cell numbers that each particle 
!                                enters, array of size total sum of 
!                                subgrid cells passed through for all particles
!     time_packed_sg  real*8   array of times that each particle enters 
!                                the corresponding subgrid cell in the 
!                                cell_packed_sg array, array of size total 
!                                sum of subgrid cells passed through for all 
!                                particles
!     adv_packed_sg      real*8   array of delta times that the particle 
!                               takes for the advective step across
!                               the cell in the cell_packed_sg array,
!                               array of size total sum of subgrid cells
!                               passed through for all particles
!     x_packed_sg     real*8   array of x coordinates that each particle enters 
!                                the corresponding subgrid cell in the 
!                                cell_packed_sg array, array of size total 
!                                sum of subgrid cells passed through for all 
!                                particles
!     y_packed_sg     real*8   array of y coordinates that each particle enters 
!                                the corresponding subgrid cell in the 
!                                cell_packed_sg array, array of size total 
!                                sum of subgrid cells passed through for all 
!                                particles
!     z_packed_sg     real*8   array of z coordinates that each particle enters 
!                                the corresponding subgrid cell in the 
!                                cell_packed_sg array, array of size total 
!                                sum of subgrid cells passed through for all 
!                                particles
!     cell_index_sg   integer  array of locations in the packed arrays 
!                                where each particle information starts,
!                                array of size npart
!     cell_path_sg    integer  array denoting whether a particle has 
!                                passed through the subgrid grid cell, array of 
!                                size n_grid_points_sg 
!     n_touched_cells_sg integer  number of subgrid cells that particles have 
!                                passed through
!     n_touched_cells_sg_max integer  max possible number of subgrid cells that particles have 
!                                passed through
!     touched_cells_sg integer array containing the condensed version of
!                                cell_path_sg, i.e. an array of only those 
!                                subgrid cells that have particles passing 
!                                through them, array of size 
!                                n_touched_cells_sg
!     concentration_sg real*8  array of computed concentrations for the
!                                cells in the compact array, array of 
!                                size n_touched_cells_sg
!     conc_mobile_sg   real*8  array of computed mobile concentrations
!                                for the cells in the compact array, 
!                                array of size n_touched_cells_sg
!     conc_total_sg    real*8  array of computed total concentrations 
!                                for the cells in the compact array, 
!                                array of size n_touched_cells_sg
!     conc_mobtot_sg   real*8  array of computed total mobile concentrations
!                                for the cells in the compact array, 
!                                array of size n_touched_cells_sg
!     nxyz             integer array of size (n_touched_cells,3) containing the
!                                number of subgrid refinements for each of the
!                                touched cells. 
!     ip_touched_cells integer array of size n_grid_points which contains pointers
!                                for stepping through the touched_cells array in
!                                ascending order. For example: 
!                                nxyz(ip_touched_cells(1),1) is the number of subgrid
!                                refinements of the grid cell with the lowest cell number.
!     ip_touched_cells_sg integer  array of size n_grid_points_sg which contains pointers
!                                for stepping through the touched_cells_sg array in
!                                ascending order. For example: 
!                                nxyz(ip_touched_cells_sg(1),1) is the number of subgrid
!                                refinements of the grid cell with the lowest cell number.
!     ip_cell_to_cell_sg integer xxxxx
!     corn              real*8 control volume corner point coordinates from SPTR
!     dxyz              real*8 control volume edge lenght from SPTR
!     scale_xyz         real*8 Array of length scale factors for each of the touched
!                                cells. Subgrid refinement insures that the number of
!                                cell subdivisions will bring the cell dimensions down
!                                to the scale length. Array is size (n_touched_cells,3)
!     i4_work          integer allocatable integer work array
!     r8_work          real*8  allocatable real*8 work array
!     boundary_coord   real*8  fixed lenght work array
!
!***********************************************************************

      integer if_subgrid
      integer input_flag_sg
      integer n_packed_sg
      integer, allocatable :: n_cells_sg(:)          ! size npart
      integer, allocatable :: id_parent_sg(:)        ! size n_grid_points_sg
c      integer, allocatable :: cell_packed_sg(:)      ! size n_packed_sg
c      real*8,  allocatable :: time_packed_sg(:)      ! size n_packed_sg
c      real*8,  allocatable :: x_packed_sg(:)         ! size n_packed_sg
c      real*8,  allocatable :: y_packed_sg(:)         ! size n_packed_sg
c      real*8,  allocatable :: z_packed_sg(:)         ! size n_packed_sg
      INTEGER, POINTER, DIMENSION(:) :: cell_packed_sg => null()
      REAL*8, POINTER, DIMENSION(:) :: time_packed_sg => null()
      REAL*8, POINTER, DIMENSION(:) :: adv_packed_sg => null()
      REAL*8, POINTER, DIMENSION(:) :: x_packed_sg => null()
      REAL*8, POINTER, DIMENSION(:) :: y _packed_sg => null()
      REAL*8, POINTER, DIMENSION(:) :: z _packed_sg => null()

      integer, allocatable :: cell_index_sg(:)       ! size npart
      integer, allocatable :: cell_path_sg(:)        ! size n_grid_points_sg
      integer n_touched_cells_sg
      integer n_touched_cells_sg_max
      integer, allocatable :: touched_cells_sg(:)    ! size n_touched_cells_sg
      real*8,  allocatable :: concentration_sg(:)    ! size n_touched_cells_sg
      real*8,  allocatable :: conc_mobile_sg(:)      ! size n_touched_cells_sg
      real*8,  allocatable :: conc_total_sg(:)       ! size n_touched_cells_sg
      real*8,  allocatable :: conc_mobtot_sg(:)      ! size n_touched_cells_sg
      integer, allocatable :: nxyz(:,:)              ! size n_touched_cells
      integer, allocatable :: ip_touched_cells(:)    ! size n_grid_points
      integer, allocatable :: ip_touched_cells_sg(:) !  size n_grid_points
      integer, allocatable :: ip_cell_to_cell_sg(:)  !  size n_grid_points
      real*8,  allocatable :: corn(:,:)              !  size n_grid_points
      real*8,  allocatable :: dxyz(:,:)              !  size n_grid_points
      real*8,  allocatable :: scale_xyz(:,:)         !  size n_touched_cells
      integer, allocatable :: i4_work(:)
      real*8,  allocatable :: r8_work(:)
      real*8   boundary_cord(6)                   !  size 6

      real*8,  allocatable :: ggg(:,:)               !  size n_grid_points
      real*8,  allocatable :: ps_trac(:)               !  size n_grid_points
      real*8,  allocatable :: water_flux_sg(: )      !  size n_grid_points_sg

c.....................................................
      integer  n_packed_sg_max
      integer n_packed_sg_tmp
      real*8 size_fac_initial
      real*8 size_fac_increament
      integer resize_counter
      parameter (size_fac_initial=.005)
      parameter (size_fac_increament=1.1)      
c.................................................
      logical flag_new_cell
c.................................................

      end module comparttr_sg
