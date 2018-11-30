      module comgrid_sg
!***********************************************************************
!***********************************************************************
!     Module containing grid information variables for subgrid
!     calculations.
!
!     Identifier      Type     Description
!
!     n_grid_points_sg  integer  number of points (cells) in the grid
!     lda_sg            integer  input flag denoting the format of the 
!                                stor file: 1 - ASCII, 2 - UNFORMATTED
!     x_sg, y_sg, z_sg  real*8   coordinates of each grid cell (center of
!                                cell), arrays of size n_grid_points_sg
!     sx1_sg            real*8   volume of each cell (m^3), array of size 
!                                n_grid_points_sg
!     izonef_sg         integer  array denoting the zone number associated
!                                with each grid cell, array of size 
!                                n_grid_points_sg
!***********************************************************************

      integer n_grid_points_sg
      integer lda_sg
      real*8,  allocatable :: x_sg(:)
      real*8,  allocatable :: y_sg(:)
      real*8,  allocatable :: z_sg(:)
      real*8,  allocatable :: sx1_sg(:)
      integer, allocatable :: izonef_sg(:)


      end module comgrid_sg
