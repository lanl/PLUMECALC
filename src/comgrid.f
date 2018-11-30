      module comgrid
!***********************************************************************
! $Id: comgrid.f,v 1.1 2006/05/17 15:23:19 zvd Exp $
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
!     Module containing grid information variables
!
!     Identifier      Type     Description
!
!     n_grid_points   integer  number of points (cells) in the grid
!     lda             integer  input flag denoting the format of the 
!                                stor file: 1 - ASCII, 2 - UNFORMATTED
!     x, y, z         real*8   coordinates of each grid cell (center of
!                                cell), arrays of size n_grid_points
!     sx1             real*8   volume of each cell (m^3), array of size 
!                                n_grid_points
!     izonef          integer  array denoting the zone number associated
!                                with each grid cell, array of size 
!                                n_grid_points
!     iflux           integer  input flag denoting the format of the 
!                                flux file: 1 - ASCII, 2 - UNFORMATTED
!     ncont           integer  number of indices and nodal connections 
!     nelm            integer  information about nodes in each element,
!                                array of size ncont
!
!***********************************************************************

      integer n_grid_points
      integer lda
      integer iflux
      integer ncont
      integer, allocatable :: izonef(:)
      integer, allocatable :: nelm(:)
      real*8, allocatable :: x(:)
      real*8, allocatable :: y(:)
      real*8, allocatable :: z(:)
      real*8, allocatable :: sx1(:)

      end module comgrid
