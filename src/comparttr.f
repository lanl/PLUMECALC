      module comparttr
!***********************************************************************
! $Id: comparttr.f,v 1.3 2006/09/20 19:48:05 zvd Exp $
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
!     npart           integer  number of particles in the simulation 
!                                (input in simulation input file, as 
!                                opposed to n_particles, input in the  
!                                sptr output file. Code checks to make  
!                                sure they are the same)
!     start_no        integer  starting particle number for each source,
!                                array of size n_sources
!     end_no          integer  ending particle number for each source, 
!                                array of size n_sources      
!     step_no         integer  step in do loop for particles associated 
!                                with each source, array of size 
!                                n_sources
!     n_part_source   integer  number of particles associated with each 
!                                source, array of size n_sources 
!     n_packed        integer  total number of cells all particles pass
!                                through
!     n_cells         integer  number of cells that each particle passed
!                                through during simulation, array of 
!                                size n_particles 
!     cell_packed     integer  array of cell numbers that each particle 
!                                enters, array of size total sum of 
!                                cells passed through for all particles
!     time_packed     real*8   array of times that each particle enters 
!                                the corresponding cell in the 
!                                cell_packed array, array of size total 
!                                sum of cells passed through for all 
!                                particles
!     x_packed        real*8   array of x coordinates where each particle 
!                                enters the corresponding cell in the 
!                                cell_packed array, array of size total 
!                                sum of cells passed through for all 
!                                particles
!     y_packed        real*8   array of y coordinates where each particle 
!                                enters the corresponding cell in the 
!                                cell_packed array, array of size total 
!                                sum of cells passed through for all 
!                                particles
!     z_packed        real*8   array of z coordinates where each particle 
!                                enters the corresponding cell in the 
!                                cell_packed array, array of size total 
!                                sum of cells passed through for all 
!                                particles
!     cell_index      integer  array of locations in the packed arrays 
!                                where each particle information starts,
!                                array of size n_particles
!     cell_path       integer  array denoting whether a particle has 
!                                passed through the grid cell, array of 
!                                size n_grid_points 
!     n_touched_cells integer  number of cells that particles have 
!                                passed through
!     touched_cells   integer  array containing the condensed version of
!                                cell_path, i.e. an array of only those 
!                                cells that have particles passing 
!                                through them, array of size 
!                                n_touched_cells
!     concentration   real*8   array of computed concentrations for the 
!                                cells in the compact array, array of 
!                                size n_touched_cells
!     kdecay          real*8   1st order decay constant
!     cfavg           real*8   computed outlet concentration from each 
!                                flux averaged concentration zone, 
!                                array of size nfavgzones
!     cmin            real*8   minimum output concentration parameter,
!                                if the calculated concentration is less
!                                than cmin it will be set to 0
!     cthreshold      real*8   minimum output concentration threshold,
!                                if the calculated concentration is less
!                                than cthreshold it will not be output for
!                                the favg pckd output option
!***********************************************************************
      
      integer npart
      integer, allocatable :: start_no(:)
      integer, allocatable :: end_no(:)
      integer, allocatable :: step_no(:)
      integer, allocatable :: n_part_source(:)
      integer n_packed
      integer, allocatable :: n_cells(:)
      integer, allocatable :: cell_packed(:)
      real*8, allocatable :: time_packed(:)
      real*8, allocatable :: x_packed(:)
      real*8, allocatable :: y_packed(:)
      real*8, allocatable :: z_packed(:)
      integer, allocatable :: cell_index(:)
      integer, allocatable :: cell_path(:)
      integer n_touched_cells
      integer, allocatable :: touched_cells(:)
      real*8, allocatable :: concentration(:)
      real*8 kdecay
      real*8, allocatable :: cfavg(:)
      real*8 cmin
      real*8 cthreshold
      parameter (cmin = 1.e-30)

      end module comparttr
