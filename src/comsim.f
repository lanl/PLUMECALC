      module comsim
!***********************************************************************
! $Id: comsim.f,v 1.1 2006/05/17 15:23:20 zvd Exp $
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
!     Module contains simulation information
!
!     Identifier      Type     Description
!     
!     total_time      real*8   total time of plume calculation
!                                simulation
!     n_out_times     integer  number of time steps in calculation
!     n_out_times     real*8   parameter controlling the number of times
!                                at which output plume concentrations 
!                                are to be output: if <0, 
!                                abs(n_out_times) is the time step
!                                if >0, int(n_out_times) is the number 
!                                of equally spaced time steps required
!                                if = 0, read in until blank line is 
!                                reached, one time at a time
!     out_string  character*4  macro keyword denoting the type of output
!                                required: if = 'avs' or 'avs ' full avs!                                output files for the entire grid; 
!                                if = 'pckd' output of concentrations 
!                                only for cells that have mass traveling
!                                through them
!     conc_string character*4  macro keyword denoting if resident or 
!                                flux averaged concentrations are 
!                                computed: 'favg' means flux averaged, 
!                                nothing means resident
!     nfavgzones      integer  number of zones at which flux averaged
!                                concentrations are being computed
!     index_favg      integer  array denoting the flux averaged zone 
!                                associated with each node, or 0 if
!                                the node is not in a flux averaged 
!                                zone, array of size n_grid_points
!     water_flux      real*8   array of outlet water fluxes for each 
!                                favg zone, water vol./day if days are 
!                                the units of time in the simulation, 
!                                array of size nfavgzones 
!     ntimes        integer    number of times at which plume 
!                                concentrations are being computed
!     delta_time    real*8     time interval to use for flux averaged
!                                concentration (used when output times
!                                are read in)
!     out_times     real*8     array of times at which plume 
!                                concentrations are being computed, 
!                                array of size ntimes        
!     out_cell      integer    array denoting which nodes the 
!                               calculation needs to be performed for, 
!                               only one asked for if output are needed,
!                               if 1 do calculation, array of size 
!                               n_grid_points       
!     noutnode      integer    number of nodes at which concentration
!                                output is requested using the 'node' 
!                                output option
!     ioutnodes     integer    array of nodes at which concentration 
!                                output is requested, array of size 
!                                noutnode 
!***********************************************************************

      real*8 total_time
      real*8 n_out_times
      real*8 delta_time
      character*3 alt_string
      character*4 out_string
      character*4 conc_string
      integer nfavgzones
      integer, allocatable :: index_favg(:)
      integer, allocatable :: nodes_favg(:)
      real*8, allocatable :: water_flux(:)
      integer ntimes
      real*8, allocatable :: out_times(:)
      integer noutnodes
      integer, allocatable :: ioutnode(:)
      integer, allocatable :: out_cell(:)
      logical :: sparse = .false., prntvar(5) = .false.

      end module comsim
