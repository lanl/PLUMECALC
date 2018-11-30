      subroutine read_rock_info()
!***********************************************************************
! $Id: read_rock_info.f,v 1.1 2006/05/17 15:23:24 zvd Exp $
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
!     DEFINITION of variables
!
!     Identifier      Type       Description
!
!     izonef          integer    array of zone numbers associated with 
!                                  each grid cell, array of size 
!                                  n_grid_points     
!     rhob            real*8     array of rock density values for each 
!                                  cell, array of size n_grid_points
!     phi             real*8     array of porosity values for each cell,
!                                  array of size n_grid_points
!     rfac            real*8     array of sorption retardation factors 
!                                  for each cell, array of size 
!                                  n_grid_points
!
!***********************************************************************
!     PSEUDOCODE
!
!     5. Read rock property and sorption information
!
!     ALLOCATE zone indexing array izonef
!     CALL subroutine zone_nnum to read in zone information
!     CALL fehm subroutine inrock to read in density and porosity
!        and rfac for retardation factors
!     CALL subroutine to read in sorption and matrix diffusion
!        parameters
!     CALL routine to correct particle times for sorption
!     CALL routine to read type curve data for dispersion
!
!     CLOSE rock property file
!
!***********************************************************************

      use comgrid, only : n_grid_points, izonef
      use comunits
      use comparttr_sg, only : if_subgrid
      implicit none
      character*4 dummy_string
      
!***************** Begin executable statements here**********

!     ALLOCATE zone indexing array izonef
      allocate(izonef(n_grid_points))
      izonef = 1

      do
         read (rock_unit_number, '(a4)', end = 100) dummy_string
!     Zone input must be read before rock or diff
         if (dummy_string(1:4) .eq. 'rock' .or. 
     &      dummy_string(1:4) .eq. 'diff'  ) exit
         if (dummy_string(1:1) .eq. "#") then 
!     This is a comment line, read next line
         else if (dummy_string(1:4) .eq. "zone") then
!     CALL subroutine zone_nnum to read in zone information
            call zone_nnum
         end if
      end do

 100  rewind rock_unit_number
!     CALL fehm subroutine inrock_plume to read in density and porosity
!        and rfac for retardation factors
      call inrock_plume

!     CALL subroutine to apply velocity corrections if used
      call velocity_correction

!     CALL subroutine to read in sorption and matrix diffusion
!        parameters
      call indiffusion

!     CALL routine to read type curve data for dispersion
      if (tcurve_unit_number .ne. 0) then
         call read_tcurve(tcurve_unit_number, 2, error_unit_number)
      end if

!     CALL routine to correct particle times for sorption or diffusion
!     For subgridding this will be called after the cells have been 
!     subgridded
      if (if_subgrid .ne. 1) then
         call delayed_times
      end if

!     CLOSE rock property file
      close (rock_unit_number)

      return
      end subroutine read_rock_info
