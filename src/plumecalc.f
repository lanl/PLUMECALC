      program plumecalc
!***********************************************************************
! $Id: plumecalc.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
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
!     Program plumecalc, Version 2.2
!     Bruce Robinson
!
!***********************************************************************
!     PURPOSE
!     
!     To compute the concentration in a contaminant plume from
!     input mass flux data and a simulation of particle tracking
!     to characterize the advective dispersive transport
!
!***********************************************************************
!     REQUIREMENTS
!
!     1. Read file information
!     2. Read simulation parameter information
!     3. Read particle tracking information
!     4. Read grid information
!     5. Read rock property and sorption information
!     6. Read source term information
!     7. Read output information
!     8. Perform convolution calculation at each time
!     9. Output concentration information
!
!***********************************************************************
!     DEFINITION of variables
!
!     Input variables
!
!     File plumecalc.files and variables
!
!     Identifier      Type       Description
!
!     grid_file   character*100) name of fehm grid file used for the 
!                                  simulation
!     stor_file   character*100) name of the fehm stor file used for 
!                                  the simulation
!     sptr_file   character*100) name of the fehm sptr2 file containing 
!                                  the particle tracking results
!     mdot_file   character*100) name of file containing the source 
!                                  term information
!     rock_file   character*100) name of file containing the fehm 
!                                  formatted rock macro and rfac macro
!     sim_file    character*100) name of the file containing the 
!                                  information needed to run the plume 
!                                  calculations and output the results
!     output_file character*100) name of the output file from the run
!     tcurve_file character*100) name of the type curve data file
!     
!***********************************************************************
!     PSEUDOCODE
!
!     1. Read file information
!
!     2. Read particle tracking information
!
!     3. Read simulation parameter information
!
!     4. Read grid information
!
!     5. Read rock property and sorption information
!
!     6. Read source term information
!     
!     7. Read output information
!
!     8. Perform convolution calculation at each time and 
!        Output concentration information
!
***********************************************************************

      use comunits
      implicit none
      real*8 run_seconds1, run_seconds2

c**************** Executable statements begin***********

      call cpu_time (run_seconds1)

!     1. Read file information

      call read_file_info()

!     2. Read particle tracking information

      call read_sptr_info()

!     3. Read simulation parameter information

      call read_sim_info()

!     4. Read grid information

      call read_grid_info

!     5. Read rock property and sorption information

      call read_rock_info()

!     6. Read source term information

      call read_mdot_info()

!     7. Read output information

      call read_output_info()

!     8. Perform convolution calculation at each time and output 
!        concentration information

      call perform_calculations()

      call cpu_time (run_seconds2)
      write (error_unit_number, *) 'Elapsed time: ',
     &     run_seconds2-run_seconds1, ' CPU seconds'

      end
