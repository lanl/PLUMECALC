      module comunits
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
!     Module containing unit numbers for files
!
!     Identifier         Type     Description
!     
!     error_unit_number  integer  Unit number of error output file 
!     grid_unit_number   integer  Unit number of the fehm grid file 
!                                   containing the x, y, z coordinates
!     stor_unit_number   integer  Unit number of the fehm stor file 
!                                   containing the cell volumes
!     sptr_unit_number   integer  Unit number for the sptr file for the
!                                   simulation
!     sptr_corn_unit_number   integer  Unit number for the sptr file for the
!                                   simulation
!     rock_unit_number   integer  Unit number of the file containing 
!                                   the modified rock macro used for 
!                                   the simulation
!     sim_unit_number    integer  Unit number of the simulation input 
!                                   parameters for the simulation
!     output_unit_number integer  Unit number for the output file for 
!                                   the simulation 
!     tcurve_unit_number integer  Unit number of the type curve data 
!                                   file used for the simulation
!     sptr_bin           logical  Flag denoting if the sptr input file
!                                   is binary
!***********************************************************************

      integer error_unit_number
      integer grid_unit_number
      integer stor_unit_number
      integer rock_unit_number
      integer sim_unit_number
      integer output_unit_number 
      integer tcurve_unit_number
      integer flux_unit_number
      integer sptr_corn_unit_number
      integer sptr_corn_file_type
      integer sptr_num
      integer, allocatable :: sptr_unit_number(:)
      
      logical, allocatable :: sptr_bin(:)

      logical flag_avs

      end module comunits
