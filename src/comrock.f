      module comrock
!***********************************************************************
! $Id: comrock.f,v 1.2 2006/06/06 20:11:36 zvd Exp $
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
!     Module containing rock properties arrays
!
!     Identifier      Type     Description
!     
!     denr            real*8   bulk rock density (kg/m3) array of size 
!                                n_grid_points
!     ps              real*8   porosity, array of size n_grid_points 
!     rfac            real*8   retardation factor for solute in each 
!                                grid cell, array of size n_grid_points
!     vcf             real*8   velocity correction factor (added to the
!                                retardation factor, rfac or rd_frac), 
!                                array of size n_grid_points
!     itrc_diff       integer  flag pointing each cell to a diffusion 
!                                model number, array of size 
!                                n_grid_points      
!     sigma_partial   real*8   parameter in diffusion model, array of 
!                                size n_grid_points
!     omega_partial   real*8   parameter in diffusion model, array of 
!                                size n_grid_points  
!     rd_frac         real*8   retardation factor in the primary medium 
!                               array of size number of diffusion models
!     matrix_por      real*8   matrix porosity for diffusion model, 
!                               array of size number of diffusion models
!     spacing_primary real*8   length scale in primary porosity for 
!                                diffusion model, array of size number 
!                                of diffusion models
!     spacing_secondary real*8 length scale in secondary porosity for  
!                                diffusion model, array of size number 
!                                of diffusion models
!     rseed           integer  random number seed
!     ndiffmodels     integer  number of diffusion models
!     diffusion_model logical  flag denoting if the diffusion model is 
!                                being used
!***********************************************************************

      real*8, allocatable :: denr(:)
      real*8, allocatable :: ps(:)
      real*8, allocatable :: rfac(:)
      real*8, allocatable :: vcf(:)
      integer, allocatable :: itrc_diff(:)
      real*8, allocatable :: sigma_partial(:)
      real*8, allocatable :: omega_partial(:)
      real*8, allocatable :: rd_frac(:)
      real*8, allocatable :: kd(:)
      real*8, allocatable :: diffmfl(:)
      real*8, allocatable :: matrix_por(:)
      real*8, allocatable :: spacing_primary(:)
      real*8, allocatable :: spacing_secondary(:)
      integer ndiffmodels
      integer rseed
      logical diffusion_model
      logical use_matrix

      end module comrock
