       module compfrac
!***********************************************************************
! $Id: compfrac.f,v 1.1 2006/05/17 15:23:20 zvd Exp $
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
!     Module containing variables for parallel fracture dispersion 
!     interpolation calculations. Developed originally for FEHM.
! 
!     Identifier      Type     Description
!
!     numparams       integer  Number of parameters defining type curves
!     nump1           integer  Number of param1 values used to define 
!                                transfer function curves.
!     nump2           integer  Number of param2 values used to define 
!                                transfer function curves.
!     nump3           integer  Number of param3 values used to define 
!                                transfer function curves.
!     d4              integer  Fracture-matrix flow interaction flag. 
!     curve_structure integer  If 1, free format structure of curves
!     log_flag        integer  If 0, use value itself in interpolation,
!                                if 1, take log of value first
!     normal_param    real*4   Array of values for normalizing the 
!                                parameters before performing the 
!                                nearest neighbor or multi-curve 
!                                interpolation.
!     nump            integer  Number of time/concentration pairs in 
!                                each curve, array of size
!     wt_curve        real*4   array of size
!     weight_factor   real*4   Optional weight factor used when 
!                                curve_structure > 1.
!     param1          real*8   Parameter 1 values used to define 
!                                transfer function curves, array of 
!                                size nump1 
!     param2          real*8   Parameter 2 values used to define 
!                                transfer function curves, array of  
!                                size nump2  
!     param3          real*8   Parameter 3 values used to define 
!                                transfer function curves, array of  
!                                size nump3
!     dtime           real*8   Transfer function curve delay times, 
!                                array of size (nump1, nump2, nump3, 
!                                d4, nump).
!     conc            real*8   Transfer function curve concentrations, 
!                                array of size (nump1, nump2, nump3,  
!                                d4, nump).
!     param_density   real*8   array of size
!     pfrac_read      logical  Flag indicating if data has been read
!     ipartout        integer
!***********************************************************************
 
      implicit none
      save
 
      integer :: numparams
      integer :: nump1
      integer :: nump2
      integer :: nump3
      integer :: d4
      integer :: curve_structure
      integer :: log_flag(100)
      real :: normal_param(100)
      integer, allocatable, dimension(:,:,:,:) :: nump      
      integer, allocatable, dimension(:,:,:) :: itf_curve
      real, allocatable, dimension(:,:,:) :: wt_curve
      real weight_factor
      real*8 sigma_low, sigma_high, omega_low, omega_high
      real*8, allocatable, dimension(:) :: param1 
      real*8, allocatable, dimension(:) :: param2
      real*8, allocatable, dimension(:) :: param3
      real*8, allocatable, dimension(:,:,:,:,:) :: dtime
      real*8, allocatable, dimension(:,:,:,:,:) :: conc
      integer, allocatable, dimension(:,:,:) :: param_density

      logical :: pfrac_read = .false.
      logical :: error_flag = .false.
      integer :: ipartout = 0

      end module compfrac
 
