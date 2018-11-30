      module commdot
!***********************************************************************
! $Id: commdot.f,v 1.1 2006/05/17 15:23:20 zvd Exp $
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
!     Module containing variables related to the source mass flux
!     information
!
!     Identifier      Type     Description
!     
!     n_sources       integer  number of individual solute sources
!     n_size_mdot     integer  total size of mdot and time arrays
!     end_no_mdot     integer  for each source, the index in the array 
!                                of the last entry in the mdot array for
!                                that source, array of size n_sources
!     column_number   integer  array defining the column number where 
!                                the mdot information for each source 
!                                region is being read in (do not count 
!                                the time array as one, it is assumed to
!                                be the first column), array of size 
!                                n_sources  
!     mdot            real*8   array of mass flux values for all sources
!                                in the time, mdot curves, array of 
!                                size n_size_mdot
!     time_mdot       real*8   array of times associated with the time,
!                                mdot curves, array of size n_size_mdot
!***********************************************************************

      integer n_sources
      integer n_size_mdot
      integer, allocatable :: end_no_mdot(:)
      integer, allocatable :: column_number(:)
      real*8, allocatable :: mdot(:)
      real*8, allocatable :: time_mdot(:)

      end module commdot
