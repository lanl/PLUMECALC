      subroutine zone_nnum
!***********************************************************************
! $Id: zone_nnum.f,v 1.2 2006/08/16 21:37:29 zvd Exp $
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

      use comunits, only : rock_unit_number
      use comgrid, only : n_grid_points, izonef
      implicit none

      character*80 wdd1

      logical null1
      integer cnum, i, izone, izonel, nin, nodez, nsl
      integer nxy,icnl_old, zone_unit_number, open_file
      character* 4 macro
      character*80 ltest
      character*100 zone_file_name
      real*8 xg, xz(8), yg, yz(8), zg, zz(8)
      real*8 tol_zone, zxy_min, zxy_max 
      integer imodel, j, n_n_n
      integer zone_dpadd

      integer, allocatable :: ncord(:)

      allocate(ncord(n_grid_points))

      read (rock_unit_number, '(a4)', end=90)  macro
      if (macro .eq. 'file') then
! zone definitions are in a separate file
         read (rock_unit_number, '(a100)') zone_file_name
         zone_unit_number = open_file(zone_file_name, 1, 'OLD')
         do
            read (zone_unit_number, '(a4)', end=90) macro
            if (macro .eq. 'zone') exit
         end do
      else
         zone_unit_number = rock_unit_number
         backspace zone_unit_number
      end if

      do
         read (zone_unit_number, '(a80)', end=90) wdd1
         if (null1(wdd1) .or. wdd1(1:4) .eq. 'rock') exit
         read(wdd1, *) izone
         read(zone_unit_number, *) macro
         if(macro .eq. 'list') then
! zone input using list format
! read in coordinates for nodes in zone
            nin = 0
            do
               read(zone_unit_number, '(a80)', end = 90) ltest
               if(.not.null1(ltest)) exit
               read(ltest, *, end = 80, err = 80) xg, yg, zg
               nin = nin + 1
               call near3(xg, yg, zg, nodez)
               ncord(nin) = nodez
               izonef(nodez) = izone
            end do
 80         continue         
         else if (macro .eq. 'nnum') then
! zone input using nnum format
            read (zone_unit_number, *) nin, (ncord (i), i = 1, nin)
            do i = 1, nin
               izonef(ncord(i)) = izone
            end do
         endif
      end do
 90   continue
      deallocate(ncord)
      if (zone_unit_number .ne. rock_unit_number) 
     &     close(zone_unit_number)
      return
      end
