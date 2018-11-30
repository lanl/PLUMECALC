      subroutine read_grid_info
!***********************************************************************
! $Id: read_grid_info.f,v 1.1 2006/05/17 15:23:24 zvd Exp $
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
!     Identifier      Type     Description
!
!     n_grid_points   integer  number of points (cells) in the grid
!     ncont           integer  number of indices and nodal connections
!     sx1             real*8   volume of each cell (m^3), array of size 
!                                n_grid_points  
!     x, y, z         real*8   coordinates of each grid cell (center of 
!                                cell), arrays of size n_grid_points   
!
!***********************************************************************
!     PSEUDOCODE
!
!     4. Read grid information
!
!     READ number of grid points
!     ALLOCATE control volume array sx1, and coordinate arrays
!     READ coordinates
!     READ sx1 array
!     CLOSE grid files
!
!***********************************************************************

      use comgrid
      use comunits, only : grid_unit_number, stor_unit_number,
     &     flux_unit_number, error_unit_number
      implicit none

      character*30 verno
      character*11 jdate
      character*8 jtime
      character*80 ptitle
      character*4 dummy4
      character*72 dummy72
      integer idummy
      integer i
      integer idum1
      integer idum2
      integer idum3
      integer idum4
      integer idum5

      read(grid_unit_number,'(a4)') dummy4
!     READ number of grid points
      read(grid_unit_number,*) n_grid_points
      
!     ALLOCATE control volume array sx1, and coordinate arrays
      allocate(sx1(n_grid_points))
      allocate(x(n_grid_points))
      allocate(y(n_grid_points))
      allocate(z(n_grid_points))

!     READ coordinates
      do i = 1, n_grid_points
         read(grid_unit_number,*) idummy, x(i), y(i), z(i)
      end do

!     READ sx1 array
      if(lda.eq.1) then
!     ASCII
         read(stor_unit_number,*) dummy72
         read(stor_unit_number,*) dummy72
         read(stor_unit_number,*) idum1, idum2, idum3, idum4, idum5
         read(stor_unit_number,*) (sx1(i),i=1,n_grid_points)
         if (flux_unit_number .ne. 0) then
! We will be reading fluxes from a FEHM restart file and need nelm
            ncont = idum3
            allocate (nelm(ncont))
            read(stor_unit_number,*) (nelm(i),i=1,ncont)
         end if
      elseif(lda.eq.2) then
!     UNFORMATTED
         read(stor_unit_number, err=100, iostat=idum1) dummy72
         read(stor_unit_number) dummy72
 100     if (idum1 .ne. 0) then
            backspace stor_unit_number
            read(stor_unit_number, err=200)  verno, jdate, jtime
            read(stor_unit_number) ptitle
         end if
         read(stor_unit_number) idum1, idum2, idum3, idum4, idum5
         read(stor_unit_number) (sx1(i),i=1,n_grid_points)
         if (flux_unit_number .ne. 0) then
! We will be reading fluxes from an FEHM restart file and need nelm
            ncont = idum3
            allocate (nelm(ncont))
            read(stor_unit_number) (nelm(i),i=1,ncont)
         end if
      end if

!     CLOSE grid files
      close(stor_unit_number)
      close (grid_unit_number)
      return
 200  write (error_unit_number, 210)
      write (error_unit_number, 211)
      stop
 210  format('ERROR reading coefficient storage file')
 211  format('STOPPING execution')
      end subroutine read_grid_info
