      subroutine indiffusion
!***********************************************************************
! $Id: indiffusion.f,v 1.1 2006/05/17 15:23:21 zvd Exp $
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

      use comunits, only : rock_unit_number, error_unit_number
      use comki
      use comgrid, only : n_grid_points
      use comrock
      implicit none

      logical macroread
      logical done, found
      character*4 dummy4
      character*80 wdd1
      character*32  cmsg(2)
      integer i, imsg(2), msg(2), nwds
      real*8 xmsg(2)
      real*8 rprime
      real*8 denominator
      logical null1

c     Read until 'diff' is found or end of file reached

      done = .false.
      diffusion_model = .false.
      allocate(itrc_diff(n_grid_points))
      itrc_diff = 0


      do while(.not.done)
         read(rock_unit_number,'(a80)',end=9000) wdd1
         if(wdd1(1:4).eq.'diff') then
            done = .true.
            diffusion_model = .true.
            use_matrix = .false.
            call parse_string(wdd1, imsg, msg, xmsg, cmsg, nwds)
            if (nwds .eq. 2) then
               if (cmsg(2) .eq. 'matrix') then
                  use_matrix = .true.
               end if
            end if
         end if
      end do

c     Read in the lines of input for each transport model

      ndiffmodels = 0

      read(rock_unit_number,*) rseed

 4000 continue

      read(rock_unit_number,'(a80)') wdd1
      if (null1(wdd1)) then
         goto 99
      else
         ndiffmodels=ndiffmodels+1
      end if
      goto 4000
 99   continue
      
c     Go back in file to beginning of diffusion model input

      do i = 1, ndiffmodels+1
         backspace rock_unit_number
      end do

      macro = 'diff'
      allocate(kd(ndiffmodels))
      allocate(diffmfl(ndiffmodels))
      allocate(rd_frac(ndiffmodels))
      allocate(matrix_por(ndiffmodels))
      allocate(spacing_primary(ndiffmodels))
      allocate(spacing_secondary(ndiffmodels))

      kd = 0.
      diffmfl = 1.e-30
      rd_frac = 1.
      matrix_por = 1.
      spacing_primary = 1.e-30
      spacing_secondary = 1.e-30

c     Read in models

      do i = 1, ndiffmodels
         read(rock_unit_number,*) kd(i), diffmfl(i), 
     2        rd_frac(i), matrix_por(i), spacing_primary(i)
      end do

c     read past blank line

      read(rock_unit_number,'(a4)') dummy4


c**** Set pointers for nodes belonging to each diffusion model ****
      narrays = 1
      itype(1) = 4
      default(1) = 0
      igroup = 1

      found=.false.


      call initdata2 (rock_unit_number, error_unit_number,
     2     n_grid_points, narrays, itype, 
     3     default, macroread, macro, igroup, ireturn,
     4     i4_1=itrc_diff(1:n_grid_points))
      
c     Set sigma and omega parameters for diffusion model

      allocate(sigma_partial(n_grid_points))
      allocate(omega_partial(n_grid_points))
      sigma_partial = 0.
      omega_partial = 0.

      do i = 1, n_grid_points
         if(itrc_diff(i).ne.0) then
            denominator = 1000.*matrix_por(itrc_diff(i))
            denominator = max(1.d-30, denominator)
            rprime = (1.+kd(itrc_diff(i))*denr(i)/denominator)*
     2           (vcf(i)**2)

            if(spacing_primary(itrc_diff(i)).lt.0.) then

               omega_partial(i) = matrix_por(itrc_diff(i))**2*
     2              diffmfl(itrc_diff(i))*rprime
               sigma_partial(i) = spacing_primary(itrc_diff(i))

            else

               omega_partial(i) = matrix_por(itrc_diff(i))*
     2              sqrt(rprime*diffmfl(itrc_diff(i)))/(0.5*
     3              spacing_primary(itrc_diff(i)))
               spacing_secondary(itrc_diff(i)) = 
     2              spacing_primary(itrc_diff(i)) / ps(i)
               sigma_partial(i) = sqrt(rprime/
     2              diffmfl(itrc_diff(i)))*0.5*
     3              (spacing_secondary(itrc_diff(i))-
     4              spacing_primary(itrc_diff(i)))
            end if
         end if

      end do


 9000 continue

      end subroutine indiffusion
