      subroutine indiffusion
!***********************************************************************
! $Id: indiffusion.f,v 1.3 2006/11/14 16:34:05 zvd Exp $
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
      use comgrid, only : n_grid_points, z
      use comrock
      implicit none

      logical macroread
      logical done, found, secondary, temp_flag
      character*4 dummy4
      character*200 tfile
      character*80 wdd1
      character*32 cmsg(6)
      character*12 file_format
      integer i, imsg(6), msg(6), nwds
      integer iform, open_file, temp_unit_number
      real*8 xmsg(6)
      real*8 rprime
      real*8 sprime
      real*8 ssecond
      real*8 denominator
      real*8 diffcurrent, difft
      real*8 tcurrent, tintercept, tgradient, zintercept
      real*8, allocatable :: temperature(:)
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

c     Check for temperature keyword to determine if diffusion
c     will be a function of temperature
      read(rock_unit_number,'(a80)') wdd1
      if (wdd1(1:4) .eq. 'temp' .or. wdd1(1:4) .eq. 'TEMP') then
         temp_flag = .true.
         iform = 0
         read(rock_unit_number,'(a80)') wdd1
         if (wdd1(1:4) .eq. 'file') then
c     Temperatures will be read from a file
c     name may be preceeded by line with file format 
            read(rock_unit_number,'(a12)') file_format
            select case (file_format(1:3))
            case ('asc','for')
               iform = 1
            case ('bin')
               iform = 3
            case ('unf')
               iform = 2
            case default
               iform = 1
               backspace rock_unit_number
            end select
            read(rock_unit_number,'(a200)') tfile
            temp_unit_number = open_file(tfile, iform, 'old')
            allocate(temperature(n_grid_points))
            if (iform .eq. 1) then
               read(temp_unit_number,*) (temperature(i), i = 1, 
     &              n_grid_points)
            else
               read(temp_unit_number) (temperature(i), i = 1, 
     &              n_grid_points)
            end if  
         else
c     Temperatures will be calculated using a linear equation
            read(wdd1,*) tintercept, tgradient, zintercept
         end if   
      else
         backspace rock_unit_number
         temp_flag = .false.
      end if

c     Read in the lines of input for each transport model

      ndiffmodels = 0

      read(rock_unit_number,*) rseed

      secondary = .false.

      do
         read(rock_unit_number,'(a80)') wdd1
         if (null1(wdd1)) exit
         ndiffmodels=ndiffmodels+1
         if (ndiffmodels .eq. 1) then
            call parse_string(wdd1, imsg, msg, xmsg, cmsg, nwds)
            if (nwds .ge. 6) secondary = .true.
         end if
      end do

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
         if (secondary) then
            read(rock_unit_number,*) kd(i), diffmfl(i), 
     2           rd_frac(i), matrix_por(i), spacing_primary(i), 
     3           spacing_secondary(i)
         else
            read(rock_unit_number,*) kd(i), diffmfl(i), 
     2           rd_frac(i), matrix_por(i), spacing_primary(i)
         end if
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
            rprime = 1.+kd(itrc_diff(i))*denr(i)/denominator
            sprime = spacing_primary(itrc_diff(i))/vcf(i)
            if (temp_flag) then
c     Apply temperature correction to diffusion
               if (iform .eq. 0) then
c     Calculate temperature using linear equation
                  tcurrent = tintercept + tgradient * (zintercept-z(i))
               else
                  tcurrent = temperature(i)
               end if
c     Put in correct relationship
               diffcurrent = difft ( diffmfl(itrc_diff(i)), tcurrent ) 
            else
               diffcurrent =  diffmfl(itrc_diff(i))
            end if
            if(spacing_primary(itrc_diff(i)).lt.0.) then

               omega_partial(i) = matrix_por(itrc_diff(i))**2*
     2              diffcurrent*rprime
               sigma_partial(i) = sprime

            else

               omega_partial(i) = matrix_por(itrc_diff(i))*
     2              sqrt(rprime*diffcurrent)/(0.5*sprime)
               if (.not. secondary)
     2              spacing_secondary(itrc_diff(i)) = sprime / ps(i)
               sigma_partial(i) = sqrt(rprime/diffcurrent)*0.5*
     2              (spacing_secondary(itrc_diff(i))-sprime)

            end if
         end if

      end do

      if (allocated(temperature)) deallocate(temperature)
 9000 continue

      end subroutine indiffusion

      real*8 function difft(diffc, tdiff)

      implicit none
      integer i
      real*8 diffc, tdiff, vcurrent, vdiff
      real*8, dimension(12) :: tv, viscosity

      tv = (/0.,5.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100./)
      viscosity = (/1.787d-6,1.519d-6,1.307d-6,1.004d-6,0.801d-6,
     &     0.658d-6,0.553d-6,0.475d-6,0.413d-6,0.365d-6,0.326d-6,
     &     0.294d-6/)

! Find bounding temperatures
      if (tdiff .le. tv(1)) then
! Diffusion will be based on t(1)
         difft = 3.424868d-9 * diffc * (tv(1) + 273.15) / viscosity(1)
      else if (tdiff .ge. tv(12)) then
! Diffusion will be based on t(12)
         difft = 3.424868d-9 * diffc * (tv(12) + 273.15) / viscosity(12)
      else
         do i = 2, 12
            if (tdiff .eq. tv(i)) then
               difft = 3.424868d-9 * diffc * (tdiff + 273.15) / 
     &              viscosity(i)
               exit
            else if (tdiff .gt. tv(i-1) .and. tdiff .lt. tv(i)) then
               vdiff = viscosity(i) - viscosity (i-1)
               vcurrent = viscosity(i-1) + vdiff *
     &              (tdiff - tv(i-1))/(tv(i) - tv(i-1))
               difft = 3.424868d-9 * diffc * (tdiff + 273.15) / 
     &              vcurrent
               exit
            end if
         end do
      end if
      
      end function difft
