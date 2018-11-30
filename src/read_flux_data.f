      subroutine read_flux_data()
!***********************************************************************
! $Id: read_flux_data.f,v 1.2 2006/06/06 20:10:56 zvd Exp $
!***********************************************************************
!  Copyright, 2006,  The  Regents of the University of California.
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

      use comgrid, only : n_grid_points, ncont, nelm, iflux
      use comsim, only : water_flux
      use comunits, only : error_unit_number, flux_unit_number
      implicit none

      integer i, i1, i2, iloop, j, numflux, ncount, neqp1
      character*80 dumtitle
      character*30 dumver, dumstring
      character*11 dumdate, dumflag
      character*8 dumtime
      real*8 dumreal
      real*8, allocatable :: dumflux(:)

      if (iflux .eq. 1) then
!     READ ASCII FEHM restart file
         read (flux_unit_number, *) dumver, dumdate, dumtime
         read (flux_unit_number, *) dumtitle
         read (flux_unit_number, *) dumreal
         read(flux_unit_number,'(a4)') dumstring(1:4)
         read(flux_unit_number,'(a4)') dumstring(5:8)
         read(flux_unit_number,'(a4)') dumstring(9:12)
         read(flux_unit_number,'(a4)') dumstring(13:16)
         read(flux_unit_number,'(a4)') dumstring(17:20)
         ncount=n_grid_points
         if(dumstring(13:16).eq.'dpdp') ncount=2*ncount
         if(dumstring(17:20).eq.'dual') ncount=3*ncount
         if (dumstring(1:4).eq.'ngas') then
            iloop = 4
         else if(dumstring(1:4).eq.'h20 ') then
            iloop = 3
         else if(dumstring(1:4).eq.'air ') then
            iloop = 2
         end if
         do i = 1, iloop
            read(flux_unit_number, *)  ( dumreal , j=1,ncount )
         end do 
         read(flux_unit_number,'(a11)', end = 100) dumflag
         if (dumflag(1:3) .eq. 'all' .or. dumflag(1:3) .eq. 'liq') then
            read(flux_unit_number, *, end = 100) numflux
            allocate (dumflux(numflux))
            read(flux_unit_number, 6002) (dumflux(i), i = 1,numflux)
         else if (dumflag(1:3) .eq. 'mas') then
            read(flux_unit_number, *, end = 100) numflux
            allocate (dumflux(numflux))
            read(flux_unit_number, 6001) (dumflux(i), i = 1,numflux)
         else
            goto 100
         end if
 6001    format(5g15.8)
 6002    format(4g25.16)
      else
!     READ BINARY FEHM restart file
         read (flux_unit_number) dumver,dumdate,dumtime,dumtitle
         read (flux_unit_number) dumreal
         read(flux_unit_number) dumstring(1:4)
         read(flux_unit_number) dumstring(5:8)
         read(flux_unit_number) dumstring(9:12)
         read(flux_unit_number) dumstring(13:16)
         read(flux_unit_number) dumstring(17:20)
         ncount=n_grid_points
         if(dumstring(13:16).eq.'dpdp') ncount=2*ncount
         if(dumstring(17:20).eq.'dual') ncount=3*ncount
         if (dumstring(1:4).eq.'ngas') then
            iloop = 4
         else if(dumstring(1:4).eq.'h20 ') then
            iloop = 3
         else if(dumstring(1:4).eq.'air ') then
            iloop = 2
         end if
         do i = 1, iloop
            read(flux_unit_number)  ( dumreal , j=1,ncount )
         end do 
         dumflag = '           '
         read(flux_unit_number, end = 100) dumflag
         if (dumflag(1:3) .eq. 'all' .or. dumflag(1:3) .eq. 'liq') then
            read(flux_unit_number) numflux
            allocate (dumflux(numflux))
            read(flux_unit_number) (dumflux(i), i = 1,numflux)
         else
            goto 100
         end if
      end if

      neqp1 = n_grid_points+1
      do i = 1, n_grid_points
         i1 = nelm(i) + 1
         i2 = nelm(i+1)
         do j = i1, i2
            if (dumflux(j-neqp1) .gt. 0) then
               water_flux(i) = water_flux(i) + dumflux(j-neqp1)
            end if
         end do
!     Convert from kg/s to l/day (assumes water density 1 kg/l)
         water_flux(i) = water_flux(i) * 86400.
      end do
         
      deallocate (dumflux)
      deallocate (nelm)
      close (flux_unit_number)
      return

 100  write (error_unit_number, *) 'ERROR reading flux data'
      write (error_unit_number, *) 'STOPPING execution'
      stop
      
      end subroutine read_flux_data
