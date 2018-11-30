      subroutine read_output_info()
!***********************************************************************
! $Id: read_output_info.f,v 1.2 2006/08/16 21:37:29 zvd Exp $
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
!     cell_path       integer  array denoting whether a particle has 
!                                passed through the grid cell, array of
!                                size n_grid_points 
!     n_touched_cells integer  number of cells that particles have
!                                passed through
!     touched_cells   integer  array containing the condensed version 
!                                 of cell_path, i.e. an array of only 
!                                 those cells that have particles 
!                                 passing through them, array of size 
!                                 n_touched_cells 
!
!     Input variables
!
!     n_out_times      real*8   parameter controlling the number of 
!                                 times at which output plume 
!                                 concentrations are to be output
!                                 if <0, abs(n_out_times) is the time 
!                                 step; if >0, int(n_out_times) is the 
!                                 number of equally spaced time steps 
!                                 required; if = 0, read in until blank 
!                                 line is reached, one time at a time
!     out_string   character*4  macro keyword denoting the type of
!                                 output required
!                                 if = 'avs' or 'avs ' full avs output 
!                                 files for the entire grid
!                                 if = 'pckd' output of concentrations 
!                                 only for cells that have mass 
!                                 traveling through them
!
!***********************************************************************
!     PSEUDOCODE
!
!     7. Read output information
!
!     ALLOCATE array of grid cells that marks whether any particles
!        pass through the cell
!
!     FOR each cell hit
!         Add one to the array deonting particles passing through a cell
!     ENDFOR each cell hit
!
!     FOR each cell
!        IF the cell is marked
!           Increment counter of number of cells touched by 1
!           Store index of cell in cell array
!        ENDIF the cell is marked
!     ENDFOR each cell
!
!     ALLOCATE indexing array of cells that have been touched
!     ALLOCATE indexing array of concentrations
!
!     FOR each cell
!        IF the cell is marked
!           Store cell number in indexing array
!        ENDIF the cell is marked
!     ENDFOR each cell
!
!     READ in the number of times at which output information is required
!     READ flag to determine if full avs output file or compressed file
!        is required
!
!     IF input number indicates time step is input
!        Compute the number of times
!        ALLOCATE array of output times
!        FOR each time
!           Set the output time
!        ENDFOR each time
!     ELSEIF input number indicates a certain number of equally spaced times is input
!        ALLOCATE array of output times
!        FOR each time
!           Compute and set the output time
!        ENDFOR each time
!     ELSE the times are being read in
!        ALLOCATE array of output times
!        READ in array of times for output information
!     ENDIF input number indicates time step is input
!
!***********************************************************************

      use comgrid, only : n_grid_points
      use comparttr, only : cell_path, cell_packed, n_packed,
     2 n_touched_cells, touched_cells, concentration, cfavg
      use comsim, only : total_time, n_out_times, out_string,
     2     ntimes, delta_time, out_times, noutnodes, ioutnode,
     3     out_cell, conc_string, nfavgzones, index_favg, water_flux
      use comunits, only : error_unit_number, sim_unit_number, 
     2     flux_unit_number
      implicit none

      integer i
      integer j
      integer inodes_favg
      integer, allocatable :: nodes_favgtemp(:)

!****************     Executable code starts here*******************

!     ALLOCATE array of grid cells that marks whether any particles
!        pass through the cell
      allocate(cell_path(n_grid_points))
      cell_path = 0
      allocate(out_cell(n_grid_points))
      out_cell = 0

!     FOR each cell hit
      do i = 1, n_packed
!         Add one to the array denoting particles passing through a cell
         cell_path(cell_packed(i)) = cell_path(cell_packed(i))+1
      end do
!     ENDFOR each cell hit

      n_touched_cells = 0
!     FOR each cell
      do i = 1, n_grid_points
!        IF the cell is marked
         if(cell_path(i).ne.0) then
!           Increment counter of number of cells touched by 1
            n_touched_cells = n_touched_cells + 1
!           Store index of cell in cell array
            cell_path(i) = n_touched_cells
         end if
!        ENDIF the cell is marked
      end do
!     ENDFOR each cell

!     ALLOCATE indexing array of cells that have been touched
      allocate(touched_cells(n_touched_cells))
!     ALLOCATE indexing array of concentrations
      allocate(concentration(n_touched_cells))
      touched_cells = 0
      concentration = 0.

!     FOR each cell
      do i = 1, n_grid_points
!        IF the cell is marked
         if(cell_path(i).ne.0) then
!           Store cell number in indexing array
            touched_cells(cell_path(i)) = i
         end if
!        ENDIF the cell is marked
      end do
!     ENDFOR each cell

!     Determine if resident or flux-averaged concentration

      read(sim_unit_number,'(a4)') conc_string
      if(conc_string.ne.'favg') then
         conc_string = 'resc'
         backspace sim_unit_number
      else
         allocate(index_favg(n_grid_points))
         index_favg = 0
         if (flux_unit_number .ne. 0) then
!     READ fluxes for each node from a FEHM restart file
            nfavgzones = n_grid_points
            allocate(water_flux(nfavgzones))
            allocate(cfavg(nfavgzones))
            cfavg = 0.
            water_flux = 0.
            call read_flux_data
            do i = 1, nfavgzones
               index_favg(i) = i
            end do
         else
!     READ in number of zones at which flux-averaged conc. is to be
!        calculated
            read(sim_unit_number,*) nfavgzones
!     ALLOCATE arrays used in flux averaged concen. calculation
            allocate(water_flux(nfavgzones))
            allocate(cfavg(nfavgzones))
            allocate(nodes_favgtemp(n_grid_points))
            water_flux = 0.
            cfavg = 0.
            nodes_favgtemp = 0
!     READ in flux associated with each zone
            read(sim_unit_number,*) (water_flux(j),j=1,nfavgzones)
!     READ in nodes associated with each favg zone
            do i = 1, nfavgzones
               read(sim_unit_number,*) inodes_favg
               read(sim_unit_number,*) (nodes_favgtemp(j),
     &              j=1,inodes_favg)
               do j = 1, inodes_favg
                  index_favg(nodes_favgtemp(j)) = i
               end do
            end do
            deallocate(nodes_favgtemp)
         end if
      end if
!     READ in the number of times at which output information is required
      read(sim_unit_number,*) total_time, n_out_times

!     IF input number indicates time step is input
      if(n_out_times .lt. 0.) then
!        Compute the number of times
         ntimes = int(total_time/abs(n_out_times)+.01)
         ntimes = max(ntimes, 1)
!        ALLOCATE array of output times
         allocate(out_times(ntimes))
         delta_time = total_time/ntimes
!        FOR each time
         do i = 1, ntimes
!           Set the output time
            out_times(i) = i*delta_time
         end do
         delta_time = 0.
!        ENDFOR each time
!     ELSEIF input number indicates a certain number of equally spaced 
!        times is input
      elseif(n_out_times .gt. 0.) then
         ntimes = int(n_out_times+.01)
         ntimes = max(ntimes, 1)
!        ALLOCATE array of output times
         allocate(out_times(ntimes))
         delta_time = total_time/ntimes
!        FOR each time
         do i = 1, ntimes
!           Compute and set the output time
            out_times(i) = i*delta_time
         end do
         delta_time = 0.
!        ENDFOR each time
!     ELSE the times are being read in
      else
         if(conc_string.eq.'favg') then
            read(sim_unit_number,*, err=200) ntimes, delta_time
         else
            read(sim_unit_number,*) ntimes
         end if
!        ALLOCATE array of output times
         allocate(out_times(ntimes))
!        READ in array of times for output information
         read(sim_unit_number,*)(out_times(i),i=1,ntimes)
      end if
!     ENDIF input number indicates time step is input

!     READ flag to determine if full avs output file or compressed file
!        is required
      read(sim_unit_number,'(a4)') out_string

      if(out_string.eq.'node' .or. out_string.eq.'tecn') then
         read(sim_unit_number,*) noutnodes
         allocate(ioutnode(noutnodes))
         read(sim_unit_number,*)(ioutnode(i),i=1,noutnodes)
         do i = 1, noutnodes
            out_cell(ioutnode(i)) = 1
         end do
      else
         do i = 1, n_grid_points
            if(cell_path(i).ne.0) out_cell(i) = 1
         end do
      end if

      return

 200  write(error_unit_number, 100) 
      write(error_unit_number, 110) 
      stop
 100  format ('ERROR - delta_time must be entered for ',
     &     'flux averaged concentrations')
 110  format ('STOPPING Execution')

      end subroutine read_output_info
