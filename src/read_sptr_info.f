      subroutine read_sptr_info()
!***********************************************************************
! $Id: read_sptr_info.f,v 1.2 2007/02/14 20:43:21 zvd Exp $
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
!     Internal variables and arrays
!
!     Identifier      Type     Description
!
!     dummy_string character*4 string to examine to look for character 
!                                flag 'summ' to find summary information
!                                for the particle tracking run
!     npart           integer  number of particles in the simulation
!     n_cells         integer  number of cells that each particle passed
!                                through during simulation, array of 
!                                size n_particles 
!     n_packed        integer  total number of cells all particles pass
!                                through
!     cell_packed     integer  array of cell numbers that each particle 
!                                enters, array of size total sum of 
!                                cells passed through for all particles
!     time_packed     integer  array of times that each particle enters 
!                                the corresponding cell in the 
!                                cell_packed array, array of size total 
!                                sum of cells passed through for all 
!                                particles
!     cell_index      integer  array of locations in the packed arrays 
!                                where each particle information starts,
!                                array of size n_particles
!
!***********************************************************************
!     PSEUDOCODE
!
!     2. Read particle tracking information
!
!     ALLOCATE array of number of cells for each particle
!     Scan file until we get to the final array information
!        on particle path information
!     Increment the counter of the number of cells each particle
!        passes through
!     READ number of particles
!     ALLOCATE packed array to store cell information for each particle
!     ALLOCATE packed array to store time information for each particle
!     Initialize working array for building the cell and time arrays
!     REWIND file, skip header information
!
!     FOR EACH line in sptr file
!
!        Store time and cell in packed arrays
!        Increment working array pointer by 1
!
!     ENDFOR each line in sptr file
!
!     CLOSE sptr file
!***********************************************************************

      use comunits
      use comparttr
      use comparttr_sg, only : if_subgrid
      implicit none

      logical done, new_format
      character*4 dummy_string
      integer partno
      integer cellno
      integer i, j, k
      real*8 tdummy, xdummy, ydummy, zdummy
      real*8, allocatable :: start_time_tr(:)
      character*30 verno
      character*11 jdate
      character*8 jtime
      character*3 char3
      character*80 ptitle
      integer, allocatable :: cells_working(:)
      integer, allocatable :: num_part(:), n_pack(:), lines(:)

!***********Begin executable statements here**********

      allocate (num_part(sptr_num), n_pack(sptr_num), lines(sptr_num))
!     Scan file until we get to the final array information
!        on particle path information
      n_packed = 0
      n_pack = 0
      npart = 0
      num_part = 0
      lines = 0
      
      do j = 1, sptr_num
!     Header lines
!     Uses file format from FEHM V2.30, not compatible with
!     previous versions of FEHM
         if(sptr_bin(j)) then
            read(sptr_unit_number(j)) verno, jdate, jtime
            read(sptr_unit_number(j)) ptitle
            read(sptr_unit_number(j)) num_part(j)
            read(sptr_unit_number(j)) char3
            if (char3 .ne. 'XYZ' .and. char3 .ne. 'TRA') then
               rewind(sptr_unit_number(j))
               read(sptr_unit_number(j)) verno, jdate, jtime
               read(sptr_unit_number(j)) ptitle
               read(sptr_unit_number(j)) num_part(j)
            end if               
         else
            read(sptr_unit_number(j), *) verno, jdate, jtime
            read(sptr_unit_number(j), *) ptitle
            read(sptr_unit_number(j), *) num_part(j)
            read(sptr_unit_number(j), *) dummy_string
            char3 = dummy_string(1:3)
            if (char3 .ne. 'XYZ' .and. char3 .ne. 'Par' .and.
     &           char3 .ne. 'TRA' .and. char3 .ne. 'COR') 
     &           backspace (sptr_unit_number(j))
         end if
         npart = npart + num_part(j)
         if (if_subgrid .eq. 1 .and. char3 .ne. 'XYZ') then
            write (error_unit_number,*) 'Program stopped, ',
     &           'XYZ data not found in sptr2 file:', j
            stop
         end if
      end do

!     ALLOCATE array of number of cells for each particle
      allocate(n_cells(npart))
      allocate(cells_working(npart))
      allocate(cell_index(npart))
      allocate(start_time_tr(npart))

      cell_index = 0
      cells_working = 0
      n_cells = 0
      start_time_tr = 0.0

      do j = 1, sptr_num
         if(sptr_bin(j)) then
            read_loop_bin: do
               if (char3 .eq. 'XYZ') then
                  read(sptr_unit_number(j), end = 100) partno, tdummy, 
     &                 cellno, xdummy, ydummy, zdummy
!     Skip time zero data if we are not subgridding
                  lines(j) = lines(j) + 1
                  if (if_subgrid .eq. 0 .and. tdummy .eq. 0.) goto 10
               else
                  read(sptr_unit_number(j), end = 100) partno, tdummy, 
     &                 cellno
                  lines(j) = lines(j) + 1
               end if
!     Increment the counter of the number of cells each particle
!     passes through
               if (j .gt. 1) then
                  do k = 1, j - 1
                     partno = partno +  num_part(k)
                  end do
               end if
               n_cells(partno) = n_cells(partno) + 1
               n_pack(j) = n_pack(j) + 1
 10         end do read_loop_bin
         else
            read_loop: do
               if (char3 .eq. 'XYZ') then
                  read(sptr_unit_number(j), *, end = 100) partno, 
     &                 tdummy, cellno, xdummy, ydummy, zdummy
!     Skip time zero data if we are not subgridding
                  lines(j) = lines(j) + 1
                  if (if_subgrid .eq. 0 .and. tdummy .eq. 0.) goto 20
               else
                  read(sptr_unit_number(j), *, end = 100) partno, 
     &                 tdummy, cellno
                  lines(j) = lines(j) + 1
            end if
!     Increment the counter of the number of cells each particle
!     passes through
               if (j .gt. 1) then
                  do k = 1, j - 1
                     partno = partno +  num_part(k)
                  end do
               end if
               n_cells(partno) = n_cells(partno) + 1
               n_pack(j) = n_pack(j) + 1
 20         end do read_loop
         end if
 100     n_packed = n_packed + n_pack(j)
      end do

!     ALLOCATE packed array to store cell information for each particle
      allocate(cell_packed(n_packed))
!     ALLOCATE packed array to store time information for each particle
      allocate(time_packed(n_packed))
      allocate(adv_packed(n_packed))
      if (char3 .eq. 'XYZ') then
         allocate(x_packed(n_packed))
         allocate(y_packed(n_packed))
         allocate(z_packed(n_packed))
      else
         if (char3 .eq. 'TRA') then
            deallocate(cell_packed, time_packed)
            n_packed = n_packed - npart
            allocate(cell_packed(n_packed))
            allocate(time_packed(n_packed))
         end if
      end if

!     Initialize working array for building the cell and time arrays

      cells_working(1) = 1
      cell_index(1) = 1
      do i = 2, npart
         cells_working(i)=cells_working(i-1)+n_cells(i-1)
         cell_index(i)=cell_index(i-1)+n_cells(i-1)
      end do

!     REWIND file, skip header information
      do j = 1, sptr_num
         rewind sptr_unit_number(j)

         if(sptr_bin(j)) then
            read(sptr_unit_number(j)) verno, jdate, jtime
            read(sptr_unit_number(j)) ptitle
            read(sptr_unit_number(j)) num_part(j)
            if (char3 .eq. 'XYZ' .or. char3 .eq. 'TRA') 
     &           read(sptr_unit_number(j)) char3
         else
            read(sptr_unit_number(j),'(a4)') dummy_string
            read(sptr_unit_number(j),'(a4)') dummy_string
            read(sptr_unit_number(j),'(a4)') dummy_string
            if (char3 .eq. 'XYZ' .or. char3 .eq. 'Par' .or. char3 .eq.
     &           'TRA' .or. char3 .eq. 'COR') 
     &           read(sptr_unit_number(j),'(a4)') dummy_string
         end if

         if(sptr_bin(j)) then
!     FOR EACH line in sptr file
            do i = 1, lines(j)
               if (char3 .eq. 'XYZ') then
                  read(sptr_unit_number(j)) partno, tdummy, cellno,
     &                 xdummy, ydummy, zdummy
!     Skip time zero data if we are not subgridding
                  if (if_subgrid .eq. 0 .and. tdummy .eq. 0.) goto 30
                  if (j .gt. 1) then
                     do k = 1, j - 1
                        partno = partno +  num_part(k)
                     end do
                  end if
                  x_packed(cells_working(partno)) = xdummy
                  y_packed(cells_working(partno)) = ydummy
                  z_packed(cells_working(partno)) = zdummy
               else
                  read(sptr_unit_number(j)) partno, tdummy, cellno
                  if (j .gt. 1) then
                     do k = 1, j - 1
                        partno = partno +  num_part(k)
                     end do
                  end if
               end if
!        Store time and cell in packed arrays
               time_packed(cells_working(partno)) = tdummy
               cell_packed(cells_working(partno)) = cellno
!        Increment working array pointer by 1
               cells_working(partno) = cells_working(partno) + 1
 30         end do
!     ENDFOR each line in sptr file
         else
!     FOR EACH line in sptr file
            do i = 1, lines(j)
               if (char3 .eq. 'XYZ') then
                  read(sptr_unit_number(j),*) partno, tdummy, cellno,
     &                 xdummy, ydummy, zdummy
!     Skip time zero data if we are not subgridding
                  if (if_subgrid .eq. 0 .and. tdummy .eq. 0.) goto 40
                  if (j .gt. 1) then
                     do k = 1, j - 1
                        partno = partno +  num_part(k)
                     end do
                  end if
                  x_packed(cells_working(partno)) = xdummy
                  y_packed(cells_working(partno)) = ydummy
                  z_packed(cells_working(partno)) = zdummy
               else
                  read(sptr_unit_number(j),*) partno, tdummy, cellno
                  if (j .gt. 1) then
                     do k = 1, j - 1
                        partno = partno +  num_part(k)
                     end do
                  end if
               end if
!        Store time and cell in packed arrays
               if (char3 .eq. 'TRA') then
                  if (cellno.le.0) then 
                     start_time_tr(partno) = tdummy
                  else	
                     time_packed(cells_working(partno)) = 
     &                    tdummy - start_time_tr(partno)
                     cell_packed(cells_working(partno)) = cellno
!        Increment working array pointer by 1
                     cells_working(partno) = cells_working(partno) + 1
                  end if
               else		
                  time_packed(cells_working(partno)) = tdummy
                  cell_packed(cells_working(partno)) = cellno
!        Increment working array pointer by 1
                  cells_working(partno) = cells_working(partno) + 1
               end if
 40         end do
!     ENDFOR each line in sptr file
         end if

!     CLOSE sptr file, deallocate working array
         close (sptr_unit_number(j))
      end do

      deallocate(cells_working, num_part, n_pack, lines, start_time_tr)

      end subroutine read_sptr_info
