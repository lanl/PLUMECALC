      subroutine output_results(first,current_time)
!***********************************************************************
! $Id: output_results.f,v 1.5 2006/09/20 19:48:06 zvd Exp $
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
!
!     PSEUDOCODE
!
!     IF this is the first time step
!        write number of times computed, total number of cells output
!     ENDIF this is the first time step
!
!     IF the entire avs file is being output
!        construct filename
!        open file
!        write header information
!        FOR each grid point
!           write the node number and concentration
!        ENDFOR each grid point
!     ELSE the compact version of the file is being written
!        write time to output file
!        FOR each point that has the possibility of nonzero concentration
!           write node number and concentration
!        ENDFOR each point that has the possibility of nonzero concentration
!     ENDIF
!
!***********************************************************************

      use comunits, only : error_unit_number, output_unit_number,
     2     flux_unit_number
      use comsim, only : out_string, ntimes, noutnodes, ioutnode,
     2     conc_string, nfavgzones
      use comparttr, only : n_touched_cells, touched_cells,
     2     concentration, cell_path, cfavg, cthreshold
      use comgrid, only : x, y, z
      implicit none

      integer i
      real*8 current_time
      character*50 formstring
      logical first
      save formstring

c****************Begin executable statements here ******************

      if(conc_string.eq.'favg') then

         if (flux_unit_number .ne. 0) then
            if (out_string(1:3) .eq. 'tec') then
               if (first) then
                  write(output_unit_number, 90)
                  write(output_unit_number, 95) current_time
                  do i = 1, n_touched_cells 
                     write(output_unit_number, 100) 
     &                    x(touched_cells(i)), y(touched_cells(i)), 
     &                    z(touched_cells(i)), touched_cells(i),
     &                    cfavg(touched_cells(i))
                  end do
               else
                  write(output_unit_number, 105) current_time
                  do i = 1, n_touched_cells
                     write(output_unit_number, 110) 
     &                    cfavg(touched_cells(i))
                  end do
               end if
            else if (out_string(1:4) .eq. 'pckd') then
!               if (first) write(output_unit_number, *) n_touched_cells
               write(output_unit_number, 115) -1, current_time
               do i = 1, n_touched_cells
                  if (cfavg(touched_cells(i)) .ge. cthreshold) 
     &                 write(output_unit_number, 115) touched_cells(i),
     &                 cfavg(touched_cells(i))
               end do
               
            endif
         else
            if(first) then
               write (formstring, 40) nfavgzones
               write(output_unit_number, formstring) (i, i=1,nfavgzones)
               write (formstring, 50) nfavgzones
            end if
            write(output_unit_number,formstring) current_time,
     2           (cfavg(i),i=1,nfavgzones)
         end if
 40      format ("('  Time(days)', ",i4,"(6x, 'Zone', i4.4, 3x))")
 50      format ("(g16.9, ",i4,"(1x, g16.9))")

      else

         if(first) then
            if (out_string(1:3).ne.'tec')
     &           write(output_unit_number, *) ntimes, n_touched_cells
            if (out_string(1:4).eq.'pckd') then
               write(output_unit_number,*) (touched_cells(i), 
     &              i=1, n_touched_cells)
            end if
            if (out_string(1:3).eq.'tec') then
               write(output_unit_number, 90)
               write(output_unit_number, 95) current_time
            end if
         end if
 90      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)"')
 95      format ('zone t="time ', g16.9,'"')
         if(out_string(1:4).eq.'pckd') then
            write(output_unit_number,*) current_time
            write(output_unit_number,*) (concentration(i), 
     &           i=1, n_touched_cells)
         elseif(out_string(1:4).eq.'tecp') then
            if (first) then
               do i = 1, n_touched_cells
                  write(output_unit_number, 100)  
     &                 x(touched_cells(i)), y(touched_cells(i)), 
     &                 z(touched_cells(i)), touched_cells(i),
     &                 concentration(i)
               end do
            else
               write(output_unit_number, 105) current_time
               do i = 1, n_touched_cells
                  write(output_unit_number, 110) concentration(i)
               end do
            end if
 100        format (1x, 3(g16.9, 1x), i7, 1x, g16.9)
 105        format ('zone t="time ',g16.9,'", VARSHARELIST = ([1-4]=1)')
 110        format (g16.9)
 115        format (1x, i7, 1x, g16.9)
         elseif(out_string(1:4).eq.'node') then
            write(output_unit_number, 110) current_time
            do i = 1, noutnodes
               if(cell_path(ioutnode(i)).ne.0) then
                  write(output_unit_number,115) ioutnode(i), 
     2                 concentration(cell_path(ioutnode(i)))
               else
                  write(output_unit_number,115) ioutnode(i), 
     2                 0.              
               end if
            end do
         elseif(out_string(1:4).eq.'tecn') then
            if (first) then
               do i = 1, noutnodes
                  if(cell_path(ioutnode(i)).ne.0) then
                     write(output_unit_number, 100)  
     &                    x(ioutnode(i)), y(ioutnode(i)), 
     &                    z(ioutnode(i)), ioutnode(i),
     &                    concentration(cell_path(ioutnode(i)))
                  else
                     write(output_unit_number, 100)  
     &                    x(ioutnode(i)), y(ioutnode(i)), 
     &                    z(ioutnode(i)), ioutnode(i), 0.
                  end if
               end do
            else               
               write(output_unit_number, 105) current_time
               do i = 1, noutnodes
                  if(cell_path(ioutnode(i)).ne.0) then
                     write(output_unit_number, 110)  
     &                    concentration(cell_path(ioutnode(i)))
                  else
                     write(output_unit_number, 110) 0.
                  end if
               end do
            end if
         else
            write(error_unit_number, 120) out_string(1:4)
            write(error_unit_number, 130) 
            write(error_unit_number, 140) 
            stop
 120        format ('Unrecognized output option: ', a4)
 130        format ('use keyword pckd, tecp, node, or tecn instead')
 140        format ('STOPPING execution')
         end if
      end if
      return
      end subroutine output_results
