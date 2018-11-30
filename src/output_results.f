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
     2     conc_string, nfavgzones, water_flux, sparse, alt_string,
     3     index_favg, nodes_favg, prntvar
      use comparttr, only : n_touched_cells, touched_cells, cfavg, 
     2     concentration, cell_path, cthreshold, conc_mobile, conc_total
      use comgrid, only : x, y, z
      use comparttr_sg, only : if_subgrid, id_parent_sg
      implicit none

      integer i, indx, len
      real*8 current_time, cum_moles
      character*50 formstring
      character*120 data_string
      logical first, alt
      save formstring

c****************Begin executable statements here ******************

      if(conc_string(1:3).eq.'fav') then
c     Output flux averaged concentrations
         if (flux_unit_number .ne. 0 .or. alt_string .eq. 'alt') then
            if (out_string(1:3) .eq. 'tec') then
               cum_moles = 0.d0
               if (first) then
                  if (alt_string .ne. 'alt') then
                     if (prntvar(1) .and. prntvar(2)) then
                        if (if_subgrid .eq. 0) then
                           write(output_unit_number, 61)
                        else
                           write(output_unit_number, 65)
                        end if
                     else if (prntvar(1)) then
                        if (if_subgrid .eq. 0) then
                           write(output_unit_number, 62)
                        else
                           write(output_unit_number, 66)
                        end if
                     else if (prntvar(2)) then
                        if (if_subgrid .eq. 0) then
                           write(output_unit_number, 63)
                        else
                           write(output_unit_number, 67)
                        end if
                     else
                        if (if_subgrid .eq. 0) then
                           write(output_unit_number, 64)
                        else
                           write(output_unit_number, 68)
                        end if
                     end if
                     write(output_unit_number, 90) current_time
                     do i = 1, n_touched_cells
                        data_string = ''
                        if (if_subgrid .eq. 0) then
                           write(data_string, 85) 
     &                          x(touched_cells(i)), 
     &                          y(touched_cells(i)),
     &                          z(touched_cells(i)), touched_cells(i),
     &                          cfavg(touched_cells(i))
                        else
                           write(data_string, 89) 
     &                          x(touched_cells(i)), 
     &                          y(touched_cells(i)),
     &                          z(touched_cells(i)), 
     &                          id_parent_sg(touched_cells(i)),
     &                          touched_cells(i),
     &                          cfavg(touched_cells(i))
                        end if
                        len = len_trim(data_string)
                        if (prntvar(1)) then
                           len = len_trim(data_string)
                           write(data_string(len + 1 : 120), 86) 
     &                          water_flux(touched_cells(i))
                        end if
                        if (prntvar(2)) then
                           len = len_trim(data_string)
                           write(data_string(len + 1 : 120), 86) 
     &                          conc_total(touched_cells(i))
                        end if
                        write(output_unit_number, 87) 
     &                       trim(data_string)
                        cum_moles =cum_moles + 
     &                       conc_total(touched_cells(i))
                     end do
                  else
                     if (prntvar(2)) then
                        write(output_unit_number, 73)
                     else
                        write(output_unit_number, 74)
                     end if
                     write(output_unit_number, 90) current_time
                     do i = 1, nfavgzones
                        data_string = ''
                        indx = index_favg(nodes_favg(i))
                        write(data_string, 85) 
     &                       x(nodes_favg(i)), y(nodes_favg(i)),
     &                       z(nodes_favg(i)), nodes_favg(i),
     &                       cfavg(indx)
                        if (prntvar(2)) then
                           len = len_trim(data_string)
                           write(data_string(len + 1 : 120), 86) 
     &                          conc_total(indx)
                        end if
                        write(output_unit_number, 87) 
     &                       trim(data_string)
                        cum_moles =cum_moles + conc_total(indx)
                     end do
                  end if

              else
                 if (sparse) then
                    if (prntvar(1)) then
                       if (if_subgrid .eq. 0) then
                          write(output_unit_number, 104) current_time
                       else
                          write(output_unit_number, 107) current_time
                       end if
                    else
                       if (if_subgrid .eq. 0) then
                          write(output_unit_number, 102) current_time
                       else
                          write(output_unit_number, 105) current_time
                       end if
                    end if
                 else
                    if (prntvar(1)) then
                       if (if_subgrid .eq. 0) then
                          write(output_unit_number, 103) current_time
                       else
                          write(output_unit_number, 106) current_time
                       end if
                    else
                       write(output_unit_number, 101) current_time
                    end if
                 end if
                 if (alt_string .ne. 'alt') then
                    do i = 1, n_touched_cells
                       data_string = ''
                       if (.not. sparse) then
                          if (if_subgrid .eq. 0) then
                             write (data_string, 88)
     &                            touched_cells(i)
                          else
                             write (data_string, 88)
     &                            id_parent_sg(touched_cells(i)),
     &                            touched_cells(i)
                          end if
                       end if
                       len = len_trim (data_string)
                       write (data_string(len + 1 : 120), 86)
     &                      cfavg(touched_cells(i))
                       if (prntvar(2)) then
                          len = len_trim (data_string)
                          write(data_string(len + 1 : 120), 86)
     &                         conc_total(touched_cells(i))  
                       end if
                       write(output_unit_number, 87) trim(data_string)
                       cum_moles = cum_moles + 
     &                      conc_total(touched_cells(i))
                    end do
                 else
                    do i = 1, nfavgzones
                       data_string = ''
                       if (.not. sparse) write (data_string, '(i7)')
     &                      nodes_favg(i)
                       indx = index_favg(nodes_favg(i))
                       len = len_trim (data_string)
                       write (data_string(len + 1 : 120), 86)
     &                         cfavg(indx)
                       if (prntvar(2)) then
                          len = len_trim (data_string)
                          write (data_string(len + 1 : 120), 86)
     &                         conc_total(indx)
                       end if
                       write(output_unit_number, 87) trim(data_string)
                       cum_moles = cum_moles + conc_total(indx)
                    end do
                 end if
               end if
               if (prntvar(2)) write (output_unit_number, 80) cum_moles

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
 61      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)", "water flux (l/day)", '
     &        '"total moles"')
 62      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)", "water flux (l/day)"')
 63      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)", "total moles"')
 64      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)"')
 65      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)", ',
     &        '"water flux (l/day)", "total moles"')
 66      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)", ',
     &        '"water flux (l/day)"')
 67      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)", ',
     &        '"total moles"')
 68      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)"')
 73      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles)", "total moles"')
 74      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles)"')
 75      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles)", "total moles"')
 76      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles)"')
 80      format ('text = "Total moles = ', g16.9, '"')
 85      format (3(g16.9, 1x), i7, 1x, g16.9)
 86      format (1x, g16.9)
 87      format (a)
 88      format (2(i7, 1x))
 89      format (3(g16.9, 1x), 2(i7, 1x), g16.9)
      else
c     Resident concentrations
         if(first) then
            if (out_string(1:3).ne.'tec')
     &           write(output_unit_number, *) ntimes, n_touched_cells
            if (out_string(1:4).eq.'pckd') then
               write(output_unit_number,*) (touched_cells(i), 
     &              i=1, n_touched_cells)
            end if
            if (out_string(1:3).eq.'tec') then
               if (if_subgrid .eq. 0) then
                  if (prntvar(1) .and. prntvar(2)) then
                     write(output_unit_number, 91)
                  else if (prntvar(1)) then
                     write(output_unit_number, 92)
                  else if (prntvar(2)) then
                     write(output_unit_number, 93)
                  else
                     write(output_unit_number, 94)
                  end if
               else
                  if (prntvar(1) .and. prntvar(2)) then
                     write(output_unit_number, 95)
                  else if (prntvar(1)) then
                     write(output_unit_number, 96)
                  else if (prntvar(2)) then
                     write(output_unit_number, 97)
                  else
                     write(output_unit_number, 98)
                  end if
               end if
               write(output_unit_number, 90) current_time
            end if
         else
            if (out_string(1:4).eq.'tecp') then
               if (sparse) then
                  if (if_subgrid .eq. 0) then
                     write(output_unit_number, 102) current_time
                  else
                     write(output_unit_number, 105) current_time
                  end if
               else
                  write(output_unit_number, 101) current_time
               end if
            end if
         end if
 90      format ('zone t="time ', g16.9,'"')
 91      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)", "mobile conc (moles/l)", '
     &        '"total moles"')
 92      format ('variables= "x", "y", "z", "node", ',
     &        '"concentration (moles/l)", "mobile conc (moles/l)"')
 93      format ('variables= "x", "y", "z", "node", ',
     &        '"mobile conc (moles/l)", "total moles"')
 94      format ('variables= "x", "y", "z", "node", ',
     &        '"mobile conc (moles/l)"')
 95      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)", ',
     &        '"mobile conc (moles/l)", "total moles"')
 96      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "concentration (moles/l)", ',
     &        '"mobile conc (moles/l)"')
 97      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "mobile conc (moles/l)", "total moles"')
 98      format ('variables= "x", "y", "z", "node_parent", ',
     &        '"node_subgrid", "mobile conc (moles/l)"')
         if(out_string(1:4).eq.'pckd') then
            write(output_unit_number,*) current_time
            write(output_unit_number,*) (conc_mobile(i), 
     &           i=1, n_touched_cells)
         elseif(out_string(1:4).eq.'tecp') then
            cum_moles = 0.d0
            do i = 1, n_touched_cells
               data_string = ''
               if (first) then
                  if (if_subgrid .eq. 0) then
                     write(data_string, 85)  
     &                    x(touched_cells(i)), y(touched_cells(i)), 
     &                    z(touched_cells(i)), touched_cells(i)
                  else
                     write(data_string, 89)  
     &                    x(touched_cells(i)), y(touched_cells(i)), 
     &                    z(touched_cells(i)), 
     &                    id_parent_sg(touched_cells(i)),
     &                    touched_cells(i)
                  end if
               else
                  if (.not. sparse) then
                     if (if_subgrid .eq. 0) then
                        write(data_string, 88) touched_cells(i)
                     else
                        write(data_string, 88) 
     &                       id_parent_sg(touched_cells(i)),
     &                       touched_cells(i)
                     end if
                  end if
               end if
                  
               if (prntvar(1)) then
                  len = len_trim(data_string)
                  write (data_string(len + 1 : 120), 86) 
     &                 concentration(i)
               end if
               len = len_trim(data_string)
               write (data_string(len + 1 : 120), 86) 
     &              conc_mobile(i)
               if (prntvar(2)) then
                  len = len_trim(data_string)
                  write (data_string(len + 1 : 120), 86) 
     &                 conc_total(i)
               end if
               cum_moles = cum_moles + conc_total(i) 
               write(output_unit_number, 87) trim(data_string)
            end do
            if (prntvar(2)) write (output_unit_number, 80) cum_moles

 100        format (1x, 3(g16.9, 1x), i7, 3(1x, g16.9))
 101        format ('zone t="time ',g16.9,'", VARSHARELIST = ([1-3]=1)')
 102        format ('zone t="time ',g16.9,'", VARSHARELIST = ([1-4]=1)')
 103        format ('zone t="time ',g16.9,
     &           '", VARSHARELIST = ([1-3,6]=1)')
 104        format ('zone t="time ',g16.9,
     &           '", VARSHARELIST = ([1-4,6]=1)')
 105        format ('zone t="time ',g16.9,'", VARSHARELIST = ([1-5]=1)')
 106        format ('zone t="time ',g16.9,
     &           '", VARSHARELIST = ([1-3,7]=1)')
 107        format ('zone t="time ',g16.9,
     &           '", VARSHARELIST = ([1-5,7]=1)')
 110        format (i7, 3(1x, g16.9))
 111        format (g16.9, 2(1x, g16.9))
 112        format (g16.9)
 115        format (1x, i7, 1x, g16.9)
         elseif(out_string(1:4).eq.'node') then
            write(output_unit_number, 112) current_time
            do i = 1, noutnodes
               if(cell_path(ioutnode(i)).ne.0) then
                  write(output_unit_number,110) ioutnode(i), 
     2                 conc_mobile(cell_path(ioutnode(i)))
               else
                  write(output_unit_number,110) ioutnode(i), 
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
     &                    conc_mobile(cell_path(ioutnode(i)))
                  else
                     write(output_unit_number, 100)  
     &                    x(ioutnode(i)), y(ioutnode(i)), 
     &                    z(ioutnode(i)), ioutnode(i), 0.
                  end if
               end do
            else
c     Always sparse
               write(output_unit_number, 102) current_time
               do i = 1, noutnodes
                  if(cell_path(ioutnode(i)).ne.0) then
                     write(output_unit_number, 112) 
     &                    conc_mobile(cell_path(ioutnode(i)))
                  else
                     write(output_unit_number, 112) 0.
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
      call flush (output_unit_number)
      return
      end subroutine output_results
