      subroutine write_particle_tracks_avs()
      use comparttr
      use comparttr_sg
      implicit none

      integer lu, nnode_att, nelem_att, nelem
      integer open_file
      
      lu = open_file('particle_track.inp', 1, 'UNKNOWN')
      
      nnode_att = 4
      nelem_att = 0
      
      nelem = n_packed_sg - npart
      
      call write_header_avs(lu,n_packed_sg,nelem,
     1                    nnode_att,nelem_att)
      call write_node_coordinates_avs(lu)
      call write_elem_connectivity_avs(lu)
      call write_node_att_header_avs(lu)
      call write_node_att_avs(lu)
c      call write_elem_att_header_avs(lu)
c      call write_elem_att_avs(lu)
      
      close (lu)
      
      return
      end subroutine write_particle_tracks_avs
cccccccccccccccccc
      subroutine write_header_avs(lu,nnode, nelem, nnode_att, nelem_att)
      integer lu, nnode, nelem, nnode_att, nelem_att
      
      write(lu,100)nnode, nelem, nnode_att, nelem_att,0
 100  format(i12,2x,i12,2x,i5,2x,i5,2x,i5)
      return
      end
cccccccccccccccccc
      subroutine write_node_coordinates_avs(lu)
      use comparttr
      use comparttr_sg
      implicit none
      integer np, icell1, icell2, num_steps_np, index
      integer node_count
      integer lu

      node_count = 0
      do np=1,npart
         icell1=cell_index_sg(np)
         if(np.eq.npart) then
            icell2=n_packed_sg+1
         else
            icell2=cell_index_sg(np+1)
         endif
         num_steps_np=icell2-icell1         
         do index=1,num_steps_np
            node_count = node_count + 1
            write(lu,100)node_count,
     2           x_packed_sg(index-1+icell1),
     3           y_packed_sg(index-1+icell1),
     4           z_packed_sg(index-1+icell1)
  100       format(i12,3(1pe20.12))
         enddo
      enddo
      return
      end subroutine write_node_coordinates_avs
cccccccccccccccccc
      subroutine write_elem_connectivity_avs(lu)
      use comparttr
      use comparttr_sg
      implicit none
      integer np, icell1, icell2, num_steps_np, index
      integer cell_sg
      integer node_count, line_count
      integer lu

      node_count = 0
      line_count = 0
      do np=1,npart
         icell1=cell_index_sg(np)
         if(np.eq.npart) then
            icell2=n_packed_sg+1
         else
            icell2=cell_index_sg(np+1)
         endif
         num_steps_np=icell2-icell1         
         do index=1,num_steps_np
            node_count = node_count + 1
            if(index .ne. 1)then
               line_count = line_count + 1
               write(lu,100)line_count,np,
     1                      ' line ',node_count-1,node_count
  100       format(i12,i6,a,2i12)
            endif
         enddo
      enddo
      return
      end subroutine write_elem_connectivity_avs
cccccccccccccccccc
      subroutine write_node_att_avs(lu)
      use comparttr
      use comparttr_sg
      implicit none
      integer np, icell1, icell2, num_steps_np, index
      integer node_count
      integer lu

      node_count = 0
      do np=1,npart
         icell1=cell_index_sg(np)
         if(np.eq.npart) then
            icell2=n_packed_sg+1
         else
            icell2=cell_index_sg(np+1)
         endif
         num_steps_np=icell2-icell1         
         do index=1,num_steps_np
            node_count = node_count + 1
            write(lu,100)node_count,
     1           time_packed_sg(index-1+icell1),
     2           cell_packed_sg(index-1+icell1),
     3           id_parent_sg(cell_packed_sg(index-1+icell1)),
     4           np
  100       format(i12,1pe20.12,3i12)
         enddo
      enddo
      return
      end subroutine write_node_att_avs
cccccccccccccccccc
      subroutine write_node_att_header_avs(lu)
      use comparttr_sg
      implicit none
      integer np, icell1, icell2, num_steps_np, index
      integer node_count
      integer lu
      
      write(lu, *)4, 1, 1, 1, 1
      write(lu,100)
  100 format('time_packed, real')
      write(lu,200)
  200 format('cell_packed, integer')
      write(lu,300)
  300 format('id_parent, integer')
      write(lu,400)
  400 format('np, integer')
  
      return
      end subroutine write_node_att_header_avs
cccccccccccccccccc
