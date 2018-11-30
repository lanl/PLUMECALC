      subroutine subgrid_driver()
      
      use comunits
      use comparttr
      use comparttr_sg
      use comsim, only : conc_string
      
      implicit none
      
      integer ijob
      
      if (if_subgrid .eq. 0)then
         return
      else

         flag_avs=.false.
      
         call read_sptr_corn_dxyz_info()
      
         ijob = 0
         if(input_flag_sg .eq. 1)then
            ijob = 1
         endif
      
         call compute_nxyz(ijob)

         call fill_ip_touched_cells()
         call fill_ip_touched_cells_sg()
         call fill_ip_cell_to_cell_sg()
         call fill_id_parent_sg()
      
         call allocmem_subgrid()
         if(conc_string.eq.'favg') call  water_flux_interpolate_sg

         call march_particles()

         if(flag_avs) call write_particle_tracks_avs()

         call variable_update()

c         call delayed_times
      
      endif
      
      return
      end subroutine subgrid_driver
