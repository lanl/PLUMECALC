      subroutine variable_update()
!
      use comgrid
      use comgrid_sg
      use comparttr
      use comparttr_sg
      use comrock
      use comsim

!
      implicit none

      integer i
!
!     comparttr variables
!
c  n_cells,cell_packed,time_packed are now
c  done in variable_packed_update
c      deallocate(n_cells)
c      allocate(n_cells(npart))
c      n_cells = n_cells_sg
c      deallocate(n_cells_sg)
      
c      deallocate(cell_packed)
c      allocate(cell_packed(n_packed_sg))
c      cell_packed = cell_packed_sg
c      deallocate(cell_packed_sg)
c      
c      deallocate(time_packed)
c      allocate(time_packed(n_packed_sg))
c      time_packed = time_packed_sg
c      deallocate(time_packed_sg)
      
c pack the cell_packed, time_packed, and cell_index arrays      
      call variable_packed_update

      deallocate(cell_path)
      allocate(cell_path(n_grid_points_sg))
      cell_path = cell_path_sg
      if (allocated(cell_path_sg)) deallocate(cell_path_sg)

      deallocate(touched_cells)
      allocate(touched_cells(n_touched_cells_sg))
      touched_cells = touched_cells_sg
      deallocate(touched_cells_sg)
      
      deallocate(concentration)
      allocate(concentration(n_touched_cells_sg))
      concentration = concentration_sg
      deallocate(concentration_sg)

      deallocate(conc_mobile)
      allocate(conc_mobile(n_touched_cells_sg))
      conc_mobile = conc_mobile_sg
      deallocate(conc_mobile_sg)

      if (conc_string .ne. 'favg') then
         deallocate(conc_total)
         allocate(conc_total(n_touched_cells_sg))
         conc_total = conc_total_sg
         deallocate(conc_total_sg)
         deallocate(conc_mobtot)
         allocate(conc_mobtot(n_touched_cells_sg))
         conc_mobtot = conc_mobtot_sg
         deallocate(conc_mobtot_sg)
      end if
!
!     what to do with cfavg
!
!
!     comgrid variables
!
      allocate(x_sg(n_grid_points_sg))
      allocate(y_sg(n_grid_points_sg))
      allocate(z_sg(n_grid_points_sg))
      x_sg=0.
      y_sg=0.
      z_sg=0.
      call fill_xyz_sg( )

      deallocate(x)
      allocate(x(n_grid_points_sg))
      x = x_sg
      deallocate(x_sg)
      
      deallocate(y)
      allocate(y(n_grid_points_sg))
      y = y_sg
      deallocate(y_sg)
      
      deallocate(z)
      allocate(z(n_grid_points_sg))
      z = z_sg
      deallocate(z_sg)
      
      if(conc_string.eq.'favg') then

         nfavgzones = n_grid_points_sg

         deallocate(water_flux)
         allocate(water_flux(n_grid_points_sg))
         water_flux = water_flux_sg
         deallocate(water_flux_sg)

         deallocate(cfavg)
         allocate(cfavg(n_grid_points_sg))
         cfavg = 0.

         deallocate(conc_total)
         allocate(conc_total(n_grid_points_sg))
         conc_total = 0.
         deallocate(conc_total_sg)

         deallocate(index_favg)
         allocate(index_favg(n_grid_points_sg))
         do i = 1, n_grid_points_sg
            index_favg(i) = i
         end do

      endif

      allocate(r8_work(n_grid_points_sg))
      call vector_to_scale_sg_vector_r8(sx1,r8_work)
      deallocate(sx1)
      allocate(sx1(n_grid_points_sg))
      sx1 = r8_work
      deallocate(r8_work)
      
      deallocate(izonef)
      allocate(izonef(n_grid_points_sg))
      izonef = izonef_sg
      deallocate(izonef_sg)
!
!     comrock variables
!
      allocate(r8_work(n_grid_points_sg))
      call vector_to_sg_vector_r8(denr,r8_work)
      deallocate(denr)
      allocate(denr(n_grid_points_sg))
      denr = r8_work
      deallocate(r8_work)
      
      allocate(r8_work(n_grid_points_sg))
      call vector_to_sg_vector_r8(ps,r8_work)
      deallocate(ps)
      allocate(ps(n_grid_points_sg))
      ps = r8_work
      deallocate(r8_work)
       
      allocate(r8_work(n_grid_points_sg))
      call vector_to_sg_vector_r8(rfac,r8_work)
      deallocate(rfac)
      allocate(rfac(n_grid_points_sg))
      rfac = r8_work
      deallocate(r8_work)
      
      allocate(r8_work(n_grid_points_sg))
      call vector_to_sg_vector_r8(vcf,r8_work)
      deallocate(vcf)
      allocate(vcf(n_grid_points_sg))
      vcf = r8_work
      deallocate(r8_work)

      allocate(i4_work(n_grid_points_sg))
      call vector_to_sg_vector_i4(itrc_diff,i4_work)
      deallocate(itrc_diff)
      allocate(itrc_diff(n_grid_points_sg))
      itrc_diff = i4_work
      deallocate(i4_work)
      
!     These will be needed for routine delayed_times which is now
!     called after subgridding has been performed
      if (diffusion_model) then
         allocate(r8_work(n_grid_points_sg))
         call vector_to_sg_vector_r8(sigma_partial,r8_work)
         deallocate(sigma_partial)
         allocate(sigma_partial(n_grid_points_sg))
         sigma_partial = r8_work
         deallocate(r8_work)
      
         allocate(r8_work(n_grid_points_sg))
         call vector_to_sg_vector_r8(omega_partial,r8_work)
         deallocate(omega_partial)
         allocate(omega_partial(n_grid_points_sg))
         omega_partial = r8_work
         deallocate(r8_work)

      end if
      
!
! comsim variables
!
      if(out_string.ne.'node') then
         deallocate(out_cell)
         allocate(out_cell(n_grid_points_sg))
         do i=1,n_grid_points_sg
            if(cell_path(i).ne.0) out_cell(i)=1
         enddo
      endif

!
!     Reset scalar variables at the end
!
c n_packed is done in variable_packed_update
c      n_packed = n_packed_sg
      n_grid_points = n_grid_points_sg
      n_touched_cells = n_touched_cells_sg
!
      return
      end subroutine variable_update
