      subroutine variable_packed_update()
!
      use comgrid
      use comgrid_sg
      use comparttr
      use comparttr_sg
!
      implicit none

      integer i,np,icell,icell1,icell2,count_cell
      integer count_np_cell
      integer, allocatable :: cell_packed_tmp (:)
      real*8,  allocatable :: time_packed_tmp (:)
      real*8,  allocatable :: adv_packed_tmp (:)

      deallocate(n_cells)
      deallocate(cell_packed)
      deallocate(time_packed)
      deallocate(adv_packed)

      allocate(n_cells(npart))
      allocate(cell_packed_tmp(n_packed_sg))
      allocate(time_packed_tmp(n_packed_sg))
      allocate(adv_packed_tmp(n_packed_sg))
      n_cells=0
      cell_packed_tmp=0
      time_packed_tmp=0.
      adv_packed_tmp=0.

      count_cell=0
      icell1=1
      do np=1,npart
      cell_index(np)=count_cell+1
         if(np.eq.npart) then
            icell2=n_packed_sg
         else
            icell2=cell_index_sg(np+1)-1
         endif
         count_np_cell = 0
         do icell=icell1+1,icell2
            count_cell=count_cell+1
            count_np_cell =  count_np_cell +1
            time_packed_tmp(count_cell)=time_packed_sg(icell)
            adv_packed_tmp(count_cell)=adv_packed_sg(icell)
            cell_packed_tmp(count_cell)=cell_packed_sg(icell)
         enddo
         icell1=icell2+1
         n_cells(np)=count_np_cell
      enddo

      deallocate(n_cells_sg)
      deallocate(cell_index_sg)
      deallocate(cell_packed_sg)
      deallocate(time_packed_sg)
      deallocate(adv_packed_sg)

      n_packed = count_cell

      allocate(cell_packed(n_packed))
      allocate(time_packed(n_packed))
      allocate(adv_packed(n_packed))
      cell_packed=0
      time_packed=0

      do i=1,n_packed
         cell_packed(i)=cell_packed_tmp(i)
         time_packed(i)=time_packed_tmp(i)
         adv_packed(i)=adv_packed_tmp(i)
      enddo

      deallocate(cell_packed_tmp)
      deallocate(time_packed_tmp)
      deallocate(adv_packed_tmp)
      
      return
      end subroutine variable_packed_update
c.....................................................................
