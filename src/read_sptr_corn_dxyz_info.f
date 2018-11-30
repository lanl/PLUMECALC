      subroutine read_sptr_corn_dxyz_info()
!***********************************************************************
c s kelkar April 13 2006. 
c this routine modified from  
c /scratch/eeyore1/dash/FEHM_V2.x/SRC_V2.30/OMR/OMR_Jan06/sptr_save.f
c read corn(i,1:3) and dxyz(i,j) arrays from the "vel_fin.sptr"
c type file output by FEHM. These arrays define the brick-shaped
c control volume at each node. 
c 
c******************************************************************** 
c INPUT VARIABLES
c     sptr_corn_file_type : defines type of input file
c                        =1 ASCII
c                        =2 Unformatted
c     n_grid_points : # of grid points in the grid, com
c OUTOUT VARIABLES
c     corn(1:n_grid_points,1:3) : global coordinates of  the lower, 
c                                left, back corner
c     dxyz*1:n_grid_points,1:3 : sides of the control volume along
c                                 x,y,z axis
c INTERNAL VARIABLES
c     iscratch,i,j,k,node_count
c********************************************************************

      use comunits
      use comgrid
      use comparttr_sg

      implicit none
      
      integer sflag, i, j, k
      integer node_count, ndum
      integer iscratch


! Read stored sptr geometry arrays arrays
      if (sptr_corn_file_type.eq.1) then
         read (sptr_corn_unit_number, *) node_count
         if(node_count.gt.0) then
            read (sptr_corn_unit_number, *)
     1           (iscratch, i = 1, node_count)
            do j = 1, 7
               read (sptr_corn_unit_number, *)
     1              (iscratch, i=1,n_grid_points)
            end do
         endif
         do j = 1, 7
            read (sptr_corn_unit_number, *)
     1           (iscratch, i=1,n_grid_points)
         end do
c     do k = 1, komrmax
c     do j = -3, 3
c     read (sptr_corn_unit_number, *) (isave_omr(i,j,k), i=1,node_count)
c     end do
c     end do
         read (sptr_corn_unit_number, *) (dxyz(i,1), i=1,n_grid_points)
         read (sptr_corn_unit_number, *) (dxyz(i,2), i=1,n_grid_points)
         read (sptr_corn_unit_number, *) (dxyz(i,3), i=1,n_grid_points)
         do j = 1, 3
            read (sptr_corn_unit_number, *) 
     1           (corn(i, j), i=1,n_grid_points)
         end do
         do j = -3, 3
            read (sptr_corn_unit_number, *) 
     1           (ggg(i, j), i=1,n_grid_points)
         end do
c         read (sptr_corn_unit_number, *) 
c     1        (ps_trac(i), i=1,n_grid_points)
      elseif(sptr_corn_file_type.eq.2) then
         read (sptr_corn_unit_number) node_count
         if(node_count.gt.0) then
            read (sptr_corn_unit_number) 
     1           (iscratch, i = 1, node_count)
            do j = 1, 7
               read (sptr_corn_unit_number) 
     1              (iscratch, i=1,n_grid_points)
            end do
         endif
         do j = 1,7
            read (sptr_corn_unit_number) 
     1           (iscratch, i=1,n_grid_points)
         end do
c     do k = 1, komrmax
c     do j = -3, 3
c     read (sptr_corn_unit_number) (isave_omr(i,j,k), i=1,node_count)
c     end do
c     end do
         read (sptr_corn_unit_number) (dxyz(i,1), i=1,n_grid_points)
         read (sptr_corn_unit_number) (dxyz(i,2), i=1,n_grid_points)
         read (sptr_corn_unit_number) (dxyz(i,3), i=1,n_grid_points)
         do j = 1, 3
            read (sptr_corn_unit_number)
     1           (corn(i, j), i=1,n_grid_points)
         end do
         do j = -3, 3
            read (sptr_corn_unit_number) (ggg(i, j), i=1,n_grid_points)
         end do
c         read (sptr_corn_unit_number) 
c     1        (ps_trac(i), i=1,n_grid_points)
      else
         write(error_unit_number,*)'Wrong format for sptr_corn_file'
         write(error_unit_number,*)'STOP'
         stop
      end if
      
      return
      
      end subroutine read_sptr_corn_dxyz_info
