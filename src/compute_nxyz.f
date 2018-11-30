      subroutine compute_nxyz(ijob)
C***********************************************************************      
!
      use comparttr
      use comparttr_sg
      use comunits, only : error_unit_number
      use comgrid,    only : n_grid_points
      use comgrid_sg, only : n_grid_points_sg
!
      implicit none
C
C     Variables passed into subroutine
C
      integer ijob
C
C     ijob = 0 : Assume nxyz was filled by reading from input.
C                Only compute the number of grid points and
C                verify valid (.ge. 1) values for nxyz.
C
C     ijob = 1 : divide cells into N equal increments so that
C                each increment is less than or equal to the
C                specified length scale.
C
C     ijob = 2 : same as one with the added constraint that N
C                must be a power of ibase = 2. If the value of
C                ibase is modified other powers can be easily
C                implemented.
C
C
C     Subroutine variables
C
      integer i,j,k, lu, icounter
      integer n_power, ibase
      integer nearest_power_int
      integer n_warning_value, if_error
!
      integer local_debug
      data    local_debug / 0 /
!
      n_warning_value = 32
      ibase = 2
C
C***********************************************************************      

      if(ijob .gt. 0)then
C
C     Compute number of subdivisions (nxyz), based on
C     cell size (dxyz) and desired length scale (scale_xyz)
C
      icounter = 0
      do i = 1, n_touched_cells
         j = touched_cells(i)
         do k = 1,3
            nxyz(i,k) = 
     1                   int(dxyz(j,k)/scale_xyz(i,k) + 1.0d0 - 
     2                 1.d-8*dxyz(j,k)/scale_xyz(i,k))
         enddo
      enddo
      endif
C
C     Promote integers to nearest larger power of 2
C      
      if(ijob .eq. 2)then
      do i = 1, n_touched_cells
         do k = 1,3
           n_power = nearest_power_int(ibase,nxyz(i,k))
           j = ibase**n_power
           if(j .lt. nxyz(i,k)) then
              nxyz(i,k) = j*ibase
           else
              nxyz(i,k) = j
           endif
         enddo
      enddo
      endif
C
C     Print error if nx,ny,nz is less than 1
C      
      if_error = 0
      do i = 1, n_touched_cells
         if((nxyz(i,1).lt. 1).or.
     1      (nxyz(i,2).lt. 1).or.
     2      (nxyz(i,3).lt. 1))then
            if_error = if_error + 1
            write(error_unit_number,*)
     1       'ERROR Subgrid Cell Refinement ERROR'
            write(error_unit_number,100)
     1       dxyz(i,1),scale_xyz(i,1), nxyz(i,1)
            write(error_unit_number,110)
     1       dxyz(i,2),scale_xyz(i,2), nxyz(i,2)
            write(error_unit_number,120)
     1       dxyz(i,3),scale_xyz(i,3), nxyz(i,3)
          endif
          if(if_error .ne. 0)then
            write(error_unit_number,*)
     1       'ERROR-STOP in compute_nxyz-ERROR'
            stop
          endif
      enddo
C
C     Compute the number of grid points in the subgrid.
C
      n_grid_points_sg = 0
      do i = 1, n_touched_cells
         n_grid_points_sg = n_grid_points_sg + 
     1                      nxyz(i,1)*nxyz(i,2)*nxyz(i,3)
      enddo
      n_grid_points_sg = 
     1  n_grid_points_sg + n_grid_points - n_touched_cells
C
C     Print warnings if nx,ny,nz is larger than n_warning_value
C      
      do i = 1, n_touched_cells
         if((nxyz(i,1).gt. n_warning_value).or.
     1      (nxyz(i,2).gt. n_warning_value).or.
     2      (nxyz(i,3).gt. n_warning_value))then
            write(error_unit_number,*)
     1       'Subgrid Cell Refinement WARNING'
            write(error_unit_number,*)
     1       'Input will result in a large number of '
     2           ,'subgrid refined cells.'
            write(error_unit_number,100)
     1       dxyz(i,1),scale_xyz(i,1), nxyz(i,1)
            write(error_unit_number,110)
     1       dxyz(i,2),scale_xyz(i,2), nxyz(i,2)
            write(error_unit_number,120)
     1       dxyz(i,3),scale_xyz(i,3), nxyz(i,3)
  100       format
     1  ('cell dx =',e14.7,' scale factor =',e14.7,' sg nx=',i6)
  110       format
     1  ('cell dy =',e14.7,' scale factor =',e14.7,' sg ny=',i6)
  120       format
     1  ('cell dz =',e14.7,' scale factor =',e14.7,' sg nz=',i6)
          endif
         enddo
!
!     Output when local_debug is not 0
!
      if(local_debug .ne. 0)then
      
      write(error_unit_number, *) 'compute_nxyz: local_debug .ne. 0'
      write(error_unit_number, *)
     1       'index, cell_id, nxyz(x), dxyz(x), scale_xyz(x)...y...z'
      do i = 1, n_touched_cells
         j = touched_cells(i)
         write(error_unit_number,900)i,j,
     1      nxyz(i,1),dxyz(j,1),scale_xyz(i,1),
     2      nxyz(i,2),dxyz(j,2),scale_xyz(i,2),
     3      nxyz(i,3),dxyz(j,3),scale_xyz(i,3)
 900        format(i8,1x,i8,1x,3(i8,1x,e14.6,1x,e14.6))
      enddo
      write(error_unit_number, *) 'compute_nxyz: local_debug .ne. 0'

      endif
      
      return
      end
      function nearest_power_int(ibase,int_value)
      integer ibase, int_value
      real*8 base, real_value
      
      base = float(ibase)
      real_value = float(int_value)
      nearest_power_int  = nint(log10(real_value)/log10(base))
      
      return
      end
