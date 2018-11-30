      subroutine particle_setup(np,x_start,y_start,z_start,
     1     time_start,cell_start,x1,y1,z1,ibout_last,
     2     index,index_np_cell,adv_start)
c s kelkar March 28 06
c set up initial variables to start march along a particle trajectory
c******************************************************************** 
c INPUT VARIABLES
c     np : particle #
c     
c OUTOUT VARIABLES
c     cell_start : 
c     x_start, y_start,z_start,
c     x1,y1,z1
c     iin,jin,kin
c     time_start
c     ibout_last
c     nx,ny,nz
c     index_np_cell
c     cell_index_sg(np)=index_np_cell
c INTERNAL VARIABLES
c     cell_index(np)
c     cell_packed(cell_index(np))
c     nxyz(ip_touched_cells(cell_start),1:3):
c     x_packed, y_packed, z_packed (cell_index(np)) : 
c     time_packed(cell_index(np)) : 
c     
c********************************************************************

      use comparttr
      use comparttr_sg

      implicit none

      integer np,cell_start,ibout_last
      integer index,index_np_cell, i_sg_in
      integer nx,ny,nz,iin,jin,kin

      real*8 x_start, y_start,z_start,time_start,adv_start 
      real*8 x1,y1,z1,xin,yin,zin

      time_start=time_packed(cell_index(np))
      x_start=x_packed(cell_index(np))
      y_start=y_packed(cell_index(np))
      z_start=z_packed(cell_index(np))
      cell_start=cell_packed(cell_index(np))
      adv_start=adv_packed(cell_index(np))
      ibout_last=0
      call global_local(cell_start,x_start,y_start,z_start,
     1     x1,y1,z1)
      
      nx=nxyz(ip_touched_cells(cell_start),1)
      ny=nxyz(ip_touched_cells(cell_start),2)
      nz=nxyz(ip_touched_cells(cell_start),3)
      call xyz_ijk(cell_start,nx,ny,nz,x1,y1,z1,iin,jin,kin)
c      call local_global(id_cell,x,y,z,xg,yg,zg)
      call path_arrays_sg(np,cell_start,x_start, y_start,z_start,
     1     time_start,iin,jin,kin,i_sg_in,index,index_np_cell,
     2     adv_start)

      cell_index_sg(np)=index_np_cell
         
      return

      end

c..................................................................
