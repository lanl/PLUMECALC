      subroutine path_arrays_sg(np,id_cell,xg,yg,zg,time_global,
     1     iout,jout,kout,i_sg_global,index,index_np_cell,advg)
c s kelkar March 28 06
c call routines to map to global info and output
c xg,yg,zg are global coordinates of the particle
c******************************************************************** 
c INPUT VARIABLES
c     np : particle #
c     id_cell : # of the global cell in the initial numbering system
c     xg,yg,zg,: local coordinates of the partricle with
c                        respect to subcell given by iout,jout,kout
c     iout,jout,kout : local indices wrt id_cell giving the local
c                      subcell
c     time_global : current value of the global time for the particle
c
c OUTOUT VARIABLES
c     i_sg_global : global # of the current subcell in the new 
c                  numbering system
c     index_np_cell :  pointer for storage arrays 
c     cell_packed_sg(index_np_cell)=i_sg_global
c     x_packed_sg(index_np_cell)=xg
c     y_packed_sg(index_np_cell)=yg
c     z_packed_sg(index_np_cell)=zg
c     time_packed_sg(index_np_cell)=time_global
c
c INTERNAL VARIABLES
c     ip_touched_cells(id_cell)
c********************************************************************
      use comparttr
      use comparttr_sg

      implicit none

      integer np,cell_start,iout,jout,kout,i_sg_global,index
      integer index_np_cell,id_cell,sg_ijk_to_global_cell 
      integer nx,ny,nz

      real*8 xout,yout,zout,time_global,xg,yg,zg,tout,advg

      n_cells_sg(np)=n_cells_sg(np)+1

      i_sg_global=sg_ijk_to_global_cell(iout,jout,kout,
     1     id_cell)
c     1     ip_touched_cells(id_cell))

c...s kelkar temp fix for verification...5/03/06.......
c      i_sg_global=id_cell
c      id_parent_sg(i_sg_global)=id_cell
c.....................................................
 
c      call define_pointers(i_sg_out,xg,yg,zg,tout)

      index_np_cell=index_np_cell+1
      if(index_np_cell.le.n_packed_sg_tmp) then
         cell_packed_sg(index_np_cell)=i_sg_global
         x_packed_sg(index_np_cell)=xg
         y_packed_sg(index_np_cell)=yg
         z_packed_sg(index_np_cell)=zg
         time_packed_sg(index_np_cell)=time_global
         adv_packed_sg(index_np_cell)=advg
      else
         call resize_packed_sg(np)
         cell_packed_sg(index_np_cell)=i_sg_global
         x_packed_sg(index_np_cell)=xg
         y_packed_sg(index_np_cell)=yg
         z_packed_sg(index_np_cell)=zg
         time_packed_sg(index_np_cell)=time_global
         adv_packed_sg(index_np_cell)=advg
      endif

c      write(91,1111)id_cell,i_sg_global,xg,yg,zg,time_global
 1111 format(2(i8,2x),4(f7.3,2x))
  
      return

      end

c..................................................................
