      subroutine cell_setup(np,icell,x1,y1,z1,time_start,
     &     cell_start,ibou_last,x_start, y_start,z_start,x_end,
     &     y_end,z_end,time_end,cell_end,x2,y2,z2,
     1     alam,amue,anue,xin,yin,zin,tin,
     2     nx,ny,nz,iin,jin,kin,i_sg_in,line_flag,advect)
c s kelkar March 28 06
c set up initial variables to start march along a particle trajectory
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************


      use comparttr
      use comparttr_sg

      implicit none

      logical line_flag

      integer np,icell,nx,ny,nz,i,j,k,i1,j1,k1,ig,ibout
      integer i_sg_in,cell_start,cell_end,iin,jin,kin,ibou_last

      real*8  x_end,y_end,z_end,time_end,advect
      real*8 x1,y1,z1,x2,y2,z2,xin,yin,zin,tin,xout,yout,zout,tout
      real*8 alam,amue,anue
      real*8 x_start,y_start,z_start,time_start

      time_end=time_packed(cell_index(np)+icell)
      if(time_end.le.time_start) then
         flag_new_cell=.false.
         ibou_last=0
         x_end=x_packed(cell_index(np)+icell)
         y_end=y_packed(cell_index(np)+icell)
         z_end=z_packed(cell_index(np)+icell)
         advect=adv_packed(cell_index(np)+icell)
         cell_end=cell_packed(cell_index(np)+icell)
      else
         flag_new_cell=.true.
         ibou_last=0
         x_end=x_packed(cell_index(np)+icell)
         y_end=y_packed(cell_index(np)+icell)
         z_end=z_packed(cell_index(np)+icell)
         advect=adv_packed(cell_index(np)+icell)
         cell_end=cell_packed(cell_index(np)+icell)
         call global_local(cell_end,x_start,y_start,z_start,
     1        x1,y1,z1)
         call global_local(cell_end,x_end,y_end,z_end,
     1        x2,y2,z2)
         call parametric_line(x1,y1,z1,time_start,x2,y2,z2,time_end,
     1        alam,amue,anue,line_flag)
         xin=x1
         yin=y1
         zin=z1
         tin=0
         
         nx=nxyz(ip_touched_cells(cell_end),1)
         ny=nxyz(ip_touched_cells(cell_end),2)
         nz=nxyz(ip_touched_cells(cell_end),3)
            if(cell_end.eq.30805) then
               write(*,*)
            endif
         call xyz_ijk(cell_end,nx,ny,nz,xin,yin,zin,iin,jin,kin)
c     call global_node_id(iin,jin,kin,i_sg_in)
      endif

      return
      
      end

c..................................................................
