      subroutine move_subcell(np,x1,y1,z1,cell_start,x_end,y_end,
     &     z_end,time_end,
     1    cell_end,x2,y2,z2,alam,amue,anue,xin,yin,zin,tin,
     2    nx,ny,nz,iin,jin,kin,i_sg_in, exit_flag ,
     3     ibout,iout,jout,kout,xout,yout,zout,tout,ibout_last,ttemp)
c s kelkar March 28 06
c call routines to move the particle one subgrid block
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      implicit none

      logical exit_flag

      integer np,icell,nx,ny,nz,i,j,k,i1,j1,k1,ig,ibout
      integer cell_start,iin,jin,kin,cell_end,i_sg_in
      integer iout,jout,kout,ibout_last

      real*8  x_end,y_end,z_end,time_end
      real*8 x1,y1,z1,x2,y2,z2,xin,yin,zin,tin,xout,yout,zout,tout
      real*8 alam,amue,anue,ttemp

      call boundary_planes(cell_end,iin,jin,kin,nx,ny,nz)
c equn of the line is defined wrt xin,yin,zin and not x1,y1,z1
      call line_plane_t(xin,yin,zin,tin,alam,amue,anue,
     1     ibout_last,ibout,xout,yout,zout,tout,ttemp)
      call next_cell_ijk(ibout,iin,jin,kin,xout,yout,zout,tout,
     1     ttemp,tin,alam,amue,anue,iout,jout,kout)
      call check_exit(cell_end,nx,ny,nz,ibout,iout,jout,kout,
     1     exit_flag)
      
      return

      end

c..................................................................
