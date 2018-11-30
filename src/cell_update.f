      subroutine cell_update(x_start,x_end,y_start,y_end,z_start,z_end,
     1     time_start,time_end,x1,x2,y1,y2,z1,z2,
     2     cell_start,cell_end)   
c s kelkar March 28 06
c update variables to move to next coarse-grid cell for a given
c particle
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      implicit none

      integer cell_start,cell_end 

      real*8 x_start,x_end,y_start,y_end,z_start,z_end
      real*8 time_start,time_end,x1,x2,y1,y2,z1,z2

      time_start=time_end
      x_start=x_end
      y_start=y_end
      z_start=z_end
      cell_start=cell_end
  
      return

      end

c..................................................................
