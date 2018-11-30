      subroutine local_global(i,xl,yl,zl,xg,yg,zg)
c s kelkar March 24, 06
c map from local  to global coordinates, origin at 
c corn(i,1:3) and scale axis by dx,dy, dz where
c i is a node# in the coarse grid, corn gives the lower, left, back
c corner of the brick shaped control volume ofi, and dx,dy,dz
c are the x,y,z cell sizes.
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************
c INPUT
c     i   : node id in the coarse grid
c     xl,yl,zl: node coordinates in the global system
c     corn(i,1:3): coordinates in the global system
c                  of the corner to be used as the origin
c     dx,dy,dz: cell sizes in the global system 
c OUTOUT
c     xg,yg,zg: node coordinates in the local system
      
      use comparttr_sg
      use comunits

      implicit none

      integer i

      real*8 xg,yg,zg,xl,yl,zl,dx,dy,dz

      dx=dxyz(i,1)
      dy=dxyz(i,2)
      dz=dxyz(i,3)

      if(dx*dy*dz.eq.0.) then
         write(error_unit_number,*)' zero cell size in global_local',
     1        'for cell# ',i
         write(error_unit_number,*)'STOP'
         stop
      else
         xg=xl*dx+corn(i,1)
         yg=yl*dy+corn(i,2)
         zg=zl*dz+corn(i,3)
      endif

      return

      end

c........................................................................
