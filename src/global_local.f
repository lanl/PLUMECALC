      subroutine global_local(i,xg,yg,zg,xl,yl,zl)
c s kelkar March 24, 06
c map from global coordinates to local coordinates, origin at 
c corn(i,1:3) and scale axis by ddx(i),ddy(i), ddz(i) where
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
c     xg,yg,zg:  coordinates in the global system
c     corn(i,1:3): coordinates in the global system
c                  of the corner to be used as the origin
c     dx(i),dy(i),dz(i): cell sizes in the global system 
c OUTOUT
c     xl,yl,zl: node coordinates in the local system
      
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
         xl=(xg-corn(i,1))/dx
         yl=(yg-corn(i,2))/dy
         zl=(zg-corn(i,3))/dz
      endif

      return

      end

c........................................................................
