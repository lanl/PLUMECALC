      subroutine xyz_ijk(ig,nx,ny,nz,x,y,z,i,j,k)
c s kelkar March 24, 06
c given the local coordinates of a point x,y,z, with respect to 
c a node# ig in coarse grid, find the indices i,j,k that specify
c the subgrid cell that the point falls in. 
c 1<=i<=nx, 1<=j<=ny, 1<=k<=nz 
c******************************************************************** 
c INPUT VARIABLES
c     ig : node # of the node in the coarse grid
c     x,y,z: coordinates of the  point in local system wrt ig
c     nx,ny,nz : # of divisions along the x,y,z,axis for the subgrid
c     
c OUTOUT VARIABLES
c     i,j,k : indices representating the location of the subcell
c             indices go along the x,y,z, axis respectively
c INTERNAL VARIABLES
c********************************************************************

      use comunits

      implicit none

      integer ig,i,j,k,nx,ny,nz

      real*8 x,y,z

      if(nx*ny*nz.gt.0) then
         i = int(x*nx)+1
         j = int(y*ny)+1
         k = int(z*nz)+1
c the next correction is for a popint sitting on a + face.
c for example, suppose point is on a +x face, ie x=1. then
c we get i=(1*nx)+1=nx+1 > nx : this is bad as it puts the 
c point in the next cell, so force i<= nx etc.
         if(i.gt.nx) i=nx
         if(j.gt.ny) j=ny
         if(k.gt.nz) k=nz
      else
         write(error_unit_number,*)' some of nx,ny,nz = 0 for ig=',ig
         write(error_unit_number,*)' subroutine xyz_ijk. STOP'
         stop
      endif

      return

      end

c........................................................................
