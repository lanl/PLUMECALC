      subroutine boundary_planes(ig,i,j,k,nx,ny,nz)
c s kelkar March 24, 06
c given the local indices i,j,k that specify the subgrid cell
c within the coarse grid cell ig, define the equations of the planes
c bounding the control volume. 
c******************************************************************** 
c INPUT VARIABLES
c     ig : node # of the node in the coarse grid
c     nx,ny,nz : # of divisions along the x,y,z,axis for the subgrid
c     i,j,k : indices representing the cell in the subgrid
c
c OUTOUT VARIABLES
c     boundary_cord(1:6) : value of the coordinate for the plane
c                 bounding the control volume of the node ig in the 
c                 following order:
c           1 : -x
c           2 : -y
c           3 : -z
c           4 : +x
c           5 : +y
c           6 : +z
c 
c INTERNAL VARIABLES
c********************************************************************
      
      use comparttr_sg
      use comunits

      implicit none

      integer ig,i,j,k,nx,ny,nz

      if(nx*ny*nz.eq.0) then
       write(error_unit_number,*)'zero subdivisions in boundary_planes '
     1        ,ig
         write(error_unit_number,*)'STOP'
         stop
      else
         boundary_cord(1)= dfloat((i-1))/dfloat(nx)
         boundary_cord(2)= dfloat((j-1))/dfloat(ny)
         boundary_cord(3)= dfloat((k-1))/dfloat(nz)
         boundary_cord(4)= dfloat((i))/dfloat(nx)
         boundary_cord(5)= dfloat((j))/dfloat(ny)
         boundary_cord(6)= dfloat((k))/dfloat(nz)
      endif

      return

      end

c........................................................................
