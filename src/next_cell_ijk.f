      subroutine next_cell_ijk(ibout,i1,j1,k1,xout,yout,zout,tout,
     1     ttemp,tin,alam,amue,anue,i2,j2,k2)
c s kelkar March 24, 06
c given the exit plane ibout, find the i,j,k for the next cell
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************
c INPUT
c     ibout= index giving the exit plane, as follows:
c     i1,j1,k1 : indices representing the cell in the subgrid
c OUTOUT
c     i2,j2,k2 : indices representing the cell in the subgrid
c           1 : -x
c           2 : -y
c           3 : -z
c           4 : +x
c           5 : +y
c           6 : +z
      
      use comparttr_sg

      implicit none

      integer ibout,i1,j1,k1,i2,j2,k2

      real*8 xout,yout,zout,tout,ttemp,tin,alam,amue,anue
      real*8 epsilon,epsilonx,epsilony,epsilonz
      epsilon = 1.d-14

      i2=i1
      j2=j1
      k2=k1

c      if(ibout.eq.1) i2=i1-1
c      if(ibout.eq.2) j2=j1-1
c      if(ibout.eq.3) k2=k1-1
c      if(ibout.eq.4) i2=i1+1
c      if(ibout.eq.5) j2=j1+1
c      if(ibout.eq.6) k2=k1+1

      epsilonx=epsilon*abs(boundary_cord(1)-boundary_cord(4))
      epsilony=epsilon*abs(boundary_cord(2)-boundary_cord(5))
      epsilonz=epsilon*abs(boundary_cord(3)-boundary_cord(6))

      if(xout.le.(boundary_cord(1)+epsilonx)) then
         if(alam.lt.0.0) then
            i2=i1-1
         endif
      elseif(xout.ge.(boundary_cord(4)-epsilonx)) then
         if(alam.gt.0.0) then
            i2=i1+1
         endif
      endif

      if(yout.le.(boundary_cord(2)+epsilony)) then
         if(amue.lt.0.0) then
            j2=j1-1
         endif
      elseif(yout.ge.(boundary_cord(5)-epsilony)) then
         if(amue.gt.0.0) then
            j2=j1+1
         endif
      endif

      if(zout.le.(boundary_cord(3)+epsilonz)) then
         if(anue.lt.0.0) then
            k2=k1-1
         endif
      elseif(zout.ge.(boundary_cord(6)-epsilonz)) then
         if(anue.gt.0.0) then
            k2=k1+1
         endif
      endif

      return

      end

c........................................................................
