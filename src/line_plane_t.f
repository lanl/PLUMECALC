      subroutine line_plane_t(xin,yin,zin,tin,alam,amue,anue,
     1     ibout_last,ibout,xout,yout,zout,tout,ttemp)
c s kelkar March 24, 06
c given the local coordinates of a point xin,yin,zin,t1 with respect to 
c a node# ig in coarse grid, and the indices i,j,k that specify
c the subgrid cell that the point falls in, find the exit point 
c xout,yout,zout along the line specified by alam, amue,anue
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************
c tin=time at which the particle entered the subcell, 
c     measured from the time at which it entered the coarse cell, ie
c     =0 when particle entered the coarse cell
c boundary_time(1:6) = time to boundary plane minus
c     the time of starting in the sub-cell
c ttemp = delta time to cross the cell (=tout-tin)
c tout = time particle exits the subcell, measured from the time
c     at which it entered the coarse cell
c INPUT
c     ig : node # of the node in the coarse grid
c     i,j,k : indices representing the cell in the subgrid
c     xin,yin,zin,tin : coordinates of the initial point
c     alam,amue,anue : equation of the straight line
c OUTOUT
c     xout,yout,zout,tout: coordinates of the exit point 
c     ibout= index giving the exit plane, as follows:
c           1 : -x
c           2 : -y
c           3 : -z
c           4 : +x
c           5 : +y
c           6 : +z
      
      use comparttr_sg
      use comunits

      implicit none

      logical temp_flag

      integer ig,i,j,k,ind,ibout,ibout_last

      real*8 xin,yin,zin,alam,amue,anue,boundary_time(6)
      real*8 epsilon,xout,yout,zout,tout,ttemp,tin
      epsilon = 1.e-22

      do ind=1,6
         boundary_time(ind)=1.e+22
      enddo

c find the intersection of the line with the boundary planes
c in the direction of the particle movement
      if(alam.gt.epsilon) then
         boundary_time(4)=(boundary_cord(4)-xin)/alam
      elseif(alam.lt.-epsilon) then
         boundary_time(1)=(boundary_cord(1)-xin)/alam
      endif
      if(amue.gt.epsilon) then
         boundary_time(5)=(boundary_cord(5)-yin)/amue
      elseif(amue.lt.-epsilon) then
         boundary_time(2)=(boundary_cord(2)-yin)/amue
      endif      
      if(anue.gt.epsilon) then
         boundary_time(6)=(boundary_cord(6)-zin)/anue
      elseif(anue.lt.-epsilon) then
         boundary_time(3)=(boundary_cord(3)-zin)/anue
      endif 

c the face on which particle entered the subcell needs to be
c excluded as a possible outlet face. Do this by setting 
c boundary_time(ibout_last) =-1 as a flag
      if(ibout_last.gt.0.and.ibout_last.le.6) then
         boundary_time(ibout_last) =-1.d00
      endif

c out of those planes to which travel time is +ve,
c find the boundary plane for which travel time is minimum.

      ttemp=1.e+22
      temp_flag=.true.
      do ind=1,6
c         if(boundary_time(ind).gt.epsilon) then
         if(boundary_time(ind).ge.0.) then
            if(boundary_time(ind).lt.ttemp) then
               ttemp=boundary_time(ind)
               ibout=ind
               temp_flag=.false.
            endif
         endif
      enddo

      if(temp_flag) then
         write(error_unit_number,*)
     1        'delta-t < epsilon in line_plane_t.STOP'
         write(error_unit_number,*)
     1        'xin,yin,zin,tin,alam,amue,anue='
         write(error_unit_number,*)
     1        xin,yin,zin,tin,alam,amue,anue
         stop
      endif

      xout=xin+alam*ttemp
      yout=yin+amue*ttemp
      zout=zin+anue*ttemp
      tout=ttemp+tin

      return

      end

c........................................................................
