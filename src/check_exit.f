      subroutine check_exit(cell_start,nx,ny,nz,ibout,i,j,k,
     1     exit_flag)
c s kelkar March 28 06
c check if the particle has reached a boundary of the cell on the 
c coarse grid 
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      implicit none

      logical exit_flag

      integer i,j,k,nx,ny,nz,cell_start,ibout
   
      if(i.lt.1) then
         i=1
         exit_flag=.true.
      elseif(i.gt.nx) then
         i=nx
         exit_flag=.true.
      endif

      if(j.lt.1) then
         j=1
         exit_flag=.true.
      elseif(j.gt.ny) then
         j=ny
         exit_flag=.true.
      endif

      if(k.lt.1) then
         k=1
         exit_flag=.true.
      elseif(k.gt.nz) then
         k=nz
         exit_flag=.true.
      endif

      return

      end

c....................................................................
