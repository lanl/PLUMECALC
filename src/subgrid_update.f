      subroutine subgrid_update(iin,jin,kin,iout,jout,kout,
     1  xin,yin,zin,tin,xout,yout,zout,tout,ibout,ibout_last)   
c s kelkar March 28 06
c update variables to move to next sub-grid cell
c******************************************************************** 
c INPUT VARIABLES
c     iout,jout,kout : indices representating the upcoming subcell
c     xout,yout,zout,tout : local coordinates and time representating 
c                          the position of the particle refered to the 
c                          current subcell
c OUTOUT VARIABLES
c     iin,jin,kin : indices representating upcoming subcell, which 
c                   becomes the current subcell for the next interation
c      xin,yin,zin,tin :local coordinates and time representating 
c                          the position of the particle refered to the 
c                          current(outdated) subcell 
c     ibout_last :  the exit plane of the particle as seen from
c                   the new control volume (ie +x goes to -x etc)
c INTERNAL VARIABLES
c********************************************************************

      implicit none

      integer iin,jin,kin,iout,jout,kout
      integer ibout,ibout_last

      real*8 xin,yin,zin,xout,yout,zout,tin,tout

      xin=xout
      yin=yout
      zin=zout
      tin=tout
      iin=iout
      jin=jout
      kin=kout
c ibout_last is the exit plane of the particle as seen from
c the new control volume (ie +x goes to -x etc)
      ibout_last=mod(ibout+3,6)
  
      return

      end

c..................................................................
