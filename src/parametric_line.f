      subroutine parametric_line(x1,y1,z1,t1,x2,y2,z2,t2,
     1     alam,amue,anue,line_flag)
c s kelkar March 24, 06
c parametric equation of a straight line passing thru the points
c (x1,y1,z1), and (x2,y2,z2), given by, with t as the parameter,
c with values t1 and t2 at the two points respectively
c x = x1 + alam*(t-t1)
c y = y1 + amue*(t-t1)
c z = z1 + anue*(t-t1)
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************
c INPUT
c     x1,y1,z1 and x2,y2,z2 : coordinates of the two points
c OUTOUT
c     alam,amue,anue : coeffecients in the parametric equations
c 
      
      use comunits

      implicit none

      logical line_flag

      real*8 x1,y1,z1,t1,x2,y2,z2,t2,dt,alam,amue,anue

      dt=t2-t1
      line_flag= .true.

      if(dt.eq.0.) then
         write(error_unit_number,*)' zero delta-t in',
     1        'parametric_line'
         write(error_unit_number,*)'STOP'
c     stop
         line_flag = .false.
      else
         alam=(x2-x1)/dt
         amue=(y2-y1)/dt
         anue=(z2-z1)/dt
      endif

      if(alam.eq.0.) then
         if(amue.eq.0.) then
            if(anue.eq.0.) then
               line_flag = .false.
            endif
         endif
      endif

      return

      end

c........................................................................
