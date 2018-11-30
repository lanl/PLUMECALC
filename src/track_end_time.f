      subroutine track_end_time(exit_flag,end_track,time_end,
     1                    time_global,x_end,y_end,z_end,xg,yg,zg)

      implicit none

      logical exit_flag,end_track
      real*8 time_global,time_end,xg,x_end,yg,y_end,zg,z_end

      exit_flag=.true.
      end_track=.true.
      time_global=time_end
      xg=x_end
      yg=y_end
      zg=z_end

      return

      end

c......................................................................
