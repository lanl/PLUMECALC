      subroutine march_particles
c s kelkar March 27, 06
c march along each particle path and call the proper routines to 
c form the subgrids along each path
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      use comparttr
      use comparttr_sg
      use comunits
      
      implicit none

      logical exit_flag, end_track, line_flag

      integer np,nx,ny,nz,i,j,k,ig,index,index_np_cell
      integer cell_start, icell,cell_end,iin,jin,kin,i_sg_in
      integer iout,jout,kout,i_sg_out,i2,j2,k2,ibout,ibout_last
      integer icell1,icell2,num_steps_np
      integer cell_parent, cell_sg, index_sub

      real*8 x_start, y_start,z_start,x_end,y_end,z_end
      real*8 time_start, time_end, time_global
      real*8 x1,y1,z1,x2,y2,z2,xin,yin,zin,tin,xout,yout,zout,tout
      real*8 alam,amue,anue,xg,yg,zg,advg,ttemp,advect

      integer local_debug
      data    local_debug / 0 /

c      open(unit=91,file='march_particles.out')

      index_np_cell=0
      index=0
      resize_counter=0
      n_cells_sg = 0

      do np=1,npart
         end_track=.false.
         call particle_setup(np,x_start, y_start,z_start,
     1        time_start,cell_start,x1,y1,z1,ibout_last,
     2        index,index_np_cell,advect)
c...................................................................
c         write(*,*)' np,cell_start,time_start=',np, cell_start,
c     1        time_start
c...................................................................
         
         icell1=cell_index(np)
         if(np.eq.npart) then
            icell2=n_packed+1
         else
            icell2=cell_index(np+1)
         endif
         num_steps_np=icell2-icell1
         icell=1
c         do icell=1,num_steps_np-1
         do while (.not.end_track)

            call cell_setup(np,icell,x1,y1,z1,time_start,cell_start,
     &           ibout_last,x_start,y_start,z_start,x_end,y_end,z_end,
     1           time_end,cell_end,x2,y2,z2,alam,amue,anue,xin,yin,
     2           zin,tin,nx,ny,nz,iin,jin,kin,i_sg_in,line_flag,advect)
            
c...................................................................
c            write(*,*)'  MOVE CELL'
c            write(*,*)' cell_start,time_start',cell_start,time_start
c            write(*,*)' cell_end, time_end ',cell_end,time_end 
c            write(*,*)'     x_start, y_start,z_start',x_start,y_start,
c     1           z_start
c            write(*,*)'     x1,y1,z1', x1,y1,z1  
c            write(*,*)'      nx,ny,nz ',nx,ny,nz
c            write(*,*)'       iin,jin,kin',iin,jin,kin
c            write(*,*)'        x2,y2,z2', x2,y2,z2
c            write(*,*)'        alam,amue,anue ',alam,amue,anue
c...................................................................

            if(flag_new_cell.and.line_flag) then
               exit_flag=.false.
               index_sub=0
               do while(.not.exit_flag)
                  index=index+1
                  index_sub=index_sub+1
                  call move_subcell(np,x1,y1,z1,cell_start,x_end,y_end,
     &                 z_end,time_end,
     1                 cell_end,x2,y2,z2,alam,amue,anue,xin,yin,zin,tin,
     2                 nx,ny,nz,iin,jin,kin,i_sg_in, exit_flag,  
     3                 ibout,iout,jout,kout,xout,yout,zout,tout,
     4                 ibout_last,ttemp)
                  
                  time_global=tout+time_start
c                  if (time_start .ne. time_end) then
c                     advg = advect * ttemp / (time_start - time_end)
c                  else
c                     advg = advect
c                     advg = 0.
c                  end if
c if time_global > time_end for the last particle entry,
c the sptr2 track in FEHM had terminated in the cell.
c set flags to end loop for this particle, reset variables 
                  if(time_global.gt.time_end) then
                     if(icell.ge.num_steps_np-1)then 
                        call track_end_time(exit_flag,end_track,
     1                 time_end,time_global,x_end,y_end,z_end,xg,yg,zg)
                     endif
                  endif
                  
c...................................................................
c     write(*,*)'        MOVE subcell'
c     write(*,111) xin,yin,zin,tin
c     write(*,111)xout,yout,zout,tout
c     111              format(20x,4(2x,f8.4))
c     write(*,*)'           iin,jin,kin,iout,jout,kout ',
c     1                 iin,jin,kin,iout,jout,kout        
c...................................................................
                  
c     if the particle is still in the cell, find global coordinates   
                 if(.not.end_track)
     1                 call local_global(cell_end,xout,yout,zout,
     2                 xg,yg,zg)

c write subgrid arrays
                  call path_arrays_sg(np,cell_end,xg,yg,zg,
     1                 time_global,iin,jin,kin,i_sg_out,index,
     2                 index_np_cell,advect)
                  call subgrid_update (iin,jin,kin,iout,jout,kout,
     1                 xin,yin,zin,tin,xout,yout,zout,tout,
     2                 ibout,ibout_last)
                  
               enddo
            else
               call path_arrays_sg(np,cell_end,x_end,y_end,z_end,
     1              time_global,iin,jin,kin,i_sg_out,index,
     2              index_np_cell,advect)
            endif

            call cell_update(x_start,x_end,y_start,y_end,z_start,z_end
     1           ,time_start,time_end,x1,x2,y1,y2,z1,z2,
     2           cell_start,cell_end)

            if(icell.ge.num_steps_np-1) then
               end_track=.true.
            else
               icell=icell+1
            endif

         enddo
         
c...................................................................
c         write(*,*)' CELL END. POSITION END'
c         write(*,*)cell_end,x_end,y_end,z_end
c         write(*,*)
c...................................................................
         
c         write(*,*) ' part#,index_np_cell=', np,index_np_cell
      enddo
      
      n_packed_sg=index_np_cell

      call fill_touched_cells_sg

      if(resize_counter.gt.0) then
         write(error_unit_number,*)
         write(error_unit_number,*)'********************************'
         write(error_unit_number,*)'arrays of type *_packed_sg '
         write(error_unit_number,*)' resized ',resize_counter,' times'
         write(error_unit_number,*)
     1   ' array size:initial guess, final guess, actual needed='
         write(error_unit_number,*)n_packed_sg_max*size_fac_initial,
     1        n_packed_sg_tmp,n_packed_sg
         write(error_unit_number,*)'*******************************'
         write(error_unit_number,*)
      endif 

c      close(91)
c................................................................
      if(local_debug.eq.1) then
         open(unit=92,file='packed_sg.out')
         write(92,*)'index-1+icell1,np,cell_packed_sg,x_packed_sg,',
     1        'y_packed_sg,z_packed_sg,time_packed_sg'
         do np=1,npart
            icell1=cell_index_sg(np)
            if(np.eq.npart) then
               icell2=n_packed_sg+1
            else
               icell2=cell_index_sg(np+1)
            endif
            num_steps_np=icell2-icell1         
            do index=1,num_steps_np
               cell_sg=cell_packed_sg(index-1+icell1)
               cell_parent=id_parent_sg(cell_sg)
               write(92,1111)index-1+icell1,np,cell_sg,
     2              x_packed_sg(index-1+icell1),
     3              y_packed_sg(index-1+icell1),
     4              z_packed_sg(index-1+icell1),
     5              time_packed_sg(index-1+icell1)
c     write(*,1111)index-1+icell1,np,cell_sg,
c     2           x_packed_sg(index-1+icell1),
c     3           y_packed_sg(index-1+icell1),
c     4           z_packed_sg(index-1+icell1),
c     5           time_packed_sg(index-1+icell1)
 1111          format(3(i8,2x),4(g15.8,2x))
            enddo
         enddo
         close(92)
      endif
c....................................................................

      return
      
      end
      
c...................................................................
