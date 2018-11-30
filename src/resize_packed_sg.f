      subroutine resize_packed_sg(np)
c     July 12, 06. S kelkar
c     increase the dimensions of packed_sg arrays 
      
      USE realloc_mod
      use comparttr_sg
      use comparttr, only : npart 
      use comunits
      
      implicit none
      
      integer old_size,np
      real*8 size_fac
      
      integer local_debug
      data    local_debug / 1 /

      resize_counter=resize_counter+1
      old_size = n_packed_sg_tmp
      if(np.gt.1) then
         size_fac=(npart*1.d0)/((np-1)*1.d0)
      else
         size_fac=1.25
      endif
      n_packed_sg_tmp = n_packed_sg_tmp * size_fac

      if(local_debug.eq.1) then
         write(error_unit_number,*)
     1        'resize#, old,new:',resize_counter,old_size,
     1        n_packed_sg_tmp
      endif
      
      x_packed_sg => reallocate(x_packed_sg,n_packed_sg_tmp)  
      y_packed_sg => reallocate(y_packed_sg,n_packed_sg_tmp)  
      z_packed_sg => reallocate(z_packed_sg,n_packed_sg_tmp)  
      time_packed_sg => reallocate(time_packed_sg,n_packed_sg_tmp)  
      cell_packed_sg=> iallocate(cell_packed_sg,n_packed_sg_tmp)  
      adv_packed_sg => reallocate(adv_packed_sg,n_packed_sg_tmp)  
      
      return
      
      end

c...............................................................
