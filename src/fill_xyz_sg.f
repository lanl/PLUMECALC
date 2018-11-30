      subroutine fill_xyz_sg( )
!
      use comgrid
      use comgrid_sg
      use comparttr
      use comparttr_sg
!
      implicit none
!
!
      integer icount, ii, i, j, k, itemp
      real*8  xtmp, ytmp, ztmp
      real*8  eps_x, eps_y, eps_z
      real*8  x_dx, y_dy, z_dz
      integer local_debug
      data    local_debug / 0 /
!
!     loop over all cells, refined or not      
!
      icount = 1
      do ii = 1, n_grid_points
       if(ip_touched_cells(ii) .ne. 0)then
        k=1
        do while (k .le. nxyz(ip_touched_cells(ii),3))
        eps_z = dxyz(ii,3)*1.e-8
        z_dz  = corn(ii,3)+dxyz(ii,3)
        ztmp = corn(ii,3) + 
     1         (2*k-1)*(dxyz(ii,3)/nxyz(ip_touched_cells(ii),3)/2.0d0)
C       Begin Check for outside face, edge or corner
        if(abs(z(ii)- corn(ii,3)).lt. eps_z) then
           if(k.eq.1) then
              ztmp = corn(ii,3)
           endif
        endif
        if(k.eq.nxyz(ip_touched_cells(ii),3)) then
           if(abs(z(ii)- z_dz) .lt. eps_z) then
              ztmp = z_dz
           endif
        endif
C       End Check for outside face, edge or corner
        j=1
        do while (j .le. nxyz(ip_touched_cells(ii),2))
        eps_y = dxyz(ii,2)*1.e-8
        y_dy  = corn(ii,2)+dxyz(ii,2)
        ytmp = corn(ii,2) + 
     1         (2*j-1)*(dxyz(ii,2)/nxyz(ip_touched_cells(ii),2)/2.0d0)
C       End Check for outside face, edge or corner
        if(abs(y(ii)- corn(ii,2)).lt. eps_y) then
           if(j.eq.1) then
              ytmp = corn(ii,2)
           endif
        endif
        if(j.eq.nxyz(ip_touched_cells(ii),2)) then
           if(abs(y(ii)- y_dy) .lt. eps_y) then
              ytmp = y_dy
           endif
        endif
C
        i=1
        do while (i .le. nxyz(ip_touched_cells(ii),1))
        eps_x = dxyz(ii,1)*1.e-8
        x_dx  = corn(ii,1)+dxyz(ii,1)
        xtmp = corn(ii,1) + 
     1         (2*i-1)*(dxyz(ii,1)/nxyz(ip_touched_cells(ii),1)/2.0d0)
C       Begin Check for outside face, edge or corner
        if(abs(x(ii)- corn(ii,1)).lt. eps_x) then
           if(i.eq.1) then
              xtmp = corn(ii,1)
           else
              xtmp= corn(ii,1) + 
     1         (i-1)*(dxyz(ii,1)/nxyz(ip_touched_cells(ii),1))
           endif
        endif
        if(abs(x(ii)- x_dx) .lt. eps_x) then
           if(i.eq.nxyz(ip_touched_cells(ii),1)) then
              xtmp = x_dx
           else
              xtmp=corn(ii,1) + 
     1         (i)*(dxyz(ii,1)/nxyz(ip_touched_cells(ii),1))
           endif
        endif
C       End Check for outside face, edge or corner
        x_sg(icount) = xtmp
        y_sg(icount) = ytmp
        z_sg(icount) = ztmp
        icount = icount + 1
        i = i + 1
       if(local_debug .eq. 1)then
          itemp=icount-1
          write(6,100)ii,x(ii),y(ii),z(ii),corn(ii,1),corn(ii,2)
     1         ,corn(ii,3)
          write(6,100)itemp,x_sg(itemp),y_sg(itemp),z_sg(itemp)
 100      format(i8,6e14.6)
       endif
        enddo
        j = j + 1
        enddo
        k = k + 1
        enddo
       else
        x_sg(icount) = x(ii)
        y_sg(icount) = y(ii)
        z_sg(icount) = z(ii)
        icount = icount + 1
       endif
      enddo
!
      return
      end subroutine fill_xyz_sg

