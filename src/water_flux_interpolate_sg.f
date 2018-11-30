      subroutine water_flux_interpolate_sg
c s kelkar Aug 22, 06.
c linearly interpolate ggg on the subgrid faces to calculate
c the net flux thru the sub-cell
      use comparttr
      use comparttr_sg
      use comrock, only :  ps 
      use comsim, only :  water_flux 
      use comgrid, only :  n_grid_points

      implicit none
      
      integer i,j,k,ix,iy,iz,icell,ipointer
      integer nx,ny,nz,i_sg_global
      integer sg_ijk_to_global_cell
      
      real*8 flx(6), sumflx,flux_out,flux_in,scale_in,scale_out
      real*8 area_xy,area_yz,area_zx, area_xyz(3)
      real*8 area_xy_sg,area_yz_sg,area_zx_sg
      
      do i=1,n_grid_points
         icell=i
c     i is the node # in the unrefined grid.if cell_path(i)=0 then 
c     the cell is not touched, hence not refined
         if(cell_path(i).eq.0) then
            nx=1
            ny=1
            nz=1
         else
            ipointer=ip_touched_cells(icell)
            nx=nxyz(ipointer,1)
            ny=nxyz(ipointer,2)
            nz=nxyz(ipointer,3)
         endif
         
         area_xy=dxyz(icell,1)*dxyz(icell,2)
         area_yz=dxyz(icell,2)*dxyz(icell,3)
         area_zx=dxyz(icell,3)*dxyz(icell,1)
c-----------------------------------------------------------------------
c s kelkar March 8-07 scale ggg to reflect mass balance from a_axy
c-----------------------------------------------------------------------

         area_xyz(1) = area_yz
         area_xyz(2) = area_zx
         area_xyz(3) = area_xy
         flux_out=0.
         flux_in=0.
         do j=1,3
            if(ggg(icell,j).ge.0.) then
               flux_out=flux_out+ggg(icell,j)*area_xyz(j)
            else
               flux_in=flux_in-ggg(icell,j)*area_xyz(j)
            endif
            if(ggg(icell,-j).ge.0.) then
               flux_out=flux_out+ggg(icell,-j)*area_xyz(j)
            else
               flux_in=flux_in-ggg(icell,-j)*area_xyz(j)
            endif
         enddo
         if (flux_in .ne. 0. .and. ps(icell) .ne. 0.)
     &        scale_in=abs(water_flux(icell)/
     &        (flux_in*ps(icell)*86400000.))         
         if (flux_out .ne. 0. .and. ps(icell) .ne. 0.)
     &        scale_out=abs(water_flux(icell)/
     &        (flux_out*ps(icell)*86400000.))
         do j=1,3
            if(ggg(icell,j).ge.0.) then
               ggg(icell,j)=ggg(icell,j)*scale_out
            else
               ggg(icell,j)=ggg(icell,j)*scale_in
            endif
            if(ggg(icell,-j).ge.0.) then
               ggg(icell,-j)=ggg(icell,-j)*scale_out
            else
               ggg(icell,-j)=ggg(icell,-j)*scale_in
            endif
         enddo
c-----------------------------------------------------------------------

         area_xy_sg=area_xy/(nx*ny)
         area_yz_sg=area_yz/(ny*nz)
         area_zx_sg=area_zx/(nz*nx)
         do ix=1,nx
            do iy=1,ny
               do iz=1,nz
                  i_sg_global=sg_ijk_to_global_cell(ix,iy,iz,icell)
                  call boundary_planes(icell,ix,iy,iz,nx,ny,nz)
c     the sign convention for flx() is -ve into & +ve out of the cell
c     flx(1) is the left hand face:+ve velocity on it is pointing into
c     the cell & sould lead to a -ve value of flx(1)
c     hence -ve sign in fron of flx(1),flx(2) and flx(3)
                  flx(1)=-((ggg(icell,1)+ggg(icell,-1))*boundary_cord(1)
     1                 -ggg(icell,-1))*area_yz_sg
                  flx(4)=((ggg(icell,1)+ggg(icell,-1))*boundary_cord(4)
     1                 -ggg(icell,-1))*area_yz_sg
                  flx(2)=-((ggg(icell,2)+ggg(icell,-2))*boundary_cord(2)
     1                 -ggg(icell,-2))*area_zx_sg
                  flx(5)=((ggg(icell,2)+ggg(icell,-2))*boundary_cord(5)
     1                 -ggg(icell,-2))*area_zx_sg
                  flx(3)=-((ggg(icell,3)+ggg(icell,-3))*boundary_cord(3)
     1                 -ggg(icell,-3))*area_xy_sg
                  flx(6)=((ggg(icell,3)+ggg(icell,-3))*boundary_cord(6)
     1                 -ggg(icell,-3))*area_xy_sg
                  
                  sumflx=0.
                  do k=1,6
                     if(flx(k).gt.0.) sumflx=sumflx+flx(k)
                  enddo
c     convert from fluid flux to darcy flux and m^3/s to l/day
c note: ps_trac has the porosity values read in from the vel_fin_sptr 
c  file, these can be dirrerent from those read into the ps array from 
c  the rock macro 
c note: variable_update has not yet been called, so ps_trac
c is still using
c      original grid nodes, but i_sg_global refers to the subgrid
                  sumflx=ps(id_parent_sg(i_sg_global))*
     1                 sumflx*86400000.
                  water_flux_sg(i_sg_global)=sumflx
                  
               enddo
            enddo
         enddo
         
      enddo
      
      return

      end subroutine water_flux_interpolate_sg

c.......................................................................


