      subroutine  near3 (xg, yg, zg, node)
!***********************************************************************
! $Id: near3.f,v 1.1 2006/05/17 15:23:22 zvd Exp $
!***********************************************************************
!  Copyright, 1993, 2005,  The  Regents of the University of California.
!  This program was prepared by the Regents of the University of 
!  California at Los Alamos National Laboratory (the University) under  
!  contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). 
!  All rights in the program are reserved by the DOE and the University.
!  Permission is granted to the public to copy and use this software 
!  without charge, provided that this Notice and any statement of 
!  authorship are reproduced on all copies. Neither the U.S. Government 
!  nor the University makes any warranty, express or implied, or 
!  assumes any liability or responsibility for the use of this software.
!***********************************************************************
!
! PURPOSE
!
! Determine the nearest node to a given set of x, y, z coordinates.
! 
!***********************************************************************
!
! REVISION HISTORY 
!
! This routine was origianally implemented in FEHM by G. Zyvoloski.
! Modified 16-Sep-05 for use with PLUMECALC V2.1 by Z. Dash
!
!***********************************************************************
!     DEFINITION of variables
!
!     Identifier      Type     Description
!
!     xg              real*8   x coordinate position
!     yg              real*8   y coordinate position
!     zg              real*8   z coordinate position
!     node            integer  number of node nearest to coordinate
!                                position
!     n_grid_points   integer  number of points (cells) in the grid
!     x, y, z         real*8   coordinates of each grid cell (center of 
!                                cell), arrays of size n_grid_points   
!     dismin          real*8   minimum distance between given 
!                                coordinates and a node
!     dist            real*8   distance between given coordinates and 
!                                node
!     i               integer  loop index
!
!***********************************************************************
!     PSEUDOCODE
!    
!     BEGIN  near3
!     
!     initialize minimum distance between nodes to a large value
!       
!     FOR each node
!         compute the distance between the node and the given x,y 
!            z coordinates
!              
!         IF the distance between the current node and the coordinates 
!            is less than the minimum distance
!            set minimum distance to current distance
!            set node to the current node
!         ENDIF
!              
!     ENDFOR    
!       
!     END  near3
!    
!***********************************************************************

      use comgrid
      implicit none

      real*8 dismin, dist, xg, yg, zg
      integer i, node

      dismin = 1.0d+10

      do i = 1, n_grid_points
         dist = sqrt((x(i) - xg)**2 + (y(i) - yg)**2 + (z(i) - zg)**2)
         if (dist .lt. dismin)  then
            dismin = dist
            node   =  i
         end if
      end do

      end
