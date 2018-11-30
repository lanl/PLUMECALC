      subroutine read_tcurve (tcunit, numpars, ierr)
!***********************************************************************
! $Id: read_tcurve.f,v 1.1 2006/05/17 15:23:25 zvd Exp $
!***********************************************************************
!  Copyright, 2002, 2004,  The  Regents of the University of California.
!  This  program  was  prepared by  the  Regents  of the  University  of
!  California  at Los Alamos National Laboratory (the  University) under
!  contract No. W-7405-ENG-36 with the U. S. Department of Energy (DOE).
!  All rights in the program are reserved by the DOE and the University.
!  Permission is  granted to the  public to copy  and  use this software
!  without  charge,  provided that this  Notice  and  any  statement  of
!  authorship are reproduced on all copies. Neither the U. S. Government
!  nor  the  University  makes any  warranty,  express  or  implied,  or
!  assumes any liability or responsibility for the use of this software.
!***********************************************************************
! 
! PURPOSE
! 
! Read type curves for parallel fracture dispersion interpolation 
! calculations.
! 
!***********************************************************************
! 
! REVISION HISTORY
! 
! Modified from:
! FEHM Version 2.20 [10086-STN-2.20-00]
! Initial implementation: 28-Oct-02, Programmer: Z. Dash
! 
!***********************************************************************
 
      use compfrac
      use comgrid, only : n_grid_points
      implicit none

      character*3 output_flag
      integer i, ierr, j, k, l, m, mm, n, dummy, numpars, nump_max
      integer ncurves, tcunit
      real*8 dum(3)
      real*8 xconvert
      character*100 tfilename
      character*4 dummy_string
      character*80 input_msg
      integer msg(10)
      integer nwds
      real*8 xmsg(10)
      integer imsg(10),ditnumlines,i2,index
      character*32 cmsg(10)
      integer neq, nspeci

      numparams = numpars
      neq = n_grid_points
      nspeci = 1

      read(tcunit,'(a4)') dummy_string
      if(dummy_string(1:4).eq.'free') then
         read(tcunit,*) (log_flag(i),i=1,numparams)
         read(tcunit,*) (normal_param(i),i=1,numparams)
         read(tcunit,*) curve_structure

c     Input used to be 1 parameter, now it's 2. This section of code
c     checks for the number of parameters input. If only 1,
c     set the second parameter weight_facto to the old hirdwired
c     value of 1.e-3 to make code behave as before. Otherwise,
c     set the value

         if(curve_structure.gt.1) then
            backspace tcunit
            read(tcunit,'(a)') input_msg
            call parse_string(input_msg, imsg, msg, xmsg, cmsg, nwds)
            curve_structure = imsg(1)
            if(nwds.gt.1) then

c     Second number could be input as an integer, so make sure the
c     code accounts for that

               if(msg(2).eq.1) then
                  weight_factor = imsg(2)
               else
                  weight_factor = xmsg(2)
               end if
            else
               weight_factor = 1.e-3
            end if
         end if

         allocate(itf_curve(nspeci,neq,curve_structure))
         allocate(wt_curve(nspeci,neq,curve_structure))
         itf_curve = 0
         wt_curve = 0.
         read(tcunit,*) ncurves
         allocate(param1(ncurves))
         allocate(param2(ncurves))
         param1 = 0.
         param2 = 0.
         nump1 = ncurves
         nump2 = 1
         if (numparams .gt. 2) then
            allocate (param3 (ncurves))
            param3 = 0.
            d4 = 4
         else
            d4 = 1
         end if
         nump3 = 1
         
      else
         backspace tcunit
         curve_structure = 0
         read (tcunit, *) nump1
         allocate (param1 (nump1))
         read (tcunit,*) (param1(i), i= 1, nump1) 
         read (tcunit, *) nump2
         allocate (param2 (nump2))
         read (tcunit, *) (param2(i), i= 1, nump2)
         if (numparams .gt. 2) then
            read (tcunit, *) nump3
            allocate (param3 (nump3))
            read (tcunit, *) (param3(i), i= 1, nump3)
            d4 = 4
         else
            nump3 = 1
            allocate (param3 (nump3))
            param3 = 0.
            d4 = 1
         end if
      end if
      read (tcunit, *) nump_max
      allocate (nump(nump1,nump2,nump3,d4))
      allocate (dtime(nump1,nump2,nump3,d4,nump_max), 
     .     conc(nump1,nump2,nump3,d4,nump_max))
      dtime = 0.
      conc = 0.
      do l = 1, d4
         do i = 1, nump1
            do j = 1, nump2
               do k = 1, nump3
                  read (tcunit, *) nump(i,j,k,l), (dum(n), 
     .                 n = 1, numparams)
                  if(curve_structure.gt.0) then
                     param1(i) = dum(1)
                     param2(i) = dum(2)
                     if(numparams.gt.2) then
                        param3(i) = max(1.d-10,dum(3))
                     end if
                  end if
                  do m = 1, nump(i,j,k,l)
                     read (tcunit, *) dtime(i,j,k,l,m), conc(i,j,k,l,m)
                  enddo
                  if(numparams.gt.2) then
c                     xconvert = (param1(i)+param2(i))/
                     xconvert = param2(i)*(1.-param3(i))/
     2                    (param1(i)+param2(i)*param3(i))
                     dtime(i,j,k,l,1:nump(i,j,k,l)) = 
     2                    (dtime(i,j,k,l,1:nump(i,j,k,l))-1.)/xconvert
                  end if
                  mm=1
                  do m=2,nump(i,j,k,l)	
                     if(conc(i,j,k,l,m)-conc(i,j,k,l,mm)
     2                    .lt.-1.e-20 )then
                        write(ierr,*)'Decreasing data found in type ',
     .                       'curve, stop'
                        write(ierr,*)'point:',m,' conc=',
     .                       conc(i,j,k,l,m)
                        stop
                     end if
                     if(conc(i,j,k,l,m).ne.conc(i,j,k,l,mm))then
                        mm=mm+1
                        if(mm.ne.m)then
                           conc(i,j,k,l,mm)=conc(i,j,k,l,m)
                           dtime(i,j,k,l,mm)=dtime(i,j,k,l,m)
                        end if
                     end if
                  end do
                  nump(i,j,k,l)=mm
               enddo
            enddo
         enddo
      enddo
      ipartout = 0
      read(tcunit,'(a3)',end=2000) output_flag
      if(output_flag(1:3).eq.'out') then
         ipartout = 1
         allocate(param_density(nump1,nump2,nump3))
         param_density = 0
      end if
 2000 continue
      close (tcunit)

      pfrac_read = .true.
      return
      end
