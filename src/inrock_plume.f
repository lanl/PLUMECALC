      subroutine inrock_plume
!***********************************************************************
! $Id: inrock_plume.f,v 1.2 2006/06/06 20:11:37 zvd Exp $
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
!   PURPOSE
!  
!   Read rock property data.
!
!   Developed originally for FEHM.
!    
!***********************************************************************
CD3
CD3 INTERFACES
CD3
CD3 Formal Calling Parameters
CD3
CD3   None
CD3
CD3 Interface Tables
CD3
CD3   None
CD3
CD3 Files
CD3
CD3   None
CD3   
C***********************************************************************
CD4
CD4 GLOBAL OBJECTS
CD4
CD4 Global Constants
CD4
CD4   None
CD4
CD4 Global Types
CD4
CD4   None
CD4
CD4 Global Variables
CD4
CD4                            COMMON
CD4   Identifier      Type     Block  Description
CD4
CD4   inpt            INT      faai   Unit number for input file
CD4   ipps            POINTER  fdd    Pointer to array variable ps
CD4   ipcpr           POINTER  fdd    Pointer to array variable cpr
CD4   ipdenr          POINTER  fdd    Pointer to array variable denr
CD4   ischk           INT      faai   Unit number for input data check file
CD4   macroread(17)   LOGICAL  macro  Flag denoting if macro rock has been read 
CD4
CD4 Global Subprograms
CD4
CD4   Identifier      Type     Description
CD4
CD4   initdata                 Read data values and set parameter values at
CD4                              given nodes
CD4 
C***********************************************************************
CD5
CD5 LOCAL IDENTIFIERS
CD5
CD5 Local Constants
CD5
CD5   None
CD5
CD5 Local Types
CD5
CD5   None
CD5
CD5 Local variables
CD5
CD5   Identifier      Type     Description
CD5
CD5   igroup          INT      Current group number in this macro
CD5   ireturn         INT      Returned error flag from input subroutine
CD5   itype           INT      Array of variable types being input
CD5   default         REAL*8   Array of default values for input arrays
CD5   macro           CHAR     Current macro being read
CD5   narrays         INT      Number of arrays being read in
CD5   pointer         INT      Integer array of pointer values for
CD5                              variables being read
CD5
CD5 Local Subprograms
CD5
CD5   None
CD5
CPS
CPS PSEUDOCODE
CPS
CPS BEGIN inrock
CPS 
CPS   set arrays used, pointers, data type and default values
CPS   call initdata to read data values and set rock property values at
CPS    given nodes
CPS   
CPS   set macroread to true
CPS
CPS END inrock
CPS
C***********************************************************************

      use comunits, only : rock_unit_number, error_unit_number
      use comki
      use comgrid, only : n_grid_points
      use comrock
      implicit none

      logical macroread
      logical found
      integer i, imsg(7), msg(7), nwds
      character*4 dummy4
      character*32  cmsg(7)
      real*8, allocatable :: kdp(:)
      real*8, allocatable :: rpor(:)
      real*8 xmsg(7)
      character*80 wdd1
      
      macro = 'rock'
      allocate(denr(n_grid_points))
      allocate(kdp(n_grid_points))
      allocate(ps(n_grid_points))
      allocate(rfac(n_grid_points))
      allocate(vcf(n_grid_points))
      allocate(rpor(n_grid_points))
c**** read rock data ****
      itype(1) = 8
      itype(2) = 8
      itype(3) = 8
      itype(4) = 8
      default(1) = 2500.
      default(2) = 0.0
      default(3) = 1.0
      default(4) = 1.0
      igroup = 1

      found=.false.
      do while(.not.found)
         read(rock_unit_number,'(a4)') dummy4
         if(dummy4.eq.'rock') exit
      end do

      read(rock_unit_number,'(a80)') wdd1
      call parse_string(wdd1, imsg, msg, xmsg, cmsg, nwds)
      backspace rock_unit_number
      if (nwds .eq. 7) then
         narrays = 4
         call initdata2 (rock_unit_number, error_unit_number,
     2        n_grid_points, narrays, itype, 
     3        default, macroread, macro, igroup, ireturn,
     4        r8_1=denr(1:n_grid_points),r8_2=kdp(1:n_grid_points),
     5        r8_3=ps(1:n_grid_points),r8_4=rpor(1:n_grid_points)) 
      else
         narrays = 3
         call initdata2 (rock_unit_number, error_unit_number,
     2        n_grid_points, narrays, itype, 
     3        default, macroread, macro, igroup, ireturn,
     4        r8_1=denr(1:n_grid_points),r8_2=kdp(1:n_grid_points),
     5        r8_3=ps(1:n_grid_points)) 
         vcf = default(4)
      end if
! Velocity correction must be applied before the times are corrected
! for retardation
! For compatibility with previous versions, check to see if a scaling factor was entered instead of a reference porosity
      do i = 1,n_grid_points
!         rfac(i) = (1 + (denr(i) * kdp(i)) / (ps(i) * 1000.)) * vcf(i)
         rfac(i) = (1 + (denr(i) * kdp(i)) / (ps(i) * 1000.))
         if (nwds .eq. 7) then
            if (rpor(i) .gt. 0. .and. rpor(i) .lt. 1.) then
! A reference porosity was entered and the scaling factor will be computed
               vcf(i) = ps(i) / rpor(i)
            else
               vcf(i) = rpor(i)
            end if
         end if
      end do

      deallocate (kdp, rpor)

      end
