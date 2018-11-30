      module comki
!***********************************************************************
! $Id: comki.f,v 1.1 2006/05/17 15:23:19 zvd Exp $
!***********************************************************************
!  Copyright, 1993, 2004,  The  Regents of the University of California.
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
!     Module containing data initialization variables and interface.
!     Developed originally for FEHM.
!  
!     Identifier      Type     Description
!     
!     max_inputs      int      Maximum number of variables being read
!                                on an input line
!     default         REAL*8   Array of default values for input arrays
!     igroup          INT      Current group number in this macro
!     ireturn         INT      Returned error flag from input subroutine
!     itype           INT      Array of variable types being input
!     macro           CHAR     Current macro being read
!     narrays         INT      Number of arrays being read in
!  
!**********************************************************************

      integer narrays
      integer max_inputs
      parameter(max_inputs = 10)
      integer itype(max_inputs)
      real*8 default(max_inputs)
      character*4 macro
      integer igroup
      integer ireturn

      interface

         subroutine initdata2(in_number, out_number, npoints,
     2        narrays, itype, default, readflag, macro, igroup,ireturn,
     3        r8_1,r8_2,r8_3,r8_4,r8_5,i4_1,i4_2,i4_3,i4_4,i4_5)
         integer in_number,out_number,npoints,narrays,ireturn
         integer itype(*)
         integer igroup
         integer max_arrays
         parameter(max_arrays=10)
         real*8 default(max_arrays),values(max_arrays)
         logical readflag
         character*4 macro
         
         real*8, optional :: r8_1(:)
         real*8, optional :: r8_2(:)
         real*8, optional :: r8_3(:)
         real*8, optional :: r8_4(:)
         real*8, optional :: r8_5(:)
         integer, optional :: i4_1(:)
         integer, optional :: i4_2(:)
         integer, optional :: i4_3(:)
         integer, optional :: i4_4(:)
         integer, optional :: i4_5(:)
         end subroutine

      end interface

      end module comki
