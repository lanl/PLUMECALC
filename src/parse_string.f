      subroutine parse_string(line,imsg,msg,xmsg,cmsg,nwds)
!***********************************************************************
! $Id: parse_string.f,v 1.1 2006/05/17 15:23:23 zvd Exp $
!***********************************************************************
!  Copyright, 2004,  The  Regents  of the  University of California.
!  This program was prepared by the Regents of the University of 
!  California at Los Alamos National Laboratory (the University) under  
!  contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). 
!  All rights in the program are reserved by the DOE and the University. 
!  Permission is granted to the public to copy and use this software 
!  without charge, provided that this Notice and any statement of 
!  authorship are reproduced on all copies. Neither the U.S. Government 
!  nor the University makes any warranty, express or implied, or 
!  assumes any liability or responsibility for the use of this software.
!**********************************************************************
!
! PURPOSE
!
! Parse character string into tokens.
!
!**********************************************************************
!
! Initial implementation: 
!     for FEHM Version 2.0
! 
!**********************************************************************

      implicit none

      integer max_entries, line_length
      parameter(max_entries=20)
      character(*) line
      integer imsg(max_entries)
      integer msg(max_entries)
      real*8 xmsg(max_entries)
      character*32 cmsg(max_entries)
      integer nwds
      logical finished

      integer ndex(max_entries,2),i,begin,entrynum,isinteger,isreal,i2

      line_length = len(line)
      entrynum=1
      begin=1
      do i=1,line_length
         if (((line(i:i).eq.' ').or.(line(i:i).eq.achar(9)))
     &        .and.(begin.eq.0)) then
            ndex(entrynum,2)=i-1
            begin=1
            entrynum=entrynum+1
         else if ((line(i:i).eq.' ').or.(line(i:i).eq.achar(9))) then
            continue
         else if (begin.eq.1) then
            ndex(entrynum,1)=i
            begin=0
         else
            continue
         endif
      enddo
      if (begin.eq.1) entrynum=entrynum-1
      nwds=entrynum

      do i=1,nwds
         isinteger=1
         isreal=1
         do i2=ndex(i,1),ndex(i,2)
            if ((line(i2:i2).ne.'+').and.(line(i2:i2).ne.'-').and.
     &           ((iachar(line(i2:i2)).gt.57).or.
     &           (iachar(line(i2:i2)).lt.48))) isinteger=0
            if ((line(i2:i2).ne.'+').and.(line(i2:i2).ne.'-').and.
     &           ((iachar(line(i2:i2)).gt.57).or.
     &           (iachar(line(i2:i2)).lt.48)).and.
     &           (line(i2:i2).ne.'.').and.
     &           (line(i2:i2).ne.'E').and.
     &           (line(i2:i2).ne.'e').and.
     &           (line(i2:i2).ne.'D').and.
     &           (line(i2:i2).ne.'d')) isreal=0
         enddo
         if ((isreal.eq.1).and.(isinteger.eq.0)) then
c check if just have e,E,d,or D
            finished=.false.
            if (ndex(i,1).eq.(ndex(i,2))) then
               if ((line(ndex(i,1):ndex(i,2)).eq.'e').or.
     &              (line(ndex(i,1):ndex(i,2)).eq.'E').or.
     &              (line(ndex(i,1):ndex(i,2)).eq.'d').or.
     &              (line(ndex(i,1):ndex(i,2)).eq.'D')) then
                  msg(i)=3
                  cmsg(i)=line(ndex(i,1):ndex(i,2))
                  finished=.true.
               endif
            endif
            if (finished.eqv..false.) then
               msg(i)=2
               read(line(ndex(i,1):ndex(i,2)),*) xmsg(i)
            endif
         else if (isinteger.eq.1) then
            msg(i)=1
            read(line(ndex(i,1):ndex(i,2)),*) imsg(i)
         else
            msg(i)=3
            cmsg(i)=line(ndex(i,1):ndex(i,2))
         endif
      enddo

      end
