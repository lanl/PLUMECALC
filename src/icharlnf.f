*dk,icharlnf
      integer function icharlnf(iword)
C
C
C#######################################################################
C
C      PURPOSE -
C
C      Find the Length of a Character String by Searching Forward
C         until the first Blank Character is found after the fist
C         non-Blank Character.
C
C      INPUT ARGUMENTS -
C
C        iword    - (character) A Character String.
C
C      OUTPUT ARGUMENTS -
C
C        icharlnf  - (integer) The length of the Character String.
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/icharlnf.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:51:28 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   02/16/95 09:52:34   ejl
CPVCS    Cleaned up and simplified. Implicit none.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:42:08   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) iword
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer lenmax, istop
C
C#######################################################################
C
C
C
      lenmax=len(iword)
C
      istop=0
C
C.... Find the first Non-Blank Character.
C
      do while ((istop .lt. lenmax) .and. 
     *          (iword(istop+1:istop+1) .eq. ' '))
C
         istop=istop+1
C
      enddo
C
C.... Now find the first Blank or Null Character.
C
      do while ((istop .lt. lenmax) .and. 
     *          (iword(istop+1:istop+1) .ne. char(0)) .and.
     *          (iword(istop+1:istop+1) .ne. ' '))
C
         istop=istop+1
C
      enddo
C
C.... Number of characters in iword.
C
      icharlnf=istop
C
      return
      end
