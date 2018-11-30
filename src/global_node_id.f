      subroutine global_node_id(id_cell,i,j,k,i_sg)
c s kelkar March 28 06
c call routines to map to global info and output
c******************************************************************** 
c INPUT VARIABLES
c     
c OUTOUT VARIABLES
c     
c INTERNAL VARIABLES
c********************************************************************

      use comparttr
      use comparttr_sg

      implicit none

      integer i,j,k,i_sg,id_cell
      integer iwork(27,10,10,10)

      iwork(1,1,4,2)=28
      iwork(1,2,4,2)=29
      iwork(1,2,5,2)=30
      iwork(1,2,5,3)=31
      iwork(1,3,5,3)=32
      iwork(1,3,5,4)=33
      iwork(2,1,4,3)=34
      iwork(2,1,4,4)=35
      iwork(2,1,4,5)=36
      iwork(2,2,4,5)=37
      iwork(2,2,4,6)=38
      iwork(5,3,1,7)=39
      iwork(5,3,2,7)=40
      iwork(5,4,2,7)=41
      iwork(5,4,3,7)=42
      iwork(14,1,1,1)=43
      iwork(14,1,1,2)=44
      iwork(14,1,2,2)=45
      iwork(14,1,2,3)=46
      iwork(23,1,1,1)=47
      iwork(24,1,4,1)=48
      iwork(24,2,4,1)=49
      iwork(24,3,4,1)=50
      iwork(24,4,4,1)=51
      iwork(27,1,1,3)=52
      iwork(27,1,2,3)=53

      return

      end

c......................................................
