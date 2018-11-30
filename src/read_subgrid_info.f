      subroutine read_subgrid_info( )
      
      use comunits
      use comparttr
      use comparttr_sg
      
      implicit none
      
      character*1024 in_msg
      integer  nwds
      integer  msg(10)
      real*8  xmsg(10)
      integer imsg(10)
      character*32 cmsg(10)
      character*12  file_format
      character*7  file_stat
      
      integer icharlnb
      integer open_file
      integer if_continue, i, isptr_corn

      integer int_cell_path
!
      integer local_debug
      data    local_debug / 0 /
!
!      msg = 1 for integer
!          = 2 for real
!          = 3 for character
!
!
!   Looking for input of the the following forms:
!
!   subgrid scale_factor
!   scale_xyz
!
!   or
!
!   subgrid scale_factor
!   scale_x scale_y scale_z
!
!   or
!
!   subgrid scale_factor
!   node# scale_x scale_y scale_z
!   node# scale_x scale_y scale_z
!   node# scale_x scale_y scale_z
!
!   or
!
!   subgrid refine_factor
!   n_xyz
!
!   or
!
!   subgrid refine_factor
!   n_x n_y n_z
!
!   or
!
!   subgrid refine_factor
!   node# n_x n_y n_z
!   node# n_x n_y n_z
!   node# n_x n_y n_z
!
!
!   Set in read_file_info
!      if_subgrid = 0
!
!***********************************************************************
!
!   Parse the next line, which is the first possible subgrid input line.
!   If it contains the keywork 'subgrid' as the first token, this is a
!   subgrid type calculation. If not, continue with normal non-subgrid
!   calculation.
!
      if(local_debug .ne. 0)then
         write(error_unit_number, *) 'Enter: read_subgrid_info'
      endif
      read(sim_unit_number,'(a)', err = 9999, end = 9998) in_msg
      call parse_string(in_msg, imsg, msg, xmsg, cmsg, nwds)
      if(nwds .eq. 0)then
!        Found a blank line.
!        This is not a subgrid type calculations.
!        Continue with normal non-subgrid calculations.
!        Check one more line just in case. Let the code recover
!        from a single blank line and then valid input.
!
         read(sim_unit_number,'(a)', err = 9999, end = 9998) in_msg
         call parse_string(in_msg, imsg, msg, xmsg, cmsg, nwds)
         write(error_unit_number, 90) in_msg(1:icharlnb(in_msg))
  90     format (
     1    'read_subgrid_info: WARNING ',/, a,/,
     1    'WARNING: Found a blank line instead of a keyword.',/,
     2    'WARNING: Attempt to continue after the blank line.')
         if(nwds .eq. 0)then
            if_subgrid = 0
            return
         endif
      endif
      if((cmsg(1) .eq. 'subgrid') .or. 
     1   (cmsg(1) .eq. 'SUBGRID'))then

         if(nwds .eq. 1)then      
            input_flag_sg = 1
         else
            if    ( cmsg(2) .eq. 'scale_factor' .or. 
     1              cmsg(2) .eq. 'SCALE_FACTOR')then
               input_flag_sg = 1
            elseif( cmsg(2) .eq. 'refine_factor' .or. 
     1              cmsg(2) .eq. 'REFINE_FACTOR')then
               input_flag_sg = 2
            endif
         endif
      else
!
!        This is not a subgrid type calculations.
!        Continue with normal non-subgrid calculations.
!
         if_subgrid = 0
         return
!
      endif
!
!***********************************************************************
!
!     Check that x_packed, y_packed, z_packed was read in
!     from the SPTR2 input files.
!
!     If it was not there, subgrid calculations cannot continue.
!      
      if ( .not. allocated(x_packed))then
!           ERROR
            write(error_unit_number, 103) in_msg(1:icharlnb(in_msg))
 103        format ('read_subgrid_info: Inconsistent input: ',/, a,/,
     1      'ERROR: Keyword SUBGRID found.',/,
     1      'ERROR: XYZ data was not found in SPTR2 input file.',/,
     1      'ERROR: Subgrid calculations require SPTR2 XYZ data.',/,
     1      'STOPPING execution')
            stop
!           ERROR
      endif
      if ( .not. allocated(y_packed))then
!           ERROR
            write(error_unit_number, 103) in_msg(1:icharlnb(in_msg))
            stop
!           ERROR
      endif
      if ( .not. allocated(z_packed))then
!           ERROR
            write(error_unit_number, 103) in_msg(1:icharlnb(in_msg))
            stop
!           ERROR
      endif
!
!***********************************************************************
!
!   If we arrive here that means this is a subgrid (_sg) calculations.
!
      if_subgrid = 1
!
!***********************************************************************
!
!   Open file with corn and dxyz information.
!
!     - sptr corn and dxyz output file (from SPTR macro FEHM output) 
!      read(sim_unit_number,'(a)', err = 9999, end = 9998) in_msg
!      call parse_string(in_msg, imsg, msg, xmsg, cmsg, nwds)
!     name may be preceeded by line with file format 
      read(sim_unit_number,'(a12)', err = 9999, end = 9998) file_format
      select case (file_format(1:3))
      case ('asc','for')
         sptr_corn_file_type = 1
      case ('bin')
         sptr_corn_file_type = 3
      case ('unf')
         sptr_corn_file_type = 2
      case default
         sptr_corn_file_type = 1
         backspace sim_unit_number
      end select
      file_stat = 'OLD'
      read(sim_unit_number,'(a)') in_msg
      sptr_corn_unit_number = 
     1            open_file(in_msg, sptr_corn_file_type, file_stat)
!
!***********************************************************************
!
!   Allocate arrays and set default values
!
      call initialize_sg()
!
!***********************************************************************
!
!   Parse the second, third, ... lines to obtain information about
!   how cells will be refined. This is either by setting a lenght
!   scale factor and letting the code figure out the refinement
!   (nxyz) or by directly setting refinement (nxyz).
!
      if(input_flag_sg .eq. 1)then
!
!     Parse scale_factor type input
!
      if_continue = 1
      do while (if_continue .eq. 1)
      read(sim_unit_number,'(a)', err = 9999, end = 9998) in_msg
      call parse_string(in_msg, imsg, msg, xmsg, cmsg, nwds)
      if(nwds .eq. 1)then
         if(msg(1) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(1)
            write(error_unit_number, 110)
            stop
!           ERROR
 100        format ('read_subgrid_info: Unrecognized input ',/, a)
 105        format ('Expected Real or Integer, Found Character', a)
 110        format ('STOPPING execution')
!
         elseif(msg(1) .eq. 1)then
            xmsg(1) = imsg(1)
         endif
!
         scale_xyz = xmsg(1)
         if_continue = 0
      elseif(nwds .eq. 2)then
         if(msg(1) .ne. 1)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(1)
            write(error_unit_number, 110)
            stop
!           ERROR
!
         endif
         if(msg(2) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(2)
            write(error_unit_number, 110)
            stop
!           ERROR
!
         elseif(msg(2) .eq. 1)then
            xmsg(2) = imsg(2)
         endif
         int_cell_path=cell_path(imsg(1))
         if(int_cell_path .ne. 0)then
            scale_xyz(int_cell_path,:) = xmsg(2)
         else
!
!        IGNORE
!        subgrid refine parameter given for a cell with no particle paths
!
         endif
      elseif(nwds .eq. 3)then
         do i = 1,3
            if(msg(i) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(i)
            write(error_unit_number, 110)
            stop
!           ERROR
!
            elseif(msg(i) .eq. 1)then
               xmsg(i) = imsg(i)
            endif
            scale_xyz(:,i) = xmsg(i)
         enddo
         if_continue = 0
      elseif(nwds .eq. 4)then
         int_cell_path=cell_path(imsg(1))
         if(int_cell_path .ne. 0)then
         do i = 1,3
            if(msg(i) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(i)
            write(error_unit_number, 110)
            stop
!           ERROR
!
            elseif(msg(i) .eq. 1)then
               xmsg(i) = imsg(i)
            endif
            scale_xyz(int_cell_path,i) = xmsg(i+1)
         enddo
         else
!
!        IGNORE
!        subgrid refine parameter given for a cell with no particle paths
!
         endif
      else
      endif
      
      enddo
      elseif(input_flag_sg .eq. 2)then
!
!     Parse refine_factor type input
!
      if_continue = 1
      do while (if_continue .eq. 1)
      read(sim_unit_number,'(a)', err = 9999, end = 9998) in_msg
      call parse_string(in_msg, imsg, msg, xmsg, cmsg, nwds)
      if(nwds .eq. 1)then
         if(msg(1) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(1)
            write(error_unit_number, 110)
            stop
!           ERROR
         elseif(msg(1) .eq. 2)then
            imsg(1) = xmsg(1)
         endif
!
         nxyz = imsg(1)
         if_continue = 0
      elseif(nwds .eq. 2)then
         if(msg(1) .ne. 1)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(1)
            write(error_unit_number, 110)
            stop
!           ERROR
!
         endif
         if(msg(2) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(2)
            write(error_unit_number, 110)
            stop
!           ERROR
!
         elseif(msg(2) .eq. 2)then
            imsg(2) = xmsg(2)
         endif
         int_cell_path=cell_path(imsg(1))
         if(int_cell_path .ne. 0)then
            nxyz(int_cell_path,:) = imsg(2)
         else
!
!        IGNORE
!        subgrid refine parameter given for a cell with no particle paths
!
         endif
      elseif(nwds .eq. 3)then
         do i = 1,3
            if(msg(i) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(i)
            write(error_unit_number, 110)
            stop
!           ERROR
!
            elseif(msg(i) .eq. 2)then
               imsg(i) = xmsg(i)
            endif
            nxyz(:,i) = imsg(i)
         enddo
         if_continue = 0
      elseif(nwds .eq. 4)then
         int_cell_path=cell_path(imsg(1))
         if( int_cell_path.ne. 0)then
         do i = 1,3
            if(msg(i) .eq. 3)then
!
!           ERROR
            write(error_unit_number, 100) in_msg(1:icharlnb(in_msg))
            write(error_unit_number, 105) cmsg(i)
            write(error_unit_number, 110)
            stop
!           ERROR
!
            elseif(msg(i) .eq. 2)then
               imsg(i) = xmsg(i)
            endif
            nxyz(int_cell_path,i) = imsg(i+1)
         enddo
         else
!
!        IGNORE
!        subgrid refine parameter given for a cell with no particle paths
!
         endif
      else
      endif
      enddo
      endif
 9998 continue
 9999 continue
      if(local_debug .ne. 0)then
         do i = 1, n_touched_cells
            write(error_unit_number,330)i,nxyz(i,1),nxyz(i,2),nxyz(i,3)
  330       format('nxyz      ', 4i7)
         enddo
         do i = 1, n_touched_cells
            write(error_unit_number,340)i,
     1                      scale_xyz(i,1),scale_xyz(i,2),scale_xyz(i,3)
  340       format('scale_xyz ', i8,4e14.7)
         enddo
      endif
      if(local_debug .ne. 0)then
         write(error_unit_number, *) 'Exit: read_subgrid_info'
      endif
      return
      end subroutine read_subgrid_info
