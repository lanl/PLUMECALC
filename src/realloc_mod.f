
      MODULE realloc_mod
      CONTAINS

      FUNCTION reallocate(p, n) ! reallocate REAL*8
      REAL*8, POINTER, DIMENSION(:) :: p, reallocate
      INTEGER, intent(in) :: n
      INTEGER :: nold, ierr
      ALLOCATE(reallocate(1:n), STAT=ierr)
      IF(ierr /= 0) STOP "allocate error"
      IF(.NOT. ASSOCIATED(p)) RETURN
      nold = MIN(SIZE(p), n)
      reallocate(1:nold) = p(1:nold)
      DEALLOCATE(p) 
      END FUNCTION REALLOCATE

      FUNCTION iallocate(p, n) ! iallocate integer
      INTEGER, POINTER, DIMENSION(:) :: p, iallocate
      INTEGER, intent(in) :: n
      INTEGER :: nold, ierr
      ALLOCATE(iallocate(1:n), STAT=ierr)
      IF(ierr /= 0) STOP "allocate error"
      IF(.NOT. ASSOCIATED(p)) RETURN
      nold = MIN(SIZE(p), n)
      iallocate(1:nold) = p(1:nold)
      DEALLOCATE(p) 
      END FUNCTION iallocate

      END MODULE realloc_mod
c...........................................................
