module mod_jacc
  use iso_c_binding

  INTERFACE
     SUBROUTINE jacc_kernel_push(code, arg) BIND(C, name='__jacc_kernel_push')
       use iso_c_binding
       CHARACTER(C_CHAR) :: code
       INTEGER(8) :: arg
     END SUBROUTINE jacc_kernel_push

     SUBROUTINE jacc_init() BIND(C, name='__jacc_init')
     END SUBROUTINE jacc_init

     SUBROUTINE jacc_close() BIND(C, name='__jacc_close')
     END SUBROUTINE jacc_close

     SUBROUTINE jacc_wait() BIND(C, name='__jacc_wait')
     END SUBROUTINE jacc_wait

     SUBROUTINE jacc_acc_init(devicetype) BIND(C, name='__jacc_acc_init')
       use iso_c_binding
       integer(C_INT), value :: devicetype
     END SUBROUTINE jacc_acc_init

     SUBROUTINE jacc_acc_shutdown(devicetype) BIND(C, name='__jacc_acc_shutdown')
       use iso_c_binding
       integer(C_INT), value :: devicetype
     END SUBROUTINE jacc_acc_shutdown

     SUBROUTINE jacc_arg_build( &
          type, symbol, addr, size, attr, dimnum, splitdim, lbound, ubound, next &
          ) BIND(C, name='__jacc_arg_build')
       use iso_c_binding
       character(C_CHAR) :: type, symbol
       TYPE(C_PTR), value :: addr
       INTEGER(C_SIZE_T), value :: size, attr
       INTEGER, value :: dimnum, splitdim
       INTEGER :: lbound(:), ubound(:)
       INTEGER(8) :: next
     END SUBROUTINE jacc_arg_build

     SUBROUTINE jacc_copyin(a, len) BIND(C, name='__jacc_copyin')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_copyin

     SUBROUTINE jacc_create(a, len) BIND(C, name='__jacc_create')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_create

     SUBROUTINE jacc_copyout(a, len) BIND(C, name='__jacc_copyout')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_copyout

     SUBROUTINE jacc_delete(a, len) BIND(C, name='__jacc_delete')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_delete

     SUBROUTINE jacc_update_self(a, len) BIND(C, name='__jacc_update_self')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_update_self

     SUBROUTINE jacc_update_device(a, len) BIND(C, name='__jacc_update_device')
       use iso_c_binding
       TYPE(C_PTR), value :: a
       INTEGER(C_SIZE_T), value :: len
     END SUBROUTINE jacc_update_device

     SUBROUTINE jacc_optimize() BIND(C, name='__jacc_optimize')
       USE ISO_C_BINDING
     END SUBROUTINE jacc_optimize

     REAL(C_DOUBLE) FUNCTION JACC_TIME () BIND(C)
       USE ISO_C_BINDING
     END FUNCTION JACC_TIME

  end interface
end module mod_jacc
