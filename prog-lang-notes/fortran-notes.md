# Fortran Notes

## Function(Routine)
### Keyword argument, optional argument and their default values.
- Put routine in a `module` to use keyword argument.
  ```fortran
  module use_keyarg
    implicit none
  
  
  contains
    subroutine sub(val)
      implicit none
      integer :: val
  
      write(*,*) val
    end subroutine sub
  end module use_keyarg
  
  
  program keyarg
    use use_keyarg
    implicit none
    
    call sub(2)
    call sub(val=3)
  end program keyarg
  ```

- Put  the routine in a `module` and add `optional` attribute to use optional argument.

  ```fortran
  module use_optional_arg_mod
    implicit none
      
  
  contains
    subroutine sub(req_arg, opt_arg)
      implicit none
      integer :: req_arg
      integer, optional :: opt_arg
  
      write(*,*) req_arg
      if (present(opt_arg)) then  ! Check whether the argument exist.
        write(*,*) opt_arg
      else
        write(*,*) "No input for opt_arg!"
      end if
    end subroutine sub
  end module use_optional_arg_mod
  
  
  program use_optional_arg
    use use_optional_arg_mod
    implicit none
      
    call sub(1)
    call sub(req_arg=1, opt_arg=2)
  end program use_optional_arg
  ```

  

- Fortran does not support argument default value intrinsicly, you can implement it use the following trick. 

  ```fortran
  module default_argval_mod
    implicit none
  
  
  contains
    subroutine PrintVal(val)
      implicit none
      
      integer, optional :: val
      integer :: local_val
      integer :: default_val = -1
  
      if (present(val)) then
        local_val = val
      else
        local_val = default_val
      end if
      print *, local_val
    end subroutine PrintVal
  end module default_argval_mod
  
  
  program use_default_argval
    use default_argval_mod
    implicit none
    
    call PrintVal()
    call PrintVal(1)
    call PrintVal(val=2)
  end program use_default_argval
  ```



## I/O

### Internal file

_Read a string as a variable or write a variable to a string._



## Develop tools

### Documentation

#### ford

_[ford](https://github.com/Fortran-FOSS-Programmers/ford) can Automatically generates FORtran Documentation from comments within the code._

- Cite a structure or a function(subroutine) in the documentation comment.

  Use `[[type_name_or_routine_name]]`, for example

  ```fortran
  subroutine ConvertDogToCat(dog_, cat_)
    implicit none
    type(Dog), intent(in) :: dog_
    	!! A [[Dog]] instance.
    type(Cat), intent(out) :: cat_
    
    ...
  end subroutine ConvertDogToCat
  ```

- Turn on source code display in the final document website.

  Use `source: true` in the ford control markdown file of the project.

  _Note: Use `function foo(bar) result(baz)` style function to display function source code._

- 