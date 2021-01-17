module operationmod
  use functionalitymod
  implicit none


contains
  !Function to generate the starting coordinates to every walker and also set sick and immune people
 function setvalues(n,lkm,amount1,amount2) result(pop)
    implicit none
    type(population) :: pop
    integer,intent(in) :: n,lkm,amount1,amount2
    integer :: i,k

    call setcoordinates(pop,lkm,n)
    call setsick(pop,amount1)
    call setimmune(pop,amount2,amount1)

  end function setvalues


  !Advance 'time' by moving the walkers and seeing if any walkers get sick or immune
  subroutine timestep(p,n,pc,ph)
    implicit none
    type(population), intent(inout) :: p  
    integer, intent(in) :: n
    real :: prob
    real, intent(in) :: pc,ph
    integer :: i,j
    integer,allocatable, dimension(:) :: sickindex
    !Use function sickpeople to get index of all the sick people
    sickindex = sickpeople(p)

    call getsick(p,pc,sickindex)
    call getimmune(p,ph,sickindex)
    call move(p,n)

  end subroutine timestep

end module operationmod
