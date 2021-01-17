module typemod
  implicit none
  !Define type population to handle the coordinates and status
  type :: population
    integer,allocatable, dimension(:,:) :: xy
    !status defined by integrals (1=healthy, 2=sick, 3=immune)
    integer,allocatable,dimension(:) :: status
  end type population
end module typemod
