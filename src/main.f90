program main
    use operationmod
    use filehandling
    implicit none
    integer :: gridsize,popsize,iarg,sick,immune
    type (population) :: pop
    character (len=80) :: arg, data(7),help
    real :: pc,ph
    integer :: i,k,step,seed

    !Read the input to variables
    call readfiles(gridsize,popsize,sick,immune,pc,ph,step)

    !Give the population values
    pop = setvalues(gridsize,popsize,sick,immune)
    
    !Use as many timestep as the user wants
    do i=1,step
    
      !Write the population status
      call writefiledat(pop)

      !Take a timestep
      call timestep(pop,gridsize,pc,ph)

      !Write the coordinates and status of every walker
      call writefilexyz(pop,i)
    end do

    !Call the plotting program
    call system("python3 plot.py")

end program main
