module filehandling
  use typemod
  implicit none

contains
  !Read input file
  subroutine readfiles(gridsize,popsize,sick,immune,pc,ph,step)
    implicit none
    integer :: ios,i
    character(len=80) :: data(7),help
    integer, intent(inout) :: gridsize,popsize,sick,immune
    real, intent(inout) ::pc,ph
    integer, intent(inout) :: step

    !Give error messages if failed to open
    open(unit=5,status='old',file="../run/input.dat",iostat=ios)
    if (ios/=0) then
      print '(a,a)','*** Error in opening file input.dat'
      stop
    end if
    !Read the input to array
    do i=1,7
      read(5,'(A50)',iostat=ios) data(i)
    end do

    !Set every value in data array to assigned variable by first deleting the unnecessary strings. Give error message if something goes wrong
    help=trim(data(1)(25:40))
    read(help,*,iostat=ios)gridsize
    if (ios<0) then
      print*, '*** Error in reading gridsize'
      stop
    end if
    if (gridsize<0) then
      print*, 'Gridsize has to be a positive integer'
      stop
    end if
    help=trim(data(2)(17:40))
    read(help,*,iostat=ios)popsize
    if (ios<0) then
      print*, '*** Error in reading populationsize'
      stop
    end if
    help=trim(data(3)(25:40))
    read(help,*,iostat=ios)pc
    if (ios<0) then
      print*, '*** Error in reading prob. to get sick'
      stop
    end if
    help=trim(data(4)(27:40))
    read(help,*,iostat=ios)ph
    if (ios<0) then
      print*, '*** Error in reading prob. to get immune'
      stop
    end if
    help=trim(data(5)(22:40))
    read(help,*,iostat=ios)sick
    if (ios<0) then
      print*, '*** Error in reading amount of sick'
      stop
    end if
    help=trim(data(6)(24:40))
    read(help,*,iostat=ios)immune
    if (ios<0) then
      print*, '*** Error in reading amount of immune'
      stop
    end if
    help=trim(data(7)(11:40))
    read(help,*,iostat=ios)step
    if (ios<0) then
      print*, '*** Error in reading timesteps'
      stop
    end if
  end subroutine readfiles


  !Write to the output file in xyz format
  subroutine writefilexyz(pop,step)
    implicit none
    type(population), intent(in) :: pop
    integer :: ios,k
    integer, intent(in) :: step

    !Open the file and give error message if error
    open(unit=1,file='../run/output.xyz',iostat=ios,status='old')

    if (ios/=0) then
      print '(a,a)','*** Error in opening file xyz.xyz'
      stop
    end if

    !Write the size of the population and the step
    write(1,'(I6.1)') size(pop%status)
    write(1,'(A6,I3)') '#Step ', step
    !Write the coordinates and status of every individual walker on a new line
    do k=1,size(pop%status)
      write(1,'(I3,A1,I3,A1,I1)') pop%xy(1,k),' ',pop%xy(2,k), ' ', pop%status(k)
    end do

  end subroutine writefilexyz


  !Write the statusdata to CSV kind of a type
  subroutine writefiledat(pop)
    implicit none
    type(population), intent(in) :: pop
    integer :: ios
    !Open the file and error messsage if needed
    open(unit=2,file='../run/output.dat',iostat=ios,status='old')
      if (ios/=0) then
        print '(a,a)','*** Error in opening file output.dat'
        stop
      end if
      !Write the status of whole population separated by commas and each timestep separated by new line
    write(2,'(I6,A1,I6,A1,I6,A1)') count(pop%status==1), ',', count(pop%status==2),',' , count(pop%status==3), ','
  end subroutine writefiledat
end module filehandling
