Module functionalitymod
  use typemod
  use mtmod
  implicit none

contains


  !Give random coordinates to walkers
  subroutine setcoordinates(pop,lkm,n)
    implicit none
    type(population), intent(inout) :: pop
    integer, intent(in) :: lkm,n
    integer :: k,i,seed

    !Use the given generator module
    seed=getseed()
    call sgrnd(seed)

    !Allocate the population type's arrays
    allocate(pop%xy(2,lkm),pop%status(lkm))

    !Using the rand. num. generation module assign random coordinates to the population
    do k=1,2
      do i=1,size(pop%xy)/2
        pop%xy(k,i)=igrnd(0,n)
      end do
    end do

    !Set everyone's status 1 (healthy) at start
    pop%status=1
  end subroutine setcoordinates


  !Set given amount of people sick
  subroutine setsick(pop,amount)
    implicit none
    type(population),intent(inout):: pop
    integer, intent(in) :: amount
    integer :: k,i
    k=amount

    !If the given amount is larger than the populationsize then everyone is sick
    if (k>=size(pop%status)) then
      pop%status=2
    !Else set the first ones to sick, if given amount is negative no-one is sick
    else if (k>0) then
      do i=1,k
        pop%status(i)=2
      end do
    end if
  end subroutine setsick


!Set given amount of people immune
  subroutine setimmune(pop,immune,sick)
    implicit none
    type(population), intent(inout) ::pop
    integer, intent(in) :: immune,sick
    integer :: k
    !If given amount is larger than the healthy part of population then everyone healthy becomes immune
    if (count(pop%status==1)<=immune) then
      where(pop%status==1)
        pop%status=3
      end where
    !Else set people immune from the first healthy person in array
    else if (sick>0 .and. immune>=0) then
      do k=sick+1,sick+immune
        pop%status(k)=3
      end do
    !If no-one is sick, then set the first ones immune, if given amount is negative then no-one is set immune
    else
      do k=1,immune
        pop%status(k)=3
      end do
    end if

  end subroutine setimmune




  !Move the walkers
  subroutine move(pop,n)
    implicit none
    integer :: i,j,seed
    integer, intent(in) :: n
    type(population),intent(inout) :: pop
    real :: prob

    !Use the generator module
    seed=getseed()
    call sgrnd(seed)

    !Loop over all of the population to move every walker independently
    do i=1,size(pop%xy)/2
      prob = grnd()
        !Using random number generator module get a value from uniform distribution and use that to determine which direction the walker goes one step
      if (prob<0.25) then
        pop%xy(1,i)=pop%xy(1,i)+1
      else if (prob>0.25 .and. prob<0.5) then
        pop%xy(1,i)=pop%xy(1,i)-1
      else if (prob>0.5 .and. prob<0.75) then
        pop%xy(2,i)=pop%xy(2,i)+1
      else
        pop%xy(2,i)=pop%xy(2,i)-1
      end if
    end do
    !Periodic boundaries:
    !x-axis is first row from pop%xy, y-axis second row
    !gridsize is n*n
    !If x->n+1 then x->0
    where (pop%xy(1,:)>n)
      pop%xy(1,:)=0
    end where
    !If y->n+1 then y->0
    where (pop%xy(2,:)>n)
      pop%xy(2,:)=0
    end where
    !If x->-1 then x=n
    where (pop%xy(1,:)<0)
      pop%xy(1,:)=n
    end where
    !If y->-1 then y=n
    where (pop%xy(2,:)<0)
      pop%xy(2,:)=n
    end where
  end subroutine move



    !Get the index from all the sick people in an arrayform
  function sickpeople(pop) result(sickindex)
    implicit none
    integer, allocatable, dimension(:) :: sickindex
    type (population), intent(in) :: pop
    integer :: i,k

    k=1
    !Allocate the size for the array for sick people
    allocate(sickindex(count(pop%status==2)))

    !Using loop go through every person's status. If status is sick (2) then set that index (column) to the sick people array
    do i=1,size(pop%status)
      if (pop%status(i)==2) then
        sickindex(k)=i
        k=k+1
      end if
    end do
  end function sickpeople



  !Set people sick with certain probability if at same position as other sick walker
  subroutine getsick(pop,pc,sickindex)
    type (population), intent(inout) :: pop
    real, intent(in) :: pc
    integer,allocatable, dimension(:) :: sickindex
    integer :: i,k,seed
    real :: prob

    !Go through every sick person using the sickpeople function
    do k=1,size(sickindex)
      !If walker has same coordinates than sick person and status is healthy then set the status to 4 (pending)
      where(pop%xy(1,:)==pop%xy(1,sickindex(k)) .and. pop%xy(2,:)==pop%xy(2,sickindex(k)) .and. pop%status==1)
        pop%status=4
      end where
    end do

    seed=getseed()
    call sgrnd(seed)


    !Go through every person and look for status 4
    do i=1,size(pop%status)
      !If status 4 then using random number and given probability pc to determine if person is sick or healthy
      if (pop%status(i)==4) then
        prob = grnd()
        !If random number less than probability then sick
        if (prob<=pc) then
          pop%status(i)=2
        !Else healthy
        else
          pop%status(i)=1
        end if
      end if
    end do

  end subroutine getsick




  !Set walkers immune with probability ph if sick
  subroutine getimmune(pop,ph,sickindex)
    implicit none
    type (population),intent(inout) :: pop
    real, intent(in) :: ph
    integer, allocatable, dimension(:) :: sickindex
    integer :: i,seed
    real :: prob

    !Use the generator module
    seed=getseed()
    call sgrnd(seed)

    !Go through every sick person using function sickpeople and if random number prob between 0-1 is less than ph then status set to immune
    do i=1,size(sickindex)
      prob= grnd()
      if (prob<=ph) then
        pop%status(sickindex(i))=3
      end if
    end do

  end subroutine getimmune

end module functionalitymod
