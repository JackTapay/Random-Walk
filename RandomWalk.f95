program RandomWalk
implicit none
integer :: position(2,100),n,y,g,z ! The 2 used in the declaration can be changed in order to create n-number of walkers, you can also limit the number of time units by altering the right-most index
real::x
do n=1,100 !You can limit the number of time units by altering the right-most index
	position(1,n)=n
end do
call random_seed
do z=2,2 !Despite the fact that this do loop is redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
	y=1
	do n=1,100 !You can limit the number of time units by altering the right-most index
        	call random_number(x)
         	if (x .le. 1.00/3.00) then
            		position(z,n)=y+1
             		y=position(z,n)
         	else if ((x .gt. 1.00/3.00) .and. (x .lt. 2.00/3.00)) then
                	position(z,n)=y-1
			y=position(z,n)
         	else if ((x .gt. 2.00/3.00) .and. (x .lt. 1.00)) then   
                	position(z,n)=y+0
			y=position(z,n)
	     	end if
	end do
end do
open(unit=10,file='RandomWalk.txt') !In order for the MATLAB script to work, the file name must be consistent in both .f95 and .mat files
do g=1,100 !You can limit the number of time units by altering the right-most index
	write(10,*) position(:,g)
end do
end program RandomWalk
