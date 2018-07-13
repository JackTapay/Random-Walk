SUBROUTINE Twist(n,m,y,z,position)
	implicit none
    INTEGER,INTENT(INOUT)::n,m,y,z,position(2,1000)! The 2 used in the declaration can be changed in order to create n-number of walkers
    if(m .gt. 6) then
        m=1
   		y=m
        position(z,n)=m
    else if (m .lt. 1) then
       	m=6
       	y=m
   		position(z,n)=m
  	else
       	m=m
       	y=m
       	position(z,n)=m
   	end if     
    RETURN
END SUBROUTINE
    
program RandomWalkTwist
implicit none
integer :: position(2,1000),n,y,g,m,z ! The 2 used in the declaration can be changed in order to create n-number of walkers
real::x
do n=1,1000
	position(1,n)=n
end do
call random_seed
do z=2,2 !Despite the fact that this do loop is redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
	y=1
	m=0
	do n=1,1000
    	call random_number(x)
       	if (x .le. 1.00/3.00) then
           	m=y+1  
			call Twist(n,m,y,z,position)
       	else if ((x .gt. 1.00/3.00) .and. (x .le. 2.00/3.00)) then
       		m=y-1
			call Twist(n,m,y,z,position)
      	else           	
           	m=y+0
         	call Twist(n,m,y,z,position)
	   	end if
	end do
end do
open(unit=10,file="RandomWalk.txt")
do g=1,1000
        write(10,*) position(:,g)
end do
end program RandomWalkTwist
