SUBROUTINE Twist(n,m,y,z,position)
	implicit none
	INTEGER,INTENT(INOUT)::n,m,y,z,position(6,100)!The 6 used in the declaration can be changed in order to create n-number of walkers and the number of time units can be altered by changing the right-most index	
	if(m .gt. 6) then !The 6 within this if statement can be altered to change the maximum value
        	m=1 !This instance of 'm' can be changed in order to alter the minimum value of the random walker
   		y=m
        	position(z,n)=m
	else if (m .lt. 1) then !The value that 'm' is compared to must be the same value of 'm' in line 5
       		m=6 !The value that 'm' is compared to must be the same value 'm' is compared to in line 4
       		y=m
   		position(z,n)=m
  	else
       		m=m
       		y=m
       		position(z,n)=m
   	end if     
   	RETURN
END SUBROUTINE
SUBROUTINE AlterPosition(position,d,ifvalue1,ifvalue2,ifvalue3,z,n)
	IMPLICIT NONE
    INTEGER,INTENT(INOUT)::position(6,100),ifvalue1,ifvalue2,ifvalue3,z,n !The 6 used in the declaration can be changed in order to create n-number of walkers and the number of time units can be altered by changing the right-most index
    REAL,INTENT(INOUT)::d
    if (d .lt. 1.00/3.00) then
    	position(z,n)=position(z,n)+ifvalue1
    else if ((d .gt. 1.00/3.00) .and. (d .lt. 2.00/3.00)) then
        position(z,n)=position(z,n)+ifvalue2
    else if ((x .gt. 1.00/3.00) .and. (x .lt. 1.00)) then
        position(z,n)=position(z,n)+ifvalue3
    end if
    RETURN
END SUBROUTINE               
program RandomWalkTwist
implicit none
integer :: position(6,100),n,y,g,m,z,counter,e !The 6 used in the declaration can be changed in order to create n-number of walkers and the number of time units can be altered by changing the right-most index
real::x,d
real,dimension(100)::randomnum !The number of time units can be altered by changing the value within the dimension
do n=1,100 !The number of time units can be altered by changing the right-most index
	position(1,n)=n
end do
call random_seed
do z=2,6 !Despite the fact that this do loop is redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
	y=1
	m=0
	do n=1,100 !You can change the time units of the Random Walk by changing the right-most value
    	call random_number(x)
        randomnum(n)=x
       	if (x .lt. 1.00/3.00) then
           	m=y+1  
		call Twist(n,m,y,z,position)
       	else if ((x .gt. 1.00/3.00) .and. (x .lt. 2.00/3.00)) then
       		m=y-1
			call Twist(n,m,y,z,position)
      	else if ((x .gt. 1.00/3.00) .and. (x .lt. 1.00)) then          	
           	m=y+0
         	call Twist(n,m,y,z,position)
	   	end if
	end do
end do
do n=1,100 !The number of time units can be altered by changing the right-most index
	call random_number(d)
	do z=2,6 !You can change number of particles by changing the column index 
		do e=1,6 !The number of site can be altered by changing the right-most index
      		counter=0
      		if (position(z,n) .eq. e) then
        		counter=counter+1
      		end if
      		if (counter .ge. 2) then !You can set the number of maximum particles occupying the same site by changing 2
        		if (randomnum(n) .le. 1.00/3.00) then
                	call AlterPosition(position,d,-1,2,-2,z,n)
        		else if ((randomnum(n) .gt. 1.00/3.00) .and. (randomnum(n) .le. 2.00/3.00)) then
        			call AlterPosition(position,d,1,-2,2,z,n)
          		else	
					if (d .le. 1.00/2.00) then
            			position(z,n)=position(z,n)+1
          			else
            			position(z,n)=position(z,n)-1
          			end if
        		end if
			end if
		end do
  	end do 
end do
open(unit=10,file="RandomWalk.txt")

do g=1,100 !You can change the time units of the Random Walk by changing the right-most value
	write(10,*) position(:,g)
end do
end program RandomWalkTwist
