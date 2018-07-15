SUBROUTINE Twist(n,m,y,z,position)
	implicit none
	INTEGER,INTENT(INOUT)::n,m,y,z,position(6,1000)! The 2 used in the declaration can be changed in order to create n-number of walkers
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
SUBROUTINE AlterPosition(position,d,ifvalue1,ifvalue2,ifvalue3,z,n)
	IMPLICIT NONE
    INTEGER,INTENT(INOUT)::position(6,1000),ifvalue1,ifvalue2,ifvalue3,z,n
    REAL,INTENT(INOUT)::d
    if (d .le. 1.00/3.00) then
    	position(z,n)=position(z,n)+ifvalue1
    else if ((d .gt. 1.00/3.00) .and. (d .le. 2.00/3.00)) then
        position(z,n)=position(z,n)+ifvalue2
    else 
        position(z,n)=position(z,n)+ifvalue3
    end if
    RETURN
END SUBROUTINE               
program RandomWalkTwist
implicit none
integer :: position(6,1000),n,y,g,m,z,counter,e ! The 2 used in the declaration can be changed in order to create n-number of walkers
real::x,d
real,dimension(1000)::randomnum
do n=1,1000
	position(1,n)=n
end do
call random_seed
do z=2,6 !Despite the fact that this do loopis redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
	y=1
	m=0
	do n=1,1000 !You can change the time units of the Random Walk by changing the right-most value
    	call random_number(x)
        randomnum(n)=x
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
do n=1,1000
	call random_number(d)
	do z=2,6 !You can change number of particles by changing the column index 
		do e=1,6
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

do g=1,1000 !You can change the time units of the Random Walk by changing the right-most value
	write(10,*) position(:,g)
end do
end program RandomWalkTwist
