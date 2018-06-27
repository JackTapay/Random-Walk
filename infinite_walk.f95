program TEST
implicit none
integer :: position(2,1000),n,y,g,z ! The 2 used in the declaration can be changed in order to create n-number of walkers
real::x,onethird,twothird
onethird=1.00/3.00
twothird=2.00/3.00
do n=1,1000
	position(1,n)=n
end do
do z=2,2 !Despite the fact that this do loop is redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
	y=1
	do n=1,1000
        	call random_number(x)
         	if (x .le. onethird) then
            		position(z,n)=y+1
             		y=position(z,n)
         	else if ((x .gt. onethird) .and. (x .le. twothird)) then
                	position(z,n)=y-1
			y=position(z,n)
         	else    
                	position(z,n)=y+0
			y=position(z,n)
	     	end if
	end do
end do
do g=1,1000
	print*, position(1,g),position(2,g)!,position(3,g),position(4,g)...position(a,g) [where a is rightmost column #]
end do
end program TEST
