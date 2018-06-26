SUBROUTINE RandomNumber(a,n,x)
	implicit none
    INTEGER,INTENT(INOUT)::n
    REAL,INTENT(INOUT)::x
    REAL,INTENT(INOUT),dimension(1000):: a
    do n=1,1000
      call random_number(x)
      a(n)=x
    end do
    RETURN
END SUBROUTINE
    
SUBROUTINE PositionCalc(a,n,y,g)
	INTEGER,INTENT(INOUT) :: n,y
    INTEGER,INTENT(INOUT),DIMENSION(1000)::g
    REAL:: third,twothird
    REAL,INTENT(INOUT), dimension(1000):: a
    third=1.00/3.00
    twothird=2.00/3.00
    y=0
    do n=1,1000
    	if (a(n) .le. third) then
        	g(n)=y+1
        	y=g(n)
    	else if ((a(n) .gt. third) .and. (a(n) .le. twothird)) then
        	g(n)=y-1
        	y=g(n)
    	else
        	g(n)=y+0
        	y=g(n)
    	end if
	end do
    RETURN
END SUBROUTINE

program RandomWalk
implicit none
integer::unit,n,y
real::x
integer,dimension(1000)::position1,position2,position3,position4,position5,position6,position7,position8
real,dimension(1000)::random1,random2,random3,random4,random5,random6,random7,random8
print*, "How many units?"
read(*,*) unit
if ((unit>10) .or. (unit<0)) then
  print*, "You chose ", unit," units. That number of units exceeds the capabilities of the program."
else
  print*, "You chose ", unit," units."
end if
open (unit=10,file="RandomWalk.txt")
select case (unit)
  case(1)
  	CALL RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    do n=1,1000
      write(10,*) n,position1(n)
    end do
  case(2)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    do n=1,1000
      write(10,*) n,position1(n),position2(n)
    end do
  case(3)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n)
    end do
  case(4)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    call RandomNumber(random4,n,x)
    call PositionCalc(random4,n,y,position4)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n),position4(n)
    end do
  case(5)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    call RandomNumber(random4,n,x)
    call PositionCalc(random4,n,y,position4)
    call RandomNumber(random5,n,x)
    call PositionCalc(random5,n,y,position5)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n),position4(n),position5(n)
    end do
  case(6)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    call RandomNumber(random4,n,x)
    call PositionCalc(random4,n,y,position4)
    call RandomNumber(random5,n,x)
    call PositionCalc(random5,n,y,position5)
    call RandomNumber(random6,n,x)
    call PositionCalc(random6,n,y,position6)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n),position4(n),position5(n),position6(n)
    end do
  case(7)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    call RandomNumber(random4,n,x)
    call PositionCalc(random4,n,y,position4)
    call RandomNumber(random5,n,x)
    call PositionCalc(random5,n,y,position5)
    call RandomNumber(random6,n,x)
    call PositionCalc(random6,n,y,position6)
    call RandomNumber(random7,n,x)
    call PositionCalc(random7,n,y,position7)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n),position4(n),position5(n),position6(n),position7(n)
    end do
  case(8)
    call RandomNumber(random1,n,x)
    call PositionCalc(random1,n,y,position1)
    call RandomNumber(random2,n,x)
    call PositionCalc(random2,n,y,position2)
    call RandomNumber(random3,n,x)
    call PositionCalc(random3,n,y,position3)
    call RandomNumber(random4,n,x)
    call PositionCalc(random4,n,y,position4)
    call RandomNumber(random5,n,x)
    call PositionCalc(random5,n,y,position5)
    call RandomNumber(random6,n,x)
    call PositionCalc(random6,n,y,position6)
    call RandomNumber(random7,n,x)
    call PositionCalc(random7,n,y,position7)
    call RandomNumber(random8,n,x)
    call PositionCalc(random8,n,y,position8)
    do n=1,1000
      write(10,*) n,position1(n),position2(n),position3(n),position4(n),position5(n),position6(n),position7(n),position8(n)
    end do
end select 
end program RandomWalk
