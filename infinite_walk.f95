program test
implicit none 
integer, dimension(1000)::tau
integer, dimension(1000)::position,position1,position2,position3,position4,position5,position6
real,dimension(1000)::x,c,d,f,g,h,j
real::y,a,b
integer::n,e,q,w,r,t,u,i,m
    
a=1.0/3.0
b=2.0/3.0
e=0

do n=1,1000
    tau(n)=n
    call random_number(y)
    x(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    c(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    d(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    f(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    g(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    h(n)=y
end do

do n=1,1000
    tau(n)=n
    call random_number(y)
    j(n)=y
end do

do n=1,1000
    if (x(n) .le. a) then
        position(n)=e+1
        e=position(n)
    else if ((x(n) .gt. a) .and. (x(n) .le. b)) then
        position(n)=e-1
        e=position(n)
    else
        position(n)=e+0
        e=position(n)
    end if
end do

do n=1,1000
    if (c(n) .le. a) then
        position1(n)=q+1
        q=position1(n)
    else if ((c(n) .gt. a) .and. (c(n) .le. b)) then
        position1(n)=q-1
        q=position1(n)
    else
        position1(n)=q+0
        q=position1(n)
    end if
end do

do n=1,1000
    if (d(n) .le. a) then
        position2(n)=w+1
        w=position2(n)
    else if ((d(n) .gt. a) .and. (d(n) .le. b)) then
        position2(n)=w-1
        w=position2(n)
    else
        position2(n)=w+0
        w=position2(n)
    end if
end do

do n=1,1000
    if (f(n) .le. a) then
        position3(n)=r+1
        r=position3(n)
    else if ((f(n) .gt. a) .and. (f(n) .le. b)) then
        position3(n)=r-1
        r=position3(n)
    else
        position3(n)=r+0
        r=position3(n)
    end if
end do

do n=1,1000
    if (g(n) .le. a) then
        position4(n)=t+1
        t=position4(n)
    else if ((g(n) .gt. a) .and. (g(n) .le. b)) then
        position4(n)=t-1
        t=position4(n)
    else
        position4(n)=t+0
        t=position4(n)
    end if
end do

do n=1,1000
    if (h(n) .le. a) then
        position5(n)=u+1
        u=position5(n)
    else if ((h(n) .gt. a) .and. (h(n) .le. b)) then
        position5(n)=u-1
        u=position5(n)
    else
        position5(n)=u+0
        u=position5(n)
    end if
end do

do n=1,1000
    if (j(n) .le. a) then
        position6(n)=i+1
        i=position6(n)
    else if ((j(n) .gt. a) .and. (j(n) .le. b)) then
        position6(n)=i-1
        i=position6(n)
    else
        position6(n)=i+0
        i=position6(n)
    end if
end do

print*, "how many particles"
read*,m
print*, "you chose ", m,"particles"
 
if (m==1) then
    do n=1,1000
        print*, tau(n),position(n)
    end do
else if (m==2) then
    do n=1,1000
        print*, tau(n),position(n),position1(n)
    end do
else if (m==3) then
    do n=1,1000
        print*, tau(n),position(n),position1(n),position2(n)
    end do
else if (m==4) then
    do n=1,1000
        print*, tau(n),position(n),position1(n),position2(n),position3(n)
    end do
else if (m==5) then
    do n=1,1000
        print*, tau(n),position(n),position1(n),position2(n),position3(n),position4(n)
    end do
else if (m==6) then
    do n=1,1000
        print*, tau(n),position(n),position1(n),position2(n),position3(n),position4(n),position5(n)
    end do
else if (m==7) then
    do n=1,1000
        print*, tau(n),position(n),position1(n),position2(n),position3(n),position4(n),position5(n),position6(n)
    end do
else
    print*, "ERROR: TOO MANY PARTICLES"
end if 
end program test
