program FiniteRandomWalkWithSites
implicit none
integer :: position(4,1000),n,y,g,m,z,e ! The 2 used in the declaration can be changed in order to create n-number of walkers
real::x,onethird,twothird
real, dimension(1000)::random
onethird=1.00/3.00
twothird=2.00/3.00
do n=1,1000
    position(1,n)=n
end do
do z=2,4 !Despite the fact that this do loop is redundant with just 1 "position column", the ending value ought to be modified to the same value as the one made on line 3
    y=1
    m=0
    do n=1,1000
        call random_number(x)
        random(n)=x
            if (x .le. onethird) then
                m=y+1
                if (m .gt. 6) then
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
            else if ((x .gt. onethird) .and. (x .le. twothird)) then
                m=y-1
                if (m .gt. 6) then
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
            else               
                m=y+0
                if (m .gt. 6) then
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
            end if
    end do
end do
jloop:do z=2,4
    ploop:do n=1,1000
        if ((position(z,n) .eq. position(z+1,n)) .and. (position(z,n) .eq. position(z+2,n))) then
            call random_number(x)
            if (z .eq. 2) then            
                if (x .le. onethird) then
                    e=z
                else if ((x .gt. onethird) .and. (x .le. twothird)) then
                    e=z+1
                else
                    e=z+2
                end if
            else if (z .eq. 3) then            
                if (x .le. onethird) then
                    e=z
                else if ((x .gt. onethird) .and. (x .le. twothird)) then
                    e=z+1
                else
                    e=z-1
                end if
            else if (z .eq. 4) then            
                if (x .le. onethird) then
                    e=z
                else if ((x .gt. onethird) .and. (x .le. twothird)) then
                    e=z-1
                else
                    e=z-2
                end if
            end if                                                
        end if
        if (random(n) .le. onethird) then
            if (x .lt. .50) then
                position(e,n)=position(e,n)+1
                if (position(e,n) .gt. 6) then
                    position(e,n)=1
                end if                
            else if (x .gt. .50) then
                position(e,n)=position(e,n)+2
                if (position(e,n) .gt. 6) then
                    position(e,n)=1
                end if                
            end if
        else if ((random(n) .gt. onethird) .and. (random(n) .le. twothird)) then
            if (x .lt. .50) then
                position(e,n)=position(e,n)-1
                if (position(e,n) .lt. 1) then
                    position(e,n)=6
                end if
            else if (x .gt. .50) then
                position(e,n)=position(e,n)-2
                if (position(e,n) .lt. 1) then
                    position(e,n)=6
                end if                
            end if
        else
            if (x .lt. .50) then
                position(e,n)=position(e,n)+1
                if (position(e,n) .gt. 6) then
                    position(e,n)=1
                end if
            else if (x .gt. .50) then
                position(e,n)=position(e,n)-1
                if (position(e,n) .lt. 1) then
                    position(e,n)=6
                end if
            end if
        end if
    end do ploop
end do jloop
do g=1,1000
        print*, position(1,g),position(2,g),position(3,g),position(4,g)!...position(a,g) [where a is rightmost column #]
end do

end program FiniteRandomWalkWithSites
