!element module 
module elementmod

    use nodemod
    implicit none
    
    type elementtype    
        integer :: index1, index2
        real :: length, engstres, engstrain, area, modulus
        real, dimension(6,6) :: stifmatelement
        type(nodetype), pointer :: node1, node2
    end type elementtype
    
    real, parameter :: pi = 3.1415927
    
    contains
    
    subroutine constructorelement(this,index1,index2,this1,this2,radius,modulus)
        implicit none
        
        integer :: i,j,k
        integer, dimension(2) :: indextmp
        real,dimension(2) :: cindex
        integer, intent (in) :: index1, index2
        real, intent(in) :: radius, modulus 
        type(nodetype), intent(in),target :: this1,this2
        type(elementtype), intent(inout) :: this
        real :: dx,dy,dz, cx, cy ,cz
        
        this%node1      => this1
        this%node2      => this2
        this%index1     = index1
        this%index2     = index2
        
        dx = this1%coorx - this2%coorx
        dy = this1%coory - this2%coory
        dz = this1%coorz - this2%coorz
        
        this%length     = (dx**2+dy**2+dz**2)**0.5
        this%area       = pi*(radius**2)
        this%modulus    = modulus
        
        cx = dx/this%length
        cy = dy/this%length
        cz = dz/this%length
        print *, "Data of element ",this%length," ",this%area," ",this%modulus," ",dx," ",dy," ",dz," ",cx," ",cy," ",cz
        
        iloop: do i = 1,6
            jloop: do j = 1,6
                indextmp(1) = mod(i,3)
                indextmp(2) = mod(j,3)
                
                kloop: do k = 1,2
                    if (indextmp(k) == 1) then
                        cindex(k) = cx
                    else if (indextmp(k) == 2) then
                        cindex(k) = cy
                    else if (indextmp(k) == 0) then
                        cindex(k) = cz
                    end if
                end do kloop
                
                this%stifmatelement(i,j) = this%area*this%modulus*cindex(1)*cindex(2)/this%length

                if (abs(this%stifmatelement(i,j)) <= 1e-12) then
                    this%stifmatelement(i,j) = 0.0
                    cycle
                end if
                
                if ( (i<=3 .and. j<=3) .or. (i>=4 .and.j>=4)) then
                    cycle
                else 
                    this%stifmatelement(i,j) = -this%stifmatelement(i,j)
                end if
                
            end do jloop
        end do iloop
        
    end subroutine constructorelement
    
    subroutine showelement(this)
        implicit none
        type (elementtype), allocatable , intent(in) :: this(:)
        integer :: i,j,k,n
        
        print *," "
        n = size(this)
        do i = 1,n
            print 1,i,this(i)%index1,this(i)%index2,this(i)%length,this(i)%area,this(i)%modulus
            1 format("Data of element", i3,":",i3,i3,f8.2,f8.2,f8.2)
            
            print 2
            2 format("STIFNESS MATRIX")
            jloop : do j = 1,6
                kloop : do k = 1,6
                    if (k /= 6) then
                        write (*,3,advance = 'no') this(i)%stifmatelement(j,k)
                        3 format(es12.3)
                    else
                        write (*,4,advance = 'yes') this(i)%stifmatelement(j,k)
                        4 format(es12.3)                    
                    end if
                    
                end do kloop
            end do jloop
        end do
        

    end subroutine showelement

end module elementmod
