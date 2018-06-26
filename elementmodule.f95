!element module 
module elementmod
    use nodemod

    implicit none
    
    type elementtype
        integer :: index1, index2
        real :: length, engstres, engstrain, area, modulus
        real, dimension(2,2) :: stifmatelement
        type(nodetype), pointer :: node1, node2
    end type elementtype
    
    real, parameter :: pi = 3.1415927
    
    contains
    
    subroutine constructorelement(this,index1,index2,this1,this2,radius,modulus)
        implicit none
        
        integer, intent (in) :: index1, index2
        real, intent(in) :: radius, modulus
        type(nodetype), intent(in),target :: this1,this2
        type(elementtype), intent(inout) :: this
        real :: dx,dy,dz
        
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
        
    end subroutine constructorelement
    
    subroutine showelement(this)
        implicit none
        type (elementtype), allocatable , intent(in) :: this(:)
        integer :: i,n
        
        print *," "
        n = size(this)
        do i = 1,n
            print 1,i,this(i)%index1,this(i)%index2,this(i)%length,this(i)%area,this(i)%modulus
            1 format("Data of element", i3,":",i3,i3,f8.2,f8.2,f8.2)
        end do
    end subroutine showelement

end module elementmod
