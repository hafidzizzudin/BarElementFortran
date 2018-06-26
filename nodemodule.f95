!module for node object
module nodemod
    implicit none
    
    !Creating characteristic for node
    type nodetype
        real :: coorx, coory, coorz, disx, disy, disz 
        !integer :: forcex, forcey, forcez
    end type nodetype
    
    contains
! 1. Set X
    subroutine setcoorx(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%coorx = input
    end subroutine setcoorx
! 2. Set Y    
    subroutine setcoory(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%coory = input
    end subroutine setcoory
! 3. Set Z    
    subroutine setcoorz(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%coorz = input
    end subroutine setcoorz
    
! 4. printout input node    
    subroutine shownode(this)
        type (nodetype), allocatable , intent(in) :: this(:)
        integer :: i,n
        
        n = size(this)
        do i = 1,n
            print 1,i,this(i)%coorx,this(i)%coory,this(i)%coorz
            1 format("Coordinate of node", i3,":",f8.2,f8.2,f8.2)
        end do
    end subroutine shownode


    
end module nodemod
