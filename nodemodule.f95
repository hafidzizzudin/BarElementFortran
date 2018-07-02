!module for node object
module nodemodule
    implicit none
    
    !Creating characteristic for node
    type nodetype
        real :: coorx, coory, coorz, disx, disy, disz 
        real :: forcex, forcey, forcez
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

! 4. Set UX
    subroutine setdisx(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%disx = input
    end subroutine setdisx
! 5. Set UY    
    subroutine setdisy(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%disy = input
    end subroutine setdisy
! 6. Set UZ    
    subroutine setdisz(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%disz = input
    end subroutine setdisz

! 7. Set FX
    subroutine setforcex(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%forcex = input
    end subroutine setforcex
! 8. Set FY    
    subroutine setforcey(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%forcey = input
    end subroutine setforcey
! 9. Set FZ    
    subroutine setforcez(this,input)
        real, intent (in) :: input
        type (nodetype), intent(inout) :: this
        
        this%forcez = input
    end subroutine setforcez
    
! 4. printout input node    
    subroutine shownode(this)
        type (nodetype), dimension(:) , intent(in) :: this
        integer :: i,n
        
        n = size(this)
        do i = 1,n
            print 1,i,this(i)%coorx,this(i)%coory,this(i)%coorz
            1 format("Coordinate of node", i3,":",f8.2,f8.2,f8.2)
        end do
    end subroutine shownode


    
end module nodemodule
