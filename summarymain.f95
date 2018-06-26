module summarymod
    use nodemod
    use elementmod
    implicit none
    
    contains
! 1. Input Node
    subroutine constructornode(this)
        type (nodetype), allocatable , intent(inout) :: this(:)
        integer :: i,n,no
        real :: x,y,z
        
        n = size(this)
        read(1,*)
        
        do i = 1,n
            read(1,*) no,x,y,z
            call setcoorx(this(i),x)
            call setcoory(this(i),y)
            call setcoorz(this(i),z)
        end do
        
    end subroutine constructornode

!2. Input Element
    subroutine inputelement(this,thisnode)
        type (elementtype), allocatable , intent(inout) :: this(:)
        type (nodetype), allocatable , intent(in) :: thisnode(:)
        
        integer :: i,n,no,index1, index2
        real :: rad, E
        
        n = size(this)
        read(1,*)
        
        do i = 1,n
            read(1,*) no,index1,index2,rad, E
            !print *,no, " ",index1," ",index2," ", rad," ",E
            call constructorelement(this(i),index1,index2,thisnode(index1),thisnode(index2),rad,E)
        end do
    end subroutine inputelement
    
end module summarymod
