module summarymodule
    use nodemodule
    use elementmodule
    use systemmodule
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
            call constructorelement(this(i),index1,index2,thisnode(index1),thisnode(index2),rad,E)
        end do
    end subroutine inputelement

!3 Input BC Displacement
    subroutine inputBCdisplacement(thissystem,BCforceException)
        type(systemtype),intent(inout) :: thissystem
        integer, intent(inout),dimension(:) :: BCforceException
        integer :: i, no,nodeid,cord
        real :: nilai
        character(len=2) :: dof
        
        print *
        print 1
        1 format("INPUT DISPLACEMENT")
        
        read(1,*)
        do i = 1, size(BCforceException)
            read(1,*) no,nodeid,dof,nilai
            !print *,no, " ",nodeid," ",dof," ", nilai
            
            if(dof == "ux") then
                call setdisx(thissystem%nodepointer(nodeid),nilai)
                cord = (nodeid-1)*3+1
            else if(dof == "uy") then
                call setdisy(thissystem%nodepointer(nodeid),nilai)
                cord = (nodeid-1)*3+2
            else if(dof == "uz") then
                call setdisz(thissystem%nodepointer(nodeid),nilai)
                cord = nodeid*3
            end if
            BCforceException(i) = cord
            call setdissystem(thissystem,cord,nilai)
        end do       
    end subroutine inputBCdisplacement

!3. Input Force
    subroutine inputBCforce(thissystem)
        type(systemtype),intent(inout) :: thissystem
        integer :: i, nbc,no,nodeid,cord
        real :: nilai
        character(len=2) :: dof
        
        read(1,*) nbc
        read(1,*)
        
        print*
        print 1
        1 format("INPUT FORCE")
        
        do i = 1,nbc
            read(1,*) no,nodeid,dof,nilai
            print *,no, " ",nodeid," ",dof," ", nilai
            
            if(dof == "fx") then
                call setforcex(thissystem%nodepointer(nodeid),nilai)
                cord = (nodeid-1)*3+1
            else if(dof == "fy") then
                call setforcey(thissystem%nodepointer(nodeid),nilai)
                cord = (nodeid-1)*3+2
            else if(dof == "fz") then
                call setforcez(thissystem%nodepointer(nodeid),nilai)
                cord = nodeid*3
            end if
            
            call setforcesystem(thissystem,cord,nilai)
        end do       
    end subroutine inputBCforce

end module summarymodule
