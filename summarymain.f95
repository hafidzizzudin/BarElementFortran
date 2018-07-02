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
        integer, allocatable,intent(inout),dimension(:) :: BCforceException
        integer :: i, nbc,no,nodeid,cord
        real :: nilai
        character(len=2) :: dof
        
        read(1,*) nbc
        read(1,*)
        print *
        print 1
        1 format("INPUT DISPLACEMENT")
        
        allocate(BCforceException(nbc))
        
        do i = 1,nbc
            read(1,*) no,nodeid,dof,nilai
            print *,no, " ",nodeid," ",dof," ", nilai
            
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

!4 Check Symmetry of matrix
    subroutine checksymmetry(this)
        real,dimension(:,:),intent(in) :: this
        logical :: check = .true.
        integer :: i,j,icheck,jcheck,sizethis
        
        sizethis = int(size(this)**0.5)
        iloop: do i=2,sizethis
            jloop: do j=1,i
                if( this(i,j) /= this(j,i) ) then
                    check = .not. check
                    icheck = i
                    jcheck = j
                    goto 99
                end if
            end do jloop
        end do iloop
        
99      if(check) then
            print 1
            1 format("The stiffness matrix is symmetry")
        else
            print 2,icheck,jcheck,size(this)
            2 format("The stiffness matrix is not symmetry, please check again!!!",i4,i4)            
        end if
    end subroutine checksymmetry

end module summarymodule
