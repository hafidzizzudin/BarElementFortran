program fembar   
!Modules
    use nodemodule
    use elementmodule
    use summarymodule
    use systemmodule
    
!Variables    
    implicit none
    type(nodetype),allocatable, dimension(:) :: nodevector
    type(elementtype),allocatable, dimension(:) :: elementvector
    type(systemtype) :: systemfem
    integer :: nnode, nelement
    integer,allocatable,dimension(:) :: BCforceException 
    
!OpenFiles
    open (1, file = 'inputbar.txt', status = 'old', action = 'read')

!NODE Factory
    !Assign number of nodes
    read (1,*)nnode

    !Mem allocation node vector
    allocate (nodevector(nnode))
    
    !Construct node type    
    call constructornode(nodevector)
!    call shownode(nodevector)

!ELEMENT Factory
    !Assign number of elements
    read (1,*) nelement
    
    !Mem allocation element type
    allocate (elementvector(nelement))
    
    !Construct element type
    call inputelement(elementvector,nodevector)    
!    call showelement(elementvector)

!SYSTEM Factory
    call systemconstructor(systemfem,nodevector,elementvector)
    call shownode(systemfem%nodepointer)
    call showelement(systemfem%elementpointer)
    call showsystem(systemfem)
    call checksymmetry(systemfem%systemstifmat)

!BOUNDARY CONDITION
!BC DISPLACEMENT
    call inputBCdisplacement(systemfem,BCforceException)
    call showdisplacementsys(systemfem)
!BC FORCE
    call inputBCforce(systemfem)
    call showforcesys(systemfem)
    
!Deallocation    
    deallocate (nodevector,elementvector,BCforceException,systemfem%displacementsys,systemfem%forcesys)
    !allocate node vector main
    !allocate elementvector main
    !allocate BCforceException main
    !allocate systemfem%displacementsys systemmodule, subroutine systemconstructor
    !allocate systemfem%forcesys systemmodule, subroutine systemconstructor

!closeFiles
    close(1)
end program fembar
