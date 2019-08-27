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
    integer :: nnode, nelement,nbc
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

!ELEMENT Factory
    !Assign number of elements
    read (1,*) nelement
    
    !Mem allocation element type
    allocate (elementvector(nelement))
    
    !Construct element type
    call inputelement(elementvector,nodevector)    

!SYSTEM Factory
    call systemconstructor(systemfem,nodevector,elementvector)

!BOUNDARY CONDITION
!BC DISPLACEMENT
    read (1,*) nbc
    allocate(BCforceException(nbc))
    BCforceException = 0
    call inputBCdisplacement(systemfem,BCforceException)
    
!BC FORCE
    call inputBCforce(systemfem)
    
!CALCULATION DISPLACEMENT
    call calculatedissystem(systemfem,BCforceException)

!CALCULATION FORCE
    call calculateforcesystem(systemfem)


!Deallocation    
    deallocate (nodevector,elementvector,BCforceException,systemfem%displacementsys,&
                systemfem%systemstifmat,systemfem%forcesys)

!closeFiles
    close(1)
end program fembar
