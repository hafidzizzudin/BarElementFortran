program fembar   
!Modules
    use nodemod
    use elementmod
    use summarymod
    
!Variable    
    implicit none
    type(nodetype),allocatable, dimension(:) :: nodevector
    type(elementtype),allocatable, dimension(:) :: elementvector
    integer :: n
    
!OpenFiles
    open (1, file = 'inputbar.txt', status = 'old', action = 'read')

!NODE
    !Assign number of nodes
    read (1,*)n

    !Mem allocation node vector
    allocate (nodevector(n))
    
    !Construct node type    
    call constructornode(nodevector)
    call shownode(nodevector)

!ELEMENT
    !Assign number of elements
    read (1,*) n
    print*,"Number of element : ",n
    
    !Mem allocation element type
    allocate (elementvector(n))
    
    !Construct element type
    call inputelement(elementvector,nodevector)    
    call showelement(elementvector)

    
!Deallocation    
    deallocate (nodevector)
    deallocate (elementvector)
!closeFiles
    close(1)
end program fembar
