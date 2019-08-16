!system module
module systemmodule
    !Modules
    use nodemodule
    use elementmodule
    
    implicit none
        
    type systemtype
        type(nodetype), pointer :: nodepointer(:)
        type(elementtype), pointer :: elementpointer(:)
        real, allocatable :: systemstifmat(:,:)
        real, allocatable :: displacementsys(:)
        real, allocatable :: forcesys(:)
    end type systemtype

    contains
    
    subroutine systemconstructor(systemfem,nodevector,elementvector)
        implicit none
        type(systemtype), intent(out) :: systemfem
        type(nodetype),intent(in), allocatable, target :: nodevector(:)
        type(elementtype), intent(in), allocatable, target :: elementvector(:)
        
        systemfem%nodepointer => nodevector
        systemfem%elementpointer => elementvector
        allocate(systemfem%systemstifmat(size(nodevector),size(nodevector)))
        systemfem%systemstifmat = 0
        allocate(systemfem%displacementsys(size(nodevector)*3))
        systemfem%displacementsys = 0
        allocate(systemfem%forcesys(size(nodevector)*3))
        systemfem%forcesys = 0
    end subroutine systemconstructor
    
    !set value of force in force vector of systemfem
    subroutine setforcesystem(systemfem,cord,value_in)
        implicit none
        type(systemtype), intent(out) :: systemfem
        integer, intent(in) :: cord
        real, intent(in) :: value_in
        
        systemfem%forcesys(cord) = value_in
    end subroutine setforcesystem
    
    !set value of displacement in dis vector of systemfem
    subroutine setdissystem(systemfem,cord,value_in)
        implicit none
        type(systemtype), intent(out) :: systemfem
        integer, intent(in) :: cord
        real, intent(in) :: value_in
        
        systemfem%displacementsys(cord) = value_in
    end subroutine setdissystem
    
end module systemmodule
