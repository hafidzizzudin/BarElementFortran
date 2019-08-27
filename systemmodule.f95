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
        integer :: iter_elm, start_index1, start_index2
        
        
        systemfem%nodepointer => nodevector
        systemfem%elementpointer => elementvector
        allocate(systemfem%systemstifmat(size(nodevector)*3,size(nodevector)*3))
        systemfem%systemstifmat = 0
        allocate(systemfem%displacementsys(size(nodevector)*3))
        systemfem%displacementsys = 0
        allocate(systemfem%forcesys(size(nodevector)*3))
        systemfem%forcesys = 0
        
        ! Constructing stifmat based on elements
        do iter_elm = 1, size(elementvector)
            start_index1 = (elementvector(iter_elm)%index1-1)*3+1
            start_index2 = (elementvector(iter_elm)%index2-1)*3+1
            !top-left
            systemfem%systemstifmat(start_index1:start_index1+2,start_index1:start_index1+2)=&
                systemfem%systemstifmat(start_index1:start_index1+2,start_index1:start_index1+2)&
                + elementvector(iter_elm)%stifmatelement(1:3,1:3)
            !bot-right
            systemfem%systemstifmat(start_index2:start_index2+2,start_index2:start_index2+2)=&
                systemfem%systemstifmat(start_index2:start_index2+2,start_index2:start_index2+2)&
                + elementvector(iter_elm)%stifmatelement(4:6,4:6)
            !top-right
            systemfem%systemstifmat(start_index1:start_index1+2,start_index2:start_index2+2)=&
                systemfem%systemstifmat(start_index1:start_index1+2,start_index2:start_index2+2)&
                + elementvector(iter_elm)%stifmatelement(1:3,4:6)
            !bot-left
            systemfem%systemstifmat(start_index2:start_index2+2,start_index1:start_index1+2)=&
                systemfem%systemstifmat(start_index2:start_index2+2,start_index1:start_index1+2)&
                + elementvector(iter_elm)%stifmatelement(4:6,1:3)
        end do
    end subroutine systemconstructor
    
    !set value of force in force vector of systemfem
    subroutine setforcesystem(systemfem,cord,value_in)
        implicit none
        type(systemtype), intent(inout) :: systemfem
        integer, intent(in) :: cord
        real, intent(in) :: value_in
        
        systemfem%forcesys(cord) = value_in
    end subroutine setforcesystem
    
    !set value of displacement in dis vector of systemfem
    subroutine setdissystem(systemfem,cord,value_in)
        implicit none
        type(systemtype), intent(inout) :: systemfem
        integer, intent(in) :: cord
        real, intent(in) :: value_in
        
        systemfem%displacementsys(cord) = value_in
    end subroutine setdissystem
    
    !showing stifmat of the system
    subroutine showsystem(systemfem)
        implicit none
        type(systemtype),intent(in) :: systemfem
        integer :: row
        
        do row = 1, size(systemfem%systemstifmat,1)
            write(*,*) systemfem%systemstifmat(row,:)
        end do
    end subroutine
    
    !showing displacement of the system
    subroutine showdisplacementsys(systemfem)
        implicit none
        type(systemtype),intent(in) :: systemfem
        
        write(*,*) systemfem%displacementsys
    end subroutine
    
    !showing forces of the system
    subroutine showforcesys(systemfem)
        implicit none
        type(systemtype),intent(in) :: systemfem
        
        write(*,*) systemfem%forcesys
    end subroutine
    
    !Calculate unknown displacement
    subroutine calculatedissystem(systemfem,BCforceException)
        implicit none
        type(systemtype),intent(inout) :: systemfem
        integer, dimension(:), intent(inout) :: BCforceException
        integer :: iter, iter2, sizeofsysmatmod, iter_bc, iter_pass, info
        integer, dimension(:), allocatable :: BCpass, pivot
        real,dimension(:,:), allocatable :: sysmatmod
        real,dimension(:), allocatable :: sysformod
        
        
        !sorting BC input
        call sorting(BCforceException)
        
        !allocate stif mat system
        sizeofsysmatmod = size(systemfem%systemstifmat,1) - size(BCforceException)
        allocate(sysmatmod(sizeofsysmatmod,sizeofsysmatmod))
        sysmatmod = 0
        allocate(BCpass(sizeofsysmatmod))
        
        !assign BCpass
        iter_pass = 1
        iter_bc = 1
        do iter = 1, size(systemfem%systemstifmat,1)
            if(iter == BCforceException(iter_bc)) then
                iter_bc = iter_bc + 1
            else
                BCpass(iter_pass) = iter
                iter_pass = iter_pass + 1
            end if
        end do
!~         write(*,*) BCpass
        
        !copy stif mat mod
        do iter = 1, size(BCpass)
            do iter2 = 1, size(BCpass)
                sysmatmod(iter,iter2) = systemfem%systemstifmat(BCpass(iter),BCpass(iter2))
            end do
        end do
!~         call checksymmetry(sysmatmod)
        
        !modify force system
        allocate(sysformod(sizeofsysmatmod))
        sysformod = 0
        
        do iter = 1, size(BCpass)
            sysformod(iter) = systemfem%forcesys(BCpass(iter))
            do iter2 = 1,size(BCpass)
                sysformod(iter) = sysformod(iter) - systemfem%systemstifmat(BCpass(iter),&
                                    BCpass(iter2))*systemfem%displacementsys(BCpass(iter2))
            end do
        end do
!~         write(*,*) sysformod
        
        !calculate dis system
        allocate(pivot(sizeofsysmatmod))
        pivot = 0
        call SGESV(sizeofsysmatmod,1,sysmatmod,sizeofsysmatmod,pivot,sysformod,&
                    sizeofsysmatmod,info)
!~         write(*,*) sysformod
        
        !save back to displacement system
        do iter = 1, size(BCpass)
            systemfem%displacementsys(BCpass(iter)) = sysformod(iter)
        end do
!~         do iter = 1,size(systemfem%displacementsys)
!~             write(*,'(i4)',advance="no") (iter-1)/3+1
!~             if(modulo(iter,3) == 1) then
!~                 write(*,'(A3)',advance="no") "UX"
!~             else if(modulo(iter,3) == 2) then
!~                 write(*,'(A3)',advance="no") "UY"
!~             else
!~                 write(*,'(A3)',advance="no") "UZ"
!~             end if
!~             write(*,*) systemfem%displacementsys(iter)
!~         end do
        
        deallocate(sysmatmod,sysformod,BCpass,pivot)
    end subroutine
    
    subroutine calculateforcesystem(systemfem)
        type(systemtype),intent(inout) :: systemfem
        integer :: iter_r, iter_c
        
        !neutralize or set all forces to zero
        systemfem%forcesys = 0

        !calculate force
        do iter_r = 1,size(systemfem%forcesys)
            do iter_c = 1,size(systemfem%systemstifmat,2)
                systemfem%forcesys(iter_r) = systemfem%forcesys(iter_r)+&
                systemfem%systemstifmat(iter_r,iter_c)*&
                systemfem%displacementsys(iter_c)
            end do
        end do
!~         write(*,*)
!~         write(*,*) systemfem%displacementsys
!~         do iter_r = 1,size(systemfem%forcesys)
!~             write(*,'(i4)',advance="no") (iter_r-1)/3+1
!~             if(modulo(iter_r,3) == 1) then
!~                 write(*,'(A3)',advance="no") "FX"
!~             else if(modulo(iter_r,3) == 2) then
!~                 write(*,'(A3)',advance="no") "FY"
!~             else
!~                 write(*,'(A3)',advance="no") "FZ"
!~             end if
!~             write(*,*) systemfem%forcesys(iter_r)
!~         end do
    end subroutine
    
    !sorting
    subroutine sorting(vectorint)
        implicit none
        integer, dimension(:), intent(inout) :: vectorint
        integer :: iter, iter2, tmp
        
        do iter = 1, size(vectorint)-1
            do iter2 = iter+1, size(vectorint)
                if(vectorint(iter)>vectorint(iter2)) then
                    tmp = vectorint(iter)
                    vectorint(iter) = vectorint(iter2)
                    vectorint(iter2) = tmp
                end if
            end do
        end do
    end subroutine
end module systemmodule
