module mod_diff

    use iso_fortran_env, only: int32, real32
    implicit none

contains

    pure function diff(levels)
        real(real32), intent(in) :: levels(:)
        real(real32) :: diff(size(levels))
        integer :: imax

        imax = size(levels)

        diff(1) = levels(1) - levels(imax)  ! Periodid BCs
        diff(2:) = levels(2:) - levels(1:imax - 1)
    end function diff

end module mod_diff