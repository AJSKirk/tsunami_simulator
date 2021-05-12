module mod_diff

    use iso_fortran_env, only: int32, real32
    implicit none
    private
    public diff_centered

contains

    pure function diff_centered(levels) result(diff)
        real(real32), intent(in) :: levels(:)
        real(real32) :: diff(size(levels))
        integer :: imax

        imax = size(levels)

        diff(1) =  .5 * (levels(2) - levels(imax))
        diff(2:imax-1) = .5 * (levels(3:imax) - levels(1:imax-2))
        diff(imax) = .5 * (levels(1) - levels(imax-1))
    end function diff_centered

end module mod_diff