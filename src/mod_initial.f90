module mod_initial

    use iso_fortran_env, only: int32, real32
    implicit none
    private
    public populate_gaussian

contains

    subroutine populate_gaussian(array, mu, decay)
        implicit none

        real(real32), intent(in out) :: array(:)
        integer(int32), intent(in) :: mu
        real(real32), intent(in) :: decay

        integer(int32) :: i

        do concurrent(i = 1:size(array))
            array(i) = exp(-decay * (i - mu) ** 2)
        end do
    end subroutine populate_gaussian

end module mod_initial
