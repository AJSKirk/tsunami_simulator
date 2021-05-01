program tsunami
    implicit none

    integer, parameter :: grid_size = 100
    integer, parameter :: num_steps = 100

    real, parameter :: flow_vel = 1.
    real, parameter :: dx = 1.
    real, parameter :: dt = 1.

    integer, parameter :: mu = 25
    real, parameter :: decay = 0.02

    integer :: i, step

    real, dimension(grid_size) :: h, dh

    ! Sense check values
    if (num_steps <= 0) stop 'Must run for positive num_steps'
    if (dt <= 0) stop 'Must have positive time step dt'
    if (dx <= 0) stop 'Must have positive spatial resolution dx'
    if (flow_vel <= 0) stop 'Must have positive flow velocity'

    call populate_gaussian(h, mu, decay)


    print *, 0, h  ! Print initial values to STDOUT

    update_loop: do step = 1, num_steps
        dh(1) = h(1) - h(grid_size)  ! Periodic BCs

        dh(2:) = h(2:) - h(1:grid_size - 1)
        h = h - flow_vel * dh / dx * dt

        print *, step, h
    end do update_loop

contains

subroutine populate_gaussian(array, mu, decay)
    implicit none

    real, intent(in out) :: array(:)
    integer, intent(in) :: mu
    real, intent(in) :: decay

    integer :: i

    do concurrent(i = 1:size(array))
        array(i) = exp(-decay * (i - mu) ** 2)
    end do
end subroutine populate_gaussian

end program tsunami