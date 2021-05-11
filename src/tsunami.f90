program tsunami
    use mod_diff, only: diff
    use mod_initial, only: populate_gaussian
    implicit none

    integer, parameter :: grid_size = 100
    integer, parameter :: num_steps = 100

    real, parameter :: flow_vel = 1.
    real, parameter :: dx = 1.
    real, parameter :: dt = 1.

    integer, parameter :: mu = 25
    real, parameter :: decay = 0.02

    integer :: i, step

    real, dimension(grid_size) :: h

    ! Sense check values
    if (num_steps <= 0) stop 'Must run for positive num_steps'
    if (dt <= 0) stop 'Must have positive time step dt'
    if (dx <= 0) stop 'Must have positive spatial resolution dx'
    if (flow_vel <= 0) stop 'Must have positive flow velocity'

    call populate_gaussian(h, mu, decay)

    print *, 0, h  ! Print initial values to STDOUT

    update_loop: do step = 1, num_steps
        h = h - flow_vel * diff(h) / dx * dt
        print *, step, h
    end do update_loop

end program tsunami