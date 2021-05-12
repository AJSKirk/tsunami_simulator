program tsunami
    use mod_diff, only: diff => diff_centered
    use mod_initial, only: populate_gaussian
    use iso_fortran_env, only: int32, real32
    implicit none

    integer, parameter :: grid_size = 100
    integer, parameter :: num_steps = 100

    real(real32), parameter :: dx = 1, dt = .02

    integer, parameter :: mu = 25
    real(real32), parameter :: decay = 0.02

    integer :: step

    ! Physical values
    real(real32), parameter :: g = 9.8
    real(real32), parameter :: h_mean = 10.
    real(real32), dimension(grid_size) :: h, u  ! Height and velocity


    ! Sense check values
    if (num_steps <= 0) stop 'Must run for positive num_steps'
    if (dt <= 0) stop 'Must have positive time step dt'
    if (dx <= 0) stop 'Must have positive spatial resolution dx'

    call populate_gaussian(h, mu, decay)
    u = 0

    print *, 0, h  ! Print initial values to STDOUT

    update_loop: do step = 1, num_steps
        u = u - (u * diff(u) + g * diff(h)) / dx * dt
        h = h - diff(u * (h_mean + h)) / dx * dt
        print *, step, h
    end do update_loop

end program tsunami