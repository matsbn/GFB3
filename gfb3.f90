! ------------------------------------------------------------------------------
! Copyright (C) 2020 Mats Bentsen
!
! This program is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! this program. If not, see <https://www.gnu.org/licenses/>.
! ------------------------------------------------------------------------------

program gfb3
! ------------------------------------------------------------------------------
! Implementation of the C-grid algorithm with generalized forward-backward time
! stepping and alternate u/v evaluation in the coriolis term. Time-staggered
! version.
! ------------------------------------------------------------------------------

   use types, only: i8, r4, r8
   use dimensions, only: nx, ny, nh
   use masks, only: nmsk, setmasks, rmsku, rmskv
   use halo, only: updhalo
   use inicon, only: mil94ini
   use diagnostics, only: wrtdiaini, wrtdia
   
   implicit none

   real(r8), dimension(1-nh:nx+nh,1-nh:ny+nh) :: u, v, eta, gamu, gamv, &
                                                 etap, up, vp
   real(r8) :: dt, h0, c0, c, q
   integer(i8) :: count_start, count_end, count_rate

   ! Parameters for time-staggered generalized forward-backward method.
   real(r8), parameter :: &
      g1 = 2._r8/3._r8, &
      g2 = 1._r8/6._r8, &
      g3 = 2._r8/3._r8, &
      d = 1._r8/6._r8, &
      c1 = 1._r8/3._r8, &
      c2 = .5_r8, &
      c3 = 1._r8/6._r8, &
      c4 = 1._r8/3._r8

   ! Derived parameters.
   real(r8), parameter :: &
      omg1 = 1._r8 - g1, &
      omg23 = 1._r8 - g2 - g3, &
      omd = 1._r8 - d, &
      omc1 = 1._r8 - c1, &
      omc2 = 1._r8 - c2, &
      omc34 = 1._r8 - c3 - c4
   
   integer :: i, j, nsteps, nstep, ih, n, nprday

   call system_clock(count_start)

   ! Set masks.
   nmsk = 0
   nmsk(1:nx,1:ny) = 1
   call updhalo(nmsk)
   call setmasks

   ! Get initial conditions and problem parameters.

   u = 0._r8
   v = 0._r8
   eta = 0._r8
   gamu = 0._r8
   gamv = 0._r8
   up = 0._r8
   vp = 0._r8
   etap = 0._r8

   call mil94ini(u(1:nx,1:ny), v(1:nx,1:ny), eta(1:nx,1:ny), &
                 gamu(1:nx,1:ny), gamv(1:nx,1:ny), dt, h0, c0, c)

   call updhalo(gamu)
   call updhalo(gamv)

   u = u*rmsku
   v = v*rmsku

   nprday = nint(86400._r8/dt)
   nsteps = nprday*200

   ! Initialize writing of diagnostics and write initial conditions.
   call wrtdiaini
   call wrtdia(u(0:nx+1,0:ny+1), v(0:nx+1,0:ny+1), eta(0:nx+1,0:ny+1), h0, c0)

   ! Integration loop.
   do nstep = 1, nsteps

      ! Update halos.
      call updhalo(u)
      call updhalo(v)
      call updhalo(eta)

      ! Prediction step.

      ih = 5
      do j = 1 - ih, ny + ih
         do i = 1 - ih, nx + ih
            etap(i,j) = eta(i,j) - c*(u(i+1,j) - u(i,j) + v(i,j+1) - v(i,j))
         enddo
      enddo

      if (mod(nstep,2) == 0) then
         ih = 4
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               up(i,j) = &
                  ( u(i,j) - c*( g1  *(etap(i,j) - etap(i-1,j)) &
                               + omg1*(eta (i,j) - eta (i-1,j))) &
                  + .25_r8*gamu(i,j)*( v(i-1,j  ) + v(i,j  ) &
                                     + v(i-1,j+1) + v(i,j+1)))*rmsku(i,j)
            enddo
         enddo
         ih = 3
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               vp(i,j) = &
                  ( v(i,j) - c*( g1  *(etap(i,j) - etap(i,j-1)) &
                               + omg1*(eta (i,j) - eta (i,j-1))) &
                  - .25_r8*gamv(i,j)*( &
                       c1  *( up(i,j-1) + up(i+1,j-1) &
                            + up(i,j  ) + up(i+1,j  )) &
                     + omc1*( u (i,j-1) + u (i+1,j-1) &
                            + u (i,j  ) + u (i+1,j  ))))*rmskv(i,j)
            enddo
         enddo
      else
         ih = 4
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               vp(i,j) = &
                  ( v(i,j) - c*( g1  *(etap(i,j) - etap(i,j-1)) &
                               + omg1*(eta (i,j) - eta (i,j-1))) &
                  - .25_r8*gamv(i,j)*( u(i,j-1) + u(i+1,j-1) &
                                     + u(i,j  ) + u(i+1,j  )))*rmskv(i,j)
            enddo
         enddo
         ih = 3
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               up(i,j) = &
                  ( u(i,j) - c*( g1  *(etap(i,j) - etap(i-1,j)) &
                               + omg1*(eta (i,j) - eta (i-1,j))) &
                  + .25_r8*gamu(i,j)*( &
                       c1  *( vp(i-1,j  ) + vp(i,j  ) &
                            + vp(i-1,j+1) + vp(i,j+1)) &
                     + omc1*( v (i-1,j  ) + v (i,j  ) &
                            + v (i-1,j+1) + v (i,j+1))))*rmsku(i,j)
            enddo
         enddo
      endif

      ! Correction step

      ih = 2
      do j = 1 - ih, ny + ih
         do i = 1 - ih, nx + ih
            etap(i,j) = g2*etap(i,j) + omg23*eta(i,j)
            eta(i,j) = eta(i,j) &
               - c*( d  *(up(i+1,j) - up(i,j) + vp(i,j+1) - vp(i,j)) &
                   + omd*(u (i+1,j) - u (i,j) + v (i,j+1) - v (i,j)))
         enddo
      enddo

      if (mod(nstep,2) == 0) then
         ih = 1
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               up(i,j) = c3*up(i,j) + omc34*u(i,j)
               u(i,j) = &
                  ( u(i,j) - c*( g3*(eta (i,j) - eta (i-1,j)) &
                               +     etap(i,j) - etap(i-1,j) ) &
                  + .25_r8*gamu(i,j)*( &
                       c2  *( vp(i-1,j  ) + vp(i,j  ) &
                            + vp(i-1,j+1) + vp(i,j+1)) &
                     + omc2*( v (i-1,j  ) + v (i,j  ) &
                            + v (i-1,j+1) + v (i,j+1))))*rmsku(i,j)
            enddo
         enddo
         ih = 0
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               v(i,j) = &
                  ( v(i,j) - c*( g3*(eta (i,j) - eta (i,j-1)) &
                               +     etap(i,j) - etap(i,j-1) ) &
                  - .25_r8*gamv(i,j)*( &
                       c4*( u (i,j-1) + u (i+1,j-1) &
                          + u (i,j  ) + u (i+1,j  )) &
                          + up(i,j-1) + up(i+1,j-1) &
                          + up(i,j  ) + up(i+1,j  ) ))*rmskv(i,j)
            enddo
         enddo
      else
         ih = 1
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               vp(i,j) = c3*vp(i,j) + omc34*v(i,j)
               v(i,j) = &
                  ( v(i,j) - c*( g3*(eta (i,j) - eta (i,j-1)) &
                               +     etap(i,j) - etap(i,j-1) ) &
                  - .25_r8*gamv(i,j)*( &
                       c2  *( up(i,j-1) + up(i+1,j-1) &
                            + up(i,j  ) + up(i+1,j  )) &
                     + omc2*( u (i,j-1) + u (i+1,j-1) &
                            + u (i,j  ) + u (i+1,j  ))))*rmskv(i,j)
            enddo
         enddo
         ih = 0
         do j = 1 - ih, ny + ih
            do i = 1 - ih, nx + ih
               u(i,j) = &
                  ( u(i,j) - c*( g3*(eta (i,j) - eta (i-1,j)) &
                               +     etap(i,j) - etap(i-1,j) ) &
                  + .25_r8*gamu(i,j)*( &
                       c4*( v (i-1,j  ) + v (i,j  ) &
                          + v (i-1,j+1) + v (i,j+1)) &
                          + vp(i-1,j  ) + vp(i,j  ) &
                          + vp(i-1,j+1) + vp(i,j+1) ))*rmsku(i,j)
            enddo
         enddo
      endif

      ! Write diagnostics.
      if (mod(nstep,10*nprday) == 0) then
         write(*,*) nstep, eta(45,70), u(45,70), v(45,70)
         call wrtdia(u(0:nx+1,0:ny+1), v(0:nx+1,0:ny+1), eta(0:nx+1,0:ny+1), &
                     h0, c0)
      endif
      
   enddo

   call system_clock(count_end, count_rate)
   write(*,*) 'Elapsed time (seconds): ', &
              real(count_end - count_start, r4)/real(count_rate, r4)

end program gfb3
