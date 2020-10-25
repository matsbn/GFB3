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

module inicon
! ------------------------------------------------------------------------------
! This module contains initialization routine for the model problem.
! ------------------------------------------------------------------------------

   use types, only: r8
   use dimensions, only: nx, ny

   implicit none

   private

   public :: mil94ini

contains

   subroutine mil94ini(u, v, eta, gamu, gamv, dt, h0, c0, c)
   ! ---------------------------------------------------------------------------
   ! Initial conditions taken from the shallow water equation monopole vortex
   ! test case of Milliff and McWilliams (1994).
   ! ---------------------------------------------------------------------------

      real(r8), dimension(nx,ny), intent(out) :: u, v, eta, gamu, gamv
      real(r8), intent(out) :: dt, h0, c0, c

      real(r8) :: dx, f0, beta, g, xc, yc, a, ll, l2, yr, xr, q, w
      integer :: i, j

      dx = 20.e3_r8
      dt = 2700._r8
      f0 = 9.e-5_r8
      beta = 1.8e-11_r8
      g = .081_r8
      h0 = 1000._r8
      xc = 900.e3_r8
      yc = 1400.e3_r8
      a = 3.2_r8/81._r8
      ll = 200.e3_r8

      l2 = ll*ll
      c0 = sqrt(g*h0)
      c = dt*c0/dx

      ! Initialization at scalar point
      do j = 1, ny
         yr = dx*(j - .5_r8) - yc
         do i = 1, nx
            xr = dx*(i - .5_r8) - xc
            eta(i,j) = a*exp(-(xr*xr + yr*yr)/l2)
         enddo
      enddo

      ! Initialization at u-point
      do j = 1, ny
         yr = dx*(j - .5_r8) - yc
         do i = 1, nx
            gamu(i,j) = dt*(f0 + beta*yr)
            xr = dx*(i - 1._r8) - xc
            q = a*exp(-(xr*xr + yr*yr)/l2)
            w = .5_r8*f0 - sqrt(.25_r8*f0*f0 - 2._r8*q*g*h0/l2)
            u(i,j) =  w*yr/c0
         enddo
      enddo

      ! Initialization at v-point
      do j = 1, ny
         yr = dx*(j - 1._r8) - yc
         do i = 1, nx
            gamv(i,j) = dt*(f0 + beta*yr)
            xr = dx*(i - .5_r8) - xc
            q = a*exp(-(xr*xr + yr*yr)/l2)
            w = .5_r8*f0 - sqrt(.25_r8*f0*f0 - 2._r8*q*g*h0/l2)
            v(i,j) = -w*xr/c0
         enddo
      enddo

      ! Test for proper handling of cyclic indices
      ! i = 33
      ! j = 40
      ! u = cshift(cshift(u,i,1),j,2)
      ! v = cshift(cshift(v,i,1),j,2)
      ! eta = cshift(cshift(eta,i,1),j,2)
      ! gamu = cshift(cshift(gamu,i,1),j,2)
      ! gamv = cshift(cshift(gamv,i,1),j,2)
   
   end subroutine mil94ini

end module inicon
