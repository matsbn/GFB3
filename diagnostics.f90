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

module diagnostics
! ------------------------------------------------------------------------------
! THis module contains routines for writing diagnotics.
! ------------------------------------------------------------------------------

   use types, only: r8
   use dimensions, only: nx, ny

   implicit none

   private

   public :: wrtdiaini, wrtdia

contains

   subroutine wrtdiaini
   ! ---------------------------------------------------------------------------
   ! Reset files for diagnostics.
   ! ---------------------------------------------------------------------------

      open(unit = 11, file = 'fort.11', status = 'replace')
      close(unit = 11)
      open(unit = 12, file = 'fort.12', status = 'replace')
      close(unit = 12)
      open(unit = 13, file = 'fort.13', status = 'replace')
      close(unit = 13)

   end subroutine wrtdiaini

   subroutine wrtdia(u, v, eta, h0, c0)
   ! ---------------------------------------------------------------------------
   ! Write diagnostics.
   ! ---------------------------------------------------------------------------

      real(r8), dimension(0:nx+1,0:ny+1), intent(in) :: u, v, eta
      real(r8), intent(in) :: h0, c0

      open(unit = 11, file = 'fort.11', form = 'unformatted', &
           position = 'append')
      write (unit = 11) u*c0
      close(unit = 11)

      open(unit = 12, file = 'fort.12', form = 'unformatted', &
           position = 'append')
      write (unit = 12) v*c0
      close(unit = 12)

      open(unit = 13, file = 'fort.13', form = 'unformatted', &
           position = 'append')
      write (unit = 13) eta*h0
      close(unit = 13)

   end subroutine wrtdia

end module diagnostics
