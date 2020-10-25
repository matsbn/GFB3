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

module dimensions
! ------------------------------------------------------------------------------
! This module defines dimension parameters.
! ------------------------------------------------------------------------------

   implicit none

   private

   integer, parameter :: &
      nx      = 180, &
      ny      = 140, &
      nh      = 6, &
      nxperio = 0, &
      nyperio = 0

   public :: nx, ny, nh, nxperio, nyperio

end module dimensions
