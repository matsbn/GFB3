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

module masks
! ------------------------------------------------------------------------------
! This module declares masks and a routine to initialize them.
! ------------------------------------------------------------------------------

   use types, only: r8
   use dimensions, only: nx, ny, nh

   implicit none

   private

   integer, dimension(1-nh:nx+nh,1-nh:ny+nh) :: nmsk, nmsku, nmskv
   real(r8), dimension(1-nh:nx+nh,1-nh:ny+nh) :: rmsk, rmsku, rmskv

   public :: nmsk, nmsku, nmskv, rmsk, rmsku, rmskv, setmasks

contains

   subroutine setmasks
   ! ---------------------------------------------------------------------------
   ! Define masks.
   ! ---------------------------------------------------------------------------

      use halo, only: updhalo

      implicit none

      nmsku(:,:) = 0
      nmskv(:,:) = 0

      nmsku(1:nx,:) = nmsk(0:nx-1,:)*nmsk(1:nx,:)
      nmskv(:,1:ny) = nmsk(:,0:ny-1)*nmsk(:,1:ny)

      call updhalo(nmsk)
      call updhalo(nmsku)
      call updhalo(nmskv)

      rmsk = real(nmsk, r8)
      rmsku = real(nmsku, r8)
      rmskv = real(nmskv, r8)

   end subroutine setmasks

end module masks
