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

module halo
! ------------------------------------------------------------------------------
! This module contains routines for halo update.
! ------------------------------------------------------------------------------

   use types, only: i4, r8
   use dimensions, only: nx, ny, nh, nxperio, nyperio

   implicit none

   private

   interface updhalo
      module procedure updhalo_i4, updhalo_r8
   end interface updhalo

   public :: updhalo

contains

   subroutine updhalo_i4(iarr)

      integer(i4), dimension(1-nh:nx+nh,1-nh:ny+nh), intent(inout) :: iarr

      integer :: i, j

      if (nyperio == 1) then
         if (ny < nh) then
            do i = 1, nx
               do j = 1, nh
                  iarr(i,j-nh) = iarr(i,modulo(j-nh-1,ny)+1)
                  iarr(i,ny+j) = iarr(i,modulo(j-1,ny)+1)
               enddo
            enddo
         else
            iarr(:,1-nh:0) = iarr(:,ny+1-nh:ny)
            iarr(:,ny+1:ny+nh) = iarr(:,1:nh)
         endif
      endif

      if (nxperio == 1) then
         if (nx < nh) then
            do j = 1-nyperio*nh, ny+nyperio*nh
               do i = 1, nh
                  iarr(i-nh,j) = iarr(modulo(i-nh-1,nx),j)
                  iarr(nx+i,j) = iarr(modulo(i-1,nx)+1,j)
               enddo
            enddo
         else
            iarr(1-nh:0,1-nyperio*nh:ny+nyperio*nh) = &
               iarr(nx+1-nh:nx,1-nyperio*nh:ny+nyperio*nh)
            iarr(nx+1:nx+nh,1-nyperio*nh:ny+nyperio*nh) = &
               iarr(1:nh,1-nyperio*nh:ny+nyperio*nh)
         endif
      endif

   end subroutine updhalo_i4

   subroutine updhalo_r8(arr)

      real(r8), dimension(1-nh:nx+nh,1-nh:ny+nh), intent(inout) :: arr

      integer :: i, j

      if (nyperio == 1) then
         if (ny < nh) then
            do i = 1, nx
               do j = 1, nh
                  arr(i,j-nh) = arr(i,modulo(j-nh-1,ny)+1)
                  arr(i,ny+j) = arr(i,modulo(j-1,ny)+1)
               enddo
            enddo
         else
            arr(:,1-nh:0) = arr(:,ny+1-nh:ny)
            arr(:,ny+1:ny+nh) = arr(:,1:nh)
         endif
      endif

      if (nxperio == 1) then
         if (nx < nh) then
            do j = 1-nyperio*nh, ny+nyperio*nh
               do i = 1, nh
                  arr(i-nh,j) = arr(modulo(i-nh-1,nx),j)
                  arr(nx+i,j) = arr(modulo(i-1,nx)+1,j)
               enddo
            enddo
         else
            arr(1-nh:0,1-nyperio*nh:ny+nyperio*nh) = &
               arr(nx+1-nh:nx,1-nyperio*nh:ny+nyperio*nh)
            arr(nx+1:nx+nh,1-nyperio*nh:ny+nyperio*nh) = &
               arr(1:nh,1-nyperio*nh:ny+nyperio*nh)
         endif
      endif

   end subroutine updhalo_r8

end module halo
