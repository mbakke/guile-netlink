;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2023 Marius Bakke <marius@gnu.org>
;;;;
;;;; This library is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this library.  If not, see <https://www.gnu.org/licenses/>.

(define-module (netlink generic)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module ((netlink route attrs) #:select (deserialize-attr-list))
  #:use-module ((netlink route) #:select
                (route-attr-list-size serialize-route-attr-list))
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:export (make-generic-message
            generic-message?
            generic-message-cmd
            generic-message-version
            generic-message-reserved
            generic-message-attrs
            deserialize-generic-message))

(define-data-type generic-message
  (lambda (msg)
    (+ 4 (route-attr-list-size (generic-message-type-attrs msg))))
  (lambda (msg pos bv)
    (match msg
      (($ generic-message-type cmd version reserved)
       (bytevector-u8-set! bv pos cmd)
       (bytevector-u8-set! bv (+ pos 1) version)
       (bytevector-u16-set! bv (+ pos 2) reserved (native-endianness))
       (serialize-route-attr-list attrs (+ pos 4) bv))))
  (cmd generic-message-cmd generic-message-type-cmd)
  (version generic-message-version generic-message-type-version)
  (reserved generic-message-reserved generic-message-type-reserved)
  (attrs generic-message-attrs generic-message-type-attrs))

(define (deserialize-generic-message decoder bv pos)
  (make-generic-message
    (bytevector-u8-ref bv pos)
    (bytevector-u8-ref bv (+ pos 1))
    (bytevector-u16-ref bv (+ pos 2) (native-endianness))
    (deserialize-attr-list 'nlattr decoder bv (+ pos 4))))
