;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (netlink route link)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink route)
  #:use-module (netlink route attrs)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:export (make-link-message
            link-message?
            link-message-family
            link-message-kind
            link-message-index
            link-message-flags
            link-message-attrs
            deserialize-link-message))

(define-data-type link-message
  (lambda (msg)
     (+ 16 (route-attr-list-size (link-message-type-attrs msg))))
  (lambda (msg pos bv)
    (match msg
      (($ link-message-type family type index flags change attrs)
       (bytevector-u16-set! bv pos family (native-endianness))
       (bytevector-u16-set! bv (+ pos 2) type (native-endianness))
       (bytevector-u32-set! bv (+ pos 4) index (native-endianness))
       (bytevector-u32-set! bv (+ pos 8) flags (native-endianness))
       (bytevector-u32-set! bv (+ pos 12) change (native-endianness))
       (serialize-route-attr-list attrs (+ pos 16) bv))))
  (family link-message-family link-message-type-family)
  (type link-message-kind link-message-type-type)
  (index link-message-index link-message-type-index)
  (flags link-message-flags link-message-type-flags)
  (change link-message-change link-message-type-change)
  (attrs link-message-attrs link-message-type-attrs))

(define (deserialize-link-message decoder bv pos)
  (make-link-message
    (bytevector-u16-ref bv pos (native-endianness))
    (bytevector-u16-ref bv (+ pos 2) (native-endianness))
    (bytevector-u32-ref bv (+ pos 4) (native-endianness))
    (bytevector-u32-ref bv (+ pos 8) (native-endianness))
    (bytevector-u32-ref bv (+ pos 12) (native-endianness))
    (deserialize-attr-list 'link-attr decoder bv (+ pos 16))))
