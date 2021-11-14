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

(define-module (netlink route route)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink error)
  #:use-module (netlink route)
  #:use-module (netlink route attrs)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:export (make-route-message
            route-message?
            route-message-family
            route-message-dest-len
            route-message-src-len
            route-message-tos
            route-message-table
            route-message-protocol
            route-message-scope
            route-message-type
            route-message-flags
            route-message-attrs
            deserialize-route-message))

(define-data-type route-message
  (lambda (msg)
    (+ 12 (route-attr-list-size (route-message-type-attrs msg))))
  (lambda (msg pos bv)
    (match msg
      (($ route-message-type family dest-len src-len tos table protocol
          scope type flags attrs)
       (bytevector-u8-set! bv pos family)
       (bytevector-u8-set! bv (+ pos 1) dest-len)
       (bytevector-u8-set! bv (+ pos 2) src-len)
       (bytevector-u8-set! bv (+ pos 3) tos)
       (bytevector-u8-set! bv (+ pos 4) table)
       (bytevector-u8-set! bv (+ pos 5) protocol)
       (bytevector-u8-set! bv (+ pos 6) scope)
       (bytevector-u8-set! bv (+ pos 7) type)
       (bytevector-u32-set! bv (+ pos 8) flags (native-endianness))
       (serialize-route-attr-list attrs (+ pos 12) bv))))
  (family route-message-family route-message-type-family)
  (dest-len route-message-dest-len route-message-type-dest-len)
  (src-len route-message-src-len route-message-type-src-len)
  (tos route-message-tos route-message-type-tos)
  (table route-message-table route-message-type-table)
  (protocol route-message-protocol route-message-type-protocol)
  (scope route-message-scope route-message-type-scope)
  (type route-message-kind route-message-type-type)
  (flags route-message-flags route-message-type-flags)
  (attrs route-message-attrs route-message-type-attrs))

(define (deserialize-route-message decoder bv pos)
  (let ((family (bytevector-u8-ref bv pos)))
    (make-route-message
      family
      (bytevector-u8-ref bv (+ pos 1))
      (bytevector-u8-ref bv (+ pos 2))
      (bytevector-u8-ref bv (+ pos 3))
      (bytevector-u8-ref bv (+ pos 4))
      (bytevector-u8-ref bv (+ pos 5))
      (bytevector-u8-ref bv (+ pos 6))
      (bytevector-u8-ref bv (+ pos 7))
      (bytevector-u32-ref bv (+ pos 8) (native-endianness))
      (deserialize-attr-list
        (cond
          ((equal? family AF_INET) 'ipv4-route-attr)
          ((equal? family AF_INET6) 'ipv6-route-attr)
          (else (raise (condition (&netlink-family-error (family family))))))
        decoder bv (+ pos 12)))))
