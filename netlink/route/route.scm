;;;; Copyright (C) 2020 Julien Lepiller <julien@lepiller.eu>
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 

(define-module (netlink route route)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink route)
  #:use-module (netlink route attrs)
  #:use-module (srfi srfi-9)
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
    (+ 12 (apply + (map (lambda (d) (align (data-size d) 4)) attrs))))
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
       (let loop ((attrs attrs) (pos (+ pos 12)))
         (match attrs
           ((attr attrs ...)
            (serialize attr pos bv)
            (loop attrs (+ pos (align (data-size attr) 4))))
           (() #t))))))
  (family addr-message-family addr-message-type-family)
  (dest-len addr-message-dest-len addr-message-type-dest-len)
  (src-len addr-message-src-len addr-message-type-src-len)
  (tos addr-message-tos addr-message-type-tos)
  (table addr-message-table addr-message-type-table)
  (protocol addr-message-protocol addr-message-type-protocol)
  (scope addr-message-scope addr-message-type-scope)
  (type addr-message-kind addr-message-type-type)
  (flags addr-message-flags addr-message-type-flags)
  (attrs addr-message-attrs addr-message-type-attrs))

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
          (else (throw 'unknown-family family)))
        decoder bv (+ pos 12)))))
