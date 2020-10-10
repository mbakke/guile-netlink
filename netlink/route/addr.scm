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

(define-module (netlink route addr)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink route attrs)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:export (make-addr-message
            addr-message?
            addr-message-family
            addr-message-prefix-len
            addr-message-flags
            addr-message-scope
            addr-message-index
            addr-message-attrs
            deserialize-addr-message))

(define (align pos to)
  (+ pos -1 (- to (modulo (- pos 1) to))))

(define-data-type addr-message
  (lambda (msg)
    (+ 8 (apply + (map (lambda (d) (align (data-size d) 4)) attrs))))
  (lambda (msg pos bv)
    (match msg
      (($ addr-message-type family prefix-len flags scope index attrs)
       (bytevector-u8-set! bv pos family)
       (bytevector-u8-set! bv (+ pos 1) prefix-len)
       (bytevector-u8-set! bv (+ pos 2) flags)
       (bytevector-u8-set! bv (+ pos 3) scope)
       (bytevector-u32-set! bv (+ pos 4) index (native-endianness))
       (let loop ((attrs attrs) (pos (+ pos 8)))
         (match attrs
           ((attr attrs ...)
            (serialize attr pos bv)
            (loop attrs (+ pos (align (data-size attr) 4))))
           (() #t))))))
  (family addr-message-family addr-message-type-family)
  (prefix-len addr-message-prefix-len addr-message-type-prefix-len)
  (flags addr-message-flags addr-message-type-flags)
  (scope addr-message-scope addr-message-type-scope)
  (index addr-message-index addr-message-type-index)
  (attrs addr-message-attrs addr-message-type-attrs))

(define (deserialize-addr-message decoder bv pos)
  (let ((family (bytevector-u8-ref bv pos)))
    (make-addr-message
      family
      (bytevector-u8-ref bv (+ pos 1))
      (bytevector-u8-ref bv (+ pos 2))
      (bytevector-u8-ref bv (+ pos 3))
      (bytevector-u32-ref bv (+ pos 4) (native-endianness))
      (let ((len (bytevector-length bv)))
        (let loop ((pos (+ pos 8)) (attrs '()))
          (if (>= pos len)
              attrs
              (let ((attr (deserialize (cond
                                         ((equal? family AF_INET) 'ipv4-attr)
                                         ((equal? family AF_INET6) 'ipv6-attr)
                                         (else (throw 'unknown-family family)))
                                       decoder bv pos)))
                (loop (+ pos (align (data-size attr) 4))
                      (cons attr attrs)))))))))
