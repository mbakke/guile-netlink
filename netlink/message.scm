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

(define-module (netlink message)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink standard)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (make-message
            message?
            message-kind
            message-flags
            message-seq
            message-pid
            message-data
            deserialize-message
            deserialize-message-header))

(define-data-type message
  message-type-len
  (lambda (msg pos bv)
    (match msg
      (($ message-type type flags seq pid data)
       (bytevector-u32-set! bv pos (message-type-len msg) (native-endianness))
       (bytevector-u16-set! bv (+ pos 4) type (native-endianness))
       (bytevector-u16-set! bv (+ pos 6) flags (native-endianness))
       (bytevector-u32-set! bv (+ pos 8) seq (native-endianness))
       (bytevector-u32-set! bv (+ pos 12) pid (native-endianness))
       (serialize data (+ 16 pos) bv))))
  (type  message-kind  message-type-type)
  (flags message-flags message-type-flags)
  (seq   message-seq   message-type-seq)
  (pid   message-pid   message-type-pid)
  (data  message-data  message-type-data))

(define (message-type-len msg)
  (+ 16 (data-size (message-type-data msg))))

(define (deserialize-message decoder bv pos)
  (let* ((len (bytevector-u32-ref bv pos (native-endianness)))
         (type (bytevector-u16-ref bv (+ pos 4) (native-endianness)))
         (data (make-bytevector len))
         (deserialize (get-next-deserialize decoder 'message type)))
    (bytevector-copy! bv pos data 0 len)
    (let ((data (deserialize decoder data 16)))
      (make-message
        type
        (bytevector-u16-ref bv (+ pos 6) (native-endianness))
        (bytevector-u32-ref bv (+ pos 8) (native-endianness))
        (bytevector-u32-ref bv (+ pos 12) (native-endianness))
        (if (< (data-size data) (- len 16))
            (make-nl-data #f (const (- len 16)) (const (make-bytevector 0)))
            data)))))

(define (deserialize-message-header decoder bv pos)
  (make-message
    (bytevector-u16-ref bv (+ pos 4) (native-endianness))
    (bytevector-u16-ref bv (+ pos 6) (native-endianness))
    (bytevector-u32-ref bv (+ pos 8) (native-endianness))
    (bytevector-u32-ref bv (+ pos 12) (native-endianness))
    no-data))
