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

(define-module (netlink standard)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:use-module (netlink message)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:export (make-error-message
            error-message?
            error-message-hdr
            error-message-err
            deserialize-error-message
            no-data
            deserialize-no-data))

(define-data-type error-message
  (lambda (msg)
    (+ 4 (data-size (error-message-type-hdr msg))))
  (lambda (msg pos bv)
    (match msg
     (($ error-message-type err hdr)
      (bytevector-s32-set! bv pos err (native-endianness))
      (serialize hdr (+ 4 pos) bv))))
  (err error-message-err error-message-type-err)
  (hdr error-message-hdr error-message-type-hdr))

(define (deserialize-error-message decoder bv pos)
  (make-error-message
    (bytevector-s32-ref bv pos (native-endianness))
    (deserialize 'message-hdr decoder bv (+ pos 4))))

(define no-data
  (make-nl-data
    #f
    (const 0)
    (const (make-bytevector 0))))

(define (deserialize-no-data decoder bv pos)
  no-data)
