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

(define-module (netlink route attrs)
  #:use-module (ice-9 match)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export(make-route-attr
           route-attr?
           route-attr-kind
           route-attr-data
           route-attr-size
           make-u8-route-attr
           make-u16-route-attr
           make-u32-route-attr
           make-s32-route-attr
           make-string-route-attr
           make-ethernet-route-attr
           make-bv-route-attr
           deserialize-route-attr
           deserialize-route-attr-data-string
           deserialize-route-attr-data-u8
           deserialize-route-attr-data-u32
           deserialize-route-attr-data-s32
           deserialize-route-attr-data-ethernet
           deserialize-route-attr-data-bv
           %default-route-attr-decoder))

(define-data-type route-attr
  attr-type-size
  (lambda (attr pos bv)
    (match attr
      (($ route-attr-type type data)
       (bytevector-u16-set! bv pos (attr-type-size attr) (native-endianness))
       (bytevector-u16-set! bv (+ pos 2) type (native-endianness))
       (serialize data (+ pos 4) bv))))
  (type route-attr-kind route-attr-type-type)
  (data route-attr-data route-attr-type-data))

(define (attr-type-size attr)
  (+ 4 (data-size (route-attr-type-data attr))))

(define (make-u8-route-attr num)
  (make-nl-data
    num
    (const 1)
    (lambda (data pos bv)
      (bytevector-u8-set! bv pos data))))

(define (make-u16-route-attr num)
  (make-nl-data
    num
    (const 2)
    (lambda (data pos bv)
      (bytevector-u16-set! bv pos data (native-endianness)))))

(define (make-u32-route-attr num)
  (make-nl-data
    num
    (const 4)
    (lambda (data pos bv)
      (bytevector-u32-set! bv pos data (native-endianness)))))

(define (make-s32-route-attr num)
  (make-nl-data
    num
    (const 4)
    (lambda (data pos bv)
      (bytevector-s32-set! bv pos data (native-endianness)))))

(define (make-string-route-attr str)
  (make-nl-data
    str
    (lambda (str) (bytevector-length (string->utf8 str)))
    (lambda (data pos bv)
      (let ((s (string->utf8 data)))
        (bytevector-copy! s 0 bv pos (bytevector-length s))))))

(define (ethernet->bv addr)
  (u8-list->bytevector (map (lambda (n) (string->number n 16))
                            (string-split addr #\:))))
(define (make-ethernet-route-attr addr)
  (make-nl-data
    addr
    (lambda (addr) (bytevector-length (ethernet->bv addr)))
    (lambda (data pos bv)
      (let ((a (ethernet->bv data)))
        (bytevector-copy! a 0 bv pos (bytevector-length a))))))

(define (make-bv-route-attr bv)
  (make-nl-data
    bv
    (lambda (bv) (bytevector-length bv))
    (lambda (data pos bv)
      (bytevector-copy! data 0 bv pos (bytevector-length data)))))

(define (deserialize-route-attr decoder bv pos)
  (let* ((len (bytevector-u16-ref bv pos (native-endianness)))
         (type (bytevector-u16-ref bv (+ pos 2) (native-endianness)))
         (deserialize (get-next-deserialize decoder 'attr type))
         (data-bv (make-bytevector (- len 4))))
    (bytevector-copy! bv (+ pos 4) data-bv 0 (- len 4))
    (make-route-attr
      type
      (deserialize decoder data-bv 0))))

(define (deserialize-route-attr-data-string decoder bv pos)
  (make-string-route-attr (utf8->string bv)))

(define (deserialize-route-attr-data-u32 decoder bv pos)
  (make-u32-route-attr (bytevector-u32-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-s32 decoder bv pos)
  (make-s32-route-attr (bytevector-s32-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-u8 decoder bv pos)
  (make-u8-route-attr (bytevector-u8-ref bv pos)))

(define (deserialize-route-attr-data-bv decoder bv pos)
  (make-bv-route-attr bv))

(define (deserialize-route-attr-data-ethernet decoder bv pos)
  (make-ethernet-route-attr
    (string-join (map (lambda (n) (number->string n 16))
                      (bytevector->u8-list bv))
                 ":")))

(define %default-route-attr-decoder
  `((,IFLA_IFNAME . ,deserialize-route-attr-data-string)
    (,IFLA_QDISC . ,deserialize-route-attr-data-string)
    (,IFLA_IFALIAS . ,deserialize-route-attr-data-string)
    (,IFLA_PHYS_PORT_NAME . ,deserialize-route-attr-data-string)
    (,IFLA_MTU . ,deserialize-route-attr-data-u32)
    (,IFLA_TXQLEN . ,deserialize-route-attr-data-u32)
    (,IFLA_LINK . ,deserialize-route-attr-data-u32)
    (,IFLA_WEIGHT . ,deserialize-route-attr-data-u32)
    (,IFLA_MASTER . ,deserialize-route-attr-data-u32)
    (,IFLA_NUM_VF . ,deserialize-route-attr-data-u32)
    (,IFLA_PROMISCUITY . ,deserialize-route-attr-data-u32)
    (,IFLA_NUM_TX_QUEUES . ,deserialize-route-attr-data-u32)
    (,IFLA_NUM_RX_QUEUES . ,deserialize-route-attr-data-u32)
    (,IFLA_GSO_MAX_SEGS . ,deserialize-route-attr-data-u32)
    (,IFLA_GSO_MAX_SIZE . ,deserialize-route-attr-data-u32)
    (,IFLA_GROUP . ,deserialize-route-attr-data-u32)
    (,IFLA_CARRIER_CHANGES . ,deserialize-route-attr-data-u32)
    (,IFLA_NET_NS_PID . ,deserialize-route-attr-data-u32)
    (,IFLA_NET_NS_FD . ,deserialize-route-attr-data-u32)
    (,IFLA_NEW_IFINDEX . ,deserialize-route-attr-data-u32)
    (,IFLA_MIN_MTU . ,deserialize-route-attr-data-u32)
    (,IFLA_MAX_MTU . ,deserialize-route-attr-data-u32)
    (,IFLA_CARRIER_UP_COUNT . ,deserialize-route-attr-data-u32)
    (,IFLA_CARRIER_DOWN_COUNT . ,deserialize-route-attr-data-u32)
    (,IFLA_OPERSTATE . ,deserialize-route-attr-data-u8)
    (,IFLA_LINKMODE . ,deserialize-route-attr-data-u8)
    (,IFLA_CARRIER . ,deserialize-route-attr-data-u8)
    (,IFLA_PROTO_DOWN . ,deserialize-route-attr-data-u8)
    (,IFLA_ADDRESS . ,deserialize-route-attr-data-ethernet)
    (,IFLA_BROADCAST . ,deserialize-route-attr-data-ethernet)
    (,IFLA_PERM_ADDRESS . ,deserialize-route-attr-data-ethernet)
    (default . ,deserialize-route-attr-data-bv)))
