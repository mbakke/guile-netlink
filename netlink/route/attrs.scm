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
           make-ipv4-route-attr
           make-ipv6-route-attr
           make-bv-route-attr
           deserialize-route-attr
           deserialize-route-attr-data-string
           deserialize-route-attr-data-u8
           deserialize-route-attr-data-u32
           deserialize-route-attr-data-s32
           deserialize-route-attr-data-ethernet
           deserialize-route-attr-data-ipv4
           deserialize-route-attr-data-ipv6
           deserialize-route-attr-data-bv
           %default-route-link-attr-decoder
           %default-route-addr-ipv4-attr-decoder
           %default-route-addr-ipv6-attr-decoder))

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

(define (ipv4->bv addr)
  (u8-list->bytevector (map (lambda (n) (string->number n))
                            (string-split addr #\.))))
(define (make-ipv4-route-attr addr)
  (make-nl-data
    addr
    (lambda (addr) (bytevector-length (ipv4->bv addr)))
    (lambda (data pos bv)
      (let ((a (ipv4->bv data)))
        (bytevector-copy! a 0 bv pos (bytevector-length a))))))

;16 bytes
(define (ipv6->bv addr)
  (let loop ((num (inet-pton AF_INET6 addr)) (lst '()))
    (match lst
      ((_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
       (u8-list->bytevector (reverse lst)))
      (_
       (loop (quotient num 256) (cons (modulo num 256) lst))))))
(define (make-ipv6-route-attr addr)
  (make-nl-data
    addr
    (lambda (addr) (bytevector-length (ipv6->bv addr)))
    (lambda (data pos bv)
      (let ((a (ipv6->bv data)))
        (bytevector-copy! a 0 bv pos (bytevector-length a))))))

(define (make-bv-route-attr bv)
  (make-nl-data
    bv
    (lambda (bv) (bytevector-length bv))
    (lambda (data pos bv)
      (bytevector-copy! data 0 bv pos (bytevector-length data)))))

(define (deserialize-route-attr message-type)
  (lambda (decoder bv pos)
    (let* ((len (bytevector-u16-ref bv pos (native-endianness)))
           (type (bytevector-u16-ref bv (+ pos 2) (native-endianness)))
           (deserialize (get-next-deserialize decoder message-type type))
           (data-bv (make-bytevector (- len 4))))
      (bytevector-copy! bv (+ pos 4) data-bv 0 (- len 4))
      (make-route-attr
        type
        (deserialize decoder data-bv 0)))))

(define (deserialize-route-attr-data-string decoder bv pos)
  (make-string-route-attr
    (or (false-if-exception (utf8->string bv))
        (make-string (bytevector-length bv) #\a))))

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

(define (deserialize-route-attr-data-ipv4 decoder bv pos)
  (make-ipv4-route-attr
    (string-join (map (lambda (n) (number->string n))
                      (bytevector->u8-list bv))
                 ".")))

(define (deserialize-route-attr-data-ipv6 decoder bv pos)
  (define (ipv6->number addr)
    (let loop ((addr (bytevector->u8-list addr)) (num 0))
      (match addr
        (() num)
        ((byte addr ...)
         (loop addr (+ (* 256 num) byte))))))
  (make-ipv6-route-attr
    (inet-ntop AF_INET6 (ipv6->number bv))))

(define %default-route-link-attr-decoder
  `((,IFLA_ADDRESS . ,deserialize-route-attr-data-ethernet)
    (,IFLA_BROADCAST . ,deserialize-route-attr-data-ethernet)
    (,IFLA_IFNAME . ,deserialize-route-attr-data-string)
    (,IFLA_MTU . ,deserialize-route-attr-data-u32)
    (,IFLA_LINK . ,deserialize-route-attr-data-u32)
    (,IFLA_QDISC . ,deserialize-route-attr-data-string)
    ;; TODO: struct rtnl_link_stats
    ;(,IFLA_STATS . ,deserialize-route-attr-data-stats)
    (default . ,deserialize-route-attr-data-bv)))

(define (default-route-addr-attr-decoder address-decoder)
  `((,IFA_ADDRESS . ,address-decoder)
    (,IFA_LOCAL . ,address-decoder)
    (,IFA_LABEL . ,deserialize-route-attr-data-string)
    (,IFA_BROADCAST . ,address-decoder)
    (,IFA_ANYCAST . ,address-decoder)
    ;; TODO: struct ifa_cacheinfo
    ;(,IFA_CACHEINFO . ,deserialize-route-attr-data-cache-info)
    (default . ,deserialize-route-attr-data-bv)))

(define %default-route-addr-ipv4-attr-decoder
  (default-route-addr-attr-decoder deserialize-route-attr-data-ipv4))

(define %default-route-addr-ipv6-attr-decoder
  (default-route-addr-attr-decoder deserialize-route-attr-data-ipv6))
