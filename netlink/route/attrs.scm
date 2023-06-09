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

(define-module (netlink route attrs)
  #:use-module (ice-9 match)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink route)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export(deserialize-attr-list
           make-route-attr
           route-attr?
           route-attr-kind
           route-attr-data
           route-attr-size
           make-route-cache-info-attr
           route-cache-info-attr?
           route-cache-info-attr-prefered
           route-cache-info-attr-valid
           route-cache-info-attr-cstamp
           route-cache-info-attr-tstamp
           route-cache-info-attr-type-prefered
           route-cache-info-attr-type-valid
           route-cache-info-attr-type-cstamp
           route-cache-info-attr-type-tstamp
           make-u8-route-attr
           make-u16-route-attr
           make-u32-route-attr
           make-u64-route-attr
           make-s32-route-attr
           make-nested-route-attr
           make-string-route-attr
           make-ethernet-route-attr
           make-ipv4-route-attr
           make-ipv6-route-attr
           make-addr-cache-info-attr
           make-bv-route-attr
           deserialize-route-attr
           deserialize-route-attr-data-u8
           deserialize-route-attr-data-u16
           deserialize-route-attr-data-u32
           deserialize-route-attr-data-u64
           deserialize-route-attr-data-s32
           deserialize-route-attr-data-nested
           deserialize-route-attr-data-string
           deserialize-route-attr-data-ethernet
           deserialize-route-attr-data-ipv4
           deserialize-route-attr-data-ipv6
           deserialize-route-attr-data-route-cache-info
           deserialize-route-attr-data-bv
           %default-route-addr-ipv4-attr-decoder
           %default-route-addr-ipv6-attr-decoder
           %default-route-link-attr-decoder
           %default-route-link-info-attr-decoder
           %default-route-route-ipv4-attr-decoder
           %default-route-route-ipv6-attr-decoder))

(define (deserialize-attr-list context decoder bv pos)
  (let ((len (bytevector-length bv)))
    (let loop ((pos pos) (attrs '()))
      (if (>= pos len)
          attrs
          (let ((attr (deserialize context decoder bv pos)))
            (loop (+ pos (align (data-size attr) 4))
                  (cons attr attrs)))))))

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

(define-data-type route-cache-info-attr
  (const 16)
  (lambda (attr pos bv)
    (match attr
      (($ route-cache-info-attr-type prefered valid cstamp tstamp)
       (bytevector-u32-set! bv pos prefered (native-endianness))
       (bytevector-u32-set! bv (+ pos 4) valid (native-endianness))
       (bytevector-u32-set! bv (+ pos 8) cstamp (native-endianness))
       (bytevector-u32-set! bv (+ pos 12) tstamp (native-endianness)))))
  (prefered route-cache-info-attr-prefered route-cache-info-attr-type-prefered)
  (valid route-cache-info-attr-valid route-cache-info-attr-type-valid)
  (cstamp route-cache-info-attr-cstamp route-cache-info-attr-type-cstamp)
  (tstamp route-cache-info-attr-tstamp route-cache-info-attr-type-tstamp))

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

(define (make-u64-route-attr num)
  (make-nl-data
    num
    (const 8)
    (lambda (data pos bv)
      (bytevector-u64-set! bv pos data (native-endianness)))))

(define (make-s32-route-attr num)
  (make-nl-data
    num
    (const 4)
    (lambda (data pos bv)
      (bytevector-s32-set! bv pos data (native-endianness)))))

(define (make-nested-route-attr lst)
  (make-nl-data
    lst
    route-attr-list-size
    serialize-route-attr-list))

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
       (u8-list->bytevector lst))
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
           (deserialize (get-next-deserialize decoder message-type type)))
      (if (= len 0)
          (let ((data-bv (make-bytevector 0)))
            (make-route-attr type (deserialize decoder data-bv 0)))
          (let ((data-bv (make-bytevector (- len 4))))
            (bytevector-copy! bv (+ pos 4) data-bv 0 (- len 4))
            (make-route-attr
              type
              (deserialize decoder data-bv 0)))))))

(define (deserialize-route-attr-data-nested attr-type)
  (lambda (decoder bv pos)
    (make-nested-route-attr
      (deserialize-attr-list attr-type decoder bv pos))))

(define (deserialize-route-attr-data-string decoder bv pos)
  (make-string-route-attr
    (or (false-if-exception (string-trim-right (utf8->string bv) #\nul))
        (make-string (bytevector-length bv) #\a))))

(define (deserialize-route-attr-data-u64 decoder bv pos)
  (make-u64-route-attr (bytevector-u64-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-u32 decoder bv pos)
  (make-u32-route-attr (bytevector-u32-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-s32 decoder bv pos)
  (make-s32-route-attr (bytevector-s32-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-u16 decoder bv pos)
  (make-u32-route-attr (bytevector-u16-ref bv pos (native-endianness))))

(define (deserialize-route-attr-data-u8 decoder bv pos)
  (make-u8-route-attr (bytevector-u8-ref bv pos)))

(define (deserialize-route-attr-data-bv decoder bv pos)
  (make-bv-route-attr bv))

(define (deserialize-route-attr-data-ethernet decoder bv pos)
  (make-ethernet-route-attr
    (string-join (map (lambda (n)
                        (let ((s (number->string n 16)))
                          (if (equal? (string-length s) 1)
                              (string-append "0" s)
                              s)))
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

(define (deserialize-route-attr-data-route-cache-info decoder bv pos)
  (make-route-cache-info-attr
    (bytevector-u32-ref bv pos (native-endianness))
    (bytevector-u32-ref bv (+ pos 4) (native-endianness))
    (bytevector-u32-ref bv (+ pos 8) (native-endianness))
    (bytevector-u32-ref bv (+ pos 12) (native-endianness))))

(define %default-route-link-attr-decoder
  `((,IFLA_ADDRESS . ,deserialize-route-attr-data-ethernet)
    (,IFLA_BROADCAST . ,deserialize-route-attr-data-ethernet)
    (,IFLA_IFNAME . ,deserialize-route-attr-data-string)
    (,IFLA_MTU . ,deserialize-route-attr-data-u32)
    (,IFLA_LINK . ,deserialize-route-attr-data-u32)
    (,IFLA_QDISC . ,deserialize-route-attr-data-string)
    (,IFLA_OPERSTATE . ,deserialize-route-attr-data-u8)
    (,IFLA_LINKMODE . ,deserialize-route-attr-data-u8)
    (,IFLA_GROUP . ,deserialize-route-attr-data-u32)
    (,IFLA_TXQLEN . ,deserialize-route-attr-data-u32)
    (,IFLA_LINKINFO . ,(deserialize-route-attr-data-nested 'linkinfo-attr))
    ;; TODO: struct rtnl_link_stats
    ;(,IFLA_STATS . ,deserialize-route-attr-data-stats)
    (default . ,deserialize-route-attr-data-bv)))

(define %default-route-link-info-attr-decoder
  `((,IFLA_INFO_KIND . ,deserialize-route-attr-data-string)
    (default . ,deserialize-route-attr-data-bv)))

(define (default-route-addr-attr-decoder address-decoder)
  `((,IFA_ADDRESS . ,address-decoder)
    (,IFA_LOCAL . ,address-decoder)
    (,IFA_LABEL . ,deserialize-route-attr-data-string)
    (,IFA_BROADCAST . ,address-decoder)
    (,IFA_ANYCAST . ,address-decoder)
    (,IFA_FLAGS . ,deserialize-route-attr-data-u32)
    (,IFA_CACHEINFO . ,deserialize-route-attr-data-route-cache-info)
    (,IFA_RT_PRIORITY . ,deserialize-route-attr-data-u32)
    (default . ,deserialize-route-attr-data-bv)))

(define (default-route-route-attr-decoder address-decoder)
  `((,RTA_DST . ,address-decoder)
    (,RTA_SRC . ,address-decoder)
    (,RTA_IIF . ,deserialize-route-attr-data-u32)
    (,RTA_OIF . ,deserialize-route-attr-data-u32)
    (,RTA_GATEWAY . ,address-decoder)
    (,RTA_PRIORITY . ,deserialize-route-attr-data-u32)
    (,RTA_PREFSRC . ,address-decoder)
    (,RTA_METRICS . ,deserialize-route-attr-data-u32)
    ;; TODO: struct rtnexthop
    ;(,RTA_MULTIPATH . ,deserialize-route-attr-data-rt-next-hop)
    (,RTA_FLOW . ,deserialize-route-attr-data-u32)
    ; TODO: struct rta_cacheinfo
    ;(,RTA_CACHEINFO . ,deserialize-route-attr-data-rta-cache-info)
    (,RTA_TABLE . ,deserialize-route-attr-data-u32)
    (,RTA_MARK . ,deserialize-route-attr-data-u32)
    ;; TODO: struct rta_mfc_stats
    ;(,RTA_MFC_STATS . ,deserialize-route-attr-data-rta-mfc-stats)
    ;; TODO: struct rtvia
    ;(,RTA_VIA . ,,deserialize-route-attr-data-rtvia)
    (,RTA_NEWDST . ,address-decoder)
    (,RTA_PREF . ,deserialize-route-attr-data-u8)
    (,RTA_ENCAP_TYPE . ,deserialize-route-attr-data-u16)
    ;; TODO: defined by RTA_ENCAP_TYPE
    ;(,RTA_ENCAP . ??)
    (,RTA_EXPIRES . ,deserialize-route-attr-data-u32)
    (default . ,deserialize-route-attr-data-bv)))

(define %default-route-addr-ipv4-attr-decoder
  (default-route-addr-attr-decoder deserialize-route-attr-data-ipv4))

(define %default-route-addr-ipv6-attr-decoder
  (default-route-addr-attr-decoder deserialize-route-attr-data-ipv6))

(define %default-route-route-ipv4-attr-decoder
  (default-route-route-attr-decoder deserialize-route-attr-data-ipv4))

(define %default-route-route-ipv6-attr-decoder
  (default-route-route-attr-decoder deserialize-route-attr-data-ipv6))
