;;;; Copyright (C) 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (ip link)
  #:use-module (ice-9 match)
  #:use-module (netlink route attrs)
  #:use-module (netlink route link)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink deserialize)
  #:use-module (netlink message)
  #:use-module (netlink standard)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (link-set
            link-show))

(define-record-type <link>
  (make-link name id type flags mtu qdisc state mode group qlen addr brd)
  link?
  (name  link-name)
  (id    link-id)
  (type  link-type)
  (flags link-flags)
  (mtu   link-mtu)
  (qdisc link-qdisc)
  (state link-state)
  (mode  link-mode)
  (group link-group)
  (qlen  link-qlen)
  (addr  link-addr)
  (brd   link-brd))

(define (get-attr attrs type)
  (let ((attrs (filter (lambda (attr) (equal? (route-attr-kind attr) type)) attrs)))
    (match attrs
      (() #f)
      ((attr) (nl-data-data (route-attr-data attr))))))

(define (split-flags flags)
  (let loop ((max-flag 262144) (flags flags) (result '()))
    (cond
      ((equal? max-flag 1)
       (if (equal? flags 1)
           (cons (int->device-flags 1) result)
           result))
      ((< flags max-flag)
       (loop (/ max-flag 2) flags result))
      (else
        (loop (/ max-flag 2) (- flags max-flag)
              (cons
                (int->device-flags max-flag)
                result))))))

(define (get-links)
  (define request-num (random 65535))
  (define message
    (make-message
      RTM_GETLINK
      (logior NLM_F_REQUEST NLM_F_DUMP)
      request-num
      0
      (make-link-message AF_UNSPEC 0 0 0 0 '())))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let* ((answer (receive-and-decode-msg sock %default-route-decoder))
           (links (filter
                    (lambda (msg) (equal? (message-kind msg) RTM_NEWLINK))
                    answer))
           (links
             (map
               (lambda (msg)
                 (let* ((data (message-data msg))
                        (attrs (link-message-attrs data)))
                 (make-link
                   (get-attr attrs IFLA_IFNAME)
                   (link-message-index data)
                   (link-message-kind data)
                   (split-flags (link-message-flags data))
                   (get-attr attrs IFLA_MTU)
                   (get-attr attrs IFLA_QDISC)
                   (get-attr attrs IFLA_OPERSTATE)
                   (get-attr attrs IFLA_LINKMODE)
                   (get-attr attrs IFLA_GROUP)
                   (get-attr attrs IFLA_TXQLEN)
                   (get-attr attrs IFLA_ADDRESS)
                   (get-attr attrs IFLA_BROADCAST))))
               links)))
      (close-socket sock)
      links)))

(define* (link-show #:key (device #f) (group #f) (up #f) (master #f) (vrf #f)
                    (type #f))
  "Return a list whose elements represent the data about the links.  If a key
is given, the resulting list is limited to those elements that match the given
criteria."
  (define print-link
    (match-lambda
      (($ <link> name id type flags mtu qdisc state mode group qlen addr brd)
       (format #t "~a: ~a: <~a>" id name
               (string-join 
                 (map
                   (lambda (s)
                     ;; IFF_UP -> UP
                     (substring (symbol->string s) 4))
                   flags)
                 ","))
       (when mtu
         (format #t " mtu ~a" mtu))
       (when qdisc
         (format #t " qdisc ~a" qdisc))
       (when state
         (format #t " state ~a"
                 (substring (symbol->string (int->operstate state)) 8)))
       (when mode
         (format #t " mode ~a" (match mode (0 "DEFAULT") (1 "DORMANT"))))
       (when group
         (format #t " group ~a" (match group (0 "DEFAULT"))))
       (when qlen
         (format #t " qlen ~a" qlen))
       (newline)
       (cond
         ((equal? type ARPHRD_ETHER)
          (format #t "    link/ether ~a brd ~a~%" addr brd))
         ((equal? type ARPHRD_LOOPBACK)
          (format #t "    link/loopback ~a brd ~a~%" addr brd))))))

  (for-each
    (lambda (link)
      (match link
        (($ <link> lname lid ltype lflags lmtu lqdisc lstate lmode lgroup lqlen laddr lbrd)
         (when (and (or (not device) (equal? device lname))
                    (or (not group) (equal? group lgroup))
                    (or (not up) (member 'IFF_UP lflags))
                    ;(or (not master) ())
                    ;(or (not vrf) ())
                    (or (not type) (equal? type ltype)))
           (print-link link)))))
    (get-links)))

(define (link-name->index device)
  (let loop ((links (get-links)))
    (match links
      (() (throw 'no-such-device device))
      ((link links ...)
       (if (equal? (link-name link) device)
           (link-id link)
           (loop links))))))

(define (answer-ok? answer)
  (cond
    ((equal? (message-kind answer) NLMSG_DONE)
     #t)
    ((equal? (message-kind answer) NLMSG_ERROR)
     (let ((data (message-data answer)))
       (if (nl-data-data data)
           (let ((err (error-message-err data)))
             (if (equal? err 0)
                 #t
                 (begin
                   (format #t "RTNETLINK answers: ~a~%" (strerror (- err)))
                   #f)))
           #f)))))

(define* (link-set device #:key (up #f) (down #f) (type #f)
                   (arp-on #f) (arp-off #f)
                   (dynamic-on #f) (dynamic-off #f)
                   (multicast-on #f) (multicast-off #f)
                   (allmulticast-on #f) (allmulticast-off #f)
                   (promisc-on #f) (promisc-off #f)
                   (trailers-on #f) (trailers-off #f)
                   (carrier-on #f) (carrier-off #f)
                   (txqueuelen #f) (name #f) (address #f)
                   (broadcast #f) (mtu #f) (netns #f))
  (define request-num (random 65535))
  (define id (if (number? device) device (link-name->index device)))
  (define netnsfd (cond
                    ((string? netns)
                     (open (string-append "/var/run/netns/" netns) O_RDONLY))
                    ((number? netns)
                     (open (string-append "/var/run/netns/" (number->string netns))
                           O_RDONLY))
                    (else
                      #f)))
  (define message
    (make-message
      RTM_NEWLINK
      (logior NLM_F_REQUEST NLM_F_ACK)
      request-num
      0
      (make-link-message
        AF_UNSPEC
        (or type 0)
        id
        (+ (if up IFF_UP 0)
           (if arp-off IFF_NOARP 0)
           (if dynamic-on IFF_DYNAMIC 0)
           (if multicast-on IFF_MULTICAST 0)
           (if allmulticast-on IFF_ALLMULTI 0)
           (if promisc-on IFF_PROMISC 0)
           (if trailers-off IFF_NOTRAILERS 0))
        (+ (if (or up down) IFF_UP 0)
           (if (or arp-on arp-off) IFF_NOARP 0)
           (if (or dynamic-on dynamic-off) IFF_DYNAMIC 0)
           (if (or multicast-on multicast-off) IFF_MULTICAST 0)
           (if (or allmulticast-on allmulticast-off) IFF_ALLMULTI 0)
           (if (or promisc-on promisc-off) IFF_PROMISC 0)
           (if (or trailers-on trailers-off) IFF_NOTRAILERS 0))
        `(,@(if (or carrier-on carrier-off)
                (list
                  (make-route-attr IFLA_CARRIER
                    (make-u32-route-attr (if carrier-on 1 0))))
                '())
          ,@(if txqueuelen
                (list
                  (make-route-attr IFLA_TXQLEN
                    (make-u32-route-attr txqueuelen)))
                '())
          ,@(if name
                (list
                  (make-route-attr IFLA_TXQLEN
                    (make-u32-route-attr txqueuelen)))
                '())
          ,@(if address
                (list
                  (make-route-attr IFLA_ADDRESS
                    (make-ethernet-route-attr address)))
                '())
          ,@(if broadcast
                (list
                  (make-route-attr IFLA_BROADCAST
                    (make-ethernet-route-attr broadcast)))
                '())
          ,@(if mtu
                (list
                  (make-route-attr IFLA_MTU
                    (make-u32-route-attr mtu)))
                '())
          ,@(if netns
                (list
                  (make-route-attr IFLA_NET_NS_FD
                    (make-u32-route-attr
                      (fileno netnsfd))))
                '())))))
  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (when netnsfd
        (close netnsfd))
      (close-socket sock)
      (answer-ok? (last answer)))))