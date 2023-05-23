;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2021 Julien Lepiller <julien@lepiller.eu>
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

(define-module (ip link)
  #:use-module (ice-9 match)
  #:use-module (ip utils)
  #:use-module (netlink route attrs)
  #:use-module (netlink route link)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink error)
  #:use-module (netlink deserialize)
  #:use-module (netlink message)
  #:use-module (netlink standard)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (link-add
            link-del
            link-set
            link-show
            link-name->index
            get-links
            print-link

            <link> make-link link?
            link-name link-id link-type link-flags link-mtu link-qdisc
            link-state link-mode link-group link-qlen link-addr link-brd))

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
                   (map int->device-flags (split-flags (link-message-flags data)))
                   (get-attr attrs IFLA_MTU)
                   (get-attr attrs IFLA_QDISC)
                   (get-attr attrs IFLA_OPERSTATE)
                   (get-attr attrs IFLA_LINKMODE)
                   (get-attr attrs IFLA_GROUP)
                   (get-attr attrs IFLA_TXQLEN)
                   (get-attr attrs IFLA_ADDRESS)
                   (get-attr attrs IFLA_BROADCAST))))
               links)))
      (close-port sock)
      links)))

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

(define* (link-show #:key (device #f) (group #f) (up #f) (master #f) (vrf #f)
                    (type #f))
  "Return a list whose elements represent the data about the links.  If a key
is given, the resulting list is limited to those elements that match the given
criteria."
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

(define* (link-set device #:key (up #f) (down #f) (type #f)
                   (arp-on #f) (arp-off #f)
                   (dynamic-on #f) (dynamic-off #f)
                   (multicast-on #f) (multicast-off #f)
                   (allmulticast-on #f) (allmulticast-off #f)
                   (promisc-on #f) (promisc-off #f)
                   (trailers-on #f) (trailers-off #f)
                   (carrier-on #f) (carrier-off #f)
                   (txqueuelen #f) (name #f) (address #f)
                   (broadcast #f) (mtu #f) (netns #f)
                   (master #f))
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
                  (make-route-attr IFLA_IFNAME
                    (make-string-route-attr name)))
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
          ,@(if master
                (list
                  (make-route-attr IFLA_MASTER
                    (make-u32-route-attr (link-name->index master))))
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
      (close-port sock)
      (answer-ok? (last answer)))))

(define* (bond-type-args #:key (mode #f) (miimon #f) (lacp-active #f) (lacp-rate #f)
                         (primary #f) (primary-reselect #f))
  `(,@(if mode
          (list
           (make-route-attr IFLA_BOND_MODE
             (match mode
               ("balance-rr" (make-u8-route-attr BOND_MODE_ROUNDROBIN))
               ("active-backup" (make-u8-route-attr BOND_MODE_ACTIVEBACKUP))
               ("balance-xor" (make-u8-route-attr BOND_MODE_XOR))
               ("broadcast" (make-u8-route-attr BOND_MODE_BROADCAST))
               ("802.3ad" (make-u8-route-attr BOND_MODE_8023AD))
               ("balance-tlb" (make-u8-route-attr BOND_MODE_TLB))
               ("balance-alb" (make-u8-route-attr BOND_MODE_ALB))
               (_ (raise (condition
                          (&message
                           (message "Bond field `mode' can be defined as \
balance-rr|active-backup|balance-xor|broadcast|802.3ad|balance-tlb|balance-alb" ))))))))
          '())
    ,@(if miimon
          (list
           (make-route-attr IFLA_BOND_MIIMON
             (make-u32-route-attr miimon)))
          '())
    ,@(if primary
          (list
           (make-route-attr IFLA_BOND_PRIMARY
             (make-u32-route-attr (link-name->index primary))))
          '())
    ,@(if primary-reselect
          (list
           (make-route-attr IFLA_BOND_PRIMARY_RESELECT
             (match primary-reselect
               ("always" (make-u8-route-attr BOND_PRIMARY_RESELECT_ALWAYS))
               ("better" (make-u8-route-attr BOND_PRIMARY_RESELECT_BETTER))
               ("failure" (make-u8-route-attr BOND_PRIMARY_RESELECT_FAILURE))
               (_ (raise (condition
                          (&message
                           (message "Bond field `primary-reselect' can be defined as always|better|failure" ))))))))
          '())
    ,@(if lacp-active
          (list
           (make-route-attr IFLA_BOND_AD_LACP_ACTIVE
             (match lacp-active
               ("on" (make-u8-route-attr BOND_AD_LACP_ACTIVE_ON))
               ("off" (make-u8-route-attr BOND_AD_LACP_ACTIVE_OFF))
               (_ (raise (condition
                          (&message
                           (message "Bond field `lacp-active' can be defined as off|on" ))))))))
          '())
    ,@(if lacp-rate
          (list
           (make-route-attr IFLA_BOND_AD_LACP_RATE
             (match lacp-rate
               ("slow" (make-u8-route-attr 0))
               ("fast" (make-u8-route-attr 1))
               (_ (raise (condition
                          (&message
                           (message "Bond field `lacp-rate' can be defined as slow|fast"))))))))
          '())))

(define (alist->keyword+value alist)
  (fold (match-lambda*
          (((k . v) r)
           (cons* (symbol->keyword k) v r))) '() alist))

(define* (link-add name type #:key (type-args '()))
  (define request-num (random 65535))
  (define type-data
    (match type
      ("vlan"
       `(,@(if (assoc-ref type-args 'id)
               (list (make-route-attr IFLA_VLAN_ID
                       (make-u16-route-attr (assoc-ref type-args 'id))))
               '())))
      ("veth"
       `(,@(if (assoc-ref type-args 'peer)
               (list (make-route-attr VETH_INFO_PEER
                       (make-link-message
                         AF_UNSPEC 0 0 0 0
                         (list
                           (make-route-attr IFLA_IFNAME
                             (make-string-route-attr
                               (assoc-ref type-args 'peer)))))))
               '())))
      ("bond" (apply bond-type-args (alist->keyword+value type-args)))
      ;; TODO: unsupported for now
      (_ '())))
  (define message
    (make-message
      RTM_NEWLINK
      (logior NLM_F_REQUEST NLM_F_ACK NLM_F_EXCL NLM_F_CREATE)
      request-num
      0
      (make-link-message
        AF_UNSPEC
        0
        0
        0
        0
        `(,(make-route-attr IFLA_IFNAME
            (make-string-route-attr name))
          ,(make-route-attr IFLA_LINKINFO
            (make-nested-route-attr
              (list
                (make-route-attr IFLA_INFO_KIND
                  (make-string-route-attr type))
                (make-route-attr IFLA_INFO_DATA
                  (make-nested-route-attr type-data)))))
          ,@(if (assoc-ref type-args 'link)
                `(,(make-route-attr IFLA_LINK
                     (make-u32-route-attr (link-name->index (assoc-ref type-args 'link)))))
                '())))))
  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-port sock)
      (answer-ok? (last answer)))))

(define* (link-del device)
  (define request-num (random 65535))

  (define message
    (make-message
      RTM_DELLINK
      (logior NLM_F_REQUEST NLM_F_ACK)
      request-num
      0
      (make-link-message
        AF_UNSPEC
        0
        (cond
          ((number? device) device)
          ((string? device) (link-name->index device)))
        0
        0
        '())))


  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-port sock)
      (answer-ok? (last answer)))))
