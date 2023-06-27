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

(define-module (ip addr)
  #:use-module (ice-9 match)
  #:use-module (ip link)
  #:use-module (ip utils)
  #:use-module (netlink route addr)
  #:use-module (netlink route attrs)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink deserialize)
  #:use-module (netlink message)
  #:use-module (netlink standard)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<addr> make-addr addr?
            addr-family addr-prefix addr-flags addr-scope
            addr-link addr-addr addr-brd addr-cacheinfo
            get-addrs
            addr-add
            addr-del
            addr-show))

(define-record-type <addr>
  (make-addr family prefix flags scope link label addr brd cacheinfo)
  addr?
  (family    addr-family)
  (prefix    addr-prefix)
  (flags     addr-flags)
  (scope     addr-scope)
  (link      addr-link)
  (label     addr-label)
  (addr      addr-addr)
  (brd       addr-brd)
  (cacheinfo addr-cacheinfo))

(define* (addr-del device cidr #:key (ipv6? #f) (peer (cidr->addr cidr))
                   (broadcast #f) (anycast #f)
                   (label #f) (scope 'global) (metric #f)
                   (home? #f) (mngtmpaddr? #f) (nodad? #f) (optimistic? #f)
                   (noprefixroute? #f) (autojoin? #f))
  (define request-num (random 65535))
  (define prefix (cidr->prefix cidr))
  (define addr (cidr->addr cidr))

  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))))

  (define scope-num
    (match scope
      ((? number? scope) scope)
      ('global RT_SCOPE_UNIVERSE)
      ('host RT_SCOPE_HOST)
      ('link RT_SCOPE_LINK)))

  (define ifa-flags
    (logior (if (and ipv6? mngtmpaddr?) IFA_F_MANAGETEMPADDR 0)
            (if noprefixroute? IFA_F_NOPREFIXROUTE 0)
            (if autojoin? IFA_F_MCAUTOJOIN 0)))

  (define message
    (make-message
      RTM_DELADDR
      (logior NLM_F_REQUEST NLM_F_ACK)
      request-num
      0
      (make-addr-message
        (if ipv6? AF_INET6 AF_INET)
        (if prefix prefix 0)
        (logior (if (and ipv6? home?) IFA_F_HOMEADDRESS 0)
                (if (and ipv6? nodad?) IFA_F_NODAD 0)
                (if (and ipv6? optimistic?) IFA_F_OPTIMISTIC 0))
        scope-num
        index
        (list
          (make-route-attr IFA_LOCAL
            ((if ipv6?
                 make-ipv6-route-attr
                 make-ipv4-route-attr)
             addr))
          (make-route-attr IFA_ADDRESS
            ((if ipv6?
                 make-ipv6-route-attr
                 make-ipv4-route-attr)
             peer))))))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-port sock)
      (answer-ok? (last answer)))))

(define* (addr-add device cidr #:key (ipv6? #f) (peer (cidr->addr cidr))
                   (broadcast #f) (anycast #f)
                   (label #f) (scope 'global) (metric #f)
                   (home? #f) (mngtmpaddr? #f) (nodad? #f) (optimistic? #f)
                   (noprefixroute? #f) (autojoin? #f))
  (define request-num (random 65535))
  (define prefix (cidr->prefix cidr))
  (define addr (cidr->addr cidr))

  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))))

  (define scope-num
    (match scope
      ((? number? scope) scope)
      ('global RT_SCOPE_UNIVERSE)
      ('host RT_SCOPE_HOST)
      ('link RT_SCOPE_LINK)))

  (define ifa-flags
    (logior (if (and ipv6? mngtmpaddr?) IFA_F_MANAGETEMPADDR 0)
            (if noprefixroute? IFA_F_NOPREFIXROUTE 0)
            (if autojoin? IFA_F_MCAUTOJOIN 0)))

  (define message
    (make-message
      RTM_NEWADDR
      (logior NLM_F_REQUEST NLM_F_ACK NLM_F_EXCL NLM_F_CREATE)
      request-num
      0
      (make-addr-message
        (if ipv6? AF_INET6 AF_INET)
        (if prefix prefix 0)
        (logior (if (and ipv6? home?) IFA_F_HOMEADDRESS 0)
                (if (and ipv6? nodad?) IFA_F_NODAD 0)
                (if (and ipv6? optimistic?) IFA_F_OPTIMISTIC 0))
        scope-num
        index
        `(,(make-route-attr IFA_LOCAL
            ((if ipv6?
                 make-ipv6-route-attr
                 make-ipv4-route-attr)
             addr))
          ,(make-route-attr IFA_ADDRESS
            ((if ipv6?
                 make-ipv6-route-attr
                 make-ipv4-route-attr)
             peer))
          ,@(if broadcast
                `((,(make-route-attr IFA_BROADCAST
                      ((if ipv6?
                           make-ipv6-route-attr
                           make-ipv4-route-attr)
                       broadcast))))
                '())
          ,@(if anycast
                `((,(make-route-attr IFA_ANYCAST
                      ((if ipv6?
                           make-ipv6-route-attr
                           make-ipv4-route-attr)
                       anycast))))
                '())
          ,@(if (> ifa-flags 0)
                `((,(make-route-attr IFA_FLAGS (make-u32-route-attr ifa-flags))))
                '())
          ,@(if label
                `((,(make-route-attr IFA_LABEL (make-string-route-attr label))))
                '())
          ,@(if metric
                `((,(make-route-attr IFA_RT_PRIORITY (make-u32-route-attr metric))))
                '())))))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-port sock)
      (answer-ok? (last answer)))))

(define (new-address-message->address msg)
  "If MSG has type 'RTM_NEWADDR', return the corresponding <addr> object.
Otherwise return #f."
  (and (eqv? (message-kind msg) RTM_NEWADDR)
       (let* ((data (message-data msg))
              (attrs (addr-message-attrs data)))
         (make-addr (addr-message-family data)
                    (addr-message-prefix-len data)
                    (map int->ifa-flag
                         (split-flags (logior (addr-message-flags data)
                                              (get-attr attrs IFA_FLAGS))))
                    (addr-message-scope data)
                    (addr-message-index data)
                    (get-attr attrs IFA_LABEL)
                    (get-attr attrs IFA_ADDRESS)
                    (get-attr attrs IFA_BROADCAST)
                    (get-attr attrs IFA_CACHEINFO)))))

(define (get-addrs)
  (define request-num (random 65535))
  (define message
    (make-message
      RTM_GETADDR
      (logior NLM_F_REQUEST NLM_F_DUMP)
      request-num
      0
      (make-addr-message AF_UNSPEC 0 0 0 0 '())))
  (let ((sock (connect-route)))
    (send-msg message sock)
    (let* ((answer (receive-and-decode-msg sock %default-route-decoder))
           (addrs (filter-map new-address-message->address answer)))
      (close-port sock)
      addrs)))

(define print-addr
  (match-lambda
    (($ <addr> family prefix flags scope link label addr brd cacheinfo)
     (format #t "    ~a ~a/~a"
             (cond
               ((= family AF_INET) "inet")
               ((= family AF_INET6) "inet6")
               (else "????"))
             addr prefix)
     (when brd
       (format #t " brd ~a" brd))
     (when scope
       (format #t " scope ~a"
               (cond
                 ((equal? scope RT_SCOPE_UNIVERSE) "global")
                 (else (string-downcase
                         (substring (symbol->string (int->rtm-scope scope))
                                    9))))))

     (for-each
       (lambda (flag)
         (unless (equal? flag 'IFA_F_PERMANENT)
           (format #t " ~a"
                   (string-downcase (substring (symbol->string flag) 6)))))
       flags)

     (when label
       (format #t " ~a" label))

     (format #t "~%")
     (when cacheinfo
       (if (member 'IFA_F_PERMANENT flags)
           (format #t "        valid_lft forever preferred_lft forever~%")
           (format #t "        valid_lft ~asec preferred_lft ~asec~%"
                   (route-cache-info-attr-type-valid cacheinfo)
                   (route-cache-info-attr-type-prefered cacheinfo)))))))


(define* (addr-show #:optional (device #f))
  (define links (get-links))
  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))
      (else #f)))
  (define addrs (get-addrs))

  (for-each
    (lambda (link)
      (unless (and index (not (equal? (link-id link) index)))
        (print-link link)
        (for-each print-addr
                  (filter (lambda (addr) (equal? (link-id link) (addr-link addr)))
                          addrs))))
    links))
