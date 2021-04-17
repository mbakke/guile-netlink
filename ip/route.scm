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

(define-module (ip route)
  #:use-module (ice-9 match)
  #:use-module (ip link)
  #:use-module (ip utils)
  #:use-module (netlink route route)
  #:use-module (netlink route attrs)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink deserialize)
  #:use-module (netlink message)
  #:use-module (netlink standard)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (route-add
            route-del
            route-show))

(define-record-type <route>
  (make-route family table dest src gateway proto scope priority device)
  route?
  (family   route-family)
  (table    route-table)
  (dest     route-dest)
  (src      route-src)
  (gateway  route-gateway)
  (proto    route-proto)
  (scope    route-scope)
  (priority route-priority)
  (device   route-device))

(define* (route-del dest
                    #:key (ipv6? #f) (device #f) (table RT_TABLE_MAIN)
                          (protocol #f) (scope RT_SCOPE_NOWHERE) (type #f)
                          (priority #f) (src #f) (via #f))
  (define request-num (random 65535))

  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))
      (else #f)))

  (define message
    (make-message
      RTM_DELROUTE
      (logior NLM_F_REQUEST NLM_F_ACK)
      request-num
      0
      (make-route-message
        (if ipv6? AF_INET6 AF_INET)
        (if (equal? dest "default") 0 (cidr->prefix dest))
        (if src (cidr->prefix src) 0)
        0
        table
        (or protocol 0)
        scope
        (or type 0)
        0
        `(,@(if (equal? dest "default")
                '()
                (list (make-route-attr RTA_DST
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         (cidr->addr dest)))))
          ,@(if index
                (list (make-route-attr RTA_OIF
                        (make-u32-route-attr index)))
                '())
          ,@(if src
                (list (make-route-attr RTA_PREFSRC
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         (cidr->addr src))))
                '())
          ,@(if via
                (list (make-route-attr RTA_GATEWAY
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         via)))
                '())
          ,@(if priority
                (list (make-route-attr RTA_PRIORITY
                        (make-u32-route-attr priority)))
                '())))))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-socket sock)
      (answer-ok? (last answer)))))

(define* (route-add dest
                    #:key (ipv6? #f) (device #f) (table RT_TABLE_MAIN)
                          (protocol #f) (scope RT_SCOPE_LINK)
                          (type RTN_UNICAST) (priority #f) (src #f) (via #f))
  (define request-num (random 65535))

  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))
      (else #f)))

  (define message
    (make-message
      RTM_NEWROUTE
      (logior NLM_F_REQUEST NLM_F_ACK NLM_F_EXCL NLM_F_CREATE)
      request-num
      0
      (make-route-message
        (if ipv6? AF_INET6 AF_INET)
        (if (equal? dest "default") 0 (cidr->prefix dest))
        (if src (cidr->prefix src) 0)
        0
        table
        (or protocol 0)
        scope
        type
        0
        `(,@(if (equal? dest "default")
                '()
                (list (make-route-attr RTA_DST
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         (cidr->addr dest)))))
          ,@(if index
                (list (make-route-attr RTA_OIF
                        (make-u32-route-attr index)))
                '())
          ,@(if src
                (list (make-route-attr RTA_PREFSRC
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         (cidr->addr src))))
                '())
          ,@(if via
                (list (make-route-attr RTA_GATEWAY
                        ((if ipv6?
                             make-ipv6-route-attr
                             make-ipv4-route-attr)
                         via)))
                '())
          ,@(if priority
                (list (make-route-attr RTA_PRIORITY
                        (make-u32-route-attr priority)))
                '())))))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-socket sock)
      (answer-ok? (last answer)))))

(define (link-ref links id)
  (let loop ((links links))
    (match links
      (() #f)
      ((link links ...)
       (if (equal? (link-id link) id)
           link
           (loop links))))))

(define (get-routes links)
  (define request-num (random 65535))
  (define message
    (make-message
      RTM_GETROUTE
      (logior NLM_F_REQUEST NLM_F_DUMP)
      request-num
      0
      (make-route-message AF_UNSPEC 0 0 0 0 0 0 0 0 '())))
  (let ((sock (connect-route)))
    (send-msg message sock)
    (let* ((answer (receive-and-decode-msg sock %default-route-decoder))
           (routes (filter
                     (lambda (msg) (equal? (message-kind msg) RTM_NEWROUTE))
                     answer))
           (routes (map
                     (lambda (msg)
                       (let* ((data (message-data msg))
                              (attrs (route-message-attrs data)))
                         (make-route
                           (route-message-family data)
                           (or (get-attr attrs RTA_TABLE)
                               (route-message-table data))
                           (let ((len (route-message-dest-len data))
                                 (dest (get-attr attrs RTA_DST)))
                             (if (or (equal? len 0) (not dest))
                                 #f
                                 (string-append dest "/" (number->string len))))
                           (let ((len (route-message-src-len data))
                                 (src (get-attr attrs RTA_PREFSRC)))
                             (if (or (equal? len 0) (not src))
                                 #f
                                 (string-append src "/" (number->string len))))
                           (get-attr attrs RTA_GATEWAY)
                           (route-message-protocol data)
                           (route-message-scope data)
                           (get-attr attrs RTA_PRIORITY)
                           (link-ref links (get-attr attrs RTA_OIF)))))
                     routes)))
      (close-socket sock)
      routes)))

(define print-route
  (match-lambda
    (($ <route> family table dest src gateway proto scope priority device)
     (format #t "    ~a"
             (or dest "default"))
     (when gateway
       (format #t " via ~a" gateway))
     (when device
       (format #t " dev ~a" (link-name device)))
     (when (and proto (> proto 0))
       (format #t " proto ~a"
               (string-downcase
                 (substring (symbol->string (int->rtm-protocol proto)) 7))))
     (when (and scope (> scope 0))
       (format #t " scope ~a"
               (string-downcase
                 (substring (symbol->string (int->rtm-scope scope)) 9))))
     (when src
       (format #t " src ~a" src))
     (when priority
       (format #t " metric ~a" priority))
     (format #t "~%"))))


(define* (route-show #:key (table RT_TABLE_MAIN) (family AF_UNSPEC))
  (define links (get-links))
  (define routes (get-routes links))

  (for-each
    (lambda (route)
      (when (and (equal? (route-table route) table)
                 (or (equal? family AF_UNSPEC)
                     (equal? (route-family route) family)))
        (print-route route)))
    routes))
