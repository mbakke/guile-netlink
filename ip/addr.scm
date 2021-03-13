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
  #:export (addr-add
            addr-del
            addr-show))

(define (cidr->addr str)
  (match (string-split str #\/)
    ((addr) addr)
    ((addr prefix) addr)
    (_ (throw 'incorrect-cidr-notation str))))

(define (cidr->prefix str)
  (match (string-split str #\/)
    ((addr) #f)
    ((addr prefix) (string->number prefix))
    (_ (throw 'incorrect-cidr-notation str))))

(define* (addr-del device cidr #:key (ipv6? #f))
  (define request-num (random 65535))
  (define prefix (cidr->prefix cidr))
  (define addr (cidr->addr cidr))

  (define index
    (cond
      ((number? device) device)
      ((string? device) (link-name->index device))))

  (define message
    (make-message
      RTM_DELADDR
      (logior NLM_F_REQUEST NLM_F_ACK)
      request-num
      0
      (make-addr-message
        (if ipv6? AF_INET6 AF_INET)
        (if prefix prefix 0)
        0
        0
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
             addr))))))

  (let ((sock (connect-route)))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-route-decoder)))
      (close-socket sock)
      (answer-ok? (last answer)))))
