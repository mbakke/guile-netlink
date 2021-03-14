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

(define-module (ip utils)
  #:use-module (ice-9 match)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink message)
  #:use-module (netlink route attrs)
  #:use-module (netlink standard)
  #:export (answer-ok?
            get-attr
            split-flags
            cidr->addr
            cidr->prefix))

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
           (cons 1 result)
           result))
      ((< flags max-flag)
       (loop (/ max-flag 2) flags result))
      (else
        (loop (/ max-flag 2) (- flags max-flag)
              (cons max-flag result))))))

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
