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

(define-module (ip utils)
  #:use-module (ice-9 match)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink error)
  #:use-module (netlink message)
  #:use-module (netlink route attrs)
  #:use-module (netlink standard)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
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
                 (raise (condition (&netlink-response-error (errno (- err)))))))
           (raise (condition (&netlink-response-error (errno 0)))))))))

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
    (_ (raise (condition (&netlink-cidr-error (str str)))))))

(define (cidr->prefix str)
  (match (string-split str #\/)
    ((addr) #f)
    ((addr prefix) (string->number prefix))
    (_ (raise (condition (&netlink-cidr-error (str str)))))))
