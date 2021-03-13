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
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink message)
  #:use-module (netlink standard)
  #:export (answer-ok?))

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
