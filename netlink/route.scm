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

(define-module (netlink route)
  #:use-module (ice-9 match)
  #:use-module (netlink data)
  #:export (align
            route-attr-list-size
            serialize-route-attr-list))

(define (align pos to)
  (+ pos -1 (- to (modulo (- pos 1) to))))

(define (route-attr-list-size attrs)
  (apply + (map (lambda (d) (align (data-size d) 4)) attrs)))

(define (serialize-route-attr-list attrs pos bv)
  (let loop ((attrs attrs) (pos pos))
    (match attrs
      ((attr attrs ...)
       (serialize attr pos bv)
       (loop attrs (+ pos (align (data-size attr) 4))))
      (() #t))))
