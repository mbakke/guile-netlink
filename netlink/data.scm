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

(define-module (netlink data)
  #:use-module (ice-9 match)
  #:use-module (netlink error)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (make-nl-data
            nl-data-data nl-data-size-proc nl-data-serialize-proc
            data-size ensure-data-size
            serialize deserialize
            get-current-deserialize get-next-deserialize
            define-data-type))

(define-record-type nl-data
  (make-nl-data data size-proc serialize-proc)
  nl-data?
  (data            nl-data-data)
  (size-proc       nl-data-size-proc)
  (serialize-proc   nl-data-serialize-proc))

(define (data-size data)
  ((nl-data-size-proc data) (nl-data-data data)))

(define (serialize data pos bv)
  ((nl-data-serialize-proc data) (nl-data-data data) pos bv))

(define (ensure-data-size data size)
  (make-nl-data
    (nl-data-data data)
    (const size)
    (nl-data-serialize-proc data)))

(define (get-next-deserialize decoder current-type target-type)
  (match (assoc-ref decoder current-type)
    ((_ . type-alist)
     (or (assoc-ref type-alist target-type)
         (assoc-ref type-alist 'default)))
    (#f (raise (condition (&netlink-decoder-error
                            (type current-type)))))))
  
(define (get-current-deserialize decoder current-type)
  (match (assoc-ref decoder current-type)
    ((current-deserialize . _) current-deserialize)
    (#f (raise (condition (&netlink-decoder-error
                            (type current-type)))))))

(define (deserialize type decoder bv pos)
  (let ((deserialize (get-current-deserialize decoder type)))
    (deserialize decoder bv pos)))

(define* (syntax-append x . s)
  (define (->symbol s)
    (if (symbol? s) s (syntax->datum s)))
  (datum->syntax x (apply symbol-append (map ->symbol s))))

(define-syntax define-data-type
  (lambda (x)
    (syntax-case x ()
      ((_ name size-proc serialize-proc (field accessor internal-accessor) ...)
       #`(begin
           (define-record-type #,(syntax-append x #'name '-type)
            (#,(syntax-append x 'make- #'name '-type) field ...)
            #,(syntax-append x #'name '-type?)
            (field internal-accessor) ...)

           (define (accessor data)
            (internal-accessor (nl-data-data data)))
           ...

           (define (#,(syntax-append x 'make- #'name) field ...)
            (make-nl-data
              (#,(syntax-append x 'make- #'name '-type) field ...)
              size-proc
              serialize-proc))

           (define (#,(syntax-append x #'name '?) data)
             (#,(syntax-append x #'name '-type?)
              (nl-data-data data))))))))
