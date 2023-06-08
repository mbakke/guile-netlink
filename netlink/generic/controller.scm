;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2023 Marius Bakke <marius@gnu.org>
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

(define-module (netlink generic controller)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink deserialize)
  #:use-module (netlink generic)
  #:use-module (netlink message)
  #:use-module ((netlink route attrs)
                #:select (make-route-attr make-string-route-attr))
  #:use-module (ip utils)
  #:use-module (srfi srfi-1)
  #:export (family-id get-families))

(define (family-id name)
  (define request-num (random 65535))
  (let* ((sock (connect-generic))
         (message (make-message
                   GENL_ID_CTRL
                   (logior NLM_F_REQUEST NLM_F_ACK)
                   request-num
                   0
                   (make-generic-message
                    CTRL_CMD_GETFAMILY
                    1
                    0
                    (list (make-route-attr
                           CTRL_ATTR_FAMILY_NAME
                           ;; TODO: Add null-terminated-string-attr??
                           (make-string-route-attr
                            (string-append name "\0"))))))))
      (send-msg message sock)
      (let ((answer (receive-and-decode-msg sock %default-control-decoder)))
        (close-port sock)
        (answer-ok? (last answer))
        (let* ((data (message-data (last answer)))
               (attrs (generic-message-attrs data)))
          (get-attr attrs CTRL_ATTR_FAMILY_ID)))))

(define (get-families)
  (define request-num (random 65535))
  (let* ((sock (connect-generic))
         (message (make-message
                   GENL_ID_CTRL
                   (logior NLM_F_REQUEST NLM_F_ACK NLM_F_DUMP)
                   request-num
                   0
                   (make-generic-message
                    CTRL_CMD_GETFAMILY 1 0 '()))))
      (send-msg message sock)
      (let ((answer (receive-and-decode-msg sock %default-control-decoder)))
        (close-port sock)
        (answer-ok? (last answer))
        answer)))
