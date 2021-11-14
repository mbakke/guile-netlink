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

(define-module (netlink error)
  #:use-module (srfi srfi-35)
  #:export (&netlink-error
            netlink-error?

            &netlink-decoder-error
            netlink-decoder-error?
            netlink-decoder-error-type

            &netlink-family-error
            netlink-family-error?
            netlink-family-error-family

            &netlink-cidr-error
            netlink-cidr-error?
            netlink-cidr-error-str

            &netlink-message-error
            netlink-message-error?

            &netlink-answer-too-big-error
            netlink-answer-too-big-error?
            netlink-answer-too-big-error-size

            &netlink-message-type-error
            netlink-message-type-error?
            netlink-message-type-error-message

            &netlink-device-error
            netlink-device-error?
            netlink-device-error-device

            &netlink-response-error
            netlink-response-error?
            netlink-response-error-errno))

(define-condition-type &netlink-error &error
  netlink-error?)

;; No decoder for type
(define-condition-type &netlink-decoder-error &netlink-error
  netlink-decoder-error?
  (type netlink-decoder-error-type))

;; Unknown protocol family
(define-condition-type &netlink-family-error &netlink-error
  netlink-family-error?
  (family netlink-family-error-family))

;; Unknow CIDR notation
(define-condition-type &netlink-cidr-error &netlink-error
  netlink-cidr-error?
  (str netlink-cidr-error-str))

;; Error when handling messages
(define-condition-type &netlink-message-error &netlink-error
  netlink-message-error?)

;; Answer is too big to handle
(define-condition-type &netlink-answer-too-big-error &netlink-message-error
  netlink-answer-too-big-error?
  (size netlink-answer-too-big-error-size))

;; Attempting to send a message that is not a message object
(define-condition-type &netlink-message-type-error &netlink-message-error
  netlink-message-type-error?
  (message netlink-message-type-error-message))

;; No such device
(define-condition-type &netlink-device-error &netlink-error
  netlink-device-error?
  (device netlink-device-error-device))

;; Got an answer, but it is an error message
(define-condition-type &netlink-response-error &netlink-error
  netlink-response-error?
  (errno netlink-response-error-errno))
