;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2021 Julien Lepiller <julien@lepiller.eu>
;;;; Copyright (C) 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink error)
  #:use-module (netlink message)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:autoload   (ice-9 suspendable-ports) (current-read-waiter
                                          current-write-waiter)
  #:export (connect
            connect-route
            close-socket
            send-msg
            receive-msg
            receive-and-decode-msg
            get-addr))

(define libc (dynamic-link))

(define* (syscall->procedure return-type function
                             argument-types
                             #:key waiter)
  "Return a procedure that calls FUNCTION, a syscall wrapper from the C library
with the given RETURN-TYPE and ARGUMENT-TYPES.  When WAITER is true and the
first argument is a port, call it upon EAGAIN or EWOULDBLOCK."
  (let ((proc (pointer->procedure return-type
                                  (dynamic-func function libc)
                                  argument-types
                                  #:return-errno? #t)))
    (lambda (first . rest)
      (let loop ()
        (let ((ret errno (apply proc
                                (if (port? first) (fileno first) first)
                                rest)))
          (if (< ret 0)
              (if (and (memv errno (list EAGAIN EWOULDBLOCK))
                       (port? first) waiter)
                  (begin
                    ((waiter) first)
                    (loop))
                  (throw 'system-error function "~A"
                         (list (strerror errno)) (list errno)))
              ret))))))

(define ffi-sendto
  (syscall->procedure int "sendto" (list int '* size_t int '* int)
                      #:waiter (lambda () (current-write-waiter))))
(define ffi-recvmsg
  (syscall->procedure int "recvmsg" (list int '* int)
                      #:waiter (lambda () (current-read-waiter))))
(define ffi-bind
  (syscall->procedure int "bind" (list int '* int)
                      #:waiter (lambda () (current-read-waiter))))

;; define simple functions to open/close sockets
(define (open-socket proto)
  (socket AF_NETLINK (logior SOCK_RAW SOCK_CLOEXEC) proto))

(define (close-socket sock)
  (issue-deprecation-warning
   "'close-socket' is deprecated; use 'close-port' instead.")
  (close-port sock))

(define (get-addr family pid groups)
  "This is a variant of 'make-socket-address' for AF_NETLINK sockets.  The
main difference is that it returns a raw bytevector that libguile procedures
such as 'bind' cannot handle."
  (let ((addr (make-bytevector 12)))
    (bytevector-u16-set! addr 0 family (native-endianness))
    (bytevector-u32-set! addr 4 pid (native-endianness))
    (bytevector-u32-set! addr 8 groups (native-endianness))
    addr))

(define (get-msghdr name namelen iov iovlen control controllen flags)
  (make-c-struct
    (list '* size_t '* size_t '* size_t int)
    (list name namelen iov iovlen control controllen flags)))

(define (get-iovec content size)
  (make-c-struct
    (list '* size_t)
    (list content size)))

(define* (connect proto addr)
  (let ((sock (open-socket proto)))
    (ffi-bind sock
              (bytevector->pointer addr)
              12)
    sock))

(define* (connect-route #:key (groups 0))
  (connect NETLINK_ROUTE (get-addr AF_NETLINK 0 groups)))

(define* (send-msg msg sock #:key (addr (get-addr AF_NETLINK 0 0)))
  (unless (message? msg)
    (raise (condition (&netlink-message-type-error
                        (message msg)))))

  (let* ((len (data-size msg))
         (bv (make-bytevector len)))
    (serialize msg 0 bv)
    (ffi-sendto sock (bytevector->pointer bv) len 0 %null-pointer 0)))

(define* (receive-msg sock #:key (addr (get-addr AF_NETLINK 0 0)))
  (let* ((len (* 1024 32))
         (bv (make-bytevector len))
         (iovec (get-iovec (bytevector->pointer bv) len))
         (msghdr (get-msghdr (bytevector->pointer addr) (bytevector-length addr)
                             iovec 1
                             %null-pointer 0
                             0))
         (size (ffi-recvmsg sock msghdr 0))
         (answer (make-bytevector size)))
    (when (> size (* 1024 32))
      (raise (condition (&netlink-answer-too-big-error (size size)))))
    (when (> size 0)
      (bytevector-copy! bv 0 answer 0 size))
    answer))

(define* (receive-and-decode-msg sock decoder
                                 #:key (addr (get-addr AF_NETLINK 0 0)))
  (let* ((answer (receive-msg sock #:addr addr))
         (size (bytevector-length answer)))
    (let loop ((messages '()) (pos 0))
      (if (>= pos size)
          (let ((last-message (car messages)))
            (if (and
                  (equal? (logand (message-flags last-message) NLM_F_MULTI)
                          NLM_F_MULTI)
                  (> (message-kind last-message) NLMSG_OVERUN))
                (append (reverse messages)
                        (receive-and-decode-msg sock decoder #:addr addr))
                (reverse messages)))
          (let ((message (deserialize 'message decoder answer pos)))
            (loop (cons message messages)
                  (+ (data-size message) pos)))))))
