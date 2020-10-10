;;;; Copyright (C) 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink message)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-9)
  #:export (connect
            connect-route
            close-socket
            send-msg
            receive-msg
            receive-and-decode-msg
            get-addr))

(define libc (dynamic-link))
(define ffi-socket (pointer->procedure int
                                       (dynamic-func "socket" libc)
                                       (list int int int)))
(define ffi-close (pointer->procedure void
                                      (dynamic-func "close" libc)
                                      (list int)))
(define ffi-sendmsg (pointer->procedure int
                                        (dynamic-func "sendmsg" libc)
                                        (list int '* int)
                                        #:return-errno? #t))
(define ffi-sendto (pointer->procedure int
                                       (dynamic-func "sendto" libc)
                                       (list int '* size_t int '* int)
                                       #:return-errno? #t))
(define ffi-recvmsg (pointer->procedure int
                                        (dynamic-func "recvmsg" libc)
                                        (list int '* int)))
(define ffi-bind (pointer->procedure int
                                     (dynamic-func "bind" libc)
                                     (list int '* int)))

;; define socket type
(define-record-type socket
    (make-socket num open?)
    socket?
    (num socket-num)
    (open? socket-open?))

;; define simple functions to open/close sockets
(define (open-socket proto)
    (make-socket (ffi-socket AF_NETLINK (logior SOCK_RAW SOCK_CLOEXEC) proto) #t))
(define (close-socket socket)
    (if (socket-open? socket)
        (ffi-close (socket-num socket)))
    (make-socket (socket-num socket) #f))

(define (get-addr family pid groups)
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
    (ffi-bind (socket-num sock)
              (bytevector->pointer addr)
              12)
    sock))

(define* (connect-route #:key (groups 0))
  (connect NETLINK_ROUTE (get-addr AF_NETLINK 0 groups)))

(define* (send-msg msg sock #:key (addr (get-addr AF_NETLINK 0 0)))
  (unless (message? msg)
    (throw 'cannot-send-not-message-type))

  (let* ((len (data-size msg))
         (bv (make-bytevector len)))
    (serialize msg 0 bv)
    (ffi-sendto (socket-num sock) (bytevector->pointer bv) len 0 %null-pointer 0)))

(define* (receive-msg sock #:key (addr (get-addr AF_NETLINK 0 0)))
  (let* ((len (* 1024 32))
         (bv (make-bytevector len))
         (iovec (get-iovec (bytevector->pointer bv) len))
         (msghdr (get-msghdr (bytevector->pointer addr) (bytevector-length addr)
                             iovec 1
                             %null-pointer 0
                             0))
         (size (ffi-recvmsg (socket-num sock) msghdr 0))
         (answer (make-bytevector size)))
    (when (> size (* 1024 32))
      (throw 'answer-too-big))
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
