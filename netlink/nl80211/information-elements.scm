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

(define-module (netlink nl80211 information-elements)
  #:use-module (netlink constant)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:export (information-elements->alist
            make-information-element
            information-element?
            information-element-name
            information-element-proc
            %information-element-printers))

(define (information-elements->alist bv)
  "Parse a NL80211_BSS_INFORMATION_ELEMENTS bytevector and return an alist
containing the '(magic-byte . bv) pairs."
  (let ((len (bytevector-length bv)))
    (let loop ((pos 0)
               (result '()))
      ;; Use (+ pos 2) in case BV is incomplete.
      (if (>= (+ pos 2) len)
          (reverse result)
          (let* ((byte (bytevector-u8-ref bv pos))
                 (size (bytevector-u8-ref bv (+ pos 1)))
                 (data (make-bytevector size)))
            ;; TODO: What if SIZE is longer than the actual data and we're
            ;; at the end of BV?
            (bytevector-copy! bv (+ pos 2) data 0 size)
            (loop (+ pos size 2)
                  (alist-cons byte data result)))))))

(define (country->string bv)
  (let ((country (list->string
                  (map integer->char
                       (list (bytevector-u8-ref bv 0)
                             (bytevector-u8-ref bv 1)))))
        (environment (case (integer->char (bytevector-u8-ref bv 2))
                       ((#\I) "Indoor only")
                       ((#\O) "Outdoor only")
                       ((#\ ) "Indoor/Outdoor")
                       (else "bogus")))
        (triplets
         (cond
          ((< (bytevector-length bv) 3)
           "\t\tNo country IE triplets present")
          ((>= (bytevector-length bv) 6)
           (let loop ((pos 3)
                      (result '()))
             (if (>= (+ pos 2) (bytevector-length bv))
                 (string-join (reverse result) "\n")
                 (let ((first (bytevector-u8-ref bv pos)))
                   (if (> first IEEE80211_COUNTRY_EXTENSION_ID)
                       (let ((class (bytevector-u8-ref bv (+ pos 1)))
                             (coverage (bytevector-u8-ref bv (+ pos 2))))
                         (loop (+ pos 3)
                               (cons (string-append "\t\t\
Extension ID: ~d Regulatory Class: ~d Coverage class: ~d"
                                             first class coverage)
                                     result)))
                       (let ((num-channels (bytevector-u8-ref bv (+ pos 1)))
                             (max-power (bytevector-s8-ref bv (+ pos 2))))
                         (loop (+ pos 3)
                               (cons (format #f "\t\tChannels [~d - ~d] @ ~d dBm"
                                             first
                                             (if (<= first 14)
                                                 (+ first (- num-channels 1)) ;2GHz
                                                 (+ first (* 4 (- num-channels 1))))
                                             max-power)
                                     result))))))))
          (else
           "\t\tFailed to decode country IE triplets"))))
    (format #f "~a\tEnvironment: ~a\n~a"
            country environment triplets)))

(define (erp->string bv)
  (let ((data (bytevector-u8-ref bv 0)))
    (cond
     ((= 0 data) "<no flags>")
     ((logtest data #x01) "NonERP_Present")
     ((logtest data #x02) "Use_Protection")
     ((logtest data #x04) "Barker_Preamble_Mode")
     (else "<Failed to decode ERP>"))))

(define (ds-parameter-set->string bv)
  (let ((data (bytevector-u8-ref bv 0)))
    (format #f "channel ~d" data)))

(define (supported-rates->string bv)
  (let ((len (bytevector-length bv)))
    (let loop ((count 0)
               (output '()))
      (if (= count len)
          (string-join (reverse output) " ")
          (let* ((byte (bytevector-u8-ref bv count))
                 (r (logand byte #x7f)))
            (loop (+ count 1)
                  (cons (format #f "~a~a"
                                (cond ((and (= r BSS_MEMBERSHIP_SELECTOR_VHT_PHY)
                                            (logtest byte #x80))
                                       "VHT")
                                      ((and (= r BSS_MEMBERSHIP_SELECTOR_HT_PHY)
                                            (logtest byte #x80))
                                       "HT")
                                      (else (format #f "~d.~d"
                                                    (floor (/ r 2))
                                                    (* 5 (logand r 1)))))
                                (if (logtest byte #x80) "*" ""))
                        output)))))))

(define (bss-load->string bv)
  (let ((first (bytevector-u8-ref bv 0))
        (second (bytevector-u8-ref bv 1))
        (third (bytevector-u8-ref bv 2))
        (fourth (bytevector-u8-ref bv 3))
        (fifth (bytevector-u8-ref bv 4)))
    (string-append (format #f "\n\t\t * station count: ~d~%"
                           (let ((shift (ash second 8)))
                             (if (zero? shift) first shift)))
                   (format #f "\t\t * channel-utilisation: ~d/255~%" third)
                   (format #f "\t\t * available admission capacity: ~d [*32us]~%"
                           (let ((shift (ash fifth 8)))
                             (if (zero? shift) fourth shift))))))

(define-record-type information-element
  (make-information-element name proc)
  information-element?
  (name information-element-name)
  (proc information-element-proc))

;; Information elements are not necessarily well-formed (see comment in cfg80211.h)
;; and will vary between drivers and connected access points.  The best resource
;; for these is ieprinters[] in iw's scan.c, which this closely models.
(define %information-element-printers
  `((0 . ,(make-information-element "SSID" utf8->string))
    (1 . ,(make-information-element "Supported rates" supported-rates->string))
    (3 . ,(make-information-element "DS Parameter set" ds-parameter-set->string))
    (7 . ,(make-information-element "Country" country->string))
    (11 . ,(make-information-element "BSS Load" bss-load->string))
    (42 . ,(make-information-element "ERP" erp->string))
    (50 . ,(make-information-element "Extended supported rates"
                                     supported-rates->string))))
