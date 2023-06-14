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

(define-module (netlink nl80211 scan)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink deserialize)
  #:use-module (netlink generic)
  #:use-module (netlink message)
  #:use-module (netlink nl80211)
  #:use-module (netlink nl80211 information-elements)
  #:use-module (netlink route attrs)
  #:use-module ((ip link) #:select (link-name->index link-index->name))
  #:use-module ((ip utils) #:select (get-attr answer-ok?))
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (get-scan print-scan))

(define (get-scan interface)
  (define request-num (random 65535))
  (let* ((ifindex (link-name->index interface))
         (sock (connect-generic))
         (message (make-message
                   %nl80211-family-id
                   (logior NLM_F_REQUEST NLM_F_ACK NLM_F_DUMP)
                   request-num
                   0
                   (make-generic-message
                    NL80211_CMD_GET_SCAN
                    1
                    0
                    (list (make-route-attr
                           NL80211_ATTR_IFINDEX
                           (make-u32-route-attr ifindex)))))))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-nl80211-decoder)))
      (close-port sock)
      (answer-ok? (last answer))
      answer)))

(define (print-bss bss)
  (let* ((last-seen-boottime (get-attr bss NL80211_BSS_LAST_SEEN_BOOTTIME))
         (tsf (get-attr bss NL80211_BSS_TSF))
         (frequency (get-attr bss NL80211_BSS_FREQUENCY))
         (beacon-interval (get-attr bss NL80211_BSS_BEACON_INTERVAL))
         (signal-mbm (get-attr bss NL80211_BSS_SIGNAL_MBM))
         (signal-unspec (get-attr bss NL80211_BSS_SIGNAL_UNSPEC))
         (last-seen-ms-ago (get-attr bss NL80211_BSS_SEEN_MS_AGO))
         (information-elements (get-attr bss NL80211_BSS_INFORMATION_ELEMENTS))
         (beacon-information-elements (get-attr bss NL80211_BSS_BEACON_IES))
         (probe-response-data (get-attr bss NL80211_BSS_PRESP_DATA)))
    (when last-seen-boottime
      (format #t "\tlast seen: ~1,3fs [boottime]~%"
              (/ last-seen-boottime 1e9)))
    (when tsf
      ;;TODO: usec->days+hours+minutes
      (format #t "\tTSF: ~d usec~%" tsf))
    (when frequency
      (let ((offset (get-attr bss NL80211_BSS_FREQUENCY_OFFSET)))
        (if (and offset (> 0 offset))
            (format #t "\tfreq: ~d.~d~%" frequency offset)
            (format #t "\tfreq: ~d~%" frequency))))
    (when beacon-interval
      (format #t "\tbeacon interval: ~d TUs~%" beacon-interval))
    ;; TODO: capability
    (when signal-mbm
      (format #t "\tsignal: ~1,2f dBm~%" (/ signal-mbm 100)))
    (when signal-unspec
      (format #t "\tsignal: ~d/100~%" signal-unspec))
    (when last-seen-ms-ago
      (format #t "\tlast seen: ~d ms ago~%" last-seen-ms-ago))
    (when information-elements
      ;; TODO: Check PRESP_DATA and BEACON_IES
      (format #t "\tInformation elements from Probe Response frame:~%")
      (for-each (lambda (pair)
                  ;; This is a primitive implementation of print_ies
                  ;; from iw scan.c.  TODO: More printers, add support
                  ;; for vendor and extensions, add sanity checks...
                  (let ((printer (assoc-ref %information-element-printers
                                            (car pair))))
                    (when printer
                      (let ((name (information-element-name printer))
                            (proc (information-element-proc printer)))
                        (format #t "\t~a: ~a~%"
                                name
                                (proc (cdr pair)))))))
                (information-elements->alist information-elements)))))

;; This output format is identical with "iw INTERFACE scan dump".
(define (print-scan scan)
  ;; XXX: Does it make sense to loop here?  Or is it always a single payload
  ;; + NLMSG_DONE?
  (for-each
   (lambda (message)
     (cond
      ((memv (message-kind message) (list NL80211_CMD_GET_SCAN))
       (let* ((data (message-data message))
              (attrs (generic-message-attrs data))
              (ifindex (get-attr attrs NL80211_ATTR_IFINDEX))
              (bss (get-attr attrs NL80211_ATTR_BSS)))
         (if bss
             (let* ((bssid (get-attr bss NL80211_BSS_BSSID))
                    (status (get-attr bss NL80211_BSS_STATUS)))
               (when bssid
                 (format #t "BSS ~a~a -- ~a~%"
                         bssid
                         (if ifindex
                             (format #f "~@[(on ~a)~]" (link-index->name ifindex))
                             "")
                         (case status
                           ;; FIXME: Why does NL80211_BSS_STATUS_ASSOCIATED and
                           ;; friends not work here?
                           ((0) "authenticated")
                           ((1) "associated")
                           ((2) "joined")
                           (else (format #f "unknown status: ~a"
                                         (if status status "?")))))
                 (print-bss bss)))
             (format (current-error-port) "bss info missing!~%"))))
      ((memv (message-kind message) (list NL80211_CMD_SCAN_ABORTED))
       (format #t "scan aborted!~%"))))
   scan))
