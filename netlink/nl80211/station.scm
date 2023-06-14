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

(define-module (netlink nl80211 station)
  #:use-module (netlink connection)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module (netlink deserialize)
  #:use-module (netlink generic)
  #:use-module (netlink message)
  #:use-module (netlink nl80211)
  #:use-module (netlink nl80211 attrs)
  #:use-module ((ip link) #:select (link-name->index link-index->name))
  #:use-module ((ip utils) #:select (get-attr answer-ok?))
  #:use-module (netlink route attrs)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (get-station print-station))

(define (get-station interface bssid)
  (define request-num (random 65535))
  (let* ((ifindex (link-name->index interface))
         (sock (connect-generic))
         (message (make-message
                   %nl80211-family-id
                   (logior NLM_F_REQUEST NLM_F_ACK NLM_F_DUMP)
                   request-num
                   0
                   (make-generic-message
                    NL80211_CMD_GET_STATION
                    1
                    0
                    (list (make-route-attr
                           NL80211_ATTR_IFINDEX
                           (make-u32-route-attr ifindex))
                          (make-route-attr
                           NL80211_ATTR_MAC
                           (make-ethernet-route-attr bssid)))))))
    (send-msg message sock)
    (let ((answer (receive-and-decode-msg sock %default-nl80211-decoder)))
      (close-port sock)
      (answer-ok? (last answer))
      answer)))

(define (format-chain-signal chain)
  (let ((signals (map (lambda (signal)
                        (nl-data-data (route-attr-data signal)))
                      chain)))
    (format #f "[~a] " (string-join (map number->string signals) ", "))))

(define (format-bitrate bitrate)
  (string-join
   (filter string?
           (list
            (let ((rate (or (get-attr bitrate NL80211_RATE_INFO_BITRATE32)
                            (get-attr bitrate NL80211_RATE_INFO_BITRATE))))
              (format #f "~a MBit/s"
                      (if (> rate 0)
                          (format #f "~f" (/ rate 10))
                          "(unknown)")))
            (and=> (get-attr bitrate NL80211_RATE_INFO_MCS)
                   (lambda (res) (format #f "MCS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_VHT_MCS)
                   (lambda (res) (format #f "VHT-MCS ~d" res)))
            (when (get-attr bitrate NL80211_RATE_INFO_40_MHZ_WIDTH)
              (format #f "40MHz"))
            (when (get-attr bitrate NL80211_RATE_INFO_80_MHZ_WIDTH)
              (format #f "80MHz"))
            (when (get-attr bitrate NL80211_RATE_INFO_80P80_MHZ_WIDTH)
              (format #f "80P80MHz"))
            (when (get-attr bitrate NL80211_RATE_INFO_160_MHZ_WIDTH)
              (format #f "160MHz"))
            (when (get-attr bitrate NL80211_RATE_INFO_320_MHZ_WIDTH)
              (format #f "320MHz"))
            (when (get-attr bitrate NL80211_RATE_INFO_SHORT_GI)
              (format #f "short GI"))
            (and=> (get-attr bitrate NL80211_RATE_INFO_VHT_NSS)
                   (lambda (res) (format #f "VHT-NSS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_HE_MCS)
                   (lambda (res) (format #f "HE-MCS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_HE_NSS)
                   (lambda (res) (format #f "HE-MCS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_HE_GI)
                   (lambda (res) (format #f "HE-GI ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_HE_DCM)
                   (lambda (res) (format #f "HE-DCM ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_RU_ALLOC)
                   (lambda (res) (format #f "HE-RU-ALLOC ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_EHT_MCS)
                   (lambda (res) (format #f "EHT-MCS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_EHT_NSS)
                   (lambda (res) (format #f "EHT-NSS ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_EHT_GI)
                   (lambda (res) (format #f "EHT-GI ~d" res)))
            (and=> (get-attr bitrate NL80211_RATE_INFO_EHT_RU_ALLOC)
                   (lambda (res) (format #f "EHT-RU-ALLOC ~d" res)))))
   " "))

(define (print-sta-info sta-info)
  (let ((inactive-time (get-attr sta-info NL80211_STA_INFO_INACTIVE_TIME))
        (rx-bytes (or (get-attr sta-info NL80211_STA_INFO_RX_BYTES64)
                      (get-attr sta-info NL80211_STA_INFO_RX_BYTES)))
        (rx-packets (get-attr sta-info NL80211_STA_INFO_RX_PACKETS))
        (tx-bytes (or (get-attr sta-info NL80211_STA_INFO_TX_BYTES64)
                      (get-attr sta-info NL80211_STA_INFO_TX_BYTES)))
        (tx-packets (get-attr sta-info NL80211_STA_INFO_TX_PACKETS))
        (tx-retries (get-attr sta-info NL80211_STA_INFO_TX_RETRIES))
        (tx-failed (get-attr sta-info NL80211_STA_INFO_TX_FAILED))
        (beacon-loss (get-attr sta-info NL80211_STA_INFO_BEACON_LOSS))
        (beacon-rx (get-attr sta-info NL80211_STA_INFO_BEACON_RX))
        (rx-drop-misc (get-attr sta-info NL80211_STA_INFO_RX_DROP_MISC))
        (signal (get-attr sta-info NL80211_STA_INFO_SIGNAL))
        (signal-avg (get-attr sta-info NL80211_STA_INFO_SIGNAL_AVG))
        (beacon-signal-avg (get-attr sta-info NL80211_STA_INFO_BEACON_SIGNAL_AVG))
        (tx-bitrate (get-attr sta-info NL80211_STA_INFO_TX_BITRATE))
        (tx-duration (get-attr sta-info NL80211_STA_INFO_TX_DURATION))
        (rx-bitrate (get-attr sta-info NL80211_STA_INFO_RX_BITRATE))
        (rx-duration (get-attr sta-info NL80211_STA_INFO_RX_DURATION))
        (ack-signal (get-attr sta-info NL80211_STA_INFO_ACK_SIGNAL))
        (ack-signal-avg (get-attr sta-info NL80211_STA_INFO_ACK_SIGNAL_AVG))
        (sta-flags (get-attr sta-info NL80211_STA_INFO_STA_FLAGS))
        (bss-param (get-attr sta-info NL80211_STA_INFO_BSS_PARAM))
        (connected-time (get-attr sta-info NL80211_STA_INFO_CONNECTED_TIME))
        (assoc-at-boottime (get-attr sta-info NL80211_STA_INFO_ASSOC_AT_BOOTTIME)))
    (when inactive-time
      (format #t "\tinactive time:\t~d ms~%" inactive-time))
    (when rx-bytes
      (format #t "\trx bytes:\t~d~%" rx-bytes))
    (when rx-packets
      (format #t "\trx packets:\t~d~%" rx-packets))
    (when tx-bytes
      (format #t "\ttx bytes:\t~d~%" tx-bytes))
    (when tx-packets
      (format #t "\ttx packets:\t~d~%" tx-packets))
    (when tx-retries
      (format #t "\ttx retries:\t~d~%" tx-retries))
    (when tx-failed
      (format #t "\ttx failed:\t~d~%" tx-failed))
    (when beacon-loss
      (format #t "\tbeacon loss:\t~d~%" beacon-loss))
    (when beacon-rx
      (format #t "\tbeacon rx:\t~d~%" beacon-rx))
    (when rx-drop-misc
      (format #t "\trx drop misc:\t~d~%" rx-drop-misc))
    (when signal
      (let ((chain-signal
             (get-attr sta-info NL80211_STA_INFO_CHAIN_SIGNAL)))
        (format #t "\tsignal:  \t~d ~adBm~%"
                signal
                (if chain-signal
                    (format-chain-signal chain-signal)
                    ""))))
    (when signal-avg
      (let ((chain-signal-avg
             (get-attr sta-info NL80211_STA_INFO_CHAIN_SIGNAL_AVG)))
        (format #t "\tsignal avg:\t~d ~adBm~%"
                signal-avg
                (if chain-signal-avg
                    (format-chain-signal chain-signal-avg)
                    ""))))
    (when beacon-signal-avg
      (format #t "\tbeacon signal avg:\t~d dBm~%"
              beacon-signal-avg))
    ;; TODO: NL80211_STA_INFO_T_OFFSET
    (when tx-bitrate
      (format #t "\ttx bitrate:\t~a~%"
              (format-bitrate tx-bitrate)))
    (when tx-duration
      (format #t "\ttx duration:\t~d us~%" tx-duration))
    (when rx-bitrate
      (format #t "\trx bitrate:\t~a~%"
              (format-bitrate rx-bitrate)))
    (when rx-duration
      (format #t "\trx duration:\t~d us~%" rx-duration))
    (when ack-signal
      (format #t "\tlast ack signal:\t~d dBm~%" ack-signal))
    (when ack-signal-avg
      (format #t "\tavg ack signal:\t~d dBm~%" ack-signal-avg))
    ;; TODO: mesh
    (when sta-flags
      (let ((mask (nl80211-sta-flag-update-attr-type-mask sta-flags))
            (set (nl80211-sta-flag-update-attr-type-set sta-flags))
            (authorized (ash 1 NL80211_STA_FLAG_AUTHORIZED))
            (authenticated (ash 1 NL80211_STA_FLAG_AUTHENTICATED))
            (associated (ash 1 NL80211_STA_FLAG_ASSOCIATED))
            (preamble (ash 1 NL80211_STA_FLAG_SHORT_PREAMBLE))
            (wme (ash 1 NL80211_STA_FLAG_WME))
            (mfp (ash 1 NL80211_STA_FLAG_MFP))
            (tdls-peer (ash 1 NL80211_STA_FLAG_TDLS_PEER)))
        (unless (zero? (logand mask authorized))
          (format #t "\tauthorized:\t~a~%"
                  (if (zero? (logand set authorized))
                      "no" "yes")))
        (unless (zero? (logand mask authenticated))
          (format #t "\tauthenticated:\t~a~%"
                  (if (zero? (logand set authenticated))
                      "no" "yes")))
        (unless (zero? (logand mask associated))
          (format #t "\tassociated:\t~a~%"
                  (if (zero? (logand set associated))
                      "no" "yes")))
        (unless (zero? (logand mask preamble))
          (format #t "\tpreamble:\t~a~%"
                  (if (zero? (logand set preamble))
                      "long" "short")))
        (unless (zero? (logand mask wme))
          (format #t "\tWMM/WME:\t~a~%"
                  (if (zero? (logand set wme))
                      "no" "yes")))
        (unless (zero? (logand mask mfp))
          (format #t "\tMFP:\t\t~a~%"
                  (if (zero? (logand set mfp))
                      "no" "yes")))
        (unless (zero? (logand mask tdls-peer))
          (format #t "\tTDLS peer:\t~a~%"
                  (if (zero? (logand set tdls-peer))
                      "no" "yes")))))
    (when bss-param
      (let ((dtim-period
             (get-attr bss-param NL80211_STA_BSS_PARAM_DTIM_PERIOD))
            (beacon-interval
             (get-attr bss-param
                       NL80211_STA_BSS_PARAM_BEACON_INTERVAL)))
        ;; TODO: flag attributes
        (when dtim-period
          (format #t "\tDTIM period:\t~d~%" dtim-period))
        (when beacon-interval
          (format #t "\tbeacon-interval:~d~%" beacon-interval))))
    (when connected-time
      (format #t "\tconnected time:\t~d seconds~%"
              connected-time))
    (when assoc-at-boottime
      (let ((now (current-time)))
        (format #t "\tassociated at [boottime]: ~1,3fs~%"
                (/ assoc-at-boottime 1e9))
        ;; TODO: Add "associated at:"?  Needs to get system uptime and who uses
        ;; this anyway.
        (format #t "\tcurrent time:\t~d ms~%"
                (* now 1e3))))))

;; The output format is identical with "iw INTERFACE station dump".
(define (print-station station)
  (for-each (lambda (message)
              (when (memv (message-kind message) (list NL80211_CMD_GET_SCAN))
                (let* ((data (message-data message))
                       (attrs (generic-message-attrs data))
                       (sta-info (get-attr attrs NL80211_ATTR_STA_INFO)))
                  (if sta-info
                      (let* ((mac (get-attr attrs NL80211_ATTR_MAC))
                             (ifindex (get-attr attrs NL80211_ATTR_IFINDEX))
                             (ifname (link-index->name ifindex)))
                        (format #t "Station ~a (on ~a)~%" mac ifname)
                        (print-sta-info sta-info))
                      (format (current-error-port) "sta stats missing!~%")))))
            station))
