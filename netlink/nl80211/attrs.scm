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

(define-module (netlink nl80211 attrs)
  #:use-module (netlink constant)
  #:use-module (netlink data)
  #:use-module ((netlink route attrs) #:prefix route:)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (make-nl80211-sta-flag-update-attr
            nl80211-sta-flag-update-attr?
            nl80211-sta-flag-update-attr-type-mask
            nl80211-sta-flag-update-attr-type-set
            deserialize-nl80211-sta-flag-update-attr
            %default-nl80211-attr-decoder
            %default-bss-attr-decoder
            %default-sta-info-attr-decoder
            %default-sta-bss-param-attr-decoder
            %default-chain-signal-attr-decoder
            %default-rate-info-attr-decoder))

(define-data-type nl80211-sta-flag-update-attr
  (const 8)
  (lambda (attr pos bv)
    (match attr
      (($ nl80211-sta-flag-update-attr-type mask set)
       (bytevector-u32-set! bv pos mask (native-endianness))
       (bytevector-u32-set! bv (+ pos 4) set (native-endianness)))))
  (mask nl80211-sta-flag-update-attr-mask nl80211-sta-flag-update-attr-type-mask)
  (set nl80211-sta-flag-update-attr-set nl80211-sta-flag-update-attr-type-set))

(define (deserialize-nl80211-sta-flag-update-attr decoder bv pos)
  (make-nl80211-sta-flag-update-attr
    (bytevector-u32-ref bv pos (native-endianness))
    (bytevector-u32-ref bv (+ pos 4) (native-endianness))))

(define %default-nl80211-attr-decoder
  `((,NL80211_ATTR_WIPHY . ,route:deserialize-route-attr-data-u32)
    (,NL80211_ATTR_WIPHY_NAME . ,route:deserialize-route-attr-data-string)
    (,NL80211_ATTR_IFINDEX . ,route:deserialize-route-attr-data-u32)
    (,NL80211_ATTR_IFNAME . ,route:deserialize-route-attr-data-string)
    (,NL80211_ATTR_MAC . ,route:deserialize-route-attr-data-ethernet)
    (,NL80211_ATTR_KEY_IDX . ,route:deserialize-route-attr-data-u8)
    (,NL80211_ATTR_STA_INFO . ,(route:deserialize-route-attr-data-nested
                                'sta-info-attr))
    (,NL80211_ATTR_GENERATION . ,route:deserialize-route-attr-data-u32)
    (,NL80211_ATTR_BSS . ,(route:deserialize-route-attr-data-nested 'bss-attr))
    (,NL80211_ATTR_BG_SCAN_PERIOD . ,route:deserialize-route-attr-data-u16)
    (,NL80211_ATTR_WDEV . ,route:deserialize-route-attr-data-u64)
    (default . ,route:deserialize-route-attr-data-bv)))

(define %default-bss-attr-decoder
  `((,NL80211_BSS_BSSID . ,route:deserialize-route-attr-data-ethernet)
    (,NL80211_BSS_FREQUENCY . ,route:deserialize-route-attr-data-u32)
    (,NL80211_BSS_TSF . ,route:deserialize-route-attr-data-u64)
    (,NL80211_BSS_BEACON_INTERVAL . ,route:deserialize-route-attr-data-u16)
    (,NL80211_BSS_CAPABILITY . ,route:deserialize-route-attr-data-u16)
    (,NL80211_BSS_SIGNAL_MBM . ,route:deserialize-route-attr-data-s32)
    (,NL80211_BSS_SIGNAL_UNSPEC . ,route:deserialize-route-attr-data-s8)
    (,NL80211_BSS_STATUS . ,route:deserialize-route-attr-data-u32)
    (,NL80211_BSS_SEEN_MS_AGO . ,route:deserialize-route-attr-data-u32)
    (,NL80211_BSS_CHAN_WIDTH . ,route:deserialize-route-attr-data-u32)
    (,NL80211_BSS_BEACON_TSF . ,route:deserialize-route-attr-data-u64)
    (,NL80211_BSS_LAST_SEEN_BOOTTIME . ,route:deserialize-route-attr-data-u64)
    (,NL80211_BSS_PARENT_TSF . ,route:deserialize-route-attr-data-u64)
    (,NL80211_BSS_PARENT_BSSID . ,route:deserialize-route-attr-data-ethernet)
    (,NL80211_BSS_CHAIN_SIGNAL . ,(route:deserialize-route-attr-data-nested
                                   'chain-signal-attr))
    (,NL80211_BSS_FREQUENCY_OFFSET . ,route:deserialize-route-attr-data-u32)
    (,NL80211_BSS_MLO_LINK_ID . ,route:deserialize-route-attr-data-u8)
    (,NL80211_BSS_MLD_ADDR . ,route:deserialize-route-attr-data-ethernet)
    (default . ,route:deserialize-route-attr-data-bv)))

(define %default-sta-info-attr-decoder
  `((,NL80211_STA_INFO_INACTIVE_TIME . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_RX_BYTES . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_TX_BYTES . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_LLID . ,route:deserialize-route-attr-data-u16)
    (,NL80211_STA_INFO_PLID . ,route:deserialize-route-attr-data-u16)
    (,NL80211_STA_INFO_RX_BYTES64 . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_TX_BYTES64 . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_SIGNAL . ,route:deserialize-route-attr-data-s8)
    (,NL80211_STA_INFO_TX_BITRATE . ,(route:deserialize-route-attr-data-nested
                                      'rate-info-attr))
    (,NL80211_STA_INFO_RX_PACKETS . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_TX_PACKETS . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_TX_RETRIES . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_TX_FAILED . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_SIGNAL_AVG . ,route:deserialize-route-attr-data-s8)
    (,NL80211_STA_INFO_RX_BITRATE . ,(route:deserialize-route-attr-data-nested
                                      'rate-info-attr))
    ;; TODO
    ;;(,NL80211_STA_INFO_PLINK_STATE . ,(route:deserialize-route-attr-data-nested
    ;;                                   'nl80211-plink-state))
    (,NL80211_STA_INFO_BSS_PARAM . ,(route:deserialize-route-attr-data-nested
                                     'sta-bss-param-attr))
    (,NL80211_STA_INFO_CONNECTED_TIME . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_STA_FLAGS . ,deserialize-nl80211-sta-flag-update-attr)
    (,NL80211_STA_INFO_BEACON_LOSS . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_T_OFFSET . ,route:deserialize-route-attr-data-s64)
    ;; TODO nl80211_mesh_power_mode
    ;;(,NL80211_STA_INFO_LOCAL_PM . ,route:deserialize-route-attr-data-s64)
    ;;(,NL80211_STA_INFO_PEER_PM . ,route:deserialize-route-attr-data-s64)
    ;;(,NL80211_STA_INFO_NONPEER_PM . ,route:deserialize-route-attr-data-s64)
    (,NL80211_STA_INFO_CHAIN_SIGNAL . ,(route:deserialize-route-attr-data-nested
                                        'chain-signal-attr))
    (,NL80211_STA_INFO_CHAIN_SIGNAL_AVG . ,(route:deserialize-route-attr-data-nested
                                            'chain-signal-attr))
    (,NL80211_STA_INFO_EXPECTED_THROUGHPUT . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_RX_DROP_MISC . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_BEACON_RX . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_BEACON_SIGNAL_AVG . ,route:deserialize-route-attr-data-s8)
    ;; cfg80211_tid_stats
    ;(,NL80211_STA_INFO_TID_STATS . ,(route:deserialize-route-attr-data-nested
    ;                                    'nl80211-tid-stats))
    (,NL80211_STA_INFO_RX_DURATION . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_ACK_SIGNAL . ,route:deserialize-route-attr-data-s8)
    (,NL80211_STA_INFO_ACK_SIGNAL_AVG . ,route:deserialize-route-attr-data-s8)
    (,NL80211_STA_INFO_RX_MPDUS . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_FCS_ERROR_COUNT . ,route:deserialize-route-attr-data-u32)
    (,NL80211_STA_INFO_CONNECTED_TO_GATE . ,route:deserialize-route-attr-data-u8)
    (,NL80211_STA_INFO_TX_DURATION . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_AIRTIME_WEIGHT . ,route:deserialize-route-attr-data-u16)
    (,NL80211_STA_INFO_AIRTIME_LINK_METRIC . ,route:deserialize-route-attr-data-u32)

    (,NL80211_STA_INFO_ASSOC_AT_BOOTTIME . ,route:deserialize-route-attr-data-u64)
    (,NL80211_STA_INFO_CONNECTED_TO_AS . ,route:deserialize-route-attr-data-u8)
    (default . ,route:deserialize-route-attr-data-bv)))

(define %default-sta-bss-param-attr-decoder
  `(;; TODO: How to deal with NLA_FLAG / nla_put_flag ?
    ;;(,NL80211_STA_BSS_PARAM_CTS_PROT . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_STA_BSS_PARAM_SHORT_PREAMBLE . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_STA_BSS_PARAM_SHORT_SLOT_TIME . ,route:deserialize-route-attr-data-flag)
    (,NL80211_STA_BSS_PARAM_DTIM_PERIOD . ,route:deserialize-route-attr-data-u8)
    (,NL80211_STA_BSS_PARAM_BEACON_INTERVAL . ,route:deserialize-route-attr-data-u16)
    (default . ,route:deserialize-route-attr-data-bv)))

(define %default-chain-signal-attr-decoder
  `((default . ,route:deserialize-route-attr-data-s8)))

(define %default-rate-info-attr-decoder
  `((,NL80211_RATE_INFO_BITRATE . ,route:deserialize-route-attr-data-u16)
    (,NL80211_RATE_INFO_MCS . ,route:deserialize-route-attr-data-u8)
    ;; TODO: nla_put_flag
    ;;(,NL80211_RATE_INFO_40_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_RATE_INFO_SHORT_GI . ,route:deserialize-route-attr-data-flag)
    (,NL80211_RATE_INFO_BITRATE32 . ,route:deserialize-route-attr-data-u32)
    ;;(,NL80211_RATE_INFO_MAX . ,route:deserialize-route-attr-data-flag)
    (,NL80211_RATE_INFO_VHT_MCS . ,route:deserialize-route-attr-data-u8)
    (,NL80211_RATE_INFO_VHT_NSS . ,route:deserialize-route-attr-data-u8)
    ;;(,NL80211_RATE_INFO_80_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_RATE_INFO_80P80_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_RATE_INFO_160_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_RATE_INFO_10_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    ;;(,NL80211_RATE_INFO_5_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    (,NL80211_RATE_INFO_HE_MCS . ,route:deserialize-route-attr-data-u8)
    (,NL80211_RATE_INFO_HE_NSS . ,route:deserialize-route-attr-data-u8)
    ;; TODO nl80211_he_gi
    ;;(,NL80211_RATE_INFO_HE_GI . ,route:deserialize-route-attr-data-nested)
    (,NL80211_RATE_INFO_HE_DCM . ,route:deserialize-route-attr-data-u8)
    ;; TODO nl80211_he_ru_alloc
    ;;(,NL80211_RATE_INFO_RU_ALLOC . ,route:deserialize-route-attr-data-nested)
    ;;(,NL80211_RATE_INFO_320_MHZ_WIDTH . ,route:deserialize-route-attr-data-flag)
    (,NL80211_RATE_INFO_EHT_MCS . ,route:deserialize-route-attr-data-u8)
    (,NL80211_RATE_INFO_EHT_NSS . ,route:deserialize-route-attr-data-u8)
    ;; TODO nl80211_eht_gi
    ;; (,NL80211_RATE_INFO_EHT_GI . ,route:deserialize-route-attr-data-nested)
    ;; TODO nl80211_eht_ru_alloc
    ;;(,NL80211_RATE_INFO_EHT_RU_ALLOC . ,route:deserialize-route-attr-data-nested)
    (default . ,route:deserialize-route-attr-data-bv)))
