;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (netlink deserialize)
  #:use-module (netlink constant)
  #:use-module (netlink generic)
  #:use-module ((netlink route addr) #:prefix route:)
  #:use-module ((netlink route attrs) #:prefix route:)
  #:use-module ((netlink route link) #:prefix route:)
  #:use-module ((netlink route route) #:prefix route:)
  #:use-module ((netlink nl80211 attrs) #:prefix nl80211:)
  #:use-module (netlink standard)
  #:use-module (netlink message)
  #:export (%default-message-decoder
            %default-route-decoder
            %default-control-decoder
            %default-nl80211-decoder))

(define %default-message-decoder
  `((,NLMSG_NOOP . ,deserialize-no-data)
    (,NLMSG_ERROR . ,deserialize-error-message)
    (,NLMSG_DONE . ,deserialize-no-data)))

;; for RTNETLINK
(define %default-route-decoder
  `((message ,deserialize-message
      ,@%default-message-decoder
      (,RTM_NEWLINK . ,route:deserialize-link-message)
      (,RTM_DELLINK . ,route:deserialize-link-message)
      (,RTM_GETLINK . ,route:deserialize-link-message)
      (,RTM_SETLINK . ,route:deserialize-link-message)
      (,RTM_NEWADDR . ,route:deserialize-addr-message)
      (,RTM_DELADDR . ,route:deserialize-addr-message)
      (,RTM_GETADDR . ,route:deserialize-addr-message)
      (,RTM_NEWROUTE . ,route:deserialize-route-message)
      (,RTM_DELROUTE . ,route:deserialize-route-message)
      (,RTM_GETROUTE . ,route:deserialize-route-message))
    (message-hdr ,deserialize-message-header '())
    (link-attr ,(route:deserialize-route-attr 'link-attr)
               ,@route:%default-route-link-attr-decoder)
    (ipv4-addr-attr ,(route:deserialize-route-attr 'ipv4-addr-attr)
                    ,@route:%default-route-addr-ipv4-attr-decoder)
    (ipv6-addr-attr ,(route:deserialize-route-attr 'ipv6-addr-attr)
                    ,@route:%default-route-addr-ipv6-attr-decoder)
    (ipv4-route-attr ,(route:deserialize-route-attr 'ipv4-route-attr)
                     ,@route:%default-route-route-ipv4-attr-decoder)
    (ipv6-route-attr ,(route:deserialize-route-attr 'ipv6-route-attr)
                     ,@route:%default-route-route-ipv6-attr-decoder)
    (linkinfo-attr ,(route:deserialize-route-attr 'linkinfo-attr)
                   ,@route:%default-route-link-info-attr-decoder)))

(define %default-control-attr-decoder
  `((,CTRL_ATTR_FAMILY_NAME . ,route:deserialize-route-attr-data-string)
    (,CTRL_ATTR_FAMILY_ID . ,route:deserialize-route-attr-data-u16)
    (,CTRL_ATTR_VERSION . ,route:deserialize-route-attr-data-u32)
    (default . ,route:deserialize-route-attr-data-bv)))

(define %default-control-decoder
  `((message ,deserialize-message
             ,@%default-message-decoder
             (,NLMSG_MIN_TYPE . ,deserialize-generic-message))
    (message-hdr ,deserialize-message-header '())
    (nlattr ,(route:deserialize-route-attr 'nlattr)
            ,@%default-control-attr-decoder)))

(define %default-nl80211-decoder
  `((message ,deserialize-message
             ,@%default-message-decoder
             ;; XXX where is nlmsg_type nl80211 == 32 defined?
             ;;   genl_family nl80211_fam does not have a .id field.
             ;; Need deserialize-generic-netlink-header?
             (,32 . ,deserialize-generic-message)
             (,NL80211_CMD_GET_STATION . ,deserialize-generic-message)
             (,NL80211_CMD_NEW_SCAN_RESULTS . ,deserialize-generic-message)
             (,NL80211_CMD_SCAN_ABORTED . ,deserialize-generic-message))
    (message-hdr ,deserialize-message-header '())
    (sta-info-attr ,(route:deserialize-route-attr 'sta-info-attr)
                   ,@nl80211:%default-sta-info-attr-decoder)
    (chain-signal-attr ,(route:deserialize-route-attr 'chain-signal-attr)
                       ,@nl80211:%default-chain-signal-attr-decoder)
    (rate-info-attr ,(route:deserialize-route-attr 'rate-info-attr)
                    ,@nl80211:%default-rate-info-attr-decoder)
    (sta-bss-param-attr ,(route:deserialize-route-attr 'sta-bss-param-attr)
                        ,@nl80211:%default-sta-bss-param-attr-decoder)
    (bss-attr ,(route:deserialize-route-attr 'bss-attr)
              ,@nl80211:%default-bss-attr-decoder)
    (nlattr ,(route:deserialize-route-attr 'nlattr)
            ,@nl80211:%default-nl80211-attr-decoder)))
