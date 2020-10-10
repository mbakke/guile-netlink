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

(define-module (netlink constant)
               #:export (define-enum))

(define-syntax define-enum-aux
  (syntax-rules ()
    ((_ num (name n))
     (define-public name n))
    ((_ num name)
     (define-public name num))
    ((_ num (name n) rest ...)
     (begin
       (define-public name n)
       (define-enum-aux (+ n 1) rest ...)))
    ((_ num name rest ...)
     (begin
       (define-public name num)
       (define-enum-aux (+ num 1) rest ...)))))

(define-syntax define-enum
  (lambda (x)
    (define (spec-names specs)
      (map
        (lambda (spec)
          (syntax-case spec ()
            ((name val) #'name)
            (name #'name)))
        specs))

    (define (getter num name)
      #`((= #,num #,name) (quote #,name)))

    (syntax-case x ()
      ((_ integer->symbol name-spec ...)
       #`(begin
           (define-enum-aux 0 name-spec ...)
           (define-public (integer->symbol num)
             (cond
               #,@(map (lambda (s) (getter #'num s)) (spec-names #'(name-spec ...))))))))))

(define-enum int->attr-kind
             IFLA_UNSPEC IFLA_ADDRESS IFLA_BROADCAST IFLA_IFNAME IFLA_MTU
             IFLA_LINK IFLA_QDISC IFLA_STATS IFLA_COST IFLA_PRIORITY
             IFLA_MASTER IFLA_WIRELESS IFLA_PROTIFO IFLA_TXQLEN IFLA_MAP
             IFLA_WEIGHT IFLA_OPERSTATE IFLA_LINKMODE IFLA_LINKINFO
             IFLA_NET_NS_PID IFLA_IFALIAS IFLA_NUM_VF IFLA_VFINFO_LIST
             IFLA_STATS64 IFLA_VF_PORTS IFLA_PORT_SELF IFLA_AF_SPEC
             IFLA_GROUP IFLA_NET_NS_FD IFLA_EXT_MASK IFLA_PROMISCUITY
             IFLA_NUM_TX_QUEUES IFLA_NUM_RX_QUEUES IFLA_CARRIER
             IFLA_PHYS_PORT_ID IFLA_CARRIER_CHANGES IFLA_PHYS_SWITCH_ID
             IFLA_LINK_NETNSID IFLA_PHYS_PORT_NAME IFLA_PROTO_DOWN
             IFLA_GSO_MAX_SEGS IFLA_GSO_MAX_SIZE IFLA_PAD IFLA_XDP
             IFLA_EVENT IFLA_NEW_NETNSID IFLA_IF_NETNSID IFLA_CARRIER_UP_COUNT
             IFLA_CARRIER_DOWN_COUNT IFLA_NEW_IFINDEX IFLA_MIN_MTU IFLA_MAX_MTU
             IFLA_PROP_LIST IFLA_ALT_IFNAME IFLA_PERM_ADDRESS)
(define-public IFLA_TARGET_NETNSID IFLA_IF_NETNSID)

(define-public AF_NETLINK 16)
(define-public AF_PACKET 17)

(define-enum int->protocol
             NETLINK_ROUTE NETLINK_UNUSED NETLINK_USERSOCK NETLINK_FIREWALL
             NETLINK_SOCK_DIAG NETLINK_NFLOG NETLINK_XFRM NETLINK_SELINUX
             NETLINK_ISCSI NETLINK_AUDIT NETLINK_FIB_LOOKUP NETLINK_CONNECTOR
             NETLINK_NETFILTER NETLINK_IP6_FW NETLINK_DNRTMSG NETLINK_KOBJECT_UEVENT
             NETLINK_GENERIC NETLINK_DM NETLINK_SCSITRANSPORT NETLINK_ECRYPTFS
             NETLINK_RDMA NETLINK_CRYPTO NETLINK_SMC)
(define-public NETLINK_INET_DIAG NETLINK_SOCK_DIAG)

(define-enum int->message-kind
  (NLMSG_NOOP 1)
  NLMSG_ERROR
  NLMSG_DONE
  NLMSG_OVERUN
  (RTM_NEWLINK 16)
  RTM_DELLINK
  RTM_GETLINK
  RTM_SETLINK

  (RTM_NEWADDR 20)
  RTM_DELADDR
  RTM_GETADDR)

(define-public NLM_F_REQUEST #x01)
(define-public NLM_F_MULTI #x02)
(define-public NLM_F_ACK #x04)
(define-public NLM_F_ECHO #x08)
(define-public NLM_F_DUMP_INTR #x10)
(define-public NLM_F_DUMP_FILTERED #x20)

;; modifiers to GET requests
(define-public NLM_F_ROOT #x100)
(define-public NLM_F_MATCH #x200)
(define-public NLM_F_ATOMIC #x400)
(define-public NLM_F_DUMP (logior NLM_F_ROOT NLM_F_MATCH))

;; modifiers to NEW requests
(define-public NLM_F_REPLACE #x100)
(define-public NLM_F_EXCL #x200)
(define-public NLM_F_CREATE #x400)
(define-public NLM_F_APPEND #x800)

(define-public NLM_F_NONREC #x100)

(define-public NLM_F_CAPPED #x100)
(define-public NLM_F_ACK_TLVS #x200)

;; operstate
(define-enum int->operstate
             IF_OPER_UNKNOWN IF_OPER_NOTPRESENT IF_OPER_DOWN
             IF_OPER_LOWERLAYERDOWN IF_OPER_TESTING IF_OPER_DORMANT
             IF_OPER_UP)
