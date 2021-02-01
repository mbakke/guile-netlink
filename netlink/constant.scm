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

(define-enum int->link-attr-kind
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

(define-enum int->addr-attr-kind
  IFA_UNSPEC IFA_ADDRESS IFA_LOCAL IFA_LABEL IFA_BROADCAST
  IFA_ANYCAST IFA_CACHEINFO IFA_MULTICAST IFA_FLAGS
  IFA_RT_PRIORITY IFA_TARGET_NETNSID)

(define-enum int->route-attr-kind
  RTA_UNSPEC RTA_DST RTA_SRC RTA_IIF RTA_OIF RTA_GATEWAY
  RTA_PRIORITY RTA_PREFSRC RTA_METRICS RTA_MULTIPATH
  RTA_PROTOINFO RTA_FLOW RTA_CACHEINFO RTA_SESSION RTA_MP_ALGO
  RTA_TABLE RTA_MARK RTA_MFC_STATS RTA_VIA RTA_NEWDST RTA_PREF
  RTA_ENCAP_TYPE RTA_ENCAP RTA_EXPIRES RTA_PAD RTA_UID
  RTA_TTL_PROPAGATE RTA_IP_PROTO RTA_SPORT RTA_DPORT RTA_NH_ID)

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
  RTM_GETADDR
  
  (RTM_NEWROUTE 24)
  RTM_DELROUTE
  RTM_GETROUTE

  (RTM_NEWNEIGH 28)
  RTM_DELNEIGH
  RTML_GETNEIGH

  (RTM_NEWRULE 32)
  RTM_DELRULE
  RTM_GETRULE

  (RTM_NEWQDISC 36)
  RTM_DELQDISC
  RTM_GETQDISC

  (RTM_NEWTCLASS 40)
  RTM_DELTCLASS
  RTM_GETTCLASS

  (RTM_NEWTFILTER 44)
  RTM_DELTFILTER
  RTM_GETTFILTER

  (RTM_NEWACTION 48)
  RTM_DELACTION
  RTM_GETACTION

  (RTM_NEPREFIX 52)
  (RTM_GETMULTICAST 58)
  (RTM_GETANYCAST 62)
  (RTM_NEWNEIGHTBL 64)
  (RTM_GETNEIGHTBL 66)
  RTM_SETNEIGHTBL

  (RTM_NEWNDUSEROPT 68)
  (RTM_NEWADDRLABEL 72)
  RTM_DELADDRLABEL
  RTM_GETADDRLABEL

  (RTM_GETDCB 78)
  RTM_SETDCB

  (RTM_NEWNETCONF 80)
  RTM_DELNETCONF
  RTM_GETNETCONF

  (RTM_NEWMDB 84)
  RTM_DELMDB
  RTM_GETMDB

  (RTM_NEWNSID 88)
  RTM_DELNSID
  RTM_GETNSID

  (RTM_NEWSTATS 92)
  (RTM_GETSTATS 94)

  (RTM_NEWCACHEREPORT 92)

  (RTM_NEWCHAIN 100)
  RTM_DELCHAIN
  RTM_GETCHAIN

  (RTM_NEWNEXTHOP 104)
  RTM_DELNEXTHOP
  RTM_GETNEXTHOP

  (RTM_NEWLINKPROP 108)
  RTM_DELLINKPROP
  RTM_GETLINKPROP

  (RTM_NEWVLAN 112)
  RTM_DELVLAN
  RTM_GETVLAN)

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

;; net_device_flags
(define-enum int->device-flags
  (IFF_UP 1)
  (IFF_BROADCAST 2)
  (IFF_DEBUG 4)
  (IFF_LOOPBACK 8)
  (IFF_POINTOPOINT 16)
  (IFF_NOTRAILERS 32)
  (IFF_RUNNING 64)
  (IFF_NOARP 128)
  (IFF_PROMISC 256)
  (IFF_ALLMULTI 512)
  (IFF_MASTER 1024)
  (IFF_SLAVE 2048)
  (IFF_MULTICAST 4096)
  (IFF_PORTSEL 8192)
  (IFF_AUTOMEDIA 16384)
  (IFF_DYNAMIC 32768)
  (IFF_LOWER_UP 65536)
  (IFF_DORMANT 131072)
  (IFF_ECHO 262144))

;; operstate
(define-enum int->operstate
  IF_OPER_UNKNOWN IF_OPER_NOTPRESENT IF_OPER_DOWN
  IF_OPER_LOWERLAYERDOWN IF_OPER_TESTING IF_OPER_DORMANT
  IF_OPER_UP)

;; rtm_type
(define-enum int->rtm-type
  RTN_UNSPEC RTN_UNICAST RTN_LOCAL RTN_BROADCAST RTN_ANYCAST
  RTN_MULTICAST RTN_BLACKHOLE RTN_UNREACHABLE RTN_PROHIBIT
  RTN_THROW RTN_NAT RTN_XRESOLVE)

;; rtm_protocol
(define-enum int->rtm-protocol
  RTPROT_UNSPEC RTPROT_REDIRECT RTPROT_KERNEL RTPROT_BOOT RTPROT_STATIC
  ;; not interpreted by the kernel, but returned anyway.
  (RTPROT_GATED 8)
  RTPROT_RA RTPROT_MRT RTPROT_ZEBRA RTPROT_BIRD RTPROT_DNROUTED
  RTPROT_XORP RTPROT_NTK RTPROT_DHCP RTPROT_MROUTED
  (RTPROT_BABEL 42)
  (RTPROT_BGP 186)
  RTPROT_ISIS RTPROT_OSPF RTPROT_RIP
  (RTPROT_EIGRP 192))

;; rtm_scope
(define-enum int->rtm-scope
  RT_SCOPE_UNIVERSE
  (RT_SCOPE_SITE 200)
  (RT_SCOPE_LINK 253)
  (RT_SCOPE_HOST 254)
  (RT_SCOPE_NOWHERE 255))

;; rtm_flags
(define-enum int->rtm-flag
  (RTM_F_NOTIFY #x100)
  (RTM_F_CLONED #x200)
  (RTM_F_EQUALIZE #x400)
  (RTM_F_PREFIX #x800)
  (RTM_F_LOOKUP_TABLE #x1000)
  (RTM_F_FIB_MATCH #x2000)
  (RTM_F_OFFLOAD #x4000)
  (RTM_F_TRAP #x8000))


;; rtm_table
(define-enum int->rtm-table
  RT_TABLE_UNSPEC
  (RT_TABLE_COMPAT 252)
  (RT_TABLE_DEFAULT 253)
  (RT_TABLE_MAIN 254)
  (RT_TABLE_LOCAL 255))

;; link type
;; more at include/uapi/linux/if_arp.h
(define-enum int->link-type
  (ARPHRD_ETHER 1)
  (ARPHRD_LOOPBACK 772))
