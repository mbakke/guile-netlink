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

(define-module (netlink deserialize)
  #:use-module (netlink constant)
  #:use-module ((netlink route attrs) #:prefix route:)
  #:use-module ((netlink route link) #:prefix route:)
  #:use-module (netlink standard)
  #:use-module (netlink message)
  #:export (%default-message-decoder
            %default-route-decoder))

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
      (,RTM_SETLINK . ,route:deserialize-link-message))
    (attr ,route:deserialize-route-attr
          ,@route:%default-route-attr-decoder)))
