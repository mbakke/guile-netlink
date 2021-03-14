;;;; Copyright (C) 2019 Julien Lepiller <julien@lepiller.eu>
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

(use-modules
  ((guix licenses) #:prefix license:)
  (guix build-system gnu)
  (guix download)
  (guix gexp)
  (guix git-download)
  (guix packages)
  (guix utils)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls))

(define %srcdir
  (dirname (current-filename)))

(package
  (name "guile-netlink")
  (version "0.1")
  (source (local-file "." "guile-netlink-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %srcdir)))
  (build-system gnu-build-system)
  (arguments
   `(#:tests? #f)); no tests
  (inputs
   `(("guile" ,guile-3.0)))
  (native-inputs
   `(("automake" ,automake)
     ("autoconf" ,autoconf)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (home-page "https://git.lepiller.eu/guile-netlink")
  (synopsis "Netlink protocol implementation for Guile")
  (description "Guile Netlink is a GNU Guile library providing an implementation
of the netlink protocol.

It provides a generic library for writing implementations of a netlink
protocol, a low-level rtnetlink implementation that uses that library and a
high-level API for network management that uses rtnetlink.")
  (license license:gpl3+))
