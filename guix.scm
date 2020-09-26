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
  (guix git-download)
  (guix packages)
  (guix utils)
  (gnu packages autotools)
  (gnu packages guile)
  (gnu packages guile-xyz)
  (gnu packages pkg-config)
  (gnu packages texinfo)
  (gnu packages tls))

(package
  (name "guile-netlink")
  (version "0.1")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://framagit.org/tyreunom/guile-netlink")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "0zfn3nwlz6xzip1j8xbj768dc299r037cfc81bk6kwl9xhzkjbrg"))))
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
  (home-page "https://framagit.org/tyreunom/guile-netlink")
  (synopsis "")
  (description "")
  (license license:gpl3+))
