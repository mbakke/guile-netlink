;;;; This file is part of Guile Netlink
;;;;
;;;; Copyright (C) 2020 Julien Lepiller <julien@lepiller.eu>
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

(use-modules (guix gexp)
             (guix utils)
             (git)
             (gnu packages base)
             (gnu packages texinfo))

(define %makeinfo-html-options
  ;; Options passed to 'makeinfo --html'.
  '("--css-ref=https://www.gnu.org/software/gnulib/manual.css"
    "-c" "EXTRA_HEAD=<meta name=\"viewport\" \
content=\"width=device-width, initial-scale=1\" />"))

(define* (texinfo-source source #:key (version "unknown"))
  (define texi-source
    (local-file
      (string-append (local-file-absolute-file-name source) "/doc/guile-netlink.texi")
      "guile-netlink.texi"))

  (define version.texi
    (plain-file
      "version.texi"
      (format #f "@set UPDATED unknown
@set UPDATED-MONTH unknown
@set EDITION unknown
@set VERSION ~a\n" version)))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p #$output)
          (symlink #$version.texi (string-append #$output "/version.texi"))
          (symlink #$texi-source (string-append #$output "/guile-netlink.texi")))))

  (computed-file "texinfo-manual-source" build))

(define* (build-manual source #:key (version "unknown"))
  (define texi (texinfo-source source #:version version))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (setenv "GUIX_LOCPATH"
                  #+(file-append glibc-utf8-locales "/lib/locale"))
          (setenv "LC_ALL" "en_US.utf8")
          (setlocale LC_ALL "en_US.utf8")

          (mkdir-p (string-append #$output "/html_node"))

          (apply invoke
                 #$(file-append texinfo "/bin/makeinfo")
                 "-o" (string-append #$output "/html_node")
                 "--html" "-c" "TOP_NODE_UP_URL=/guile-netlink/manual"
                 (string-append #$texi "/guile-netlink.texi")
                 '#$%makeinfo-html-options)
          (apply invoke
                 #$(file-append texinfo "/bin/makeinfo")
                 "--no-split"
                 "-o" (string-append #$output "/manual.html")
                 "--html" "-c" "TOP_NODE_UP_URL=/guile-netlink/manual"
                 (string-append #$texi "/guile-netlink.texi")
                 '#$%makeinfo-html-options))))

  (computed-file "manual" build))

(let* ((root (canonicalize-path
              (string-append (current-source-directory) "/..")))
       (source (local-file root "guix" #:recursive? #t)))
  (build-manual source))
