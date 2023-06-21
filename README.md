Guile Netlink
=============

Guile Netlink is a [GNU Guile](https://gnu.org/software/guile) library
providing an implementation of the netlink protocol.

It provides:

* a generic library for writing implementations of a netlink protocol
* a low-level rtnetlink implementation that uses that library
* a high-level API for network management that uses rtnetlink
* an implementation of the _generic_ netlink protocol (genetlink)
* an interface for communicating with wireless devices (nl80211)

Installation
------------

### Manually, from source

Guile Netlink is a pure Guile library and does not require any library except
for a libc.

Guile Netlink uses the autotools to manage the build and installation scripts.
From a checkout, you can run the following commands to build and install
in the default prefix, `/usr/local`.

```bash
./bootstrap
./configure
make
sudo make install
```

To use the result, you can extend the following environment variables.

```bash
export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_COMPILED_LOAD_PATH"
```

Alternatively, you can skip the `make install` phase and use the result
directly from this checkout, with the `pre-inst-env` script.

```bash
./bootstrap
./configure
make
./pre-inst-env guile
```

### With Guix

For convenience, we provide a guix.scm file that defines a package
you can use with [GNU Guix](https://guix.gnu.org).  If you have Guix installed,
run:

```bash
guix install -f guix.scm
```

License
-------

Guile Netlink is licenced under GPLv3 or later. See COPYING file for details.
