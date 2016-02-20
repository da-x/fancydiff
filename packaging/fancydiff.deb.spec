%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/control
Source: @@PKG_NAME@@
Section: devel
Priority: optional
Maintainer: Dan Aloni <alonid@gmail.com>
Build-Depends: debhelper (>= 9), netbase, ca-certificates, curl, libssl-dev, libgmp-dev, libicu-dev, libz-dev,
 locales, language-pack-en-base, stackage-dist-@@RESOLVER@@-stack, stackage-dist-@@RESOLVER@@-downloads, stackage-dist-@@RESOLVER@@-indices
Standards-Version: 3.9.5
Homepage: @@PKG_SITE@@

Package: @@PKG_NAME@@
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}
Description: @@PKG_ONELINE@@

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/build.sh
#!/bin/bash

set -x

source /usr/lib/stackage-dist-@@RESOLVER@@/helpers.sh
stackage-dist-setup @@RESOLVER@@ $(pwd)

stack --no-terminal build
install $(stack exec -- which fancydiff) debian/@@PKG_NAME@@/usr/bin

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/@@PKG_NAME@@.dirs
usr/bin

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/docs

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/rules
#!/usr/bin/make -f
# See debhelper(7) (uncomment to enable)
# output every command that modifies files on the build system.
#DH_VERBOSE = 1

# see EXAMPLES in dpkg-buildflags(1) and read /usr/share/dpkg/*
DPKG_EXPORT_BUILDFLAGS = 1
include /usr/share/dpkg/default.mk

# see FEATURE AREAS in dpkg-buildflags(1)
#export DEB_BUILD_MAINT_OPTIONS = hardening=+all

# see ENVIRONMENT in dpkg-buildflags(1)
# package maintainers to append CFLAGS
#export DEB_CFLAGS_MAINT_APPEND  = -Wall -pedantic
# package maintainers to append LDFLAGS
#export DEB_LDFLAGS_MAINT_APPEND = -Wl,--as-needed


# main packaging script based on dh7 syntax
%:
	dh $@

override_dh_auto_install:
	bash debian/build.sh

# debmake generated override targets
# This is example for Cmake (See http://bugs.debian.org/641051 )
#override_dh_auto_configure:
#	dh_auto_configure -- \
#	-DCMAKE_LIBRARY_PATH=$(DEB_HOST_MULTIARCH)

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/changelog
@@PKG_NAME@@ (@@PKG_FULLVER@@.@@DISTRO@@) @@DISTRO@@; urgency=low

  * Ubuntu/Debian build. See upstream changelog.

 -- Dan Aloni <alonid@gmail.com>  @@PKG_CHANGELOG_TIMESTAMP@@

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/source/format
3.0 (quilt)

%%%%%%%%%%%%%%%%%%%% CUT %%%%%%%%%%%%%%%%%%%%
%%%% debian/compat
9
