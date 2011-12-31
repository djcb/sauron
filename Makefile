## Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

VERSION=$(shell grep "^;; Version:" sauron.el  | sed 's/^[^0-9]*//')

FILES=	sauron.el	\
	sauron-dbus.el  \
	sauron-erc.el   \
	sauron-org.el

ELPA_FILES = $(FILES)	\
	sauron-pkg.el

marmalade: elpa
elpa: sauron-$(VERSION).tar

sauron-$(VERSION).tar: $(ELPA_FILES)
	mkdir sauron-$(VERSION)
	cp --preserve=timestamps $(ELPA_FILES) sauron-$(VERSION)
	tar -cvf sauron-$(VERSION).tar sauron-$(VERSION)
	rm -rf sauron-$(VERSION)

sauron-pkg.el: sauron-pkg.el.in
	sed -e s/@VERSION@/$(VERSION)/ < $< > $@

clean:
	rm -rf sauron-pkg.el *.tar *.gz sauron-$(VERSION)

