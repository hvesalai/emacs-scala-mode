############################################################-*-Makefile-*-####
# Makefile for compiling the major mode of Emacs
##############################################################################

## This Makefile has been copied from the scala project.
## See the LICENSE at the bottom of this file.

##############################################################################
# Configuration

ROOT			 = .

SOURCE_DIR		 = $(ROOT)

##############################################################################
# Variables

MODE_NAME               = scala-mode2

# Emacs Lisp
ELISP_COMMAND		?= emacs
ELISP_OPTIONS		+= -batch -no-site-file -q
ELISP_OPTIONS           += -L $(ROOT)


ELISP_FILES		+= $(MODE_NAME)-lib
ELISP_FILES		+= $(MODE_NAME)
ELISP_FILES		+= $(MODE_NAME)-syntax
ELISP_FILES		+= $(MODE_NAME)-indent
ELISP_FILES		+= $(MODE_NAME)-paragraph
ELISP_FILES		+= $(MODE_NAME)-fontlock
ELISP_FILES		+= $(MODE_NAME)-map
ELISP_FILES		+= $(MODE_NAME)-sbt
ELISP_SOURCES		+= $(ELISP_FILES:%=$(SOURCE_DIR)/%.el)

PKG_FILE		+= $(SOURCE_DIR)/$(MODE_NAME)-pkg.el

##############################################################################

RM			?= rm -f
RMDIR			?= rmdir
TOUCH			?= touch

# Strip the version out of the pkg file
VERSION                 := $(shell ${ELISP_COMMAND} $(ELISP_OPTIONS) --eval '(princ (format "%s\n" (nth 2 (read (find-file "$(PKG_FILE)")))))')
MODE_NAME_VERSION       = $(MODE_NAME)-$(VERSION)

##############################################################################
# Commands

all: .latest-build

clean:
	$(RM) *.elc .latest-* autoloads.el $(MODE_NAME_VERSION).tar
	[ ! -d $(MODE_NAME_VERSION) ] || $(RM) $(MODE_NAME_VERSION)/*
	[ ! -d $(MODE_NAME_VERSION) ] || $(RMDIR) $(MODE_NAME_VERSION)

.PHONY: all
.PHONY: clean

##############################################################################
# Rules

.latest-build: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) -f batch-byte-compile $(ELISP_SOURCES)
	@$(TOUCH) $@

##############################################################################

autoloads: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) --eval "(setq make-backup-files nil)" --eval "(setq generated-autoload-file (expand-file-name \"autoloads.el\"))" -f batch-update-autoloads `pwd`

package:
	mkdir -p $(MODE_NAME_VERSION)
	cp $(ELISP_SOURCES) $(PKG_FILE) $(MODE_NAME_VERSION)
	tar cf $(MODE_NAME_VERSION).tar $(MODE_NAME_VERSION)


## SCALA LICENSE
##
## Copyright (c) 2002-2011 EPFL, Lausanne, unless otherwise specified.
## All rights reserved.
##
## This software was developed by the Programming Methods Laboratory of the
## Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
##
## Permission to use, copy, modify, and distribute this software in source
## or binary form for any purpose with or without fee is hereby granted,
## provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright
## notice, this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright
## notice, this list of conditions and the following disclaimer in the
## documentation and/or other materials provided with the distribution.
##
## 3. Neither the name of the EPFL nor the names of its contributors
## may be used to endorse or promote products derived from this
## software without specific prior written permission.
##
##
## THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
## LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
## OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.
