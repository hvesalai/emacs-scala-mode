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

# Emacs Lisp
ELISP_COMMAND		?= emacs
ELISP_OPTIONS		+= -batch -no-site-file
ELISP_OPTIONS           += -L $(ROOT)
ELISP_OPTIONS		+= -f batch-byte-compile


ELISP_FILES		+= scala-mode-lib
ELISP_FILES		+= scala-mode
ELISP_FILES		+= scala-mode-syntax
ELISP_FILES		+= scala-mode-indent
ELISP_FILES		+= scala-mode-paragraph
ELISP_FILES		+= scala-mode-fontlock
ELISP_FILES		+= scala-mode-map

ELISP_SOURCES		+= $(ELISP_FILES:%=$(SOURCE_DIR)/%.el)

##############################################################################

RM			?= rm -f
TOUCH			?= touch

##############################################################################
# Commands

all: .latest-build

clean:
	$(RM) *.elc .latest-* autoloads.el

.PHONY: all
.PHONY: clean

##############################################################################
# Rules

.latest-build: $(ELISP_SOURCES)
	$(ELISP_COMMAND) $(ELISP_OPTIONS) $(ELISP_SOURCES)
	@$(TOUCH) $@

##############################################################################

autoloads: $(ELISP_SOURCES)
	emacs -batch -q --no-site-file --eval "(setq make-backup-files nil)" --eval "(setq generated-autoload-file (expand-file-name \"autoloads.el\"))" -f batch-update-autoloads `pwd`



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
