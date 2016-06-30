NAME:=tightrope
BUILDDIR:=/dev/shm/$(NAME)-build
TARGET:=$(BUILDDIR)/$(NAME)
TARGETSRC:=$(TARGET).ss
TARGETOBJ:=$(TARGET).so
DOCUMENT:=$(BUILDDIR)/$(NAME).pdf
DOCSRC:=$(BUILDDIR)/$(NAME).org
PNGS+=$(patsubst ./%,$(BUILDDIR)/%,$(patsubst %.aa,%.png,$(shell find . -name "*.aa")))

CAT:=cat
CHMOD:=chmod
ECHO:=echo
RM:=rm
SED:=sed

SRCS:=$(wildcard *.scm)

all: $(TARGET) $(TARGETOBJ)

doc: $(DOCUMENT)

prebuild:
ifeq "$(wildcard $(BUILDDIR))" ""
	@mkdir -p $(BUILDDIR)
endif

$(DOCUMENT): $(DOCSRC) $(HOME)/templates/pandoc-template.tex $(HOME)/templates/style.sty $(PNGS)
	cd $(BUILDDIR);pandoc -H $(HOME)/templates/style.sty --latex-engine=xelatex --template=$(HOME)/templates/pandoc-template.tex -f org -o $@ $(DOCSRC)

$(DOCSRC): prebuild | core.org java.org erlang.org clang.org
	$(CAT) core.org java.org erlang.org clang.org > $(DOCSRC)

$(TARGETSRC): $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/main.scm
	$(ECHO) "(import (chezscheme))" > $(TARGETSRC)
	$(CAT) $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/main.scm >> $(TARGETSRC)
	$(ECHO) "(main (command-line))" >> $(TARGETSRC)
	$(SED) -i -r '/\(load \".*\"\)/d' $(TARGETSRC)

$(TARGETOBJ): $(TARGETSRC)
	$(ECHO) '(compile-program "$(TARGETSRC)")' | chez-scheme -q --optimize-level 3

$(TARGET):
	$(ECHO) '#! /bin/sh' > $(TARGET)
	$(ECHO) 'LINK=`readlink -f $$0`' >> $(TARGET)
	$(ECHO) 'BASE=`dirname $$LINK`' >> $(TARGET)
	$(ECHO) 'chez-scheme --program $$BASE/$(NAME).so $$@' >> $(TARGET)
	$(CHMOD) 755 $(TARGET)

$(BUILDDIR)/%.png: %.aa | prebuild
	java -jar /opt/ditaa0_9.jar -e utf-8 -s 1.0 $< $@

$(BUILDDIR)/core.scm $(BUILDDIR)/main.scm: core.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

$(BUILDDIR)/java.scm: java.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

$(BUILDDIR)/erlang.scm: erlang.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

$(BUILDDIR)/clang.scm: clang.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

clean:
	$(RM) $(BUILDDIR) -rf

.PHONY: all clean doc prebuild
