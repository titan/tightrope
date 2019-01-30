NAME:=tightrope
BUILDDIR:=/dev/shm/$(NAME)-build
TARGET:=$(BUILDDIR)/$(NAME)
TARGETSRC:=$(TARGET).ss
TARGETOBJ:=$(TARGET).so
DOCUMENT:=$(BUILDDIR)/$(NAME).pdf
DOCSRC:=$(BUILDDIR)/$(NAME).org
PNGS+=$(patsubst ./%,$(BUILDDIR)/%,$(patsubst %.aa,%.png,$(shell find . -name "*.aa")))
THEME=eisvogel
FONT=FZYaSong-GBK

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

$(DOCUMENT): $(DOCSRC) $(PNGS)
	cd $(BUILDDIR); pandoc --pdf-engine=xelatex --template $(THEME).tex -V CJKmainfont:$(FONT) -o $@ $(DOCSRC); cd -

$(DOCSRC): prebuild | core.org java.org erlang.org clang.org nim.org python.org
	$(CAT) core.org java.org erlang.org clang.org nim.org python.org > $(DOCSRC)

$(TARGETSRC): $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/nim.scm $(BUILDDIR)/python.scm $(BUILDDIR)/main.scm
	$(ECHO) "(import (chezscheme))" > $(TARGETSRC)
	$(CAT) $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/nim.scm $(BUILDDIR)/python.scm $(BUILDDIR)/main.scm >> $(TARGETSRC)
	$(ECHO) "(main (command-line))" >> $(TARGETSRC)
	$(SED) -i -r '/\(load \".*\"\)/d' $(TARGETSRC)

$(TARGETOBJ): $(TARGETSRC)
	$(ECHO) '(compile-program "$(TARGETSRC)")' | chez-scheme -q --optimize-level 3

$(TARGET): | prebuild
	$(ECHO) '#! /bin/sh' > $(TARGET)
	$(ECHO) 'LINK=`readlink -f $$0`' >> $(TARGET)
	$(ECHO) 'BASE=`dirname $$LINK`' >> $(TARGET)
	$(ECHO) 'chez-scheme --program $$BASE/$(NAME).so $$@' >> $(TARGET)
	$(CHMOD) 755 $(TARGET)

$(BUILDDIR)/%.png: %.aa | prebuild
	java -jar /opt/ditaa0_9.jar -e utf-8 -s 1.0 $< $@

$(BUILDDIR)/core.scm $(BUILDDIR)/main.scm: core.org | prebuild
	org-tangle $<

$(BUILDDIR)/java.scm: java.org | prebuild
	org-tangle $<

$(BUILDDIR)/erlang.scm: erlang.org | prebuild
	org-tangle $<

$(BUILDDIR)/clang.scm: clang.org | prebuild
	org-tangle $<

$(BUILDDIR)/nim.scm: nim.org | prebuild
	org-tangle $<

$(BUILDDIR)/python.scm: python.org | prebuild
	org-tangle $<

clean:
	$(RM) $(BUILDDIR) -rf

.PHONY: all clean doc prebuild
