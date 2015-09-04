NAME:=tightrope
BUILDDIR:=/dev/shm/$(NAME)-build
TARGET:=$(BUILDDIR)/$(NAME)
DOCUMENT:=$(BUILDDIR)/$(NAME).pdf
DOCSRC:=$(BUILDDIR)/$(NAME).org
PNGS+=$(patsubst ./%,$(BUILDDIR)/%,$(patsubst %.aa,%.png,$(shell find . -name "*.aa")))

CAT:=cat
CHMOD:=chmod
ECHO:=echo
RM:=rm
SED:=sed

SRCS:=$(wildcard *.scm)

all: $(TARGET)

doc: $(DOCUMENT)

prebuild:
ifeq "$(wildcard $(BUILDDIR))" ""
	@mkdir -p $(BUILDDIR)
endif

$(DOCUMENT): $(DOCSRC) $(HOME)/templates/pandoc-template.tex $(HOME)/templates/style.sty $(PNGS)
	cd $(BUILDDIR);pandoc -H $(HOME)/templates/style.sty --latex-engine=xelatex --template=$(HOME)/templates/pandoc-template.tex -f org -o $@ $(DOCSRC)

$(DOCSRC): prebuild | core.org java.org erlang.org clang.org
	$(CAT) core.org java.org erlang.org clang.org > $(DOCSRC)

$(TARGET): $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/main.scm
	$(ECHO) "#! /usr/bin/petite --script" > $(TARGET)
	$(CAT) $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/clang.scm $(BUILDDIR)/main.scm >> $(TARGET)
	$(ECHO) "(main (command-line))" >> $(TARGET)
	$(SED) -i -r '/\(load \".*\"\)/d' $(TARGET)
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
