NAME:=tightrope
BUILDDIR:=/dev/shm/$(NAME)-build
TARGET:=$(BUILDDIR)/$(NAME)
DOCUMENT:=$(BUILDDIR)/$(NAME).pdf
DOCSRC:=$(BUILDDIR)/$(NAME).org

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

$(DOCUMENT): $(DOCSRC) $(HOME)/templates/pandoc-template.tex $(HOME)/templates/style.sty
	pandoc -H $(HOME)/templates/style.sty --latex-engine=xelatex --template=$(HOME)/templates/pandoc-template.tex -f org -o $@ $(DOCSRC)

$(DOCSRC): prebuild | core.org java.org erlang.org
	$(CAT) core.org java.org erlang.org > $(DOCSRC)

$(TARGET): $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/main.scm
	$(ECHO) "#! /usr/bin/petite --script" > $(TARGET)
	$(CAT) $(BUILDDIR)/core.scm $(BUILDDIR)/java.scm $(BUILDDIR)/erlang.scm $(BUILDDIR)/main.scm >> $(TARGET)
	$(ECHO) "(main (command-line))" >> $(TARGET)
	$(SED) -i -r '/\(load \".*\"\)/d' $(TARGET)
	$(CHMOD) 755 $(TARGET)

$(BUILDDIR)/core.scm $(BUILDDIR)/main.scm: core.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

$(BUILDDIR)/java.scm: java.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

$(BUILDDIR)/erlang.scm: erlang.org | prebuild
	emacs $< --batch -f org-babel-tangle --kill

clean:
	$(RM) $(BUILDDIR) -rf

.PHONY: all clean doc prebuild
