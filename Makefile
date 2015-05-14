TARGET:=tightrope

CAT:=cat
CHMOD:=chmod
ECHO:=echo
RM:=rm
SED:=sed

SRCS:=$(wildcard *.scm)

all: $(TARGET)

$(TARGET): $(SRCS)
	$(ECHO) "#! /usr/bin/petite --script" > $(TARGET)
	$(CAT) $(SRCS) >> $(TARGET)
	$(ECHO) "(main (command-line))" >> $(TARGET)
	$(SED) -i -r '/\(load \".*\"\)/d' $(TARGET)
	$(CHMOD) 755 $(TARGET)

clean:
	$(RM) $(TARGET)

.PHONY: all clean
