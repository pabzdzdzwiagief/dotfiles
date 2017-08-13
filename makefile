NEW_HOME=$(HOME)
PREFIX=$(NEW_HOME)/.local
CONFIG=$(NEW_HOME)/.config
AUTOSTART=$(CONFIG)/autostart

BIN_TARGETS = $(notdir $(wildcard bin/*))
HOME_TARGETS = $(notdir $(wildcard home/.*))
CONFIG_TARGETS = $(notdir $(wildcard config/*))
AUTOSTART_TARGETS = $(notdir $(wildcard autostart/*))
FONTS_TARGETS = \
        SourceCodePro-Bold \
		SourceCodePro-Light \
		SourceCodePro-Regular \
		SourceCodePro-Black \
		SourceCodePro-ExtraLight \
		SourceCodePro-Medium \
		SourceCodePro-Semibold

.PHONY: install-fonts \
        install-idea \
        install

install-fonts: $(FONTS_TARGETS:%=$(PREFIX)/share/fonts/source-code-pro/%.otf)
	fc-cache -f -v

install-idea: $(PREFIX)/bin/idea $(NEW_HOME)/idea-configuration.jar

install: $(BIN_TARGETS:%=$(PREFIX)/bin/%) \
	     $(HOME_TARGETS:%=$(NEW_HOME)/%) \
	     $(CONFIG_TARGETS:%=$(NEW_HOME)/.config/%) \
	     $(AUTOSTART_TARGETS:%=$(NEW_HOME)/.config/autostart/%) \
	     install-fonts

$(PREFIX)/share/fonts/source-code-pro $(PREFIX)/bin $(CONFIG)/idea $(NEW_HOME)/. $(NEW_HOME)/.. $(AUTOSTART) $(CONFIG) $(NEW_HOME):
	mkdir -p $@

$(PREFIX)/share/fonts/source-code-pro/%: fonts/OTF/% | $(PREFIX)/share/fonts/source-code-pro
	cp -v $< $@

fonts/OTF/%: fonts/download
	sh -c "cd fonts && ./download"

$(PREFIX)/bin/idea: idea/download | $(PREFIX)/bin $(PREFIX)/share
	sh -c "cd idea && ./download"
	ln -s $(PREFIX)/share/idea/bin/idea.sh $@
	touch $@

$(CONFIG)/idea/idea-configuration.jar: bin/zip-create idea/config | $(CONFIG)/idea
	bin/zip-create $@ idea/config/*

$(PREFIX)/bin/%: bin/% | $(PREFIX)/bin
	ln -s `bin/relpath $(CURDIR)/$< $|` $@

$(CONFIG)/%: config/% | $(CONFIG)
	ln -s `bin/relpath $(CURDIR)/$< $|` $@

$(AUTOSTART)/%: autostart/% | $(AUTOSTART)
	ln -s `bin/relpath $(CURDIR)/$< $|` $@

$(NEW_HOME)/%: home/% | $(NEW_HOME)
	ln -s `bin/relpath $(CURDIR)/$< $|` $@
