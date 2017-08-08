NEW_HOME = $(HOME)

BIN_TARGETS = $(notdir $(wildcard bin/*))
HOME_TARGETS = $(notdir $(wildcard home/.*))
CONFIG_TARGETS = $(notdir $(wildcard config/*))
AUTOSTART_TARGETS = $(notdir $(wildcard autostart/*))
FONTS_TARGETS = SourceCodePro-Bold \
		SourceCodePro-Light \
		SourceCodePro-Regular \
		SourceCodePro-Black \
		SourceCodePro-ExtraLight \
		SourceCodePro-Medium \
		SourceCodePro-Semibold

.PHONY: install-fonts install idea

install-fonts: $(FONTS_TARGETS:%=$(NEW_HOME)/.local/share/fonts/source-code-pro/%.otf)
	fc-cache -f -v

install: $(BIN_TARGETS:%=$(NEW_HOME)/.local/bin/%) \
	 $(HOME_TARGETS:%=$(NEW_HOME)/%) \
	 $(CONFIG_TARGETS:%=$(NEW_HOME)/.config/%) \
	 $(AUTOSTART_TARGETS:%=$(NEW_HOME)/.config/autostart/%) \
	 install-fonts
	;

idea: $(NEW_HOME)/.local/bin/idea $(NEW_HOME)/idea-configuration.jar

$(NEW_HOME)/.local/bin/idea: idea/download | $(NEW_HOME)/.local/bin \
					     $(NEW_HOME)/.local/share
	sh -c "cd idea && ./download"
	ln -s $(NEW_HOME)/.local/share/idea/bin/idea.sh $@
	touch $@

$(NEW_HOME)/idea-configuration.jar:
	sh -c "cd idea/config && ../../bin/zip-create $@ *"

$(NEW_HOME)/.local/bin/%: | $(NEW_HOME)/.local/bin
	ln -s `bin/relpath bin $(NEW_HOME)/.local/bin`/$* $@

$(NEW_HOME)/.config/autostart/%: | $(NEW_HOME)/.config/autostart
	ln -s `bin/relpath autostart $(NEW_HOME)/.config/autostart`/$* $@

$(NEW_HOME)/.config/%: | $(NEW_HOME)/.config
	ln -s `bin/relpath config $(NEW_HOME)/.config`/$* $@

$(NEW_HOME)/.local/share/fonts/source-code-pro/%: fonts/OTF/% | $(NEW_HOME)/.local/share/fonts/source-code-pro
	mv $< $@

$(NEW_HOME)/.: | home/.
	;

$(NEW_HOME)/..: | home/..
	;

$(NEW_HOME)/%: home/% | $(NEW_HOME)
	ln -s `bin/relpath home $(NEW_HOME)`/$* $@

$(NEW_HOME)/.local/bin: | $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.local/share: | $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.config/autostart: | $(NEW_HOME)/.config
	mkdir -p $@

$(NEW_HOME)/.config: | $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.local/share/fonts/source-code-pro: | $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME):
	mkdir -p $@

fonts/OTF/%.otf: fonts/download
	sh -c "cd fonts && ./download"
