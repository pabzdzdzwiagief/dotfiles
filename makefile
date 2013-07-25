NEW_HOME = $(HOME)

BIN_TARGETS = $(notdir $(wildcard bin/*))
HOME_TARGETS = $(notdir $(wildcard home/.*))
CONFIG_TARGETS = $(notdir $(wildcard config/*))
FONTS_TARGETS = SourceCodePro-Bold \
                SourceCodePro-Light \
                SourceCodePro-Regular \
                SourceCodePro-Black \
                SourceCodePro-ExtraLight \
                SourceCodePro-Medium \
                SourceCodePro-Semibold

install: $(BIN_TARGETS:%=$(NEW_HOME)/.local/bin/%) \
         $(HOME_TARGETS:%=$(NEW_HOME)/%) \
         $(CONFIG_TARGETS:%=$(NEW_HOME)/.config/%) \
         $(FONTS_TARGETS:%=$(NEW_HOME)/.fonts/%.otf)

$(NEW_HOME)/.local/bin/%: $(NEW_HOME)/.local/bin
	ln -s `bin/relpath bin $(NEW_HOME)/.local/bin`/$* $@

$(NEW_HOME)/.config/%: $(NEW_HOME)/.config
	ln -s `bin/relpath config $(NEW_HOME)/.config`/$* $@

$(NEW_HOME)/.fonts/%: $(NEW_HOME)/.fonts fonts
	ln -s `bin/relpath fonts $(NEW_HOME)/.fonts`/$* $@

$(NEW_HOME)/.: $(NEW_HOME) home/.

$(NEW_HOME)/..: $(NEW_HOME) home/..

$(NEW_HOME)/%: $(NEW_HOME) home/$*
	ln -s `bin/relpath home $(NEW_HOME)`/$* $@

$(NEW_HOME)/.local/bin: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.config: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.fonts: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME):
	mkdir -p $@

fonts:
	./download-fonts.sh
