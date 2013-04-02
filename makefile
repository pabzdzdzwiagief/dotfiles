USER := lalman
NEW_HOME := "/home/$(USER)"

install: $(NEW_HOME)/.fonts/SourceCodePro-Bold.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-Light.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-Regular.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-Black.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-ExtraLight.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-Medium.otf \
         $(NEW_HOME)/.fonts/SourceCodePro-Semibold.otf
#	 $(NEW_HOME)/.local/bin/relpath \
#        $(NEW_HOME)/.config/parcellite \
#        $(NEW_HOME)/.zshrc \
#        $(NEW_HOME)/.fonts

$(NEW_HOME)/.local/bin: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.config: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.fonts: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME):

$(NEW_HOME)/.local/bin/%: $(NEW_HOME)/.local/bin bin/$*
	ln -s `bin/relpath bin $(NEW_HOME)/.local/bin/`/$* $@

$(NEW_HOME)/.config/%: $(NEW_HOME)/.config config/$*
	ln -s `bin/relpath config $(NEW_HOME)/.config/`/$* $@

$(NEW_HOME)/.fonts/%: $(NEW_HOME)/.fonts fonts/$*
	ln -s `bin/relpath fonts $(NEW_HOME)/.fonts/`/$* $@

$(NEW_HOME)/%: $(NEW_HOME) home/$*
	ln -s `bin/relpath home $(NEW_HOME)`/$* $@

fonts/%.otf:
	cd .fonts && ./fonts-dl.sh
