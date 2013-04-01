USER := lalman
NEW_HOME := "/home/$(USER)"

all: $(NEW_HOME)/.local/bin/relpath \
     $(NEW_HOME)/.config/parcellite \
     $(NEW_HOME)/.zshrc \
     $(NEW_HOME)/.fonts

$(NEW_HOME)/.fonts: $(NEW_HOME)
	mkdir -p $@
	cd $@ && curl "http://garr.dl.sourceforge.net/project/sourcecodepro.adobe/SourceCodePro_FontsOnly-1.017.zip" > SourceCodePro.zip
	cd $@ && unzip SourceCodePro.zip
	cd $@ && mv SourceCodePro_FontsOnly-1.017/OTF/*.otf .
	cd $@ && rm -rf SourceCodePro_FontsOnly-1.017 SourceCodePro.zip

$(NEW_HOME)/.local/bin: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME)/.config: $(NEW_HOME)
	mkdir -p $@

$(NEW_HOME):
	sudo adduser -m -d$@ -s/usr/bin/zsh $(USER)

$(NEW_HOME)/.local/bin/%: $(NEW_HOME)/.local/bin bin/$*
	ln -s `bin/relpath bin $(NEW_HOME)/.local/bin/`/$* $@

$(NEW_HOME)/.config/%: $(NEW_HOME)/.config config/$*
	ln -s `bin/relpath config $(NEW_HOME)/.config/`/$* $@

$(NEW_HOME)/%: $(NEW_HOME) home/$*
	ln -s `bin/relpath home $(NEW_HOME)`/$* $@
