#!/usr/bin/env sh

mkdir fonts
cd fonts
curl "http://garr.dl.sourceforge.net/project/sourcecodepro.adobe/"\
"SourceCodePro_FontsOnly-1.017.zip" > SourceCodePro.zip
unzip SourceCodePro.zip
mv SourceCodePro_FontsOnly-1.017/OTF/*.otf .
rm -rf SourceCodePro_FontsOnly-1.017 SourceCodePro.zip
