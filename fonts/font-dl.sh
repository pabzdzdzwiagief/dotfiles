#!/usr/bin/env sh

curl "http://garr.dl.sourceforge.net/project/sourcecodepro.adobe/"\
"SourceCodePro_FontsOnly-1.017.zip" > SourceCodePro.zip
7za x SourceCodePro.zip
mv SourceCodePro_FontsOnly-1.017/OTF/*.otf .
rm -rf SourceCodePro_FontsOnly-1.017 SourceCodePro.zip
