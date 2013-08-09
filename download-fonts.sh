#!/usr/bin/env sh

mkdir fonts
cd fonts
wget "http://sourceforge.net/projects/sourcecodepro.adobe/files/SourceCodePro_FontsOnly-1.017.zip"
../bin/zip-extractall SourceCodePro_FontsOnly-1.017.zip
mv SourceCodePro_FontsOnly-1.017/OTF/*.otf .
rm -rf SourceCodePro_FontsOnly-1.017 SourceCodePro.zip
