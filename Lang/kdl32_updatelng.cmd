@echo off

echo * Updating LNG files for KeyNote NF
echo *
echo * Make sure that 'keynote.exe' and 'keynote.drc' are copied to this folder
echo * To get the Delphi Resource String File (keynote.drc):
echo *  - In Delphi Environment open menu Project / Options / Linker and set MAP file to Detailed.

rem Update main language file from .exe and .drc
"..\..\src\3rd_party\Kryvich Delphi Localizer\kdlscan.exe" Keynote.exe


rem Update translation files from keynote.lng and old keynote.<language>.lng

del Keynote.spanish.lng.bak
copy Keynote.spanish.lng   Keynote.spanish.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.spanish.lng  Keynote.lng -! -x

del Keynote.dutch.lng.bak
copy Keynote.dutch.lng   Keynote.dutch.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.dutch.lng  Keynote.lng -! -x

del Keynote.german.lng.bak
copy Keynote.german.lng   Keynote.german.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.german.lng  Keynote.lng -! -x

del Keynote.chinese-simplified.lng.bak
copy Keynote.chinese-simplified.lng   Keynote.chinese-simplified.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.chinese-simplified.lng  Keynote.lng -! -x

del Keynote.french.lng.bak
copy Keynote.french.lng   Keynote.french.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.french.lng  Keynote.lng -! -x


echo ****************** 

pause
@echo on