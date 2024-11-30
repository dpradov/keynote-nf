@echo off

echo * Updating LNG files for KeyNote NF
echo *
echo * Make sure that 'keynote.exe' and 'keynote.drc' are copied to this folder
echo * To get the Delphi Resource String File (keynote.drc):
echo *  - In Delphi Environment open menu Project / Options / Linker and set MAP file to Detailed.

rem Update main language file from .exe and .drc
"..\..\src\3rd_party\Kryvich Delphi Localizer\kdlscan.exe" Keynote.exe


rem Update translation files from keynote.lng and old keynote.<language>.lng

del Keynote.spanish_WRK.lng.bak
copy Keynote.spanish_WRK.lng   Keynote.spanish_WRK.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.spanish_WRK.lng  Keynote.lng -! -x

del Keynote.dutch_WRK.lng.bak
copy Keynote.dutch_WRK.lng   Keynote.dutch_WRK.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.dutch_WRK.lng  Keynote.lng -! -x

del Keynote.german_WRK.lng.bak
copy Keynote.german_WRK.lng   Keynote.german_WRK.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.german_WRK.lng  Keynote.lng -! -x

del Keynote.chinese-simplified_WRK.lng.bak
copy Keynote.chinese-simplified.lng   Keynote.chinese-simplified.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.chinese-simplified_WRK.lng  Keynote.lng -! -x

del Keynote.french_WRK.lng.bak
copy Keynote.french_WRK.lng   Keynote.french_WRK.lng.bak
"..\..\src\3rd_party\Kryvich Delphi Localizer\lngupdate.exe" Keynote.french_WRK.lng  Keynote.lng -! -x


echo ****************** 

pause
@echo on