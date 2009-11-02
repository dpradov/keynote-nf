@echo off
echo Updating LNG files for the TestApp application

cd ..\..\Output\bin
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\kdlscan.exe" Keynote.exe


del Keynote.spanish.lng.bak
copy Lang\Keynote.spanish.lng   Lang\Keynote.spanish.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Lang\Keynote.spanish.lng  Keynote.lng -! -x

del Keynote.dutch.lng.bak
copy Lang\Keynote.dutch.lng   Lang\Keynote.dutch.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Lang\Keynote.dutch.lng  Keynote.lng -! -x

del Keynote.german.lng.bak
copy Lang\Keynote.german.lng   Lang\Keynote.german.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Lang\Keynote.german.lng  Keynote.lng -! -x

del Keynote.chinese-simplified.lng.bak
copy Lang\Keynote.chinese-simplified.lng   Lang\Keynote.chinese-simplified.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Lang\Keynote.chinese-simplified.lng  Keynote.lng -! -x

del Keynote.french.lng.bak
copy Lang\Keynote.french.lng   Lang\Keynote.french.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Lang\Keynote.french.lng  Keynote.lng -! -x

pause
@echo on