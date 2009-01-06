@echo off
echo Updating LNG files for the TestApp application

cd ..\..\Output\bin
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\kdlscan.exe" Keynote.exe


del Keynote.spanish.lng.bak
copy Keynote.spanish.lng   Keynote.spanish.lng.bak
"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\lngupdate.exe" Keynote.spanish.lng  Keynote.lng -! -x


pause
@echo on