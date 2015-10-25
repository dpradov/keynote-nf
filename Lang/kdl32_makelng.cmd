@echo off

echo * Creating/updating keynote.lng file
echo *
echo * Make sure that 'keynote.exe' and 'keynote.drc' are copied to this folder
echo * To get the Delphi Resource String File (keynote.drc):
echo *  - In Delphi Environment open menu Project / Options / Linker and set MAP file to Detailed.

rem Update main language file from .exe and .drc

"..\..\src\3rd_party\kdl32_Kryvich's Delphi Localizer\kdlscan.exe" Keynote.exe

echo ***************

pause
@echo on