// -- KeyNote NF.iss --
//
#pragma verboselevel 9

#define AppName "KeyNote NF"
#define AppFileExe "keynote.exe"
#define Version "2.1.4.1"
#define AppVersion "2.1.4 .01"
;#define AppVersion GetVersionNumbersString("..\Output\bin\keynote.exe")
#define DefaultProfile "{app}\Profiles\Default"

#define _KNT_FILETYPE  'KeyNote file'
#define _KNE_FILETYPE  'KeyNote encrypted file'
#define _KNM_FILETYPE  'KeyNote macro'
#define _KNL_FILETYPE  'KeyNote plugin'


[Setup]
SignTool=mySignTool
TouchDate=2025-12-24
TouchTime=19:00
AppName={#AppName}
AppVersion={#AppVersion}
VersionInfoVersion={#Version}
AppCopyright=Copyright (C) 2007-2025 Daniel Prado Velasco   (C) 2000-2005 Marek Jedlinski
;AppComments=
AppContact=Daniel Prado Velasco <dprado.keynote@gmail.com>
AppPublisher=Daniel Prado Velasco
AppPublisherURL=https://github.com/dpradov/keynote-nf
AppReadmeFile=https://github.com/dpradov/keynote-nf#readme
AppSupportURL=https://github.com/dpradov/keynote-nf/issues
AppUpdatesURL=https://github.com/dpradov/keynote-nf/releases
WizardStyle=modern
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
AllowNoIcons=yes
UninstallDisplayIcon={app}\{#AppFileExe}
LicenseFile={#file AddBackslash(SourcePath) + "doc\License_agreement.txt"}
OutputDir=..\..\Output
OutputBaseFilename=kntSetup_{#Version}
SetupIconFile=keynote_Icon.ico
WizardImageFile=resources\Aux_\keynote_4.bmp
WizardSmallImageFile=resources\Aux_\keynote_0.bmp, resources\Aux_\keynote_1.bmp, resources\Aux_\keynote_2.bmp, resources\Aux_\keynote_3.bmp
WizardImageStretch=no
DisableWelcomePage=no
Uninstallable=not WizardIsTaskSelected('portablemode')
ChangesAssociations=WizardIsTaskSelected('associate')

InfoBeforeFile=doc\NotesBeforeSetup.txt
InfoAfterFile=doc\history.txt
SignedUninstaller=yes


[Types]
Name: "full"; Description: "Full installation"
Name: "compact"; Description: "Compact installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "main"; Description: "Program Files"; Types: full compact custom; Flags: fixed
Name: "help"; Description: "Help File"; Types: full
Name: "plugins"; Description: "Plugins"; Types: full


[Tasks]
Name: portablemode; Description: "&Portable Mode (no uninstallation support)"; Flags: unchecked
Name: desktopicon; Description: "Create a &Shortcut on the &Desktop"; Components: main; Flags: unchecked
Name: associate; Description: "Associate KeyNote NF with the .knt, kne and .knm file extension"; Flags: unchecked
; GroupDescription: "Additional icons:"


[Dirs]
Name: "{app}\lang"; Permissions: users-modify
Name: "{app}\macros"; Permissions: users-modify
Name: "{app}\plugins"; Permissions: users-modify;
Name: "{app}\templates"; Permissions: users-modify
Name: "{app}\Profiles" ; Permissions: users-modify
Name: "{app}\Profiles\Default"
Name: "{app}\Profiles\Default\macros"
Name: "{app}\Profiles\Help"
;Flags: uninsalwaysuninstall uninsneveruninstall

[InstallDelete]
Type: files; Name: "{app}\KeyNote Handbook.lnk"
Type: files; Name: "{app}\KeyNote Help 1.6.lnk"
Type: files; Name: "{app}\KeyNote.chm"
Type: files; Name: "{app}\doc\Images_Readme.txt"
Type: files; Name: "{app}\help\Comments on KNT file formats.txt"
Type: files; Name: "{app}\help\sample.knt"
Type: files; Name: "{app}\help\KeyNote Handbook.lnk"
Type: files; Name: "{app}\help\KeyNote Help 1.6.lnk"
Type: files; Name: "{app}\help\KeyNote Help chm.lnk"
Type: filesandordirs; Name: "{app}\help\kntHelpFiles"
Type: files; Name: "{app}\plugins\funckey.knl"
Type: files; Name: "{app}\plugins\funckey_readme.txt"
Type: files; Name: "{app}\plugins\kncalendar_readme.txt"
Type: files; Name: "{app}\plugins\readme.txt"
Type: files; Name: "{app}\Profiles\Help\keynote_hlp.ico"
Type: files; Name: "{app}\doc\fileformat_1.6.5.txt"
Type: files; Name: "{app}\doc\fileformat.knt"
Type: files; Name: "{app}\kntutils.dll"

[UninstallDelete]
Type: files; Name: "{app}\Profiles\Help\keynote.mgr"
Type: files; Name: "{app}\Profiles\Help\keynote.mru"


[Files]
Source: "..\Output\bin\{#AppFileExe}"; DestDir: "{app}"; Components: main; Flags: touch
Source: "..\Output\bin\kntLauncher.exe"; DestDir: "{app}"; Components: main; Flags: touch
Source: "general\keyboard.css"; DestDir: "{app}" ; Components: main
Source: "misc_files\clip.wav"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\alert.wav"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\KeyNote.tip"; DestDir: "{app}" ; Components: main
Source: "misc_files\dateformats.txt"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\timeformats.txt"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist

; Profiles
Source: "doc\Profiles.txt"; DestDir: "{app}\Profiles" ; Components: main

; {#DefaultProfile}
Source: "misc_files\keynote.kns"    ; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\keynote.exp"    ; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\keynote.icn"    ; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\nodehead.rtf"   ; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\notehead.rtf"   ; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist
;Source: "misc_files\keyboard.ini"; DestDir: "{#DefaultProfile}" ; Components: main; Flags: onlyifdoesntexist

; Help profile
Source: "misc_files\keynote_Help.ini"; DestDir: "{app}\Profiles\Help"; DestName: "keynote.ini" ; Components: help; Flags: onlyifdoesntexist
Source: "misc_files\keynote.kns"     ; DestDir: "{app}\Profiles\Help" ; Components: help; Flags: onlyifdoesntexist

; Lang
Source: "Lang\readme.txt"; DestDir: "{app}\lang" ; Components: main

; {app}\macros
Source: "macros\examples\_AutoNewFile.knm"; DestDir: "{app}\macros\examples" ; Components: main
Source: "macros\examples\_AutoNewNode.knm"; DestDir: "{app}\macros\examples" ; Components: main
Source: "macros\examples\_AutoNewNote.knm"; DestDir: "{app}\macros\examples" ; Components: main
Source: "macros\examples\_AutoNewTree.knm"; DestDir: "{app}\macros\examples" ; Components: main
Source: "macros\examples\_Test_Macro.knm" ; DestDir: "{app}\macros\examples" ; Components: main

; {app}\doc
Source: "doc\history.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\acknowledgments.txt"; DestDir: "{app}\doc"; Components: main
Source: "doc\References.md"; DestDir: "{app}\doc" ; Components: main
Source: "README.md"; DestDir: "{app}\doc" ; Components: main; Flags: isreadme
Source: "LICENSE.txt"; DestDir: "{app}\doc" ; Components: main;
Source: "doc\README_News.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_1.x (until v1.6.5).txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_2.0.knt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_2.1 (until v1.9.5).txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_3.0 (since v2.0.0).txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_3.0.knt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_3.1 (since v2.1.0).txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_3.1.knt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_minimal.knt"; DestDir: "{app}\doc"; Components: main
Source: "doc\kn_fileformat\fileformat_readme.txt"; DestDir: "{app}\doc" ; Components: main
Source: "misc_files\wordweb.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.0 Beta1-6.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.1 Beta1-6.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.2 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.3 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.4 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.5 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.9.0 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.9.1 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.9.2 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.9.3 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.9.5 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.0.0 .10.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.0.1 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.0.2 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.1.0 Beta1.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.1.1 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.1.2 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.1.3 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 2.1.4 .01.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\dart.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\dart_format.txt"; DestDir: "{app}\doc" ; Components: main

; {app}\help
Source: "doc\cmdline.txt"; DestDir: "{app}\help" ; Components: help
Source: "misc_files\KeyNoteNF_Help.knt"; DestDir: "{app}\help" ; Components: help
Source: "resources\Icons\keynote_hlp.ico"  ; DestDir: "{app}\help" ; Components: help

; {app}\templates
Source: "misc_files\Meeting template - sample.rtf"; DestDir: "{app}\templates"; Flags: onlyifdoesntexist

; {app}\plugins
Source: "plugins\Binary examples\32bits\kncalendar.knl"; DestDir: "{app}\plugins" ; Components: plugins
Source: "plugins\Binary examples\32bits\Readme_plugins.txt"; DestDir: "{app}\plugins" ; Components: plugins
Source: "plugins\Binary examples\32bits\GoogleSearch.knl"; DestDir: "{app}\plugins" ; Components: plugins
;Source: "plugins\Binary examples\64bits\GoogleSearch.knl"; DestDir: "{app}\plugins" ; Components: plugins

; confirmoverwrite promptifolder


[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppFileExe}"
Name: "{group}\{#AppName} GitHub"; Filename: "https://github.com/dpradov/keynote-nf"
Name: "{group}\{#AppName} GitHub - Issues"; Filename: "https://github.com/dpradov/keynote-nf/issues"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#AppFileExe}"; Tasks: desktopicon

; Links to help file
Name: "{app}\KeyNote NF Help"; Filename: "{app}\kntLauncher.exe"; Parameters: "Profiles\Help\keynote.ini  help\keynoteNF_Help.knt -ignSI -dnd -jmp""file:///*8|2"" -title""KeyNote NF Topics"""; Components: help; IconFilename: "{app}\help\keynote_hlp.ico"

Name: "{app}\help\Profiles.txt"; Filename: "{app}\Profiles\Profiles.txt"; Components: help

[Registry]
; Associate .knt, .kne, .knm files with Keynote (requires ChangesAssociations=yes)
Root: HKA; Subkey: "Software\Classes\.knt\OpenWithProgids"; ValueType: string; ValueName: "{#_KNT_FILETYPE}.knt"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#_KNT_FILETYPE}"; ValueType: string; ValueName: ""; ValueData: "{#_KNT_FILETYPE}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#_KNT_FILETYPE}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#AppFileExe},0"
Root: HKA; Subkey: "Software\Classes\{#_KNT_FILETYPE}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppFileExe}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\Applications\{#AppFileExe}\SupportedTypes"; ValueType: string; ValueName: ".knt"; ValueData: ""

Root: HKA; Subkey: "Software\Classes\.kne\OpenWithProgids"; ValueType: string; ValueName: "{#_KNE_FILETYPE}.kne"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#_KNE_FILETYPE}"; ValueType: string; ValueName: ""; ValueData: "{#_KNE_FILETYPE}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#_KNE_FILETYPE}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#AppFileExe},0"
Root: HKA; Subkey: "Software\Classes\{#_KNE_FILETYPE}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppFileExe}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\Applications\{#AppFileExe}\SupportedTypes"; ValueType: string; ValueName: ".kne"; ValueData: ""

Root: HKA; Subkey: "Software\Classes\.knm\OpenWithProgids"; ValueType: string; ValueName: "{#_KNM_FILETYPE}.knm"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#_KNM_FILETYPE}"; ValueType: string; ValueName: ""; ValueData: "{#_KNM_FILETYPE}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#_KNM_FILETYPE}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#AppFileExe},0"
Root: HKA; Subkey: "Software\Classes\{#_KNM_FILETYPE}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppFileExe}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\Applications\{#AppFileExe}\SupportedTypes"; ValueType: string; ValueName: ".knm"; ValueData: ""

Root: HKA; Subkey: "Software\Classes\.knm\OpenWithProgids"; ValueType: string; ValueName: "{#_KNL_FILETYPE}.knm"; ValueData: ""; Flags: uninsdeletevalue
Root: HKA; Subkey: "Software\Classes\{#_KNL_FILETYPE}"; ValueType: string; ValueName: ""; ValueData: "{#_KNL_FILETYPE}"; Flags: uninsdeletekey
Root: HKA; Subkey: "Software\Classes\{#_KNL_FILETYPE}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#AppFileExe},0"
Root: HKA; Subkey: "Software\Classes\{#_KNL_FILETYPE}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppFileExe}"" ""%1"""
Root: HKA; Subkey: "Software\Classes\Applications\{#AppFileExe}\SupportedTypes"; ValueType: string; ValueName: ".knm"; ValueData: ""

