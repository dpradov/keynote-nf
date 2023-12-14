// -- KeyNote NF.iss --
//
#pragma verboselevel 9

#define AppName "KeyNote NF"
#define AppFileExe "keynote.exe"
#define Version "1.8.1.8"
#define AppVersion "1.8.1 .08"
;#define AppVersion GetVersionNumbersString("..\Output\bin\keynote.exe")
#define DefaultProfile "{app}\Profiles\Default"

#define _KNT_FILETYPE  'KeyNote file'
#define _KNE_FILETYPE  'KeyNote encrypted file'
#define _KNM_FILETYPE  'KeyNote macro'
#define _KNL_FILETYPE  'KeyNote plugin'


[Setup]
AppName={#AppName}
AppVersion={#AppVersion}
VersionInfoVersion={#Version}
AppCopyright=Copyright (C) 2007-2023 Daniel Prado Velasco   (C) 2000-2005 Marek Jedlinski
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
TouchDate=2023-12-14
TouchTime=21:00
SetupIconFile=keynote_Icon.ico
WizardImageFile=resources\keynote_4.bmp
WizardSmallImageFile=resources\keynote_0.bmp, resources\keynote_1.bmp, resources\keynote_2.bmp, resources\keynote_3.bmp
WizardImageStretch=no
DisableWelcomePage=no
Uninstallable=not WizardIsTaskSelected('portablemode')
ChangesAssociations=WizardIsTaskSelected('associate')

InfoBeforeFile=doc\NotesBeforeSetup.txt
InfoAfterFile=doc\history.txt


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
;Name: "{app}\lang"
Name: "{app}\macros"; Permissions: users-modify
Name: "{app}\plugins"; Permissions: users-modify;
Name: "{app}\templates"; Permissions: users-modify
Name: "{app}\Profiles" ; Permissions: users-modify
Name: "{app}\Profiles\Default"
Name: "{app}\Profiles\Default\macros"
Name: "{app}\Profiles\Help"
Name: "{app}\help\kntHelpFiles"; Permissions: users-modify; Components: help
;Flags: uninsalwaysuninstall uninsneveruninstall

[InstallDelete]
Type: files; Name: "{app}\Profiles\Help\dateformats.txt"
Type: files; Name: "{app}\Profiles\Help\timeformats.txt"

[UninstallDelete]
Type: files; Name: "{app}\Profiles\Help\keynote.mgr"
Type: files; Name: "{app}\Profiles\Help\keynote.mru"


[Files]
Source: "..\Output\bin\{#AppFileExe}"; DestDir: "{app}"; Components: main; Flags: touch
Source: "..\Output\bin\kntutils.dll" ; DestDir: "{app}" ; Components: main
;Source: "Lang\keynote.lan"; DestDir: "{app}" ; Components: main
Source: "general\keyboard.css"; DestDir: "{app}" ; Components: main
Source: "misc_files\clip.wav"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist
Source: "misc_files\alert.wav"; DestDir: "{app}" ; Components: main; Flags: onlyifdoesntexist
Source: "doc\Help\KeyNote.chm"; DestDir: "{app}" ; Components: help
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
Source: "misc_files\keynote_Help.ini"; DestDir: "{app}\Profiles\Help"; DestName: "keynote.ini" ; Components: help
Source: "misc_files\keynote.kns"     ; DestDir: "{app}\Profiles\Help" ; Components: help; Flags: onlyifdoesntexist
Source: "resources\keynote_hlp.ico"  ; DestDir: "{app}\Profiles\Help" ; Components: help

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
Source: "doc\kn_fileformat\fileformat_1.6.5.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat.knt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\kn_fileformat\fileformat_minimal.knt"; DestDir: "{app}\doc"; Components: main
Source: "doc\kn_fileformat\fileformat_readme.txt"; DestDir: "{app}\doc" ; Components: main
Source: "misc_files\wordweb.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.0 Beta1-6.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Changes in 1.8.1 Beta1-6.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\Images_Readme.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\dart.txt"; DestDir: "{app}\doc" ; Components: main
Source: "doc\dart_format.txt"; DestDir: "{app}\doc" ; Components: main

; {app}\help
Source: "doc\Help\hlp_1.6.5\KN_Handbook_1.6.knt"; DestDir: "{app}\help\kntHelpFiles" ; Components: help
Source: "doc\Help\hlp_1.6.5\KN_Help_1.6.knt"; DestDir: "{app}\help\kntHelpFiles" ; Components: help
Source: "doc\Help\hlp_1.6.5\KN_Help_Readme.txt"; DestDir: "{app}\help\kntHelpFiles" ; Components: help
Source: "doc\cmdline.txt"; DestDir: "{app}\help" ; Components: help
Source: "doc\Comments on KNT file formats.txt"; DestDir: "{app}\help" ; Components: help
Source: "misc_files\sample.knt"; DestDir: "{app}\help" ; Components: help

; {app}\templates
Source: "misc_files\Meeting template - sample.rtf"; DestDir: "{app}\templates"; Flags: onlyifdoesntexist

; {app}\plugins
Source: "plugins\examples\funckey.knl"; DestDir: "{app}\plugins" ; Components: plugins
Source: "plugins\examples\funckey_readme.txt"; DestDir: "{app}\plugins" ; Components: plugins
Source: "plugins\examples\kncalendar.knl"; DestDir: "{app}\plugins" ; Components: plugins
Source: "plugins\examples\readme.txt"; DestDir: "{app}\plugins" ; Components: plugins

; confirmoverwrite promptifolder


[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppFileExe}"
Name: "{group}\{#AppName} GitHub"; Filename: "https://github.com/dpradov/keynote-nf"
Name: "{group}\{#AppName} GitHub - Issues"; Filename: "https://github.com/dpradov/keynote-nf/issues"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#AppFileExe}"; Tasks: desktopicon

; Links to help files
Name: "{app}\KeyNote Help 1.6"; Filename: "{app}\{#AppFileExe}"; Parameters: "Profiles\Help\keynote.ini  help\kntHelpFiles\KN_Help_1.6.knt"; Components: help; IconFilename: "{app}\Profiles\Help\keynote_hlp.ico"
Name: "{app}\KeyNote Handbook"; Filename: "{app}\{#AppFileExe}"; Parameters: "Profiles\Help\keynote.ini  help\kntHelpFiles\KN_Handbook_1.6.knt"; Components: help; IconFilename: "{app}\Profiles\Help\keynote_hlp.ico"
Name: "{app}\help\KeyNote Help chm"; Filename: "{app}\keynote.chm"; Components: help
Name: "{app}\help\KeyNote Help 1.6"; Filename: "{app}\{#AppFileExe}"; Parameters: "Profiles\Help\keynote.ini  help\kntHelpFiles\KN_Help_1.6.knt"; Components: help; IconFilename: "{app}\Profiles\Help\keynote_hlp.ico"
Name: "{app}\help\KeyNote Handbook"; Filename: "{app}\{#AppFileExe}"; Parameters: "Profiles\Help\keynote.ini  help\kntHelpFiles\KN_Handbook_1.6.knt"; Components: help; IconFilename: "{app}\Profiles\Help\keynote_hlp.ico"

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

