#Notes about translating KeyNote
Translation based on the utility 'Kryvich Delphi Localizer' (https://sites.google.com/site/kryvich/localizer)

##KeyNote.Lan
This is a configuration file containing the list of languages supported. For each language it is included 
some data such as name and files from the translation of interface and translation of tips. These files should
be in the Lang subfolder

Example:
```
[Spanish]
Comment="Partially translated"
Name="Spanish (Castellano)"
LangFile="Keynote.spanish.lng"
TipFile="Keynote.spanish.tip"
Translator="......@gmail.com"
```
<br>

##Translation of interface. LNG files

This is an example of a partial translation:

Original:
```
[TAboutBox]
Caption=About
BTN_Close.Caption=&Close
Label3.Caption=E-mail:
Label4.Hint=Double-click to send email; Right-click to copy\+(No HTML-formatted
email, PLEASE!)
....
Pages.Tab_Tree.GBox_Tree.Label_TreeFonts.Caption= Font and background color: 
```
<br>

Translated:
```
[TAboutBox]
Caption=Acerca de
BTN_Close.Caption=&Cerrar
Label3.Caption=E-mail:
Label4.Hint=Haga doble clic para enviar email; Clic con botón derecho para copiar
...
Pages.Tab_Tree.GBox_Tree.Label_TreeFonts.Caption= Fuente y color de fondo: 
```
<br>
Sections, in square brackets, ((TAboutBox], for example) refer to forms. This may help put
in context the phrases and thus help in translation.
It should be remembered that sometimes, at the end of the phrases there are one or more 
spaces, which should be respected. For example, after "Font and background color:" there 
is a space.
As recalled at the beginning of the files, the line breaks are shown with some
special characters for easy reading:
```
HumanizedCR=\^
HumanizedCRLF=\+
```
<br>
At the end of the file is a section called [ResourceStrings], which includes all
strings (which makes sense to translate) that have been taken from the code:

```
[ResourceStrings]
64465_kn_INI_STR_INIMail_01=Fichero adjunto: %F
....
[ResourceStrings]
64465_kn_INI_STR_INIMail_01=Attached file: %F
```
<br>

In the files .LNG you don't have to translate all the text. You may keep strings
untranslated. It is also possible (althougth not recommended, to facilitate improving
the translation) to remove some of the lines. Those ones that are not present will simply
not be translated.
<br>

##Translation of tips. TIP files
Each line of the file .TIP file corresponds to an individual tip. To translate the file it
is only needed to translate the text of these lines.
<br>
<br>

##Creating or updating languages files
To keep translation files (.LNG) working and up to date, it is necessary to update the files 
contained in the Lang folder.

- Make sure that 'keynote.exe' and 'keynote.drc' are copied to 'Lang' folder
    To get .exe and .drc (Delphi Resource String File): 
     - In Delphi Environment open menu Project | Options | Linker and set MAP file to Detailed.
     - Build the project. EXE and DRC files will be created.
- Execute "kdl32_updatelng.cmd" 

Because of optional switch "-!" set in the .cmd file, untranslated (new & modified) strings 
will be marked with "(!)". This way it is easy to find out the new strings in a language file. 
After translation completion all these marks should be removed from language files.

Similarly, because of the use of "-x" switch, not used strings (obsolete & deleted from the 
application) in an output language file marked with "(x)" mark. After translation completion all 
these not used strings should be removed entirely.

For more information, see [HowTo](../3rd_party/kdl32_Kryvich's%20Delphi%20Localizer/Help/howto.htm)
and [FAQ](../3rd_party/kdl32_Kryvich's%20Delphi%20Localizer/Help/faq.htm) of Kryvich Delphi Localizer
