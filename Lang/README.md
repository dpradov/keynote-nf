# Notes about translating KeyNote
Translation based on the utility 'Kryvich Delphi Localizer' (https://sites.google.com/site/kryvich/localizer)

## KeyNote.Lan
This is a configuration file containing the list of languages supported. For each language it is included 
some data such as name and files from the translation of interface and translation of tips. These files should
be in the Lang subfolder

Example:
```
[Dutch]
Comment=Translation by Ennovy and Plankje
Name=Dutch
LangFile=Keynote.dutch.lng
TipFile=Keynote.dutch.tip
Translator=http://forum.goeiedageven.nl/
```
<br>

## Translation of interface. LNG files

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
Label4.Hint=Doble clic para enviar email; Clic derecho para copiar\+(Sólo email en texto plano, POR FAVOR)
...
Pages.Tab_Tree.GBox_Tree.Label_TreeFonts.Caption= Fuente y color de fondo: 
```
<br>
Sections, in square brackets, ((TAboutBox], for example) refer to forms. This may help put
in context the phrases and thus help in translation.
It should be remembered that sometimes, at the beginning and/or end of the phrases there are one or more 
spaces, which should be respected. For example, after "Font and background color:" there 
is a space.
As recalled at the beginning of the files, the line breaks are shown with some
special characters for easy reading:

```
HumanizedCR=\^
HumanizedCRLF=\+
```

At the end of the file is a section called "[ResourceStrings]", which includes all
strings (which makes sense to translate) that have been taken from the code:

```
[ResourceStrings]
...
64764_knt_RS_sMacM48=Ir a la línea
64765_knt_RS_sMacM49=Introduzca número de la línea o incremento (+-):

....
[ResourceStrings]
64764_knt_RS_sMacM48=Go to line
64765_knt_RS_sMacM49=Enter line number or increment (+-):
```
<br>

In the files .LNG you don't have to translate all the text. You may keep strings
untranslated. It is also possible to remove some of the lines. Those ones that are not present will simply
not be translated.
<br>
<br>

## Translation of tips. TIP files
Each line of the file .TIP file corresponds to an individual tip. To translate the file it
is only needed to translate the text of these lines.
<br>
<br>


## Updating an existing LNG file

Starting from existing .LNG files, the developer will update the .lng files to reflect
the changes included since the last version (if any), marking with the help of a utility
the strings that have been modified, the new ones as well as those that may have been removed.

As a translator, what you will need to do is review these changes:

### New strings
They are identified by displaying the "(!)" mark at the beginning of the line (and not having "{1}"
just before the "=" character).
They may correspond to completely new sections, if new forms have been added, such as:
```
[TForm_Image]
(!)Caption=Image properties
(!)btnCreateFile.Hint=Creates a file with the image content
...
```

or they can simply be new texts available in the code or in existing forms. Example:

```
[ResourceStrings]
...
(!)64381_knt_RS_sFile21=OK to deduce the missing date information?\^
(!)64382_knt_RS_sFile22=OK to remove date from note name?\^
...
```


### Removed strings
There may be strings that are no longer needed, perhaps because a certain functionality is removed or 
substantially modified, as is the case with the TForm_Mail form:

```
[(x)TForm_Mail]
(x)Caption=Zend notitie via E-mail
...
```

Occasionally, strings are deleted because they are removed from the interface as static text and 
are constructed or assigned dynamically. In such cases, it is possible that a string exists in the 
"ResourceStrings" section that includes all or part of that text. That new string will be marked with "(!)".
For example:

```
[TAboutBox]
...
(x)Label4.Hint=Dubbelklikken voor verzenden email; Rechtsklikken voor kopiëren\+(Alleen email met platte tekst, AUB!!)
...
[ResourceStrings]
...
(!)65070_knt_RS_sAB00=About - 
(!)65071_knt_RS_sAB01=Double-click to send email; Right-click to copy\^(No HTML-formatted email, PLEASE!)
..
```

You will need to remove from the file the lines corresponding to those deleted strings, marked with (x).


### Modified strings

Strings that have been modified since the last translation reflected by the .LNG file will be marked with "(!)"
at the beginning of the line and "{1}" just before the "=" character. For example:

```
(!)Panel_Main.Image1.Hint{1}=Created with Delphi 11 Community Edition
(x)Panel_Main.Image1.Hint=Gemaakt met Borland Delphi 2006
```

You can see that after the line marked as modified, which is shown with the original text (in English and as it
appears in the keynote.exe file), the line with the previous translation is offered. This makes it easier
to recognize what the change is and adjust the translation.
In case it might be helpful to see what the original text was in the previous version
(Ex: "Panel_Main.Image1.Hint=Created with Borland Delphi 2006") the main file with the latest changes (keynote.lng)
and the same one from the previous version (keynote_OLD.lng) will always be offered. You can locate the string 
from its code: "Panel_Main.Image1.Hint"

In many cases the quickest way will be to modify the line with the existing translation (marked with (x))
and simply delete the other one. In other cases, if the change is major, it may be better to simply translate
the new one from scratch. In the example, we could modify the second line and delete the first one:
```
Panel_Main.Image1.Hint=Gemaakt met Delphi 11 Community Edition
```

Sometimes the modification of some strings responds to a change in terminology in the application, such as
the one made in version v 1.9.2: Simple notes / Tree Notes + nodes ==> "Folders" with "notes"
Thus, for example:

Keynote_OLD.lng 
```
64760_knt_RS_sMacM44=This action cannot be performed, because there is no active note (%d)
64761_knt_RS_sMacM45=This note cannot be set as Read-only, because it is being used for clipboard capture.
```

Keynote.lng 
```
64760_knt_RS_sMacM44{1}=This action cannot be performed, because there is no active folder (%d)
64761_knt_RS_sMacM45{1}=This folder cannot be set as Read-only, because it is being used for clipboard capture.
```

This modification is reflected in the Keynote.dutch.lng file as:
```
(!)64760_knt_RS_sMacM44{1}=This action cannot be performed, because there is no active folder (%d)
(x)64760_knt_RS_sMacM44=Deze actie kan niet worden uitgevoerd, omdat er geen actieve notitie (%d) is.
(!)64761_knt_RS_sMacM45{1}=This folder cannot be set as Read-only, because it is being used for clipboard capture.
(x)64761_knt_RS_sMacM45=Deze notitie kan niet worden ingesteld als alleen-lezen, omdat het in gebruik is als klembordopname.
```

* **Remember**: It is possible to leave lines untranslated, as well as to remove some untranslated lines, but you cannot keep
  lines marked with (x) or (!) in the final .LNG file. It is possible to leave the "{1}" marks, although I recommend
  removing them in the final file (just replace "{1}=" with "=")

* It is **very important** to respect special characters included in strings, especially those such as "%s" or "%d",
  for example. If they are removed or added unnecessarily, an exception will be thrown when the application uses that string,
  since there may no longer be a correspondence between the terms to be replaced ("%s" or "%d") and the strings that the
  application will use for that purpose.
  
  This is actually one reason why .lng files from older versions should not be used, as they may contain strings with
  different usage of these replacement markers, thus causing exceptions.
  As an example of string:
```
64743_knt_RS_sSty02= %s, %s space, %s, L:%d F:%d R:%d, Bef:%d Aft:%d
```

## Creating or updating languages files (Developers)
To keep translation files (.LNG) working and up to date, it is necessary to update the files 
contained in the Lang folder.

- Make sure that 'keynote.exe' and 'keynote.drc' are copied to 'Lang' folder
    To get .exe and .drc (Delphi Resource String File): 
     - In Delphi Environment open menu Project | Options | Linker and set MAP file to Detailed.
     - Build the project. EXE and DRC files will be created.
- Execute "kdl32_updatelng.cmd" 

Because of optional switch "-!" set in the .cmd file, untranslated (new & modified) strings 
will be marked with "(!)". This way it is easy to find out the new strings in a language file. 
Modified strings will also marked with "{1}". In previous section (Updating an existing LNG file) 
have been shown some examples.
After translation completion all these marks should be removed from language files.

Similarly, because of the use of "-x" switch, not used strings (obsolete & deleted from the 
application) in an output language file marked with "(x)" mark. After translation completion all 
these not used strings should be removed entirely.

Note that the main language file, used as a reference to identify changes, "keynote.lng", will also
be modified after executing "kdl32_updatelng.cmd": modified strings will be marked with "{1}".
Once language files are translated it is convenient to remove al these {1} marks, in keynote.lng
and the rest of .lng files. New and removes strings are not shown in "keynote.lng" but reflected in
the rest of language files.


For more information, see [HowTo](../3rd_party/kdl32_Kryvich's%20Delphi%20Localizer/Help/howto.htm)
and [FAQ](../3rd_party/kdl32_Kryvich's%20Delphi%20Localizer/Help/faq.htm) of Kryvich Delphi Localizer
