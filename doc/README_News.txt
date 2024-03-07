
===========================================================
  KeyNote NF: NEWS
===========================================================


------------------------------------------------------------
07 mar 2024
------------------------------------------------------------
Release 1.9.0 is signed with a code signing certificate
KeyNote NF help have been completely revised/updated, based on its own
.knt format. 
KeyNote NF can be used to generate help files for other applications, 
as an alternative to .chm files, with the help of a new add-on utility,
KntLauncher.exe

------------------------------------------------------------
11 dec 2023
------------------------------------------------------------
A significant improvement in image management is incorporated in version 1.8.1

------------------------------------------------------------
 09 JUn 2023
------------------------------------------------------------
Adaptation from Borland Delphi 2006 to to Embarcadero Delphi CE 11.3
(Community Edition 11.3  Alexandria).

I have had to revise and modify many units, specific from KeyNote but 
also from 3rd Party components and libraries. 
In some cases it was easier to stop using some 3rd party components 
and start using internal components in Delphi, custom coding certain 
aspects no covered (like TDfsStatusBar). In others I found much better 
(or neccesary) to look for a more recent version, as with 'ICS Internet
Component Suit' or 'Kryvich Delphi Localizer'. 
I have also changed Rx Library (RxRichEd,..) with UnRxLib. It is a
version based in Rx Library 2.71, adapted to actual Delphi IDEs. KeyNote
was based in Rx Library 2.71 also, with some changes and corrections
over time, for example to adapt to Unicode. But the adaptation of UnRxLib
to Unicode and newer versions is better. So I preferred to rebase in that
library and apply on it the changes (and some correction) still needed.

From Delphi 2009 there was complete support to Unicode, also in VCL controls, 
so there was no need to use TntControls, with which I got to give 
unicode support in 2009 (on Delphi 2006). But that change since Delphi 2009
also implied a serious revision of all the code, because of the different
management of the strings (Char, WideChar, String, WideString, PChar, ...)

In his process I have decided to drop support for the 'Dart format'. I have 
enclosed the code in a conditional compilation ({$IFDEF WITH_DART}), but 
I have not revised that code during the adaptation. In the case that someone
needs to reuse a file in that format, can use an older version of KeyNote
to convert to normal KeyNote format.


------------------------------------------------------------
 23 Aug 2015 
------------------------------------------------------------
Since august 2015 the project is hosted in GitHub

   https://github.com/dpradov/keynote-nf
   
The active forum is restarted in the following direction:

  http://keynote-newfeat.sourceforge.net/forum/
  
Old forums and group becomes read only:

  https://sourceforge.net/p/keynote-newfeat/discussion/
  http://groups.google.com/group/keynote-nf


------------------------------------------------------------
 29 Dic 2008  (Version: 1.7.4)
------------------------------------------------------------
From version 1.7.4 the program is renamed as "KeyNote NF"
					      

------------------------------------------------------------
 28 Nov 2008  (Version: 1.7.3 Rev.3)
------------------------------------------------------------
Issues and posts have already been migrated to Google Code. 
There is also a discussion group.
                                               
  http://code.google.com/p/keynote-nf/  
  http://groups.google.com/group/keynote-nf  


The project is still available in SourceForge, but only to host new binaries:

  http://sourceforge.net/projects/keynote-newfeat/



=================== IMPORTANT ==============================
Version: 1.7.0
Developer: Daniel Prado Velasco  (dprado.keynote@gmail.com)
============================================================

Project KeyNote was closed in 2005 by Marek.

At http://www.tranglos.com/free/keynote.html appeared a notice:
"20 Oct 2005: All projects are closed down"

In November 2007 I've decided to continue with it adding new functionality,
at first according to my own needs (I use KeyNote intensively at work).

The new features included from version 1.7.0 are described in "History.txt".
The more important one is the capacity to hide or show nodes:

· Filtering tree nodes by search criterions (Resource Panel) 
· Hiding checked nodes 
· Setting alarms on nodes 

Daniel Prado Velasco (Spain) <dprado.keynote@gmail.com>





-----------------------------------------------------------
KEYNOTE
Version   : 1.6.5
Copyright : (C) Marek Jedlinski 2000-2002 (Freeware)
Released  : 28 July 2000
License   : Mozilla Public License (source code available)
-----------------------------------------------------------

HELP AND DOCUMENTATION:

The Help file is incomplete and partly outdated.

PLEASE READ THE FAQ (Frequently Asked Questions) before
emailing for support. The FAQ is available at
http://freeware.tranglos.com/free/keynote_faq.html

Other sources of information:

Primary URL:
http://www.tranglos.com/free/
(here you can subscribe to mailing lists)

Frequently Asked Questions:
http://freeware.tranglos.com/free/keynote_faq.html

Support / discussion forums:
http://forum.tranglos.com

KeyNote 2.0 Development FAQ:
http://www.tranglos.com/free/knt2devfaq.html

View and report bugs:
http://mantis.tranglos.com

SourceForge (KeyNote 2.0 development):
http://sourceforge.net/projects/keynote/

The file "history.txt" is the most important file to look at,
especially if you are upgrading from a previous version.
New users can also find this file helpful, as it contains
detailed information about *all* KeyNote features added since
the program was first released.

The file "bugs.txt" contains a list of known bugs and limitations
of the program. Windows NT and Windows 2000/ME users: you MUST
read this file!

"sample.knt" - this file is opened automatically when you
start KeyNote for the first time. It contains many tips,
features, keyboard shortcuts, and showcases some of KeyNote's
capabilities. DO NOT USE THIS FILE FOR PERSONAL NOTES! It will
be overwritten when you upgrade Keynote to a new version.

All these and several other files are located in the
"\DOC" subdirectory, below the directory where you
installed KeyNote. Please browse through these files
before you send a bug report or a support question.
Thank you!


-----------------------------------------------------------

FURTHER INFORMATION:

There are two mailing lists which you can subscribe to,
to keep up with the latest news about KeyNote and other
freeware programs I wrote. One is an announcement-only
list (note more than 2 messages a month; information
about major upgrades and additions). The other is an
unmoderated discussion list. To subscribe to either list,
please go to the General Frenetics Freeware website:

http://www.tranglos.com

and click the "Mailing lists" link.

-----------------------------------------------------------

IMPORTANT FOR USERS UPGRADING VERSION EARLIER THAN 0.90

As of version 0.90, KeyNote supports two major new features:
encrypting the KeyNote files and creating Tree-type notes.
These features are incompatible with any previous release
of KeyNote. Therefore, encrypted files or files which contain
Tree-type notes cannot be opened using any version of KeyNote
earlier than 0.90. However, any file that does NOT use these
features will be automatically saved in a format that is
compatible with earlier releases of KeyNote.

-----------------------------------------------------------

SOURCE CODE:

KeyNote is an open-source program, distributed under Mozilla Public
License (MPL). You can download full KeyNote source code from
	http://sourceforge.net/projects/keynote/
(Borland Delphi 3.0 required to compile. You may need to install some third-party components, some of which are included in the distribution.)

-----------------------------------------------------------

Please see file "acknowledgments.txt" for a list of third-party
components without which I would certainly not have been able
to take KeyNote as far as it is today.

-----------------------------------------------------------
CONTACTING THE AUTHOR
-----------------------------------------------------------

Primary URL:
http://www.tranglos.com

Primary email address:
marek@tranglos.com

Alternate email address:
marekjed@users.sourceforge.net

NOTES:
Please do NOT send HTML-formatted email to any of the
addresses above. All HTML-formatted email will be
bounced to sender. It will NOT be read or answered.

Please do NOT send any irrelevant attachments (.vcf cards,
graphic backgrounds, "winmail.dat" files, etc). Attachments
are OK if they are directly relevant to a program you are
contacting me about: for instance, if you are reporting a bug,
you are welcome to attach a screenshot where the bug is apparent
(but please send GIF or JPG images only, no bitmaps unless they
are compressed!).

PGP signatures are welcome.

Please do not request that files be sent to you via email.
I have a rather slow and metered connection. Since all the
software I offer for downloading is free, I would really
like to avoid any additional costs. Whatever is available
can be downloaded directly from the website.

Thank you.
-----------------------------------------------------------
Questions, comments, suggestions: email
<marekjed@pobox.com>
No HTML-formatted email please!
