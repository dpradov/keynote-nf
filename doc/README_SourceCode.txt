==============================================
KeyNote NF: SOURCE CODE README
==============================================

This is the full source code for KeyNote NF program. It is distributed under the
conditions of Mozilla Public License (MPL). See file LICENSE.txt for details.
This license applies only to units written by the authors of KeyNote and KeyNote NF:
Marek Jedlinski and Daniel Prado, respectively.

Third-party units may be regulated by their own licenses.
Plase, see also the comments of Marek, in "Important notes about third-party code",
later in this document.


KeyNote NF compiles with Borland Delphi 2006.
KeyNote NF consists of two Delphi projects. The main project, keynote.dpr, 
is the base application. The other project, kntutils.dpr, generates a DLL, used by
the base application. Actually the DLL contains very little functionality.


******************************************************************************************
* IMPORTANT *. The headers of the files specific to KeyNote and written by Marek have 
  been adapted to MPL 2.0 and, at the same time, the information shown has been simplified,
  although maintaining the notices of copyright ownership. Please see notes at the bottom 
  of the file <https://github.com/dpradov/keynote-nf/blob/master/doc/References.md>
       Keep in mind that the history of the files can be queried with the help of the 
  version control system, in GitHub. This could be contrasted with the original files, 
  of KeyNote 1.6.5 (see References.md)
******************************************************************************************


HOW TO Compile
===============
To compile the program, please, keep the following steps, in the same order:

0 - (Optional)
   - I like, personally, to configure options in Borland Delphi, 
     in Environment Options > Delphi Options > Library - Win32
     so that "Package output directory" and "DCP output directory" point to a foder
	 of my preference (..Output\Bpl)
   - Save and restart Delphi
   - On enter Delphi: 
       "Your current Delphi for Win32 Package Output" is not part of your system path. 
       In order to use runtime packages that are built into this directory, this 
	   directory needs to be on your path.
	   Would you like to add it now? 
       Directory to be added: <....Output\Bpl>

     Current Path: ......
     Selecting 'Yes' will cause a user-specific 'PATH' environment variable to be created or update.
     >>
   - "Yes", and restart again

KeyNote NF uses several third party components. I have created a Packed to easily
install most of them (Components.dpk). The rest must be installed separately:

1 - Tnt Unicode Controls  (TntUnicodeVcl)
   => Compile: 3rd_party\TntUnicodeControls\Delphi\bds4\TntUnicodeVcl.dpk
   => Install: 3rd_party\TntUnicodeControls\Delphi\bds4\TntUnicodeVcl_Design.dpk

2 - Most 3rd Party Components
   => Install: Packages\Components.dpk

3 - DCPcrypt v1.3
   => Install: 3rd_party\dcpcrypt-1_3\DCP_d4.dpk

4 - RX Library 2.75 port to Delphi 2006 (Win32), v1.0
   => Compile: 3rd_party\rx275d2006\Units\rxctl2006.dpk    
   => Install: 3rd_party\rx275d2006\Units\dclrx2006.dpk
	      
Note:		  
----
To keep translation files (.LNG) working it is necessary to update the files contained in the Lang folder
Look at the README.md file included in that folder for more information.

		  

COMPONENTS REQUIRED
===================
The steps indicated will install all the components needed to compile and run KeyNote NF. 

ALL third-party components used in KeyNote are freeware and are distributed with full source code.
NOTE that they may be (and indeed are) distributed under their own licenses.

All these components can be downloaded from the Internet.

Torry's Delphi Pages
	http://www.torry.net/
	http://homepages.borland.com/torry/

Delphi Super Page
	http://sunsite.icm.edu.pl/delphi/

Delphi Free Stuff
	http://www.delphifreestuff.com/

Delphi Inspiration – Delphi Components and Software Applications:
	http://www.wikitaxi.org/delphi/doku.php/products/index
	


List of required components and libraries:
-------------------------------------------
(It is shown in square brackets the folder inside "3rd_party" where each 
 of this elements is included, inside KeyNote NF)

* TdfsBrowseDirectoryDlg   [browsedr]
  TdfsMRUFileList          [mruflist]
  TdfsStatusBar            [DFSStatusBar]
  TdfsSystemImageList      [sysimglist]
  (see Brad Stowers' Delphi Free Stuff, URL above)  
   
* RX Library (http://sourceforge.net/projects/rxlib/);   [rx275d2006]

* TPage95Control, by Ryan J. Mills   [ComCtrls95]
  
* TGFXListBox, by Wim Coetzee    [gfxlbcb]

* TToolbar97, by Jordan Russell (http://www.jrsoftware.org) [tb97_178a]

* TTreeNT, by Mike Lischke (http://www.lischke-online.de/) [treent]

* DCPCrypt, by David Barton (http://www.cityinthesky.co.uk/cryptography.html) [dcpcrypt-1_3]

* TMathParser, by The BitSoft team (http://www.bitsoft.com)  [expression_evaluator]

* TFreeWordWeb, by Antony Lewis  [wordweb]

* ICS component library, by Francois Piette (http://www.overbyte.be/) [ICS_InternetComponentSuite]

* TRichPrinter, by Gerrit Wolsink  [richprint]

* TColorPicker, by Enzo Costantini  [colorpicker]

* CRCCalculator, by Earl F. Glynn, Overland Park, KS [CRCDelphi]

* TAJBSpell, by Andrew Baylis  [ajbspeller]

* TTopMostWindow, by Stephan Schneider [topmostwindow]

* TLanguagesCombo, by Alexander Obukhov [langcombo]

* Tnt Unicode Controls, by Troy Wolbrink (http://tnt.ccci.org/delphi_unicode_controls/) [TntUnicodeControls]
	  
* Kryvich Delphi Localizer, by Kryvich, Belarusian Linguistic Software team  [kdl32_Kryvich's Delphi Localizer]
  (https://sites.google.com/site/kryvich/localizer)
  
* Delphi Fast ZLib 1.2.3, by Roberto Della Pasqua and previous (http://www.dellapasqua.com/delphizlib/)
  [Delphi Fast Zlib 1.2.3]

* UAS, UltimaShell Autocompletion Server, (flashpeak@yifan.net) [UAS]

 
* StreamIO, text-file device driver that allows textfile-style I/O on streams, by Dr. Peter Below
  [_Others\StreamIO.pas]

* StringContainers, collection of special classes for string storage and manipulation, by Mike Lischke 
 [_Others\StringContainers.pas]

* UWebBrowserWrapper, by Peter Johnson (http://www.delphidabbler.com/articles?article=14) 
 [_Others\UWebBrowserWrapper.pas]

 
If you find this list incomplete and/or need help with obtaining these components, please contact me by email.


    ** NOTE:
       The source code of original KeyNote 1.6.5 (by Marek Jedlinski) is also included insided 3rd_party folder:
       [_Keynote_1.6.5]


IMPORTANT:
--------------------
The original files of third-party code is included in .zip files inside subfolders of "3rd_party"
(In each of the subfolders, shown between square brackets, in the above list)

For example: \3rd_party\dcpcrypt-1_3\_dcpcrypt-1_3.zip contains original code of DCPCrypt, version 1.3, by David Barton.
In that folder, \3rd_party\dcpcrypt-1_3\, it is also the source files extracted from that archive, the files that KeyNote NF
is actually using. 

Have been necessary to modify some of the 3rd-party source code. All changes have been made in the extracted files, 
inside each subfolder. I have tried to mark that modifications with the comment "// [dpv]", although not always. Marek
also modified some files, with "// [mj]". I want to check all that files and make sure that all changes and clearly 
recognizable.
Anyway, it is easy to find out where the changes are, by comparing actual files (in .zip) with the original ones,
and also by looking at the history of that files in the repository of KeyNote NF, initially in Google Code,
in Subversion, and now available as Git repository in: https://github.com/dpradov/keynote-nf

		  

. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
<Notes from my first release>:

 
============================================================
Updated: 13 Nov 2007
Version: 1.7.0
Developer: Daniel Prado Velasco  (dprado.keynote@gmail.com)
============================================================
From the source code provided by Marek Jedlinski, corresponding 
to version 1.6.5 I've made the necessary corrections in order to
compile it with Delphi 2006. Among other things, it have been 
necessary to locate on the Internet some of the units of third 
party that were not included in the source code. This code is still 
being distributed under the terms of the Mozilla Public License (MPL), 
to apply to units written by Marek Jedlinski, as well as those written 
or edited by me (Daniel Prado). Third party's code is governed 
by their own licenses.

The projects "Keynote.bdsproj" and "kntutils.bdsproj" exclusively 
use files in the folder "keynote_source" as well as a large part 
of those in "keynote_source\3rd_party". Files originally in 
'modified_3rdparty_units',' misc_files' and 'support_units' have 
been kept in "Other". Some were already in the folder keynote_source,
others have been taken into account when updating some units in '3rd_party'.

So at this project as at "kntutil.bdsproj" are made explicit the 
search folders of units (not defined as a configuration of IDE).

Apart from the necessary corrections to the compilation in Delphi 2006 
and from changes made to add new features or correct some mistakes, I 
have made the following modification in order to facilitate understanding 
of source code: 
I have fragmented the file 'kn_main.pas' into multiple files, based 
on a criterion of functionality (possibly I'll make some more changes):
kn_Global.pas, kn_BookmarksMng.pas, kn_ConfigFileMng.pas, kn_NoteMng.pas, ...
(kn_...Mng.pas)

Third party's Units and components
----------------------------------
In the folder '3rd_party' it have been decompressed files in the folder
3rd_party_distributable, replacing those files included in 
modified_3rdparty_units (edited by Marek)

There have been some minor changes to files to be compiled in Delphi 2006.
Has taken advantage of one of the files "DFS.INC", slightly modified to 
accommodate new versions of Delphi; for it, it has been copied to the 
folders of other components and has been added the line {$ I DFS.INC} 
to the necessary .pas files.

Initially the changes made by me have been marked with the comment "// [dpv]". 
Since KeyNote NF is hosted in Google Code and all the changes are clearly 
visibles with Subversion I'm not marking new modifications.


Before opening Keynote projects is needed to install the following components:

*Most of the components are included in a packet named "_PaqueteTerceros"

*RX Library 2.75 port to Delphi 2006 (Win32), v1.0 (by Oleg Fedorov)
   --> 'rx275d2006. Follow the signs for the file "readme.txt" for installation

   **NOTE**: See the coment at the beginning, to date 20 sept 2009 

*DCPcrypt v1.3      David Barton (davebarton@bigfoot.com)
   --> 'dcpcrypt-1_3'  (It has been installed package DCP_d4.bdsproj)

Inside the folder "3rd_party_distributable" has been included the original
source code of individual components and units used in KeyNote. They are 
all the used.


. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
<Notes from Marek Jedlinski>:


===========================================================
KEYNOTE: SOURCE CODE README
Released: 30 June 2001
Updated: 12 Nov 2003
Version: 1.6
-----------------------------------------------------------

This is the full (or as full as possible) source code
for the KeyNote program. It is distributed under the
conditions of Mozilla Public License (MPL). See file
MPL.TXT for details. The license, of course, applies
only to units written by the author, Marek Jedlinski.
Third-party units may be regulated by their own
licenses.

-----------------------------------------------------------

DISCLAIMERS and such.

KeyNote is a work in progress. The source does contain
"dead" code and paths that lead nowhere. There are not
many, but they are there.

I am not a professional programmer. I larn things as I go.
In the process, I have re-invented the wheel many times
over, and occasionally the wheel is somewhat, well, angular.
Imperfections abound.

-----------------------------------------------------------

IF YOU NEED ASSISTANCE in compliing KeyNote, contact me.
See URls and addresses at the end of this file.


-----------------------------------------------------------

KeyNote would never have come to existence if not for
many great libraries written by other Delphi programmers.
KeyNote makes a MASSIVE use of third-party units and
components. The file "compile.txt" has a complete list
of third-party libraries required to compile KeyNote.

Important notes about third-party code:

1. In developing KeyNote, I have ONLY used third-party
units and components which are "freeware" and distributed
with full source code. All these libraries can be downloaded
for free (see Torry's Delphi Pages, the Delphi Super Page,
and Delphi Free Stuff).

2. While the third-party libraries are "free" in the common
usage of the word, they are not necessarily licensed as
"free software" or "open source software". Sometimes their
licenses may be even more open (even public domain), and
at other times, the licensing conditions may be more restrictive.
I have yet to investigate which of these third-party libraries
I can legitimately distribute with KeyNote; but you can just
download them all yourself.

3. In a few (very few) cases, I have found it necessary to
modify third-party source code. This, unfortunately, makes
it even harder to distribute those libraries. I will try to
resolve the issue as soon as possible.

-----------------------------------------------------------

CODING PRACTICES

Fair warnings. I am utterly convinced that all variables
should be initialized. (This generates tons of "value not
used" compiler hints.) I lay out code with plenty of space.
I prefer clarity over compactness. My eyesight isn't the
best in the world, and I find constructs such as
	foo:=(vara-varb)*CalcSomething(varc);
unreadable. In my source, they'll always be written	as
	foo := (( varA - varB ) * CalcSomething( varC ));
I do not write
	if not Verified then exit;
Instead, I write
	if ( not Verified ) then
		exit;
I find this much more readable, easier to follow, and
I can have a breakpoint on the 'exit;' line.


-----------------------------------------------------------

SPECIAL REQUEST

If you are interested in compiling or modifying KeyNote
source code, I would very much appreciate if you started
by taking a look at the "bugs.txt" file (installed with
KeyNote) and see if you have any ideas on how some of
the problems described there can be fixed. I have no clue
as to how these issues should be handled. Thank you.

-----------------------------------------------------------

 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net
THIS SITE IS UNUSED. It could provide a priceless resource
for the development of KeyNote, but I don't have enough time
to work on it. I'll appreciate any help with this.

 - original author's software site:
 http://www.tranglos.com

 Email addresses (at least one should be valid)
 <marekjed@pobox.com>
 <marekjed@users.sourceforge.net>
 <cicho@polbox.com>
-----------------------------------------------------------

