RX Library 2.75
===============

The Set of Native Delphi Components for Borland Delphi
versions 1, 2, 3, 4 & 5 and Borland C++ Builder 1, 3 & 4.
100% Source Code.

Last revision date Oct 12, 1999.

PLEASE FOLLOW THE INSTRUCTIONS PROVIDED IN THE INSTALLATION SECTION!


TABLE OF CONTENTS
-----------------
Latest Changes
Overview
History
License Agreement
Installation
Demonstration Programs
Source Files
Using GIF Images
Copyright Notes


NEW FOR VERSION 2.75
--------------------

Delphi 5.0 & C++Builder 4.0 Compatibility

New components:
 TRxLoginDialog

New properties, events:
 TFormPlacement.RegistryRoot
 TFormPlacement.Version
 TFontComboBox.UseFonts
 TRxDBGrid.OnTopLeftChanged
 TRxDBLookupCombo.DisplayValues
 TStrHolder.Macros, TStrHolder.OnExpandMacros
 RxSpin.TValueType.vtHex

New routines, methods, constants:
 SaveClipboardToStream, LoadClipboardFromStream (clipmon.pas)
 AppFileName, AppVerInfo (rxverinf.pas)
 XorString, XorEncode, XorDecode (strutils.pas)

BUG FIXES.

Overview
--------

RX Library contains a large number of components, objects and routines
for Borland Delphi with full source code. This library is compatible
with Borland Delphi 1, 2, 3, 4, 5 and Borland C++ Builder 1, 3, 4.

This collection includes over 60 native Delphi components.

RX Library is a freeware product. Feel free to distribute the library as
long as all files are unmodified and kept together.

The authors disclaim all warranties as to this software, whether express
or implied, including without limitation any implied warranties of
merchantability or fitness for a particular purpose. Use under your own
responsibility, but comments (even critique) in English (or in Russian)
are welcome.

1. Components:

TRxDBLookupCombo provides an incremental search through lookup list by
directly typing into the combo control while the lookup list is
displayed, LookupSource can refer to TTable, TQuery, TRxQuery or
TQBEQuery. It even incrementally searches on the query results and
much more...

TRxDBLookupList is the same as TRxDBLookupCombo (in functionality).

TRxDBComboBox is a TDBComboBox descendant allowing to have different
values displayed from that stored in database without using a lookup
table. Allows you to display understandable text versions of stored
codes in your table.

TRxDBGrid provides you with the ability to change the background color
and font displayed within individual cells and entire rows and columns;
save and restore columns order and display width in ini-files and system
registry; display icons for BLOB, memo, OLE and picture fields;
select multiple records; convert columns headings to buttons.

TDBStatusLabel displays the DataSet state (for all datasets) or
current record number (for DBase or Paradox tables).

TDateEdit and TDBDateEdit (data-aware version) allows direct typing and
has a button to bring up calendar in popup window (combo-box alike) or
in a dialog.

TQBEQuery enables Delphi applications to use Paradox-style
Query-by-example (QBE) statements to query tables, and perform insert
and update queries.

TRxQuery is a TQuery descendant. It supports macros in the SQL text,
which are similar to Params. Macros allow to change query text easily
and handy.

TSQLScript allows multiple SQL statements in one query.

TRxDBFilter encapsulates BDE ability to filter records locally.
The component provides event on filtering and/or conditions in
StringList property.

TDBProgress displays BDE operations progress for IDAPI drivers
that support callback-functions.

TDBIndexCombo is a visual interface component that provides your end-
users with an easy means of changing the current display order of data
being retrieved from an indexed table. When the user selects the
TDBIndexCombo component, it's drop-down selection list is populated
with the DisplayNames of all indexes available for the table it's
assigned to.

TBDEItems, TDatabaseItems, TTableItems are lists populated by the
corresponding BDE information (database list, table list, field list etc).

TDBSecurity provides most common dialogs for database applications:
Login Dialog and Change Password Dialog.

TRxRichEdit is a wrapper for a Windows rich text 2.0 & 3.0 edit control
with OLE objects support (Delphi 2.0 or higher).

TRxDBRichEdit - permits the user to display and enter Rich Text Format
(RTF) data in a memo or BLOB field (only for Delphi 2.0 or higher).

TAnimatedImage - animation "bitmap by bitmap". Design-time editor allows
you to load Windows animation cursors (ani-files) into this component.

TClipboardViewer is a visual control that displays the contents of
Clipboard using different formats.

TCurrencyEdit - input of money amounts, and other numbers too.

TRxCalcEdit and TRxDBCalcEdit (data-aware version) take the display
and editing of numeric data one step further: they provide a popup
calculator to help you calculate the number.

TPicClip represents a bitmap as the logical array of bitmaps
and supports access by index.

TFormPlacement allows to save and restore form size, position and window
state in/from ini-file or Registry.

TFormStorage allows you to read and write virtually any component
published property to an INI file or the system Registry with virtually no
code. Works with 3rd party and your own custom controls as well. Don't be
stuck with dozens of INI-Aware components, use TFormStorage and let it
manage all that for you.

TPageManager is useful when creating multipage dialogs such as "Wizards"
(or "Experts").

TColorComboBox is a combo box to pick up color.

TFontComboBox is a combo box to pick up font name.

TRxLabel is a customizable shadowed label.

TTextListBox is TListBox successor with automatic horizontal scrollbar
if necessary.

TRxSplitter separates two controls to allow to change their sizes at
run-time. Never again create a splitter by hand using panel components.
Add splitter support to any application by using the RxSplitter component.

TRxSlider and TRxSwitch are slider and switch controls respectively
with the ability to change their looks. TRxSlider provides more features
than the Win32 common control, such as custom thumbs.

TRxSpinEdit and TRxSpinButton are spin controls variants.

TSpeedBar with horizontal or vertical orientation that can be placed
at the top, bottom, left, right of the form. Customizable in designer
and at run-time using drag & drop operation, similar to Delphi speedbar.
Allow you use flat and transparent buttons such as standard-style
buttons. Button's transparent ability lets graphics show through from
under the speedbar.

TComboEdit, TRxDBComboEdit (data-aware version), TFilenameEdit,
TDirectoryEdit are the edit boxes with buttons. User can type data into
an edit box or can bring up dialog or popup window by clicking a button.

TMemoryTable implements BDE in-memory table as a dataset component
(with Delete operation).

TRxMemoryData is a in-memory dataset that mimics the behaviors of a regular
Delphi dataset. Unlike TMemoryTable component, TRxMemoryData is BDE
independent. This means it can still be used by any application that
replace the BDE with some other 3rd party database engines (only for
Delphi 3.0 or higher).

TRxCheckListBox is a list box with built-in check boxes.

TRxSpeedButton provides you more features than the standard TSpeedButton
component, such as Transparent, Flat, AllowTimer and GrayedInactive
properties and Drop-Down Menu.

TRxTimerList component provides all the functions of the standard TTimer
component, plus the additional benefit of using only one Windows timer
for up to 32767 timing events. You can customize this component in designer
using easy-to-use component editor.

TAppEvents is an Application wrapper component, which makes it easier to
work with the TApplication object properties and events at design time.

TRxFolderMonitor component provides notification if any changes matching
the filter conditions occur in the specified directory or subtree.

TRxTrayIcon component enables 32-bit applications to add static and
animated icons to the Windows system tray.

TRxClock, TSecretPanel, TRxDice, TRxCalculator, TStrHolder, TMRUManager,
TRxWindowHook, TRxGradientCaption, TRxLoginDialog, TRxMemoryData,
owner-draw menus and much more.

2. The editors of standard properties with some advanced features.

TPicture and TGraphic editor adds Copy and Paste Buttons, supports
Icons in Clipboard, favorites directories, uses open dialog box with
preview.

Hint property editor enables multi-line hint entry.

Project Resource Expert for Delphi 3.0 or higher and C++Builder 3.0 or
higher allows you to manage your project resource file. This expert can be
found as "View | Project Resources" in the Delphi 3 and C++Builder 3 IDE
menu and as "Project | Resources" in the Delphi 4 or higher IDE menu.

3. Units that provide functions and objects to work with databases, images,
strings, dates, files, INI-files.

4. RX Library help files in Russian only. Help files in other languages
do not exist.

5. A couple of simple demo applications.


History
-------

RX 2.75 (Oct,12,1999). Delphi 5.0 & C++Builder 4.0 compatibility.
Bug fixes for RX 2.60.

RX 2.60 (Jan,27,1999). New components TRxMemoryData, TRxRichEdit.
Bug fixes for RX 2.50.

RX 2.50 (Jul,22,1998). New components: TRxFolderMonitor, TRxLookupEdit,
TRxGradientCaption, TRxMainMenu, TRxPopupMenu. Delphi 4.0 and C++Builder
3.0 compatibility. Bug fixes.

RX 2.40 (Feb,11,1998). New components: TRxDBComboEdit, TRxCalcEdit,
TRxDBCalcEdit. GIF Image support (including new component TRxGIFAnimator
and new demo program). Project Resource Expert for Delphi 3. Bug fixes.

RX 2.32 (Jul,30,1997). Bug fixes. Full compatibility with all Delphi versions
(including Delphi 3 Professional and Developer editions) and
Borland C++ Builder 1.0. C++ Builder demo.

RX 2.31 (May,08,1997). Delphi 3.0 C/S release (build 5.53) compatibility.
Bug fixes.

RX 2.30 (Apr,22,1997). New components: TMRUList, TRxTimerList, TRxWindowHook.
Delphi 3 (confidential beta pre-release, build 3.0.4.78) compatibility.
Bug fixes.

RX 2.01 (Feb,12,1997), 2.02 (Mar,10,1997). Bug fixes. New component
TRxDBRichEdit. Version 2.02 became available for download on Internet.

RX 2.00 (Nov,24,1996). New components: TMemoryTable, TRxCheckListBox,
TRxDBComboBox, TStrHolder, TAppEvents. Bug fixes.

RX 1.32 (Nov,11,1996). Delphi 2.0 compatible version. New components:
TSQLScript, TPageManager, TRxTrayIcon.

RX 1.03 (Jan,14,1996). Compiled DCU-files removed from installation
package. New components: TFormStorage, TDBSecurity, TRxQuery, TSpeedbar,
TRxSpinButton, TRxSpinEdit, TRxCalculator.

RX 1.02 (Dec,27,1995). Private version.

RX 1.01 (Dec,04,1995). Bug fixes, new component TRxDBFilter.

RX 1.00 (Nov 1995). Initial release for Delphi 1.0 available in
russian Fido.

ExtVCL component collections from Apr,25,1995 and Jul,20,1995.


License Agreement
-----------------

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appears in all copies and
that both the above copyright notice and this permission notice appear
in supporting documentation, and that the name of RX Library authors
not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission. This
software is made available "as is", and RX LIBRARY AUTHORS DISCLAIM
ALL WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO THIS SOFTWARE,
INCLUDING WITHOUT LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND IN NO EVENT SHALL AUTHORS BE
LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, TORT (INCLUDING NEGLIGENCE) OR
STRICT LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

You can find full text of RX Library Software License agreement in the
file LICENSE.TXT.


Installation
------------

Run RXINST.EXE.

Before installing RX Library components into Delphi, check out RX.INC
file located in RX\UNITS subdirectory. This file is included in all RX
Library units and contains conditional defines that affects compilation.
You can change some of these defines or specify global compiler options
there.

1. Delphi 5.x:

Uninstall previous installed version of RX Library from Delphi 5 IDE.
Remove previously compiled RX packages (if any) RXCTL5.BPL, RXDB5.BPL,
RXBDE5.BPL, DCLRX5.BPL, DCLRXDB5.BPL and DCLRXBD5.BPL from your hard disk.

If you have Delphi 5 Professional or Standard Edition, deactivate the
conditional define {$DEFINE DCS} in the RX.INC file before compiling the
library.

Use "File\Open..." menu item of Delphi IDE to open RX' runtime
package RXCTL5.DPK. In "Package..." window click "Compile" button to
compile packages RXCTL5.DPK. After compiling repeat that for other RX
Library run-time packages RXDB5.DPK, RXBDE5.DPK. Put compiled BPL files
into directory that is accessible through the search PATH (i.e. DOS
"PATH" environment variable; for example, in the Windows\System directory).
After compiling RX run-time packages you must install RX design-time
packages into the IDE.

Use "File\Open..." menu item to open RX design-time package DCLRX5.DPK.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register RX Library components on
the component palette. Repeat that for other RX Library design-time
packages DCLRXDB5.DPK and DCLRXBD5.DPK.

NOTE: do not save package sources in the Delphi IDE.

2. Delphi 4.x:

Uninstall previous installed version of RX Library from Delphi 4 IDE.
Remove previously compiled RX packages (if any) RXCTL4.BPL, RXDB4.BPL,
DCLRX4.BPL and DCLRXDB4.BPL from your hard disk.

If you have Delphi 4 Professional or Standard Edition, deactivate the
conditional define {$DEFINE DCS} in the RX.INC file before compiling the
library.

Use "File\Open..." menu item of Delphi IDE to open RX' runtime
package RXCTL4.DPK. In "Package..." window click "Compile" button to
compile packages RXCTL4.DPK. After compiling repeat that for other RX
Library run-time package RXDB4.DPK. Put compiled BPL files into directory
that is accessible through the search PATH (i.e. DOS "PATH" environment
variable; for example, in the Windows\System directory). After
compiling RX run-time packages you must install RX design-time packages
into the IDE.

Use "File\Open..." menu item to open consistently RX design-time
packages DCLRX4.DPK (MUST be first) and DCLRXDB4.DPK.
In "Package..." window click "Compile" button to compile the package
and then click "Install" button to register RX Library components on
the "RX Controls", "RX DBAware" and "RX Tools" pages.

NOTE: do not save package sources in the Delphi IDE.

3. C++Builder 4.x:

Uninstall previous installed version of RX Library from C++Builder IDE.
Remove previously compiled RX packages (if any) RXCTL4.BPL, RXDB4.BPL,
DCLRX4.BPL and DCLRXDB4.BPL from your hard disk.

Be sure that linker option "Use dynamic RTL" is unchecked.
Use "File\Open..." menu item of C++Builder IDE to open RX' runtime
package RXCTL4.BPK. Then use "Project\Make..." or "Project\Build..." menu
item to compile package RXCTL4.BPK. After compiling repeat that consistently
for each of the other RX Library packages (.BPK files) RXDB4.BPK,
DCLRX4.BPK and DCLRXDB4.BPK (be sure to keep the sequence mentioned).
For runtime packages (RXCTL4 and RXDB4) put compiled BPL files into
directory that is accessible through the search PATH (DOS envirounment
variable, not IDE search path; for example, in the Windows\System
directory). After compiling RX packages you must install RX design-time
packages into the C++Builder IDE.

Use "Component\Install packages..." menu item to open "Packages" dialog
box. Then click "Add..." button and locate DCLRX4.BPL from the
RX\UNITS directory and click "OK" to install package into IDE. After
installing DCLRX4 package install DCLRXDB4.BPL package as above.

NOTE: do not save package sources in the C++Builder IDE.

4. Delphi 3.x:

Uninstall previous installed version of RX Library from Delphi 3 IDE.
Remove previously compiled RX packages RXCTL.DPL, RXDB.DPL, RXTOOLS.DPL,
DCLRXCTL.DPL, DCLRXDB.DPL and DCLRXTLS.DPL from your hard disk.

Use "File\Open..." menu item of Delphi IDE to open consistently RX
run-time packages RXCTL.DPK (MUST be first), RXDB.DPK and RXTOOLS.DPK.
In "Package..." window click "Compile" button to compile RX Library
run-time packages. Put compiled DPL files into directory that is
accessible through the search PATH (i.e. DOS "PATH" environment
variable; for example, in the Windows\System directory). After
compiling RX run-time packages you must install RX design-time
packages into the IDE.

Use "File\Open..." menu item to open consistently RX design-time
packages DCLRXCTL.DPK (MUST be first), DCLRXDB.DPK and DCLRXTLS.DPK.
In "Package..." window click "Install" button to register RX Library
components on the "RX Controls", "RX DBAware" and "RX Tools" pages
accordingly.

NOTE: do not save package sources in the Delphi IDE.

5. C++Builder 3.x:

Uninstall previous installed version of RX Library from C++Builder IDE.
Remove previously compiled RX packages (if any) RXCTL.BPL, RXDB.BPL,
DCLRXCTL.BPL and DCLRXDB.BPL from your hard disk.

Be sure that linker option "Use dynamic RTL" is unchecked.
Use "File\Open..." menu item of C++Builder IDE to open RX' runtime
package RXCTL.BPK. Then use "Project\Make..." or "Project\Build..." menu
item to compile package RXCTL.BPK. After compiling repeat that consistently
for each of the other RX Library packages (.BPK files) RXDB.BPK,
DCLRXCTL.BPK and DCLRXDB.BPK (be sure to keep the sequence mentioned).
For runtime packages (RXCTL and RXDB) put compiled BPL files into
directory that is accessible through the search PATH (DOS envirounment
variable, not IDE search path; for example, in the Windows\System
directory). After compiling RX packages you must install RX design-time
packages into the C++Builder IDE.

Use "Component\Install packages..." menu item to open "Packages" dialog
box. Then click "Add..." button and locate DCLRXCTL.BPL from the
RX\UNITS directory and click "OK" to install package into IDE. After
installing DCLRXCTL package install DCLRXDB.BPL package as above.

NOTE: do not save package sources in the C++Builder IDE.

6. Delphi 2.x and C++ Builder 1.0:

Use the "Install..." item on Delphi's "Component" menu to add
the RxCtlReg.PAS, RxDBReg.PAS and RxTooReg.PAS units to the component
library. These units registers all RX Library components on the
"RX Controls", "RX DBAware" and "RX Tools" pages accordingly.

7. Delphi 1.x:

Use the "Install Components..." item on Delphi's Options menu to add
the RxCtlReg.PAS, RxDBReg.PAS and RxTooReg.PAS units to the component
library. These units registers all RX Library components on the
"RX Controls", "RX DBAware" and "RX Tools" pages accordingly.

8. Help files:

The help files (in Russian only) are distributed in separate
installation packages for each version of Delphi and C++Builder.

To install the help and keyword file into Delphi 1.0 and 2.0, follow
these steps:

Click on the HelpInst icon in the Delphi group in the Program Manager.

Select File|Open from the menu. Change directories so you are in
the \Delphi\Bin directory, and select the Delphi.hdx file. Click
the OK button.

Select Keywords|Add Keyword File from the menu. Change directories
so you are in the directory where you stored the help files of the RX.
Select the RX.KWF (in Delphi 1.0) or RX32.KWF (in Delphi 2.0) file,
and click the OK button.

Select File|Save from the menu. This will recompile the Delphi.hdx
file. You can then close the Helpinst program. You will now be able to
jump to the help file for the RX Library by selecting the appropriate
property and hitting 'F1.'

Help file now available is only in Russian. This help requires Arial Cyr
and Courier New Cyr fonts (Windows code page 1251).


C++ Builder Compatibility
-------------------------

This version of RX Library is compatible with Borland C++ Builder 1.x,
3.x and 4.x.

RX Library is not tested properly with any version of Borland C++ Builder.
Use at your own risk.


Demonstration Programs
----------------------

Demonstration programs included in RX Library use tables from
DELPHI\DEMOS\DATA directory and BDE alias "DBDEMOS".


Source Files
------------

All sources (100%) of RX Library are available in RX\UNITS directory.
All language specific string constants used in RX Library are
collected in resource files. There are sources for all string resource
files in English and Russian. The rest resource files containing
bitmap images and icons for component palette are also included as
compiled resources (*.R16, *.R32) and as *.RC-files. Resource files
sources are available in RX\RESOURCE directory. So, you can use any
resource compiler (i.e. BRCC.EXE or BRCC32.EXE) to create *.RES
(*.R16 & *.R32) and *.DCR (*.D16 & *.D32) files.


Using GIF Images
----------------

To use GIF images in an your application at runtime, include the RXGIF
unit in your unit's "uses" clause.

If you are using another GIF image support library (like ImageLib),
deactivate the conditional define USE_RX_GIF in RX\UNITS\RX.INC file and
rebuild your component library.

WARNING. You will need an LZW patent license from Unisys in order to
provide end-user GIF support legally in any commercial or shareware
application. It is provided as a development tool only.


Copyright Notes
---------------

Most of the modules in our library are written by us. We have to make
a note of units based on sources of other authors.

Unit RXLOOKUP is based on original Delphi VCL units DBLOOKUP, DBCTRLS.

Unit STRUTILS is based on a unit of string-handling routines AGSLIB by
Alexey Lukin.

Unit DBEXCPT is based on a unit DBEXCEPT.PAS from Borland Delphi.

Procedures to read and write GIF files, GIF-decoding and encoding based
on freeware C source code of GBM package by Andy Key
(nyangau@interalpha.co.uk).

Some components in a unit DBLISTS are based on DELPHI\DEMOS\DB\TOOLS\BDETABLE
unit from Delphi 1.0 Demos.

Unit PICTEDIT contains advanced property editor based on unit PICEDIT.PAS
from VCL, unit QBNDDLG is a modified unit QBINDDLG.PAS from VCL, unit
IMAGPRVW.PAS is a modified unit IMAGEWIN.PAS from Borland Delphi 1.0 C/S
Demos DELPHI\DEMOS\IMAGVIEW.

  RX Library Web Site:
    http://www.rxlib.com

  Authors:
    Fedor Kozhevnikov  (fkozh@iname.com)
    Igor Pavluk        (igorp@mail.com)
    Serge Korolev      (korolev@usa.net)
