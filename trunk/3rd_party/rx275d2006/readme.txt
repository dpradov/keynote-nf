DPV (27 sep 2008): Use of msftedit.dll
---------------------------


  RX Library 2.75 port to Delphi 2006 (Win32), v1.0 (by Oleg Fedorov)
  ====================================================================

TABLE OF CONTENTS
-----------------
  Overview
  Compatibility with your older code
  History
  Installation
  Help files
  Copyright Notes


Overview
--------
  This port is a non-commercial product. Feel free to distribute it as
long as all files are unmodified and kept together.

  The authors disclaim all warranties as to this software, whether express
or implied, including without limitation any implied warranties of
merchantability or fitness for a particular purpose. Use under your own
responsibility, but comments (even critique) in English (or in Russian)
are welcome.


Compatibility with your older code
-----------------------------------
  - StrUtils is renamed to rxStrUtils to avoid the names conflict with Borland's StrUtils module.
  - RxGrids.TInplaceEditStyle type is replaced with Delphi 2006 TEditStyle type.


History
-------
14 December 2005 - version 1.0
  First version. 

Installation
------------
If you have Delphi 2006 Professional or Personal Edition, deactivate the
conditional define {$DEFINE DCS} in the RX.INC file before compiling the
library.

Use "File\Open..." menu item of Delphi IDE to open the project rxctl2006.bdsproj. 
In "Project Manager" window select "Compile" from popup menu to
compile package rxctl2006.bpl. After compiling repeat that for the following
projects: rxdb2006.bdsproj and rxbde2006.bdsproj. Put compiled BPL files
into directory that is accessible through the search PATH (i.e. DOS
"PATH" environment variable; for example, in the Windows\System directory).
After compiling RX run-time packages you must install RX design-time
packages into the IDE.

Use "File\Open..." menu item to open RX design-time package dclrx2006.bdsproj.
In "Project Manager" window select "Install" from popup menu to register
RX Library components on the component palette. Repeat that for other RX
Library design-time packages dclrxdb2006.bdsproj and dclrxbd2006.bdsproj.

Copyright Notes
---------------
  RX Library is a copyright of http://www.rxlib.com team: 
    Fedor Kozhevnikov  (fkozh@iname.com)
    Igor Pavluk        (igorp@mail.com)
    Serge Korolev      (korolev@usa.net)

  Delphi 2006 port made by:
    Oleg Fedorov      (http://www.oxygensoftware.com)

