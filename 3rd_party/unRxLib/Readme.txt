Unofficial version Rx library
for Delphi 2005/2006/2007/2009/2010/XE/XE2/XE3/XE4/XE5/XE6/XE7/XE8/Seattle/Berlin/Tokyo/Rio/Sydney/Alexandria

DISCLAIMER:
 * This software is provided "as is" and is without warranty of any kind.
   The author(s) of this software does not warrant, guarantee or make any
   representations regarding the use or results of use of this software
   in terms of reliability, accuracy or fitness for purpose. You assume
   the entire risk of direct or indirect, consequential or inconsequential
   results from the correct or incorrect usage of this software even if the
   author(s) has been informed of the possibilities of such damage. Neither
   the author(s) nor anybody connected to this software in any way can assume
   any responsibility.
 * All rights held by the author(s) or owner(s) of units or documents.

Update 1.21

 1/ Update for Delphi 11 Alexandria
 2/ Revision 64 bit. conditionals
_______________________________________________________________________________

Update 1.20

 1/ Update for Delphi 10.4 Sydney
_______________________________________________________________________________
Update 1.19

 1/ Update for Delphi 10.3 Rio
_______________________________________________________________________________
Update 1.18

 1/ Update for Delphi 10.2 Tokyo
_______________________________________________________________________________
Update 1.17

 1/ Update for Delphi 10.1 Berlin
_______________________________________________________________________________
Update 1.16

 1/ Update for Delphi 10 Seattle
 2/ Some improvements of code
_______________________________________________________________________________
Update 1.15

 1/ Update for Delphi XE8
_______________________________________________________________________________
Update 1.14

 1/ Update for Delphi XE7
_______________________________________________________________________________
Update 1.13

 1/ Update for Delphi XE6
_______________________________________________________________________________
Update 1.12

 1/ Update for Delphi XE5
 2/ New component TRxPanel added
_______________________________________________________________________________
Update 1.11

 1/ Update for Delphi XE4
_______________________________________________________________________________
Update 1.10

 1/ Update for Delphi XE3

_______________________________________________________________________________
Update 1.09

 1/ Delphi package source actualization
 2/ Delphi 6 conditional corections
 3/ TRxFindFiles support class added
 4/ Corrections for default (ENG) lang. resource
_______________________________________________________________________________
Update 1.08

 1/ file case name unit corrections.
 2/ only one language file can be used in your applications like:
      a) in Rx.inc activate {$DEFINE _LNG_ONE_}.
      b) activate yours own language in new include RxLangDef.inc like
          {$DEFINE RXLANG_Cze}.
         note: Languages different from English locates in utf-8 files, may
          be editor problem for lower version Delphi than 2005.
      c) in your project use global conditional define like RXLANG_MYLANG
          (it activate your lang file only).
      d) rebuild your application, it will be smaller with one your language
          mutation for RxLibrary only.
 3/ repair malfunction of TColor property (big thanks to Remy Lebeau).
 4/ many new constant color names added into module RxColors
     (+ 229 named constant).
 5/ new component TRxThread added for better access.
 6/ repair malfunction of property caption editor.
 7/ new components TRxAnimBitBtn, TRxAnimSpeedButton added.
 8/ repair malfunction with styles in TRxProgress.
 9/ refresh code in TRxDBGridSorter.
10/ adopted 20 function utilities by Alexey Popov into module RxProps.
11/ activate Align property in TRxSpinButton.
12/ rename parameter Name to AName in define event TExecOpenDialogEvent,
     because occur names conflict.
13/ repair conflict in string property in module RxTranslate for unicodes.
14/ some functions added into module RxVerInf for better work with versions.

Note for users Delphi 5/6/7:
----------------------------
RxLibrary is not directly designed for this Delphi versions. When you will
want compile source code in this versions of Delphi, you have to open all
units contain form (*.dfm) and resave it for resource compatibility (crash
prevent IDE) before rebuild and install into IDE. Some functionality will
be lost.

Note for users Delphi XE2 (64 bit.ver):
---------------------------------------
This source code is 64 bit ready but untested. BDE packs must be removed from
64 bit project as unsuported technology.

Note for users CBuilder:
------------------------
This source code is CBuilder (2006/2007) ready but uncomplete and untested yet.
Releases February 29, 2012

_______________________________________________________________________________
Update 1.07

1/ Update all packages in unit scope.
2/ Change namespace in RxViewer unit for XE2.

_______________________________________________________________________________
Update 1.06

1/ repair malfunction of TFormStorage under Unicode Delphi.
2/ update packages file for Delphi 2005 - XE.
3/ update package for Delphi XE2 32 bit.

_______________________________________________________________________________
Update 1.05

1/ better compatibility with Delphi 2009.
2/ new adopted component for view any supported files.
3/ convert dfm files to text.

_______________________________________________________________________________
Update 1.04

Updated:
1/ small mistake in RxDBCtrl.pas (wrong show DBGrid).
2/ activate tables in general RxDemo.
3/ replacement deprecated FileAge() in RxFileUtils.

_______________________________________________________________________________
The initial release(s) no included