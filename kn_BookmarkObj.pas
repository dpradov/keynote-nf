unit kn_BookmarkObj;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface

uses Windows, Classes, graphics, comctrls,
  SysUtils, gf_misc, gf_files,
  kn_Const, kn_Info, kn_NoteObj, kn_NodeList;

const
  MAX_BOOKMARKS = 9; // zero-based

type
  TBookmark = record
    Name : string;
    SelStart : integer;
    SelLength : integer;
    Note : TTabNote;
    Node : TNoteNode;
  end;


type
  TBookmarks = array[0..MAX_BOOKMARKS] of TBookmark;

implementation

end.
