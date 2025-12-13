unit knt.ui.info;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface
uses
   Winapi.Windows,
   Winapi.Messages,
   System.Classes,
   System.SysUtils,
   Vcl.Controls,
   kn_Const,
   kn_Info,
   knt.model.note,
   knt.ui.editor
   ;


type
  INoteUI = interface
     ['{8D9BDE14-3373-482A-B097-0C1E4F4A981C}']
     procedure SetFocusOnEditor;
     procedure ConfigureEditor;

     function GetEditor: TKntRichEdit;
     function GetNNode: TNoteNode;
     function GetFolder: TObject;
     function GetSelectedNEntry: TNoteEntry;
     property Editor: TKntRichEdit read GetEditor;
     property NNode: TNoteNode read GetNNode;
     property SelectedNEntry: TNoteEntry read GetSelectedNEntry;

     procedure LoadFromNNode(NNode: TNoteNode; SavePreviousContent: boolean);
     procedure ReloadFromDataModel;
     function ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
     procedure ReloadNoteName;
     procedure SaveToDataModel;
     procedure CreateNewEntry(RequestedFromEditor: TKntRichEdit);
     procedure Refresh;

     procedure SetImagesMode(ImagesMode: TImagesMode);
     procedure ResetImagesReferenceCount;
     function  GetImagesInstances: TImageIDs;
     procedure ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
     procedure ReloadImagesOnEditor;

     procedure EditTags;
     procedure RefreshTags;
     procedure SetInfoPanelHidden(value: boolean);

     procedure SetReadOnly( AReadOnly : boolean );
     procedure NNodeDeleted;
     function GetNNodeDeleted: boolean;

     procedure SetOnEnter(AEvent: TNotifyEvent);
     procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
     procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);

     //procedure TestPanels;
  end;


type
  TModeEntriesUI = (
    meMultipleEntries,
    meSingleEntry
  );

  TScopeInEntriesPanel = (
    fsCurrentNode,
    fsSelectedNode,
    fsCurrentNodeAndSubtree,
    fsCurrentNodeAndAncestors,
    fsFolder,
    fsFile
  );

  TContentInMultipleMode = (
    cmOnlyHeaders,
    cmWholeEntries,
    cmOnlyFirstLines
  );

  TOrderInEntriesInPanel = (
    eoDateCreation,
    eoHierarchyAndDateCreation,       // Use hierarchy in tree + DataCreation
    eoTagsAndDateCreation             // Use TNoteAdvancedOptions.DefaultTagsOrder + DataCreation
  );

  TFilterOptionsInPanel = packed record
    TagsIncl: TNoteTagArray;          // Consider notes/entries with ALL of the selected tags in its metadata (TagsModeOR=False) (TagsText=False)
    InheritedTags: boolean;           // Each node will be considered as having its own tags and the tags of its ancestors
    ExcludeTaggedToIgnore: boolean;   // Use TFindOptions.DefaultTagsExcl  ("i")
    TextFilter : string;              // Entry to consider must include the pattern
    MatchCase : boolean;              // case-sensitive ("c")
    WholeWordsOnly : boolean;         // only match whole words ("w")
    SearchMode : TSearchMode;         // "e":Exact phrase, "&":All the words, "|":Any of the words
    ShowExcerpts: boolean;            // (when using TextFilter) "x"
  end;


  //*1 In most cases, it will not be necessary to save anything in the .knt file, and the current node will be considered.
  //   However, it will be possible to display a list of possible explicitly selected nodes on a specific panel.
  //   It will be saved in the .knt file as NNode1.GID,NNode2.GID,...  (NNode.GID.ToString)


  TPanelConfiguration = record
    Panel: TNEntriesPanel;
    Auxiliar: boolean;
    Visible: boolean;
    Scope : TScopeInEntriesPanel;
    Mode: TModeEntriesUI;
    NNodes: TNoteNodeList;             // *1
    SelectedNNode: TNoteNode;          // *1
    NEntryID: Word;
    MMContent: TContentInMultipleMode;
    MMShowDateInHeader: boolean;
    MMShowTagsInHeader: boolean;
    Order: TOrderInEntriesInPanel;
    DescendingOrder: boolean;
    Filter: TFilterOptionsInPanel;
  end;


implementation

end.

