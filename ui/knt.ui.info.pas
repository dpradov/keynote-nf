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
   Vcl.Graphics,
   kn_Const,
   kn_Info,
   knt.model.note,
   knt.ui.editor
   ;


  //*1 In most cases, it will not be necessary to save anything in the .knt file, and the current node will be considered.
  //   However, it will be possible to display a list of possible explicitly selected nodes on a specific panel.
  //   It will be saved in the .knt file as NNode1.GID,NNode2.GID,...  (NNode.GID.ToString)


type
  TStatusPanelLayout = (
     spInQL,                 // In QueryLayout,
     spInQL_ets,             //   ,,   with an entry to show  (a note with only one entry that matches the tags of a vinculated-tag panel)
     spInEL                  // In EditingLayout
   );


 {
  *2
    PanelConfiguration.SelStart --> .SSImLink

   Before support for entry excerpts was introduced, the position within each entry was always saved exactly as it was obtained from
   the editor, using Editor.SelStart. Consequently, that position depended on the current image display mode (View|Images) and on which
   images were actually visible. Even when image display was disabled, some images could still be visible if they had been inserted after
   image display had been turned off.

   This was the approach used when saving the current position within a note from NoteEntries.SavePositionInPanel (or from the equivalent
   method before support for multiple entries was implemented). It has also been, and is expected to remain, the approach used when
   obtaining the current position from GetKntLocation, which is called, for example, from TKntFolder.NodeSelected or kn_LinksMng:JumpToLocation.
     Note that JumpToLocation does not, by default, apply any image-related offset. It simply sets the editor position (SelStart) according
   to the position stored in the TLocation object, assuming that the position was originally recorded in the same way. The only exception is
   when this method is called from FindAllResults_FollowMatch. In that case, the image-related offset is computed beforehand because we know
   with certainty that the position stored by FindAllEx in the TLocation object corresponds to the note content as stored in the stream,
   rather than to a position obtained from an editor. This is necessary because FindAll searches across all notes, whether or not they are
   currently open in an editor, and when jumping to a match the target editor may have just been created, with all images visible, only some
   visible, or all hidden.

   As a consequence, if the cursor was at a given position, the user jumped to another node and later returned, the restored cursor position
   could differ slightly if the original node contained images and the set of visible and hidden images had changed. This could happen if the
   image display mode (View|Images) was changed before returning, or if image display was disabled but some images were still visible because
   they had been inserted after image display had been disabled. Those newly inserted images would no longer be visible when returning to the
   note after it had been reloaded.
   In practice, this was rarely a problem. Most of the time the cursor would not appear noticeably displaced because either the note contained
   no images or, if it did, all images were usually either visible or hidden. That state normally remained unchanged when returning to the
   node after visiting another one.

   Likewise, jumps to bookmarks are unaffected because they locate a bookmark rather than relying on a character position, so they always reach
   the exact location regardless of the current image visibility state. Find All search results are also unaffected for the reasons explained
   above. Therefore, in most situations we avoid performing unnecessary conversions between the different position formats.

   For this reason, there was no need to ensure that the editor's cursor position was stored in a particular format (imLinkTextPlain or
   imImageTextPlain). The position reported by the editor (Editor.SelStart) was simply stored, regardless of whether the editor was displaying
   all, some, or none of the images.


   However, once it became possible to display entry excerpts as the result of applying a filter to a panel, and because it became desirable
   to jump from a position within an excerpt to the corresponding position in the complete, unfiltered entry (to allow viewing or editing it),
   or conversely from a position within the complete entry to the corresponding position within the excerpts (or to the closest available
   excerpt if the exact position does not belong to any excerpt), it became necessary to ensure that the position saved for a panel and an
   entry always corresponded to a well-defined format, independent of the current image display state. This is required because, when
   performing these conversions, the current visibility state of the images is unknown, and the conversion relies on a fragment table built
   in imLink mode. And even the extracts themselves may contain images, which may or may not be visible

   From this point on, instead of saving the position in PanelConfig.SelStart, it is stored in PanelConfig.SSImLink, explicitly indicating
   that the position corresponds to the entry with all images hidden —that is, exactly as the entry is stored in the underlying model. This
   is also the format in which RunFindAllEx records search matches.

   Naturally, this requires some additional processing, both when saving the position and when restoring it to the editor. The implementation
   has been heavily optimized and is based on calculating an offset between the editor position and the corresponding position in imLink mode
   (i.e. with all images hidden and hyperlinks displayed in their place).

   Furthermore, even when an editor is displaying only excerpts, the saved position must always correspond to the full entry rather than to
   the position within the excerpts editor.

 See:
  TKntNoteEntriesUI.SavePositionInPanel, .GetImLinkPositionInEntry, .GetImLinkPositionInEntryExcerpts, .ReloadFromDataModel
 }

  TPanelConfiguration = class
    StLayout: TStatusPanelLayout;
    Panel: TNEntriesPanel;
    Hidden: boolean;                    // In QueryLayout, when no entry is available (panels not shown because of maximized other panel will not be marked as hidden)
    ShowEditorInfoPanel: boolean;
    Maximized: boolean;
    Scope : TScopeInEntriesPanel;
    Use: TNEntriesPanelUse;
    CurrentMode: TModeEntriesUI;
    NNodes: TNoteNodeList;             // *1
    SelectedNNode: TNoteNode;          // *1
    LinkedTags: TNoteTagArray;

    MECustomiz: TMEPanelCustomization;

    EntriesOnlyHeader: TNoteEntryArray;
    HiddenEntriesDisplayed: TNoteEntryArray;
    FilteredOutIgnoredEntries: TNoteEntryArray;

    SelNEntry: TNoteEntry;            // Only one per note will be saved in disk (in note's attributes)
    SSImLink : integer;               // ,,                                                                // *2
    SelLength : integer;              // ,,
    ScrollPosInEditor: TPoint;        // ,,
    ZoomCurrent: integer;

    constructor Create;
    destructor Destroy; override;
    function UseIsMultiEntry: boolean;  inline;
    function EntryModeForUse: TModeEntriesUI;  inline;
  end;


type
  INoteUI = interface
     ['{8D9BDE14-3373-482A-B097-0C1E4F4A981C}']
     procedure SetFocusOnEditor;
     procedure ConfigureEditor;

     function GetEditor: TKntRichEdit;
     function GetNNode: TNoteNode;
     function GetFolder: TObject;
     function GetSelectedNEntry: TNoteEntry;
     function GetSelectedNEntriesUI (Editor: TKntRichEdit): TObject;
     function GetNEntriesUITargetForJump(LocationObj: TObject): TObject;
     procedure GetPanelConfigOrderForFindSearch(NNode: TNoteNode; NEntry: TNoteEntry; TagsIncl: TNoteTagArray; var DescendingOrder: boolean);
     function GetNEntriesUITargetForFindSelection(NEntry: TNoteEntry; TagsIncl: TNoteTagArray = nil): TObject;
     function GetBasicNEntriesLayout: boolean;
     property Editor: TKntRichEdit read GetEditor;
     property NNode: TNoteNode read GetNNode;
     property SelectedNEntry: TNoteEntry read GetSelectedNEntry;
     function MultipleVisibleEditors: boolean;
     function NumberOfVisibleEntries(Panel: TNEntriesPanel): integer;
     function GetHideFocusFlag: boolean;
     procedure SetHideFocusFlag(value: boolean);
     property HideFocusFlag: boolean read GetHideFocusFlag write SetHideFocusFlag;
     function NavigatePanels(NavDirection: TNavDirection): boolean;
     procedure ToggleMaximizeSelectedPanel;
     procedure ShowEntriesUIPanel(Panel: TNEntriesMainPanel; Show: boolean);
     procedure PanelEmpty(Panel: TNEntriesMainPanel; WithoutVisibleEntries: boolean);

     procedure LoadFromNNode(NNode: TNoteNode; SavePreviousContent: boolean;
                             NEntriesLayout: TBasicNEntriesLayout;
                             OfferEditorForNewEntry: boolean = False;
                             TagsToAddToNewEntry: TNoteTagArray = nil);

     procedure ReloadFromDataModel;
     procedure ReloadMetadataFromDataModel(ReloadTags: boolean = true);
     procedure ReloadNoteName;
     procedure SaveToDataModel;
     procedure SetAsDefaultLayoutInFolder(var NoteAdvOptions: TNoteAdvancedOptions);
     procedure ResetPanelSizes;
     procedure NewEntryRequested(ReqFromEditor: TKntRichEdit);
     procedure IntroInEditorOfEntriesUI(RequestedFromEditor: TKntRichEdit; CtrlDown: boolean);
     procedure SelectNextEntry;
     procedure SelectPreviousEntry;
     procedure Refresh;

     procedure SetBGColorInEditors(Color: TColor);
     procedure SetEditorZoom( ZoomValue : integer; const ZoomString : string; Increment: integer= 0);
     procedure RestoreZoomGoal;

     procedure SetImagesMode(ImagesMode: TImagesMode);
     procedure ResetImagesReferenceCount;
     function  GetImagesInstances: TImageIDs;
     procedure ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
     procedure ReloadImagesOnEditor;

     procedure EditTags;
     procedure RefreshTags;
     procedure RefreshHeaderOfEntries(OnlyNEntry: TNoteEntry = nil);
     procedure ApplyChangeInPanelCustomiz(MECustomiz: TMEPanelCustomization; ForceApplyFilter: boolean; IgnorePanel: TNEntriesPanel);
     procedure ModifiedMetadataOfEntry(NEntry: TNoteEntry);
     procedure ReconsiderVisibilityOfEntries;
     procedure ShowHiddenEntries;
     procedure HideHiddenRevealed;
     procedure SetInfoPanelHidden(value: boolean);
     procedure KeepInfoPanelTemporarilyVisible;
     procedure RefreshPanelsLayout;
     procedure TreeFocused;

     procedure SetReadOnly( AReadOnly : boolean );
     procedure NNodeDeleted;
     function GetNNodeDeleted: boolean;

     procedure SetOnEnter(AEvent: TNotifyEvent);
     procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
     procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);

     //procedure TestPanels;
  end;




implementation

constructor TPanelConfiguration.Create;
begin
  NNodes:= nil;
end;

destructor TPanelConfiguration.Destroy;
begin
    if assigned(NNodes) then begin
      FreeAndNil(NNodes);
    end;

   inherited;
end;


function TPanelConfiguration.UseIsMultiEntry: boolean;
begin
   Result:= (Use <> pnuShowSelectedEntry);
end;

function TPanelConfiguration.EntryModeForUse: TModeEntriesUI;
begin
    if Use = pnuShowSelectedEntry then
       Result:= meSingleEntry
    else
       Result:= meMultiEntry;
end;


end.

