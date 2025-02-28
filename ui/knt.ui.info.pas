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
     property Editor: TKntRichEdit read GetEditor;
     property NNode: TNoteNode read GetNNode;

     procedure LoadFromNNode(NNode: TNoteNode; SavePreviousContent: boolean);
     procedure ReloadFromDataModel;
     function ReloadMetadataFromDataModel(ReloadTags: boolean = true): TNoteEntry;
     procedure ReloadNoteName;
     function  SaveToDataModel: TMemoryStream;

     procedure SetImagesMode(ImagesMode: TImagesMode);
     procedure ResetImagesReferenceCount;
     function  GetImagesInstances: TImageIDs;
     procedure ReconsiderImageDimensionGoalsOnEditor(Selection: boolean; ImagesMode: TImagesMode);
     procedure ReloadImagesOnEditor;

     procedure EditTags;
     procedure SetInfoPanelHidden(value: boolean);
     procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);

     procedure SetReadOnly( AReadOnly : boolean );
     procedure NNodeDeleted;

     procedure SetOnEnter(AEvent: TNotifyEvent);
     procedure SetOnMouseUpOnNote(AEvent: TNotifyEvent);
     procedure SetOnMouseMoveOnNote(AEvent: TNotifyEvent);
  end;

implementation

end.

