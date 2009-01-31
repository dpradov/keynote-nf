unit TreeNTRegister;

// Registration file for TTreeNT and its editors.
//     Dipl. Ing. Mike Lischke
//     public@lischke-online.com

{$I DFS.inc}

interface

uses
  TreeNT, TNTEditor, Dialogs, Forms, SysUtils,
{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
    DesignIntf,
    DesignEditors;
{$ELSE}
    DsgnIntf;
{$ENDIF}

type
  TTreeNTNodesProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TTreeNTEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

//------------------------------------------------------------------------------

implementation

uses
  Classes, Graphics;

//----------------- TTreeNTNodesProperty ---------------------------------------

procedure TTreeNTNodesProperty.Edit;

begin
  if EditTreeViewItems(TTreeNTNodes(GetOrdValue)) then Modified;
end;

//------------------------------------------------------------------------------

function TTreeNTNodesProperty.GetAttributes: TPropertyAttributes;

begin
  Result := [paDialog, paReadOnly];
end;

//----------------- TTreeNTEditor ----------------------------------------------

const
  VerbCount = 7;

var
  Verbs : array[0..VerbCount-1] of String = ('Edit tree nodes ...',
                                             'Load from file ...',
                                             'Save to tree file ...',
                                             'Save to text file ...',
                                             'Save to bitmap file ...',
                                             'Print tree...',
                                             'Clear tree');
  SaveDir1 : String = '';
  SaveDir2 : String = '';

procedure TTreeNTEditor.Edit;

begin
  ExecuteVerb(0);
end;

//------------------------------------------------------------------------------

function TTreeNTEditor.GetVerbCount: Integer;

begin
  Result := VerbCount;
end;

//------------------------------------------------------------------------------

function TTreeNTEditor.GetVerb(Index: Integer): string;

begin
  Result := Verbs[Index];
end;

//------------------------------------------------------------------------------

procedure TTreeNTEditor.ExecuteVerb(Index: Integer);

var
  OpenDialog: TOpenDialog;
  SaveDialog: TSaveDialog;
  PrintDialog: TPrintDialog;
  Bitmap: TBitmap;

begin
  case Index of
    0:
      if EditTreeViewItems(TTreeNT(Component).Items) then Designer.Modified;
    1:
      begin
      OpenDialog := TOpenDialog.Create(Application);
      with OpenDialog do
      begin
        Filter := 'TreeNT files (*.tnt)|*.tnt|All Files (*.*)|*.*';
        DefaultExt := 'tnt';
        InitialDir := SaveDir1;
        Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist];
        try
          if OpenDialog.Execute then
          begin
            TTreeNT(Component).LoadFromFile(FileName);
            Designer.Modified;
          end;
        finally
          SaveDir1 := ExtractFileDir(FileName);
          Free;
        end;
      end;
    end;
    2, 3:
      begin
      SaveDialog := TSaveDialog.Create(Application);
      with SaveDialog do
      begin
        Filter := 'TreeNT files (*.tnt)|*.tnt|Text files (*.txt)|*.txt|All Files (*.*)|*.*';
        DefaultExt := 'txt';
        InitialDir := SaveDir2;
        Options := [ofPathMustExist, ofOverwritePrompt];
        try
          if SaveDialog.Execute then TTreeNT(Component).SaveToFile(FileName,Index = 2);
        finally
          SaveDir2 := ExtractFileDir(FileName);
          Free;
        end;
      end;
    end;
    4:
      begin
      SaveDialog := TSaveDialog.Create(Application);
      with SaveDialog do
      begin
        Filter := 'Bitmaps (*.bmp)|*.bmp|All Files (*.*)|*.*';
        DefaultExt := 'bmp';
        InitialDir := SaveDir2;
        Options := [ofPathMustExist, ofOverwritePrompt];
        try
          if SaveDialog.Execute then
          begin
            Bitmap := TBitmap.Create;
            try
              // capture screen output
              {$ifdef DFS_COMPILER_3_UP}
                Bitmap.PixelFormat := pf24Bit;
              {$endif}
              Bitmap.Width := TTreeNT(Component).TreeWidth;
              Bitmap.Height := TTreeNT(Component).Treeheight;

              // draw the tree to our bitmap
              TTreeNT(Component).DrawTo(Bitmap.Canvas);
              Bitmap.SaveToFile(FileName);
            finally
              Bitmap.Free;
            end;
          end;
        finally
          SaveDir2 := ExtractFileDir(FileName);
          Free;
        end;
      end;
    end;
    5:
      begin
        PrintDialog := TPrintDialog.Create(Application);
        try
          if PrintDialog.Execute then TTreeNT(Component).Print(0, 0, 100);
        finally
          PrintDialog.Free;
        end;
      end;
    6:
      begin
        TTreeNT(Component).Items.Clear;
        Designer.Modified;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('Tools', [TTreeNT]);
  RegisterPropertyEditor(TypeInfo(TTreeNTNodes), nil, '', TTreeNTNodesProperty);
  RegisterComponentEditor(TTreeNT, TTreeNTEditor);
end;

//------------------------------------------------------------------------------

end.

