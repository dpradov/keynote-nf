unit kn_Glossary;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ComCtrls,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   RxPlacemnt
   ;


type
  TForm_Glossary = class(TForm)
    FormPlacement: TFormPlacement;
    Panel1: TPanel;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button_New: TButton;
    Button_Edit: TButton;
    Button_Del: TButton;
    Panel2: TPanel;
    LV: TListView;
    Button_Help: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button_NewClick(Sender: TObject);
    procedure Button_EditClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_DelClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    procedure EditTerm( const NewTerm : boolean );
    procedure DeleteTerm;
    procedure UpdateCount;
  end;

procedure LoadGlossaryInfo;


var
   GlossaryList : TStringList;


implementation
uses
   kn_Info,
   kn_global,
   kn_ExpTermDef,
   kn_main,
   knt.App,
   knt.RS;

{$R *.DFM}


const
   NAME_VALUE_SEPARATOR = Chr(18);       // 18 ($12): DC2 (Device Control 2)

procedure LoadGlossaryInfo;
var
   i: integer;
   str: string;
begin
    try
       if FileExists( Glossary_FN) then begin
          GlossaryList.Sorted:= false;
          GlossaryList.LoadFromFile (Glossary_FN);

          if (GlossaryList.Count > 0 ) then begin
              for i := 0 to pred( GlossaryList.Count ) do begin
                 str:= GlossaryList[i];
                 str:= StringReplace(str, '\=', Chr(17),  [rfReplaceAll]);                // 17 ($11): DC1 (Device Control 1)
                 str:= StringReplace(str, '=', NAME_VALUE_SEPARATOR,  []);
                 str:= StringReplace(str, Chr(17), '=',  [rfReplaceAll]);
                 GlossaryList[i]:= str;
              end;
          end;

       end;

    except
      On E : Exception do begin
        ShowMessage( GetRS(sGlss05) + E.Message );
      end;
    end;

    GlossaryList.NameValueSeparator := NAME_VALUE_SEPARATOR;
    GlossaryList.sorted := true;
end;



procedure TForm_Glossary.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {
  case key of
    27 : if ( Shift = [] ) then
    begin
      key := 0;
      ModalResult := mrCancel;
    end;
  end;
  }
end; // KeyDown

procedure TForm_Glossary.FormCreate(Sender: TObject);
var
  item : TListItem;
  i : integer;
  name, value : string;
begin

  OK_Click := false;

  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  GlossaryList.BeginUpdate;
  try
    if (GlossaryList.Count > 0 ) then begin
        for i := 0 to pred( GlossaryList.Count ) do begin
          name := GlossaryList.Names[i];
          value := GlossaryList.ValueFromIndex[i];
          item := LV.Items.Add;
          item.caption := name;
          item.subitems.add( value );
        end;
    end;

  finally
    GlossaryList.EndUpdate;
    UpdateCount;
  end;

end;

function TForm_Glossary.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// CREATE

procedure TForm_Glossary.EditTerm( const NewTerm : boolean );
var
  Form_Term : TForm_TermDef;
  namestr, valuestr : string;
  item, dupItem : TListItem;
  i : integer;
begin

  item := nil;

  if NewTerm then begin
    namestr := '';
    valuestr := '';
  end
  else begin
    item := LV.Selected;

    if ((not assigned( item )) or
        (LV.Items.Count = 0 )) then begin
       messagedlg( GetRS(sGlss00), mtInformation, [mbOK], 0 );
       exit;
    end;

    namestr := item.caption;
    valuestr := item.subitems[0];
  end;


  Form_Term := TForm_TermDef.Create( self );
  try
    with Form_Term do begin
       Edit_Term.Text := namestr;
       Edit_Exp.Text := valuestr;
    end;

    if ( Form_Term.ShowModal = mrOK ) then begin
      with Form_Term do begin
         namestr := Edit_Term.Text;
         valuestr := Edit_Exp.Text;
      end;

      if (( namestr = '' ) or ( valuestr = '' )) then begin
         messagedlg( GetRS(sGlss01), mtError, [mbOK], 0 );
         exit;
      end;

      dupItem := nil;

      if NewTerm then begin
          if ( LV.Items.Count > 0 ) then begin
             for i := 0 to pred( LV.Items.Count ) do begin
                if ( LV.Items[i].Caption = namestr ) then begin
                   dupItem := LV.Items[i];
                   break;
                end;
             end;
          end;

          if assigned( dupItem ) then begin
             if ( App.DoMessageBox( Format(GetRS(sGlss02), [namestr,dupItem.subitems[0] ,valuestr] ),
                                 mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then
                 exit;
             item := dupItem;
          end;
      end;

      try
        if ( item = nil ) then
           item := LV.Items.Add;

        item.caption := namestr;
        item.subitems.Clear;
        item.subitems.Add( valuestr );
        LV.Selected := item;

      except
        on E : Exception do
           messagedlg( E.Message, mtError, [mbOK], 0 );
      end;

    end;
  finally
    Form_Term.Free;
  end;

  LV.SetFocus;
  UpdateCount;

end; // EditTerm

procedure TForm_Glossary.DeleteTerm;
var
  item : TListItem;
begin
  item := LV.Selected;

  if ( not assigned( item )) then
  begin
    messagedlg( GetRS(sGlss00), mtInformation, [mbOK], 0 );
    exit;
  end;

  LV.Items.Delete( LV.Items.IndexOf( item ));
  UpdateCount;

end; // DeleteTerm

procedure TForm_Glossary.Button_NewClick(Sender: TObject);
begin
  EditTerm( true );
end;

procedure TForm_Glossary.Button_EditClick(Sender: TObject);
begin
  EditTerm( false );
end;

procedure TForm_Glossary.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_Glossary.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i : integer;
  item : TListItem;
  name: string;
  Strs : TStringList;
begin
  if OK_Click then
  begin
      try
        Strs := TStringList.Create;
        try
          GlossaryList.Sorted := false;
          GlossaryList.Clear;

          for i := 0 to pred( LV.Items.Count ) do begin
             item := LV.Items[i];
             name:= item.Caption;
             GlossaryList.Add( name + NAME_VALUE_SEPARATOR + item.subitems[0]);

             name:= StringReplace(name, '=', '\=',  [rfReplaceAll]);          // Escape '=' to avoid problems when using something like: =>=****
             Strs.Add( Format( '%s=%s', [name, item.subitems[0]] ));
          end;

          Strs.SaveToFile( Glossary_FN, TEncoding.UTF8);

        finally
          Strs.Free;
          GlossaryList.Sorted := true;
        end;

    except
      on E : Exception do
         messagedlg( GetRS(sGlss03) + E.Message, mtError, [mbOK], 0 );
    end;

  end;
end;

procedure TForm_Glossary.Button_DelClick(Sender: TObject);
begin
  DeleteTerm;
  LV.SetFocus;
end;

procedure TForm_Glossary.UpdateCount;
begin
  Caption := Format( GetRS(sGlss04), [LV.Items.Count] );
end; // UpdateCount


procedure TForm_Glossary.Button_HelpClick(Sender: TObject);
begin
  ActiveKeyNoteHelp(self.HelpContext);  // Node
  //Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

procedure TForm_Glossary.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  try
    if ( LV.Items.Count > 0 ) then begin
      LV.Selected := LV.Items[0];
      LV.ItemFocused := LV.Selected;
    end;

  except
  end;
end;


procedure EditGlossaryTerms;
var
  Form_Glossary : TForm_Glossary;
begin
  Form_Glossary := TForm_Glossary.Create( Form_Main );
  try
     Form_Glossary.ShowModal;
  finally
     Form_Glossary.Free;
  end;
end;


Initialization
  GlossaryList := TStringList.Create;
  with GlossaryList do begin
     duplicates := dupError;
  end;


Finalization
  if ( GlossaryList <> nil ) then
       GlossaryList.Free;
  GlossaryList := nil;

end.
