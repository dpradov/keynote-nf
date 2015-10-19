unit kn_PluginForm;

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

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  gf_misc, kn_Info, kn_Const,
  kn_Plugins, StdCtrls, ExtCtrls,
  kn_PluginBase, Placemnt;

type
  TForm_Plugins = class(TForm)
    Panel_List: TPanel;
    Splitter1: TSplitter;
    Panel_ResPlugins: TPanel;
    List: TListBox;
    Panel_Btn: TPanel;
    Button_Close: TButton;
    Button_Config: TButton;
    Button_Exec: TButton;
    FormPlacement: TFormStorage;
    Button_Help: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure Button_ConfigClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure ShowPluginInfo;

  end;



implementation

{$R *.DFM}

procedure TForm_Plugins.FormCreate(Sender: TObject);
begin
  // LB_Info.Font.Color := clHighlight;
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

end; // CREATE

procedure TForm_Plugins.FormActivate(Sender: TObject);
var
  i : integer;
  aName, aInfo : string;
  aVersion : integer;
  Plugin : TPlugin;

begin

  screen.Cursor := crHourGlass;

  try

    if assigned( Plugin_List ) then
    begin
      for i := 1 to Plugin_List.Count do
      begin
        if GetPluginInfo(
          Plugin_Folder + Plugin_List[pred( i )],
          aName, aVersion, aInfo ) then
        begin
          Plugin := TPlugin.Create;
          with Plugin do
          begin
            FileName := Plugin_Folder + Plugin_List[pred( i )];
            Name := aName;
            Version := aVersion;
            Info := aInfo;
            Features := GetPluginFeatures( Plugin_Folder + Plugin_List[pred( i )] );
          end;
          List.Items.AddObject( Format(
            '%s  (%s)', [aName, Plugin_List[pred( i )]] ), Plugin );
        end;
      end;
    end;

  finally
    screen.Cursor := crDefault;
  end;

  Button_Exec.Enabled := ( List.Items.Count > 0 );
  Button_Config.Enabled := Button_Exec.Enabled;

  if ( List.Items.Count > 0 ) then
  begin
    List.ItemIndex := 0;
    ShowPluginInfo;
  end;

end;  // ACTIVATE


procedure TForm_Plugins.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( Shift = [] ) then
  begin
    case key of
      27 : begin
        key := 0;
        Close;
      end;
    end;
  end;
end; // OnKeyDown

procedure TForm_Plugins.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  try
    for i := 1 to List.Items.Count do
    begin
      List.Items.Objects[pred( i )].Free;
    end;
  except
  end;
end; // DESTROY

procedure TForm_Plugins.ListClick(Sender: TObject);
begin
  ShowPluginInfo;
end;

procedure TForm_Plugins.ShowPluginInfo;
var
  Plugin : TPlugin;
  s : string;
  pl : TPluginFeature;
begin
  if (( List.Items.Count = 0 ) or ( List.ItemIndex < 0 )) then
    exit;
  Plugin := TPlugin( List.Items.Objects[List.ItemIndex] );
  LB_Info.Caption := Plugin.Info;

  s := '';
  for pl := low( TPluginFeature ) to high( TPluginFeature ) do
  begin
    if ( pl in Plugin.Features ) then
      s := s + PluginFeatureNames[pl] + '  ';
    {
    s := s + Format(
      '%s : %s ; ',
      [PluginFeatureNames[pl], BOOLARRAY[( pl in Plugin.Features )]]
    );
    }
  end;
  s := s + Format( '(version %d)', [Plugin.Version] );
  LB_Features.Caption := s;


end; // ShowPluginInfo

procedure TForm_Plugins.Button_ConfigClick(Sender: TObject);
begin
  if ( not ExecutePluginConfig( TPlugin( List.Items.Objects[List.ItemIndex] ).FileName, self.Handle )) then
    messagedlg( 'Could not execute plugin function: KNTConfigurePluginProc',
      mtWarning, [mbOK], 0 );
end;

procedure TForm_Plugins.ListDblClick(Sender: TObject);
begin
  ListClick( self );
  if ( List.ItemIndex >= 0 ) then
    ModalResult := mrOK; // close form and execute plugin
end;


procedure TForm_Plugins.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

end.
