unit Main;

// TreeNT Demo program. Be careful while compiling this demo with D2, because D2's ShlObj unit is wrong.
// See Brad Stowers adapted unit for a replacement (address in DFS.inc). D3 +  users are fine...

{$I DFS.inc}

// [dpv] Changes to test new functionality: hidden nodes

interface

uses Windows, Forms, StdCtrls, Buttons, Classes, Controls, TreeNT,
  ComCtrls, Graphics, Dialogs, ExtCtrls, Mask,
  ImgList,
  Menus;

type
  TMainForm = class(TForm)
    ImageList1: TImageList;
    CloseButton: TBitBtn;
    InsertMarkTimer: TTimer;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;                                       
    Label4: TLabel;
    Tree1: TTreeNT;
    Label5: TLabel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Label1: TLabel;
    BitBtn3: TBitBtn;
    Tree2: TTreeNT;
    Label2: TLabel;
    BitBtn4: TBitBtn;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Image1: TImage;
    SaveDialog: TSaveDialog;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Button3: TButton;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Nodes: TMaskEdit;
    Levels: TMaskEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Button4: TButton;
    Label16: TLabel;
    TabSheet5: TTabSheet;
    SystemImages: TImageList;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    DownButton: TSpeedButton;
    UpButton: TSpeedButton;
    ScrollTimer: TTimer;
    Panel2: TPanel;
    Label22: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    ConstrainedCheckBox: TCheckBox;
    bBorrarNodo: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    OpenDialog: TOpenDialog;
    Tree3: TTreeNT;
    Tree4: TTreeNT;
    Tree5: TTreeNT;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    procedure Button15Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure bBorrarNodoClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Tree3AfterItemPaint(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates);
    procedure FormCreate(Sender: TObject);
    procedure Tree3Hint(Sender: TObject; Node: TTreeNTNode; var NewText: string);
    procedure InsertMarkTimerTimer(Sender: TObject);
    procedure Tree1KeyPress(Sender: TObject; var Key: Char);
    procedure Tree1Change(Sender: TObject; Node: TTreeNTNode);
    procedure Tree2Hint(Sender: TObject; Node: TTreeNTNode; var NewText: String);
    procedure Tree4AfterItemPaint(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates);
    procedure Button1Click(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Tree3BeforeItemPaint(Sender: TObject; Node: TTreeNTNode;
      ItemRect: TRect; NodeStates: TNodeStates; var OwnerDraw: Boolean);
    procedure Tree3Collapsed(Sender: TObject; Node: TTreeNTNode);
    procedure Tree3Deletion(Sender: TObject; Node: TTreeNTNode);
    procedure ScrollTimerTimer(Sender: TObject);
    procedure ScrollButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollButtonClick(Sender: TObject);
    procedure Tree3Change(Sender: TObject; Node: TTreeNTNode);
    procedure Tree5BeforeItemPaint(Sender: TObject; Node: TTreeNTNode;
      ItemRect: TRect; NodeStates: TNodeStates; var OwnerDraw: Boolean);
    procedure Tree5BeforePaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Tree5Expanding(Sender: TObject; Node: TTreeNTNode;
      var AllowExpansion: Boolean);
    procedure Tree5AfterPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ConstrainedCheckBoxClick(Sender: TObject);
  private
    FTree3Building: Boolean;
    FBackground: TBitmap;
    FFirstRun: Boolean;
    procedure AddNodes(Node: TTreeNTNode; Levels, Level, Nodes: Integer);
    procedure FillTree1(Levels, Nodes: Integer);
    procedure FillTree3;
    procedure LoadImageLists;
    procedure ScrollOneItem(Tag: Integer);
  end;

var MainForm: TMainForm;

//------------------------------------------------------------------------------

implementation

{$ifndef DFS_DELPHI_4_UP}
  {$ifndef DFS_CPPB}
    {$define WithCommCrtl98}
  {$endif}
{$endif}

uses
  {$ifdef WithCommCrtl98} CommCtrl98, {$else} CommCtrl, {$endif}
  TNTEditor, SysUtils, MMSystem, ShellAPI, Messages,
  {$ifdef DFS_COMPILER_3_UP}
    ActiveX, ComObj,
  {$else}
    OLE2, OleAuto,
  {$endif}
  ShlObj;

{$R *.DFM}
{$R Resources\Icons.res}

var Counter: Integer;

    IconFolderClose: Integer;
    IconFolderOpen: Integer;
    IconFont1: Integer;
    IconFont2: Integer;
    IconFont3: Integer;

    nodo: TTreeNTNode;
    nodo2: TTreeNTNode;

{$ifndef DFS_COMPILER_3_UP}
  // This constant is not declared in D2 ShlObj.pas
  const IID_IExtractIconA : TGUID = (D1:$000214EB; D2:$0000; D3:$0000; D4:($C0, $00, $00, $00, $00, $00, $00, $46));
{$endif}

//------------------------------------------------------------------------------

procedure TMainForm.LoadImageLists;

// D2 and D3 image lists are incompatible, thus I load the icons here from the resource. BTW. Loading icons
// this way produces much better looking images in the trees than by loading it in the IDE.

var Icon : TIcon;
    H, W : Integer;

begin
  // ImageList1 with predefined icons
  H := ImageList1.Height;
  W := ImageList1.Width;
  Icon := TIcon.Create;
  Icon.Handle := LoadImage(HInstance, 'ICON_1', IMAGE_ICON, W, H, 0);
  ImageList1.AddIcon(Icon);
  IconFolderClose := ImageList1.Count-1;

  Icon.Handle := LoadImage(HInstance, 'ICON_2', IMAGE_ICON, W, H, 0);
  ImageList1.AddIcon(Icon);
  IconFolderOpen := ImageList1.Count-1;

  Icon.Handle := LoadImage(HInstance, 'ICON_3', IMAGE_ICON, W, H, 0);
  ImageList1.AddIcon(Icon);
  IconFont1 := ImageList1.Count-1;

  Icon.Handle := LoadImage(HInstance, 'ICON_4', IMAGE_ICON, W, H, 0);
  ImageList1.AddIcon(Icon);
  IconFont2 := ImageList1.Count-1;

  Icon.Handle := LoadImage(HInstance, 'ICON_5', IMAGE_ICON, W, H, 0);
  ImageList1.AddIcon(Icon);
  IconFont3 := ImageList1.Count-1;

  // system images for dummy history tree (if IE4+ is not installed)
  Icon.Handle := LoadImage(HInstance, 'ICON_7', IMAGE_ICON, W, H, 0);
  SystemImages.AddIcon(Icon);

  Icon.Handle := LoadImage(HInstance, 'ICON_8', IMAGE_ICON, W, H, 0);
  SystemImages.AddIcon(Icon);

  Icon.Handle := LoadImage(HInstance, 'ICON_6', IMAGE_ICON, W, H, 0);
  SystemImages.AddIcon(Icon);

  Icon.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.EditButtonClick(Sender: TObject);

begin
  FTree3Building := True;
  case TBitBtn(Sender).Tag of
    1 : EditTreeViewItems(Tree1.Items);
    2 : EditTreeViewItems(Tree2.Items);
    3 : EditTreeViewItems(Tree3.Items);
  end;
  FTree3Building := False;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then
  begin
    Key := #0;
    Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree3BeforeItemPaint(Sender: TObject; Node: TTreeNTNode;
                                         ItemRect: TRect; NodeStates: TNodeStates; var OwnerDraw: Boolean);

begin
  Node.BeginUpdate;
  if Node.Level > 0 then
  begin
    if nsHot in NodeStates then Node.Font.Style := [fsUnderline]
                           else Node.Font.Style := [];
    if TreeNT.nsFocused in NodeStates then Node.Color := clBtnShadow
                                      else Node.Color := clBtnHighlight;
  end
  else
  begin
    Node.Font.Style := [];
    if nsHot in NodeStates then Node.Color := clBtnFace
                           else
      if (TreeNT.nsSelected in NodeStates) or Node.Expanded then Node.Color := clBtnHighlight
                                                            else Node.Color := clBtnFace;
  end;
  Node.EndUpdate;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree3AfterItemPaint(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates);

var BState : UINT;
    Edge   : UINT;

begin
  if Node.Level = 0 then
  begin
    if nsHot in NodeStates then Edge := BDR_RAISEDINNER
                           else
      if (TreeNT.nsFocused in NodeStates) or Node.Expanded then Edge := BDR_SUNKENOUTER
                                                           else Edge := 0;
    if Edge <> 0 then
    begin
      if Node.Expanded then BState := BF_LEFT or BF_TOP or BF_RIGHT
                       else BState := BF_RECT;
      DrawEdge(Tree3.Canvas.Handle, Itemrect, Edge, BState);
    end;
  end
  else
  begin
    BState := BF_LEFT or BF_RIGHT;
    DrawEdge(Tree3.Canvas.Handle, Itemrect, BDR_SUNKENOUTER, BState);
  end;
end;

//------------------------------------------------------------------------------

function EnumFontCallback(lpelf: PEnumLogFont; lpntm: PNewTextMetric;
                          FontType: Integer; lParam: LPARAM): Integer; stdcall;

var
  Items: TTreeNTNodes;
  Node: TTreeNTNode;
  AFont: TFont;
  Name: String[32];
  Index: Integer;

begin
  Items := TTreeNTNodes(lParam);
  AFont := TFont.Create;
  AFont.Size := 10;
  {$ifdef DFS_COMPILER_3_UP}
    AFont.Charset := DEFAULT_CHARSET;
  {$endif}
  try
    Name := lpelf.elfLogFont.lfFaceName;
    Node := Items.Add(nil, name);
    if Random(100) < 25 then // just to bring a bit more fun into the list :-)
    begin
      Node.Color := RGB(Random(180) + 75, Random(180) + 75, Random(180) + 75);
      AFont.Color := RGB(Random(180) + 75, Random(180) + 75, Random(180) + 75);
    end;
    AFont.Name := Name;
    Node.Font := AFont;   // the use of a temporary font is needed to make
                        // the font manager realizing the new font
    if FontType = 0 then Index := IconFont3
                    else
      if (FontType and TRUETYPE_FONTTYPE) > 0 then Index := IconFont2
                                              else Index := IconFont1;
    Node.ImageIndex := Index;
    Node.SelectedIndex := Index;
  finally
    AFont.Free;
  end;
  Result := 1;
end;

//------------------------------------------------------------------------------

procedure TMainForm.AddNodes(Node: TTreeNTNode; Levels, Level, Nodes: Integer);

var I     : Integer;
    Child : TTreeNTNode;

begin
  with Tree1.Items do
  begin
    Node.BeginUpdate;
    for I := 1 to Nodes do
    begin
      //Child := AddChild(Node, Format('Child %d %d', [Levels - Level, I]));
      Child := AddChild(Node, Format(Node.Text + ' - Child %d', [I]));
      if Level > 1 then AddNodes(Child, Levels, Level - 1, Nodes);
    end;
    Node.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FillTree1(Levels, Nodes: Integer);

var I     : Integer;
    Child : TTreeNTNode;

begin
  Randomize;
  with Tree1.Items do
  begin
    BeginUpdate;
    for I := 1 to Nodes do
    begin
      Child := Add(nil, 'Node ' + IntToStr(I));
      Child.CheckType := ctRadioButton;
      Child.ImageIndex := 1;
      Child.SelectedIndex := 1;
      if Random < 0.5 then Child.StateIndex := 0;
      if Levels > 1 then AddNodes(Child, Levels, Levels - 1, Nodes);
    end;
    Tree1.FullExpand;
    Tree1.Items.GetFirstNode.MakeVisible;
    EndUpdate;
    Label14.Caption := IntToStr(Count);
  end;
end;

//------------------------------------------------------------------------------

// The following declarations are taken from D4's ShlObj.pas (IQueryInfo) and VC +  + 6.0 comdef.h (IID_IQueryInfo).
// The IQueryInfo interface is used to get the original tooltip string for the entries in the explorer cache.
// The returned strings are stored as string reference in a node's data member. If this interface is not available
// then the data member remains nil and a default string is used instead.

const IID_IQueryInfo: TGUID = (D1:$00021500; D2:$0000; D3:$0000; D4:($C0, $00, $00, $00, $00, $00, $00, $46));

{$ifndef DFS_COMPILER_4_UP}
const SID_IQueryInfo = '{00021500-0000-0000-C000-000000000046}';

type
  {$ifdef DFS_COMPILER_3_UP}
    IQueryInfo = interface(IUnknown)
      [SID_IQueryInfo]
      function GetInfoTip(dwFlags: DWORD; var ppwszTip: PWideChar): HResult; stdcall;
      function GetInfoFlags(out pdwFlags: DWORD): HResult; stdcall;
    end;
  {$else} // Delphi 2
    IQueryInfo = class(IUnknown)
      function GetInfoTip(dwFlags: DWORD; var ppwszTip: PWideChar): HResult; virtual; stdcall; abstract;
      function GetInfoFlags(var pdwFlags: DWORD): HResult; virtual; stdcall; abstract;
    end;
  {$endif}
  
{$endif}
        
procedure TMainForm.FillTree3;

// fills tree3 with explorer history if available, SHGetSpecialFolderLocation might not work and/or CSIDL_HISTORY
// is not defined, thus I use an alternative way (by registry) to get the history folder

  //------------------------------------------------------------

  function DisplayToString(PIDL: PItemIDList; Display: TStrRet): String;

  begin
    case Display.uType of
      STRRET_CSTR:
        SetString(Result, Display.cStr, StrLen(Display.cStr));
      STRRET_WSTR:
        Result := WideCharToString(Display.pOleStr);
      STRRET_OFFSET:
        SetString(Result, PChar(PIDL) + Display.uOffset, StrLen(PChar(PIDL) + Display.uOffset));
    end;
  end;

  //------------------------------------------------------------

var WeekNode,
    SiteNode,
    URLNode: TTreeNTNode;
    History: PItemIDList;
    Malloc: IMalloc;
    Folder,
    HistoryFolder,
    WeekFolder,
    SiteFolder: IShellFolder;
    Eaten,
    Attrib: {$ifdef DFS_COMPILER_4_UP}
              Cardinal
            {$else}
              {$ifdef DFS_COMPILER_3_UP}
                Integer
              {$else}
                ULONG
              {$endif}
            {$endif};
    {$ifndef DFS_COMPILER_3_UP}
      Flags: UINT;
    {$endif}
    Key: HKEY;
    Buffer: array[0..4096] of Char;
    AType,
    Size: Integer;
    {$ifdef DFS_COMPILER_3_UP}
      Name: WideString;
    {$else}
      Name: String;
      PName: PWideChar;
    {$endif}
    HistoryEnumList, 
    WeekEnumList, 
    SiteEnumList: IEnumIDList;
    Week,
    Site,
    URL: PItemIDList;
    Fetched: {$ifdef DFS_COMPILER_4_UP}
               Cardinal
             {$else}
               {$ifdef DFS_COMPILER_3_UP}
                 Integer
               {$else}
                 ULONG
               {$endif}
             {$endif};
    Display: TStrRet;
    Icon: IExtractIcon;
    IconLarge,
    WeekIcon,
    SiteIcon,
    URLIcon: HICON;
    Index: Integer;
    TempIcon: TIcon;
    Info: IQueryInfo;
    InfoTip: PWideChar;
    FinalString,
    InfoTipStr: String;

begin
  // get task allocator to handle PIDLs
  SHGetMalloc(Malloc);
  History := nil;
  TempIcon := TIcon.Create;
  Tree3.Items.BeginUpdate;
  try
    // get desktop folder interface
    SHGetDesktopFolder(Folder);
    try
      // determine history folder name
      RegOpenKey(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', Key);
      Size := SizeOf(Buffer);
      AType := REG_SZ;
      RegQueryValueEx(Key, 'History', nil, @AType, @Buffer, @Size);
      RegCloseKey(Key);
      // Is there a history folder on this machine?
      if StrLen(Buffer) > 0 then
      begin
        // convert folder path to wide string

        {$ifdef DFS_COMPILER_3_UP}
          Name := Buffer;
          // convert path to pidl
          Folder.ParseDisplayName(Handle, nil, PWideChar(Name), Eaten, History, Attrib);
        {$else}
          PName := StringToOLEStr(Buffer);
          try
            // convert path to pidl
            Folder.ParseDisplayName(Handle, nil, PName, Eaten, History, Attrib);        
          finally
            SysFreeString(PName);
          end;
        {$endif}

        // Are we succesfull?
        if assigned(History) then
        begin
          // get a shell folder to this pidl
          OLECheck(Folder.BindToObject(History, nil, IID_IShellFolder, Pointer(HistoryFolder)));
          try
            // now that we are in the history folder enumerate all site folders and their subitems;
            // we don't use findfirst/findnext here since the history folder does not contain real entries
            // but a binary file which must be parsed
            if Succeeded(HistoryFolder.EnumObjects(Handle, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN, HistoryEnumList)) then
            begin
              // free dummy content of tree3
              Tree3.Items.Clear;
              SystemImages.Clear;
              WeekIcon := 0;
              SiteIcon := 0;
              URLIcon := 0;
              IconLarge := 0;
              while HistoryEnumList.Next(1, Week, Fetched) = S_OK do
              begin
                // for each subfolder do:
                // get display string as in IE
                HistoryFolder.GetDisplayNameOf(Week, STRRET_CSTR, Display);
                Name := DisplayToString(Week, Display);
                // add a corresponding tree node
                WeekNode := Tree3.Items.Add(nil, Name);
                // try getting the tooltip text
                Info := nil;
                HistoryFolder.GetUIObjectOf(Handle, 1, Week, IID_IQUERYINFO, nil, Pointer(Info));
                // interface available?
                if assigned(Info) then
                begin
                  Info.GetInfoTip(0, InfoTip);
                  // convert wide string to ansi string
                  {$ifdef DFS_COMPILER_3_UP}
                    InfoTipStr := InfoTip;
                  {$else}
                    InfoTipStr := WideCharToString(InfoTip);
                  {$endif}
                  // I'm lazy, let Delphi make the copy
                  WeekNode.Data := StrNew(PChar(InfoTipStr));
                  Malloc.Free(InfoTip);
                end;
                // get icon if not already retrieved
                if WeekIcon = 0 then
                begin
                  HistoryFolder.GetUIObjectOf(Handle, 1, Week, IID_IExtractIconA, nil, Pointer(Icon));
                  if assigned(Icon) then
                  begin
                    {$ifdef DFS_COMPILER_3_UP}
                      if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Attrib) = NOERROR then
                      begin
                        if (Attrib and GIL_NOTFILENAME) <> 0
                    {$else}
                      if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Flags) = NOERROR then
                      begin
                        if (Flags and GIL_NOTFILENAME) <> 0 
                    {$endif}
                                                           then Icon.Extract(Buffer, Index, IconLarge, WeekIcon, MakeLParam(16, 16))
                                                           else ExtractIconEx(Buffer, Index, IconLarge, WeekIcon, 1);

                      ImageList_AddIcon(SystemImages.Handle, WeekIcon);
                      DestroyIcon(WeekIcon);
                    end
                    else WeekIcon := HICON(-1);
                  end
                  else WeekIcon := HICON(-1);
                end;
                WeekNode.ImageIndex := 0;
                WeekNode.SelectedIndex := 0;
                WeekNode.Color := clBtnFace;
                WeekNode.Font.Color := clBlack;

                // for each week entry get it's site entries
                OLECheck(HistoryFolder.BindToObject(Week, nil, IID_IShellFolder, Pointer(WeekFolder)));
                try
                  if Succeeded(WeekFolder.EnumObjects(Handle, SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN, WeekEnumList)) then
                  begin
                    while WeekEnumList.Next(1, Site, Fetched) = S_OK do
                    begin
                      // for each subfolder do:
                      // get display string as in IE
                      WeekFolder.GetDisplayNameOf(Site, STRRET_CSTR, Display);
                      // add a corresponding tree node
                      SiteNode := Tree3.Items.AddChild(WeekNode, DisplayToString(Site, Display));
                      // try getting the tooltip text
                      Info := nil;
                      WeekFolder.GetUIObjectOf(Handle, 1, Site, IID_IQUERYINFO, nil, Pointer(Info));
                      // interface available?
                      if assigned(Info) then
                      begin
                        Info.GetInfoTip(0, InfoTip);
                        // convert wide string to ansi string
                        {$ifdef DFS_COMPILER_3_UP}
                          InfoTipStr := InfoTip;
                        {$else}
                          InfoTipStr := WideCharToString(InfoTip);
                        {$endif}
                        // I'm lazy, let Delphi make the copy
                        SiteNode.Data := StrNew(PChar(InfoTipStr));
                        Malloc.Free(InfoTip);
                      end;
                      // get icon if not already retrieved
                      if SiteIcon = 0 then
                      begin
                        WeekFolder.GetUIObjectOf(Handle, 1, Site, IID_IExtractIconA, nil, Pointer(Icon));
                        if assigned(Icon) then
                        begin
                          IconLarge := 0;
                          {$ifdef DFS_COMPILER_3_UP}
                            if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Attrib) = NOERROR then
                            begin
                              if (Attrib and GIL_NOTFILENAME) <> 0
                          {$else}
                            if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Flags) = NOERROR then
                            begin
                              if (Flags and GIL_NOTFILENAME) <> 0
                          {$endif}
                                                                 then Icon.Extract(Buffer, Index, IconLarge, SiteIcon, MakeLParam(16, 16))
                                                                 else ExtractIconEx(Buffer, Index, IconLarge, SiteIcon, 1);

                            ImageList_AddIcon(SystemImages.Handle, SiteIcon);
                            DestroyIcon(SiteIcon);
                          end
                          else SiteIcon := HICON(-1);
                        end
                        else SiteIcon := HICON(-1);
                      end;
                      SiteNode.ImageIndex := 1;
                      SiteNode.SelectedIndex := 1;
                      SiteNode.Color := clBtnHighlight;
                      SiteNode.Font.Color := clBlack;

                      // for each site entry get it's URL entries
                      OLECheck(WeekFolder.BindToObject(Site, nil, IID_IShellFolder, Pointer(SiteFolder)));
                      try
                        if Succeeded(SiteFolder.EnumObjects(Handle, SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, SiteEnumList)) then
                        begin
                          while SiteEnumList.Next(1, URL, Fetched) = S_OK do
                          begin
                            // for each item do:
                            // get display string as in IE, URLs are differently shown than the other nodes and since
                            // I want to show the tree exactly like IE I have to model the same behaviour

                            // try getting the tooltip text
                            Info := nil;
                            InfoTip := nil;
                            SiteFolder.GetUIObjectOf(Handle, 1, URL, IID_IQUERYINFO, nil, Pointer(Info));
                            // interface available?
                            if assigned(Info) then Info.GetInfoTip(0, InfoTip);

                            if assigned(InfoTip) then
                            begin
                              // convert wide string to ansi string
                              {$ifdef DFS_COMPILER_3_UP}
                                InfoTipStr := InfoTip;
                              {$else}
                                InfoTipStr := WideCharToString(InfoTip);
                              {$endif}
                              Malloc.Free(InfoTip);
                            end
                            else
                            begin
                              SiteFolder.GetDisplayNameOf(URL, STRRET_CSTR, Display);
                              InfoTipStr := DisplayToString(URL, Display);
                            end;

                            // find first line break
                            Size := Pos(#13, InfoTipStr);
                            if Size = 0 then FinalString := InfoTipStr
                                        else
                              if InfoTipStr[Size - 1] = #10 then FinalString := Copy(InfoTipStr, 1, Size - 2)
                                                            else FinalString := Copy(InfoTipStr, 1, Size - 1);

                            // now shorten the string until it fits into the client area, DisplayRect doesn't work
                            // here, since the node hasn't been displayed yet
                            Size := Tree3.Canvas.TextWidth(FinalString);
                            // we are on level 2, hence add the indentation plus some extra space for
                            // level 0 icon and offset
                            if Size + 2 * Tree3.Indent + 40 > Tree3.ClientWidth then
                            begin
                              while Size + 2 * Tree3.Indent + 40 > Tree3.ClientWidth do
                              begin
                                Delete(FinalString, Length(FinalString), 1);
                                Size := Tree3.Canvas.TextWidth(FinalString + '...');
                              end;
                              // add a corresponding tree node
                              URLNode := Tree3.Items.AddChild(SiteNode, FinalString + '...');
                            end
                            else URLNode := Tree3.Items.AddChild(SiteNode, FinalString);
                            // finally make a copy of the entire display string for the tooltip
                            URLNode.Data := StrNew(PChar(InfoTipStr));

                            // get icon if not already retrieved
                            if URLIcon = 0 then
                            begin
                              SiteFolder.GetUIObjectOf(Handle, 1, URL, IID_IExtractIconA, nil, Pointer(Icon));
                              if assigned(Icon) then
                              begin
                                IconLarge := 0;
                                {$ifdef DFS_COMPILER_3_UP}
                                  if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Attrib) = NOERROR then
                                  begin
                                    if (Attrib and GIL_NOTFILENAME) <> 0
                                {$else}
                                  if Icon.GetIconLocation(GIL_FORSHELL, Buffer, SizeOf(Buffer), Index, Flags) = NOERROR then
                                  begin
                                    if (Flags and GIL_NOTFILENAME) <> 0
                                {$endif}
                                                                       then Icon.Extract(Buffer, Index, IconLarge, URLIcon, MakeLParam(16, 16))
                                                                       else ExtractIconEx(Buffer, Index, IconLarge, URLIcon, 1);

                                  ImageList_AddIcon(SystemImages.Handle, URLIcon);
                                  DestroyIcon(URLIcon);
                                end
                                else URLIcon := HICON(-1);
                              end
                              else URLIcon := HICON(-1);
                            end;
                            URLNode.ImageIndex := 2;
                            URLNode.SelectedIndex := 2;
                            URLNode.Color := clBtnHighlight;
                            URLNode.Font.Color := clBlack;
                            Malloc.Free(URL);
                          end;
                          SiteNode.AlphaSort;
                          {$ifdef DFS_COMPILER_2} SiteEnumList.Release; {$endif}
                        end;
                      finally
                        // here we need an explicit release, since we are going to use this interface several times
                        SiteFolder := nil;
                      end;

                      // finally free current week entry
                      Malloc.Free(Site);
                    end;
                    WeekNode.AlphaSort;
                    {$ifdef DFS_COMPILER_2} WeekEnumList.Release; {$endif}
                  end;
                finally
                  // here we need an explicit release, since we are going to use this interface several times
                  WeekFolder := nil;
                end;
                // finally free current week entry
                Malloc.Free(Week);
              end;
            end;
          finally
            {$ifdef DFS_COMPILER_2}
              HistoryFolder.Release;
            {$endif}
          end;
        end;
      end;
    finally
      {$ifdef DFS_COMPILER_2}
        Folder.Release;
      {$endif}
    end;
  finally
    Malloc.Free(History);
    {$ifdef DFS_COMPILER_2}
      Malloc.Release;
    {$endif}
    TempIcon.Free;
    Tree3.Items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  FFirstRun := True;
  LoadImageLists; // ImageList1
  FBackground := TBitmap.Create;
  FBackground.Handle := LoadBitmap(HInstance, 'BACKGROUND');

  ConstrainedCheckBox.checked := toLevelSelectConstraint in Tree1.Options;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree3Hint(Sender: TObject; Node: TTreeNTNode; var NewText: string);

begin
  if assigned(Node.Data) then NewText := PChar(Node.Data)
                         else
    case Node.Level of
      0 : NewText := 'visited pages on ' + Node.Text;
      1 : NewText := Node.Text + ' visited pages'
    else NewText := Node.Text + ' (' + Node.Parent.Text + ')';
    end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.InsertMarkTimerTimer(Sender: TObject);

begin
  Inc(Counter);
  if Counter = 10 then Counter := 0;
  Tree2.InsertMarkColor := RGB(Random(255), Random(255), Random(255));
  Tree2.ShowInsertMark(Tree2.Items[Counter], False);
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree1KeyPress(Sender: TObject; var Key: Char);

var Node : TTreeNTNode;

begin
  Node := TTreeNT(Sender).Selected;
  if (Key = #13) and assigned(Node) then
  begin
    if Node.Expanded then Node.Collapse(False)
                     else Node.Expand(False);
    Key := #0;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree1Change(Sender: TObject; Node: TTreeNTNode);

begin
  Label15.Caption := IntToStr(Tree1.Items.SelectedCount);
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree2Hint(Sender: TObject; Node: TTreeNTNode; var NewText: String);

begin
  NewText := Node.Text;
  if Node.ImageIndex = IconFont1 then NewText := NewText + ' (this is actually a raster font)';
  if Node.ImageIndex = IconFont2 then NewText := NewText + ' (this is actually a true type font)';
  if Node.ImageIndex = IconFont3 then NewText := NewText + ' (this is actually a device font)';
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree4AfterItemPaint(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates);

const FreeText = 'A tree view control is a window that displays a hierarchical list of items, ' + 
                 'such as the headings in a document, the entries in an index, or the files and ' + 
                 'directories on a disk. Each item consists of a label and an optional bitmapped ' + 
                 'image, and each item can have a list of subitems associated with it. By clicking ' + 
                 'an item, the user can expand or collapse the associated list of subitems. ';

var SI : TScrollInfo;

begin
  if Node.Level = 0 then
    with Tree4 do
    begin
      if TreeNT.nsSelected in NodeStates then Canvas.Brush.Color := ColorSelected
                                         else Canvas.Brush.Color := Color;
      Canvas.Font.Height := -13;
      Canvas.Font.Color := $0080FF;
      // note that we need to take the horizontal scrolling position into account
      // here, because all values are in child window coordinates
      with SI do
      begin
        cbSize := SizeOf(SI);
        fMask := SIF_POS;
      end;

      GetScrollInfo(Tree4.Handle, SB_HORZ, SI);
      Itemrect.Top := ItemRect.Top - Node.Font.Height + 8;

      Canvas.Draw(ItemRect.Left + Indent - SI.nPos + 30, ItemRect.Top, Image1.Picture.Graphic);

      ItemRect.Left := 7 * Indent - SI.nPos;
      Dec(ItemRect.Right, 50 + SI.nPos);
      Canvas.FillRect(ItemRect);
      DrawText(Canvas.Handle, FreeText, Length(FreeText), ItemRect, DT_LEFT or DT_WORDBREAK or DT_EXTERNALLEADING or
                                                                    DT_EDITCONTROL or DT_END_ELLIPSIS);
    end;
end;

//------------------------------------------------------------------------------

// Ocultar nodo
procedure TMainForm.bBorrarNodoClick(Sender: TObject);
begin
   if nodo = nil then exit;
      if nodo.MarkedHidden then
        Beep
      else
        nodo.Hidden := True;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
   if nodo = nil then exit;
  if not nodo.MarkedHidden then
    Beep
  else
     nodo.Hidden := False;
end;


procedure TMainForm.Button6Click(Sender: TObject);
begin
    tree1.Selected.Delete;
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
   if nodo2 = nil then exit;

      if nodo2.MarkedHidden then
        Beep
      else
        nodo2.Hidden := True;
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
   if nodo2 = nil then exit;
  if not nodo2.MarkedHidden then
    Beep
  else
     nodo2.Hidden := False;
end;

procedure TMainForm.Button9Click(Sender: TObject);
begin
    nodo:= tree1.Selected;
end;

procedure TMainForm.Button10Click(Sender: TObject);
begin
    nodo2:= tree1.Selected;
end;

procedure TMainForm.Button11Click(Sender: TObject);
begin
   if SaveDialog.Execute then
  begin
    Tree1.SaveToFile(SaveDialog.FileName, True);
    Beep;
  end;

end;

procedure TMainForm.Button12Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Tree1.LoadFromFile (OpenDialog.FileName);
    Beep;
  end;

end;

procedure TMainForm.Button13Click(Sender: TObject);
begin
   nodo.MakeVisible
end;

procedure TMainForm.Button14Click(Sender: TObject);
begin
   nodo2.MakeVisible
end;

procedure TMainForm.Button15Click(Sender: TObject);
begin
  Tree1.FullNotHidden
end;

procedure TMainForm.Button1Click(Sender: TObject);

begin
  if SaveDialog.Execute then
  begin
    Tree1.SaveToFile(SaveDialog.FileName, False);
    Beep;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Button2Click(Sender: TObject);

var Start : DWORD;

begin
  Screen.Cursor := crHourGlass;
  try
    Start := timeGetTime;
    Tree1.Items.Clear;
    Label10.Caption := Format('%d ms', [timeGetTime - Start]);
    Label14.Caption := '0';
  finally
    Screen.Cursor := crDefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Button3Click(Sender: TObject);

var Start : DWORD;

begin
  Screen.Cursor := crHourGlass;
  try
    Start := timeGetTime;
    FillTree1(StrToInt(Trim(Levels.Text)), StrToInt(Trim(Nodes.Text)));
    Label11.Caption := Format('%d ms', [timeGetTime - Start]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Button4Click(Sender: TObject);

var Start : DWORD;

begin
  Screen.Cursor := crHourGlass;
  try
    Start := timeGetTime;
    Tree1.AlphaSort;
    Label16.Caption := Format('%d ms', [timeGetTime - Start]);
  finally
    Screen.Cursor := crDefault;
  end;
end;


//------------------------------------------------------------------------------

procedure TMainForm.Tree3Collapsed(Sender: TObject; Node: TTreeNTNode);

var ScrollInfo : TScrollInfo;
    Temp       : TTreeNTNode;
    Counter    : Integer;
     
begin
  if FTree3Building then Exit;

  with Tree3 do
  begin
    // init the scroll info structure
    with ScrollInfo do
    begin
      FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_ALL;
    end;

    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    // vertical dimension is given in number of items
    if ScrollInfo.nPage = 0 then
    begin
      Temp := Items.GetFirstNode;
      Counter := 0;
      while assigned(Temp) do
      begin
        Inc(Counter);
        Temp := Temp.GetNextVisible;
      end;
      Height := Counter * ItemHeight;
    end
    else Height := (ScrollInfo.nMax + 1) * ItemHeight;
  end;
  // enable scroll button if necessary
  if (Tree3.Top + Tree3.Height) > Panel2.Height then DownButton.Enabled := True
                                                else
  begin
    DownButton.Enabled := False;
    if Tree3.Height <= Panel2.Height then
    begin
      Tree3.Top := 0;
      UpButton.enabled := False;
    end
    else
      if Tree3.Height - Tree3.Top < Panel2.Height then Tree3.Top := Panel2.Height + Tree3.Top; // Top is negative!
      if (Tree3.Top + Tree3.Height) < Panel2.Top then Tree3.Top := Panel2.Height - Tree3.Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree3Deletion(Sender: TObject; Node: TTreeNTNode);

begin
  if FTree3Building then exit;

  // if there's a tooltip string assigned then free it now
  if assigned(Node.Data) then StrDispose(Node.Data);
  Node.Data:= nil;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ScrollOneItem(Tag: Integer);

begin
  if Tag = 1 then
  begin
    Tree3.Top := Tree3.Top-Tree3.ItemHeight;
    if (Tree3.Top + Tree3.Height) <= Panel2.Height then
    begin
      DownButton.enabled := False;
      ScrollTimer.enabled := False;
    end;
    if Tree3.Top < 0 then UpButton.Enabled := True;
  end
  else
  begin
    Tree3.Top := Tree3.Top + Tree3.ItemHeight;
    if Tree3.Top >= 0 then
    begin
      UpButton.enabled := False;
      ScrollTimer.enabled := False;
      Tree3.Top := 0;
    end;
    if (Tree3.Top + Tree3.Height) > Panel2.Height then DownButton.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ScrollTimerTimer(Sender: TObject);

begin
  ScrollTimer.Interval := 50;
  ScrollOneItem(ScrollTimer.Tag);
end;

//------------------------------------------------------------------------------

procedure TMainForm.ScrollButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  ScrollTimer.Tag := (Sender as TSpeedButton).Tag;
  ScrollTimer.Interval := 300;
  ScrollTimer.enabled := True;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ScrollButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  ScrollTimer.enabled := False;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ScrollButtonClick(Sender: TObject);

begin
  ScrollOneItem((Sender as TSpeedButton).Tag);
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree3Change(Sender: TObject; Node: TTreeNTNode);

var R : TRect;

begin
  if not FTree3Building and assigned(Node) then
  begin
    R := Node.DisplayRect(False);
    if (R.Top + Tree3.Top) < 0 then Tree3.Top := -R.Top;
    if (R.Bottom + Tree3.Top) > Panel2.Height then Tree3.Top := Panel2.Height-R.Bottom;

    if (Tree3.Top + Tree3.Height) <= Panel2.Height then DownButton.enabled := False;
    if Tree3.Top < 0 then UpButton.Enabled := True;

    if Tree3.Top >= 0 then
    begin
      UpButton.enabled := False;
      Tree3.Top := 0;
    end;
    if (Tree3.Top + Tree3.Height) > Panel2.Height then DownButton.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree5BeforeItemPaint(Sender: TObject;
  Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates;
  var OwnerDraw: Boolean);

var I, L : Integer;

begin
  OwnerDraw := True;
  with Tree5 do
  begin
    I := ItemRect.Bottom - ItemRect.Top;
    L := ItemRect.Left + Node.Level * Indent;
    BitBlt(Canvas.Handle, 0, ItemRect.Top, FBackground.Width, I, FBackground.Canvas.Handle, 0, ItemRect.Top, SRCCOPY);
    if TreeNT.nsSelected in NodeStates then
    begin
      Canvas.Brush.Color := clBtnShadow;
      Canvas.FillRect(ItemRect);
    end       
    else Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := Font.Color;
    if nsHot in NodeStates then Canvas.Font.Style := [fsUnderline]
                           else Canvas.Font.Style := [];
    Canvas.TextOut(L, ItemRect.Top, Node.Text);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree5BeforePaint(Sender: TObject);

begin
  with Tree5 do
  begin
    Canvas.Font := Font;
    SetBkMode(Canvas.Handle, TRANSPARENT);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  FBackground.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree5Expanding(Sender: TObject; Node: TTreeNTNode; var AllowExpansion: Boolean);

var R : TRect;
    N : TTreeNTNode;

begin
  N := Node.GetNextSibling;
  if assigned(N) then
  begin
    R := N.DisplayRect(False);
    R.Bottom := Tree5.Height;
    InvalidateRect(Tree5.Handle, @R, False);
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.Tree5AfterPaint(Sender: TObject);

var Node : TTreeNTNode;
    R    : TRect;

begin
  with Tree5 do
  begin
    Node := LastVisibleNode;
    if assigned(Node) then
    begin
      R := Node.DisplayRect(False);
      if R.Bottom < Height then
      begin
        Canvas.Brush.Color := Color;
        R.Top := R.Bottom;
        R.Bottom := Height;
        Canvas.FillRect(R);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if FFirstRun then
  begin
    FFirstRun := False;
    Screen.Cursor := crHourGlass;
    try
      PageControl.ActivePage := TabSheet1;
      Application.ProcessMessages;
      FillTree1(2, 3);  //FillTree1(3, 15);



      Application.ProcessMessages;
      with Tree2 do
      begin
        Items.BeginUpdate;
        EnumFontFamilies(Self.Canvas.Handle, nil, @EnumFontCallback, Longint(Items));
        AlphaSort;
        Items.EndUpdate;
      end;

      FTree3Building := True;
      Application.ProcessMessages;
      FillTree3;
      Tree3.Selected := Tree3.Items.GetFirstNode;
      FTree3Building := False;
      Tree3.FullCollapse;
      Label24.Caption := Format('%d entries in history', [Tree3.Items.Count]);

      InsertMarkTimer.enabled  := True;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMainForm.ConstrainedCheckBoxClick(Sender: TObject);

begin
  if ConstrainedCheckBox.checked then Tree1.Options := Tree1.Options + [toLevelSelectConstraint]
                                 else Tree1.Options := Tree1.Options - [toLevelSelectConstraint];
end;

//------------------------------------------------------------------------------

end.
