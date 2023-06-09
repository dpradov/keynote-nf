{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1999,2000 Alexey Popov          }
{                                                       }
{*******************************************************}
{*******************************************************}
{                                                       }
{          adopted from Alex Ghost Library              }
{                                                       }
{*******************************************************}
unit RxNTSecurity;

{$I RX.INC}
{$A-}

interface

uses
  SysUtils, Windows, Classes;

type
  TWellKnown = DWORD;

  TAccessItem = class(TObject)
  public
    AceType: Byte;
    AceFlags: Byte;
    AceMask: DWORD;
    SID: PSID;
    function AccountName: string;
    function WellKnown: TWellKnown;
  end;

  TAccess = record
    AccType: Byte;
    Dirs, Files: DWORD;
    UseFiles: Boolean;
  end;

  TAccessList = class(TList)
  private
    function Get(const Index: Integer): TAccessItem;
  public
    destructor Destroy; override;
    property Items[const Index: Integer]: TAccessItem read Get; default;
    function IndexOfSID(SID: PSID): Integer;
    function AddItemSID(AType, AFlags: Byte; AMask: DWORD; ASID: PSID): Integer;
    function AddItemWellKnown(AType, AFlags: Byte; AMask: DWORD; WK: TWellKnown): Integer;
    function AddItemName(AType, AFlags: Byte; AMask: DWORD; const Name: string): Integer;
    procedure AddSID(Access: TAccess; SID: PSID; CheckDup: Boolean);
    procedure AddWellKnown(Access: TAccess; WK: TWellKnown; CheckDup: Boolean);
    procedure AddName(Access: TAccess; const Name: string; CheckDup: Boolean);
    procedure AddNames(Access: TAccess; Names: TStrings; CheckDup: Boolean);
  end;

  TSubAuthoritiesArray = array[0..7] of DWORD;

  TSetPermsCallback = procedure(Path: string; StartOp, Failed: Boolean;
    var Cancel: Boolean);

const

  // ACE types
  atAllowed = 0;
  atDenied = 1;

  // Standard access rights
  amNoAccess: TAccess = (AccType: atDenied;
    Dirs: GENERIC_ALL;
    Files: GENERIC_ALL;
    UseFiles: True);

  amList: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_READ or GENERIC_EXECUTE;
    Files: 0;
    UseFiles: False);

  amRead: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_READ or GENERIC_EXECUTE;
    Files: GENERIC_READ or GENERIC_EXECUTE;
    UseFiles: True);

  amAdd: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_WRITE or GENERIC_EXECUTE;
    Files: 0;
    UseFiles: False);

  amReadWrite: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE;
    Files: GENERIC_READ or GENERIC_EXECUTE;
    UseFiles: True);

  amChange: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE or _DELETE;
    Files: GENERIC_READ or GENERIC_WRITE or GENERIC_EXECUTE or _DELETE;
    UseFiles: True);

  amFullAccess: TAccess = (AccType: atAllowed;
    Dirs: GENERIC_ALL;
    Files: GENERIC_ALL;
    UseFiles: True);

  // ACE flags
  OBJECT_INHERIT_ACE = 1;
  CONTAINER_INHERIT_ACE = 2;
  NO_PROPAGATE_INHERIT_ACE = 4;
  INHERIT_ONLY_ACE = 8;
  VALID_INHERIT_FLAGS = 15;

  // some ACE flags combinations
  afDirectories = CONTAINER_INHERIT_ACE;
  afFiles = INHERIT_ONLY_ACE or OBJECT_INHERIT_ACE;

  // well-known SIDs
  wkNull = 0;
  wkWorld = 1;
  wkLocal = 2;
  wkCreatorOwner = 3;
  wkCreatorGroup = 3 + $100;
  wkNonUnique = 4;

  wkNTAuthority = 5;
  wkDialup = 5 + $100;
  wkNetwork = 5 + $200;
  wkBatch = 5 + $300;
  wkInteractive = 5 + $400;
  wkService = 5 + $600;
  wkAnonymous = 5 + $700;

  wkLogon = 5 + $500;

  wkLocalSystem = 5 + $1200;

  wkNTNonUnique = 5 + $1500;

  wkBuiltInDomain = 5 + $2000;

  wkDomainUserAdmin = 5 + $2000 + $1F40000; // domain controller only
  wkDomainUserGuest = 5 + $2000 + $1F50000; // domain controller only

  wkDomainGroupAdmins = 5 + $2000 + $2000000; // domain controller only
  wkDomainGroupUsers = 5 + $2000 + $2010000; // domain controller only
  wkDomainGroupGuests = 5 + $2000 + $2020000; // domain controller only

  wkDomainAliasAdmins = 5 + $2000 + $2200000;
  wkDomainAliasUsers = 5 + $2000 + $2210000;
  wkDomainAliasGuests = 5 + $2000 + $2220000;
  wkDomainAliasPowerUsers = 5 + $2000 + $2230000;
  wkDomainAliasAccountOps = 5 + $2000 + $2240000;
  wkDomainAliasSystemOps = 5 + $2000 + $2250000;
  wkDomainAliasPrintOps = 5 + $2000 + $2260000;
  wkDomainAliasBackupOps = 5 + $2000 + $2270000;
  wkDomainAliasReplicator = 5 + $2000 + $2280000;

  // current ACL revision
  ACL_REVISION = 2;

  WellKnownNameMark: Char = '*';

function CreateSID(IdAuthority: TSIDIdentifierAuthority; SubAuthorityCount: Byte;
  SubAuthorities: TSubAuthoritiesArray): PSID;

function CreateWellKnownSID(AWK: TWellKnown): PSID;

function MakeAccess(AccType: Byte; Dirs, Files: Cardinal; UseFiles: Boolean): TAccess;

procedure GetFileAccess(Path: string; AccessList: TAccessList);
procedure SetFileAccess(Path: string; AccessList: TAccessList);

procedure SetPermissions(Path: string; AccessList: TAccessList;
  ChangeFilePerms, RecursDirs: Boolean; CallbackProc: TSetPermsCallback);

function IsAdminRights: Boolean;
function IsPermissionsSupports(const FileName: string): Boolean;

type
  ESecurityError = class(Exception);

implementation

uses
  RxFileUtil;

type
  TAceHeader = record
    AceType: Byte;
    AceFlags: Byte;
    AceSize: Word;
  end;

  PAce = ^TAce;
  TAce = record
    Header: TAceHeader;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  end;

  TWellKnownRec = record
    WK: TWellKnown;
    Name: string;
  end;

  TAllocateAndInitializeSid = function(const pIdentifierAuthority: TSIDIdentifierAuthority;
    nSubAuthorityCount: Byte; nSubAuthority0, nSubAuthority1: DWORD;
    nSubAuthority2, nSubAuthority3, nSubAuthority4: DWORD;
    nSubAuthority5, nSubAuthority6, nSubAuthority7: DWORD;
    var pSid: Pointer): BOOL; stdcall;

  TLookupAccountSid = function(lpSystemName: PChar; Sid: PSID;
    Name: PChar; var cbName: DWORD; ReferencedDomainName: PChar;
    var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): BOOL; stdcall;

  TGetSidIdentifierAuthority = function(pSid: Pointer): PSIDIdentifierAuthority; stdcall;

  TGetSidSubAuthority = function(pSid: Pointer; nSubAuthority: DWORD): PDWORD; stdcall;

  TGetSidSubAuthorityCount = function(pSid: Pointer): PUCHAR; stdcall;

  TEqualSid = function(pSid1, pSid2: Pointer): BOOL; stdcall;

  TGetLengthSid = function(pSid: Pointer): DWORD; stdcall;

  TCopySid = function(nDestinationSidLength: DWORD;
    pDestinationSid, pSourceSid: Pointer): BOOL; stdcall;

  TLookupAccountName = function(lpSystemName, lpAccountName: PChar;
    Sid: PSID; var cbSid: DWORD; ReferencedDomainName: PChar;
    var cbReferencedDomainName: DWORD; var peUse: SID_NAME_USE): BOOL; stdcall;

  TGetFileSecurity = function(lpFileName: PChar; RequestedInformation: SECURITY_INFORMATION;
    pSecurityDescriptor: PSecurityDescriptor; nLength: DWORD;
    var lpnLengthNeeded: DWORD): BOOL; stdcall;

  TGetSecurityDescriptorDacl = function(pSecurityDescriptor: PSecurityDescriptor;
    var lpbDaclPresent: BOOL; var pDacl: PACL; var lpbDaclDefaulted: BOOL): BOOL; stdcall;

  TGetAce = function(const pAcl: TACL; dwAceIndex: DWORD; var pAce: Pointer): BOOL; stdcall;

  TInitializeAcl = function(var pAcl: TACL; nAclLength, dwAclRevision: DWORD): BOOL; stdcall;

  TAddAce = function(var pAcl: TACL; dwAceRevision, dwStartingAceIndex: DWORD;
    pAceList: Pointer; nAceListLength: DWORD): BOOL; stdcall;

  TInitializeSecurityDescriptor = function(pSecurityDescriptor: PSecurityDescriptor;
    dwRevision: DWORD): BOOL; stdcall;

  TSetSecurityDescriptorDacl = function(pSecurityDescriptor: PSecurityDescriptor;
    bDaclPresent: BOOL; pDacl: PACL; bDaclDefaulted: BOOL): BOOL; stdcall;

  TSetFileSecurity = function(lpFileName: PChar; SecurityInformation: SECURITY_INFORMATION;
    pSecurityDescriptor: PSecurityDescriptor): BOOL; stdcall;

  TOpenProcessToken = function(ProcessHandle: THandle; DesiredAccess: DWORD;
    var TokenHandle: THandle): BOOL; stdcall;

  TGetTokenInformation = function(TokenHandle: THandle;
    TokenInformationClass: TTokenInformationClass; TokenInformation: Pointer;
    TokenInformationLength: DWORD; var ReturnLength: DWORD): BOOL; stdcall;

  TFreeSid = function(pSid: Pointer): Pointer; stdcall;

var
  AllocateAndInitializeSidProc: TAllocateAndInitializeSid;
  LookupAccountSidProc: TLookupAccountSid;
  GetSidIdentifierAuthorityProc: TGetSidIdentifierAuthority;
  GetSidSubAuthorityProc: TGetSidSubAuthority;
  GetSidSubAuthorityCountProc: TGetSidSubAuthorityCount;
  EqualSidProc: TEqualSid;
  GetLengthSidProc: TGetLengthSid;
  CopySidProc: TCopySid;
  LookupAccountNameProc: TLookupAccountName;
  GetFileSecurityProc: TGetFileSecurity;
  GetSecurityDescriptorDaclProc: TGetSecurityDescriptorDacl;
  GetAceProc: TGetAce;
  InitializeAclProc: TInitializeAcl;
  AddAceProc: TAddAce;
  InitializeSecurityDescriptorProc: TInitializeSecurityDescriptor;
  SetSecurityDescriptorDaclProc: TSetSecurityDescriptorDacl;
  SetFileSecurityProc: TSetFileSecurity;
  OpenProcessTokenProc: TOpenProcessToken;
  GetTokenInformationProc: TGetTokenInformation;
  FreeSidProc: TFreeSid;

  DiskPermSupp: TStringList;

const
  AdvApi32Lib: THandle = 0;

  WellKnownArray: array[1..26] of TWellKnownRec = (
    (WK: wkWorld; Name: 'Everyone'),
    (WK: wkLocal; Name: 'Local'),
    (WK: wkCreatorOwner; Name: 'CreatorOwner'),
    (WK: wkCreatorGroup; Name: 'CreatorGroup'),
    (WK: wkDialup; Name: 'Dialup'),
    (WK: wkNetwork; Name: 'Network'),
    (WK: wkBatch; Name: 'Batch'),
    (WK: wkInteractive; Name: 'Interactive'),
    (WK: wkService; Name: 'Service'),
    (WK: wkAnonymous; Name: 'Anonymous'),
    (WK: wkLogon; Name: 'Logon'),
    (WK: wkLocalSystem; Name: 'System'),
    (WK: wkDomainUserAdmin; Name: 'Admin'),
    (WK: wkDomainUserGuest; Name: 'Guest'),
    (WK: wkDomainGroupAdmins; Name: 'DomainAdmins'),
    (WK: wkDomainGroupUsers; Name: 'DomainUsers'),
    (WK: wkDomainGroupGuests; Name: 'DomainGuests'),
    (WK: wkDomainAliasAdmins; Name: 'Admins'),
    (WK: wkDomainAliasUsers; Name: 'Users'),
    (WK: wkDomainAliasGuests; Name: 'Guests'),
    (WK: wkDomainAliasPowerUsers; Name: 'PowerUsers'),
    (WK: wkDomainAliasAccountOps; Name: 'AccountOps'),
    (WK: wkDomainAliasSystemOps; Name: 'SystemOps'),
    (WK: wkDomainAliasPrintOps; Name: 'PrintOps'),
    (WK: wkDomainAliasBackupOps; Name: 'BackupOps'),
    (WK: wkDomainAliasReplicator; Name: 'Replicator')
    );

  {$IFNDEF D4}
  SECURITY_DESCRIPTOR_REVISION = 1;
  {$ENDIF}

{system resource - no localize}
resourcestring
  EAdvApiNotLoaded = 'AdvApi32.dll not loaded';
  ECantInitSid = 'Can''t initialize a SID';
  ECantCopySid = 'Can''t create a copy of SID';
  EAccountNotFound = 'Account not found';
  EDuplicateAccount = 'Account already exists in access list';
  EPermissionsNotSupports = 'Getting/setting permissions not supported on this path';
  ECantGetSecurityInfo = 'Can''t get file security information';
  ECantSetSecurityInfo = 'Can''t set file security information';
  ECantGetDACL = 'Can''t get DACL';
  ECantSetDACL = 'Can''t set DACL';
  ECantInitACL = 'Can''t initialize ACL';
  ECantAddAce = 'Can''t add ACE to an ACL';
  ECantInitSecurDesc = 'Can''t initialize a security descriptor';
  ECancelled = 'Setting permissions was cancelled';

function CreateSID(IdAuthority: TSIDIdentifierAuthority; SubAuthorityCount: Byte;
  SubAuthorities: TSubAuthoritiesArray): PSID;
begin
  Result := nil;
  if AdvApi32Lib = 0 then raise ESecurityError.Create(EAdvApiNotLoaded);
  if not AllocateAndInitializeSidProc(IdAuthority, SubAuthorityCount, SubAuthorities[0],
    SubAuthorities[1], SubAuthorities[2], SubAuthorities[3], SubAuthorities[4],
    SubAuthorities[5], SubAuthorities[6], SubAuthorities[7], Result) then raise ESecurityError.Create(ECantInitSid);
end;

function CreateWellKnownSID(AWK: TWellKnown): PSID;
var
  IdAuthority: TSIDIdentifierAuthority;
  SubAuthorityCount: Byte;
  SubAuthorities: TSubAuthoritiesArray;
begin
  FillChar(SubAuthorities, SizeOf(SubAuthorities), 0);
  FillChar(IdAuthority, SizeOf(IdAuthority), 0);
  IdAuthority.Value[5] := LoByte(LoWord(AWK));
  SubAuthorityCount := 1;
  SubAuthorities[0] := HiByte(LoWord(AWK));
  SubAuthorities[1] := HiWord(AWK);
  if SubAuthorities[1] <> 0 then Inc(SubAuthorityCount);
  Result := CreateSID(IdAuthority, SubAuthorityCount, SubAuthorities);
end;

function MakeAccess(AccType: Byte; Dirs, Files: Cardinal; UseFiles: Boolean): TAccess;
begin
  Result.AccType := AccType;
  Result.Dirs := Dirs;
  Result.Files := Files;
  Result.UseFiles := UseFiles;
end;

function NameToWellKnown(const AName: string): TWellKnown;
var
  i: Integer;
  s: string;
begin
  Result := wkNull;
  i := Pos(WellKnownNameMark, AName);
  if i > 0 then
  begin
    s := Copy(AName, i + 1, Length(AName) - i);
    for i := 1 to High(WellKnownArray) do
      if CompareText(WellKnownArray[i].Name, s) = 0 then
      begin
        Result := WellKnownArray[i].WK;
        Break;
      end;
  end;
end;

{ TAccessItem }

function TAccessItem.AccountName: string;
var
  User, Dom: string;
  us, ds, use: DWORD;
begin
  Result := '';
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  us := 0; ds := 0;
  LookupAccountSidProc(nil, SID, nil, us, nil, ds, use);
  SetLength(User, us);
  SetLength(Dom, ds);
  if LookupAccountSidProc(nil, SID, PChar(User), us, PChar(Dom), ds, use) then
  begin
    SetLength(User, us);
    SetLength(Dom, ds);
    Result := Format('%s\%s', [Dom, User]); ;
  end;
end;

function TAccessItem.WellKnown: TWellKnown;
var
  auth: PSIDIdentifierAuthority;
begin
  if AdvApi32Lib = 0 then raise ESecurityError.Create(EAdvApiNotLoaded);
  auth := GetSidIdentifierAuthorityProc(SID);
  Result := auth^.Value[5] + (GetSidSubAuthorityProc(SID, 0)^ shl 8);
  if (Result = wkBuiltInDomain) and (GetSidSubAuthorityCountProc(SID)^ > 1) then
    Inc(Result, (GetSidSubAuthorityProc(SID, 1)^ shl 16));
end;

{ TAccessList }

destructor TAccessList.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    // FreeSidProc(Items[i].SID);
    FreeMem(Items[i].SID);
    Items[i].Free;
  end;
  inherited Destroy;
end;

function TAccessList.Get(const Index: Integer): TAccessItem;
begin
  Result := inherited Items[Index];
end;

function TAccessList.IndexOfSID(SID: PSID): Integer;
var
  i: Integer;
begin
  Result := -1;
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  for i := 0 to Count - 1 do
    if EqualSidProc(Items[i].SID, SID) then
    begin
      Result := i;
      Break;
    end;
end;

function TAccessList.AddItemSID(AType, AFlags: Byte; AMask: DWORD;
  ASID: PSID): Integer;
var
  ai: TAccessItem;
  sidsize: Integer;
begin
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  ai := TAccessItem.Create;
  with ai do
  begin
    AceType := AType;
    AceFlags := AFlags;
    AceMask := AMask;
    sidsize := GetLengthSidProc(ASID);
    GetMem(SID, sidsize);
    if not CopySidProc(sidsize, SID, ASID) then
      raise ESecurityError.Create(ECantCopySid);
  end;
  Result := Add(ai);
end;

function TAccessList.AddItemName(AType, AFlags: Byte; AMask: DWORD;
  const Name: string): Integer;
var
  ss, ds, use: DWORD;
  SID: PSID;
  Dom: string;
  WK: TWellKnown;
begin
  Result := -1;
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  WK := NameToWellKnown(Name);
  if WK <> wkNull then
    Result := AddItemWellKnown(AType, AFlags, AMask, WK)
  else
  begin
    ss := 0;
    ds := 0;
    LookupAccountNameProc(nil, PChar(Name), nil, ss, nil, ds, use);
    GetMem(SID, ss);
    try
      SetLength(Dom, ds);
      if LookupAccountNameProc(nil, PChar(Name), SID, ss, PChar(Dom), ds, use) then
        Result := AddItemSID(AType, AFlags, AMask, SID)
      else
        raise ESecurityError.Create(EAccountNotFound);
    finally
      FreeMem(SID);
    end;
  end;
end;

function TAccessList.AddItemWellKnown(AType, AFlags: Byte; AMask: DWORD;
  WK: TWellKnown): Integer;
var
  SID: PSID;
begin
  SID := CreateWellKnownSID(WK);
  try
    Result := AddItemSID(AType, AFlags, AMask, SID);
  finally
    FreeSidProc(SID);
  end;
end;

procedure TAccessList.AddSID(Access: TAccess; SID: PSID; CheckDup: Boolean);
begin
  if CheckDup then
    if IndexOfSID(SID) <> -1 then
      raise ESecurityError.Create(EDuplicateAccount);
  AddItemSID(Access.AccType, afDirectories, Access.Dirs, SID);
  if Access.UseFiles then
    AddItemSID(Access.AccType, afFiles, Access.Files, SID);
end;

procedure TAccessList.AddName(Access: TAccess; const Name: string; CheckDup: Boolean);
var
  ss, ds, use: DWORD;
  sid: PSID;
  Dom: string;
  wk: TWellKnown;
begin
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  wk := NameToWellKnown(Name);
  if wk <> wkNull then
    AddWellKnown(Access, wk, CheckDup)
  else
  begin
    ss := 0; ds := 0;
    LookupAccountNameProc(nil, PChar(Name), nil, ss, nil, ds, use);
    GetMem(sid, ss);
    try
      SetLength(Dom, ds);
      if LookupAccountNameProc(nil, PChar(Name), sid, ss, PChar(Dom), ds, use) then
      begin
        if CheckDup then
          if IndexOfSID(sid) <> -1 then raise ESecurityError.Create(EDuplicateAccount);
        AddItemSID(Access.AccType, afDirectories, Access.Dirs, sid);
        if Access.UseFiles then
          AddItemSID(Access.AccType, afFiles, Access.Files, sid);
      end
      else
        raise ESecurityError.Create(EAccountNotFound);
    finally
      FreeMem(sid);
    end;
  end;
end;

procedure TAccessList.AddNames(Access: TAccess; Names: TStrings; CheckDup: Boolean);
var
  i: Integer;
begin
  for i := 0 to Names.Count - 1 do
    AddName(Access, Names[i], CheckDup);
end;

procedure TAccessList.AddWellKnown(Access: TAccess; WK: TWellKnown; CheckDup: Boolean);
var
  sid: PSID;
begin
  sid := CreateWellKnownSID(WK);
  try
    if CheckDup then
      if IndexOfSID(sid) <> -1 then
        raise ESecurityError.Create(EDuplicateAccount);
    AddItemSID(Access.AccType, afDirectories, Access.Dirs, sid);
    if Access.UseFiles then
      AddItemSID(Access.AccType, afFiles, Access.Files, sid);
  finally
    FreeSidProc(sid);
  end;
end;

{ GetFileAccess }

procedure GetFileAccess(Path: string; AccessList: TAccessList);
var
  sd: Pointer;
  size: DWORD;
  acl: PACL;
  ace: PAce;
  DaclPresent, DaclDef: LongBool;
  i: Integer;
  serv: string;
begin
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);
  if not IsPermissionsSupports(Path) then
    raise ESecurityError.Create(EPermissionsNotSupports);
  AccessList.Clear;
  // find name of a remote computer
  serv := ExpandUNCFileName(Path);
  if Copy(serv, 1, 2) = '\\' then
  begin
    Delete(serv, 1, 2);
    serv := Copy(serv, 1, Pos('\', serv) - 1);
  end
  else
    serv := '';
  // get file security descriptor size
  GetFileSecurityProc(PChar(Path), DACL_SECURITY_INFORMATION, nil, 0, size);
  GetMem(sd, size);
  try
    // get file security information (DACL)
    if not GetFileSecurityProc(PChar(Path), DACL_SECURITY_INFORMATION, sd,
      size, size) then
      raise ESecurityError.Create(ECantGetSecurityInfo);
    // get DACL from security descriptor
    if not GetSecurityDescriptorDaclProc(sd, DaclPresent, acl, DaclDef) then
      raise ESecurityError.Create(ECantGetDACL);
    if not DaclPresent then
      Exit;
    if Assigned(acl) then
      // add information from ACEs to AccessList
      for i := 0 to acl^.AceCount - 1 do
        if GetAceProc(acl^, i, Pointer(ace)) then
          with ace^ do
            AccessList.AddItemSID(Header.AceType, Header.AceFlags, Mask,
              @SidStart);
  finally
    FreeMem(sd);
  end;
end;

{ SetFileAccess }

procedure SetFileAccess(Path: string; AccessList: TAccessList);

  procedure AddAccessItem(Item: TAccessItem; acl: PACL);
  var
    ace: PAce;
    AceSize: Cardinal;
  begin
    with Item do
    begin
      // create ACE
      AceSize := SizeOf(TAce) + GetLengthSidProc(SID) - SizeOf(DWORD);
      GetMem(ace, AceSize);
      try
        with PAce(ace)^ do
        begin
          // fill ACE
          Header.AceType := AceType;
          Header.AceFlags := AceFlags;
          Header.AceSize := AceSize;
          Mask := AceMask;
          if not CopySidProc(GetLengthSidProc(SID), @SidStart, SID) then
            raise ESecurityError.Create(ECantCopySid);
        end;
        // add ACE to ACL
        if not AddAceProc(acl^, ACL_REVISION, 0, ace, AceSize) then
          raise ESecurityError.Create(ECantAddAce);
      finally
        FreeMem(ace);
      end;
    end;
  end;

var
  sd: PSecurityDescriptor;
  acl: PACL;
  aclsize: Cardinal;
  i: Integer;
begin
  if AdvApi32Lib = 0 then raise ESecurityError.Create(EAdvApiNotLoaded);
  if not IsPermissionsSupports(Path) then
    raise ESecurityError.Create(EPermissionsNotSupports);
  // calculate ACL size
  aclsize := SizeOf(TACL);
  for i := 0 to AccessList.Count - 1 do
    Inc(aclsize, SizeOf(TACE) + GetLengthSidProc(AccessList[i].SID) - SizeOf(DWORD));
  // create new ACL
  GetMem(acl, aclsize);
  // create new security descriptor
  New(sd);
  try
    // init ACL
    if not InitializeAclProc(acl^, aclsize, ACL_REVISION) then
      raise ESecurityError.Create(ECantInitACL);
    // add deny ACEs (must be first)
    for i := 0 to AccessList.Count - 1 do
      if AccessList[i].AceType = atDenied then AddAccessItem(AccessList[i], acl);
    // add other ACEs
    for i := 0 to AccessList.Count - 1 do
      if AccessList[i].AceType <> atDenied then AddAccessItem(AccessList[i], acl);
    // init security descriptor
    if not InitializeSecurityDescriptorProc(sd, SECURITY_DESCRIPTOR_REVISION) then
      raise ESecurityError.Create(ECantInitSecurDesc);
    // fill security descriptor (set DACL)
    if not SetSecurityDescriptorDaclProc(sd, True, acl, False) then
      raise ESecurityError.Create(ECantSetDACL);
    // set file security information (DACL from security descriptor)
    if not SetFileSecurityProc(PChar(Path), DACL_SECURITY_INFORMATION, sd) then
      raise ESecurityError.Create(ECantSetSecurityInfo);
  finally
    Dispose(sd);
    FreeMem(acl);
  end;
end;

procedure SetPermissions(Path: string; AccessList: TAccessList;
  ChangeFilePerms, RecursDirs: Boolean; CallbackProc: TSetPermsCallback);
var
  sr: TSearchRec;
  i: Integer;
  Cancel: Boolean;
begin
  if AdvApi32Lib = 0 then
    raise ESecurityError.Create(EAdvApiNotLoaded);

  Path := RemoveBackSlash(Path);

  // start directory processing callback
  if Assigned(CallbackProc) then
  begin
    Cancel := False;
    CallbackProc(Path, True, False, Cancel);
    if Cancel then
      raise ESecurityError.Create(ECancelled);
  end;

  try
    // set permissions to directory Path
    SetFileAccess(Path, AccessList);
    // process files
    if ChangeFilePerms then
    begin
      i := SysUtils.FindFirst(Path + '\*.*', faAnyFile - (faDirectory{$IFDEF VER80} + faVolumeID{$ENDIF}), sr);
      try
        while i = 0 do
        begin
          SetFileAccess(Path + '\' + sr.Name, AccessList);
          i := SysUtils.FindNext(sr);
        end;
      finally
        SysUtils.FindClose(sr);
      end;
    end;
  except
    if Assigned(CallbackProc) then
      CallbackProc(Path, False, True, Cancel);
    raise;
  end;

  // end directory processing callback
  if Assigned(CallbackProc) then
  begin
    Cancel := False;
    CallbackProc(Path, False, False, Cancel);
    if Cancel then
      raise ESecurityError.Create(ECancelled);
  end;

  if RecursDirs then
  begin
    // process subdirectories
    i := SysUtils.FindFirst(Path + '\*.*', faDirectory, sr);
    try
      while i = 0 do
      begin
        SetPermissions(Path + '\' + sr.Name, AccessList, ChangeFilePerms,
          RecursDirs, CallbackProc);
        i := SysUtils.FindNext(sr);
      end;
    finally
      SysUtils.FindClose(sr);
    end;
  end;
end;

function IsAdminRights: Boolean;
var
  hProcess, hToken: THandle;
  InfoBuffer: PTokenGroups;
  size: DWORD;
  i: Integer;
  AdminsSid: PSID;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Result := True
  else
    Result := False;
  if AdvApi32Lib = 0 then
    Exit;

  hProcess := GetCurrentProcess;
  if not OpenProcessTokenProc(hProcess, TOKEN_READ, hToken) then
    Exit;
  GetTokenInformationProc(hToken, TokenGroups, nil, 0, size);
  GetMem(InfoBuffer, size);
  AdminsSid := CreateWellKnownSID(wkDomainAliasAdmins);
  try
    if not GetTokenInformationProc(hToken, TokenGroups, InfoBuffer, size, size) then
      Exit;
    for i := 0 to InfoBuffer^.GroupCount - 1 do
      if EqualSidProc(InfoBuffer^.Groups[i].SID, AdminsSid) then
      begin
        Result := True;
        Break;
      end;
  finally
    FreeSidProc(AdminsSid);
    FreeMem(InfoBuffer);
  end;
end;

function IsPermissionsSupports(const FileName: string): Boolean;
var
  Root: string;
  MaxCompLen, FSFlags: DWORD;
  i: Integer;
begin
  Result := False;
  Root := ExtractFileDrive(FileName);
  if Root = '' then
    Root := ExtractFileDrive(GetCurrentDir);

  i := DiskPermSupp.IndexOf(Root);
  if i <> -1 then
  begin
    Result := Boolean(DiskPermSupp.Objects[i]);
    Exit;
  end;

  if GetVolumeInformation(PChar(NormalDir(Root)), nil, 0, nil, MaxCompLen,
    FSFlags, nil, 0) then
  begin
    if (FSFlags and FS_PERSISTENT_ACLS) = FS_PERSISTENT_ACLS then
      Result := True;
    DiskPermSupp.AddObject(Root, Pointer(Result));
  end;
end;

procedure LoadAdvApi32Dll;
begin
  if Win32Platform in [VER_PLATFORM_WIN32s, VER_PLATFORM_WIN32_WINDOWS] then
    Exit;
  AdvApi32Lib := LoadLibrary('advapi32.dll');
  if AdvApi32Lib <> 0 then
  begin
    AllocateAndInitializeSidProc := GetProcAddress(AdvApi32Lib,
      'AllocateAndInitializeSid');
    LookupAccountSidProc := GetProcAddress(AdvApi32Lib, 'LookupAccountSidA');
    GetSidIdentifierAuthorityProc := GetProcAddress(AdvApi32Lib,
      'GetSidIdentifierAuthority');
    GetSidSubAuthorityProc := GetProcAddress(AdvApi32Lib, 'GetSidSubAuthority');
    GetSidSubAuthorityCountProc := GetProcAddress(AdvApi32Lib,
      'GetSidSubAuthorityCount');
    EqualSidProc := GetProcAddress(AdvApi32Lib, 'EqualSid');
    GetLengthSidProc := GetProcAddress(AdvApi32Lib, 'GetLengthSid');
    CopySidProc := GetProcAddress(AdvApi32Lib, 'CopySid');
    LookupAccountNameProc := GetProcAddress(AdvApi32Lib, 'LookupAccountNameA');
    GetFileSecurityProc := GetProcAddress(AdvApi32Lib, 'GetFileSecurityA');
    GetSecurityDescriptorDaclProc := GetProcAddress(AdvApi32Lib,
      'GetSecurityDescriptorDacl');
    GetAceProc := GetProcAddress(AdvApi32Lib, 'GetAce');
    InitializeAclProc := GetProcAddress(AdvApi32Lib, 'InitializeAcl');
    AddAceProc := GetProcAddress(AdvApi32Lib, 'AddAce');
    InitializeSecurityDescriptorProc := GetProcAddress(AdvApi32Lib,
      'InitializeSecurityDescriptor');
    SetSecurityDescriptorDaclProc := GetProcAddress(AdvApi32Lib,
      'SetSecurityDescriptorDacl');
    SetFileSecurityProc := GetProcAddress(AdvApi32Lib, 'SetFileSecurityA');
    OpenProcessTokenProc := GetProcAddress(AdvApi32Lib, 'OpenProcessToken');
    GetTokenInformationProc := GetProcAddress(AdvApi32Lib,
      'GetTokenInformation');
    FreeSidProc := GetProcAddress(AdvApi32Lib, 'FreeSid');
  end;
end;

initialization
  LoadAdvApi32Dll;
  DiskPermSupp := TStringList.Create;
finalization
  DiskPermSupp.Free;
  if AdvApi32Lib <> 0 then
    FreeLibrary(AdvApi32Lib);
end.