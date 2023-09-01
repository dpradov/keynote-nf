Unit gf_FileAssoc;

(****** LICENSE INFORMATION **************************************************

  by Peter Below (TeamB)}
  - with modifications by [MJ] to make sure registeru entries are correctly quoted}

  - Original in: <dpradov/keynote-nf> 3rd_party\_Others\_associations.pas.zip

  Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
  in https://github.com/dpradov/keynote-nf

 ----
 sept/23:
  Created new method by [dpv], based in example in
   https://subscription.packtpub.com/book/programming/9781783559589/1/ch01lvl1sec19/associating-a-file-extension-with-your-application-on-windows
  and in the use of current methods, here

 *****************************************************************************)



Interface

Function  FiletypeIsRegistered( Const extension, filetype: String ) : Boolean;                            // [dpv]
Procedure RegisterFiletype(
        Const extension, filetype, description, verb, ApplicationFullPath: string;                        // [dpv]
        ICONResourceFileFullPath: String = '');


//Procedure RegisterFiletype( Const extension, filetype, description, verb: String; ApplicationPath, Params: String );
//Procedure RegisterFileIcon( Const filetype : string; iconsource: String; iconindex: Cardinal );
//Function  FiletypeIsRegistered( Const extension, filetype: String ) : Boolean;

Implementation

uses
   Winapi.Windows,
   Winapi.shlobj,
   System.Win.Registry,
   System.Classes,
   System.SysUtils,
   gf_strings;

{
ResourceString
  msgECannotCreateKeyF =
   'Cannot create key %s, the user account may not have the required '+
   'rights to create registry keys under HKEY_CLASSES_ROOT.';

Type
  ERegistryError = Class( Exception );
}


Function FiletypeIsRegistered( Const extension, filetype: String ): Boolean;
Var
  R: TRegistry;
  regstrpos : integer;
  keystring: string;
Begin
  Result := False;

  R:= TRegistry.Create (KEY_READ);
  Try
    R.Rootkey := HKEY_CURRENT_USER;

    if R.OpenKey('\Software\Classes\' + extension, false) then begin
      keystring := R.ReadString('');                                    // Extension is registered, check filetype
      if CompareText( keystring, filetype) = 0 Then Begin               // Filetype is registered for this extension, check server
          if R.OpenKey('\Software\Classes\' + filetype + '\shell\open\command', false) then begin
             keystring := AnsiUpperCase( R.ReadString(''));
             R.CloseKey;
             regstrpos := Pos( AnsiUpperCase(ParamStr(0)), keystring );
             if (( regstrpos = 1 ) or ( regstrpos = 2 )) then           // server string may be enclosed in quotes
               Result := True;                                          // Yes, server matches!
        end;
      End;
    end;


  finally
    R.free;
  end;

end;



Procedure RegisterFiletype(
        Const extension, filetype, description, verb, ApplicationFullPath: string;
        ICONResourceFileFullPath: String = '');
Var
  R: TRegistry;
Begin
  R := TRegistry.Create;
  try
  {
    if OnlyForCurrentUser then
      R.RootKey := HKEY_CURRENT_USER
    else
      R.RootKey := HKEY_LOCAL_MACHINE;
  }
    R.RootKey := HKEY_CURRENT_USER;

    if ICONResourceFileFullPath = '' then
       ICONResourceFileFullPath:= ApplicationFullPath;

    if R.OpenKey('\Software\Classes\' + extension, true) then begin
      R.WriteString('', filetype);
      if R.OpenKey('\Software\Classes\' + filetype, true) then begin
        R.WriteString('', description);
        if R.OpenKey('\Software\Classes\' + filetype + '\DefaultIcon', true) then  begin
          R.WriteString('', ICONResourceFileFullPath);
          if R.OpenKey('\Software\Classes\' + filetype + '\shell\' + verb + '\command', true) then
            R.WriteString('', ApplicationFullPath + ' "%1"');
        end;
      end;
    end;
  finally
    R.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);
End;




{+------------------------------------------------------------
 | Procedure CreateKey
 |
 | Visibility : restricted to unit
 | Description:
 |   This is a helper function which uses the passed reg object
 |   to create a registry key.
 | Error Conditions:
 |   If the key cannot be created a ERegistryError exception is
 |   raised.
 | Created: 14.03.99 by P. Below
 +------------------------------------------------------------}
 (*
Procedure CreateKey( reg: TRegistry; Const keyname: String );
  Begin
    If not reg.OpenKey( keyname, True ) Then
      raise ERegistryError.CreateFmt( msgECannotCreateKeyF, [keyname] );
  End; { CreateKey }
 *)
{+------------------------------------------------------------
 | Procedure InternalRegisterFiletype
 |
 | Parameters :
 |   extension  : file extension, including the dot, to register
 |   filetype   : string to use as key for the file extension
 |   description: string to show in Explorer for files with this
 |                extension. If description is empty the file
 |                type will not show up in Explorers list of
 |                registered associations!
 |   verb       : action to register, 'open', 'edit', 'print' etc.
 |                The action will turn up as entry in the files
 |                context menu in Explorer.
 |   serverapp  : full pathname of the executable to associate with
 |                the file extension, including any command line
 |                switches. Include the "%1" placeholder as well.
 |                Actions like printto may require more than one
 |                placeholder.
 | Visibility : restricted to unit
 | Description:
 |   Creates the three basic registry keys for a file extension.
 |   HKCR\<extension> = <filetype>
 |   HKCR\<filetype>  = <description>
 |   HKCR\<filetype>\shell\<verb>\command = <serverapp>
 |   If the keys already exist they are overwritten!
 | Error Conditions:
 |   A ERegistryError exception will result if a key cannot be
 |   created. Failure to create a key is usually due to insufficient
 |   user rights and only a problem on NT.
 | Created: 14.03.99 by P. Below
 +------------------------------------------------------------}
 (*    Requires administrative rights
Procedure InternalRegisterFiletype( Const extension, filetype,
description,
             verb, serverapp: String );
  Var
    reg: TRegistry;
    keystring: String;
  Begin
    reg:= TRegistry.Create;
    Try
      reg.Rootkey := HKEY_CLASSES_ROOT;
      CreateKey( reg, extension );
      reg.WriteString( '', filetype );
      reg.CloseKey;
      CreateKey( reg, filetype );
      reg.WriteString('', description );
      reg.closekey;
      keystring := Format('%s\shell\%s\command', [filetype, verb] );
      CreateKey( reg, keystring );
      reg.WriteString( '', serverapp);
      reg.CloseKey;
    Finally
      reg.free;
    End;
  End; { InternalRegisterFiletype }
*)



{+------------------------------------------------------------
 | Procedure RegisterFiletype
 |
 | Parameters :
 |   extension  : file extension, including the dot, to register
 |   filetype   : string to use as key for the file extension
 |   description: string to show in Explorer for files with this
 |                extension. If description is empty the file
 |                type will not show up in Explorers list of
 |                registered associations!
 |   verb       : action to register, 'open', 'edit', 'print' etc.
 |                The action will turn up as entry in the files
 |                context menu in Explorer.
 |   params     : The command line parameters to pass to the
 |                app when a file action is requested. If this
 |                parameter is empty "%1" is used by default.
 | Visibility : exported from unit
 | Description:
 |   Builds the commandline to use from the applications filename
 |   and the passed params and hands the rest of the work off to
 |   InternalRegisterFiletype.
 | Error Conditions: none
 | Created: 20.03.99 by P. Below
 +------------------------------------------------------------}
 (*
Procedure RegisterFiletype( Const extension, filetype, description,
             verb: String; ApplicationPath, Params: String );
  Begin
    // [MJ]
    if ( Params = '' ) then
      params := '%1'
    else
      UnquoteString( Params ); // make sure the string is quoted, but do not add two layers of quotes

    // [MJ]
    if ( ApplicationPath = '' ) then
      ApplicationPath := ParamStr( 0 )
    else
      UnquoteString( ApplicationPath );

    InternalRegisterFiletype(
      extension, filetype, description, verb,
      Format( '"%s" "%s"', [ApplicationPath, Params] ));     // [MJ]
  End; { RegisterFiletype }
*)

{+------------------------------------------------------------
 | Procedure RegisterFileIcon
 |
 | Parameters :
 |   filetype  : file type key name to register the icon for
 |   iconsource: full pathname of the executable or ICO file
 |               that contains the icon
 |   iconindex : index of the icon to use, if several are containd
 |               in iconsource. Counts from 0!
 | Visibility : exported from unit
 | Description:
 |   Creates the registry keys required to tell Explorer which icon
 |   to display for files of this type. RegisterFileType needs
 |   to be called first to associate the filetype with an extension.
 |   The registry key added is
 |   HKCR\<filetype>\DefaultIcon = <iconsource>,<iconindex>
 |   If the key already exists it is overwritten!
 |   The icon specified should contain both large (32*32) and small
 |   (16*16) versions of the icon, to optain optimal display
 |    quality. If only one icon format is present Windows will
 |    generate the other from it.
 | Error Conditions:
 |   A ERegistryError exception will result if a key cannot be
 |   created. Failure to create a key is usually due to insufficient
 |   user rights and only a problem on NT.
 | Error Conditions: none
 | Created: 21.03.99 by P. Below
 +------------------------------------------------------------}
 (*
Procedure RegisterFileIcon( Const filetype : string; iconsource: String;
                            iconindex: Cardinal );
  Var
    reg: TRegistry;
    keystring: String;
  Begin
    reg:= TRegistry.Create;
    Try
      reg.Rootkey := HKEY_CLASSES_ROOT;
      keystring := Format( '%s\DefaultIcon',[filetype] );
      CreateKey( reg, keystring );

      // [MJ]
      if ( iconsource = '' ) then
        iconsource := ParamStr( 0 )
      else
        UnquoteString( iconsource );

      reg.WriteString( '', Format( '"%s",%d', [iconsource,iconindex] )); // [MJ]
      reg.CloseKey;
    Finally
      reg.free;
    End;
  End; { RegisterFileIcon }
  *)

{+------------------------------------------------------------
 | Function FiletypeIsRegistered
 |
 | Parameters :
 |   extension  : file extension, including the dot, to search for
 |   filetype   : string to use as key for the file extension
 | Returns    : True if this application is registered as server
 |              for the 'open' action, false otherwise.
 | Visibility : exported from unit
 | Description:
 |   Checks if there is a registry entry for the passed extension,
 |   if it is associated with the expected file type and if this
 |   application is registered as server for the 'open' action.
 | Error Conditions: none
 | Created: 21.03.99 by P. Below
 +------------------------------------------------------------}
 (*
Function FiletypeIsRegistered( Const extension, filetype: String ):
Boolean;
  Var
    reg: TRegistry;
    keystring: String;
    regstrpos : integer;
  Begin
    Result := False;
    reg:= TRegistry.Create (KEY_READ);
    Try
      reg.Rootkey := HKEY_CLASSES_ROOT;
      if reg.OpenKey(extension, false) Then Begin
        { Extension is registered, check filetype }
        keystring := reg.ReadString('');
        reg.Closekey;
        if CompareText( keystring, filetype) = 0 Then Begin
          { Filetype is registered for this extension, check server. }
          keystring := Format( '%s\shell\open\command',[filetype] );
          if reg.OpenKey( keystring, false ) Then Begin
            { Command key exists, but is this app the server? }
            keystring := AnsiUpperCase( reg.ReadString(''));
            reg.CloseKey;
            // [MJ] server string may be enclosed in quotes
            regstrpos := Pos( AnsiUpperCase(ParamStr(0)), keystring );
            if (( regstrpos = 1 ) or ( regstrpos = 2 )) then
            begin
              { Yes, server matches! }
              Result := True;
            end; { If }
          end; { If }
        end; { If }
      end; { If }
    finally
      reg.free;
    end;
  end; { FiletypeIsRegistered }
*)

end { Unit FileAssoc }.

