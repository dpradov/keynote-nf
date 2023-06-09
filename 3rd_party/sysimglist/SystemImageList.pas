{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsSystemImageList v1.16                                                    }
{------------------------------------------------------------------------------}
{ A component to extend the TImageList so that it gives access to the system   }
{ image list.                                                                  }
{                                                                              }
{ Copyright 2000-2001, Brad Stowers.  All Rights Reserved.                     }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TDFSColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See SystemImageList.txt for notes, known issues, and revision history.       }
{------------------------------------------------------------------------------}
{ Date last modified:  June 28, 2001                                           }
{------------------------------------------------------------------------------}

{CE_Desc_Begin(SystemImageList.pas)}
{This unit provides the <%LINK TdfsSystemImageList%> component which extends \
the TImageList so that it gives access to the Win32 system image list.  The \
system image list is a list of images owned by the Win32 operating system \
that is made up of all the images the OS uses in things like Explorer.}
{CE_Desc_End}
unit SystemImageList;

{$IFNDEF DFS_WIN32}
  !! { ERROR!  Only available for Win32! }
{$ENDIF}

{CE_Desc_Begin(@LIST_OVERVIEW)}
{<%LINK TdfsSystemImageList%> is the only class provided with this component.

There are several unit level functions that are used by the component that \
I have provided in case you want do things at a lower level than using the \
component.}
{CE_Desc_End}

{CE_Desc_Begin(@HIERARCHY_OVERVIEW)}
{Their is only the <%LINK TdfsSystemImageList%> component in this package, and it \
descends from TImageList.}
{CE_Desc_End}

{CE_Desc_Begin(@UNIT_OVERVIEW)}
{The <%LINK TdfsSystemImageList%> component is wholly contained in the \
SystemImageList.pas unit.}
{CE_Desc_End}


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.CommCtrl,
   Winapi.ShlObj,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs;

const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsSystemImageList v1.16';

{$IFDEF DFS_COMPILER_2}
  // Delphi 2 and C++Builder 1 don't have these defined in Windows.pas
  // C1 does have them defined in a header file, but Pascal code can't get at
  // those.  So, C1 is going to complain about them being redefined.  Just
  // ignore those two warnings.
  FILE_ATTRIBUTE_COMPRESSED           = $00000800;
  FILE_ATTRIBUTE_OFFLINE              = $00001000;
{$ENDIF}

type
  TImageSize = (isLarge, isSmall);

  // Note that not all of these are available on every flavor of Win9x/NT4 SPx
  TShellItem = (siDesktop, siInternet, siPrograms, siControlPanel, siPrinters,
     siPersonalDocs, siFavorites, siStartup, siRecentDocs, siSendTo,
     siRecycleBin, siStartMenu, siDrives, siNetworkNeighborhood, siFonts,
     siTemplates, siInternetCache, siCookies, siHistory);

  { TSystemFileAttribute is used to provide a Delphi-ish interface for the
    various Win32 file attributes.}
  TSystemFileAttribute = (sfaReadOnly, sfaHidden, sfaSystem, sfaDirectory,
     sfaArchive, sfaNormal, sfaTemporary ,sfaCompressed, sfaOffline);
  { TSystemFileAttributes is a set of TSystemFileAttribute.  This allows you
    to provide more than a single attribute at one time. }
  TSystemFileAttributes = set of TSystemFileAttribute;

{CE_Desc_Begin(TdfsSystemImageList)}
{TdfsSystemImageList component which extends the TImageList component so that \
it gives access to the Win32 system image list.  The system image list is \
a list of images owned by the Win32 operating system that is made up of \
all the images the OS uses in things like Explorer.

It is derived from TImageList instead of TCustomImageList because \
components such TListView and TTreeView have properties of TImageList \
type.  If it were derived from TCustomImageList, it would not be \
compatible with those properties.  That would make it pretty useless; thanks \
Borland...err...Inprise.}
{CE_Desc_End}
  TdfsSystemImageList = class(TImageList)
  private
    FImageSize: TImageSize;

    procedure SetImageSize(Val: TImageSize);
    function GetHeight: integer;
    function GetWidth: integer;
    function GetShareImages: boolean;
    procedure SetShareImages(Val: boolean);
    function GetHandle: HImageList;
  protected
    function GetVersion: string;
    procedure SetVersion(const Val: string);
    procedure SetName(const NewName: TComponentName); override;
    procedure SetImageListHandle(Shared: boolean); virtual;
    procedure Loaded; override;
    // EXTREMELY IMPORTANT!!!!
    procedure WriteState(Writer: TWriter); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SaveToStream(Stream: TStream); virtual;

{CE_Desc_Begin(TdfsSystemImageList.GetImageIndex)}
{The <%BOLD%>GetImageIndex<%BOLD0%> method is used to retrive the index into \
the image list of given filename or directory.  The return value is the image \
index.

You must specify the full pathname if you want the system to determine the \
attributes of the file.  In this case, you can simply pass an empty set ( [] ) \
to the <%BOLD%>Attrs<%BOLD0%> parameter.

If you do not specify the full pathname, or the file simply does not exist, \
you must specify the attributes to be used in determining the image index.

The <%BOLD%>Attrs<%BOLD0%> parameter is a set of zero or more of the following \
values:
<%TABLE%><%BOLD%>sfaReadOnly<%BOLD0%>	The file or directory is read-only. \
Applications can read the file but cannot write to it or delete it. In the \
case of a directory, applications cannot delete it.
<%BOLD%>sfaHidden<%BOLD0%>	The file or directory is hidden. It is not \
included in an ordinary directory listing.
<%BOLD%>sfaSystem<%BOLD0%>	The file or directory is part of, or is used \
exclusively by, the operating system.
<%BOLD%>sfaDirectory<%BOLD0%>	The "file or directory" is a directory.
<%BOLD%>sfaArchive<%BOLD0%>	The file or directory is an archive file or \
directory. Applications use this flag to mark files for backup or removal.
<%BOLD%>sfaNormal<%BOLD0%>	The file or directory has no other attributes set. \
This attribute is valid only if used alone.
<%BOLD%>sfaTemporary<%BOLD0%>	The file is being used for temporary storage. \
File systems attempt to keep all of the data in memory for quicker access \
rather than flushing the data back to mass storage. A temporary file should be \
deleted by the application as soon as it is no longer needed.
<%BOLD%>sfaCompressed<%BOLD0%>	The file or directory is compressed. For a \
file, this means that all of the data in the file is compressed. For a \
directory, this means that compression is the default for newly created files \
and subdirectories.
<%BOLD%>sfaOffline<%BOLD0%>	The data of the file is not immediately available. \
Indicates that the file data has been physically moved to offline storage.
<%ENDTABLE%>

<%SEEALSO GetFileInformation%>

<%EXAMPLE%>
<%TEXT%>
This example retrieves the image index for a file or directory that does \
exist.  For example, if you were populating a listview with the files in a \
directory on the user's system, this would be appropriate.
<%CODE%>
  function AddListItem(const ARealFilename: string);
  var
    Item: TListItem;
    s: string;
  begin
    SomeListView.Items.BeginUpdate;
    try
      Item := SomeListView.Items.Add;
      // ARealFilename must have full path information
      Item.Caption := ExtractFileName(ARealFileName);
      // The filename is real, so let the system figure out the attributes.
      Item.ImageIndex := SomeSystemImageList.GetImageIndex(ARealFilename, []);
    finally
      SomeListView.Items.EndUpdate;
    end;
  end;

<%TEXT%>
This example retrieves the image index for a file type based on it's \
extension. This is appropriate for if you had a listview that was to be filled \
with filenames that did not exist; a zip file viewer or the files on an FTP \
server, for example.
<%CODE%>
  function AddListItem(const AFakeFilename: string, IsADirectory: boolean);
  var
    Item: TListItem;
    s: string;
    Attrs: TSystemFileAttributes;
  begin
    SomeListView.Items.BeginUpdate;
    try
      Item := SomeListView.Items.Add;
      // AFakeFilename does not exist, we must supply the attributes.
      if IsADirectory then
        Attrs := [sfaDirectory] // tell it we want the folder image index.
      else
        Attrs := [sfaNormal]; // figure it out based on file extension.
      Item.ImageIndex := SomeSystemImageList.GetImageIndex(AFakeFilename, Attrs);
    finally
      SomeListView.Items.EndUpdate;
    end;
  end;
}
{CE_Desc_End}
    {$IFDEF DFS_COMPILER_4_UP}
    function GetImageIndex(const APath: string; Selected, Open: boolean;
       Attrs: TSystemFileAttributes): integer; overload;
    function GetImageIndex(const APidl: PItemIDList; Selected,
       Open: boolean ): integer; overload;
    function GetImageIndex(SpecialItem: TShellItem; Selected,
       Open: boolean): integer; overload;
    {$ELSE}
    function GetImageIndex(const APath: string; Selected, Open: boolean;
       Attrs: TSystemFileAttributes): integer;
    function GetImageIndexPIDL(const APidl: PItemIDList; Selected,
       Open: boolean): integer;
    function GetImageIndexSpecial(SpecialItem: TShellItem; Selected,
       Open: boolean): integer;
    {$ENDIF}

{CE_Desc_Begin(TdfsSystemImageList.GetFileInformation)}
{The <%BOLD%>GetFileInformation<%BOLD0%> method is identical to the \
<%BOLD%><%LINK GetImageIndex%><%BOLD0%> method with the exception that it also \
retrieves the system description text for the file type.  This text is what \
you see in the <%BOLD%>Type<%BOLD0%> column of Explorer.

Simply pass a string variable in the <%BOLD%>Descr<%BOLD0%> parameter and it \
will be assigned the system description text.

All other aspects of this method are identical to <%BOLD%>GetImageIndex<%BOLD0%>.

<%SEEALSO GetImageIndex%>
}
{CE_Desc_End}
    {$IFDEF DFS_COMPILER_4_UP}
    function GetFileInformation(const APath: string; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer; overload;
    function GetFileInformation(const APidl: PItemIDList; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer; overload;
    function GetFileInformation(SpecialItem: TShellItem; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer; overload;
    {$ELSE}
    function GetFileInformation(const APath: string; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer;
    function GetFileInformationPIDL(const APidl: PItemIDList; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer;
    function GetFileInformationSpecial(SpecialItem: TShellItem; Selected, Open: boolean;
       Attrs: TSystemFileAttributes; var Descr: string): integer;
    {$ENDIF}



{CE_Desc_Begin(TdfsSystemImageList.Handle)}
{The <%BOLD%>Handle<%BOLD0%> property is the Win32 handle of the image list in \
use.  If the <%BOLD%><%LINK ShareImages%><%BOLD0%> property is TRUE, the \
handle is the <%BOLD%>REAL<%BOLD0%> system image list.  That means any changes \
to it will affect the <%ITALIC%><%BOLD%>entire system<%BOLD0%><%ITALIC0%>.  If \
<%BOLD%>ShareImages<%BOLD0%> is FALSE, the component has made a copy of the \
system image list and changes will affect only the component.

<%SEEALSO ShareImages%>
}
{CE_Desc_End}
    property Handle: HImageList { read only! }
       read GetHandle;
  published
{CE_Desc_Begin(TdfsSystemImageList.Version)}
{Displays the version number of the component.  This allows you to easily \
compare the version installed with the version you *think* you are using.

The property editor for this property also displays the address to my web site \
where you can find the most current version of this component, along with many \
other freeware components written by myself and others.}
{CE_Desc_End}
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;
{CE_Desc_Begin(TdfsSystemImageList.ImageSize)}
{<%BOLD%>ImageSize<%BOLD0%> indicates what size image is to be provided by the \
component.

Possible values are:
<%TABLE%><%BOLD%>isLarge<%BOLD0%>	The large image list is most commonly used \
for list view controls with ViewStyle set to vsIcon.
<%BOLD%>isSmall<%BOLD0%>	The small image list is most commonly used for list \
view controls with ViewStyle set to a value other than vsIcon, and also tree \
view controls.
<%ENDTABLE%>
If you need to determine the exact size of either large or small images in the \
list, use the <%BOLD%><%LINK Height%><%BOLD0%> and <%BOLD%><%LINK Width%> \
<%BOLD0%> properties.

<%SEEALSO Height, Width%>
}
{CE_Desc_End}
    property ImageSize: TImageSize
       read FImageSize
       write SetImageSize
       default isLarge;
{CE_Desc_Begin(TdfsSystemImageList.Height)}
{The <%BOLD%>Height<%BOLD0%> property is used to report the height of the \
images contained in the list.  You can not directly change this value since \
it is dictated by the system.  To change the size of images in the list, use \
the <%BOLD%><%LINK ImageSize%><%BOLD0%> property.

<%SEEALSO Width, ImageSize%>
}
{CE_Desc_End}
    property Height: integer
       read GetHeight { read only! }
       stored FALSE;
{CE_Desc_Begin(TdfsSystemImageList.Width)}
{The <%BOLD%>Width<%BOLD0%> property is used to report the width of the \
images contained in the list.  You can not directly change this value since \
it is dictated by the system.  To change the size of images in the list, use \
the <%BOLD%><%LINK ImageSize%><%BOLD0%> property.

<%SEEALSO Height, ImageSize%>
}
{CE_Desc_End}
    property Width: integer
       read GetWidth { read only! }
       stored FALSE;
{CE_Desc_Begin(TdfsSystemImageList.ShareImages)}
{The <%BOLD%>ShareImages<%BOLD0%> property is used to indicate whether the \
component should use the <%BOLD%>real<%BOLD0%> system image list or make a \
copy of it for the components used.

Using the real system image list means any changes made to it will affect the \
<%ITALIC%><%BOLD%>entire system<%BOLD0%><%ITALIC0%> until it is restarted.

<%NOTE%>For safety reasons, you should always set <%BOLD%>ShareImages<%BOLD0%> \
to TRUE when it is possible.  The drawback to doing this is that it can be \
quite time consuming to make the initial copy of the system's image list (it \
can be very large, upwards of several megabytes).

<%SEEALSO Handle%>
}
{CE_Desc_End}
    property ShareImages: boolean
       read GetShareImages
       write SetShareImages
       nodefault;
  end;


{CE_Desc_Begin(GeTdfsSystemImageList)}
{<%BOLD%>GeTdfsSystemImageList<%BOLD0%> is a function that is can be used to get \
the handle of the system's large and small image list.  This list is \
<%BOLD%>owned by the system<%BOLD0%>.  It is <%BOLD%>NOT<%BOLD0%> a copy.

The <%BOLD%>Large<%BOLD0%> parameter indicates whether to return the image \
list handle that contains large or small icons.

You should <%ITALIC%><%BOLD%>never<%BOLD0%><%ITALIC0%> free this handle when \
you are done with it.  Doing so will leave the entire OS without an image \
list.  Explorer looks damn funny that way.}
{CE_Desc_End}
function GeTdfsSystemImageList(Large: boolean): HImageList;

{CE_Desc_Begin(GetIconIndex)}
{Retrieves the index into the system image list of a file or directory item. \
If the item does not exist, the <%BOLD%>Attrs<%BOLD0%> parameter is used to \
describe its attributes. If the file does exist, <%BOLD%>Attrs<%BOLD0%> is \
ignored.

The <%BOLD%>Attrs<%BOLD0%> parameter accepts any of the \
<%BOLD%>FILE_ATTRIBUTE_xxx<%BOLD0%> constants ORed together bitwise, or 0 if \
the system should determine the attributes itself.  You can find a list of \
these constants in the Win32.hlp file under the \
<%BOLD%>GetFileAttributes<%BOLD0%> topic.

<%SEEALSO GetFileInfo%>
<%EXAMPLE%>
<%TEXT%>
If you wanted to get the index of a file, say c:\windows\notepad.exe, that did \
exist, you would call it like this:
<%CODE%>
  Index := GetIconIndex('c:\windows\notepad.exe', 0);

<%TEXT%>
If you wanted to get the index for a file that did not exist, you would need \
to specify what file attributes should be used in determining the image index.
<%CODE%>
  Index := GetIconIndex('c:\bogus\dir\badfile.html', FILE_ATTRIBUTE_NORMAL);
}
{CE_Desc_End}
{$IFDEF DFS_COMPILER_4_UP}
function GetIconIndex(const APath: string; Selected, Open: boolean;
  Attrs: DWORD; AlwaysUseAttrs: boolean): integer; overload;
function GetIconIndex(const APidl: PItemIDList; Selected,
  Open: boolean): integer; overload;
function GetIconIndex(SpecialItem: TShellItem; Selected,
  Open: boolean): integer; overload;
{$ELSE}
function GetIconIndex(const APath: string; Selected, Open: boolean;
  Attrs: DWORD; AlwaysUseAttrs: boolean): integer;
function GetIconIndexPIDL(const APidl: PItemIDList; Selected,
  Open: boolean): integer;
function GetIconIndexSpecial(SpecialItem: TShellItem; Selected,
  Open: boolean): integer;
{$ENDIF}

{CE_Desc_Begin(GetFileInfo)}
{This function is exactly the same as <%BOLD%><%LINK GetIconIndex%><%BOLD0%> \
except that it takes an extra variable parameter that is assigned the system \
description for the file.  The contents of this string parameter does not \
matter when the function is called, it is used strictly for output.

<%SEEALSO GetIconIndex%>
}
{CE_Desc_End}
{$IFDEF DFS_COMPILER_4_UP}
function GetFileInfo(const APath: string; Selected, Open: boolean; Attrs: DWORD;
   AlwaysUseAttrs: boolean; var Descr: string): integer; overload;
function GetFileInfo(const APidl: PItemIDList; Selected, Open: boolean; Attrs: DWORD;
   AlwaysUseAttrs: boolean; var Descr: string): integer; overload;
function GetFileInfo(SpecialItem: TShellItem; Selected, Open: boolean; Attrs: DWORD;
   AlwaysUseAttrs: boolean; var Descr: string): integer; overload;
{$ELSE}
function GetFileInfo(const APath: string; Selected, Open: boolean; Attrs: DWORD;
   AlwaysUseAttrs: boolean; var Descr: string): integer;
function GetFileInfoPIDL(const APidl: PItemIDList; Selected, Open: boolean;
   Attrs: DWORD; AlwaysUseAttrs: boolean; var Descr: string): integer;
function GetFileInfoSpecial(SpecialItem: TShellItem; Selected, Open: boolean;
   Attrs: DWORD; AlwaysUseAttrs: boolean; var Descr: string): integer;
{$ENDIF}


implementation


uses
   Winapi.ShellAPI,
   {$IFDEF DFS_COMPILER_3_UP}
   Winapi.ActiveX,
   {$ELSE}
   OLE2,
   {$ENDIF}
   Vcl.FileCtrl;


// I'll get to it in a minute, now shut up compiler.
function SFA2API(Attrs: TSystemFileAttributes): DWORD; forward;
function GetValidHandle(ImgList: TdfsSystemImageList): HWND; forward;


constructor TdfsSystemImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageSize := isLarge;
  ShareImages := TRUE;
end;

procedure TdfsSystemImageList.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  // I really couldn't give a flying doughnut about SetName, I just needed
  // something I could override that would happen when the component was being
  // created dynamically so I could create the handle.  In a windowed component,
  // I could override CreateWnd which would make sense, but this little freak
  // doesn't make it's CreateHandle virtual.  Matter of fact, it makes damn
  // little of itself virtual, making my life a lot harder than it should be.
  // Can you tell I'm really ticked off at the moment?

  // If it isn't loading, create the handle.  If it is, wait until all
  // properties have been loaded before doing it so we don't have to recreate
  // it every time one changes.
  if not (csLoading in ComponentState) then
    SetImageListHandle(ShareImages);
end;

procedure TdfsSystemImageList.Loaded;
begin
  inherited Loaded;
  SetImageListHandle(ShareImages);
end;

procedure TdfsSystemImageList.WriteState(Writer: TWriter);
var
  TempHandle: HImageList;
begin
  // We don't want the system image list being streamed out to disk.  It is
  // like a couple of meg in size.
  TempHandle := Handle;
  inherited Handle := 0;
  inherited WriteState(Writer);
  inherited Handle := TempHandle;
end;

procedure TdfsSystemImageList.SetImageListHandle(Shared: boolean);
var
  TempHandle: HImageList;
  TempList: TImageList;
  OldCursor: TCursor;
begin
  { if we have a handle already, this will get rid of it according to
    ShareImages property }
  inherited Handle := 0;

  TempHandle := GeTdfsSystemImageList(FImageSize = isLarge);
  if Shared then
    // give them the real thing
    inherited Handle := TempHandle
  else begin
    // make a copy of it.  This can be quite slow.
    TempList := TImageList.Create(Self);
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      TempList.ShareImages := TRUE;
      TempList.Handle := TempHandle;
      Assign(TempList);
    finally
      Screen.Cursor := OldCursor;
      TempList.Free;
    end;
  end;
end;

procedure TdfsSystemImageList.SetImageSize(Val: TImageSize);
begin
  if FImageSize <> Val then
  begin
    FImageSize := Val;
    if HandleAllocated then
      SetImageListHandle(ShareImages);
  end;
end;

function TdfsSystemImageList.GetHeight: integer;
begin
  Result := inherited Height;
end;

function TdfsSystemImageList.GetWidth: integer;
begin
  Result := inherited Width;
end;

function TdfsSystemImageList.GetShareImages: boolean;
begin
  Result := inherited ShareImages;
end;

procedure TdfsSystemImageList.SetShareImages(Val: boolean);
begin
  if HandleAllocated then
    SetImageListHandle(Val);
  inherited ShareImages := Val;
end;

function TdfsSystemImageList.GetHandle: HImageList;
begin
  if not HandleAllocated then
    SetImageListHandle(ShareImages);
  Result := inherited Handle;
end;

// Only need Attrs if APath doesn't exist, otherwise just pass []
function TdfsSystemImageList.GetImageIndex(const APath: string; Selected,
   Open: boolean; Attrs: TSystemFileAttributes): integer;
begin
  Result := GetIconIndex(APath, Selected, Open, SFA2API(Attrs), Attrs <> []);
end;

{$IFDEF DFS_COMPILER_4_UP}
function TdfsSystemImageList.GetImageIndex(const APidl: PItemIDList;
   Selected, Open: boolean): integer;
begin
  Result := GetIconIndex(APidl, Selected, Open);
end;
{$ELSE}
function TdfsSystemImageList.GetImageIndexPIDL(const APidl: PItemIDList;
   Selected, Open: boolean): integer;
begin
  Result := GetIconIndexPIDL(APidl, Selected, Open);
end;
{$ENDIF}

{$IFDEF DFS_COMPILER_4_UP}
function TdfsSystemImageList.GetImageIndex(SpecialItem: TShellItem;
   Selected, Open: boolean): integer;
begin
  Result := GetIconIndex(SpecialItem, Selected, Open);
end;
{$ELSE}
function TdfsSystemImageList.GetImageIndexSpecial(SpecialItem: TShellItem;
   Selected, Open: boolean): integer;
begin
  Result := GetIconIndexSpecial(SpecialItem, Selected, Open);
end;
{$ENDIF}


// Only need Attrs if APath doesn't exist, otherwise just pass []
function TdfsSystemImageList.GetFileInformation(const APath: string;
   Selected, Open: boolean; Attrs: TSystemFileAttributes;
   var Descr: string): integer;
begin
  Result := GetFileInfo(APath, Selected, Open, SFA2API(Attrs), Attrs <> [], Descr);
end;

{$IFDEF DFS_COMPILER_4_UP}
function TdfsSystemImageList.GetFileInformation(const APidl: PItemIDList;
   Selected, Open: boolean; Attrs: TSystemFileAttributes;
   var Descr: string): integer;
begin
  Result := GetFileInfo(APidl, Selected, Open, SFA2API(Attrs), Attrs <> [], Descr);
end;
{$ELSE}
function TdfsSystemImageList.GetFileInformationPIDL(const APidl: PItemIDList;
   Selected, Open: boolean; Attrs: TSystemFileAttributes;
   var Descr: string): integer;
begin
  Result := GetFileInfoPIDL(APidl, Selected, Open, SFA2API(Attrs), Attrs <> [],
    Descr);
end;
{$ENDIF}

{$IFDEF DFS_COMPILER_4_UP}
function TdfsSystemImageList.GetFileInformation(SpecialItem: TShellItem;
   Selected, Open: boolean; Attrs: TSystemFileAttributes;
   var Descr: string): integer;
begin
  Result := GetFileInfo(SpecialItem, Selected, Open, SFA2API(Attrs), Attrs <> [],
    Descr);
end;
{$ELSE}
function TdfsSystemImageList.GetFileInformationSpecial(SpecialItem: TShellItem;
   Selected, Open: boolean; Attrs: TSystemFileAttributes;
   var Descr: string): integer;
begin
  Result := GetFileInfoSpecial(SpecialItem, Selected, Open, SFA2API(Attrs),
    Attrs <> [], Descr);
end;
{$ENDIF}


function TdfsSystemImageList.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsSystemImageList.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;

// Needed to support the "Save to bitmap" component editor.
procedure TdfsSystemImageList.SaveToStream(Stream: TStream);
var
  DIB1, DIB2: TBitmap;
  DC: HDC;
  S: TMemoryStream;

  procedure WriteDIB(BM: HBitmap);
    { The ImageList leaves its bitmap handle selected into a DC somewhere,
      so we can't select it into our own DC to copy from it.  The only safe
      operation is GetDIB (GetDIBits), which extracts the pixel bits without
      selecting the BM into a DC.  This code builds our own bitmap from
      those bits, then crops it to the minimum size before writing it out.}
  var
    BitsSize: DWORD;
    Header, Bits: PChar;
    DIBBits: Pointer;
    R: TRect;
    HeaderSize: DWORD;
    GlyphsPerRow, Rows: Integer;
  begin
    if BM = 0 then Exit;
    GetDIBSizes(BM, HeaderSize, BitsSize);
    GetMem(Header, HeaderSize + BitsSize);
    try
      Bits := Header + HeaderSize;
      GetDIB(BM, 0, Header^, Bits^);
      DIB1.Handle := CreateDIBSection(DC, PBitmapInfo(Header)^, DIB_RGB_COLORS,
         {$IFDEF DFS_COMPILER_2}
         DIBBits, NIL, 0);
         {$ELSE}
         DIBBits, 0, 0);
         {$ENDIF}
      System.Move(Bits^, DIBBits^, BitsSize);
      with PBitmapInfo(Header)^.bmiHeader do
      begin
        GlyphsPerRow := biWidth div Width;
        if GlyphsPerRow = 0 then Inc(GlyphsPerRow);
        if GlyphsPerRow > Count then GlyphsPerRow := Count;
        biWidth := GlyphsPerRow * Width;
        Rows := Count div GlyphsPerRow;
        if Count > Rows * GlyphsPerRow then Inc(Rows);
        biHeight := Rows * Height;
        R := Rect(0, 0, biWidth, biHeight);
      end;
      DIB2.Handle := CreateDIBSection(DC, PBitmapInfo(Header)^, DIB_RGB_COLORS,
         {$IFDEF DFS_COMPILER_2}
         DIBBits, NIL, 0);
         {$ELSE}
         DIBBits, 0, 0);
         {$ENDIF}
      DIB2.Canvas.CopyRect(R, DIB1.Canvas, R);
      DIB2.SaveToStream(S);
    finally
      FreeMem(Header);
    end;
  end;

begin
  DIB1 := nil;
  DIB2 := nil;
  DC := 0;
  S := TMemoryStream.Create;
  try
    DIB1 := TBitmap.Create;
    DIB2 := TBitmap.Create;
    DC := GetDC(0);
    WriteDIB(GetImageBitmap);
    Stream.WriteBuffer(S.Memory^, S.Size);
  finally
    ReleaseDC(0, DC);
    DIB1.Free;
    DIB2.Free;
    S.Free;
  end;
end;

{------------------------------------------------------------------------------}
{ Utility functions                                                            }
{------------------------------------------------------------------------------}

function GetValidHandle(ImgList: TdfsSystemImageList): HWND;
begin
  if assigned(ImgList) and assigned(ImgList.Owner) and
    (ImgList.Owner is TWinControl) and TWinControl(ImgList.Owner).HandleAllocated then
    Result := TWinControl(ImgList.Owner).Handle
  else if assigned(ImgList) and (ImgList.Owner is TControl) and
    (GetParentForm(TControl(ImgList.Owner)) <> NIL) and (GetParentForm(
    TControl(ImgList.Owner)).HandleAllocated) then
    Result := GetParentForm(TControl(ImgList.Owner)).Handle
  else if assigned(Application.MainForm) and
     Application.MainForm.HandleAllocated then
    Result := Application.MainForm.Handle
  else
    Result := 0;
end;


function SFA2API(Attrs: TSystemFileAttributes): DWORD;
const
  API_VALUES: array[TSystemFileAttribute] of DWORD = (
     FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_SYSTEM,
     FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_NORMAL,
     FILE_ATTRIBUTE_TEMPORARY, FILE_ATTRIBUTE_COMPRESSED, FILE_ATTRIBUTE_OFFLINE);
var
  x: TSystemFileAttribute;
begin
  Result := 0;
  for x := Low(x) to High(x) do
    if x in Attrs then
      Result := Result or API_VALUES[x];
end;

function SI2API(Item: TShellItem): integer;
const
  {$IFNDEF DFS_COMPILER_4_UP}
  CSIDL_INTERNET        = $0001;
  CSIDL_INTERNET_CACHE  = $0020;
  CSIDL_COOKIES         = $0021;
  CSIDL_HISTORY         = $0022;
  {$ENDIF}
  API_VALUES: array[TShellItem] of integer = (
     CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
     CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
     CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU, CSIDL_DRIVES,
     CSIDL_NETWORK, CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_INTERNET_CACHE,
     CSIDL_COOKIES, CSIDL_HISTORY);

begin
  Result := API_VALUES[Item];
end;

function GeTdfsSystemImageList(Large: boolean): HImageList;
var
  SFI: TSHFileInfo;
begin
  // SHGetFileInfo puts the requested information in the SFI variable, but it
  // also can return the handle of the system image list.  We just pass an
  // empty file because we aren't interested in it, only the returned handle.
  if Large then
    Result := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_LARGEICON)
  else
    Result := SHGetFileInfo('', 0, SFI, SizeOf(SFI),
                            SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
end;

const
  SELECTED_FLAG: array[boolean] of DWORD = (0, SHGFI_SELECTED);
  OPEN_FLAG: array[boolean] of DWORD = (0, SHGFI_OPENICON);

function GetIconIndex(const APath: string; Selected, Open: boolean;
   Attrs: DWORD; AlwaysUseAttrs: boolean): integer;
var
  SFI: TSHFileInfo;
begin
  if (not AlwaysUseAttrs) and (FileExists(APath) or System.SysUtils.DirectoryExists(APath)) then
    // If the file or directory exists, just let Windows figure out it's attrs.
    SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
       SHGFI_SYSICONINDEX or OPEN_FLAG[Open] or SELECTED_FLAG[Selected])
  else
    // File doesn't exist, so Windows doesn't know what to do with it.  We have
    // to tell it by passing the attributes we want, and specifying the
    // SHGFI_USEFILEATTRIBUTES flag so that the function knows to use them.
    SHGetFileInfo(PChar(APath), Attrs, SFI, SizeOf(TSHFileInfo),
       SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or OPEN_FLAG[Open] or
       SELECTED_FLAG[Selected]);
  Result := SFI.iIcon;
end;

{$IFDEF DFS_COMPILER_4_UP}
function GetIconIndex(const APidl: PItemIDList; Selected, Open: boolean
{$ELSE}
function GetIconIndexPIDL(const APidl: PItemIDList; Selected, Open: boolean
{$ENDIF}
   ): integer;
var
  SFI: TSHFileInfo;
begin
  SHGetFileInfo(PChar(APidl), 0, SFI, SizeOf(TSHFileInfo),
     SHGFI_PIDL or SHGFI_SYSICONINDEX or OPEN_FLAG[Open] or
     SELECTED_FLAG[Selected]);
  Result := SFI.iIcon;
end;

{$IFDEF DFS_COMPILER_4_UP}
function GetIconIndex(SpecialItem: TShellItem; Selected, Open: boolean
{$ELSE}
function GetIconIndexSpecial(SpecialItem: TShellItem; Selected, Open: boolean
{$ENDIF}
  ): integer;
var
  pidl: PItemIDList;
  ShellMalloc: IMalloc;
begin
  SHGetMalloc(ShellMalloc);
  SHGetSpecialFolderLocation(GetValidHandle(NIL), SI2API(SpecialItem),
     pidl);
  try
    {$IFDEF DFS_COMPILER_4_UP}
    Result := GetIconIndex(pidl, Selected, Open);
    {$ELSE}
    Result := GetIconIndexPIDL(pidl, Selected, Open);
    {$ENDIF}
  finally
    ShellMalloc.Free(pidl);
    {$IFNDEF DFS_NO_COM_CLEANUP} // Delphi 2 won't free automatically, 3 and up will
    ShellMalloc.Release;
    {$ENDIF}
  end;
end;


function GetFileInfo(const APath: string; Selected, Open: boolean; Attrs: DWORD;
   AlwaysUseAttrs: boolean; var Descr: string): integer;
const
  SELECTED_FLAG: array[boolean] of DWORD = (0, SHGFI_SELECTED);
var
  SFI: TSHFileInfo;
begin
  if FileExists(APath) or System.SysUtils.DirectoryExists(APath) then
    SHGetFileInfo(PChar(APath), Attrs, SFI, SizeOf(TSHFileInfo),
       SHGFI_TYPENAME or SHGFI_SYSICONINDEX or OPEN_FLAG[Open] or
       SELECTED_FLAG[Selected])
  else
    SHGetFileInfo(PChar(APath), Attrs, SFI, SizeOf(TSHFileInfo),
       SHGFI_TYPENAME or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or
       OPEN_FLAG[Open] or SELECTED_FLAG[Selected]);
  Descr := SFI.szTypeName;
  Result := SFI.iIcon;
end;

{$IFDEF DFS_COMPILER_4_UP}
function GetFileInfo(const APidl: PItemIDList; Selected, Open: boolean;
{$ELSE}
function GetFileInfoPIDL(const APidl: PItemIDList; Selected, Open: boolean;
{$ENDIF}
   Attrs: DWORD; AlwaysUseAttrs: boolean; var Descr: string): integer;
var
  SFI: TSHFileInfo;
begin
  SHGetFileInfo(PChar(APidl), 0, SFI, SizeOf(TSHFileInfo),
     SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_SYSICONINDEX or OPEN_FLAG[Open] or
     SELECTED_FLAG[Selected]);
  Descr := SFI.szTypeName;
  Result := SFI.iIcon;
end;

{$IFDEF DFS_COMPILER_4_UP}
function GetFileInfo(SpecialItem: TShellItem; Selected, Open: boolean;
{$ELSE}
function GetFileInfoSpecial(SpecialItem: TShellItem; Selected, Open: boolean;
{$ENDIF}
   Attrs: DWORD; AlwaysUseAttrs: boolean; var Descr: string): integer;
var
  pidl: PItemIDList;
  ShellMalloc: IMalloc;
begin
  SHGetMalloc(ShellMalloc);
  SHGetSpecialFolderLocation(GetValidHandle(NIL), SI2API(SpecialItem),
     pidl);
  try
    {$IFDEF DFS_COMPILER_4_UP}
    Result := GetFileInfo(pidl, Selected, Open, Attrs, AlwaysUseAttrs, Descr);
    {$ELSE}
    Result := GetFileInfoPIDL(pidl, Selected, Open, Attrs, AlwaysUseAttrs, Descr);
    {$ENDIF}
  finally
    ShellMalloc.Free(pidl);
    {$IFNDEF DFS_NO_COM_CLEANUP} // Delphi 2 won't free automatically, 3 and up will
    ShellMalloc.Release;
    {$ENDIF}
  end;
end;


end.
