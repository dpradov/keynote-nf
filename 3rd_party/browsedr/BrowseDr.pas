{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsBrowseDirectoryDlg v2.60                                                 }
{------------------------------------------------------------------------------}
{ A component to encapsulate the Win32 style directory selection dialog        }
{ SHBrowseForFolder().                                                         }
{                                                                              }
{ Copyright 1999, Brad Stowers.  All Rights Reserved.                          }
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
{ example, if you create a descendant of TdfsColorButton, you must include in  }
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
{ receive and address all problems that are reported to me, you must           }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See BrowseDr.txt for notes, known issues, and revision history.              }
{------------------------------------------------------------------------------}
{ Date last modified:  April 14, 2000                                          }
{------------------------------------------------------------------------------}


{: This unit provides a component that displays a standard Windows 95/NT 4...
   dialog containing the user's system in a heirarchial manner and allows a...
   selection to be made.  It is a wrapper for the SHBrowseForFolder() API,...
   which is quite messy to use directly.  Also provided is an editor which...
   allows you to display the dialog at design time with the selected options.

   Note:
   This component Requires Delphi 3 or Delphi v2.01's ShlObj unit.  If you...
   have Delphi 2.00, you can get the equivalent using Pat Ritchey's ShellObj...
   unit.  It is freely available on his web site at...
   http://ourworld.compuserve.com/homepages/PRitchey/.  Both Borland's ShlObj...
   unit and Pat's ShellObj unit contain errors that should be fixed.  I have...
   included instructions on how to do this.  They are in the included...
   ShellFix.txt file.  Delphi 3's ShlObj unit does not have any errors that I...
   am currently aware of.
}


unit BrowseDr;

{$IFNDEF DFS_WIN32}
  ERROR!  Only available for Win32!
{$ENDIF}

interface

uses
   Winapi.Windows,
   Vcl.Dialogs,
   {$IFDEF DFS_COMPILER_3_UP}
   Winapi.ActiveX,
   {$ELSE}
   OLE2,
   {$ENDIF}
   {$IFDEF DFS_USEDEFSHLOBJ}
   ShlObj { Delphi 3 fixes all of 2.01's bugs! },
   {$ELSE}
  // If you get a compiler error here, read the included SHELLFIX.TXT file for
  // instructions on creating MyShlObj.pas.
   MyShlObj,
   {$ENDIF}
   System.Classes,
   Vcl.Controls;

const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsBrowseDirectoryDlg v2.60';

  {: This is a newly documented folder identifier that is not in the Delphi...
     units yet.  You can use it with any of the Win32 Shell API functions...
      that wants a CSIDL_* identifier such as SHGetSpecialFolderLocation. }

  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM CSIDL_INTERNET}
  {$ENDIF}
  CSIDL_INTERNET         = $0001;
  {$IFDEF DFS_COMPILER_2}
  { IDs that exist in Delphi/C++B 3 ShlObj.pas unit, but not Delphi 2. }
  CSIDL_COMMON_STARTMENU              = $0016;
  CSIDL_COMMON_PROGRAMS               = $0017;
  CSIDL_COMMON_STARTUP                = $0018;
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;
  CSIDL_APPDATA                       = $001a;
  CSIDL_PRINTHOOD                     = $001b;
  {$ENDIF}

  {: This folder identifer is undocumented, but should work for a long time...
     since the highest ID is currently around 30 or so.  It is used to open...
     the tree already expanded with the desktop as the root item. }
  CSIDL_DESKTOPEXPANDED  = $FEFE;
  {$IFDEF DFS_COMPILER_2}
  {: This constant was missing from the Delphi 2 units, but was added to...
     Delphi 3.  It causes files to be included in the tree as well as folders. }
  BIF_BROWSEINCLUDEFILES = $4000;
  {$ENDIF}

  {$IFNDEF DFS_COMPILER_4_UP}
  {: These constants are new to v4.71 of SHELL32.DLL.  Delphi 4 defines them...
     but the are missing in all previous versions. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM BIF_EDITBOX}
  {$ENDIF}
  BIF_EDITBOX            = $0010;
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM BIF_VALIDATE}
  {$ENDIF}
  BIF_VALIDATE           = $0020;  { insist on valid result (or CANCEL) }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM BFFM_VALIDATEFAILED}
  {$ENDIF}
  BFFM_VALIDATEFAILED    = 3;      { lParam:szPath ret:1(cont),0(EndDialog) }
  {$ENDIF}
  {$IFNDEF DFS_COMPILER_6_UP}
  {$IFDEF DFS_CPPB_3_UP} {EXTERNALSYM BIF_BROWSEINCLUDEURLS} {$ENDIF}
  BIF_BROWSEINCLUDEURLS  = $0080;
  {$IFDEF DFS_CPPB_3_UP} {$EXTERNALSYM BIF_NEWDIALOGSTYLE} {$ENDIF}
  BIF_NEWDIALOGSTYLE = $0040;
  {$IFDEF DFS_CPPB_3_UP} {$EXTERNALSYM BIF_SHAREABLE} {$ENDIF}
  BIF_SHAREABLE = $8000;
  {$IFDEF DFS_CPPB_3_UP} {$EXTERNALSYM BIF_USENEWUI} {$ENDIF}
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
  {$ENDIF}

type
  {: This enumerated type is the equivalent of the CSIDL_* constants in the...
     Win32 API. They are used to specify the root of the heirarchy tree.

    idDesktop: Windows desktop -- virtual folder at the root of the name space.
    idInternet: Internet Explorer -- virtual folder of the Internet Explorer.
    idPrograms: File system directory that contains the user's program groups...
       (which are also file system directories).
    idControlPanel: Control Panel -- virtual folder containing icons for the...
       control panel applications.
    idPrinters: Printers folder -- virtual folder containing installed printers.
    idPersonal: File system directory that serves as a common respository for...
       documents.
    idFavorites: Favorites folder -- virtual folder containing the user's...'
       Internet Explorer bookmark items and subfolders.
    idStartup: File system directory that corresponds to the user's Startup...
       program group.
    idRecent: File system directory that contains the user's most recently...
       used documents.
    idSendTo: File system directory that contains Send To menu items.
    idRecycleBin: Recycle bin -- file system directory containing file...
       objects in the user's recycle bin. The location of this directory is...
       not in the registry; it is marked with the hidden and system...
       attributes to prevent the user from moving or deleting it.
    idStartMenu: File system directory containing Start menu items.
    idDesktopDirectory: File system directory used to physically store file...
       objects on the desktop (not to be confused with the desktop folder itself).
    idDrives: My Computer -- virtual folder containing everything on the...
       local computer: storage devices, printers, and Control Panel. The...
       folder may also contain mapped network drives.
    idNetwork: Network Neighborhood -- virtual folder representing the top...
       level of the network hierarchy.
    idNetHood: File system directory containing objects that appear in the...
       network neighborhood.
    idFonts: Virtual folder containing fonts.
    idTemplates: File system directory that serves as a common repository for...
       document templates.
    idCommonStartMenu: File system directory that contains the programs and...
       folders that appear on the Start menu for all users on Windows NT.
    idCommonPrograms: File system directory that contains the directories for...
       the common program groups that appear on the Start menu for all users...
       on Windows NT.
    idCommonStartup: File system directory that contains the programs that...
       appear in the Startup folder for all users. The system starts these...
       programs whenever any user logs on to Windows NT.
    idCommonDesktopDirectory: File system directory that contains files and...
       folders that appear on the desktop for all users on Windows NT.
    idAppData: File system directory that contains data common to all...
       applications.
    idPrintHood: File system directory containing object that appear in the...
       printers folder.
    idDesktopExpanded: Same as idDesktop except that the root item is already...
       expanded when the dialog is initally displayed.

    NOTE: idCommonStartMenu, idCommonPrograms, idCommonStartup, and...
       idCommonDesktopDirectory only have effect when the dialog is being...
       displayed on an NT system.  On Windows 95, these values will be...
       mapped to thier "non-common" equivalents, i.e. idCommonPrograms will...
       become idPrograms.
  }

  TRootID = (
    idDesktop, idInternet, idPrograms, idControlPanel, idPrinters, idPersonal,
    idFavorites, idStartup, idRecent, idSendTo, idRecycleBin, idStartMenu,
    idDesktopDirectory, idDrives, idNetwork, idNetHood, idFonts, idTemplates,
    idCommonStartMenu, idCommonPrograms, idCommonStartup,
    idCommonDesktopDirectory, idAppData, idPrintHood, idDesktopExpanded
   );

  {: These are equivalent to the BIF_* constants in the Win32 API.  They are...
     used to specify what items can be expanded, and what items can be...
     selected by combining them in a set in the Options property.

     bfDirectoriesOnly: Only returns file system directories. If the user...
        selects folders that are not part of the file system, the OK button...
        is grayed.
     bfDomainOnly: Does not include network folders below the domain level...
        in the dialog.
     bfAncestors: Only returns file system ancestors (items which contain...
        files, like drives).  If the user selects anything other than a file...
        system ancestor, the OK button is grayed.
     bfComputers: Shows other computers.  If anything other than a computer...
        is selected, the OK button is disabled.
     bfPrinters:	Shows all printers.  If anything other than a printers is...
        selected, the OK button is disabled.
     bfIncludeFiles: Show non-folder items that exist in the folders.
     bfEditBox:   Includes an edit control in which the user can type the ...
        of an item.  Requires v4.71 of SHELL32.DLL.
     bfIncludeURLs: The browse dialog box can display URLs. The bfUseNewUI and
        bfIncludeFiles flags must also be set. If these three flags are not set,
        the browser dialog box will reject URLs. Even when these flags are set,
        the browse dialog box will only display URLs if the folder that contains
        the selected item supports them. When the folder's
        IShellFolder::GetAttributesOf method is called to request the selected
        item's attributes, the folder must set the SFGAO_FOLDER attribute flag.
        Otherwise, the browse dialog box will not display the URL. Requires
        v5.0 of SHELL32.DLL
     bfNewDialogStyle: Use the new user-interface. Setting this flag provides
        the user with a larger dialog box that can be resized. It has several
        new capabilities including: drag and drop capability within the dialog
        box, reordering, context menus, new folders, delete, and other context
        menu commands. Requires v5.0 of SHELL32.DLL
     bfShareable: The browse dialog box can display shareable resources on
        remote systems. It is intended for applications that want to expose
        remote shares on a local system. The bfUseNewUI flag must also be set.
        Requires v5.0 of SHELL32.DLL
     bfUseNewUI: Use the new user-interface including an edit box. This flag is
        equivalent to bfEditBox and bfNewDialogStyle. Requires v5.0 of
        SHELL32.DLL
  }
  TBrowseFlag = (
    bfDirectoriesOnly, bfDomainOnly, bfAncestors, bfComputers, bfPrinters,
    bfIncludeFiles, bfEditBox, bfIncludeURLs, bfNewDialogStyle, bfShareable,
    bfUseNewUI
   );

  {: A set of TBrowseFlag items. }
  TBrowseFlags = set of TBrowseFlag;

  { TBDSelChangedEvent is used for events associated with...
    TdfsBrowseDirectoryDlg's OnSelChanged event.

    The Sender parameter is the TdfsBrowseDirectoryDlg object whose event handler...
    is called.  The NewSel parameter is the text representation of the new...
    selection.  The NewSelPIDL is the new PItemIDList representation of the...
    new selection. }
  TBDSelChangedEvent = procedure(Sender: TObject; NewSel: string;
     NewSelPIDL: PItemIDList) of object;

  TBDValidateFailedEvent = procedure(Sender: TObject; Path: string;
     var Cancel: boolean) of object;
     
type
  {: TdfsBrowseDirectoryDlg provides a component that displays a standard...
     Windows 95/NT 4 dialog containing the user's system in a heirarchial...
     manner and allows a selection to be made.  It is a wrapper for the...
     SHBrowseForFolder() API, which is quite messy to use directly. }
  TdfsBrowseDirectoryDlg = class(TComponent)
  private
    { Property variables }
    FDlgWnd: HWND;
    FCaption: string;
    FParent: TWinControl;
    FShowSelectionInStatus: boolean;
    FFitStatusText: boolean;
    FTitle: string;
    FRoot: TRootID;
    FOptions: TBrowseFlags;
    FSelection: string;
    FCenter: boolean;
    FStatusText: string;
    FEnableOKButton: boolean;
    FImageIndex: integer;
    FSelChanged: TBDSelChangedEvent;
    FOnCreate: TNotifyEvent;
		FSelectionPIDL: PItemIDList;
    FShellMalloc: IMalloc;
    FDisplayName: string;
    FOnValidateFailed: TBDValidateFailedEvent;

		function GetDisplayName: string;
  protected
    // internal methods
    function FittedStatusText: string;
    procedure SendSelectionMessage;
    // internal event methods.
    procedure DoInitialized(Wnd: HWND); virtual;
    procedure DoSelChanged(Wnd: HWND; Item: PItemIDList); virtual;
    procedure DoValidateFailed(Path: string; var Cancel: boolean); virtual;
    // property methods
    procedure SetFitStatusText(Val: boolean);
    procedure SetOptions(const Val: TBrowseFlags);
    procedure SetStatusText(const Val: string);
    procedure SetSelection(const Val: string);
		procedure SetSelectionPIDL(Value: PItemIDList);
    procedure SetEnableOKButton(Val: boolean);
    function GetCaption: string;
    procedure SetCaption(const Val: string);
    procedure SetParent(AParent: TWinControl);
    function GetVersion: string;
    procedure SetVersion(const Val: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: Displays the browser folders dialog.  It returns TRUE if user selected...
       an item and pressed OK, otherwise it returns FALSE. }
    function Execute: boolean; virtual;

    {: The window component that is the browse dialog's parent window.  By...
       assigning a value to this property, you can control the parent window...
       independant of the form that the component exists on.

       You do not normally need to assign any value to this property as it...
       will use the form that contains the component by default. }
    property Parent: TWinControl
       read FParent
       write SetParent;
    {: An alternative to the Selection property.  Use this property if the...
       item you are interested in does not have a path (Control Panels, for...
       example).  The most common way to retrieve a value for this property...
       is to use the SHGetSpecialFolderLocation Windows API function. Once...
       you have assigned a value to this property, it is "owned" by the...
       component.  That is, the component will take care of freeing it when...
       it is no longer needed.

       When setting this property before calling the Execute method, it will...
       only be used if the Selection property is blank.  If Selection is not...
       blank, it will be used instead.

       Upon return from the Execute method, this property will contain the...
       PItemIDList of the item the user selected.  In some cases, this will...
       the only way to get the user's choice since items such as Control...
       Panel do not have a string that can be placed in the Selection property.}
		property SelectionPIDL: PItemIDList
       read FSelectionPIDL
       write SetSelectionPIDL;
    {: DisplayName is run-time, read-only property that returns the display...
       name of the selection.  It only has meaning after the dialog has been...
       executed and the user has made a selection.  It returns the "human...
       readable" form of the selection.  This generally is the same as the...
       Selection property when it is a file path, but in the case of items...
       such as the Control Panel which do not have a path, Selection is blank.
       In this case, the only way to access the users' selection is to use...
       the SelectionPIDL property.  That doesn't provide an easy way of...
       presenting a textual representation of what they chose, but this...
       property will do that for you.

       If, for example, the user chose the Control Panel folder, the Selection...
       property would be blank, but DisplayName would be "Control Panel".  You...
       could not actually use this value to get to the Control Panel, for that...
       you need to use the SelectionPIDL property and various Shell Namespace...
       API functions. }
		property DisplayName: string
       read GetDisplayName;
    {: Handle is a run-time, read-only property that returns the window handle...
       of the browse dialog window.  It is valid only while the dialog is...
       displayed.  That is, it's not valid until the OnCreate event fires, and
       is no longer valid after the Execute method returns. }
    property Handle: HWND
       read FDlgWnd;
  published
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;

    {: The selected item in the browse folder dialog.

       Setting this before calling the Execute method will cause the assigned...
       value to be initially selected when the dialog is initially displayed...
       if the item exists.  If it does not exist, the root item will be selected.

       If this value is blank, the SelectionPIDL item will be used instead.

       After the Execute method returns, you can read this value to determine...
       what item the user selected, unless that item does not have a string...
       representation (Control Panel, for example). }
    property Selection: string
       read FSelection
       write SetSelection;
    {: Specifies the text to appear at the top of the dialog above the tree...
       control.  There is enough room for two lines of text, and it will be...
       word-wrapped for you automatically.

       Generally, this is used to provide user instructions or as a title for
       the StatusText property.

       Example:

       // Title property set to "The current selection is:"
       procedure TForm1.BrowseDirectoryDlgSelChanged(Sender: TObject; const NewSel: string);
       begin
         // NewSel has the full selection
         BrowseDirectoryDlg.StatusText := NewSel;
       end;
    }
    property Title: string
       read FTitle
       write FTitle;
    {: Specifies the item that is to be treated as the root of the tree...
       display.

    idDesktop: Windows desktop -- virtual folder at the root of the name space.
    idInternet: Internet Explorer -- virtual folder of the Internet Explorer.
    idPrograms: File system directory that contains the user's program groups...
       (which are also file system directories).
    idControlPanel: Control Panel -- virtual folder containing icons for the...
       control panel applications.
    idPrinters: Printers folder -- virtual folder containing installed printers.
    idPersonal: File system directory that serves as a common respository for...
       documents.
    idFavorites: Favorites folder -- virtual folder containing the user's...'
       Internet Explorer bookmark items and subfolders.
    idStartup: File system directory that corresponds to the user's Startup...
       program group.
    idRecent: File system directory that contains the user's most recently...
       used documents.
    idSendTo: File system directory that contains Send To menu items.
    idRecycleBin: Recycle bin -- file system directory containing file...
       objects in the user's recycle bin. The location of this directory is...
       not in the registry; it is marked with the hidden and system...
       attributes to prevent the user from moving or deleting it.
    idStartMenu: File system directory containing Start menu items.
    idDesktopDirectory: File system directory used to physically store file...
       objects on the desktop (not to be confused with the desktop folder itself).
    idDrives: My Computer -- virtual folder containing everything on the...
       local computer: storage devices, printers, and Control Panel. The...
       folder may also contain mapped network drives.
    idNetwork: Network Neighborhood -- virtual folder representing the top...
       level of the network hierarchy.
    idNetHood: File system directory containing objects that appear in the...
       network neighborhood.
    idFonts: Virtual folder containing fonts.
    idTemplates: File system directory that serves as a common repository for...
       document templates.
    idCommonStartMenu: File system directory that contains the programs and...
       folders that appear on the Start menu for all users on Windows NT.
    idCommonPrograms: File system directory that contains the directories for...
       the common program groups that appear on the Start menu for all users...
       on Windows NT.
    idCommonStartup: File system directory that contains the programs that...
       appear in the Startup folder for all users. The system starts these...
       programs whenever any user logs on to Windows NT.
    idCommonDesktopDirectory: File system directory that contains files and...
       folders that appear on the desktop for all users on Windows NT.
    idAppData: File system directory that contains data common to all...
       applications.
    idPrintHood: File system directory containing object that appear in the...
       printers folder.
    idDesktopExpanded: Same as idDesktop except that the root item is already...
       expanded when the dialog is initally displayed.

    NOTE: idCommonStartMenu, idCommonPrograms, idCommonStartup, and...
       idCommonDesktopDirectory only have effect when the dialog is being...
       displayed on an NT system.  On Windows 95, these values will be...
       mapped to thier "non-common" equivalents, i.e. idCommonPrograms will...
       become idPrograms.
    }
    property Root: TRootID
       read FRoot
       write FRoot
       default idDesktop;
    {: Options is a set of TBrowseFlag items that controls what is allowed to...
       be selected and expanded in the tree.  It can be a combination of any...
       (or none) of the following:

     bfDirectoriesOnly: Only returns file system directories. If the user...
        selects folders that are not part of the file system, the OK button...
        is grayed.
     bfDomainOnly: Does not include network folders below the domain level...
        in the dialog.
     bfAncestors: Only returns file system ancestors (items which contain...
        files, like drives).  If the user selects anything other than a file...
        system ancestor, the OK button is grayed.
     bfComputers: Shows other computers.  If anything other than a computer...
        is selected, the OK button is disabled.
     bfPrinters:	Shows all printers.  If anything other than a printers is...
        selected, the OK button is disabled.
     bfIncludeFiles: Show non-folder items that exist in the folders.
     bfEditBox:   Includes an edit control in which the user can type the ...
        of an item.  If the user enters an invalid path, the OnValidateFailed...
        event will fire.  Requires v4.71 of SHELL32.DLL.
     bfIncludeURLs: The browse dialog box can display URLs. The bfUseNewUI and
        bfIncludeFiles flags must also be set. If these three flags are not set,
        the browser dialog box will reject URLs. Even when these flags are set,
        the browse dialog box will only display URLs if the folder that contains
        the selected item supports them. When the folder's
        IShellFolder::GetAttributesOf method is called to request the selected
        item's attributes, the folder must set the SFGAO_FOLDER attribute flag.
        Otherwise, the browse dialog box will not display the URL. Requires
        v5.0 of SHELL32.DLL
     bfNewDialogStyle: Use the new user-interface. Setting this flag provides
        the user with a larger dialog box that can be resized. It has several
        new capabilities including: drag and drop capability within the dialog
        box, reordering, context menus, new folders, delete, and other context
        menu commands. Requires v5.0 of SHELL32.DLL
     bfShareable: The browse dialog box can display shareable resources on
        remote systems. It is intended for applications that want to expose
        remote shares on a local system. The bfUseNewUI flag must also be set.
        Requires v5.0 of SHELL32.DLL
     bfUseNewUI: Use the new user-interface including an edit box. This flag is
        equivalent to bfEditBox and bfNewDialogStyle. Requires v5.0 of
        SHELL32.DLL
    }
    property Options: TBrowseFlags
       read FOptions
       write SetOptions
       default [];
    {: Indicates whether the dialog should be centered on the screen or shown...
      in a default, system-determined location. }
    property Center: boolean
       read FCenter
       write FCenter
       default TRUE;
    {: A string that is displayed directly above the tree view control and...
       just under the Title text in the dialog box. This string can be used...
       for any purpose such as to specify instructions to the user, or show...
       the full path of the currently selected item.  You can modify this...
       value while the dialog is displayed from the the OnSelChanged event.

       If StatusText is blank when the Execute method is called, the dialog...
       will not have a status text area and assigning to the StatusText...
       property will have no effect.

       Example:

       // Title property set to "The current selection is:"
       procedure TForm1.BrowseDirectoryDlgSelChanged(Sender: TObject; const NewSel: string);
       begin
         // NewSel has the full selection
         BrowseDirectoryDlg.StatusText := NewSel;
       end;
       }
    property StatusText: string
       read FStatusText
       write SetStatusText;
    {: Indicates whether the StatusText string should be shortened to make it...
       fit in available status text area.  The status text area is only large...
       enough to hold one line of text, and if the text is too long for the...
       available space, it will simply be chopped off.  However, if this...
       property is set to TRUE, the text will be shortened using an ellipsis...
       ("...").

       For example, if the status text property were...
       "C:\Windows\Start Menu\Programs\Applications\Microsoft Reference", it
       could be shortened to...
       "C:\...\Start Menu\Programs\Applications\Microsoft Reference" depending
       on the screen resolution and dialog font size.
    }
    property FitStatusText: boolean
       read FFitStatusText
       write SetFitStatusText
       default TRUE;
    {: This property enables or disables the OK button on the browse folders...
       dialog.  This allows control over whether a selection can be made or...
       not. You can modify this value while the dialog is displayed from the...
       the OnSelChanged event.  This allows you to control whether the user...
       can select an item based on what the current selection is.

       Example:
       procedure TForm1.BrowseDirectoryDlgSelChanged(Sender: TObject; const NewSel: string);
       begin
         // NewSel has the full selection.  Only allow items greater than 10 characters to be selected.
         BrowseDirectoryDlg.EnableOKButton := Length(NewSel > 10);
       end;
    }
    property EnableOKButton: boolean
       read FEnableOKButton
       write SetEnableOKButton
       default TRUE;
    {: After a selection has been made in the dialog, this property will...
       contain the index into the system image list of the selected node. See...
       the demo application for an example how this can be used. }
    property ImageIndex: integer
       read FImageIndex;
    {: Specifies the text in the dialog's caption bar. Use Caption to specify...
       the text that appears in the browse folder dialog's title bar. If no...
       value is assigned to Title, the dialog has a title based on the...
       Options property.

       For example, if bfPrinters was set, the title would be "Browse for...
       Printer". }
    property Caption: string
       read GetCaption
       write SetCaption;
    {: Automatically shows the current selection in the status text area of...
       the dialog.  }
    property ShowSelectionInStatus: boolean
       read FShowSelectionInStatus
       write FShowSelectionInStatus;
    {: The OnSelChange event is fired every time a new item is selected in...
       the tree.

      The Sender parameter is the TdfsBrowseDirectoryDlg object whose event...
      handler is called.  The NewSel parameter is the text representation of...
      the new selection.  The NewSelPIDL is the new PItemIDList...
      representation of the new selection.

      NOTE:  You will need to add ShlObj to your uses clause if you define...
      a handler for this event. }
    property OnSelChanged: TBDSelChangedEvent
       read FSelChanged
       write FSelChanged;
    { The OnCreate event is fired when dialog has been created, but just...
       before it is displayed to the user. }
    property OnCreate: TNotifyEvent
       read FOnCreate
       write FOnCreate;
    { If the bfEditBox flag is set in the Options property, the user can type...
      a path into the dialog.  If the path entered is invalid, this event...
      will be fired.  This event is not used if bfEditBox is not specified in...
      Options.  Requires v4.71 of SHELL32.DLL. }
    property OnValidateFailed: TBDValidateFailedEvent
       read FOnValidateFailed
       write FOnValidateFailed;
  end;

{ Utility function you may find useful }
function DirExists(const Dir: string): boolean;

implementation

uses
   Winapi.Messages,
   Winapi.ShellAPI,
   System.SysUtils,
   Vcl.Forms;

// Utility functions used to convert from Delphi set types to API constants.
function ConvertRoot(Root: TRootID): integer;
const
  WinNT_RootValues: array[TRootID] of integer = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
    CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
    CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS,
    CSIDL_COMMON_STARTUP, CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA,
    CSIDL_PRINTHOOD, CSIDL_DESKTOPEXPANDED
  );
  Win95_RootValues: array[TRootID] of integer = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
    CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
    CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_STARTMENU, CSIDL_PROGRAMS,
    CSIDL_STARTUP, CSIDL_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD,
    CSIDL_DESKTOPEXPANDED
  );
var
  VerInfo: TOSVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  if VerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    Result := WinNT_RootValues[Root]
  else
    Result := Win95_RootValues[Root];
end;

function ConvertFlags(Flags: TBrowseFlags): UINT;
const
  FlagValues: array[TBrowseFlag] of UINT = (
    BIF_RETURNONLYFSDIRS, BIF_DONTGOBELOWDOMAIN, BIF_RETURNFSANCESTORS,
    BIF_BROWSEFORCOMPUTER, BIF_BROWSEFORPRINTER, BIF_BROWSEINCLUDEFILES,
    BIF_EDITBOX, BIF_BROWSEINCLUDEURLS, BIF_NEWDIALOGSTYLE, BIF_SHAREABLE,
    BIF_USENEWUI
   );
var
  Opt: TBrowseFlag;
begin
  Result := 0;
  { Loop through all possible values }
  for Opt := Low(TBrowseFlag) to High(TBrowseFlag) do
    if Opt in Flags then
      Result := Result OR FlagValues[Opt];
end;

function GetTextWidth(DC: HDC; const Text: String): Integer;
var
  Extent: TSize;
begin
  if GetTextExtentPoint(DC, PChar(Text), Length(Text), Extent) then
    Result := Extent.cX
  else
    Result := 0;
end;

function MinimizeName(Wnd: HWND; const Filename: string): string;

  procedure CutFirstDirectory(var S: string);
  var
    Root: Boolean;
    P: Integer;
  begin
    if S = '\' then
      S := ''
    else begin
      if S[1] = '\' then begin
        Root := True;
        Delete(S, 1, 1);
      end else
        Root := False;
      if S[1] = '.' then
        Delete(S, 1, 4);
      P := Pos('\',S);
      if P <> 0 then begin
        Delete(S, 1, P);
        S := '...\' + S;
      end else
        S := '';
      if Root then
        S := '\' + S;
    end;
  end;

var
  Drive: string;
  Dir: string;
  Name: string;
  R: TRect;
  DC: HDC;
  MaxLen: integer;
  OldFont, Font: HFONT;
begin
  Result := FileName;
  if Wnd = 0 then exit;
  DC := GetDC(Wnd);
  if DC = 0 then exit;
  Font := HFONT(SendMessage(Wnd, WM_GETFONT, 0, 0));
  OldFont := SelectObject(DC, Font);
  try
    GetWindowRect(Wnd, R);
    MaxLen := R.Right - R.Left;

    Dir := ExtractFilePath(Result);
    Name := ExtractFileName(Result);

    if (Length(Dir) >= 2) and (Dir[2] = ':') then begin
      Drive := Copy(Dir, 1, 2);
      Delete(Dir, 1, 2);
    end else
      Drive := '';
    while ((Dir <> '') or (Drive <> '')) and (GetTextWidth(DC, Result) > MaxLen) do begin
      if Dir = '\...\' then begin
        Drive := '';
        Dir := '...\';
      end else if Dir = '' then
        Drive := ''
      else
        CutFirstDirectory(Dir);
      Result := Drive + Dir + Name;
    end;
  finally
    SelectObject(DC, OldFont);
    ReleaseDC(Wnd, DC);
  end;
end;

function MinimizeString(Wnd: HWND; const Text: string): string;
var
  R: TRect;
  DC: HDC;
  MaxLen: integer;
  OldFont, Font: HFONT;
  TempStr: string;
begin
  Result := Text;
  TempStr := Text;
  if Wnd = 0 then exit;
  DC := GetDC(Wnd);
  if DC = 0 then exit;
  Font := HFONT(SendMessage(Wnd, WM_GETFONT, 0, 0));
  OldFont := SelectObject(DC, Font);
  try
    GetWindowRect(Wnd, R);
    MaxLen := R.Right - R.Left;
    while (TempStr <> '') and (GetTextWidth(DC, Result) > MaxLen) do begin
      SetLength(TempStr, Length(TempStr)-1);
      Result := TempStr + '...';
    end;
  finally
    SelectObject(DC, OldFont);
    ReleaseDC(Wnd, DC);
  end;
end;


function DirExists(const Dir: string): boolean;
  function StripTrailingBackslash(const Dir: string): string;
  begin
    Result := Dir;
    // Make sure we have a string, and if so, see if the last char is a \
    if (Result <> '') and (Result[Length(Result)] = '\') then
      SetLength(Result, Length(Result)-1); // Shorten the length by one to remove
  end;
var
  Tmp: string;
  DriveBits: set of 0..25;
  SR: TSearchRec;
  Found: boolean;
  OldMode: Word;
begin
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if (Length(Dir) = 3) and (Dir[2] = ':') and (Dir[3] = '\') then begin
      Integer(DriveBits) := GetLogicalDrives;
      Tmp := UpperCase(Dir[1]);
      Result := (ord(Tmp[1]) - ord('A')) in DriveBits;
    end else begin
      Found := FindFirst(StripTrailingBackslash(Dir), faDirectory, SR) = 0;
      Result := Found and (Dir <> '');
      if Result then
        Result := (SR.Attr and faDirectory) = faDirectory;
      if Found then
        // only call FinClose if FindFirst succeeds.  Can lock NT up if it didn't
        FindClose(SR);
    end;
  finally
    SetErrorMode(OldMode);
  end;
end; // DirExists

function BrowseCallbackProc(Wnd: HWnd; Msg: UINT; lParam: LPARAM; lData: LPARAM): integer; stdcall;
var
  Cancel: boolean;
begin
  Result := 0;
  if lData <> 0 then
  begin
    case Msg of
      BFFM_INITIALIZED:
        TdfsBrowseDirectoryDlg(lData).DoInitialized(Wnd);
      BFFM_SELCHANGED:
        TdfsBrowseDirectoryDlg(lData).DoSelChanged(Wnd, PItemIDList(lParam));
      BFFM_VALIDATEFAILED:
        begin
          Cancel := FALSE;
          TdfsBrowseDirectoryDlg(lData).DoValidateFailed(string(PChar(lParam)),
             Cancel);
          if Cancel then
            Result := 0
          else
            Result := 1;
        end;
    end;
  end;
end;


(*
function CopyPIDL(ShellMalloc: IMalloc; AnID: PItemIDList): PItemIDList;
var
  Size: integer;
begin
  Size := 0;
  if AnID <> NIL then
  begin
    while AnID.mkid.cb > 0 do
    begin
      Inc(Size, AnID.mkid.cb  + SizeOf(AnID.mkid.cb));
      AnID := PItemIDList(Longint(AnID) + AnID.mkid.cb);
    end;
  end;

  if Size > 0 then
  begin
    Result := ShellMalloc.Alloc(Size); // Create the memory
    FillChar(Result^, Size, #0); // Initialize the memory to zero
    Move(AnID^, Result^, Size); // Copy the current ID
  end else
    Result := NIL;
end;
*)

function GetImageIndex(const AFile: string): integer;
var
  SFI: TSHFileInfo;
begin
  SHGetFileInfo(PChar(AFile), 0, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX);
  Result := SFI.iIcon;
end;


function BrowseDirectory(const ShellMalloc: IMalloc; var Dest: string;
   var DestPIDL: PItemIDList; var ImgIdx: integer; var DisplayName: string;
   const AParent: TWinControl; const Title: string; Root: TRootID;
   Flags: TBrowseFlags; WantStatusText: boolean; Callback: TFNBFFCallBack;
   Data: Longint): boolean;
var
  shBuff: PChar;
  BrowseInfo: TBrowseInfo;
  idRoot, idBrowse: PItemIDList;
  WndHandle: HWND;
  OldErrorMode: word;
begin
  Result := FALSE; // Assume the worst.
  Dest := ''; // Clear it out.
  SetLength(Dest, MAX_PATH);  // Make sure their will be enough room in dest.
  if assigned(AParent) then
    WndHandle := AParent.Handle
  else
    WndHandle := 0;
  shBuff := PChar(ShellMalloc.Alloc(MAX_PATH)); // Shell allocate buffer.
  if assigned(shBuff) then begin
    CoInitialize(NIL);
    try
      // Get id for desired root item.
      SHGetSpecialFolderLocation(WndHandle, ConvertRoot(Root), idRoot);
      try
        with BrowseInfo do begin  // Fill info structure
          hwndOwner := WndHandle;
          pidlRoot := idRoot;
          pszDisplayName := shBuff;
          lpszTitle := PChar(Title);
          ulFlags := ConvertFlags(Flags);
          { See if we need to handle the validate event }
          if bfEditBox in Flags then
            ulFlags := ulFlags or BIF_VALIDATE;
          if WantStatusText then
            ulFlags := ulFlags or BIF_STATUSTEXT;
          lpfn := Callback;
          lParam := Data;
        end;
        OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
        try
          idBrowse := SHBrowseForFolder(BrowseInfo);
        finally
          SetErrorMode(OldErrorMode);
        end;
        DestPIDL := idBrowse;
        if assigned(idBrowse) then begin
          // Try to turn it into a real path.
          if (bfComputers in Flags) then
          begin
            { Make a copy because SHGetPathFromIDList will whack it }
            Dest:= '\\' + string(shBuff);
            Result := SHGetPathFromIDList(idBrowse, shBuff);
            { Is it a valid path? }
            if Result then
              Dest := shBuff // Put it in user's variable.
            else
              { do nothing, the copy we made above is set to go };
            Result:= True;
          end else begin
            Result := SHGetPathFromIDList(idBrowse, shBuff);
            Dest := shBuff; // Put it in user's variable.
          end;
          // Stupid thing won't return the index if the user typed it in.
          if Result and (BrowseInfo.iImage = -1) then
            ImgIdx := GetImageIndex(Dest)
          else
            ImgIdx := BrowseInfo.iImage; // Update the image index.
        end;
        if not Result then
          Result := DestPIDL <> NIL;
        if Result then
          DisplayName := BrowseInfo.pszDisplayName;
      finally
        ShellMalloc.Free(idRoot); // Clean-up.
      end;
    finally
      ShellMalloc.Free(shBuff); // Clean-up.
      CoUninitialize;
    end;
  end;
end;

constructor TdfsBrowseDirectoryDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayName := '';
  FDlgWnd := 0;
  FFitStatusText := TRUE;
  FEnableOKButton := TRUE;
  FTitle := '';
  FRoot := idDesktop;
  FOptions := [];
  FSelection := '';
  FSelectionPIDL := NIL;
  FCenter := TRUE;
  FSelChanged := NIL;
  FStatusText := '';
  FImageIndex := -1;
  FCaption := '';
  SHGetMalloc(FShellMalloc);

  if assigned(AOwner) then
    if AOwner is TWinControl then
      FParent := TWinControl(Owner)
    else if assigned(Application) and assigned(Application.MainForm) then
      FParent := Application.MainForm;
end;

destructor TdfsBrowseDirectoryDlg.Destroy;
begin
  if assigned(FSelectionPIDL) then
    FShellMalloc.Free(FSelectionPIDL);
  // D3 cleans it up for you, D2 does not.
  {$IFNDEF DFS_NO_COM_CLEANUP} FShellMalloc.Release; {$ENDIF}

  inherited Destroy;
end;

function TdfsBrowseDirectoryDlg.Execute: boolean;
var
  S: string;
  AParent: TWinControl;
  TempPIDL: PItemIDList;
begin
  FDisplayName := '';
  { Assume the worst }
  AParent := NIL;
  if not (csDesigning in ComponentState) then
    { Determine who the parent is. }
    if assigned(FParent) then
      AParent := FParent
    else begin
      if assigned(Owner) then
        if Owner is TWinControl then
          AParent := TWinControl(Owner)
        else
          if assigned(Application) and assigned(Application.MainForm) then
            AParent := Application.MainForm;
    end;

  { Call the function }
  Result := BrowseDirectory(FShellMalloc, S, TempPIDL, FImageIndex,
     FDisplayName, AParent, FTitle, FRoot, FOptions,
     (FStatusText <> '') or FShowSelectionInStatus, BrowseCallbackProc,
     LongInt(Self));

  FDlgWnd := 0; { Not valid any more. }

  { If selection made, update property }
  if Result then
  begin
    FSelection := S;
    SelectionPIDL := TempPIDL;
  end else begin
    FSelection := '';
    SelectionPIDL := NIL;
  end;
end;

function FormatSelection(const APath: string): string;
begin
  Result := APath;
  if Result <> '' then begin
    if (Length(Result) < 4) and (Result[2] = ':') then begin
      if Length(Result) = 2 then
        Result := Result + '\'
    end else
      if (Result[Length(Result)] = '\') and (Result <> '\') then
        SetLength(Result, Length(Result)-1);
  end;
end;

procedure TdfsBrowseDirectoryDlg.SendSelectionMessage;
var
  TempSelectionPIDL: PItemIDList;
  ShellFolder: IShellFolder;
  OLEStr: array[0..MAX_PATH] of TOLEChar;
  Eaten: ULONG;
  Attr: ULONG;
  shBuff: PChar;
begin
  if (FSelection = '') and assigned(FSelectionPIDL) then
  begin
    shBuff := PChar(FShellMalloc.Alloc(MAX_PATH)); // Shell allocate buffer.
    try
      if SHGetPathFromIDList(FSelectionPIDL, shBuff) then
        FSelection := shBuff
      else
        FSelection := '';
    finally
      FShellMalloc.Free(shBuff); // Clean-up.
    end;
    SendMessage(FDlgWnd, BFFM_SETSELECTION, 0, LPARAM(FSelectionPIDL));
  end else begin
    if Copy(FSelection, 1, 2) = '\\' then // UNC name!
    begin
      if SHGetDesktopFolder(ShellFolder) = NO_ERROR then
      begin
        try
          if ShellFolder.ParseDisplayName(FDlgWnd, NIL,
             StringToWideChar(FSelection, OLEStr, MAX_PATH), Eaten,
             TempSelectionPIDL, Attr) = NO_ERROR then
          begin
            SelectionPIDL := TempSelectionPIDL;
            SendMessage(FDlgWnd, BFFM_SETSELECTION, 0, LPARAM(FSelectionPIDL));
          end;
        finally
          {$IFNDEF DFS_NO_COM_CLEANUP} ShellFolder.Release; {$ENDIF}
        end;
      end;
    end else begin { normal path }
      if SHGetDesktopFolder(ShellFolder) = NO_ERROR then
      begin
        try
          if ShellFolder.ParseDisplayName(FDlgWnd, NIL,
             StringToWideChar(FSelection, OLEStr, MAX_PATH), Eaten,
             TempSelectionPIDL, Attr) = NO_ERROR then
            SelectionPIDL := TempSelectionPIDL;
        finally
          {$IFNDEF DFS_NO_COM_CLEANUP} ShellFolder.Release; {$ENDIF}
        end;
        SendMessage(FDlgWnd, BFFM_SETSELECTION, 1,
           LPARAM(FormatSelection(FSelection)));
      end;
    end;
  end;
end;

procedure TdfsBrowseDirectoryDlg.DoInitialized(Wnd: HWND);
var
  Rect: TRect;
begin
  FDlgWnd := Wnd;
  if FCenter then begin
    GetWindowRect(Wnd, Rect);
    SetWindowPos(Wnd, 0,
      (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
      (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 2,
      0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
  end;
  // Documentation for BFFM_ENABLEOK is incorrect.  Value sent in LPARAM, not WPARAM.
  SendMessage(FDlgWnd, BFFM_ENABLEOK, 0, LPARAM(FEnableOKButton));
  if FStatusText <> '' then
    SendMessage(Wnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
  if (FSelection <> '') or (FSelectionPIDL <> NIL) then
    SendSelectionMessage;
  if FCaption <> '' then
    SendMessage(FDlgWnd, WM_SETTEXT, 0, LPARAM(FCaption));
  if assigned(FOnCreate) then
    FOnCreate(Self);
end;

procedure TdfsBrowseDirectoryDlg.DoSelChanged(Wnd: HWND; Item: PItemIDList);
var
  Name: string;
begin
  if FShowSelectionInStatus or assigned(FSelChanged) then
  begin
    Name := '';
    SetLength(Name, MAX_PATH);
    SHGetPathFromIDList(Item, PChar(Name));
    SetLength(Name, StrLen(PChar(Name)));
    if FShowSelectionInStatus then
      StatusText := Name;
    if assigned(FSelChanged) then
      FSelChanged(Self, Name, Item);
  end;
end;

procedure TdfsBrowseDirectoryDlg.DoValidateFailed(Path: string;
   var Cancel: boolean);
begin
  if assigned(FOnValidateFailed) then
    FOnValidateFailed(Self, Path, Cancel);
end;

procedure TdfsBrowseDirectoryDlg.SetFitStatusText(Val: boolean);
begin
  if FFitStatusText = Val then exit;
  FFitStatusText := Val;
  // Reset the status text area if needed.
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
end;

procedure TdfsBrowseDirectoryDlg.SetStatusText(const Val: string);
begin
  if FStatusText = Val then exit;
  FStatusText := Val;
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, BFFM_SETSTATUSTEXT, 0, LPARAM(FittedStatusText));
end;

procedure TdfsBrowseDirectoryDlg.SetSelection(const Val: string);
begin
  if FSelection = Val then exit;
  FSelection := Val;
  // Add trailing backslash so it looks better in the IDE.
  if (FSelection <> '') and (FSelection[Length(FSelection)] <> '\') and
     DirExists(FSelection) then
    FSelection := FSelection + '\'
  else if (FSelection = '') and assigned(FSelectionPIDL) then
  begin
    FShellMalloc.Free(FSelectionPIDL);
    FSelectionPIDL := NIL;
  end;
  if FShowSelectionInStatus then
    StatusText := FSelection;
  if FDlgWnd <> 0 then
    SendSelectionMessage;
end;

procedure TdfsBrowseDirectoryDlg.SetSelectionPIDL(Value: PItemIDList);
begin
	if (FSelectionPIDL <> Value) then
  begin
    if assigned(FSelectionPIDL) then
      FShellMalloc.Free(FSelectionPIDL);
		FSelectionPIDL := Value;
	end;
end;



procedure TdfsBrowseDirectoryDlg.SetEnableOKButton(Val: boolean);
begin
  FEnableOKButton := Val;
  if FDlgWnd <> 0 then
    // Documentation for BFFM_ENABLEOK is incorrect.  Value sent in LPARAM, not WPARAM.
    SendMessage(FDlgWnd, BFFM_ENABLEOK, 0, LPARAM(FEnableOKButton));
end;

function TdfsBrowseDirectoryDlg.GetCaption: string;
var
  Temp: array[0..255] of char;
begin
  if FDlgWnd <> 0 then
  begin
    SendMessage(FDlgWnd, WM_GETTEXT, SizeOf(Temp), LPARAM(@Temp));
    Result := string(Temp);
  end else
    Result := FCaption;
end;

procedure TdfsBrowseDirectoryDlg.SetCaption(const Val: string);
begin
  FCaption := Val;
  if FDlgWnd <> 0 then
    SendMessage(FDlgWnd, WM_SETTEXT, 0, LPARAM(FCaption));
end;

procedure TdfsBrowseDirectoryDlg.SetParent(AParent: TWinControl);
begin
  FParent := AParent;
end;

// Note that BOOL <> boolean type.  Important!
function EnumChildWndProc(Child: HWND; Data: LParam): BOOL; stdcall;
const
  STATUS_TEXT_WINDOW_ID = 14147;
type
  PHWND = ^HWND;
begin
  if GetWindowLong(Child, GWL_ID) = STATUS_TEXT_WINDOW_ID then begin
    PHWND(Data)^ := Child;
    Result := FALSE;
  end else
    Result := TRUE;
end;

function TdfsBrowseDirectoryDlg.FittedStatusText: string;
var
  ChildWnd: HWND;
begin
  Result := FStatusText;
  if FFitStatusText then begin
    ChildWnd := 0;
    if FDlgWnd <> 0 then
      // Enumerate all child windows of the dialog to find the status text window.
      EnumChildWindows(FDlgWnd, @EnumChildWndProc, LPARAM(@ChildWnd));
    if (ChildWnd <> 0) and (FStatusText <> '') then
      if DirExists(FStatusText) then
        Result := MinimizeName(ChildWnd, FStatusText)
      else
        Result := MinimizeString(ChildWnd, FStatusText);
  end;
end;

function TdfsBrowseDirectoryDlg.GetDisplayName: string;
var
  ShellFolder: IShellFolder;
  Str : TStrRet;
begin
  Result := '';
  if FSelectionPIDL <> NIL then
  begin
    if SHGetDesktopFolder(ShellFolder) = NO_ERROR then
    begin
      try
        if ShellFolder.GetDisplayNameOf(FSelectionPIDL, SHGDN_FORPARSING,
           Str) = NOERROR then
        begin
          case Str.uType of
            STRRET_WSTR:   Result := WideCharToString(Str.pOleStr);
            {$IFDEF DFS_COMPILER_4_UP}
            STRRET_OFFSET: Result := PChar(LongWord(FSelectionPIDL) + Str.uOffset);
            {$ELSE}
            STRRET_OFFSET: Result := PChar(Longint(FSelectionPIDL) + Str.uOffset);
            {$ENDIF}
            STRRET_CSTR:   Result := Str.cStr;
          end;
        end;
      finally
        {$IFNDEF DFS_NO_COM_CLEANUP} ShellFolder.Release; {$ENDIF}
      end;
    end;
  end;
  if Result = '' then
    Result := FDisplayName;
  if Result = '' then
    Result := FSelection;
end;

function TdfsBrowseDirectoryDlg.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsBrowseDirectoryDlg.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;

procedure TdfsBrowseDirectoryDlg.SetOptions(const Val: TBrowseFlags);
begin
  if FOptions <> Val then
  begin
    FOptions := Val;
    if bfIncludeURLs in FOptions then
      FOptions := FOptions + [bfIncludeFiles, bfUseNewUI];
    if bfShareable in FOptions then
      FOptions := FOptions + [bfUseNewUI];
    if bfUseNewUI in FOptions then
      FOptions := FOptions + [bfNewDialogStyle, bfEditBox];
  end;
end;

end.
