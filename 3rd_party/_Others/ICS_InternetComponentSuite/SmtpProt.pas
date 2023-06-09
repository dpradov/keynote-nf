{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       Franois PIETTE
Object:       TSmtpCli class implements the SMTP protocol (RFC-821)
              Support file attachement using MIME format (RFC-1521)
              Support authentification (RFC-2104)
Creation:     09 october 1997
Version:      2.22
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by Franois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Oct 25, 1997  Added the OnHeaderLine event to allow modification/deletion of
              header lines.
Oct 26, 1997  V1.00 Released
              Changed the OnGetData event arguments to have code compatible
              between 16 and 32 bit versions (replaced string with PChar).
Jan 10, 1998  V1.01 Added a Port property
Feb 14, 1998  V1.02 Added an intermeditae TCustomSmtpClient in order to
              support MIME in the TSmtpCli. I implemented MIME with the
              help of code donated by Brad Choate <choate@delphiexchange.com>
              Mime is used for file attachement.
              Added a SetRcptName to copy values from a string list in place
              of copying the string list reference.
Feb 15, 1998  V1.03 Added a CharSet property, defaulting to iso-8859-1
Mar 02, 1998  V1.04 Corrected result for QUIT command.
              Marcus Schmutz <schmutz@kwsoft.de>
Mar 06, 1998  V1.05 Use OnDataSent event to prenvent over-buffering
Mar 15, 1998  V1.06 Implemented the Date header line
Apr 01, 1998  V1.07 Adapted for BCB V3
Apr 10, 1998  V1.08 Corrected DayNames: sunday is day 1, saturday is day 7.
              Changed UUEncode procedures to virtual methods to ease component
              inheritance.
Apr 26, 1998  V1.09 Ignore any empty file name (a very common error !)
              Check if file exists and raise an exception if not.
              Made Rfc822DateTime public.
              Added Rset method from Victor Garcia Aprea <vga@overnet.com.ar>
              Added Abort procedure to close the socket and abort any operation
              Made the underlaying TWSocket accessible using a property.
Apr 28, 1998  V1.10 Reset FTimeOutFlag in the mail procedure.
May 05, 1998  V1.11 Handled correctly lines beginning with a dot.
May 21, 1998  V1.12 Check for nil argument in SetEMailFiles
              Added OnCommand and OnResponse events.
              Added SendDataLine procedure (same as SendCommand, but do not
              trigger OnCommand event) used for header and message lines.
Jul 29, 1998  V2.00 Asynchronous functions and new TSyncSmtpCli component
              to be a placer holder for synchronous version.
              Renamed source file from SmtpCli to SmtpProt.
Aug 06, 1998  V2.01 Made HighLevelAsync public and added smtpCustom to be used
              for custom calls to HighLevelAsync.
Sep 22, 1998  V2.02 Removed useless Wait unit from the uses clause.
Oct 04, 1998  V2.03 Checked for Error in TriggerRequestDone.
Oct 11, 1998  V2.04 Removed -1 in DataNext. Thanks to Dennis V. Turov
              <chip@quorum.ru> for finding this bug.
Nov 22, 1998  V2.05 Implemented VRFY command with code proposed by
              DZ-Jay <dz@caribe.net> but use HdrTo property as name to verify.
Nov 29, 1998  V2.06 Added SetErrorMessage in WSocketSessionConnected when an
              error occured. Thanks to DZ-Jay.
              Changed FMimeBoundary format to use numbered month instead of
              month names. Thanks to Dmitry Kislov <kislov@tekom.odessa.ua> who
              found that some foreign charsets are invalid in mime boundaries.
Dec 22, 1998  V2.07 Handle exception when connecting (will be triggered when
              an invalid port has been given).
              Force readonly when reading attached files.
              Added ContentType property as suggested by Henri Fournier
              <hfournier@home.com>
Feb 13, 1999  V2.08 Published the state property and OnSessionConnected,
              OnSessionClosed events.
Feb 27, 1999  V2.09 Added Connected property.
              Added code from Larry Pesyna <ldpesyna@aep.com> to handle time
              zone bias.
              Added OnAttachContentType event. Thanks to Vladimir M.
              Zakharychev <zak@dzbjaro.bertelsmann.de> for his suggestion.
              Added ReplyTo and ReturnPath properties. Thanks to Eric Bullen
              <eric@thedeepsky.com> for his code.
Mar 06, 1999  V2.10 Conditional compile to remove timezone code unsupported by
              Delphi 1.
Mar 09, 1999  V2.11 Made state property [really] published.
Mar 27, 1999  V2.12 Published OnProcessHeader
              Changed sign for time zone bias (thanks to Larry Pesyna).
May 10, 1999  V2.13 'daylight' functionality for timezonebias function.
              Thanks to Bernhard Goebel <Bernhard.Goebel@t-online.de>
              Do not set FRequestType in Connect when called from HighLevel
              function. Thanks to Eugene V. Krapivin <evk@tagil.ru>.
May 18, 1999  V2.14 Added Sender field.  If ommited, the sender is becomes
              HdrFrom. Jon Glazer <jglazer@adconn.com>
Jul 30, 1999  V2.15 Added MailMessage property by Thomas Kvamme
              <thokvamm@online.no>. MailMessage property can be used with
              OnGetData event. If both are used, MailMessages lines appears
              before lines got by OnGetData.
Oct 02, 1999  V2.16 Added OnAttachHeader event as suggested by Vladimir M.
              Zakharychev <zak@dzbjaro.bertelsmann.de>
              Accept friendly EMail addresses. Thanks to Thierry De Leeuw
              <thierry.deleeuw@proxis.be> for his code.
Nov 01, 1999  V2.17 Made all fields protected to easy component inheritance.
Oct 15, 2000  V2.18 Check for too long lines in TriggerGetData.
              Thanks to Steve Williams <stevewilliams@kromestudios.com>
Jun 18, 2001  V2.19 Use AllocateHWnd and DeallocateHWnd from wsocket.
              Renamed property WSocket to CtrlSocket (this require code change
              in user application too).
Jul 26, 2001  V2.20  Angus Robertson <angus@magsys.co.uk> found a problem when
              using the MailSync method that it's not possible to send a body
              that takes longer than the timeout in WaitUntilReady. Timeout has
              to be reevaluated in TriggerGetData.
              Jake Traynham <jake@comm-unity.net> added authentification and
              EHLO code. Well done job.
Aug 18, 2001  V2.21 Angus V2.21 added OwnHeaders property flag which allows
              mail relaying where the body includes all headers and none are
              added by the component
Sep 09, 2001  V2.22 Beat Boegli <leeloo999@bluewin.ch> added LocalAddr property
              for multihomed hosts.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*  
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]

   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf 
  
 ****************************************************************)
 
unit SmtpProt;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

uses
    WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, Menus, WSocket, WinSock, MD5, WideStrings;             // [dpv]: use of WideStrings

const
  SmtpCliVersion     = 222;
  CopyRight : String = ' SMTP component (c) 1997-2001 F. Piette V2.22 ';
{$IFDEF VER80}
  { Delphi 1 has a 255 characters string limitation }
  SMTP_RCV_BUF_SIZE = 255;
{$ELSE}
  SMTP_RCV_BUF_SIZE = 4096;
{$ENDIF}
  WM_SMTP_REQUEST_DONE = WM_USER + 1;

type
    SmtpException    = class(Exception);
    TSmtpState       = (smtpReady,           smtpDnsLookup,
                        smtpConnecting,      smtpConnected,
                        smtpInternalReady,   smtpWaitingBanner,
                        smtpWaitingResponse, smtpAbort);
    TSmtpRequest     = (smtpConnect, smtpHelo,   smtpMailFrom,
                        smtpVrfy,    smtpRcptTo, smtpData,
                        smtpQuit,    smtpRset,   smtpOpen,
                        smtpMail,    smtpEhlo,   smtpAuth,
                        smtpCustom);
    TSmtpFct         = (smtpFctNone,       smtpFctHelo,   smtpFctConnect,
                        smtpFctMailFrom,   smtpFctRcptTo, smtpFctData,
                        smtpFctVrfy,       smtpFctQuit,   smtpFctRset,
                        smtpFctEhlo,       smtpFctAuth);
    TSmtpFctSet      = set of TSmtpFct;
    TSmtpContentType = (smtpHTML,       smtpPlainText);
    TSmtpAuthType    = (smtpAuthNone,   smtpAuthPlain, smtpAuthLogin,
                        smtpAuthCramMD5);

    TSmtpDisplay               = procedure(Sender  : TObject;
                                           Msg     : String) of object;
    TSmtpHeaderLineEvent       = procedure(Sender  : TObject;
                                           Msg     : PChar;
                                           Size    : Integer) of object;
    TSmtpProcessHeaderEvent    = procedure(Sender  : TObject;
                                           HdrLines  : TWideStrings) of object;
    TSmtpGetDataEvent          = procedure(Sender  : TObject;
                                           LineNum : Integer;
                                           MsgLine : PChar;
                                           MaxLen  : Integer;
                                           var More: Boolean) of object;
    TSmtpRequestDone           = procedure(Sender  : TObject;
                                           RqType    : TSmtpRequest;
                                           Error     : Word) of object;
    TSmtpAttachmentContentType = procedure(Sender          : TObject;
                                           FileNumber      : Integer;
                                           var FileName    : WideString;
                                           var ContentType : WideString) of object;
    TSmtpAttachHeader          = procedure(Sender          : TObject;
                                           FileNumber      : Integer;
                                           FileName        : WideString;
                                           HdrLines        : TWideStrings) of object;
    TSmtpNextProc              = procedure of object;

    { Base component, implementing the transport, without MIME support }
    TCustomSmtpClient = class(TComponent)
    protected
        FWSocket             : TWSocket;     { Underlaying socket          }
        FHost                : String;       { SMTP server hostname or IP  }
        FLocalAddr           : String; {bb}  { Local Address for mulithome }        
        FPort                : String;       { Should be 'smtp'            }
        FSignOn              : String;       { Used for the 'HELO' command }
        FUsername            : String;       { Used with the 'AUTH' command }
        FPassword            : String;       { Used with the 'AUTH' command }
        FAuthType            : TSmtpAuthType;{ Used with the 'AUTH' command }
        FFromName            : String;       { Sender's EMail              }
        FRcptName            : TWideStrings;     { Recepients EMails list      }
	FMailMessage	     : TWideStrings;
        FHdrFrom             : String;
        FHdrTo               : String;
        FHdrReplyTo          : String;
        FHdrReturnPath       : String;
        FHdrSubject          : String;
        FHdrSender           : String;       { Mail Sender's Email         }
        FState               : TSmtpState;
        FCharSet             : String;
        FContentType         : TSmtpContentType;
        FContentTypeStr      : String;
        FLastResponse        : String;
        FErrorMessage        : String;
        FTag                 : LongInt;
        FConnected           : Boolean;
        FESmtpSupported      : Boolean;
        FRequestType         : TSmtpRequest;
        FRequestDoneFlag     : Boolean;
        FReceiveLen          : Integer;
        FRequestResult       : Integer;
        FStatusCode          : Integer;
        FReceiveBuffer       : array [0..SMTP_RCV_BUF_SIZE - 1] of char;
        FNext                : TSmtpNextProc;
        FWhenConnected       : TSmtpNextProc;
        FFctSet              : TSmtpFctSet;
        FFctPrv              : TSmtpFct;
        FHighLevelResult     : Integer;
        FHighLevelFlag       : Boolean;
        FNextRequest         : TSmtpNextProc;
        FLastResponseSave    : String;
        FStatusCodeSave      : Integer;
        FRestartFlag         : Boolean;
        FOkResponses         : array [0..15] of Integer;
        FDoneAsync           : TSmtpNextProc;
        FWindowHandle        : HWND;
        FItemCount           : LongInt;
        FHdrLines            : TWideStrings;
        FLineNum             : Integer;
        FMoreLines           : Boolean;
        FOwnHeaders          : Boolean ;  { Angus V2.21 }
        FOnDisplay           : TSmtpDisplay;
        FOnCommand           : TSmtpDisplay;
        FOnResponse          : TSmtpDisplay;
        FOnGetData           : TSmtpGetDataEvent;
        FOnHeaderLine        : TSmtpHeaderLineEvent;
        FOnProcessHeader     : TSmtpProcessHeaderEvent;
        FOnRequestDone       : TSmtpRequestDone;
        FOnStateChange       : TNotifyEvent;
        FOnSessionConnected  : TSessionConnected;
        FOnSessionClosed     : TSessionClosed;
        procedure   TriggerDisplay(Msg : String); virtual;
        procedure   TriggerCommand(Msg : String); virtual;
        procedure   TriggerResponse(Msg : String); virtual;
        procedure   TriggerRequestDone(Error: Word); virtual;
        procedure   TriggerStateChange; virtual;
        procedure   TriggerGetData(LineNum  : Integer;
                                   MsgLine  : PChar;
                                   MaxLen   : Integer;
                                   var More : Boolean); virtual;
        procedure   TriggerHeaderLine(Line : PChar; Size : Integer); virtual;
        procedure   TriggerProcessHeader(HdrLines : TWideStrings); virtual;
        procedure   TriggerSessionConnected(Error : Word); virtual;
        procedure   TriggerSessionClosed(Error : Word); virtual;
        procedure   ClearErrorMessage;
        procedure   SetErrorMessage;
        procedure   StateChange(NewState : TSmtpState);
        procedure   SendCommand(Cmd : String); virtual;
        procedure   SetRcptName(newValue : TWideStrings);
	procedure   SetMailMessage(newValue : TWideStrings);
        procedure   InitUUEncode(var hFile: File; sFile: string); virtual;
        procedure   DoUUEncode(var hFile: File; var sLine: WideString; var More: boolean); virtual;
        procedure   EndUUEncode(var hFile: File); virtual;
        procedure   CheckReady;
        procedure   WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure   WSocketSessionConnected(Sender: TObject; Error: Word);
        procedure   WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure   WSocketDataSent(Sender : TObject; Error : Word);
        procedure   WSocketSessionClosed(Sender : TObject; Error : WORD);
        procedure   DisplayLastResponse;
        procedure   DoHighLevelAsync;
        procedure   ExecAsync(RqType      : TSmtpRequest;
                              Cmd         : String;
                              OkResponses : array of Word;
                              DoneAsync   : TSmtpNextProc);
        procedure   NextExecAsync;
        procedure   EhloNext;
        procedure   AuthNextLogin;
        procedure   AuthNextLoginNext;
        procedure   AuthNextCramMD5;
        procedure   RcptToNext;
        procedure   RcptToDone;
        procedure   DataNext;
        procedure   WndProc(var MsgRec: TMessage); virtual;
        procedure   WMSmtpRequestDone(var msg: TMessage);
                        message WM_SMTP_REQUEST_DONE;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;
        procedure   Connect;  virtual;    { Connect to the mail server }
        procedure   Helo;     virtual;    { Send the HELO command      }
        procedure   Ehlo;     virtual;    { Send the EHLO command      }
        procedure   Auth;     virtual;    { Send the AUTH command      }
        procedure   Vrfy;     virtual;    { Send the VRFY command      }
        procedure   MailFrom; virtual;    { Send the MAILFROM command  }
        procedure   RcptTo;   virtual;    { Send RECPTTO command       }
        procedure   Data;     virtual;    { Send DATA command          }
        procedure   Quit;     virtual;    { Send QUITE command, close  }
        procedure   Rset;     virtual;    { Send RSET command          }
        procedure   Abort;    virtual;    { Abort opertaion, close     }
        procedure   Open;     virtual;    { Connect, Helo/Ehlo, Auth   }
        procedure   Mail;     virtual;    { MailFrom, RcptTo, Data     }

        property    CtrlSocket : TWSocket            read  FWSocket;
        property    Handle     : HWND                read  FWindowHandle;
        property    Connected  : Boolean             read  FConnected;
        procedure   HighLevelAsync(RqType : TSmtpRequest; Fcts : TSmtpFctSet);
        procedure   SetContentType(newValue : TSmtpContentType);
    protected
        property Host : String                       read  FHost
                                                     write FHost;
        property LocalAddr : String                  read  FLocalAddr  {bb}
                                                     write FLocalAddr; {bb}                                                     
        property Port : String                       read  FPort
                                                     write FPort;
        property SignOn : String                     read  FSignOn
                                                     write FSignOn;
        property Username : String                   read  FUsername
                                                     write FUsername;
        property Password : String                   read  FPassword
                                                     write FPassword;
        property AuthType : TSmtpAuthType            read  FAuthType
                                                     write FAuthType;
        property FromName : String                   read  FFromName
                                                     write FFromName;
        property RcptName : TWideStrings             read  FRcptName
                                                     write SetRcptName;
	property MailMessage : TWideStrings 	     read  FMailMessage
						     write SetMailMessage;
        property HdrFrom : String                    read  FHdrFrom
                                                     write FHdrFrom;
        property HdrTo : String                      read  FHdrTo
                                                     write FHdrTo;
        property HdrReplyTo : String                 read  FHdrReplyTo
                                                     write FHdrReplyTo;
        property HdrReturnPath : String              read  FHdrReturnPath
                                                     write FHdrReturnPath;
        property HdrSubject : String                 read  FHdrSubject
                                                     write FHdrSubject;
        property HdrSender: String                   read  FHdrSender
                                                     write FHdrSender;
        property CharSet      : String               read  FCharSet
                                                     write FCharSet;
        property ContentType  : TSmtpContentType     read  FContentType
                                                     write SetContentType;
        property ErrorMessage : String               read  FErrorMessage;
        property LastResponse : String               read  FLastResponse;
        property State        : TSmtpState           read  FState;
        property Tag          : LongInt              read  FTag
                                                     write FTag;
        property OwnHeaders   : Boolean              read  FOwnHeaders
                 { Angus V2.21 }                     write FOwnHeaders;
        property OnDisplay : TSmtpDisplay            read  FOnDisplay
                                                     write FOnDisplay;
        property OnCommand: TSmtpDisplay             read  FOnCommand
                                                     write FOnCommand;
        property OnResponse: TSmtpDisplay            read  FOnResponse
                                                     write FOnResponse;
        property OnGetData : TSmtpGetDataEvent       read  FOnGetData
                                                     write FOnGetData;
        property OnHeaderLine : TSmtpHeaderLineEvent read  FOnHeaderLine
                                                     write FOnHeaderLine;
        property OnProcessHeader  : TSmtpProcessHeaderEvent
                                                     read  FOnProcessHeader
                                                     write FOnProcessHeader;
        property OnRequestDone : TSmtpRequestDone    read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnStateChange : TNotifyEvent        read  FOnStateChange
                                                     write FOnStateChange;
        property OnSessionConnected : TSessionConnected
                                                     read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed
                                                     read  FOnSessionClosed
                                                     write FOnSessionClosed;
    end;

    { Descending component adding MIME (file attach) support }
    TSmtpCli = class(TCustomSmtpClient)
    protected
        FEmailBody    : TWideStrings; { Message body text         }
        FEmailFiles   : TWideStrings; { File names for attachment }
        FCurrentFile  : Integer;  { Current file being sent   }
        FMimeBoundary : String;   { Message parts boundary    }
        FFile         : File;
        FFileStarted  : Boolean;
        FBodyFlag     : Boolean;
        FBodyLine     : Integer;
        FOnAttachContentType : TSmtpAttachmentContentType;
        FOnAttachHeader      : TSmtpAttachHeader;
        procedure   TriggerAttachContentType(FileNumber      : Integer;
                                             var FileName    : WideString;
                                             var ContentType : WideString); virtual;
        procedure   TriggerAttachHeader(FileNumber : Integer;
                                        FileName   : WideString;
                                        HdrLines   : TWideStrings); virtual;
        procedure   TriggerGetData(LineNum  : Integer;
                                   MsgLine  : PChar;
                                   MaxLen   : Integer;
                                   var More : Boolean); override;
        procedure   TriggerHeaderLine(Line : PChar; Size : Integer); override;
        procedure   SetEMailFiles(newValue : TWideStrings);
        procedure   PrepareEMail;
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;
        procedure   Data;                        override;
    published
        property Host;
        property LocalAddr; {bb}        
        property Port;
        property SignOn;
        property Username;
        property Password;
        property AuthType;
        property FromName;
        property RcptName;
        property MailMessage;
        property HdrFrom;
        property HdrTo;
        property HdrReplyTo;
        property HdrReturnPath;
        property HdrSubject;
        property HdrSender;
	property State;
        property CharSet;
        property ContentType;
        property ErrorMessage;
        property LastResponse;
        property Tag;
        property OwnHeaders ;             { Angus V2.21 }
        property OnDisplay;
        property OnCommand;
        property OnResponse;
        property OnGetData;
        property OnHeaderLine;
        property OnProcessHeader;
        property OnRequestDone;
        property OnSessionConnected;
        property OnSessionClosed;
        property EmailFiles : TWideStrings           read  FEmailFiles
                                                     write SetEmailFiles;
        property OnAttachContentType : TSmtpAttachmentContentType
                                                     read  FOnAttachContentType
                                                     write FOnAttachContentType;
        property OnAttachHeader  : TSmtpAttachHeader read  FOnAttachHeader
                                                     write FOnAttachHeader;
    end;

    { TSyncSmtpCli add synchronous functions. You should avoid using this   }
    { component because synchronous function, apart from being easy, result }
    { in lower performance programs.                                        }
    TSyncSmtpCli = class(TSmtpCli)
    protected
        FTimeout       : Integer;                 { Given in seconds }
        FTimeStop      : LongInt;                 { Milli-seconds    }
        FMultiThreaded : Boolean;
        function WaitUntilReady : Boolean; virtual;
        function Synchronize(Proc : TSmtpNextProc) : Boolean;
        procedure TriggerGetData(LineNum  : Integer;
                                 MsgLine  : PChar;
                                 MaxLen   : Integer;
                                 var More : Boolean); override;
    public
        constructor Create(AOwner : TComponent); override;
        function    ConnectSync  : Boolean; virtual;
        function    HeloSync     : Boolean; virtual;
        function    EhloSync     : Boolean; virtual;
        function    AuthSync     : Boolean; virtual;
        function    VrfySync     : Boolean; virtual;
        function    MailFromSync : Boolean; virtual;
        function    RcptToSync   : Boolean; virtual;
        function    DataSync     : Boolean; virtual;
        function    QuitSync     : Boolean; virtual;
        function    RsetSync     : Boolean; virtual;
        function    AbortSync    : Boolean; virtual;
        function    OpenSync     : Boolean; virtual;
        function    MailSync     : Boolean; virtual;
    published
        property Timeout : Integer       read  FTimeout
                                         write FTimeout;
        property MultiThreaded : Boolean read  FMultiThreaded
                                         write FMultiThreaded;
    end;

{ Function to convert a TDateTime to an RFC822 timestamp string }
function Rfc822DateTime(t : TDateTime) : String;

procedure Register;

implementation

{$B-} { Partial boolean evaluation }

type
  TLookup = array [0..64] of Char;
  TLookup2 = array[0..127] of Byte;

const
  Base64Out: TLookup =
    (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
   );
  Base64In: TLookup2 =
    (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
     56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
     13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
     33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
     46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
  );


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RTrim(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LTrim(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := LTrim(Rtrim(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Encode(Input : String) : String;
var
    Final : String;
    Count : Integer;
    Len   : Integer;
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                       ((Byte(Input[Count+1]) and $F0) shr 4)];
            if (Count+2) <= Len then begin
                Final := Final + Base64Out[((Byte(Input[Count+1]) and $0F) shl 2) +
                                           ((Byte(Input[Count+2]) and $C0) shr 6)];
                Final := Final + Base64Out[(Byte(Input[Count+2]) and $3F)];
            end
            else begin
                Final := Final + Base64Out[(Byte(Input[Count+1]) and $0F) shl 2];
                Final := Final + '=';
            end
        end
        else begin
            Final := Final + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Final := Final + '==';
        end;
        Count := Count + 3;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(Input : String) : String;
var
    Final   : String;
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Final := '';
    Count := 1;
    Len := Length(Input);
    while Count <= Len do begin
        DataIn0 := Base64In[Byte(Input[Count])];
        DataIn1 := Base64In[Byte(Input[Count+1])];
        DataIn2 := Base64In[Byte(Input[Count+2])];
        DataIn3 := Base64In[Byte(Input[Count+3])];

        Final := Final + Char(((DataIn0 and $3F) shl 2) +
                              ((DataIn1 and $30) shr 4));
        if DataIn2 <> $40 then begin
            Final := Final + Char(((DataIn1 and $0F) shl 4) +
                                  ((DataIn2 and $3C) shr 2));
            if DataIn3 <> $40 then
                Final := Final + Char(((DataIn2 and $03) shl 6) +
                                      (DataIn3 and $3F));
        end;
        Count := Count + 4;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$I+}   { Activate I/O check (EInOutError exception generated) }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.InitUUEncode(var hFile: File; sFile: string);
var
    OldFileMode : Byte;
begin
    AssignFile(hFile, sFile);
    OldFileMode := FileMode;
    FileMode := 0; { Force readonly }
    try
        Reset(hFile, 1);
    finally
        FileMode := OldFileMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DoUUEncode(var hFile: File; var sLine: WideString; var More: boolean);
var
    Count     : integer;
    DataIn    : array [0..2] of byte;
    DataOut   : array [0..80] of byte;
    ByteCount : integer;
    i         : integer;
begin
    Count := 0;
{$I-}
    while not Eof(hFile) do begin
{$I+}
        BlockRead(hFile, DataIn, 3, ByteCount);
        DataOut[Count]     := (DataIn[0] and $FC) shr 2;
        DataOut[Count + 1] := (DataIn[0] and $03) shl 4;
        if ByteCount > 1 then begin
            DataOut[Count + 1] := DataOut[Count + 1] +
                                  (DataIn[1] and $F0) shr 4;
            DataOut[Count + 2] := (DataIn[1] and $0F) shl 2;
            if ByteCount > 2 then begin
                DataOut[Count + 2] := DataOut[Count + 2] +
                                      (DataIn[2] and $C0) shr 6;
                DataOut[Count + 3] := (DataIn[2] and $3F);
            end
            else begin
                DataOut[Count + 3] := $40;
            end;
        end
        else begin
            DataOut[Count + 2] := $40;
            DataOut[Count + 3] := $40;
        end;

        for i := 0 to 3 do
            DataOut[Count + i] := Byte(Base64Out[DataOut[Count + i]]);

        Count := Count + 4;

        if Count > 59 then
            break;
    end;

    DataOut[Count] := $0;
    sLine := StrPas(@DataOut[0]);

{$I-}
    More := not Eof(hFile);
{$I+}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.EndUUEncode(var hFile: File);
begin
    CloseFile(hFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomSmtpClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle            := WSocket.AllocateHWnd(WndProc);
    FWSocket                 := TWSocket.Create(nil);
    FWSocket.OnSessionClosed := WSocketSessionClosed;
    FState                   := smtpReady;
    FRcptName                := TWideStringList.Create;
    FMailMessage	     := TWideStringList.Create;
    FPort                    := 'smtp';
    FCharSet                 := 'iso-8859-1';
    FAuthType                := smtpAuthNone;
    FLocalAddr               := '0.0.0.0'; {bb}    
    SetContentType(smtpPlainText);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomSmtpClient.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    if Assigned(FHdrLines) then begin
        FHdrLines.Destroy;
        FHdrLines := nil;
    end;

    FMailMessage.Destroy;
    FRcptName.Destroy;

    WSocket.DeallocateHWnd(FWindowHandle);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WndProc(var MsgRec: TMessage);
begin
     with MsgRec do begin
         case Msg of
         WM_SMTP_REQUEST_DONE : WMSmtpRequestDone(MsgRec);
         else
             Result := DefWindowProc(Handle, Msg, wParam, lParam);
         end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WMSmtpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(Data : PChar; var Number : Integer) : PChar;
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data);

    if (Result = nil) then
        Exit;

    { Remember the sign }
    if Result^ in ['-', '+'] then begin
        bSign := (Result^ = '-');
        Inc(Result);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (Result^ <> #0) and (Result^ in ['0'..'9']) do begin
        Number := Number * 10 + ord(Result^) - ord('0');
        Inc(Result);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.CheckReady;
begin
    if not (FState in [smtpReady, smtpInternalReady]) then
        raise SmtpException.Create('SMTP component not ready');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerSessionConnected(Error : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerRequestDone(Error: Word);
begin
    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;

{ --Jake Traynham, 06/12/01  Bug - removed "(Error = 0) and" because we  }
{                            want DoHighLevelAsync to handle any errors  }
{                            we get while doing a High Level function:   }
{ *bug* if (Error = 0) and Assigned(FNextRequest) then begin             }
        if Assigned(FNextRequest) then begin
            if FState <> smtpAbort then
                StateChange(smtpInternalReady);
            FNextRequest;
        end
        else begin
            StateChange(smtpReady);
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            PostMessage(Handle, WM_SMTP_REQUEST_DONE, 0, Error);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.StateChange(NewState : TSmtpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DisplayLastResponse;
begin
     TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
    p   : PChar;
begin
    Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen],
                            sizeof(FReceiveBuffer) - FReceiveLen);

    if Len <= 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := #0;
    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
        I := Pos(#13#10, FReceiveBuffer);
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;

        FLastResponse := Copy(FReceiveBuffer, 1, I - 1);
        TriggerResponse(FLastResponse);

{$IFDEF DUMP}
        FDumpBuf := '>|';
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
        FDumpBuf := '|' + #13#10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
{$IFDEF VER80}
        { Add a nul byte at the end of string for Delphi 1 }
        FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
        FReceiveLen := FReceiveLen - I - 1;
        if FReceiveLen > 0 then
            Move(FReceiveBuffer[I + 1], FReceiveBuffer[0], FReceiveLen + 1);

        if FState = smtpWaitingBanner then begin
            DisplayLastResponse;
            p := GetInteger(@FLastResponse[1], FStatusCode);
            if p^ = '-' then
                Continue;  { Continuation line, ignore }
            if FStatusCode <> 220 then begin
                SetErrorMessage;
                FRequestResult := FStatusCode;
                FWSocket.Close;
                Exit;
            end;

            StateChange(smtpConnected);
            TriggerSessionConnected(Error);

            if Assigned(FWhenConnected) then
                FWhenConnected
            else begin
                TriggerRequestDone(0);
            end;
        end
        else if FState = smtpWaitingResponse then begin
            if Assigned(FNext) then
                FNext
            else
                raise SmtpException.Create('Program error: FNext is nil');
        end
        else begin
            { Unexpected data received }
            DisplayLastResponse;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if Error <> 0 then begin
        FLastResponse := '500 ' + WSocketErrorDesc(Error) +
                         ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode   := 500;
        FConnected    := FALSE;
{ --Jake Traynham, 06/12/01  Bug - Need to set FRequestResult so High    }
{                            Level Open will exit out. (See also         }
{                            TriggerRequestDone bug.)                    }
        FRequestResult:= 500;
        SetErrorMessage;
        TriggerRequestDone(Error);
        FWSocket.Close;
        StateChange(smtpReady);
    end
    else begin
        FConnected := TRUE;
        StateChange(smtpWaitingBanner);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        FLastResponse := '500 ' + WSocketErrorDesc(Error) +
                         ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode   := 500;
        SetErrorMessage;
        TriggerRequestDone(Error);
    end
    else begin
        FWSocket.Addr               := FWSocket.DnsResult;
        FWSocket.LocalAddr          := FLocalAddr; {bb}
        FWSocket.Proto              := 'tcp';
        FWSocket.Port               := FPort;
        FWSocket.OnSessionConnected := WSocketSessionConnected;
        FWSocket.OnDataAvailable    := WSocketDataAvailable;
        StateChange(smtpConnecting);
        try
            FWSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse  := '500 ' + E.ClassName + ': ' + E.Message;
                FStatusCode    := 500;
                FRequestResult := FStatusCode;
                SetErrorMessage;
                TriggerRequestDone(FStatusCode);
            end;
        end
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SendCommand(Cmd : String);
begin
    TriggerCommand(Cmd);
    TriggerDisplay('> ' + Cmd);
    if FWSocket.State = wsConnected then
        FWSocket.SendStr(Cmd + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.ExecAsync(
    RqType      : TSmtpRequest;
    Cmd         : String;         { Command to execute                      }
    OkResponses : array of Word;  { List of responses like '200 221 342'    }
    DoneAsync   : TSmtpNextProc); { What to do when done                    }
var
    I : Integer;
begin
    CheckReady;

    if not FConnected then
        raise SmtpException.Create('SMTP component not connected');

    if not FHighLevelFlag then
        FRequestType := RqType;

    for I := 0 to High(OkResponses) do
        FOkResponses[I] := OkResponses[I];
    FOkResponses[High(OkResponses) + 1] := 0;

    FRequestDoneFlag  := FALSE;
    FNext             := NextExecAsync;
    FDoneAsync        := DoneAsync;
    StateChange(smtpWaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.NextExecAsync;
var
    I : Integer;
    p : PChar;
begin
    DisplayLastResponse;
    p := GetInteger(@FLastResponse[1], FStatusCode);
    if p^ = '-' then
        Exit; { Continuation line, nothing to do }

    if FOkResponses[0] = 0 then begin
        { The list of ok responses is empty }
        if FStatusCode >= 500 then begin
            { Not a good response }
            FRequestResult := FStatusCode;
            SetErrorMessage;
        end
        else
            FRequestResult := 0;
    end
    else begin
        { We have a list of ok response codes }
        for I := 0 to High(FOkResponses) do begin
            if FOkResponses[I] = 0 then begin
                { No good response found }
                FRequestResult := FStatusCode;
                SetErrorMessage;
                break;
            end;
            if FOkResponses[I] = FStatusCode then begin
                { Good response found }
                FRequestResult := 0;
                Break;
            end;
        end;
    end;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Helo;
begin
    FFctPrv := smtpFctHelo;
    if FSignOn = '' then
        ExecAsync(smtpHelo, 'HELO ' + LocalHostName, [250], nil)
    else
        ExecAsync(smtpHelo, 'HELO ' + FSignOn, [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Ehlo;
begin
    FFctPrv := smtpFctEhlo;
    if FSignOn = '' then
        ExecAsync(smtpEhlo, 'EHLO ' + LocalHostName, [250], EhloNext)
    else
        ExecAsync(smtpEhlo, 'EHLO ' + FSignOn, [250], EhloNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.EhloNext;
begin
    { It's possible that some really old mail servers will disconnect you }
    { if you use the 'EHLO' command.  If we've been disconnected, then do }
    { nothing. RequestDone event handler is called from socket            }
    { SessionClose event.                                                 }
    if not FConnected
      then Exit;

    if (FRequestResult = 0)
      then FESmtpSupported := TRUE;

    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Auth;
var
  AuthPlain : String;
begin
    { If ESMTP is not supported, or if we didn't start with EHLO, then we }
    { can't use the AUTH command.                                         }
    if (not FESmtpSupported)
      then begin
        FLastResponse := '500 ESMTP not supported.';
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
      end;

    FFctPrv := smtpFctAuth;
    case FAuthType of
      smtpAuthNone :
        begin
          { shouldn't happen }
          FLastResponse := '500 No Authorization Type Selected.';
          SetErrorMessage;
          TriggerRequestDone(500);
          Exit;
        end;
      smtpAuthPlain :
        begin
           AuthPlain := FUsername;
           AuthPlain := AuthPlain + #0;
           if (FFromName <> '') {FromName should be set before calling Auth}
             then
               AuthPlain := AuthPlain + FFromname
             else
               AuthPlain := AuthPlain + FUsername;
           AuthPlain := AuthPlain + #0;
           AuthPlain := AuthPlain + FPassword;
           AuthPlain := Base64Encode(AuthPlain);
           ExecAsync(smtpAuth, 'AUTH PLAIN ' + AuthPlain, [235], nil);
        end;
      smtpAuthLogin :
        begin
          ExecAsync(smtpAuth, 'AUTH LOGIN', [334], AuthNextLogin);
        end;
      smtpAuthCramMD5 :
        begin
          ExecAsync(smtpAuth, 'AUTH CRAM-MD5', [334], AuthNextCramMD5);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextLogin;
begin
    { If there was an error, tell the user and exit.                      }
    if (FRequestResult <> 0)
      then begin
        TriggerRequestDone(FRequestResult);
        Exit;
      end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(FUsername), [334], AuthNextLoginNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextLoginNext;
begin
    { If there was an error, tell the user and exit.                      }
    if (FRequestResult <> 0)
      then begin
        TriggerRequestDone(FRequestResult);
        Exit;
      end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(FPassword), [235], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.AuthNextCramMD5;
var
  Challenge  : String;
  Response   : String;
  HexDigits  : String;
  MD5Digest  : TMD5Digest;
  MD5Context : TMD5Context;
  Count      : Integer;
  IPAD       : Array[0..63] of Byte;
  OPAD       : Array[0..63] of Byte;
begin
    { If there was an error, tell the user and exit.                      }
    if (FRequestResult <> 0)
      then begin
        TriggerRequestDone(FRequestResult);
        Exit;
      end;

    { Server should be returning something like                           }
    {   334 PDEyMzc5MTU3NTAtNjMwNTcxMzRAZm9vLmJhci5jb20+                  }
    { If it does not, then exit.                                          }
    if (Length(FLastResponse) < 5)
      then begin
        FLastResponse := '500 Malformed MD5 Challege: ' + FLastResponse;
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
      end;

    Challenge := Copy(FLastResponse, 5, Length(FLastResponse) - 4);

    Challenge := Base64Decode(Challenge);

    {See RFC2104 }
    for Count := 0 to 63 do
      begin
        if ((Count+1) <= Length(FPassword))
          then begin
            IPAD[Count] := Byte(FPassword[Count+1]) xor $36;
            OPAD[Count] := Byte(FPassword[Count+1]) xor $5C;
          end
          else begin
            IPAD[Count] := 0 xor $36;
            OPAD[Count] := 0 xor $5C;
          end;
      end;

    MD5Init(MD5Context);
    MD5Update(MD5Context, IPAD, 64);
    MD5UpdateBuffer(MD5Context, @Challenge[1], Length(Challenge));
    MD5Final(MD5Digest, MD5Context);

    MD5Init(MD5Context);
    MD5Update(MD5Context, OPAD, 64);
    MD5Update(MD5Context, MD5Digest, 16);
    MD5Final(MD5Digest, MD5Context);

    HexDigits := '0123456789abcdef';
    Response := FUsername;
    Response := Response + ' ';
    for Count := 0 to 15 do
      begin
        Response := Response + HexDigits[((Byte(MD5Digest[Count]) and $F0) shr 4)+1];
        Response := Response + HexDigits[(Byte(MD5Digest[Count]) and $0F)+1];
      end;

    FState := smtpInternalReady;
    ExecAsync(smtpAuth, Base64Encode(Response), [235], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Vrfy;
begin
    FFctPrv := smtpFctVrfy;
    ExecAsync(smtpVrfy, 'VRFY ' + FHdrTo, [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.MailFrom;
begin
    FFctPrv := smtpFctMailFrom;
    if (Pos('<', FFromName) <> 0) and (Pos('>', FFromName) <> 0) then
        ExecAsync(smtpMailFrom, 'MAIL FROM:' + Trim(FFromName), [250], nil)
    else
        ExecAsync(smtpMailFrom,
                  'MAIL FROM:<' + Trim(FFromName) + '>', [250], nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Rset;
begin
    FFctPrv := smtpFctRset;
    ExecAsync(smtpRset, 'RSET', [250], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptTo;
begin
    if FRcptName.Count <= 0 then
        raise SmtpException.Create('RcptName list is empty');

    FItemCount := -1;
    RcptToNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptToNext;
var
    WhenDone : TSmtpNextProc;
begin
    Inc(FItemCount);
    if FItemCount >= (FRcptName.Count - 1) then
        WhenDone := nil
    else
        WhenDone := RcptToDone;
    FFctPrv    := smtpFctRcptTo;
    if (Pos('<', FRcptName.Strings[FItemCount]) <> 0) and
       (Pos('>', FRcptName.Strings[FItemCount]) <> 0) then
        ExecAsync(smtpRcptTo,
                  'RCPT TO:' + Trim(FRcptName.Strings[FItemCount]),
                  [250, 251], WhenDone)
    else
        ExecAsync(smtpRcptTo,
                  'RCPT TO:<' + Trim(FRcptName.Strings[FItemCount])+ '>',
                  [250, 251], WhenDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.RcptToDone;
begin
    FState := smtpInternalReady;
    RcptToNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetContentType(newValue : TSmtpContentType);
begin
    if FContentType = newValue then
        Exit;
    FContentType := newValue;
    if FContentType = smtpPlainText then
        FContentTypeStr := 'text/plain'
    else
        FContentTypeStr := 'text/html';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Data;
begin
    FLineNum   := 0;
    FMoreLines := TRUE;
    FItemCount := -1;
    if not Assigned(FHdrLines) then
        FHdrLines := TWideStringList.Create
    else
        FHdrLines.Clear;
    if not FOwnHeaders then begin
        { Angus V2.21 - the body must contain all the headers }
        if Length(Trim(FHdrReplyTo)) > 0 then
            FHdrLines.Add('Reply-To: '    + FHdrReplyTo);
        if Length(Trim(FHdrReturnPath)) > 0 then
            FHdrLines.Add('Return-Path: '    + FHdrReturnPath);
        FHdrLines.Add('From: '    + FHdrFrom);
        FHdrLines.Add('To: '      + FHdrTo);
        FHdrLines.Add('Subject: ' + FHdrSubject);
        if Length(Trim(FHdrSender)) > 0 then
            FHdrLines.Add('Sender: ' + FHdrSender)
        else if Length(Trim(FHdrFrom)) > 0 then
            FHdrLines.Add('Sender: ' + FHdrFrom);
        FHdrLines.Add('Mime-Version: 1.0');
        FHdrLines.Add('Content-Type: ' + FContentTypeStr + '; charset="' + FCharSet + '"');
        FHdrLines.Add('Date: ' + Rfc822DateTime(Now));
        TriggerProcessHeader(FHdrLines);
        { An empty line mark the header's end }
        FHdrLines.Add('');
    end
    else
        FItemCount := 0;
    FFctPrv := smtpFctData;
    ExecAsync(smtpData, 'DATA', [354], DataNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DataNext;
var
    MsgLine  : array [0..1023] of char;
begin
    { If we have been disconnected, then do nothing.                      }
    { RequestDone event handler is called from socket SessionClose event. }
    if not FConnected then begin
        FWSocket.OnDataSent := nil;
        Exit;
    end;

    Inc(FItemCount);
    if FItemCount < FHdrLines.Count then begin
        { There are still header lines to send }
        StrPCopy(@MsgLine, FHdrLines.Strings[FItemCount]);
        TriggerHeaderLine(@MsgLine, SizeOf(MsgLine));
        TriggerDisplay('> ' + StrPas(MsgLine));
        FWSocket.OnDataSent := WSocketDataSent;
        FWSocket.PutDataInSendBuffer(@MsgLine, strlen(MsgLine));
        FWSocket.SendStr(#13+#10);
    end
    else begin
        { Now we need to send data lines }
        if FMoreLines then begin
            try
                Inc(FLineNum);
                TriggerGetData(FLineNum, @MsgLine, High(MsgLine), FMoreLines);
            except
                FMoreLines := FALSE;
            end;
        end;

        if FMoreLines then begin
            if MsgLine[0] = '.' then
                Move(MsgLine[0], MsgLine[1], StrLen(MsgLine) + 1);
            TriggerDisplay('> ' + StrPas(MsgLine));
            FWSocket.OnDataSent := WSocketDataSent;
            FWSocket.PutDataInSendBuffer(@MsgLine, StrLen(MsgLine));
            FWSocket.SendStr(#13 + #10);
        end
        else begin
            { Send the last message line }
            FWSocket.OnDataSent := nil;
            ExecAsync(smtpData, '.', [250], nil);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketDataSent(Sender : TObject; Error : Word);
begin
    FState := smtpInternalReady;
    DataNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Abort;
begin
    StateChange(smtpAbort);
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
    StateChange(smtpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Connect;
begin
    CheckReady;
    if FConnected then
        raise SmtpException.Create('SMTP component already connected');

    if not FHighLevelFlag then
        FRequestType  := smtpConnect;   { 10/05/99 }
    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequestResult    := 0;
    FESmtpSupported   := FALSE;
    StateChange(smtpDnsLookup);
    FWSocket.OnDataSent      := nil;
    FWSocket.OnDnsLookupDone := WSocketDnsLookupDone;
    FWSocket.DnsLookup(FHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Quit;
begin
    CheckReady;
    FFctPrv := smtpFctQuit;
    if not FConnected then begin
        { We are not connected, it's ok... }
        FRequestType     := smtpQuit;
        FRequestDoneFlag := FALSE;
        TriggerRequestDone(0);
        Exit;
    end;
    ExecAsync(smtpQuit, 'QUIT', [221], nil); { Should I force a FWSocket.Close }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.DoHighLevelAsync;
begin
{$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = smtpAbort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        { EHLO wasn't supported, so just log in with HELO }
        if FFctPrv = smtpFctEhlo then
            FFctSet := [smtpFctHelo]
        else begin
            FHighLevelResult := FRequestResult;
            if (FFctPrv = smtpFctQuit) or (not (smtpFctQuit in FFctSet)) then
                FFctSet := []
            else
                FFctSet := [smtpFctQuit];
        end;
    end;

    if smtpFctConnect in FFctSet then begin
        FFctPrv := smtpFctConnect;
        FFctSet := FFctSet - [FFctPrv];
        Connect;
        Exit;
    end;

    if smtpFctHelo in FFctSet then begin
        FFctPrv := smtpFctHelo;
        FFctSet := FFctSet - [FFctPrv];
        Helo;
        Exit;
    end;

    if smtpFctEhlo in FFctSet then begin
        FFctPrv := smtpFctEhlo;
        FFctSet := FFctSet - [FFctPrv];
        Ehlo;
        Exit;
    end;

    if smtpFctAuth in FFctSet then begin
        FFctPrv := smtpFctAuth;
        FFctSet := FFctSet - [FFctPrv];
        Auth;
        Exit;
    end;

    if smtpFctVrfy in FFctSet then begin
        FFctPrv := smtpFctVrfy;
        FFctSet := FFctSet - [FFctPrv];
        Vrfy;
        Exit;
    end;

    if smtpFctMailFrom in FFctSet then begin
        FFctPrv := smtpFctMailFrom;
        FFctSet := FFctSet - [FFctPrv];
        MailFrom;
        Exit;
    end;

    if smtpFctRcptTo in FFctSet then begin
        FFctPrv := smtpFctRcptTo;
        FFctSet := FFctSet - [FFctPrv];
        RcptTo;
        Exit;
    end;

    if smtpFctData in FFctSet then begin
        FFctPrv := smtpFctData;
        FFctSet := FFctSet - [FFctPrv];
        Data;
        Exit;
    end;

    if smtpFctQuit in FFctSet then begin
        FFctPrv := smtpFctQuit;
        FFctSet := FFctSet - [FFctPrv];
        Quit;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.HighLevelAsync(
    RqType : TSmtpRequest; Fcts : TSmtpFctSet);
begin
    if FConnected and (smtpFctConnect in Fcts) then
        raise SmtpException.Create('SMTP component already connected');
    CheckReady;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := smtpFctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FErrorMessage     := '';
    FRestartFlag      := FALSE;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Open;
begin
    if FAuthType <> smtpAuthNone then
        HighLevelAsync(smtpOpen, [smtpFctConnect, smtpFctEhlo, smtpFctAuth])
    else
        HighLevelAsync(smtpOpen, [smtpFctConnect, smtpFctHelo]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.Mail;
begin
    HighLevelAsync(smtpMail, [smtpFctMailFrom, smtpFctRcptTo, smtpFctData]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.WSocketSessionClosed(Sender : TObject; Error : WORD);
begin
    FConnected := FALSE;
    TriggerSessionClosed(Error);
    TriggerRequestDone(WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerHeaderLine(Line : PChar; Size : Integer);
begin
    if Assigned(FOnHeaderLine) then
        FOnHeaderLine(Self, Line, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerGetData(
    LineNum: Integer;
    MsgLine: PChar;
    MaxLen: Integer;
    var More: Boolean);
begin
    if not Assigned(FOnGetData) then
        More := FALSE
    else
        FOnGetData(Self, LineNum, MsgLine, MaxLen, More);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetRcptName(newValue : TWideStrings);
var
    I : Integer;
begin
    FRcptName.Clear;
    for I := 0 to newValue.Count - 1 do
        FRcptName.Add(newValue.Strings[I]);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetMailMessage(newValue : TWideStrings);
var
    I : Integer;
begin
    FMailMessage.Clear;
    for I := 0 to newValue.Count - 1 do
        FMailMessage.Add(newValue.Strings[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TimeZoneBias : String;
{$IFDEF VER80}  { Delphi 1 doesn't support timezone API }
begin
    Result := '-0000';
end;
{$ELSE}
const
    Time_Zone_ID_DayLight = 2;
var
    TZI       : tTimeZoneInformation;
    TZIResult : Integer;
    aBias     : Integer;
begin
    TZIResult := GetTimeZoneInformation(TZI);
    if TZIResult = -1 then
        Result := '-0000'
    else begin
         if TZIResult = Time_Zone_ID_DayLight then   { 10/05/99 }
             aBias := TZI.Bias + TZI.DayLightBias
         else
             aBias := TZI.Bias + TZI.StandardBias;
         Result := Format('-%.2d%.2d', [Abs(aBias) div 60, Abs(aBias) mod 60]);
         if aBias < 0 then
             Result[1] := '+';
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Rfc822DateTime(t : TDateTime) : String;
var
    I                   : Integer;
    SaveShortDayNames   : array[1..7] of string;
    SaveShortMonthNames : array[1..12] of string;
const
    MyShortDayNames: array[1..7] of string =
        ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
    MyShortMonthNames: array[1..12] of string =
        ('Jan', 'Feb', 'Mar', 'Apr',
         'May', 'Jun', 'Jul', 'Aug',
         'Sep', 'Oct', 'Nov', 'Dec');
begin
    if ShortDayNames[1] = MyShortDayNames[1] then
        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias
    else begin
        { We used a localized Delphi version, the day and month names are no }
        { more english names ! We need to save and replace them              }
        for I := Low(ShortDayNames) to High(ShortDayNames) do begin
            SaveShortDayNames[I] := ShortDayNames[I];
            ShortDayNames[I]     := MyShortDayNames[I];
        end;

        for I := Low(ShortMonthNames) to High(ShortMonthNames) do begin
            SaveShortMonthNames[I] := ShortMonthNames[I];
            ShortMonthNames[I]     := MyShortMonthNames[I];
        end;

        Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t) +
                  ' ' + TimeZoneBias;

        for I := Low(ShortDayNames) to High(ShortDayNames) do
            ShortDayNames[I] := SaveShortDayNames[I];
        for I := Low(ShortMonthNames) to High(ShortMonthNames) do
            ShortMonthNames[I] := SaveShortMonthNames[I];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerProcessHeader(HdrLines : TWideStrings);
begin
    if Assigned(FOnProcessHeader) then
        FOnProcessHeader(Self, HdrLines);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerCommand(Msg : String);
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.TriggerResponse(Msg : String);
begin
    if Assigned(FOnResponse) then
        FOnResponse(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.ClearErrorMessage;
begin
    FErrorMessage := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSmtpClient.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSmtpCli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FEmailBody  := TWideStringList.Create;
    FEmailFiles := TWideStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSmtpCli.Destroy;
begin
    if Assigned(FEmailBody) then begin
        FEMailBody.Destroy;
        FEMailBody := nil;
    end;
    if Assigned(FEmailFiles) then begin
        FEmailFiles.Destroy;
        FEmailFiles := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerAttachContentType(
    FileNumber      : Integer;
    var FileName    : WideString;
    var ContentType : WideString);
begin
    if Assigned(FOnAttachContentType) then
        FOnAttachContentType(Self, FileNumber, FileName, ContentType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerAttachHeader(
    FileNumber : Integer;
    FileName   : WideString;
    HdrLines   : TWideStrings);
begin
    if Assigned(FOnAttachHeader) then
        FOnAttachHeader(Self, FileNumber, FileName, HdrLines);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerGetData(
    LineNum  : Integer;
    MsgLine  : PChar;
    MaxLen   : Integer;
    var More : Boolean);
var
    sLine        : WideString;
    FileName     : WideString;
    sFileName    : WideString;
    sContentType : WideString;
begin
    if FEmailBody.Count > 0 then begin
        if MaxLen > 1023 then
            MaxLen := 1023;  { RFC say 1024 char max, including nul char }
        StrPLCopy(MsgLine, FEmailBody[0], MaxLen);
        FEmailBody.Delete(0);
        More := TRUE;
        Exit;
    end;

    if FBodyFlag then begin
        Inc(FBodyLine);
        inherited TriggerGetData(FBodyLine, MsgLine, MaxLen, More);
        if More then
            Exit;
        FBodyFlag := FALSE;
    end;

    if not FFileStarted then begin
        if (not Assigned(FEMailFiles)) or
           (FEmailFiles.Count <= FCurrentFile) then begin
            { No file to send }
            More := FALSE;
            Exit;
        end;

        StrPCopy(MsgLine, '');
        FileName := FEmailFiles[FCurrentFile];
        InitUUEncode(FFile, FileName);
        sFileName    := ExtractFileName(FileName);
        sContentType := 'application/octet-stream';
        TriggerAttachContentType(FCurrentFile, sFileName, sContentType);
        FEmailBody.Add('--' + FMimeBoundary);
        FEmailBody.Add('Content-Type: ' + sContentType + ';');
        FEmailBody.Add(#9'name="' + sFileName + '"');
        FEmailBody.Add('Content-Transfer-Encoding: base64');
        FEmailBody.Add('Content-Disposition: attachment;');
        FEmailBody.Add(#9'filename="' + ExtractFileName(FileName) + '"');
        TriggerAttachHeader(FCurrentFile, sFileName, FEmailBody);
        FEmailBody.Add('');
        FFileStarted := TRUE;
        More := TRUE;
        Exit;
    end;

    DoUUEncode(FFile, sLine, More);
    StrPCopy(MsgLine, sLine);
    if not More then begin  { we hit the end of file. }
        EndUUEncode(FFile);
        FFileStarted := FALSE;
        Inc(FCurrentFile);
        if (FEmailFiles.Count <= FCurrentFile) then begin
            FEmailBody.Add('');
            FEmailBody.Add('--' + FMimeBoundary + '--');
        end;
        More := TRUE;
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.TriggerHeaderLine(Line : PChar; Size : Integer);
begin
    { if we have a MIME type message, then replace the content-type }
    { header with the proper MIME content-type.                     }
    if FMimeBoundary <> '' then begin
        if StrLIComp('CONTENT-TYPE:', Line, 13) = 0 then
            StrPCopy(Line, 'Content-Type: multipart/mixed;'#13#10#9'boundary="'
                     + FMimeBoundary + '"');
    end;
    inherited TriggerHeaderLine(Line, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.SetEMailFiles(newValue : TWideStrings);
var
    I        : Integer;
    FilePath : WideString;
begin
    FEMailFiles.Clear;
    if not Assigned(newValue) then
        Exit;
    for I := 0 to newValue.Count - 1 do begin
        FilePath := Trim(newValue.Strings[I]);
        { Ignore any empty file name (a very common error !) }
        if FilePath > '' then begin
            { Check if file exists and raise an exception if not }
            if FileExists(FilePath) then
                FEMailFiles.Add(FilePath)
            else
                raise SmtpException.Create('File not found ''' + FilePath + '''');
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.Data;
begin
    PrepareEMail;
    inherited Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpCli.PrepareEMail;
var i : integer;
begin
    FBodyFlag    := TRUE;
    FCurrentFile := 0;
    FBodyLine    := 0;
    FFileStarted := FALSE;

    FEmailBody.Clear;
    if Assigned(FEMailFiles) and (FEmailFiles.Count > FCurrentFile) then begin
        FMimeBoundary := '= Multipart Boundary '
                              + FormatDateTime('mmddyyhhnn', Now);

        FEmailBody.Add('This is a multipart MIME message.');
        FEmailBody.Add('');
        FEmailBody.Add('--' + FMimeBoundary);
        FEmailBody.Add('Content-Type: ' + FContentTypeStr + '; charset="' + FCharSet + '"');
        FEmailBody.Add('Content-Transfer-Encoding: 7bit');
        FEmailBody.Add('');
    end
    else
        FMimeBoundary := '';

    for i := 0 to FMailMessage.Count - 1 do
      FEmailBody.Add(FMailMessage.Strings[I]);
   
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSyncSmtpCli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.WaitUntilReady : Boolean;
begin
    Result := TRUE;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
    while TRUE do begin
        if FState = smtpReady then begin
            { Back to ready state, the command is finiched }
            Result := (FRequestResult = 0);
            break;
        end;

        if  Application.Terminated or
            ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) then begin
            { Application is terminated or timeout occured }
            inherited Abort;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
{$IFNDEF VER80}
        if FMultiThreaded then
            FWSocket.ProcessMessages
        else
{$ENDIF}
            Application.ProcessMessages;
{$IFNDEF VER80}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.Synchronize(Proc : TSmtpNextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.ConnectSync : Boolean;
begin
    Result := Synchronize(Connect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.HeloSync : Boolean;
begin
    Result := Synchronize(Helo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.EhloSync : Boolean;
begin
    Result := Synchronize(Ehlo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.AuthSync : Boolean;
begin
    Result := Synchronize(Auth);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.VrfySync : Boolean;
begin
    Result := Synchronize(Vrfy);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.OpenSync : Boolean;
begin
    Result := Synchronize(Open);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.MailFromSync : Boolean;
begin
    Result := Synchronize(MailFrom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.RcptToSync : Boolean;
begin
    Result := Synchronize(RcptTo);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.DataSync : Boolean;
begin
    Result := Synchronize(Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.MailSync : Boolean;
begin
    Result := Synchronize(Mail);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.QuitSync : Boolean;
begin
    Result := Synchronize(Quit);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.RsetSync : Boolean;
begin
    Result := Synchronize(RSet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncSmtpCli.AbortSync : Boolean;
begin
    Result := Synchronize(Abort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSyncSmtpCli.TriggerGetData(
LineNum  : Integer;
MsgLine  : PChar;
MaxLen   : Integer;
var More : Boolean);
begin
    inherited TriggerGetData(LineNum, MsgLine, MaxLen, More);
    { Evaluate the timeout period again }
    if FTimeout > 0 then
        FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TSmtpCli, TSyncSmtpCli]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.