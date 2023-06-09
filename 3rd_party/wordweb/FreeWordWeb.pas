{*******************************************************}
{                                                       }
{    TFreeWordWeb Thesaurus/Dictionary component        }
{                                                       }
{    Uses the free WordWeb thesaurus/dictionary         }
{    available from http://wordweb.co.uk/free           }
{                                                       }
{    For even more flexibility see WordWeb Developer    }
{    at http://www.x-word.com/wwdev/                    }
{                                                       }
{    Copyright (c) 2000 Antony Lewis                    }
{    This component is freeware                         }
{                                                       }
{*******************************************************}

unit FreeWordWeb;

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs;

Type TShowModalWordWeb = function(InP,OutP:PChar;CloseOnCopy:WordBool;AParent:Integer):WordBool;stdcall;

Type EFreeWordWebError=Class(Exception);

type
  TFreeWordWeb = class(TComponent)
  private
    { Private declarations }
    FLookupWord: String;
    FReturnWord: String;
    FCloseOnCopy:Boolean;
    LoadCount : Integer;
    HWordWeb:THandle;
    ShowModal:TShowModalWordWeb;
  protected
    { Protected declarations }
  public
    { Public declarations }
     procedure LoadWordWeb;
      //Load the WordWeb DLL. If not found an EFreeWordWeb exception is raised
     procedure UnLoadWordWeb;
      //Unload the WordWeb DLL.
     function Execute:Boolean;
      //Show the WordWeb window (modally)
  published
    { Published declarations }
     property CloseOnCopy:Boolean read FCloseOnCopy write FCloseOnCopy;
       //Set to true if you want WordWeb to return when the user presses
       //the copy button. Use "Return Word" to find which word was copied
       //If false the word is copied to the clipboard
     property LookupWord:String read FLookupWord write FLookupWord;
       //The word to lookup when WordWeb starts
     property ReturnWord:String read FReturnWord write FReturnWord;
       //The word copied if CloseOnCopy is true
  end;

procedure Register;

implementation


{ TFreeWordWeb }

function TFreeWordWeb.Execute:Boolean;
var OutS:Array[0..255] of char;
begin
LoadWordWeb;
try
Result:=ShowModal(PChar(LookupWord),@OutS,CloseOnCopy,Application.Handle);
if Result then ReturnWord:=OutS;
finally
UnLoadWordWeb;
end;
end;

procedure TFreeWordWeb.LoadWordWeb;
begin
if LoadCount>0 then
 begin
 inc(LoadCount);
 end
  else
   begin
    HWordWeb:=LoadLibrary('WWEB32.DLL');
    If HWordWeb>HINSTANCE_ERROR then
            @ShowModal:=GetProcAddress(HWordWeb,'ShowModalWordWeb');
    If (HWordWeb>HINSTANCE_ERROR) and Assigned(ShowModal) then
     begin
     Inc(LoadCount);
     end
      else
       begin
       HWordWeb:=0;
       Raise EFreeWordWebError.Create('Could not load WordWeb');
       end;
   end;
end;

procedure TFreeWordWeb.UnLoadWordWeb;
begin
  if (HWordWeb<>0) then
   begin
    Dec(LoadCount);
    if LoadCount=0 then FreeLibrary(HWordWeb);
   end;
end;

procedure Register;
begin
  RegisterComponents('Freeware', [TFreeWordWeb]);
end;


end.
