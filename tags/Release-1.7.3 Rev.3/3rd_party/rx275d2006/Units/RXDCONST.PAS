{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit RXDConst;

{ RX Data aware controls constants }
{
  Reserved range
  from MaxExtStrID - 86
  to   MaxExtStrID - 134
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ DBLists }

  SLocalDatabase          = MaxExtStrID - 86;

{ DBUtils }

  SRetryLogin             = MaxExtStrID - 87;

{ DBFilter }

  SExprNotBoolean         = MaxExtStrID - 88;
  SExprBadNullTest        = MaxExtStrID - 89;
  SExprBadField           = MaxExtStrID - 90;
  SCaptureFilter          = MaxExtStrID - 91;
  SNotCaptureFilter       = MaxExtStrID - 92;

{ RxDBCtrl }

  SInactiveData           = MaxExtStrID - 93;
  SBrowseData             = MaxExtStrID - 94;
  SEditData               = MaxExtStrID - 95;
  SInsertData             = MaxExtStrID - 96;
  SSetKeyData             = MaxExtStrID - 97;
  SCalcFieldsData         = MaxExtStrID - 98;

{ LoginDlg }

  SRegistration           = MaxExtStrID - 99;
  SAppTitleLabel          = MaxExtStrID - 100;
  SHintLabel              = MaxExtStrID - 101;
  SUserNameLabel          = MaxExtStrID - 102;
  SPasswordLabel          = MaxExtStrID - 103;
  SInvalidUserName        = MaxExtStrID - 104;

{ ChPswDlg }

  SChangePassword         = MaxExtStrID - 105;
  SOldPasswordLabel       = MaxExtStrID - 106;
  SNewPasswordLabel       = MaxExtStrID - 107;
  SConfirmPasswordLabel   = MaxExtStrID - 108;
  SPasswordChanged        = MaxExtStrID - 109;
  SPasswordNotChanged     = MaxExtStrID - 110;
  SPasswordsMismatch      = MaxExtStrID - 111;

{ DBExcpt }

  SDBExceptCaption        = MaxExtStrID - 112;
  SBDEErrorLabel          = MaxExtStrID - 113;
  SServerErrorLabel       = MaxExtStrID - 114;
  SErrorMsgLabel          = MaxExtStrID - 115;
  SNextButton             = MaxExtStrID - 116;
  SPrevButton             = MaxExtStrID - 117;

{ DBFilter expression parser }

  SExprIncorrect          = MaxExtStrID - 118;
  SExprTermination        = MaxExtStrID - 119;
  SExprNameError          = MaxExtStrID - 120;
  SExprStringError        = MaxExtStrID - 121;
  SExprInvalidChar        = MaxExtStrID - 122;
  SExprNoRParen           = MaxExtStrID - 123;
  SExprExpected           = MaxExtStrID - 124;
  SExprBadCompare         = MaxExtStrID - 125;

{ DBUtils }

  SConfirmSave            = MaxExtStrID - 126;
  SDatabaseName           = MaxExtStrID - 127;  

{ LoginDlg }
  
  SUnlockCaption          = MaxExtStrID - 128;
  SUnlockHint             = MaxExtStrID - 129;

{ RxDBCtrl }

  SDeleteMultipleRecords  = MaxExtStrID - 130;

implementation

{$IFDEF WIN32}
 {$R *.R32}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.