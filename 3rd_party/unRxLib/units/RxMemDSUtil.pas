{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2003 Alexey Dred                }
{                                                       }
{*******************************************************}

{$I RX.INC}

unit RxMemDSUtil;

// ---------------------------------------------------------------------------------------
// Extra tools for work with TRxMemoryData
// ---------------------------------------------------------------------------------------

interface

uses Classes, SysUtils, RxMemDS, Variants;

type
  // Exception for errors whileloading/saving (messages are in Russian)
  ERxMemoryDataWriteError = class(Exception);
  ERxMemoryDataReadError = class(Exception);
  // Callback while processing
  TReadWriteRxMemoryDataCallback = procedure(ACurrent, ATotal: Integer; var ACancel: Boolean) of object;

  // Saving to stream. Exceptions are rased for errors.
procedure WriteRxMemoryDataToStream(AObject: TRxMemoryData; AStream: TStream; ABufSize: Integer = 32768;
  ACallback: TReadWriteRxMemoryDataCallback = nil);

  // Loading from stream (Structure and data would be loaded from stream 'as is'
  //  - with no respect to current structure).
  // Exceptions are rased for errors.
procedure ReadRxMemoryDataFromStream(AObject: TRxMemoryData; AStream: TStream; ABufSize: Integer = 32768;
  ACallback: TReadWriteRxMemoryDataCallback = nil);

  // Savign to file. Exceptions are rased for errors.
procedure WriteRxMemoryDataToFile(AObject: TRxMemoryData; AFileName: string;
  AFileMode: Word = (fmCreate or fmOpenWrite or fmShareDenyWrite); ABufSize: Integer = 32768;
  ACallback: TReadWriteRxMemoryDataCallback = nil);

  // Loading from file. Exceptions are rased for errors.
procedure ReadRxMemoryDataFromFile(AObject: TRxMemoryData; AFileName: string;
  AFileMode: Word = (fmOpenRead or fmShareDenyWrite); ABufSize: Integer = 32768;
  ACallback: TReadWriteRxMemoryDataCallback = nil);

implementation

uses DB, TypInfo;

// ---------------------------------------------------------------------------------------
// Internal types and constants
// ---------------------------------------------------------------------------------------

const
  // Supported field datatypes
  DefProcessableFields: set of TFieldType = [
  ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftDate, ftTime,
    ftDateTime, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftBytes
    ];

// ---------------------------------------------------------------------------------------
// Internal calls
// ---------------------------------------------------------------------------------------

procedure _WriteFieldValueToStream(AField: TField; AWriter: TWriter);
var
  tmpBool: Boolean;
begin
  with AField, AWriter do
  begin
    // Watching for NULL
    tmpBool := (IsNull and (not (DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])));
    WriteBoolean(tmpBool);
    if (tmpBool) then
      Exit;
    // String or binary
    if ((DataType in [ftString, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftBytes]) or IsBlob) then
      WriteString(AsString)
    else
    begin
      case (DataType) of
        // whole numbers
        ftSmallint, ftInteger, ftWord, ftAutoInc: WriteInteger(AsInteger);
        // boolean
        ftBoolean: WriteBoolean(AsBoolean);
        // floating point
        ftFloat: WriteFloat(AsFloat);
        // Money
        ftCurrency: WriteCurrency(AsCurrency);
        // Data and time
        ftDate, ftTime, ftDateTime: WriteDate(AsDateTime);
      else
        raise ERxMemoryDataWriteError.Create('Write error: unknown datatype of field');
      end;
    end;
  end;
end;

procedure _ReadFieldValueFromStream(AField: TField; AReader: TReader);
begin
  with AField, AReader do
  begin
    // Watching for null
    if (ReadBoolean) then
    begin
      Value := Null;
      Exit;
    end;
    // String or binary
    if ((DataType in [ftString, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftBytes]) or IsBlob) then
      AsString := ReadString
    else
    begin
      case (DataType) of
        // whole number
        ftSmallint, ftInteger, ftWord, ftAutoInc: AsInteger := ReadInteger;
        // Boolean
        ftBoolean: AsBoolean := ReadBoolean;
        // floating point
        ftFloat: AsFloat := ReadFloat;
        // Money
        ftCurrency: AsCurrency := ReadCurrency;
        // Date and time
        ftDate, ftTime, ftDateTime: AsDateTime := ReadDate;
      else
        raise ERxMemoryDataReadError.Create('Read error: unknown datatype of field');
      end;
    end;
  end;
end;

procedure _Callback(ACallback: TReadWriteRxMemoryDataCallback;
  ACurrent, ATotal: Integer; AExceptionClass: ExceptClass);
var
  tmpCancel: Boolean;
  tmp: string;
begin
  if (not Assigned(ACallback)) then
    Exit;
  tmpCancel := False;
  try
    ACallback(ACurrent, ATotal, tmpCancel);
    if (tmpCancel) then
    begin
      tmp := ' ';
      if (AExceptionClass = ERxMemoryDataWriteError) then
        tmp := ' Saving ';
      if (AExceptionClass = ERxMemoryDataReadError) then
        tmp := ' Loading ';
      raise AExceptionClass.Create(tmp + ' of data was aborted.');
    end;
  finally
    tmp := '';
  end;
end;

// ---------------------------------------------------------------------------------------
// Extermal calls
// ---------------------------------------------------------------------------------------

// Record to stream. Exceptions are raised if any error.

procedure WriteRxMemoryDataToStream(AObject: TRxMemoryData; AStream: TStream; ABufSize: Integer;
  ACallback: TReadWriteRxMemoryDataCallback);
var
  tmpWriter: TWriter;
  tmpRecNo: Integer;
  i, n: Integer;
begin
  // checking arguments
  if (not Assigned(AObject)) then
    raise ERxMemoryDataWriteError.Create('Invalid parameter (AObject).');
  if (not Assigned(AStream)) then
    raise ERxMemoryDataWriteError.Create('Invalid parameter  (AStream).');
  if (ABufSize <= 0) then
    raise ERxMemoryDataWriteError.Create('Invalid parameter  (ABufSize).');
  with AObject do
  begin
    // Quering currecnt position (and is the dataset active at once)
    tmpRecNo := RecNo;
    // checking fields' datatypes
    for i := 0 to Fields.Count - 1 do
    begin
      if (not (Fields[i].DataType in DefProcessableFields)) then
        raise ERxMemoryDataWriteError.Create('Datatype is not supported (field ' +
          Fields[i].FieldName + ',  type ' + GetEnumName(TypeInfo(TFieldType),
          Integer(Fields[i].DataType)) + ').');
    end;
  end;

  AObject.DisableControls;
  tmpWriter := TWriter.Create(AStream, ABufSize);
  try
    with tmpWriter, AObject do
    begin
      // callback - process indicator or user abort or whatever
      _Callback(ACallback, 0, RecordCount, ERxMemoryDataWriteError);
      // saving class name and signature
      WriteSignature;
      WriteString(ClassName);
      // saving stucture of dataset
      WriteCollection(FieldDefs);
      // saving data
      WriteInteger(RecordCount);
      WriteListBegin;
      First;
      n := 0;
      while (not Eof) do
      begin
        for i := 0 to Fields.Count - 1 do
          _WriteFieldValueToStream(Fields[i], tmpWriter);
        Inc(n);
        // callback again
        _Callback(ACallback, n, RecordCount, ERxMemoryDataWriteError);

        Next;
      end;
      WriteListEnd;
      if (n <> RecordCount) then
        raise ERxMemoryDataWriteError.Create('Unexpected error (record count do not match).');
      // complete
      FlushBuffer;
    end;
  finally
    tmpWriter.Free;
    AObject.RecNo := tmpRecNo;
    AObject.EnableControls;
  end;
end;

 // Loading from stream (Structure and data would be loaded from stream 'as is'
 // - with no respect to current structure). // Exceptions are rased for errors.

procedure ReadRxMemoryDataFromStream(AObject: TRxMemoryData; AStream: TStream; ABufSize: Integer;
  ACallback: TReadWriteRxMemoryDataCallback);
var
  tmpReader: TReader;
  i, j, n: Integer;
begin
  // Checking arguments
  if (not Assigned(AObject)) then
    raise ERxMemoryDataReadError.Create('Invalid parameter (AObject).');
  if (not Assigned(AStream)) then
    raise ERxMemoryDataReadError.Create('Invalid parameter (AStream).');
  if (ABufSize <= 0) then
    raise ERxMemoryDataWriteError.Create('Invalid parameter (ABufSize).');
  // Is dataset active? (must be active for both reading and writing)
  // AObject.Next;

  AObject.DisableControls;
  tmpReader := TReader.Create(AStream, ABufSize);
  try
    with tmpReader, AObject do
    begin
      // Clearing data before loading them
      Open;
      EmptyTable;
      Close;
      FieldDefs.Clear;
      Fields.Clear;
      //  callback - user information and/or reaction
      _Callback(ACallback, 0, 0, ERxMemoryDataReadError);
      // Reading class signature and name
      ReadSignature;
      if (ReadString <> AObject.ClassName) then
        raise ERxMemoryDataReadError.Create('Saved object do not match destination object.');
      // Loading dataset structure
      ReadValue;
      ReadCollection(AObject.FieldDefs);

      Open;
      // checking field datatypes
      for i := 0 to Fields.Count - 1 do
      begin
        if (not (Fields[i].DataType in DefProcessableFields)) then
          raise ERxMemoryDataReadError.Create('Datatype is not supported (field ' +
            Fields[i].FieldName + ',  type ' + GetEnumName(TypeInfo(TFieldType),
            Integer(Fields[i].DataType)) + ').');
      end;
      // Loading the data itself
      n := ReadInteger;
      ReadListBegin;
      j := 0;
      while (j <> n) do
      begin
        Append;
        for i := 0 to Fields.Count - 1 do
          _ReadFieldValueFromStream(Fields[i], tmpReader);
        Post;
        Inc(j);
        _Callback(ACallback, j, n, ERxMemoryDataReadError);
      end;
      ReadListEnd;
      if ((j <> n) or (n <> RecordCount)) then
        raise ERxMemoryDataReadError.Create('Unexpected error (record count do not match).');
      First;
      // complete
    end;
  finally
    tmpReader.Free;
    AObject.EnableControls;
  end;
end;

// Saving to file. Exceptions are raised in case of errors.

procedure WriteRxMemoryDataToFile(AObject: TRxMemoryData; AFileName: string; AFileMode: Word;
  ABufSize: Integer; ACallback: TReadWriteRxMemoryDataCallback);
var
  tmpStream: TFileStream;
begin
  tmpStream := TFileStream.Create(AFileName, AFileMode);
  try
    WriteRxMemoryDataToStream(AObject, tmpStream, ABufSize, ACallback);
  finally
    tmpStream.Free;
  end;
end;

//  Loading from file. Exceptions are raised in case of errors.

procedure ReadRxMemoryDataFromFile(AObject: TRxMemoryData; AFileName: string;
  AFileMode: Word; ABufSize: Integer; ACallback: TReadWriteRxMemoryDataCallback);
var
  tmpStream: TFileStream;
begin
  tmpStream := TFileStream.Create(AFileName, AFileMode);
  try
    ReadRxMemoryDataFromStream(AObject, tmpStream, ABufSize, ACallback);
  finally
    tmpStream.Free;
  end;
end;

// ---------------------------------------------------------------------------------------

end.