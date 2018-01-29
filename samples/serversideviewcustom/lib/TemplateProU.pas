//
// Copyright (c) 2017 Daniele Teti
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit TemplateProU;

interface

uses
  System.Generics.Collections,
  Classes,
  SysUtils,
  Data.DB,
  System.RTTI;

type
  EParserException = class(Exception)

  end;

  ITPDataSourceAdapter = interface
    ['{9A0E5797-A8D2-413F-A8B0-5D6E67DD1701}']
    function CurrentIndex: Int64;
    procedure Reset;
    function GetMemberValue(const aMemberName: string): string;
    procedure Next;
    function Eof: Boolean;
  end;

  TTPLoopControl = record
    Identifier: string;
    class function Create(aIdentifier: string): TTPLoopControl; static;
  end;

  TTPDatasetDictionary = class(TDictionary<string, TDataSet>);

  TTPObjectListDictionary = class(TObjectDictionary < string, TObjectList < TObject >> );

  TTPDatasetAdapter = class(TInterfacedObject, ITPDataSourceAdapter)
  private
    fDataSet: TDataSet;
  public
    constructor Create(const aDataSet: TDataSet);
  protected
    function CurrentIndex: Int64;
    procedure Reset;
    function GetMemberValue(const aMemberName: string): string;
    procedure Next;
    function Eof: Boolean;
  end;

  TTPObjectListAdapter = class(TInterfacedObject, ITPDataSourceAdapter)
  private
  class var
    CTX: TRttiContext;
    fObjectList: TObjectList<TObject>;
    fIndex: Integer;
  public
    constructor Create(const aObjectList: TObjectList<TObject>);
    class constructor Create;
    class destructor Destroy;
  protected
    function Current: TObject;
    function CurrentIndex: Int64;
    procedure Reset;
    function GetMemberValue(const aMemberName: string): string;
    procedure Next;
    function Eof: Boolean;
  end;

  TTemplateProEngine = class
  strict private
    fOutput: string;
    fVariables: TDictionary<string, string>;
    function MatchStartTag: Boolean;
    function MatchEndTag: Boolean;
    function MatchIdentifier(var aIdentifier: string): Boolean;
    function MatchValue(var aValue: string): Boolean;
    function MatchReset(var aDataSet: string): Boolean;
    function MatchField(var aDataSet: string; var aFieldName: string): Boolean;
    function MatchSymbol(const aSymbol: string): Boolean;
  private
    fDataSources: TDictionary<string, ITPDataSourceAdapter>;
    fInputString: string;
    fCharIndex: Int64;
    fCurrentLine: Integer;
    fCurrentColumn: Integer;
    fLoopStack: TStack<Integer>;
    fLoopIdentStack: TStack<TTPLoopControl>;
    fCurrentDataSource: ITPDataSourceAdapter;
    fOutputStreamWriter: TStreamWriter;
    fEncoding: TEncoding;
    procedure Error(const aMessage: string);
    procedure ErrorFmt(const aMessage: string; aParameters: array of const);

    function ExecuteFunction(aFunctionName: string;
      aParameters: TArray<string>;
      aValue: string): string;

    function ExecuteFieldFunction(aFunctionName: string;
      aParameters: TArray<string>;
      aValue: TValue): string;

    function SetDataSourceByName(const aName: string): Boolean;
    function GetFieldText(const aFieldName: string): string;
    procedure CheckParNumber(const aHowManyPars: Integer;
      const aParameters: TArray<string>); overload;
    procedure CheckParNumber(const aMinParNumber, aMaxParNumber: Integer;
      const aParameters: TArray<string>); overload;
    procedure AppendOutput(const aValue: string);
    procedure LoadDataSources(const aObjectDictionary: TTPObjectListDictionary; const aDatasetDictionary: TTPDatasetDictionary);
  public
    procedure Execute(const aTemplateString: string; const aObjectDictionary: TTPObjectListDictionary; const aDatasetDictionary: TTPDatasetDictionary; aStream: TStream); overload;
    procedure Execute(const aTemplateString: string;
      const aObjectNames: array of string; aObjects: array of TObjectList<TObject>;
      const aDataSetNames: array of string; aDataSets: array of TDataSet;
      aStream: TStream); overload;
    constructor Create(aEncoding: TEncoding = nil);
    destructor Destroy; override;
    procedure SetVar(const aName: string; aValue: string);
    function GetVar(const aName: string): string;
    procedure ClearVariables;
  end;

implementation

uses
  System.StrUtils;

const
  IdenfierAllowedFirstChars = ['a' .. 'z', 'A' .. 'Z', '_'];
  IdenfierAllowedChars = IdenfierAllowedFirstChars + ['0' .. '9'];
  ValueAllowedChars = IdenfierAllowedChars + [' ', '-', '+', '*', '.', '@', '/', '\']; // maybe a lot others
  START_TAG_1 = '{';
  END_TAG_1 = '}';

  { TParser }

procedure TTemplateProEngine.AppendOutput(const aValue: string);
begin
  fOutputStreamWriter.Write(aValue);
end;

procedure TTemplateProEngine.CheckParNumber(const aMinParNumber,
  aMaxParNumber: Integer; const aParameters: TArray<string>);
var
  lParNumber: Integer;
begin
  lParNumber := Length(aParameters);
  if (lParNumber < aMinParNumber) or (lParNumber > aMaxParNumber) then
  begin
    if aMinParNumber = aMaxParNumber then
      Error(Format('Expected %d parameters, got %d', [aMinParNumber, lParNumber]))
    else
      Error(Format('Expected from %d to %d parameters, got %d', [aMinParNumber, aMaxParNumber, lParNumber]));
  end;
end;

procedure TTemplateProEngine.ClearVariables;
begin
  fVariables.Clear;
end;

constructor TTemplateProEngine.Create(aEncoding: TEncoding = nil);
begin
  inherited Create;
  if aEncoding = nil then
    fEncoding := TEncoding.UTF8 { default encoding }
  else
    fEncoding := aEncoding;
  fOutput := '';
  fVariables := TDictionary<string, string>.Create;
  fLoopStack := TStack<Integer>.Create;
  fLoopIdentStack := TStack<TTPLoopControl>.Create;
  fDataSources := TDictionary<string, ITPDataSourceAdapter>.Create;
end;

destructor TTemplateProEngine.Destroy;
begin
  fDataSources.Free;
  fLoopIdentStack.Free;
  fLoopStack.Free;
  fVariables.Free;
  fOutputStreamWriter.Free;
  inherited;
end;

function TTemplateProEngine.SetDataSourceByName(const aName: string): Boolean;
var
  ds: TPair<string, ITPDataSourceAdapter>;
begin
  Result := False;
  for ds in fDataSources do
  begin
    if SameText(ds.Key, aName) then
    begin
      fCurrentDataSource := ds.Value;
      Result := True;
      Break;
    end;
  end;
end;

function TTemplateProEngine.GetFieldText(const aFieldName: string): string;
begin
  if not Assigned(fCurrentDataSource) then
    Error('Current datasource not set');
  Result := fcurrentDataSource.GetMemberValue(aFieldName);
end;

function TTemplateProEngine.GetVar(const aName: string): string;
begin
  if not fVariables.TryGetValue(aName, Result) then
    Result := '';
end;

procedure TTemplateProEngine.LoadDataSources(
  const aObjectDictionary: TTPObjectListDictionary;
  const aDatasetDictionary: TTPDatasetDictionary);
var
  lDatasetPair: TPair<string, TDataset>;
  lObjectPair: TPair<string, TObjectList<TObject>>;
begin
  fDataSources.Clear;

  for lDatasetPair in aDatasetDictionary do
  begin
    fDataSources.Add(lDatasetPair.Key, TTPDatasetAdapter.Create(lDatasetPair.Value));
  end;

  for lObjectPair in aObjectDictionary do
  begin
    fDataSources.Add(lObjectPair.Key, TTPObjectListAdapter.Create(lObjectPair.Value));
  end;
end;

function TTemplateProEngine.MatchEndTag: Boolean;
begin
  Result := fInputString.Chars[fCharIndex] = END_TAG_1;
  if Result then
    Inc(fCharIndex, 1);
end;

function TTemplateProEngine.MatchField(var aDataSet: string; var aFieldName: string): Boolean;
begin
  Result := False;
  aFieldName := '';
  if not MatchSymbol(':') then
    Exit;
  if not MatchIdentifier(aDataSet) then
    Error('Expected dataset name');
  if not MatchSymbol('.') then
    Error('Expected "."');
  if not MatchIdentifier(aFieldName) then
    Error('Expected field name');
  Result := True;
end;

function TTemplateProEngine.MatchIdentifier(var aIdentifier: string): Boolean;
begin
  aIdentifier := '';
  Result := False;
  if CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedFirstChars) then
  begin
    while CharInSet(fInputString.Chars[fCharIndex], IdenfierAllowedChars) do
    begin
      aIdentifier := aIdentifier + fInputString.Chars[fCharIndex];
      Inc(fCharIndex);
    end;
    Result := True;
  end
end;

function TTemplateProEngine.MatchReset(var aDataSet: string): Boolean;
begin
  if not MatchSymbol('reset') then
    Exit(False);
  Result := MatchSymbol('(') and MatchIdentifier(aDataSet) and MatchSymbol(')');
end;

function TTemplateProEngine.MatchStartTag: Boolean;
begin
  Result := fInputString.Chars[fCharIndex] = START_TAG_1;
  if Result then
    Inc(fCharIndex, 1);
end;

function TTemplateProEngine.MatchSymbol(const aSymbol: string): Boolean;
var
  lSymbolIndex: Integer;
  lSavedCharIndex: Int64;
begin
  if aSymbol.IsEmpty then
    Exit(True);
  lSavedCharIndex := fCharIndex;
  lSymbolIndex := 0;
  // lChar := FInputString.Chars[FCharIndex];
  while fInputString.Chars[fCharIndex] = aSymbol.Chars[lSymbolIndex] do
  begin
    Inc(fCharIndex);
    Inc(lSymbolIndex);
    // lChar := FInputString.Chars[FCharIndex]
  end;
  Result := (lSymbolIndex > 0) and (lSymbolIndex = Length(aSymbol));
  if not Result then
    fCharIndex := lSavedCharIndex;
end;

function TTemplateProEngine.MatchValue(var aValue: string): Boolean;
begin
  aValue := '';
  while CharInSet(fInputString.Chars[fCharIndex], ValueAllowedChars) do
  begin
    aValue := aValue + fInputString.Chars[fCharIndex];
    Inc(fCharIndex);
  end;
  Result := not aValue.IsEmpty;
end;

procedure TTemplateProEngine.Execute(const aTemplateString: string; const aObjectDictionary: TTPObjectListDictionary; const aDatasetDictionary: TTPDatasetDictionary; aStream: TStream);
var
  lChar: Char;
  lVarName: string;
  lFuncName: string;
  lIdentifier: string;
  lDataSet: string;
  lFieldName: string;
  lIgnoreOutput: Boolean;
  lFuncParams: TArray<string>;
  lDataSourceName: string;
  function GetFunctionParameters: TArray<string>;
  var
    lFuncPar: string;
  begin
    Result := [];
    while MatchSymbol(':') do
    begin
      lFuncPar := '';
      if not MatchValue(lFuncPar) then
        Error('Expected function parameter');
      Result := Result + [lFuncPar];
    end;
  end;

begin
  lIgnoreOutput := False;
  FreeAndNil(fOutputStreamWriter);
  fOutputStreamWriter := TStreamWriter.Create(aStream, fEncoding);
  LoadDataSources(aObjectDictionary, aDatasetDictionary);
  fLoopStack.Clear;
  fLoopIdentStack.Clear;
  fCharIndex := 0;
  fCurrentLine := 1;
  fCurrentColumn := 0;
  fInputString := aTemplateString;
  while fCharIndex < aTemplateString.Length do
  begin
    lChar := aTemplateString.Chars[fCharIndex];
    if lChar = #13 then
    begin
      Inc(fCurrentLine);
      fCurrentColumn := 1;
    end
    else
    begin
      Inc(fCurrentColumn);
    end;

    // starttag
    if MatchStartTag then
    begin
      // loop
      if not lIgnoreOutput and MatchSymbol('loop') then
      begin
        if not MatchSymbol('(') then
          Error('Expected "("');
        if not MatchIdentifier(lIdentifier) then
          Error('Expected identifier after "loop("');
        if not MatchSymbol(')') then
          Error('Expected ")" after "' + lIdentifier + '"');
        if not MatchEndTag then
          Error('Expected closing tag for "loop(' + lIdentifier + ')"');
        if not SetDataSourceByName(lIdentifier) then
          Error('Unknown dataset: ' + lIdentifier);
        fLoopStack.Push(fCharIndex);
        fLoopIdentStack.Push(TTPLoopControl.Create(lIdentifier));
        lIgnoreOutput := false; // FCurrentDataset.Eof;
        Continue;
      end;

      // endloop
      if MatchSymbol('endloop') then
      begin
        if not MatchEndTag then
          Error('Expected closing tag');
        lIdentifier := fLoopIdentStack.Peek.Identifier;
        if not SetDataSourceByName(lIdentifier) then
          Error('Invalid datasource name: ' + lIdentifier);

        FCurrentDataSource.Next;
        if FCurrentDataSource.Eof then
        begin
          fLoopIdentStack.Pop;
          fLoopStack.Pop;
          lIgnoreOutput := False;
        end
        else
        begin
          fCharIndex := fLoopStack.Peek;
        end;
        Continue;
      end;

      // dataset field
      if not lIgnoreOutput and MatchField(lDataSourceName, lFieldName) then
      begin
        if lFieldName.IsEmpty then
          Error('Invalid field name');
        lFuncName := '';
        if MatchSymbol('|') then
        begin
          if not MatchIdentifier(lFuncName) then
            Error('Invalid function name');
          lFuncParams := GetFunctionParameters;
          if not MatchEndTag then
            Error('Expected end tag');
        end
        else
        begin
          if not MatchEndTag then
            Error('Expected closing tag');
          if not SetDataSourceByName(lDataSourceName) then
            Error('Unknown datasource: ' + lDataSourceName);
        end;
        if lFuncName.IsEmpty then
          AppendOutput(GetFieldText(lFieldName))
        else
          AppendOutput(ExecuteFieldFunction(lFuncName, lFuncParams, GetFieldText(lFieldName)));
      end;

      // reset
      if not lIgnoreOutput and MatchReset(lDataSet) then
      begin
        if not MatchEndTag then
          Error('Expected closing tag');
        SetDataSourceByName(lDataSet);
        fCurrentDataSource.Reset;
        Continue;
      end;

      // identifier
      if not lIgnoreOutput and MatchIdentifier(lVarName) then
      begin
        if lVarName.IsEmpty then
          Error('Invalid variable name');
        lFuncName := '';
        if MatchSymbol('|') then
        begin
          if not MatchIdentifier(lFuncName) then
            Error('Invalid function name');
          lFuncParams := GetFunctionParameters;
          if not MatchEndTag then
            Error('Expected end tag');
          AppendOutput(ExecuteFunction(lFuncName, lFuncParams, GetVar(lVarName)));
        end
        else
        begin
          if not MatchEndTag then
            Error('Expected end tag');
          AppendOutput(GetVar(lVarName));
        end;
      end;
    end
    else
    begin
      // output verbatim
      if not lIgnoreOutput then
        AppendOutput(lChar);
      Inc(fCharIndex);
    end;
  end;
end;

procedure TTemplateProEngine.SetVar(
  const
  aName: string;
  aValue: string);
begin
  fVariables.AddOrSetValue(aName, aValue);
end;

function CapitalizeString(
  const
  s: string;
  const
  CapitalizeFirst: Boolean): string;
const
  ALLOWEDCHARS = ['a' .. 'z', '_'];
var
  index: Integer;
  bCapitalizeNext: Boolean;
begin
  bCapitalizeNext := CapitalizeFirst;
  Result := lowercase(s);
  if Result <> EmptyStr then
  begin
    for index := 1 to Length(Result) do
    begin
      if bCapitalizeNext then
      begin
        Result[index] := UpCase(Result[index]);
        bCapitalizeNext := False;
      end
      else if not CharInSet(Result[index], ALLOWEDCHARS) then
      begin
        bCapitalizeNext := True;
      end;
    end; // for
  end; // if
end;

procedure TTemplateProEngine.Error(const aMessage: string);
begin
  raise EParserException.CreateFmt('%s - at line %d col %d',
    [aMessage, fCurrentLine, fCurrentColumn]);
end;

procedure TTemplateProEngine.ErrorFmt(const aMessage: string;
  aParameters: array of const);
begin
  Error(Format(aMessage, aParameters));
end;

procedure TTemplateProEngine.CheckParNumber(const aHowManyPars: Integer; const aParameters: TArray<string>);
begin
  CheckParNumber(aHowManyPars, aHowManyPars, aParameters);
end;

function TTemplateProEngine.ExecuteFunction(aFunctionName: string; aParameters: TArray<string>; aValue: string): string;
begin
  aFunctionName := lowercase(aFunctionName);
  if aFunctionName = 'uppercase' then
  begin
    Exit(UpperCase(aValue));
  end;
  if aFunctionName = 'lowercase' then
  begin
    Exit(lowercase(aValue));
  end;
  if aFunctionName = 'capitalize' then
  begin
    Exit(CapitalizeString(aValue, True));
  end;
  if aFunctionName = 'rpad' then
  begin
    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
      Exit(aValue.PadRight(aParameters[0].ToInteger))
    else
      Exit(aValue.PadRight(aParameters[0].ToInteger, aParameters[1].Chars[0]));
  end;
  if aFunctionName = 'lpad' then
  begin
    if Length(aParameters) = 1 then
      Exit(aValue.PadLeft(aParameters[0].ToInteger))
    else
      Exit(aValue.PadLeft(aParameters[0].ToInteger, aParameters[1].Chars[0]));
  end;

  raise EParserException.CreateFmt('Unknown function [%s]', [aFunctionName]);
end;

procedure TTemplateProEngine.Execute(const aTemplateString: string;
  const aObjectNames: array of string; aObjects: array of TObjectList<TObject>;
  const aDataSetNames: array of string; aDataSets: array of TDataSet;
  aStream: TStream);
var
  lDatasets: TTPDataSetDictionary;
  lObjects: TTPObjectListDictionary;
  I: Integer;
begin
  if Length(aObjectNames) <> Length(aObjects) then
    ErrorFmt('Wrong Names/Objects count. Names: %d, Objects: %d', [Length(aObjectNames), Length(aObjects)]);
  if Length(aDataSetNames) <> Length(aDataSets) then
    ErrorFmt('Wrong Names/DataSets count. Names: %d, DataSets: %d', [Length(aDataSetNames), Length(aDataSets)]);

  lDatasets := TTPDatasetDictionary.Create;
  try
    for I := 0 to Length(aDataSetNames) - 1 do
    begin
      lDatasets.Add(aDataSetNames[i], aDataSets[i]);
    end;

    lObjects := TTPObjectListDictionary.Create([]);
    try
      for I := 0 to Length(aObjectNames) - 1 do
      begin
        lObjects.Add(aObjectNames[i], aObjects[i]);
      end;

      Execute(aTemplateString, lObjects, lDatasets, aStream);
    finally
      lObjects.Free;
    end;
  finally
    lDatasets.Free;
  end;
end;

function TTemplateProEngine.ExecuteFieldFunction(aFunctionName: string;
  aParameters: TArray<string>;
  aValue: TValue): string;
var
  lDateValue: TDate;
  lDateTimeValue: TDateTime;
  lStrValue: string;
begin
  aFunctionName := lowercase(aFunctionName);

  if aFunctionName = 'uppercase' then
  begin
    Exit(UpperCase(aValue.AsString));
  end;
  if aFunctionName = 'lowercase' then
  begin
    Exit(lowercase(aValue.AsString));
  end;
  if aFunctionName = 'capitalize' then
  begin
    Exit(CapitalizeString(aValue.AsString, True));
  end;
  if aFunctionName = 'rpad' then
  begin
    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      ErrorFmt('Invalid parameter/s for function: %s', [aFunctionName]);

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
    begin
      Exit(lStrValue.PadRight(aParameters[0].ToInteger));
    end
    else
    begin
      Exit(lStrValue.PadRight(aParameters[0].ToInteger, aParameters[1].Chars[0]));
    end;
  end;
  if aFunctionName = 'lpad' then
  begin
    if aValue.IsType<Integer> then
      lStrValue := aValue.AsInteger.ToString
    else if aValue.IsType<string> then
      lStrValue := aValue.AsString
    else
      ErrorFmt('Invalid parameter/s for function: ', [aFunctionName]);

    CheckParNumber(1, 2, aParameters);
    if Length(aParameters) = 1 then
    begin
      Exit(lStrValue.PadLeft(aParameters[0].ToInteger));
    end
    else
    begin
      Exit(lStrValue.PadLeft(aParameters[0].ToInteger, aParameters[1].Chars[0]));
    end;
  end;

  if aFunctionName = 'datetostr' then
  begin
    if not aValue.TryAsType<TDate>(lDateValue) then
      Error('Invalid Date');
    Exit(DateToStr(lDateValue));
  end;
  if aFunctionName = 'datetimetostr' then
  begin
    if not aValue.TryAsType<TDateTime>(lDateTimeValue) then
      Error('Invalid DateTime');
    Exit(DateTimeToStr(lDateTimeValue));
  end;
  if aFunctionName = 'formatdatetime' then
  begin
    CheckParNumber(1, aParameters);
    if not aValue.TryAsType<TDateTime>(lDateTimeValue) then
      Error('Invalid DateTime');
    Exit(FormatDateTime(aParameters[0], lDateTimeValue));
  end;

  ErrorFmt('Unknown function [%s]', [aFunctionName]);
end;

class
  function TTPLoopControl.Create(aIdentifier: string): TTPLoopControl;
begin
  Result.Identifier := aIdentifier;
end;

{ TTPDatasetAdapter }

constructor TTPDatasetAdapter.Create(const aDataSet: TDataSet);
begin
  inherited Create;
  fDataSet := aDataSet;
end;

function TTPDatasetAdapter.CurrentIndex: Int64;
begin
  Result := fDataSet.RecNo;
end;

function TTPDatasetAdapter.Eof: Boolean;
begin
  Result := fDataSet.Eof;
end;

function TTPDatasetAdapter.GetMemberValue(const aMemberName: string): string;
begin
  Result := fDataSet.FieldByName(aMemberName).AsWideString;
end;

procedure TTPDatasetAdapter.Next;
begin
  fDataSet.Next;
end;

procedure TTPDatasetAdapter.Reset;
begin
  fDataSet.First;
end;

{ TTPObjectListAdapter }

constructor TTPObjectListAdapter.Create(
  const aObjectList: TObjectList<TObject>);
begin
  inherited Create;
  fObjectList := aObjectList;
  if fObjectList.Count > 0 then
    fIndex := 0
  else
    fIndex := -1;
end;

class constructor TTPObjectListAdapter.Create;
begin
  TTPObjectListAdapter.CTX := TRttiContext.Create;
end;

function TTPObjectListAdapter.Current: TObject;
begin
  if fIndex <> -1 then
    Result := fObjectList[fIndex]
  else
    raise Exception.Create('Empty DataSource');
end;

function TTPObjectListAdapter.CurrentIndex: Int64;
begin
  Result := fIndex;
end;

class destructor TTPObjectListAdapter.Destroy;
begin
  TTPObjectListAdapter.CTX.Free;
end;

function TTPObjectListAdapter.Eof: Boolean;
begin
  Result := fIndex = fObjectList.Count - 1;
end;

function TTPObjectListAdapter.GetMemberValue(const aMemberName: string): string;
var
  lRttiType: TRttiType;
  lRttiProp: TRttiProperty;
  lCurrentObj: TObject;
begin
  lCurrentObj := Current;
  lRttiType := CTX.GetType(lCurrentObj.ClassInfo);
  lRttiProp := lRttiType.GetProperty(aMemberName);
  Result := lRttiProp.GetValue(lCurrentObj).AsString;
end;

procedure TTPObjectListAdapter.Next;
begin
  if Eof then
    raise Exception.Create('DataSource is already at EOF');
  Inc(fIndex);
end;

procedure TTPObjectListAdapter.Reset;
begin
  if fObjectList.Count > 0 then
    fIndex := 0
  else
    fIndex := -1;
end;

end.
