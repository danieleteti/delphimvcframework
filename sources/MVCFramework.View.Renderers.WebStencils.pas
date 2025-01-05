// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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

unit MVCFramework.View.Renderers.WebStencils;

{$I dmvcframework.inc}

interface

{$if Defined(WEBSTENCILS)}

//This unit is usable from Delphi 12.2+

uses
  MVCFramework, System.Generics.Collections, System.SysUtils,
  MVCFramework.Commons, System.IOUtils, System.Classes, Web.Stencils,
  System.Rtti, MVCFramework.Nullables, System.DateUtils, System.TypInfo;

type
  TMVCWebStencilsEvent = reference to procedure(const WebStencilsProcessor: TWebStencilsProcessor);

  { This class implements the WebStencils view engine for server side views }
  TMVCWebStencilsViewEngine = class(TMVCBaseViewEngine)
  protected
    procedure RegisterWSFunctions(WSProcessor: TWebStencilsProcessor);
    procedure OnGetValue(Sender: TObject; const AObjectName, APropName: string; var AReplaceText: string; var AHandled: Boolean);
    procedure OnGetFile(Sender: TObject; const AFilename: string; var AText: string; var AHandled: Boolean);
  public
    class function GetTValueVarAsString(const Value: TValue; const VarName: string; const Processor: TWebStencilsProcessor): String;
    procedure Execute(const ViewName: string; const Builder: TStringBuilder); override;
  end;

  TMVCWebStencilsConfiguration = class sealed
  private
    class var fOnProcessorConfiguration: TMVCWebStencilsEvent;
  public
    class property OnProcessorConfiguration: TMVCWebStencilsEvent
      read fOnProcessorConfiguration
      write fOnProcessorConfiguration;
  end;

{$endif}

implementation


{$if Defined(WEBSTENCILS)}
uses
  MVCFramework.Serializer.Defaults,
  MVCFramework.Serializer.Intf,
  MVCFramework.DuckTyping,
  System.Bindings.EvalProtocol,
  System.Bindings.Methods,
  MVCFramework.Cache,
  Data.DB,
  JsonDataObjects;

{$WARNINGS OFF}

var
  gFunctionInitialized: Boolean = False;
  gWSLock: TObject = nil;

function GetDataSetOrObjectListCount(const aValue: TValue; const aParameters: TArray<string>): TValue;
var
  lWrappedList: IMVCList;
begin
  if aValue.IsEmpty or  (not aValue.IsObject) then
  begin
    Result := False;
  end;

  if Length(aParameters) <> 0 then
  begin
    Result := '(Error: Expected 0 params, got ' + Length(aParameters).ToString + ')';
  end;

  if aValue.AsObject is TDataSet then
  begin
    Result := TDataSet(aValue.AsObject).RecordCount;
  end
  else if aValue.AsObject is TJsonArray then
  begin
    Result := TJsonArray(aValue.AsObject).Count;
  end
  else if aValue.AsObject is TJsonObject then
  begin
    Result := TJsonObject(aValue.AsObject).Count;
  end
  else
  begin
    if (aValue.AsObject <> nil) and TDuckTypedList.CanBeWrappedAsList(aValue.AsObject, lWrappedList) then
    begin
      Result := lWrappedList.Count;
    end
    else
    begin
      Result := False;
    end;
  end;
end;

function DumpAsJSONString(const aValue: TValue; const aParameters: TArray<string>): TValue;
var
  lWrappedList: IMVCList;
begin
  if not aValue.IsObject then
  begin
    Result := '(Error: Cannot serialize non-object as JSON)';
  end;

  if TDuckTypedList.CanBeWrappedAsList(aValue.AsObject, lWrappedList) then
  begin
    Result := GetDefaultSerializer.SerializeCollection(lWrappedList)
  end
  else
  begin
    if aValue.AsObject is TDataSet then
      Result := GetDefaultSerializer.SerializeDataSet(TDataSet(aValue.AsObject))
    else
      Result := GetDefaultSerializer.SerializeObject(aValue.AsObject);
  end;
end;


function MakeMethodJSON: IInvokable;
begin
  Result :=
    MakeInvokable(function(Args: TArray<IValue>): IValue
    begin
      Result := TValueWrapper.Create(DumpAsJSONString(Args[0].GetValue.AsObject, []));
    end);
end;

procedure TMVCWebStencilsViewEngine.OnGetFile(Sender: TObject; const AFilename: string; var AText: string; var AHandled: Boolean);
var
  lFName: String;
begin
  AHandled := False;
  if TPath.IsRelativePath(AFilename) then
  begin
    lFName := TPath.Combine(FViewPath, AFilename);
    lFName := TPath.ChangeExtension(lfname, FDefaultViewFileExtension);
    lFName := TPath.Combine(AppPath, lfname);
    AText := TFile.ReadAllText(lfname);
    AHandled := True;
  end;
end;

procedure TMVCWebStencilsViewEngine.OnGetValue(Sender: TObject; const AObjectName, APropName: string; var AReplaceText: string; var AHandled: Boolean);
var
  lValue: TValue;
begin
  AHandled := False;
  if (ViewModel <> nil) and ViewModel.TryGetValue(AObjectName, lValue) then
  begin
    AReplaceText := GetTValueVarAsString(lValue, AObjectName, TWebStencilsProcessor(Sender));
    AHandled := True;
  end
  else
  begin
    AReplaceText := '';
    AHandled := True;
  end;
end;

procedure TMVCWebStencilsViewEngine.RegisterWSFunctions(WSProcessor: TWebStencilsProcessor);
begin
  if gFunctionInitialized then Exit;
  TMonitor.Enter(gWSLock);
  try
    if gFunctionInitialized then Exit;
    gFunctionInitialized := True;

    TBindingMethodsFactory.RegisterMethod(
     TMethodDescription.Create(
      MakeInvokable(function(Args: TArray<IValue>): IValue
      begin
        if Length(Args) <> 1 then
        begin
          raise EMVCSSVException.Create(500, 'Expected 1 parameter in "JSON" function, got ' + Length(Args).ToString);
        end;
        Result := TValueWrapper.Create(DumpAsJSONString(Args[0].GetValue.AsObject, []));
      end) as IInvokable,
      'json', 'json', '', True, 'Serialize an object to JSON', nil));

    TBindingMethodsFactory.RegisterMethod(
     TMethodDescription.Create(
      MakeInvokable(function(Args: TArray<IValue>): IValue
      begin
        if Length(Args) <> 1 then
        begin
          raise EWebStencilsException.Create('Expected 1 parameter, got ' + Length(Args).ToString);
        end;
        Result := TValueWrapper.Create(TMVCWebStencilsViewEngine.GetTValueVarAsString(Args[0].GetValue, '', nil));
      end),
      'ValueOf', 'ValueOf', '', True, 'ValueOf returns the inner value of a nullable as string - the non-nullable types are returned as-is', nil));

    TBindingMethodsFactory.RegisterMethod(
     TMethodDescription.Create(
      MakeInvokable(function(Args: TArray<IValue>): IValue
      begin
        if Length(Args) <> 1 then
        begin
          raise EWebStencilsException.Create('Expected 1 parameter, got ' + Length(Args).ToString);
        end;
        if (ViewModel <> nil) and ViewModel.ContainsKey(Args[0].GetValue.AsString) then
          Result := TValueWrapper.Create(True)
        else
          Result := TValueWrapper.Create(False);
      end),
      'Defined', 'Defined', '', True, 'Defined returns true if variable is defined', nil));


  finally
    TMonitor.Exit(gWSLock);
  end;
end;

procedure TMVCWebStencilsViewEngine.Execute(const ViewName: string; const Builder: TStringBuilder);
var
  lViewFileName: string;
  lWebStencilsProcessor: TWebStencilsProcessor;
  lPair: TPair<String, TValue>;
begin
  lViewFileName := GetRealFileName(ViewName);
  if lViewFileName.IsEmpty then
    raise EMVCSSVException.CreateFmt('View [%s] not found', [ViewName]);

  lWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
  try
    RegisterWSFunctions(lWebStencilsProcessor);
    try
      if Assigned(TMVCWebStencilsConfiguration.fOnProcessorConfiguration) then
      begin
        TMVCWebStencilsConfiguration.OnProcessorConfiguration(lWebStencilsProcessor);
      end;
      lWebStencilsProcessor.OnValue := OnGetValue;
      lWebStencilsProcessor.InputFileName := lViewFileName;
      lWebStencilsProcessor.PathTemplate := Config[TMVCConfigKey.ViewPath];
      lWebStencilsProcessor.WebRequest := WebContext.Request.RawWebRequest;
      lWebStencilsProcessor.OnFile := OnGetFile;

      if Assigned(ViewModel) then
      begin
        for lPair in ViewModel do
        begin
          if ViewModel[lPair.Key].IsObject then
            lWebStencilsProcessor.AddVar(lPair.Key, ViewModel[lPair.Key].AsObject, False);
        end;
      end;
      if Assigned(WebContext.LoggedUser) then
      begin
        lWebStencilsProcessor.UserLoggedIn := True;
        lWebStencilsProcessor.UserRoles := WebContext.LoggedUser.Roles.ToString;
      end;
      if Assigned(FBeforeRenderCallback) then
      begin
        FBeforeRenderCallback(lWebStencilsProcessor);
      end;
      Builder.Append(lWebStencilsProcessor.Content);
    except
      on E: EWebStencilsException do
      begin
        raise EMVCViewError.CreateFmt('View [%s] error: %s (%s)',
          [ViewName, E.Message, E.ClassName]);
      end;
    end;
  finally
    lWebStencilsProcessor.Free;
  end;
end;

class function TMVCWebStencilsViewEngine.GetTValueVarAsString(const Value: TValue; const VarName: string; const Processor: TWebStencilsProcessor): String;
var
  lIsObject: Boolean;
  lAsObject: TObject;
  lNullableInt32: NullableInt32;
  lNullableUInt32: NullableUInt32;
  lNullableInt16: NullableInt16;
  lNullableUInt16: NullableUInt16;
  lNullableInt64: NullableInt64;
  lNullableUInt64: NullableUInt64;
  lNullableCurrency: NullableCurrency;
  lNullableBoolean: NullableBoolean;
  lNullableTDate: NullableTDate;
  lNullableTTime: NullableTTime;
  lNullableTDateTime: NullableTDateTime;
begin
  if Value.IsEmpty then
  begin
    Exit('');
  end;

  lIsObject := False;
  lAsObject := nil;
  if Value.IsObject then
  begin
    lIsObject := True;
    lAsObject := Value.AsObject;
  end;

  if lIsObject then
  begin
    if lAsObject is TField then
      Result := TField(Value.AsObject).AsString
    else if lAsObject is TJsonBaseObject then
      Result := TJsonBaseObject(lAsObject).ToJSON()
    else
      Result := lAsObject.ToString;
  end
  else
  begin
    if Value.TypeInfo.Kind = tkRecord then
    begin
      Result := '';
      if Value.TypeInfo = TypeInfo(NullableInt32) then
      begin
        lNullableInt32 := Value.AsType<NullableInt32>;
        if lNullableInt32.HasValue then
          Result := lNullableInt32.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableUInt32) then
      begin
        lNullableUInt32 := Value.AsType<NullableUInt32>;
        if lNullableUInt32.HasValue then
          Result := lNullableUInt32.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableInt16) then
      begin
        lNullableInt16 := Value.AsType<NullableInt16>;
        if lNullableInt16.HasValue then
          Result := lNullableInt16.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableUInt16) then
      begin
        lNullableUInt16 := Value.AsType<NullableUInt16>;
        if lNullableUInt16.HasValue then
          Result := lNullableUInt16.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableInt64) then
      begin
        lNullableInt64 := Value.AsType<NullableInt64>;
        if lNullableInt64.HasValue then
          Result := lNullableInt64.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableUInt64) then
      begin
        lNullableUInt64 := Value.AsType<NullableUInt64>;
        if lNullableUInt64.HasValue then
          Result := lNullableUInt64.Value.ToString
      end
      else if Value.TypeInfo = TypeInfo(NullableString) then
      begin
        Result := Value.AsType<NullableString>.ValueOrDefault;
      end
      else if Value.TypeInfo = TypeInfo(NullableCurrency) then
      begin
        lNullableCurrency := Value.AsType<NullableCurrency>;
        if lNullableCurrency.HasValue then
          Result := FloatToStr(lNullableCurrency.Value);
          //Result := FloatToStr(lNullableCurrency.Value, fLocaleFormatSettings);
      end
      else if Value.TypeInfo = TypeInfo(NullableBoolean) then
      begin
        lNullableBoolean := Value.AsType<NullableBoolean>;
        if lNullableBoolean.HasValue then
          Result := BoolToStr(lNullableBoolean.Value, True);
      end
      else if Value.TypeInfo = TypeInfo(NullableTDate) then
      begin
        lNullableTDate := Value.AsType<NullableTDate>;
        if lNullableTDate.HasValue then
          Result := DateToISO8601(lNullableTDate.Value);
      end
      else if Value.TypeInfo = TypeInfo(NullableTTime) then
      begin
        lNullableTTime := Value.AsType<NullableTTime>;
        if lNullableTTime.HasValue then
          Result := DateToISO8601(lNullableTTime.Value);
      end
      else if Value.TypeInfo = TypeInfo(NullableTDateTime) then
      begin
        lNullableTDateTime := Value.AsType<NullableTDateTime>;
        if lNullableTDateTime.HasValue then
          Result := DateToISO8601(lNullableTDateTime.Value);
      end
      else
      begin
        raise EWebStencilsException.Create('Unsupported type for variable "' + VarName + '"');
      end;
    end
    else
    begin
      case Value.Kind of
        tkInteger: Result := Value.AsInteger.ToString;
        tkInt64: Result := Value.AsInt64.ToString;
        tkString, tkUString, tkWString, tkLString: Result := Value.AsString;
        tkWChar, tkChar: Result := Value.AsType<Char>;
        tkFloat: begin
          if Value.TypeInfo.Name = 'TDate' then
          begin
            //Result := DateToStr(Value.AsExtended, fLocaleFormatSettings);
            Result := DateToStr(Value.AsExtended);
          end
          else if Value.TypeInfo.Name = 'TDateTime' then
          begin
            //Result := DateTimeToStr(Value.AsExtended, fLocaleFormatSettings);
            Result := DateTimeToStr(Value.AsExtended);
          end
          else
          begin
            //Result := FloatToStr(Value.AsExtended, fLocaleFormatSettings);
            Result := FloatToStr(Value.AsExtended);
          end;
        end;
        tkEnumeration: Result := Value.ToString.ToLower;
        else
          raise EWebStencilsException.Create('Unsupported type for variable "' + VarName + '"');
      end;
    end;
  end;

end;


initialization

gWSLock := TObject.Create;

finalization

FreeAndNil(gWSLock);

{$endif}

end.
