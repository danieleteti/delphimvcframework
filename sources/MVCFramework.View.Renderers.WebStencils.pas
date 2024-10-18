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
  MVCFramework.Commons, System.IOUtils, System.Classes, Web.Stencils;

type
  TMVCWebStencilsEvent = reference to procedure(const WebStencilsProcessor: TWebStencilsProcessor);

  { This class implements the WebStencils view engine for server side views }
  TMVCWebStencilsViewEngine = class(TMVCBaseViewEngine)
  public
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
  System.Rtti,
  JsonDataObjects;

{$WARNINGS OFF}

var
  gFunctionInitialized: Boolean = False;
  gWSLock: TObject = nil;

function GetDataSetOrObjectListCount(const aValue: TValue; const aParameters: TArray<string>): TValue;
var
  lWrappedList: IMVCList;
begin
  if not aValue.IsObject then
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
  Result := MakeInvokable(function(Args: TArray<IValue>): IValue
      begin
        Result := TValueWrapper.Create(DumpAsJSONString(Args[0].GetValue.AsObject, []));
      end)
end;

procedure RegisterWSFunctions(WSProcessor: TWebStencilsProcessor);
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
      //lWebStencilsProcessor.OnFile := Self.OnFile; {12.2, any filename starting with ..\ is not read correctly by the parser. Is it a feature? }
      lWebStencilsProcessor.InputFileName := lViewFileName;
      lWebStencilsProcessor.PathTemplate := Config[TMVCConfigKey.ViewPath];
      if Assigned(ViewModel) then
      begin
        for lPair in ViewModel do
        begin
          if ViewModel[lPair.Key].IsObject then
            lWebStencilsProcessor.AddVar(lPair.Key, ViewModel[lPair.Key].AsObject, False);
        end;
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

initialization

gWSLock := TObject.Create;

finalization

FreeAndNil(gWSLock);

{$endif}

end.
