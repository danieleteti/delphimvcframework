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
// *************************************************************************** }

unit MVCFramework.SysControllers;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.DateUtils,
  System.Rtti,
  MVCFramework,
  MVCFramework.Commons, MVCFramework.Swagger.Commons;

const
  DESCRIBE_PLATFORM_RESPONSE_SWAGGER_SCHEMA =
    '{' + sLineBreak +
    '    "type": "object",' + sLineBreak +
    '    "properties": {' + sLineBreak +
    '        "OS": {' + sLineBreak +
    '            "type": "string",' + sLineBreak +
    '            "description": "Operating System Information"' + sLineBreak +
    '        },' + sLineBreak +
    '        "CPU_count": {' + sLineBreak +
    '            "type": "integer",' + sLineBreak +
    '            "description": "Numbers of cores available "' + sLineBreak +
    '        },' + sLineBreak +
    '        "CPU_architecture": {' + sLineBreak +
    '            "type": "string",' + sLineBreak +
    '            "description": "CPU''s Architecture"' + sLineBreak +
    '        },' + sLineBreak +
    '        "system_time": {' + sLineBreak +
    '            "type": "string",' + sLineBreak +
    '            "description": "Server timestamp"' + sLineBreak +
    '        }' + sLineBreak +
    '    }' + sLineBreak +
    '}';


type

  [MVCPath('/system')]
  [MVCDoc('Built-in DelphiMVCFramework System Controller')]
  TMVCSystemController = class(TMVCController)
  private
    { private declarations }
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
    function GetUpTime: string;
  public
    [MVCPath('/describeserver.info')]
    [MVCHTTPMethods([httpGET])]
    [MVCDoc('Describe controllers and actions published by the RESTful server per resources')]
    [MVCSwagSummary('DMVCFramework System Controller', 'Describe controllers and actions published by the RESTful server per resources')]
    [MVCSwagResponses(HTTP_STATUS.InternalServerError, 'Internal server error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.OK, 'OK')]
    procedure DescribeServer;

    [MVCPath('/describeplatform.info')]
    [MVCDoc('Describe the system where server is running')]
    [MVCHTTPMethods([httpGET])]
    [MVCSwagSummary('DMVCFramework System Controller', 'Describe the system where server is running')]
    [MVCSwagResponses(HTTP_STATUS.InternalServerError, 'Internal server error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.OK, 'OK', DESCRIBE_PLATFORM_RESPONSE_SWAGGER_SCHEMA)]
    procedure DescribePlatform;

    [MVCPath('/serverconfig.info')]
    [MVCHTTPMethods([httpGET])]
    [MVCDoc('Server configuration')]
    [MVCSwagSummary('DMVCFramework System Controller', 'Server configuration')]
    [MVCSwagResponses(HTTP_STATUS.InternalServerError, 'Internal server error', TMVCErrorResponse)]
    [MVCSwagResponses(HTTP_STATUS.OK, 'OK')]
    procedure ServerConfig;
  end;

implementation

uses
  JsonDataObjects, MVCFramework.ActiveRecord;

function MSecToTime(mSec: Int64): string;
const
  secondTicks = 1000;
  minuteTicks = 1000 * 60;
  hourTicks = 1000 * 60 * 60;
  dayTicks = 1000 * 60 * 60 * 24;
var
  D, H, M, S: string;
  ZD, ZH, ZM, ZS: Integer;
begin
  ZD := mSec div dayTicks;
  Dec(mSec, ZD * dayTicks);
  ZH := mSec div hourTicks;
  Dec(mSec, ZH * hourTicks);
  ZM := mSec div minuteTicks;
  Dec(mSec, ZM * minuteTicks);
  ZS := mSec div secondTicks;
  D := IntToStr(ZD);
  H := IntToStr(ZH);
  M := IntToStr(ZM);
  S := IntToStr(ZS);
  Result := D + '.' + H + ':' + M + ':' + S;
end;

{ TMVCSystemController }

procedure TMVCSystemController.DescribePlatform;
var
  Jo: TJSONObject;
begin
  Jo := TJSONObject.Create;
  try
    Jo.S['OS'] := TOSVersion.ToString;
    Jo.I['CPU_count'] := TThread.ProcessorCount;
    Jo.S['CPU_architecture'] := GetEnumName(TypeInfo(TOSVersion.TArchitecture), Ord(TOSVersion.Architecture));
    Jo.DUtc['system_time'] := Now;
    ContentType := TMVCMediaType.APPLICATION_JSON;
    Render(Jo, False);
  finally
    Jo.Free;
  end;
end;

procedure TMVCSystemController.DescribeServer;
var
  LJoResp: TJSONObject;
  LController: TMVCControllerDelegate;
  LRttiType: TRttiInstanceType;
  LRttiCtx: TRttiContext;
  LAttribute: TCustomattribute;
  LJaMethods: TJSONArray;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LFoundAttrib: Boolean;
  LStrRelativePath: string;
  LStrHTTPMethods: string;
  LStrDoc: string;
  LStrConsumes: string;
  LStrProduces: string;
  LJoMethod: TJSONObject;
  lControllerClassName: string;
begin
  LRttiCtx := TRttiContext.Create;
  try
    LJoResp := TJSONObject.Create;
    try
      for LController in Engine.Controllers do
      begin
        lControllerClassName := LController.Clazz.QualifiedClassName;
        if lControllerClassName.EndsWith('TMVCActiveRecordController') then
        begin
          LJoResp.O[lControllerClassName].S['description'] := 'Automatic CRUD API for entities: ' +
            String.Join(',', ActiveRecordMappingRegistry.GetEntities);
        end;

        LRttiType := LRttiCtx.GetType(LController.Clazz) as TRttiInstanceType;
        for LAttribute in LRttiType.GetAttributes do
        begin
          if LAttribute is MVCPathAttribute then
            LJoResp.O[lControllerClassName].S['resource_path'] := MVCPathAttribute(LAttribute).Path;
          if LAttribute is MVCDocAttribute then
            LJoResp.O[lControllerClassName].S['description'] := MVCDocAttribute(LAttribute).Value;
        end;

        if not LController.URLSegment.IsEmpty then
        begin
          LJoResp.O[lControllerClassName].S['resource_path'] := LController.URLSegment;
        end;

        LJaMethods := LJoResp.O[lControllerClassName].A['actions']; // TJSONArray.Create;
        // LJoControllerInfo.AddPair('actions', LJaMethods);
        LMethods := LRttiType.GetMethods;
        for LMethod in LMethods do
        begin
          LFoundAttrib := False;
          LStrRelativePath := '';
          LStrHTTPMethods := '';
          LStrConsumes := '';
          LStrProduces := '';
          LStrDoc := '';
          LStrHTTPMethods := 'httpGET,httpPOST,httpPUT,httpDELETE,httpHEAD,httpOPTIONS,httpPATCH,httpTRACE';
          for LAttribute in LMethod.GetAttributes do
          begin
            if LAttribute is MVCDocAttribute then
            begin
              LStrDoc := MVCDocAttribute(LAttribute).Value;
              LFoundAttrib := true;
            end;
            if LAttribute is MVCPathAttribute then
            begin
              LStrRelativePath := MVCPathAttribute(LAttribute).Path;
              LFoundAttrib := true;
            end;
            if LAttribute is MVCHTTPMethodAttribute then
            begin
              LStrHTTPMethods := MVCHTTPMethodAttribute(LAttribute).MVCHTTPMethodsAsString;
              LFoundAttrib := true;
            end;
            if LAttribute is MVCConsumesAttribute then
            begin
              LStrConsumes := MVCConsumesAttribute(LAttribute).Value;
              LFoundAttrib := true;
            end;
            if LAttribute is MVCProducesAttribute then
            begin
              LStrProduces := MVCProducesAttribute(LAttribute).Value;
              LFoundAttrib := true;
            end;
          end;

          if LFoundAttrib then
          begin
            LJoMethod := LJaMethods.AddObject;
            LJoMethod.S['action_name'] := LMethod.Name;
            LJoMethod.S['relative_path'] := LStrRelativePath;
            LJoMethod.S['consumes'] := LStrConsumes;
            LJoMethod.S['produces'] := LStrProduces;
            LJoMethod.S['http_methods'] := LStrHTTPMethods;
            LJoMethod.S['description'] := LStrDoc;
          end;
        end;
      end;
      ContentType := TMVCMediaType.APPLICATION_JSON;
      Render(LJoResp, False);
    finally
      LJoResp.Free;
    end;
  finally
    LRttiCtx.Free;
  end;
end;

function TMVCSystemController.GetUpTime: string;
begin
  Result := MSecToTime(MilliSecondsBetween(Now, 0));
end;

procedure TMVCSystemController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean);
var
  ClientIp: string;
begin
  inherited;
  ClientIp := Context.Request.ClientIp;
  AHandled := not((ClientIp = '::1') or (ClientIp = '127.0.0.1') or (ClientIp = '0:0:0:0:0:0:0:1') or
    (ClientIp.ToLower = 'localhost'));
  if AHandled then
  begin
    AContext.Response.StatusCode := HTTP_STATUS.Forbidden;
  end;
end;

procedure TMVCSystemController.ServerConfig;
var
  Keys: TArray<string>;
  Key: string;
  Jo: TJSONObject;
begin
  Jo := TJSONObject.Create;
  try
    Keys := Config.Keys;
    for Key in Keys do
      Jo.S[Key] := Config[Key];
    ContentType := TMVCMediaType.APPLICATION_JSON;
    Render(Jo, False);
  finally
    Jo.Free;
  end;
end;

end.
