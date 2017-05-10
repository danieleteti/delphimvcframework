// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2017 Daniele Teti and the DMVCFramework Team
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
  MVCFramework.Commons,
  MVCFramework.TypesAliases;

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
    [MVCHTTPMethods([httpGET, httpPOST])]
    [MVCDoc('Describe controllers and actions published by the RESTful server per resources')]
    procedure DescribeServer(AContext: TWebContext);

    [MVCPath('/describeplatform.info')]
    [MVCDoc('Describe the system where server is running')]
    procedure DescribePlatform(AContext: TWebContext);

    [MVCPath('/serverconfig.info')]
    [MVCDoc('Server configuration')]
    procedure ServerConfig(AContext: TWebContext);
  end;

implementation

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

procedure TMVCSystemController.DescribePlatform(AContext: TWebContext);
var
  Jo: TJSONObject;
begin
  Jo := TJSONObject.Create;
  try
    Jo.AddPair('OS', TOSVersion.ToString);
    Jo.AddPair('CPU_count', TJSONNumber.Create(TThread.ProcessorCount));
    Jo.AddPair('CPU_architecture', GetEnumName(TypeInfo(TOSVersion.TArchitecture), Ord(TOSVersion.Architecture)));
    Jo.AddPair('system_time', FormatDateTime('YYYY-MM-DD HH:NN:SS', Now));
    ContentType := TMVCMediaType.APPLICATION_JSON;
    Render(Jo, False);
  finally
    Jo.Free;
  end;
end;

procedure TMVCSystemController.DescribeServer(AContext: TWebContext);
var
  LJoResp: TJSONObject;
  LController: TMVCControllerDelegate;
  LJoControllerInfo: TJSONObject;
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
begin
  LRttiCtx := TRttiContext.Create;
  try
    LJoResp := TJSONObject.Create;
    try
      for LController in Engine.Controllers do
      begin
        LJoControllerInfo := TJSONObject.Create;
        LJoResp.AddPair(LController.Clazz.QualifiedClassName, LJoControllerInfo);

        LRttiType := LRttiCtx.GetType(LController.Clazz) as TRttiInstanceType;
        for LAttribute in LRttiType.GetAttributes do
        begin
          if LAttribute is MVCPathAttribute then
            LJoControllerInfo.AddPair('resource_path', MVCPathAttribute(LAttribute).Path);
          if LAttribute is MVCDocAttribute then
            LJoControllerInfo.AddPair('description', MVCDocAttribute(LAttribute).Value);
        end;

        LJaMethods := TJSONArray.Create;
        LJoControllerInfo.AddPair('actions', LJaMethods);
        LMethods := LRttiType.GetDeclaredMethods;
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
            LJoMethod := TJSONObject.Create;
            LJoMethod.AddPair('action_name', LMethod.Name);
            LJoMethod.AddPair('relative_path', LStrRelativePath);
            LJoMethod.AddPair('consumes', LStrConsumes);
            LJoMethod.AddPair('produces', LStrProduces);
            LJoMethod.AddPair('http_methods', LStrHTTPMethods);
            LJoMethod.AddPair('description', LStrDoc);
            LJaMethods.AddElement(LJoMethod);
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
  AHandled := not((ClientIp = '::1') or (ClientIp = '127.0.0.1') or (ClientIp = '0:0:0:0:0:0:0:1') or (ClientIp.ToLower = 'localhost'));
end;

procedure TMVCSystemController.ServerConfig(AContext: TWebContext);
var
  Keys: TArray<string>;
  Key: string;
  Jo: TJSONObject;
begin
  Jo := TJSONObject.Create;
  try
    Keys := Config.Keys;
    for Key in Keys do
      Jo.AddPair(Key, Config[Key]);
    ContentType := TMVCMediaType.APPLICATION_JSON;
    Render(Jo, False);
  finally
    Jo.Free;
  end;
end;

end.
