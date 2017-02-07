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

interface

{$I dmvcframework.inc}


uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/system')]
  [MVCDoc('Built-in DelphiMVCFramework System controller')]
  TMVCSystemController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    function GetUpTime: string;
  public
    [MVCPath('/describeserver.info')]
    [MVCHTTPMethods([httpGET, httpPOST])]
    [MVCDoc('Describe controllers and actions published by the RESTful server per resources')
      ]
    procedure DescribeServer(Context: TWebContext);

    [MVCPath('/describeplatform.info')]
    [MVCDoc('Describe the system where server is running')]
    procedure DescribePlatform(Context: TWebContext);

    [MVCPath('/serverconfig.info')]
    [MVCDoc('Server configuration')]
    procedure ServerConfig(Context: TWebContext);
  end;

implementation

uses
  System.SysUtils
    , System.Rtti
    , System.Classes
    , Winapi.Windows
    , System.TypInfo
{$IFDEF SYSTEMJSON} // XE6
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

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

procedure TMVCSystemController.DescribePlatform(Context: TWebContext);
var
  LJRes: TJSONObject;
begin
  LJRes := TJSONObject.Create;
  try
    LJRes.AddPair('OS', TOSVersion.ToString);
    LJRes.AddPair('CPU_count', TJSONNumber.Create(TThread.ProcessorCount));
    LJRes.AddPair('CPU_architecture',
      GetEnumName(TypeInfo(TOSVersion.TArchitecture),
      Ord(TOSVersion.Architecture)));
    // LJRes.AddPair('system_uptime', GetUpTime);
    LJRes.AddPair('system_time', FormatDateTime('YYYY-MM-DD HH:NN:SS', Now));
    ContentType := TMVCMediaType.APPLICATION_JSON;
    Render(LJRes, False);
  finally
    LJRes.Free;
  end;
end;

procedure TMVCSystemController.DescribeServer(Context: TWebContext);
var
  LJResp: TJSONObject;
  LControllerRoutable: TMVCControllerRoutable;
  ControllerInfo: TJSONObject;
  LRTTIType: TRttiInstanceType;
  LCTX: TRttiContext;
  LAttribute: TCustomattribute;
  LJMethods: TJSONArray;
  LMethods: TArray<TRttiMethod>;
  LMethod: TRttiMethod;
  LFoundAttrib: Boolean;
  LStrRelativePath: string;
  LStrHTTPMethods: string;
  LStrDoc: string;
  LStrConsumes: string;
  LStrProduces: string;
  LJMethod: TJSONObject;
begin
  LCTX := TRttiContext.Create;
  try
    LJResp := TJSONObject.Create;
    try
      for LControllerRoutable in GetMVCEngine.RegisteredControllers do
      begin
        ControllerInfo := TJSONObject.Create;
        LJResp.AddPair(LControllerRoutable.&Class.QualifiedClassName,
          ControllerInfo);

        LRTTIType := LCTX.GetType(LControllerRoutable.&Class)
          as TRttiInstanceType;
        for LAttribute in LRTTIType.GetAttributes do
        begin
          if LAttribute is MVCPathAttribute then
            ControllerInfo.AddPair('resource_path',
              MVCPathAttribute(LAttribute).Path);
          if LAttribute is MVCDocAttribute then
            ControllerInfo.AddPair('description',
              MVCDocAttribute(LAttribute).Value);
        end;

        LJMethods := TJSONArray.Create;
        ControllerInfo.AddPair('actions', LJMethods);
        LMethods := LRTTIType.GetDeclaredMethods;
        for LMethod in LMethods do
        begin
          LFoundAttrib := False;
          LStrRelativePath := '';
          LStrHTTPMethods := '';
          LStrConsumes := '';
          LStrProduces := '';
          LStrDoc := '';
          LStrHTTPMethods :=
            'httpGET,httpPOST,httpPUT,httpDELETE,httpHEAD,httpOPTIONS,httpPATCH,httpTRACE';
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
              LStrHTTPMethods := MVCHTTPMethodAttribute(LAttribute)
                .MVCHTTPMethodsAsString;
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
            LJMethod := TJSONObject.Create;
            LJMethod.AddPair('action_name', LMethod.Name);
            LJMethod.AddPair('relative_path', LStrRelativePath);
            LJMethod.AddPair('consumes', LStrConsumes);
            LJMethod.AddPair('produces', LStrProduces);
            LJMethod.AddPair('http_methods', LStrHTTPMethods);
            LJMethod.AddPair('description', LStrDoc);
            LJMethods.AddElement(LJMethod);
          end;
        end;
      end;
      ContentType := TMVCMediaType.APPLICATION_JSON;
      Render(LJResp, False);
    finally
      LJResp.Free;
    end;
  finally
    LCTX.Free;
  end;
end;

function TMVCSystemController.GetUpTime: string;
begin
  Result := MSecToTime(GetTickCount);
end;

procedure TMVCSystemController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
var
  LClientIP: string;
begin
  inherited;
  LClientIP := Context.Request.ClientIP;
  Handled := not((LClientIP = '::1') or (LClientIP = '127.0.0.1') or
    (LClientIP = '0:0:0:0:0:0:0:1') or (LClientIP.ToLower = 'localhost'));
end;

procedure TMVCSystemController.ServerConfig(Context: TWebContext);
var
  LKeys: TArray<string>;
  LKey: string;
  LJRes: TJSONObject;
begin
  ContentType := TMVCMediaType.APPLICATION_JSON;

  LJRes := TJSONObject.Create;
  try
    LKeys := Config.Keys;
    for LKey in LKeys do
    begin
      LJRes.AddPair(LKey, Config[LKey]);
    end;
  except
    LJRes.Free;
    raise;
  end;
  Render(LJRes, true);
end;

end.
