unit MVCFramework.SysControllers;

interface

uses
  MVCFramework;

type

  [MVCPath('/system')]
  TMVCSystemController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    function GetUpTime: string;
  public
    [MVCPath('/describeserver.info')]
    procedure DescribeServer(Context: TWebContext);

    [MVCPath('/describeplatform.info')]
    procedure DescribePlatform(Context: TWebContext);

    [MVCPath('/serverconfig.info')]
    procedure ServerConfig(Context: TWebContext);
  end;

implementation

uses
{$IF CompilerVErsion < 27}
  Data.DBXJSON,
{$ELSE}
  System.JSON,
{$IFEND}
  System.SysUtils, System.Rtti, MVCFramework.Commons, System.Classes,
  Winapi.Windows, System.TypInfo;

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
    LJRes.AddPair('CPU_architecture', GetEnumName(TypeInfo(TOSVersion.TArchitecture),
      Ord(TOSVersion.Architecture)));
    LJRes.AddPair('system_uptime', GetUpTime);
    ContentType := TMVCMimeType.APPLICATION_JSON;
    Render(LJRes, False);
  finally
    LJRes.Free;
  end;
end;

procedure TMVCSystemController.DescribeServer(Context: TWebContext);
var
  LJResp: TJSONObject;
  LController: TMVCControllerClass;
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
  LStrConsumes: string;
  LStrProduces: string;
  LJMethod: TJSONObject;
begin
  LJResp := TJSONObject.Create;
  try
    for LController in GetMVCEngine.RegisteredControllers do
    begin
      ControllerInfo := TJSONObject.Create;
      LJResp.AddPair(LController.QualifiedClassName, ControllerInfo);

      LRTTIType := LCTX.GetType(LController) as TRttiInstanceType;
      for LAttribute in LRTTIType.GetAttributes do
      begin
        if LAttribute is MVCPathAttribute then
          ControllerInfo.AddPair('resource_path', MVCPathAttribute(LAttribute).Path)
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
        for LAttribute in LMethod.GetAttributes do
        begin
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
          LJMethod := TJSONObject.Create;
          LJMethod.AddPair('action_name', LMethod.Name);
          LJMethod.AddPair('relative_path', LStrRelativePath);
          LJMethod.AddPair('consumes', LStrConsumes);
          LJMethod.AddPair('produces', LStrProduces);
          LJMethod.AddPair('http_methods', LStrHTTPMethods);
          LJMethods.AddElement(LJMethod);
        end;
      end;
    end;
    ContentType := TMVCMimeType.APPLICATION_JSON;
    Render(LJResp, False);
  finally
    LJResp.Free;
  end;
end;

function TMVCSystemController.GetUpTime: string;
begin
  Result := MSecToTime(GetTickCount);
end;

procedure TMVCSystemController.OnBeforeAction(Context: TWebContext; const AActionNAme: string;
  var Handled: Boolean);
begin
  inherited;
  if (Context.Request.ClientIP <> '127.0.0.1') and (Context.Request.ClientIP.ToLower <> 'localhost')
  then
    Handled := true;
end;

procedure TMVCSystemController.ServerConfig(Context: TWebContext);
var
  LKeys: TArray<string>;
  LKey: string;
  LJRes: TJSONObject;
begin
  ContentType := TMVCMimeType.APPLICATION_JSON;

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
