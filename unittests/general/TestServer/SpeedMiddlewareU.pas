// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit SpeedMiddlewareU;

interface

uses
  MVCFramework;

type
  TMVCSpeedMiddleware = class(TInterfacedObject, IMVCMiddleware)
  public
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string;
	  const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string;
      var Handled: Boolean);
    procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  end;

implementation

uses
  MVCFramework.Serializer.Commons, System.SysUtils, DateUtils;

type
  ISpeedData = interface
    ['{72B0C1CA-00D1-431F-B093-C91A90147F90}']
    procedure SetData(value: TDateTime);
    function GetData: TDateTime;
  end;

  TSpeedData = class(TInterfacedObject, ISpeedData)
  private
    fData: TDateTime;
  protected
    procedure SetData(value: TDateTime);
    function GetData: TDateTime;
  public
    destructor Destroy; override;
  end;

  { TMVCSpeedMiddleware }

procedure TMVCSpeedMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName: string; const AActionNAme: string;
  const Handled: Boolean);
begin
  // Context.Response.CustomHeaders.Values['request_gen_time'] :=
  // MilliSecondsBetween(Now, ISOTimeStampToDateTime(Context.Data[classname + 'startup'])).ToString
  Context.Response.CustomHeaders.Values['request_gen_time'] :=
    MilliSecondsBetween(Now, ISOTimeStampToDateTime(Context.Data['start_req_timestamp'])).ToString;
end;

procedure TMVCSpeedMiddleware.OnAfterRouting(AContext: TWebContext;
  const AHandled: Boolean);
begin

end;

procedure TMVCSpeedMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string;
  var Handled: Boolean);
begin

end;

procedure TMVCSpeedMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin
  if Context.Request.PathInfo = '/handledbymiddleware' then
  begin
    Handled := True;
    Context.Response.RawWebResponse.Content := 'This is a middleware response';
    Context.Response.StatusCode := 200;
  end
  else
  begin
    Context.Data['start_req_timestamp'] := DateTimeToISOTimeStamp(now);
  end;
end;

{ TSpeedData }

destructor TSpeedData.Destroy;
begin

  inherited;
end;

function TSpeedData.GetData: TDateTime;
begin
  Result := fData;
end;

procedure TSpeedData.SetData(value: TDateTime);
begin
  fData := value;
end;

end.
