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

unit MVCFramework.Filters.Router;

interface

uses
  System.Generics.Collections,
  MVCFramework,
  MVCFramework.Commons, System.Rtti;

{$I dmvcframework.inc}

type
  TWebContextHack = class(TWebContext)

  end;

  TMVCControllerHack = class(TMVCController)

  end;

  TMVCRouterFilter = class(TCustomProtocolFilter)
  private
    fEngine: TMVCEngine;
    fConfig: TMVCConfig;
    fControllers: TObjectList<TMVCControllerDelegate>;
    fConfigCache_DefaultContentType: String;
    fConfigCache_DefaultContentCharset: String;
    fConfigCache_PathPrefix: String;
    fControllerFilterChain: IControllerFilterChain;
  protected
    procedure DoFilter(Context: TWebContext); override;
//    function GetActualParam(const AFormalParam: TRttiParameter; const AStringValue: String): TValue;
//    procedure FillActualParamsForAction(const ASelectedController: TMVCController;
//      const AContext: TWebContext; const AActionFormalParams: TArray<TRttiParameter>;
//      const AActionName: string; var AActualParams: TArray<TValue>; out ABodyParameter: TObject);
//    procedure HandleDefaultValueForInjectedParameter(var InjectedParamValue: String;
//      const InjectableParamAttribute: MVCInjectableParamAttribute);
  public
    constructor Create(
      const Engine: TMVCEngine;
      const Config: TMVCConfig;
      const Controllers: TObjectList<TMVCControllerDelegate>;
      const ConfigCache_DefaultContentType: String;
      const ConfigCache_DefaultContentCharset: String;
      const ConfigCache_PathPrefix: String;
      const ControllerFilterChain: IControllerFilterChain
      );
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  MVCFramework.Router,
  MVCFramework.Logger,
  Web.HTTPApp, MVCFramework.DuckTyping, MVCFramework.Rtti.Utils,
  MVCFramework.Serializer.Abstract, MVCFramework.Serializer.Commons;

{ TMVCRouterFilter }

constructor TMVCRouterFilter.Create(
  const Engine: TMVCEngine;
  const Config: TMVCConfig;
  const Controllers: TObjectList<TMVCControllerDelegate>;
  const ConfigCache_DefaultContentType: String;
  const ConfigCache_DefaultContentCharset: String;
  const ConfigCache_PathPrefix: String;
  const ControllerFilterChain: IControllerFilterChain
  );
begin
  inherited Create;
  fEngine := Engine;
  fConfig := Config;
  fControllers := Controllers;
  fConfigCache_DefaultContentType := ConfigCache_DefaultContentType;
  fConfigCache_DefaultContentCharset := ConfigCache_DefaultContentCharset;
  fConfigCache_PathPrefix := ConfigCache_PathPrefix;
  fControllerFilterChain := ControllerFilterChain;
end;

procedure TMVCRouterFilter.DoFilter(Context: TWebContext);
var
  lRouter: IMVCRouter;
  lParamsTable: TMVCRequestParamsTable;
begin
  {TODO -odanielet -cGeneral : ParamsTable in an objectpool?}
  lParamsTable := TMVCRequestParamsTable.Create;
  try
    lRouter := TMVCRouter.Create(FConfig, fEngine.gMVCGlobalActionParamsCache);
    try
        if lRouter.TryFindRoute(
          Context.Request.PathInfo,
          Context.Request.GetOverwrittenHTTPMethod,
          Context.Request.ContentType,
          Context.Request.Accept,
          fControllers,
          fConfigCache_DefaultContentType,
          fConfigCache_DefaultContentCharset,
          fConfigCache_PathPrefix,
          lParamsTable) then
        begin
          MVCFramework.Logger.InitThreadVars;
          Context.ParamsTable := lParamsTable;
          fControllerFilterChain.Execute(Context, lRouter);
        end
        else // execute-routing
        begin
          if fConfig[TMVCConfigKey.AllowUnhandledAction] = 'false' then
          begin
            Context.Response.StatusCode := http_status.NotFound;
            Context.Response.ReasonString := 'Not Found';
            //fOnRouterLog(lRouter, rlsRouteNotFound, lContext);
            //raise EMVCRouteNotFound.CreateFmt('"%s %s" - not found',[Context.Request.HTTPMethodAsString, Context.Request.PathInfo]);
          end
          else
          begin
            Context.Response.FlushOnDestroy := False;
          end;
        end; // end-execute-routing
    except
      on Ex: EMVCException do
      begin
        if not fEngine.CustomExceptionHandling(Ex, nil, Context) then
        begin
          Log.Error('[%s] %s [PathInfo "%s"] [%s]',
            [
              Ex.Classname,
              Ex.Message,
              GetRequestShortDescription(Context.Request.RawWebRequest),
              ClassName
            ],
            LOGGERPRO_TAG);
          fEngine.SendRawHTTPStatus(Context, Ex.HttpErrorCode,
            Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
        end;
      end;
      on Ex: EMVCRouteNotFound do
      begin
        if not fEngine.CustomExceptionHandling(Ex, nil, Context) then
        begin
          Log.Error('[%s] %s [PathInfo "%s"] [%s]',
            [
              Ex.Classname,
              Ex.Message,
              GetRequestShortDescription(Context.Request.RawWebRequest),
              ClassName
            ],
            LOGGERPRO_TAG);
          fEngine.SendRawHTTPStatus(
            Context,
            HTTP_STATUS.NotFound,
            Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
        end;
      end;
      on Ex: Exception do
      begin
        if not fEngine.CustomExceptionHandling(Ex, nil, Context) then
        begin
          Log.Error('[%s] %s [PathInfo "%s"] [%s]',
            [
              Ex.Classname,
              Ex.Message,
              GetRequestShortDescription(Context.Request.RawWebRequest),
              ClassName
            ],
            LOGGERPRO_TAG);
          fEngine.SendRawHTTPStatus(Context, http_status.InternalServerError,
            Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
        end;
      end;
    end;
  finally
    lParamsTable.Free;
  end;
end;

end.
