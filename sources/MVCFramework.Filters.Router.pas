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

  TMVCRouterFilter = class(TProtocolFilter)
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
  lSelectedController: TMVCController;
  lRouter: IMVCRouter;
  lParamsTable: TMVCRequestParamsTable;
begin
  lParamsTable := TMVCRequestParamsTable.Create;
  try
    lRouter := TMVCRouter.Create(FConfig, fEngine.gMVCGlobalActionParamsCache);
    lSelectedController := nil;
    try // only for lSelectedController
      try // global exception handler
          if lRouter.TryFindRoute(
            Context.Request.PathInfo,
            Context.Request.GetOverwrittenHTTPMethod { lContext.Request.HTTPMethod } ,
            Context.Request.ContentType,
            Context.Request.Accept,
            fControllers,
            fConfigCache_DefaultContentType,
            fConfigCache_DefaultContentCharset,
            fConfigCache_PathPrefix,
            lParamsTable) then
          begin
//            lRouterMethodToCallName := lRouter.ActionMethod.Name;
//            lRouterControllerClazzQualifiedClassName := lRouter.ControllerClazz.QualifiedClassName;
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
              raise EMVCException.Create(Context.Response.ReasonString, Context.Request.HTTPMethodAsString + ' ' +
                Context.Request.PathInfo, 0, http_status.NotFound);
            end
            else
            begin
              Context.Response.FlushOnDestroy := False;
            end;
          end; // end-execute-routing
      except
        on ESess: EMVCSessionExpiredException do
        begin
          if not fEngine.CustomExceptionHandling(ESess, lSelectedController, Context) then
          begin
            Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
              [ESess.Classname, ESess.Message, GetRequestShortDescription(Context.Request.RawWebRequest), ESess.DetailedMessage],
              LOGGERPRO_TAG);
            Context.SessionStop;
            lSelectedController.ResponseStatus(ESess.HTTPErrorCode);
            lSelectedController.Render(ESess);
          end;
        end;
        on E: EMVCException do
        begin
          if not fEngine.CustomExceptionHandling(E, lSelectedController, Context) then
          begin
            Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
              [E.Classname, E.Message, GetRequestShortDescription(Context.Request.RawWebRequest), E.DetailedMessage], LOGGERPRO_TAG);
            if Assigned(lSelectedController) then
            begin
              lSelectedController.ResponseStatus(E.HTTPErrorCode);
              lSelectedController.Render(E);
            end
            else
            begin
              fEngine.SendRawHTTPStatus(Context, E.HTTPErrorCode, Format('[%s] %s', [E.Classname, E.Message]), E.Classname);
            end;
          end;
        end;
        on EIO: EInvalidOp do
        begin
          if not fEngine.CustomExceptionHandling(EIO, lSelectedController, Context) then
          begin
            Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
              [EIO.Classname, EIO.Message, GetRequestShortDescription(Context.Request.RawWebRequest), 'Invalid Op'], LOGGERPRO_TAG);
            if Assigned(lSelectedController) then
            begin
              lSelectedController.ResponseStatus(http_status.InternalServerError);
              lSelectedController.Render(EIO);
            end
            else
            begin
              fEngine.SendRawHTTPStatus(Context, http_status.InternalServerError,
                Format('[%s] %s', [EIO.Classname, EIO.Message]), EIO.Classname);
            end;
          end;
        end;
        on Ex: Exception do
        begin
          if not fEngine.CustomExceptionHandling(Ex, lSelectedController, Context) then
          begin
            Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
              [Ex.Classname, Ex.Message, GetRequestShortDescription(Context.Request.RawWebRequest), 'Global Action Exception Handler'],
              LOGGERPRO_TAG);
            if Assigned(lSelectedController) then
            begin
              lSelectedController.ResponseStatus(http_status.InternalServerError);
              lSelectedController.Render(Ex);
            end
            else
            begin
              fEngine.SendRawHTTPStatus(Context, http_status.InternalServerError,
                Format('[%s] %s', [Ex.Classname, Ex.Message]), Ex.Classname);
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(lSelectedController);
    end;
  finally
    lParamsTable.Free;
  end;
end;

//procedure TMVCRouterFilter.FillActualParamsForAction(
//  const ASelectedController: TMVCController; const AContext: TWebContext;
//  const AActionFormalParams: TArray<TRttiParameter>; const AActionName: string;
//  var AActualParams: TArray<TValue>; out ABodyParameter: TObject);
//var
//  lParamName: string;
//  I: Integer;
//  lStrValue: string;
//  lFromBodyAttribute: MVCFromBodyAttribute;
//  lFromQueryStringAttribute: MVCFromQueryStringAttribute;
//  lFromHeaderAttribute: MVCFromHeaderAttribute;
//  lFromCookieAttribute: MVCFromCookieAttribute;
//  lAttributeInjectedParamCount: Integer;
//  lInjectedParamValue: string;
//  lList: IMVCList;
//  lItemClass: TClass;
//begin
//  ABodyParameter := nil;
//  lAttributeInjectedParamCount := 0;
//  SetLength(AActualParams, Length(AActionFormalParams));
//  for I := 0 to Length(AActionFormalParams) - 1 do
//  begin
//    lParamName := AActionFormalParams[I].name;
//    if Length(AActionFormalParams[I].GetAttributes) > 0 then
//    begin
//      // Let's check how to inject this parameter
//      if TRttiUtils.HasAttribute<MVCFromBodyAttribute>(AActionFormalParams[I], lFromBodyAttribute)
//      then
//      begin
//        Inc(lAttributeInjectedParamCount, 1);
//        if AActionFormalParams[I].ParamType.QualifiedName <> 'System.string' then
//        begin
//          ABodyParameter := TRttiUtils.CreateObject(AActionFormalParams[I].ParamType.QualifiedName);
//          if TDuckTypedList.CanBeWrappedAsList(ABodyParameter, lList) then
//          begin
//            lItemClass := TMVCAbstractSerializer(ASelectedController.Serializer).GetObjectTypeOfGenericList(ABodyParameter.ClassInfo);
//            ASelectedController.Serializer.DeserializeCollection(AContext.Request.Body,
//              ABodyParameter, lItemClass, stDefault, [], lFromBodyAttribute.RootNode);
//          end
//          else
//          begin
//            ASelectedController.Serializer.DeserializeObject(AContext.Request.Body,
//              ABodyParameter, stDefault, [], lFromBodyAttribute.RootNode);
//          end;
//          AActualParams[I] := ABodyParameter;
//        end
//        else
//        begin
//          AActualParams[I] := AContext.Request.Body;
//          Continue;
//        end;
//      end
//      else if TRttiUtils.HasAttribute<MVCFromQueryStringAttribute>(AActionFormalParams[I],
//        lFromQueryStringAttribute) then
//      begin
//        Inc(lAttributeInjectedParamCount, 1);
//        lInjectedParamValue := AContext.Request.QueryStringParam
//          (lFromQueryStringAttribute.ParamName);
//        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromQueryStringAttribute);
//        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
//      end
//      else if TRttiUtils.HasAttribute<MVCFromHeaderAttribute>(AActionFormalParams[I],
//        lFromHeaderAttribute) then
//      begin
//        Inc(lAttributeInjectedParamCount, 1);
//        lInjectedParamValue := AContext.Request.GetHeader(lFromHeaderAttribute.ParamName);
//        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromHeaderAttribute);
//        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
//      end
//      else if TRttiUtils.HasAttribute<MVCFromCookieAttribute>(AActionFormalParams[I],
//        lFromCookieAttribute) then
//      begin
//        Inc(lAttributeInjectedParamCount, 1);
//        lInjectedParamValue := AContext.Request.Cookie(lFromCookieAttribute.ParamName);
//        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromCookieAttribute);
//        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
//      end
//      else
//      begin
//        raise EMVCException.Create(http_status.InternalServerError,
//          'Unknown custom attribute on action parameter: ' + AActionFormalParams[I].name +
//          '. [HINT: Allowed attributes are MVCFromBody, MVCFromQueryString, MVCFromHeader, MVCFromCookie]');
//      end;
//      Continue;
//    end;
//
//    // From now on we'll check for url mapped parameters
//    if not AContext.Request.SegmentParam(lParamName, lStrValue) then
//      raise EMVCException.CreateFmt(http_status.BadRequest,
//        'Invalid parameter %s for action %s (Hint: Here parameters names are case-sensitive)',
//        [lParamName, AActionName]);
//    AActualParams[I] := GetActualParam(AActionFormalParams[I], lStrValue);
//  end;
//
//  if (AContext.Request.SegmentParamsCount + lAttributeInjectedParamCount) <>
//    Length(AActionFormalParams) then
//    raise EMVCException.CreateFmt(http_status.BadRequest,
//      'Parameters count mismatch (expected %d actual %d) for action "%s"',
//      [Length(AActionFormalParams), AContext.Request.SegmentParamsCount, AActionName]);
//end;

//procedure TMVCRouterFilter.HandleDefaultValueForInjectedParameter(var InjectedParamValue: String;
//  const InjectableParamAttribute: MVCInjectableParamAttribute);
//begin
//  if InjectedParamValue.IsEmpty then
//  begin
//    if InjectableParamAttribute.CanBeUsedADefaultValue then
//    begin
//      InjectedParamValue := InjectableParamAttribute.DefaultValueAsString;
//    end
//    else
//    begin
//      raise EMVCException.CreateFmt
//        ('Required parameter "%s" injected using "%s" has not provided and cannot be used a default value for it',
//        [InjectableParamAttribute.ParamName, InjectableParamAttribute.Classname]);
//    end;
//  end;
//end;

//function TMVCRouterFilter.GetActualParam(const AFormalParam: TRttiParameter;
//  const AStringValue: String): TValue;
//  var lWasDateTime: Boolean; lQualifiedName: String;
//  lFormatSettings: TFormatSettings;
//begin
//  case AFormalParam.ParamType.TypeKind of
//    tkInteger:
//      try
//        Result := StrToInt(AStringValue);
//      except
//        on E: Exception do
//        begin
//          raise EMVCException.CreateFmt(http_status.BadRequest,
//            'Invalid Integer value for param [%s] - [CLASS: %s][MSG: %s]',
//            [AFormalParam.name, E.Classname, E.Message]);
//        end;
//      end;
//    tkInt64:
//      try
//        Result := StrToInt64(AStringValue);
//      except
//        on E: Exception do
//        begin
//          raise EMVCException.CreateFmt(http_status.BadRequest,
//            'Invalid Int64 value for param [%s] - [CLASS: %s][MSG: %s]',
//            [AFormalParam.name, E.Classname, E.Message]);
//        end;
//      end;
//    tkUString:
//      begin
//        Result := AStringValue;
//      end;
//    tkFloat:
//      begin
//        lWasDateTime := False;
//        lQualifiedName := AFormalParam.ParamType.QualifiedName;
//        if lQualifiedName = 'System.TDate' then
//        begin
//          try
//            lWasDateTime := True;
//            Result := ISODateToDate(AStringValue);
//          except
//            on E: Exception do
//            begin
//              raise EMVCException.CreateFmt(http_status.BadRequest,
//                'Invalid TDate value for param [%s] - [CLASS: %s][MSG: %s]',
//                [AFormalParam.name, E.Classname, E.Message]);
//            end;
//          end;
//        end
//        else if lQualifiedName = 'System.TDateTime' then
//        begin
//          try
//            lWasDateTime := True;
//            Result := ISOTimeStampToDateTime(AStringValue);
//          except
//            on E: Exception do
//            begin
//              raise EMVCException.CreateFmt(http_status.BadRequest,
//                'Invalid TDateTime value for param [%s] - [CLASS: %s][MSG: %s]',
//                [AFormalParam.name, E.Classname, E.Message]);
//            end;
//          end;
//        end
//        else if lQualifiedName = 'System.TTime' then
//        begin
//          try
//            lWasDateTime := True;
//            Result := ISOTimeToTime(AStringValue);
//          except
//            on E: Exception do
//            begin
//              raise EMVCException.CreateFmt(http_status.BadRequest,
//                'Invalid TTime value for param [%s] - [CLASS: %s][MSG: %s]',
//                [AFormalParam.name, E.Classname, E.Message]);
//            end;
//          end;
//        end;
//        if not lWasDateTime then
//          try
//            lFormatSettings.DecimalSeparator := '.';
//            Result := StrToFloat(AStringValue, lFormatSettings);
//          except
//            on E: Exception do
//            begin
//              raise EMVCException.CreateFmt(http_status.BadRequest,
//                'Invalid Float value for param [%s] - [CLASS: %s][MSG: %s]',
//                [AFormalParam.name, E.Classname, E.Message]);
//            end;
//          end;
//      end;
//    tkEnumeration:
//      begin
//        if AFormalParam.ParamType.QualifiedName = 'System.Boolean' then
//        begin
//          if SameText(AStringValue, 'true') or SameText(AStringValue, '1') then
//            Result := True
//          else if SameText(AStringValue, 'false') or SameText(AStringValue, '0') then
//            Result := False
//          else
//          begin
//            raise EMVCException.CreateFmt(http_status.BadRequest,
//              'Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false" or "1"/"0".',
//              [AFormalParam.name]);
//          end;
//        end
//        else
//        begin
//          raise EMVCException.CreateFmt(http_status.BadRequest,
//            'Invalid type for parameter %s. Allowed types are ' +
//            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
//        end;
//      end;
//    tkRecord:
//      begin
//        if AFormalParam.ParamType.QualifiedName = 'System.TGUID' then
//        begin
//          try
//            Result := TValue.From<TGUID>(TMVCGuidHelper.StringToGUIDEx(AStringValue));
//          except
//            raise EMVCException.CreateFmt('Invalid Guid value for param [%s]', [AFormalParam.name]);
//          end;
//        end
//        else
//          raise EMVCException.CreateFmt('Invalid type for parameter %s. Allowed types are ' +
//            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
//      end
//  else
//    begin
//      raise EMVCException.CreateFmt(http_status.BadRequest,
//        'Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
//        [AFormalParam.name]);
//    end;
//  end;
//end;


end.
