unit MVCFramework.Filters.Action;

interface

uses
  MVCFramework, MVCFramework.Router, MVCFramework.Commons, System.RTTI;

type
  TMVCActionControllerFilter = class(TControllerFilter)
  private const
    ALLOWED_TYPED_ACTION_PARAMETERS_TYPES =
      'Integer, Int64, Single, Double, Extended, Boolean, TDate, TTime, TDateTime, String and TGUID';
  private
    procedure FillActualParamsForAction(
      const ASelectedController: TMVCController; const AContext: TWebContext;
      const AActionFormalParams: TArray<TRttiParameter>;
      const AActionName: string; var AActualParams: TArray<TValue>;
      out ABodyParameter: TObject);
    procedure HandleDefaultValueForInjectedParameter(
      var InjectedParamValue: String;
      const InjectableParamAttribute: MVCInjectableParamAttribute);
    function GetActualParam(const AFormalParam: TRttiParameter;
      const AStringValue: String): TValue;
  protected
    procedure DoFilter(
      const Context: TWebContext;
      const Router: IMVCRouter); override;
  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Rtti.Utils,
  System.Generics.Collections,
  MVCFramework.DuckTyping, MVCFramework.Serializer.Abstract,
  MVCFramework.Serializer.Commons;

type
  TWebContextHack = class(TWebContext)

  end;

  TMVCControllerHack = class(TMVCController)

  end;

{ TMVCActionControllerFilter }

procedure TMVCActionControllerFilter.DoFilter(
      const Context: TWebContext;
      const Router: IMVCRouter);
var
  lSelectedController: TMVCController;
  lBodyParameter: TObject;
  lActionFormalParams: TArray<TRttiParameter>;
  lActualParams: TArray<TValue>;
  lRouterMethodToCallName: string;
  lHandled: Boolean;
begin
  try
    lSelectedController := Router.CreateControllerInstance;
  except
    on Ex: Exception do
    begin
      Log.Error('[%s] %s [PathInfo "%s"] (Custom message: "%s")',
        [Ex.Classname, Ex.Message, GetRequestShortDescription(Context.Request.RawWebRequest),
        'Cannot create controller'], LOGGERPRO_TAG);
      raise EMVCException.Create(http_status.InternalServerError, 'Cannot create controller');
    end;
  end;

  TWebContextHack(Context).fActionQualifiedName := Router.ActionQualifiedName;
  lSelectedController.Engine := Self.GetEngine;
  TMVCControllerHack(lSelectedController).Context := Context;
  lBodyParameter := nil;
  TMVCControllerHack(lSelectedController).MVCControllerAfterCreate;
  try
    TMVCControllerHack(lSelectedController).ContentType :=
      BuildContentType(Router.ResponseContentMediaType, Router.ResponseContentCharSet);
    lActionFormalParams := Router.ActionMethod.GetParameters;
    if (Length(lActionFormalParams) = 0) then
      SetLength(lActualParams, 0)
    else if (Length(lActionFormalParams) = 1) and
      (SameText(lActionFormalParams[0].ParamType.QualifiedName, 'MVCFramework.TWebContext')) then
    begin
      SetLength(lActualParams, 1);
      lActualParams[0] := Context;
    end
    else
    begin
      FillActualParamsForAction(lSelectedController, Context, lActionFormalParams, lRouterMethodToCallName,
        lActualParams, lBodyParameter);
    end;
    TMVCControllerHack(lSelectedController).OnBeforeAction(Context, lRouterMethodToCallName, lHandled);
    if not lHandled then
    begin
      try
        Router.ActionMethod.Invoke(lSelectedController, lActualParams);
      finally
        TMVCControllerHack(lSelectedController).OnAfterAction(Context, lRouterMethodToCallName);
      end;
    end;
  finally
    try
      lBodyParameter.Free;
    except
      on E: Exception do
      begin
        LogE(Format('Cannot free Body object: [CLS: %s][MSG: %s]', [E.Classname, E.Message]));
      end;
    end;
    TMVCControllerHack(lSelectedController).MVCControllerBeforeDestroy;
  end;
  Context.Response.ContentType := TMVCControllerHack(lSelectedController).ContentType;
  // fOnRouterLog(lRouter, rlsRouteFound, lContext);
end;

procedure TMVCActionControllerFilter.FillActualParamsForAction(
  const ASelectedController: TMVCController; const AContext: TWebContext;
  const AActionFormalParams: TArray<TRttiParameter>; const AActionName: string;
  var AActualParams: TArray<TValue>; out ABodyParameter: TObject);
var
  lParamName: string;
  I: Integer;
  lStrValue: string;
  lFromBodyAttribute: MVCFromBodyAttribute;
  lFromQueryStringAttribute: MVCFromQueryStringAttribute;
  lFromHeaderAttribute: MVCFromHeaderAttribute;
  lFromCookieAttribute: MVCFromCookieAttribute;
  lAttributeInjectedParamCount: Integer;
  lInjectedParamValue: string;
  lList: IMVCList;
  lItemClass: TClass;
begin
  ABodyParameter := nil;
  lAttributeInjectedParamCount := 0;
  SetLength(AActualParams, Length(AActionFormalParams));
  for I := 0 to Length(AActionFormalParams) - 1 do
  begin
    lParamName := AActionFormalParams[I].name;
    if Length(AActionFormalParams[I].GetAttributes) > 0 then
    begin
      // Let's check how to inject this parameter
      if TRttiUtils.HasAttribute<MVCFromBodyAttribute>(AActionFormalParams[I], lFromBodyAttribute)
      then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        if AActionFormalParams[I].ParamType.QualifiedName <> 'System.string' then
        begin
          ABodyParameter := TRttiUtils.CreateObject(AActionFormalParams[I].ParamType.QualifiedName);
          if TDuckTypedList.CanBeWrappedAsList(ABodyParameter, lList) then
          begin
            lItemClass := TMVCAbstractSerializer(ASelectedController.Serializer).GetObjectTypeOfGenericList(ABodyParameter.ClassInfo);
            ASelectedController.Serializer.DeserializeCollection(AContext.Request.Body,
              ABodyParameter, lItemClass, stDefault, [], lFromBodyAttribute.RootNode);
          end
          else
          begin
            ASelectedController.Serializer.DeserializeObject(AContext.Request.Body,
              ABodyParameter, stDefault, [], lFromBodyAttribute.RootNode);
          end;
          AActualParams[I] := ABodyParameter;
        end
        else
        begin
          AActualParams[I] := AContext.Request.Body;
          Continue;
        end;
      end
      else if TRttiUtils.HasAttribute<MVCFromQueryStringAttribute>(AActionFormalParams[I],
        lFromQueryStringAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := AContext.Request.QueryStringParam
          (lFromQueryStringAttribute.ParamName);
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromQueryStringAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else if TRttiUtils.HasAttribute<MVCFromHeaderAttribute>(AActionFormalParams[I],
        lFromHeaderAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := AContext.Request.GetHeader(lFromHeaderAttribute.ParamName);
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromHeaderAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else if TRttiUtils.HasAttribute<MVCFromCookieAttribute>(AActionFormalParams[I],
        lFromCookieAttribute) then
      begin
        Inc(lAttributeInjectedParamCount, 1);
        lInjectedParamValue := AContext.Request.Cookie(lFromCookieAttribute.ParamName);
        HandleDefaultValueForInjectedParameter(lInjectedParamValue, lFromCookieAttribute);
        AActualParams[I] := GetActualParam(AActionFormalParams[I], lInjectedParamValue);
      end
      else
      begin
        raise EMVCException.Create(http_status.InternalServerError,
          'Unknown custom attribute on action parameter: ' + AActionFormalParams[I].name +
          '. [HINT: Allowed attributes are MVCFromBody, MVCFromQueryString, MVCFromHeader, MVCFromCookie]');
      end;
      Continue;
    end;

    // From now on we'll check for url mapped parameters
    if not AContext.Request.SegmentParam(lParamName, lStrValue) then
      raise EMVCException.CreateFmt(http_status.BadRequest,
        'Invalid parameter %s for action %s (Hint: Here parameters names are case-sensitive)',
        [lParamName, AActionName]);
    AActualParams[I] := GetActualParam(AActionFormalParams[I], lStrValue);
  end;

  if (AContext.Request.SegmentParamsCount + lAttributeInjectedParamCount) <>
    Length(AActionFormalParams) then
    raise EMVCException.CreateFmt(http_status.BadRequest,
      'Parameters count mismatch (expected %d actual %d) for action "%s"',
      [Length(AActionFormalParams), AContext.Request.SegmentParamsCount, AActionName]);
end;


procedure TMVCActionControllerFilter.HandleDefaultValueForInjectedParameter(var InjectedParamValue: String;
  const InjectableParamAttribute: MVCInjectableParamAttribute);
begin
  if InjectedParamValue.IsEmpty then
  begin
    if InjectableParamAttribute.CanBeUsedADefaultValue then
    begin
      InjectedParamValue := InjectableParamAttribute.DefaultValueAsString;
    end
    else
    begin
      raise EMVCException.CreateFmt
        ('Required parameter "%s" injected using "%s" has not provided and cannot be used a default value for it',
        [InjectableParamAttribute.ParamName, InjectableParamAttribute.Classname]);
    end;
  end;
end;


function TMVCActionControllerFilter.GetActualParam(const AFormalParam: TRttiParameter;
  const AStringValue: String): TValue;
  var lWasDateTime: Boolean; lQualifiedName: String;
  lFormatSettings: TFormatSettings;
begin
  case AFormalParam.ParamType.TypeKind of
    tkInteger:
      try
        Result := StrToInt(AStringValue);
      except
        on E: Exception do
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid Integer value for param [%s] - [CLASS: %s][MSG: %s]',
            [AFormalParam.name, E.Classname, E.Message]);
        end;
      end;
    tkInt64:
      try
        Result := StrToInt64(AStringValue);
      except
        on E: Exception do
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid Int64 value for param [%s] - [CLASS: %s][MSG: %s]',
            [AFormalParam.name, E.Classname, E.Message]);
        end;
      end;
    tkUString:
      begin
        Result := AStringValue;
      end;
    tkFloat:
      begin
        lWasDateTime := False;
        lQualifiedName := AFormalParam.ParamType.QualifiedName;
        if lQualifiedName = 'System.TDate' then
        begin
          try
            lWasDateTime := True;
            Result := ISODateToDate(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TDate value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end
        else if lQualifiedName = 'System.TDateTime' then
        begin
          try
            lWasDateTime := True;
            Result := ISOTimeStampToDateTime(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TDateTime value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end
        else if lQualifiedName = 'System.TTime' then
        begin
          try
            lWasDateTime := True;
            Result := ISOTimeToTime(AStringValue);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid TTime value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
        end;
        if not lWasDateTime then
          try
            lFormatSettings.DecimalSeparator := '.';
            Result := StrToFloat(AStringValue, lFormatSettings);
          except
            on E: Exception do
            begin
              raise EMVCException.CreateFmt(http_status.BadRequest,
                'Invalid Float value for param [%s] - [CLASS: %s][MSG: %s]',
                [AFormalParam.name, E.Classname, E.Message]);
            end;
          end;
      end;
    tkEnumeration:
      begin
        if AFormalParam.ParamType.QualifiedName = 'System.Boolean' then
        begin
          if SameText(AStringValue, 'true') or SameText(AStringValue, '1') then
            Result := True
          else if SameText(AStringValue, 'false') or SameText(AStringValue, '0') then
            Result := False
          else
          begin
            raise EMVCException.CreateFmt(http_status.BadRequest,
              'Invalid boolean value for parameter %s. Boolean parameters accepts only "true"/"false" or "1"/"0".',
              [AFormalParam.name]);
          end;
        end
        else
        begin
          raise EMVCException.CreateFmt(http_status.BadRequest,
            'Invalid type for parameter %s. Allowed types are ' +
            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
        end;
      end;
    tkRecord:
      begin
        if AFormalParam.ParamType.QualifiedName = 'System.TGUID' then
        begin
          try
            Result := TValue.From<TGUID>(TMVCGuidHelper.StringToGUIDEx(AStringValue));
          except
            raise EMVCException.CreateFmt('Invalid Guid value for param [%s]', [AFormalParam.name]);
          end;
        end
        else
          raise EMVCException.CreateFmt('Invalid type for parameter %s. Allowed types are ' +
            ALLOWED_TYPED_ACTION_PARAMETERS_TYPES, [AFormalParam.name]);
      end
  else
    begin
      raise EMVCException.CreateFmt(http_status.BadRequest,
        'Invalid type for parameter %s. Allowed types are ' + ALLOWED_TYPED_ACTION_PARAMETERS_TYPES,
        [AFormalParam.name]);
    end;
  end;
end;


end.
