unit MVCFramework.Container;

{$I dmvcframework.inc}

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, System.TypInfo;


type
{$SCOPEDENUMS ON}
  TRegistrationType = (Transient, Singleton, SingletonPerRequest);

  TClassOfInterfacedObject = class of TInterfacedObject;

  TInterfacedObjectFactory = reference to function: TInterfacedObject;

  IMVCServiceContainerResolver = interface
    ['{2C920EC2-001F-40BE-9911-43A65077CADD}']
    function Resolve(const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface; overload;
  end;

  IMVCServiceContainer = interface
    ['{1BB3F4A8-DDA1-4526-981C-A0BF877CFFD5}']
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aRegType: TRegistrationType = TRegistrationType.Transient; const aName : string = ''): IMVCServiceContainer; overload;
    function RegisterType(const aDelegate: TInterfacedObjectFactory; const aInterface: TGUID; const aRegType: TRegistrationType = TRegistrationType.Transient; const aName : string = ''): IMVCServiceContainer; overload;
    procedure Build();
  end;

  EMVCContainerError = class(Exception) end;
  EMVCContainerErrorUnknownService = class(EMVCContainerError) end;
  EMVCContainerErrorInterfaceNotSupported = class(EMVCContainerError) end;
  EMVCContainerErrorUnknownConstructor = class(EMVCContainerError) end;


  function DefaultMVCServiceContainer: IMVCServiceContainer;
  function NewMVCServiceContainer: IMVCServiceContainer;
  function NewServiceContainerResolver: IMVCServiceContainerResolver; overload;
  function NewServiceContainerResolver(Container: IMVCServiceContainer): IMVCServiceContainerResolver; overload;

implementation

uses
  MVCFramework.Rtti.Utils, MVCFramework;

type
  IMVCServiceInternalResolver = interface
    ['{81527509-BA94-48C1-A030-E26F1FC9BFF5}']
    function Resolve(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface;
    function ResolveEx(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string; out ServiceKey: String; out RegType: TRegistrationType): IInterface; overload;
  end;

  TRegistration = class
  public
    Intf: TGUID;
    Clazz: TClassOfInterfacedObject;
    RttiType: TRttiType;
    Instance: IInterface;
    Delegate: TInterfacedObjectFactory;
    RegistrationType: TRegistrationType;
  end;

  TMVCServiceContainer = class(TInterfacedObject, IMVCServiceContainer, IMVCServiceInternalResolver)
  private
    fBuilt: Boolean;
    fRegistry: TObjectDictionary<string, TRegistration>;
    function CreateServiceWithDependencies(
      const ServiceContainerResolver: IMVCServiceContainerResolver;
      const ServiceClass: TClassOfInterfacedObject;
      const ConstructorMethod: TRttiMethod): TInterfacedObject;
  protected
    class function GetKey(const aGUID: TGUID; const aName: String): String;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CheckBuilt;
  public
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aRegType: TRegistrationType = TRegistrationType.Transient; const aName : string = ''): IMVCServiceContainer; overload;
    function RegisterType(const aDelegate: TInterfacedObjectFactory; const aInterface: TGUID; const aRegType: TRegistrationType = TRegistrationType.Transient; const aName : string = ''): IMVCServiceContainer; overload;
    function Resolve(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface; overload;
    function ResolveEx(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string; out ServiceKey: String; out RegType: TRegistrationType): IInterface; overload;
    procedure Build();
  end;

  TMVCServiceContainerAdapter = class(TInterfacedObject, IMVCServiceContainerResolver)
  private
    fCachedServices: TDictionary<String, IInterface>;
    fContainer: IMVCServiceInternalResolver;
  protected
    function Resolve(const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface; overload;
  public
    constructor Create(Container: IMVCServiceContainer);
    destructor Destroy; override;
  end;


var
  gDefaultMVCServiceContainer: IMVCServiceContainer = nil;
  gLock: TObject = nil;

{ TMVCServiceContainer }

function TMVCServiceContainer.CreateServiceWithDependencies(
  const ServiceContainerResolver: IMVCServiceContainerResolver;
  const ServiceClass: TClassOfInterfacedObject;
  const ConstructorMethod: TRttiMethod): TInterfacedObject;
var
  lActionFormalParams: TArray<TRttiParameter>;
  lActualParams: TArray<TValue>;
  I: Integer;
  lIntf, lOutIntf: IInterface;
begin
  if ConstructorMethod <> nil then
  begin
    lActionFormalParams := ConstructorMethod.GetParameters;
    SetLength(lActualParams, Length(lActionFormalParams));
    if Length(lActionFormalParams) > 0 then
    begin
      for I := 0 to Length(lActionFormalParams) - 1 do
      begin
        if ServiceContainerResolver = nil then
          lIntf := Resolve(nil, lActionFormalParams[I].ParamType.Handle)
        else
          lIntf := ServiceContainerResolver.Resolve(lActionFormalParams[I].ParamType.Handle);
        if not Supports(lIntf, lActionFormalParams[I].ParamType.Handle.TypeData.GUID, lOutIntf) then
        begin
          raise EMVCContainerError.CreateFmt('Cannot inject parameter %s: %s into constructor of %s', [
            lActionFormalParams[I].name,
            lActionFormalParams[I].ParamType.ToString,
            ServiceClass.ClassName
            ]);
        end;
        TValue.Make(@lOutIntf, lActionFormalParams[I].ParamType.Handle, lActualParams[I]);
      end;
    end;
    Result := TInterfacedObject(ConstructorMethod.Invoke(ServiceClass, lActualParams).AsObject);
  end
  else
  begin
    Result := TInterfacedObject(TRttiUtils.CreateObject(ServiceClass.QualifiedClassName));
  end;
end;


procedure TMVCServiceContainer.CheckBuilt;
begin
  if fBuilt then
  begin
    raise EMVCContainerError.Create('Cannot register new service if the container has been already built');
  end;
end;

constructor TMVCServiceContainer.Create;
begin
  inherited;
  fRegistry := TObjectDictionary<String, TRegistration>.Create([doOwnsValues]);
  fBuilt := False;
end;

destructor TMVCServiceContainer.Destroy;
begin
  fRegistry.Free;
  inherited;
end;

class function TMVCServiceContainer.GetKey(const aGUID: TGUID; const aName: String): String;
begin
  Result := aGUID.ToString + '_' + aName;
end;

function TMVCServiceContainer.RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID;
  const aRegType: TRegistrationType; const aName: string): IMVCServiceContainer;
var
  lReg: TRegistration;
  lKey: string;
begin
  CheckBuilt;
  if Supports(aImplementation, aInterface) then
  begin
    lReg := TRegistration.Create;
    lReg.Clazz := aImplementation;
    lReg.Delegate := nil;
    lReg.RttiType := TRttiUtils.GlContext.GetType(lReg.Clazz);
    lReg.RegistrationType := aRegType;
    lKey := GetKey(aInterface, aName);
    {$IF Defined(RIOORBETTER)}
    if not fRegistry.TryAdd(lKey, lReg) then
    begin
      raise EMVCContainerError.CreateFmt('Cannot register duplicated service "%s"',[lKey]);
    end;
    {$ELSE}
    if not fRegistry.ContainsKey(lKey) then
    begin
      fRegistry.Add(lKey, lReg)
    end
    else
    begin
      raise EMVCContainerError.CreateFmt('Cannot register duplicated service "%s"',[lKey]);
    end;
    {$ENDIF}

  end
  else
  begin
    raise EMVCContainerErrorUnknownService.CreateFmt('"%s" doesn''t supports requested interface', [aImplementation.QualifiedClassName]);
  end;
  Result := Self;
end;

function TMVCServiceContainer.RegisterType(
  const aDelegate: TInterfacedObjectFactory; const aInterface: TGUID;
  const aRegType: TRegistrationType; const aName: string): IMVCServiceContainer;
var
  lReg: TRegistration;
  lKey: string;
begin
  CheckBuilt;
  lReg := TRegistration.Create;
  lReg.Clazz := nil;
  lReg.Delegate := aDelegate;
  lReg.RttiType := nil; //TRttiUtils.GlContext.GetType(lReg.Clazz);
  lReg.RegistrationType := aRegType;
  lKey := GetKey(aInterface, aName);
  {$IF Defined(RIOORBETTER)}
  if not fRegistry.TryAdd(lKey, lReg) then
  begin
    raise EMVCContainerError.CreateFmt('Cannot register duplicated service "%s"',[lKey]);
  end;
  {$ELSE}
  if not fRegistry.ContainsKey(lKey) then
  begin
    fRegistry.Add(lKey, lReg)
  end
  else
  begin
    raise EMVCContainerError.CreateFmt('Cannot register duplicated service "%s"',[lKey]);
  end;
  {$ENDIF}
  Result := Self;
end;

function TMVCServiceContainer.Resolve(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string): IInterface;
var
  lReg: TRegistration;
  lTypeInfo: PTypeInfo;
  lType: TRttiType;
  lService: TObject;
begin
  if not fBuilt then
  begin
    raise EMVCContainerError.Create('Container has not been built');
  end;
  lTypeInfo := aTypeInfo;
  if not fRegistry.TryGetValue(GetKey(lTypeInfo.TypeData.GUID, aName), lReg) then
  begin
    raise EMVCContainerErrorUnknownService.CreateFmt('Unknown service "%s" with name "%s"', [lTypeInfo.Name, aName])
  end;
  lType := lReg.RttiType;

  case lReg.RegistrationType of
    TRegistrationType.Transient, TRegistrationType.SingletonPerRequest:
    begin
      lService := CreateServiceWithDependencies(ServiceContainerResolver, lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
      Supports(lService, lTypeInfo.TypeData.GUID, Result);
    end;

    TRegistrationType.Singleton:
    begin
      if lReg.Instance = nil then
      begin
        TMonitor.Enter(Self);
        try
          if lReg.Instance = nil then
          begin
            lService := CreateServiceWithDependencies(ServiceContainerResolver, lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
            Supports(lService, lTypeInfo.TypeData.GUID, lReg.Instance)
          end;
        finally
          TMonitor.Exit(Self)
        end;
      end;
      Supports(lReg.Instance, lTypeInfo.TypeData.GUID, Result);
    end;
    else
      raise EMVCContainerError.Create('Unsupported RegistrationType');
  end;
end;

function TMVCServiceContainer.ResolveEx(const ServiceContainerResolver: IMVCServiceContainerResolver; const aTypeInfo: PTypeInfo; const aName: string;
  out ServiceKey: String; out RegType: TRegistrationType): IInterface;
var
  lReg: TRegistration;
  lTypeInfo: PTypeInfo;
  lType: TRttiType;
  lService: TObject;
  lServiceKey: string;
begin
  if not fBuilt then
  begin
    raise EMVCContainerError.Create('Container has not been built');
  end;
  lTypeInfo := aTypeInfo;
  lServiceKey := GetKey(lTypeInfo.TypeData.GUID, aName);
  if not fRegistry.TryGetValue(lServiceKey, lReg) then
  begin
    raise EMVCContainerErrorUnknownService.CreateFmt('Unknown service "%s" with name "%s"', [lTypeInfo.Name, aName])
  end;

  lType := lReg.RttiType;
  RegType := lReg.RegistrationType;
  ServiceKey := lServiceKey;
  case lReg.RegistrationType of
    TRegistrationType.Transient, TRegistrationType.SingletonPerRequest:
    begin
      if lReg.Delegate = nil then
      begin
        lService := CreateServiceWithDependencies(ServiceContainerResolver, lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
      end
      else
      begin
        lService := lReg.Delegate();
      end;
      if not Supports(lService, lTypeInfo.TypeData.GUID, Result) then
      begin
        raise EMVCContainerErrorUnknownService.
          CreateFmt('"%s" doesn''t supports requested interface', [TInterfacedObject(lReg.Instance).QualifiedClassName]);
      end;
      {rtSingletonPerRequest is destroyed by the adapter owned by Context}
    end;

    TRegistrationType.Singleton:
    begin
      if lReg.Instance = nil then
      begin
        TMonitor.Enter(Self);
        try
          if lReg.Instance = nil then
          begin
            if lReg.Delegate = nil then
            begin
              lService := CreateServiceWithDependencies(ServiceContainerResolver, lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
            end
            else
            begin
              lService := lReg.Delegate();
            end;
            if not Supports(lService, lTypeInfo.TypeData.GUID, lReg.Instance) then
            begin
              raise EMVCContainerErrorUnknownService.
                CreateFmt('"%s" doesn''t supports requested interface', [TInterfacedObject(lReg.Instance).QualifiedClassName]);
            end;
          end;
        finally
          TMonitor.Exit(Self)
        end;
      end;
      Supports(lReg.Instance, lTypeInfo.TypeData.GUID, Result);
    end;
    else
      raise EMVCContainerError.Create('Unsupported RegistrationType');
  end;
end;

procedure TMVCServiceContainer.Build;
begin
  fBuilt := True;
end;

function DefaultMVCServiceResolver: IMVCServiceContainerResolver;
begin
  Result := DefaultMVCServiceContainer as IMVCServiceContainerResolver;
end;

function DefaultMVCServiceContainer: IMVCServiceContainer;
begin
  if gDefaultMVCServiceContainer = nil then
  begin
    TMonitor.Enter(gLock);
    try
      if gDefaultMVCServiceContainer = nil then
      begin
        gDefaultMVCServiceContainer := TMVCServiceContainer.Create;
      end;
    finally
      TMonitor.Exit(gLock);
    end;
  end;
  Result := gDefaultMVCServiceContainer;
end;

function NewMVCServiceContainer: IMVCServiceContainer;
begin
  Result := TMVCServiceContainer.Create;
end;

{ TMVCServiceContainerAdapter }

constructor TMVCServiceContainerAdapter.Create(Container: IMVCServiceContainer);
begin
  inherited Create;
  fCachedServices := TDictionary<String, IInterface>.Create;
  fContainer := Container as IMVCServiceInternalResolver;
end;

destructor TMVCServiceContainerAdapter.Destroy;
begin
  fCachedServices.Free;
  inherited;
end;

function TMVCServiceContainerAdapter.Resolve(const aTypeInfo: PTypeInfo; const aName: string): IInterface;
var
  lKey: string;
  lIntf: IInterface;
  lRegType: TRegistrationType;
begin
  lKey := TMVCServiceContainer.GetKey(aTypeInfo.TypeData.GUID, aName);
  if fCachedServices.TryGetValue(lKey, lIntf) then
  begin
    Supports(lIntf, aTypeInfo.TypeData.GUID, Result);
  end
  else
  begin
    Result := fContainer.ResolveEx(Self, aTypeInfo, aName, lKey, lRegType);
    if lRegType = TRegistrationType.SingletonPerRequest then
    begin
      fCachedServices.Add(lKey, Result);
    end;
  end;
end;


function NewServiceContainerResolver: IMVCServiceContainerResolver;
begin
  Result := TMVCServiceContainerAdapter.Create(DefaultMVCServiceContainer);
end;

function NewServiceContainerResolver(Container: IMVCServiceContainer) : IMVCServiceContainerResolver;
begin
  Result := TMVCServiceContainerAdapter.Create(Container);
end;


initialization

gLock := TObject.Create;

finalization

gLock.Free;

end.
