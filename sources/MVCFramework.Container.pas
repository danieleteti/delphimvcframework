unit MVCFramework.Container;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, System.TypInfo;


type
  TClassOfInterfacedObject = class of TInterfacedObject;
  TRegistrationType = (rtTransient, rtSingleton, rtSingletonPerRequest);
  IMVCServiceContainer = interface
    ['{1BB3F4A8-DDA1-4526-981C-A0BF877CFFD5}']
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function Re vsolve(const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface;
    procedure Build();
  end;

  IMVCServiceContainerEx = interface
    ['{2C920EC2-001F-40BE-9911-43A65077CADD}']
    function ResolveEx(const aTypeInfo: PTypeInfo; const aName: string; out ServiceKey: String; out RegType: TRegistrationType): IInterface; overload;
  end;


  MVCInjectAttribute = class(TCustomAttribute)
  private
    fServiceName: String;
  public
    constructor Create(ServiceName: String = '');
    property ServiceName: String read fServiceName;
  end;

  EMVCContainerError = class(Exception) end;
  EMVCContainerErrorUnknownService = class(EMVCContainerError) end;
  EMVCContainerErrorInterfaceNotSupported = class(EMVCContainerError) end;
  EMVCContainerErrorUnknownConstructor = class(EMVCContainerError) end;


  function DefaultMVCServiceContainer: IMVCServiceContainer;

implementation

uses
  MVCFramework.Rtti.Utils;

type
  TRegistration = class
    Intf: TGUID;
    Clazz: TClassOfInterfacedObject;
    RttiType: TRttiType;
    Instance: IInterface;
    RegistrationType: TRegistrationType;
  end;
  TMVCServiceContainer = class(TInterfacedObject, IMVCServiceContainer)
  private
    fBuilt: Boolean;
    fRegistry: TObjectDictionary<string, TRegistration>;
    function CreateServiceWithDependencies(const ServiceClass: TClassOfInterfacedObject;
      const ConstructorMethod: TRttiMethod): TInterfacedObject;
  protected
    class function GetKey(const aGUID: TGUID; const aName: String): String;
    constructor Create; virtual;
    destructor Destroy; override;
    class var fInstance: IMVCServiceContainer;
  public
    class function Instance: IMVCServiceContainer;
    class constructor Create;
    class destructor Destroy;
    function RegisterType<TImpl: TInterfacedObject>(const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function Resolve<TIntf: IInterface>(const aName: string = ''): TIntf; overload;
    function Resolve(const aTypeInfo: PTypeInfo; const aName: string = ''): IInterface; overload;
    function ResolveEx(const aTypeInfo: PTypeInfo; const aName: string; out ServiceKey: String; out RegType: TRegistrationType): IInterface; overload;
    procedure Build();
  end;

  TMVCServiceContainerAdapter = class(TInterfacedObject, IMVCServiceContainerEx)
  protected
    function ResolveEx(const aTypeInfo: PTypeInfo; const aName: string; out ServiceKey: string; out RegType: TRegistrationType): IInterface;
  end;

{ TMVCServiceContainer }

function TMVCServiceContainer.CreateServiceWithDependencies(const ServiceClass: TClassOfInterfacedObject;
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
        lIntf := Resolve(lActionFormalParams[I].ParamType.Handle);
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

class destructor TMVCServiceContainer.Destroy;
begin
  fInstance := nil;
end;

class function TMVCServiceContainer.GetKey(const aGUID: TGUID; const aName: String): String;
begin
  Result := aGUID.ToString + '_' + aName;
end;

class function TMVCServiceContainer.Instance: IMVCServiceContainer;
begin
  Result := fInstance;
end;

function TMVCServiceContainer.RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID;
  const aName: string; const aRegType: TRegistrationType): IMVCServiceContainer;
var
  lReg: TRegistration;
begin
  if fBuilt then
  begin
    raise EMVCContainerError.Create('Cannot register new service if the container has been already built');
  end;

  if Supports(aImplementation, aInterface) then
  begin
    lReg := TRegistration.Create;
    lReg.Clazz := aImplementation;
    lReg.RttiType := TRttiUtils.GlContext.GetType(lReg.Clazz);
    lReg.RegistrationType := aRegType;
    if not fRegistry.TryAdd(GetKey(aInterface, aName), lReg) then
    begin
      raise EMVCContainerError.CreateFmt('Cannot register duplicated service "%s"',[GetKey(aInterface, aName)]);
    end;
  end
  else
  begin
    raise EMVCContainerErrorUnknownService.CreateFmt('"%s" doesn''t supports requested interface', [aImplementation.QualifiedClassName]);
  end;
  Result := Self;
end;

function TMVCServiceContainer.RegisterType<TImpl>(const aInterface: TGUID; const aName: string; const aRegType: TRegistrationType): IMVCServiceContainer;
begin
  Result := RegisterType(TImpl, aInterface, aName, aRegType);
end;

function TMVCServiceContainer.Resolve(const aTypeInfo: PTypeInfo; const aName: string): IInterface;
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
    rtTransient:
    begin
      lService := CreateServiceWithDependencies(lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
      Supports(lService, lTypeInfo.TypeData.GUID, Result);
    end;

    rtSingleton:
    begin
      if lReg.Instance = nil then
      begin
        TMonitor.Enter(Self);
        try
          if lReg.Instance = nil then
          begin
            lService := CreateServiceWithDependencies(lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
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

function TMVCServiceContainer.Resolve<TIntf>(const aName: string): TIntf;
begin
  Result := Resolve(TypeInfo(TIntf), aName);
end;

function TMVCServiceContainer.ResolveEx(const aTypeInfo: PTypeInfo; const aName: string;
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
    rtTransient, rtSingletonPerRequest:
    begin
      lService := CreateServiceWithDependencies(lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
      Supports(lService, lTypeInfo.TypeData.GUID, Result);
      {rtSingletonPerRequest is destroyed by the adapter owned by Context}
    end;

    rtSingleton:
    begin
      if lReg.Instance = nil then
      begin
        TMonitor.Enter(Self);
        try
          if lReg.Instance = nil then
          begin
            lService := CreateServiceWithDependencies(lReg.Clazz, TRttiUtils.GetFirstDeclaredConstructor(lType));
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

procedure TMVCServiceContainer.Build;
begin
  fBuilt := True;
end;

class constructor TMVCServiceContainer.Create;
begin
  fInstance := TMVCServiceContainer.Create;
end;

function DefaultMVCServiceContainer: IMVCServiceContainer;
begin
  Result := TMVCServiceContainer.fInstance;
end;

{ MVCInjectAttribute }

constructor MVCInjectAttribute.Create(ServiceName: String);
begin
  inherited Create;
  fServiceName := ServiceName;
end;

end.
