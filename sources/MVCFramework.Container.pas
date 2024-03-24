unit MVCFramework.Container;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils, System.TypInfo;


type
  TClassOfInterfacedObject = class of TInterfacedObject;
  TRegistrationType = (rtTransient, rtSingleton {, rtSingletonPerThread});
  IMVCServiceContainer = interface
    ['{1BB3F4A8-DDA1-4526-981C-A0BF877CFFD5}']
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function Resolve(const aTypeInfo: PTypeInfo; const aName: string = ''; const aParams: TArray<TValue> = nil): IInterface;
    procedure Build();
  end;

  MVCInjectAttribute = class(TCustomAttribute)

  end;



  function DefaultServiceContainer: IMVCServiceContainer;

implementation

uses
  MVCFramework.Rtti.Utils;

type
  TRegistration = class
    Intf: TGUID;
    Clazz: TClassOfInterfacedObject;
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
    function GetKey(const aGUID: TGUID; const aName: String): String;
    constructor Create; virtual;
    destructor Destroy; override;
    class var fInstance: IMVCServiceContainer;
  public
    class function Instance: IMVCServiceContainer;
    class constructor Create;
    class destructor Destroy;
    function RegisterType<TImpl: TInterfacedObject>(const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function RegisterType(const aImplementation: TClassOfInterfacedObject; const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient): IMVCServiceContainer; overload;
    function Resolve<TIntf: IInterface>(const aName: string = ''; const aParams: TArray<TValue> = nil): TIntf; overload;
    function Resolve(const aTypeInfo: PTypeInfo; const aName: string = ''; const aParams: TArray<TValue> = nil): IInterface; overload;
    procedure Build();
    type
      EMVCContainerError = class(Exception) end;
      EMVCUnknownService = class(EMVCContainerError) end;
      EMVCInterfaceNotSupported = class(EMVCContainerError) end;
      EMVCUnknownConstructor = class(EMVCContainerError) end;
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

function TMVCServiceContainer.GetKey(const aGUID: TGUID; const aName: String): String;
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
  lType: TRttiType;
  lReg: TRegistration;
begin
  if fBuilt then
  begin
    raise EMVCContainerError.Create('Cannot register new service if the container has been already built');
  end;
  lType := TRttiUtils.GlContext.GetType(aImplementation);
  if Supports(aImplementation, aInterface) then
  begin
    lReg := TRegistration.Create;
    lReg.Clazz := aImplementation;
    lReg.RegistrationType := aRegType;
    fRegistry.Add(GetKey(aInterface, aName), lReg);
  end
  else
  begin
    raise EMVCUnknownService.Create(lType.Name + ' doesn''t supports requested interface');
  end;
  Result := Self;
end;

function TMVCServiceContainer.RegisterType<TImpl>(const aInterface: TGUID; const aName: string; const aRegType: TRegistrationType): IMVCServiceContainer;
begin
  Result := RegisterType(TImpl, aInterface, aName, aRegType);
end;

function TMVCServiceContainer.Resolve(const aTypeInfo: PTypeInfo; const aName: string;
  const aParams: TArray<TValue>): IInterface;
var
  lReg: TRegistration;
  lTypeInfo: PTypeInfo;
  lType: TRttiType;
  lService: TObject;
begin
  if not fBuilt then
  begin
    raise EMVCContainerError.Create('Cannot resolve service if the container has not been built');
  end;
  lTypeInfo := aTypeInfo;
  if not fRegistry.TryGetValue(GetKey(lTypeInfo.TypeData.GUID, aName), lReg) then
  begin
    raise EMVCUnknownService.CreateFmt('Unknown service for "%s"', [lTypeInfo.Name]);
  end;
  lType := TRttiUtils.GlContext.GetType(lReg.Clazz);

  case lReg.RegistrationType of
    rtTransient:
    begin
      lService := CreateServiceWithDependencies(lReg.Clazz, TRttiUtils.GetConstructorWithAttribute<MVCInjectAttribute>(lType));
      //lService := TRttiUtils.CreateObject(lType, AParams);
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
            lService := TRttiUtils.CreateObject(lType, AParams);
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

function TMVCServiceContainer.Resolve<TIntf>(const aName: string; const aParams: TArray<TValue>): TIntf;
begin
  Result := Resolve(TypeInfo(TIntf), aName, aParams);
end;

procedure TMVCServiceContainer.Build;
begin
  fBuilt := True;
end;

class constructor TMVCServiceContainer.Create;
begin
  fInstance := TMVCServiceContainer.Create;
end;

function DefaultServiceContainer: IMVCServiceContainer;
begin
  Result := TMVCServiceContainer.fInstance;
end;

end.
