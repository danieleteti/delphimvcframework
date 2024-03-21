unit MVCFramework.Injector;

interface

uses
  System.Generics.Collections, System.Rtti, System.SysUtils;

type
  TMVCServiceContainer = class
  private
    type
      TRegistrationType = (rtTransient, rtSingleton {, rtSingletonPerThread});
      TRegistration = class
        Intf: TGUID;
        Clazz: TClass;
        Instance: IInterface;
        RegistrationType: TRegistrationType;
      end;
  private
    fRegistry: TObjectDictionary<string, TRegistration>;
  protected
    function GetKey(const aGUID: TGUID; const aName: String): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RegisterType<TImpl: class>(const aInterface: TGUID; const aName : string = ''; const aRegType: TRegistrationType = rtTransient);
    function Resolve<TIntf: IInterface>(const aName: string = ''; const aParams: TArray<TValue> = nil): TIntf;
    type
      EMVCContainerError = class(Exception) end;
      EMVCUnknownService = class(EMVCContainerError) end;
      EMVCInterfaceNotSupported = class(EMVCContainerError) end;
      EMVCUnknownConstructor = class(EMVCContainerError) end;
  end;

implementation

uses
  MVCFramework.Rtti.Utils, System.TypInfo;

{ TMVCServiceContainer }

constructor TMVCServiceContainer.Create;
begin
  inherited;
  fRegistry := TObjectDictionary<String, TRegistration>.Create([doOwnsValues]);
end;

destructor TMVCServiceContainer.Destroy;
begin
  fRegistry.Free;
  inherited;
end;

function TMVCServiceContainer.GetKey(const aGUID: TGUID; const aName: String): String;
begin
  Result := aGUID.ToString + '_' + aName;
end;

procedure TMVCServiceContainer.RegisterType<TImpl>(const aInterface: TGUID; const aName: string; const aRegType: TRegistrationType);
var
  lType: TRttiType;
  lReg: TRegistration;
begin
  lType := TRttiUtils.GlContext.GetType(TImpl);
  if Supports(TImpl, aInterface) then
  begin
    lReg := TRegistration.Create;
    lReg.Clazz := TImpl;
    lReg.RegistrationType := aRegType;
    fRegistry.Add(GetKey(aInterface, aName), lReg);
  end
  else
  begin
    raise EMVCUnknownService.Create(lType.Name + ' doesn''t supports requested interface');
  end;
end;

function TMVCServiceContainer.Resolve<TIntf>(const aName: string; const aParams: TArray<TValue>): TIntf;
var
  lReg: TRegistration;
  lTypeInfo: PTypeInfo;
  lType: TRttiType;
  lService: TObject;
begin
  lTypeInfo := TypeInfo(TIntf);
  if not fRegistry.TryGetValue(GetKey(lTypeInfo.TypeData.GUID, aName), lReg) then
  begin
    raise EMVCUnknownService.CreateFmt('Unknown service for "%s"', [lTypeInfo.Name]);
  end;
  lType := TRttiUtils.GlContext.GetType(lReg.Clazz);

  case lReg.RegistrationType of
    rtTransient:
    begin
      lService := TRttiUtils.CreateObject(lType, AParams);
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

end.
