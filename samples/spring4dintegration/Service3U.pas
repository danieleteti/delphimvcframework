unit Service3U;

interface

uses
  ServicesInterfaceU;

type
  TCommonService = class(TInterfacedObject, ICommonService)
  private
    FID: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetID: string;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Logger;

{ TService3 }

constructor TCommonService.Create;
begin
  inherited;
  LogI('creating ' + ClassName);
  FID := Random(1000).ToString;
end;

destructor TCommonService.Destroy;
begin
  LogI('destroying ' + ClassName);
  inherited;
end;

function TCommonService.GetID: string;
begin
  Result := FID;
end;

end.
