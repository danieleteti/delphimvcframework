unit MVCFramework.Serializer.Defaults;

interface

uses
  MVCFramework.Serializer.Intf;

function GetDefaultSerializer: IMVCSerializer;

implementation

uses
  MVCFramework.Serializer.JsonDataObjects;

function GetDefaultSerializer: IMVCSerializer;
begin
  Result := TMVCJsonDataObjectsSerializer.Create;
end;

end.
