unit CustomJSONSerializersU;

interface

uses
  MVCFramework.Serializer.Intf, System.Rtti;

type
  {
    This class is completely serialized using a custom serializer.
    Object of type TMyClass will be serialized as jsonstring with "ID;Description" format
  }
  TMyClassSerializerJSON = class(TInterfacedObject, IMVCTypeSerializer)
  public
    procedure SerializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const InstanceField: TValue;
      var SerializerObject: TObject);
    procedure DeserializeInstance(
      const ElementType: TRTTIType;
      const ElementAttributes: TArray<TCustomAttribute>;
      const SerializerObject: TObject;
      var InstanceField: TValue);
  end;

implementation

uses
  BOs, MVCFramework.TypesAliases, System.SysUtils;

procedure TMyClassSerializerJSON.DeserializeInstance(
  const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const SerializerObject: TObject; var InstanceField: TValue);
var
  lJSONString: TJSONString;
  lPieces: TArray<string>;
  lObj: TMyClass;
begin
  lJSONString := SerializerObject as TJSONString;
  lPieces := lJSONString.Value.Split([';'], 2);
  lObj := InstanceField.AsObject as TMyClass;
  lObj.ID := lPieces[0].ToInteger;
  lObj.Description := lPieces[1];
end;

procedure TMyClassSerializerJSON.SerializeInstance(const ElementType: TRTTIType;
  const ElementAttributes: TArray<TCustomAttribute>;
  const InstanceField: TValue; var SerializerObject: TObject);
var
  lMyObject: TMyClass;
begin
  lMyObject := InstanceField.AsObject as TMyClass;
  SerializerObject := TJSONString.Create(lMyObject.ID.ToString + ';' + lMyObject.Description);
end;

end.
