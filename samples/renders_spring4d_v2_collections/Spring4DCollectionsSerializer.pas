unit Spring4DCollectionsSerializer;

interface

uses
  MVCFramework.Serializer.Intf,
  System.Rtti,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.JsonDataObjects;

type
  TSpringListSerializer = class(TInterfacedObject, IMVCTypeSerializer)
  private
    FSerializer: IMVCSerializer;
    function Serializer: TMVCJsonDataObjectsSerializer;
  public
    procedure SerializeAttribute(const AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure SerializeRoot(const AObject: TObject; out ASerializerObject: TObject;
      const AAttributes: TArray<TCustomAttribute>; const ASerializationAction: TMVCSerializationAction = nil);

    procedure DeserializeAttribute(var AElementValue: TValue; const APropertyName: string;
      const ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>);

    procedure DeserializeRoot(const ASerializerObject: TObject; const AObject: TObject;
      const AAttributes: TArray<TCustomAttribute>);
  end;


implementation

uses
  MVCFramework.Serializer.JsonDataObjects.NullableTypes,
  JsonDataObjects,
  System.TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Lists;

{ TSpringListSerializer }

type
  THackFoldedList<T> = class(TFoldedList<T>);

procedure TSpringListSerializer.DeserializeAttribute(var AElementValue: TValue; const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  LList: TAbstractArrayList<TObject>;
  LJson: TJDOJsonArray;
  I: Integer;
  LObj: TObject;
begin
  if AElementValue.Kind = tkInterface then
    LList := TAbstractArrayList<TObject>(AElementValue.AsInterface)
  else
    LList := TAbstractArrayList<TObject>(AElementValue.AsObject);

  LJson := ASerializerObject as TJDOJsonArray;
  for I := 0 to Pred(LJson.Count) do
  begin
    LObj := TActivator.CreateInstance(THackFoldedList<TObject>(LList).GetElementType);
    Serializer.JsonObjectToObject(LJson.O[I], LObj, stDefault, []);
    LList.Add(LObj);
  end;
end;

procedure TSpringListSerializer.DeserializeRoot(const ASerializerObject, AObject: TObject; const AAttributes: TArray<TCustomAttribute>);
var
  LList: TAbstractArrayList<TObject>;
  LJson: TJDOJsonArray;
  I: Integer;
  LObj: TObject;
begin
  LList := AObject as TAbstractArrayList<TObject>;
  LJson := ASerializerObject as TJDOJsonArray;
  for I := 0 to Pred(LJson.Count) do
  begin
    LObj := TActivator.CreateInstance(THackFoldedList<TObject>(LList).GetElementType);
    Serializer.JsonObjectToObject(LJson.O[I], LObj, stDefault, []);
    LList.Add(LObj);
  end;
end;

procedure TSpringListSerializer.SerializeAttribute(const AElementValue: TValue; const APropertyName: string; const ASerializerObject: TObject;
  const AAttributes: TArray<TCustomAttribute>);
var
  LList: TAbstractArrayList<TObject>;
  LObj: TObject;
  LOutObject: TJDOJsonObject;
  LJsonArray: TJDOJsonArray;
begin
  if AElementValue.Kind = tkInterface then
    LList := TAbstractArrayList<TObject>(AElementValue.AsInterface)
  else
    LList := TAbstractArrayList<TObject>(AElementValue.AsObject);

  LOutObject := ASerializerObject as TJsonObject;
  LOutObject.A[APropertyName] := TJDOJsonArray.Create;
  LJsonArray := LOutObject.A[APropertyName];
  if Assigned(LJsonArray) then
  begin
    for LObj in LList do
    begin
      Serializer.ObjectToJsonObject(LObj, LJsonArray.AddObject, stDefault, []);
    end;
  end;
end;

function TSpringListSerializer.Serializer: TMVCJsonDataObjectsSerializer;
begin
  if not Assigned(FSerializer) then
  begin
    FSerializer := TMVCJsonDataObjectsSerializer.Create;
    FSerializer.RegisterTypeSerializer(TypeInfo(TFoldedList<TObject>), TSpringListSerializer.Create);
    RegisterNullableTypeSerializersInSerializer(FSerializer);

  end;
  Result := FSerializer as TMVCJsonDataObjectsSerializer;
end;

procedure TSpringListSerializer.SerializeRoot(const AObject: TObject; out ASerializerObject: TObject; const AAttributes: TArray<TCustomAttribute>;
  const ASerializationAction: TMVCSerializationAction);
var
  LOutJsonArray: TJDOJsonArray;
  LList: TAbstractArrayList<TObject>;
  LObj: TObject;
begin
  LOutJsonArray := TJDOJsonArray.Create;

  LList := AObject as TAbstractArrayList<TObject>;
  for LObj in LList do
  begin
    Serializer.ObjectToJsonObject(LObj, LOutJsonArray.AddObject, stDefault, []);
  end;
  ASerializerObject := LOutJsonArray;
end;

end.

