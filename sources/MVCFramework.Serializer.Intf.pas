unit MVCFramework.Serializer.Intf;

interface

uses
  Data.DB, MVCFramework.DuckTyping, System.Rtti;

const
  DMVC_CLASSNAME = '$dmvc_classname';

type
  TMVCSerializationType = (Properties, Fields);

  IMVCSerializerListener = interface
    ['{5976F9DA-1B89-4F8C-B333-C3612071DEE0}']
    procedure BeforeSerialize(const AObject: TObject);
    procedure AfterDeserialize(const AObject: TObject);
  end;

  IMVCSerializer = interface
    ['{1ECA942A-E3C4-45DD-9D23-C00363B5E334}']
    function SerializeObject(AObject: TObject; AIgnoredProperties: array of string): String;
    function SerializeDataSet(ADataSet: TDataSet; AIgnoredFields: array of string): String;
    function SerializeCollection(AList: TObject; AIgnoredProperties: array of string): String;
    procedure DeserializeObject(ASerializedObject: String; AObject: TObject);
    procedure DeserializeCollection(ASerializedObjectList: string; AList: IMVCList; AClazz: TClass);
    procedure DeserializeDataSet(ASerializedObject: String; const ADataSet: TDataSet);
  end;

  IMVCTypeSerializer = interface
    ['{806EC547-D1CB-4DA9-92D3-A8A7C0BD4009}']
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

end.
