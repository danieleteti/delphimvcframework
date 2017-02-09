unit MVCFramework.Serializer.Intf;

interface

uses
  Data.DB, MVCFramework.DuckTyping;

const
  DMVC_CLASSNAME = '$dmvc_classname';

type
  TMVCSerializationType = (Properties, Fields);

  IMVCSerUnSerListener = interface
    ['{5976F9DA-1B89-4F8C-B333-C3612071DEE0}']
    procedure BeforeSerialize(const AObject: TObject);
    procedure AfterDeserialize(const AObject: TObject);
  end;

  IMVCSerUnSer = interface
    ['{1ECA942A-E3C4-45DD-9D23-C00363B5E334}']
    function SerializeObject(AObject: TObject; AIgnoredProperties: array of string): String;
    function SerializeObjectStrict(AObject: TObject): String;
    function SerializeDataSet(ADataSet: TDataSet; AIgnoredFields: array of string): String;
    function SerializeCollection(AList: TObject; AIgnoredProperties: array of string): String;
    function SerializeCollectionStrict(AList: TObject): String;
    procedure DeserializeObject(ASerializedObject: String; AObject: TObject);
    procedure DeserializeCollection(ASerializedObjectList: string; AList: IMVCList; AClazz: TClass);
  end;

implementation

end.
