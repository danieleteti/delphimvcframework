unit MVCFramework.Serializer.Intf;

interface

uses
  Data.DB, MVCFramework.DuckTyping;

type
  IMVCSerUnSer = interface
    ['{1ECA942A-E3C4-45DD-9D23-C00363B5E334}']
    function SerializeObject(AObject: TObject; AIgnoredProperties: array of string): String;
    function SerializeDataSet(ADataSet: TDataSet; AIgnoredFields: array of string): String;
    function SerializeCollection(AList: TObject; AIgnoredProperties: array of string): String;
    procedure DeserializeObject(ASerializedObject: String; AObject: TObject);
    procedure DeserializeCollection(ASerializedObjectList: string; AList: IMVCList; AClazz: TClass);
  end;

implementation

end.
