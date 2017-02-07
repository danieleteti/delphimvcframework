unit MVCFramework.Serializer.Intf;

interface

uses
  Data.DB;

type
  IMVCSerUnSer = interface
    ['{1ECA942A-E3C4-45DD-9D23-C00363B5E334}']
    function SerializeObject(AObject: TObject; AIgnoredProperties: array of string): String;
    function SerializeDataSet(ADataSet: TDataSet; AIgnoredFields: array of string): String;
    procedure DeserializeObject(ASerializedObject: String; AObject: TObject);
  end;

implementation

end.
