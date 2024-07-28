unit JSONSampleController;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    function Index: String;
  end;

implementation

uses
  System.Classes, System.JSON.Writers, System.JSON.Types;

function TMyController.Index: String;
begin
  var lJSONWriter := TJsonTextWriter.Create(TStringWriter.Create(), True);
  try
    lJSONWriter.Formatting := TJsonFormatting.Indented;
    lJSONWriter.WriteStartObject;
    lJSONWriter.WritePropertyName('Users');
    lJSONWriter.WriteStartArray;
    var Arr := ['Daniele','Peter','Scott'];
    for var oUser in Arr do
    begin
      lJSONWriter.WriteStartObject;
      lJSONWriter.WritePropertyName('UserName');
      lJSONWriter.WriteValue(oUser);
      lJSONWriter.WriteEndObject;
    end;
    lJSONWriter.WriteEndArray;
    lJSONWriter.WriteEndObject;
    Result := lJSONWriter.Writer.ToString;
  finally
    lJSONWriter.Free;
  end;
end;

end.
