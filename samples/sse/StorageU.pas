unit StorageU;

interface

const
  TITLES: array [1 .. 4] of String = ('IBM', 'AAPL', 'GOOG', 'MSFT');

function GetNextDataToSend(const LastID: Integer;
  out CurrentEventID: Integer): String;

implementation

uses
  System.JSON;

function GetNextDataToSend(const LastID: Integer;
  out CurrentEventID: Integer): String;
var
  lIndex: Integer;
  lJOBJ: TJSONObject;
begin
  if LastID < High(TITLES) then
    lIndex := LastID + 1
  else
    lIndex := 1;

  lJOBJ := TJSONObject.Create;
  try
    lJOBJ.AddPair('stock', TITLES[lIndex]);
    lJOBJ.AddPair('value', TJSONNumber.Create((500 + Random(200)) +
      (Random(50) / 100)));
    Result := lJOBJ.ToJSON;
    CurrentEventID := lIndex;
  finally
    lJOBJ.Free;
  end;
end;

end.
