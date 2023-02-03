unit StorageU;

interface

const
  TITLES: array [1 .. 4] of string = ('IBM', 'AAPL', 'GOOG', 'MSFT');

function GetNextDataToSend(const LastID: Integer;
  out CurrentEventID: Integer): string;

implementation

uses
  System.JSON;

function GetNextDataToSend(const LastID: Integer;
  out CurrentEventID: Integer): string;
var
  lIndex: Integer;
  lJOBJ: TJSONObject;
begin
  // You can get the "next" event reading the LastID or, as in this case,
  // just send another event

  lIndex := LastID;
  while lIndex = LastID do
  begin
    lIndex := Random(Length(Titles)) + 1;
  end;

  lJOBJ := TJSONObject.Create;
  try
    lJOBJ.AddPair('stock', TITLES[lIndex]);
    lJOBJ.AddPair('value', TJSONNumber.Create((500 + Random(200)) +
      (Random(50) / 100)));
    Result := lJOBJ.ToJSON;
    CurrentEventID := LastID + 1;
  finally
    lJOBJ.Free;
  end;
end;

end.
