unit IocpDsServerMethods;

interface

uses
  System.SysUtils, Datasnap.DSServer, Data.DBXJSON;

type
  TServerMethodsDs = class(TDSServerModule)
  public
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function TestJSON(Level: Integer): TJSONValue;
  end;

implementation

uses
  System.StrUtils;

{$R *.dfm}

function TServerMethodsDs.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethodsDs.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

function TServerMethodsDs.TestJSON(Level: Integer): TJSONValue;
var
  LResult: TJSONArray;
  LChild: TJSONArray;
  I: Integer;
begin
  LResult := TJSONArray.Create;
  while (Level > 0) do
  begin
    LChild := TJSONArray.Create;
    for I := 1 to 5 do
    begin
      LChild.Add('child' + IntToStr(I));
    end;
    LResult.Add(LChild);
    Dec(Level);
  end;
  Result := LResult;
end;

end.
