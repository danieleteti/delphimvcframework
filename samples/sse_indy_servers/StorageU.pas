unit StorageU;

interface

uses
  System.Classes;

type
  TStockPriceGenerator = class(TThread)
  private
    FChannel: string;
    FEventId: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AChannel: string);
  end;

procedure StartStockPriceGenerator(const AChannel: string = '/stocks');
procedure StopStockPriceGenerator;

implementation

uses
  System.SysUtils, System.JSON, MVCFramework.SSE;

const
  STOCK_NAMES: array[0..3] of string = ('IBM', 'AAPL', 'GOOG', 'MSFT');

var
  GGenerator: TStockPriceGenerator = nil;

procedure StartStockPriceGenerator(const AChannel: string);
begin
  if GGenerator = nil then
    GGenerator := TStockPriceGenerator.Create(AChannel);
end;

procedure StopStockPriceGenerator;
begin
  if GGenerator <> nil then
  begin
    GGenerator.Terminate;
    GGenerator.WaitFor;
    FreeAndNil(GGenerator);
  end;
end;

{ TStockPriceGenerator }

constructor TStockPriceGenerator.Create(const AChannel: string);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FChannel := AChannel;
  FEventId := 0;
end;

procedure TStockPriceGenerator.Execute;
var
  LJSON: TJSONObject;
  LStockIdx: Integer;
  LMsg: TSSEMessage;
begin
  Randomize;
  while not Terminated do
  begin
    if SSEBroker.ConnectionCount(FChannel) > 0 then
    begin
      Inc(FEventId);
      LStockIdx := Random(Length(STOCK_NAMES));

      LJSON := TJSONObject.Create;
      try
        LJSON.AddPair('stock', STOCK_NAMES[LStockIdx]);
        LJSON.AddPair('value', TJSONNumber.Create((500 + Random(200)) + (Random(50) / 100)));

        LMsg := TSSEMessage.Create('stockupdate', LJSON.ToJSON, FEventId.ToString);
        SSEBroker.Broadcast(FChannel, LMsg);
      finally
        LJSON.Free;
      end;
    end;
    // Random delay between 1000 and 1500ms
    Sleep(1000 + Random(500));
  end;
end;

end.
