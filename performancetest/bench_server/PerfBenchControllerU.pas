unit PerfBenchControllerU;

{
  DMVCFramework Performance Benchmark - Controllers
  --------------------------------------------------
  Scenarios to isolate framework overhead:

  - /bench/health         : tiny JSON (~50 bytes), pure framework overhead
  - /bench/json/small     : fixed ~500 B JSON, TJsonObject -> opt#1 fast path
  - /bench/json/large     : ~20 KB JSON array, TJsonObject -> opt#1 fast path
  - /bench/upload         : POST sink, measures body read path
  - /bench/heavy          : tiny payload traversing the middleware chain
  - /bench/pods/small     : same shape as json/small but built as a TObject
                            (TBenchItem), forces the serializer to walk RTTI
                            and exercise opt#6 (UTF-8 bytes direct path).
  - /bench/pods/large     : 100-record TObjectList<TBenchItem>, exercises
                            opt#6 under larger tree-building cost.

  Keep this unit lean. No DB, no logging, no file I/O. We measure the
  framework, not IO subsystems.
}

interface

uses
  System.Generics.Collections,
  MVCFramework, MVCFramework.Commons;

type
  TBenchItem = class
  private
    FId: Integer;
    FSku: string;
    FName: string;
    FDescription: string;
    FPrice: Double;
    FStock: Integer;
    FActive: Boolean;
  public
    property Id: Integer read FId write FId;
    property Sku: string read FSku write FSku;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Price: Double read FPrice write FPrice;
    property Stock: Integer read FStock write FStock;
    property Active: Boolean read FActive write FActive;
  end;

  [MVCPath('/bench')]
  TPerfBenchController = class(TMVCController)
  public
    [MVCPath('/health')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function Health: IMVCResponse;

    [MVCPath('/json/small')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function JsonSmall: IMVCResponse;

    [MVCPath('/json/large')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function JsonLarge: IMVCResponse;

    [MVCPath('/upload')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function Upload: IMVCResponse;

    [MVCPath('/heavy')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function Heavy: IMVCResponse;

    [MVCPath('/pods/small')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function PodSmall: IMVCResponse;

    [MVCPath('/pods/large')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.APPLICATION_JSON)]
    function PodLarge: IMVCResponse;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.Classes,
  JsonDataObjects;

function TPerfBenchController.Health: IMVCResponse;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  LObj.S['status'] := 'ok';
  LObj.L['ts'] := DateTimeToUnix(Now, False);
  Result := OKResponse(LObj);
end;

function TPerfBenchController.JsonSmall: IMVCResponse;
var
  LObj: TJSONObject;
  I: Integer;
begin
  LObj := TJSONObject.Create;
  LObj.I['id'] := 12345;
  LObj.S['name'] := 'Benchmark Item';
  LObj.S['description'] := 'A small fixed-size payload for serializer warm-path measurement';
  LObj.B['active'] := True;
  LObj.F['price'] := 19.99;
  for I := 1 to 5 do
    LObj.A['tags'].Add(Format('tag_%d', [I]));
  Result := OKResponse(LObj);
end;

function TPerfBenchController.JsonLarge: IMVCResponse;
var
  LRoot: TJSONObject;
  LArr: TJSONArray;
  LItem: TJSONObject;
  I: Integer;
begin
  LRoot := TJSONObject.Create;
  LRoot.I['count'] := 100;
  LArr := LRoot.A['items'];
  for I := 1 to 100 do
  begin
    LItem := LArr.AddObject;
    LItem.I['id'] := I;
    LItem.S['sku'] := Format('SKU-%.6d', [I]);
    LItem.S['name'] := Format('Product number %d with a moderately long name', [I]);
    LItem.S['description'] :=
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
      'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
    LItem.F['price'] := 9.99 + I;
    LItem.I['stock'] := I * 7;
    LItem.B['active'] := (I mod 3) <> 0;
  end;
  Result := OKResponse(LRoot);
end;

function TPerfBenchController.Upload: IMVCResponse;
var
  LObj: TJSONObject;
  LSize: Int64;
begin
  LSize := Context.Request.ContentLength;
  LObj := TJSONObject.Create;
  LObj.L['received'] := LSize;
  LObj.S['status'] := 'ok';
  Result := OKResponse(LObj);
end;

function TPerfBenchController.Heavy: IMVCResponse;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  LObj.S['status'] := 'ok';
  LObj.S['scenario'] := 'heavy-chain';
  Result := OKResponse(LObj);
end;

function TPerfBenchController.PodSmall: IMVCResponse;
var
  LItem: TBenchItem;
begin
  LItem := TBenchItem.Create;
  LItem.Id := 12345;
  LItem.Sku := 'SKU-000001';
  LItem.Name := 'Benchmark Item';
  LItem.Description := 'A small fixed-size POD exercised through the RTTI serializer path';
  LItem.Price := 19.99;
  LItem.Stock := 42;
  LItem.Active := True;
  Result := OKResponse(LItem);
end;

function TPerfBenchController.PodLarge: IMVCResponse;
var
  LList: TObjectList<TBenchItem>;
  LItem: TBenchItem;
  I: Integer;
begin
  LList := TObjectList<TBenchItem>.Create(True);
  for I := 1 to 100 do
  begin
    LItem := TBenchItem.Create;
    LItem.Id := I;
    LItem.Sku := Format('SKU-%.6d', [I]);
    LItem.Name := Format('Product number %d with a moderately long name', [I]);
    LItem.Description :=
      'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' +
      'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.';
    LItem.Price := 9.99 + I;
    LItem.Stock := I * 7;
    LItem.Active := (I mod 3) <> 0;
    LList.Add(LItem);
  end;
  Result := OKResponse(LList);
end;

end.
