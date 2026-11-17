unit SerializerPrecedenceSupportU;

// Support for the serializer precedence tests (TestClient: SerializerPrecedenceTestsU).
//
// Rule 1: a serializer the user registered always wins, also for application/json.
// Rule 2: nothing registered for application/*+json -> the JSON serializer answers,
//         the Content-Type stays the negotiated one.
//
// The controller has the same actions under three media types:
//   /serprec/json/...  no MVCProduces            (application/json)
//   /serprec/vnd/...   application/vnd.api+json  (nothing registered for it)
//   /serprec/own/...   application/x-own+json    (TMarkerJSONSerializer registered for it)
// so the body of a +json type can be compared with the body of the application/json twin.

interface

uses
  System.Classes,
  System.Generics.Collections,
  Data.DB,
  JsonDataObjects,
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Intf,
  MVCFramework.Serializer.JsonDataObjects;

type
  TPrecItem = class
  private
    fName: string;
    fValue: Integer;
  public
    property Name: string read fName write fName;
    property Value: Integer read fValue write fValue;
  end;

  // a nested object: not streamable by the fast path, the serializer builds the tree
  TPrecNested = class
  private
    fName: string;
    fChild: TPrecItem;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read fName write fName;
    property Child: TPrecItem read fChild;
  end;

  // JSON serializer that tells itself: the body it writes is {"custom":true,"body":<what the built-in writes>}
  TMarkerJSONSerializer = class(TMVCJsonDataObjectsSerializer, IMVCSerializer)
  public
    function SerializeObject(const AObject: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;
    function SerializeCollection(const AList: TObject; const AType: TMVCSerializationType = stDefault;
      const AIgnoredAttributes: TMVCIgnoredList = []; const ASerializationAction: TMVCSerializationAction = nil)
      : string; overload;
    function SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList = [];
      const ANameCase: TMVCNameCase = ncAsIs; const ASerializationAction: TMVCDatasetSerializationAction = nil): string;
    function SerializeDataSetRecord(const DataSet: TDataSet; const IgnoredFields: TMVCIgnoredList;
      const NameCase: TMVCNameCase = ncAsIs; const SerializationAction: TMVCDatasetSerializationAction = nil): string;
  end;

const
  MARKER_PREFIX = '{"custom":true,"body":';
  MEDIA_VND = 'application/vnd.api+json';
  MEDIA_OWN = 'application/x-own+json';

type
  [MVCPath('/serprec')]
  TSerializerPrecedenceController = class(TMVCController)
  public
    // ---- application/json ---------------------------------------------
    [MVCPath('/json/jsonobject')]
    [MVCHTTPMethod([httpGET])]
    function JsonJsonObject: IMVCResponse;
    [MVCPath('/json/simple')]
    [MVCHTTPMethod([httpGET])]
    function JsonSimple: IMVCResponse;
    [MVCPath('/json/list')]
    [MVCHTTPMethod([httpGET])]
    function JsonList: IMVCResponse;
    [MVCPath('/json/nested')]
    [MVCHTTPMethod([httpGET])]
    function JsonNested: IMVCResponse;
    [MVCPath('/json/okdataset')]
    [MVCHTTPMethod([httpGET])]
    function JsonOKDataSet: IMVCResponse;
    [MVCPath('/json/direct')]
    [MVCHTTPMethod([httpGET])]
    function JsonDirect: TJsonObject;
    [MVCPath('/json/dataset')]
    [MVCHTTPMethod([httpGET])]
    function JsonDataSet: TDataSet;

    // ---- application/vnd.api+json, nothing registered -----------------
    [MVCPath('/vnd/jsonobject')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndJsonObject: IMVCResponse;
    [MVCPath('/vnd/simple')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndSimple: IMVCResponse;
    [MVCPath('/vnd/list')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndList: IMVCResponse;
    [MVCPath('/vnd/nested')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndNested: IMVCResponse;
    [MVCPath('/vnd/okdataset')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndOKDataSet: IMVCResponse;
    [MVCPath('/vnd/direct')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndDirect: TJsonObject;
    [MVCPath('/vnd/dataset')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_VND)]
    function VndDataSet: TDataSet;

    // ---- application/x-own+json, TMarkerJSONSerializer registered -----
    [MVCPath('/own/jsonobject')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_OWN)]
    function OwnJsonObject: IMVCResponse;
    [MVCPath('/own/simple')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_OWN)]
    function OwnSimple: IMVCResponse;
    [MVCPath('/own/nested')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(MEDIA_OWN)]
    function OwnNested: IMVCResponse;

    // ---- the action sets Context.Response.ContentType by itself (the renderer is not told) --------
    [MVCPath('/csv/dataset')]
    [MVCHTTPMethod([httpGET])]
    function CsvDataSet: TDataSet;
    [MVCPath('/csv/jsonobject')]
    [MVCHTTPMethod([httpGET])]
    function CsvJsonObject: IMVCResponse;
    [MVCPath('/csv/direct')]
    [MVCHTTPMethod([httpGET])]
    function CsvDirect: TJsonObject;
    [MVCPath('/csv/simple')]
    [MVCHTTPMethod([httpGET])]
    function CsvSimple: IMVCResponse;

    // ---- the action sets Context.Response.ContentType to JSON by itself -----------------------
    [MVCPath('/jsonct/jsonobject')]
    [MVCHTTPMethod([httpGET])]
    function JsonCTJsonObject: IMVCResponse;
    [MVCPath('/jsonct/simple')]
    [MVCHTTPMethod([httpGET])]
    function JsonCTSimple: IMVCResponse;
    [MVCPath('/jsonct/direct')]
    [MVCHTTPMethod([httpGET])]
    function JsonCTDirect: TJsonObject;
    [MVCPath('/jsonct/dataset')]
    [MVCHTTPMethod([httpGET])]
    function JsonCTDataSet: TDataSet;

    // ---- the request body: Content-Type application/vnd.api+json ------
    [MVCPath('/req/frombody')]
    [MVCHTTPMethod([httpPOST])]
    function ReqFromBody(const [MVCFromBody] Item: TPrecItem): IMVCResponse;
    [MVCPath('/req/bodyas')]
    [MVCHTTPMethod([httpPOST])]
    function ReqBodyAs: IMVCResponse;
    [MVCPath('/req/bodyfor')]
    [MVCHTTPMethod([httpPOST])]
    function ReqBodyFor: IMVCResponse;
    [MVCPath('/req/bodyaslist')]
    [MVCHTTPMethod([httpPOST])]
    function ReqBodyAsListOf: IMVCResponse;
    [MVCPath('/req/bodyforlist')]
    [MVCHTTPMethod([httpPOST])]
    function ReqBodyForListOf: IMVCResponse;
  end;

// Minimal API routes: /serprec/min/json (application/json), /serprec/min/vnd (the handler sets
// application/vnd.api+json), both answering OKResponse(TJsonObject) and OKResponse(simple object)
procedure RegisterSerializerPrecedenceRoutes(const AEngine: TMVCEngine);

implementation

uses
  System.SysUtils,
  MVCFramework.MinimalAPI,
  FireDAC.Comp.Client;

function NewJsonObject: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.S['name'] := 'Daniele';
  Result.I['value'] := 42;
end;

function NewItem(const AName: string; const AValue: Integer): TPrecItem;
begin
  Result := TPrecItem.Create;
  Result.Name := AName;
  Result.Value := AValue;
end;

function NewList: TObjectList<TPrecItem>;
begin
  Result := TObjectList<TPrecItem>.Create(True);
  Result.Add(NewItem('one', 1));
  Result.Add(NewItem('two', 2));
end;

function NewNested: TPrecNested;
begin
  Result := TPrecNested.Create;
  Result.Name := 'parent';
  Result.Child.Name := 'child';
  Result.Child.Value := 7;
end;

function NewDataSet: TDataSet;
var
  lTable: TFDMemTable;
begin
  lTable := TFDMemTable.Create(nil);
  try
    lTable.FieldDefs.Add('id', ftInteger);
    lTable.FieldDefs.Add('name', ftString, 20);
    lTable.CreateDataSet;
    lTable.AppendRecord([1, 'one']);
    lTable.AppendRecord([2, 'two']);
    lTable.First;
  except
    lTable.Free;
    raise;
  end;
  Result := lTable;
end;

{ TPrecNested }

constructor TPrecNested.Create;
begin
  inherited;
  fChild := TPrecItem.Create;
end;

destructor TPrecNested.Destroy;
begin
  fChild.Free;
  inherited;
end;

{ TMarkerJSONSerializer }

function TMarkerJSONSerializer.SerializeObject(const AObject: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  Result := MARKER_PREFIX + inherited SerializeObject(AObject, AType, AIgnoredAttributes, ASerializationAction) + '}';
end;

function TMarkerJSONSerializer.SerializeCollection(const AList: TObject; const AType: TMVCSerializationType;
  const AIgnoredAttributes: TMVCIgnoredList; const ASerializationAction: TMVCSerializationAction): string;
begin
  Result := MARKER_PREFIX + inherited SerializeCollection(AList, AType, AIgnoredAttributes, ASerializationAction) + '}';
end;

function TMarkerJSONSerializer.SerializeDataSet(const ADataSet: TDataSet; const AIgnoredFields: TMVCIgnoredList;
  const ANameCase: TMVCNameCase; const ASerializationAction: TMVCDatasetSerializationAction): string;
begin
  Result := MARKER_PREFIX + inherited SerializeDataSet(ADataSet, AIgnoredFields, ANameCase, ASerializationAction) + '}';
end;

function TMarkerJSONSerializer.SerializeDataSetRecord(const DataSet: TDataSet; const IgnoredFields: TMVCIgnoredList;
  const NameCase: TMVCNameCase; const SerializationAction: TMVCDatasetSerializationAction): string;
begin
  Result := MARKER_PREFIX + inherited SerializeDataSetRecord(DataSet, IgnoredFields, NameCase, SerializationAction) + '}';
end;

{ TSerializerPrecedenceController: application/json }

function TSerializerPrecedenceController.JsonJsonObject: IMVCResponse;
begin
  Result := OKResponse(NewJsonObject);
end;

function TSerializerPrecedenceController.JsonSimple: IMVCResponse;
begin
  Result := OKResponse(NewItem('Daniele', 42));
end;

function TSerializerPrecedenceController.JsonList: IMVCResponse;
begin
  Result := OKResponse(NewList);
end;

function TSerializerPrecedenceController.JsonNested: IMVCResponse;
begin
  Result := OKResponse(NewNested);
end;

function TSerializerPrecedenceController.JsonOKDataSet: IMVCResponse;
begin
  Result := OKResponse(NewDataSet);
end;

function TSerializerPrecedenceController.JsonDirect: TJsonObject;
begin
  Result := NewJsonObject;
end;

function TSerializerPrecedenceController.JsonDataSet: TDataSet;
begin
  Result := NewDataSet;
end;

{ application/vnd.api+json }

function TSerializerPrecedenceController.VndJsonObject: IMVCResponse;
begin
  Result := OKResponse(NewJsonObject);
end;

function TSerializerPrecedenceController.VndSimple: IMVCResponse;
begin
  Result := OKResponse(NewItem('Daniele', 42));
end;

function TSerializerPrecedenceController.VndList: IMVCResponse;
begin
  Result := OKResponse(NewList);
end;

function TSerializerPrecedenceController.VndNested: IMVCResponse;
begin
  Result := OKResponse(NewNested);
end;

function TSerializerPrecedenceController.VndOKDataSet: IMVCResponse;
begin
  Result := OKResponse(NewDataSet);
end;

function TSerializerPrecedenceController.VndDirect: TJsonObject;
begin
  Result := NewJsonObject;
end;

function TSerializerPrecedenceController.VndDataSet: TDataSet;
begin
  Result := NewDataSet;
end;

{ application/x-own+json }

function TSerializerPrecedenceController.OwnJsonObject: IMVCResponse;
begin
  Result := OKResponse(NewJsonObject);
end;

function TSerializerPrecedenceController.OwnSimple: IMVCResponse;
begin
  Result := OKResponse(NewItem('Daniele', 42));
end;

function TSerializerPrecedenceController.OwnNested: IMVCResponse;
begin
  Result := OKResponse(NewNested);
end;

{ Response.ContentType set directly }

function TSerializerPrecedenceController.CsvDataSet: TDataSet;
begin
  Context.Response.ContentType := 'text/csv';
  Result := NewDataSet;
end;

function TSerializerPrecedenceController.CsvJsonObject: IMVCResponse;
begin
  Context.Response.ContentType := 'text/csv';
  Result := OKResponse(NewJsonObject);
end;

function TSerializerPrecedenceController.CsvDirect: TJsonObject;
begin
  Context.Response.ContentType := 'text/csv';
  Result := NewJsonObject;
end;

function TSerializerPrecedenceController.CsvSimple: IMVCResponse;
begin
  Context.Response.ContentType := 'text/csv';
  Result := OKResponse(NewItem('Daniele', 42));
end;

{ Response.ContentType set directly to JSON }

function TSerializerPrecedenceController.JsonCTJsonObject: IMVCResponse;
begin
  Context.Response.ContentType := 'application/json; charset=utf-8';
  Result := OKResponse(NewJsonObject);
end;

function TSerializerPrecedenceController.JsonCTSimple: IMVCResponse;
begin
  Context.Response.ContentType := 'application/json; charset=utf-8';
  Result := OKResponse(NewItem('Daniele', 42));
end;

function TSerializerPrecedenceController.JsonCTDirect: TJsonObject;
begin
  Context.Response.ContentType := 'application/json; charset=utf-8';
  Result := NewJsonObject;
end;

function TSerializerPrecedenceController.JsonCTDataSet: TDataSet;
begin
  Context.Response.ContentType := 'application/json; charset=utf-8';
  Result := NewDataSet;
end;

{ request body }

function TSerializerPrecedenceController.ReqFromBody(const Item: TPrecItem): IMVCResponse;
begin
  Result := OKResponse(Item.Name + ':' + Item.Value.ToString);
end;

function TSerializerPrecedenceController.ReqBodyAs: IMVCResponse;
var
  lItem: TPrecItem;
begin
  lItem := Context.Request.BodyAs<TPrecItem>;
  try
    Result := OKResponse(lItem.Name + ':' + lItem.Value.ToString);
  finally
    lItem.Free;
  end;
end;

function TSerializerPrecedenceController.ReqBodyFor: IMVCResponse;
var
  lItem: TPrecItem;
begin
  lItem := TPrecItem.Create;
  try
    Context.Request.BodyFor<TPrecItem>(lItem);
    Result := OKResponse(lItem.Name + ':' + lItem.Value.ToString);
  finally
    lItem.Free;
  end;
end;

function TSerializerPrecedenceController.ReqBodyAsListOf: IMVCResponse;
var
  lList: TObjectList<TPrecItem>;
begin
  lList := Context.Request.BodyAsListOf<TPrecItem>;
  try
    Result := OKResponse(lList.Count.ToString + ':' + lList[0].Name);
  finally
    lList.Free;
  end;
end;

function TSerializerPrecedenceController.ReqBodyForListOf: IMVCResponse;
var
  lList: TObjectList<TPrecItem>;
begin
  lList := TObjectList<TPrecItem>.Create(True);
  try
    Context.Request.BodyForListOf<TPrecItem>(lList);
    Result := OKResponse(lList.Count.ToString + ':' + lList[0].Name);
  finally
    lList.Free;
  end;
end;

{ Minimal API }

procedure RegisterSerializerPrecedenceRoutes(const AEngine: TMVCEngine);
begin
  AEngine.Root.MapGet('/serprec/min/json',
    function: IMVCResponse
    begin
      Result := Ok(NewJsonObject);
    end);
  AEngine.Root.MapGet('/serprec/min/simple',
    function: IMVCResponse
    begin
      Result := Ok(NewItem('Daniele', 42));
    end);
  AEngine.Root.MapGet<TWebContext>('/serprec/min/vnd',
    function(AContext: TWebContext): IMVCResponse
    begin
      AContext.Response.ContentType := MEDIA_VND;
      Result := Ok(NewJsonObject);
    end);
  AEngine.Root.MapGet<TWebContext>('/serprec/min/vndsimple',
    function(AContext: TWebContext): IMVCResponse
    begin
      AContext.Response.ContentType := MEDIA_VND;
      Result := Ok(NewItem('Daniele', 42));
    end);
end;

end.
