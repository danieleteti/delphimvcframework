unit WebSiteControllerU;

interface

uses
  MVCFramework, System.Diagnostics, JsonDataObjects, MVCFramework.Commons, DAL,
  System.Generics.Collections;

type

  [MVCPath('/')]
  TWebSiteController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionNAme: string;
      var Handled: Boolean); override;
    function GeneratePeopleListAsCSV: String;
  public
    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function PeopleList: String;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_CSV)]
    // RESTful API, requires ACCEPT=text/csv
    function ExportPeopleListAsCSV_API: String;

    [MVCPath('/people/formats/csv')]
    [MVCHTTPMethods([httpGET])]
    // Route usable by the browser, doesn't requires ACCEPT=text/csv
    function ExportPeopleListAsCSV: String;

    [MVCPath('/people')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure SavePerson(const [MVCFromBody] Person: TPerson);

    [MVCPath('/deleteperson')]
    [MVCHTTPMethods([httpPOST])]
    [MVCConsumes(TMVCMediaType.APPLICATION_FORM_URLENCODED)]
    procedure DeletePerson([MVCFromContentField('guid')] const GUID: String);

    [MVCPath('/new')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function NewPerson: String;

    [MVCPath('/edit/($guid)')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function EditPerson(guid: string): String;

    [MVCPath('/')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    procedure Index;

    [MVCPath('/showcase')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function MustacheTemplateShowCase: String;

    [MVCPath('/loadviewtest')]
    [MVCHTTPMethods([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_PLAIN)]
    procedure LoadViewTest;
  end;

implementation

{ TWebSiteController }

uses System.SysUtils, Web.HTTPApp, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

procedure TWebSiteController.DeletePerson(const GUID: String);
var
  LDAL: IPeopleDAL;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  LDAL.DeleteByGUID(GUID);
  Redirect('/people');
end;

function TWebSiteController.EditPerson(guid: string): String;
var
  LDAL: IPeopleDAL;
  lPerson: TPerson;
  lDevices: TArray<String>;
  lJDevices: TJSONArray;
  lItem: string;
  lIdx: Integer;
  lJObj: TJsonObject;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPerson := LDAL.GetPersonByGUID(guid);
  try
    lDevices := LDAL.GetDevicesList;
    ViewData['person'] := lPerson;
    lJObj := TJsonObject.Create;
    try
      lJDevices := lJObj.A['devices'];
      for lItem in lDevices do
      begin
        var lJItm := lJDevices.AddObject;
        lJItm.S['name'] := lItem;
        lJItm.B['selected'] := TArray.BinarySearch<String>(lDevices, lItem, lIdx);
      end;
      Result := Page(['editperson'], lJObj);
    finally
      lJObj.Free;
    end;
  finally
    lPerson.Free;
  end;
end;

function TWebSiteController.ExportPeopleListAsCSV: String;
begin
  Result := GeneratePeopleListAsCSV;
  // define the correct behaviour to download the csv inside the browser
  ContentType := TMVCMediaType.TEXT_CSV;
  Context.Response.CustomHeaders.Values['Content-Disposition'] :=
    'attachment; filename=people.csv';
end;

function TWebSiteController.ExportPeopleListAsCSV_API: String;
begin
  Result := GeneratePeopleListAsCSV;
end;

function TWebSiteController.GeneratePeopleListAsCSV: String;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    Result := PageFragment(['people_header.csv', 'people_list.csv']);
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.Index;
begin
  Redirect('/people');
end;

procedure TWebSiteController.LoadViewTest;
var
  lDS: TFDMemTable;
begin
  lDS := TFDMemTable.Create(nil);
  try
    lDS.FieldDefs.Add('id', ftInteger);
    lDS.FieldDefs.Add('first_name', ftString, 40);
    lDS.FieldDefs.Add('last_name', ftString, 40);
    lDS.FieldDefs.Add('age', ftInteger);
    lDS.CreateDataSet;
    lDS.AppendRecord([1,'Daniele','Teti',44]);
    lDS.AppendRecord([2,'Bruce','Banner',54]);
    lDS.AppendRecord([3,'Peter','Parker',34]);
    lDS.First;

    ViewData['people'] := lDS;
    LoadView(['people_list_test','people_list_test']);
    RenderResponseStream;
  finally
    lDS.Free;
  end;
end;

function TWebSiteController.MustacheTemplateShowCase: String;
var
  LDAL: IPeopleDAL;
  lPeople, lPeople2: TPeople;
  lMyObj: TMyObj;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    lPeople2 := TPeople.Create;
    try
      lMyObj := TMyObj.Create;
      try
        lMyObj.RawHTML := '<h1>This is</h1>Raw<br><span>HTML</span>';
        ViewData['people'] := lPeople;
        ViewData['people2'] := lPeople2;
        ViewData['myobj'] := lMyObj;
        Result := Page(['showcase'], False);
      finally
        lMyObj.Free;
      end;
    finally
      lPeople2.Free;
    end;
  finally
    lPeople.Free;
  end;
end;

function TWebSiteController.NewPerson: String;
var
  LDAL: IPeopleDAL;
  lDevices: TArray<String>;
  lJObj: TJsonObject;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lDevices := LDAL.GetDevicesList;
  lJObj := TJsonObject.Create;
  try
    var lJDevices := lJObj.A['devices'];
    for var lItem in lDevices do
    begin
      var lJItm := lJDevices.AddObject;
      lJItm.S['name'] := lItem;
      lJItm.B['selected'] := False;
    end;
    Result := Page(['editperson'], lJObj);
  finally
    lJObj.Free;
  end;
end;

procedure TWebSiteController.OnBeforeAction(Context: TWebContext;
  const AActionNAme: string; var Handled: Boolean);
begin
  inherited;
  SetPagesCommonHeaders(['header']);
  SetPagesCommonFooters(['footer']);
  if not AActionNAme.ToLower.Contains('test') then ContentType := 'text/html';
  Handled := False;
end;

function TWebSiteController.PeopleList: String;
var
  LDAL: IPeopleDAL;
  lPeople: TPeople;
begin
  LDAL := TServicesFactory.GetPeopleDAL;
  lPeople := LDAL.GetPeople;
  try
    ViewData['people'] := lPeople;
    Result := Page(['people_list']);
  finally
    lPeople.Free;
  end;
end;

procedure TWebSiteController.SavePerson(const [MVCFromBody] Person: TPerson);
var
  LPeopleDAL: IPeopleDAL;
begin
  if Person.FirstName.IsEmpty or Person.LastName.IsEmpty or (Person.Age <= 0) then
  begin
    { TODO -oDaniele -cGeneral : Show how to properly render an exception }
    raise EMVCException.Create('Invalid data',
      'First name, last name and age are not optional', 0);
  end;

  LPeopleDAL := TServicesFactory.GetPeopleDAL;
  LPeopleDAL.AddPerson(Person.FirstName, Person.LastName,
    Person.Age, Person.Devices);
  Redirect('/people');
end;

end.
