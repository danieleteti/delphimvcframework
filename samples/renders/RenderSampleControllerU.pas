// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators with this file: Ezequiel Juliano Müller (ezequieljuliano@gmail.com)
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

unit RenderSampleControllerU;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Serializer.Intf,
  System.Rtti,
  System.Generics.Collections,
  BusinessObjectsU, Data.DB, System.Classes, System.SysUtils;

type

  [MVCPath('/')]
  TRenderSampleController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;
  public
    // Result BASED
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/people/1')]
    [MVCProduces('application/json')]
    function GetPerson_AsFunction: TPerson;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/people')]
    [MVCProduces('application/json')]
    function GetPeople_AsObjectList_AsFunction: TEnumerable<TPerson>;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/customers/simple')]
    function GetCustomers_AsDataSet_AsFunction: TDataSet;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/customers/($ID)')]
    [MVCProduces('text/plain')]
    function GetPerson_AsText_AsFunction(const ID: Integer): String;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/customers.csv')]
    function GetPeopleAsCSV_AsFunction: String;


    // this action is polymorphic
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/skilledpeople')]
    //[MVCProduces('application/json')]
    function GetProgrammersAndPhilosophersAsObjectList_AsFunction: TObjectList<TPerson>;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/func/skilledpeople/withmvcresponse')]
    [MVCProduces('application/json')]
    function GetProgrammersAndPhilosophersAsObjectList_withmvcresponse_AsFunction: IMVCResponse;


    // Render BASED
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/simple')]
    procedure GetCustomers_AsDataSet;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers')]
    procedure GetCustomersAsDataSetWithRefLinks;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/withcallback')]
    procedure GetCustomersWithCallback;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($ID)')]
    [MVCProduces('text/plain')]
    procedure GetPerson_AsText(const ID: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/dateandtimes/showcase')]
    procedure GetDateAndTimeShowcase;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers2')]
    procedure GetCustomersWithObjectDictionary;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($ID)/asdataset')]
    procedure GetCustomer_AsDataSetRecord(const ID: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/metadata/all')]
    [MVCProduces('application/json')]
    procedure GetDataSetWithMetadata;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers2/($ID)')]
    [MVCProduces('application/json')]
    procedure GetCustomerByID_AsTObject(const ID: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/multi')]
    [MVCProduces('application/json')]
    procedure GetCustomersAndCountry_AsDataSet;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people/alias')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList_With_Alias;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/objectdict/nil')]
    [MVCProduces('application/json')]
    procedure GetNil;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/interfacedpeople')]
    [MVCProduces('application/json')]
    procedure GetInterfacedPeople;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people/hateoas')]
    [MVCProduces('application/json')]
    procedure GetPeople_AsObjectList_HATEOAS;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people/withtiming')]
    [MVCProduces('application/json')]
    procedure GetPeopleWithTiming;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/people/($ID)')]
    [MVCProduces('application/json')]
    procedure GetPersonById(const ID: Integer);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/lotofobjects')]
    procedure GetLotOfPeople;

    // this action is polymorphic
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/skilledpeople')]
    [MVCProduces('application/json')]
    procedure GetProgrammersAndPhilosophersAsObjectList;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/view/($ID).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetPerson_AsHTMLView;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/($ID).html')]
    [MVCProduces('text/html')]
    procedure GetPerson_AsHTML;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers.csv')]
    procedure GetPeopleAsCSV;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customers/unicode/($ID).html')]
    [MVCProduces('text/html', 'UTF-8')]
    procedure GetUnicodeText_AsHTML;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/files/customers.json')]
    [MVCProduces('application/json')]
    procedure GetPersonJSON;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/files/customers.png')]
    [MVCProduces('image/png')]
    procedure GetPersonPhoto;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/images/customers/($ID)')]
    procedure GetPersonPhotoAsStream;

    [MVCHTTPMethod([httpPOST])]
    [MVCConsumes(TMVCMediaType.MULTIPART_FORM_DATA)]
    [MVCPath('/files')]
    procedure UploadBinaryData;

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people/mvcfrombody')]
    procedure CreatePerson(
      const [MVCFromBody] Person: TPerson
      );

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people/mvcfromquerystring')]
    procedure CreatePersonEx(
      const [MVCFromBody] Person: TPerson;
      const [MVCFromQueryString('par1')] Par1: Boolean
      );

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/people/mvcfromheader')]
    procedure CreatePersonEx2(
      const [MVCFromBody] Person: TPerson;
      const [MVCFromQueryString('par1')] Par1: Boolean;
      const [MVCFromHeader('X-MY-HEADER')] XMyHeader: TDateTime
      );

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/files/($filename)')]
    procedure GetBinaryData(const filename: string);

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/exception')]
    procedure RaiseException;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/exception/ashtml')]
    procedure RaiseExceptionHTML;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customserializationtype/root')]
    procedure GetCustomSerializationTypeROOT;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/customserializationtype/attribute')]
    procedure GetCustomSerializationTypeATTRIBUTE;


    [MVCHTTPMethod([httpGET])]
    [MVCPath('/simplearray')]
    procedure GetSimpleArrays;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/simplelists')]
    procedure GetSimpleLists;

    [MVCHTTPMethod([httpGET])]
    [MVCPath('/objectwithjson')]
    procedure GetObjectWithJSONProperty;

    // Nullables
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/nullables/single')]
    procedure GetOneNullableObject;

    // Nullables
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/nullables/many')]
    procedure GetManyNullableObjects;

    // Arrays
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/arrays')]
    procedure GetClassWithArrays;

    //Records
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/record')]
    procedure GetSingleRecord;


    // Enums
    [MVCHTTPMethod([httpGET])]
    [MVCPath('/enums')]
    procedure GetClassWithEnums;

    [MVCHTTPMethod([httpPOST])]
    [MVCPath('/enums')]
    procedure EchoClassWithEnums;
  end;

implementation

uses
  Generics.Collections,
  MVCFramework.DataSet.Utils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Serializer.Defaults,
  MVCFramework.Logger,
  MyDataModuleU,
  System.IOUtils,
  WebModuleU,
  CustomTypesU,
  InMemoryDataU,
  JsonDataObjects,
  MVCFramework.Serializer.JsonDataObjects,
  Web.HTTPApp,
  Graphics,
  System.Types, FireDAC.Comp.Client;

procedure DrawLogo(const Logo: TBitmap);
var
  lRect: TRect;
  lText: string;
begin
  lRect := Rect(0, 0, 300, 200);
  lText := 'DMVCFramework';
  Logo.SetSize(lRect.Width, lRect.Height);
  Logo.Canvas.Brush.Color := clRed;
  Logo.Canvas.FillRect(lRect);
  Logo.Canvas.Font.Size := 24;
  Logo.Canvas.Font.Name := 'Tahoma';
  Logo.Canvas.Font.Color := clWhite;
  lRect.Inflate(-20, -60);
  Logo.Canvas.TextRect(lRect, lText, [TTextFormats.tfCenter]);
end;
{ TRoutingSampleController }

procedure TRenderSampleController.GetUnicodeText_AsHTML;
var
  s: string;
begin
  s := '<html><body>';
  s := s + '什么是Unicode(统一码)? in Simplified Chinese <br>';
  s := s + 'Što je Unicode? in Croatian <br>';
  s := s + 'Co je Unicode? in Czech';
  s := s + '</body></html>';
  Render(s);
end;

procedure TRenderSampleController.OnBeforeAction(AContext: TWebContext; const AActionName: string;
  var AHandled: Boolean);
begin
  inherited;

end;

procedure TRenderSampleController.RaiseException;
var
  a: Integer;
begin
  a := 0;
  Render(IntToStr(10 div a));
end;

procedure TRenderSampleController.RaiseExceptionHTML;
var
  a: Integer;
begin
  ContentType := TMVCMediaType.TEXT_HTML;
  a := 0;
  Render(IntToStr(10 div a));
end;

procedure TRenderSampleController.UploadBinaryData;
var
  lFile: TAbstractWebRequestFile;
  lFileExt: string;
  lOutputFileName: string;
  lOutputFullPath: string;
  lOutFile: TFileStream;
  lOutputFolder: string;
begin
  if Context.Request.Files.Count <> 1 then
  begin
    raise EMVCException.Create(HTTP_STATUS.BadRequest, 'Expected exactly 1 file');
  end;
  lFile := Context.Request.Files[0];

  LogI(Format('Upload: [FieldName: %s] [FileName: %s] [ContentType: %s] [Size: %d bytes]',
    [lFile.FieldName, lFile.filename, lFile.ContentType, lFile.Stream.Size]));

  { Be sure that our data directory always exists. We could also do it in the server startup. }
  lOutputFolder := TPath.Combine(AppPath, 'uploadedfiles');
  if not TDirectory.Exists(lOutputFolder) then
  begin
    TDirectory.CreateDirectory(lOutputFolder);
  end;

  lFileExt := TPath.GetExtension(lFile.filename);
  {
    Here we could check for allowed extensions or check the file contents looking for
    accepted file headers (e.g. Zip, PNG, BMP, TIFF etc).
    In this case we just use the extension of the filename sent by the client.
  }

  { Find a valid random filename to store the stream on disk. }
  repeat
    lOutputFileName := TPath.ChangeExtension(TPath.GetRandomFileName, lFileExt);
    lOutputFullPath := TPath.Combine(lOutputFolder, lOutputFileName);
  until not TFile.Exists(lOutputFullPath);

  lOutFile := TFileStream.Create(lOutputFullPath, fmCreate);
  try
    lOutFile.CopyFrom(lFile.Stream, 0);
  finally
    lOutFile.Free;
  end;

  { Inform the client about the name assigned to the file
    on disk and how to retrieve it. }
  Context.Response.ContentType := TMVCMediaType.APPLICATION_JSON;
  Context.Response.StatusCode := HTTP_STATUS.OK;
  Render(StrDict(['filename', 'ref'], [lOutputFileName, '/files/' + lOutputFileName]));
end;

procedure TRenderSampleController.CreatePerson(const Person: TPerson);
begin
  Render(Person, False);
end;

procedure TRenderSampleController.CreatePersonEx(const Person: TPerson; const Par1: Boolean);
begin
  Person.Married := Par1;
  Render(Person, False);
end;

procedure TRenderSampleController.CreatePersonEx2(const Person: TPerson;
  const Par1: Boolean; const XMyHeader: TDateTime);
begin
  Person.Married := Par1;
  Person.DOB := XMyHeader;
  Render(Person, False);
end;

procedure TRenderSampleController.EchoClassWithEnums;
var
  lObj: TClassWithEnums;
begin
  lObj := Context.Request.BodyAs<TClassWithEnums>;
  lObj.RGBSet := [ctBlue, ctGreen, ctRed];
  lObj.EnumWithName := ctBlue;
  Render(lObj, True);
end;

procedure TRenderSampleController.GetBinaryData(const filename: string);
var
  lFilesFolder: string;
  lFullFilePath: string;
begin
  lFilesFolder := TPath.Combine(AppPath, 'uploadedfiles');
  lFullFilePath := TPath.Combine(lFilesFolder, filename);
  if not TFile.Exists(lFullFilePath) then
  begin
    raise EMVCException.Create('File not found');
  end;
  Context.Response.ContentType := TMVCMediaType.APPLICATION_OCTET_STREAM;
  Context.Response.StatusCode := HTTP_STATUS.OK;
  Context.Response.CustomHeaders.Values['Content-Disposition'] := 'attachment; filename=' +
    filename + ';';
  Render(TFileStream.Create(lFullFilePath, fmOpenRead or fmShareDenyNone));
end;

procedure TRenderSampleController.GetClassWithArrays;
var
  lClass: TClassWithArrays;
begin
  lClass := TClassWithArrays.Create;
  lClass.ArrayOfString := ['one', 'two', 'three'];
  lClass.ArrayOfInt := [1, 2, 3];
  lClass.ArrayOfInt64 := [high(Int64), high(Int64) - 1, high(Int64) - 2];
  lClass.ArrayOfDouble := [1234.5678, 2345.6789, 3456.78901];
  Render(lClass);
end;

procedure TRenderSampleController.GetClassWithEnums;
var
  lObj: TClassWithEnums;
begin
  lObj := TClassWithEnums.Create;
  lObj.RGBSet := [ctGreen, ctBlue];
  lObj.EnumDefaultSerialization := ctGreen;
  lObj.EnumWithName := ctGreen;
  lObj.EnumWithOrdValue := ctGreen;
  lObj.EnumWithMappedValues := ctGreen;
  Render(lObj);
end;

procedure TRenderSampleController.GetCustomerByID_AsTObject(const ID: Integer);
var
  Cust: TCustomer;
begin
  if ID = 7 then // just a sample
    Render(HTTP_STATUS.NotFound, 'Customer Not Found')
  else
  begin
    Cust := TCustomer.Create;
    Cust.Name := 'Daniele Teti Inc.';
    Cust.ContactFirst := 'Daniele';
    Cust.ContactLast := 'Teti';
    Cust.AddressLine1 := 'Rome Street 12';
    Cust.AddressLine2 := '00100';
    Cust.City := 'ROME';
    DrawLogo(Cust.Logo);
    Render(Cust);
  end;
end;

procedure TRenderSampleController.GetCustomersAndCountry_AsDataSet;
var
  lDM: TMyDataModule;
  lJObj: TJSONObject;
  lSer: TMVCJsonDataObjectsSerializer;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    lDM.qryCountry.Open;

    lJObj := TJSONObject.Create;
    try
      // We need a non standard representation, let's create a specific serializer.
      lSer := TMVCJsonDataObjectsSerializer.Create;
      try
        lSer.DataSetToJsonArray(lDM.qryCustomers, lJObj.a['customers'],
          TMVCNameCase.ncLowerCase, []);
        lSer.DataSetToJsonArray(lDM.qryCountry, lJObj.a['countries'], TMVCNameCase.ncLowerCase, []);
        lJObj.O['info'].s['timestamp'] := DateTimeToISOTimeStamp(Now);
      finally
        lSer.Free;
      end;
      Render(lJObj);
    except // avoid memory leaks in case of exceptions
      lJObj.Free;
      raise;
    end;
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomers_AsDataSet;
var
  lDM: TMyDataModule;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    Render(lDM.qryCustomers, False);
  finally
    lDM.Free;
  end;
end;

function TRenderSampleController.GetCustomers_AsDataSet_AsFunction: TDataSet;
var
  lDM: TMyDataModule;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    Result := TFDMemTable.Create(nil);
    TFDMemTable(Result).CloneCursor(lDM.qryCustomers, True);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomersAsDataSetWithRefLinks;
var
  lDM: TMyDataModule;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    Render(lDM.qryCustomers, False,
      procedure(const DS: TDataset; const Links: IMVCLinks)
      begin
        Links
          .AddRefLink
            .Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString)
            .Add(HATEOAS.REL, 'self')
            .Add(HATEOAS._TYPE, 'application/json');
        Links
          .AddRefLink
            .Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString + '/orders')
            .Add(HATEOAS.REL, 'orders')
            .Add(HATEOAS._TYPE, 'application/json');
      end);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomersWithCallback;
var
  lDM: TMyDataModule;
  lSer: TMVCJsonDataObjectsSerializer;
  lJArray: TJsonArray;
  lJObj: TJSONObject;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open('SELECT * FROM CUSTOMER ORDER BY CUST_NO');
    lSer := TMVCJsonDataObjectsSerializer.Create;
    try
      lJObj := TJSONObject.Create;
      lJArray := lJObj.a['customers'];
      lSer.DataSetToJsonArray(lDM.qryCustomers, lJArray, TMVCNameCase.ncLowerCase, [],
        procedure(const aField: TField; const aJsonObject: TJSONObject; var Handled: Boolean)
        var
          lTmp: string;
          lPieces: TArray<string>;
        begin
          // ignore one attribute
          if SameText(aField.FieldName, 'contact_last') then
          begin
            Handled := True;
          end;

          // change the attribute value
          if SameText(aField.FieldName, 'on_hold') then
          begin
            aJsonObject.B['on_hold'] := not aField.IsNull;
            Handled := True;
          end;

          // change the attribute type!
          if SameText(aField.FieldName, 'phone_no') then
          begin
            lTmp := aField.AsString.Replace('(', '').Replace(')', '').Replace('-', ' ')
              .Replace('  ', ' ', [rfReplaceAll]).Trim;
            if lTmp.IsEmpty then
            begin
              Handled := True;
              Exit;
            end;
            lPieces := lTmp.Split([' ']);
            aJsonObject.O['phone'].s['intl_prefix'] := lPieces[0];
            Delete(lPieces, 0, 1);
            aJsonObject.O['phone'].s['number'] := string.Join('-', lPieces);
            Handled := True;
          end;

          // add an attribute
          if SameText(aField.FieldName, 'country') then
          begin
            aJsonObject.B['is_usa_customer'] := SameText(aField.AsString, 'usa');
          end;

          // merge 2 or more attributes
          if SameText(aField.FieldName, 'contact_first') then
          begin
            aJsonObject.s['contact_full_name'] := aField.DataSet.FieldByName('contact_first')
              .AsString + ', ' + aField.DataSet.FieldByName('contact_last').AsString;
            Handled := True;
          end;
        end);
    finally
      lSer.Free;
    end;
    Render(lJObj);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomersWithObjectDictionary;
var
  lDM: TMyDataModule;
  lDict: IMVCObjectDictionary;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    lDict := ObjectDict(False { data are not freed after ObjectDict if freed } )
      .Add('customers', lDM.qryCustomers,
        procedure(const DS: TDataset; const Links: IMVCLinks)
        begin
          Links.AddRefLink.Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString)
            .Add(HATEOAS.REL, 'self').Add(HATEOAS._TYPE, 'application/json');
          Links.AddRefLink.Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString +
            '/orders').Add(HATEOAS.REL, 'orders').Add(HATEOAS._TYPE, 'application/json');
        end)
      .Add('singleCustomer', lDM.qryCustomers,
        procedure(const DS: TDataset; const Links: IMVCLinks)
        begin
          Links.AddRefLink.Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString)
            .Add(HATEOAS.REL, 'self').Add(HATEOAS._TYPE, 'application/json');
          Links.AddRefLink.Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString +
            '/orders').Add(HATEOAS.REL, 'orders').Add(HATEOAS._TYPE, 'application/json');
        end, dstSingleRecord, ncPascalCase);
    Render(lDict);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomer_AsDataSetRecord(const ID: Integer);
var
  lDM: TMyDataModule;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open('SELECT * FROM CUSTOMER WHERE CUST_NO = ?', [ID]);
    Render(lDM.qryCustomers, False, [], dstSingleRecord,
      procedure(const DS: TDataset; const Links: IMVCLinks)
      begin
        Links.AddRefLink.Add(HATEOAS.HREF, '/customers').Add(HATEOAS.REL, 'customers')
          .Add(HATEOAS._TYPE, TMVCMediaType.APPLICATION_JSON);
        Links.AddRefLink.Add(HATEOAS.HREF, '/customers/' + DS.FieldByName('cust_no').AsString)
          .Add(HATEOAS.REL, 'self').Add(HATEOAS._TYPE, TMVCMediaType.APPLICATION_JSON);
      end);
  finally
    lDM.Free;
  end;
end;

procedure TRenderSampleController.GetCustomSerializationTypeATTRIBUTE;
begin
  // TSysUser2 contains a type with a custom serializer
  Render(TSysUser2.Create('daniele', ['poweruser', 'role1', 'role2']), True);
end;

procedure TRenderSampleController.GetCustomSerializationTypeROOT;
begin
  // TSysUser is a type with a custom serializer
  Render(TSysUser.Create('daniele', ['poweruser', 'role1', 'role2']), True);
end;

procedure TRenderSampleController.GetDataSetWithMetadata;
var
  lDM: TMyDataModule;
  lDict: IMVCObjectDictionary;
begin
  lDM := TMyDataModule.Create(nil);
  try
    lDM.qryCustomers.Open;
    lDict := ObjectDict(False)

      .Add('ncUpperCaseList', lDM.qryCustomers, nil, dstAllRecords, ncUpperCase)
      .Add('ncLowerCaseList', lDM.qryCustomers, nil, dstAllRecords, ncLowerCase)
      .Add('ncCamelCaseList', lDM.qryCustomers, nil, dstAllRecords, ncCamelCase)
      .Add('ncPascalCaseList', lDM.qryCustomers, nil, dstAllRecords, ncPascalCase)

      .Add('ncUpperCaseSingle', lDM.qryCustomers, nil, dstSingleRecord, ncUpperCase)
      .Add('ncLowerCaseSingle', lDM.qryCustomers, nil, dstSingleRecord, ncLowerCase)
      .Add('ncCamelCaseSingle', lDM.qryCustomers, nil, dstSingleRecord, ncCamelCase)
      .Add('ncPascalCaseSingle', lDM.qryCustomers, nil, dstSingleRecord, ncPascalCase)

      .Add('meta', StrDict(['page', 'count'], ['1', lDM.qryCustomers.RecordCount.ToString]));

    Render(lDict);
  finally
    lDM.Free;
  end;

end;

procedure TRenderSampleController.GetDateAndTimeShowcase;
begin
  Render(GetDataSet);
end;

procedure TRenderSampleController.GetInterfacedPeople;
begin
  Render(ToMVCList(GetInterfacedPeopleList, True));
end;

procedure TRenderSampleController.GetLotOfPeople;
begin
  { classic approach }
  // Render<TPerson>(GetPeopleList, False);
  { new approach with ObjectDict }
  Render(ObjectDict(False).Add('data', GetPeopleList));
end;

procedure TRenderSampleController.GetManyNullableObjects;
var
  lList: TObjectList<TNullablesTest>;
  I: Integer;
begin
  lList := TObjectList<TNullablesTest>.Create(True);
  for I := 1 to 10 do
  begin
    lList.Add(TNullablesTest.Create);
    lList.Last.LoadSomeData;
  end;
  Render<TNullablesTest>(lList);
end;

procedure TRenderSampleController.GetNil;
begin
  Render(ObjectDict().Add('data', nil));
end;

procedure TRenderSampleController.GetObjectWithJSONProperty;
var
  lObj: TObjectWithJSONObject;
begin
  lObj := TObjectWithJSONObject.Create;
  lObj.StringProp := 'Daniele Teti';
  lObj.JSONObject.s['stringprop'] := 'String Prop';
  lObj.JSONObject.O['innerobj'].s['innerstringprop'] := 'Inner String Prop';
  Render(lObj);
end;

procedure TRenderSampleController.GetOneNullableObject;
begin
  Render(TNullablesTest.Create);
end;

function TRenderSampleController.GetPerson_AsFunction: TPerson;
begin
  Result := TPerson.GetNew('Daniele','Teti', EncodeDate(1979,11,4), True);
end;

procedure TRenderSampleController.GetPerson_AsHTML;
begin
  ResponseStream.Append('<html><body><ul>').Append('<li>FirstName: Daniele</li>')
    .Append('<li>LastName: Teti').AppendFormat('<li>DOB: %s</li>',
    [DateToISODate(EncodeDate(1975, 5, 2))]).Append('<li>Married: yes</li>')
    .Append('</ul></body></html>');
  RenderResponseStream;
end;

procedure TRenderSampleController.GetPerson_AsHTMLView;
var
  Cust: TCustomer;
begin
  Cust := TCustomer.Create;
  Cust.Name := 'Daniele Teti Inc.';
  Cust.ContactFirst := 'Daniele';
  Cust.ContactLast := 'Teti';
  Cust.AddressLine1 := 'Rome Street 12';
  Cust.AddressLine2 := '00100';
  Cust.City := 'ROME';
  ViewData['customer'] := Cust;
  LoadView(['header', 'customer', 'footer']);
  RenderResponseStream;
  { If you need more flexibility, you can use GetRenderedView to compose your
    output using small views.
    Here's an example:

    ContentType := TMVCMediaType.TEXT_HTML;
    Render(GetRenderedView(['header', 'customer','footer']));
  }
end;

procedure TRenderSampleController.GetPerson_AsText(const ID: Integer);
begin
  ResponseStream
    .AppendLine('ID        :  ' + ID.ToString)
    .AppendLine('FirstName : Daniele')
    .AppendLine('LastName  : Teti')
    .AppendLine('DOB       : ' + DateToStr(EncodeDate(1979, 5, 2)))
    .AppendLine('Married   : yes');
  RenderResponseStream;
end;

function TRenderSampleController.GetPerson_AsText_AsFunction(
  const ID: Integer): String;
begin
  var lSBldr := TStringBuilder.Create;
  try
    lSBldr
      .AppendLine('ID        :  ' + ID.ToString)
      .AppendLine('FirstName : Daniele')
      .AppendLine('LastName  : Teti')
      .AppendLine('DOB       : ' + DateToStr(EncodeDate(1979, 5, 2)))
      .AppendLine('Married   : yes');
    Result := lSBldr.ToString;
  finally
    lSBldr.Free;
  end;
end;

procedure TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList;
var
  List: TObjectList<TPerson>;
  p: TProgrammer;
  ph: TPhilosopher;
begin
  List := TObjectList<TPerson>.Create(True);
  p := TProgrammer.Create;
  p.Married := True;
  p.FirstName := 'Peter';
  p.LastName := 'Parker';
  p.Skills := 'Delphi, JavaScript, Python, C++';
  List.Add(p);
  ph := TPhilosopher.Create;
  p.Married := False;
  ph.FirstName := 'Bruce';
  ph.LastName := 'Banner';
  ph.Mentors := 'Abbagnano, Algarotti, Cavalieri, Pareyson';
  List.Add(ph);
  p := TProgrammer.Create;
  p.Married := False;
  p.FirstName := 'Sue';
  p.LastName := 'Storm';
  p.Skills := 'Delphi, JavaScript';
  List.Add(p);
  Render<TPerson>(List);
end;

function TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList_AsFunction: TObjectList<TPerson>;
var
  List: TObjectList<TPerson>;
  p: TProgrammer;
  ph: TPhilosopher;
begin
  List := TObjectList<TPerson>.Create(True);
  try
    p := TProgrammer.Create;
    p.Married := True;
    p.FirstName := 'Peter';
    p.LastName := 'Parker';
    p.Skills := 'Delphi, JavaScript, Python, C++';
    List.Add(p);
    ph := TPhilosopher.Create;
    p.Married := False;
    ph.FirstName := 'Bruce';
    ph.LastName := 'Banner';
    ph.Mentors := 'Abbagnano, Algarotti, Cavalieri, Pareyson';
    List.Add(ph);
    p := TProgrammer.Create;
    p.Married := False;
    p.FirstName := 'Sue';
    p.LastName := 'Storm';
    p.Skills := 'Delphi, JavaScript';
    List.Add(p);
  except
    List.Free;
    raise;
  end;
  Result := List;
end;

function TRenderSampleController.GetProgrammersAndPhilosophersAsObjectList_withmvcresponse_AsFunction: IMVCResponse;
begin
  Result := MVCResponseBuilder
    .Body(GetPeople_AsObjectList_AsFunction)
    .Build;
end;

procedure TRenderSampleController.GetSimpleArrays;
begin
  Render(TArrayTest.Create);
end;

procedure TRenderSampleController.GetSimpleLists;
begin
  Render(TSimpleListTest.Create);
end;

procedure TRenderSampleController.GetSingleRecord;
var
  lSR: TSimpleRecord;
begin
  lSR := TSimpleRecord.Create;
  Render<TSimpleRecord>(200, lSR);
end;

procedure TRenderSampleController.GetPeopleAsCSV;
begin
  ResponseStream.AppendLine('first_name;last_name;age');
  ResponseStream.AppendLine('Daniele;Teti;38');
  ResponseStream.AppendLine('Peter;Parker;22');
  ResponseStream.AppendLine('Bruce;Banner;60');
  ContentType := TMVCMediaType.TEXT_CSV;
  RenderResponseStream;
end;

function TRenderSampleController.GetPeopleAsCSV_AsFunction: String;
var
  lSS: TStringBuilder;
begin
  ContentType := TMVCMediaType.TEXT_CSV;
  lSS := TStringBuilder.Create('');
  try
    lSS.AppendLine('first_name;last_name;age');
    lSS.AppendLine('Daniele;Teti;38');
    lSS.AppendLine('Peter;Parker;22');
    lSS.AppendLine('Bruce;Banner;60');
    Result := lSS.ToString;
  finally
    lSS.Free;
  end;
end;

procedure TRenderSampleController.GetPeopleWithTiming;
var
  p: TPerson;
  People: TPeopleWithMetadata;
begin
  People := TPeopleWithMetadata.Create;
  try
    People.Metadata.StartProcessing := Now;

{$REGION 'Fake data'}
    Sleep(1000); // processing...

    p := TPerson.Create;
    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.DOB := EncodeDate(1979, 11, 4);
    p.Married := True;
    People.Items.Add(p);

    p := TPerson.Create;
    p.FirstName := 'John';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1879, 10, 2);
    p.Married := False;
    People.Items.Add(p);

    p := TPerson.Create;
    p.FirstName := 'Jane';
    p.LastName := 'Doe';
    p.DOB := EncodeDate(1883, 1, 5);
    p.Married := True;
    People.Items.Add(p);

{$ENDREGION}
    People.Metadata.CustomData := Format('There are %d people in the list', [People.Items.Count]);
    People.Metadata.StopProcessing := Now;
    Render(People, False);
  finally
    People.Free;
  end;
end;

procedure TRenderSampleController.GetPeople_AsObjectList;
var
  People: TObjectList<TPerson>;
begin
  People := TObjectList<TPerson>.Create(True);
  People.Add(TPerson.GetNew('Daniele','Teti', EncodeDate(1979, 11, 4), True));
  People.Add(TPerson.GetNew('John','Doe', EncodeDate(1879, 10, 2), False));
  People.Add(TPerson.GetNew('Jane','Doe', EncodeDate(1883, 1, 5), True));

  { classic approach }
  //Render<TPerson>(HTTP_STATUS.OK, People, True);
  { new approach with ObjectDict }
  Render(HTTP_STATUS.OK, ObjectDict().Add('data', People));
end;

function TRenderSampleController.GetPeople_AsObjectList_AsFunction: TEnumerable<TPerson>;
begin
  Result := TObjectList<TPerson>.Create(True);
  TObjectList<TPerson>(Result).Add(TPerson.GetNew('Daniele','Teti', EncodeDate(1979, 11, 4), True));
  TObjectList<TPerson>(Result).Add(TPerson.GetNew('John','Doe', EncodeDate(1879, 10, 2), False));
  TObjectList<TPerson>(Result).Add(TPerson.GetNew('Jane','Doe', EncodeDate(1883, 1, 5), True));
end;

procedure TRenderSampleController.GetPeople_AsObjectList_HATEOAS;
var
  p: TPerson;
  People: TObjectList<TPerson>;
begin
  People := TObjectList<TPerson>.Create(True);

{$REGION 'Fake data'}
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := 'Teti';
  p.DOB := EncodeDate(1979, 11, 4);
  p.Married := True;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'John';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1879, 10, 2);
  p.Married := False;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'Jane';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1883, 1, 5);
  p.Married := True;
  People.Add(p);

{$ENDREGION}
  { classic approach }
  {
    Render<TPerson>(People, True,
    procedure(const APerson: TPerson; const Links: IMVCLinks)
    begin
    Links
    .AddRefLink
    .Add(HATEOAS.HREF, '/people/' + APerson.ID.ToString)
    .Add(HATEOAS.REL, 'self')
    .Add(HATEOAS._TYPE, 'application/json')
    .Add('title', 'Details for ' + APerson.FullName);
    Links
    .AddRefLink
    .Add(HATEOAS.HREF, '/people')
    .Add(HATEOAS.REL, 'people')
    .Add(HATEOAS._TYPE, 'application/json');
    end);
  }
  { new approach with ObjectDict }
  Render(ObjectDict().Add('data', People,
    procedure(const APerson: TObject; const Links: IMVCLinks)
    begin
      Links.AddRefLink.Add(HATEOAS.HREF, '/people/' + TPerson(APerson).ID.ToString).Add(HATEOAS.REL,
        'self').Add(HATEOAS._TYPE, 'application/json').Add('title',
        'Details for ' + TPerson(APerson).FullName);
      Links.AddRefLink.Add(HATEOAS.HREF, '/people').Add(HATEOAS.REL, 'people').Add(HATEOAS._TYPE,
        'application/json');
    end));
end;

procedure TRenderSampleController.GetPeople_AsObjectList_With_Alias;
var
  p: TPerson;
  People: TPeople;
begin
  People := TPeople.Create(True);
{$REGION 'Fake data'}
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := 'Teti';
  p.DOB := EncodeDate(1979, 11, 4);
  p.Married := True;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'John';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1879, 10, 2);
  p.Married := False;
  People.Add(p);

  p := TPerson.Create;
  p.FirstName := 'Jane';
  p.LastName := 'Doe';
  p.DOB := EncodeDate(1883, 1, 5);
  p.Married := True;
  People.Add(p);

  People.Add(nil);

{$ENDREGION}
  { classic approach }
  // Render<TPerson>(People, True);
  // Render(People, True);
  // Render<TPerson>(HTTP_STATUS.OK, People, True);
  { new approach with ObjectDict }
  Render(HTTP_STATUS.OK, ObjectDict().Add('data', People));
end;

procedure TRenderSampleController.GetPersonById(const ID: Integer);
var
  lPerson: TPerson;
begin
  lPerson := TPerson.Create;
  try
    lPerson.ID := ID;
    lPerson.FirstName := 'Daniele';
    lPerson.LastName := 'Teti';
    lPerson.DOB := EncodeDate(1979, 11, 4);
    lPerson.Married := True;
    Render(lPerson, False,
      procedure(const AObject: TObject; const Links: IMVCLinks)
      begin
        Links.AddRefLink.Add(HATEOAS.HREF, '/people/' + TPerson(AObject).ID.ToString)
          .Add(HATEOAS.REL, 'self').Add(HATEOAS._TYPE, TMVCMediaType.APPLICATION_JSON);
        Links.AddRefLink.Add(HATEOAS.HREF, '/people').Add(HATEOAS.REL, 'people').Add(HATEOAS._TYPE,
          TMVCMediaType.APPLICATION_JSON);
      end);
  finally
    lPerson.Free;
  end;
end;

procedure TRenderSampleController.GetPersonJSON;
var
  lJSONPerson: TJSONObject;
begin
  lJSONPerson := TJSONObject.Create;
  lJSONPerson.s['FirstName'] := 'Daniele';
  lJSONPerson.s['LastName'] := 'Teti';
  lJSONPerson.s['DOB'] := DateToISODate(EncodeDate(1975, 5, 2));
  lJSONPerson.B['Married'] := True;
  Render(lJSONPerson);
end;

procedure TRenderSampleController.GetPersonPhoto;
begin
  SendFile('..\..\_\customer.png');
end;

procedure TRenderSampleController.GetPersonPhotoAsStream;
var
  LPhoto: TFileStream;
begin
  LPhoto := TFileStream.Create('..\..\_\customer.png', fmOpenRead or fmShareDenyWrite);
  ContentType := 'image/png'; // you can also use MVCProduces attribute

  // LPhoto is a plain TStream descendant, so it can be rendered as usual
  Render(LPhoto, True);
end;

end.
