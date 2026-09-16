unit ActiveRecordControllerTestU;

// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
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
// *************************************************************************** }

interface

uses
  DUnitX.TestFramework,
  FireDAC.Comp.Client, FireDAC.ConsoleUI.Wait, FireDAC.VCLUI.Wait,
  PGUtilsU, LiveServerTestU, MVCFramework.Server, MVCFramework.RESTClient.Intf;


type
  [TestFixture]
  TTestActiveRecordController = class(TObject)
  private
    fListener: IMVCListener;
    fClient: IMVCRESTClient;
    procedure LoadData;
  public
    class procedure CreatePrivateFirebirdSQLConnDef(const ConDefName: String; AIsPooled: boolean);
    [SetupFixture]
    procedure Setup;
    [TeardownFixture]
    procedure TearDown;
    [Test]
    procedure TestGetAll;
    [Test]
    procedure TestCRUD;
    [Test]
    procedure TestDelete;
    // Composite (two-column) primary key over HTTP: POST creates, a JSON-array
    // URL segment ([k1,k2]) addresses the row for GET/PUT/DELETE.
    [Test]
    procedure TestCompositePKCRUD;
    // Composite key with a string column whose value contains ';' and ',':
    // proves the JSON-array segment removes any delimiter-collision limit.
    [Test]
    procedure TestCompositePKCRUD_StringKeyWithDelimiters;
  end;

implementation

uses
  System.Classes, System.IOUtils, BOs, MVCFramework.ActiveRecord,
  System.SysUtils, System.Threading, System.Generics.Collections, Data.DB,
  FireDAC.Stan.Intf, ShellAPI, Winapi.Windows, FDConnectionConfigU,
  MVCFramework, MVCFramework.Serializer.JsonDataObjects,
  MVCFramework.Server.Impl,
  MVCFramework.Middleware.Session, MVCFramework.Middleware.ActiveRecord,
  MVCFramework.ActiveRecordController,
  MVCFramework.RESTClient, JsonDataObjects, System.StrUtils,
  MVCFramework.Commons;

const
  AR_CONTROLLER_CON_DEF_NAME = 'AR_CONTROLLER_CON_DEF_NAME';


{ TTestActiveRecordController }
class procedure TTestActiveRecordController.CreatePrivateFirebirdSQLConnDef(const ConDefName: String; AIsPooled: boolean);
var
  LParams: TStringList;
  lDriver: IFDStanDefinition;
  GDBFileName: string;
  GDBTemplateFileName: string;
begin
  if not Assigned(FDManager.DriverDefs.FindDefinition('FBEMBEDDED')) then
  begin
    lDriver := FDManager.DriverDefs.Add;
    lDriver.Name := 'FBEMBEDDED';
    lDriver.AsString['BaseDriverID'] := 'FB';
    lDriver.AsString['DriverID'] := 'FBEMBEDDED';
    lDriver.AsString['VendorLib'] := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebird\fbclient.dll');
    lDriver.Apply;
  end;

  LParams := TStringList.Create;
  try
    GDBFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebirdtest2.fdb');
    GDBTemplateFileName := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'firebirdtest_template.fdb');
    TFile.Copy(GDBTemplateFileName, GDBFileName, True);
    LParams.Add('Database=' + GDBFileName);
    LParams.Add('user_name=sysdba');
    LParams.Add('password=masterkey');
    if AIsPooled then
    begin
      LParams.Add('Pooled=True');
      LParams.Add('POOL_MaximumItems=100');
    end
    else
    begin
      LParams.Add('Pooled=False');
    end;
    FDManager.AddConnectionDef(ConDefName, 'FBEMBEDDED', LParams);
  finally
    LParams.Free;
  end;
end;

procedure TTestActiveRecordController.LoadData;
var
  lProc: TProc;
const
  Cities: array [0 .. 4] of string = ('Rome', 'New York', 'London', 'Melbourne', 'Berlin');
  CompanySuffix: array [0 .. 5] of string = ('Corp.', 'Inc.', 'Ltd.', 'Srl', 'SPA', 'doo');
  Stuff: array [0 .. 4] of string = ('Burger', 'GAS', 'Motors', 'House', 'Boats');
begin
  TMVCActiveRecord.DeleteRQL(TCustomer, 'in(City,["Rome","New York","London","Melbourne","Berlin"])');
  lProc := procedure
    var
      lCustomer: TCustomer;
      I: Integer;
    begin
      for I := 1 to 30 do
      begin
        lCustomer := TCustomer.Create;
        try
          lCustomer.Code := Format('%5.5d', [TThread.CurrentThread.ThreadID, I]);
          lCustomer.City := Cities[I mod Length(Cities)];
          lCustomer.CompanyName := Format('%s %s %s', [lCustomer.City, Stuff[Random(high(Stuff) + 1)],
            CompanySuffix[Random(high(CompanySuffix) + 1)]]);
          lCustomer.Note := Stuff[I mod Length(Stuff)];
          lCustomer.Rating := 1;
          lCustomer.CreationTime := EncodeTime(I mod 23, I, 60 - 1, 0);
          lCustomer.CreationDate := EncodeDate(2020 - I, (I mod 12) + 1, (I mod 27) + 1);
          lCustomer.Insert;
        finally
          lCustomer.Free;
        end;
      end;
    end;
  lProc();
end;

procedure TTestActiveRecordController.Setup;
begin
  CreatePrivateFirebirdSQLConnDef(AR_CONTROLLER_CON_DEF_NAME, True);

  var lConn := TFDConnection.Create(nil);
  try
    lConn.ConnectionDefName := AR_CONTROLLER_CON_DEF_NAME;
    lConn.Open;
    for var lSQL in SQLs_FIREBIRD do
    begin
      lConn.ExecSQL(lSQL);
    end;
    ActiveRecordConnectionsRegistry.AddDefaultConnection(lConn, False);
    LoadData;
    ActiveRecordConnectionsRegistry.RemoveDefaultConnection();
  finally
    lConn.Free;
  end;

  fListener := TMVCListener.Create(
    TMVCListenerProperties
      .New
      .SetName('Listener1')
      .SetPort(5000)
      .SetMaxConnections(512)
      .SetConfigAction(
        procedure(Config: TMVCConfig)
        begin
          Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
          Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
          Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
          Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
          Config[TMVCConfigKey.ViewPath] := 'templates';
          Config[TMVCConfigKey.ExposeServerSignature] := 'true';
        end)
      .SetEngineConfig(
        procedure(AEngine: TMVCEngine)
        begin
          AEngine.AddMiddleware(UseMemorySessionMiddleware(0));
          AEngine.AddMiddleware(TMVCActiveRecordMiddleware.Create(AR_CONTROLLER_CON_DEF_NAME));
          AEngine.AddController(TMVCActiveRecordController, '/api/entities');
        end));
  fListener.Start;

  fClient := TMVCRESTClient.New.BaseURL('http://localhost', 5000);
  fClient.ReadTimeout(60 * 1000 * 30);
end;

procedure TTestActiveRecordController.Teardown;
begin
  fListener.Stop;
  FDManager.CloseConnectionDef(AR_CONTROLLER_CON_DEF_NAME);
  FDManager.DeleteConnectionDef(AR_CONTROLLER_CON_DEF_NAME);
end;

procedure TTestActiveRecordController.TestDelete;
var
  lResp: IMVCRESTResponse;
  lLocation: String;
begin
  var lCust := TCustomer.Create;
  try
    lCust.Code := 'MYCODE';
    lCust.CompanyName := 'bit Time Professionals';
    lCust.City := 'Rome';
    lResp := fClient.Post('/api/entities/customers', lCust, False);
    lLocation := lResp.HeaderValue('X-REF');
  finally
    lCust.Free;
  end;

  lResp := fClient.Delete(lLocation);
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
  lResp := fClient.Get(lLocation);
  Assert.AreEqual(HTTP_STATUS.NotFound, lResp.StatusCode);
end;

procedure TTestActiveRecordController.TestGetAll;
var
  lResp: IMVCRESTResponse;
begin
  lResp := fClient.Get('/api/entities/customers');
  var lJSON := lResp.ToJSONObject;
  try
    Assert.IsTrue(lJSON.Contains('data') and (lJSON.Types['data'] = TJsonDataType.jdtArray), 'incorrect "data" property in JSON');
    var lCustomers := TJSONUtils.JSONArrayToListOf<TCustomer>(lJSON.A['data']);
    try
      Assert.AreEqual<Integer>(20, lCustomers.Count);
    finally
      lCustomers.Free;
    end;
  finally
    lJSON.Free;
  end;
end;

procedure TTestActiveRecordController.TestCRUD;
var
  lResp: IMVCRESTResponse;
  lLocation: String;
  lJSON: TJsonObject;
  lPieces: TArray<String>;
  lIDFromURL: Integer;
  lCust: TCustomer;
begin
  lCust := TCustomer.Create;
  try
    lCust.Code := 'MYCODE';
    lCust.CompanyName := 'The Company';
    lCust.City := 'Rome';
    lCust.CreationTime := Time;
    lCust.CreationDate := Date;
    lResp := fClient.Post('/api/entities/customers', lCust, False);
    lLocation := lResp.HeaderValue('X-REF');
  finally
    lCust.Free;
  end;

  lResp := fClient.Get(lLocation);
  lJSON := lResp.ToJSONObject;
  try
    Assert.IsTrue(lJSON.Contains('data') and (lJSON.Types['data'] = TJsonDataType.jdtObject), 'incorrect "data" property in JSON');
    lCust := TJSONUtils.JSONObjectToObject<TCustomer>(lJSON.O['data']);
    try
      lPieces := lLocation.Split(['/']);
      lIDFromUrl := lPieces[High(lPieces)].ToInteger;
      Assert.AreEqual(lIDFromUrl, lCust.ID.Value);
      Assert.AreEqual('MYCODE', lCust.Code.Value);
      Assert.AreEqual('The Company', lCust.CompanyName.Value);
      Assert.AreEqual('Rome', lCust.City);
      Assert.IsFalse(lCust.Rating.HasValue);
      Assert.IsTrue(lCust.CreationTime.HasValue);
      Assert.IsTrue(lCust.CreationDate.HasValue);
      Assert.IsEmpty(lCust.Note);

      //update
      lCust.Code := nil; //.SetNull;
      lCust.CompanyName.Value := 'bit Time Professionals';
      fClient.Put(lLocation, lCust, False);
    finally
      lCust.Free;
    end;
  finally
    lJSON.Free;
  end;
  
  lResp := fClient.Get(lLocation);
  lJSON := lResp.ToJSONObject;
  try
    lCust := TJSONUtils.JSONObjectToObject<TCustomer>(lJSON.O['data']);
    try
      lPieces := lLocation.Split(['/']);
      lIDFromUrl := lPieces[High(lPieces)].ToInteger;
      Assert.AreEqual(lIDFromUrl, lCust.ID.Value);
      Assert.IsFalse(lCust.Code.HasValue);
      Assert.AreEqual('bit Time Professionals', lCust.CompanyName.Value);
      Assert.AreEqual('Rome', lCust.City);
      Assert.IsFalse(lCust.Rating.HasValue);
      Assert.IsTrue(lCust.CreationTime.HasValue);
      Assert.IsTrue(lCust.CreationDate.HasValue);
      Assert.IsEmpty(lCust.Note);
    finally
      lCust.Free;
    end;
  finally
    lJSON.Free;
  end;
  lResp := fClient.Delete(lLocation);
  Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
end;

procedure TTestActiveRecordController.TestCompositePKCRUD;
var
  lConn: TFDConnection;
  lResp: IMVCRESTResponse;
  lLocation: String;
  lJSON: TJsonObject;
  lRole: TUserRoleCtrl;
begin
  // Self-contained: create the junction table on the controller's connection def,
  // exercise it through HTTP, then drop it. TUserRoleCtrl is registered against the
  // "userroles" URL segment in BOs.pas.
  lConn := TFDConnection.Create(nil);
  try
    lConn.ConnectionDefName := AR_CONTROLLER_CON_DEF_NAME;
    lConn.Open;
    try lConn.ExecSQL('DROP TABLE ar_ctrl_user_roles'); except end;
    lConn.ExecSQL('CREATE TABLE ar_ctrl_user_roles (user_id INTEGER NOT NULL, ' +
      'role_id INTEGER NOT NULL, note VARCHAR(200), PRIMARY KEY(user_id, role_id))');
    try
      // POST create
      lRole := TUserRoleCtrl.Create;
      try
        lRole.UserID := 7;
        lRole.RoleID := 42;
        lRole.Note := 'admin';
        lResp := fClient.Post('/api/entities/userroles', lRole, False);
      finally
        lRole.Free;
      end;
      Assert.AreEqual(HTTP_STATUS.Created, lResp.StatusCode);
      lLocation := lResp.HeaderValue('X-REF');
      Assert.IsTrue(lLocation.EndsWith('/[7,42]'),
        'X-REF must address the composite key as a JSON array: ' + lLocation);

      // GET /.../[7,42]
      lResp := fClient.Get(lLocation);
      Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
      lJSON := lResp.ToJSONObject;
      try
        lRole := TJSONUtils.JSONObjectToObject<TUserRoleCtrl>(lJSON.O['data']);
        try
          Assert.AreEqual(7, lRole.UserID);
          Assert.AreEqual(42, lRole.RoleID);
          Assert.AreEqual('admin', lRole.Note.Value);
        finally
          lRole.Free;
        end;
      finally
        lJSON.Free;
      end;

      // PUT /.../7;42
      lRole := TUserRoleCtrl.Create;
      try
        lRole.UserID := 7;
        lRole.RoleID := 42;
        lRole.Note := 'editor';
        lResp := fClient.Put(lLocation, lRole, False);
      finally
        lRole.Free;
      end;
      Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);

      lResp := fClient.Get(lLocation);
      lJSON := lResp.ToJSONObject;
      try
        lRole := TJSONUtils.JSONObjectToObject<TUserRoleCtrl>(lJSON.O['data']);
        try
          Assert.AreEqual('editor', lRole.Note.Value, 'PUT persisted on the composite-key row');
        finally
          lRole.Free;
        end;
      finally
        lJSON.Free;
      end;

      // DELETE /.../[7,42]
      lResp := fClient.Delete(lLocation);
      Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
      lResp := fClient.Get(lLocation);
      Assert.AreEqual(HTTP_STATUS.NotFound, lResp.StatusCode);
    finally
      try lConn.ExecSQL('DROP TABLE ar_ctrl_user_roles'); except end;
    end;
  finally
    lConn.Free;
  end;
end;

procedure TTestActiveRecordController.TestCompositePKCRUD_StringKeyWithDelimiters;
const
  TRICKY_CODE = 'A;B,C'; // contains BOTH the old delimiters
var
  lConn: TFDConnection;
  lResp: IMVCRESTResponse;
  lLocation: String;
  lJSON: TJsonObject;
  lDoc: TCtrlDoc;
begin
  lConn := TFDConnection.Create(nil);
  try
    lConn.ConnectionDefName := AR_CONTROLLER_CON_DEF_NAME;
    lConn.Open;
    try lConn.ExecSQL('DROP TABLE ar_ctrl_docs'); except end;
    lConn.ExecSQL('CREATE TABLE ar_ctrl_docs (doc_code VARCHAR(50) NOT NULL, ' +
      'line_no INTEGER NOT NULL, note VARCHAR(200), PRIMARY KEY(doc_code, line_no))');
    try
      // POST create, with a string key value that contains ';' and ','
      lDoc := TCtrlDoc.Create;
      try
        lDoc.DocCode := TRICKY_CODE;
        lDoc.LineNo := 1;
        lDoc.Note := 'first';
        lResp := fClient.Post('/api/entities/ctrldocs', lDoc, False);
      finally
        lDoc.Free;
      end;
      Assert.AreEqual(HTTP_STATUS.Created, lResp.StatusCode);
      lLocation := lResp.HeaderValue('X-REF');
      // The key rides in a JSON array, so the ';' and ',' live inside a JSON
      // string and cannot collide with the array structure.
      Assert.IsTrue(lLocation.Contains('"A;B,C"'),
        'X-REF must carry the string key inside a JSON array: ' + lLocation);

      // GET it back by the very same URL the server handed us
      lResp := fClient.Get(lLocation);
      Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
      lJSON := lResp.ToJSONObject;
      try
        lDoc := TJSONUtils.JSONObjectToObject<TCtrlDoc>(lJSON.O['data']);
        try
          Assert.AreEqual(TRICKY_CODE, lDoc.DocCode, 'the delimiter-laden key round-tripped intact');
          Assert.AreEqual(1, lDoc.LineNo);
          Assert.AreEqual('first', lDoc.Note.Value);
        finally
          lDoc.Free;
        end;
      finally
        lJSON.Free;
      end;

      // DELETE by the same URL, then confirm it is gone
      lResp := fClient.Delete(lLocation);
      Assert.AreEqual(HTTP_STATUS.OK, lResp.StatusCode);
      lResp := fClient.Get(lLocation);
      Assert.AreEqual(HTTP_STATUS.NotFound, lResp.StatusCode);
    finally
      try lConn.ExecSQL('DROP TABLE ar_ctrl_docs'); except end;
    end;
  finally
    lConn.Free;
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestActiveRecordController);

finalization

end.
