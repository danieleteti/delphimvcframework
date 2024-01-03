// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit MainClientFormU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  System.Net.HttpClientComponent,
  Vcl.StdCtrls,
  System.Net.URLClient,
  System.Net.HttpClient,
  Data.DB,
  Vcl.Grids,
  Vcl.DBGrids,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  MVCFramework.JSONRPC.Client, Vcl.Mask, WaitingFormU;

type
  TMainForm = class(TForm)
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    FDMemTable1Code: TIntegerField;
    FDMemTable1Name: TStringField;
    GroupBox1: TGroupBox;
    edtValue1: TEdit;
    edtValue2: TEdit;
    btnSubtract: TButton;
    edtResult: TEdit;
    edtReverseString: TEdit;
    btnReverseString: TButton;
    edtReversedString: TEdit;
    GroupBox2: TGroupBox;
    edtUserName: TEdit;
    btnGetUser: TButton;
    lbPerson: TListBox;
    GroupBox4: TGroupBox;
    edtFirstName: TLabeledEdit;
    edtLastName: TLabeledEdit;
    chkMarried: TCheckBox;
    dtDOB: TDateTimePicker;
    btnSave: TButton;
    dtNextMonday: TDateTimePicker;
    btnAddDay: TButton;
    btnInvalid1: TButton;
    btnInvalid2: TButton;
    btnNotification: TButton;
    btnInvalidMethod: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox5: TGroupBox;
    edtSearchText: TEdit;
    btnSearch: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    btnDates: TButton;
    btnFloatsTests: TButton;
    btnWithJSON: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    btnSubtractWithNamedParams: TButton;
    Edit3: TEdit;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    edtFilter: TEdit;
    edtGetCustomers: TButton;
    DBGrid1: TDBGrid;
    btnGetMulti: TButton;
    lbMulti: TListBox;
    btnGenericException: TButton;
    TabSheet5: TTabSheet;
    Label1: TLabel;
    btnException: TButton;
    btnGenericExcWithCustomHandling: TButton;
    btnGenericExcWithCustomHAndling2: TButton;
    btnGenericExcWithoutCustomHandling: TButton;
    TabSheet6: TTabSheet;
    btnSingleRec: TButton;
    lbLogRec: TMemo;
    btnGetArrayOfRecords: TButton;
    btnGetDynArray: TButton;
    btnPassAndGetRecord: TButton;
    btnEchoComplexArray: TButton;
    btnComplex: TButton;
    btnParallel: TButton;
    procedure btnSubtractClick(Sender: TObject);
    procedure btnReverseStringClick(Sender: TObject);
    procedure edtGetCustomersClick(Sender: TObject);
    procedure btnGetUserClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddDayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInvalid1Click(Sender: TObject);
    procedure btnInvalid2Click(Sender: TObject);
    procedure btnNotificationClick(Sender: TObject);
    procedure btnInvalidMethodClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnDatesClick(Sender: TObject);
    procedure btnFloatsTestsClick(Sender: TObject);
    procedure btnWithJSONClick(Sender: TObject);
    procedure btnSubtractWithNamedParamsClick(Sender: TObject);
    procedure btnGetMultiClick(Sender: TObject);
    procedure btnGetListOfDatasetClick(Sender: TObject);
    procedure btnObjDictClick(Sender: TObject);
    procedure btnExceptionClick(Sender: TObject);
    procedure btnGenericExceptionClick(Sender: TObject);
    procedure btnGenericExcWithCustomHandlingClick(Sender: TObject);
    procedure btnGenericExcWithCustomHAndling2Click(Sender: TObject);
    procedure btnGenericExcWithoutCustomHandlingClick(Sender: TObject);
    procedure btnSingleRecClick(Sender: TObject);
    procedure btnGetArrayOfRecordsClick(Sender: TObject);
    procedure btnGetDynArrayClick(Sender: TObject);
    procedure btnPassAndGetRecordClick(Sender: TObject);
    procedure btnEchoComplexArrayClick(Sender: TObject);
    procedure btnComplexClick(Sender: TObject);
    procedure btnParallelClick(Sender: TObject);
  private
    FExecutor: IMVCJSONRPCExecutorAsync;
    fGeneralErrorHandler : TJSONRPCErrorHandlerProc;
    fWaiting: TWaitingForm;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Generics.Collections,
  MVCFramework.JSONRPC,
  MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects,
  System.UITypes,
  MVCFramework.Serializer.Commons,
  MVCFramework.Commons,
  MVCFramework.Logger,
  MVCFramework.Serializer.Defaults,
  MVCFramework.DataSet.Utils,
  SyncObjs,
  BusinessObjectsU,
  System.Math,
  System.Rtti, CommonTypesU, MVCFramework.AsyncTask;

{$R *.dfm}

procedure TMainForm.btnAddDayClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getnextmonday';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(dtNextMonday.Date);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      dtNextMonday.Date := ISODateToDate(Resp.Result.AsString);
    end);
end;

procedure TMainForm.btnComplexClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lComplex: TNestedArraysRec;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'EchoComplexArrayOfRecords2';
  lReq.RequestID := Random(1000);
  lComplex.TestRecProp := TTestRec.Create(10);
  SetLength(lComplex.ArrayProp1, 2);
  SetLength(lComplex.ArrayProp2, 2);
  lComplex.ArrayProp1[0] := TTestRec.Create(10);
  lComplex.ArrayProp1[1] := TTestRec.Create(10);
  lComplex.ArrayProp2[0] := TTestRec.Create(10);
  lComplex.ArrayProp2[1] := TTestRec.Create(10);
  lReq.Params.Add(TValue.From<TNestedArraysRec>(lComplex), pdtRecordOrArrayOfRecord);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      lComplex := TJSONUtils.JSONObjectToRecord<TNestedArraysRec>(Resp);
      lbLogRec.Lines.Clear;
      lbLogRec.Lines.Add(lComplex.ToString);
    end,
    procedure (Exc: Exception)
    begin
      ShowMessage(Exc.ClassName + ': ' + Exc.Message);
    end);
end;

procedure TMainForm.btnDatesClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create(1234, 'playwithdatesandtimes');
  lReq.Params.Add(1234.5678, pdtFloat);
  lReq.Params.Add(Time(), pdtTime);
  lReq.Params.Add(Date(), pdtDate);
  lReq.Params.Add(Now(), pdtDateTime);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      ShowMessage(Resp.Result.AsString);
    end);
end;

procedure TMainForm.btnEchoComplexArrayClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lPeople: TTestRecDynArray;
  I: Integer;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'EchoComplexArrayOfRecords';
  lReq.RequestID := Random(1000);
  SetLength(lPeople, 2);
  lPeople[0] := TTestRec.Create(1);
  lPeople[1] := TTestRec.Create(2);
  lReq.Params.Add(TValue.From<TTestRecDynArray>(lPeople), pdtRecordOrArrayOfRecord);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
  procedure(Resp: IJSONRPCResponse)
  begin
    lPeople := TJSONUtils.JSONArrayToArrayOfRecord<TTestRec>(Resp);
    lbLogRec.Lines.Clear;
    lbLogRec.Lines.Add('--- array of record elements ---');
    I := 1;
    for var lPRec in lPeople do
    begin
      lbLogRec.Lines.Add('ITEM: ' + I.ToString);
      lbLogRec.Lines.Add(lPRec.ToString);
      Inc(I);
    end;
  end);
end;

procedure TMainForm.btnExceptionClick(Sender: TObject);
var
  lReq: IJSONRPCNotification;
begin
  ShowMessage('Now will be raised a custom exception on the server. This exception will be catched by the client');
  lReq := TJSONRPCNotification.Create('RaiseCustomException');
  FExecutor.ExecuteNotificationAsync('/jsonrpc', lReq, fGeneralErrorHandler);
end;

procedure TMainForm.btnFloatsTestsClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lRes: Extended;
begin
  lReq := TJSONRPCRequest.Create(1234, 'floatstest');
  lReq.Params.Add(1234.5678, pdtFloat);
  lReq.Params.Add(2345.6789, pdtFloat);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      lRes := Resp.Result.AsType<Extended>;
      lRes := RoundTo(lRes, -4);
      Assert(SameValue(lRes, 3580.2467), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18, 9));

      lReq := TJSONRPCRequest.Create(1234, 'floatstest');
      lReq.Params.Add(123);
      lReq.Params.Add(234);
      FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
      procedure(Resp: IJSONRPCResponse)
      var
        lRes: Extended;
      begin
        lRes := Resp.Result.AsType<Extended>;
        lRes := RoundTo(lRes, -4);
        Assert(SameValue(lRes, 357), 'Wrong result: ' + FloatToStrF(lRes, ffGeneral, 18, 9));
      end);
    end);

end;

procedure TMainForm.btnGetUserClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lbPerson.Clear;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'getuser';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(edtUserName.Text);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
  procedure(Resp: IJSONRPCResponse)
  var
    lJSON: TJsonObject;
  begin
    // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
    // are serialized as JSON objects
    lJSON := Resp.Result.AsObject as TJsonObject;
    lbPerson.Items.Add('First Name:'.PadRight(15) + lJSON.S['firstname']);
    lbPerson.Items.Add('Last Name:'.PadRight(15) + lJSON.S['lastname']);
    lbPerson.Items.Add('Married:'.PadRight(15) + lJSON.B['married'].ToString(TUseBoolStrs.True));
    lbPerson.Items.Add('DOB:'.PadRight(15) + DateToStr(lJSON.D['dob']));
  end);
end;

procedure TMainForm.btnInvalid1Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create(1234);
  lReq.Method := 'invalidmethod1';
  lReq.Params.Add(1);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
  procedure(Resp: IJSONRPCResponse)
  begin
    ShowMessage(Resp.Error.ErrMessage);
  end);
end;

procedure TMainForm.btnInvalid2Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create(1234);
  lReq.Method := 'invalidmethod2';
  lReq.Params.Add(1);
  FExecutor.ExecuteNotificationAsync(
    '/jsonrpc',
    lReq,
    procedure (Exc: Exception)
    begin
      ShowMessage(Exc.Message);
    end);
end;

procedure TMainForm.btnInvalidMethodClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'notexists';
  FExecutor.ExecuteNotificationAsync('/jsonrpc', lNotification);
end;

procedure TMainForm.btnNotificationClick(Sender: TObject);
var
  lNotification: IJSONRPCNotification;
begin
  lNotification := TJSONRPCNotification.Create;
  lNotification.Method := 'dosomething';
  FExecutor.ExecuteNotificationAsync('/jsonrpc', lNotification);
end;

procedure TMainForm.btnObjDictClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lResp: IJSONRPCResponse;
  lMultiDS: TMultiDataset;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getobjdict');
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(JSONRPCResponse: IJSONRPCResponse)
    begin
      lMultiDS := TMultiDataset.Create;
      try
        JsonObjectToObject(lResp.ResultAsJSONObject, lMultiDS);
        lbMulti.Clear;

        lMultiDS.Customers.First;
        lbMulti.Items.Add('** CUSTOMERS **');
        while not lMultiDS.Customers.Eof do
        begin
          lbMulti.Items.Add(Format('%-20s (Code %3s)', [lMultiDS.Customers.FieldByName('Name').AsString,
            lMultiDS.Customers.FieldByName('Code').AsString]));
          lMultiDS.Customers.Next;
        end;

        lMultiDS.People.First;
        lbMulti.Items.Add('** PEOPLE **');
        while not lMultiDS.People.Eof do
        begin
          lbMulti.Items.Add(Format('%s %s', [lMultiDS.People.FieldByName('FirstName').AsString,
            lMultiDS.People.FieldByName('LastName').AsString]));
          lMultiDS.People.Next;
        end;

      finally
        lMultiDS.Free;
      end;
    end);
end;

procedure TMainForm.btnReverseStringClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'reversestring';
  lReq.RequestID := Random(1000);
  lReq.Params.AddByName('aString', edtReverseString.Text);
  lReq.Params.AddByName('aUpperCase', CheckBox1.Checked);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
  procedure (Resp: IJSONRPCResponse)
  begin
    edtReversedString.Text := Resp.Result.AsString;
  end);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  lPerson: TPerson;
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'saveperson';
  lReq.RequestID := Random(1000);
  lPerson := TPerson.Create;
  lReq.Params.AddByName('Person', lPerson, pdtObject);
  lPerson.FirstName := edtFirstName.Text;
  lPerson.LastName := edtLastName.Text;
  lPerson.Married := chkMarried.Checked;
  lPerson.DOB := dtDOB.Date;
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      ShowMessage('Person saved with ID = ' + Resp.Result.AsInteger.ToString);
    end);
end;

procedure TMainForm.btnSearchClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lJSON: TJsonArray;
  lJObj: TJsonObject;
begin
  ListBox1.Clear;
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'searchproducts';
  lReq.RequestID := 1234;
  lReq.Params.Add(edtSearchText.Text);
  FExecutor.ExecuteRequestAsync('/rpcdatamodule', lReq,
    procedure(Resp: IJSONRPCResponse)
    var
      I: Integer;
    begin
      // Remember that TObject descendants (but TDataset, TJDOJSONObject and TJDOJSONArray)
      // are serialized as JSON objects
      lJSON := Resp.Result.AsObject as TJsonArray;
      for I := 0 to lJSON.Count - 1 do
      begin
        lJObj := lJSON[I].ObjectValue;
        ListBox1.Items.Add(Format('%6s: %-34s € %5.2f', [lJObj.S['codice'], lJObj.S['descrizione'], lJObj.F['prezzo']]));
      end;
    end);
end;

procedure TMainForm.btnSingleRecClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'GetPersonRec';
  lReq.RequestID := Random(1000);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    var
      lPersonRec: TTestRec;
    begin
      lPersonRec := TJSONUtils.JSONObjectToRecord<TTestRec>(Resp);
      lbLogRec.Lines.Text := Resp.ResultAsJSONObject.ToJSON(False);
      lbLogRec.Lines.Add('-- record --');
      lbLogRec.Lines.Add(lPersonRec.ToString);
    end, fGeneralErrorHandler);
end;

procedure TMainForm.btnSubtractClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lExecutorAsync: IMVCJSONRPCExecutorAsync;
begin
  lExecutorAsync := TMVCJSONRPCExecutor.Create('http://localhost:8080');
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.RequestID := Random(1000);
  lReq.Params.Add(StrToInt(edtValue1.Text));
  lReq.Params.Add(StrToInt(edtValue2.Text));
  lExecutorAsync
    .ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(JSONRPCResp: IJSONRPCResponse)
    begin
      edtResult.Text := JSONRPCResp.Result.AsInteger.ToString;
    end);
end;

procedure TMainForm.btnSubtractWithNamedParamsClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.RequestID := Random(1000);
  lReq.Params.AddByName('Value1', StrToInt(Edit1.Text));
  lReq.Params.AddByName('Value2', StrToInt(Edit2.Text));
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      Edit3.Text := Resp.Result.AsInteger.ToString;
    end);
end;

procedure TMainForm.btnWithJSONClick(Sender: TObject);
var
  lPerson: TJsonObject;
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'SaveObjectWithJSON';
  lReq.RequestID := 1234;
  lPerson := TJsonObject.Create;
  lReq.Params.Add(lPerson, pdTJDOJsonObject);
  lPerson.S['StringProp'] := 'Hello World';
  lPerson.O['JSONObject'] := TJsonObject.Parse('{"name":"Daniele"}') as TJsonObject;
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      lPerson := Resp.Result.AsObject as TJsonObject;
      ShowMessage(lPerson.ToJSON(False));
    end);
end;

procedure TMainForm.btnParallelClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lThreadCount: Int64;
  Val1, Val2, Val3, Val4: String;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'subtract';
  lReq.RequestID := Random(1000);
  lReq.Params.AddByName('Value1', StrToInt(Edit1.Text));
  lReq.Params.AddByName('Value2', StrToInt(Edit2.Text));
  lThreadCount := 4;

  TThread.CreateAnonymousThread(
  procedure
  begin
    while TInterlocked.Read(lThreadCount) > 0 do
    begin
      Sleep(100);
    end;
    TThread.Synchronize(nil,
    procedure
    begin
      ShowMessage(
        Val1 + sLineBreak +
        Val2 + sLineBreak +
        Val3 + sLineBreak +
        Val4 + sLineBreak
      );
    end);
  end).Start;

  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      Val1 := Resp.Result.AsInteger.ToString;
      TInterlocked.Decrement(lThreadCount);
    end);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      Val2 := Resp.Result.AsInteger.ToString;
      TInterlocked.Decrement(lThreadCount);
    end);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      Val3 := Resp.Result.AsInteger.ToString;
      TInterlocked.Decrement(lThreadCount);
    end);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      Val4 := Resp.Result.AsInteger.ToString;
      TInterlocked.Decrement(lThreadCount);
    end);
end;

procedure TMainForm.btnPassAndGetRecordClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lPersonRec: TTestRec;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'SavePersonRec';
  lReq.RequestID := Random(1000);
  lPersonRec := TTestRec.Create(2);
  lReq.Params.Add(TValue.From<TTestRec>(lPersonRec), pdtRecordOrArrayOfRecord);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure (Resp: IJSONRPCResponse)
    var
      lResPersonRec: TTestRec;
    begin
      lResPersonRec := TJSONUtils.JSONObjectToRecord<TTestRec>(Resp);
      lbLogRec.Lines.Text := Resp.ResultAsJSONObject.ToJSON(False);
    end);
end;

procedure TMainForm.btnGenericExceptionClick(Sender: TObject);
var
  lReq: IJSONRPCNotification;
begin
  ShowMessage('Now will be raised a EDivByZero exception on the server. This exception will be catched by the client');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  FExecutor.ExecuteNotificationAsync('/jsonrpc', lReq);
end;

procedure TMainForm.btnGenericExcWithCustomHAndling2Click(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage
    ('Now will be raised a EInvalidPointerOperation exception on the server. However this exception will be handled by a custom exception handler wich will add a data property with extra data');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(2);
  FExecutor.ExecuteRequestAsync('/jsonrpcex', lReq, nil);
end;

procedure TMainForm.btnGenericExcWithCustomHandlingClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage
    ('Now will be raised a EDivByZero exception on the server. However this exception will be handled by a custom exception handler wich will add a data property with extra data');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(1);
  FExecutor.ExecuteRequestAsync('/jsonrpcex', lReq, nil);
end;

procedure TMainForm.btnGenericExcWithoutCustomHandlingClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  ShowMessage('Now will be raised a Exception exception on the server.');
  lReq := TJSONRPCRequest.Create(1234, 'RaiseGenericException');
  lReq.Params.Add(99);
  FExecutor.ExecuteRequestAsync('/jsonrpcex', lReq, nil, fGeneralErrorHandler);
end;

procedure TMainForm.btnGetArrayOfRecordsClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
  lPeopleRec: TArray<TTestRec>; // server serializes a static array, we read it as dynarray
  I: Integer;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'GetPeopleRecStaticArray';
  lReq.RequestID := Random(1000);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      lPeopleRec := TJSONUtils.JSONArrayToArrayOfRecord<TTestRec>(Resp);
      lbLogRec.Lines.Text := Resp.ResultAsJSONArray.ToJSON(False);
      lbLogRec.Lines.Add('-- array of record elements --');
      I := 1;
      for var lPRec in lPeopleRec do
      begin
        lbLogRec.Lines.Add('ITEM : ' + I.ToString);
        lbLogRec.Lines.Add(lPRec.ToString);
        Inc(I);
      end;
    end, fGeneralErrorHandler);
end;

procedure TMainForm.btnGetDynArrayClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  lReq := TJSONRPCRequest.Create;
  lReq.Method := 'GetPeopleRecDynArray';
  lReq.RequestID := Random(1000);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    var
      lPeopleRec : TArray<TTestRec>;
    begin
      lPeopleRec := TJSONUtils.JSONArrayToArrayOfRecord<TTestRec>(Resp);
      lbLogRec.Lines.Text := Resp.ResultAsJSONArray.ToJSON(False);
    end, fGeneralErrorHandler);
end;

procedure TMainForm.btnGetListOfDatasetClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'GetDataSetList');
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    var
      lMultiDS: TObjectList<TDataSet>;
    begin
      lMultiDS := TObjectList<TDataSet>.Create(True);
      try
        JsonArrayToList(Resp.ResultAsJSONArray, WrapAsList(lMultiDS), TDataSet, TMVCSerializationType.stDefault, nil);
      finally
        lMultiDS.Free;
      end;
    end);
end;

procedure TMainForm.btnGetMultiClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getmulti');
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    var
      lMultiDS: TMultiDataset;
    begin
      lMultiDS := TMultiDataset.Create;
      try
        JsonObjectToObject(Resp.ResultAsJSONObject, lMultiDS);
        lbMulti.Clear;

        lMultiDS.Customers.First;
        lbMulti.Items.Add('** CUSTOMERS **');
        while not lMultiDS.Customers.Eof do
        begin
          lbMulti.Items.Add(Format('%-20s (Code %3s)', [lMultiDS.Customers.FieldByName('Name').AsString,
            lMultiDS.Customers.FieldByName('Code').AsString]));
          lMultiDS.Customers.Next;
        end;

        lMultiDS.People.First;
        lbMulti.Items.Add('** PEOPLE **');
        while not lMultiDS.People.Eof do
        begin
          lbMulti.Items.Add(Format('%s %s', [lMultiDS.People.FieldByName('FirstName').AsString,
            lMultiDS.People.FieldByName('LastName').AsString]));
          lMultiDS.People.Next;
        end;
      finally
        lMultiDS.Free;
      end;
    end,
  nil,
  jrpcGET);
end;

procedure TMainForm.edtGetCustomersClick(Sender: TObject);
var
  lReq: IJSONRPCRequest;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create(Random(1000), 'getcustomers');
  lReq.Params.AddByName('FilterString', edtFilter.Text);
  FExecutor.ExecuteRequestAsync('/jsonrpc', lReq,
    procedure(Resp: IJSONRPCResponse)
    begin
      FDMemTable1.Active := True;
      FDMemTable1.LoadFromTValue(Resp.Result);
      FDMemTable1.First;
    end,
    procedure(Exc: Exception)
    begin
      ShowMessage(Exc.ClassName + ': ' + Exc.Message);
    end,
    jrpcGET);
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  SIMULATE_SLOW_NETWORK = False;
begin
  FExecutor := TMVCJSONRPCExecutor.Create('http://localhost:8080');

  FExecutor.SetOnSendCommandAsync(
    procedure(JSONRPCObject: IJSONRPCObject)
    begin
      if SIMULATE_SLOW_NETWORK then
      begin
        Sleep(1000 + Random(3000));
      end;
      Log.Debug('REQUEST : ' + JSONRPCObject.ToString(True), 'jsonrpc');
    end);

  FExecutor.SetOnReceiveResponseAsync(
    procedure(Req, Resp: IJSONRPCObject)
    begin
      Log.Debug('>> OnReceiveResponse // start', 'jsonrpc');
      Log.Debug('     REQUEST : ' + Req.ToString(True), 'jsonrpc');
      Log.Debug('     RESPONSE: ' + Resp.ToString(True), 'jsonrpc');
      Log.Debug('<< OnReceiveResponse // end', 'jsonrpc');
    end);

  FExecutor.SetOnReceiveHTTPResponseAsync(
    procedure(HTTPResp: IHTTPResponse)
    begin
      Log.Debug('RESPONSE: ' + HTTPResp.ContentAsString(), 'jsonrpc');
    end);


  FExecutor.SetConfigureHTTPClientAsync(
      procedure (HTTPClient: THTTPClient)
      begin
        HTTPClient.ResponseTimeout := 20000;
        HTTPClient.CustomHeaders['X-DMVCFRAMEWORK'] := 'DMVCFRAMEWORK_VERSION ' + DMVCFRAMEWORK_VERSION;
      end);


  dtNextMonday.Date := Date;
  // these are the methods to handle http headers in JSONRPC
  // the following line and the check on the server is just for demo
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));
  Assert(FExecutor.HTTPHeadersCount = 1);
  FExecutor.ClearHTTPHeaders;
  Assert(FExecutor.HTTPHeadersCount = 0);
  FExecutor.AddHTTPHeader(TNetHeader.Create('x-token', TGUID.NewGuid.ToString));

  PageControl1.ActivePageIndex := 0;

  fGeneralErrorHandler := procedure(Exc: Exception)
    begin
      ShowMessage(Exc.ClassName + ': ' + Exc.Message);
    end;


  fWaiting := TWaitingForm.Create(Self);
  fWaiting.PopupParent := Self;
  FExecutor.SetOnBeginAsyncRequest(
    procedure
    begin
      fWaiting.IncreaseWaitingCount;
    end);

  FExecutor.SetOnEndAsyncRequest(
    procedure
    begin
      fWaiting.DecreaseWaitingCount;
    end);
end;

end.
