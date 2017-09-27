unit MainClientFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Net.HttpClientComponent, Vcl.StdCtrls,
  System.Net.URLClient, System.Net.HttpClient, Data.DB, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TForm10 = class(TForm)
    edtValue1: TEdit;
    edtValue2: TEdit;
    btnSubstract: TButton;
    edtResult: TEdit;
    edtReverseString: TEdit;
    btnReverseString: TButton;
    edtReversedString: TEdit;
    edtFilter: TEdit;
    edtGetCustomers: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    FDMemTable1Code: TIntegerField;
    FDMemTable1Name: TStringField;
    procedure btnSubstractClick(Sender: TObject);
    procedure btnReverseStringClick(Sender: TObject);
    procedure edtGetCustomersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses
  MVCFramework.JSONRPC, MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects, MVCFramework.Serializer.Commons, MVCFramework.DataSet.Utils;

{$R *.dfm}

procedure JSONRPCExec(const aJSONRPCURL: string; const aJSONRPCRequest: TJSONRPCRequest; out aJSONRPCResponse: TJSONRPCResponse);
var
  lSS: TStringStream;
  lHttpResp: IHTTPResponse;
  lHTTP: THTTPClient;
begin
  lSS := TStringStream.Create(aJSONRPCRequest.AsJSONString);
  try
    lSS.Position := 0;
    lHTTP := THTTPClient.Create;
    try
      lHttpResp := lHTTP.Post('http://localhost:8080/jsonrpc', lSS, nil,
        [
        TNetHeader.Create('content-type', 'application/json'),
        TNetHeader.Create('accept', 'application/json')
        ]);
      if (lHttpResp.StatusCode <> 204) then
      begin
        aJSONRPCResponse := TJSONRPCResponse.Create;
        try
          aJSONRPCResponse.AsJSONString := lHttpResp.ContentAsString;
          if Assigned(aJSONRPCResponse.Error) then
            raise Exception.CreateFmt('Error [%d]: %s', [aJSONRPCResponse.Error.Code, aJSONRPCResponse.Error.ErrMessage]);
        except
          aJSONRPCResponse.Free;
          raise;
        end;
      end;
    finally
      lHTTP.Free;
    end;
  finally
    lSS.Free;
  end;
end;

procedure TForm10.btnReverseStringClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'reversestring';
    lReq.ID := Random(1000);
    lReq.Params.Add(edtReverseString.Text);
    JSONRPCExec('http://localhost:8080/jsonrpc', lReq, lResp);
    try
      edtReversedString.Text := lResp.Result.AsString;
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.btnSubstractClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'subtract';
    lReq.ID := Random(1000);
    lReq.Params.Add(StrToInt(edtValue1.Text));
    lReq.Params.Add(StrToInt(edtValue2.Text));

    JSONRPCExec('http://localhost:8080/jsonrpc', lReq, lResp);
    try
      edtResult.Text := lResp.Result.AsInteger.ToString;
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

procedure TForm10.edtGetCustomersClick(Sender: TObject);
var
  lReq: TJSONRPCRequest;
  lResp: TJSONRPCResponse;
begin
  FDMemTable1.Active := False;
  lReq := TJSONRPCRequest.Create;
  try
    lReq.Method := 'getcustomers';
    lReq.ID := Random(1000);
    lReq.Params.Add(edtFilter.Text);
    JSONRPCExec('http://localhost:8080/jsonrpc', lReq, lResp);
    try
      FDMemTable1.Active := True;
      FDMemTable1.LoadFromTValue(lResp.Result);
    finally
      lResp.Free;
    end;
  finally
    lReq.Free;
  end;
end;

end.
