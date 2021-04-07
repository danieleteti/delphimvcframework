unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.StdCtrls, MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  Vcl.DBCtrls;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    dsArticles: TFDMemTable;
    dsrcArticles: TDataSource;
    DBNavigator1: TDBNavigator;
    btnOpen: TButton;
    btnRefreshRecord: TButton;
    Button1: TButton;
    Panel2: TPanel;
    EditFilter: TEdit;
    Label1: TLabel;
    btnFilter: TButton;
    procedure FormCreate(Sender: TObject);
    procedure dsArticlesBeforePost(DataSet: TDataSet);
    procedure dsArticlesBeforeDelete(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure dsArticlesBeforeRefresh(DataSet: TDataSet);
    procedure dsArticlesAfterOpen(DataSet: TDataSet);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure dsArticlesBeforeRowRequest(DataSet: TFDDataSet);
    procedure btnRefreshRecordClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
  private
    fFilter: string;
    fLoading: Boolean;
    fRESTClient: IMVCRESTClient;
    { Private declarations }
    procedure ShowError(const AResponse: IMVCRESTResponse);
    procedure SetFilter(const Value: string);
  public
    property Filter: string read fFilter write SetFilter;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.UITypes, MVCFramework.DataSet.Utils, MVCFramework.FireDAC.Utils, MVCFramework.Commons,
  JsonDataObjects, MVCFramework.Logger;

{$R *.dfm}

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  dsArticles.Close;
end;

procedure TMainForm.btnFilterClick(Sender: TObject);
begin
  dsArticles.Close;
  Filter := EditFilter.Text;;
  dsArticles.Open;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
  Res: IMVCRESTResponse;
  lJMeta: TJSONObject;
begin
  dsArticles.Close;
  Res := fRESTClient.Get('/articles/meta');
  if not Res.Success then
  begin
    ShowError(Res);
    Exit;
  end;

  Log.Debug(Res.Content, 'trace');
  lJMeta := StrToJSONObject(Res.Content);
  try
    TFireDACUtils.CreateDatasetFromMetadata(dsArticles, lJMeta.O['data']);
    // dsArticles.CreateDatasetFromMetadata(lJMeta);
  finally
    lJMeta.Free;
  end;

  Filter := '';
  dsArticles.Open;
end;

procedure TMainForm.btnRefreshRecordClick(Sender: TObject);
begin
  dsArticles.RefreshRecord;
end;

procedure TMainForm.dsArticlesAfterOpen(DataSet: TDataSet);
var
  Res: IMVCRESTResponse;
begin
  if fFilter.IsEmpty then
  begin
    // this a simple sychronous request...
    Res := fRESTClient.Get('/articles');
  end
  else
  begin
    Res := fRESTClient.AddQueryStringParam('q', fFilter).Get('/articles/searches');
  end;

  if not Res.Success then
  begin
    ShowError(Res);
    Exit;
  end;

  Log.Debug(Res.Content, 'trace');

  DataSet.DisableControls;
  try
    fLoading := true;
    dsArticles.LoadJSONArrayFromJSONObjectProperty('data', Res.Content);
    fLoading := false;
    dsArticles.First;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TMainForm.dsArticlesBeforeDelete(DataSet: TDataSet);
var
  Res: IMVCRESTResponse;
begin
  if dsArticles.State = dsBrowse then
    Res := fRESTClient.DataSetDelete('/articles', dsArticles.FieldByName('id').AsString);
  if not(Res.StatusCode in [200]) then
  begin
    ShowError(Res);
    Abort;
  end;
end;

procedure TMainForm.dsArticlesBeforePost(DataSet: TDataSet);
var
  Res: IMVCRESTResponse;
begin
  if not fLoading then
  begin
    if dsArticles.State = dsInsert then
      Res := fRESTClient.DataSetInsert('/articles', dsArticles)
    else
      Res := fRESTClient.DataSetUpdate('/articles', dsArticles.FieldByName('id').AsString,
        dsArticles);
    if not(Res.StatusCode in [200, 201]) then
    begin
      ShowError(Res);
      Abort;
    end
    else
    begin
      DataSet.Refresh;
    end;
  end;
end;

procedure TMainForm.dsArticlesBeforeRefresh(DataSet: TDataSet);
begin
  DataSet.Close;
  DataSet.Open;
end;

procedure TMainForm.dsArticlesBeforeRowRequest(DataSet: TFDDataSet);
var
  Res: IMVCRESTResponse;
begin
  Res := fRESTClient
    .AddPathParam('param1', DataSet.FieldByName('id').AsString)
    .Get('/articles/{param1}');
  fLoading := true;
  DataSet.LoadJSONObjectFromJSONObjectProperty('data', Res.Content);
  fLoading := false;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fRESTClient := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fRESTClient := TMVCRESTClient.New.BaseURL('localhost', 8080);
end;

procedure TMainForm.SetFilter(const Value: string);
begin
  fFilter := Value;
  EditFilter.Text := Value;
end;

procedure TMainForm.ShowError(const AResponse: IMVCRESTResponse);
begin
  if AResponse.Success then
    MessageDlg(AResponse.StatusCode.ToString + ': ' + AResponse.StatusText + sLineBreak + '[' +
      AResponse.Content + ']', mtError, [mbOK], 0)
  else
    MessageDlg(AResponse.StatusCode.ToString + ': ' + AResponse.StatusText + sLineBreak +
      AResponse.Content, mtError, [mbOK], 0);
end;

end.
