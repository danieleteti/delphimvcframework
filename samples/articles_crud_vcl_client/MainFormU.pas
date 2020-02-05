unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.StdCtrls, MVCFramework.RESTClient,
  Vcl.DBCtrls;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    dsArticles: TFDMemTable;
    dsArticlesid: TIntegerField;
    dsArticlescode: TStringField;
    dsArticlesdescription: TStringField;
    dsArticlesprice: TCurrencyField;
    dsrcArticles: TDataSource;
    DBNavigator1: TDBNavigator;
    btnOpen: TButton;
    btnRefreshRecord: TButton;
    Button1: TButton;
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
  private
    fLoading: Boolean;
    fRESTClient: TRESTClient;
    { Private declarations }
    procedure ShowError(const AResponse: IRESTResponse);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.UITypes, MVCFramework.DataSet.Utils;

{$R *.dfm}


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  dsArticles.Close;
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  dsArticles.Open;
end;

procedure TMainForm.btnRefreshRecordClick(Sender: TObject);
begin
  dsArticles.RefreshRecord;
end;

procedure TMainForm.dsArticlesAfterOpen(DataSet: TDataSet);
var
  Res: IRESTResponse;
begin
  // this a simple sychronous request...
  Res := fRESTClient.doGET('/articles', []);
  if Res.HasError then
  begin
    ShowError(Res);
    Exit;
  end;

  DataSet.DisableControls;
  try
    fLoading := true;
    dsArticles.LoadFromJSONArrayString(Res.BodyAsString);
    fLoading := false;
    dsArticles.First;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TMainForm.dsArticlesBeforeDelete(DataSet: TDataSet);
var
  Res: IRESTResponse;
begin
  if dsArticles.State = dsBrowse then
    Res := fRESTClient.DataSetDelete('/articles', dsArticlesid.AsString);
  if not(Res.ResponseCode in [200]) then
  begin
    ShowError(Res);
    Abort;
  end;
end;

procedure TMainForm.dsArticlesBeforePost(DataSet: TDataSet);
var
  Res: IRESTResponse;
begin
  if not fLoading then
  begin
    if dsArticles.State = dsInsert then
      Res := fRESTClient.DataSetInsert('/articles', dsArticles)
    else
      Res := fRESTClient.DataSetUpdate('/articles', dsArticles, dsArticlesid.AsString);
    if not(Res.ResponseCode in [200, 201]) then
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
  Res: IRESTResponse;
begin
  Res := fRESTClient.doGET('/articles', [DataSet.FieldByName('id').AsString]);
  fLoading := true;
  DataSet.LoadFromJSONObjectString(Res.BodyAsString);
  fLoading := false;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fRESTClient.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fRESTClient := MVCFramework.RESTClient.TRESTClient.Create('localhost', 8080);
end;

procedure TMainForm.ShowError(const AResponse: IRESTResponse);
begin
  if AResponse.HasError then
    MessageDlg(
      AResponse.Error.HTTPError.ToString + ': ' + AResponse.Error.ExceptionMessage + sLineBreak +
      '[' + AResponse.Error.ExceptionClassname + ']',
      mtError, [mbOK], 0)
  else
    MessageDlg(
      AResponse.ResponseCode.ToString + ': ' + AResponse.ResponseText + sLineBreak +
      AResponse.BodyAsString,
      mtError, [mbOK], 0);
end;

end.
