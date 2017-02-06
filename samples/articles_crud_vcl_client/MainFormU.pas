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
    FLoading: Boolean;
    Clt: TRESTClient;
    { Private declarations }
    procedure ShowError(const AResponse: IRESTResponse);
  end;

var
  MainForm: TMainForm;

implementation

uses
  ObjectsMappers, System.UITypes;

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
  Res := Clt.doGET('/articles', []);
  DataSet.DisableControls;
  try
    FLoading := true;
    dsArticles.AppendFromJSONArrayString(Res.BodyAsString);
    FLoading := false;
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
    Res := Clt.DataSetDelete('/articles', dsArticlesid.AsString);
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
  if not FLoading then
  begin
    if dsArticles.State = dsInsert then
      Res := Clt.DataSetInsert('/articles', dsArticles)
    else
      Res := Clt.DataSetUpdate('/articles', dsArticles, dsArticlesid.AsString);
    if not(Res.ResponseCode in [200, 201]) then
    begin
      ShowError(Res);
      Abort;
    end
    else
      DataSet.Refresh;
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
  Res := Clt.doGET('/articles', [DataSet.FieldByName('id').AsString]);
  FLoading := true;
  DataSet.Edit;
  DataSet.LoadFromJSONObjectString(Res.BodyAsString);
  DataSet.Post;
  FLoading := false;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Clt.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Clt := TRESTClient.Create('localhost', 8080);
end;

procedure TMainForm.ShowError(const AResponse: IRESTResponse);
begin
  MessageDlg(
    AResponse.ResponseCode.ToString + ': ' + AResponse.ResponseText + sLineBreak +
    AResponse.BodyAsJsonObject.Get('message').JsonValue.Value,
    mtError, [mbOK], 0);
end;

end.
