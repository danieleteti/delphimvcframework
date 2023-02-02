unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, REST.Client, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, REST.Response.Adapter, Data.Bind.ObjectScope,
  FMX.ListView, FMX.Controls.Presentation, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, REST.Types, FireDAC.Stan.StorageBin,
  System.Net.URLClient, System.Net.HttpClientComponent,
  FMX.TabControl, System.Actions, FMX.ActnList, FMX.Edit, FMX.Layouts,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    ListView1: TListView;
    FDMemTable1: TFDMemTable;
    Button1: TButton;
    FDMemTable1id: TIntegerField;
    FDMemTable1name: TStringField;
    FDMemTable1year: TIntegerField;
    FDMemTable1grapes: TStringField;
    FDMemTable1country: TStringField;
    FDMemTable1region: TStringField;
    FDMemTable1description: TStringField;
    BindSourceDB12: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    Layout1: TLayout;
    Layout2: TLayout;
    Label1: TLabel;
    Edit1: TEdit;
    Layout3: TLayout;
    Label2: TLabel;
    Edit2: TEdit;
    Layout4: TLayout;
    lblYear: TLabel;
    Edit3: TEdit;
    Layout5: TLayout;
    Layout6: TLayout;
    Label3: TLabel;
    Edit4: TEdit;
    Layout7: TLayout;
    Label4: TLabel;
    Edit5: TEdit;
    Layout8: TLayout;
    Label5: TLabel;
    Memo1: TMemo;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    ChangeTabAction1: TChangeTabAction;
    Button2: TButton;
    PreviousTabAction1: TPreviousTabAction;
    procedure Button1Click(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure PreviousTabAction1Update(Sender: TObject);
    procedure ListView1DeleteItem(Sender: TObject; AIndex: Integer);
  private
    procedure LoadWines;
  public
    { Public declarations }
  end;

const
{$IF Defined(MSWINDOWS)}
  URL = 'http://localhost:3000/api';
{$ENDIF}
{$IF Defined(ANDROID)}
  URL = 'http://192.168.1.153:3000/api';
{$ENDIF}

var
  HeaderFooterForm: THeaderFooterForm;

implementation

uses
  MVCFramework.DataSet.Utils, MVCFramework.AsyncTask, System.Threading,
  System.Net.HttpClient, FMX.DialogService;

{$R *.fmx}

procedure THeaderFooterForm.Button1Click(Sender: TObject);
begin
  LoadWines;
end;

procedure THeaderFooterForm.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    ChangeTabAction1.Tab := TabItem1;
    ChangeTabAction1.Execute;
    Key := 0;
  end;
end;

procedure THeaderFooterForm.FormShow(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  LoadWines;
end;

procedure THeaderFooterForm.ListView1DeleteItem(Sender: TObject;
  AIndex: Integer);
begin
  var lItemID := FDMemTable1id.Value;
  if FDMemTable1.Locate('id', lItemID) then
  begin
    FDMemTable1.Delete;
  end;
  MVCAsync.Run<IHTTPResponse>(
    function : IHTTPResponse
    begin
      var lClient := THTTPClient.Create;
      try
        Result := lClient.Delete(URL + '/wines/' + lItemID.ToString);
      finally
        lClient.Free;
      end;
    end,
    procedure (const Response: IHTTPResponse)
    begin
      if Response.StatusCode <> 200 then
      begin
        TDialogService.ShowMessage('Cannot delete item');
      end;
    end
  );
end;

procedure THeaderFooterForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  NextTabAction1.ExecuteTarget(Self);
end;

procedure THeaderFooterForm.LoadWines;
begin
//  TTask.Run(
//    procedure
//    begin
//      var lClient := THTTPClient.Create;
//      try
//        var lResp := lClient.Get(URL + '/wines', nil, []);
//        TThread.Synchronize(nil,
//          procedure
//          begin
//            if lResp.StatusCode <> 200 then
//            begin
//              raise Exception.Create('Error: ' + lResp.ContentAsString());
//            end;
//            FDMemTable1.Active := True;
//            FDMemTable1.EmptyDataSet;
//            FDMemTable1.LoadFromJSONArray(lResp.ContentAsString());
//            FDMemTable1.First;
//          end);
//      finally
//        lClient.Free;
//      end;
//    end);

  MVCAsync.Run<IHTTPResponse>(
    function : IHTTPResponse
    begin
      var lClient := THTTPClient.Create;
      try
        Result := lClient.Get(URL + '/wines', nil, []);
      finally
        lClient.Free;
      end;
    end,
    procedure (const Response: IHTTPResponse)
    begin
      FDMemTable1.Active := True;
      FDMemTable1.EmptyDataSet;
      FDMemTable1.LoadFromJSONArray(Response.ContentAsString());
      FDMemTable1.First;
    end
  );
end;


procedure THeaderFooterForm.PreviousTabAction1Update(Sender: TObject);
begin
  PreviousTabAction1.Visible := TabControl1.ActiveTab = TabItem2;
end;

end.
