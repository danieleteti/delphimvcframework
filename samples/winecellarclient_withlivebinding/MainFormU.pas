unit MainFormU;

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
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ComCtrls,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  Vcl.Mask,
  Vcl.DBCtrls,
  Vcl.ExtCtrls,
  MVCFramework.RESTClient.Intf,
  MVCFramework.RESTClient,
  Data.Bind.GenData,
  Vcl.Bind.GenData,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  System.Rtti,
  System.Bindings.Outputs,
  Vcl.Bind.Editors,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  Vcl.Buttons,
  Vcl.Bind.Navigator,
  WinesBO,
  Generics.Collections,
  Data.Bind.Controls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PrototypeBindSource1: TPrototypeBindSource;
    ListView1: TListView;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Memo2: TMemo;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    BindNavigator1: TBindNavigator;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1DblClick(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure BindNavigator1BeforeAction(Sender: TObject; Button: TNavigateButton);

  private
    RESTClient: IMVCRESTClient;
    WinesAdapter: TListBindSourceAdapter<TWine>;
    FWines: TObjectList<TWine>;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses
  MVCFramework.SystemJSONUtils,
  MVCFramework.Serializer.Defaults,
  System.JSON,
  MVCFramework.Commons;

{$R *.dfm}


procedure TForm5.BindNavigator1BeforeAction(Sender: TObject; Button: TNavigateButton);
var
  Resp: IMVCRESTResponse;
begin
  Resp := nil;
  case Button of
    nbDelete:
      begin
        Resp := RESTClient
          .AddPathParam('id',(WinesAdapter.Current as TWine).id.ToString)
          .Delete('/wines/{id}');
      end;
    nbPost:
      begin
        case WinesAdapter.State of
          seEdit:
            begin
              WinesAdapter.Post;
              Resp := RESTClient
                .AddPathParam('id', (WinesAdapter.Current as TWine).id.ToString)
                .Put('/wines/{id}',
                GetDefaultSerializer.SerializeObject(WinesAdapter.Current));
              Abort;
            end;
          seInsert:
            begin
              WinesAdapter.Post;
              Resp := RESTClient.Post('/wines', GetDefaultSerializer.SerializeObject(WinesAdapter.Current));
              Abort;
            end;
        end;
      end;
  end;

  if Assigned(Resp) and (not Resp.StatusCode in [200, 201]) then
    raise Exception.Create(Resp.StatusText);
end;

procedure TForm5.Button1Click(Sender: TObject);
var
  response: IMVCRESTResponse;
begin
  response := RESTClient.Get('/wines');
  Memo1.Lines.Text := response.Content;
  FWines.Clear;
  GetDefaultSerializer.DeserializeCollection(response.Content, FWines, TWine);
  WinesAdapter.SetList(FWines, false);
  WinesAdapter.Active := True;
end;

procedure TForm5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RESTClient := nil;
  FWines.free;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  RESTClient := TMVCRESTClient.New.BaseURL('localhost', 3000);
  FWines := TObjectList<TWine>.Create(True);
  PrototypeBindSource1.Active := True;
end;

procedure TForm5.ListView1DblClick(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 1;
end;

procedure TForm5.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  WinesAdapter := TListBindSourceAdapter<TWine>.Create(PrototypeBindSource1);
  ABindSourceAdapter := WinesAdapter;
end;

end.
