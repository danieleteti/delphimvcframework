unit MainFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, REST.Client, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, REST.Response.Adapter, Data.Bind.ObjectScope,
  FMX.ListView, FMX.Controls.Presentation, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    ListView1: TListView;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    RESTResponse1: TRESTResponse;
    BindSourceDB12: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

{$R *.fmx}

procedure THeaderFooterForm.Button1Click(Sender: TObject);
begin
  {
    client -> request http
    server -> controller ->
      (c) deserializza i dati della richiesta
      (c) passa i dati agli strati più interni
      (bl) gli strati più interni utilizano i dati per farci qualcosa
      (c) recupera i dati dagli strati più interni
      (c) li serializza per il client

  }
  RESTRequest1.Execute;
end;

end.
