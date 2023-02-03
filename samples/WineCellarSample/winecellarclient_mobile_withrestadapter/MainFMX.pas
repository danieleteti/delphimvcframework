unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, MVCFramework.RESTAdapter, RESTServicesU,
  FMX.ListView.Types, FMX.ListView, System.Actions, FMX.ActnList,
  System.Generics.Collections, WinesBO, FMX.ListBox, FMX.Layouts, FMX.MultiView,
  Data.Bind.GenData, FMX.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Controls.Presentation, FMX.Edit,
  Data.Bind.Controls, FMX.Bind.Navigator, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base;

type
  TTabbedForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl1: TTabControl;
    WineListTabItem: TTabItem;
    EdtTabItem: TTabItem;
    BtnWineList: TButton;
    ActionList1: TActionList;
    acWineList: TAction;
    WineListView: TListView;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    DrawerBtn: TButton;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    EdtName: TEdit;
    LblName: TLabel;
    LblCountry: TLabel;
    EdtCountry: TEdit;
    EdtRegion: TEdit;
    LblRegion: TLabel;
    LblYear: TLabel;
    EdtYear: TEdit;
    EdtGrapes: TEdit;
    LblGrapes: TLabel;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    ListBoxItem2: TListBoxItem;
    ChangeTabActionEdtWine: TChangeTabAction;
    ChangeTabActionWineList: TChangeTabAction;
    ToolBar1: TToolBar;
    Button1: TButton;
    acSaveWine: TAction;
    EdtID: TEdit;
    LinkControlToField6: TLinkControlToField;
    procedure FormCreate(Sender: TObject);
    procedure acWineListExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure WineListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
    procedure acSaveWineExecute(Sender: TObject);
  private
    WineRESTService: IWineResource;
    WinesAdapter: TListBindSourceAdapter<TWine>;
    { Private declarations }
  protected
    function GetWine: TWine;
  public
    { Public declarations }
  end;

var
  TabbedForm: TTabbedForm;

implementation

uses Generics.Collections;

{$R *.fmx}


procedure TTabbedForm.acSaveWineExecute(Sender: TObject);
var
  Wine: TWine;
  AsynchReq: IAsynchRequest;
begin
  Wine := GetWine;
  // Asynch
  AsynchReq := TAsynchRequest.Create(
    procedure(AValue: TValue)
    begin
      PrototypeBindSource1.Cancel;
      acWineList.Execute;
      ChangeTabActionWineList.ExecuteTarget(Sender);
    end, nil, true);
  if Wine.id > 0 then
    WineRESTService.UpdateWineById(Wine.id, Wine, AsynchReq)
  else
    WineRESTService.SaveWine(Wine, AsynchReq);
end;

procedure TTabbedForm.acWineListExecute(Sender: TObject);
var
  AsynchReq: IAsynchRequest;
begin
  AsynchReq := TAsynchRequest.Create(
    procedure(AValue: TValue)
    begin
      WinesAdapter.SetList(AValue.AsType<TWines>);
      WinesAdapter.Active := true;
    end, nil, true);
  WineRESTService.GetWineList(AsynchReq);
end;

procedure TTabbedForm.FormCreate(Sender: TObject);
var
  RESTAdapter: TRESTAdapter<IWineResource>;
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := WineListTabItem;
  // REST Service
  RESTAdapter := TRESTAdapter<IWineResource>.Create;
  WineRESTService := RESTAdapter.Build('localhost', 3000);
  PrototypeBindSource1.Active := true;
end;

function TTabbedForm.GetWine: TWine;
var
  FWines: TObjectList<TWine>;
begin
  Result := TWine.Create;
  if not EdtID.Text.IsEmpty then
    Result.id := EdtID.Text.ToInteger;
  Result.name := EdtName.Text;
  Result.year := EdtYear.Text;
  Result.grapes := EdtGrapes.Text;
  Result.country := EdtCountry.Text;
  Result.region := EdtRegion.Text;
  FWines := TObjectList<TWine>(WinesAdapter.List);
  Result.description := FWines[PrototypeBindSource1.ItemIndex].description;
end;

procedure TTabbedForm.ListBoxItem1Click(Sender: TObject);
begin
  ChangeTabActionWineList.ExecuteTarget(Sender);
  MultiView1.HideMaster;
end;

procedure TTabbedForm.ListBoxItem2Click(Sender: TObject);
begin
  PrototypeBindSource1.Insert;
  ChangeTabActionEdtWine.ExecuteTarget(Sender);
  MultiView1.HideMaster;
end;

procedure TTabbedForm.PrototypeBindSource1CreateAdapter(Sender: TObject;
var ABindSourceAdapter: TBindSourceAdapter);
begin
  WinesAdapter := TListBindSourceAdapter<TWine>.Create(PrototypeBindSource1);
  ABindSourceAdapter := WinesAdapter;
end;

procedure TTabbedForm.WineListViewItemClick(const Sender: TObject;
const AItem: TListViewItem);
begin
  ChangeTabActionEdtWine.ExecuteTarget(Sender);
end;

end.
