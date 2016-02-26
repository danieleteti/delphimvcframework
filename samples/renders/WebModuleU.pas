unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp, MVCFramework, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.IBBase, FireDAC.Phys.IB, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait;

type
  TWebModule1 = class(TWebModule)
    FDConnection1: TFDConnection;
    qryCustomers: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    procedure WebModuleCreate(Sender: TObject);
  private
    DMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{$R *.dfm}

uses RenderSampleControllerU, MVCFramework.Commons;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DMVC := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.ViewPath] = 'templates';
    end);
  DMVC.AddController(TRenderSampleController);
end;

end.
