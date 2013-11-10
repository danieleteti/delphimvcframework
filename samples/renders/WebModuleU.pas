unit WebModuleU;

interface

uses
  System.SysUtils, System.Classes,
  Web.HTTPApp, MVCFramework, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.IBBase, FireDAC.Phys.IB;

type
  TWebModule1 = class(TWebModule)
    FDConnection1: TFDConnection;
    qryCustomers: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    procedure WebModuleCreate(Sender: TObject);
  private
    DMVC: TMVCEngine;
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation


{$R *.dfm}


uses RenderSampleControllerU;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  DMVC := TMVCEngine.Create(self);
  DMVC.AddController(TRenderSampleController);
end;

end.
