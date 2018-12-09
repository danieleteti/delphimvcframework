unit MainWebModuleU;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TMyWebModule = class(TWebModule)
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FMVC: TMVCEngine;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMyWebModule;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  MVCFramework.Commons,
  MyObjectU,
  MVCFramework.JSONRPC,
  MainDM;

procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self);

  FMVC.PublishObject(
    function: TObject
    begin
      Result := TMyObject.Create;
    end, '/jsonrpc');

  FMVC.PublishObject(
    function: TObject
    begin
      Result := TdmMain.Create(nil);
    end, '/rpcdatamodule');
end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
