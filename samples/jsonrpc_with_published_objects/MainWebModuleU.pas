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
  MainDM, MVCFramework.Serializer.Commons, JsonDataObjects;

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

  FMVC.PublishObject(
    function: TObject
    begin
      Result := TMyObject.Create;
    end, '/jsonrpcex',
    procedure(Exc: Exception;
      WebContext: TWebContext;
      var ErrorInfo: TMVCJSONRPCExceptionErrorInfo;
      var ExceptionHandled: Boolean)
    var
      lExtra: TJSONObject;
    begin
      if Exc is EInvalidPointer then
      begin
        ExceptionHandled := True;
        ErrorInfo.Code := 9999;
        ErrorInfo.Msg := 'Custom Message: ' + Exc.Message;
        // add a json object to the "data" field of the response
        lExtra := TJsonObject.Create;
        lExtra.S['extra'] := 'some extra data';
        ErrorInfo.Data := lExtra;
        ExceptionHandled := true;
      end
      else if Exc is EDivByZero then
      begin
        ExceptionHandled := True;
        ErrorInfo.Code := 888;
        ErrorInfo.Msg := 'Custom Message: ' + Exc.Message;
        ErrorInfo.Data := 'You cannot divide by 0';
      end
      else
      begin
        ExceptionHandled := False;
      end;
    end);

end;

procedure TMyWebModule.WebModuleDestroy(Sender: TObject);
begin
  FMVC.Free;
end;

end.
