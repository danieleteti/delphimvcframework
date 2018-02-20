unit WebModuleUnit1;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

  private
    MVC: TMVCEngine;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses CallbackControllerU, MVCFramework.Commons;

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>Web Server Application</body></html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC := TMVCEngine.Create(self,
    procedure(Config: TMVCConfig)
    begin
      Config[TMVCConfigKey.Messaging] := 'true';
    end);
  MVC.AddController(TCallbackController);
  MVC.Config['document_root'] := ExtractFilePath(GetModuleName(HInstance)) + '..\..\document_root';
end;

procedure TWebModule1.WebModuleDestroy(Sender: TObject);
begin
  MVC.Free;
end;

end.
