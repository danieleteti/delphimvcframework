unit WebModuleUnit;

interface

uses System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleDestroy(Sender: TObject);
    procedure WebModuleCreate(Sender: TObject);

  private
    MVCEngine: TMVCEngine;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

{$R *.dfm}

uses TestServerControllerU;

procedure Twm.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html><heading/><body>Web Server Application</body></html>';
end;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine.AddController(TTestServerController);
  MVCEngine.Config['stompserver'] := 'localhost';
  MVCEngine.Config['stompserverport'] := '61613';
  MVCEngine.Config['stompusername'] := 'guest';
  MVCEngine.Config['stomppassword'] := 'guest';
end;

procedure Twm.WebModuleDestroy(Sender: TObject);
begin
  MVCEngine.Free;
end;

end.
