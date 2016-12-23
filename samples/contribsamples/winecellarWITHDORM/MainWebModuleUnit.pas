unit MainWebModuleUnit;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework;

type
  Twm = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);

  private
    MVCEngine: TMVCEngine;

  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = Twm;

implementation

uses
  WineCellarAppControllerU;

{$R *.dfm}


procedure Twm.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html><heading/><body>Web Server Application</body></html>';
end;

procedure Twm.WebModuleCreate(Sender: TObject);
begin
  MVCEngine := TMVCEngine.Create(self);
  MVCEngine.AddController(TWineCellarApp);
  MVCEngine.Config['document_root'] := '..\..\www';
end;

procedure Twm.WebModuleDestroy(Sender: TObject);
begin
  MVCEngine.Free;
end;

end.
