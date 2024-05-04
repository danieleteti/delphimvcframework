unit MyControllerU;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath]
  TMyController = class(TMVCController)
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/hellos/($FirstName)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetSpecializedHello(const FirstName: string);
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  end;

implementation

procedure TMyController.Index;
begin
  // use Context property to access to the HTTP request and response
  ContentType := TMVCMediaType.TEXT_PLAIN;
  ResponseStream
    .AppendLine('WARNING!')
    .AppendLine
    ('Run this program in debug and check the Delphi "Event" debug window to see the custom logs')
    .AppendLine('Also, the log file are generated in the custom path "MyFolder\MyLogs"');
  RenderResponseStream;
end;

procedure TMyController.GetSpecializedHello(const FirstName: string);
begin
  Render('Hello ' + FirstName);
end;

procedure TMyController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
