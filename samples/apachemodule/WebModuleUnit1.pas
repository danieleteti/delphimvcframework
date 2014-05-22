unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, mvcframework;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    Engine: TMVCEngine;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

uses
  Winapi.Windows, Web.ApacheHTTP;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}


function GetAllEnvVars(const Vars: TStrings): Integer;
var
  PEnvVars: PChar; // pointer to start of environment block
  PEnvEntry: PChar; // pointer to an env string in block
begin
  // Clear the list
  if Assigned(Vars) then
    Vars.Clear;
  // Get reference to environment block for this process
  PEnvVars := GetEnvironmentStrings;
  if PEnvVars <> nil then
  begin
    // We have a block: extract strings from it
    // Env strings are #0 separated and list ends with #0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        if Assigned(Vars) then
          Vars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);
      end;
      // Calculate length of block
      Result := (PEnvEntry - PEnvVars) + 1;
    finally
      // Dispose of the memory block
      FreeEnvironmentStrings(PEnvVars);
    end;
  end
  else
    // No block => zero length
    Result := 0;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  sl: TStringList;
begin
  // TApacheRequest(Request).GetFieldByName()
  // Response.ContentType := 'text/plain';
  // // Response.Content :=   GetEnvironmentVariable('HTTP_USER_AGENT');
  // sl := TStringList.Create;
  // try
  // GetAllEnvVars(sl);
  // Response.Content := sl.Text;
  // finally
  // sl.Free;
  // end;

  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body><h1>APACHE Web Server Application</h1><h3>Proudly built with Delphi XE6</h3></body>' +
    '</html>';
end;

end.
