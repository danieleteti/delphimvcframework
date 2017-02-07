unit JSONSampleController;

interface

uses
  MVCFramework, MVCFramework.Commons;

type

  [MVCPath('/')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index(ctx: TWebContext);
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext;
      const AActionName: string); override;
  end;

implementation

uses
  System.Classes, System.JSON.Writers, System.JSON.Types;

procedure TMyController.Index(ctx: TWebContext);
var
  StringWriter: TStringWriter;
  Writer: TJSONTextWriter;
  oUser: String;
  Arr: TArray<String>;
begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter);
  try
    Writer.Formatting := TJsonFormatting.Indented;
    Writer.WriteStartObject;
    Writer.WritePropertyName('Users');
    Writer.WriteStartArray;
    Arr := ['Daniele','Peter','Scott'];
    for oUser in Arr do
    begin
      Writer.WriteStartObject;
      Writer.WritePropertyName('UserName');
      Writer.WriteValue(oUser);
      Writer.WriteEndObject;
    end;
    Writer.WriteEndArray;
    Writer.WriteEndObject;
    Render(StringWriter, False);
  finally
    Writer.Free;
    StringWriter.Free;
  end;
end;

procedure TMyController.OnAfterAction(Context: TWebContext;
  const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMyController.OnBeforeAction(Context: TWebContext;
  const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }
  inherited;
end;

end.
