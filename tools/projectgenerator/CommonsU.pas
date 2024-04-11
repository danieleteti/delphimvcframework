unit CommonsU;

interface

uses
  MVCFramework.Commons, System.SysUtils, JsonDataObjects;

type
  IGenCommand = interface
    ['{B5F6B048-FB5A-48EA-80F9-D8395B4DE40D}']
    procedure ExecuteInterface(Section: TStringBuilder;  Model: TJSONObject);
    procedure ExecuteImplementation(Section: TStringBuilder;  Model: TJSONObject);
  end;

  TCustomCommand = class abstract(TInterfacedObject, IGenCommand)
  protected
    procedure CheckFor(const Key: String; Model: TJSONObject);
  public
    procedure ExecuteInterface(Section: TStringBuilder;  Model: TJSONObject); virtual; abstract;
    procedure ExecuteImplementation(Section: TStringBuilder;  Model: TJSONObject); virtual; abstract;
  end;

implementation

{ TCustomCommand }

procedure TCustomCommand.CheckFor(const Key: String;
  Model: TJSONObject);
begin
  if not Model.Contains(Key) then
  begin
    raise Exception.CreateFmt('Required key "%s" not found while processing %s', [Key, ClassName]);
  end;
end;

end.
