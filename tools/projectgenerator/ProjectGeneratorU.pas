unit ProjectGeneratorU;

interface

uses
  MVCFramework.Commons, System.SysUtils, System.Generics.Collections,
  JsonDataObjects, DMVC.Expert.Commons;

type
  TMVCCodeGenerator = class
  private
    fIntf: TStringBuilder;
    fImpl: TStringBuilder;
    fCommands: TList<IGenCommand>;
    fSource: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute(Model: TJSONObject);
    function Commands: TList<IGenCommand>;
    function Source: String;
    class function GenerateSource(ConfigModelRef: TJSONObject; FillerProc: TProc<TMVCCodeGenerator>): String;
  end;


implementation

function TMVCCodeGenerator.Commands: TList<IGenCommand>;
begin
  Result := fCommands;
end;

constructor TMVCCodeGenerator.Create;
begin
  inherited;
  fCommands := TList<IGenCommand>.Create;
  fSource := '';
end;

destructor TMVCCodeGenerator.Destroy;
begin
  fCommands.Free;
  inherited;
end;

{ TMVCProjectGenerator }

procedure TMVCCodeGenerator.Execute(Model: TJSONObject);
var
  I: Integer;
begin
  fSource := '';
  fIntf := TStringBuilder.Create;
  try
    fImpl := TStringBuilder.Create;
    try
      for I := 0 to fCommands.Count - 1 do
      begin
        fCommands[I].ExecuteInterface(fIntf, Model);
        fCommands[I].ExecuteImplementation(fImpl, Model);
      end;
      fSource := fIntf.ToString + fImpl.ToString;
    finally
      fImpl.Free;
    end;
  finally
    fIntf.Free;
  end;
end;

class function TMVCCodeGenerator.GenerateSource(ConfigModelRef: TJSONObject;
  FillerProc: TProc<TMVCCodeGenerator>): String;
var
  lGenerator: TMVCCodeGenerator;
begin
  lGenerator := TMVCCodeGenerator.Create;
  try
    lGenerator.Commands.Clear;
    FillerProc(lGenerator);
    lGenerator.Execute(ConfigModelRef);
    Result := lGenerator.Source;
  finally
    lGenerator.Free;
  end;
end;

{ TIntfCommand }

function TMVCCodeGenerator.Source: String;
begin
  Result := fSource;
end;

end.
