unit DMVC.Expert.CodeGen.NewWebModuleUnit;

interface

uses
  ToolsApi,
  DMVC.Expert.CodeGen.NewUnit;

type
  TNewWebModuleUnitEx = class(TNewUnit)
  private
    FUnitIdent, FFormName, FFileName : String;
  protected
    FWebModuleClassName : string;
    FControllerClassName: string;
    FControllerUnit: string;
    function GetCreatorType: string; override;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; override;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; override;
  public
    constructor Create(const aWebModuleClassName: string; aControllerClassName: string; aControllerUnit: string; const APersonality : String = '' );
  end;


implementation

uses
  Winapi.Windows,
  System.SysUtils,
  VCL.Dialogs,
  DMVC.Expert.CodeGen.Templates,
  DMVC.Expert.CodeGen.SourceFile;


constructor TNewWebModuleUnitEx.Create(const aWebModuleClassName : string; aControllerClassName: string; aControllerUnit: string; const APersonality : String = '' );
begin
  Assert(Length(aWebModuleClassName) > 0);
  FAncestorName := '';
  FFormName := '';
  FImplFileName := '';
  FIntfFileName := '';
  FWebModuleClassName := aWebModuleClassName;
  FControllerClassName := aControllerClassName;
  FControllerUnit := aControllerUnit;
  Personality := APersonality;
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName( '', FUnitIdent, FFormName, FFileName);
end;

function TNewWebModuleUnitEx.GetCreatorType: string;
begin
  Result := sForm;
end;

function TNewWebModuleUnitEx.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TSourceFile.Create(sWebModuleDFM, [FormIdent, FWebModuleClassName]);
end;

function TNewWebModuleUnitEx.NewImplSource(const ModuleIdent, FormIdent,  AncestorIdent: string): IOTAFile;
begin
  //ModuleIdent is blank for some reason.
  // http://stackoverflow.com/questions/4196412/how-do-you-retrieve-a-new-unit-name-from-delphis-open-tools-api
  // So using method mentioned by Marco Cantu.
  Result := TSourceFile.Create(sWebModuleUnit, [FUnitIdent, FWebModuleClassName, FControllerUnit, FControllerClassName]);
end;



end.
