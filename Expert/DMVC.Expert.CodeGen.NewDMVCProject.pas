unit DMVC.Expert.CodeGen.NewDMVCProject;

interface


uses
  ToolsAPI,
  DMVC.Expert.CodeGen.NewProject;

type
  TDMVCProjectFile = class(TNewProjectEx)
  protected
    function NewProjectSource(const ProjectName: string): IOTAFile; override;
    function GetFrameworkType: string; override;
  public
    constructor Create; overload;
    constructor Create(const APersonality: string); overload;
  end;

implementation

uses
  DMVC.Expert.CodeGen.SourceFile,
  DMVC.Expert.CodeGen.Templates,
  System.SysUtils;


{ TProjectFile }

constructor TDMVCProjectFile.Create;
begin
 //TODO: Figure out how to make this be DMVCProjectX where X is the next available.
 //Return Blank and the project will be 'ProjectX.dpr' where X is the next available number
  FFileName := '';
end;

constructor TDMVCProjectFile.Create(const APersonality: string);
begin
  Create;
  Personality := APersonality;
end;

function TDMVCProjectFile.GetFrameworkType: string;
begin
  Result := 'VCL';
end;

function TDMVCProjectFile.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TSourceFile.Create(sDMVCDPR,[ProjectName]);
end;

end.
