unit DMVC.Expert.CodeGen.NewProject;
// This is done to Warnings that I can't control, as Embarcadero has
// deprecated the functions, but due to design you are still required to
// to implement.
{$WARN SYMBOL_DEPRECATED OFF}
interface

uses
  PlatformAPI,
  ToolsAPI;

type
   TNewProject = class abstract(TNotifierObject,IOTACreator, IOTAProjectCreator,IOTAProjectCreator80)
   protected
    //IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string; deprecated;
    function GetShowSource: Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const ProjectName: string): IOTAFile; deprecated;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile; virtual; abstract;  // MUST OVERRIDE!
    // IOTAProjectCreator80
    function GetProjectPersonality: string;virtual;
    procedure NewDefaultProjectModule(const Project: IOTAProject);
  private
    procedure SetFileName(const Value: String);
  protected
    FFileName : String;
  public
     property FileName : String read GetFileName write SetFileName;
  end;

  TNewProjectEx = class(TNewProject, IOTAProjectCreator160)
  private
   FPersonality: string;
  protected
    function GetProjectPersonality: string;override;

    // IOTAProjectCreator160
    function GetPlatforms: TArray<string>;
    function GetFrameworkType: string; virtual;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  public
    property Personality : string read FPersonality write FPersonality;
  end;

implementation

uses
  System.SysUtils;


{ TNewProject }

function TNewProject.GetCreatorType: string;
begin
  Result := sConsole; // May want to change this in the future, at least making method virtual
end;

function TNewProject.GetExisting: Boolean;
begin
  Result := False;
end;

function TNewProject.GetFileName: string;
begin
  Result := FFileName;
end;

function TNewProject.GetFileSystem: string;
begin
  Result := '';
end;

function TNewProject.GetOptionFileName: string;
begin
  Result := '';
end;

function TNewProject.GetOwner: IOTAModule;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
end;

function TNewProject.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TNewProject.GetShowSource: Boolean;
begin
  Result := False;
end;

function TNewProject.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TNewProject.NewDefaultModule;
begin
end;

procedure TNewProject.NewDefaultProjectModule(const Project: IOTAProject);
begin
end;

function TNewProject.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TNewProject.NewProjectResource(const Project: IOTAProject);
begin
end;


procedure TNewProject.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

function TNewProjectEx.GetFrameworkType: string;
begin
  Result := '';
end;

function TNewProjectEx.GetPlatforms: TArray<string>;
begin
  Result := TArray<string>.Create(cWin32Platform, cWin64Platform);
end;

function TNewProjectEx.GetPreferredPlatform: string;
begin
  Result := '';
end;

function TNewProjectEx.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality
end;

procedure TNewProjectEx.SetInitialOptions(const NewProject: IOTAProject);
var
  LBuildConf: IOTAProjectOptionsConfigurations;
begin
  if Supports(NewProject.ProjectOptions, IOTAProjectOptionsConfigurations, LBuildConf) then
  begin
    LBuildConf.BaseConfiguration.AsBoolean['UsingDelphiRTL'] := True;
  end;

end;

end.
