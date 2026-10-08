// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit DMVC.Expert.ProjectItems;

// What the Project Manager "DMVCFramework" menu adds to an existing project:
// the new units and views, and where a controller or a route group is wired in.
// No ToolsAPI here: the menu applies the edits to the IDE buffer, the template
// test suite applies them to generated projects and compiles the result.

interface

type
  TDMVCCodeEdit = record
    Offset: Integer; // 0-based, in characters of the source string
    Text: string;
  end;

  TDMVCCodeEdits = TArray<TDMVCCodeEdit>;

  /// What ActiveRecord needs for one database: the FireDAC driver unit and DriverID
  /// (FireDAC.Stan.Consts S_FD_*Id), the SQL generator unit (sources/MVCFramework.SQLGenerators.*),
  /// and which connection parameters the driver has (FireDAC.Phys.*Def)
  TDMVCDatabase = record
    Caption: string;
    PhysUnit: string;
    DriverID: string;
    GeneratorUnit: string;
    HasServer, HasPort: Boolean;
    DefaultPort, DefaultDatabase, DefaultUser: string;
    function IsSQLite: Boolean;
  end;

  TDMVCNewUnit = record
    UnitName: string;
    TypeName: string;  // controller class, or the MapXxxRoutes procedure
    FileName: string;  // UnitName + '.pas'
    Source: string;
  end;

/// The wizard's Minimal API projects call ConfigureRoutes from the .dpr; the
/// controller projects never do. Decides which menu items make sense.
function IsMinimalAPIProject(const ADprSource: string): Boolean;

/// A Pascal identifier, used to build unit, class and procedure names
function IsValidItemName(const AName: string): Boolean;
/// A relative URL path or view name: letters, digits, '-', '_', '/'; no '..'
function IsValidPathName(const APath: string): Boolean;

/// The model class the controller or route group binds bodies to: 'TOrder' for 'Orders'.
/// A guess (plain English plurals), the dialog lets the user change it.
function DefaultModelClass(const AName: string): string;

/// ASource sets up the OpenAPI document new items of that kind show up in: the Swagger
/// middleware for controllers, MVCFramework.OpenAPI3 for the Minimal API.
function HasOpenAPI(const ASource: string; AMinimalAPI: Boolean): Boolean;

/// The controller uses AModelClass from AModelUnit and never touches its fields, so an
/// existing model of any shape works. AWithService: the service of NewService(AName, ...)
/// is injected and every action calls it.
function NewRestController(const AName, AResource, AModelClass, AModelUnit: string;
  ACrud, AOpenAPI: Boolean; AWithService: Boolean = False): TDMVCNewUnit;
/// Services.<AName>U: I<AName>Service and T<AName>Service (TypeName). ACrud: CRUD methods
/// over AModelClass (from AModelUnit), implemented with ActiveRecord when AActiveRecord,
/// stubs otherwise; without ACrud a single example method.
function NewService(const AName, AModelClass, AModelUnit: string; ACrud, AActiveRecord: Boolean): TDMVCNewUnit;
function ServiceInterfaceName(const AName: string): string;
/// Where a new model class goes: Entities.OrderU for TOrder
function ModelUnitName(const AModelClass: string): string;
/// Entities.<Model>U with AModelClass: a TMVCActiveRecord on ATableName when AActiveRecord,
/// a plain class otherwise. Both have ID and a required Name.
function NewModel(const AName, AModelClass, ATableName: string; AActiveRecord: Boolean): TDMVCNewUnit;
/// ASource declares AClassName as a class (not a forward declaration, not a class reference);
/// AIsActiveRecord when it descends directly from TMVCActiveRecord
function DeclaresClass(const ASource, AClassName: string; out AIsActiveRecord: Boolean): Boolean;
/// The databases "Add Database Connection" offers, in menu order
function Databases: TArray<TDMVCDatabase>;
/// FDConnectionConfigU: CON_DEF_NAME and SetupDatabaseConnection, with the two units ADatabase needs
function NewDatabaseConfig(const ADatabase: TDMVCDatabase; const AConDefName: string): TDMVCNewUnit;
/// The db.* keys SetupDatabaseConnection reads, for the .env
function DatabaseEnvLines(const ADatabase: TDMVCDatabase): string;
/// The controller renders AResource + '/index' (see NewViewSource)
function NewWebController(const AName, AResource, AProgramName: string): TDMVCNewUnit;
/// ACallFmt is the argument PlanRoutesRegistration expects
function NewRoutesUnit(const AName, AResource, AModelClass: string; ACrud, AOpenAPI: Boolean;
  out ACallFmt: string): TDMVCNewUnit;
/// AViewName relative to the views folder, without extension (e.g. 'orders/index')
function NewViewSource(const AViewName, ATitle: string; AFragment: Boolean): string;

/// Adds AUnitName to the implementation uses clause (creating one if needed).
/// Nothing to add (the unit is already used) still returns True.
function PlanUsesInsertion(const ASource, AUnitName: string; var AEdits: TDMVCCodeEdits): Boolean;

/// AddController(AClassName) next to the existing ones: before the scaffold's
/// "// Controllers - END" marker, or after the last AddController call.
function PlanControllerRegistration(const ASource, AUnitName, AClassName: string;
  out AEdits: TDMVCCodeEdits): Boolean;

/// A call at the end of ConfigureRoutes. ACallFmt receives the name of the
/// ConfigureRoutes group parameter, e.g. 'MapOrdersRoutes(%s.Prefix(''/api/orders''))'.
function PlanRoutesRegistration(const ASource, AUnitName, ACallFmt: string;
  out AEdits: TDMVCCodeEdits): Boolean;

/// RegisterType(AClassName, AInterfaceName, SingletonPerRequest) in RegisterServices:
/// before the scaffold's "// Register other services here", or after the last
/// RegisterType, or at the end of the body.
function PlanServiceRegistration(const ASource, AUnitName, AClassName, AInterfaceName: string;
  out AEdits: TDMVCCodeEdits): Boolean;
/// The line PlanServiceRegistration adds, for the manual step
function ServiceRegistrationLine(const AReceiver, AClassName, AInterfaceName: string): string;

/// SetupDatabaseConnection and the ActiveRecord middleware, after the last AddController.
/// Refused when the source already adds a TMVCActiveRecordMiddleware: one connection per project.
function PlanDatabaseRegistration(const ASource, AUnitName: string; out AEdits: TDMVCCodeEdits): Boolean;
/// The lines PlanDatabaseRegistration adds, for the manual step
function DatabaseRegistrationLines(const AReceiver: string): string;

function ApplyEdits(const ASource: string; const AEdits: TDMVCCodeEdits): string;

implementation

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.RegularExpressions,
  JsonDataObjects,
  DMVC.Expert.ProjectGenerator;

function IsMinimalAPIProject(const ADprSource: string): Boolean;
begin
  Result := TRegEx.IsMatch(ADprSource, '(?i)\bConfigureRoutes\s*\(');
end;

function IsValidItemName(const AName: string): Boolean;
begin
  Result := TRegEx.IsMatch(AName, '^[A-Za-z][A-Za-z0-9_]*$');
end;

function IsValidPathName(const APath: string): Boolean;
begin
  Result := TRegEx.IsMatch(APath, '^[A-Za-z0-9_-]+(/[A-Za-z0-9_-]+)*$');
end;

function DefaultModelClass(const AName: string): string;
begin
  if AName.EndsWith('ies', True) and (AName.Length > 3) then
    Result := AName.Substring(0, AName.Length - 3) + 'y'
  else if AName.EndsWith('s', True) and not AName.EndsWith('ss', True) and (AName.Length > 1) then
    Result := AName.Substring(0, AName.Length - 1)
  else
    Result := AName + 'Item';
  Result := 'T' + Result;
end;

function HasOpenAPI(const ASource: string; AMinimalAPI: Boolean): Boolean;
begin
  if AMinimalAPI then
    Result := ASource.Contains('MVCFramework.OpenAPI3')
  else
    Result := ASource.Contains('MVCFramework.Middleware.Swagger');
end;

// what the OpenAPI metadata of a new item needs
procedure SetOpenAPIData(const AConfig: TJsonObject; const AName, AModelClass: string);
begin
  AConfig.S['tag'] := AName;
  if AModelClass.StartsWith('T') and (AModelClass.Length > 1) then
    AConfig.S['model_title'] := AModelClass.Substring(1)
  else
    AConfig.S['model_title'] := AModelClass;
end;

function RenderUnit(const ATemplate, AUnitName, ATypeName: string;
  const AConfig: TJsonObject): TDMVCNewUnit;
begin
  AConfig.S['unit_name'] := AUnitName;
  Result.UnitName := AUnitName;
  Result.TypeName := ATypeName;
  Result.FileName := AUnitName + '.pas';
  Result.Source := TDMVCProjectGenerator.RenderTemplate(ATemplate, AConfig);
end;

function ServiceInterfaceName(const AName: string): string;
begin
  Result := 'I' + AName + 'Service';
end;

function NewRestController(const AName, AResource, AModelClass, AModelUnit: string;
  ACrud, AOpenAPI: Boolean; AWithService: Boolean): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['class_name'] := 'T' + AName + 'Controller';
    lConfig.S['model_class'] := AModelClass;
    lConfig.S['model_unit_name'] := AModelUnit;
    lConfig.B['openapi_swagger'] := AOpenAPI;
    SetOpenAPIData(lConfig, AName, AModelClass);
    lConfig.S['resource'] := AResource;
    lConfig.B['crud'] := ACrud;
    lConfig.B['with_service'] := AWithService;
    lConfig.S['service_unit_name'] := 'Services.' + AName + 'U';
    lConfig.S['service_interface_name'] := ServiceInterfaceName(AName);
    Result := RenderUnit('add_controller_rest.pas.tpro', 'Controllers.' + AName + 'U',
      lConfig.S['class_name'], lConfig);
  finally
    lConfig.Free;
  end;
end;

{ TDMVCDatabase }

function TDMVCDatabase.IsSQLite: Boolean;
begin
  Result := DriverID = 'SQLite';
end;

function Database(const ACaption, APhysUnit, ADriverID, AGenerator: string; AHasServer, AHasPort: Boolean;
  const ADefaultPort, ADefaultDatabase, ADefaultUser: string): TDMVCDatabase;
begin
  Result.Caption := ACaption;
  Result.PhysUnit := APhysUnit;
  Result.DriverID := ADriverID;
  Result.GeneratorUnit := AGenerator;
  Result.HasServer := AHasServer;
  Result.HasPort := AHasPort;
  Result.DefaultPort := ADefaultPort;
  Result.DefaultDatabase := ADefaultDatabase;
  Result.DefaultUser := ADefaultUser;
end;

function Databases: TArray<TDMVCDatabase>;
begin
  Result := [
    Database('PostgreSQL', 'FireDAC.Phys.PG', 'PG', 'MVCFramework.SQLGenerators.PostgreSQL',
      True, True, '5432', 'mydb', 'postgres'),
    Database('Firebird', 'FireDAC.Phys.FB', 'FB', 'MVCFramework.SQLGenerators.Firebird',
      True, True, '3050', 'C:\data\mydb.fdb', 'SYSDBA'),
    Database('InterBase', 'FireDAC.Phys.IB', 'IB', 'MVCFramework.SQLGenerators.Interbase',
      True, True, '3050', 'C:\data\mydb.ib', 'SYSDBA'),
    // MariaDB too: it connects through the MySQL driver and ActiveRecord sees it as 'mysql'
    Database('MySQL / MariaDB', 'FireDAC.Phys.MySQL', 'MySQL', 'MVCFramework.SQLGenerators.MySQL',
      True, True, '3306', 'mydb', 'root'),
    // no Port parameter: a non-default port goes in Server as host,port
    Database('SQL Server', 'FireDAC.Phys.MSSQL', 'MSSQL', 'MVCFramework.SQLGenerators.MSSQL',
      True, False, '', 'mydb', 'sa'),
    // no Server nor Port: Database is the connect string (EZConnect host:port/service)
    Database('Oracle', 'FireDAC.Phys.Oracle', 'Ora', 'MVCFramework.SQLGenerators.Oracle',
      False, False, '', 'localhost:1521/XEPDB1', 'system'),
    Database('SQLite', 'FireDAC.Phys.SQLite', 'SQLite', 'MVCFramework.SQLGenerators.Sqlite',
      False, False, '', 'data\app.db', '')
  ];
end;

function NewDatabaseConfig(const ADatabase: TDMVCDatabase; const AConDefName: string): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['con_def_name'] := AConDefName;
    lConfig.S['db_caption'] := ADatabase.Caption;
    lConfig.S['db_phys_unit'] := ADatabase.PhysUnit;
    lConfig.S['db_driver_id'] := ADatabase.DriverID;
    lConfig.S['db_generator_unit'] := ADatabase.GeneratorUnit;
    lConfig.B['db_sqlite'] := ADatabase.IsSQLite;
    lConfig.B['db_server'] := ADatabase.HasServer;
    lConfig.B['db_port'] := ADatabase.HasPort;
    lConfig.S['db_default_port'] := ADatabase.DefaultPort;
    lConfig.S['db_default_database'] := ADatabase.DefaultDatabase;
    lConfig.S['db_default_user'] := ADatabase.DefaultUser;
    Result := RenderUnit('add_database.pas.tpro', 'FDConnectionConfigU', 'SetupDatabaseConnection', lConfig);
  finally
    lConfig.Free;
  end;
end;

function DatabaseEnvLines(const ADatabase: TDMVCDatabase): string;
begin
  Result := sLineBreak + '# Database (' + ADatabase.Caption + '), read by FDConnectionConfigU' + sLineBreak;
  if ADatabase.HasServer then
    Result := Result + 'db.server=localhost' + sLineBreak;
  if ADatabase.HasPort then
    Result := Result + 'db.port=' + ADatabase.DefaultPort + sLineBreak;
  Result := Result + 'db.database=' + ADatabase.DefaultDatabase + sLineBreak;
  if not ADatabase.IsSQLite then
    Result := Result + 'db.user=' + ADatabase.DefaultUser + sLineBreak + 'db.password=' + sLineBreak;
  Result := Result + 'db.pool_max=50' + sLineBreak;
end;

function NewService(const AName, AModelClass, AModelUnit: string; ACrud, AActiveRecord: Boolean): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['class_name'] := 'T' + AName + 'Service';
    lConfig.S['interface_name'] := ServiceInterfaceName(AName);
    lConfig.S['model_class'] := AModelClass;
    lConfig.S['model_unit_name'] := AModelUnit;
    lConfig.S['tag'] := AName;
    lConfig.B['crud'] := ACrud;
    lConfig.B['model_ar'] := AActiveRecord;
    Result := RenderUnit('add_service.pas.tpro', 'Services.' + AName + 'U', lConfig.S['class_name'], lConfig);
  finally
    lConfig.Free;
  end;
end;

function ModelUnitName(const AModelClass: string): string;
begin
  if AModelClass.StartsWith('T') and (AModelClass.Length > 1) then
    Result := 'Entities.' + AModelClass.Substring(1) + 'U'
  else
    Result := 'Entities.' + AModelClass + 'U';
end;

function NewModel(const AName, AModelClass, ATableName: string; AActiveRecord: Boolean): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['model_class'] := AModelClass;
    lConfig.S['table_name'] := ATableName;
    lConfig.S['tag'] := AName;
    lConfig.B['model_ar'] := AActiveRecord;
    Result := RenderUnit('add_model.pas.tpro', ModelUnitName(AModelClass), AModelClass, lConfig);
  finally
    lConfig.Free;
  end;
end;

function DeclaresClass(const ASource, AClassName: string; out AIsActiveRecord: Boolean): Boolean;
var
  lMatch: TMatch;
begin
  // "TOrder = class", "TOrder = class(TBase)", attributes before it on their own line;
  // not "TOrder = class;" (forward) nor "TOrderClass = class of TOrder"
  lMatch := TRegEx.Match(ASource, '(?im)^\s*' + TRegEx.Escape(AClassName) +
    '\s*=\s*class\b(?!\s*;)(?!\s+of\b)\s*(\(\s*([\w.]+))?');
  Result := lMatch.Success;
  // the ancestor group is there only when the declaration names one
  AIsActiveRecord := Result and (lMatch.Groups.Count > 2) and
    SameText(lMatch.Groups[2].Value, 'TMVCActiveRecord');
end;

function NewWebController(const AName, AResource, AProgramName: string): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['class_name'] := 'T' + AName + 'Controller';
    lConfig.S['resource'] := AResource;
    lConfig.S['view_name'] := AResource + '/index';
    lConfig.S['program_name'] := AProgramName;
    Result := RenderUnit('add_controller_web.pas.tpro', 'Controllers.' + AName + 'U',
      lConfig.S['class_name'], lConfig);
  finally
    lConfig.Free;
  end;
end;

function NewRoutesUnit(const AName, AResource, AModelClass: string; ACrud, AOpenAPI: Boolean;
  out ACallFmt: string): TDMVCNewUnit;
var
  lConfig: TJsonObject;
begin
  lConfig := TJsonObject.Create;
  try
    lConfig.S['procedure_name'] := 'Map' + AName + 'Routes';
    lConfig.S['model_class'] := AModelClass;
    lConfig.B['openapi_native'] := AOpenAPI;
    SetOpenAPIData(lConfig, AName, AModelClass);
    lConfig.S['resource'] := AResource;
    lConfig.B['crud'] := ACrud;
    Result := RenderUnit('add_routes.pas.tpro', AName + 'RoutesU', lConfig.S['procedure_name'], lConfig);
  finally
    lConfig.Free;
  end;
  // %s is the ConfigureRoutes group parameter; quotes doubled for Format
  ACallFmt := Result.TypeName + '(%s.Prefix(''/api/' + AResource + '''))';
end;

function NewViewSource(const AViewName, ATitle: string; AFragment: Boolean): string;
var
  lLayout: string;
  I: Integer;
begin
  if AFragment then
    Exit(TDMVCProjectGenerator.LoadTemplate('views\add_view_fragment.tpro')
      .Replace('{{:view_id}}', AViewName.Replace('/', '-')));
  // extends is relative to the view's own folder
  lLayout := 'baselayout.html';
  for I := 1 to AViewName.CountChar('/') do
    lLayout := '../' + lLayout;
  Result := TDMVCProjectGenerator.LoadTemplate('views\add_view_page.tpro')
    .Replace('{{:layout_path}}', lLayout)
    .Replace('{{:view_title}}', ATitle);
end;

function LineBreakOf(const ASource: string): string;
begin
  if ASource.Contains(#13#10) then
    Result := #13#10
  else
    Result := #10;
end;

procedure AddEdit(var AEdits: TDMVCCodeEdits; AOffset: Integer; const AText: string);
var
  lEdit: TDMVCCodeEdit;
begin
  lEdit.Offset := AOffset;
  lEdit.Text := AText;
  AEdits := AEdits + [lEdit];
end;

// 0-based offset of the start of the line after the one containing AOffset
function NextLineStart(const ASource: string; AOffset: Integer): Integer;
begin
  Result := ASource.IndexOf(#10, AOffset);
  if Result < 0 then
    Result := ASource.Length
  else
    Inc(Result);
end;

function PlanUsesInsertion(const ASource, AUnitName: string; var AEdits: TDMVCCodeEdits): Boolean;
var
  lNL: string;
  lImpl, lUses, lStop: TMatch;
  lAfterUses: Integer;
begin
  lNL := LineBreakOf(ASource);
  if TRegEx.IsMatch(ASource, '(?i)\b' + TRegEx.Escape(AUnitName) + '\s*[,;]') then
    Exit(True);
  lImpl := TRegEx.Match(ASource, '(?im)^implementation\b');
  if not lImpl.Success then
    Exit(False);
  // a uses clause belongs to the implementation section only if nothing else comes first
  lUses := TRegEx.Match(ASource.Substring(lImpl.Index - 1), '(?im)^\s*uses\b');
  lStop := TRegEx.Match(ASource.Substring(lImpl.Index - 1),
    '(?im)^\s*(type|var|const|procedure|function|constructor|destructor|class|begin|initialization|end\.)\b');
  if lUses.Success and ((not lStop.Success) or (lUses.Index < lStop.Index)) then
  begin
    lAfterUses := lImpl.Index - 1 + lUses.Index - 1 + lUses.Length;
    if ASource.Substring(lAfterUses).TrimLeft([' ', #9]).StartsWith(#13) or
       ASource.Substring(lAfterUses).TrimLeft([' ', #9]).StartsWith(#10) then
      AddEdit(AEdits, NextLineStart(ASource, lAfterUses), '  ' + AUnitName + ',' + lNL)
    else
      AddEdit(AEdits, lAfterUses, ' ' + AUnitName + ',');
  end
  else
    AddEdit(AEdits, NextLineStart(ASource, lImpl.Index - 1),
      lNL + 'uses' + lNL + '  ' + AUnitName + ';' + lNL);
  Result := True;
end;

function PlanControllerRegistration(const ASource, AUnitName, AClassName: string;
  out AEdits: TDMVCCodeEdits): Boolean;
var
  lNL: string;
  lCalls: TMatchCollection;
  lLast, lMarker: TMatch;
  lIndent, lReceiver: string;
begin
  AEdits := [];
  lNL := LineBreakOf(ASource);
  lCalls := TRegEx.Matches(ASource, '(?im)^([ \t]*)([\w.]+)\.AddController\s*\(');
  if lCalls.Count = 0 then
    Exit(False);
  lLast := lCalls[lCalls.Count - 1];
  lIndent := lLast.Groups[1].Value;
  lReceiver := lLast.Groups[2].Value;
  if TRegEx.IsMatch(ASource, '(?i)\.AddController\s*\(\s*' + TRegEx.Escape(AClassName) + '\s*[,)]') then
    Exit(False); // already registered: leave the code alone
  if not PlanUsesInsertion(ASource, AUnitName, AEdits) then
    Exit(False);
  lMarker := TRegEx.Match(ASource, '(?im)^[ \t]*//\s*Controllers\s*-\s*END\b');
  if lMarker.Success and (lMarker.Index > lLast.Index) then
    AddEdit(AEdits, lMarker.Index - 1,
      lIndent + lReceiver + '.AddController(' + AClassName + ');' + lNL)
  else
    AddEdit(AEdits, NextLineStart(ASource, lLast.Index - 1),
      lIndent + lReceiver + '.AddController(' + AClassName + ');' + lNL);
  Result := True;
end;

function PlanRoutesRegistration(const ASource, AUnitName, ACallFmt: string;
  out AEdits: TDMVCCodeEdits): Boolean;
var
  lNL: string;
  lHeaders: TMatchCollection;
  lHeader, lEnd: TMatch;
  lGroupParam: string;
  lBodyStart: Integer;
begin
  AEdits := [];
  lNL := LineBreakOf(ASource);
  // the last header is the implementation; the first one may be the interface declaration
  lHeaders := TRegEx.Matches(ASource,
    '(?im)^procedure\s+ConfigureRoutes\s*\(\s*(const\s+|var\s+)?(\w+)\s*:');
  if lHeaders.Count = 0 then
    Exit(False);
  lHeader := lHeaders[lHeaders.Count - 1];
  lGroupParam := lHeader.Groups[2].Value;
  lBodyStart := lHeader.Index - 1 + lHeader.Length;
  // a routine's own "end;" is the first one at column 0
  lEnd := TRegEx.Match(ASource.Substring(lBodyStart), '(?m)^end;');
  if not lEnd.Success then
    Exit(False);
  if not PlanUsesInsertion(ASource, AUnitName, AEdits) then
    Exit(False);
  AddEdit(AEdits, lBodyStart + lEnd.Index - 1, '  ' + Format(ACallFmt, [lGroupParam]) + ';' + lNL);
  Result := True;
end;

function ServiceRegistrationLine(const AReceiver, AClassName, AInterfaceName: string): string;
begin
  Result := AReceiver + '.RegisterType(' + AClassName + ', ' + AInterfaceName +
    ', TRegistrationType.SingletonPerRequest);';
end;

function PlanServiceRegistration(const ASource, AUnitName, AClassName, AInterfaceName: string;
  out AEdits: TDMVCCodeEdits): Boolean;
var
  lNL, lBody, lIndent, lReceiver: string;
  lHeaders, lCalls: TMatchCollection;
  lHeader, lEnd, lMarker, lLast: TMatch;
  lBodyStart, lStatementEnd: Integer;
begin
  AEdits := [];
  lNL := LineBreakOf(ASource);
  // the last header is the implementation; the first one may be the interface declaration
  lHeaders := TRegEx.Matches(ASource,
    '(?im)^procedure\s+RegisterServices\s*\(\s*(const\s+|var\s+)?(\w+)\s*:');
  if lHeaders.Count = 0 then
    Exit(False);
  if TRegEx.IsMatch(ASource, '(?i)\.RegisterType\s*\(\s*' + TRegEx.Escape(AClassName) + '\s*,') then
    Exit(False); // already registered: leave the code alone
  lHeader := lHeaders[lHeaders.Count - 1];
  lReceiver := lHeader.Groups[2].Value;
  lBodyStart := lHeader.Index - 1 + lHeader.Length;
  // a routine's own "end;" is the first one at column 0
  lEnd := TRegEx.Match(ASource.Substring(lBodyStart), '(?m)^end;');
  if not lEnd.Success then
    Exit(False);
  lBody := ASource.Substring(lBodyStart, lEnd.Index - 1);
  if not PlanUsesInsertion(ASource, AUnitName, AEdits) then
    Exit(False);
  lIndent := '  ';
  lCalls := TRegEx.Matches(lBody, '(?im)^([ \t]*)\w+\.RegisterType\s*\(');
  if lCalls.Count > 0 then
    lIndent := lCalls[0].Groups[1].Value;
  lMarker := TRegEx.Match(lBody, '(?im)^[ \t]*//\s*Register other services here');
  if lMarker.Success then
    AddEdit(AEdits, lBodyStart + lMarker.Index - 1,
      lIndent + ServiceRegistrationLine(lReceiver, AClassName, AInterfaceName) + lNL)
  else if lCalls.Count > 0 then
  begin
    // after the statement, which may span several lines
    lLast := lCalls[lCalls.Count - 1];
    lStatementEnd := lBody.IndexOf(';', lLast.Index - 1);
    if lStatementEnd < 0 then
    begin
      AEdits := []; // WireIn applies whatever is planned: no half edit
      Exit(False);
    end;
    AddEdit(AEdits, NextLineStart(ASource, lBodyStart + lStatementEnd),
      lIndent + ServiceRegistrationLine(lReceiver, AClassName, AInterfaceName) + lNL);
  end
  else
    AddEdit(AEdits, lBodyStart + lEnd.Index - 1,
      lIndent + ServiceRegistrationLine(lReceiver, AClassName, AInterfaceName) + lNL);
  Result := True;
end;

function DatabaseRegistrationLines(const AReceiver: string): string;
begin
  Result := 'SetupDatabaseConnection;' + sLineBreak +
    AReceiver + '.AddMiddleware(TMVCActiveRecordMiddleware.Create(CON_DEF_NAME));';
end;

function PlanDatabaseRegistration(const ASource, AUnitName: string; out AEdits: TDMVCCodeEdits): Boolean;
var
  lNL, lIndent, lReceiver: string;
  lCalls: TMatchCollection;
  lLast, lMarker: TMatch;
  lOffset: Integer;
  lMissing, lUnit: string;
begin
  AEdits := [];
  lNL := LineBreakOf(ASource);
  if ASource.Contains('TMVCActiveRecordMiddleware') then
    Exit(False); // the project has its connection already: leave the code alone
  lCalls := TRegEx.Matches(ASource, '(?im)^([ \t]*)([\w.]+)\.AddController\s*\(');
  if lCalls.Count = 0 then
    Exit(False);
  lLast := lCalls[lCalls.Count - 1];
  lIndent := lLast.Groups[1].Value;
  lReceiver := lLast.Groups[2].Value;
  // one insertion for both: two would create two uses clauses where there is none
  lMissing := '';
  for lUnit in ['MVCFramework.Middleware.ActiveRecord', AUnitName] do
    if not TRegEx.IsMatch(ASource, '(?i)\b' + TRegEx.Escape(lUnit) + '\s*[,;]') then
      if lMissing = '' then
        lMissing := lUnit
      else
        lMissing := lMissing + ', ' + lUnit;
  if (lMissing <> '') and not PlanUsesInsertion(ASource, lMissing, AEdits) then
  begin
    AEdits := [];
    Exit(False);
  end;
  // the connection def exists before the first request; the middleware binds it per request.
  // After the scaffold's "// Controllers - END", or after the last AddController.
  lMarker := TRegEx.Match(ASource, '(?im)^[ \t]*//\s*Controllers\s*-\s*END\b');
  if lMarker.Success and (lMarker.Index > lLast.Index) then
    lOffset := NextLineStart(ASource, lMarker.Index - 1)
  else
    lOffset := NextLineStart(ASource, lLast.Index - 1);
  AddEdit(AEdits, lOffset,
    lIndent + '// ActiveRecord: the connection of FDConnectionConfigU, one per request' + lNL +
    lIndent + 'SetupDatabaseConnection;' + lNL +
    lIndent + lReceiver + '.AddMiddleware(TMVCActiveRecordMiddleware.Create(CON_DEF_NAME));' + lNL);
  Result := True;
end;

function ApplyEdits(const ASource: string; const AEdits: TDMVCCodeEdits): string;
var
  lSorted: TDMVCCodeEdits;
  I: Integer;
begin
  lSorted := Copy(AEdits);
  TArray.Sort<TDMVCCodeEdit>(lSorted, TComparer<TDMVCCodeEdit>.Construct(
    function(const L, R: TDMVCCodeEdit): Integer
    begin
      Result := R.Offset - L.Offset; // from the end, so earlier offsets stay valid
    end));
  Result := ASource;
  for I := 0 to High(lSorted) do
    Result := Result.Insert(lSorted[I].Offset, lSorted[I].Text);
end;

end.
