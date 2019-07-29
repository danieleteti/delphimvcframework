unit mwAnalytics;

interface

uses
  MVCFramework, MVCFramework.Logger, System.Classes;

type
  IFileAnalytics = interface
    ['{C77426C4-1D34-4B3A-BD21-3C07B7E0B8BD}']
    function GetRecentlyUsedAPI() : string;
    function GetConsumeAPICount(ControllerName, ActionName : string) : Integer;
  end;

  TMVCAnalyticsMiddleware = class(TInterfacedObject, IMVCMiddleware, IFileAnalytics)
  private
    FAnalyticsFileName: string;
    FAnalyticsFile: TStringList;
    procedure SetAnalyticsFilename(const Value: string);
  protected
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnAfterControllerAction(Context: TWebContext; const AActionNAme: string;
      const Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext;
      const AControllerQualifiedClassName: string; const AActionNAme: string; var Handled: Boolean);
  public
    property AnalyticsFilename: string read FAnalyticsFileName write SetAnalyticsFilename;

    //Own Function
    function GetRecentlyUsedAPI : string;
    function GetConsumeAPICount(ControllerName, ActionName : string) : Integer;
  end;


implementation

uses
  System.SysUtils, System.JSON, REST.Json;

{ TMVCAnalyticsMiddleware }

function TMVCAnalyticsMiddleware.GetConsumeAPICount(ControllerName,
  ActionName: string): Integer;
begin
  Result := 5;
end;

function TMVCAnalyticsMiddleware.GetRecentlyUsedAPI: string;
begin
  Result := 'Test'
end;

procedure TMVCAnalyticsMiddleware.OnAfterControllerAction(Context: TWebContext;
  const AActionNAme: string; const Handled: Boolean);
begin
  //Context.Response.Content := Context.Response.Content + '[][][]' ;
end;

procedure TMVCAnalyticsMiddleware.OnBeforeControllerAction(Context: TWebContext;
  const AControllerQualifiedClassName, AActionNAme: string;
  var Handled: Boolean);
var
  Data    : TJSONObject;
  CSVFile : TextFile;
  ApplicationPath : string;
begin
  {AnalyticsFilename := 'analytics.json';

  if not Assigned(FAnalyticsFile) then
    raise Exception.Create('Analytics: Cannot read or write to file.');

  Data := TJSONObject.Create;
  Data.AddPair('TimeStamp', DateTimeToStr(Now));
  Data.AddPair('IPAddress', Context.Request.ClientIp);
  Data.AddPair('ControllerName', AControllerQualifiedClassName);
  Data.AddPair('ActionName', AActionNAme);
  FAnalyticsFile.Add(Data.ToJSON);
  FAnalyticsFile.SaveToFile(FAnalyticsFileName);
  Data.Free;}
  //Context.Request;
  ApplicationPath := ExtractFilePath(GetModuleName(hInstance));
  AssignFile(CSVFile, ApplicationPath+'analytics.csv');
  if not FileExists('analytics.csv') then
  begin
    Rewrite(CSVFile);
    WriteLn(CSVFile, 'DateTime, IpAddress, ControllerName, ActionName, DomainName, Host');
  end;
  Append(CSVFile);
  WriteLn(CSVFile, DateTimeToStr(Now), ',', Context.Request.ClientIp, ',', AControllerQualifiedClassName, ',',AActionNAme, ',' , Context.Request.RawWebRequest.Referer, ',', Context.Request.RawWebRequest.Host);
  CloseFile(CSVFile);
end;

procedure TMVCAnalyticsMiddleware.OnBeforeRouting(Context: TWebContext;
  var Handled: Boolean);
begin

end;

procedure TMVCAnalyticsMiddleware.SetAnalyticsFilename(const Value: string);
begin
  // create the class if not there
  if not Assigned(FAnalyticsFile) then
    FAnalyticsFile := TStringList.Create;

  // Create the file if it doesn't exist
  if not FileExists(Value) then
    FAnalyticsFile.SaveToFile(Value)
  else
    FAnalyticsFile.LoadFromFile(Value);

  // assign the name
  FAnalyticsFileName := Value;
end;


end.
