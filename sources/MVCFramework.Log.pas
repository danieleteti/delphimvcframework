unit MVCFramework.Log;

interface

uses
  MVCFramework.Router,
  Web.HTTPApp;

type
  TLogActionProc = reference to procedure(const ARouter: TMVCRouter; const ARequest: TWebRequest;
    const AResponse: TWebResponse);
  TLogNoActionProc = reference to procedure(const ARequest: TWebRequest; const AResponse: TWebResponse);

  TMVCFrameworkLog = class
  private class var
    FLogActionProc: TLogActionProc;
    FLogNoActionProc: TLogNoActionProc;
  public
    class constructor Create;
    class procedure LogAction(const ARouter: TMVCRouter; const ARequest: TWebRequest;
      const AResponse: TWebResponse);
    class procedure LogNoAction(const ARequest: TWebRequest; const AResponse: TWebResponse);
    class procedure SetLogActionProc(const ALogActionProc: TLogActionProc);
    class procedure SetLogNoActionProc(const ALogNoActionProc: TLogNoActionProc);
  end;

implementation

uses
  MVCFramework.Logger,
  System.SysUtils;

procedure LogAction(const ARouter: TMVCRouter; const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  Log(TLogLevel.levNormal, ARequest.Method + ':' + ARequest.RawPathInfo + ' -> ' +
    ARouter.ControllerClazz.QualifiedClassName + ' - ' + IntToStr(AResponse.StatusCode) + ' ' +
    AResponse.ReasonString);
end;

procedure LogNoAction(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  Log(TLogLevel.levNormal, ARequest.Method + ':' + ARequest.RawPathInfo + ' -> NO ACTION ' + ' - ' +
    IntToStr(AResponse.StatusCode) + ' ' + AResponse.ReasonString);
end;

{ TMVCFrameworkLog }

class constructor TMVCFrameworkLog.Create;
begin
  // Default log actions
  SetLogActionProc(
    procedure(const ARouter: TMVCRouter; const ARequest: TWebRequest; const AResponse: TWebResponse)
    begin
      Log(TLogLevel.levNormal, ARequest.Method + ':' + ARequest.RawPathInfo + ' -> ' +
        ARouter.ControllerClazz.QualifiedClassName + ' - ' + IntToStr(AResponse.StatusCode) + ' ' +
        AResponse.ReasonString)
    end);
  SetLogNoActionProc(
    procedure(const ARequest: TWebRequest; const AResponse: TWebResponse)
    begin
      Log(TLogLevel.levNormal, ARequest.Method + ':' + ARequest.RawPathInfo + ' -> NO ACTION ' + ' - ' +
        IntToStr(AResponse.StatusCode) + ' ' + AResponse.ReasonString);
    end);
end;

class procedure TMVCFrameworkLog.LogAction(const ARouter: TMVCRouter; const ARequest: TWebRequest;
  const AResponse: TWebResponse);
begin
  FLogActionProc(ARouter, ARequest, AResponse);
end;

class procedure TMVCFrameworkLog.LogNoAction(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  FLogNoActionProc(ARequest, AResponse);
end;

class procedure TMVCFrameworkLog.SetLogActionProc(const ALogActionProc: TLogActionProc);
begin
  Assert(Assigned(ALogActionProc));
  FLogActionProc := ALogActionProc;
end;

class procedure TMVCFrameworkLog.SetLogNoActionProc(const ALogNoActionProc: TLogNoActionProc);
begin
  Assert(Assigned(ALogNoActionProc));
  FLogNoActionProc := ALogNoActionProc;
end;

end.
