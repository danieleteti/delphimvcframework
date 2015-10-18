program serversideviews;
{$APPTYPE CONSOLE}


uses
	System.SysUtils,
	Winapi.Windows,
	Winapi.ShellAPI,
	IdHTTPWebBrokerBridge,
	Web.WebReq,
	Web.WebBroker,
	WebModuleU in 'WebModuleU.pas' {WebModule1: TWebModule} ,
	SampleControllerU in 'SampleControllerU.pas';

{$R *.res}


procedure RunServer(APort: Integer);
var
	LInputRecord: TInputRecord;
	LEvent: DWord;
	LHandle: THandle;
	LServer: TIdHTTPWebBrokerBridge;
begin
{$WARN SYMBOL_PLATFORM OFF}
	Writeln(Format('Starting HTTP Server or port %d', [APort]));
	LServer := TIdHTTPWebBrokerBridge.Create(nil);
	try
		LServer.DefaultPort := APort;
		LServer.Active := True;
		ShellExecute(0, PChar('open'), PChar('http://localhost:8080/index.html'),
			nil, nil, SW_SHOWMAXIMIZED);
		Writeln('Press ESC to stop the server');
		LHandle := GetStdHandle(STD_INPUT_HANDLE);
		while True do
		begin
			Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
			if (LInputRecord.EventType = KEY_EVENT) and
				LInputRecord.Event.KeyEvent.bKeyDown and
				(LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
				break;
		end;
	finally
		LServer.Free;
	end;
{$WARN SYMBOL_PLATFORM ON}
end;

begin
	ReportMemoryLeaksOnShutdown := True;
	try
		if WebRequestHandler <> nil then
			WebRequestHandler.WebModuleClass := WebModuleClass;
		RunServer(8080);
	except
		on E: Exception do
			Writeln(E.ClassName, ': ', E.Message);
	end

end.
