// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************

program WebSocketStockSample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  MVCFramework,
  MVCFramework.Logger,
  MVCFramework.DotEnv,
  MVCFramework.Commons,
  MVCFramework.Serializer.Commons,
  MVCFramework.WebSocket,
  MVCFramework.WebSocket.Server,
  IdContext,
  IdHTTPWebBrokerBridge,
  MVCFramework.Signal,
  JsonDataObjects,
  {$IF Defined(MSWINDOWS)}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF}
  ControllerU in 'ControllerU.pas',
  WebModuleU in 'WebModuleU.pas' {MyWebModule: TWebModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LServer: TIdHTTPWebBrokerBridge;
  lWSServer: TMVCWebSocketServer;
  LProtocol: String;
begin
  LProtocol := 'http';
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.OnParseAuthentication := TMVCParseAuthentication.OnParseAuthentication;
    LServer.DefaultPort := APort;
    LServer.KeepAlive := dotEnv.Env('dmvc.indy.keep_alive', True);
    LServer.MaxConnections := dotEnv.Env('dmvc.webbroker.max_connections', 0);
    LServer.ListenQueue := dotEnv.Env('dmvc.indy.listen_queue', 500);
    LServer.Active := True;
    LogI('Listening on ' + LProtocol + '://localhost:' + APort.ToString);

    LWSServer := TMVCWebSocketServer.Create(9091);
    try
      LWSServer.OnLog := procedure(const AMessage: string)
      begin
        LogI(Format('[%s] %s', [TimeToStr(Now), AMessage]));
      end;

      LWSServer.OnClientConnect := procedure(AClient: TWebSocketClient)
      var
        StockListJSON: string;
      begin
        LogI(Format('[%s] CLIENT CONNECTED: %s', [TimeToStr(Now), AClient.ClientId]));

        // Send available stocks list immediately on connect
        AClient.PeriodicInterval := 1000; // Update interval in milliseconds

        StockListJSON := '{"type":"stocklist","updateInterval":' + IntToStr(AClient.PeriodicInterval) + ',"stocks":[' +
          '{"symbol":"GOOG","name":"Google","color":"#fbbf24"},' +
          '{"symbol":"AAPL","name":"Apple","color":"#ef4444"},' +
          '{"symbol":"MSFT","name":"Microsoft","color":"#10b981"},' +
          '{"symbol":"AMZN","name":"Amazon","color":"#f59e0b"},' +
          '{"symbol":"TSLA","name":"Tesla","color":"#8b5cf6"},' +
          '{"symbol":"META","name":"Meta","color":"#ec4899"},' +
          '{"symbol":"NVDA","name":"NVIDIA","color":"#14b8a6"}' +
          ']}';
        AClient.SendText(StockListJSON);
        LogI(Format('[%s] Sent stock list to %s (update interval: %dms)', [TimeToStr(Now), AClient.ClientId, AClient.PeriodicInterval]));
      end;

      LWSServer.OnClientDisconnect := procedure(AClient: TWebSocketClient)
      begin
        LogI(Format('[%s] CLIENT DISCONNECTED: %s', [TimeToStr(Now), AClient.ClientId]));
      end;

      LWSServer.OnMessage := procedure(AClient: TWebSocketClient; const AMessage: string)
      var
        JSON: TJSONObject;
        SubscribedStocks: string;
      begin
        LogI(Format('[%s] Message from %s: %s', [TimeToStr(Now), AClient.ClientId, AMessage]));

        // Try to parse as JSON for subscription message
        try
          JSON := StrToJSONObject(AMessage);
          try
            if JSON.S['type'] = 'subscribe' then
            begin
              // Store subscribed stocks in client data
              SubscribedStocks := JSON.A['symbols'].ToJSON;
              AClient.Data := TJSONObject.Create;
              (AClient.Data as TJSONObject).S['subscribed'] := SubscribedStocks;
              LogI(Format('[%s] Client %s subscribed to: %s', [TimeToStr(Now), AClient.ClientId, SubscribedStocks]));
            end
            else
            begin
              // Echo back the message
              AClient.SendText(Format('Echo: %s', [AMessage]));
            end;
          finally
            JSON.Free;
          end;
        except
          // Not JSON, echo back (it's a demo...)
          AClient.SendText(Format('Echo: %s', [AMessage]));
        end;
      end;

      LWSServer.OnError := procedure(AClient: TWebSocketClient; const AError: string)
      begin
        LogI(Format('[%s] ERROR from %s: %s', [TimeToStr(Now), AClient.ClientId, AError]));
      end;

      LWSServer.OnPeriodicMessage := procedure(AClient: TWebSocketClient; out AMessage: String)
      var
        GOOG, AAPL, MSFT, AMZN, TSLA, META, NVDA: Double;
        FS: TFormatSettings;
        JSONResult: TJSONObject;
        SubscribedJSON: TJSONArray;
        I: Integer;
        Symbol: string;
        ClientData: TJSONObject;
      begin
        AMessage := '';

        // Check if client has subscribed stocks
        if not Assigned(AClient.Data) then
          Exit;

        ClientData := AClient.Data as TJSONObject;
        if not ClientData.Contains('subscribed') then
          Exit;

        // Use invariant format settings (dot as decimal separator for JSON)
        FS := TFormatSettings.Create('en-US');

        // Simulate stock prices with random variations (larger amplitude for visual effect)
        GOOG := 2800 + Random(300) - 150;
        AAPL := 180 + Random(60) - 30;
        MSFT := 420 + Random(100) - 50;
        AMZN := 3500 + Random(400) - 200;
        TSLA := 250 + Random(120) - 60;
        META := 500 + Random(150) - 75;
        NVDA := 900 + Random(250) - 125;

        // Build JSON with only subscribed stocks
        JSONResult := TJSONObject.Create;
        try
          JSONResult.S['type'] := 'stocks';

          SubscribedJSON := TJSONObject.Parse(ClientData.S['subscribed']) as TJSONArray;
          try
            for I := 0 to SubscribedJSON.Count - 1 do
            begin
              Symbol := SubscribedJSON.S[I];
              if Symbol = 'GOOG' then JSONResult.F['GOOG'] := GOOG
              else if Symbol = 'AAPL' then JSONResult.F['AAPL'] := AAPL
              else if Symbol = 'MSFT' then JSONResult.F['MSFT'] := MSFT
              else if Symbol = 'AMZN' then JSONResult.F['AMZN'] := AMZN
              else if Symbol = 'TSLA' then JSONResult.F['TSLA'] := TSLA
              else if Symbol = 'META' then JSONResult.F['META'] := META
              else if Symbol = 'NVDA' then JSONResult.F['NVDA'] := NVDA;
            end;
          finally
            SubscribedJSON.Free;
          end;

          AMessage := JSONResult.ToJSON;
        finally
          JSONResult.Free;
        end;

        LogI(Format('[%s] Sent stock data to %s', [TimeToStr(Now), AClient.ClientId]));
      end;

      LWSServer.Active := True;
      LogI('Listening on ws://localhost:' + lWSServer.DefaultPort.ToString);
      {$IF Defined(MSWINDOWS)}
      ShellExecute(0, 'open', PChar(LProtocol + '://localhost:' + APort.ToString + '/static'), nil, nil, SW_SHOW);
      {$ENDIF}

      WaitForTerminationSignal;
      EnterInShutdownState;


      LogI('Stopping server...');

    finally
      lWSServer.Free;
    end;

    LogI('Application started. Press Ctrl+C to shut down.');
    LServer.Active := False;
  finally
    LServer.Free;
  end;
end;

begin
  { Enable ReportMemoryLeaksOnShutdown during debug }
  // ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;

  // DMVCFramework Specific Configurations
  //   When MVCSerializeNulls = True empty nullables and nil are serialized as json null.
  //   When MVCSerializeNulls = False empty nullables and nil are not serialized at all.
  MVCSerializeNulls := True;

  // MVCNameCaseDefault defines the name case of property names generated by the serializers.
  //   Possibile values are: ncAsIs, ncUpperCase, ncLowerCase (default), ncCamelCase, ncPascalCase, ncSnakeCase
  MVCNameCaseDefault := TMVCNameCase.ncLowerCase;

  // UseConsoleLogger defines if logs must be emitted to also the console (if available).
  UseConsoleLogger := True;

  // UseLoggerVerbosityLevel defines the lowest level of logs that will be produced.
  UseLoggerVerbosityLevel := TLogLevel.levNormal;


  LogI('** DMVCFramework Server ** build ' + DMVCFRAMEWORK_VERSION);

  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;

    WebRequestHandlerProc.MaxConnections := dotEnv.Env('dmvc.handler.max_connections', 1024);

{$IF CompilerVersion >= 34} //SYDNEY+
    if dotEnv.Env('dmvc.profiler.enabled', false) then
    begin
      Profiler.ProfileLogger := Log;
      Profiler.WarningThreshold := dotEnv.Env('dmvc.profiler.warning_threshold', 1000);
      Profiler.LogsOnlyIfOverThreshold := dotEnv.Env('dmvc.profiler.logs_only_over_threshold', True);
    end;
{$ENDIF}

    RunServer(dotEnv.Env('dmvc.server.port', 8080));
  except
    on E: Exception do
      LogF(E.ClassName + ': ' + E.Message);
  end;
end.
