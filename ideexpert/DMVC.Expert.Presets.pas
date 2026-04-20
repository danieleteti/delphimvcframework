// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
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

unit DMVC.Expert.Presets;

interface

uses
  DMVC.Expert.Forms.NewProjectWizard;

type
  TDMVCProjectPreset = (
    ppRESTfulAPI,
    ppWebApplication,
    ppJSONRPC,
    ppRealTime,
    ppFullStack,
    ppCustom
  );

  TDMVCProjectPresetInfo = record
    Caption: string;
    Hint: string;
    IDSuffix: string;
    IconResource: string;
  end;

const
  PRESET_INFOS: array[TDMVCProjectPreset] of TDMVCProjectPresetInfo = (
    ( // ppRESTfulAPI
      Caption: 'RESTful API';
      Hint: 'REST controller with CRUD methods, CORS, compression, service container. For mobile backends, SPAs, third-party integrations.';
      IDSuffix: '1.RestfulAPI';
      IconResource: 'DMVCPresetRestfulAPI'
    ),
    ( // ppWebApplication
      Caption: 'Web Application';
      Hint: 'TemplatePro views, static files, sessions, compression, HTMX support. For server-rendered web apps with forms and navigation.';
      IDSuffix: '2.WebApp';
      IconResource: 'DMVCPresetWebApp'
    ),
    ( // ppJSONRPC
      Caption: 'JSON-RPC Service';
      Hint: 'JSON-RPC 2.0 endpoint with sample methods, hooks, error handling, MVCDoc introspection, CORS, compression. For RPC-style APIs and internal services.';
      IDSuffix: '3.JSONRPC';
      IconResource: 'DMVCPresetJSONRPC'
    ),
    ( // ppRealTime
      Caption: 'Real-Time Application';
      Hint: 'WebSocket server with message handlers, static files, sessions, CORS, compression. For chat, live dashboards, IoT, push notifications.';
      IDSuffix: '4.RealTime';
      IconResource: 'DMVCPresetRealTime'
    ),
    ( // ppFullStack
      Caption: 'Full-Stack Application';
      Hint: 'Everything enabled: REST + TemplatePro + WebSocket + HTMX + JWT + ActiveRecord + ETag + RateLimit + Analytics + HTTPS. For complete production apps.';
      IDSuffix: '5.FullStack';
      IconResource: 'DMVCPresetFullStack'
    ),
    ( // ppCustom
      Caption: 'Custom Project';
      Hint: 'Full wizard with all configuration options. Choose exactly what you need.';
      IDSuffix: '6.Custom';
      IconResource: 'DMVCPresetCustom'
    )
  );

procedure ApplyPresetToForm(APreset: TDMVCProjectPreset; AForm: TfrmDMVCNewProject);

implementation

procedure ApplyPreset_RESTfulAPI(AForm: TfrmDMVCNewProject);
begin
  // Controller. RESTful preset always emits both sample areas:
  // - Controllers.HomeU with an "index" endpoint that sanity-checks the server
  // - Controllers.PeopleU with the CRUD sample
  AForm.edtControllerClassName.Text := 'THomeController';
  AForm.chkCreateIndexMethod.Checked := True;
  AForm.chkCreateCRUDMethods.Checked := True;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := False;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := False;
  AForm.chkWebSocketServer.Checked := False;
  AForm.cbSSV.ItemIndex := 0; // None
  AForm.chkSqids.Checked := False;
  AForm.chkHtmx.Checked := False;
  AForm.chkHtmx.Visible := False;  // HTMX only relevant with server-side views
  AForm.chkCustomConfigDotEnv.Checked := False;
  AForm.rgJWTAlgorithm.ItemIndex := 1;  // HS256 (REST APIs commonly use bearer JWT)

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := False;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 0; // None (stateless - JWT is the session)

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.cbServerEngine.ItemIndex := 1;    // Indy Direct (default for new projects)
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPreset_WebApplication(AForm: TfrmDMVCNewProject);
begin
  // Controller
  AForm.edtControllerClassName.Text := 'TWebController';
  AForm.chkCreateIndexMethod.Checked := True;
  AForm.chkCreateCRUDMethods.Checked := False;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := False;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := False;
  AForm.chkWebSocketServer.Checked := False;
  AForm.cbSSV.ItemIndex := 1; // TemplatePro
  AForm.chkSqids.Checked := False;
  AForm.chkHtmx.Checked := True;
  AForm.chkHtmx.Visible := True;  // Restore visibility (may have been hidden by another preset)
  AForm.chkCustomConfigDotEnv.Checked := False;
  AForm.rgJWTAlgorithm.ItemIndex := 0;  // None — web apps use server-side sessions

  // Middleware
  AForm.chkCORS.Checked := False;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 2; // File-based (no external dependencies, survives server restart)

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.cbServerEngine.ItemIndex := 1;    // Indy Direct (default for new projects)
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPreset_JSONRPC(AForm: TfrmDMVCNewProject);
begin
  // Controller - minimal, just for health check
  AForm.edtControllerClassName.Text := 'THealthController';
  AForm.chkCreateIndexMethod.Checked := True;
  AForm.chkCreateCRUDMethods.Checked := False;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := False;

  // Features - JSON-RPC is the main feature
  AForm.chkServicesContainer.Checked := False;
  AForm.chkJSONRPC.Checked := True;
  AForm.EdtJSONRPCClassName.Text := 'TMyJSONRPCService';
  AForm.chkWebSocketServer.Checked := False;
  AForm.cbSSV.ItemIndex := 0; // None
  AForm.chkSqids.Checked := False;
  AForm.chkHtmx.Checked := False;
  AForm.chkHtmx.Visible := False;  // HTMX only relevant with server-side views
  AForm.chkCustomConfigDotEnv.Checked := False;
  AForm.rgJWTAlgorithm.ItemIndex := 1;  // HS256 — JSON-RPC services typically need auth

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := False;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 0; // None (stateless)

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.cbServerEngine.ItemIndex := 1;    // Indy Direct (default for new projects)
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase
end;

procedure ApplyPreset_RealTime(AForm: TfrmDMVCNewProject);
begin
  // Controller
  AForm.edtControllerClassName.Text := 'TRealTimeController';
  AForm.chkCreateIndexMethod.Checked := True;
  AForm.chkCreateCRUDMethods.Checked := False;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := False;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := False;
  AForm.chkWebSocketServer.Checked := True;
  AForm.cbSSV.ItemIndex := 0; // None
  AForm.chkSqids.Checked := False;
  AForm.chkHtmx.Checked := False;
  AForm.chkHtmx.Visible := False;  // HTMX only relevant with server-side views
  AForm.chkCustomConfigDotEnv.Checked := False;
  AForm.rgJWTAlgorithm.ItemIndex := 0;  // None

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True; // HTTP endpoints still benefit from compression
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 1; // Memory

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.cbServerEngine.ItemIndex := 1;    // Indy Direct (supports WebSocket; HTTP.sys does not)
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPreset_FullStack(AForm: TfrmDMVCNewProject);
begin
  // Controller
  AForm.edtControllerClassName.Text := 'TAppController';
  AForm.chkCreateIndexMethod.Checked := True;
  AForm.chkCreateCRUDMethods.Checked := True;
  AForm.chkCreateActionFiltersMethods.Checked := True;
  AForm.chkProfileActions.Checked := True;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := False;
  AForm.chkWebSocketServer.Checked := True;
  AForm.cbSSV.ItemIndex := 1; // TemplatePro
  AForm.chkSqids.Checked := False;
  AForm.chkHtmx.Checked := True;
  AForm.chkHtmx.Visible := True;  // Restore visibility (may have been hidden by another preset)
  AForm.chkCustomConfigDotEnv.Checked := False;
  AForm.rgJWTAlgorithm.ItemIndex := 0;  // None — Full-Stack uses session-based auth

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := True;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := True;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := True;
  AForm.chkActiveRecord.Checked := True;
  AForm.cbSessionType.ItemIndex := 2; // File-based (no external dependencies, survives server restart)

  // Server
  AForm.rgServerProtocol.ItemIndex := 1; // HTTPS
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.cbServerEngine.ItemIndex := 1;    // Indy Direct (supports WebSocket + HTTPS via TaurusTLS)
  AForm.edtServerPort.Text := '443';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPresetToForm(APreset: TDMVCProjectPreset; AForm: TfrmDMVCNewProject);
begin
  case APreset of
    ppRESTfulAPI:      ApplyPreset_RESTfulAPI(AForm);
    ppWebApplication:  ApplyPreset_WebApplication(AForm);
    ppJSONRPC:         ApplyPreset_JSONRPC(AForm);
    ppRealTime:        ApplyPreset_RealTime(AForm);
    ppFullStack:       ApplyPreset_FullStack(AForm);
    ppCustom:
    begin
      // Reset visibility that may have been changed by other presets
      AForm.chkHtmx.Visible := True;
      AForm.cbServerEngine.ItemIndex := 1; // Default Indy Direct for Custom
    end;
  end;
end;

end.
