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
    ppMicroservice,
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
      Hint: 'JSON API for mobile apps, SPAs, or third-party integrations';
      IDSuffix: '1.RestfulAPI';
      IconResource: 'DMVCPresetRestfulAPI'
    ),
    ( // ppWebApplication
      Caption: 'Web Application';
      Hint: 'Server-side rendered web app with HTML templates and sessions';
      IDSuffix: '2.WebApp';
      IconResource: 'DMVCPresetWebApp'
    ),
    ( // ppMicroservice
      Caption: 'Microservice';
      Hint: 'Lightweight stateless service for distributed architectures';
      IDSuffix: '3.Microservice';
      IconResource: 'DMVCPresetMicroservice'
    ),
    ( // ppRealTime
      Caption: 'Real-Time Application';
      Hint: 'WebSocket server for chat, live dashboards, IoT';
      IDSuffix: '4.RealTime';
      IconResource: 'DMVCPresetRealTime'
    ),
    ( // ppFullStack
      Caption: 'Full-Stack Application';
      Hint: 'Complete production app with all features enabled';
      IDSuffix: '5.FullStack';
      IconResource: 'DMVCPresetFullStack'
    ),
    ( // ppCustom
      Caption: 'Custom Project';
      Hint: 'Full wizard with all configuration options';
      IDSuffix: '6.Custom';
      IconResource: 'DMVCPresetCustom'
    )
  );

procedure ApplyPresetToForm(APreset: TDMVCProjectPreset; AForm: TfrmDMVCNewProject);

implementation

procedure ApplyPreset_RESTfulAPI(AForm: TfrmDMVCNewProject);
begin
  // Controller
  AForm.edtControllerClassName.Text := 'TAPIController';
  AForm.chkCreateIndexMethod.Checked := False;
  AForm.chkCreateCRUDMethods.Checked := True;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := False;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := False;
  AForm.chkWebSocketServer.Checked := False;
  AForm.cbSSV.ItemIndex := 0; // None
  AForm.chkSqids.Checked := False;
  AForm.chkMSHeap.Checked := False;
  AForm.chkCustomConfigDotEnv.Checked := False;

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := False;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkJWT.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 0; // None

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
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
  AForm.chkMSHeap.Checked := False;
  AForm.chkCustomConfigDotEnv.Checked := False;

  // Middleware
  AForm.chkCORS.Checked := False;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkJWT.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 1; // Memory

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPreset_Microservice(AForm: TfrmDMVCNewProject);
begin
  // Controller
  AForm.edtControllerClassName.Text := 'TServiceController';
  AForm.chkCreateIndexMethod.Checked := True; // Health check
  AForm.chkCreateCRUDMethods.Checked := False;
  AForm.chkCreateActionFiltersMethods.Checked := False;
  AForm.chkProfileActions.Checked := True;

  // Features
  AForm.chkServicesContainer.Checked := True;
  AForm.chkJSONRPC.Checked := True;
  AForm.chkWebSocketServer.Checked := False;
  AForm.cbSSV.ItemIndex := 0; // None
  AForm.chkSqids.Checked := False;
  AForm.chkMSHeap.Checked := False;
  AForm.chkCustomConfigDotEnv.Checked := False;

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := False;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkJWT.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 0; // None (stateless)

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.edtServerPort.Text := '8080';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
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
  AForm.chkMSHeap.Checked := False;
  AForm.chkCustomConfigDotEnv.Checked := False;

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True; // HTTP endpoints still benefit from compression
  AForm.chkETAG.Checked := False;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := False;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := False;
  AForm.chkJWT.Checked := False;
  AForm.chkActiveRecord.Checked := False;
  AForm.cbSessionType.ItemIndex := 1; // Memory

  // Server
  AForm.rgServerProtocol.ItemIndex := 0; // HTTP
  AForm.rgApplicationType.ItemIndex := 0; // Console
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
  AForm.chkMSHeap.Checked := False;
  AForm.chkCustomConfigDotEnv.Checked := False;

  // Middleware
  AForm.chkCORS.Checked := True;
  AForm.chkCompression.Checked := True;
  AForm.chkETAG.Checked := True;
  AForm.chkStaticFiles.Checked := True;
  AForm.chkAnalyticsMiddleware.Checked := True;
  AForm.chkTrace.Checked := False;
  AForm.chkRateLimit.Checked := True;
  AForm.chkJWT.Checked := True;
  AForm.chkActiveRecord.Checked := True;
  AForm.cbSessionType.ItemIndex := 3; // Database

  // Server
  AForm.rgServerProtocol.ItemIndex := 1; // HTTPS
  AForm.rgApplicationType.ItemIndex := 0; // Console
  AForm.edtServerPort.Text := '443';

  // Serializer
  AForm.cbNameCase.ItemIndex := 3; // CamelCase (0=AsIs, 1=UpperCase, 2=LowerCase, 3=CamelCase, 4=PascalCase, 5=SnakeCase)
end;

procedure ApplyPresetToForm(APreset: TDMVCProjectPreset; AForm: TfrmDMVCNewProject);
begin
  case APreset of
    ppRESTfulAPI:      ApplyPreset_RESTfulAPI(AForm);
    ppWebApplication:  ApplyPreset_WebApplication(AForm);
    ppMicroservice:    ApplyPreset_Microservice(AForm);
    ppRealTime:        ApplyPreset_RealTime(AForm);
    ppFullStack:       ApplyPreset_FullStack(AForm);
    ppCustom:          ; // No changes - use default form state
  end;
end;

end.
