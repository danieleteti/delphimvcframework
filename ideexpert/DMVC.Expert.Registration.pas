// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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
//
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************


unit DMVC.Expert.Registration;

{ .$R 'SplashScreenIcon.RES' }

interface

// Note: "Register" method name is case senstive.
procedure Register;

implementation

uses
  ToolsApi,
  DesignIntf,
  System.SysUtils,
  DMVC.Expert.ProjectWizardEx,
  DMVC.Expert.NewUnitWizardEx,
  Winapi.Windows;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  TDMVCNewProjectWizard.RegisterDMVCProjectWizard(sDelphiPersonality);
  TDMVCNewUnitWizard.RegisterDMVCNewUnitWizard(sDelphiPersonality);
end;

// procedure RegisterSplashScreen;
// var
// LBmp: Vcl.Graphics.TBitmap;
// begin
// LBmp := Vcl.Graphics.TBitmap.Create;
// LBmp.LoadFromResourceName(HInstance, 'SPLASH');
// SplashScreenServices.AddPluginBitmap(resPackageName, LBmp.Handle, False, resLicense, '');
// LBmp.Free;
// end;
//
// procedure RegisterAboutBox;
// var
// LProductImage: HBITMAP;
// begin
// Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices);
// LProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'SPLASH');
// AboutBoxIndex := AboutBoxServices.AddPluginInfo(resPackageName, resAboutDescription, LProductImage, False, resLicense);
// end;
//
// procedure UnregisterAboutBox;
// begin
// if (AboutBoxIndex = 0) and Assigned(AboutBoxServices) then
// begin
// AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
// AboutBoxIndex := 0;
// AboutBoxServices := nil;
// end;
// end;

initialization

finalization

end.
