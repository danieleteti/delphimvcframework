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


unit DMVC.Splash.Registration;

interface

uses
  Winapi.Windows;

var
  bmSplashScreen: HBITMAP;
  // iAboutPluginIndex: Integer = 0;

implementation

uses
  ToolsAPI, SysUtils, Vcl.Dialogs,
  MVCFramework.Commons;

resourcestring
  resPackageName = 'DelphiMVCFramework ' + DMVCFRAMEWORK_VERSION;
  resLicense = 'Apache License, Version 2.0';
  resAboutCopyright = 'Copyright Daniele Teti and the DMVCFramework Team';
  resAboutTitle = 'DelphiMVCFramework';
  resAboutDescription = 'https://github.com/danieleteti/delphimvcframework';

initialization

bmSplashScreen := LoadBitmap(hInstance, 'SplashScreen');
(SplashScreenServices as IOTASplashScreenServices).AddPluginBitmap(
  resPackageName,
  bmSplashScreen,
  False,
  resLicense);

finalization


// Remove Aboutbox Plugin Interface
// if iAboutPluginIndex > 0 then
// (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(iAboutPluginIndex);

end.
