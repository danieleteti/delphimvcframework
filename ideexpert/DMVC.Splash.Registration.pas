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
//
// This IDE expert is based off of the one included with the DUnitX
// project.  Original source by Robert Love.  Adapted by Nick Hodges and Daniele Teti.
//
// The DUnitX project is run by Vincent Parrett and can be found at:
//
// https://github.com/VSoftTechnologies/DUnitX
// ***************************************************************************


unit DMVC.Splash.Registration;

interface

{$R DMVC.Splash.Resources.res}


uses
  Winapi.Windows;

procedure Register;

implementation

uses
  ToolsAPI, System.SysUtils, System.Classes,
  Vcl.Dialogs, Vcl.Imaging.PngImage, Vcl.Graphics,
  MVCFramework.Commons;

const
  {$IF CompilerVersion >= 35}
  ABOUT_RES_NAME = 'DMVCSPLASH48PNG';
  SPLASH_RES_NAME = 'DMVCSPLASH48PNG';
  {$ELSE}
  ABOUT_RES_NAME = 'DMVCPLASH24BMP';
  SPLASH_RES_NAME = 'DMVCPLASH24BMP';
  {$IFEND}

var
  iAboutPluginIndex: Integer;
  AboutBoxServices: IOTAAboutBoxServices = nil;

resourcestring
  resPackageName = 'DelphiMVCFramework ' + DMVCFRAMEWORK_VERSION;
  resLicense = 'Apache License, Version 2.0';
  resAboutCopyright = 'Copyright Daniele Teti and the DMVCFramework Team';
  resAboutTitle = 'DelphiMVCFramework';
  resAboutDescription = 'https://github.com/danieleteti/delphimvcframework';

{$IF CompilerVersion >= 35}
function CreateBitmapFromPngRes(const AResName: string): Vcl.Graphics.TBitmap;
var
  LPngImage: TPngImage;
  LResStream: TResourceStream;
begin
  LPngImage := nil;
  try
    Result := Vcl.Graphics.TBitmap.Create;
    LPngImage := TPngImage.Create;
    LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
    try
      LPngImage.LoadFromStream(LResStream);
      Result.Assign(LPngImage);
    finally
      LResStream.Free;
    end;
  finally
    LPngImage.Free;
  end;
end;

procedure RegisterAboutBox;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LBitmap := CreateBitmapFromPngRes(ABOUT_RES_NAME);
  try
    iAboutPluginIndex := AboutBoxServices.AddPluginInfo(
      resAboutTitle+' '+DMVCFRAMEWORK_VERSION,
      resAboutDescription+sLineBreak+resAboutCopyright,
      LBitmap.Handle, False, resLicense);
  finally
    LBitmap.Free;
  end;
end;

procedure UnregisterAboutBox;
begin
  if (iAboutPluginIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(iAboutPluginIndex);
    iAboutPluginIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  LBitmap := CreateBitmapFromPngRes(SPLASH_RES_NAME);
  try
    SplashScreenServices.AddPluginBitmap(
      resAboutTitle+' '+DMVCFRAMEWORK_VERSION,
      LBitmap.Handle, False, resLicense, '');
  finally
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ABOUT_RES_NAME);
  iAboutPluginIndex := AboutBoxServices.AddPluginInfo(resAboutTitle+' '+DMVCFRAMEWORK_VERSION,
    resAboutDescription+sLineBreak+resAboutCopyright, ProductImage, False, resLicense);
end;

procedure UnregisterAboutBox;
begin
  if (iAboutPluginIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(iAboutPluginIndex);
    iAboutPluginIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), SPLASH_RES_NAME);
  SplashScreenServices.AddPluginBitmap(resAboutTitle, ProductImage,
    False, resLicense);
end;
{$IFEND}

procedure Register;
begin
  RegisterWithSplashScreen;
end;

initialization
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

end.
