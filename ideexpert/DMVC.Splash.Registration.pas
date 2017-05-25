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
