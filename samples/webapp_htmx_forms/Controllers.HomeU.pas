// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit Controllers.HomeU;

interface

uses
  MVCFramework, MVCFramework.Commons, MVCFramework.Serializer.Commons, System.Generics.Collections;

type
  [MVCPath('/web')]
  THomeController = class(TMVCController)
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function Index: String;

    [MVCPath('/about')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function About: String;

    // HTMX fragment endpoints — return partial HTML, no full page
    [MVCPath('/fragment/clock')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function GetClockFragment: String;

    [MVCPath('/fragment/info')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function GetInfoFragment: String;

  end;

implementation

uses
  System.StrUtils, System.SysUtils, System.DateUtils, MVCFramework.Logger;

procedure THomeController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  inherited;
  // Global ViewData available to all views (used by baselayout.html)
  ViewData['app_name'] := 'WebAppHTMXForms';
  ViewData['dmvc_version'] := DMVCFRAMEWORK_VERSION;
  ViewData['current_year'] := YearOf(Now);
  ViewData['page_id'] := AActionName.ToLower;
end;

function THomeController.Index: String;
begin
  ViewData['current_date'] := FormatDateTime('dddd, dd mmmm yyyy', Now);
  ViewData['compiler_version'] := {$IF CompilerVersion >= 37}'Delphi 13 Florence'
    {$ELSEIF CompilerVersion >= 36}'Delphi 12 Athens'
    {$ELSEIF CompilerVersion >= 35}'Delphi 11 Alexandria'
    {$ELSEIF CompilerVersion >= 34}'Delphi 10.4 Sydney'
    {$ELSEIF CompilerVersion >= 33}'Delphi 10.3 Rio'
    {$ELSEIF CompilerVersion >= 32}'Delphi 10.2 Tokyo'
    {$ELSE}'Delphi'{$ENDIF};

  Result := RenderView('home/index');
end;

function THomeController.About: String;
begin
  Result := RenderView('about/index');
end;

function THomeController.GetClockFragment: String;
begin
  // Just the value: the page decides where it goes (hx-swap="innerHTML").
  Result := '<time datetime="' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now) + '">' +
    FormatDateTime('hh:nn:ss', Now) + '</time>';
end;

function THomeController.GetInfoFragment: String;
begin
  Result :=
    '<dl class="meta-list">' +
    '<dt>Application</dt><dd>WebAppHTMXForms</dd>' +
    '<dt>DMVCFramework</dt><dd>' + DMVCFRAMEWORK_VERSION + '</dd>' +
    '<dt>Compiler</dt><dd>' + Format('Delphi %.1f', [CompilerVersion], TFormatSettings.Invariant) + '</dd>' +
    '<dt>Server time</dt><dd>' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + '</dd>' +
    '</dl>';
end;


end.
