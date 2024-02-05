// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// Collaborators on this file: David Moorhouse (info@moorhouse.net.nz)
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
// *************************************************************************** }

unit MVCFramework.HTMX;

{$I dmvcframework.inc}

interface

uses
  MVCFramework, MVCFramework.Commons, System.Rtti, JsonDataObjects, System.StrUtils;

type
  THTMXRequestHeaderType = record
  public const
    CurrentUrl = 'HX-Current-URL';
    HistoryRestoreRequest = 'HX-History-Restore-Request';
    Prompt = 'HX-Prompt';
    Request = 'HX-Request';
    Target = 'HX-Target';
    TriggerName = 'HX-Trigger-Name';
    Trigger = 'HX-Trigger';
    Boosted = 'HX-Boosted';
    TriggeringEvent = 'Triggering-Event';
  end;

  ///<summary>Helper class to expose HTMX headers as native functions on WebRequest objects</summary>
  THTMXRequestHelper = class helper for TMVCWebRequest
  private
    function GetHtmxHeader(Header: string): string;
    function GetHtmxHeaderToBool(Header: string): Boolean;
    function HasHeader(Header: string): Boolean;
  public
    /// <summary>Indicates that the request is triggered by HTMX.</summary>
    function IsHTMX: Boolean;

    /// <summary>Indicates that the request is via an element using hx-boost.</summary>
    function HXIsBoosted: Boolean;

    /// <summary>True if the request is for history restoration after a miss in the local history cache</summary>
    function HXIsHistoryRestoreRequest: Boolean;

    /// <summary>The current URL of the browser.</summary>
    function HXGetCurrentUrl: string;

    /// <summary>The user response to an hx-prompt.</summary>
    function HXGetPrompt: string;

    /// <summary>The id of the target element if it exists.</summary>
    function HXGetTarget: string;

    /// <summary>The id of the triggered element if it exists.</summary>
    function HXGetTrigger: string;

    /// <summary>The name of the triggered element if it exists.</summary>
    function HXGetTriggerName: string;

    /// <summary>The value of the header is a JSON serialized</summary>
    /// <remarks>Requires the event-header extension to be installed and loaded on the page </remarks>
    /// <see>https://htmx.org/extensions/event-header/ </see>
    function HXGetTriggeringEvent: TArray<string>;

    /// <summary>The value of the header is a JSON serialized</summary>
    /// <remarks>Requires the event-header extension to be installed and loaded on the page </remarks>
    /// <see>https://htmx.org/extensions/event-header/ </see>
    function HXGetTriggeringEventAsJSON: TJsonObject;
  end;

  THTMXResponseHeaderType = record
  public const
    Location = 'HX-Location';
    Refresh = 'HX-Refresh';
    PushURL = 'HX-Push-Url';
    Redirect = 'HX-Redirect';
    ReplaceURL = 'HX-Replace-Url';
    Reselect = 'HX-Reselect';
    Reswap = 'HX-Reswap';
    Retarget = 'HX-Retarget';
    Trigger = 'HX-Trigger';
    TriggerAfterSettle = 'HX-Trigger-After-Settle';
    TriggerAfterSwap = 'HX-Trigger-After-Swap';
  end;


  ///<summary>Helper class to expose HTMX options as native functions on WebResponse objects</summary>
  THTMXResponseHelper = class helper for TMVCWebResponse
  public type
    TClientEventType = (etReceived, etSettled, etSwapped);
    TSwapOption = (soInnerHTML, soOuterHTML, soBeforeBegin, soAfterBegin, soBeforeEnd, soAfterEnd, soDelete, soNone);
    TShowScrollType = (ssNone, ssShow, ssScroll);
    TSwapScrollTo = (sstTop, sstBottom);
  private const
    ClientEventTypes: array [TClientEventType] of string = (THTMXResponseHeaderType.Trigger,
      THTMXResponseHeaderType.TriggerAfterSettle, THTMXResponseHeaderType.TriggerAfterSwap);
    SwapOptions: array [TSwapOption] of string = ('innerHTML', 'outerHTML', 'beforebegin', 'afterbegin', 'beforeend', 'afterend',
      'delete', 'none');
    ShowScrollTypes: array [TShowScrollType] of string = ('', 'show', 'scroll');
    SwapScrollTo: array [TSwapScrollTo] of string = ('top', 'bottom');
  public
    /// <summary> Pushes a new url into the browser history history.</summary>
    /// <remarks>This creates a new history entry, allowing navigation with the browser�s back and forward buttons.
    /// This is similar to the hx-push-url attribute.
    /// If present, this header overrides any behavior defined with attributes.</remarks>
    /// <param name="URL">A URL to be pushed into the location bar.
    /// This may be relative or absolute, as per history.pushState().
    /// If omitted, the header will output "false", which prevents the browser�s history from being updated.</param>
    function HXSetPushUrl(URL: string = ''): TMVCWebResponse;

    /// <summary>Replaces the current URL in the browser location history.</summary>
    /// <remarks>This does not create a new history entry; in effect, it removes the previous current URL from the browser�s history.
    /// This is similar to the hx-replace-url attribute.
    /// If present, this header overrides any behavior defined with attributes.</remarks>
    /// <param name="URL">A URL to replace the current URL in the location bar.
    /// This may be relative or absolute, as per history.replaceState(), but must have the same origin as the current URL.
    /// If omitted, the header will output "false", which prevents the browser�s current URL from being updated.</param>
    function HXSetReplaceUrl(URL: string = ''): TMVCWebResponse;

    /// <summary>Allows you to specify how the response will be swapped. See hx-swap for possible values</summary>
    /// Check if transition: true works ?
    function HXSetReswap(Option: TSwapOption): TMVCWebResponse; overload;

    /// <summary>Allows you to specify how the response will be swapped. See hx-swap for possible values</summary>
    /// <remarks>You can modify the timing of the browser update to synchronize htmx with the timing of CSS transition effects.</remarks>
    /// <param name="SwapDelay">The amount of time that htmx will wait after receiving a response to swap the content (in milliseconds) default is 0</param>
    /// <param name="SettleDelay">The amount of time between the swap and the settle logic(in milliseconds) default is 20mS</param>
    function HXSetReswap(Option: TSwapOption; SwapDelay: Integer; SettleDelay: Integer = 20): TMVCWebResponse; overload;

    /// <summary>Allows you to specify how the response will be swapped. See hx-swap for possible values</summary>
    /// <remarks>You can modify nature of the browser update to show or scroll to the top or bottom of a target.</remarks>
    /// <param name="Option">The target for the swap</param>
    /// <param name="ShowScroll">Whether to set the display to the target, or to scroll to the target</param>
    /// <param name="To">Either top or bottom</param>
    /// <param name="Selector">Allows targetting of a different element for scrolling or showing</param>
    function HXSetReswap(Option: TSwapOption; ShowScroll: TShowScrollType; &To: TSwapScrollTo; Selector: string = '')
      : TMVCWebResponse; overload;

    /// <summary>A CSS selector that updates the target of the content to a different element on the page</summary>
    function HXSetRetarget(Selector: string): TMVCWebResponse;

    /// <summary>Allows you to trigger a client side event.</summary>
    /// <remarks>Using events gives you a lot of flexibility to add functionality to normal htmx responses.</remarks>
    /// <param name="Name">The name of the javscript event to be triggered</param>
    /// <param name="After">The timing of the event</param>
    function HXTriggerClientEvent(Name: string; After: TClientEventType = etReceived): TMVCWebResponse; overload;

    /// <summary>Allows you to trigger a collection of client side events.</summary>
    /// <remarks>Using events gives you a lot of flexibility to add functionality to normal htmx responses.</remarks>
    /// <param name="Names">A collection of the names of the javscript events to be triggered</param>
    /// <param name="After">The timing of the event</param>
    function HXTriggerClientEvents(Names: TArray<string>; After: TClientEventType = etReceived): TMVCWebResponse; overload;

    /// <summary>Allows you to trigger a collection of client side events.</summary>
    /// <remarks>Using events gives you a lot of flexibility to add functionality to normal htmx responses.</remarks>
    /// <param name="EventsDescriptors">A JSON object with events descriptors (https://htmx.org/headers/hx-trigger/)</param>
    function HXTriggerClientEvents(EventsDescriptors: TJSONObject; After: TClientEventType = etReceived): TMVCWebResponse; overload;

    /// <summary>Allows you to trigger a client side event with parameters.</summary>
    /// <remarks>Using events gives you a lot of flexibility to add functionality to normal htmx responses.</remarks>
    /// <param name="Name">The name of the javscript event to be triggered</param>
    /// <param name="Params">An object containing the parameters to be sent to the event</param>
    /// <param name="After">The timing of the event</param>
    function HXTriggerClientEvent(Name: string; Params: TValue; After: TClientEventType = etReceived): TMVCWebResponse; overload;

    /// <summary>if set to �true� the client side will do a a full refresh of the page</summary>
    function HXSetPageRefresh(Refresh: Boolean = true): TMVCWebResponse;

    /// <summary>Allows you to do a client-side redirect that does not do a full page reload</summary>
    /// <remarks>Instead of changing the page�s location it will act like following a hx-boost link, creating a new history entry,
    /// issuing an ajax request to the value of the header and pushing the path into history. </remarks>
    function HXSetLocation(Path: string): TMVCWebResponse; overload;

    /// <summary>Used to do a client-side redirect to a new location</summary>
    function HXSetRedirect(Path: string): TMVCWebResponse;

    /// <summary>Sends an error response bcack to client.</summary>
    function HXSetErrorResponse(ErrorCode: Integer; ErrorMessage: string): TMVCWebResponse;

    /// <summary>A CSS selector that allows you to choose which part of the response is used to be swapped in.</summary>
    /// <remarks> Overrides an existing hx-select on the triggering element</remarks>
    /// <param name="Selector">A CSS selector </param>
    function HXSetReSelect(Selector: string): TMVCWebResponse;
  end;

implementation

uses
  System.SysUtils, MVCFramework.Utils, MVCFramework.Serializer.JsonDataObjects, MVCFramework.Serializer.Commons;

{ THTMXRequestHelper }

function THTMXRequestHelper.HXGetCurrentUrl: string;
begin
  Result := GetHtmxHeader(THTMXRequestHeaderType.CurrentUrl);
end;

function THTMXRequestHelper.GetHtmxHeader(Header: string): string;
begin
  Result := Headers[Header];
end;

function THTMXRequestHelper.GetHtmxHeaderToBool(Header: string): Boolean;
begin
  Result := SameText('true', Headers[Header]);
end;

function THTMXRequestHelper.HXGetPrompt: string;
begin
  Result := GetHtmxHeader(THTMXRequestHeaderType.Prompt);
end;

function THTMXRequestHelper.HXGetTarget: string;
begin
  Result := GetHtmxHeader(THTMXRequestHeaderType.Target);
end;

function THTMXRequestHelper.HXGetTrigger: string;
begin
  Result := GetHtmxHeader(THTMXRequestHeaderType.Trigger);
end;

function THTMXRequestHelper.HXGetTriggeringEvent: TArray<string>;
begin
  Result := nil;
  if HasHeader(THTMXRequestHeaderType.TriggeringEvent) then
    Result := GetHtmxHeader(THTMXRequestHeaderType.TriggeringEvent).Split([',']);
end;

function THTMXRequestHelper.HXGetTriggeringEventAsJSON: TJsonObject;
begin
  Result := nil;
  if HasHeader(THTMXRequestHeaderType.TriggeringEvent) then
    Result := TJsonBaseObject.Parse(GetHtmxHeader(THTMXRequestHeaderType.TriggeringEvent)) as TJsonObject;
end;

function THTMXRequestHelper.HXGetTriggerName: string;
begin
  Result := GetHtmxHeader(THTMXRequestHeaderType.TriggerName);
end;

function THTMXRequestHelper.HasHeader(Header: string): Boolean;
begin
  Result := not Headers[Header].IsEmpty;
end;

function THTMXRequestHelper.HXIsBoosted: Boolean;
begin
  Result := GetHtmxHeaderToBool(THTMXRequestHeaderType.Boosted);
end;

function THTMXRequestHelper.HXIsHistoryRestoreRequest: Boolean;
begin
  Result := GetHtmxHeaderToBool(THTMXRequestHeaderType.HistoryRestoreRequest);
end;

function THTMXRequestHelper.IsHTMX: Boolean;
begin
  Result := GetHtmxHeaderToBool(THTMXRequestHeaderType.Request);
end;

{ THTMXResponseHelper }

function THTMXResponseHelper.HXSetErrorResponse(ErrorCode: Integer; ErrorMessage: string): TMVCWebResponse;
begin
  Self.StatusCode := ErrorCode;
  Self.Content := '{"error":"' + ErrorMessage + '"}';
  Result := Self;
end;

function THTMXResponseHelper.HXSetLocation(Path: string): TMVCWebResponse;
begin
  SetCustomHeader(THTMXResponseHeaderType.Location, Path);
  Result := Self;

  (* todo:
    This response header can be used to trigger a client side redirection without reloading the whole page. Instead of changing the page�s location it will act like following a hx-boost link, creating a new history entry, issuing an ajax request to the value of the header and pushing the path into history.

    A sample response would be:
    HX-Location: /test
    Which would push the client to test as if the user had clicked on <a href="/test" hx-boost="true">

    If you want to redirect to a specific target on the page rather than the default of document.body, you can pass more details along with the event, by using JSON for the value of the header:
    HX-Location: {"path":"/test2", "target":"#testdiv"}
    Path is required and is url to load the response from. The rest of the data mirrors the ajax api context, which is:
    source - the source element of the request
    event - an event that �triggered� the request
    handler - a callback that will handle the response HTML
    target - the target to swap the response into
    swap - how the response will be swapped in relative to the target
    values - values to submit with the request
    headers - headers to submit with the request
  *)
end;

function THTMXResponseHelper.HXSetPageRefresh(Refresh: Boolean): TMVCWebResponse;
begin
  SetCustomHeader(THTMXResponseHeaderType.Refresh, ifthen(Refresh, 'true','false')); //must be lowercase
  Result := Self;
end;

function THTMXResponseHelper.HXSetPushUrl(URL: string): TMVCWebResponse;
begin
  if URL.IsEmpty then
    URL := 'false';
  SetCustomHeader(THTMXResponseHeaderType.PushURL, URL);
  Result := Self;
end;

function THTMXResponseHelper.HXSetRedirect(Path: string): TMVCWebResponse;
begin
  SetCustomHeader(THTMXResponseHeaderType.Redirect, Path);
  Result := Self;
end;

function THTMXResponseHelper.HXSetReplaceUrl(URL: string): TMVCWebResponse;
begin
  if URL.IsEmpty then
    URL := 'false';
  SetCustomHeader(THTMXResponseHeaderType.ReplaceURL, URL);
  Result := Self;
end;

function THTMXResponseHelper.HXSetReSelect(Selector: string): TMVCWebResponse;
begin
  SetCustomHeader(THTMXResponseHeaderType.Reselect, Selector);
  Result := Self;
end;

function THTMXResponseHelper.HXSetReswap(Option: TSwapOption; ShowScroll: TShowScrollType; &To: TSwapScrollTo;
  Selector: string = ''): TMVCWebResponse;
var
  Modifiers: string;
begin
  if (ShowScroll <> ssNone) then
  begin
    Modifiers := Format(' %s', [ShowScrollTypes[ShowScroll]]);
    if not Selector.IsEmpty then
      Modifiers := Format('%s:%s', [Modifiers, Selector]);
    Modifiers := Format('%s:%s', [Modifiers, SwapScrollTo[&To]]);
  end;
  SetCustomHeader(THTMXResponseHeaderType.Reswap, SwapOptions[Option] + Modifiers);
  Result := Self;
end;

function THTMXResponseHelper.HXSetReswap(Option: TSwapOption; SwapDelay, SettleDelay: Integer): TMVCWebResponse;
var
  Modifiers: string;
begin
  if SwapDelay > 0 then
    Modifiers := Format('swap:%dms ', [SwapDelay]);
  if (SettleDelay > 0) and (SettleDelay <> 20) then
    Modifiers := Modifiers + Format('settle:%dms', [SettleDelay]);
  if not Modifiers.IsEmpty then
    Modifiers := ' ' + Modifiers.Trim;
  SetCustomHeader(THTMXResponseHeaderType.Reswap, SwapOptions[Option] + Modifiers);
  Result := Self;
end;

function THTMXResponseHelper.HXSetReswap(Option: TSwapOption): TMVCWebResponse;
begin
  // todo: support Focus scroll ?
  Result := HXSetReswap(Option, 0, 0);
end;

function THTMXResponseHelper.HXSetRetarget(Selector: string): TMVCWebResponse;
begin
  SetCustomHeader(THTMXResponseHeaderType.Retarget, Selector);
  Result := Self;
end;

function THTMXResponseHelper.HXTriggerClientEvent(Name: string; After: TClientEventType): TMVCWebResponse;
begin
  Result := HXTriggerClientEvent(Name, TValue.Empty, After);
end;

function THTMXResponseHelper.HXTriggerClientEvent(Name: string; Params: TValue; After: TClientEventType): TMVCWebResponse;
var
  lSer: TMVCJsonDataObjectsSerializer;
  lData: TJsonObject;
begin
  if not Params.IsEmpty then
  begin
    lData := TJsonObject.Create;
    lSer := TMVCJsonDataObjectsSerializer.Create;
    try
      lSer.TValueToJSONObjectProperty(lData, Name, Params, stdefault, [], []);
      SetCustomHeader(ClientEventTypes[After], lData.ToJSON);
    finally
      lSer.Free;
      lData.Free;
    end;
  end
  else
    SetCustomHeader(ClientEventTypes[After], Name);

  Result := Self;
end;

function THTMXResponseHelper.HXTriggerClientEvents(EventsDescriptors: TJSONObject;
  After: TClientEventType): TMVCWebResponse;
begin
  if EventsDescriptors = nil then
  begin
    Exit(Self);
  end;

  SetCustomHeader(ClientEventTypes[After], EventsDescriptors.ToJSON(true));
  Result := Self;
end;

function THTMXResponseHelper.HXTriggerClientEvents(Names: TArray<string>; After: TClientEventType): TMVCWebResponse;
var
  Value: string;
begin
  if Length(Names) = 0 then
    Exit(Self);

  Value := Names[0];
  for var I := Low(Names) + 1 to High(Names) do
    Value := Value + ', ' + Names[I];

  SetCustomHeader(ClientEventTypes[After], Value);
  Result := Self;
end;

end.
