// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************

unit Controllers.PeoplePagesU;

// The People pages - a table (search, filter, sort, HTMX) and a form built with
// the TemplatePro forms library (templates/lib/forms_bootstrap5.tpro).
// The data lives in PeopleSampleU.

interface

uses
  MVCFramework, MVCFramework.Commons;

type
  [MVCPath('/web/people')]
  TPeoplePagesController = class(TMVCController)
  private
    function RenderForm(AModel: TObject; AErrors: TObject; AID: Integer): String;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
  public
    // Search, status filter and sorting come from the query string,
    // so every state of the table has its own URL.
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function Index(
      [MVCFromQueryString('q', '')] const Search: String;
      [MVCFromQueryString('status', '')] const Status: String;
      [MVCFromQueryString('sort', 'name')] const Sort: String;
      [MVCFromQueryString('dir', 'asc')] const Dir: String): String;

    [MVCPath('/new')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function NewPerson: String;

    [MVCPath('/new')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function CreatePerson: IMVCResponse;

    [MVCPath('/($ID:int)')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function EditPerson(ID: Integer): String;

    [MVCPath('/($ID:int)')]
    [MVCHTTPMethod([httpPOST])]
    [MVCProduces(TMVCMediaType.TEXT_HTML)]
    function SavePerson(ID: Integer): IMVCResponse;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.Generics.Collections, MVCFramework.HTMX, PeopleSampleU;

procedure TPeoplePagesController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  inherited;
  // What baselayout.html reads on every page
  ViewData['app_name'] := 'WebAppHTMXForms';
  ViewData['dmvc_version'] := DMVCFRAMEWORK_VERSION;
  ViewData['current_year'] := YearOf(Now);
  ViewData['page_id'] := 'people';
end;

function TPeoplePagesController.Index(const Search, Status, Sort, Dir: String): String;
var
  lPage: TPeoplePage;
begin
  lPage := BuildPeoplePage(Search, Status, Sort, Dir);
  try
    ViewData['people'] := lPage.Rows;
    ViewData['q'] := Search;
    ViewData['query'] := Context.Request.QueryParams; // model of the search box
    ViewData['status'] := LowerCase(Status);
    ViewData['sort'] := lPage.Sort;
    ViewData['dir'] := lPage.Dir;
    ViewData['count_all'] := lPage.CountActive + lPage.CountInvited + lPage.CountSuspended;
    ViewData['count_active'] := lPage.CountActive;
    ViewData['count_invited'] := lPage.CountInvited;
    ViewData['count_suspended'] := lPage.CountSuspended;
    ViewData['count_shown'] := lPage.Rows.Count;

    // HTMX asks for the table only (search box, sort links, filter tabs);
    // a normal request, a history restore or a browser without JavaScript gets the whole page.
    Context.Response.SetCustomHeader('Vary', 'HX-Request'); // same URL, two bodies: caches must tell them apart
    if Context.Request.IsHTMX and not Context.Request.HXIsBoosted and
      not Context.Request.HXIsHistoryRestoreRequest then
      Result := RenderView('people/table')
    else
      Result := RenderView('people/index');
  finally
    lPage.Rows.Free;
  end;
end;

// formModel and formErrors are the variables the forms library reads by default
function TPeoplePagesController.RenderForm(AModel: TObject; AErrors: TObject; AID: Integer): String;
begin
  ViewData['formModel'] := AModel;
  if AErrors <> nil then
    ViewData['formErrors'] := AErrors;
  ViewData['person_id'] := AID;
  ViewData['is_new'] := AID = 0;
  if AID = 0 then
    ViewData['form_action'] := '/web/people/new'
  else
    ViewData['form_action'] := '/web/people/' + AID.ToString;
  ViewData['roles'] := PeopleRoles;
  ViewData['statuses'] := PeopleStatuses;
  Result := RenderView('people/edit');
end;

function TPeoplePagesController.NewPerson: String;
var
  lPerson: TPersonRow;
begin
  lPerson := PeopleSampleU.NewPerson;
  try
    Result := RenderForm(lPerson, nil, 0);
  finally
    lPerson.Free;
  end;
end;

function TPeoplePagesController.CreatePerson: IMVCResponse;
var
  lPerson: TPersonRow;
  lErrors: TDictionary<String, String>;
  lPage: TMVCHTMLResponse;
begin
  lPerson := PeopleSampleU.NewPerson;
  lErrors := TDictionary<String, String>.Create; // field name -> message
  try
    ReadPerson(lPerson, Context.Request.ContentFields, lErrors);
    if lErrors.Count = 0 then
    begin
      AddPerson(lPerson);
      // Post/Redirect/Get: reloading the list does not submit the form again
      Exit(RedirectResponse('/web/people'));
    end;
    // Invalid: the same page, with what was typed and a message per field
    lPage := TMVCHTMLResponse.Create;
    Result := lPage; // owned by the interface from here, also if RenderView raises
    lPage.StatusCode := HTTP_STATUS.UnprocessableEntity;
    lPage.HTMLBody := RenderForm(Context.Request.ContentFields, lErrors, 0);
  finally
    lErrors.Free;
    lPerson.Free;
  end;
end;

function TPeoplePagesController.EditPerson(ID: Integer): String;
var
  lPerson: TPersonRow;
begin
  lPerson := FindPerson(ID);
  if lPerson = nil then
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'Person not found');
  try
    Result := RenderForm(lPerson, nil, ID);
  finally
    lPerson.Free;
  end;
end;

function TPeoplePagesController.SavePerson(ID: Integer): IMVCResponse;
var
  lPerson: TPersonRow;
  lErrors: TDictionary<String, String>;
  lPage: TMVCHTMLResponse;
begin
  lPerson := FindPerson(ID);
  if lPerson = nil then
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'Person not found');
  lErrors := TDictionary<String, String>.Create;
  try
    ReadPerson(lPerson, Context.Request.ContentFields, lErrors);
    if lErrors.Count = 0 then
    begin
      StorePerson(lPerson);
      Exit(RedirectResponse('/web/people'));
    end;
    lPage := TMVCHTMLResponse.Create;
    Result := lPage;
    lPage.StatusCode := HTTP_STATUS.UnprocessableEntity;
    lPage.HTMLBody := RenderForm(Context.Request.ContentFields, lErrors, ID);
  finally
    lErrors.Free;
    lPerson.Free;
  end;
end;

end.
