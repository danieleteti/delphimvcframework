// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2026 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
// ***************************************************************************


unit ShowcaseModelsU;

// Companion unit to the Minimal API WebApp Showcase preset
// (routes_minimal_web_showcase.pas). Defines two records, each demonstrating
// a different non-JSON binding source.

interface

uses
  MVCFramework; // MVCFromContentField, MVCFromHeader, MVCFromCookie attributes

type
  // Form-bound record. Each scalar field maps to a form-urlencoded value via
  // [MVCFromContentField]. Interests is a TArray<string> — multi-value form
  // fields (e.g. <input type="checkbox" name="interest" value="..."> repeated)
  // bind to the entire array, NOT just the last value.
  TSignupForm = record
    [MVCFromContentField('username')]                Username:  string;
    [MVCFromContentField('email')]                   Email:     string;
    [MVCFromContentField('newsletter', 'off')]       Newsletter: string;  // 'on' if checked
    [MVCFromContentField('interest')]                Interests: TArray<string>;
  end;

  // Mixed-source record: [MVCFromHeader] reads request headers, [MVCFromCookie]
  // reads cookies. Both accept a default value as the second constructor arg,
  // returned when the source is missing.
  TContextInfo = record
    [MVCFromHeader('User-Agent', 'unknown')]         UserAgent: string;
    [MVCFromHeader('Accept-Language', 'en')]         Language:  string;
    [MVCFromCookie('theme', 'light')]                Theme:     string;
  end;

  // Query-string-bound record. Each field carries [MVCFromQueryString] with a
  // default — a request that omits the param still produces a valid value.
  // Same shape as the REST showcase, surfaced here as a server-rendered
  // search page.
  TSearchQuery = record
    [MVCFromQueryString('q')]            Term:     string;
    [MVCFromQueryString('page', '1')]    Page:     Integer;
    [MVCFromQueryString('size', '10')]   PageSize: Integer;
  end;

implementation

end.
