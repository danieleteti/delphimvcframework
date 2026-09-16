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

// Companion unit to the Minimal API Showcase preset (routes_minimal_showcase.pas).
// Defines:
//   * TPersonInput  — body-bound class with automatic validation
//   * TSearchQuery  — query-bound record demonstrating per-field attributes

interface

uses
  MVCFramework, // MVCFromQueryStringAttribute and friends
  MVCFramework.Validation,
  MVCFramework.Validators,
  MVCFramework.Serializer.Commons;

type
  // Body-bound class. TMVCValidatable + [MVCRequired]/[MVCMinLength]/[MVCEmail]
  // attributes trigger TMVCValidationEngine.ValidateAndRaise inside the minimal-
  // API arg resolver BEFORE the handler runs. A failure short-circuits with a
  // ProblemDetails 400 — the handler body never sees an invalid instance.
  TPersonInput = class(TMVCValidatable)
  private
    fFirstName: string;
    fLastName: string;
    fEmail: string;
  published
    [MVCRequired('first name is required')]
    [MVCMinLength(2, 'first name must be at least 2 chars')]
    property FirstName: string read fFirstName write fFirstName;

    [MVCRequired('last name is required')]
    property LastName: string read fLastName write fLastName;

    [MVCRequired('email is required')]
    [MVCEmail('email must be a valid address')]
    property Email: string read fEmail write fEmail;
  end;

  // Query-bound record. Each field carries an attribute that names its binding
  // source. [MVCFromQueryString] is the most common shape; swap in
  // [MVCFromHeader] / [MVCFromCookie] / [MVCFromContentField] / [MVCFromBody]
  // to bind from those sources instead — the per-field default-value syntax
  // ('20' below) is identical across attributes.
  TSearchQuery = record
    [MVCFromQueryString('q')] Term: string;
    [MVCFromQueryString('page', '1')] Page: Integer;
    [MVCFromQueryString('size', '20')] PageSize: Integer;
  end;

  // Repeated query keys bind to a typed dynamic array: ?tag=a&tag=b&tag=c ->
  // Tags = ['a','b','c']. Works for TArray<string>, TArray<Integer>, etc.
  TTagSearch = record
    [MVCFromQueryString('tag')] Tags: TArray<string>;
  end;

implementation

end.
