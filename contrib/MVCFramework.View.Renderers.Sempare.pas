// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2024 Daniele Teti and the DMVCFramework Team
//
// https://github.com/danieleteti/delphimvcframework
//
//
// ***************************************************************************
//
// Sempare Template Engine
//
// Copyright (c) 2019-2023 Conrad Vermeulen and Sempare Limited
//
// https://github.com/sempare/sempare-delphi-template-engine
//
// NOTE: The Sempare Template Engine is available under GPL or commercial license.
//
// Free as in speech, NOT Free as in beer.
//
// ***************************************************************************
//
// This adaptor is licensed under the Apache License.
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

unit MVCFramework.View.Renderers.Sempare;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Logger,
  System.Classes,
  Sempare.Template;

type
  Template = Sempare.Template.Template;
  TTemplateValue = Sempare.Template.TTemplateValue;

  TMVCSempareTemplateEngine = class(TMVCBaseViewEngine)
  public
    class constructor Create;
  public
    procedure Execute(const ViewName: string; const OutputStream: TStream); override;
  end;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  Data.DB,
  Sempare.Template.TemplateRegistry,
  Sempare.Template.AST,
  Sempare.Template.Common,
  Sempare.Template.ResourceStrings,
  Sempare.Template.Rtti;

class constructor TMVCSempareTemplateEngine.Create;
begin
  // The default Start and End token is <% and %> respectively.
  // Changing to {{ }} as it is what people may be used to when working with Mustache in the DelphiMVCFramework.
  with Template.Resolver.Context do
  begin
    StartToken := '{{';
    EndToken := '}}';
    UseHtmlVariableEncoder;
    Encoding := TEncoding.UTF8;
  end;
end;

procedure TMVCSempareTemplateEngine.Execute(const ViewName: string; const OutputStream: TStream);
begin
  try
    // the template engine will resolve the view. Using self, the template engine can dereference the ViewEngine properties.
    Template.Resolve(ViewName, self, OutputStream);
  except
    on e: Exception do
    begin
      Log.Error('[%s] %s', [e.Classname, e.Message], LOGGERPRO_TAG);
      raise e;
    end;
  end;
end;

function DerefBaseViewEngine(const APosition: IPosition; const AObj: TTemplateValue; const ADeref: TTemplateValue; const ARaiseIfMissing: boolean; const AContext: ITemplateContext; out AFound: boolean): TTemplateValue;
var
  LViewEngine: TMVCBaseViewEngine;
  LKey: string;
  LDataSet: TDataSet;
begin
  LViewEngine := AObj.AsType<TMVCBaseViewEngine>;
  LKey := AsString(ADeref, AContext);
  // check if the key exists in the ViewModel
  if assigned(LViewEngine.ViewModel) then
  begin
    AFound := LViewEngine.ViewModel.TryGetValue(LKey, result);
    if AFound then
      exit;
  end;
  // check if we have a dataset
  if assigned(LViewEngine.ViewDataSets) then
  begin
    AFound := LViewEngine.ViewDataSets.TryGetValue(LKey, LDataSet);
    if AFound then
      exit(LDataSet);
  end;
  // we don't know anything about the key, so behave accordingly
  if ARaiseIfMissing then
    RaiseError(APosition, SCannotDereferenceValueOnObject, [LKey, SDictionary]);
  exit('');
end;

function MatchBaseViewEngine(const ATypeInfo: PTypeInfo; const AClass: TClass): boolean;
begin
  exit(AClass = TMVCSempareTemplateEngine);
end;

initialization

RegisterDeref(MatchBaseViewEngine, DerefBaseViewEngine);

end.
