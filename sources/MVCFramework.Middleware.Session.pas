// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2025 Daniele Teti and the DMVCFramework Team
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
// *************************************************************************** }

unit MVCFramework.Middleware.Session;

{$I dmvcframework.inc}

interface

uses
  MVCFramework,
  MVCFramework.Session,
  MVCFramework.Middleware.Session.Internal
  ;

  function UseMemorySessionMiddleware(aTimeoutInMinutes: Integer = 0): TMVCSessionMiddleware;
  function UseFileSessionMiddleware(aTimeoutInMinutes: Integer = 0; aSessionFolder: String = 'dmvc_sessions'): TMVCSessionMiddleware;
  function UseDatabaseSessionMiddleware(aTimeoutInMinutes: Integer = 0): TMVCSessionMiddleware;

implementation

uses
  MVCFramework.Session.Database;


function UseMemorySessionMiddleware(aTimeoutInMinutes: Integer = 0): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(TMVCWebSessionMemoryFactory.Create(aTimeoutInMinutes));
end;

function UseFileSessionMiddleware(aTimeoutInMinutes: Integer = 0; aSessionFolder: String = 'dmvc_sessions'): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(TMVCWebSessionFileFactory.Create(aTimeoutInMinutes, aSessionFolder));
end;

function UseDatabaseSessionMiddleware(aTimeoutInMinutes: Integer = 0): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(
    TMVCWebSessionDatabaseFactory.Create(aTimeoutInMinutes, 'notused'));
end;




end.
