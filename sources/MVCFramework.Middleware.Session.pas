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

  function UseMemorySessionMiddleware(const aTimeoutInMinutes: Integer = 0; const aHttpOnly: Boolean = False): TMVCSessionMiddleware;
  function UseFileSessionMiddleware(const aTimeoutInMinutes: Integer = 0; const aHttpOnly: Boolean = False; const aSessionFolder: String = 'dmvc_sessions'): TMVCSessionMiddleware;
  function UseDatabaseSessionMiddleware(const aTimeoutInMinutes: Integer = 0; const aHttpOnly: Boolean = False): TMVCSessionMiddleware;

implementation

uses
  MVCFramework.Session.Database;


function UseMemorySessionMiddleware(const aTimeoutInMinutes: Integer; const aHttpOnly: Boolean): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(TMVCWebSessionMemoryFactory.Create(aHttpOnly, aTimeoutInMinutes));
end;

function UseFileSessionMiddleware(const aTimeoutInMinutes: Integer; const aHttpOnly: Boolean; const aSessionFolder: String): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(TMVCWebSessionFileFactory.Create(aHttpOnly, aTimeoutInMinutes, aSessionFolder));
end;

function UseDatabaseSessionMiddleware(const aTimeoutInMinutes: Integer; const aHttpOnly: Boolean): TMVCSessionMiddleware;
begin
  Result := TMVCSessionMiddleware.Create(
    TMVCWebSessionDatabaseFactory.Create(aHttpOnly, aTimeoutInMinutes, 'notused'));
end;




end.
