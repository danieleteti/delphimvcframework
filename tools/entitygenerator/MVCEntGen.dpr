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

program MVCEntGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FireDAC.Phys.FB,
  FireDAC.Phys.PG,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.IB,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.ODBC,
  FireDAC.ConsoleUI.Wait,
  EntGen.Core in 'EntGen.Core.pas',
  EntGen.CLIMain in 'EntGen.CLIMain.pas',
  MVCFramework.Commons in '..\..\sources\MVCFramework.Commons.pas';

begin
  try
    RunCLI;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
