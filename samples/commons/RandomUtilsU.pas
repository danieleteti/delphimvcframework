// ***************************************************************************
//
// Delphi MVC Framework
//
// Copyright (c) 2010-2023 Daniele Teti and the DMVCFramework Team
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

unit RandomUtilsU;

interface

const
  FirstNames: array [0 .. 9] of string = (
    'Daniele',
    'Debora',
    'Mattia',
    'Peter',
    'James',
    'Tim',
    'Joseph',
    'David',
    'Aleš',
    'Thomas'
    );

  LastNames: array [0 .. 9] of string = (
    'Smith',
    'Stark',
    'Williams',
    'Parker',
    'Jones',
    'Miller',
    'Davis',
    'Müller',
    'Martinez',
    'Anderson'
    );

  Countries: array [0 .. 9] of string = (
    'Italy',
    'New York',
    'Illinois',
    'Arizona',
    'Nevada',
    'UK',
    'France',
    'Germany',
    'Norway',
    'California'
    );

function GetRndFirstName: string;
function GetRndLastName: string;
function GetRndCountry: string;

implementation

function GetRndCountry: string;
begin
  Result := Countries[Random(10)];
end;

function GetRndFirstName: string;
begin
  Result := FirstNames[Random(10)];
end;

function GetRndLastName: string;
begin
  Result := LastNames[Random(10)];
end;

end.
