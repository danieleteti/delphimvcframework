{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit Sample.Data.Employee;

interface

uses
  Sample.Data.Address;

type
  TEmployee = class(TObject)
  strict private
    fName: String;
    fHireDate: TDate;
    fPhone: String;
    fId: Int64;
    fSalary: Double;
    fAddress: TAddress;
  public
    property Id: Int64 read fId write fId;
    property Name: String read fName write fName;
    property Phone: String read fPhone write fPhone;
    property HireDate: TDate read fHireDate write fHireDate;
    property Salary: Double read fSalary write fSalary;
    property Address: TAddress read fAddress write fAddress;
  end;

implementation

end.
