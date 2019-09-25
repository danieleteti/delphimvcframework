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

unit Sample.Data.Address;

interface

type
  TAddress = class(TObject)
  private
    fPostalCode: string;
    fCountry: string;
    fDescription: string;
    fCity: string;
    fRegion: string;
  public
    property Description: string read fDescription write fDescription;
    property City: string read fCity write fCity;
    property Region: string read fRegion write fRegion;
    property Country: string read fCountry write fCountry;
    property PostalCode: string read fPostalCode write fPostalCode;
  end;

implementation

end.
