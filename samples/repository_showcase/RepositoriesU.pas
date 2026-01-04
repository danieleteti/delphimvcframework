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

unit RepositoriesU;

interface

uses
  MVCFramework.Repository,
  EntitiesU,
  System.Generics.Collections;

type
  /// <summary>
  /// Customer Repository - uses default implementation
  /// Can be injected as IMVCRepository<TCustomer> in controllers
  /// </summary>

  ICustomerRepository = interface(IMVCRepository<TCustomer>)
    ['{969BC9CE-E30E-4104-8F74-CE2A7C18AD21}']
    function GetCustomersByCity(const City: string): TObjectList<TCustomer>;
    function GetTopRatedCustomers: TObjectList<TCustomer>;
  end;

  TCustomerRepository = class(TMVCRepository<TCustomer>, ICustomerRepository)
  public
    // Add custom methods here if needed
    function GetCustomersByCity(const City: string): TObjectList<TCustomer>;
    function GetTopRatedCustomers: TObjectList<TCustomer>;
  end;

  /// <summary>
  /// Article Repository - uses default implementation
  /// Can be injected as IMVCRepository<TArticle> in controllers
  /// </summary>
  TArticleRepository = class(TMVCRepository<TArticle>)
  public
    // Add custom methods here if needed
    function GetArticlesByPriceRange(MinPrice, MaxPrice: Currency): TObjectList<TArticle>;
  end;

  /// <summary>
  /// OrderDetail Repository - uses default implementation
  /// Can be injected as IMVCRepository<TOrderDetail> in controllers
  /// </summary>
  TOrderDetailRepository = class(TMVCRepository<TOrderDetail>)
  public
    // Add custom methods here if needed
    function GetOrderDetailsByOrder(OrderID: Integer): TObjectList<TOrderDetail>;
  end;

implementation

uses
  MVCFramework.ActiveRecord;

{ TCustomerRepository }

function TCustomerRepository.GetCustomersByCity(const City: string): TObjectList<TCustomer>;
begin
  Result := GetWhere('city = ?', [City]);
end;

function TCustomerRepository.GetTopRatedCustomers: TObjectList<TCustomer>;
begin
  Result := SelectByNamedQuery('BestCustomers', [], []);
end;

{ TArticleRepository }

function TArticleRepository.GetArticlesByPriceRange(MinPrice, MaxPrice: Currency): TObjectList<TArticle>;
begin
  Result := GetWhere('price >= ? AND price <= ?', [MinPrice, MaxPrice]);
end;

{ TOrderDetailRepository }

function TOrderDetailRepository.GetOrderDetailsByOrder(OrderID: Integer): TObjectList<TOrderDetail>;
begin
  Result := GetWhere('id_order = ?', [OrderID]);
end;

end.
