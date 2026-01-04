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

unit MVCFramework.Repository;

{$I dmvcframework.inc}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  MVCFramework.ActiveRecord;

type
  /// <summary>
  /// Generic Repository interface for CRUD operations on entities.
  /// Use this interface for dependency injection in controllers and services.
  /// All methods delegate to TMVCActiveRecord, ensuring no code duplication.
  /// </summary>
  /// <example>
  /// <code>
  /// type
  ///   TMyController = class(TMVCController)
  ///   private
  ///     fCustomerRepo: IMVCRepository&lt;TCustomer&gt;;
  ///   public
  ///     constructor Create(const CustomerRepo: IMVCRepository&lt;TCustomer&gt;);
  ///     [MVCPath('/customers')]
  ///     procedure GetAll;
  ///   end;
  /// </code>
  /// </example>
  IMVCRepository<T: TMVCActiveRecord, constructor> = interface
    ['{8B5F3A21-9C4D-4E5F-B8A1-3D2E1F4C5A6B}']

    // Basic CRUD operations
    /// <summary>
    /// Retrieves an entity by its primary key (Int64)
    /// </summary>
    /// <param name="aValue">Primary key value</param>
    /// <param name="RaiseExceptionIfNotFound">If True, raises EMVCActiveRecordNotFound when entity not found</param>
    /// <returns>Entity instance or nil if not found and RaiseExceptionIfNotFound=False</returns>
    function GetByPK(const aValue: Int64; const RaiseExceptionIfNotFound: Boolean = True): T; overload;

    /// <summary>
    /// Retrieves an entity by its primary key (string)
    /// </summary>
    function GetByPK(const aValue: string; const RaiseExceptionIfNotFound: Boolean = True): T; overload;

    /// <summary>
    /// Retrieves an entity by its primary key (GUID)
    /// </summary>
    function GetByPK(const aValue: TGuid; const RaiseExceptionIfNotFound: Boolean = True): T; overload;

    /// <summary>
    /// Inserts a new entity into the database
    /// </summary>
    /// <param name="aEntity">Entity to insert (must not be nil)</param>
    /// <exception cref="EMVCActiveRecord">Raised if aEntity is nil</exception>
    procedure Insert(const aEntity: T);

    /// <summary>
    /// Updates an existing entity in the database
    /// </summary>
    /// <param name="aEntity">Entity to update (must not be nil)</param>
    /// <param name="RaiseExceptionIfNotFound">If True, raises exception when entity not found</param>
    procedure Update(const aEntity: T; const RaiseExceptionIfNotFound: Boolean = True);

    /// <summary>
    /// Deletes an entity from the database
    /// </summary>
    /// <param name="aEntity">Entity to delete (must not be nil)</param>
    procedure Delete(const aEntity: T; const RaiseExceptionIfNotFound: Boolean = True);

    /// <summary>
    /// Stores an entity (Insert if PK is null, Update otherwise)
    /// </summary>
    procedure Store(const aEntity: T);

    // Query operations
    function GetAll: TObjectList<T>;
    function GetWhere(const SQLWhere: string; const Params: array of Variant): TObjectList<T>; overload;
    function GetWhere(const SQLWhere: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>; overload;
    function GetOneByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;
    function GetFirstByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;

    // RQL operations
    function SelectRQL(const RQL: string; const MaxRecordCount: Integer = 1000): TObjectList<T>;
    function GetOneByRQL(const RQL: string; const RaiseExceptionIfNotFound: Boolean = True): T;
    function CountRQL(const RQL: string = ''): Int64;
    function DeleteRQL(const RQL: string = ''): Int64;

    // SQL operations
    function Select(const SQL: string; const Params: array of Variant): TObjectList<T>; overload;
    function Select(const SQL: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>; overload;
    function SelectOne(const SQL: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;

    // Named Query operations
    function SelectByNamedQuery(const QueryName: String; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>;
    function SelectRQLByNamedQuery(const QueryName: String; const Params: array of const; const MaxRecordCount: Integer = 1000): TObjectList<T>;
    function CountRQLByNamedQuery(const QueryName: string; const Params: array of const): Int64;
    function DeleteRQLByNamedQuery(const QueryName: String; const Params: array of const): Int64;

    // Utility operations
    /// <summary>
    /// Counts entities, optionally filtered by RQL
    /// </summary>
    /// <param name="RQL">RQL filter string (empty = count all)</param>
    function Count(const RQL: string = ''): Int64;

    /// <summary>
    /// Deletes all entities of this type
    /// </summary>
    /// <returns>Number of deleted records</returns>
    function DeleteAll: Int64;

    /// <summary>
    /// Checks if an entity with the given PK exists
    /// </summary>
    function Exists(const aValue: Int64): Boolean; overload;
    function Exists(const aValue: string): Boolean; overload;
    function Exists(const aValue: TGuid): Boolean; overload;
  end;

  /// <summary>
  /// Base implementation of Repository pattern using TMVCActiveRecord
  /// All persistence logic is delegated to TMVCActiveRecord
  /// </summary>
  TMVCRepository<T: TMVCActiveRecord, constructor> = class(TInterfacedObject, IMVCRepository<T>)
  protected
    // Override this method to provide custom connection logic
    function GetConnection: TFDConnection; virtual;
  public
    constructor Create; virtual;

    // Basic CRUD operations
    function GetByPK(const aValue: Int64; const RaiseExceptionIfNotFound: Boolean = True): T; overload;
    function GetByPK(const aValue: string; const RaiseExceptionIfNotFound: Boolean = True): T; overload;
    function GetByPK(const aValue: TGuid; const RaiseExceptionIfNotFound: Boolean = True): T; overload;

    procedure Insert(const aEntity: T);
    procedure Update(const aEntity: T; const RaiseExceptionIfNotFound: Boolean = True);
    procedure Delete(const aEntity: T; const RaiseExceptionIfNotFound: Boolean = True);
    procedure Store(const aEntity: T);

    // Query operations
    function GetAll: TObjectList<T>;
    function GetWhere(const SQLWhere: string; const Params: array of Variant): TObjectList<T>; overload;
    function GetWhere(const SQLWhere: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>; overload;
    function GetOneByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;
    function GetFirstByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;

    // RQL operations
    function SelectRQL(const RQL: string; const MaxRecordCount: Integer = 1000): TObjectList<T>;
    function GetOneByRQL(const RQL: string; const RaiseExceptionIfNotFound: Boolean = True): T;
    function CountRQL(const RQL: string = ''): Int64;
    function DeleteRQL(const RQL: string = ''): Int64;

    // SQL operations
    function Select(const SQL: string; const Params: array of Variant): TObjectList<T>; overload;
    function Select(const SQL: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>; overload;
    function SelectOne(const SQL: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean = True): T;

    // Named Query operations
    function SelectByNamedQuery(const QueryName: String; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>;
    function SelectRQLByNamedQuery(const QueryName: String; const Params: array of const; const MaxRecordCount: Integer = 1000): TObjectList<T>;
    function CountRQLByNamedQuery(const QueryName: string; const Params: array of const): Int64;
    function DeleteRQLByNamedQuery(const QueryName: String; const Params: array of const): Int64;

    // Utility operations
    function Count(const RQL: string = ''): Int64;
    function DeleteAll: Int64;
    function Exists(const aValue: Int64): Boolean; overload;
    function Exists(const aValue: string): Boolean; overload;
    function Exists(const aValue: TGuid): Boolean; overload;
  end;

  /// <summary>
  /// Repository with explicit connection management
  /// Useful for injecting specific connections in controllers
  /// </summary>
  TMVCRepositoryWithConnection<T: TMVCActiveRecord, constructor> = class(TMVCRepository<T>)
  private
    fConnection: TFDConnection;
    fOwnsConnection: Boolean;
  protected
    function GetConnection: TFDConnection; override;
  public
    constructor Create(const aConnection: TFDConnection; const aOwnsConnection: Boolean = False); reintroduce;
    destructor Destroy; override;
  end;

  TMVCRepository = class
  {$IF Defined(CUSTOM_MANAGED_RECORDS)}
      class function UseTransactionContext: TMVCTransactionContext;
  {$ENDIF}
  end;

implementation

{ TMVCRepository<T> }

constructor TMVCRepository<T>.Create;
begin
  inherited Create;
end;

function TMVCRepository<T>.GetConnection: TFDConnection;
begin
  // Use default ActiveRecord connection
  Result := TMVCActiveRecord.CurrentConnection;
end;

function TMVCRepository<T>.GetByPK(const aValue: Int64; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.GetByPK<T>(aValue, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.GetByPK(const aValue: string; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.GetByPK<T>(aValue, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.GetByPK(const aValue: TGuid; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.GetByPK<T>(aValue, RaiseExceptionIfNotFound);
end;

procedure TMVCRepository<T>.Insert(const aEntity: T);
begin
  if not Assigned(aEntity) then
    raise EMVCActiveRecord.Create('Cannot insert nil entity');
  aEntity.Insert;
end;

procedure TMVCRepository<T>.Update(const aEntity: T; const RaiseExceptionIfNotFound: Boolean);
begin
  if not Assigned(aEntity) then
    raise EMVCActiveRecord.Create('Cannot update nil entity');
  aEntity.Update(RaiseExceptionIfNotFound);
end;

procedure TMVCRepository<T>.Delete(const aEntity: T; const RaiseExceptionIfNotFound: Boolean);
begin
  if not Assigned(aEntity) then
    raise EMVCActiveRecord.Create('Cannot delete nil entity');
  aEntity.Delete(RaiseExceptionIfNotFound);
end;

procedure TMVCRepository<T>.Store(const aEntity: T);
begin
  if not Assigned(aEntity) then
    raise EMVCActiveRecord.Create('Cannot store nil entity');
  aEntity.Store;
end;

function TMVCRepository<T>.GetAll: TObjectList<T>;
begin
  Result := TMVCActiveRecord.All<T>;
end;

function TMVCRepository<T>.GetWhere(const SQLWhere: string; const Params: array of Variant): TObjectList<T>;
begin
  if SQLWhere.Trim.IsEmpty then
    raise EMVCActiveRecord.Create('SQLWhere parameter cannot be empty. Use GetAll() to retrieve all records.');
  Result := TMVCActiveRecord.Where<T>(SQLWhere, Params);
end;

function TMVCRepository<T>.GetWhere(const SQLWhere: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>;
begin
  if SQLWhere.Trim.IsEmpty then
    raise EMVCActiveRecord.Create('SQLWhere parameter cannot be empty. Use GetAll() to retrieve all records.');
  if Length(Params) <> Length(ParamTypes) then
    raise EMVCActiveRecord.Create('Params and ParamTypes arrays must have the same length');
  Result := TMVCActiveRecord.Where<T>(SQLWhere, Params, ParamTypes);
end;

function TMVCRepository<T>.GetOneByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.GetOneByWhere<T>(SQLWhere, Params, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.GetFirstByWhere(const SQLWhere: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.GetFirstByWhere<T>(SQLWhere, Params, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.SelectRQL(const RQL: string; const MaxRecordCount: Integer): TObjectList<T>;
begin
  Result := TMVCActiveRecord.SelectRQL<T>(RQL, MaxRecordCount);
end;

function TMVCRepository<T>.GetOneByRQL(const RQL: string; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.SelectOneByRQL<T>(RQL, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.CountRQL(const RQL: string): Int64;
begin
  Result := TMVCActiveRecord.Count<T>(RQL);
end;

function TMVCRepository<T>.DeleteRQL(const RQL: string): Int64;
begin
  Result := TMVCActiveRecord.DeleteRQL<T>(RQL);
end;

function TMVCRepository<T>.Select(const SQL: string; const Params: array of Variant): TObjectList<T>;
begin
  Result := TMVCActiveRecord.Select<T>(SQL, Params);
end;

function TMVCRepository<T>.Select(const SQL: string; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>;
begin
  Result := TMVCActiveRecord.Select<T>(SQL, Params, ParamTypes);
end;

function TMVCRepository<T>.SelectOne(const SQL: string; const Params: array of Variant; const RaiseExceptionIfNotFound: Boolean): T;
begin
  Result := TMVCActiveRecord.SelectOne<T>(SQL, Params, RaiseExceptionIfNotFound);
end;

function TMVCRepository<T>.SelectByNamedQuery(const QueryName: String; const Params: array of Variant; const ParamTypes: array of TFieldType): TObjectList<T>;
begin
  Result := TMVCActiveRecord.SelectByNamedQuery<T>(QueryName, Params, ParamTypes);
end;

function TMVCRepository<T>.SelectRQLByNamedQuery(const QueryName: String; const Params: array of const; const MaxRecordCount: Integer): TObjectList<T>;
begin
  Result := TMVCActiveRecord.SelectRQLByNamedQuery<T>(QueryName, Params, MaxRecordCount);
end;

function TMVCRepository<T>.CountRQLByNamedQuery(const QueryName: string; const Params: array of const): Int64;
begin
  Result := TMVCActiveRecord.CountRQLByNamedQuery<T>(QueryName, Params);
end;

function TMVCRepository<T>.DeleteRQLByNamedQuery(const QueryName: String; const Params: array of const): Int64;
begin
  Result := TMVCActiveRecord.DeleteRQLByNamedQuery<T>(QueryName, Params);
end;

function TMVCRepository<T>.Count(const RQL: string): Int64;
begin
  Result := TMVCActiveRecord.Count<T>(RQL);
end;

function TMVCRepository<T>.DeleteAll: Int64;
begin
  Result := TMVCActiveRecord.DeleteAll(T);
end;

function TMVCRepository<T>.Exists(const aValue: Int64): Boolean;
var
  lEntity: T;
begin
  lEntity := GetByPK(aValue, False);
  Result := Assigned(lEntity);
  if Assigned(lEntity) then
    lEntity.Free;
end;

function TMVCRepository<T>.Exists(const aValue: string): Boolean;
var
  lEntity: T;
begin
  lEntity := GetByPK(aValue, False);
  Result := Assigned(lEntity);
  if Assigned(lEntity) then
    lEntity.Free;
end;

function TMVCRepository<T>.Exists(const aValue: TGuid): Boolean;
var
  lEntity: T;
begin
  lEntity := GetByPK(aValue, False);
  Result := Assigned(lEntity) ;
  if Assigned(lEntity) then
    lEntity.Free;
end;

{ TMVCRepositoryWithConnection<T> }

constructor TMVCRepositoryWithConnection<T>.Create(const aConnection: TFDConnection; const aOwnsConnection: Boolean);
begin
  inherited Create;
  fConnection := aConnection;
  fOwnsConnection := aOwnsConnection;
end;

destructor TMVCRepositoryWithConnection<T>.Destroy;
begin
  if fOwnsConnection then
    fConnection.Free;
  inherited;
end;

function TMVCRepositoryWithConnection<T>.GetConnection: TFDConnection;
begin
  Result := fConnection;
end;


{$IF Defined(CUSTOM_MANAGED_RECORDS)}
class function TMVCRepository.UseTransactionContext: TMVCTransactionContext;
begin
  Result := TMVCTransactionContext.Create(0);
end;
{$ENDIF}


end.
