unit ControllerU;

// ===========================================================================
//  Two actions, two serialization shapes, SAME transparent streaming path.
//  Both are FUNCTIONAL actions (function ... Result := ...). There is no
//  manual socket writing, no TMVCJSONArrayWriter - the framework handles
//  everything since 3.5.0-silicon.
//
//    GET /people/dataset       - function: TDataSet
//       Return the TDataSet directly. Render(TDataSet, ...) intercepts it
//       and writes rows directly to a TMemoryStream via TJsonTextWriter -
//       no TJsonArray DOM, no intermediate string. Output: raw [...] array.
//
//    GET /people/okresponse    - function: IMVCResponse
//       OKResponse(TDataSet) wraps the dataset in a {"data":[...]} envelope.
//       TryFastStreamingPath intercepts it and calls TryWriteDataSet. Same
//       no-DOM streaming writer, different output shape.
//
//  Peak server RAM on both paths = size of the final JSON response only.
//  Peak DB-side RAM = RowsetSize rows, thanks to SelectUnidirectionalDataSet
//  (forward-only cursor + fmOnDemand).
// ===========================================================================

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  Data.DB;

type
  [MVCPath('/people')]
  TPeopleController = class(TMVCController)
  public
    [MVCPath('/dataset')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/json')]
    function GetPeopleDataSet: TDataSet;

    [MVCPath('/okresponse')]
    [MVCHTTPMethod([httpGET])]
    [MVCProduces('application/json')]
    function GetPeopleOKResponse: IMVCResponse;
  end;

implementation

uses
  MVCFramework.ActiveRecord;

const
  SELECT_PEOPLE_SQL = 'SELECT id, full_name, email, country, age, joined_at FROM people';

function TPeopleController.GetPeopleDataSet: TDataSet;
begin
  // Return the TDataSet - the framework takes care of the rest.
  // SelectUnidirectionalDataSet => forward-only TFDQuery, fmOnDemand,
  // ReadOnly. DB buffer stays bounded to RowsetSize rows.
  // Framework side => Render(TDataSet, ...) writes rows directly via
  // TJsonTextWriter, no DOM, no intermediate string. Peak RAM = JSON size.
  Result := TMVCActiveRecord.SelectUnidirectionalDataSet(SELECT_PEOPLE_SQL, []);
end;

function TPeopleController.GetPeopleOKResponse: IMVCResponse;
begin
  // Same streaming serializer, different output shape: OKResponse wraps
  // the payload in {"data":[...]}. TryFastStreamingPath picks it up and
  // calls TryWriteDataSet. The controller code is a single expression.
  Result := OKResponse(
    TMVCActiveRecord.SelectUnidirectionalDataSet(SELECT_PEOPLE_SQL, []));
end;

end.
