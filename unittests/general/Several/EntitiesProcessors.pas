unit EntitiesProcessors;

interface

uses
  MVCFramework.ActiveRecord,
  MVCFramework,
  MVCFramework.Serializer.Intf;

type
  TArticleProcessor = class(TInterfacedObject, IMVCEntityProcessor)
  public
    procedure CreateEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntities(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
    procedure UpdateEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
    procedure DeleteEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
  end;

  TContactProcessor = class(TInterfacedObject, IMVCEntityProcessor)
  public
    procedure CreateEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntities(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
    procedure UpdateEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
    procedure DeleteEntity(const Context: TWebContext;
      const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
      var Handled: Boolean);
  end;

implementation

{ TArticleProcessor }

uses
  System.SysUtils,
  Entities,
  MVCFramework.Serializer.JsonDataObjects,
  JsonDataObjects,
  MVCFramework.Serializer.Commons,
  System.Generics.Collections,
  MVCFramework.DuckTyping, MVCFramework.Commons, System.NetEncoding;

procedure TArticleProcessor.CreateEntity(const Context: TWebContext;
  const Renderer: TMVCRenderer; const entityname: string; var Handled: Boolean);
var
  lArticle: TArticle;
begin
  lArticle := Context.Request.BodyAs<TArticle>;
  try
    lArticle.Insert;
    Renderer.Render(lArticle, False);
  finally
    lArticle.Free;
  end;
  Handled := True;
end;

procedure TArticleProcessor.DeleteEntity(const Context: TWebContext;
  const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TArticleProcessor.GetEntities(const Context: TWebContext;
  const Renderer: TMVCRenderer; const entityname: string; var Handled: Boolean);
begin
  Handled := True;
  Renderer.Render(ObjectDict().Add('data', TMVCActiveRecord.All<TArticle>,
    procedure(const AObject: TObject; const Links: IMVCLinks)
    begin
      Links.AddRefLink
        .Add(HATEOAS.HREF, 'https://www.google.com/search?q=' + TNetEncoding.URL.EncodeQuery(TArticle(AObject).Description))
        .Add(HATEOAS._TYPE, 'text/html')
        .Add(HATEOAS.REL, 'googlesearch');
    end));
end;

procedure TArticleProcessor.GetEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
var Handled: Boolean);
begin
  Handled := False;
end;

procedure TArticleProcessor.UpdateEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
var Handled: Boolean);
begin
  Handled := False;
end;

{ TPeopleProcessor }

procedure TContactProcessor.CreateEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; var Handled: Boolean);
var
  lSer: TMVCJsonDataObjectsSerializer;
  lJSON: TJsonObject;
  lPerson: TPerson;
  lPhones: TObjectList<TPhone>;
  lPhone: TPhone;
  lID: Int64;
begin
  Handled := True;

  // If you have an entity already defined you can use the usual BodyAs<T>, if not
  // you have to deserialize request body manually
  lSer := TMVCJsonDataObjectsSerializer.Create;
  try
    lJSON := lSer.ParseObject(Context.Request.Body);
    try
      lPerson := TPerson.Create;
      try
        // deserialize person
        lSer.JsonObjectToObject(lJSON, lPerson,
          TMVCSerializationType.stDefault, nil);

        lPhones := TObjectList<TPhone>.Create(True);
        try
          // deserialize phones
          lSer.JsonArrayToList(lJSON.A['phones'], WrapAsList(lPhones), TPhone,
            TMVCSerializationType.stDefault, nil);

          // persist to database using transaction
          TMVCActiveRecord.CurrentConnection.StartTransaction;
          try
            lPerson.Insert; // insert Person
            lID := lPerson.id;
            for lPhone in lPhones do
            begin
              lPhone.IDPerson := lPerson.id;
              lPhone.Insert; // insert phone
            end;
            TMVCActiveRecord.CurrentConnection.Commit;
          except
            TMVCActiveRecord.CurrentConnection.Rollback;
            raise;
          end;

        finally
          lPhones.Free;
        end;
      finally
        lPerson.Free;
      end;
    finally
      lJSON.Free;
    end;
  finally
    lSer.Free;
  end;
  Context.Response.CustomHeaders.Values['X-REF'] := Context.Request.PathInfo +
    '/' + lID.ToString;
  Renderer.Render(TMVCResponse.Create(201, 'Contact created with phones', ''));
end;

procedure TContactProcessor.DeleteEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
var Handled: Boolean);
begin
  Handled := False; // inherit the default behaviour
end;

procedure TContactProcessor.GetEntities(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; var Handled: Boolean);
begin
  Handled := False; // inherit the default behaviour
end;

procedure TContactProcessor.GetEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
var Handled: Boolean);
var
  lContact: TContact;
  lSer: TMVCJsonDataObjectsSerializer;
  lJSON: TJsonObject;
  lPhones: TObjectList<TPhone>;
begin
  // You can write your own entity which already load relations
  // The following is the manual approach
  lContact := TMVCActiveRecord.GetByPK<TContact>(id);
  try
    lPhones := TMVCActiveRecord.Where<TPhone>('id_person = ?', [id]);
    try
      lSer := TMVCJsonDataObjectsSerializer.Create;
      try
        lJSON := TJsonObject.Create;
        try
          lSer.ObjectToJsonObject(lContact, lJSON,
            TMVCSerializationType.stDefault, nil);
          lSer.ListToJsonArray(WrapAsList(lPhones), lJSON.A['phones'],
            TMVCSerializationType.stDefault, nil);
          Renderer.Render(lJSON, False);
        finally
          lJSON.Free;
        end;
      finally
        lSer.Free;
      end;
    finally
      lPhones.Free;
    end;
  finally
    lContact.Free;
  end;
  Handled := True;
end;

procedure TContactProcessor.UpdateEntity(const Context: TWebContext;
const Renderer: TMVCRenderer; const entityname: string; const id: Integer;
var Handled: Boolean);
begin
  Handled := False; // inherit the default behaviour
end;

initialization

ActiveRecordMappingRegistry.AddEntityProcessor('articles',
  TArticleProcessor.Create);
ActiveRecordMappingRegistry.AddEntityProcessor('contacts',
  TContactProcessor.Create);

finalization

end.
