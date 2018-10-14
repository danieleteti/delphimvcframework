unit EntitiesProcessors;

interface

uses
  MVCFramework.ActiveRecord,
  MVCFramework,
  MVCFramework.Serializer.Intf;

type
  TArticleProcessor = class(TInterfacedObject, IMVCEntityProcessor)
  public
    procedure CreateEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntities(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
      var Handled: Boolean);
    procedure GetEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
      const id: Integer; var Handled: Boolean);
    procedure UpdateEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
      const id: Integer; var Handled: Boolean);
    procedure DeleteEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
      const id: Integer; var Handled: Boolean);
  end;

implementation

{ TArticleProcessor }

uses
  Entities;

procedure TArticleProcessor.CreateEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
  var Handled: Boolean);
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

procedure TArticleProcessor.DeleteEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
  const id: Integer; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TArticleProcessor.GetEntities(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TArticleProcessor.GetEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
  const id: Integer; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TArticleProcessor.UpdateEntity(const Context: TWebContext; const Renderer: TMVCRenderer; const entityname: string;
  const id: Integer; var Handled: Boolean);
begin
  Handled := False;
end;

initialization

ActiveRecordMappingRegistry.AddEntityProcessor('articles', TArticleProcessor.Create);

finalization

end.
