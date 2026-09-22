# Guia de Migração do SwagDoc: Swagger 2.0 para OpenAPI 3.2.1

De `svSwagger2` para `svOpenApi3`: o que mudou na especificação, o que alterar no seu código Delphi, quais arquivos entram no deploy e como validar o resultado.

Biblioteca SwagDoc - versão que introduz `TSwagDoc.SpecVersion` (setembro de 2026). Somente Delphi RTL, utilizável em aplicações VCL, FMX, console e servidor.

## 1. Visão geral

O SwagDoc agora grava o mesmo modelo de objetos como um documento Swagger 2.0 (`swagger.json`) ou como um documento OpenAPI 3 (`openapi.json`). A família é selecionada pela propriedade `SpecVersion` de `TSwagDoc`.

Pontos principais antes de começar:

- `SpecVersion` tem `svSwagger2` como padrão. Uma aplicação que não usa a nova propriedade continua produzindo o mesmo documento Swagger 2.0 que produzia antes.
- Uma única linha muda a saída para OpenAPI 3.2.1: `vSwagDoc.SpecVersion := svOpenApi3;`.
- As classes e propriedades do Swagger 2.0 continuam funcionando quando o documento é gerado como OpenAPI 3. Host, base path, schemes, consumes, produces e os parâmetros body e formData são traduzidos automaticamente.
- Os objetos do OpenAPI 3 (servers, corpos de requisição, media types, links, callbacks, webhooks, requisitos de segurança com escopos, extensões e todos os componentes reutilizáveis) estão disponíveis no modelo de objetos. Eles são traduzidos ou omitidos quando um documento Swagger 2.0 é gerado.
- `LoadFromFile` lê documentos Swagger 2.0 e OpenAPI 3.0, 3.1 e 3.2, de modo que um `swagger.json` existente pode ser convertido.
- Documentos OpenAPI 3.2 precisam do Swagger UI 5. A pasta `Deploy` agora tem uma pasta por família.

### Respostas rápidas

| Pergunta | Resposta |
|----------|--------|
| Preciso alterar meu código depois de atualizar a biblioteca? | Não, a menos que seu código se enquadre em um dos casos de código-fonte da seção 4.3 (enumerações estendidas, definições de segurança personalizadas ou uma lista explícita de units em um package). |
| A minha saída Swagger 2.0 muda? | Documentos construídos em código mantêm a mesma saída. Documentos carregados de um arquivo swagger.json agora são gravados com mais informações (seção 4.2). |
| Qual é a menor migração possível? | Definir `SpecVersion := svOpenApi3` e publicar o `openapi.json` com os arquivos do Swagger UI 5 de `Deploy\OpenApi3`. |
| Posso publicar as duas versões? | Sim. Gere o documento duas vezes a partir do mesmo modelo, alterando `SpecVersion` (seção 6.15). |
| Qual versão do OpenAPI é gravada? | A versão 3.x mais recente suportada pelo SwagDoc, atualmente a 3.2.1. `SwaggerVersion` retorna o valor exato. |

### Estratégias de migração

1. **Apenas trocar.** Mantenha o modelo Swagger 2.0 e defina `SpecVersion := svOpenApi3`. O SwagDoc traduz tudo. Recomendado como primeiro passo.
2. **Período de transição.** Publique `swagger.json` e `openapi.json` lado a lado enquanto os consumidores da API migram para o novo documento.
3. **Adoção completa.** Substitua as construções do Swagger 2.0 pelos objetos do OpenAPI 3 (servers, corpos de requisição, content por media type, requisitos de segurança com escopos) e passe a usar os objetos que só existem no OpenAPI 3.

## 2. O modelo de versões

| SpecVersion | Campo raiz do documento | Nome de arquivo padrão | Dialeto de schema |
|-------------|----------------------------|-------------------|----------------|
| `svSwagger2` (padrão) | `"swagger": "2.0"` | swagger.json | Schema object do Swagger 2.0 (subconjunto do JSON Schema draft 4) |
| `svOpenApi3` | `"openapi": "3.2.1"` | openapi.json | JSON Schema 2020-12 |

`SpecVersion` identifica uma **família** da especificação, não uma versão específica:

- Quando uma versão 3.x mais recente passa a ser suportada, `svOpenApi3` começa a gravá-la e o seu código não muda.
- Um novo valor só é adicionado para uma nova família (OpenAPI 4) ou para uma versão incompatível com os documentos já produzidos pela sua família.
- A versão exata gravada no documento é retornada por `SwaggerVersion` ("2.0" ou "3.2.1").

```delphi
uses
  Swag.Common.Types, Swag.Doc;

vSwagDoc.SpecVersion := svOpenApi3;
vSwagDoc.GenerateSwaggerJson;                 // vSwagDoc.SwaggerVersion = '3.2.1'
vSwagDoc.SwaggerFilesFolder := 'C:\MyApi\Help';
vSwagDoc.SaveSwaggerJsonToFile;               // grava C:\MyApi\Help\openapi.json
```

O nome do arquivo segue a família, a menos que `SwaggerFileName` seja atribuído. Atribua `SwaggerFileName := 'swagger.json'` quando a URL publicada não puder mudar.

## 3. O que mudou na especificação

### 3.1 Estrutura do documento: Swagger 2.0 comparado com OpenAPI 3.2.1

| Área | Swagger 2.0 | OpenAPI 3.2.1 | Modelo de objetos do SwagDoc |
|------|-------------|---------------|----------------------|
| Campo de versão | `swagger: "2.0"` | `openapi: "3.2.1"` | `SpecVersion` |
| Identidade do documento | nenhuma | `$self`, `jsonSchemaDialect` | `SelfUri`, `JsonSchemaDialect` |
| Servidor de destino | `host`, `basePath`, `schemes` | `servers` com templates de URL, variáveis e nome | `Servers`, `AddServer`, `TSwagServer.Name` |
| Media types | `consumes`, `produces` | mapa `content` em cada corpo da requisição, resposta, parâmetro e cabeçalho | `AddMediaType` de `RequestBody`, `TSwagResponse`, `TSwagRequestParameter`, `TSwagHeaders` |
| Payload da requisição | parâmetros `body` e `formData` | `requestBody` | `TSwagPathOperation.RequestBody` |
| Objetos reutilizáveis | `definitions`, `parameters`, `responses`, `securityDefinitions` | `components` com schemas, responses, parameters, examples, requestBodies, headers, securitySchemes, links, callbacks, pathItems, mediaTypes | `Definitions`, `Responses`, `Parameters`, `Examples`, `RequestBodies`, `Headers`, `SecurityDefinitions`, `Links`, `Callbacks`, `PathItems`, `MediaTypes` |
| Schemas | Schema object, extensão `x-nullable` | JSON Schema 2020-12 (arrays em `type`, limites exclusivos numéricos, array `examples`) | Convertidos automaticamente |
| Localizações de parâmetros | query, header, path, formData, body | query, header, path, cookie, querystring | `rpiCookie`, `rpiQueryString` |
| Serialização de parâmetros | `collectionFormat` | `style`, `explode`, `allowReserved`, `content` | `Style`, `Explode`, `AllowReserved`, `Content` |
| Métodos HTTP | get, put, post, delete, options, head, patch | adiciona trace, query e `additionalOperations` | `ohvTrace`, `ohvQuery`, `AddAdditionalOperation` |
| Exemplos | `examples` de uma resposta, por tipo MIME | Example Object com `dataValue`, `serializedValue`, `externalValue` | `TSwagExample` |
| Cabeçalhos | type e format | Header Object com schema, content, examples, required, deprecated | `TSwagHeaders` |
| Relações entre operações | nenhuma | links e callbacks | `TSwagLink`, `TSwagCallback` |
| Eventos enviados pela API | nenhum | `webhooks` | `Webhooks` |
| Esquemas de segurança | basic, apiKey, oauth2 (um fluxo) | http, apiKey (também em cookie), oauth2 (vários fluxos), openIdConnect, mutualTLS | `TSwagSecurityDefinitionHttp`, `...ApiKey`, `...OAuth2`, `...OpenIdConnect`, `...MutualTls` |
| Requisitos de segurança | escopos somente para OAuth2 | escopos para todos os esquemas, combinações AND e OR | `TSwagSecurityRequirement` |
| Tags | name, description, externalDocs | adiciona summary, parent (tags aninhadas) e kind | `Summary`, `Parent`, `Kind` |
| Extensões | campos `x-` | campos `x-` em todos os objetos | Propriedade `Extensions` |

### 3.2 Novidades por versão

**OpenAPI 3.0**

- Servers, components, corpos de requisição, content e media types, encoding de conteúdos multipart e de formulário.
- Callbacks, links, parâmetros de cookie, `style` e `explode`.
- Esquemas de segurança HTTP e OpenID Connect, o Example Object e o Header Object com schema.

**OpenAPI 3.1**

- Alinhamento completo com o JSON Schema 2020-12: valores nulos são gravados como arrays em `type`, `exclusiveMinimum` e `exclusiveMaximum` são números, e os schemas aceitam um array `examples`.
- Webhooks, `info.summary`, `license.identifier` (SPDX), `components.pathItems`, o esquema de segurança mutual TLS e `jsonSchemaDialect`.
- `summary` e `description` junto a um `$ref` substituem os valores referenciados, e os requisitos de segurança aceitam roles para todos os esquemas.

**OpenAPI 3.2**

- `$self`, o método QUERY, `additionalOperations` e a localização de parâmetro `querystring`.
- `summary`, `parent` e `kind` de tag, `name` de server e `summary` de resposta.
- `itemSchema` de media type para media types sequenciais (JSON Lines, server-sent events), `prefixEncoding`, `itemEncoding` e `components.mediaTypes`.
- `dataValue` e `serializedValue` de exemplo.
- Fluxo OAuth2 `deviceAuthorization`, `oauth2MetadataUrl` e o campo `deprecated` dos esquemas de segurança.

**OpenAPI 3.2.1** é uma versão de correção (patch) da 3.2, com esclarecimentos e correções. Não adiciona novos objetos.

## 4. Compatibilidade e mudanças incompatíveis

### 4.1 O que permanece igual

- `TSwagDoc` inicia com `SpecVersion = svSwagger2`, portanto `GenerateSwaggerJson` grava Swagger 2.0 como antes.
- Os documentos construídos em código pelas demos do repositório (`SampleApi`, `GenerateSwaggerJsonFromCode`) são idênticos, byte a byte, à saída da versão anterior. O gerador de Swagger 2.0 só ganhou caminhos de código para propriedades que não existiam antes.
- As units usadas pelas aplicações existentes continuam disponíveis. `Swag.Doc.Path` e `Swag.Doc.Path.Operation.ResponseHeaders` continuam declarando `TSwagPath` e `TSwagHeaders` como aliases das classes que foram movidas (seção 4.3).
- `TJsonSchema` e seus campos produzem os mesmos schemas. A propriedade `Nullable` e o `Format` dos campos string são novos e opcionais.

### 4.2 Mudanças na saída de documentos carregados de um arquivo swagger.json

Quando um arquivo Swagger 2.0 é carregado com `LoadFromFile` e gerado novamente como Swagger 2.0, a versão anterior perdia parte do documento. São correções, mas o arquivo gerado é diferente:

| Conteúdo do arquivo carregado | Versão anterior | Versão atual |
|----------------------------|------------------|-----------------|
| `securityDefinitions` | Não gravado | Gravado |
| `security` do documento e das operações | Não gravado, ou gravado sem escopos | Gravado com os escopos (por exemplo `["write:pets", "read:pets"]`) |
| `externalDocs` do documento | Não gravado | Gravado |
| `items` e `enum` de parâmetros array | Perdidos | Gravados |
| `allowEmptyValue` | Gravado como `true` para parâmetros query que não o declaravam | Gravado somente quando declarado |
| `responses` do documento e extensões `x-` | Perdidos | Gravados |

Se algum processo posterior compara o arquivo gerado com uma cópia armazenada, atualize a cópia armazenada depois da atualização.

### 4.3 Mudanças no código-fonte que podem exigir alterações no código

#### Enumerações estendidas

| Enumeração | Valores adicionados |
|-------------|--------------|
| `TSwagSecurityDefinitionType` | `ssdHttp`, `ssdOpenIdConnect`, `ssdMutualTls` |
| `TSwagRequestParameterInLocation` | `rpiCookie`, `rpiQueryString` |
| `TSwagPathTypeOperation` | `ohvQuery` |
| `TSwagSecurityDefinitionApiKeyInLocation` | `kilCookie` |
| Novos tipos | `TSwagVersion`, `TSwagRequestParameterStyle`, `TSwagOAuth2FlowType` |

Verifique no seu código:

- Arrays constantes indexados por uma dessas enumerações, como `array[TSwagRequestParameterInLocation] of string`. Eles deixam de compilar (E2072, o número de elementos difere) e precisam dos novos valores.
- Loops de `Low` a `High` dessas enumerações. Agora eles percorrem os novos valores; por exemplo, um gerador de código cliente passaria a receber o método `query`.
- Instruções `case` sem ramo `else` que deveriam tratar todos os valores.

#### Definições de segurança personalizadas

`TSwagSecurityDefinition` ganhou overloads e novos membros virtuais. Uma classe que herda dela continua compilando, mas adicione a diretiva `overload` para que os novos overloads continuem visíveis, e chame `inherited` no seu destructor, pois a classe base agora é dona do objeto `Extensions`.

```delphi
// Versão anterior
TMySecurityDefinition = class(TSwagSecurityDefinition)
protected
  function GetTypeSecurity: TSwagSecurityDefinitionType; override;
public
  function GenerateJsonObject: TJSONObject; override;
  procedure Load(pJson: TJSONObject); override;
end;

// Versão atual
TMySecurityDefinition = class(TSwagSecurityDefinition)
protected
  function GetTypeSecurity: TSwagSecurityDefinitionType; override;
public
  function GenerateJsonObject: TJSONObject; overload; override;
  function GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject; overload; override;
  procedure Load(pJson: TJSONObject); overload; override;
  procedure Load(pJson: TJSONObject; const pVersion: TSwagVersion); overload; override;
  function SupportsVersion(const pVersion: TSwagVersion): Boolean; override;
end;
```

- `GenerateJsonObject(pVersion)` e `Load(pJson, pVersion)` chamam, por padrão, os métodos sem versão; portanto, sobrescreva-os somente quando a representação no OpenAPI 3 for diferente.
- Retorne `False` em `SupportsVersion` para uma família que não consegue representar o esquema. O esquema e os requisitos que o utilizam são então removidos dessa família.
- `Description`, `Deprecated` e `Extensions` estão disponíveis para todos os esquemas.

#### Classes que foram movidas para outras units

| Classe | Unit anterior | Unit atual | A unit anterior ainda funciona? |
|-------|---------------|--------------|----------------------------|
| `TSwagPath` | `Swag.Doc.Path` | `Swag.Doc.Path.Operation` | Sim, `Swag.Doc.Path` declara um alias |
| `TSwagHeaders` | `Swag.Doc.Path.Operation.ResponseHeaders` | `Swag.Doc.Path.Operation.Content` | Sim, `ResponseHeaders` declara um alias |

Os aliases são o mesmo tipo, portanto `is`, `as` e as variáveis existentes continuam funcionando.

#### Novas units

Aplicações que adicionam a pasta `Source` ao library path não precisam de mais nada. Packages ou projetos que listam as units do SwagDoc explicitamente devem adicionar:

| Unit | Conteúdo |
|------|---------|
| `Swag.Common.Json` | Funções auxiliares de leitura de JSON |
| `Swag.Doc.Extensions` | Extensões da especificação (`TSwagExtensions`) |
| `Swag.Doc.Example` | Example Object (`TSwagExample`) |
| `Swag.Doc.Link` | Link Object (`TSwagLink`) |
| `Swag.Doc.SecurityRequirement` | Requisitos de segurança com escopos |
| `Swag.Doc.Server` | Server Object e variáveis de servidor |
| `Swag.Doc.Path.Operation.Content` | Media types, encodings e cabeçalhos |
| `Swag.Doc.Path.Operation.RequestBody` | Request Body Object |
| `Swag.Doc.SecurityDefinitionHttp` | Esquema de segurança HTTP (basic, bearer e outros) |
| `Swag.Doc.SecurityDefinitionOpenIdConnect` | Esquema de segurança OpenID Connect |
| `Swag.Doc.SecurityDefinitionMutualTls` | Esquema de segurança mutual TLS |
| `Swag.Doc.JsonConverter` | Conversão de referências e palavras-chave de schema entre as famílias |
| `Swag.Doc.OpenApi.Generator` | Gravador de OpenAPI 3 |
| `Swag.Doc.OpenApi.Loader` | Leitor de OpenAPI 3 |

#### Outros comportamentos importantes

- `LoadFromFile` define `SpecVersion` de acordo com o arquivo carregado. Depois de carregar um arquivo OpenAPI 3, defina `SpecVersion := svSwagger2` se for esperado um documento Swagger 2.0.
- `Extensions.Add` gera `ESwagErrorExtensionName` quando o nome não começa com `x-`.
- Propriedades que recebem valores JSON (`Example`, `DataValue`, `Value`, `Items`, `RequestBody` de um link, `ItemEncoding`) assumem a posse do objeto atribuído. Não o libere.
- `Deploy\index.html`, `Deploy\swagger.json` e os arquivos do Swagger UI da raiz de `Deploy` foram movidos para `Deploy\Swagger2`. Atualize os scripts que os copiam.

## 5. Migração passo a passo

1. Atualize os fontes do SwagDoc e recompile. Se você instala o `Source\SwagDoc.dpk`, recompile o package e remova os arquivos `.dcu` e `.bpl` antigos das suas pastas de saída.
2. Corrija os casos de código-fonte da seção 4.3, se houver.
3. Compile a aplicação sem alterar `SpecVersion` e compare o `swagger.json` gerado com o anterior (seção 9.1).
4. Defina `SpecVersion := svOpenApi3`, gere o `openapi.json` e valide-o (seção 9.2).
5. Faça o deploy dos arquivos do Swagger UI 5 de `Deploy\OpenApi3` e configure a URL do documento (seção 8).
6. Teste a página no navegador: operações, schemas, a janela Authorize e as requisições Try it out.
7. Opcionalmente, publique os dois documentos durante um período de transição (seção 6.15).
8. Migre o modelo para os objetos do OpenAPI 3 onde eles melhorarem a documentação (seção 6).
9. Atualize os consumidores da API: geradores de clientes, gateways e testes de contrato que leem o documento.

## 6. Migração do código

Cada tópico mostra a construção do Swagger 2.0, que continua válida, e a construção do OpenAPI 3. As cláusulas uses listam apenas as units necessárias para o exemplo.

### 6.1 Trocando a versão e mantendo a URL

```delphi
vSwagDoc.SpecVersion := svOpenApi3;
vSwagDoc.GenerateSwaggerJson;
vSwagDoc.SwaggerFilesFolder := 'C:\MyApi\Help';
vSwagDoc.SwaggerFileName := 'swagger.json'; // opcional: mantém o nome de arquivo anterior
vSwagDoc.SaveSwaggerJsonToFile;
```

### 6.2 Host, base path e schemes se tornam servers

Construção do Swagger 2.0:

```delphi
vSwagDoc.Host := 'api.example.com';
vSwagDoc.BasePath := '/v1';
vSwagDoc.Schemes := [tpsHttps];
```

Quando a lista `Servers` está vazia, o OpenAPI 3 recebe um server por scheme: `https://api.example.com/v1`. Com servers:

```delphi
uses
  Swag.Doc.Server;

var
  vServer: TSwagServer;
  vVariable: TSwagServerVariable;
begin
  vServer := vSwagDoc.AddServer('https://{environment}.example.com/v1', 'Cloud server');
  vServer.Name := 'cloud';
  vVariable := vServer.AddVariable('environment', 'api', 'The environment of the API.');
  vVariable.Enum.Add('api');
  vVariable.Enum.Add('sandbox');

  vSwagDoc.AddServer('http://localhost:8080/v1', 'Local development server');
end;
```

> Os servers não são gravados no Swagger 2.0. Se você também publica o `swagger.json`, mantenha `Host`, `BasePath` e `Schemes`. Quando `Servers` tem itens, o OpenAPI 3 ignora essas três propriedades. `TSwagPath` e `TSwagPathOperation` também têm uma lista `Servers`.

### 6.3 Parâmetros body se tornam um corpo da requisição

Construção do Swagger 2.0:

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'employee';
vParameter.InLocation := rpiBody;
vParameter.Required := True;
vParameter.Schema.Name := 'Employee';
vOperation.Parameters.Add(vParameter);
vOperation.Consumes.Add('application/json');
```

O OpenAPI 3 recebe um `requestBody` com um media type para cada tipo MIME consumido. Com o objeto de corpo da requisição, cada media type pode ter o seu próprio schema:

```delphi
vOperation.RequestBody.Description := 'The employee data.';
vOperation.RequestBody.Required := True;
vOperation.RequestBody.AddMediaType('application/json').Schema.Name := 'Employee';
vOperation.RequestBody.AddMediaType('application/xml').Schema.Name := 'Employee';
```

No Swagger 2.0, o corpo da requisição é gravado como um parâmetro body com o schema do primeiro media type, e seus media types vão para `consumes`. Isso só acontece quando a operação não tem parâmetro body nem formData.

### 6.4 Parâmetros formData se tornam um corpo da requisição de formulário ou multipart

Construção do Swagger 2.0:

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'file';
vParameter.InLocation := rpiFormData;
vParameter.TypeParameter := stpFile;
vParameter.Required := True;
vOperation.Parameters.Add(vParameter);
```

O OpenAPI 3 recebe um corpo da requisição `multipart/form-data` com um schema de objeto, e o tipo `file` se torna `type: string, format: binary`. Com o objeto de corpo da requisição:

```delphi
uses
  Swag.Doc.Path.Operation.Content;

var
  vMediaType: TSwagMediaType;
begin
  vMediaType := vOperation.RequestBody.AddMediaType('multipart/form-data');
  vMediaType.Schema.JsonSchema := TJSONObject.ParseJSONValue(
    '{"type":"object","properties":{"title":{"type":"string"},' +
    '"file":{"type":"string","contentMediaType":"application/pdf"}},"required":["file"]}') as TJSONObject;
  vMediaType.AddEncoding('file').ContentType := 'application/pdf';
end;
```

No Swagger 2.0, um corpo da requisição de formulário ou multipart é gravado como parâmetros formData. Uma propriedade com `format: binary` ou `contentMediaType` se torna um parâmetro `file`, e uma propriedade `object` se torna um parâmetro `string`.

### 6.5 Produces e schemas de resposta se tornam content

Construção do Swagger 2.0:

```delphi
vOperation.Produces.Add('application/json');

vResponse := TSwagResponse.Create;
vResponse.StatusCode := '200';
vResponse.Description := 'The employee.';
vResponse.Schema.Name := 'Employee';
vOperation.Responses.Add(vResponse.StatusCode, vResponse);
```

O OpenAPI 3 recebe o schema no `content` da resposta para cada media type produzido (`application/json` quando nada é produzido). Com content por media type:

```delphi
vResponse.AddMediaType('application/json').Schema.Name := 'Employee';
vResponse.AddMediaType('application/jsonl').ItemSchema.Name := 'Employee'; // um funcionário por linha
```

- Quando `Content` tem itens, `Produces` e `Schema` não são usados para essa resposta no OpenAPI 3.
- No Swagger 2.0, o schema do primeiro media type é gravado quando `Schema` está vazio.
- `Summary` e `Links` de uma resposta são gravados somente no OpenAPI 3.

### 6.6 Cabeçalhos de resposta

A construção do Swagger 2.0 funciona nas duas famílias:

```delphi
vHeader := vResponse.AddHeader('X-Rate-Limit-Remaining');
vHeader.Description := 'The number of requests left in the current period.';
vHeader.ValueType := 'integer';
```

O Header Object do OpenAPI 3 aceita um schema, exemplos, `Required`, `Deprecated`, `Style`, `Explode` e `Content`:

```delphi
vHeader := vResponse.AddHeader('X-Rate-Limit-Remaining');
vHeader.Description := 'The number of requests left in the current period.';
vHeader.Schema.JsonSchema := TJSONObject.Create.AddPair('type', 'integer');
vHeader.Example := TJSONNumber.Create(99);
```

No Swagger 2.0, `ValueType` e `Format` são obtidos do schema quando estão vazios. Cabeçalhos que são referências não são gravados no Swagger 2.0.

### 6.7 Exemplos

Construção do Swagger 2.0:

```delphi
vResponse.Examples.Add('application/json', TJSONObject.ParseJSONValue('{"id":42,"name":"John Smith"}') as TJSONObject);
```

O OpenAPI 3 recebe um Example Object por entrada, dentro do content de cada media type produzido. Com o Example Object:

```delphi
uses
  Swag.Doc.Example, Swag.Doc.Path.Operation.Content;

var
  vMediaType: TSwagMediaType;
  vExample: TSwagExample;
begin
  vMediaType := vResponse.AddMediaType('application/json');
  vMediaType.Schema.Name := 'Employee';
  vExample := vMediaType.AddExample('johnSmith');
  vExample.Summary := 'An employee';
  vExample.DataValue := TJSONObject.ParseJSONValue('{"id":42,"name":"John Smith"}');

  vMediaType := vResponse.AddMediaType('text/csv');
  vMediaType.AddExample('csv').SerializedValue := 'id,name'#10'42,John Smith';
end;
```

- Use apenas um entre `DataValue`, `SerializedValue` e `ExternalValue`. `Value` ainda é aceito, mas não deve ser combinado com `DataValue` ou `SerializedValue`.
- Exemplos reutilizáveis vão para `vSwagDoc.Examples` e são referenciados com `Ref := '#/components/examples/Name'`.
- No Swagger 2.0, o primeiro exemplo de cada media type é gravado em `examples` da resposta.

### 6.8 Componentes reutilizáveis e referências

As referências escritas para o Swagger 2.0 são reescritas nas duas direções:

| Referência no Swagger 2.0 | Referência no OpenAPI 3 |
|--------------------------|------------------------|
| `#/definitions/Name` | `#/components/schemas/Name` |
| `#/parameters/Name` | `#/components/parameters/Name`, ou `#/components/requestBodies/Name` para parâmetros body e formData |
| `#/responses/Name` | `#/components/responses/Name` |

Prefira `Schema.Name` a referências escritas à mão: o SwagDoc grava o caminho correto para cada família. Os componentes que não existem no Swagger 2.0 (examples, headers, links, callbacks, path items e media types) devem ser referenciados com o caminho do OpenAPI 3.

```delphi
vResponse := TSwagResponse.Create;
vResponse.Name := 'notFound';                       // chave em components/responses
vResponse.Description := 'The employee was not found.';
vResponse.AddMediaType('application/problem+json').Schema.Name := 'Problem';
vSwagDoc.Responses.Add(vResponse);

vResponse := TSwagResponse.Create;
vResponse.StatusCode := '404';
vResponse.Ref := '#/components/responses/notFound';
vResponse.Summary := 'Unknown employee';             // substitui o summary referenciado
vOperation.Responses.Add(vResponse.StatusCode, vResponse);
```

| Propriedade de TSwagDoc | Campo de `components` | Chave de cada item |
|-------------------|--------------------|------------------|
| `Definitions` | `schemas` | `Name` |
| `Responses` | `responses` | `Name` |
| `Parameters` | `parameters` | `Name` |
| `Examples` | `examples` | `Name` |
| `RequestBodies` | `requestBodies` | `Name` |
| `Headers` | `headers` | `Name` |
| `SecurityDefinitions` | `securitySchemes` | `SchemeName` |
| `Links` | `links` | `Name` |
| `Callbacks` | `callbacks` | `Name` |
| `PathItems` | `pathItems` | `Uri` |
| `MediaTypes` | `mediaTypes` | `Name` |

A `Description` gravada junto com o `Ref` de um parâmetro, corpo da requisição, resposta ou cabeçalho substitui a referenciada no OpenAPI 3, assim como o `Summary` de uma resposta.

### 6.9 Esquemas e requisitos de segurança

#### Autenticação basic e bearer

```delphi
// Construção do Swagger 2.0, ainda válida: gravada como http/basic no OpenAPI 3
vBasic := TSwagSecurityDefinitionBasic.Create;
vBasic.SchemeName := 'basicAuth';
vSwagDoc.SecurityDefinitions.Add(vBasic);

// Alternativa do Swagger 2.0 para tokens bearer
vApiKey := TSwagSecurityDefinitionApiKey.Create;
vApiKey.SchemeName := 'bearerAuth';
vApiKey.InLocation := kilHeader;
vApiKey.Name := 'Authorization';
vSwagDoc.SecurityDefinitions.Add(vApiKey);

// OpenAPI 3: esquema HTTP bearer (gravado como a API key acima no Swagger 2.0)
vBearer := TSwagSecurityDefinitionHttp.Create;
vBearer.SchemeName := 'bearerAuth';
vBearer.Scheme := 'bearer';
vBearer.BearerFormat := 'JWT';
vSwagDoc.SecurityDefinitions.Add(vBearer);
```

#### Fluxos OAuth2

Construção do Swagger 2.0, com um fluxo:

```delphi
vOAuth2 := TSwagSecurityDefinitionOAuth2.Create;
vOAuth2.SchemeName := 'petstore_auth';
vOAuth2.Flow := 'accessCode';
vOAuth2.AuthorizationUrl := 'https://auth.example.com/authorize';
vOAuth2.TokenUrl := 'https://auth.example.com/token';
vOAuth2.AddScope('read:pets', 'Read your pets');
vSwagDoc.SecurityDefinitions.Add(vOAuth2);
```

Os nomes dos fluxos são traduzidos entre as famílias:

| Swagger 2.0 | OpenAPI 3 | TSwagOAuth2FlowType |
|-------------|-----------|---------------------|
| `implicit` | `implicit` | `oftImplicit` |
| `password` | `password` | `oftPassword` |
| `application` | `clientCredentials` | `oftClientCredentials` |
| `accessCode` | `authorizationCode` | `oftAuthorizationCode` |
| nenhum | `deviceAuthorization` | `oftDeviceAuthorization` |

Construção do OpenAPI 3, com vários fluxos:

```delphi
uses
  Swag.Doc.SecurityDefinitionOAuth2;

var
  vOAuth2: TSwagSecurityDefinitionOAuth2;
  vFlow: TSwagSecurityDefinitionOAuth2Flow;
begin
  vOAuth2 := TSwagSecurityDefinitionOAuth2.Create;
  vOAuth2.SchemeName := 'oauth2Auth';
  vOAuth2.OAuth2MetadataUrl := 'https://auth.example.com/.well-known/oauth-authorization-server';

  vFlow := vOAuth2.AddFlow(oftAuthorizationCode);
  vFlow.AuthorizationUrl := 'https://auth.example.com/authorize';
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:write', 'Creates and updates the employees');

  vFlow := vOAuth2.AddFlow(oftClientCredentials);
  vFlow.TokenUrl := 'https://auth.example.com/token';
  vFlow.AddScope('employees:read', 'Reads the employees');

  vSwagDoc.SecurityDefinitions.Add(vOAuth2);
end;
```

No Swagger 2.0, é gravado o primeiro fluxo que existe no Swagger 2.0. Quando `Flows` está vazio, as propriedades de fluxo único são usadas nas duas famílias.

#### Requisitos de segurança

Construção do Swagger 2.0, sem escopos:

```delphi
vOperation.Security.Add('petstore_auth');
```

Construção do OpenAPI 3, com escopos e combinações:

```delphi
uses
  Swag.Doc.SecurityRequirement;

var
  vRequirement: TSwagSecurityRequirement;
begin
  // documento: token bearer OU o escopo de leitura do OAuth2
  vSwagDoc.AddSecurityRequirement.AddScheme('bearerAuth', []);
  vSwagDoc.AddSecurityRequirement.AddScheme('oauth2Auth', ['employees:read']);

  // operação: o escopo de escrita E um certificado de cliente
  vRequirement := vOperation.AddSecurityRequirement;
  vRequirement.AddScheme('oauth2Auth', ['employees:write']);
  vRequirement.AddScheme('mutualTlsAuth', []);

  // operação sem autenticação: security: []
  vHealthOperation.DisableSecurity := True;
end;
```

Cada `AddSecurityRequirement` é uma alternativa (OR) e os esquemas de um requisito são todos obrigatórios (AND). Os requisitos são gravados nas duas famílias. A precedência é:

| Nível | Primeira opção | Segunda opção | Terceira opção |
|-------|--------------|---------------|--------------|
| Documento | `SecurityRequirements` | `DisableSecurity` grava `[]` | `GlobalSecurityFromDefinitions` grava todas as definições de segurança como alternativas sem escopos |
| Operação | `SecurityRequirements` | Lista `Security`, sem escopos | `DisableSecurity` grava `[]` |

> `GlobalSecurityFromDefinitions` é False por padrão, então um documento sem requisitos não grava um `security` próprio e cada operação mantém a segurança que declara. Defina como True para tornar cada esquema uma forma alternativa de chamar a API inteira.

#### Esquemas em um documento Swagger 2.0

| Esquema no modelo | Saída no Swagger 2.0 |
|---------------------|--------------------|
| HTTP basic | `type: basic` |
| HTTP bearer ou outro esquema HTTP | `type: apiKey`, `in: header`, `name: Authorization` |
| OpenID Connect | `type: apiKey` no cabeçalho `Authorization` com a extensão `x-openIdConnectUrl` |
| API key em um cookie | Removido, junto com os requisitos que o utilizam |
| Mutual TLS | Removido, junto com os requisitos que o utilizam |
| OAuth2 apenas com o fluxo device authorization | Removido, junto com os requisitos que o utilizam |
| `Deprecated` de qualquer esquema | Não gravado |

### 6.10 Schemas e valores nulos

```delphi
vPhone := vSchema.AddField<string>('phone', 'The employee phone number.');
vPhone.Nullable := True;
```

As palavras-chave de schema são convertidas para a família que está sendo gerada, inclusive nos JSON schemas atribuídos manualmente a `JsonSchema`:

| Construção | Saída no Swagger 2.0 | Saída no OpenAPI 3.2.1 |
|--------------|--------------------|----------------------|
| Campo `Nullable` ou `nullable: true` | Não gravado, ou `x-nullable: true` quando `WriteNullableExtension` é True | Array em `type` que inclui `"null"` |
| Referência anulável | Apenas a referência, ou `allOf` com `x-nullable` quando `WriteNullableExtension` é True | `anyOf` com a referência e `type: "null"` |
| `x-nullable: true` lido de um documento Swagger 2.0 | Mantido | Array em `type` que inclui `"null"` |
| `exclusiveMinimum` e `exclusiveMaximum` | Booleanos, junto com `minimum` e `maximum` | Limites numéricos |
| `type: file` | Mantido | `type: string`, `format: binary` |
| Array `examples` do schema | Primeiro item como `example` | Mantido |

Um documento OpenAPI 3.0 carregado com `LoadFromFile` é atualizado quando é gerado novamente.

### 6.11 Parâmetros

| Propriedade | Uso | Saída no Swagger 2.0 |
|----------|-----|--------------------|
| `InLocation := rpiCookie` | Parâmetro enviado em um cookie | Não gravado |
| `InLocation := rpiQueryString` | A query string inteira como um único parâmetro, com `Content` (padrão `application/x-www-form-urlencoded`) | Não gravado |
| `Style`, `Explode`, `AllowReserved` | Serialização de arrays e objetos | Não gravado |
| `Deprecated` | Parâmetro em processo de descontinuação | Não gravado |
| `Example`, `Examples` | Exemplos do valor | Não gravado |
| `Content` | Valores complexos, por exemplo JSON em um parâmetro query | Não gravado |
| `Schema` | Qualquer schema, em qualquer localização | Gravado somente para parâmetros body |
| `Description` com `Ref` | Substitui a descrição referenciada | Não gravado |

```delphi
vParameter := TSwagRequestParameter.Create;
vParameter.Name := 'department';
vParameter.InLocation := rpiQuery;
vParameter.TypeParameter := stpString;
vParameter.Style := rpsForm;
vParameter.Explode := True;
vExample := vParameter.AddExample('sales');
vExample.Summary := 'Sales department';
vExample.DataValue := TJSONString.Create('sales');
vOperation.Parameters.Add(vParameter);
```

### 6.12 Objetos que existem apenas no OpenAPI 3

| Recurso | Código | Saída no Swagger 2.0 |
|---------|------|--------------------|
| URI do documento | `vSwagDoc.SelfUri := 'https://api.example.com/v1/openapi.json'` | Não gravado |
| Dialeto de schema | `vSwagDoc.JsonSchemaDialect := '...'` | Não gravado |
| Summary do info, licença SPDX | `Info.Summary`, `Info.License.Identifier` | Summary não gravado; o identificador se torna a URL da licença quando `Url` está vazio |
| Tags aninhadas | `Tag.Summary`, `Tag.Parent`, `Tag.Kind` | Não gravado |
| Método QUERY | `vPath.AddOperation(ohvQuery)` | Operação não gravada |
| Outros métodos | `vPath.AddAdditionalOperation('LINK')` | Não gravado |
| Path item reutilizável | `vSwagDoc.PathItems` e `vPath.Ref := '#/components/pathItems/Name'` | O path item referenciado é gravado inline |
| Webhooks | `vSwagDoc.Webhooks.Add(vWebhook)` | Não gravado |
| Callbacks | `vOperation.AddCallback('Name').AddPathItem('{$request.body#/callbackUrl}')` | Não gravado |
| Links | `vResponse.AddLink('Name')` | Não gravado |
| Media types sequenciais | `vMediaType.ItemSchema` | Não gravado |
| Encodings | `AddEncoding`, `AddPrefixEncoding`, `ItemEncoding` | Não gravado |
| Extensões da especificação | `Extensions.Add('x-name', vValue)` | Gravadas para os objetos que existem no Swagger 2.0 |

```delphi
vLink := vResponse.AddLink('GetEmployeeById');
vLink.OperationId := 'getEmployee';
vLink.AddParameter('id', '$response.body#/id');

vCallback := vOperation.AddCallback('exportCompleted');
vCallbackOperation := vCallback.AddPathItem('{$request.body#/callbackUrl}').AddOperation(ohvPost);
vCallbackOperation.RequestBody.AddMediaType('application/json').Schema.Name := 'ExportResult';

vWebhook := TSwagPath.Create;
vWebhook.Uri := 'employeeHired';
vWebhook.AddOperation(ohvPost).RequestBody.AddMediaType('application/json').Schema.Name := 'Employee';
vSwagDoc.Webhooks.Add(vWebhook);
```

### 6.13 Convertendo um arquivo swagger.json existente

```delphi
procedure ConvertToOpenApi3(const pSwaggerFile, pOutputFolder: string);
var
  vSwagDoc: TSwagDoc;
begin
  vSwagDoc := TSwagDoc.Create;
  try
    vSwagDoc.LoadFromFile(pSwaggerFile);   // SpecVersion passa a ser svSwagger2
    vSwagDoc.SpecVersion := svOpenApi3;
    vSwagDoc.GenerateSwaggerJson;
    vSwagDoc.SwaggerFilesFolder := pOutputFolder;
    vSwagDoc.SaveSwaggerJsonToFile;         // openapi.json
  finally
    vSwagDoc.Free;
  end;
end;
```

A direção oposta também funciona. Quando um documento OpenAPI 3 é carregado, `Host`, `BasePath` e `Schemes` são preenchidos a partir do primeiro server, substituindo suas variáveis pelos valores padrão.

### 6.14 Definições de segurança personalizadas

Consulte a seção 4.3. Um esquema que tem uma representação diferente no OpenAPI 3 sobrescreve `GenerateJsonObject(pVersion)`:

```delphi
function TMySecurityDefinition.GenerateJsonObject(const pVersion: TSwagVersion): TJSONObject;
begin
  if pVersion <> svOpenApi3 then
    Exit(GenerateJsonObject);

  Result := TJSONObject.Create;
  Result.AddPair('type', 'http');
  Result.AddPair('scheme', 'digest');
  if not fDescription.IsEmpty then
    Result.AddPair('description', fDescription);
end;
```

### 6.15 Publicando as duas famílias a partir do mesmo modelo

```delphi
procedure TApiDocumentation.Publish(const pRootFolder: string);
begin
  fSwagDoc.SpecVersion := svSwagger2;
  fSwagDoc.GenerateSwaggerJson;
  fSwagDoc.SwaggerFilesFolder := TPath.Combine(pRootFolder, 'v2');
  fSwagDoc.SaveSwaggerJsonToFile;           // v2\swagger.json

  fSwagDoc.SpecVersion := svOpenApi3;
  fSwagDoc.GenerateSwaggerJson;
  fSwagDoc.SwaggerFilesFolder := TPath.Combine(pRootFolder, 'v3');
  fSwagDoc.SaveSwaggerJsonToFile;           // v3\openapi.json
end;
```

`TPath` é declarado em `System.IOUtils`. Mantenha `Host`, `BasePath` e `Schemes` no modelo para o documento Swagger 2.0 quando usar `Servers`.

## 7. O que um documento Swagger 2.0 não recebe

Quando o modelo usa objetos do OpenAPI 3 e o documento é gerado como Swagger 2.0, o SwagDoc grava um documento Swagger 2.0 válido, traduzindo ou omitindo o que o Swagger 2.0 não consegue representar:

| Modelo de objetos | Comportamento no Swagger 2.0 |
|--------------|----------------------|
| `RequestBody` | Parâmetro body, ou parâmetros formData para conteúdos de formulário e multipart |
| `Content` de uma resposta | Schema e primeiro exemplo do primeiro media type |
| Esquemas HTTP e OpenID Connect | API key no cabeçalho `Authorization` (seção 6.9) |
| OAuth2 com vários fluxos | Primeiro fluxo que existe no Swagger 2.0 |
| Requisitos de segurança com esquemas não suportados | Removidos |
| Parâmetros cookie e querystring | Não gravados |
| Método QUERY, operações adicionais | Não gravados |
| Webhooks, callbacks, links | Não gravados |
| Examples, headers, links, callbacks e media types reutilizáveis | Não gravados |
| Path items reutilizáveis referenciados por um path | Gravados inline no path |
| `Servers` | Não gravados; são usados `Host`, `BasePath` e `Schemes` |
| `SelfUri`, `JsonSchemaDialect`, summary, parent e kind de tag, name de server, summary de resposta | Não gravados |
| Palavras-chave do JSON Schema 2020-12 | Convertidas conforme descrito na seção 6.10 |

## 8. Deploy

### 8.1 Arquivos da biblioteca

O SwagDoc depende apenas da Delphi RTL (`System.JSON`, `System.Generics.Collections`, `System.RegularExpressions` e units relacionadas). Ele foi compilado e testado com o Delphi 12 (Studio 23.0). Há duas formas de usá-lo:

- **Library path.** Adicione a pasta `Source` ao library path ou ao search path do projeto. As novas units são encontradas automaticamente.
- **Package de runtime.** Instale o `Source\SwagDoc.dpk`. Depois de atualizar, recompile o package e remova os arquivos `.dcu` e `.bpl` antigos, para que as novas units sejam compiladas.

Units da versão atual (N marca as units que são novas nesta versão):

| Grupo | Units |
|-------|-------|
| Construtor de JSON Schema | `Json.Common.Helpers`, `Json.Schema`, `Json.Schema.Common.Types`, `Json.Schema.Field`, `Json.Schema.Field.Arrays`, `Json.Schema.Field.Booleans`, `Json.Schema.Field.DateTimes`, `Json.Schema.Field.Enums`, `Json.Schema.Field.Numbers`, `Json.Schema.Field.Objects`, `Json.Schema.Field.Strings` |
| Comuns | `Swag.Common.Consts`, `Swag.Common.Types`, `Swag.Common.Types.Helpers`, `Swag.Common.Json` (N) |
| Documento | `Swag.Doc`, `Swag.Doc.Info`, `Swag.Doc.Info.Contact`, `Swag.Doc.Info.License`, `Swag.Doc.Tags`, `Swag.Doc.Definition`, `Swag.Doc.Server` (N), `Swag.Doc.Extensions` (N), `Swag.Doc.Example` (N), `Swag.Doc.Link` (N) |
| Paths e operações | `Swag.Doc.Path` (alias), `Swag.Doc.Path.Operation`, `Swag.Doc.Path.Operation.RequestParameter`, `Swag.Doc.Path.Operation.RequestBody` (N), `Swag.Doc.Path.Operation.Response`, `Swag.Doc.Path.Operation.ResponseHeaders` (alias), `Swag.Doc.Path.Operation.Content` (N) |
| Segurança | `Swag.Doc.SecurityDefinition`, `Swag.Doc.SecurityDefinitionBasic`, `Swag.Doc.SecurityDefinitionApiKey`, `Swag.Doc.SecurityDefinitionOAuth2`, `Swag.Doc.SecurityDefinitionHttp` (N), `Swag.Doc.SecurityDefinitionOpenIdConnect` (N), `Swag.Doc.SecurityDefinitionMutualTls` (N), `Swag.Doc.SecurityRequirement` (N) |
| Leitura, gravação e conversão | `Swag.Doc.FileLoader`, `Swag.Doc.JsonConverter` (N), `Swag.Doc.OpenApi.Generator` (N), `Swag.Doc.OpenApi.Loader` (N) |

> As classes de definição de segurança se registram na sua seção `initialization`. `LoadFromFile` só as encontra quando suas units estão linkadas, o que `Swag.Doc.FileLoader` já garante. Mantenha essas units no projeto quando as units do SwagDoc forem listadas explicitamente.

### 8.2 Arquivos do Swagger UI

A pasta `Deploy` tem uma pasta por família. Copie a pasta da família que você publica para o servidor web e adicione o documento gerado.

**Deploy\Swagger2 - Swagger UI 3.3.1, para swagger.json**

| Arquivo | Obrigatório | Função |
|------|----------|------|
| `index.html` | Sim | Página e configuração. A opção `url` é `/api/help/swagger.json`; altere-a para o endereço do seu documento |
| `swagger-ui.css` | Sim | Estilos |
| `swagger-ui-bundle.js` | Sim | Swagger UI |
| `swagger-ui-standalone-preset.js` | Sim | Layout da barra superior |
| `oauth2-redirect.html` | Para OAuth2 | Recebe o token da janela Authorize |
| `favicon-16x16.png`, `favicon-32x32.png` | Não | Ícones |
| `swagger-ui.js` | Não | Versão em módulo do Swagger UI, não usada pelo `index.html` |
| `*.map` | Não | Source maps, usados apenas para depurar o Swagger UI |
| `swagger.json` | Sim | Gerado pelo SwagDoc com `svSwagger2` |
| `readme.txt` | Não | Instruções |

A página também carrega fontes de `fonts.googleapis.com`. Sem acesso à internet, a página usa fontes locais.

**Deploy\OpenApi3 - Swagger UI 5.32.15, para openapi.json**

| Arquivo | Obrigatório | Função |
|------|----------|------|
| `index.html` | Sim | Página |
| `index.css` | Sim | Estilos da página |
| `swagger-initializer.js` | Sim | Configuração. A opção `url` é `./openapi.json` |
| `swagger-ui.css` | Sim | Estilos |
| `swagger-ui-bundle.js` | Sim | Swagger UI |
| `swagger-ui-standalone-preset.js` | Sim | Layout da barra superior |
| `oauth2-redirect.html` | Para OAuth2 | Recebe o token da janela Authorize |
| `favicon-16x16.png`, `favicon-32x32.png` | Não | Ícones |
| `LICENSE`, `NOTICE`, `*.LICENSE.txt` | Ao redistribuir | Avisos da Apache License 2.0 do Swagger UI |
| `openapi.json` | Sim | Gerado pelo SwagDoc com `svOpenApi3` |
| `readme.txt` | Não | Instruções |

> O Swagger UI 3.x não consegue renderizar documentos OpenAPI 3.1 ou 3.2. Publique o `openapi.json` com o Swagger UI 5 (os arquivos de `Deploy\OpenApi3`, ou uma versão 5.x mais recente). O Swagger UI 5 também renderiza documentos Swagger 2.0.

### 8.3 Configurando a página

`swagger-initializer.js` de `Deploy\OpenApi3`:

```javascript
window.onload = function() {
  window.ui = SwaggerUIBundle({
    url: "/api/help/openapi.json",        // endereço do documento no seu servidor
    validatorUrl: null,
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [SwaggerUIBundle.presets.apis, SwaggerUIStandalonePreset],
    plugins: [SwaggerUIBundle.plugins.DownloadUrl],
    layout: "StandaloneLayout"
  });
};
```

Para oferecer os dois documentos em uma única página do Swagger UI 5, substitua `url` por `urls`:

```javascript
urls: [
  { url: "/api/help/openapi.json", name: "OpenAPI 3.2.1" },
  { url: "/api/help/swagger.json", name: "Swagger 2.0" }
],
"urls.primaryName": "OpenAPI 3.2.1",
```

### 8.4 Checklist do servidor web

- Sirva a página via HTTP ou HTTPS. Os navegadores bloqueiam a requisição que carrega o documento quando o `index.html` é aberto a partir do sistema de arquivos.
- Sirva os arquivos `.json` com `Content-Type: application/json` e UTF-8.
- Quando o documento for servido por uma origem diferente da página, envie os cabeçalhos CORS (`Access-Control-Allow-Origin`).
- Gere o documento novamente quando a API mudar (na inicialização ou no build) e evite caches desatualizados: envie `Cache-Control: no-cache` para o documento ou adicione uma versão à sua URL.
- Para OAuth2, registre o endereço do `oauth2-redirect.html` como redirect URI no servidor de autorização.
- Proteja a rota da documentação quando a API não for pública.

### 8.5 Estrutura de transição

```text
/api/help/v2/   arquivos de Deploy\Swagger2 + swagger.json   (consumidores existentes)
/api/help/v3/   arquivos de Deploy\OpenApi3 + openapi.json   (novos consumidores)
```

Mantenha as duas até que os consumidores (geradores de clientes, API gateways, testes de contrato) leiam o documento OpenAPI 3 e, então, remova a rota v2.

### 8.6 Mudanças na estrutura do repositório

| Versão anterior | Versão atual |
|------------------|-----------------|
| `Deploy\index.html`, `Deploy\swagger.json` e os arquivos do Swagger UI na raiz de `Deploy` | `Deploy\Swagger2` |
| A demo `SampleApi` grava o seu executável e o `swagger.json` em `Deploy` | Grava-os em `Deploy\Swagger2` |
| nenhuma | `Deploy\OpenApi3` com Swagger UI 5.32.15 e `openapi.json` |
| nenhuma | `Demos\SampleOpenApi3`: a versão OpenAPI 3.2.1 da `SampleApi`, gravando em `Deploy\OpenApi3` |

### 8.7 Publicando o documento pela própria aplicação

Uma API escrita com um framework web publica o documento pelo próprio servidor, em vez de copiar um arquivo para um servidor web. A pasta `Integrations` do repositório tem uma página por framework.

Com o [Horse](https://github.com/HashLoad/horse), o middleware de `Integrations\Horse` publica o documento e a página que o apresenta. Uma aplicação que já o utiliza muda uma propriedade:

```delphi
THorse.Use(HorseSwagDoc);

SwagDocApi.SpecVersion := svOpenApi3;
SwagDocConfig.DocumentRoute := '/docs/openapi.json';
```

Com o [DelphiMVCFramework](https://github.com/danieleteti/delphimvcframework), o SwagDoc é distribuído dentro da pasta `lib/swagdoc` do framework e o middleware `MVCFramework.Middleware.Swagger` monta o documento a cada requisição. Publicar um documento OpenAPI 3 exige uma versão distribuída que declare `TSwagVersion` e o middleware definindo `SpecVersion := svOpenApi3`.

A página que apresenta o documento também precisa entender a família. Os arquivos do Swagger UI publicados por uma aplicação escrita para o Swagger 2.0 são substituídos pela distribuição de `Deploy\OpenApi3`.

## 9. Validação e testes

### 9.1 Regressão do Swagger 2.0

Gere o `swagger.json` com a versão anterior e com a versão atual, sem alterar `SpecVersion`, e compare os arquivos:

```bat
fc /b before\swagger.json after\swagger.json
```

Documentos construídos em código devem ser idênticos. Documentos carregados de arquivos podem ser diferentes, conforme descrito na seção 4.2.

### 9.2 Validação do OpenAPI 3.2.1

- Valide o `openapi.json` com o JSON Schema da especificação OpenAPI 3.2 publicado em https://spec.openapis.org, usando qualquer validador de JSON Schema 2020-12.
- Abra o documento no Swagger UI 5 e verifique as operações, os schemas, a janela Authorize e as requisições Try it out.
- Carregue o arquivo gerado com `LoadFromFile` e gere-o novamente: o resultado deve ser idêntico ao arquivo original.

### 9.3 Validação do Swagger 2.0

- Valide o `swagger.json` no Swagger Editor ou com um validador de Swagger 2.0.
- O Delphi grava a barra das strings JSON como `\/`. Isso é JSON válido, mas ferramentas que interpretam o texto como YAML (o Swagger Editor 4, por exemplo) informam "unknown escape character". Reformate o JSON antes de colá-lo nessas ferramentas. A versão anterior tinha a mesma saída.

## 10. Limitações conhecidas

- O Swagger UI 5.32.15 não lista os webhooks de documentos OpenAPI 3.2 (somente de documentos 3.1) e não exibe `additionalOperations`. Ambos estão presentes no `openapi.json`.
- O Swagger UI 5.32.15 exibe um aviso quando `jsonSchemaDialect` é diferente de `https://spec.openapis.org/oas/3.1/dialect/base`. Deixe `JsonSchemaDialect` vazio, a menos que os schemas usem outro dialeto.
- A conversão para Swagger 2.0 omite os objetos listados na seção 7.
- `Servers` não são convertidos para `host`, `basePath` e `schemes` quando um documento construído em código é gerado como Swagger 2.0.
- Uma classe de definição de segurança personalizada não pode ser lida por `LoadFromFile`, porque os tipos de esquema são uma enumeração fechada.

## 11. Checklist de migração

- [ ] Fontes do SwagDoc atualizados, package recompilado e arquivos `.dcu` e `.bpl` antigos removidos.
- [ ] Arrays, loops e instruções `case` sobre as enumerações estendidas revisados.
- [ ] Definições de segurança personalizadas atualizadas com `overload` e `SupportsVersion`.
- [ ] Novas units adicionadas aos packages que listam as units do SwagDoc.
- [ ] Saída Swagger 2.0 comparada com a da versão anterior.
- [ ] `SpecVersion := svOpenApi3` definido, e `SwaggerFileName` atribuído se a URL não puder mudar.
- [ ] `openapi.json` validado com o schema do OpenAPI 3.2.
- [ ] Arquivos do Swagger UI 5 de `Deploy\OpenApi3` publicados e `swagger-initializer.js` apontando para o documento.
- [ ] Servidor web servindo JSON com o content type correto e com os cabeçalhos de CORS e de cache.
- [ ] Redirect URI do OAuth2 registrada para `oauth2-redirect.html`.
- [ ] Scripts atualizados para as pastas `Deploy\Swagger2` e `Deploy\OpenApi3`.
- [ ] `Host`, `BasePath` e `Schemes` mantidos se o `swagger.json` ainda for publicado.
- [ ] Requisitos de segurança declarados explicitamente, com escopos.
- [ ] Consumidores da API informados e migrados para o documento OpenAPI 3.

## 12. Referências

- Especificação Swagger 2.0: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/2.0.md
- Especificação OpenAPI 3.2.1: https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.2.1.md
- JSON Schema 2020-12: https://json-schema.org/draft/2020-12
- Distribuição do Swagger UI: https://github.com/swagger-api/swagger-ui/tree/master/dist
- Repositório do SwagDoc: https://github.com/marcelojaloto/SwagDoc
- Demos do SwagDoc: `Demos\SampleApi` (Swagger 2.0) e `Demos\SampleOpenApi3` (OpenAPI 3.2.1)
- Aplicações de exemplo que publicam um documento OpenAPI 3.2.1: https://github.com/marcelojaloto/Delphi/tree/master/samples — `server-api-rest-dmvc` (DelphiMVCFramework) e `tasks-manager-horse` (Horse)
