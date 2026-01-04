# Server Side Views with the Sempare Template Engine

## Introduction

This example illustrates how to use the Sempare Template Engine in the DelphiMVCFramework.

The Sempare Template Engine is available on GitHub (https://github.com/sempare/sempare-delphi-template-engine), Embarcadero's GetIt, boss or Delphinus.

This example is based on the ServerSideViewsMustache example. The functionality is almost exactly the same, but there are subtle differences where the Sempare Template Engine may make some things a little easier to use.

Enjoy.

## Helper bat file to install Sempare Template Engine

In the contrib folder, run the get-sempare-template-engine.bat. NOTE: It does rely on git.exe being installed on your machine in the Windows PATH.

It will create a folder lib/sempare-delphi-template-engine. This demo project already has lib/sempare-delphi-template-engine/src in the search path.

## Call To Action

Please 'star' the Sempare Template Engine on GitHub.

## Licensing

Note that the Sempare Template Engine is distributed under the GPL license and a commercial license.

We adopted open source with the mantra 'free as in speech' - not 'free as in beer'.

By default, if your project uses the Sempare Template Engine, it would be bound by the GPL license, which means you are obliged to distribute the source code for your application as well under the GPL.

The commercial license, which applies once you pay the nominal license fee, helps to keep the project live, and means that you can include the Sempare Template Engine in your applications. More information is available on the Sempare Template Engine repository. It isn't much, and we hope you will contribute to it's support.

Thank you.


## Requirements

In order to use the Sempare Template Engine in your project, you need to:
- ensure you have the Sempare Template Engine/src folder in your search path.
- add <delphimvcframework-path>/contrib to your search path so MVCFrameWork.View.Renderers.Sempare.pas can be referenced.

## Reviewing the demo

### Location of templates

Under bin/templates, there are a number of templates. By default, these have a .tpl extension.

Note that templates can also be embedded into resources. The templates folder could also be at the project root level so that it is easy for other
platforms/configurations to use the templates (assuming you are using the build paths project/platform/configuration). There are options to customise loading templates. There is also an auto-load feature
where templates will refresh without you having to restart your application. This will help while doing template development!

### Enabling the Sempare Template Engine in the demo

In the sample 'WebModuleU.pas', you will see:
- the initialisation on TMVCEngine:
        SetViewEngine(TMVCSempareTemplateEngine)
- add custom functions to templates via:
        Template.Resolver.Context.Functions.AddFunctions(TMySempareHelpers)
- The templates folder is set with the setting:
        Template.Resolver.TemplateRootFolder := 'templates';

The 'templates' directory is actually a default path.

### Endpoint support

The following example is called based on HTTP GET to the /people endpoint and the content type text/html is returned.

```
  1 // [MVCPath('/people')]
  2 // [MVCHTTPMethods([httpGET])]
  3 // [MVCProduces(TMVCMediaType.TEXT_HTML)]
  4
  5 procedure TWebSiteController.PeopleList;
  6 var
  7  LDAL: IPeopleDAL;
  7  lPeople: TPeople;
  9 begin
 10   LDAL := TServicesFactory.GetPeopleDAL;
 11   lPeople := LDAL.GetPeople;
 12   try
 13     ViewData['people'] := lPeople;
 14     LoadView(['people_list']);
 15     RenderResponseStream;
 16   finally
 17     lPeople.Free;
 18   end;
 19 end;
```

The concepts are very similar to the Mustache demo, so lets review the above:
- line 1-3: these are in the interface section on TWebSiteController.PeopleList and describe the HTTP mapping
- line 6: we get the IPeopleDAL database access interface.
- line 7: we query a list of people from the data access layer
- line 9: we populate the ViewData with the list of people.
- line 11: we render he people_list.tpl. This extends 'page.tpl' to wrap with a header and footer.
- line 13: free the list of people

The use of the Sempare Template Engine in this demo is pretty transparent to the developer and that the developer simply needs to set the ViewData and call LoadView.

The template as managed by the template engine can extend other templates, so it is not necessary to expilicity reference headers and footers in the controller
as is done the the Mustache demo. Now, you may think that this means that you have no control over the template header/footer. The example we have been working
with is pretty simple in that there is a simple 'page.tpl'. We could introduce another variable called 'page', which could be set as:
```
        ViewData['page'] := 'page';
```
Then in the template, we can do the following:
```
        {{ extends(page); block 'body' }}
        ...
        {{ end; end }}
```

### Notes on how templates work

There is quite a lot of documentation on
```
https://github.com/sempare/sempare-delphi-template-engine
```

#### dereferencing a variable


```
{{ name }}
```

The above will output whatever is set by view data:

```
        ViewData['name'] := 'Conrad';
```

The template engine uses RTTI to dereference variables on most structures. If there is an issue, a custom dereference method can be added.

Note that the Sempare Template Engine does not pick up attribute MVCNameAs:
```
type
        TPerson = class
                // ...
                [MVCNameAs('first_name')]
                property FirstName: string read FFirstName write SetFirstName;
                // ...
        end;
```

In Mustache, you would dereference as
```
        {{ person.first_name }}
```
With the Sempare Template Engine, you do the same as:
```
        {{ person.FirstName }}
```

The Sempare Template Engine is case insensitive, like Object Pascal / Delphi.



#### loops

```
{{ for i := 1 to 10 }}X{{end}}
```

This will output
```
XXXXXXXXXX
```

#### if

```
{{ if condition }}
hello
{{ elif another }}
hello world
{{ else }}
hello from the UK
{{ end }}
```

You may have variables like:
```
        ViewData['condition'] := false;
        ViewData['another'] := true;
```

# support

If you have any further queries on features, require training or advice, please contact <a href="mailto:info@sempare.ltd">info@sempare.ltd</a>
