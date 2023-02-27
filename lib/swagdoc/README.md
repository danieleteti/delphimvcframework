# SwagDoc
SwagDoc is a Delphi library to generate swagger.json file for Swagger Spec version 2.0. Create a public documentation REST API using Swagger 2.0 for Delphi Language. SwagDoc's only responsibility is to generate the swagger.json file. The swagger.json file is responsible for containing all the documentation for your REST API. This file must be attached to the Swagger UI (User Interface) files. 

[![PayPal donate button](https://user-images.githubusercontent.com/26885358/62580349-60bd8780-b87c-11e9-901e-425cf2a83671.png)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AW8TZ2QTDA7K8)


## Swagger (Open API) - version 2.0

SwagDoc follows the specification 2.0 because it is more popular in the market and also because it is considered a more stable version to exist the longest. SwagDoc does not yet support the Swagger 3.0 version, but depending on the demand and contributions to the project it may evolve to support spec 3.0.

The main prerequisite for working with SwagDoc is to know the Swagger 2.0 specification that can be viewed in the link below.

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md

https://swagger.io/docs/specification/2-0/basic-structure/

When creating a Swagger documentation for your REST API you can produce a page like the following example.

https://app.swaggerhub.com/apis-docs/swagdoc/sample-api/v1

![image](https://user-images.githubusercontent.com/20048296/46588904-c6cd5880-ca79-11e8-8a8a-ec38ba7ff95a.png)


## Json Schema

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#schemaObject

http://json-schema.org


## SwagDoc Speeches

https://www.youtube.com/watch?v=9U3HP3B5UT0 (Pt-Br)

https://www.youtube.com/watch?v=PhgMQAd8O6c (Pt-Br)


## Swagger References and Tutorials 

https://swagger.io/swagger/media/blog/wp-content/uploads/2017/02/Documenting-An-Existing-API-with-Swagger-2.pdf

https://idratherbewriting.com/learnapidoc/pubapis_swagger_intro.html


## Swagger Tools

- Swagger:
https://swagger.io

- Swagger Editor:
https://editor.swagger.io

- Swagger Hub:
https://swagger.io/tools/swaggerhub

- The classic swagger sample:
http://petstore.swagger.io

- Tools and Integrations:
https://swagger.io/tools/open-source/open-source-integrations


## Swagger UI distribution files

For you to produce a page containing a Swagger documentation you need the Swagger UI distribution files.

These files you can find in the github swagger-api / swagger-ui repository.

https://github.com/swagger-api/swagger-ui/tree/master/dist

![image](https://user-images.githubusercontent.com/20048296/39937130-2925f868-5525-11e8-921d-c9ff0f59fefd.png)


First you need to download the swagger user interface files and generate the swagger.json file. You then need to change the index.html file to indicate the relative path of the location where the swagger.json file is located on your web server that is hosting the swagger user interface files.

See an example below.

![image](https://user-images.githubusercontent.com/20048296/39946376-49ad0df0-5544-11e8-8a5c-0980f5e6c257.png)
