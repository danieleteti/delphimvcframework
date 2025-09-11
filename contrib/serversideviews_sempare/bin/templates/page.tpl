<!DOCTYPE html>
<html>
<header>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">
    <style>
        body {
            padding: 20px 50px 20px 50px;
        }
    </style>
</header>

<body>

    <div id="main" class="container">
        <h1>Server Side Views Primer <small>DMVCFramework</small> with the <small>Sempare Template Engine {{ SempareVersion() }}</small></h1>

        <div class="row_fluid">
                <p>The Sempare Template Engine is available on GitHub at <a target="_blank" href="https://github.com/sempare/sempare-delphi-template-engine">https://github.com/sempare/sempare-delphi-template-engine</a>
                </p>
        </div>

        {{ block 'body' }}content goes here{{ end }}

        <div class="row_fluid">
            <div class="col-sm-12">
                <div style="height: 100px"></div>
            </div>
        </div>
        <div class="row_fluid">
            <div class="col-sm-12">
                <span>N.B. All these views are UTF-8 encoded with BOM</span>
            </div>
            <div class="col-sm-8 bg-primary">
                <span>Powered by DMVCFramework and the <a style="color: yellow" target="_blank" href="https://github.com/sempare/sempare-delphi-template-engine">Sempare Template Engine</a></span>
            </div>
            <div class="col-sm-4 bg-success">
                <span>Server Side Views <a href="/showcase">showcase</a></span>
            </div>
        </div>

        </div>
  </body>
</html>