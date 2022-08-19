`SynMustache` is a Delphi/FPC implementation of the [Mustache template language](http://mustache.github.io/).


Presentation
============

  * SynMustache is the first Delphi implementation of Mustache, supporting Delphi 6 up to latest Delphi, and FPC/Lazarus;
  * It has a separate parser and renderer (so you can compile your templates ahead of time);
  * The parser features a shared cache of compiled templates;
  * It [passes all official Mustache specification tests](https://github.com/mustache/spec) - including all weird whitespace process;
  * External partials can be supplied as `TSynMustachePartials` dictionaries;
  * `{{.}}`, `{{-index}}` and `{{"some text}}` pseudo-variables were added to the standard Mustache syntax;
  * `{{#-first}}`, `{{#-last}}` and `{{#-odd}}` pseudo-sections were added to the standard Mustache syntax;
  * `{{helperName value}}` *Expression Helpers* were added to the standard Mustache syntax;
  * `{{if value<=>value}}` *Expression Helper* for conditional sections;
  * Internal partials can be defined via `{{<partial}}` - also a nice addition to the standard Mustache syntax;
  * It allows the data context to be supplied as JSON or our `TDocVariant` custom variant type;
  * Almost no memory allocation is performed during the rendering;
  * It is natively UTF-8, from the ground up, with optimized conversion of any string data;
  * Performance has been tuned, with benefit from `SynCommons` optimized code;
  * Each parsed template is thread-safe and re-entrant;
  * It follows the SOLID Open/Close principle so that any aspect of the process can be customized and extended (e.g. for any kind of data context);
  * It is perfectly integrated with the other bricks of our *mORMot* framework, ready to implement dynamic web sites with true MVC design, and full separation of concerns in the views written in Mustache, the controllers being e.g. interface-based services;
  * API is flexible and easy to use.

Get It
======

The version here on GitHub should be in synch with our main repository.

In fact, this repository is a miror of the following files extracted from our [Synopse Open Source code repository](http://synopse.info/fossil/):

  * `SynMustache.pas`
  * `SynCommons.pas`
  * `SynLz.pas`
  * `Synopse.inc`
  * `SynopseCommit.inc`

Note that even if `SynMustache` is part of the [mORMot Open Source framework](http://mormot.net/), it is just one brick of it, so you can use this unit with any of your projects, without the need to use either the database, ORM, SOA or other features of *mORMot*.

If you download the whole *mORMot* source code, you do not need this separate package: ensure you get rid of any existing separated `SynMustache` installation, and use the units as available in the main *mORMot* trunk.
This *DMustache* distribution/GitHub account targets only people needing an optimized *Mustache* template, without other *mORMot* features.

License
=======

This library is part of the Open Source *mORMot* framework, so is released under the same disjunctive tri-license giving you the choice of one of the three following sets of free software/open source licensing terms:

  * Mozilla Public License, version 1.1 or later;
  * GNU General Public License, version 2.0 or later;
  * GNU Lesser General Public License, version 2.1 or later.

This allows the use of our code in as wide a variety of software projects as possible, while still maintaining copyleft on code we wrote.


Sample Code
===========

Variables
---------

First, we define our needed variables:

    var mustache: TSynMustache;
        doc: variant;

In order to parse a template, you just need to call:

      mustache := TSynMustache.Parse(
        'Hello {{name}}'#13#10'You have just won {{value}} dollars!');

It will return a compiled instance of the template.

The `Parse()` class method will use the shared cache, so you won't need to release the mustache instance once you are done with it: no need to write a `try ... finally mustache.Free; end` block.

You can use a `TDocVariant` custom variant type (defined in `SynCommons.pas`) to supply the context data (with late-binding):

      TDocVariant.New(doc);
      doc.name := 'Chris';
      doc.value := 10000;

As an alternative, you may have defined the context data as such:

      doc := _ObjFast(['name','Chris','value',1000]);

Now you can render the template with this context:

      html := mustache.Render(doc);
      // now html='Hello Chris'#13#10'You have just won 10000 dollars!'

If you want to supply the context data as JSON, then render it, you may write:

      mustache := TSynMustache.Parse(
        'Hello {{value.name}}'#13#10'You have just won {{value.value}} dollars!');
      html := mustache.RenderJSON('{value:{name:"Chris",value:10000}}');
      // now html='Hello Chris'#13#10'You have just won 10000 dollars!'

Note that here, the JSON is supplied with an extended syntax (i.e. field names are unquoted), and that `TSynMustache` is able to identify a dotted-named variable within the execution context.

As an alternative, you could use the following syntax to create the data context as JSON, with a set of parameters, therefore easier to work with in real code storing data in variables (for instance, any string variable is quoted as expected by JSON, and converted into UTF-8):

      mustache := TSynMustache.Parse(
        'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
      html := mustache.RenderJSON('{name:?,value:?}',[],['Chris',10000]);
      html='Hello Chris'#13#10'You have just won 10000 dollars!'

You can find in the `mORMot.pas` unit the `ObjectToJSON()` function which is able to transform any `TPersistent` instance into valid JSON content, ready to be supplied to a `TSynMustache` compiled instance.

If the object's published properties have some getter functions, they will be called on the fly to process the data (e.g. returning 'FirstName Name' as FullName by concatenating both sub-fields).

Sections
--------

Sections are handled as expected:

      mustache := TSynMustache.Parse('Shown.{{#person}}As {{name}}!{{/person}}end{{name}}');
      html := mustache.RenderJSON('{person:{age:?,name:?}}',[10,'toto']);
      // now html='Shown.As toto!end'

Note that the sections change the data context, so that within the #person section, you can directly access to the data context person member, i.e. writing directly name

It supports also inverted sections:

      mustache := TSynMustache.Parse('Shown.{{^person}}Never shown!{{/person}}end');
      html := mustache.RenderJSON('{person:true}');
      // now html='Shown.end'

To render a list of items, you can write for instance (using the `{{.}}` pseudo-variable):

      mustache := TSynMustache.Parse('{{#things}}{{.}}{{/things}}');
      html := mustache.RenderJSON('{things:["one", "two", "three"]}');
      // now html='onetwothree'

The `{{-index]}}` pseudo-variable allows to numerate the list items, when rendering:

      mustache := TSynMustache.Parse(
        'My favorite things:'#$A'{{#things}}{{-index}}. {{.}}'#$A'{{/things}}');
      html := mustache.RenderJSON('{things:["Peanut butter", "Pen spinning", "Handstands"]}');
      // now html='My favorite things:'#$A'1. Peanut butter'#$A'2. Pen spinning'#$A+
      //          '3. Handstands'#$A,'-index pseudo variable'

Partials
--------

External partials (i.e. standard Mustache partials) can be defined using `TSynMustachePartials`.
You can define and maintain a list of TSynMustachePartials instances, or you can use a one-time partial, for a given rendering process, as such:

      mustache := TSynMustache.Parse('{{>partial}}'#$A'3');
      html := mustache.RenderJSON('{}',TSynMustachePartials.CreateOwned(['partial','1'#$A'2']));
      // now html='1'#$A'23','external partials'

Here `TSynMustachePartials.CreateOwned()` expects the partials to be supplied as name/value pairs.

Internal partials (one of the SynMustache extensions), can be defined directly in the main template:

      mustache := TSynMustache.Parse('{{<partial}}1'#$A'2{{name}}{{/partial}}{{>partial}}4');
      html := mustache.RenderJSON('{name:3}');
      // now html='1'#$A'234','internal partials'

Internationalization
--------------------

You can define `{{"some text}}` pseudo-variables in your templates, which text will be supplied to a callback, ready to be transformed on the fly: it may be convenient for i18n of web applications.

By default, the text will be written directly to the output buffer, but you can define a callback which may be used e.g. for text translation:

    procedure TTestLowLevelTypes.MustacheTranslate(var English: string);
    begin
      if English='Hello' then
        English := 'Bonjour' else
      if English='You have just won' then
        English := 'Vous venez de gagner';
    end;

Of course, in a real application, you may assign one `TLanguageFile.Translate(var English: string)` method, as defined in the `mORMoti18n.pas` unit.

Then, you will be able to define your template as such:

      mustache := TSynMustache.Parse(
        '{{"Hello}} {{name}}'#13#10'{{"You have just won}} {{value}} {{"dollars}}!');
      html := mustache.RenderJSON('{name:?,value:?}',[],['Chris',10000],nil,MustacheTranslate);
      // now html='Bonjour Chris'#$D#$A'Vous venez de gagner 10000 dollars!'

All text has indeed been translated as expected.


Some Links
==========

We wrote a series of blog articles, about Mustache in general, and `SynMustache` unit in particular:

  * [Mustache Logic-less templates for Delphi - part 1: general presentation of Mustache](https://blog.synopse.info/?post/2014/04/28/Mustache-Logic-less-templates-for-Delphi-part-1);
  * [Mustache Logic-less templates for Delphi - part 2: the Mustache syntax](https://blog.synopse.info/?post/2014/04/28/Mustache-Logic-less-templates-for-Delphi-part-2);
  * [Mustache Logic-less templates for Delphi - part 3: SynMustache implementation](https://blog.synopse.info/?post/2014/04/28/Mustache-Logic-less-templates-for-Delphi-part-3).

You can use also [Synopse forums](http://synopse.info/forum/viewtopic.php?id=1720) to obtain direct support from the developpers, or send your feedback.

The documentation is [available as a single pdf file](http://blog.synopse.info/public/Documents/SynMustache.pdf), if needed. Note that this `pdf` can be outdated, so you should better consult the "Mustache" part of the *mORMot* SAD pdf, which should be more accurate.


*The Synopse team*
