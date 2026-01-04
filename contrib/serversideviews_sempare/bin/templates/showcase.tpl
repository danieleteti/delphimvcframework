<!DOCTYPE html>
<html>
<header>
    <style>
    body {
		  font-family: Consolas, 'Courier New';
		}
		
		blockquote {
		   font-style: italic;
		   color: #a0a0a0;
		   padding: 0.2em;
		}
		
		.section {
		  background-color: #3a3a3a;
		  color: white;
			border-left: 0.5em red solid;
			padding: 0.5em;		  
		}
		
		.box {
			border: thin black solid;
			margin: auto;
			width: 80%;
			padding: 2em;
		}
    </style>
</header>

<body>
<h1>Sempare Template Engine Showcase</h1>

<p>
This page is a showcase for some of the Sempare Template Engine features usable from DMVCFramework Server Side Views.
</p>

<div>	
	<h2 class="section">List of objects</h2>
	<div>
            {{ if isempty(people) }}
              <div>No People Found</div>
            {{ end }}
            {{ for person in people }}
              <div>{{ _loop_idx_ }}. {{ person.FirstName }} {{ person.LastName }}</div>
            {{ end }}
	</div>
</div>
<div>
	<h2 class="section">Handle empty list of objects</h2>
	<div>
            {{ if isempty(people2) }}
              <div>No People Found</div>
            {{ end }}
            {{ for i in people2; with people2[i] }}
              <div>{{ _loop_idx_ }}. {{FirstName}} {{LastName}}</div>
            {{ end; end }}
	</div>
</div>

<div>
	<h2 class="section">HTML is safe when variables are dereferenced {{print('{{') }} {{ "'<html></html>'" }} {{print('}}')}}</h2>

	<div class="box">
	{{ if myobj}}
                Safe: {{ myobj.rawhtml }}
                <hr>
                Unsafe: {{ print(myobj.rawhtml) }}
        {{ end }}
    <br>
		<blockquote >Check source code to know how to escape curly braces</blockquote >
	</div>
</div>


{{ template 'partial_person' }}
        {{ FirstName }}, {{LastName}}
{{ end }}


<div>
	<h2 class="section">Handling partials</h2>
        Templates can be defined as external resources/files, or defined internally within a template.
	<div>
		<ul>
                {{ for person of people}}
                      <li>{{ include('partial_person', person)}}</li>
                {{end}}
                </ul>
	</div>
</div>

<div>
	<h2 class="section">Using Standard Helpers</h2>
	<ul>
	<li>Using syntax {{ print('{{UpperCase("value as string") }}') }} Helper UpperCase is invoked passing "value as string" as constant value.</li>
	<li>Using syntax {{ print('{{UpperCase(FirstName) }}') }} Helper UpperCase is invoked passing the value contained in the attribute FirstName.</li>
	<li>Helpers can be nested: {{ print('{{UpperCase(LowerCase(FirstName)) }}') }} Helper LowerCase is invoked passing the value contained in the attribute last_name and then, the result, is passed to the helper UpperCase.</li>
	</ul>
	<div>
		{{ for person of people  }}
		<div>{{ __loop_idx__ }}. {{ UpperCase(person.FirstName) }} {{UpperCase(LowerCase(person.lastname))}}</div>
		{{ end  }}
	</div>

	<hr>
	<h3>The Sempare Template Engine has many helper methods as exposed on the <a target="_blank" href="https://github.com/sempare/sempare-delphi-template-engine/blob/main/docs/builtin-functions.md">docs</a></h3>
	<hr>
	<h3>Illustrating some of them:</h3>
	<p>
		{{ print('{{ UpperCase("conrad vermeulen"}}') }}  outputs: {{ UpperCase("conrad vermeulen") }}
	</p>
	<p>
		{{ print('{{ LowerCase("conrad vermeulen")}}') }} outputs: {{LowerCase("conrad vermeulen")}}
	</p>

</div>

<div>
	<h2 class="section">Using Project Specific Helpers</h2>
	<h3>Any project can define its own custom helpers</h3>
	<p>
	{{ MyHelper1("this is a text")}}
	</p>
	<p>
	{{ MyHelper2("this is another text")}}
	</p>
</div>
<hr>
<div>
        More information available, see the <a target="_blank" href="https://github.com/sempare/sempare-delphi-template-engine/blob/main/docs/custom-functions.md">docs</a>
</div>

</body>
</html>
