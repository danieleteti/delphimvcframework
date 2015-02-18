<div id="content">

<h2>Appender de console</h2>

<p>O console &eacute; o appender mais simples. Ele apenas escreve as mensagens de
log em <code>io.stdout</code>.</p>

<pre class="example">
function logging.console([logPattern])
</pre>

<ul>
    <li><code>logPattern</code>:<br /> &Eacute; poss&iacute;vel especificar um
    padr&atilde;o para controlar o modo como a mensagem &eacute; escrita.<br />
    O valor padr&atilde;o &eacute; <code>&quot;%date %level %message\n&quot;</code>.</li>
</ul>

<h2>Exemplos</h2>

<pre class="example">
require&quot;logging.console&quot;

local logger = logging.console()

logger:info(&quot;teste de logging.console&quot;)
logger:debug(&quot;depurando...&quot;)
logger:error(&quot;erro!&quot;)
</pre>

</div> <!-- id="content" -->
