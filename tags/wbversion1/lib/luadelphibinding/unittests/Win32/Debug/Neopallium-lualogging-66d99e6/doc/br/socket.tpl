<div id="content">

<h2>Appender de <em>socket</em></h2>

<p>Este appender pode ser usado para enviar solicita&ccedil;&otilde;es de log
por um <em>socket</em>. O appender de <em>socket</em> depende do
<a href="http://www.tecgraf.puc-rio.br/luasocket">LuaSocket</a>
para ser executado.<br/> Depois de cada solicita&ccedil;&atilde;o de log,
uma conex&atilde;o &eacute; aberta, a mensagem, enviada e a conex&atilde;o, fechada.</p>

<pre class="example">
function logging.socket(address, port [,logPattern])
</pre>

<ul>
    <li><code>address</code>:<br /> O endere&ccedil;o pode ser um endere&ccedil;o IP
    ou o nome de um host ao qual a mensagem de log ser&aacute; enviada.</li>
    
    <li><code>port</code>:<br /> A porta deve ser um n&uacute;mero inteiro dentro do
    intervalo [1..64K).</li>
    
    <li><code>logPattern</code>:<br /> &Eacute; poss&iacute;vel especificar um
    padr&atilde;o para controlar o modo como a mensagem &eacute; gravada.<br />
    O valor padr&atilde;o &eacute; <code>&quot;%date %level %message\n&quot;</code>.</li>
</ul>

<h2>Exemplo</h2>

<pre class="example">
require&quot;logging.socket&quot;

local logger = logging.socket(&quot;localhost&quot;, 5000)

logger:info(&quot;logging.socket test&quot;)
logger:debug(&quot;debugging...&quot;)
logger:error(&quot;error!&quot;)
</pre>

</div> <!-- id="content" -->
