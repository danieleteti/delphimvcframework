<div id="content">

<h2>Appender de SQL</h2>

<p>O appender de SQL pode ser usado para escrever mensagens de log em uma tabela
de banco de dados SQL. Ele utiliza para tal 
<a href="http://www.keplerproject.org/luasql/">LuaSQL</a>, portanto 
&eacute; poss&iacute;vel usar qualquer banco de dados suportado.</p>

<pre class="example">
function logging.sql{
    connectionfactory = <i>function</i>,
    [tablename = <i>string</i>,]
    [logdatefield = <i>string</i>,]
    [loglevelfield = <i>string</i>,]
    [logmessagefield = <i>string</i>,]
    [keepalive = <i>boolean</i>],
}
</pre>

<ul>
    <li><code>connectionfactory</code>:<br /> Precisa necessariamente ser uma
    fun&ccedil;&atilde;o que cria um objeto de conex&atilde;o LuaSQL.
    Essa fun&ccedil;&atilde;o ser&aacute; chamada sempre que for preciso criar
    uma conex&atilde;o.</li>
    
    <li><code>tablename</code>:<br /> O nome da tabela para gravar as
    solicita&ccedil;&otilde;es de log. O valor padr&atilde;o &eacute;
    <code>&quot;LogTable&quot;</code>.</li>
    
    <li><code>logdatefield</code>:<br /> O nome do campo para gravar a data de
    cada solicita&ccedil;&atilde;o de log. O valor padr&atilde;o &eacute;
    <code>&quot;LogDate&quot;</code>.</li>
    
    <li><code>loglevelfield</code>:<br /> O nome do campo para gravar o
    n&iacute;vel de cada solicita&ccedil;&atilde;o de log.
    O valor padr&atilde;o &eacute; <code>&quot;LogLevel&quot;</code>.</li>
    
    <li><code>logmessagefield</code>:<br /> O nome do campo para gravar a
    mensagem de cada solicita&ccedil;&atilde;o de log.
    O valor padr&atilde;o &eacute; <code>&quot;LogMessage&quot;</code>.</li>
    
    <li><code>keepalive</code>:<br /> Em toda solicita&ccedil;&atilde;o de log,
    uma conex&atilde;o com o banco de dados &eacute; aberta, a mensagem, escrita
    e a conex&atilde;o, fechada.<br /> Se o usu&aacute;rio quiser manter a
    conex&atilde;o aberta, pode especificar <code>keepalive = true</code>.</li>
</ul>

<h2>Exemplo</h2>

<pre class="example">
require&quot;logging.sql&quot;
require&quot;luasql.jdbc&quot;

local env, err = luasql.jdbc('com.mysql.jdbc.Driver')

local logger = logging.sql {
  connectionfactory = function()
    local con, err = env:connect('jdbc:mysql://localhost/test',
                                 'tcp', '123')
    assert(con, err)
    return con
  end,
  keepalive = true,
}

logger:info(&quot;teste de logging.sql&quot;)
logger:debug(&quot;depurando...&quot;)
logger:error(&quot;erro!&quot;)
</pre>

</div> <!-- id="content" -->
