<div id="content">

<h2>Appender de arquivo</h2>

<p>O appender de arquivo pode ser usado para escrever mensagens de log em
um arquivo. Ele usa rotinas de E/S de Lua para realizar essa tarefa.</p>

<pre class="example">
function logging.file(filename, [datePattern], [logPattern])
</pre>

<ul>
    <li><code>filename</code>:<br /> O nome do arquivo de destino da grava&ccedil;&atilde;o.
    A cada chamada para registrar uma mensagem, o arquivo &eacute; aberto para
    anexa&ccedil;&atilde;o e fechado imediatamente.<br /> Se n&atilde;o for
    poss&iacute;vel abrir o arquivo para anexa&ccedil;&atilde;o, a solicita&ccedil;&atilde;o
    de log retorna nil e uma mensagem de erro.</li>
    
    <li><code>datePattern</code>:<br /> Trata-se de um par&acirc;metro opcional
    que pode ser usado para especificar um padr&atilde;o de data que ser&aacute;
    passado para a fun&ccedil;&atilde;o
    <a href="http://www.lua.org/manual/5.0/manual.html#libiosys"><code>os.date</code></a>
    de modo a compor o nome do arquivo.<br /> Isso &eacute; &uacute;til para criar
    arquivos de log di&aacute;rios ou mensais. Se o usu&aacute;rio quiser criar um
    arquivo de log por dia, deve especificar um padr&atilde;o <code>&quot;%A-%m-%d&quot;</code>
    e um nome de arquivo como <code>&quot;temp%s.log&quot;</code>.</li>
    
    <li><code>logPattern</code>:<br /> &Eacute; poss&iacute;vel especificar um
    padr&atilde;o para controlar o modo como a mensagem &eacute; gravada.<br />
    O valor padr&atilde;o &eacute; <code>&quot;%date %level %message\n&quot;</code>.</li>
</ul>

<h2>Exemplo</h2>

<pre class="example">
require&quot;logging.file&quot;

local logger = logging.file(&quot;teste%s.log&quot;, &quot;%A-%m-%d&quot;)

logger:info(&quot;teste de logging.file&quot;)
logger:debug(&quot;depurando...&quot;)
logger:error(&quot;erro!&quot;)
</pre>

</div> <!-- id="content" -->
