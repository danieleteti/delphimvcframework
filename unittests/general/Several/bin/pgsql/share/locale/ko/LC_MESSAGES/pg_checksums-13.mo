Þ    8      Ü  O         Ø  X   Ù  
   2     =  5   Y  P     5   à  A     :   X  2     1   Æ  G   ø  3   @  *   t       T   ¹          "     6     J     h          ¥     À     Ô  k   ê  &   V	     }	  a   	     ç	     
  ;   &
     b
  !   |
     
  (   »
  3   ä
       )   5  5   _  .     -   Ä  )   ò  "        ?     G  3   O  +        ¯  2   Ë  !   þ  )         J  /   a       	   §  ^  ±  k        |       @   ¥  Y   æ  3   @  @   t  K   µ  C     @   E  <     @   Ã  2        7     R     è     ÿ          1  F   J  C        Õ     ò          *  A   ¿       y     #     #   «  D   Ï  $     $   9  !   ^  4     M   µ  .     .   2  D   a  E   ¦  B   ì  >   /  6   n     ¥     ®  >   ·  /   ö  0   &  >   W  %     4   ¼      ñ  <        O     n     8          .   )                   !   #               /          4   (   *              ,       0       3      &                              7   6          1            2   '       $                          +              "              
          %             5      -   	        
If no data directory (DATADIR) is specified, the environment variable PGDATA
is used.

 
Options:
   %s [OPTION]... [DATADIR]
   -?, --help               show this help, then exit
   -N, --no-sync            do not wait for changes to be written safely to disk
   -P, --progress           show progress information
   -V, --version            output version information, then exit
   -c, --check              check data checksums (default)
   -d, --disable            disable data checksums
   -e, --enable             enable data checksums
   -f, --filenode=FILENODE  check only relation with specified filenode
   -v, --verbose            output verbose messages
  [-D, --pgdata=]DATADIR    data directory
 %*s/%s MB (%d%%) computed %s enables, disables, or verifies data checksums in a PostgreSQL database cluster.

 %s home page: <%s>
 Bad checksums:  %s
 Blocks scanned: %s
 Checksum operation completed
 Checksums disabled in cluster
 Checksums enabled in cluster
 Data checksum version: %d
 Files scanned:  %s
 Report bugs to <%s>.
 The database cluster was initialized with block size %u, but pg_checksums was compiled with block size %u.
 Try "%s --help" for more information.
 Usage:
 checksum verification failed in file "%s", block %u: calculated checksum %X but block contains %X checksums enabled in file "%s" checksums verified in file "%s" cluster is not compatible with this version of pg_checksums cluster must be shut down could not open directory "%s": %m could not open file "%s": %m could not read block %u in file "%s": %m could not read block %u in file "%s": read %d of %d could not stat file "%s": %m could not write block %u in file "%s": %m could not write block %u in file "%s": wrote %d of %d data checksums are already disabled in cluster data checksums are already enabled in cluster data checksums are not enabled in cluster database cluster is not compatible error:  fatal:  invalid filenode specification, must be numeric: %s invalid segment number %d in file name "%s" no data directory specified option -f/--filenode can only be used with --check pg_control CRC value is incorrect seek failed for block %u in file "%s": %m syncing data directory too many command-line arguments (first is "%s") updating control file warning:  Project-Id-Version: pg_checksums (PostgreSQL) 13
Report-Msgid-Bugs-To: pgsql-bugs@lists.postgresql.org
PO-Revision-Date: 2020-10-06 11:13+0900
Last-Translator: Ioseph Kim <ioseph@uri.sarang.net>
Language-Team: PostgreSQL Korea <kr@postgresql.org>
Language: ko
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
 
DATADIRì¸ ë°ì´í° ëë í°ë¦¬ë¥¼ ì§ì íì§ ìì¼ë©°, PGDATA íê²½ ë³ìê°ì
ì¬ì©í©ëë¤.

 
ìµìë¤:
   %s [ìµì]... [DATADIR]
   -?, --help               ì´ ëìë§ì ë³´ì¬ì£¼ê³  ë§ì¹¨
   -N, --no-sync            ìì ìë£ ë¤ ëì¤í¬ ëê¸°í ììì íì§ ìì
   -P, --progress           ì§í ê³¼ì  ë³´ì¬ì¤
   -V, --version            ë²ì  ì ë³´ë¥¼ ë³´ì¬ì£¼ê³  ë§ì¹¨
   -c, --check              ì¤ ìì ìì´, ê·¸ë¥ ê²ì¬ë§ (ê¸°ë³¸ê°)
   -d, --disable            ìë£ íì´ì§ ì²´í¬ì¬ ë¹íì±í
   -e, --enable             ìë£ íì´ì§ ì²´í¬ì¬ íì±í
   -f, --filenode=FILENODE  ì§ì í íì¼ë¸ëë§ ê²ì¬
   -v, --verbose            ìì¸í ìì ë©ìì§ ë³´ì¬ì¤
  [-D, --pgdata=]DATADIR    ë°ì´í° ëë í°ë¦¬
 %*s/%s MB (%d%%) ê³ì°ë¨ %s ëªë ¹ì PostgreSQL ë°ì´í°ë² ì´ì¤ í´ë¬ì¤í° ë´ ìë£ ì²´í¬ì¬ì íì±í ëë
ë¹íì±í ëë ì í¨ì± ê²ì¬ë¥¼ í©ëë¤.

 %s ííì´ì§: <%s>
 ìëª»ë ì²´í¬ì¬: %s
 ì¡°ì¬í ë¸ë­ì: %s
 ì²´í¬ì¬ ìì ìë£
 ì´ í´ë¬ì¤í°ë ìë£ ì²´í¬ì¬ ìµìì´ ë¹íì±í ëìì
 ì´ í´ë¬ì¤í°ë ìë£ ì²´í¬ì¬ ìµìì´ íì±í ëìì
 ìë£ ì²´í¬ì¬ ë²ì : %d
 ì¡°ì¬í íì¼ì: %s
 ë¬¸ì ì  ë³´ê³  ì£¼ì: <%s>
 ì´ ë°ì´í°ë² ì´ì¤ í´ë¬ì¤í°ë %u ë¸ë¡ í¬ê¸°ë¡ ì´ê¸°í ëìì§ë§, pg_checksumì %u ë¸ë¡ í¬ê¸°ë¡ ì»´íì¼ ëì´ììµëë¤.
 ìì í ì¬í­ì "%s --help" ëªë ¹ì¼ë¡ ì´í´ë³´ì­ìì¤.
 ì¬ì©ë²:
 "%s" íì¼, %u ë¸ë­ì  ì²´í¬ì¬ ê²ì¬ ì¤í¨: ê³ì°ë ì²´í¬ì¬ì %X ê°ì´ì§ë§, ë¸ë­ìë %X ê°ì´ ìì "%s" íì¼ ì²´í¬ì¬ íì±í í¨ "%s" íì¼ ì²´í¬ì¬ ê²ì¬ ë§ì¹¨ í´ë¹ í´ë¬ì¤í°ë ì´ ë²ì  pg_checksumê³¼ í¸íëì§ ìì ë¨¼ì  ìë²ê° ì¤ì§ëì´ì¼ í¨ "%s" ëë í°ë¦¬ ì´ ì ìì: %m "%s" íì¼ì ì´ ì ìì: %m %u ë¸ë­ì "%s" íì¼ìì ì½ì ì ìì: %m %u ë¸ë­ì "%s" íì¼ìì ì½ì ì ìì: %d / %d ë°ì´í¸ë§ ì½ì "%s" íì¼ì ìíê°ì ì ì ìì: %m %u ë¸ë­ì "%s" íì¼ì ì¸ ì ìì: %m %u ë¸ë­ì "%s" íì¼ì ì¸ ì ìì: %d / %d ë°ì´í¸ë§ ì ì´ í´ë¬ì¤í°ë ì´ë¯¸ ìë£ ì²´í¬ì¬ì´ ë¹íì±í ìíì ì´ í´ë¬ì¤í°ë ì´ë¯¸ ìë£ ì²´í¬ì¬ì´ íì±í ìíì ì´ í´ë¬ì¤í°ë ìë£ ì²´í¬ì¬ì´ ë¹íì±í ìíì ë°ì´í°ë² ì´ì¤ í´ë¬ì¤í°ë í¸íëì§ ìì ì¤ë¥:  ì¬ê°:  íì¼ë¸ë ê°ì´ ì´ìí¨. ì´ ê°ì ì«ìì¬ì¼ í¨: %s ìëª»ë ì¡°ê° ë²í¸ %d, í´ë¹ íì¼: "%s" ë°ì´í° ëë í°ë¦¬ë¥¼ ì§ì íì§ ììì -f/--filenode ìµìì --check ìµìë§ ì¬ì©í  ì ìì pg_control CRC ê°ì´ ìëª»ëìì %u ë¸ë­ì "%s" íì¼ìì ì°¾ì ì ìì: %m ë°ì´í° ëë í°ë¦¬ fsync ì¤ ëë¬´ ë§ì ëªë ¹í ì¸ìë¥¼ ì§ì íì (ì²ì "%s") ì»¨í¸ë¡¤ íì¼ ë°ê¾¸ë ì¤ ê²½ê³ :  