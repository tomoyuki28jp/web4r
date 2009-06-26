Tutorial Part 1. S式マークアップ言語
=====================================
Tutorial Part 1.では[sml](http://github.com/tomoyuki28jp/sml/) (S式マークアップ言語)の使い方を説明します。デザイナーと協業する為に、一般的なHTMLテンプレートを利用したい場合、Ediが開発した[HTML-TEMPLATE](http://www.weitz.de/html-template/)をsmlの代わりに利用することが出来ます。


ダウンロード、インストール、及び利用
-------------------------------------
smlはweb4rの依存パッケージである為、既にweb4rをインストールしていれば、smlもインストールされているはずです。まだsmlをインストールしていない場合は、[このページ](https://github.com/tomoyuki28jp/sml)からsmlをダウンロードしてインストールして下さい。

smlを利用する為には次のコードを実行して下さい。

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :sml))
    (use-package :sml)

Syntax (文法)
--------------
[tag attributes... values...]

例:

    [html [body "Hello, world!"]]

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <body>Hello, world!</body>
    ;   </html>

Attributes (属性)
------------------
キーワード引数、もしくは(attr ...)内のパラメーターがattributes(属性)になります。

例:

    ; 下記全てのコードは同じ(x)htmlを生成します
    ;=> <p id="id1" class="class1">value</p>
    
    [p :id "id1" :class "class1" "value"]
    [p (attr :id "id1") (attr :class "class1") "value"]
    [p (attr "id" "id1") (attr "class" "class1") "value"]
    [p (attr :id "id1" :class "class1") "value"]
    
生成するマークアップ言語
-------------------------
生成するマークアップ言語は\*markup-lang\*の値によって変更をすることができます。有効な\*markup-lang\*の値は:xhtml, :html, :xmlのいずれかであり、デフォルトは:xhtmlです。

例:

    (let ((*markup-lang* :xhtml))
      [html [body "xhtml"]])

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <body>xhtml</body>
    ;   </html>


    (let ((*markup-lang* :html))
      [html [body "html"]])      

    ;=> <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
    ;   "http://www.w3.org/TR/html4/loose.dtd">
    ;   <html>
    ;       <body>html</body>
    ;   </html>


    (let ((*markup-lang* :xml))
      [html [body "xml"]])

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <html>
    ;       <body>xml</body>
    ;   </html>

閉じ括弧
---------
閉じ括弧が不要な場合、引数の最後に/を渡して下さい。

例:

    [br] ;=> <br></br>

    (let ((*markup-lang* :xhtml))
      [br /]) ;=> <br />

    (let ((*markup-lang* :html))
      [br /]) ;=> <br>

    (let ((*markup-lang* :xml))
      [br /]) ;=> <br />

エスケープ
-----------
smlはデフォルトで全ての属性と値を自動的にエスケープします。

    [p "<>"] ;=> <p>&lt;&gt;</p>

特定の値だけをエスケープしたくない場合、safeマクロを利用して下さい。

    [p (safe "<>")] ;=> <p><></p>

escape関数を利用することにより、手動で値をエスケープすることが出来ます。

    [p (safe (escape "<>") "<br />")] ;=> <p>&lt;&gt;<br /></p>

一度エスケープされた値は二重エスケープされません。

    [p (escape (escape "<>"))] ;=> <p>&lt;&gt;</p>

smlコードの分離
----------------
load-smlマクロは別ファイルに記述されたsmlコードをコンパイル時にLispコード内に展開する為、別途テンプレートに値をassignするような手間は不要です。

例:

*/tmp/template.sml*

    [p x]

*/tmp/use.lisp*

    (let ((x "ok!"))
      (load-sml "/tmp/template.sml")) ;=> <p>ok!</p>

*展開イメージ*

    (let ((x "ok!"))
      [p x])

テンプレートの定義とエレメントの操作
-------------------------------------

### Lispコード内で完結する方法
define-templateマクロを利用してテンプレートを定義することが出来ます。

    (define-template :template1
        [html [head [title "Default title"]]
              [body [p :id "id1" "p1"]
                    [p :class "class1" "p2"]]])

定義したテンプレートのエレメントはselectorを利用して選択可能であり、manipulatorを利用して選択したエレメントを操作することが出来ます。selectorの文法はtag, #id, もしくは.classであり、定義済のmanipulatorはappend, replace, そしてremoveです。	

    (with-template (:template1)
      (replace title [title "new title"]) ; タイトルを変更
      (remove  "#id1") ; idの値が'id1'であるエレメントを削除
      (remove  ".class1") ; classの値が'class1'であるエレメントを削除
      (append  body [p "ok!"])) ; bodyに新たなエレメントを追加

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html>
    ;       <head>
    ;           <title>new title</title>
    ;       </head>
    ;       <body>
    ;           <p>ok!</p>
    ;       </body>
    ;   </html>

### テンプレートファイルを分離する方法

**/tmp/template.sml**

    [html :xmlns "http://www.w3.org/1999/xhtml" :lang "en"
          [head [meta :http-equiv "content-type" :content "text/html; charset=utf-8" /]
                [title "Default template title"]]
          [body [p "Default template body"]]]

**use.lisp**

    (with-sml-file "/tmp/template.sml"
      (replace title [title "new title"]) ; タイトルを変更
      (replace body  [body "new body"]))  ; bodyを変更

    ;=> <?xml version="1.0" encoding="UTF-8"?>
    ;   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;   <html xmlns="http://www.w3.org/1999/xhtml" lang="en">
    ;       <head>
    ;           <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    ;           <title>new title</title>
    ;       </head>
    ;       <body>new body</body>
    ;   </html>

*with-sml-fileマクロはsmlファイル内でも利用可能です*

インデント
-----------
smlは比較的綺麗にインデントされたコードを生成します。

    [body [table (dotimes (x 3) [tr [td x]])]]
    
    ;=> <body>
    ;       <table>
    ;           <tr>
    ;               <td>0</td>
    ;   
    ;           </tr>
    ;           <tr>
    ;               <td>1</td>
    ;   
    ;           </tr>
    ;           <tr>
    ;               <td>2</td>
    ;   
    ;           </tr>
    ;   
    ;       </table>
    ;   
    ;   </body>

インデントが不要な場合、\*indent-mode\*の値をnilにセットして下さい。

    (let ((*indent-mode* nil))
      [body [table (dotimes (x 3) [tr [td x]])]])

    ;=> <body><table><tr><td>0</td></tr><tr><td>1</td></tr><tr><td>2</td></tr></table></body>

FAQ
----
### Q. [cl-who](http://www.weitz.de/cl-who/)は普通のマクロでsmlと同様のことが出来るのに、なぜ新たなマークアップ言語をリードマクロで開発したのですか？

A. そちらの方がマークアップ言語を短く記述出来るからです。例えば、次のsmlコードをcl-whoで書き直した場合、smlよりも長くなると思います。

    [html [body [table (dotimes (x 3) [tr [td x]])]]]
    
    ; =>
    ;<?xml version="1.0" encoding="UTF-8"?>
    ;<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
    ;"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    ;<html>
    ;    <body>
    ;        <table>
    ;            <tr><td>0</td></tr>
    ;            <tr><td>1</td></tr>
    ;            <tr><td>2</td></tr>
    ;        </table>
    ;    </body>
    ;</html>


[cl-who](http://www.weitz.de/cl-who/)の方がお好みであれば、cl-whoをsmlの代わりに利用することが出来ます。
