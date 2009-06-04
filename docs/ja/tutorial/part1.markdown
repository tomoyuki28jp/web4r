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
    
Markup language (マークアップ言語)
-----------------------------------
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

escape関数を利用することにより、手動で値のエスケープが可能であり、一度エスケープされた値は二重エスケープされません。

    [p (escape"<>")] ;=> <p>&lt;&gt;</p>
    [p (escape (escape"<>"))] ;=> <p>&lt;&gt;</p>

特定の値だけをエスケープしたくない場合、safeマクロを利用して下さい。

    [p (safe"<>")] ;=> <p><></p>

テンプレート
-------------
smlはテンプレート言語としても利用することが出来ます。別ファイルに記述されたsmlコードはコンパイル時にLispコード内に展開される為、テンプレートに値をassignするような手間は不要です。

例:
*/tmp/template.sml*

    (print x)

*/tmp/use.lisp*

    (let ((x "ok!"))
      (load-sml "/tmp/template.sml")) ;=> "ok!"

テンプレートエレメントの操作
-----------------------------
selectorを利用してテンプレートエレメントを選択し、manipulatorを利用して、テンプレートエレメントを操作することが出来ます。selectorの文法はtag, #id, もしくは.classであり、定義済のmanipulatorはappend, replace, そしてremoveです。

例:

    (define-template :template1
        [html [head [title "Default title"]]
              [body [p :id "id1" "p1"]
                    [p :class "class1" "p2"]]])
    
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

利用例
-------

Lispプログラムを利用して(x)htmlテーブルを生成:

    [table (dotimes (x 3) [tr [td x]])]

    ;=> <table>
    ;       <tr><td>0</td></tr>
    ;       <tr><td>1</td></tr>
    ;       <tr><td>2</td></tr>
    ;   </table>    