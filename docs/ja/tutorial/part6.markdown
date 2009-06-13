Tutorial Part 6. 拡張スロットオプションとgenpagesマクロ
========================================================
web4rは永続化クラスの為の拡張スロットオプションと、永続化クラスインスタンスの一覧/詳細表示、編集、削除に必要となるページを一括生成する為の[genpagesマクロ](http://web4r.org/en/api#genpages)を提供します。

拡張スロットオプション
-----------------------
- **index:**    trueであればslotにindexを作成
- **label:**    slotのラベル (表示用)
- **unique:**   nilでなければ、slotの値は重複非許可
- **required:** nilでなければ、slotの値は必須
- **rows:**     テキストエリア入力フォームの行幅
- **cols:**     テキストエリア入力フォームの列幅
- **size:**     テキスト入力フォームのサイズ
- **length:**   nilでなければ、slotの値の文字数を検証。数値の場合は最大文字数を意味し、リストの場合は'(最大 最小)文字数を検証
- **hide-for:** slotを表示しないページを指定。:allの場合は、全てのページで表示しない。文字列型の正規表現で指定した場合、リクエストURIが正規表現にマッチしたページでのみ非表示
- **options:**  セレクトボックス、ラジオボタン、チェックボックスの為の選択肢リスト
- **comment:**  slotコメント (表示用)
- **input:**    入力フォームタイプ。:text, :textarea, :radio, :checkbox, :select, :password, :fileのいずれか
- **format:**   値のフォーマットバリデーション。:alpha, :alnum, :integer, :email, :date, :image, 文字列型の正規表現、関数、nil(チェックを行わない)のいずれか

Genpagesマクロ
---------------

### 生成される主要ページとその機能

#### index page (一覧ページ)
- 一覧の表示
- 一覧の並び替え with/without ajax
- ページング with/without ajax 
- インスタンスの削除 with/without ajax

[![customer-index](http://web4r.org/customer-index-thumbnail.png)](http://web4r.org/customer-index.png)

#### show page (詳細ページ)
- インスタンスのslot値詳細表示

[![customer-show](http://web4r.org/customer-show-thumbnail.png)](http://web4r.org/customer-show.png)

#### edit page (編集ページ)
- インスタンスの新規作成、もしくは既存のインスタンスの編集
- バリデーション
    - Javascriptによるクライアントサイド バリデーション (重複チェックはajaxで行う)
    - サーバーサイド バリデーション

[![customer-new](http://web4r.org/customer-new-thumbnail.png)](http://web4r.org/customer-new.png)
[![customer-edit](http://web4r.org/customer-edit-thumbnail.png)](http://web4r.org/customer-edit.png)


### 例:

#### Blogアプリケーション

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    
    (in-package :cl-user)
    (defpackage :blog (:use :cl :web4r))
    (in-package :blog)
    
    (ele:open-store *example-bdb*)
    
    (defpclass blog ()
      ((title :length 50 :index t)
       (body  :length 3000)))
    
    (genpages blog)
    
    (start-server)

- [Blogデモ](http://demo.web4r.org/blog)

#### Customerアプリケーション

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (asdf:oos 'asdf:load-op :web4r))
    (use-package :web4r)
    
    (in-package :cl-user)
    (defpackage :customer (:use :cl :web4r))
    (in-package :customer)
    
    (ele:open-store *example-bdb*)
    
    (defpclass customer ()
      ((name         :length 50 :label "Full Name" :size 30 :index t)
       (password     :input :password :length (8 12) :comment "8-12 characters"
                     :hide-for "^(?!/customer/edit/)")
       (email        :format :email :unique t)
       (sex          :input :radio :options ("Male" "Female"))
       (marriage     :input :select :options ("single" "married" "divorced"))
       (hobbies      :input :checkbox :options ("sports" "music" "reading"))
       (birth-date   :format :date :index t)
       (nickname     :length 50 :required nil)
       (phone-number :format "^\\d{3}-\\d{3}-\\d{4}$" :comment "xxx-xxx-xxxx" :index t)
       (zip-code     :type integer :length (5 5) :comment "5 digit" :index t)
       (note         :length 3000 :rows 5 :cols 30)
       (image        :input :file :format :image :length (1000 500000) :required nil)))
    (genpages customer)
    
    (start-server)

- [Customerデモ](http://demo.web4r.org/customer)

**※ 永続化クラスは単数形で命名して下さい。（genpagesマクロは必要に応じて永続化クラス名を複数形に変換します。）**
