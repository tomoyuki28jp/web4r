Tutorial Part 8. バリデーション
================================

バリデーション・タイプ
-----------------------
- **length**   値の文字数チェック。数値の場合は最大文字数を意味し、リストの場合は'(最大 最小)文字数を検証。
- **format**   値のフォーマットチェック。:alpha, :alnum, :integer, :email, :date, :image, 文字列型の正規表現、関数、nil(チェックを行わない)のいずれか。
- **member**   値が有効な選択肢であるか検証。リストの有効な選択肢オプション。
- **required** 未入力チェック。nilでなければ入力必須。
- **unique**   値の重複チェック。nilでなければ重複非許可。

値のバリデーション
-------------------
### validation-errors

#### 文法:

    (validation-errors label value validators)

[validation-errors](http://web4r.org/en/api#validation-errors)はVALUEをVALIDATORSで検証し、エラーがあればそれを返します。LABELはエラーメッセージの主語として利用されます。

#### 例:

    (validation-errors "label" nil '(:required t :length 3))
    ;=> ("label can't be empty") 
    
    (validation-errors "label" "12345" '(:required t :length 3))
    ;=> ("label is too long (maximum is 3 characters)") 

### with-validations

#### 文法:

    (with-validations validations error-handler body)

[with-validations](http://web4r.org/en/api#with-validations)はVALIDATIONSのバリデーションを実行します。もしエラーがあった場合はERROR-HANDLERを実行し、それ以外の場合はBODYを実行します。ERROR-HANDLERはひとつの引数をとる手続きである必要があり、渡される引数はリストのエラーメッセージです。

#### 例:

    (with-validations (("1" "v" '(:required t))
                       ("2" nil '(:required t)))
      (lambda (e) e)
      "ok") ;=> ("2 can't be empty")

    (with-validations (("1" "v" '(:required t))
                       ("2" "v" '(:required t)))
      (lambda (e) e)
      "ok") ;=> "ok"

### slot-validation-errors

#### 文法:

    (slot-validation-errors class slot &optional instance)

[slot-validation-errors](http://web4r.org/en/api#slot-validation-errors)は永続化クラスCLASSのインスタンスのSLOT値を編集する為にpostパラメーターを検証し、エラーが発生した場合はエラーメッセージを返します。INSTANCEは既存のインスタンスを更新する場合のみ必要とされる、永続化クラスCLASSのインスタンスです。

#### 例:

    (defpclass blog ()
      ((body :required t)))
    
    (with-post-parameters '(("blog_body" . nil))
      (slot-validation-errors 'blog (get-slot 'blog 'body)))
      
    ;=> ("Body can't be empty") 

### class-validation-errors

#### 文法:

    (class-validation-errors class &optional instance)

[class-validation-errors](http://web4r.org/en/api#class-validation-errors)は永続化クラスCLASSのインスタンスを作成/更新する為にpostパラメーターを検証し、エラーが発生した場合はエラーメッセージを返します。INSTANCEは既存のインスタンスを更新する場合のみ必要とされる、永続化クラスCLASSのインスタンスです。

#### 例:

    (defpclass blog ()
      ((title :required t)
       (body  :required t)))
    
    (with-post-parameters '(("blog_title" . nil)
                            ("blog_body"  . nil))
      (class-validation-errors 'blog))
    
    ;=> ("Title can't be empty" "Body can't be empty") 

エラーメッセージ
-----------------
バリデーションエラーメッセージは[\*validation-error-messages\*](http://web4r.org/en/api#*validation-error-messages*)の値を変更するか、error-msg関数を再定義することにより、変更することが出来ます。
