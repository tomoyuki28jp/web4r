Tutorial Part 8. Validations
=============================

Types of Validation
--------------------
- **length**   Length validation. An integer for a max length or a list of two elements for '(min max) length.
- **format**   Format validation. Either one of :alpha, :alnum, :integer, :email, :date, :image, regexp as a string, a function or nil.
- **member**   Member validation. A list of valid input options.
- **required** Required validation. A value is required if this is non nil.
- **unique**   Uniquness validation. A slot value of a persistent class must be unique if this is non nil.

Validating a value
-------------------
### validation-errors

#### Syntax:

    (validation-errors label value validators)

[validation-errors](http://web4r.org/en/api#validation-errors) validates VALUE with VALIDATORS and returns error messages if any.  LABEL is used as a subject of error messages.

#### Examples:

    (validation-errors "label" nil '(:required t :length 3))
    ;=> ("label can't be empty") 
    
    (validation-errors "label" "12345" '(:required t :length 3))
    ;=> ("label is too long (maximum is 3 characters)") 

### with-validations

#### Syntax:

    (with-validations validations error-handler body)

[with-validations](http://web4r.org/en/api#with-validations) executes VALIDATIONS. Then executes ERROR-HANDLER if there is any error and BODY otherwise. The ERROR-HANDLER takes one argument which is a list of validation error messages.

#### Examples:

    (with-validations (("1" "v" '(:required t))
                       ("2" nil '(:required t)))
      (lambda (e) e)
      "ok") ;=> ("2 can't be empty")

    (with-validations (("1" "v" '(:required t))
                       ("2" "v" '(:required t)))
      (lambda (e) e)
      "ok") ;=> "ok"

### slot-validation-errors

#### Syntax:

    (slot-validation-errors class slot &optional instance)

[slot-validation-errors](http://web4r.org/en/api#slot-validation-errors) validates posted parameters to edit the SLOT value in the persistent CLASS and returns a list of error messages if any. INSTANCE is an instance of the persistent CLASS only needed to update an existing instance.


#### Examples:

    (defpclass blog ()
      ((body :required t)))
    
    (with-post-parameters '(("blog_body" . nil))
      (slot-validation-errors 'blog (get-slot 'blog 'body)))
      
    ;=> ("Body can't be empty") 

### class-validation-errors

#### Syntax:

    (class-validation-errors class &optional instance)

[class-validation-errors](http://web4r.org/en/api#class-validation-errors) validates posted parameters to make/update an instance of the persistent CLASS and returns a list of error messages if any. INSTANCE is an instance of the persistent CLASS only needed to update an existing instance.

#### Examples:

    (defpclass blog ()
      ((title :required t)
       (body  :required t)))
    
    (with-post-parameters '(("blog_title" . nil)
                            ("blog_body"  . nil))
      (class-validation-errors 'blog))
    
    ;=> ("Title can't be empty" "Body can't be empty") 

Validation error messages
--------------------------
You can change the validation error messages by changing the value of [\*validation-error-messages\*](http://web4r.org/en/api#*validation-error-messages*) or redefining the error-msg function.
