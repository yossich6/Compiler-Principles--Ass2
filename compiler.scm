 (load "pattern-matcher.scm")
   (load "pc.scm")


(define <digit-0-9>
  (range #\0 #\9))


(define <digit-1-9>
  (range #\1 #\9))

(define <natural> 
  (new 
     (*parser (char #\0)) *plus
     (*parser <digit-1-9>)
     (*parser  <digit-0-9>) *star
     (*caten 3)
     (*pack-with 
        (lambda (z x xs)
          (string->number (list->string `(,x ,@xs)))))


     (*parser <digit-1-9>)
     (*parser  <digit-0-9>) *star
     (*caten 2)
     (*pack-with 
        (lambda (x xs)
          (string->number (list->string `(,x ,@xs)))))


     (*parser (char #\0)) *plus
       (*pack (lambda (_) 0))

     (*disj 3)

    done))

(define <Integer>
  (new (*parser (char #\+))
     (*parser <natural>)
     (*caten 2)
     (*pack-with 
        (lambda (++ num)
          (+ num ) ))

     (*parser (char #\-))
     (*parser <natural>)
     (*caten 2)
     (*pack-with
      (lambda (-- n) (- n)))

     (*parser <natural>)

     (*disj 3)
    done))

(define <Fraction>
  (new (*parser <Integer> )
     (*parser (char #\/))
     (*parser <natural> )
     (*guard (lambda (n) (not (zero? n))))
     (*caten 3)
     (*pack-with 
      (lambda (int div num)
        (/ int num)))
    done))
; add fractions


(define <Number>
  (new  
     (*parser <Fraction>)
      (*parser <Integer>)
     (*disj 2)
    done))

(define <Boolean>
  (new
    (*parser (word-ci "#t"))
    (*pack
    (lambda (n) #t) )

    (*parser (word-ci "#f"))
    (*pack
    (lambda (n) #f) )
    (*disj 2)
  done))




;//// gets "\\\\" and #\\  will pring #\\

(define ^<meta-char> 
  (lambda (str ch)
  (new (*parser (word-ci str))
     (*pack (lambda (_) ch))
     done)))



;/////


(define <a-f>
  (range #\a #\f))

(define <A-F>
  (range #\A #\F))

(define <HexChar>
     (new
      (*parser <digit-0-9>) 
      (*parser <a-f>)
      (*parser <A-F>)
      (*disj 3)
     done))


;<HexUnicodeChar> ::= x<hexChar>+
(define <HexUnicodeChar>
  (new
    (*parser (char-ci #\x))
    (*parser <HexChar>) *plus
    (*caten 2)
    (*pack-with 
      (lambda (x ch) (integer->char (string->number (list->string ch) 16))) )
    ;(lambda (x ch) (string->number (list->string ch) 16))) 
    ;(*guard (lambda (num)  (< num  1114111) ))

    ;(lambda (ch) (integer->char ch ))
  done))

;/////
(define <whitespace>
  (const 
    (lambda(ch)
      (char<=? ch #\space))))


(define <VisibleSimpleChar> 
  (range #\x21 #\xff) )

(define <CharPrefix>
  (new
    (*parser (word "#\\" )) 
    done))

(define <NamedChar>
 (new 

    (*parser (^<meta-char> "lambda"  (integer->char 955)))
    (*parser (^<meta-char> "newline" (integer->char 10) ))
    (*parser (^<meta-char> "nul" (integer->char 0)))
    (*parser (^<meta-char> "page" (integer->char 12)))  
    (*parser (^<meta-char> "return" (integer->char 13)))
    (*parser (^<meta-char> "space" (integer->char 32) ))
    (*parser (^<meta-char> "tab" (integer->char 9) ))

    (*disj 7 )
    done))

(define <Char>
  (new 
    (*parser <CharPrefix> )
    
    (*parser <NamedChar> )
    (*parser <HexUnicodeChar> )
    (*parser <VisibleSimpleChar> )

    (*disj 3)

    (*caten 2)
    (*pack-with 
      (lambda (prefix ch) ch ))

  done))


;//////////////////////////////////////////////////////
;             Strings
;//////////////////////////////////////////////////////

(define <StringLiteralChar> 
  (new 
    (*parser <any-char>)
    (*parser (char #\\))
    *diff

  done))


(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f"  (integer->char 12)))

       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" (integer->char 13)))

    (*disj 6)
    

  done))


(define <StringHexChar>
  (new (*parser (char #\\))
     (*parser (char-ci #\x))
     (*parser <HexChar> ) *star
     (*parser (char #\;))

     (*caten 4)
     (*pack-with 
        ;(lambda (slash x ch end) (integer->char (string->number (list->string ch) 16))) )
      (lambda (slash x ch end)  (string->number (list->string ch) 16)) )
     
     (*guard (lambda (num)  (< num  1114111) ))
     (*pack (lambda (x) (integer->char x) ))

  done))

(define <StringChar>
  (new (*parser <StringHexChar>)
    (*parser <StringMetaChar>)
    (*parser <StringLiteralChar>)
    (*disj 3)
    (*parser (char #\"))
    *diff
  done))

(define <String> 
  (new (*parser (char #\"))
     (*parser <StringChar>) *star
     (*parser (char #\"))
     (*caten 3)

  (*pack-with
        (lambda (open-delim chars close-delim)
            (list->string chars)))

  done))
;//////////////////////////////////////////////////////
;             symbol
;//////////////////////////////////////////////////////


(define <SymbolChar>
  (new (*parser  (range #\0 #\9))
     (*parser  (range #\a #\z))
     (*parser  (range #\A #\Z))
     (*pack (lambda (ch) (char-downcase ch)))
     (*parser   (char #\!))
     (*parser   (char #\$))
     (*parser   (char #\^))
     (*parser   (char #\*))
     (*parser   (char #\-))
     (*parser   (char #\_))
     (*parser   (char #\=))
     (*parser   (char #\+))
     (*parser   (char #\<))
     (*parser   (char #\>))
     (*parser   (char #\?))
     (*parser   (char #\/))
     (*parser   (char #\:))

  (*disj 16)
  done))


(define <Symbol> 
  (new (*parser <SymbolChar>)*plus
   (*pack 
   (lambda (ch) 
    (string->symbol (list->string ch))))
  done))




;//////////////////////////////////////////////////////
;             ProperList
;//////////////////////////////////////////////////////
(define <space>
  (new (*parser (char #\ ))  *star
    done))



(define <ProperList111>
  (new  ;(*parser (char #\( ) )  
      
      
        (*parser (^<separated-exprs> (*delayed (lambda () <sexpr> ) ) <space> ) )

        
        ;(*parser (char #\) ) ) 
       ;(*caten 3)
     ;  (*pack-with
     ;    (lambda (openBar expr closeBar) expr ))
  done))



 ;***************** Mayer part  Start ******************

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>)) ;;;;;;
       (*delayed (lambda () <sexpr>))
       (*disj 2)  ;;;;;
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
  <sexpr-comment>))

 (define <skip>
  (disj <comment>
    <whitespace>))

 (define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))


(define ^<skipped*> (^^<wrapped> (star <skip>)))

1
 ;***************** Mayer part  END ******************

 ;////////////////////////////

(define <spaceRemove> 
  (new (*parser <whitespace>) *star
     (*delayed (lambda () <sexpr>))
     (*parser <whitespace>) *star
     (*caten 3)
     (*pack-with (lambda (s1 exps s2) exp) )

  done))

;//////////////////////////////////////////////////////
;             sexpr
;//////////////////////////////////////////////////////
(define <sexpr> 
  (^<skipped*>
    (new
      (*parser  <Boolean>)
      (*parser  <Char>)

      (*parser  <Number>) 

      (*parser  <Symbol>)
      (*parser <digit-0-9>)
      *diff
        *not-followed-by

      (*parser  <Symbol>)

      (*parser  <String>)
      (*delayed (lambda () <ProperList>))
      (*delayed (lambda () <ImproperList>))
      (*delayed (lambda () <Vector>))
      (*delayed (lambda () <Quoted>))
      (*delayed (lambda () <QuasiQuoted>))
      (*delayed (lambda () <Unquoted>))
      (*delayed (lambda () <UnquoteAndSpliced>))
      (*delayed (lambda () <InfixExtension>))
      ;(*parser <spaceRemove>)

      
    ; (*parser  <InfixExtension>)

      (*disj 13)  

  done)));)



;;;
; (test-string <sexpr> "(123 \"\\x6a;\" #t 1/4)" )
(define <CloseBarcket>
  (new (*parser <whitespace>) *star
      (*parser (char #\) ) )
     (*parser <whitespace>) *star

      (*caten 3)
       ; (*pack-with
          ;(lambda (openBar expr) expr ))
  
  done))
 (define <OpenBarcket>
  (new (*parser <whitespace>) *star
      (*parser (char #\( ) ) 
        (*parser <whitespace>) *star

      (*caten 3)
       ; (*pack-with
          ;(lambda (openBar expr) expr ))
  
  done))


(define <ProperList>
  (new   
      (*parser <OpenBarcket>)
      
  
      
      (*parser (^<separated-exprs> <sexpr>   <space>  ) )
      

        (*parser <CloseBarcket>)
       (*caten 3)
      (*pack-with
        (lambda (openBar expr closeBar) expr ))


      (*parser (char #\( ));empty no args
      (*parser (char #\)) )
      (*caten 2)
      (*pack-with
      (lambda (op cl)
                `() ))
      (*disj 2)

  done))


 (define <ImproperList>
  (new ;(*parser (char #\( ) )  
      (*parser <OpenBarcket>)
          (*parser <sexpr>) *plus
      (*parser (char #\.))
      (*parser <sexpr>)
      ;(*parser (char #\) ) )  
          (*parser <CloseBarcket>)

      (*caten 5)
      (*pack-with
      (lambda (a exp1 dot exp2 e)
          `(,@exp1 . ,exp2)
                            ))
    done))


  (define <Vector>
    (new (*parser (char #\#))
       (*parser <ProperList>)
        (*caten 2)
        (*pack-with
      (lambda (ch lst)
          (list->vector lst)  ))

    done))

 (define <Quoted>
    (new (*parser (char #\'))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quo expr)
          (list 'quote expr)))

    done))

 (define <QuasiQuoted>
    (new (*parser (char #\`))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quasi expr)
          (list 'quasiquote expr)))

    done))

 (define <Unquoted>
    (new (*parser (char #\,))
       (*parser <sexpr>) 
        (*caten 2)
       (*pack-with
      (lambda (quo expr)
          (list 'unquote expr)))

    done))

 (define <UnquoteAndSpliced>
    (new (*parser (char #\,))
       (*parser (char #\@))
       (*parser <sexpr>) 
        (*caten 3)
       (*pack-with
      (lambda (unquo s expr)
          (list 'unquote-splicing expr)))

    done))

;----------------------------------------------------------------------------

(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
     (*parser (char #\#))
     (*caten 2)

     (*parser (char #\#))
     (*parser (char #\%))
     (*caten 2)

     (*disj 2)
  done))


(define <Operators>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser (char #\*))
       (*parser (word "**"))
       (*parser (char #\^))
       (*parser (char #\/))
       (*disj 6)
  done))


(define <InfixSymbol>
  (new  (*parser <SymbolChar>)
      (*parser <Operators> )
    *diff
     *plus

     (*guard 
      (lambda (infixNumberSymbol) 
      (<Number> infixNumberSymbol
         (lambda (n remaining-chars )
          (if (null? remaining-chars) #f #t))
         (lambda (_) #t))))

     
    (*pack (lambda (sym)
     (string->symbol (list->string sym))))
  done))

(define <PlusOrMinus>  
  (new (*delayed (lambda () <MulOrDiv>))
 
        (*parser (^<skipped*> (char #\+ ) ) )
      (*parser (^<skipped*> (char #\- ) ) )
      (*disj 2)
      (*delayed (lambda () <MulOrDiv>))
          (*caten 2)

    (*pack-with 
        (lambda (oprator exp)
          (lambda (elment) 
            (list (string->symbol (string oprator)) elment exp) )))
      
      *star
      (*caten 2)

      (*pack-with 
        (lambda (e1 lmbe2)
          (fold-left 
            (lambda (elm lamb)  
              (lamb elm))
            e1 lmbe2)))
    done))
            

(define <MulOrDiv> 
    (new (*delayed (lambda () <Power>))

          (*parser (^<skipped*> (char #\* ) ) )
      (*parser (^<skipped*> (char #\/ ) ) )
      (*disj 2)
      (*delayed (lambda () <Power>))
      (*caten 2)
      (*pack-with
       (lambda (oprator exp)
          (lambda (elment) 
            (list (string->symbol (string oprator)) elment exp) )))

      *star
      (*caten 2)

      (*pack-with 
        (lambda (e1 lmblis)
          (fold-left 
            (lambda (elm lamb)  
              (lamb elm))
            e1 lmblis)))          
  done))


(define <Power> 
  (new (*delayed (lambda () <Helper> ))

          (*parser (^<skipped*> (char #\^ ) ) )
      (*parser (^<skipped*> (word "**" ) ) )
      (*disj 2)
      (*caten 2)
      

      (*pack-with 
        (lambda (exp oprator)
          (lambda (elment) 
            (list 'expt exp elment) )))
      *star

      (*delayed (lambda () <Helper> ))

      (*caten 2)
      (*pack-with 
       (lambda (lmblis e1)
          (fold-left
            (lambda (elm lamb)  
              (lamb elm))
              e1 (reverse lmblis))))    
  done))

(define <InfixNeg>
  (new (*parser  (char #\-))
      (*delayed (lambda () <Helper>))
      (*caten 2)
      (*pack-with
      (lambda (minus exp)
            ;  (- exp)))
                 `(,(string->symbol (string minus)),exp)))
      done))


(define <Helper> 
  (new (*parser <skip>) *star
     (*delayed (lambda () <InfixFuncall>))
     (*delayed (lambda () <InfixArrayGet>))
    ;     (*parser <InfixNeg>)

     (*delayed (lambda () <numExp>))
     (*delayed (lambda () <InfixSexprEscape> ))
     (*disj 4)
    (*parser <skip>) *star
    (*caten 3)
    (*pack-with
    (lambda (sk1 exp sk2) exp ))

     
  done))


(define <numExp> 
  (new (*parser <InfixSymbol> )
      (*parser <Number> )
      (*parser <InfixNeg>)
      
     ; (*parser <InfixSymbol> )
      
      (*parser <OpenBarcket> )  ;'('
          (*parser <PlusOrMinus> )
      (*parser <CloseBarcket>)  ;')'
      (*caten 3)
      (*pack-with 
        (lambda (openBar exp closeBar) exp ))

      (*disj 4)

  done))




(define <InfixArrayGet>
  (new (*parser <numExp>)

    
    (*parser  (^<skipped*> (char #\[ ) ))
    (*delayed (lambda () <InfixExpression>))
    (*parser (^<skipped*> (char #\])) )
      (*caten 3)
      (*pack-with
      (lambda (o infExp c)
        (lambda (elment)
          `(vector-ref ,elment ,infExp ))))
      *plus
      
      (*caten 2)
      (*pack-with 
        (lambda (numE vec)
          (fold-left (lambda (elem func)(func elem ))
            numE vec)))
      
     done))

;(f (x, y,g(x,z)) -> (f x y z) )

(define <InfixArgList>
  (new 
      (*parser <OpenBarcket>)
      
          (*delayed (lambda () <InfixExpression>))
      
      (*parser (char #\, ))
          (*delayed (lambda () <InfixExpression>))
      (*caten 2)
      (*pack-with (lambda (comma infixExp) infixExp))
      *star
      
      (*parser <CloseBarcket>)
      (*caten 4) 
      (*pack-with
          (lambda (open  infExp lstInfixExp close)
              (lambda (function)
                `(,function ,infExp  ,@lstInfixExp))))
    
      (*parser <OpenBarcket>);empty no args
      (*parser <CloseBarcket>)
      (*caten 2)
      (*pack-with
      (lambda (op cl)
              (lambda (function)
                `(,function ))))
      (*disj 2)

  done))

(define <InfixFuncall>
  (new (*parser <numExp>)
    (*parser <InfixArgList>)  *plus
      (*caten 2)
      (*pack-with 
        (lambda (func args) 
          (fold-left (lambda (elem lam)
                (lam elem) ) func   args )))
  done))




;-----------------------

(define <InfixExpression>
  (^<skipped*>
   (disj <PlusOrMinus> )))


(define <InfixExtension> 
  (new (*parser <InfixPrefixExtensionPrefix>)
     (*parser <InfixExpression>)
     (*caten 2)
     (*pack-with (lambda (inpre expr) expr ))
  done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix> )
     (*parser <sexpr>)
     (*caten 2)
     (*pack-with
     (lambda (pre exp) exp))
    done))

(define <Sexpr> <sexpr>)













(define *void-object* 
  (void) )

(define beginify
  (lambda (s)
    (cond
      ((null? s) *void-object*)
      ((null? (cdr s)) (cdr s))
      (else `(begin ,@s)))))

(define simple-const? 
  (lambda (x) 
    (or (null? x) (vector? x) (boolean? x) (char? x) (number? x) (string? x))))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* 
        letrec or quasiquote unquote unquote-splicing quote set!))


(define var? 
  (lambda (v) 
    (and (symbol? v) 
         (not (member v *reserved-words* ))))) 


	
;check a list to see if there are duplicates .
;Return true if there are no duplicates
(define hasNoDups?
  (lambda (lst)
    (if  (null? lst) 
         #t 
         (if (member (car lst) (cdr lst))
             #f ;there is duplicate we failed. 
             (hasNoDups? (cdr lst)) )
         )))


(define checkLambdaOptArg?
  (lambda (p)
    (if(pair? p)
       (and (var? (car p)) (checkLambdaOptArg? (cdr p) ))
       (var? p))
    ))


(define eraseNestedSeq
  (lambda (exp)
    (letrec ((nestedSeq (lambda (nestExp)
                          (if (equal? (car nestExp) 'seq)  
                              (fold-left append '() (map nestedSeq (cadr nestExp))) 
                              (list nestExp) ))))
      (list 'seq (fold-left append '() (map nestedSeq (cadr exp)))))))






(define beginCare
  (lambda (expr) 
    (letrec  
        ((beg (lambda (expr) 
                (if (null? expr) 
                    (list) 
                    (cons (parse (car expr)) (beg (cdr expr))))))) 
      `(seq ,(beg expr)) 
      ))
  )

(define let-var?
  (lambda (letvar)
    (and (list? letvar)
         (andmap list? letvar)
         (andmap pair? letvar)
         ;check that the letvar has a variable .
         (andmap (lambda (item) (var? (car item))) letvar) 	
         
         )))



(define get-args 
  (lambda (arg)
    (if (pair? arg)
        (cons (car arg) (get-args (cdr arg)))
        '()) 
    ))
(define get-rest
  (lambda (rest)
    (if (pair? rest) (get-rest (cdr rest)) rest)))


;;***********************************************************
;;				Mayer QQ
;;***********************************************************

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const? e1) (const? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;					PARSE 			              	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse
  (let ((run
         (compose-patterns
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;        IF          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;    
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))      
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		OR  		        ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(or ,(? 'first) . ,(? 'rest))
           (lambda (first rest)
             (if (null? rest) 
                 `( ,@(parse first) )
                 `(or (,(parse first) ,@(map parse rest) )))))
          
          (pattern-rule 
           `(or )
           (lambda () 
             (parse #f)))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Simple-lambda   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter list? hasNoDups? (lambda (v) (andmap var? v)) ) ,(? 'body) . ,(? 'rest ) )
           (lambda (para body rest)
             (let ((newbody (cons body rest)))
               `(lambda-simple ,para ,(parse `(begin ,@newbody ) ) )))) 
          
          
          
          ;lambda-opt (lambda (x y . rest) exp )
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Lambda-opt      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter pair?  checkLambdaOptArg?)  ,(? 'body) . ,(? 'rest ) )
           (lambda (para body rest )
             (let ((newbody (cons body rest))
                   (args   (get-args para))
                   (rest   (get-rest para)))
               `(lambda-opt ,args ,rest ,(parse `(begin ,@newbody ) ) )))) 
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		Lambda-var      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(lambda ,(? 'parameter var?) ,(? 'body). ,(? 'rest ) )
           (lambda (para body rest)
             (let ((newbody (cons body rest)))
               
               `(lambda-var ,para ,(parse `(begin ,@newbody)) ))))
          
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	  Regular define  ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(define ,(? 'var var?) .  ,(? 'expr) )
           (lambda (var expr)
             `(def ,(parse var) ,(parse  `(begin ,@expr) ))))
          
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		MIT-style define      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule
           `(define ,(? 'var  ) ,(? 'expr) . ,(? 'rest) )
           (lambda (var expr rest)
             `(def ,(parse (car var)) ,(parse `(lambda ,(cdr var) ,expr ,@rest) ))))
          
          ; MIT-style define <-----Need to check if this case is ligell or mayer parse has a bug.
          ;(define a b c d) ==> (def (var a) (seq ((var b) (var c) (var d))))
          (pattern-rule
           ;	`(define ,(? 'var  ) ,(? 'expr) )
           `(define ,(? 'var  ) ,(? 'expr) . ,(? 'rest) )
           (lambda (var expr rest)
             `(def ,(parse (car var)) ,(parse `(lambda ,(cdr var) ,expr ,@rest) ))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	Applications     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	
          (pattern-rule 
           `( ,(? 'expr (lambda (v) (not (member v *reserved-words* )))) . ,(? 'rest ))
           (lambda (expr rest)
             `(applic ,(parse expr) ,(map parse rest) )))
          
          
          ;;application
          
          (pattern-rule 
           `(begin . ,(? 'expr list? ) )
           (lambda (exprs)
             (cond ( (null? exprs) 	`(const ,*void-object*) ) 
                   ( (equal? (length exprs) 1) (parse (car exprs)))
                   (else 	
                    (eraseNestedSeq (beginCare exprs))
                    ))))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	   	Set! 		      ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(set! ,(? 'var) ,(? 'expr ) )
           (lambda (var expr)
             `(set ,(parse var) ,(parse expr) ) ) 
           )
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;	   	And 	      	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	 
          (pattern-rule
           `(and .,(? 'expr) )
           (lambda (expr)  ; `(,first ,rest)))
             (if (null? expr) 
                 (parse #t) 
                 (let ((first (car expr))
                       (dit (cdr expr)))
                   
                   (cond ((= (length expr) 1) (parse first )) 
                         (else 
                          (parse `(if  ,first  (and ,@dit) #f))))))))
          
          (pattern-rule
           `(and )
           (lambda () (parse #t)))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		    Cond 		    ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;	  
          (pattern-rule
           `(cond ( ,(? 'test) ,(? 'dit ) .,(? 'firstRest ) ) . ,(? 'rest) )
           (lambda (test dit ditRest rest)
             ;`(,test ,dit ,ditRest  ! ,rest)))
             (let ( ( dit   (cons dit ditRest ) ))
               (cond ((and (null? rest) (not (equal? 'else test) ) ) (parse `(if ,test  (begin ,@dit)  )))
                     ((and (null? rest) (equal? 'else test)  ) (parse `(begin ,@dit)) )
                     
                     (else (parse `(if ,test  (begin ,@dit) (cond ,@rest ) ) )))
               )))
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;     Quasiquote     ;; 
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(quasiquote .,(? 'expr-list list?))
           (lambda ( expr-list)
             (parse (expand-qq (car expr-list) ))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		let 	         	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(let () ,(? 'body))
           (lambda (body)
             (parse `((lambda () ,body) ))))
          
          (pattern-rule
           `(let ,(? 'varval  let-var?)  ,(? 'body) . ,(? 'rest list?))
           (lambda (var body rest)
             (let ((variables (map car var))
                   (values    (map cadr var) ))
               (parse `((lambda ,variables ,@(cons body rest)) ,@values)))))
          
          
          
          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		let* 		        ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (let ((b (cons expr exprs) )) 
               (parse `(let ()  ,@b)))))
          
          
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
           (lambda (var val rest exprs)	
             (if (not (null? rest))	
                 (parse `(let ((,var ,val))
                             (let* ,rest . ,exprs)))
                 (parse `(let ((,var ,val))
                             ,@exprs)))))
          

          ;;;;;;;;;;;;;;;;;;;;;;;;
          ;;		letrec*       	;;
          ;;;;;;;;;;;;;;;;;;;;;;;;              
          (pattern-rule
           `(letrec ,(? 'expr list?) ,@(? 'body))
           (lambda (expr body)
             (let ((variables (map car expr)))
               (parse `((lambda ,variables
                            (begin ,@(append (map (lambda (var_val) (list 'set! (car var_val) (cadr var_val))) expr) 
                                             `(((lambda () ,@body))))))
                          ,@(map (lambda (x) #f) expr))))))
          
          
          
          
          
          
          ))) 
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "I can't recognize this: ~s" e)))))))


(define parse-2 parse)