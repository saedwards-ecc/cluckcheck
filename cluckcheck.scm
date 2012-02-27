(module cluckcheck (gen-int gen-bool gen-char gen-list gen-string for-all)
    (import scheme chicken extras)

    (use srfi-1) ; lists

    (define (gen-int)
        (random 256))

    (define (gen-real)
        (/ (random 10000) 10000.0))

    (define (gen-rational)
        (/ (gen-int) (gen-int)))

    (define (gen-bool)
        (= (random 2) 0))

    (define (gen-char)
        (integer->char (random 128)))

    (define (gen-list gen)
        (map (lambda (x) (gen)) (iota (random 100))))

    (define (gen-string)
        (list->string (gen-list gen-char)))

    (define (gen-vector gen)
        (list->vector (gen-list gen)))

    (define (scalar-spec-item? item)
        (or (eq? item 'int)
            (eq? item 'char)
            (eq? item 'string)
            (eq? item 'bool)
            (eq? item 'unit)
            (eq? item 'real)
            (eq? item 'rational)
            (eq? item 'natural)))

    (define (compound-spec-item? item)
        (or (eq? item 'list)
            (eq? item 'vector)))

    (define (valid-spec? spec)
        "check if the specification given to cluck check is a valid one"
        (cond
            ((null? spec) #t)
            ((scalar-spec-item? (car spec))
                (valid-spec? (cddr spec))) ;; cop out; should really check that cadr eq? '->
            ((compound-spec-item? (car spec))
                (if (and (>= (length (cdr spec)) 1)
                         (scalar-spec-item (cadr spec)))
                    (valid-spec? (cdddr spec))
                    #f))
            (else #f)))
               
    (define (generate-scalar-spec-data spec)
        " a simple swith & call the correct generator; it's meant to 
          reuse code between generate-spec-data & the compound data
          generators, gen-list, gen-vector"
        (cond
            ((eq? (car spec) 'int)
                (gen-int))
            ((eq? (car spec) 'real)
                (gen-real))
            ((eq? (car spec) 'rational)
                (gen-rational))
            ((eq? (car spec) 'natural)
                (gen-natural))
            ((eq? (car spec) 'char)
                (gen-char))
            ((eq? (car spec) 'string)
                (gen-string)) 
            ((eq? (car spec) 'unit)
                (void))
            ((eq? (car spec) 'bool)
                (gen-bool))))
            
    ;; it might be easier for large/complex systems if specs were "compiled" into lambdas
    ;; that could be simply run for each pass of cluckcheck. 
    ;; also, need to cut the last item off, since we don't want to generate data for the 
    ;; result here...
    (define (generate-spec-data spec)
        "generate a list with random data that matches what is defined in spec"
        (cond
            ((null? spec) '())
            ((scalar-spec-item? (car spec))
             (cons (generate-scalar-spec-data (car spec)) (generate-spec-data (cdr spec))))
            ((eq? (car spec) 'list)
             (cons (generate-list (lambda () (generate-scalar-spec-data (cadr spec))))
                   (generate-spec-data (cddr spec))))
            ((eq? (car spec) 'vector)
             (cons (generate-vector (lambda () (generate-scalar-spec-data (cadr spec))))
                   (generate-spec-data (cddr spec))))))
             
    (define (spec-pass? spec result)
        "verify that a result is in line with its specification"
        #f)

    (define (cluckcheck property spec #!optional (tests 100) (verbose #f))
        " given a property and a specification, test if this spec holds for this property"
        (if (valid-spec? spec)
            (let test-loop ((test-offset 0)
                            (test-data (generate-spec-data spec)
                            (error-count 0)
                            (failure-count 0)
                            (pass-count 0)))
                (if (< test-offset tests)
                    (guard (cnd (display (format "--- Error!\n~a\n" cnd)) 
                            (test-loop
                                (+ test-offset 1)
                                (generate-spec-data spec) 
                                (+ error-count 1)
                                failure-count
                                pass-count))
                        (if (spec-pass? spec (apply property test-data))
                            (test-loop
                                (+ test-offset 1)
                                (generate-spec-data spec)
                                error-count
                                failure-count
                                (+ pass count 1))
                            (begin
                                (when verbose
                                    (display "*** Failure!\n"))
                                (test-loop
                                    (+ test-offset 1)
                                    (generate-spec-data spec)
                                    error-count
                                    (+ failure-count 1)
                                    passcount))))
                    (display (format "+++ Passing tests: ~a\n*** Failing tests: ~a\n--- Errors: ~a\n"
                                pass-count
                                failure-count
                                error-count))))
            (display "Invalid specification\n")))

    (define (for-all property . generators)
        (let* ((values (map (lambda (x) (map (lambda (f) (f)) generators)) (iota 100)))
               (failure (find (lambda (vs) (not (apply property vs))) values)))
              (if (list? failure)
                  (display (format "*** Failed!\n~a\n" failure))
                  (display (format "+++ OK, passed 100 tests\n"))))))
