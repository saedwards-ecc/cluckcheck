(module cluckcheck (gen-int gen-bool gen-char gen-list gen-string for-all)
    (import scheme chicken extras)

    (use srfi-1) ; lists

    (define (gen-int)
        (random 256))

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

    (define (valid-spec? spec)
        "check if the specification given to cluck check is a valid one"
        #f)

    (define (generate-spec-data spec)
        "generate a list with random data that matches what is defined in spec"
        #f)

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
                                (generate-spoec-data spec)
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
                                error-count))))))

    (define (for-all property gen #!optional (tests 100) . generators)
        (let* ((values (map (lambda (x) (map (lambda (f) (f)) (cons gen generators))) (iota tests)))
               (failure (find (lambda (vs) (not (apply property vs))) values)))
              (if (list? failure)
                    (display (format "*** Failed!\n~a\n" failure))
                    (display (format "+++ OK, passed ~a tests\n" tests))))))
