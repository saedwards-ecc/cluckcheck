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

    (define (cluckcheck property spec #!optional (tests 100) (verbose #f))
        #f) 

	(define (for-all property gen #!optional (tests 100) . generators)
		(let* ((values (map (lambda (x) (map (lambda (f) (f)) (cons gen generators))) (iota tests)))
			 (failure (find (lambda (vs) (not (apply property vs))) values)))
				(if (list? failure)
					(display (format "*** Failed!\n~a\n" failure))
					(display (format "+++ OK, passed ~a tests\n" tests))))))
