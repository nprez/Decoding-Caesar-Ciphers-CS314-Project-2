; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2016                                *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ctv", "vtc",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs.
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

(load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

(define spell-checker-helper
	(lambda (w d)
		(cond
			((pair? d) 
				(if (equal? (car d) w)
					#t
					(spell-checker-helper w (cdr d))
				)
			)
			(else #f)
		)
	)
)

;encodes a given paragraph using a given encoder
(define encode-d-helper
	(lambda (p encoder)
		(cond
			((pair? p)
				(append
					(list (encoder (car p)))
					(encode-d-helper (cdr p) encoder)
				)
			)
			(else '())
		)
	)
)

;;gets the number of correct words in a paragraph using a given decoder
(define correctlySpelledCount
	(lambda (p decoder)
		(if (pair? p)
			(if (spell-checker (decoder (car p)))
				(+ 1 (correctlySpelledCount (cdr p) decoder))
				(correctlySpelledCount (cdr p) decoder)
			)
			0
		)
	)
)

(define correctWordsList-helper
	(lambda (p n)
		(if (= n 25)
			(list (correctlySpelledCount
					p
					(lambda (w)
						(reduce
							(lambda (first second)
								(append
									;ctv((vtc(x)+26-n) mod 26)
									(list (vtc (modulo (- (+ 26 (ctv first)) n) 26)))
									second
								)
							)
							w
							'()
						)
					)
				  )
			)
			(append 
				(list (correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) n) 26)))
										second
									)
								)
								w
								'()
							)
						)
					  )
				)
				(correctWordsList-helper p (+ n 1))
			)
		)
	)
)

;;gets a list of number of correct words for a given paragraph for
;;each possible shift value
(define correctWordsList
	(lambda (p)
		(correctWordsList-helper p 0)
	)
)

(define indexOfMax-helper
	(lambda (l i maxIndex m)
		(if (pair? l)
			(if (> (car l) m)
				(indexOfMax-helper (cdr l) (+ i 1) i (car l))
				(indexOfMax-helper (cdr l) (+ i 1) maxIndex m)
			)
			maxIndex
		)
	)
)

;;returns the index of the greatest element of a list
(define indexOfMax
	(lambda (l)
		(indexOfMax-helper l 0 0 0)
	)
)

;;finds the correct n' value for a given paragraph p
(define findBestShiftValue-A
	(lambda (p)
		(indexOfMax (correctWordsList p))
	)
)

(define nthIndex-helper
	(lambda (l n soFar)
		(if (= soFar n)
			(car l)
			(nthIndex-helper (cdr l) n (+ soFar 1))
		)
	)
)

;;returns the element at index n in list l
(define nthIndex
	(lambda (l n)
		(nthIndex-helper l n 0)
	)
)

;;given a character, returns the shift value of that character from e
(define findShiftFromE
	(lambda (c)
		(modulo (- (+ (ctv c) 26) (ctv 'e)) 26)
	)
)

;;given a character, returns the shift value of that character from t
(define findShiftFromT
	(lambda (c)
		(modulo (- (+ (ctv c) 26) (ctv 't)) 26)
	)
)

;;given a character, returns the shift value of that character from a
(define findShiftFromA
	(lambda (c)
		(modulo (- (+ (ctv c) 26) (ctv 'a)) 26)
	)
)

;;given a paragraph and a character, returns the greatest number of correctly
;;spelled words, assuming the character represents e, t, or a
(define correctlySpelledCount-modified
	(lambda (p c)
		(let ((l (list
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromE c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromT c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromA c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
				 )))
			(nthIndex l (indexOfMax l))
		)
	)
)

;;given a paragraph and a character, returns the shift value to e, t, or a, depending on
;;which one would lead to the greatest number of correctly spelled characters
(define findShiftFromETA
	(lambda (p c)
		(let ((l (list
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromE c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromT c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
					(correctlySpelledCount
						p
						(lambda (w)
							(reduce
								(lambda (first second)
									(append
										;ctv((vtc(x)+26-n) mod 26)
										(list (vtc (modulo (- (+ 26 (ctv first)) (findShiftFromA c)) 26)))
										second
									)
								)
								w
								'()
							)
						)
					)
				 )))
			(cond
				((= (indexOfMax l) 0) (findShiftFromE c))
				((= (indexOfMax l) 1) (findShiftFromT c))
				((= (indexOfMax l) 2) (findShiftFromA c))
			)
		)
	)
)

(define findBestShiftValue-B-helper
	(lambda (l p index bestFitIndex bestFit)
		(if (pair? l)
			(if (> (correctlySpelledCount-modified p (car l)) bestFit)
				(findBestShiftValue-B-helper
					(cdr l)
					p
					(+ 1 index)
					index
					(correctlySpelledCount-modified p (car l))
				)
				(findBestShiftValue-B-helper (cdr l) p (+ 1 index) bestFitIndex bestFit)
			)
			bestFitIndex
		)
	)
)

;;given a list of frequent characters and a paragraph and finds the best shift value
;;if we assume one of them represents the encoded character e
(define findBestShiftValue-B
	(lambda (l p)
		(findShiftFromETA p (nthIndex l (findBestShiftValue-B-helper l p 0 0 0)))
	)
)

(define listAdd
	(lambda (a b)
		(if (pair? a)
			(append (list (+ (car a) (car b))) (listAdd (cdr a) (cdr b)))
			'()
		)
	)
)

;;returns the frequency of a given letter within a word
(define letterFrequency
	(lambda (w c)
		(if (pair? w)
			(if (eqv? c (car w))
				(+ 1 (letterFrequency (cdr w) c))
				(letterFrequency (cdr w) c)
			)
			0
		)
	)
)

(define frequencyListW-helper
	(lambda (w n)
		(if (= n 25)
			(list (letterFrequency w (vtc n)))
			(append (list (letterFrequency w (vtc n))) (frequencyListW-helper w (+ 1 n)))
		)
	)
)

;;creates a list of the frequencies of each of the 26 letters within a word
(define frequencyListW
	(lambda (w)
		(frequencyListW-helper w 0)
	)
)

;;creates a list of the frequencies of each of the 26 letters within a paragraph
(define frequencyListP
	(lambda (p)
		(if (pair? p)
			(listAdd (frequencyListW (car p)) (frequencyListP (cdr p)))
			'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		)
	)
)

(define maxFreqList-helper
	(lambda (l index maxFreq)
		(if (pair? l)
			(if (= maxFreq (car l))
				(append
					(list (vtc index))
					(maxFreqList-helper (cdr l) (+ index 1) maxFreq)
				)
				(maxFreqList-helper (cdr l) (+ index 1) maxFreq)
			)
			'()
		)
	)
)

;;given a list of character frequencies, constructs a list of the characters whose values
;;correspond to the indexes where the greatest frequency occurs
(define maxFreqList
	(lambda (l)
		(maxFreqList-helper l 0 (nthIndex l (indexOfMax l)))
	)
)

;;returns a list of the characters that appear the most frequently in paragraph p
;;usually returns a list of length 1, unless there are ties
(define mostFrequentChars
	(lambda (p)
		;make a list with the frequencies of each character
		;return a list that contains the vtc of the index of each occurence of the max frequency
		(maxFreqList (frequencyListP p))
	)
)

;;decodes paragraphs using a given decoder
(define Code-Breaker-helper
	(lambda (p decoder)
		(if (pair? p)
			(append
				(list (decoder (car p)))
				(Code-Breaker-helper (cdr p) decoder)
			)
			'()
		)
	)
)

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
	(lambda (w)
		(spell-checker-helper w dictionary)
	)
)

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate a Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input=a word, output=encoded word
(define encode-n
	(lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
		(lambda (w);;"w" is the word to be encoded
			(reduce
				(lambda (first second)
					(append
						;vtc((ctv(x) + n) mod 26)
						(list (vtc (modulo (+ n (ctv first)) 26)))
						second
					)
				)
				w
				'()
			)
		)
	)
)

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
	(lambda (d encoder)
		(cond
			((pair? d)
				(append
					(list (encode-d-helper (car d) encoder))
					(encode-d (cdr d) encoder)
				)
			)
			(else '())
		)
	)
)
		
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
	(lambda (p)
		(let ((n (findBestShiftValue-A p)))
			(lambda (w)
				(reduce
					(lambda (first second)
						(append
							;ctv((vtc(x)+26-n) mod 26)
							(list (vtc (modulo (- (+ 26 (ctv first)) n) 26)))
							second
						)
					)
					w
					'()
				)
			)
		)
	)
)

;;generate a decoder using frequency analysis
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-B
	(lambda (p)
		(let ((n (findBestShiftValue-B (mostFrequentChars p) p)))
			(lambda (w)
				(reduce
					(lambda (first second)
						(append
							;ctv((vtc(x)+26-n) mod 26)
							(list (vtc (modulo (- (+ 26 (ctv first)) n) 26)))
							second
						)
					)
					w
					'()
				)
			)
		)
	)
)

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
	(lambda (d decoder)
		(cond
			((pair? d)
				(append
					(list (Code-Breaker-helper (car d) decoder))
					(Code-Breaker (cdr d) decoder)
				)
			)
			(else '())
		)
	)
)

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)

;;(define testdoc (encode-d document (encode-n 5)))
;;(define testpar (car (cdr testdoc)))
;;(define decoder (Gen-Decoder-B testpar))