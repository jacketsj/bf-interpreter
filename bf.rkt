#lang racket
;; String String -> String
;; Produces the output of a given BF program with the given input

;; Example BF code
(check-expect (bf "" "") "")
(check-expect (bf "+++++++++++++++++++++++++++++++++." "") "!")
(check-expect (bf ",[.[-],]" "a") "a")
(check-expect (bf ",[.[-],]" "ab") "ab")
(check-expect (bf "+++++++++[>+++[>++<-]<-]>>." "") "6")
(check-expect (bf "+++++++++[<+++[<++>-]>-]<<." "") "6")
(check-expect (bf ",.,." "ab") "ab")
(check-expect (bf ",.,.,.,." "abab") "abab")
(check-expect (bf ",.,.,.,.,.,." "bbtghi") "bbtghi")
(check-expect (bf ",.>,." "ab") "ab")
(check-expect (bf "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." "") "Hello World!\n")

(define (bf pgrm in)
  (local [(define (bf mem reg rpoint ipoint out in)
            (cond [(= ipoint (string-length mem)) (list->string (reverse out))]
                  [(char=? (string-ref mem ipoint) #\>)
                   (if (= rpoint (sub1 (length reg)))
                       (bf mem (append reg (list 0)) (add1 rpoint) (add1 ipoint) out in)
                       (bf mem reg (add1 rpoint) (add1 ipoint) out in))]
                  [(char=? (string-ref mem ipoint) #\<)
                   (if (= rpoint 0)
                       (bf mem (cons 0 reg) rpoint (add1 ipoint) out in)
                       (bf mem reg (sub1 rpoint) (add1 ipoint) out in))]
                  [(char=? (string-ref mem ipoint) #\+)
                   (bf mem (add-at reg rpoint) rpoint (add1 ipoint) out in)]
                  [(char=? (string-ref mem ipoint) #\-)
                   (bf mem (sub-at reg rpoint) rpoint (add1 ipoint) out in)]
                  [(char=? (string-ref mem ipoint) #\[)
                   (if (= 0 (list-ref reg rpoint))
                       (bf mem reg rpoint (add1 (forward (add1 ipoint) mem 1)) out in)
                       (bf mem reg rpoint (add1 ipoint) out in))]
                  [(char=? (string-ref mem ipoint) #\])
                   (bf mem reg rpoint (backward (sub1 ipoint) mem 1) out in)]
                  [(char=? (string-ref mem ipoint) #\.)
                   (bf mem reg rpoint (add1 ipoint) (cons (integer->ascii (list-ref reg rpoint)) out) in)]
                  [(char=? (string-ref mem ipoint) #\,)
                   (if (string=? "" in)
                       (bf mem reg rpoint (add1 ipoint) out in)
                       (bf mem (set-at reg rpoint (ascii->integer (string-ref in 0))) rpoint (add1 ipoint) out (substring in 1)))]))
                  [else
                   (bf mem reg rpoint (add1 ipoint) out in)]
          (define (add-at reg p)
            (build-list (length reg)
                        (λ (r) (if (= r p)
                                   (if (= 255 (list-ref reg r))
                                       0
                                       (add1 (list-ref reg r)))
                                   (list-ref reg r)))))
          (define (sub-at reg p)
            (build-list (length reg)
                        (λ (r) (if (= r p)
                                   (if (= 0 (list-ref reg r))
                                       255
                                       (sub1 (list-ref reg r)))
                                   (list-ref reg r)))))
          (define (set-at reg p j)
            (build-list (length reg)
                        (λ (r) (if (= r p)
                                   j
                                   (list-ref reg r)))))
          (define (forward p mem c)
            (cond [(and (char=? (string-ref mem p) #\]) (<= c 1)) p]
                  [(char=? (string-ref mem p) #\]) (forward (add1 p) mem (sub1 c))]
                  [(char=? (string-ref mem p) #\[) (forward (add1 p) mem (add1 c))]
                  [else (forward (add1 p) mem c)]))
          (define (backward p mem c)
            (cond [(and (char=? (string-ref mem p) #\[) (<= c 1)) p]
                  [(char=? (string-ref mem p) #\[) (backward (sub1 p) mem (sub1 c))]
                  [(char=? (string-ref mem p) #\]) (backward (sub1 p) mem (add1 c))]
                  [else (backward (sub1 p) mem c)]))]
    (bf pgrm (list 0) 0 0 empty in)))
 
 
 ;; Explicit ascii conversions for consistency (auto-generated)
(define (ascii->integer c)
  (cond [(char=? c #\n) 10]
        [(char=? c #\ ) 32]
        [(char=? c #\!) 33]
        [(char=? c #\“) 34]
        [(char=? c #\#) 35]
        [(char=? c #\$) 36]
        [(char=? c #\%) 37]
        [(char=? c #\&) 38]
        [(char=? c #\') 39]
        [(char=? c #\() 40]
        [(char=? c #\)) 41]
        [(char=? c #\*) 42]
        [(char=? c #\+) 43]
        [(char=? c #\,) 44]
        [(char=? c #\-) 45]
        [(char=? c #\.) 46]
        [(char=? c #\/) 47]
        [(char=? c #\0) 48]
        [(char=? c #\1) 49]
        [(char=? c #\2) 50]
        [(char=? c #\3) 51]
        [(char=? c #\4) 52]
        [(char=? c #\5) 53]
        [(char=? c #\6) 54]
        [(char=? c #\7) 55]
        [(char=? c #\8) 56]
        [(char=? c #\9) 57]
        [(char=? c #\:) 58]
        [(char=? c #\;) 59]
        [(char=? c #\<) 60]
        [(char=? c #\=) 61]
        [(char=? c #\>) 62]
        [(char=? c #\?) 63]
        [(char=? c #\@) 64]
        [(char=? c #\A) 65]
        [(char=? c #\B) 66]
        [(char=? c #\C) 67]
        [(char=? c #\D) 68]
        [(char=? c #\E) 69]
        [(char=? c #\F) 70]
        [(char=? c #\G) 71]
        [(char=? c #\H) 72]
        [(char=? c #\I) 73]
        [(char=? c #\J) 74]
        [(char=? c #\K) 75]
        [(char=? c #\L) 76]
        [(char=? c #\M) 77]
        [(char=? c #\N) 78]
        [(char=? c #\O) 79]
        [(char=? c #\P) 80]
        [(char=? c #\Q) 81]
        [(char=? c #\R) 82]
        [(char=? c #\S) 83]
        [(char=? c #\T) 84]
        [(char=? c #\U) 85]
        [(char=? c #\V) 86]
        [(char=? c #\W) 87]
        [(char=? c #\X) 88]
        [(char=? c #\Y) 89]
        [(char=? c #\Z) 90]
        [(char=? c #\[) 91]
        [(char=? c #\\) 92]
        [(char=? c #\]) 93]
        [(char=? c #\^) 94]
        [(char=? c #\_) 95]
        [(char=? c #\`) 96]
        [(char=? c #\a) 97]
        [(char=? c #\b) 98]
        [(char=? c #\c) 99]
        [(char=? c #\d) 100]
        [(char=? c #\e) 101]
        [(char=? c #\f) 102]
        [(char=? c #\g) 103]
        [(char=? c #\h) 104]
        [(char=? c #\i) 105]
        [(char=? c #\j) 106]
        [(char=? c #\k) 107]
        [(char=? c #\l) 108]
        [(char=? c #\m) 109]
        [(char=? c #\n) 110]
        [(char=? c #\o) 111]
        [(char=? c #\p) 112]
        [(char=? c #\q) 113]
        [(char=? c #\r) 114]
        [(char=? c #\s) 115]
        [(char=? c #\t) 116]
        [(char=? c #\u) 117]
        [(char=? c #\v) 118]
        [(char=? c #\w) 119]
        [(char=? c #\x) 120]
        [(char=? c #\y) 121]
        [(char=? c #\z) 122]
        [(char=? c #\{) 123]
        [(char=? c #\|) 124]
        [(char=? c #\}) 125]
        [(char=? c #\~) 126]
        [else 255]))
 
(define (integer->ascii i)
  (cond [(= i 10) (string-ref "\n" 0)]
        [(= i 32) #\ ]
        [(= i 33) #\!]
        [(= i 34) #\“]
        [(= i 35) #\#]
        [(= i 36) #\$]
        [(= i 37) #\%]
        [(= i 38) #\&]
        [(= i 39) #\']
        [(= i 40) #\(]
        [(= i 41) #\)]
        [(= i 42) #\*]
        [(= i 43) #\+]
        [(= i 44) #\,]
        [(= i 45) #\-]
        [(= i 46) #\.]
        [(= i 47) #\/]
        [(= i 48) #\0]
        [(= i 49) #\1]
        [(= i 50) #\2]
        [(= i 51) #\3]
        [(= i 52) #\4]
        [(= i 53) #\5]
        [(= i 54) #\6]
        [(= i 55) #\7]
        [(= i 56) #\8]
        [(= i 57) #\9]
        [(= i 58) #\:]
        [(= i 59) #\;]
        [(= i 60) #\<]
        [(= i 61) #\=]
        [(= i 62) #\>]
        [(= i 63) #\?]
        [(= i 64) #\@]
        [(= i 65) #\A]
        [(= i 66) #\B]
        [(= i 67) #\C]
        [(= i 68) #\D]
        [(= i 69) #\E]
        [(= i 70) #\F]
        [(= i 71) #\G]
        [(= i 72) #\H]
        [(= i 73) #\I]
        [(= i 74) #\J]
        [(= i 75) #\K]
        [(= i 76) #\L]
        [(= i 77) #\M]
        [(= i 78) #\N]
        [(= i 79) #\O]
        [(= i 80) #\P]
        [(= i 81) #\Q]
        [(= i 82) #\R]
        [(= i 83) #\S]
        [(= i 84) #\T]
        [(= i 85) #\U]
        [(= i 86) #\V]
        [(= i 87) #\W]
        [(= i 88) #\X]
        [(= i 89) #\Y]
        [(= i 90) #\Z]
        [(= i 91) #\[]
        [(= i 92) #\\]
        [(= i 93) #\]]
        [(= i 94) #\^]
        [(= i 95) #\_]
        [(= i 96) #\`]
        [(= i 97) #\a]
        [(= i 98) #\b]
        [(= i 99) #\c]
        [(= i 100) #\d]
        [(= i 101) #\e]
        [(= i 102) #\f]
        [(= i 103) #\g]
        [(= i 104) #\h]
        [(= i 105) #\i]
        [(= i 106) #\j]
        [(= i 107) #\k]
        [(= i 108) #\l]
        [(= i 109) #\m]
        [(= i 110) #\n]
        [(= i 111) #\o]
        [(= i 112) #\p]
        [(= i 113) #\q]
        [(= i 114) #\r]
        [(= i 115) #\s]
        [(= i 116) #\t]
        [(= i 117) #\u]
        [(= i 118) #\v]
        [(= i 119) #\w]
        [(= i 120) #\x]
        [(= i 121) #\y]
        [(= i 122) #\z]
        [(= i 123) #\{]
        [(= i 124) #\|]
        [(= i 125) #\}]
        [(= i 126) #\~]
        [else #\`]))
