#lang racket
;Santiago Minga
;Daniel Loredo
;Test-Regex

;JSReader
(define (JSreader infile ) ;outfile
  (define in(open-input-file infile))
  ;(define out(open-output-file outfile))
  (define lst(read_file in))
  (display lst)
  ;(html_print lst out)
  (close-input-port in))
  ;(close-output-port out))
;read
(define (read_file file)
  (if (eof-object? (peek-char file))
      (list)
      (if(char-whitespace?(peek-char file))
         (and(read-char file)(read_file file))
         (if (not(equal?(regexp-match #rx"/\\*" (peek-string 2 0 file)) #f))
             (append(list(append(regexp-match  #rx"/\\*.*?\\*/" file)(list "Comentario Multilinea")))(read_file file))
             (if (not(equal?(regexp-match #rx"//" (peek-string 2 0 file)) #f))
                 (append(list(append(regexp-match #rx"//.*?\n" file)(list "Comentario"))) (read_file file))
                 (if(not(equal?(regexp-match #rx"var|const|let" (peek-string 5 0 file)) #f))
                    (append(list(append(regexp-match #rx"var |const |let.*?\n" file) (list "Identificador"))) (read_file file))
                    (append(list(append(list(read file)) (list "otro"))) (read_file file))))))))

;html_print
(define (html_print lst out)
  (list))


;ignorar
; |
; V
;mult-line-comment          
;identify
(define (identify line)
  (regexp-replace #rx" " line "\n")
  (define ch(open-input-string line))
  (identify_helper ch))
  
(define (identify_helper ch)
  (if (eof-object? (peek-char ch))
      (list)
      (if(not(equal?(regexp-match #rx"var|const|let" (peek-string 6 0 ch)) #f))
         (display(append(list(append(regexp-match #rx"var.*\r|const|let" (read-line ch))(list "Identificador")))(identify_helper ch)))
         (if(not(equal?(regexp-match #rx".=|=|\\+|\\*|/|-|%"  (peek-string 6 0 ch)) #f))
            (append(list(append(regexp-match #rx".=|=|\\+|\\*|/|-|%" (read-line ch))(list "Operador")))(identify_helper ch))
            (append(list(read-line ch))(identify_helper ch))))))
  
            
;operation
(define (operation ch)
  (list))

;var_list
(define (var_list ch)
  (list))

          
          
          



