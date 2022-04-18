#lang racket
;Santiago Minga
;Daniel Loredo
;Test-Regex

;JSReader
(define (JSreader infile ) ;outfile
  (define in(open-input-file infile))
  ;(define out(open-output-file outfile))
  (define lst(read_file in))
  (write lst)
  (display lst)
  ;(html_print lst out)
  (close-input-port in))
  ;(close-output-port out))
;read
(define (read_file file)
  (if (eof-object? (peek-char file))
      (list)
      (if(char-whitespace?(peek-char file))
         (append(list(read-char file))(read_file file))
         ;Comentarios Multilinea
         (if (is_comentario_mult file)             
             (append(comentario_mult  file)(read_file file))
              ;Comentarios 
             (if (is_comentario file)
                 (append(comentario file)(read_file file))
                 ;Operadores
                 (if (is_operador file)
                    (append(operador file) (read_file file))
                    ;Identificadores
                    (if (is_identificador file)                       
                       (append (identificador file)(read_file file))
                       ;Palabras Reservadas
                       (if (is_palabra_reservada file)                         
                          (append(palabra_reservada file) (read_file file))
                          ;Strings
                          (if (is_string file)                             
                             (append (strings file)(read_file file))
                             ;Numeros
                             (if (is_num file)                               
                                (append (numeros file) (read_file file))
                                ;Delimitadores
                                (if (is_del file)
                                   (append (delimitadores file)(read_file file))
                                   ;Funciones
                                   (if (is_func file)
                                      (append (funciones file) (read_file file))                               
                                      (append(list(append(regexp-match  #rx".*? " file) (list "Purple"))) (read_file file))))))))))))))

;html_print
(define (html_print lst out)
  (list))

;is_comentario_mult
(define (is_comentario_mult file)
  (not(equal?(regexp-match #rx"/\\*" (peek-string 2 0 file)) #f)))
;comentario_mult       
(define (comentario_mult  file)
  (list(append(regexp-match  #rx"/\\*.*?\\*/" file)(list "Green"))))
;is_comentario
(define (is_comentario file)
  (not(equal?(regexp-match #rx"//" (peek-string 2 0 file)) #f)))
;comentario
(define (comentario file)
  (list(append(regexp-match #rx"//.*?\n" file)(list "Green"))))
;is_operator
(define (is_operador file)
  (not(equal?(regexp-match #rx"=|!|\\+|\\*|/|-|%|>|<|\\(|\\)|\\[|\\]|{|}|;|,"  (peek-string 1 0 file)) #f)))
;operator
(define (operador file)
  (list(append(regexp-match #rx"===|==|=|!=|!==|\\+=|\\+|\\*=|\\*|\\*{2}=|\\*{2}|/=|/|-=|-|%=|%|>=|>|<=|<|\\(|\\)|\\[|\\]|{|}|;|," file) (list "Black"))))
;is_identificador
(define (is_identificador file)
  (not(equal?(regexp-match #rx"var |const |let " (peek-string 6 0 file)) #f)))
;identificador
(define (identificador file)
  (list(append(regexp-match #rx"var |const |let " file) (list "LightBlue"))))
;is_palabra_reservada
(define (is_palabra_reservada file)
  (not(equal?(regexp-match #rx"float |int |boolean |return |in " (peek-string 8 0 file)) #f)))
;palabra_reservada
(define (palabra_reservada file)
  (list(append(regexp-match #rx"float |int |boolean |return |in " file) (list "Blue"))))
;is_strings
(define (is_string file)
  (not(equal?(regexp-match #rx"\"" (peek-string 2 0 file)) #f)))
;strings
(define (strings file)
  (list(append(regexp-match #rx"\".*\"" file) (list "LightGreen"))))
;is_num
(define (is_num file)
  (not(equal?(regexp-match #rx"-?[1234567890]" (peek-string 2 0 file)) #f)))
;numeros
(define (numeros file)
  (list(append(regexp-match #rx"-?[1234567890]*[\\.e]?-?[1234567890]*" file) (list "Red"))))
;is_del
(define (is_del file)
  (not(equal?(regexp-match #rx"if |for" (peek-string 3 0 file)) #f)))
;delimitarores
(define (delimitadores file)
  (list(append(regexp-match #rx"if |for" file) (list "DarkRed"))))
;is_func
(define (is_func file)
  (not(equal?(regexp-match #rx"function " (peek-string 9 0 file)) #f)))
;funciones
(define (funciones file)
  (list(append(regexp-match #rx"function " file) (list "Orange"))))

;Execute
(JSreader "CasoPrueba1.txt")

          
          
          



