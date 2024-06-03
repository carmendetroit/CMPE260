;Yusuf Suat Polat
;2021400312
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))


(define (binary_to_decimal binary)
    (define (helper bin acc)
    (if (null? bin)
        acc
        (helper (cdr bin) (+ (* 2 acc) (string->number (string (car bin)))))))
  (helper (string->list binary) 0))

(define (relocator args limit base)
  (define (to-physical address)
    (let ((dec (binary_to_decimal address)))
      (if (> dec limit)
          -1
          (+ dec base))))
  (map to-physical args))

(define (log2 x)
  (/ (log x) (log 2)))

(define (divide_address_space address page-size-kb)
  (let* ([page-size-bits (exact-round (log2 (* page-size-kb 1024)))]
         [address-length (string-length address)]
         [page-number-length (- address-length page-size-bits)]
         [page-number (substring address 0 page-number-length)]
         [page-offset (substring address page-number-length)])
    (list page-number page-offset)))

(define (page address-list page-table page-size)
  (map (lambda (address)
         (let* ((divided (divide_address_space address page-size))
                (page-number (string->number (car divided) 2))  
                (page-offset (cadr divided))
                (frame-number (list-ref page-table page-number)))  
           (string-append frame-number page-offset)))  
       address-list))

(define (find_sin degree n)
  (define x (* degree (/ pi 180))) 
  (define (fact x)
    (if (= x 0)
        1
        (* x (fact (- x 1)))))
  (define (term k)
    (* (expt -1 k)
       (/ (expt x (+ (* 2 k) 1))
          (fact (+ (* 2 k) 1)))))
  (foldl + 0 (map term (range 0 n))))


(define (myhash arg table_size)
  (let* ((decimal (binary_to_decimal arg))
         (n-terms (+ 1 (modulo decimal 5)))
         (sin-value (find_sin decimal n-terms))
         (sin-str (number->string sin-value))
         (significant-digits (substring sin-str 2 12))
         (digits-list (string->list significant-digits))
         (digits-numbers (map (lambda (ch) (string->number (string ch))) digits-list))
         (digits-sum (foldl + 0 digits-numbers)))
    (modulo digits-sum table_size)))

(define (hashed_page arg table_size page_table page_size)
  (let* ((parts (divide_address_space arg page_size))  
         (page-number (car parts)) 
         (page-offset (cadr parts))  
         (hash-index (myhash page-number table_size))  
         (bucket (list-ref page_table hash-index)) 
         (frame-number (find-frame bucket page-number)))  
    (string-append frame-number page-offset)))  

(define (find-frame bucket page-number)
  (cond ((null? bucket) "000")  
        ((equal? (caar bucket) page-number) (cadar bucket)) 
        (else (find-frame (cdr bucket) page-number)))) 



(define (split_addresses args size)
  (map (lambda (start) (substring args start (+ start size)))
       (range 0 (string-length args) size)))

(define (map_addresses args table_size page_table page_size address_space_size)
  (let ((split-addrs (split_addresses args address_space_size)))
    (map (lambda (arg)
           (hashed_page arg table_size page_table page_size))
         split-addrs)))

