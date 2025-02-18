#lang racket

(require racket/list) ; Import list utilities

; Function to read integers from a file efficiently
(define (read-integers filename)
  (define in-port (open-input-file filename))
  (let loop ([integers '()])
    (define num (read in-port))
    (if (eof-object? num)
        (begin
          (close-input-port in-port)
          (reverse integers))
        (loop (cons num integers)))))

; Function to create value-frequency pairs efficiently
(define (create-frequency-pairs integers)
  (for/fold ([freq-map (hash)]) ([num integers])
    (hash-update freq-map num add1 0))) ; Efficient frequency count using hash map

; Merge Sort function for better sorting performance
(define (merge-sort lst)
  (define (merge left right)
    (cond
      [(null? left) right]
      [(null? right) left]
      [(<= (car left) (car right)) (cons (car left) (merge (cdr left) right))]
      [else (cons (car right) (merge left (cdr right)))]))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ([mid (quotient (length lst) 2)]
             [left (take lst mid)]
             [right (drop lst mid)])
        (merge (merge-sort left) (merge-sort right)))))

; Function to sort the frequency pairs by value
(define (sort-pairs pairs)
  (merge-sort (map car pairs))) ; Use merge-sort to sort by the number

; Function to print the sorted value-frequency pairs in the desired format on the same line
(define (print-pairs pairs)
  (for-each (λ (pair)
              (printf "(~a ~a) " (car pair) (cdr pair))) ; Print in the format (number No.oftimes repeated)
            pairs)
  (newline)) ; Move to the next line after printing all pairs

; Function to ask the user if they want to go again
(define (ask-to-go-again)
  (printf "Do you want to process another file? (yes/no): ")
  (let ([response (read-line)])
    (string-ci=? response "yes")))

; Main function
(define (main)
  (let loop ()
    (printf "Enter the filename (in the same folder): ")
    (let ([filename (read-line)])
      (if (file-exists? filename)
          (let* ([integers (read-integers filename)]
                 [freq-map (create-frequency-pairs integers)]
                 [pairs (hash->list freq-map)]
                 [sorted-keys (sort-pairs pairs)]
                 [sorted-pairs (map (λ (key) (cons key (hash-ref freq-map key))) sorted-keys)])
            (print-pairs sorted-pairs))
          (printf "File not found. Please try again.\n")))
    (if (ask-to-go-again)
        (loop)
        (printf "Goodbye!\n"))))

; Start the program
(main)
