# 1BRC — PicoLisp (simple/idiomatic version)
# Usage: picolisp 1brc.l /path/to/measurements.txt

# PicoLisp has no floats — use integer arithmetic (temp * 10)

(de parse-temp (Str)
   "Parse temperature string to integer * 10 (e.g. '12.3' -> 123, '-5.1' -> -51)"
   (let (Neg NIL  Chars (chop Str)  Result 0)
      (when (= (car Chars) "-")
         (setq Neg T)
         (setq Chars (cdr Chars)) )
      (for C Chars
         (unless (= C ".")
            (setq Result (+ (* Result 10) (- (char C) (char "0")))) ) )
      (if Neg (- Result) Result) ) )

(de format-1dp (N)
   "Format integer*10 to string with 1 decimal place"
   (let (Neg (lt0 N)  Abs (abs N)  W (/ Abs 10)  F (% Abs 10))
      (if Neg
         (pack "-" W "." F)
         (pack W "." F) ) ) )

(de main ()
   (let (File (opt)  Tree NIL  Start (usec))
      (unless File
         (prinl "Usage: picolisp 1brc.l <file>")
         (bye 1) )
      # Read file and accumulate
      (in File
         (while (line T)
            (let L @
               (let Parts (split (chop L) ";")
                  (let (Name (pack (car Parts))
                        Temp (parse-temp (pack (cadr Parts))) )
                     (let Entry (lup Tree Name)
                        (if (and Entry (= (car Entry) Name))
                           (let V (cdr Entry)
                              (set V (min (car V) Temp))
                              (con V
                                 (list (max (cadr V) Temp)
                                    (+ (caddr V) Temp)
                                    (+ (cadddr V) 1) ) ) )
                           (idx 'Tree
                              (cons Name (list Temp Temp Temp 1))
                              T ) ) ) ) ) ) )
      # Collect sorted results
      (let Results (idx 'Tree)
         # Format output
         (let Parts
            (mapcar
               '((Entry)
                  (let (Name (car Entry)
                        V (cdr Entry)
                        Mn (car V)
                        Mx (cadr V)
                        Sum (caddr V)
                        Cnt (cadddr V)
                        Mean (/ (+ Sum (/ Cnt 2)) Cnt) )
                     (pack Name "=" (format-1dp Mn) "/" (format-1dp Mean) "/" (format-1dp Mx)) ) )
               Results )
            (prinl "{" (glue ", " Parts) "}") )
         (let Elapsed (/ (- (usec) Start) 1000)
            (out 2 (prinl "Elapsed: " Elapsed " ms")) ) ) ) ) )

(main)
(bye)
