;; parametri del programma
(defparameter *stradario* "stradario.txt")
(defparameter *percorso* "percorso.txt")
(defparameter *out* "output.txt")

;; costanti utili
(defparameter *zero* (char-int #\0))
(defparameter *high* 10000)

(defun navigator ()
  "funzione principale del programma"
  (let*
      ((stradario (parse-stradario (split-string (read-file *stradario*) #\.)))
       (num (nth 0 stradario)) (citta (nth 1 stradario)) (tab (nth 2 stradario)) ;; imposto numero di citta', citta' e tabella
       (percorso (split-string (read-file *percorso*) #\.)) ;; parsing del percorso
       (distanze (make-dist num citta citta tab))) ;; creo tabella delle distanze iniziali
    (setq cities citta dists distanze) ;; alcune variabili globali
    (write-output percorso (get-path-floyd percorso)))) ;; stampa in output il risultato dopo aver processato l'input

(defun split-string (string char)
  "divide una stringa in una lista di stringhe dato un 
	carattere divisorio, char deve anche essere l'ultimo carattere"
  (setq temp-string "")
  (let
      ((dim (length string)) (ris ()))
    (do ((idx 0 (1+ idx)))
	((equal idx dim))
      ;; corpo del do
      (cond
       ((equal (elt string idx) char) 
	(setq ris (cons temp-string ris))
	(setq temp-string ""))
       (T (setq temp-string (concatenate 'string temp-string (char-to-string (elt string idx)))))))
    (reverse ris)))

(defun parse-stradario (strad)
  "divide lo stradario nelle sue 3 componenti tornandole al chiamante"
  (let ((num (get-num (car strad))))
    (list num (take num (drop 1 strad)) (map 'list 'get-num (drop (1+ num) strad)))))
    
(defun make-dist (num cities cits tab)
  "creo la tabella delle distanze per tutte le citta"
  (cond
   ((equal cits nil) nil) ;; caso base
   (T (append (city-dist (car cits) cities (take num tab)) (make-dist num cities (cdr cits) (drop num tab))))))

(defun city-dist (c cits tab)
  "crea le distanze per una certa citta"
  (cond
   ((equal cits nil) nil)
   ((< (car tab) 0) (city-dist c (cdr cits) (cdr tab))) ;; se la distanza e' minore di 0 non ci sono collegamenti e non la scrivo
   (T (cons (cons (list c (car cits)) (car tab)) (city-dist c (cdr cits) (cdr tab))))))

(defun read-file (input)
  "legge un file composto da una sola riga"
  (with-open-file (stream (merge-pathnames input))
		  (read-line stream)))

(defun get-num (string)
  "faccio il parsing numerico di una stringa"
  (let
      ((base (char-code #\0)) (ris 0) (dim (length string)))
    (do
	((idx 0 (incf idx)))
	((equal idx dim))
      (let*
	  ((exp (- dim (1+ idx))) ;; imposto l'esponente per trasformare correttamente in base 10
	   (n (power 10 exp 1))
	   (cifra (- (char-code (elt string idx)) base)))
	(incf ris (* n cifra))))
    ris))

(defun power (base exp r)
  "potenza di base alla exp"
  (cond
   ((zerop exp) r)
   (t (power base (1- exp) (* r base)))))

(defun get-dist (c1 c2)
  "ottiene la distanza fra due citta"
  (let 
      ((d (cdr (assoc (list c1 c2) dists :test 'equal))))
    (cond 
     ((equal d NIL) *high*) ;; in caso di collegamento mancante ritorno un valore molto alto per impedirne la scelta
     (T d))))

(defun char-to-string (char)
 (make-string 1 :initial-element char))

(defun get-path-floyd (percorso)
  "ottiene il percorso finale analizzando il risultato di floyd warshall"
  (floyd-warshall)
    (let
      ;; inserisco gia il primo elemento
      ((path (list (car percorso))) (tot_dist 0) (dim (length percorso)))
    (do
	((i 0 (incf i)))
	((equal i  (1- dim)))
      (let*
	  ((sh-path (get-min-path (nth i percorso) (nth (1+ i) percorso))) ;; ottengo il percorso piu breve fra due tappe adiacenti
	   (p  (car sh-path)) (d (cdr sh-path)))
	(cond
	 ((>= (cdr sh-path) *high*)
	  (print "percorso non totalmente percorribile") (return (list path tot_dist)))
	 ;; aggiungo con side effect le nuove tappe e incremento la distanza
	 (t (setq path (append path (but-last p) (list (car (last p))))) (incf tot_dist d)))))
    (list path tot_dist)))

(defun floyd-warshall ()
  "crea una tabella n*n con le distanze piu brevi fra le citta"
  ;; tengo due tabelle di hash, una in lettura (la precendente) e l'altra in scrittura
  (setq min-dist-old (make-hash-table :test 'equal)) (setq min-dist-new (make-hash-table :test 'equal))
  ;; primo ciclo imposta i collegamenti diretti fra le citta (k == 0)
  (dolist (c1 cities)
    (let*
	((pos (position c1 cities)) ;; in questo modo minimizzo le chiamate a get-dist
	 (sublist (drop pos cities)))
      (dolist (c2 sublist)
	(add-to-min-dist c1 c2 (list c2) (get-dist c1 c2) min-dist-old)
	(add-to-min-dist c2 c1 (list c1) (get-dist c1 c2) min-dist-old))))
  ;; ora vado iterativamente a calcolare tutte le distanze minime
  (do
      ((k 1 (incf k)))
      ((equal k (length cities)))
    (dolist (c1 cities)
      (dolist (c2 cities)
	(let*
	    ((direct (get-min-path c1 c2))
	     (before (get-min-path c1 (nth (1- k) cities)))
	     (after (get-min-path (nth (1- k) cities) c2))
	     (total (cons (append (car before) (car after)) (+ (cdr before) (cdr after)))))
	  (if
	      (<= (cdr direct) (cdr total)) ;; preferisco i percorsi diretti
	      ;; il percorso diretto per k-1 nodi e' il piu breve
	      (add-to-min-dist c1 c2 (car direct) (cdr direct) min-dist-new)
	    ;; percorso con una tappa
	    (add-to-min-dist c1 c2 (car total) (cdr total) min-dist-new)))))
    (setq min-dist-old min-dist-new)))

(defun add-to-min-dist (src dst infra dist hash)
  "aggiunge una riga alla tabella di hash"
  (let
      ((key (list src dst))
       (val (cons infra dist)))
    (setf (gethash key hash) val)))

(defun get-min-path (src dst)
  "prende dalla tabella di hash"
  (gethash (list src dst) min-dist-old))

(defun dot (percorso temp)
  "ritorna una stringa con freccie fra gli elementi della lista data in ingresso"
  (cond
   ((null (car (cdr percorso))) (concatenate 'string temp (car percorso)))
   (T (concatenate 'string (car percorso) "." (dot (cdr percorso) temp)))))

(defun mark (string)
  "marca una stringa"
  (concatenate 'string "*" string "*"))

(defun write-output (percorso risultato)
  "scrive in output il risultato finale"
  (let
      ((ris (first risultato))
       (dist (second risultato)))
	(format t "~A ~&~D ~&" 
		(dot ris "") dist)))
	; ((format nil (dot ris "")))))
    ; (print dist))))
;; funzioni su liste di varia utilita

(defun take (n list)
  "da haskell, prendo i primi n elementi"
  (let ((r ()))
    (dotimes (i n)
      (setq r (cons (nth i list) r)))
    (reverse r)))

(defun drop (n list)
  "da haskell, prendo la lista meno i primi n elementi"
  (let ((r ())
	 (dim (length list)))
    (do
	((i n (incf i)))
	((equal i dim))
      (setq r (cons (nth i list) r)))
    (reverse r)))

(defun but-last (list)
  "tutti meno ultimo elemento"
  (let 
      ((dim (length list)))
    (take (1- dim) list)))
