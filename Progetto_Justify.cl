;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                       ;;;;;;;;
;;;;;;      PROJECT   JUSTIFY    ;;;;;;
;;;;;;;;                       ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;											                      ;;
;;				NOTE SULL'AVVIO DELL'ALGORITMO				          ;;
;;											                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PER INIZIARE: Digitare al prompt '(start 1)'

;;
;; MENU INIZIALE

;; Il menu permette la scelta di 3 opzioni: 

;; 1 - Imposta parametri: permette di impostare i parametri per la giustificazione del testo. Il testo
;;     non può essere giustificato se prima non vengono impostati i parametri.
;;     Questa opzione richiederà la definizione del valore di Jwidth e Jlimit.
;;     Dovranno inoltre essere inseriti i percorsi per i file di input e output nel formato:
;;     "c:/nome_percorso/nome_file.txt". (virgolette comprese)

;; 2 - Giustifica il testo: procede con la giustificazione

;; 3 - Esci

(defun start (scelta)
  (while (not(equal scelta 3))
  (format t "~%~%Selezionare l'operazione desiderata~%~%")
  (format t "1 - Imposta Parametri~%")
  (format t "2 - Giustifica il testo~%")
  (format t "3 - Esci~%")
  (setq scelta (read))
    (cond ((equal scelta 1)(format t "Impostare parametro Jwidth: ")
                           (setq jwidth (+ (read) 1))
                           (format t "~%Impostare parametro Jlimit (da 0 a 1): ")
                           (setq jlimit (read))
                           (format t "~%Percorso file di Input (immettere il percorso tra virgolette):")
                           (setq file_input (read))
                           (format t "~%Percorso file di Output (immettere il percorso tra virgolette):")
                           (setq file_output (read))
           )
          
          ;; Vengono inizializzate le variabili la cui funzionalità verrà analizzata in seguito
          
          ((equal scelta 2) (setq last-space 0)
                            (setq num-spaces 0)
                            (setq lista-def ())
                            (setq lista ())
                            (setq val 0)
                            (setq lista-format ())
                            (setq flag NIL)
          ;; Memorizza nella lista "lista-chars" carattere per carattere, l'intero file in input
                            (setq lista-chars (with-open-file (a  file_input  :direction  :input)
                            (read-all-chars  a) ))
                            (setq lista-chars (preformat lista-chars))
                            (proc1 lista-chars)
          ;; La lista di caratteri viene trasformata in una stringa più 'leggibile' e salvata su file
                            (setq lista-def (mkstr lista-def))
                            (salva_file file_output)
                            
           )
                            
                         
                            
          ((equal scelta 3) (format t "Arrivederci!"))
          )
  )
  )

;; proc1
;; viene passata una lista in ingresso sulla quale verrà effettuato il controllo

(defun proc1 (lst)
  (cond ((null lst) lista-def)
        (t (dotimes (i (+ 1 jwidth))
    
             ;; Il ciclo scandisce, carattere per carattere, l'intera lista fino al raggiungimento 
             ;; del carattere alla posizione jwidth. Se viene individuato un Newline,
             ;; il campo flag viene impostato su T, il valore di val equivale all'indice i, ossia alla
             ;; lunghezza della linea appena scandita. num, il numero di spazi che deve essere
             ;; eventualmente inserito, è uguale a 0 in quanto la linea non richiede alcun trattamento
             
                   
                   (cond ((equalp (estrai i lst) #\newline)
                          (setq val i)
                          (setq num 0)
                          (setq flag T)
                          
               ;; Esco dal ciclo             
                          (setq i (+ 2 jwidth)))
          
                         ;; Se il carattere trovato è uno spazio, lo memorizzo nella lista spaces.
                         ;; Anche nel caso in cui lo spazio sia alla posizione numero 'jwidth'
                         ;; la linea non richiede alcun trattamento, quindi num = 0.
          
                         ((equalp (estrai i lst) #\space)  
                          (setq last-space i)
                          (setq num-spaces (+ 1 num-spaces)) (setq val i) (setq num 0))
                         
                         ;; Se nessuna delle precedenti condizioni è soddisfatta, il valore della variabile
                         ;; val sarà uguale a jwidth, e il numero di spazi da inserire num sarà dato
                         ;; dalla differenza tra il valore di jwidth e l'ultimo spazio di posizione < a jwidth
                         
                         ((equalp i jwidth) (setq val jwidth) (setq num (- jwidth last-space)))
                   )
           )
           
           ;; Se il campo flag è impostato a T (caso di una linea di lunghezza inferiore al parametro preimpostato
           ;; jwidth) o se è possibile giustificare la linea senza l'ausilio della silabazione, allora il controllo
           ;; viene passato alla procedura giustifica...
           
           (if (or flag (verifica last-space num-spaces))
              (setq lista-chars (giustifica lista-chars val num))
             
             ;;...altrimenti è necessario l'ausilio della sillabazione
             
             (sillabazione (- val 1) lista-chars 0))
          (setq flag NIL)
          (proc1 lista-chars))
          )
  )




;; Procedura Sillabazione
;; Viene controllato tramite la sottoprocedura c-sillabaz se è possibile effettuare una corretta suddivisione in base
;; alle principali regole grammaticali.
;; La procedura continua ricorsivamente, decrementando di volta in volta l'indice i, fino a quando non viene trovata
;; la corretta sillabazione

(defun sillabazione (i lst num)
  (cond ((c-sillabaz (estrai (- i 1) lst) (estrai i lst) (estrai (+ 1 i) lst))
         (setq lista-chars (giustifica (f-sillabaz lista-chars i) jwidth num)))
        (t (sillabazione (- i 1) lst (+ num 1)))
  )
)

;; La procedura f-sillabaz si occupa della formattazione del testo in modo che il
;; penultimo carattere della riga sia un '-' e l'ultimo un Newline

(defun f-sillabaz (lst i)
  (cond ((> i 1) (setq lst (cons (car lst) (f-sillabaz (cdr lst) (- i 1)))) )
        ((= i 1) (setq lst (cons #\- (f-sillabaz lst (- 1 i) ))) )
        ((= i 0) (setq lst (cons #\space (f-sillabaz lst(- i 1)))))
        (t lst))
  )

;; Restituisce T se char è una vocale

(defun vocale (char)
  (if (or (equal char #\a) (equal char #\e) (equal char #\i) (equal char #\o) 
          (equal char #\u) (equal char #\y) (equal char #\j) (equal char #\A)
          (equal char #\E) (equal char #\I) (equal char #\O) (equal char #\U)
          (equal char #\Y) (equal char #\J)) t nil)
  )

;; Controlla che il carattere sia una consonante. In caso contrario restituisce nil

(defun consonante (char)
  (if (or (equal char #\b) (equal char #\c) (equal char #\d) (equal char #\f) (equal char #\g)
          (equal char #\h) (equal char #\k) (equal char #\l) (equal char #\m) (equal char #\n)
          (equal char #\p) (equal char #\q) (equal char #\r) (equal char #\s) (equal char #\t)
          (equal char #\v) (equal char #\w) (equal char #\x) (equal char #\z)
          (equal char #\B) (equal char #\C) (equal char #\D) (equal char #\F) (equal char #\G)
          (equal char #\H) (equal char #\K) (equal char #\L) (equal char #\M) (equal char #\N)
          (equal char #\P) (equal char #\Q) (equal char #\R) (equal char #\S) (equal char #\T)
          (equal char #\V) (equal char #\W) (equal char #\X) (equal char #\Z)) t nil)
  )

;; Controlla la correttezza della sillabazione attraverso 4 regole:

(defun c-sillabaz (char2 char3 char4)
  
  ;; E' possibile la suddivisione se l'ultimo carattere della linea è una vocale seguita da una
  ;; consonante e da un'altra vocale...
  
  (cond ((and (vocale char2) (consonante char3) (vocale char4)) t)
        
  ;; ...o se l'ultimo carattere è una consonante e questa è seguito da l, m, n oppure r.
        
        ((and (or (equal char2 #\l) (equal char2 #\m) (equal char2 #\n) (equal char2 #\r))
          (consonante char3)) t)
        
        ;; Altrimenti si possono suddividere due consonanti, quando queste sono uguali, o se le consonanti sono
        ;; 'c' e 'q'
        
        ((and (consonante char2) (consonante char3) 
              (or (equal char2 char3) (and (equal char2 #\c) (equal char3 #\q)))) t)
        
        ;; Infine è possibile una suddivisione quando l'ultimo carattere della linea è una vocale,
        ;; seguita da una consonante che non sia l, m, n, oppure r, ed infine che le due consonanti
        ;; successive alla vocale non siano uguali.
        
        ((and (vocale char2) (consonante char3)
              (not (or (equal char3 #\l) (equal char3 #\m) (equal char3 #\n) (equal char3 #\r)))
              (consonante char4) (not (equal char3 char4))) t)
        
        ;; Altrimenti non è possibile ottenere una corretta suddivisione
        
        (t nil)
        )
  )
        
        
;; Procedura giustifica  
;; Vengono passati i seguenti argomenti:
;; 1 - la lista sulla quale verranno effettuate le operazioni
;; 2 - il parametro i che specifica il valore limite dell'iterazione dotimes
;; 3 - il parametro num che specifica il numero di spazi da aggiungere per giustificare
;;     correttamente la linea.

(defun giustifica (lst i num)
  (dotimes (c i)
    
    (setq lista (cons (car lst) lista))
    
    ;; Se viene trovato uno spazio e il valore num è >0, allora viene inserito un nuovo spazio
    
    (cond ((and (equalp (car lst) #\space) (> num 0))
           (setq num (- num 1)) (setq c (+ 1 c)) (setq lista (cons #\space lista))))
    (setq lst (cdr lst)))

  (setq num-spaces 0)

  ;; Poichè i caratteri vengono inseriti dalla fine, è necessario rovesciare la lista
  (setq lista (cons #\newline (cdr lista)))
  (setq lista-def (append lista-def (reverse lista)))
  (setq lista ())
  lst
)



;; Procedura verifica: restituisce T se la giustificazione può avvenire, altrimenti
;; restituisce NIL
;; La giustificazione può essere effettuata senza l'ausilio della sillabazione sse
;; il rapporto tra l'ultimo spazio (precedente al valore di jwidth) e il numero di spazi
;; (escluso l'ultimo) è inferiore o uguale al valore di jlimit

(defun verifica (ls ns)
  (if (equal ns 1) nil 
    (if (<= (/ (- jwidth ls) (- ns 1)) jlimit) t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;                                                                                          ;;;;;;;;;;;;;
;;;;;;;;;;;;        P        R        E        F        O        R         M        A        T        ;;;;;;;;;;;;;
;;;;;;;;;;;;                                                                                          ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; La procedura di preformattazione inizialmente elimina eventuali blank all'inizio e alla fine del documento.
;; Sulla lista così ottenuta effettua il controllo dei blank.

(defun preformat (lst)
  (c-blank (c-char (reverse (c-inizio-fine (cons #\newline (reverse (c-inizio-fine lst)))))))
  )

;; Nel testo del progetto è previsto che nel testo preformattato non siano presenti due blank
;; consecutivi. Adotto la convenzione che, nel caso uno space sia seguito da un newline, viene
;; mantenuto lo spazio ed omesso il newline

(defun c-blank (lst)
(setq char (car lst))
 (cond ((null lst) nil)
        
        ;; Se il carattere trovato è uno space, e questo precede un Newline, il Newline immediatamente
        ;; successivo non dovrà essere inserito per evitare che ci siano due blank consecutivi.
        ;; Lo stesso vale nel caso ci siano due Space adiacenti.
        
        ((and (equal char #\space) (or (equal (cadr lst) #\newline) (equal (cadr lst) #\space)))
         (c-blank (cons char (cddr lst))))

        ;; Se il carattere trovato è uno Space o un Newline, controlleremo che questo non preceda un
        ;; segno di interpunzione. In questo caso, non inseriremo il Blank
        
        ((and (or (equal char #\space) (equal char #\newline))
              (or (equal (cadr lst) #\.)
                  (equal (cadr lst) #\,)
                  (equal (cadr lst) #\:)
                  (equal (cadr lst) #\;)
                  (equal (cadr lst) #\?)
                  (equal (cadr lst) #\!)))
         (c-blank (cdr lst)))
        
        ;; Se troviamo un separatore di paragrafo o un Newline, ed il carattere successivo è un Newline,
        ;; allora questo deve essere inserito nella lista. In caso contrario verrà sostituito da uno Space.
        ;; Questa clausola è necessaria per una corretta giustificazione.
        ;; Ovviamente bisogna effettuare un ulteriore controllo: valutare cioè che lo Space così inserito
        ;; non sia adiacente ad un altro Space, ad un Newline o ad un segno di interpunzione.
        
         ((and (not (or (equal char #\!)
                        (equal char #\?)
                        (equal char #\.)
                        (equal char #\newline)
                        (equal char #\-)))
               (equal (cadr lst) #\newline))
           (cons char (c-blank (cons #\space (cddr lst)))))
        
       ;; Se troviamo una parola già sillabata, elimineremo tale sillabazione
       
        ((and (equal char #\-) (equal (cadr lst) #\newline)) (c-blank (cddr lst)) )
       
       ;; Infine, se precedentemente ad uno Space troviamo un Newline, non dovremo tener conto di quest'ultimo
        
        ((and (equal char #\newline) (equal (cadr lst) #\space))
         (cons char (c-blank (cddr lst))))
        
        ;; Se nessuna delle precedenti condizioni è verificata, il carattere viene inserito senza alcun tipo
        ;; di controllo
        
        (t (cons char (c-blank (cdr lst))))
   )
)

;; Lista dei caratteri ammessi. Se il carattere da inserire non è presente in questa gamma, verrà omesso

(defun c-char (lst)
(cond ((null lst) nil)
      (t (setq char (car lst))
         (if (or (equal char #\Q) (equal char #\q) (equal char #\W) (equal char #\w) (equal char #\E) (equal char #\e) 
                 (equal char #\R) (equal char #\r) (equal char #\T) (equal char #\t) (equal char #\Y) (equal char #\y)
                 (equal char #\U) (equal char #\u) (equal char #\I) (equal char #\i) (equal char #\O) (equal char #\o) 
                 (equal char #\P) (equal char #\p) (equal char #\A) (equal char #\a) (equal char #\S) (equal char #\s) 
                 (equal char #\D) (equal char #\d) (equal char #\F) (equal char #\f) (equal char #\G) (equal char #\g) 
                 (equal char #\H) (equal char #\h) (equal char #\J) (equal char #\j) (equal char #\K) (equal char #\k) 
                 (equal char #\L) (equal char #\l) (equal char #\Z) (equal char #\z) (equal char #\X) (equal char #\x) 
                 (equal char #\C) (equal char #\c) (equal char #\V) (equal char #\v) (equal char #\B) (equal char #\b) 
                 (equal char #\N) (equal char #\n) (equal char #\M) (equal char #\m) (equal char #\0) (equal char #\1) 
                 (equal char #\2) (equal char #\3) (equal char #\4) (equal char #\5) (equal char #\6) (equal char #\7) 
                 (equal char #\8) (equal char #\9) (equal char #\!) (equal char #\") (equal char #\#) (equal char #\$) 
                 (equal char #\%) (equal char #\&) (equal char #\') (equal char #\() (equal char #\)) (equal char #\*) 
                 (equal char #\+) (equal char #\-) (equal char #\.) (equal char #\/) (equal char #\:) (equal char #\;) 
                 (equal char #\<) (equal char #\>) (equal char #\=) (equal char #\?) (equal char #\@) (equal char #\[) 
                 (equal char #\]) (equal char #\\) (equal char #\^) (equal char #\_) (equal char #\`) (equal char #\{) 
                 (equal char #\|) (equal char #\}) (equal char #\~) (equal char #\;) (equal char #\,) (equal char #\space) 
                 (equal char #\newline))
                 (cons char (c-char (cdr lst))) (c-char (cdr lst)))
           )
     )
)
  
;; Controlla che all'inizio della lista non siano presenti degli spazi

(defun c-inizio-fine (lst)
  (if (equal (car lst) #\space) (c-inizio-fine (cdr lst)) lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNZIONI DI UTILITA' GENERALE:

;; MKSTR: Crea una stringa a partire da una lista di caratteri

(defun mkstr (lst)
  (with-output-to-string (s)
    (dolist (a lst) (princ a s))))


;; READ-ALL-CHARS: Restituisce una lista di tutti i caratteri presenti in un file di input

(defvar  *END-OF-FILE*  (gensym) "Punto di EOF")
(defun  eof-p  (x)  (eq  x  *END-OF-FILE*))

(defun  read-all-chars (stream)
     (let  ((result  '()))
	(loop  (let  ((char  (read-char  stream nil
                            *END-OF-FILE*  nil) ))
          (if  (eof-p  char)
	      (return  (nreverse  result)) )
          (push  char  result) )) ) )

;; SALVA_FILE: Salva il testo della lista-def (testo giustificato e preformattato)
;;	       creando un nuovo file nel percorso output_file

(defun salva_file (output_file)
(with-open-file (s output_file :direction :output)
  (princ lista-def s)
  )
)


;; PROCEDURA ESTRAI: Estrae l'elemento numero 'i' dalla lista 'lst'

(defun estrai (i lst)
  (if (= i 1) (car lst)
    (if (> i 0) (estrai (- i 1) (cdr lst)) nil)))
