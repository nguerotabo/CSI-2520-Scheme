#lang scheme
(define (read-f filename) (call-with-input-file filename
(lambda (input-port)
(let loop ((line (read-line input-port)))
(cond
 ((eof-object? line) '())
 (#t (begin (cons (string-split (clean-line line) ",") (loop (read-line input-port))))))))))

(define (format-resident lst)
  (list (car lst) (cadr lst) (caddr lst) (cdddr lst)))

(define (format-program lst)
  (list (car lst) (cadr lst) (string->number (caddr lst)) (map string->number(cdddr lst))))


(define (clean-line str)
  (list->string
   (filter (lambda (c) (not (or (char=? c #\") (char=? c #\[) (char=? c #\]) )))
           (string->list str))))

(define (read-residents filename)
(map (lambda(L) (format-resident (cons (string->number (car L)) (cdr L)))) (cdr (read-f filename))))

(define (read-programs filename)
(map format-program (cdr (read-f filename))))


(define PLIST (read-programs "programs4000.csv"))
(define RLIST (read-residents "residents4000.csv"))



; on parcour la liste jusqua trouver le resident avec le bon id
; si on trouve rien on retourne la liste vide
(define (get-resident-info rid rlist)
  (if (null? rlist) '()
      (if (equal? (caar rlist) rid) (car rlist)
          (get-resident-info rid (cdr rlist)))))


; meme principe que get-resident-info, on cherche le programme
; par son id (qui est un string genre "HEP")
(define (get-program-info pid plist)
  (if (null? plist) '()
      (if (equal? (caar plist) pid) (car plist)
          (get-program-info pid (cdr plist)))))



; rank retourne la position du resident dans la ROL du programme
; le premier rang est 0. on utilise une fonction recursive ranked
; qui incremente un compteur a chaque etape
(define (rank rid pinfo)
  (define x 0)
  (ranked rid (cadddr pinfo) x))

; ranked est le helper recursive de rank, on avance dans la liste
; et on retourne n quand on trouve le rid, sinon on continu
(define (ranked rid l n)
  (if (null? l) n
      (if (equal? (car l) rid) n
          (ranked rid (cdr l) (+ n 1)))))
        
    


; matched? regarde si un resident a deja ete place dans un des
; programme de la liste des matches, on parcour tous les matches
; et on verifie avec findInPairs
(define (matched? rid matches)
  (if (null? matches) #f
      (if (findInPairs rid (cadr (car matches))) #t
          (matched? rid (cdr matches)))))
; helper de matched? qui cherche dans la liste des paires
; (rid . rang) si notre rid est present
(define (findInPairs rid l)
  (if (null? l) #f
      (if (equal? rid (caar l)) #t
          (findInPairs rid (cdr l)))))


; get-match retourne l'entree du programme dans la liste des
; matches (donc pid avec sa liste de residents apparier)
(define (get-match pid matches)
  (if (null? matches) '()
      (if(equal? pid (caar matches)) (car matches)
         (get-match pid (cdr matches)))))



; insert-pair insere une paire (rid . rang) dans une liste trier
; par rang decroissant. le pire rang (grand nombre) est en
; premier, comme sa on peut facilement le remplacer avec car
(define (insert-pair pair l)
  (cond
    ((null? l) (list pair))
    ((> (cdr pair) (cdr (car l))) (cons pair l))
    (else (cons (car l) (insert-pair pair (cdr l))))))

; add-resident-to-match prends une paire et la met dans la liste
; de resident apparier a un programme, en gardant l'ordre
(define (add-resident-to-match pair match)
  (list (car match)
        (insert-pair pair (cadr match)))) 


;(add-resident-to-match (cons 828 3)
;(get-match "NRS" lt))

; program-quota c'est juste un accesseur pour le quota qui se
; trouve en 3eme position dans l'info du programme
(define (program-quota pinfo)
  (caddr pinfo))

; update-match remplace le match existant d'un programme par le
; nouveau, ou l'ajoute si il existe pas encore
(define (update-match newmatch matches)
  (if (null? matches) (list newmatch)
      (if (equal? (car newmatch) (caar matches))
          (cons newmatch (cdr matches))
          (cons (car matches) (update-match newmatch (cdr matches))))))

; remove-first-pair enleve la premiere paire de la liste, c'est
; utiliser quand on expulse le pire resident d'un programme plein
(define (remove-first-pair l)
  (if (null? l) '()
      (cdr l)))

; remove-program-from-pref enleve un programme de la ROL du
; resident (important apres un rejet pour pas re-essayer)
(define (remove-program-from-pref pid prefs)
  (if (null? prefs) '()
      (if (equal? pid (car prefs)) (cdr prefs)
          (cons (car prefs) (remove-program-from-pref pid (cdr prefs))))))

; advance-resident fait passer le resident a son choix suivant en
; enlevant la premiere prefs de sa liste
(define (advance-resident rinfo)
  (list (car rinfo)
        (cadr rinfo)
        (caddr rinfo)
        (cdr (cadddr rinfo))))

; resident-after-rejection prepare un resident expulser pour qu'il
; puisse re-essayer, on va chercher son info original dans RLIST
; et on enleve juste le programme qui vien de le rejeter
(define (resident-after-rejection rid pid rlist)
  (let* ((rinfo (get-resident-info rid rlist))
         (prefs (remove-program-from-pref pid (cadddr rinfo))))
    ; on reprend le resident sans le programme deja essaye
    (list (car rinfo)
          (cadr rinfo)
          (caddr rinfo)
          prefs)))

; evaluate c'est la fonction d'evaluation de l'algo McVitie-Wilson.
; quand un resident propose a un programme on regarde 3 cas: 
; le programme a aucun match, il a encore de la place, ou bien
; il est plein (dans ce cas on compare avec le pire deja place)
(define (evaluate rinfo pinfo rlist plist matches)
  (let* ((rid (car rinfo))
         (pid (car pinfo))
         (quota (program-quota pinfo))
         (rnk (rank rid pinfo))
         (pair (cons rid rnk))
         (pmatch (get-match pid matches)))
    
    ; si le programme na encore aucun match
    (if (null? pmatch)
        (cons (list pid (list pair)) matches)
        
        ; si le programme a encore de la place
        (if (< (length (cadr pmatch)) quota)
            (update-match (add-resident-to-match pair pmatch) matches)
            
            ; sinon le programme est plein
            (let* ((worst-pair (car (cadr pmatch)))
                   (worst-rank (cdr worst-pair)))
              
              ; si le nouveau resident est meilleur
              (if (< rnk worst-rank)
                  (let* ((reduced-match (list pid (remove-first-pair (cadr pmatch))))
                         (newmatch (add-resident-to-match pair reduced-match))
                         (updated-matches (update-match newmatch matches))
                         (expelled-rid (car worst-pair))
                         (expelled-rinfo (resident-after-rejection expelled-rid pid rlist)))
                    
                    ; on relance loffre pour le resident expulse
                    (offer expelled-rinfo rlist plist updated-matches))
                  
                  ; sinon on garde les matches tels quels
                  matches))))))

; offer c'est la fonction d'offre de l'algorithme McVitie-Wilson.
; le resident fait une offre au programme en tete de sa liste de
; preference. si ca marche pas on avance au suivant, jusqu'a ce
; quil soit place ou que sa liste soit vide
(define (offer rinfo rlist plist matches)
  ; si le resident na plus de choix
  (if (null? (cadddr rinfo)) matches
      
      ; si le resident est deja place
      (if (matched? (car rinfo) matches) matches
          
          (let* ((pid (car (cadddr rinfo)))
                 (pinfo (get-program-info pid plist))
                 (newmatches (evaluate rinfo pinfo rlist plist matches)))
            
            ; si apres evaluation il est place on arrete
            (if (matched? (car rinfo) newmatches)
                newmatches
                
                ; sinon il essaye son choix suivant
                (offer (advance-resident rinfo) rlist plist newmatches))))))

; gale-shapley c'est la fonction principale qui applique offer a
; tous les residents de la liste. elle accumule les matches au
; fur et a mesure. conformement a McVitie-Wilson il suffit
; d'appeler offer une fois par resident
(define (gale-shapley rlist plist matches)
  ; on traite tous les residents un par un
  (if (null? rlist) matches
      (gale-shapley (cdr rlist)
                    plist
                    (offer (car rlist) RLIST plist matches))))








; get-not-matched-list retourne la liste des id de residents qui
; on pas ete apparie a un programme. on parcour la liste des
; residents et on garde ceux qui son pas dans les matches
(define (get-not-matched-list rlist matches)
  (if (null? rlist) '()
      (if (matched? (caar rlist) matches)
          ; ce resident est deja placer, on passe au suivant
          (get-not-matched-list (cdr rlist) matches)
          ; sinon on l'ajoute a la liste des non-apparie
          (cons (caar rlist)
                (get-not-matched-list (cdr rlist) matches)))))


; display-pair-info affiche une seule ligne pour un resident
; apparie. le format est: nom,prenom,id,idProg,nomProg
(define (display-pair-info pair pid pname rlist)
  (let* ((rid (car pair))
         (rinfo (get-resident-info rid rlist))
         (fname (cadr rinfo))
         (lname (caddr rinfo)))
    (display lname) (display ",")
    (display fname) (display ",")
    (display rid) (display ",")
    (display pid) (display ",")
    (display pname) (newline)))


; display-program-matches prends un match (cad un programme et sa
; liste de residents apparier) et affiche chaque ligne avec
; l'aide de display-pair-info
(define (display-program-matches m rlist plist)
  (let* ((pid (car m))
         (pinfo (get-program-info pid plist))
         (pname (cadr pinfo))
         (pairs (cadr m)))
    ; on itere sur toute les paires du programme
    (for-each (lambda (p)
                (display-pair-info p pid pname rlist))
              pairs)))


; display-not-matched affiche toute les residents qui on pas eu
; de jumelage. on les marque avec XXX et NOT_MATCHED comme
; indiquer dans l'exemple du projet
(define (display-not-matched not-matched-list rlist)
  (for-each
   (lambda (rid)
     (let* ((rinfo (get-resident-info rid rlist))
            (fname (cadr rinfo))
            (lname (caddr rinfo)))
       (display lname) (display ",")
       (display fname) (display ",")
       (display rid) (display ",")
       (display "XXX,NOT_MATCHED")
       (newline)))
   not-matched-list))


; get-total-available-positions calcul combien de places il reste
; dans les programmes. pour chaque programme on fait quota moins
; le nombre de residents deja apparier a celui ci
(define (get-total-available-positions matches plist)
  (if (null? plist) 0
      (let* ((pinfo (car plist))
             (pid (car pinfo))
             (quota (program-quota pinfo))
             (m (get-match pid matches))
             ; si le programme a pas de match, 0 sinon on prend la longueur
             (filled (if (null? m) 0 (length (cadr m)))))
        (+ (- quota filled)
           (get-total-available-positions matches (cdr plist))))))


; gale-shapley-print c'est la fonction de plus haut niveau. elle
; est construite sur le template donner dans le document du projet.
; elle appel gale-shapley, ensuite affiche les apparier, les non
; apparier, et les deux statistiques final
(define (gale-shapley-print rlist plist)
  (let* ((matches (gale-shapley rlist plist '()))
         (not-matched-list (get-not-matched-list rlist matches)))
    (for-each (lambda(m)
                (display-program-matches m rlist plist)) matches)
    (display-not-matched not-matched-list rlist)
    (display "Number of unmatched residents: ")
    (display (length not-matched-list)) (newline)
    (display "Number of positions available: ")
    (display (get-total-available-positions matches plist))
    (newline)))


; appel final de la fonction pour produire la sortie
(with-output-to-file "output.txt"
  (lambda ()
    (gale-shapley-print RLIST PLIST)
  )
  #:exists 'replace)

