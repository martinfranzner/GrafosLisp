
; "/Users/martinfranzner/Documents/PUC_COMPUTACAO/5oSemestre/ProgramacaoFuncional/codes/pajekLisp.txt"
; para chamar
;chamada do grafo
; (setq grafo (grafoPronto "/Users/martinfranzner/Documents/PUC_COMPUTACAO/5oSemestre/ProgramacaoFuncional/codes/pajekLisp.txt"))
;(setq grafo1 (grafoPronto "/Users/martinfranzner/Documents/PUC_COMPUTACAO/5oSemestre/ProgramacaoFuncional/PajekActor.txt"))
;(setq grafo (grafoPronto "/Users/martinfranzner/Documents/PUC_COMPUTACAO/5oSemestre/ProgramacaoFuncional/codes/pajekGrafoGrande.txt"))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect line
    )
  )
)

(defun string-to-list (str)
  (if (not (streamp str))
     (string-to-list (make-string-input-stream str))
     (if (listen str)
       (cons (read str) (string-to-list str))
       nil
     )
  )
)

(defun transformaEmLista (arquivo)
  (let ( (resposta))
    (setq listaComoStrings (get-file arquivo))
    (dolist (x listaComoStrings resposta)
      (setq resposta (append resposta (list (string-to-list x))))
    )
  )
)

(defun preparaGrafo (lista)
  (let ( (counter 0) (listaComNomes) )
    (setq fezes (list (cadr (car lista))))
    (setq lista (cdr lista))
    (dolist (x lista)
      (if (< counter (car fezes))
        (progn
          (setq listaComNomes (append listaComNomes (list x)))
          (setq lista ( cdr lista))
          (incf counter)
        )
      )
    )
    (setq lista (cdr lista))
    (list (car fezes) listaComNomes lista)
  )
)

(defun printTabela (grafo)
  (let ((closen) (between) (degr) (out))
    (setq closen (sort (closeness grafo) #'> :key #'cadr))
    (setq between (sort (betweeness grafo)  #'> :key #'cadr))
    (setq degr (sort (degree grafo) #'> :key #'cadr))
    
    ;(print closen)
    ;(print between)
    ;(print degr)
    (setq out (listToPrint degr closen between))
    
    (prin1 "----------------------------------------------------------------------------")
    (print "  ")(prin1 'Grau) (prin1 "       |     ") (prin1 'Proximidade) (prin1 "          |  ") (prin1 'Intermediacao)
    (print 'Vert) (prin1 "  ") (prin1 'Value) (prin1 "  |  ") (prin1 'Vert) (prin1 "    ") (prin1 'Value) (prin1 "         |  ") (prin1 'Vert) (prin1 "  ") (prin1 'Value)                                                                                            
    
    (dolist (x out)
      (print (caar x));Grau Vert
      (prin1 "     ")
      (prin1 (cadr (car x))); Grau Value
      (prin1 "   |   ")
      (prin1 (car (cadr x)));Proximidade Vert
      (prin1 "     ")
      (prin1 (cadr (cadr x)));Proximidade Value
      (prin1 "   |   ")
      (prin1 (car (caddr x)));Intermed Vert
      (prin1 "    ")
      (prin1 (cadr (caddr x)));Intermed Value
    )
  )
)


(defun listToPrint (degr closen between)
  (loop for x in degr for y in closen for j in between 
      collect (list x y j)
  )
)


(defun grauVertice (vert listaGrafo)
  (let ((grau 0))
    (mapc #'(lambda (x) (if (or (= (car x) vert) (= (cadr x) vert)) (incf grau))) listaGrafo)
    grau
  )
)

;chamada para fazer o grafo a partir do pajek
(defun grafoPronto (arquivo)
  (preparaGrafo(transformaEmLista arquivo))
)

(defun degree (grafo)
  (let ((nodes (car grafo)) (saida) (adjGrafoLista (caddr grafo)) (counter 1))
    (loop
      (setq saida (append saida  (list (list counter (/ (grauVertice counter adjGrafoLista) (- nodes 1.0))))))
      (incf counter)
      (when (> counter nodes) (return saida))
    )
  )
)

;chamada (closeness grafo)
(defun closeness (grafo) ; 1 / somatoria da distancia do noh x a todos os outros
  (let ((nodes (car grafo)) (saida) (somatorioLista) (sum 0) (adjGrafoLista (caddr grafo)) (counterIn 1) (counterOut 1))
    (dotimes (nSingle nodes)
      (dotimes (nDist nodes)
        (setq caminho `((0(,counterOut))))
        (when (/= counterIn counterOut)  
          ;(print counterOut)
          ;(print counterIn)
          (setq somatorioLista (busca caminho counterIn adjGrafoLista))
          ;(print somatorioLista)
          ;(print (car somatorioLista))
          (setq sum (+ sum (car somatorioLista)))
          ;(print sum)
        )
        (incf counterIn)
      )
      (setq saida (append saida (list(list counterOut (/ 1.0 sum)))))
      (setq sum 0)
      (setq counterIn 1)
      (incf counterOut)    
    )
    saida
  )
)

;chamada (betweeness grafo)
(defun betweeness (grafo)
  (let ((nodes (car grafo)) (saida) (resposta) (somatorioLista) (sum 0) (adjGrafoLista (caddr grafo)) (counterIn 1) (counterOut 1))
    (dotimes (nSingle nodes)
      (dotimes (nDist nodes)
        (setq caminho `((0(,counterOut))))
        (when (/= counterIn counterOut)  
          (setq somatorioLista (busca caminho counterIn adjGrafoLista))
          (setq saida (append saida (list (cadr somatorioLista)))) ;joga somente o caminho das buscas
        )
        (incf counterIn)
      )
      (setq counterIn 1)
      (incf counterOut)    
    )
    ;(print saida)
    (setq counterOut 1)
    (dotimes (nLast nodes)
       ;(print counterOut)
       ;(print "nova dolist")
      (dolist (x saida)
        (when (and (member counterOut x) (and (not (eq counterOut (car x))) (not (eq counterOut (car (last x))))));verifica se esta no meio do caminho (1 2 3 4) 2 e 3
          (incf sum)
          ;(print sum)
        )
      )
      (setq resposta (append resposta (list (list counterOut (* 2(/ (* (/ sum 2) 2.0) (* (- nodes 1.0) (- nodes 2.0))))))))
      (incf counterOut)  
      (setq sum 0)
    ) 
    resposta
  )
)


;( (1 (a b)) (7 (a b c)) )
(defun melhor (caminho)
  (let ((peso 9999) (saida))
    (mapc #'(lambda (x) (if (< (car x) peso)(progn (setq saida x)(setq peso (car x)) ) )) caminho)
    saida
  )
)

(defun ehSolucao (lista solucao)
  (if (eq (car (last (cadr lista))) solucao)
      T
      nil

   )
)



;( (1 (a b)) (7 (a b c)) )
;grafo de testes ((a b 2) (a c 1) (b c 5) (b d 2) (b e 7) (c d 4) (c e 3) (d e 8))
(defun membro (atom lista)
  (if (member atom lista)
      T
      nil
  )
)
(defun membroExp (atom lista)
  (if (or (eq (car atom) (car lista)) (eq (car atom) (cadr lista)))
      T
      nil
  )
)


(defun expande (best grafo)
  (let ((expandNode) (junta))
    (setq expandNode(last (cadr best)))
    ;(print best)
    ;(print expandNode)
    (dolist (x grafo)
      ;(print x)
      (when (membroExp expandNode x)   ; se o nó a expandir esta na lista e com quem ele faz ligacao ainda nao esta no best, expandir
        ;(print x)
        (if (and (eq (car x) (car expandNode)) (not (membro (cadr x) (cadr best))));aqui ve se o car eh o expandNode e ele nao ta ja no best, dai ira expandir  
            (progn
            (setq junta (append junta (list (list (+ (car best) (caddr x)) (append (cadr best) (list (cadr x)))))))
            ;(print junta)
            )
            (when (and (eq (cadr x) (car expandNode)) (not (membro (car x) (cadr best))))
              (setq junta (append junta (list (list (+ (car best) (caddr x)) (append (cadr best) (list (car x)))))))
              ;(print junta)
            )
        )
      )
    )
    junta
  )
)


(defun busca (caminho solucao grafo)
  (let ((best) (X))
  (setq best (melhor caminho))
  ;(print best)
  (if (eq caminho nil)
      nil  
      (if (eq (ehSolucao best solucao) T)
          best
          (progn
            (setq X (remove best caminho :test #'equal)) ;X vira caminhos mas sem o best  
            (setq X (append X (expande best grafo)))
            (busca X solucao grafo)
          )
      )
  )
  )
)


;grafo de testes ((a b 2) (a c 1) (b c 5) (b d 2) (b e 7) (c d 4) (c e 3) (d e 8))
;(remove `(a b) `((c d) (a b) (r t)) :test #'equal)

;tira = (3 (A C E))
;m = ((4 (A B)) (7 (A B C)) (3 (A C E)))
;(remove tira m :test #'equal)
; ((4 (A B)) (7 (A B C)))

;(position `(a b) `((c d) (a b) (r t)) :test #'equal)


#|
algoritmo de melhor camimnho

X = busca( ((0 (a))), e )

(1 (a c))

busca (caminho, solucao)
  se caminho == nulo entao return nulo;
  senao
    best = melhor (caminho) ;melhor caminho eh com o menor peso
    se best eh solucao entao ;acha a solucao olhando o last da lista de camimnhos (7 (a b c)) c == solucao
      return best
    senao
      X = remove best
      X = expande(best) U X ; U sinal de uniao
      return busca (X, Solucao)
    fimsenao
  fimsenao
|#








