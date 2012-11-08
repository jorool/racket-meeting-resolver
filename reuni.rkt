#lang racket

; Este programa encontra horários disponíveis que sejam comuns entre vários
; horários especificados e que tenham um tamanho mínimo especificado.
;
; ** Conceitos **
;  Hora
;    Um momento no tempo, definido em termos da hora e minutos
;  Intervalo (abreviado inter)
;    Um intervalo no tempo, tem uma hora de início e uma hora de fim
;  Disponibilidade do dia (abreviado dispo)
;    Uma lista de intervalos que estão disponíveis em um determinado dia
;  Disponibilidade semanal (abreviado dispo-semana)
;    Uma lista com as disponibilidades de cada dia
;  Lista de associações
;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;    elemento do par é chamado de chave, e o segundo elemento é chamado de
;    valor. Uma lista de associações é uma maneira simples de implementar uma
;    tabela associativa (dicionário).  Ex: o dicionário
;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado com a lista associativa
;    (list (list 1 4) (list 20 12) (list 6 70)).
;    A função assoc é utilizada para consultar uma lista associativa.
;
; ** Formatação de entrada e saída **
; Toda operação de entrada e saída deve ser feita respeitando essas
; formatações. A sua implementação não precisa validar as entradas. Para os
; testes automatizados as entradas sempre serão válidas.
;
;  Hora (HH:MM) (sempre 5 dígitos)
;  Exemplos
;     08:30 = 8 horas e 30 minutos
;     12:07 = 12 horas e 7 minutos
;
;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;  Exemplos
;     08:30-12:07 = o intervalo tem início as 8 horas e 30 minutos e tem
;                   o fim as 12 horas e 7 minutos
;
;  Dias da semana
;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;
;  Disponibilidade semanal
;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;    intervalos disponíveis naquele dia
;  Exemplo
;    ter 10:20-12:00 16:10-17:30
;    sex 08:30-11:30
;  Observe que nem todos os dias devem estar especificados. Os dias
;  que não têm disponibilidades não devem ser especificados.

(require 2htdp/batch-io)

; exporta as função que podem ser utilizadas em outros arquivos
(provide hora
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

; hora
(struct hora (h m) #:transparent)

; intervalo
(struct intervalo (inicio fim) #:transparent)

; Constante que define um intervalo vazio
(define intervalo-vazio (void))

; intervalo -> bool
; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

; intervalo, intervalo -> intervalo
; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  ; qual eh a maior hora inicial
  (define maior-hora-inicial (if (>= (hora->minutos (intervalo-inicio a)) (hora->minutos (intervalo-inicio b))) (intervalo-inicio a) (intervalo-inicio b)))
  ; qual eh a menor hora final
  (define menor-hora-final  (if (<= (hora->minutos (intervalo-fim a)) (hora->minutos (intervalo-fim b))) (intervalo-fim a) (intervalo-fim b)))
  ; retorno
  (define intervalo-resultado (intervalo maior-hora-inicial menor-hora-final))
  (if (intervalo-valido? intervalo-resultado) intervalo-resultado intervalo-vazio))

; list intervalo, list intervalo -> list intervalo
; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  ; a ideia eh fazer um plano cartesiano (dois "fors")
  (let ([percorrer-dispo-a (lambda (intervalo-b)
                             (map (lambda (intervalo-a)
                                    (intervalo-intersecao intervalo-a intervalo-b)) dispo-a))])
    (filter (negate intervalo-vazio?) (foldr append (list) (map percorrer-dispo-a dispo-b)))))

; hora, list dispo-semana -> dispo-semana
; Esta função encontra os intervalos disponíveis para cada dia da semana que
; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
; da lista dispos.
; 
; dispo-semana é uma lista de associações entre um dia (string) e a
; disponibilidade naquele dia. Veja a definição de lista de associações no
; início deste arquivo.
; 
; Por exemplo, a disponibilidade semanal (dispo-semana):
; ter 10:20-12:00 16:10-17:30
; sex 08:30-11:30
; é representado da seguinte maneira:
; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;                         (intervalo (hora 16 10) (hora 17 30))))
;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;
; Observe que esta função recebe como parâmetro uma lista de disponibilidades
; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
; Veja os testes de unidade para exemplos de entrada e saída desta função
(define (encontrar-dispo-semana-em-comum tempo dispos)
  ; recebe dispo-a dispo-b retorna o dispo em comum entre eles (elimina dias sem intervalos)
  (define (funcao dispo-a dispo-b) (filter pair? (map (λ (dia) (compara-dispos-dia dispo-a dispo-b dia tempo)) dias-da-semana)))
  (define dispo-semana-em-comum (foldl funcao (first dispos) (cdr dispos)))    
  (filter pair? dispo-semana-em-comum)) 

; list string -> void
; Esta é a função principal. Esta função é chamada a partir do arquivo
; reuni-main.rkt
;
; args é a lista de parâmetros para o programa.
;
; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
; devem ter. O tempo mínimo é especificado usando a formatação da hora.
;
; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
; contêm uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
; testes.
;
; A saída desta função é a escrita na tela dos intervalos em comum que
; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
; semanal.
(define (main args)
  ; pega o tempo minimo
  (define tempo-minimo (string->hora (first args)))
  ; extrair disponibilidades
  (define todas-dispos (retorna-todas-dispos (cdr args)))
  ; tratar caso de um unico arquivo de entrada
  ; ps: sim, isso pode ser considerada uma solucao de contorno, pois soh descobri o erro ao executar todos os testes...
  ; esse caso nao foi devidamente tratado antes pois, dada a ideia do programa, nao faria sentido reuniao de uma pessoa soh.
  (cond
    [(< (length (cdr args)) 2) 
     ; monta uma lista com ela mesma
     (define todas-dispos-duplicadas (append todas-dispos (reverse todas-dispos)))
     ; chama função interseção
     (define resultado (encontrar-dispo-semana-em-comum tempo-minimo todas-dispos-duplicadas))
     ; imprimir os resultados
     (imprimir-resultados resultado)
     (void)]
    [else
     ; chama função interseção
     (define resultado (encontrar-dispo-semana-em-comum tempo-minimo todas-dispos))
     ; imprimir os resultados
     (imprimir-resultados resultado)
     (void)]))

(define (imprimir-resultados resultados)
  (map imprimir-dispo resultados))

(define (imprimir-dispo dispo)
  (display (car dispo))
  (map imprimir-intervalos (cdr dispo))
  (display "\n"))

(define (imprimir-intervalos intervalos)
  (for/list ([intervalo intervalos])
    (define inicio (intervalo-inicio intervalo))
    (define fim (intervalo-fim intervalo))
    (display (string-append " " (hora->string inicio) "-" (hora->string fim)))))

(define (hora->string hora)
  (define horas (hora-h hora))
  (define minutos (hora-m hora))
  ; como formatar numeros?
  (cond
    [(and (< horas 10) (< minutos 10)) (string-append "0" (number->string horas) ":0" (number->string minutos))]
    [(< horas 10) (string-append "0" (number->string horas) ":" (number->string minutos))]
    [(< minutos 10) (string-append (number->string horas) ":0" (number->string minutos))]
    [else (string-append (number->string horas) ":" (number->string minutos))]))

(define (string->hora string)
  (hora (string->number (substring string 0 2)) (string->number (substring string 3 5))))

(define (hora->minutos hora)
  (define minutos-hora (* 60 (hora-h hora)))
  (define minutos (hora-m hora))
  (+ minutos-hora minutos))

(define (intervalo-valido? intervalo)
  (< (hora->minutos (intervalo-inicio intervalo)) (hora->minutos (intervalo-fim intervalo))))

(define (intervalo-maior-igual-que-tempo-minimo? tempo-minimo intervalo)
  (>= (extrai-minutos-intervalo intervalo) (hora->minutos tempo-minimo)))

(define (extrai-minutos-intervalo intervalo)
  (define minutos-inicio (hora->minutos (intervalo-inicio intervalo)))
  (define minutos-fim (hora->minutos (intervalo-fim intervalo)))
  (- minutos-fim minutos-inicio))

(define (retorna-todas-dispos lista-arquivos) 
  (map arquivo->dispo-semana lista-arquivos))

(define (arquivo->dispo-semana arquivo)
  (linhas->dispo (file->lines arquivo)))

(define (linhas->dispo linhas-arquivo)
  (define lista-linhas (map string-split linhas-arquivo))
  (map string->dispo lista-linhas))

(define (string->dispo linha)
  (define dia-semana (car linha))
  (define lista-intervalos (map string->intervalo (cdr linha)))
  (cons dia-semana (list lista-intervalos)))

(define (string->intervalo string)
  (define string-inicio (substring string 0 5))
  (define string-fim (substring string 6 11))
  (define hora-inicio (string->hora string-inicio))
  (define hora-fim (string->hora string-fim))
  (intervalo hora-inicio hora-fim))

(define dias-da-semana (list "dom" "seg" "ter" "qua" "qui" "sex" "sab"))

(define (compara-dispos-dia dispo-a dispo-b dia tempo-minimo)
  (cond
    [(and (assoc dia dispo-a) (assoc dia dispo-b))
     (define dispos (encontrar-dispo-em-comum (car (cdr (assoc dia dispo-a))) (car (cdr (assoc dia dispo-b)))))
     (define a (filter (lambda (intervalo)
            (intervalo-maior-igual-que-tempo-minimo? tempo-minimo intervalo)) dispos))
     (if (null? a) (void) (list dia a))]))