(define (mk-connection cost idPointsTo)
  (cons cost idPointsTo))
(define (cost connection)
  (car connection))
(define (idPointsTo connection)
  (cdr connection))
(define (mk-node id . connections)
  (cons id connections))
(define (id node)
  (car node))
(define (connections node)
  (cdr node))
(define (mk-graph . nodes) nodes)
(define g
  (mk-graph
   (mk-node 0 [mk-connection 1 1] [mk-connection 2 2])
   (mk-node 1 [mk-connection 1 0] [mk-connection 3 2]
            [mk-connection 2 3] [mk-connection 3 4])
   (mk-node 2 [mk-connection 2 0] [mk-connection 3 1]
            [mk-connection 1 5] [mk-connection 2 7])
   (mk-node 3 [mk-connection 2 1] [mk-connection 1 4])
   (mk-node 4 [mk-connection 3 1] [mk-connection 1 4]
            [mk-connection 1 6] [mk-connection 3 7])
   (mk-node 5 [mk-connection 1 2] [mk-connection 2 7])
   (mk-node 6 [mk-connection 1 4] [mk-connection 1 7])
   (mk-node 7 [mk-connection 2 2] [mk-connection 3 4]
            [mk-connection 2 5] [mk-connection 1 6])))
(define (first-node graph) (car graph))
(define (except-first graph) (cdr graph))
(define (isEmpty? graph) (null? graph))
(define (associate node-id graph)
  (cond [(isEmpty? graph) #f]
        [(eq? (id (first-node graph)) node-id) (first-node graph)]
        [else (associate node-id (except-first graph))]
    ))
(define (shortest-path node-id1 node-id2 graph)
  (define (insert connection current-cost cur-path lst)
    (cond [(null? lst) (list (cons
                              (cons (idPointsTo connection)
                                   (+ current-cost (cost connection)))
                              (append cur-path (list (idPointsTo connection)))))]
          [(< (+ (cost connection) current-cost) (cadar lst))
           (cons (cons
                  (cons (idPointsTo connection)
                       (+ current-cost (cost connection)))
                  (append cur-path (list (idPointsTo connection))))
                 lst)]
          [else (cons (car lst) (insert connection current-cost cur-path (cdr lst)))]
      ))
  (define (add-connections-to-list connections cur-path cur-cost lst)
    (cond [(null? connections) lst]
          [else (add-connections-to-list (cdr connections) cur-path cur-cost
                 (insert (car connections) cur-cost cur-path lst))]
      ))
  (define (iter lst)
    (cond [(eq? (caaar lst) node-id2) (list (cdaar lst) (cdar lst))]
          [else (iter (add-connections-to-list
                       (connections (associate (caaar lst) graph))
                       (cdar lst)
                       (cdaar lst)
                       (cdr lst))
                     )]
      ))
  (iter (list (cons
               (cons
                node-id1
                0
                )
               (list node-id1)))))