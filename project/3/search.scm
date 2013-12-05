(define (displayln a) (display a) (newline))
(define write-line displayln)
;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))
(define first car)

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))
(define second cadr)

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))
(define third caddr)

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))


; (DFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)
  
  
;; you will need to write a similar search procedure that handles cycles
; Exercise 1
; only swap the postion of 'new and 'old, keeping all number of char unchanged.
; becaue breadth-first-search should check old (sibling node) first.
; so append new node (child node) to old
(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

; (BFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;;

; index class (index
(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry); if here is cdr, index struct would be more simple.
        '())))

; add-to-index! is method of INDEX
(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((index-entry (find-entry-in-index index key)))
    (if (null? index-entry)
      ;; no entry -- create and insert a new one...
      ; (cons key (list (list value))) -> (key (value))
      (set-cdr! index (cons (cons key (list (list value))) (cdr index)))
    ;; entry exists -- insert value if not already there...
    (set-cdr! index-entry (list (cons value (cadr index-entry))))
	))
  index)

;; Testing
; (define test-index (make-index))
; (add-to-index! test-index 'key1 'value1)
; (add-to-index! test-index 'key2 'value2)
; (displayln test-index)
; (add-to-index! test-index 'key1 'another-value1)
; (displayln test-index)
; 
; (displayln (find-in-index test-index 'key1))
; (displayln (find-in-index test-index 'key2))


;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))

;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ...))

;;-----------------------------------------------------------------------------
;; Warmup Exercise 2:
;; Why DFS-simple proc not suit to the web define as above?
;; Because there are circles.
;; / -> SchemeImplementations -> getting-help.html -> /
;; SchemeImplementations <-> getting-help.html
;; Merge Method ->	  (lambda (new old) (append new old))
;;          would be infinite and never stop.
;; should modify to check if not exists in old, then append it.
;;-----------------------------------------------------------------------------


;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies

; Exercise 2 Marking the visited node
; add PASSED as inner loop state
(define (search-with-cycles init-state goal? successors merge graph)
  (define (search-inner still-to-do passed)
    (if (null? still-to-do) #f
      (let ((current (car still-to-do)))
       (if *search-debug*
         (write-line (list 'now-at current)))
       (if (goal? current)
         #t
         (search-inner
           ; filter
           ; only node not passed and not insert to todo-list
           ; add to queue.
           (merge (filter (lambda (x) (and (not (member x passed))
                                           (not (member x still-to-do))))
                          (successors graph current))
                  (cdr still-to-do))
           ; add current to passed state
           (cons current passed))))))
  (search-inner (list init-state) '()))

(define (DFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append new old))
                      graph))

(define (BFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append old new))
                      graph))
;;; -------------- TEST CODE -------------------------------------
;(DFS 'a
;     (lambda (node) (eq? node 'l))
;     the-web)
;
;(BFS 'a
;     (lambda (node) (eq? node 'l))
;     test-cycle)
;
;(map (lambda (ele)
;       (displayln (cadr ele))
;       (displayln (caddr ele))
;       (newline)) 
;     (cdr the-web))
;(newline)
;
;(DFS 'http://sicp.csail.mit.edu/
;     (lambda (node) #f)
;     the-web)
;
;(newline)
;(BFS 'http://sicp.csail.mit.edu/
;     (lambda (node) #f)
;     the-web)
;;;---------------------------------------------------------------
;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; add-document-to-index!: Index, Web, URL
;; (define (add-document-to-index! index web url)
;; ...
;; )
(define (add-document-to-index! index web url)
  (map (lambda (x) (add-to-index! index x url))
       (find-URL-text web url)))

;; Example use
;; 
;; (define the-web-index (make-index))
;; 
;; (add-document-to-index! the-web-index
;;                         the-web 
;;                         'http://sicp.csail.mit.edu/)
;; (displayln the-web-index)
;;  
;; (displayln (find-in-index the-web-index 'HELP))
;; ;Value: (http://sicp.csail.mit.edu/)
;;
;; (displayln (find-in-index the-web-index '*magic*))
;; ;Value: #f


;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

