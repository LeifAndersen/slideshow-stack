#lang typed/racket/base/no-check

(provide stack-do
         make-stack
         make-stack-frame)
(require (for-syntax typed/racket/base/no-check
                     racket/match
                     racket/list
                     syntax/parse
                     syntax/parse/experimental/template
                     unstable/syntax)
         pict
         slideshow/base
         slideshow/play
         unstable/gui/pslide
         unstable/gui/ppict)

;; ===================================================================================================
;;
;; Stack Definitions
;;
(define current-frame-color (make-parameter "yellow"))
(define current-frame-height (make-parameter 100))
(define current-frame-width (make-parameter 300))
(define current-frame-font (make-parameter (current-main-font)))
(define current-frame-font-size (make-parameter (current-font-size)))

(struct stack ([frames : (Listof stack-frame)]
               [xpos : Number]
               [ypos : Number])
  #:prefab)
(define (make-stack #:frames [frames null]
                    #:xpos [xpos 0]
                    #:ypos [ypos 1])
  (stack frames xpos ypos))

(struct stack-frame ([name : String]
                     [color : String]
                     [height : Positive-Integer]
                     [width : Positive-Integer]
                     [font : Font]
                     [font-size : Positive-Integer])
  #:prefab)
(define (make-stack-frame name
                          #:color [color (current-frame-color)]
                          #:height [height (current-frame-height)]
                          #:width [width (current-frame-width)]
                          #:font [font (current-frame-font)]
                          #:font-size [font-size (current-frame-font-size)])
  (stack-frame name color height width font font-size))

;; ===================================================================================================
;;
;; Stack Operations
;;
(: stack-push (stack stack-frame ... -> stack))
(define (stack-push st . frames)
  (match st
    [(struct* stack ([frames fr]))
     (struct-copy stack st [frames (append frames fr)])]))

(: stack-pop (stack Positive-Integer -> stack))
(define (stack-pop st count)
  (match st
    [(struct* stack ([frames fr]))
     (if ((length fr) . >= . count)
         (struct-copy stack st [frames (list-tail fr count)])
         (raise-user-error "Stack does not have ~a elements" count))]))

(: stack-insert (stack (Dict stack-frame Positive-Integer) -> stack))
(define (stack-insert st frame-pairs)
  (match st
    ([struct* stack ([frames fr])]
     (struct-copy stack st
                  [frames (append* (for/list ([index (in-range (length fr))]
                                              [i (in-list st)])
                                     (if (dict-has-key? frame-pairs index)
                                         (list (dict-ref frame-pairs index) i)
                                         (list i))))]))))

(: stack-remove (stack (Setof Positive-Integer) -> stack))
(define (stack-remove st frame-indexes)
  (match st
    ([struct* stack ([frames fr])]
     (struct-copy stack st
                  [frames (for/list ([index (in-range (length fr))]
                                     [i (in-list fr)]
                                     #:unless (set-member? frame-indexes index))
                            i)]))))

(: stack-split (stack (Setof Positive Integer) #:direction (U 'bottom 'top) -> (Listof Stack)))
(define (stack-split st splits #:direction [direction 'bottom])
  (match st
     [(struct* stack ([frames frames]
                     [xpos xpos]
                     [ypos ypos]))
     (define splits* (for/set ([i (in-set splits)])
                       (if (equal? direction 'bottom) (- (length frames) i) i)))
     (define-values (current-stack return-stacks)
       (for/fold ([current-stack null]
                  [return-stacks null])
                 ([index (in-range (length frames))]
                  [i (in-list frames)])
         (if (set-member? splits* index)
             (values (list i) (append return-stacks (list current-stack)))
             (values (append current-stack (list i)) return-stacks))))
     (set-map (append return-stacks (list current-stack)) (lambda (x) (stack x xpos ypos)))]))

;; ===================================================================================================
;;
;; Stack Renderers
;;
(: stack-frame->pict (frame -> pict))
(define (stack-frame->pict frame)
  (match frame
    [(struct* stack-frame ([name name]
                           [color color]
                           [height height]
                           [width width]
                           [font font]
                           [font-size font-size]))
     (cc-superimpose (colorize (filled-rectangle width height) color)
                     (rectangle width height)
                     (text name font font-size))]))

(: stack->pict (stack -> pict))
(define (stack->pict st)
  (match st
    [(struct* stack ([frames frames]))
     (apply vc-append 0 (map stack-frame->pict frames))]))

;; ===================================================================================================
;;
;; Stack drawing operations
;;
(define (stack-pict-start st)
  (define st-pict (stack->pict st))
  (match st
    [(struct* stack ([xpos xpos]
                     [ypos ypos]))
     (ppict-do ((pslide-base-pict))
               #:go (coord xpos ypos 'lb)
               st-pict)]))

(define (stack-pict-push st . frames)
  (define st-pict (stack->pict st))
  (define frame-picts (map stack-frame->pict frames))
  (match st
    [(struct* stack ([xpos xpos]
                     [ypos ypos]))

     (lambda (n)
       (ppict-do ((pslide-base-pict))
                #:go (coord xpos ypos 'lb)
                (apply vl-append frame-picts)
                (* 1000 (- 1 n))
                st-pict))]))

(define (stack-pict-pop st count)
  (define final-st (stack-pop st count))
  (define final-st-pict (stack->pict final-st))
  (match-define (list top _ ...) (set-map (stack-split st (list count) #:direction 'top) stack->pict))
  (match st
    [(struct* stack ([xpos xpos]
                     [ypos ypos]))
     (lambda (n)
       (ppict-do ((pslide-base-pict))
                 #:go (coord xpos ypos 'lb)
                 top
                 (* 1000 n)
                 final-st-pict))]))

;; ===================================================================================================
;;
;; stack-do
;;
(begin-for-syntax
  (struct operation ([type : Symbol]
                     [id : Syntax]
                     [pict-op : Syntax]
                     [op : Syntax])
    #:prefab))
(define-syntax (stack-do stx)
  (syntax-parse stx
    [(_ start commands ...)
     (define states
       (for/list ([command (in-list (syntax->list #'(commands ...)))])
         (syntax-parse command
           [((~datum push) id frames ...)
            (operation 'stack
                       #'id
                       #'(stack-pict-push (dict-ref state id) frames ...)
                       #'(stack-push (dict-ref state id) frames ...))]
           [((~datum pop) id count)
            (operation 'stack
                       #'id
                       #'(stack-pict-pop (dict-ref state id) count)
                       #'(stack-pop (dict-ref state id) count))]
           [((~datum background) pict)
            (operation 'background
                       #'#f
                       #'pict
                       #'#f)])))
     #`(begin
         (define state start)
         (define background ((pslide-base-pict)))
         (slide
          (for/fold ([acc ((pslide-base-pict))])
                    ([(k v) state])
            (cc-superimpose (stack-pict-start v) acc)))
         #,@(append* (for/list ([s (in-list states)])
                       (match s
                         [(struct* operation ([type 'background]
                                              [pict-op pict-op]))
                          (list #`(set! background #,pict-op))]
                         [(struct* operation ([type 'stack]
                                              [id id]
                                              [pict-op pict-op]
                                              [op op]))
                          (list #`(play-n
                                   #:steps 20
                                   #:delay 0.01
                                   #:skip-first? #t
                                   (lambda (n)
                                     (apply cc-superimpose
                                            background
                                            (#,pict-op n)
                                            (for/list ([(k v) state]
                                                       #:unless (equal? k #,id))
                                              (stack-pict-start v)))))
                                #`(set! state (dict-set state #,id #,op)))]))))]))

;; ===================================================================================================
;;
;; Test
;;
(module+ teset
  (define test-frame1 (make-stack-frame "hello"))
  (define test-frame2 (make-stack-frame "world" #:color "green"))
  (define test-stack1 (stack (list test-frame1) 0 1))
  (define test-stack2 (stack-push test-stack1 test-frame2))
  (define test-stack3 (stack-pop test-stack2 1))
  (define test-stack4
    #s(stack
       (#s(stack-frame "bob" "blue" 100 300 ("Gill Sans" . swiss) 32)
        #s(stack-frame "world" "green" 100 300 ("Gill Sans" . swiss) 32)
        #s(stack-frame "hello" "yellow" 100 300 ("Gill Sans" . swiss) 32))
       1/2
       1))

  (play-n
   #:steps 20
   #:delay 0.01
   #:skip-last? #t
   (stack-pict-push test-stack1 test-frame2))

  (play-n
   #:steps 20
   #:delay 0.01
   (stack-pict-pop test-stack2 1))

  (stack-do (hash 1 test-stack1
                  2 test-stack4)
            (push 1 (make-stack-frame "world" #:color "green"))
            (push 1 (make-stack-frame "bob" #:color "blue"))
            (pop 1 1)
            (push 1 (make-stack-frame "nope" #:color "red"))
            (pop 1 2)
            (push 2 (make-stack-frame "why?" #:color "blue"))
            (pop 1 1)))
