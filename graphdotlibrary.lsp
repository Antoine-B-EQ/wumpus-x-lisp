(defun dot-name(exp)
(substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(complement #'alphanumericp)

(defparameter *max-label-length* 30)
(defun dot-label (exp)
(if exp
(let ((s (write-to-string exp :pretty nil)))
(if (> (length s) *max-label-length*)
(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
s))
""))

(defun nodes->dot (nodes)
(mapc (lambda (node)
(fresh-line)
(princ (dot-name (car node)))
(princ "[label=\"")
(princ (dot-label node))
(princ "\"];"))
nodes))

; example usage: 

; (nodes->dot *wizard-nodes*)
;LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];

;

(defun edges->dot (edges)
(mapc (lambda (node)
(mapc (lambda (edge)
(fresh-line)
(princ (dot-name (car node)))
(princ "->")
(princ (dot-name (car edge)))
(princ "[label=\"")
(princ (dot-label (cdr edge)))
(princ "\"];"))
(cdr node)))
edges))

; /* example usage:
; (edges->dot *wizard-edges*);
; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
*/
 ; end example ;
 
 
(defun graph->dot (nodes edges)
(princ "digraph{")
(nodes->dot nodes)
(edges->dot edges)
(princ "}"))

;/* example:

; (graph->dot *wizard-nodes* *wizard-edges*)
;digraph{
;120 Chapter 7
;LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}

;*/

(defun dot->png (fname thunk)
(with-open-file (*standard-output*
fname
:direction :output
:if-exists :supersede
(funcall thunk))
(ext:shell (concatenate 'string "dot -Tpng -0 " fname)))

;/* convert to a picture */

;/* or output to a file

;(with-open-file (my-stream
; "testfile.txt"
 ;:direction :output
 ;:if-exists :supersede)
 ;(princ "Hello File!" my-stream))
 
 ;*/
 
 (defun graph->png (fname nodes edges)
 (dot->png fname
 (lambda ()
 (graph->dot nodes edges))))
 
 ;/*fname for the function takes the name of a DOT file as the variable fname*/
 
 ;/*example to draw the graph 
 ;(with-open-file (my-stream
 ;"testfile.txt"
 ;:direction :output
 ;:if-exists :supersede)
 ;(princ "Hello File!" my-stream))
 ;*/
 
 /* for undirected graph :
 
 (defun uedges->dot (edges)
 (maplist (lambda (lst)
 (mapc (lambda (edge)
(unless (assoc (car edge) (cdr lst))
(fresh-line)
 (princ (dot-name (caar lst)))
 (princ "--")
 (princ (dot-name (car edge)))
 (princ "[label=\"")
 (princ (dot-label (cdr edge)))
 (princ "\"];")))
 (cdar lst)))
 edges))
 (defun ugraph->dot (nodes edges)
(princ "graph{")
 (nodes->dot nodes)
 (uedges->dot edges)
 (princ "}"))
  (defun ugraph->png (fname nodes edges)
   (dot->png fname
 (lambda ()
 (ugraph->dot nodes edges))))