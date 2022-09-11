(in-package :cl-user)

(defpackage xml.parse
  (:use :cl)

  (:import-from
   :stw.meta
   :map-filtered-precedents
   :filter-slots-by-type
   :find-slot-definition)
   
  (:import-from
   :stw.util
   :sequence-from-file
   :ensure-list
   :aif
   :awhen
   :self)
  
  (:import-from
   :stw.util
   :*document*
   :*char-index*
   :*line-number*
   :*length* 
   :*decoder*
   :*consume-whitespace*
   :stw-read-char
   :next
   :stw-peek-next-char
   :stw-peek-last-char
   :match-string
   :match-character
   :read-until
   :read-and-decode
   :consume-while
   :whitespacep)

  (:import-from
   :stw.util
   :trie
   :make-trie
   :insert-word
   :walk-branch
   :trie-leaf
   :insert-word)

  (:import-from
   :closer-mop
   :standard-class
   :standard-direct-slot-definition
   :standard-effective-slot-definition
   :direct-slot-definition-class
   :effective-slot-definition-class
   :compute-effective-slot-definition
   :validate-superclass
   :slot-definition-name
   :slot-definition-type)

  (:export

   ;; specials
   :*element-class-map*
   :*element-class-trie*
   :*preserve-whitespace*
   :*element-name*
   :*next-attribute*
   :*next-element*
   :*element-tags*
   :*end-sgml*
   :*end-comment*
   :*end-cdata*

   ;; meta
   :element-class
   :xml-direct-slot-definition
   :xml-effective-slot-definition

   ;; model
   :define-element-node
   :define-sgml-node
   :document-node
   :xml-document-node
   :dom-node
   :standard-element-node
   :element-node
   :branch-node
   :leaf-node
   :content-node
   :text-node
   :whitespace-node

   :sgml-node
   :?xml
   :!--
   :![CDATA[
   :!DOCTYPE

   :closing-tag

   ;; functions
   :parse-document
   :read-element
   :read-element-name
   :read-element-attributes
   :read-attribute
   :read-attribute-value
   :read-content
   :read-into
   :read-subelements
   :bind-child-node
   :prepare-slot
   :assign-value
   :parse-value
   :get-element-name
   :map-attribute-to-slot
   :map-attribute
   :tag-open-char
   :serialize
   :write-to-file))
   

(in-package xml.parse)
