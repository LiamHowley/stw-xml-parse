(in-package :cl-user)

(defpackage xml.parse
  (:nicknames :xml)
  (:use :cl)
  (:import-from
   :cl-comp
   :map-filtered-precedents
   :filter-slots-by-type
   :find-slot-definition)
   
  (:import-from
   :stw.util
   :sequence-from-file
   :sequence-to-file
   :parse-stream
   :ensure-list
   :concat-string
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
   :*encoder*
   :consume-whitespace
   :consume-until
   :stw-read-char
   :next
   :stw-peek-next-char
   :stw-peek-last-char
   :match-string
   :match-character
   :read-until
   :read-and-decode
   :read-and-encode
   :with-encoder)

  (:import-from
   :stw.util
   :trie
   :make-trie
   :insert-word
   :find-word
   :walk-branch
   :trie-leaf)

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

   ;;util
   :defclass-with-initargs

   ;; specials
   :*opening-char*
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
   :element
   :class->element
   :slot-index
   :attribute->slot
   :slot-definition-attribute

   ;; type
   :multiple-attributes

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
   :attribute-node
   :text-node
   :whitespace-node
   :generic-node

   :parent-node
   :child-nodes
   :file
   :document
   :text
   :the-content

   :sgml-node
   :?xml
   :!--
   :![CDATA[
   :!DOCTYPE

   :the-content
   :closing-tag)

   ;; reading/parsing functions
  (:export
   :parse%
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
   :assign-slot-value
   :parse-value
   :get-element-name
   :map-attribute-to-slot
   :map-attribute)

  ;; printing / writing
  (:export
   :*indent*
   :*print-childnodes*
   :serialize
   :serialize-object
   :write-to-file
   :print-slot
   :indent-string)

  ;; conditions & restarts
  (:export
   :*mode*
   :class-not-found-error
   :slot-not-found-error
   :assign-generic-node
   :assign-slot-to-attribute
   :ignore-missing-slot)

  ;; query functions
  (:export
   :find-ancestor-node
   :clone-node
   :walk-tree
   :retrieve-comments
   :retrieve-text-nodes
   :retrieve-text-nodes-from-parents
   :retrieve-text-nodes-with-token
   :retrieve-text-nodes-with-tokens
   :retrieve-text-nodes-with-all-tokens
   :get-elements-by-tagname
   :get-element-with-attribute
   :get-element-with-attributes
   :get-element-with-attribute-value
   :get-element-with-attribute-values
   :get-elements-with-attribute
   :get-elements-with-attributes
   :get-elements-with-attribute-value
   :get-elements-with-attribute-values
   :get-next-sibling
   :get-previous-sibling
   :query-select
   :query-select-all
   :remove-node
   :add-node
   :insert-before
   :insert-after
   :first-child
   :last-child
   :first-of-type
   :last-of-type)

  (:export
   :remove-reader
   :set-reader
   :readerp
   :read-xml))
