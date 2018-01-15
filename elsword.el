(require 'json)

(require 'dash)
(require 'ht)

;; IDEA: Store verse text in a vector for constant-time access.  Map book-chapter-verse to verse
;; text in hash-tables.  To lookup a range, lookup the starting and ending verse numbers, then fetch
;; that range.

;;;; Functions

(defun elsword-json-to-ht (bible)
  "Return BIBLE as a hash-table.
BIBLE should be a list of alists of the form:

'(\"Genesis\" 1 1 \"In the beginning...\")

The verses in the hash table will have keys of the form:

'(\"Genesis\" 1 1)

The hash table will have these special keys:

`books': A list of the books found in BIBLE, in order."
  (let ((verses (let ((ht (ht)))
                  (seq-doseq (it bible)
                    (pcase-let* (((map book chapter verse text) it))
                      (ht-set ht (list book chapter verse) text)))
                  ht))
        (books (-uniq (seq-map (lambda (it)
                                 (a-get it 'book))
                               bible))))
    (a-list 'verses verses
            'books books)))

(defun elsword-json-to-nested-ht (bible)
  ;; FIXME: Docstring
  "Return BIBLE as a hash-table.
BIBLE should be a list of alists of the form:

'(\"Genesis\" 1 1 \"In the beginning...\")

The verses in the hash table will have keys of the form:

'(\"Genesis\" 1 1)

The hash table will have these special keys:

`books': A list of the books found in BIBLE, in order."
  (cl-labels ((transform-book-name (name)
                                   (if (string-match (rx bos (one-or-more "I")) name)
                                       (let ((length (length (match-string 0 name))))
                                         (s-replace-regexp (rx bos (1+ "I")) (int-to-string length) name))
                                     name)))
    (let* ((books (let ((books-ht (ht)))
                    (seq-doseq (it bible)
                      (pcase-let* (((map book chapter verse text) it)
                                   (book (transform-book-name book))
                                   (book-ht (or (ht-get books-ht book)
                                                (progn
                                                  (ht-set books-ht book (ht))
                                                  (ht-get books-ht book))))
                                   (chapter-ht (or (ht-get book-ht chapter)
                                                   (progn
                                                     (ht-set book-ht chapter (ht))
                                                     (ht-get book-ht chapter)))))

                        (ht-set chapter-ht verse text)))
                    books-ht))
           (book-names (-uniq (ht-keys books))))
      (a-list 'books books
              'book-names book-names))))

(defun elsword-json-to-hybrid-ht (bible)
  ;; FIXME: Docstring
  "Return BIBLE as a hash-table.
BIBLE should be a list of alists of the form:

'(\"Genesis\" 1 1 \"In the beginning...\")

The verses in the hash table will have keys of the form:

'(\"Genesis\" 1 1)

The hash table will have these special keys:

`books': A list of the books found in BIBLE, in order."
  (cl-labels ((transform-book-name (name)
                                   (if (string-match (rx bos (one-or-more "I")) name)
                                       (let ((length (length (match-string 0 name))))
                                         (s-replace-regexp (rx bos (1+ "I")) (int-to-string length) name))
                                     name)))
    (let* ((map-key-to-verse-num (ht))
           (map-verse-num-to-key)
           (verse-counter 0)
           book-names
           verses)
      (seq-doseq (it bible)
        (pcase-let* (((map book chapter verse text) it)
                     (book-name (transform-book-name book))
                     (key (list book-name chapter verse)))
          (ht-set map-key-to-verse-num key verse-counter)
          (push key map-verse-num-to-key)
          (push book-name book-names)
          (push text verses)
          (incf verse-counter)))
      (setq book-names (nreverse (-uniq book-names)))
      (setq map-verse-num-to-key (apply #'vector (nreverse map-verse-num-to-key)))
      (ht ('books book-names)
          ('key-to-num-map map-key-to-verse-num)
          ('num-to-key-map map-verse-num-to-key)
          ('verses (apply #'vector (nreverse verses)))))))

(let ((name "I John"))
  (if (string-match (rx bos (one-or-more "I")) name)
      (let ((length (length (match-string 0 name))))
        (s-replace (rx bos (one-or-more "I")) (int-to-string length) name))
    name))

(s-replace-regexp (rx bos (1+ "I")) "1" "I John")
(s-match (rx bos (1+ "I")) "II John")

(defun elsword-book-names (bible name)
  "Return full book name for NAME in BIBLE.
If NAME does not exactly match a name in BIBLE, return all
partial matches."
  ;; FIXME: Docstring
  (let ((books (ht-get bible 'books)))
    (if (member name books)
        ;; Always return a list
        (list name)
      ;; No match: find partial matches
      (--filter (string-match (rx-to-string `(seq bos ,name)) it)
                books))))

(cl-defmacro elsword-search (bible query)
  (declare (indent defun))
  (cl-labels ((wrap (it)
                    (pcase it
                      (`(not . ,clauses) `(not (or ,@(wrap clauses))))
                      (`(and . ,clauses) `(and ,@(wrap clauses)))
                      (`(or . ,clauses) `(or ,@(wrap clauses)))
                      ((pred stringp) `(string-match ,it text))
                      (else (-map #'wrap else)))))
    `(map-filter (lambda (_ text)
                   ,(wrap query))
                 (a-get ,bible 'verses))))

(defun elsword-lookup (bible book chapter verse)
  (ht-get (a-get bible 'verses) (list book chapter verse)))

(defun elsword-lookup-range (bible from to)
  ;; NOTE: nested-ht version
  (cl-labels ((book-name (name)
                         (let ((matches (elsword-book-names bible name)))
                           (when (> (length matches) 1)
                             (user-error "Ambiguous book name: %s" name))
                           (car matches))))
    (pcase-let* (((map book-names books) bible)
                 (`(,from-book ,from-chapter ,from-verse) from)
                 (`(,to-book ,to-chapter ,to-verse) to)
                 (from-book (book-name from-book))
                 (to-book (book-name to-book))
                 (from-book-index (seq-position book-names from-book))
                 (to-book-index (seq-position book-names to-book))
                 (book-names (-slice book-names from-book-index (1+ to-book-index))))
      (cl-loop for book in book-names
               for starting-chapter = (if (equal book from-book)
                                          from-chapter
                                        1)
               for ending-chapter = (if (equal book to-book)
                                        to-chapter
                                      nil)
               append (cl-loop for chapter from starting-chapter
                               for starting-verse = (if (and (equal book from-book)
                                                             (equal chapter from-chapter))
                                                        from-verse
                                                      1)
                               for ending-verse = (if (and (equal book to-book)
                                                           (equal chapter to-chapter))
                                                      to-verse
                                                    nil)
                               for verses = (cl-loop for verse from starting-verse
                                                     for text = (ignore-errors (ht-get* books book chapter verse))
                                                     while (and text
                                                                (or (null ending-verse)
                                                                    (<= verse ending-verse)))
                                                     collect (a-list 'key (list book chapter verse)
                                                                     'text text))
                               while (and verses
                                          (or (null ending-chapter)
                                              (<= chapter ending-chapter)))
                               append verses)))))

(defun elsword-split-key (key)
  "Return KEY split into a list containing the book name as a string, then the chapter number and verse number as integers."
  ;; NOTE: This could work fairly well, but I think the best way is to take the actual list of books into account.
  (cl-labels ((int (arg) (ignore-errors (string-to-int arg))))
    (let* ((tokens (s-split (rx (or space ":")) key)))
      (pcase (reverse tokens)
        (`(,(and (pred int) verse)
           ,(and (pred int) chapter)
           ,book)
         (list book (int chapter) (int verse)))
        (`(,(and (pred int) chapter)
           ,book)
         (list book (int chapter)))
        (`(,book) (list book))
        ))))

(defun elsword-split-key (bible key)
  "Return KEY split into a list containing the book name as a string, then the chapter number and verse number as integers."
  ;; Split key into tokens, then accumulate from the first until a matching book name is found
  (cl-labels ((get-names (key)
                         (pcase-let* ((tokens (s-split (rx (or space ":")) key))
                                      ((map book-name other-tokens)
                                       (cl-loop for token = (pop tokens)
                                                for book-name = (s-trim (s-join " " (list book-name token)))
                                                for book-names = (elsword-book-names bible book-name)
                                                until (or book-names
                                                          (null token))
                                                finally return (a-list 'book-name (car book-names)
                                                                       'other-tokens tokens))))
                           (when book-name
                             (cons book-name (mapcar #'string-to-int other-tokens)))))
              (arabic-to-roman (s)
                               (pcase s
                                 ("1" "I")
                                 ("2" "II")
                                 ("3" "III")))
              (transform (key)
                         ;; Change Arabic numerals in book names to Roman numerals
                         (let* ((tokens (s-split (rx (or space ":")) key)))
                           (when (member (car tokens) '("1" "2" "3"))
                             (setf (car tokens) (arabic-to-roman (car tokens)))
                             (s-join " " tokens)))))
    (if-let ((result (get-names key)))
        result
      (when-let ((key (transform key))
                 (result (get-names key)))
        result
        )
      )))

(defun elsword-split-key (bible key)
  "Return KEY split into a list containing the book name as a string, then the chapter number and verse number as integers."
  ;; Split key into tokens, then accumulate from the first until a matching book name is found
  (pcase-let* ((tokens (s-split (rx (or space ":")) key))
               (book-name (cl-loop for token = (pop tokens)
                                   for book-name = (s-trim (s-join " " (list book-name token)))
                                   for book-names = (elsword-book-names bible book-name)
                                   until (or (and book-names
                                                  (= 1 (length book-names)))
                                             (null token))
                                   finally return (car book-names)))
               (non-name-tokens (elsword-remove-prefix (s-split-words book-name) tokens)))
    (when book-name
      (cons book-name (mapcar #'string-to-int non-name-tokens)))))


(defun elsword-remove-prefix (remove from)
  (if (-is-prefix? remove from)
      (-slice from (length remove))
    from))



(elsword-remove-prefix '("1" "John") '("1" "John" "1" "1"))

;;;; Forms

;;;;; Loading

;; Load JSON to list
(setq elsword-esv-json (json-read-file "esv.json"))

;; Convert list to hash table
(setq elsword-esv-ht (elsword-json-to-ht elsword-esv-json))
(setq elsword-esv-ht (elsword-json-to-nested-ht elsword-esv-json))
(setq elsword-esv-hybrid-ht (elsword-json-to-hybrid-ht elsword-esv-json))

(defun lookup (bible key)
  (let* ((key (elsword-split-key bible key))
         (verse-num (ht-get* bible 'map key))
         (text (elt (ht-get bible 'verses) verse-num)))
    (list key text)))

(defun lookup (bible from &optional to)
  ;; TODO: Handle full-chapter and full-book lookup
  (let* ((from (elsword-split-key bible from))
         (to (when to (elsword-split-key bible to)))
         (from-verse-num (ht-get* bible 'key-to-num-map from))
         (to-verse-num (if to
                           (ht-get* bible 'key-to-num-map to)
                         from-verse-num)))
    (cl-loop for verse-num from from-verse-num to to-verse-num
             for key = (elt (ht-get bible 'num-to-key-map) verse-num)
             for text = (elt (ht-get bible 'verses) verse-num)
             collect (cons key text))))

(lookup elsword-esv-hybrid-ht "Gen 1:1")
(message "%s" (lookup elsword-esv-hybrid-ht "Mark 1:2"))

(lookup elsword-esv-hybrid-ht "Matt 1:1" "Mark 1:3")
(s-join "  " (--map (cdr it) (lookup elsword-esv-hybrid-ht "John 1:1" "John 1:3")))
(setq print-length nil)
(bench 1 (lookup elsword-esv-hybrid-ht "Gen 1:1" "Gen 1:3"))

;;;;; Book names

(elsword-book-names elsword-esv-ht "2")

;;;;; Keys

;; TODO: Make tests for these.

(elsword-split-key elsword-esv-ht "1 John 1:1")
(elsword-split-key elsword-esv-ht "2 John 1:1")
(elsword-split-key elsword-esv-ht "John 1:1")
(elsword-split-key elsword-esv-ht "John 1")
(elsword-split-key elsword-esv-ht "John")

;;;;; Lookup

(elsword-lookup elsword-esv-ht "Genesis" 1 1)

(elsword-lookup-range elsword-esv-ht '("Matt" 28 19) '("Mark" 1 2))
(elsword-lookup-range elsword-esv-ht '("Matt" 28 19) '("Matt" 28 20))
(elsword-lookup-range elsword-esv-ht '("Gen" 28 18) '("Gen" 28 19))
(elsword-lookup-range elsword-esv-ht '("Gen" 1 1) '("Gen" 1 2))


(ht-get* (a-get elsword-esv-ht 'books) "Genesis" 28 19)


;;;;; Search

(elsword-search elsword-esv-ht
  (and "Jesus" "Paul" (not "apostle" "test")))

;;;; Benchmarks

(bench 1 (map-filter (lambda (_ val)
                       (string-match "Jesus" val))
                     esv-ht))
(bench 1
  (elsword-lookup elsword-esv-ht "Genesis" 1 1))

(bench 1
  (elsword-search elsword-esv-ht
    (and "Jesus" "Paul"
         (not "apostle"))))


;; Local Variables:
;; flycheck-mode: nil
;; End:
