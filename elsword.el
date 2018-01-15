(require 'json)

(require 'dash)
(require 'ht)

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
  (let ((books (a-get bible 'book-names)))
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

(defun elsword-lookup-range (bible key1 key2)
  ;; NOTE: Works but just for entire books.
  (cl-labels ((book-chapters (book)
                             (cl-loop for chapter-num from 1
                                      for chapter = (chapter-verses chapter-num)
                                      while chapter
                                      append chapter))
              (chapter-verses (chapter-num)
                              (cl-loop for verse-num from 1
                                       for key = (list book chapter-num verse-num)
                                       for text = (ht-get all-verses key)
                                       while text
                                       append (a-list 'key key
                                                      'text text))))
    (pcase-let* (((map ('books all-books) ('verses all-verses)) bible)
                 (`(,book1 ,chapter1 ,verse1) key1)
                 (`(,book2 ,chapter2 ,verse2) key2)
                 (book1-index (seq-position all-books book1))
                 (book2-index (seq-position all-books book2))
                 (books (-slice all-books book1-index (1+ book2-index)))
                 (result))
      (cl-loop for book in books
               append (book-chapters book)))))

(defun elsword-lookup-range (bible key1 key2)
  ;; NOTE: Aborted attempt
  (cl-labels ((book-chapters (book)
                             (cl-loop for chapter-num from 1
                                      for chapter = (chapter-verses chapter-num)
                                      while chapter
                                      append chapter))
              (chapter-verses (chapter-num &optional (start 1) end)
                              (cl-loop for verse-num from start
                                       for key = (list book chapter-num verse-num)
                                       for text = (ht-get all-verses key)
                                       while (and text
                                                  (or (not end)
                                                      (< verse-num end)))
                                       append (a-list 'key key
                                                      'text text))))
    (pcase-let* (((map ('books all-books) ('verses all-verses)) bible)
                 (`(,book1 ,chapter1 ,verse1) key1)
                 (`(,book2 ,chapter2 ,verse2) key2)
                 (book1-index (seq-position all-books book1))
                 (book2-index (seq-position all-books book2))
                 (books (-slice all-books book1-index (1+ book2-index)))
                 (result))
      (append
       ;; Up to first whole book
       (cl-loop with chapter-num = chapter1
                with verse-num = verse1
                with book-end = t
                for key = (list book1 chapter-num verse-num)
                for text = (ht-get all-verses key)
                if text
                collect text
                and do (progn
                         (incf verse-num)
                         (setq book-end nil))
                else do (cond (book-end (return))
                              (t (incf chapter-num)
                                 (setq verse-num 1
                                       book-end t))))
       ;; Middle (entire) books
       (cl-loop with books = (cons book1 (butlast books))
                for book in books
                append (book-chapters book))))))

(defun elsword-lookup-range (bible key1 key2)
  ;; NOTE: Aborted, pre-nested-ht attempt
  (cl-labels ((book-name (name)
                         (let ((books (elsword-book-names bible name)))
                           (when (> (length books) 1)
                             (user-error "Ambiguous book name: %s" name))
                           (car books)))
              (book-chapters (book &optional (chapter-start 1) (verse-start 1) chapter-end verse-end)
                             (cl-loop for chapter-num from chapter-start
                                      for chapter = (cond ((and chapter-end
                                                                (= chapter-num chapter-end))
                                                           ;; Last chapter
                                                           (chapter-verses chapter-num 1))
                                                          ((chapter-verses chapter-num verse-start)))
                                      while (and chapter
                                                 (or (not chapter-end)
                                                     (<= chapter-num chapter-end)))
                                      append chapter))
              (chapter-verses (chapter-num &optional (start 1) end)
                              (cl-loop for verse-num from start
                                       for key = (list book chapter-num verse-num)
                                       for text = (ht-get all-verses key)
                                       while (and text
                                                  (or (not end)
                                                      (<= verse-num end)))
                                       collect (cons key text))))
    (pcase-let* (((map ('books all-books) ('verses all-verses)) bible)
                 (`(,book1 ,chapter1 ,verse1) key1)
                 (`(,book2 ,chapter2 ,verse2) key2)
                 (book1 (book-name book1))
                 (book2 (book-name book2))
                 (book1-index (seq-position all-books book1))
                 (book2-index (seq-position all-books book2))
                 (books (-slice all-books book1-index (1+ book2-index)))
                 (verses ))
      (cond ((equal book1 book2)
             ;; Within a single book
             (book-chapters book1 chapter1 verse1 chapter2 verse2))
            (t (cl-loop for book in books
                        append (cond ((equal book book1)
                                      ;; First book
                                      (book-chapters book chapter1 verse1))
                                     ((equal book book2)
                                      ;; Last book: pass last chapter/verse as arguments
                                      (book-chapters book 1 1 chapter2 verse2))
                                     (t
                                      ;; Middle books
                                      (book-chapters book 1 1 book2 verse2)))))))))

(defun elsword-lookup-range (bible key1 key2)
  ;; NOTE: nested-ht version
  (cl-labels ((book-name (name)
                         (let ((matches (elsword-book-names bible name)))
                           (when (> (length matches) 1)
                             (user-error "Ambiguous book name: %s" name))
                           (car matches))))
    (pcase-let* (((map book-names books) bible)
                 (`(,book1 ,chapter1 ,verse1) key1)
                 (`(,book2 ,chapter2 ,verse2) key2)
                 (book1 (book-name book1))
                 (book2 (book-name book2))
                 (book1-index (seq-position book-names book1))
                 (book2-index (seq-position book-names book2))
                 (book-names (-slice book-names book1-index (1+ book2-index))))
      (cl-loop for book in book-names
               for starting-chapter = (if (equal book book1)
                                          chapter1
                                        1)
               for ending-chapter = (if (equal book book2)
                                        chapter2
                                      nil)
               append (cl-loop for chapter from starting-chapter
                               for starting-verse = (if (and (equal book book1)
                                                             (equal chapter chapter1))
                                                        verse1
                                                      1)
                               for ending-verse = (if (and (equal book book2)
                                                           (equal chapter chapter2))
                                                      verse2
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
