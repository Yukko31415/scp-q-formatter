
(uiop:define-package #:scp-q-formatter.error
  (:use #:cl-scheme-like-syntax)
  (:mix #:cl-scheme-like-syntax #:uiop #:alexandria)
  (:export #:invalid-answer-number
	   #:invalid-question-sentence
	   #:question-parse-time-error
	   #:too-few-options
	   #:unmatch-question-number))

(in-package #:scp-q-formatter.error)




(define-condition question-parse-time-error (error)
  ((line :reader parse-error-line :initarg :line))
  (:documentation "問題をパースした際に問題を通知するエラー"))




(define-condition unmatch-question-number (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "選択肢番号が適切ではありません。")))
  (:documentation "選択肢の数字が不一致の際に通知されるエラー"))

(define-condition too-few-options (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "選択肢は必ず2つ以上必要です。")))
  (:documentation "選択肢が1つだけしかない場合に通知されるエラー"))

(define-condition invalid-answer-number (error)
  ((answer-number :reader answer-number :initarg :answer-number))
  (:report (lambda (c s) (format s "~A は無効な解答番号です。" (answer-number c))))
  (:documentation "無効な解答選択肢があった場合に通知されるエラー")
  ;; (>= 1 n max-question-number)
  )

(define-condition invalid-question-sentence (error)
  ((sentence :reader question-sentence :initarg :sentence))
  (:report (lambda (c s) (format s "\"~A\"は無効な文章です。" (question-sentence c))))
  (:documentation "無効な問題文があった場合に通知されるエラー"))








(uiop:define-package #:scp-q-formatter
  (:use #:cl-scheme-like-syntax)
  (:mix #:cl-scheme-like-syntax #:uiop #:alexandria #:scp-q-formatter.error)
  (:export #:main))

(in-package #:scp-q-formatter)




;; 内容が変わらないもの

(defparameter *header*
  (format nil "~{~A~%~}~%"
	  '("[[include component:coltop show=▶ クイズ|hide=▲ 閉じる]]"
	    "[[html]]"
	    "<!DOCTYPE html>"
	    "<html>"
	    "<head>"
	    "  <meta content=\"width=device-width ,initial-scale=1.0\" name=\"viewport\">"
	    "  <style>"
	    "  @import url (\"https://d3g0gp89917ko0.cloudfront.net/v--291054f06006/common--theme/base/css/style.css\");"
	    "  @import url (\"https://scp-jp.wdfiles.com/local--code/component%3Atheme/2\");"
	    "  </style>"
	    "  <title>問答部門からの挑戦状</title>"
	    "</head>"
	    "<body>")))

(defparameter *tail*
  (format nil "~{~A~^~%~}"
	  '("</body>"
	    "</html>"
	    "[[/html]]"
	    "[[include component:colend hide=▲ クイズを閉じる]]")))


(defparameter *js-start* (format nil "<script type=\"text/javascript\">~%~%"))

(defparameter *js-end* (format nil "</script>~%~%"))





;; <p><b>問1:</b> 「SCP-3801-JP - おっぱいのんで、ねんねしな」において、アルバートが飼育していたヨリックはどの動物でしょう？</p>
;; <form id="quiz1" name="quiz1">
     ;; <input name="answer" type="radio" value="a"> イエイヌ<br>
     ;; <input name="answer" type="radio" value="b"> ラット<br>
     ;; <input name="answer" type="radio" value="c"> ニワトリ<br>
     ;; <input name="answer" type="radio" value="d"> キンギョ<br>
  ;; <br>
  ;; <input onclick="TrueOrFalse1()" type="button" value="解答する">
;; </form>
;; <p id="ans1">〔解答はここに表示されます〕</p>


;; ---------
;;;; utils
;; ---------


(defun circled-num? (char)
  (let ((circled-1 (load-time-value (char->code #\①)))
	(circled-20 (load-time-value (char->code #\⑳)))
	(char-code (char->code char)))
    (<= circled-1 char-code circled-20)))

(defun option-number? (char)
  (or (circled-num? char)
      (char-numeric? char)))

(deftype option-int ()
  `(and character (satisfies char-numeric?)))

(deftype circled-num ()
  `(and character (satisfies circled-num?)))

(deftype option-number ()
  `(or circled-num option-int))



(defun circled->int (char)
  ;; 丸数字を普通の数字に変換する
  (declare (circled-num char))
  (let ((char-code (char->code char)))
    (- char-code (load-time-value (1- (char->code #\①))))))


(defun parse-option-number (str &key (start 0) end junk-allowed)
  (unless (or junk-allowed (not (find-if (complement (cut #'type? <> 'option-number))
				      str :start start :end end)))
    (simple-parse-error "junk in string ~S" str))
  (let ((circled-num? (type? (char str start) 'circled-num)))
    (cond (circled-num? (values (circled->int (char str start)) (1+ start)))
	  (else         (parse-integer str :start start :end end :junk-allowed junk-allowed)))))

(defun integer->alpha (num)
  ;; 1 = #\a, 2 = #\b
  (declare ((integer 1) num))
  (code->char (+ num (1- (char->code #\a)))))



;; ------------------------
;;;; make-question-string
;; ------------------------

(defparameter *question-string*
  (string-append "<p><b>問~0@*~a:</b> ~1@*~a</p>~%"
		 "    <form id=\"quiz~0@*~a\" name=\"quiz~0@*~a\">~%"
		 "~2@*~{~a~}"
		 "        <br>~%"
		 "        <input onclick=\"TrueOrFalse~0@*~a()\" type=\"button\" value=\"解答する\">~%"
		 "    </form>~%"
		 "<p id=\"ans~0@*~a\">〔解答はここに表示されます〕</p>~%~%"))


(defun make-question-option (option-number option)
  (format nil "        <input name=\"answer\" type=\"radio\" value=\"~a\"> ~a<br>~%"
	  (integer->alpha option-number) option))


(defun make-question-string (number body options &optional (stream nil))
  ;; make-question-string number body options &optional stream
  ;; number = a positive interger.
  ;; body = a string.
  ;; options = a list of strings.
  (let ((option (loop :for i :from 1 :for opt :in options
		      :collect (make-question-option i opt))))
    (format stream *question-string* number body option)))


;; -----------------------------
;;;; make-true-or-false-string
;; -----------------------------


(defparameter *true-or-false-string*
  (string-append "function TrueOrFalse~a() {~%"
		 "    ~{~A~}~%"
		 "}~%~%"))

(defparameter *if-statement*
  (string-append "~0@*~A (quiz~1@*~A.answer.value == '~2@*~A') {~%"
		 "        var myp = document.getElementById(\"ans~1@*~A\");~%"
		 "        myp.innerHTML = ~3@*~S;~%"
		 "    } "))

(defparameter *if-statement/else*
  (string-append "~0@*~A {~%"
		 "        var myp = document.getElementById(\"ans~1@*~A\");~%"
		 "        myp.innerHTML = ~2@*~S;~%"
		 "    }"))


(defun %make-if-statements (if-or-else question-number option-number return-string)
  (ecase if-or-else
    ((:if :else-if)
     (format nil *if-statement* (ecase if-or-else (:if "if") (:else-if "else if"))
	     question-number (if option-number (integer->alpha option-number) "") return-string))
    (:else (format nil *if-statement/else* "else" question-number return-string))))


(defun make-if-statements (question-number ans-number option-length)
  (assert (<= 2 option-length) () 'too-few-options)
  (assert (<= ans-number option-length) () 'invalid-answer-number :answer-number ans-number)
  (loop :for num :from 1 :to (1+ option-length)
	:if (= num 1)
	  :collect (if (= num ans-number)
		       (%make-if-statements :if question-number num "正解です！")
		       (%make-if-statements :if question-number num "不正解です。")) :else
	:if (<= num option-length)
	  :collect (if (= num ans-number)
		       (%make-if-statements :else-if question-number num "正解です！")
		       (%make-if-statements :else-if question-number num "不正解です。"))
	:else
	  :collect (%make-if-statements :else question-number num
					(string-append (number->string question-number)
						       "問目の答えを選択してください"))))

(defun make-true-or-false-string (question-number ans-number option-length &optional stream)
  ;; make-true-or-false-string question-number ans-number option-length
  ;; question-number = a positive integer.
  ;; ans-number = a positive integer.
  ;; option-length = a non-negative integer.
  (format stream *true-or-false-string* question-number
	  (make-if-statements question-number ans-number option-length)))


;; ------------------
;;;; make-questions
;; ------------------

;; Q. 「SCP-001-JP - アースリングス」において、SCP-001-JPを構成する大多数の物質はどの元素を主体としているでしょう？
;; ① アルミニウム
;; ② ケイ素
;; ③ カルシウム
;; ④ ゲルマニウム
;; 解答 ②

(defstruct (question (:constructor %make-question))
  (question-number 0 :type integer)
  (body ""           :type string)
  (options (list)    :type list)
  (option-length 0   :type integer)
  (ans-number 0      :type integer))


(defun parse-body (string initial?)
  (if initial?
      (and-let* (((string-prefix-p "Q." string))
		 (pos (position-if (complement #'char-whitespace?) string :start 2)))
	(subseq string pos))
      string))

(defun parse-option (string)
  (let* ((pos-1 (position-if (complement #'option-number?) string))
	 (pos-2 (position-if (complement #'char-whitespace?) string :start pos-1)))
    (subseq string pos-2)))

(defun parse-answer (string)
  (and-let* (((string-prefix-p "解答" string))
	     (pos (position-if (complement #'char-whitespace?) string :start 2)))
    (parse-option-number string :start pos :junk-allowed t)))

(defun list-right-trim-whitespace (list)
  (cond ((null? list) nil)
	((every (cut #'member <> (list "" (string #\newline)) :test #'string=?) list) nil)
	(t (cons (car list) (list-right-trim-whitespace (cdr list))))))




(defmacro next@ (place)
  `(ecase ,place
     (:body   (set@ ,place :option))
     (:option (set@ ,place :answer))))

(defun make-question (question-number stream)
  (loop :with phase    := :body
	:with initial? := t
	:for str := (when-let (str (read-line stream nil nil)) (string-right-trim '(#\cr #\lf) str))
	:when (null? str) :return nil
	  :do (case phase
		(:body   (when (string-prefix-p "①" str)   (next@ phase)))
		(:option (when (string-prefix-p "解答" str) (next@ phase))))

	:when (eq? phase :body)
	  :if initial?
	    :collect (parse-body str t) :into body :and
	    :do (set@ initial? nil)
	  :else
	    :collect (string #\newline)   :into body :and
	    :collect (parse-body str nil) :into body :end :else
	:when (eq? phase :option)
	  :when (some (complement #'char-whitespace?) str)
	    :collect (parse-option str) :into options
	    :and :count t :into length :end :else
	:when (eq? phase :answer)
	  :maximize (parse-answer str) :into ans-number
	  :and :do (loop-finish) :end

	:finally (return (%make-question :question-number question-number
					 :body (apply #'string-append (list-right-trim-whitespace body))
					 :options options
					 :option-length length
					 :ans-number ans-number))))

(defun %make-questions (stream)
  (loop :for question-num :from 1
	:when (make-question question-num stream)
	  :collect :it
	:else :do (loop-finish)))

(defun make-questions (pathname)
  (with-input-file (stream pathname)
    (%make-questions stream)))



;; --------
;;;; main
;; --------

(defun %make-new-pathname (pathname &optional number)
  (let ((name (make-pathname :directory (pathname-directory pathname)
			     :name (format nil "~A_formatted~@[-~A~]" (pathname-name pathname) number)
			     :type "txt")))
    (if (file-exists-p name)
	(%make-new-pathname pathname (1+ (or number 0)))
	name)))

(defun make-new-pathname (pathname)
  (%make-new-pathname pathname))

(defun get-pathname-from-terminal ()
  (loop (handler-case
	    (return (progn (format t "~A" "ファイルパスを入力: ") (finish-output)
			   (truename (parse-namestring (string-trim '(#\space) (read-line))))))
	  (error () (format t "エラーが発生しました。もう一度入力してください。~%")))))


(defun main ()
  (let* ((pathname (get-pathname-from-terminal))
	 (questions (make-questions pathname)))
    (with-output-file (stream (make-new-pathname pathname)
			      :if-does-not-exist :create)
      (format stream "~A" *header*)
      (loop :for question :in questions
	    :with initialp := t
	    :do (if initialp (set@ initialp nil) (format stream "<hr>~%~%"))
		(with-slots (question-number body options) question
		  (make-question-string question-number body options stream))
	    :finally (terpri stream))
      (loop :for question :in questions
	    :initially (format stream "~A" *js-start*)
	    :do (with-slots (question-number ans-number option-length) question
		  (make-true-or-false-string question-number ans-number option-length stream))
	    :finally (format stream "~A" *js-end*))
      (format stream "~A" *tail*)))
  (quit))















