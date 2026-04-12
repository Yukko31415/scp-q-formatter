

(uiop:define-package #:scp-q-formatter
  (:mix #:cl #:uiop #:alexandria)
  (:export #:main))

(in-package #:scp-q-formatter)



;; プロット
;; まずフラグ管理をするか
;; Q: が出てきた時に立つフラグ
;; 解答: でフラグを初期化
;; Q: のフラグが立っている間にもう一度Q: が来るとエラー
;; Q: の後は選択肢が来ることを前提とする
;; プロットここまで





;; 内容が変わらないもの

(defparameter *header*
  (concatenate 'string
	       "[[include component:coltop show=▶ クイズ|hide=▲ 閉じる]]~%"
	       "[[html]]~%"
	       "<!DOCTYPE html>~%"
	       "<html>~%"
	       "<head>~%"
	       "  <meta content=\"width=device-width ,initial-scale=1.0\" name=\"viewport\">~%"
	       "  <style>~%"
	       "  @import url (\"https://d3g0gp89917ko0.cloudfront.net/v--291054f06006/common--theme/base/css/style.css\");~%"
	       "  @import url (\"https://scp-jp.wdfiles.com/local--code/component%3Atheme/2\");~%"
	       "  </style>~%"
	       "  <title>問答部門からの挑戦状</title>~%"
	       "</head>~%"
	       "<body>~%~%"))

(defparameter *tail*
  (concatenate 'string
	       "</body>~%"
	       "</html>~%"
	       "[[/html]]~%"
	       "[[include component:colend hide=▲ クイズを閉じる]]~%"))


(defparameter *js-start* "<script type=\"text/javascript\">~%~%")

(defparameter *js-end* "</script>~%~%")





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



(defun circled->int (char)
  ;; 丸数字を普通の数字に変換する
  (let ((circled-1 (load-time-value (char-code #\①)))
	(circled-20 (load-time-value (char-code #\⑳)))
	(char-code (char-code char)))
    (assert (<= circled-1 char-code circled-20))
    (- char-code 9311)))

(defun circled-num-p (char)
  (let ((circled-1 (load-time-value (char-code #\①)))
	(circled-20 (load-time-value (char-code #\⑳)))
	(char-code (char-code char)))
    (<= circled-1 char-code circled-20)))

(defun option-int-p (char)
  (<= (char-code #\0) (char-code char) (char-code #\9)))

(defun option-number-p (char)
  (or (circled-num-p char)
      (option-int-p char)))

(defun parse-option-number (str &key (start 0) end junk-allowed)
  (unless junk-allowed (assert (not (find-if (complement #'option-number-p) str :start start :end end)) ()
			       (simple-parse-error "junk in string ~S" str)))
  (cond ((circled-num-p (char str start)) (values (circled->int (char str start)) 1))
	(t (parse-integer str :start start :end end :junk-allowed junk-allowed))))

(defun char-code-from-a (num)
  ;; 1 = #\a, 2 = #\b
  (assert (plusp num))
  (code-char (+ num (1- (char-code #\a)))))


;; -----------------------
;;;; make-question-string
;; -----------------------

(defparameter *question-string*
  (concatenate 'string
	       "<p><b>問~0@*~a:</b> ~1@*~a</p>~%"
	       "    <form id=\"quiz~0@*~a\" name=\"quiz~0@*~a\">~%"
	       "~2@*~a"
	       "        <br>~%"
	       "        <input onclick=\"TrueOrFalse~0@*~a()\" type=\"button\" value=\"解答する\">~%"
	       "    </form>~%"
	       "<p id=\"ans~0@*~a\">〔解答はここに表示されます〕</p>~%~%"))


(defun make-question-option (option-number option)
  (format nil "        <input name=\"answer\" type=\"radio\" value=\"~a\"> ~a<br>~%"
	  (char-code-from-a option-number) option))


(defun make-question-string (number body options &optional (stream nil))
  ;; make-question-string number body options &optional stream
  ;; number = a positive interger.
  ;; body = a string.
  ;; options = a list of strings.
  (let ((option (apply #'concatenate 'string (loop :for i :from 1 :for opt :in options
						   :collect (make-question-option i opt)))))
    (format stream *question-string* number body option)))



;; ----------------------------
;;;; make-true-or-false-string
;; ----------------------------


(defparameter *true-or-false-string*
  (concatenate 'string
	       "function TrueOrFalse~a() {~%"
	       "~{~A~}~%"
	       "}~%~%"))

(defparameter *if-statement*
  (concatenate 'string
	       "    ~0@*~A (quiz~1@*~A.answer.value == '~2@*~A') {~%"
	       "        var myp = document.getElementById(\"ans~1@*~A\");~%"
	       "        myp.innerHTML = ~3@*~S;~%"
	       "    } "))

(defun %make-if-statements (if-or-else question-number option-number answerp)
  (format nil *if-statement* (ecase if-or-else (:if "if") (:else "else") (:elseif "else if"))
	  question-number (char-code-from-a option-number) (if answerp "正解です！" "不正解です。")))

(defun make-if-statements (question-number ans-number option-length)
  (assert (>= option-length ans-number))
  (assert (>= option-length 2))
  (loop :for i :from 1 :to option-length
	:if (= i 1)
	  :collect (%make-if-statements :if question-number 1 (= ans-number 1))
	:else :if (< 1 i option-length)
		:collect (%make-if-statements :elseif question-number i (= ans-number i))
	:else :collect (%make-if-statements :else question-number option-length (= ans-number option-length))
	      :and :do (loop-finish)))

(defun make-true-or-false-string (question-number ans-number option-length &optional stream)
  ;; make-true-or-false-string question-number ans-number option-length
  ;; question-number = a positive integer.
  ;; ans-number = a positive integer.
  ;; option-length = a non-negative integer.
  (let ((if-statements (make-if-statements question-number ans-number option-length)))
    (format stream *true-or-false-string* question-number if-statements)))




;; ----------------
;;;; make-question
;; ----------------

;; Q. 「SCP-001-JP - アースリングス」において、SCP-001-JPを構成する大多数の物質はどの元素を主体としているでしょう？
;; ① アルミニウム
;; ② ケイ素
;; ③ カルシウム
;; ④ ゲルマニウム
;; 解答 ②

;; このような文字列をフォーマットしたい
;; 行で区切り、先頭の文字列一致で確認していくか？

(defstruct (question (:constructor %make-question))
  question-number body options option-length ans-number)

(defun skip-space (sequence &key (start 0))
  (position-if (curry (complement #'char=) #\space) sequence :start start))

(defun skip-brank-line (stream)
  (loop :for str := (read-line stream)
	:when (notevery (rcurry #'member '(#\space #\newline) :test #'char=) str)
	  :return str))

(defun make-question.body (stream &aux (it (skip-brank-line stream)))
  (when (string-prefix-p "Q." it)
    (subseq it (skip-space it :start 2))))

(defun %make-question.options (string)
  (multiple-value-bind (num pos) (parse-option-number string :junk-allowed t)
    (when num (subseq string (skip-space string :start pos)))))

(defun %make-question.answer (str)
  (when (string-prefix-p "解答" str)
    (parse-option-number str :start (skip-space str :start 2))))

(defun make-question.options (stream)
  (loop :for i :from 1
	:for str := (skip-brank-line stream)
	:if (%make-question.options str)
	  :collect :it :into options
	  :and :count t :into length
	:else
	  :return (when-let (ans (%make-question.answer str))
		    (values options length ans))))

(defun make-question (question-number stream &aux body options length ans)
  (handler-case
      (progn (setf body (make-question.body stream))
	     (setf (values options length ans) (make-question.options stream))
	     (%make-question :question-number question-number :body body :options options
			     :option-length length :ans-number ans))
    (end-of-file nil)))

(defun %make-questions (stream)
  (loop :for question-num :from 1
	:when (make-question question-num stream)
	  :collect :it
	:else :do (loop-finish)))

(defun make-questions (pathname)
  (with-input-file (stream pathname)
    (%make-questions stream)))

(defun %make-new-pathname (pathname &optional number)
  (let ((name (make-pathname :directory (pathname-directory pathname)
			     :name (format nil "~A_formatted~@[-~A~]" (pathname-name pathname) number)
			     :type "txt")))
    (if (file-exists-p name)
	(%make-new-pathname pathname (1+ (or number 0)))
	name)))

(defun make-new-pathname (pathname)
  (%make-new-pathname pathname))

(defun main ()
  (let* ((pathname (parse-namestring (read-line)))
	 (questions (make-questions pathname)))
    (with-output-file (stream (make-new-pathname pathname)
			      :if-does-not-exist :create)
      (format stream *header*)
      (loop :for question :in questions
	    :do (with-slots (question-number body options) question
		  (make-question-string question-number body options stream))
	    :finally (terpri stream))
      (loop :for question :in questions
	    :initially (format stream *js-start*)
	    :do (with-slots (question-number ans-number option-length) question
		  (make-true-or-false-string question-number ans-number option-length stream))
	    :finally (format stream *js-end*))
      (format stream *tail*)))
  (quit))















