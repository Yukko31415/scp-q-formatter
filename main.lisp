

(uiop:define-package #:scp-q-formatter
  (:mix #:cl #:uiop #:alexandria)
  (:export #:main))

(in-package #:scp-q-formatter)




#| プロット


まずフラグ管理をするか

Q: が出てきた時に立つフラグ
解答: でフラグを初期化

Q: のフラグが立っている間にもう一度Q: が来るとエラー
Q: の後は選択肢が来ることを前提とする


プロットここまで |#

(defun read-all-input-to-string ()
  "標準入力からEOFに達するまですべての行を読み込み、
   一つの文字列にまとめて返す"
  (with-output-to-string (s)
			 (loop for line = (read-line *standard-input* nil nil)
			       while line
			       do (write-string line s)
			       (write-char #\newline s))))


(defmacro deflag (name)
  (let ((flag-name (intern (format nil "*~a*" (string-upcase name)))))
    `(progn
       (defparameter ,flag-name nil)
       (defun ,flag-name (flag)
	 (setf ,flag-name flag)))))

(defmacro defp (name &optional (number 0))
  (let ((flag-name (intern (format nil "*~a*" (string-upcase name)))))
    `(progn
       (defparameter ,flag-name ,number)
       (defun ,flag-name (flag)
	 (case flag
	       (inc (incf ,flag-name))
	       (res (setf ,flag-name 0)))))))

(defmacro put (&rest body)
  `(the (or string null) (format s ,@body)))

(defmacro defun-s (name args &body body)
  `(defun ,name ,args
     (the string
	  (with-output-to-string (s)
				 ,@body))))

;; 変数宣言

(defparameter *stdin-input* nil)
(defparameter *list*  nil)

;; 変数宣言ここまで


;; フラグ作成

(deflag questionp)

;; フラグ作成終了


;; parameter定義

(defp question-number)
(defp ans-value)

;; 定義終了


;; 内容の変わらないI/O

(defun-s head ()
	 "ヘッダー"
	 (put "
[[include component:coltop show=▶ クイズ|hide=▲ 閉じる]]
[[html]]
<!DOCTYPE html>
<html>
<head>
    <meta content=\"width=device-width,initial-scale=1.0\" name=\"viewport\">
    <style>
    @import url(\"https://d3g0gp89917ko0.cloudfront.net/v--291054f06006/common--theme/base/css/style.css\");
    @import url(\"https://scp-jp.wdfiles.com/local--code/component%3Atheme/2\");
    </style>
    <title>問答部門からの挑戦状</title>
</head>

<body>"))


(defun-s end ()
	 (put "
</body>
</html>
[[/html]]
[[include component:colend hide=▲ クイズを閉じる]]
"))


(defun-s js-start ()
	 "JSの始まり"
	 (put "~%<script type=\"text/javascript\">" i))

(defun-s js-end ()
	 "JSの終わり"
	 (put "</script>"))


(defun increment-char-by-number (number)
  "指定した文字を指定した数字分インクリメントして返す"
  (code-char (+ (char-code #\a) number)))



#|

<p><b>問1:</b> 「SCP-3801-JP - おっぱいのんで、ねんねしな」において、アルバートが飼育していたヨリックはどの動物でしょう？</p>
<form id="quiz1" name="quiz1">
     <input name="answer" type="radio" value="a"> イエイヌ<br>
     <input name="answer" type="radio" value="b"> ラット<br>
     <input name="answer" type="radio" value="c"> ニワトリ<br>
     <input name="answer" type="radio" value="d"> キンギョ<br>
<br>
     <input onclick="TrueOrFalse1()" type="button" value="解答する">
    </form>
<p id="ans1">〔解答はここに表示されます〕</p>

|#


#|

(format t "~%<p><b>問~a:</b>" *question-number*)
(format t "<form id=\"quiz~a\" name=\"quiz~a\">~%" *question-number*  *question-number*)
(format t "<input name=\"answer\" type=\"radio\" value=\"~a\"> ~a<br>~%" (increment-char-by-number *ans-value*))
(format t "<br>~%")
(format t "<input onclick=\"TrueOrFalse~a()\" type=\"button\" value=\"解答する\">~%" *question-number*)
(format t "</form>~%")
(format t "<p id=\"ans~a\">〔解答はここに表示されます〕</p>" *question-number*)

|#



(defun maru-to-digit (char)
  "丸数字を普通の数字に変換する"
  (let ((maru-ichi-code 9312)
	(char-code (char-code char)))
    (if (<= maru-ichi-code char-code (char-code #\⑳))
	(- char-code 9311)
      char)))

(defun-s question-title (stream)
	 (*question-number* 'inc)
	 (put "~%<p><b>問~a:</b>" *question-number*)
	 (put "~a</p>~%" stream)
	 (*questionp* t))

(defun-s answer-head ()
	 (put "    <form id=\"quiz~a\" name=\"quiz~a\">~%" *question-number* *question-number*))

(defun-s answer-option (content)
	 (put "        <input name=\"answer\" type=\"radio\" value=\"~a\"> ~a<br>~%" (increment-char-by-number *ans-value*) content)
	 (*ans-value* 'inc))

(defun answer-memory (number)
  (push *question-number* *list*)
  (push *ans-value* *list*)
  (push (maru-to-digit number) *list*))

(defun-s question-end ()
	 (put "        <br>~%")
	 (put "        <input onclick=\"TrueOrFalse~a()\" type=\"button\" value=\"解答する\">~%" *question-number*)
	 (put "    </form>~%")
	 (put "<p id=\"ans~a\">〔解答はここに表示されます〕</p>~%" *question-number*)
	 (*questionp* nil)
	 (*ans-value* 'res))




(defun-s TrueOrFalse-head (q-number)
	 (put "~%function TrueOrFalse~a() {~%" q-number))

(defun-s TrueOrFalse-if (q-number i)
	 (put "~a (quiz~a.answer.value == '~a') {~%"
	      (if (= i 0)
		  "    if"
		"    } else if")
	      q-number (increment-char-by-number i)))

(defun-s TrueOrFalse-body (q-number a-number i)
	 (put "        var myp = document.getElementById(\"ans~a\");~%" q-number)
	 (put "        myp.innerHTML = \"~a\";~%" (if
						      (= i (1- a-number))
						      "正解です！"
						    "不正解です。")))

(defun-s TrueOrFalse-else-end (q-number)
	 (put "    } else {~%")
	 (put "        var myp = document.getElementById(\"ans~a\");~%" q-number)
	 (put "        myp.innerHTML = \"~a問目の答えを選択してください。\";~%" q-number)
	 (put "    }~%}~%"))
  




(defun-s make (path)
	 (setf *stdin-input* path)
	 (put (head))
	 (with-open-file (stream *stdin-input*)
			 (loop
			  (let ((data (read stream nil :eof)))
			    (when (eq data :eof) (progn (*question-number* 'res) (return)))
			    (if *questionp*
				(case data
				      (解答
				       (progn (answer-memory (read-char stream))
					      (put (question-end))))
				      (t
				       (put (answer-option (read-line stream)))))
			      (case data
				    (Q. (progn (put (question-title (read-line stream)))
					       (put (answer-head))))))))
			 (put (js-start))
			 (let ((ans-list (reverse *list*)))
			   (loop
			    (if ans-list
				(let ((q-number (the fixnum (pop ans-list)))
				      (ans-value (the fixnum (pop ans-list)))
				      (a-number (the fixnum (pop ans-list))))
				  (declare (fixnum q-number ans-value a-number))
				  (put (TrueOrFalse-head q-number))
				  (loop for i from 0 below ans-value
					do (put (TrueOrFalse-if q-number i))
					(put (TrueOrFalse-body q-number a-number i)))
				  (put (TrueOrFalse-else-end q-number)))
			      (progn (setf *list* nil) (return)))))
			 (put (js-end))
			 (put (end))))



(defun save-with-new-name (original-path name-suffix new-extension content)
  "元のファイル名に文字列を追加し、拡張子を変更してファイルを保存する関数"
  (let* ((original-pathname (pathname original-path))
	 (original-name (pathname-name original-pathname))
	 (new-name (format nil "~a~a" original-name name-suffix))
	 (new-pathname (make-pathname :name new-name
				      :type new-extension
				      :defaults original-pathname)))
    (with-open-file (stream new-pathname :direction :output
			    :if-exists :supersede)
		    (write-string content stream))))

(defun main ()
  (let ((path (pathname (read-line))))
    (save-with-new-name path "_formated" "txt" (format nil (make path))))
  (quit))



