

(asdf:defsystem #:scp-q-formatter
  :version "0.1.0"
  :author "Yukko"
  :license "CC0"
  :depends-on ("alexandria")
  :components ((:file "main"))
  :description "SCP財団問答部門のモック作成を簡単にするスクリプト"
  :build-operation "program-op"
  :build-pathname "scp-q-formatter"
  :entry-point "scp-q-formatter:main")
