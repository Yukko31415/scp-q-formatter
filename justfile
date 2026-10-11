
[arg("port", long="port")]
[arg("client", long="client")]
exec port="4005" client="sly":
	sbcl --noinform --eval '(asdf:load-system :scp-q-formatter)' \
	     --eval '(asdf:load-system :{{ if client == "slime" { "swank" } else { "slynk" } }})' \
		 --eval '({{ if client == "slime" { "swank" } else { "slynk" } }}:create-server :port {{port}} :dont-close t)'


app := file_name(justfile_directory())
make:
	ocicl install
	buildapp --asdf-tree '{{app}}' \
	         --eval '(asdf:make :scp-q-formatter)'


