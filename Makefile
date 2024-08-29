public/api/api.cgi: api.scm kelp-config.scm
	ol -i third-party/robusta -x c -o - api.scm | clang -x c - -o public/api/api.cgi
all:
	$(MAKE) public/api/api.cgi
