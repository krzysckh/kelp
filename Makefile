OL=ol
maybe_sqlite != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'

public/api/api.cgi: api.scm kelp-config.scm Makefile
	$(OL) -i third-party/robusta -x c -o - api.scm | clang -x c - $(maybe_sqlite) -static -lm -lpthread -o public/api/api.cgi
all:
	$(MAKE) public/api/api.cgi
