DOMAIN=migalmoreno.com
.PHONY: publish

build:
	haunt build

serve:
	haunt serve --watch

publish:
	haunt build
	rsync -P -rvz --delete site/ cygnus:/srv/http/${DOMAIN} --cvs-exclude

repl:
	guix shell guile-next guix guile-ares-rs -- guile -L . \
	-c "((@ (nrepl server) run-nrepl-server) #:port 7888)"
