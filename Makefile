.PHONY: publish

publish:
	rsync -P -rvz --delete site/ cygnus:/srv/http/conses.eu --cvs-exclude
