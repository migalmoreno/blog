DOMAIN=migalmoreno.com
.PHONY: publish/vps publish/sourcehut

build:
	haunt build

serve:
	haunt serve --watch

publish:
	haunt build
	rsync -P -rvz --delete site/ cygnus:/srv/http/${DOMAIN} --cvs-exclude
