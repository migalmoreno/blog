DOMAIN=mianmoreno.com
.PHONY: publish/vps publish/sourcehut

publish/vps:
	haunt build
	rsync -P -rvz --delete site/ cygnus:/srv/http/${DOMAIN} --cvs-exclude

publish/sourcehut:
	haunt build
	tar -C site -cvz . -f site.tar.gz
	hut pages publish -d ${DOMAIN} site.tar.gz
