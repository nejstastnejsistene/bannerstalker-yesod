default:
	cabal-dev install

devel:
	yesod -d devel

deploy-daemon:
	# Stop daemon
	service bannerstalkerd stop
	# Remove previous init.d script
	rm -f /etc/init.d/bannerstalkerd
	update-rc.d bannerstalkerd remove
	# Install init.d script
	cp config/init-script /etc/init.d/bannerstalkerd
	chmod 555 /etc/init.d/bannerstalkerd
	update-rc.d bannerstalkerd defaults
	# Copy the executable to /usr/sbin
	cp dist/build/bannerstalkerd/bannerstalkerd /usr/sbin/
	chmod 555 /usr/sbin/bannerstalkerd
	# Restart daemon
	service bannerstalkerd start
