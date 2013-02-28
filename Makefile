rundaemon:
	runhaskell -package-conf=cabal-dev/packages-7.4.2.conf -read-dot-ghci daemon/Main.hs Development

install:
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
