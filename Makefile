rundaemon:
	runhaskell -package-conf=cabal-dev/packages-7.4.2.conf -read-dot-ghci daemon/Main.hs Development

install:
	cp config/bannerstalkerd.rc /usr/local/etc/rc.d/bannerstalkerd
	chmod 555 /usr/local/etc/rc.d/bannerstalkerd
	cp dist/build/bannerstalkerd/bannerstalkerd /usr/local/sbin/
	chmod 555 /usr/local/sbin/bannerstalkerd
