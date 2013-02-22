rundaemon:
	runhaskell -read-dot-ghci daemon/Main.hs Development

install:
	cp config/bannerstalkerd.rc /usr/local/etc/rc.d/bannerstalkerd
	chmod 555 /usr/local/etc/rc.d/bannerstalkerd
	cp ../.cabal/bin/bannerstalkerd /usr/local/sbin/
	chmod 555 /usr/local/sbin/bannerstalkerd
