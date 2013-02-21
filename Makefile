rundaemon:
	runhaskell -read-dot-ghci daemon/Main.hs Development

testnotify:
	runhaskell -read-dot-ghci src/Notifications.hs Development
