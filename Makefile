rundaemon:
	runhaskell -read-dot-ghci daemon/Main.hs Development

testmail:
	runhaskell -read-dot-ghci src/Email.hs

testnotify:
	runhaskell -read-dot-ghci src/Notifications.hs Development
