rundaemon:
	runhaskell -read-dot-ghci daemon/Main.hs Development

testmail:
	runhaskell -read-dot-ghci src/Email.hs
