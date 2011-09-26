-record(crypto_state, {authenticated=false, encrypt_key, decrypt_key}).

-record(session, {
		rest=(<<>>),
		crypto_state=#crypto_state{},
		address}).
