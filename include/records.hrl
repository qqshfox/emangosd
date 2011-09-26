-record(hash, {modulus=16#894B645E89E1535BBDAD5B8B290650530801B18EBFBF5E8FAB3C82872A3E9BB7,
		generator=7, multiplier=3, public, secret, verifier, salt, session_key,
		session_proof, client_proof}).

-record(account, {id, name, password}).

-record(realm, {id, icon=0, lock=0, flags=16#04,
		name="eMaNOSd Server", address="192.168.56.100:8085",
		population_level=0.0, amount_of_characters=0, timezone=1, unknown=16#2C,
		version_major=3, version_minor=3, version_bugfix=5, build=13507}).
