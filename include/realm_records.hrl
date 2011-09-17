-record(challenge, {error, size, gamename,
		version_major, version_minor, version_bugfix, build,
		platform, os, country, timezone_bias, ip, ilen, i}).

-record(proof, {a, m1, crc, number_of_keys, security_flags}).

-record(logon_state, {authenticated=false, hash, account}).

