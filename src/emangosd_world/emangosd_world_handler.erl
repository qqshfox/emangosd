%%%------------------------------------------------------------------
%%% emangosd
%%% Copyright (C) 2011 Hanfei Shen <qqshfox@gmail.com>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%------------------------------------------------------------------

-module(emangosd_world_handler).

-author('Hanfei Shen <qqshfox@gmail.com>').

-compile(export_all).

-include("world_opcodes.hrl").
-include("world_records.hrl").
-include("records.hrl").

auth_session(Data, #crypto_state{address=Address}=State) ->
	{ok, Build, Account, ClientSeed, Digest} = emangosd_world_auth_session_codec:decode(Data),
	ServerSeed = <<0:32>>,
	error_logger:info_report([{build, Build}, {account, Account}, {client_seed, ClientSeed}, {server_seed, ServerSeed}, {digest, Digest}]),
	[{Account, K}] = ets:lookup(logon_authenticated_accounts, Account),
	error_logger:info_report([{k, K}]),
	case emangosd_world_crypto:encrypt_key(Account, ClientSeed, ServerSeed, K) of
		Digest ->
			EncryptKey = emangosd_world_crypto:session_crypto_key(K),
			DecryptKey = emangosd_world_crypto:session_crypto_key(K),
			error_logger:info_report([authenticated_successfully, {account, Account}, {address, Address}, {encrypt_key, EncryptKey}, {decrypt_key, DecryptKey}]),
			{{send, ?SMSG_AUTH_RESPONSE, <<12, 0:32, 0, 0:32, 1>>}, State#crypto_state{authenticated=true, encrypt_key=EncryptKey, decrypt_key=DecryptKey}};
		_Other ->
			error_logger:info_report(_Other),
			{error, authenticated_failed}
	end.
