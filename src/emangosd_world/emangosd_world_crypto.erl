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

-module(emangosd_world_crypto).

-author('Hanfei Shen <qqshfox@gmail.com>').

-compile(export_all).

-include("world_records.hrl").

encrypt(Header, #crypto_state{authenticated=true, encrypt_key=K}=State) ->
	error_logger:info_report([encrypt, {authenticated, true}, {encrypt_key, K}]),
	{NewKey, NewHeader} = crypto:rc4_encrypt_with_state(K, Header),
	{State#crypto_state{encrypt_key=NewKey}, NewHeader};
encrypt(Header, #crypto_state{authenticated=true, encrypt_key=K}=State) ->
	error_logger:info_report([encrypt, {authenticated, true}]),
	{NewKey, NewHeader} = crypto:rc4_encrypt_with_state(K, Header),
	{State#crypto_state{encrypt_key=NewKey}, NewHeader};
encrypt(Header, #crypto_state{authenticated=false}=State) ->
	error_logger:info_report([encrypt, {authenticated, false}]),
	{State, Header}.

decrypt(Header, #crypto_state{authenticated=true, decrypt_key=K}=State) ->
	error_logger:info_report([decrypt, {authenticated, true}, {decrypt_key, K}]),
	{NewKey, NewHeader} = crypto:rc4_encrypt_with_state(K, Header),
	{State#crypto_state{decrypt_key=NewKey}, NewHeader};
decrypt(Header, #crypto_state{authenticated=false}=State) ->
	error_logger:info_report([decrypt, {authenticated, false}]),
	{State, Header}.

encrypt_key(Account, ClientSeed, ServerSeed, K) ->
	Sha1 = crypto:sha_init(),
	Sha1Update1 = crypto:sha_update(Sha1, Account),
	Sha1Update2 = crypto:sha_update(Sha1Update1, <<0:32>>),
	Sha1Update3 = crypto:sha_update(Sha1Update2, ClientSeed),
	Sha1Update4 = crypto:sha_update(Sha1Update3, ServerSeed),
	Sha1Update5 = crypto:sha_update(Sha1Update4, K),
	crypto:sha_final(Sha1Update5).

session_crypto_key(server_encryption_key, K) ->
	Seed = 16#5753914293C0DD12CAEA97E804AE98CC,
	InitKey = crypto:rc4_set_key(crypto:sha_mac(<<Seed:128/little>>, K)),
	crypto:rc4_encrypt_with_state(InitKey, <<0:(1024*8)/little>>);
session_crypto_key(server_decryption_key, K) ->
	Seed = 16#CE67432FEE533C34B5D9AEC63C72B3C2,
	InitKey = crypto:rc4_set_key(crypto:sha_mac(<<Seed:128/little>>, K)),
	crypto:rc4_encrypt_with_state(InitKey, <<0:(1024*8)/little>>).
