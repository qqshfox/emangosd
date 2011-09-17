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

-module(emangosd_realm_handler).

-author('Hanfei Shen <qqshfox@gmail.com>').

-export([challenge/2, proof/2]).

-include("realm_opcodes.hrl").
-include("realm_records.hrl").
-include("records.hrl").

challenge(Data, State) ->
	error_logger:info_report([{data, Data}, {state, State}]),
	case emangosd_realm_challenge_codec:decode(Data) of
		{ok, #challenge{i=Username}, Rest} ->
			error_logger:info_report([{username, Username}]),
			case get_account(Username) of
				{ok, Account} ->
					Hash = emangosd_srp6a:challenge(Account),
					NewState = State#logon_state{account=Account, hash=Hash},
					error_logger:info_report([{hash, Hash}]),
					PacketToSend = build_challenge_reply(Hash),
					error_logger:info_report([{packet_to_send, PacketToSend}]),
					{{send, PacketToSend}, Rest, NewState};
				{error, Reason} ->
					error_logger:info_report([{error, Reason}]),
					error(Reason)
			end;
		{error, partial} ->
			{ok, Data, State}
	end.

proof(Data, #logon_state{account=Account, hash=Hash}=State) ->
	error_logger:info_report([{data, Data}, {state, State}]),
	case emangosd_realm_proof_codec:decode(Data) of
		{ok, #proof{a=A, m1=M1}, Rest} ->
			error_logger:info_report([{a, A}, {m1, M1}, {account, Account}, {hash, Hash}]),
			NewHash = emangosd_srp6a:proof(A, Hash, Account),
			error_logger:info_report([{new_hash, NewHash}]),
			case NewHash#hash.client_proof of
				M1 ->
					PacketToSend = build_proof_reply(NewHash),
					NewState = State#logon_state{authenticated=true, hash=NewHash},
					{{send, PacketToSend}, Rest, NewState};
				_Other ->
					Reason = incorrect_password,
					error_logger:info_report([{error, Reason}]),
					error(Reason)
			end;
		{error, partial} ->
			{ok, Data, State}
	end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_account(Username) ->
	{ok, #account{name=Username, password="123123"}}.

build_challenge_reply(Hash) ->
	<<?CMD_AUTH_LOGON_CHALLENGE, 0, 0, (Hash#hash.public):256/little, 1, (Hash#hash.generator):(1*8)/little, 32, (Hash#hash.modulus):(32*8)/little, (Hash#hash.salt):256/little, 0:128/little, 0>>.

build_proof_reply(Hash) ->
	<<?CMD_AUTH_LOGON_PROOF, 0, (Hash#hash.session_proof):160/little, 16#00800000:32, 0:32, 0:16>>.
