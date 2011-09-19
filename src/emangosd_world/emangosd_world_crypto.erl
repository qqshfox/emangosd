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

encrypt(Header, #crypto_state{authenticated=false}) ->
	Header.

decrypt(Header, #crypto_state{authenticated=false}) ->
	Header.

encrypt_key(Account, ClientSeed, ServerSeed, K) ->
	Sha1 = crypto:sha_init(),
	Sha1Update1 = crypto:sha_update(Sha1, Account),
	Sha1Update2 = crypto:sha_update(Sha1Update1, <<0:32>>),
	Sha1Update3 = crypto:sha_update(Sha1Update2, ClientSeed),
	Sha1Update4 = crypto:sha_update(Sha1Update3, ServerSeed),
	Sha1Update5 = crypto:sha_update(Sha1Update4, K),
	crypto:sha_final(Sha1Update5).
