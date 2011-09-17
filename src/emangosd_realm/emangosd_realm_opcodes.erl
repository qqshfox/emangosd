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

-module(emangosd_realm_opcodes).

-compile([export_all]).

-include("realm_opcodes.hrl").

get_handler(?CMD_AUTH_LOGON_CHALLENGE)     -> challenge;
get_handler(?CMD_AUTH_LOGON_PROOF)         -> proof;
get_handler(?CMD_AUTH_RECONNECT_CHALLENGE) -> rechallenge;
get_handler(?CMD_AUTH_RECONNECT_PROOF)     -> reproof;
get_handler(?CMD_REALM_LIST)               -> realm_list.