%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(rtp_channel).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/rtp.hrl").
-include("../include/rtcp.hrl").
-include("../include/stun.hrl").

% Default value of RTP timeout in milliseconds.
-define(INTERIM_UPDATE, 30000).

-record(state, {
		parent,
		subscriber = null,
		rtp,
		rtcp,
		ip = null,
		rtpport = null,
		rtcpport = null,
		ssrc = null,
		proxy,
		sendrecv,
		mux,
		lastseen = null,
		% If set to true then we'll have another one INTERIM_UPDATE
		% interval to wait for initial data
		alive = false,
		tref
	}
).

start(Args) ->
	gen_server:start(?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init([Parent, Params]) when is_pid(Parent) ->
	% Choose udp, tcp, sctp, dccp - FIXME only udp is supported
	Transport = proplists:get_value(transport, Params),
	SockParams = proplists:get_value(sockparams, Params, []),
	% Either specify IPv4 or IPv6 explicitly or provide two special
	% values - "::" for any available IPv6 or "0.0.0.0" or "0" for
	% any available IPv4.
	{ok, IpAddr} = inet_parse:address(proplists:get_value(ip, Params, "0.0.0.0")),
	% Either specify port explicitly or provide none if don't care
	IpPort = proplists:get_value(port, Params, 0),
	% 'weak' - receives data from any Ip and Port with any SSRC
	% 'selective' - receives data from only one Ip and Port *or* with the same SSRC as before
	% 'enforcing' - Ip, Port and SSRC *must* match previously recorded data
	% 'srtp' - depends on SRTP/ZRTP
	SendRecvStrategy = proplists:get_value(sendrecv, Params, weak),
	% 'true' - act as a proxy (send RTP and RTCP packets further as is)
	% 'false' - strip-off everything except RTP payload. Regenerate RTCP. 
	% FIXME only proxy (bypass) mode for now
	Proxy = proplists:get_value(proxy, Params, true),
	% 'false' = no muxing at all (RTP will be sent in the RTP and RTCP - in the RTCP channels separately)
	% 'true' - both RTP and RTCP will be sent in the RTP channel
	% 'auto' - the same as 'false' until we'll find muxed packet.
	MuxRtpRtcp = proplists:get_value(rtcpmux, Params, auto),

	{ok, Timeout} = proplists:get_value(timeout, Params, ?INTERIM_UPDATE),
	{ok, TRef} = timer:send_interval(Timeout, interim_update),

	{Fd0, Fd1} = get_fd_pair({IpAddr, IpPort, SockParams}),

	% Initially all traffic will be sent to the Parent
	{ok, #state{
			parent = Parent,
			subscriber = Parent,
			rtp = Fd0,
			rtcp = Fd1,
			mux = MuxRtpRtcp,
			sendrecv = SendRecvStrategy,
			proxy = Proxy,
			tref = TRef
		}
	}.

handle_call(_Call, _From, State) ->
	{stop, bad_call, State}.

handle_cast({rtp, Ip, Port, #rtp{} = Pkts}, #state{rtp = Fd} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkts)),
	{noreply, State};
handle_cast({rtp, Ip, Port, Pkts}, #state{rtcp = Fd, mux = false} = State) when is_list(Pkts) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkts)),
	{noreply, State};
handle_cast({rtp, Ip, Port, Pkts}, #state{rtp = Fd, mux = true} = State) when is_list(Pkts) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkts)),
	{noreply, State};
handle_cast({rtp, Ip, Port, Pkts}, #state{rtp = Fd, mux = auto} = State) when is_list(Pkts) ->
	% FIXME - see transport parameter in the init(...) function arguments
	% FIXME
	{noreply, State};

handle_cast({raw, Ip, Port, {PayloadType, Msg}}, State) ->
	% FIXME
	{noreply, State};

handle_cast({update, Params}, State) ->
	% FIXME consider changing another params as well
	SendRecvStrategy = proplists:get_value(sendrecv, Params, weak),
	{ok, State#state{sendrecv = SendRecvStrategy}};

handle_cast(_Cast, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{parent = Parent, rtp = Fd0, rtcp = Fd1, tref = TRef}) ->
	% FIXME - see transport parameter in the init(...) function arguments
	timer:cancel(TRef),
	gen_udp:close(Fd0),
	% FIXME We must send RTCP bye here
	gen_udp:close(Fd1),
	ok.

% 'weak' mode - just get data, decode and notify subscriber
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = weak, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), ip = Ip, rtpport = Port, alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = weak, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), ip = Ip, rtcpport = Port, alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

% 'selective' mode - get data, check for the Ip and Port or for the SSRC (after decoding), decode and notify subscriber
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = selective, ip = Ip, rtpport = Port, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = selective, rtpport = null, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		% FIXME Are we sure that first packet will always be a RTP one?
		{ok, #rtp{ssrc = SSRC} = Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{ip = Ip, rtpport = Port, ssrc = SSRC, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = selective, ssrc = SSRC, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		% FIXME what if this will be a muxed RTCP?
		{ok, #rtp{ssrc = SSRC} = Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		% FIXME we shouldn't set rtcpport to null here - use SSRC instead
		{noreply, State#state{ip = Ip, rtpport = Port, rtcpport = null, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = selective, ip = Ip, rtcpport = Port,  subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = selective, rtcpport = null, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), ip = Ip, rtcpport = Port, alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

% 'enforcing' - Ip, Port and SSRC must match previously recorded data
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = enforcing, ip = Ip, rtpport = Port, ssrc = SSRC, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		% FIXME what if this will be a muxed RTCP?
		{ok, #rtp{ssrc = SSRC} = Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtp = Fd, sendrecv = enforcing, rtpport = null, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		% FIXME Are we sure that first packet will always be a RTP one?
		{ok, #rtp{ssrc = SSRC} = Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{ip = Ip, rtpport = Port, ssrc = SSRC, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = enforcing, ip = Ip, rtcpport = Port,  subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = enforcing, rtcpport = null, subscriber = Subscriber} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtp:decode(Msg),
		gen_server:cast(Subscriber, {rtp, Ip, Port, Pkts}),
		{noreply, State#state{lastseen = now(), ip = Ip, rtcpport = Port, alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

% 'srtp' - depends on SRTP/ZRTP
handle_info({udp, Fd, Ip, Port, Msg}, #state{rtcp = Fd, sendrecv = srtp, rtcpport = null, subscriber = Subscriber} = State) ->
	% TODO
	{noreply, State};

handle_info(interim_update, #state{parent = Parent, alive = true} = State) ->
	gen_server:cast(Parent, {interim_update, self()}),
	{noreply, State#state{alive = false}};
handle_info(interim_update, #state{alive = false} = State) ->
	{stop, timeout, State};

handle_info(_Info, State) ->
	{noreply, State}.

%%
%% Private functions
%%

%% Open a pair of UDP ports - N and N+1 (for RTP and RTCP consequently)
get_fd_pair({{I0,I1,I2,I3,I4,I5,I6,I7} = IPv6, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 65536,
	is_integer(I1), 0 =< I1, I1 < 65536,
	is_integer(I2), 0 =< I2, I2 < 65536,
	is_integer(I3), 0 =< I3, I3 < 65536,
	is_integer(I4), 0 =< I4, I4 < 65536,
	is_integer(I5), 0 =< I5, I5 < 65536,
	is_integer(I6), 0 =< I6, I6 < 65536,
	is_integer(I7), 0 =< I7, I7 < 65536 ->
	get_fd_pair(IPv6, Port, proplists:delete(ipv6, SockParams) ++ [inet6], 10);
get_fd_pair({{I0,I1,I2,I3} = IPv4, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 256,
	is_integer(I1), 0 =< I1, I1 < 256,
	is_integer(I2), 0 =< I2, I2 < 256,
	is_integer(I3), 0 =< I3, I3 < 256 ->
	get_fd_pair(IPv4, Port, proplists:delete(ipv6, SockParams), 10).

get_fd_pair(Ip, Port, SockParams, 0) ->
	error_logger:error_msg("Create new socket at ~s:~b FAILED (~p)", [inet_parse:ntoa(Ip), Port,  SockParams]),
	error;
get_fd_pair(Ip, Port, SockParams, NTry) ->
	case gen_udp:open(Port, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
		{ok, Fd} ->
			{ok, {Ip,Port}} = inet:sockname(Fd),
			Port2 = case Port rem 2 of
				0 -> Port + 1;
				1 -> Port - 1
			end,
			case gen_udp:open(Port2, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
				{ok, Fd2} ->
					if
						Port > Port2 -> {Fd2, Fd};
						Port < Port2 -> {Fd, Fd2}
					end;
				{error, _} ->
					gen_udp:close(Fd),
					get_fd_pair(Ip, Port, SockParams, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(Ip, Port, SockParams, NTry - 1)
	end.
