%%%-------------------------------------------------------------------
%%% @author David Gao <david.alpha.fox@gmail.com>
%%% @copyright (C) 2021, David Gao
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2021 by David Gao <david.alpha.fox@gmail.com>
%%%-------------------------------------------------------------------
-module(aikcp_socket).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(UDP_SOCK_OPTS, [{active, 10}, {header, 0}, binary]).
-define(KCP_UPDATE_INTERVAL, 50).

-record(state, {sock,
                pcb,
                peer_port}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(integer(),integer()) -> {ok, Pid :: pid()} |
        {error, Error :: {already_started, pid()}} |
        {error, Error :: term()} |
        ignore.
start_link(Port,PeerPort) ->
  gen_server:start_link(?MODULE, [Port,PeerPort], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
        {ok, State :: term(), Timeout :: timeout()} |
        {ok, State :: term(), hibernate} |
        {stop, Reason :: term()} |
        ignore.
init([Port,PeerPort]) ->
  process_flag(trap_exit, true),
  PCB = aikcp_pcb:new(1),
  case gen_udp:open(Port, ?UDP_SOCK_OPTS) of
    {error, Reason} -> {stop, Reason};
    {ok, Socket} ->
      timer:send_after(?KCP_UPDATE_INTERVAL, self(), kcp_update),
      {ok, #state{sock = Socket, peer_port = PeerPort,pcb = PCB}}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
        {reply, Reply :: term(), NewState :: term()} |
        {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
        {reply, Reply :: term(), NewState :: term(), hibernate} |
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
        {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
        {noreply, NewState :: term()} |
        {noreply, NewState :: term(), Timeout :: timeout()} |
        {noreply, NewState :: term(), hibernate} |
        {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({udp_passive, Socket}, State = #state{sock = Socket}) ->
  inet:setopts(Socket, [{active, 10}]),
  {noreply, State};

handle_info({udp, Socket, _IP, InPortNo, Packet},
            #state{sock = Socket,pcb = PCB} = State) ->
  {Payload,PCB2} = aikcp_pcb:recv(Packet,PCB),
  if Payload == undefined -> true;
     Payload == <<>> -> true;
     true -> io:format("Remote: ~p Payload: ~p~n",[InPortNo,Payload])
  end,
  {noreply,State#state{pcb = PCB2}};


handle_info(kcp_update, #state{pcb = PCB,sock = Socket,peer_port = PeerPort} = State) ->
  {Buffers,PCB2} = aikcp_pcb:update(PCB),
  lists:foreach(fun(Data)->
                    gen_udp:send(Socket,{127,0,0,1},PeerPort,Data)
                end,Buffers),
  timer:send_after(?KCP_UPDATE_INTERVAL, self(), kcp_update),
  {noreply,State#state{pcb = PCB2}};
handle_info({send, Data}, #state{pcb = PCB} = State) ->
  PCB2 = aikcp_pcb:send(Data,PCB),
  {noreply,State#state{pcb = PCB2}};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
        {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
