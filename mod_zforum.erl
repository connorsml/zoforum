%% @author Michael Connors <michael@bring42.net>
%% @copyright 2010 Michael Connors
%% @date 2010-28-12
%% @doc A simple forum module for Zotonic.

%% Copyright 2010 Michael Connors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_zforum).
-author("Michael Connors <michael@bring42.net>").

-mod_title("Forum").
-mod_description("A simple forum for Zotonic").
-mod_prio(500).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_datamodel:manage(?MODULE, datamodel(), Context),
    z_notifier:observe(post_added, self(), Context),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({{post_added, ThreadId, PostId}, Ctx}, State) ->
     %% In here we update the has_unviewed forum posts for any users that are following this thread
     %% perhaps later also send message to inbox or notification area 
    {rsc_list, Followers} = m_rsc:p(ThreadId, zf_has_followers, Ctx),
    F = fun(UserId, PostId) ->
            {ok, _} = m_edge:insert(UserId, has_unviewed_forum_posts, PostId, Ctx)
	end,
    [ F(UserId, PostId) || UserId <- Followers ],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%
%% The datamodel that is used in this module.
%%
datamodel() ->
    [{categories,
      [
       {zforum_category,
        categorization,
        [{title, <<"Forum Category">>}]},
       {zforum_thread,
        meta,
        [{title, <<"Forum Thread">>}]},
       {zforum_post,
        text,
        [{title, <<"Forum Post">>}]}
      ]
     },
     {predicates,
      [
       {has_forum_category,
        [{title, <<"Has Category">>}],
        [{zforum_thread, zforum_category}]},
       {zf_has_followers,
        [{title, <<"Has Followers">>}],
        [{zforum_thread, person}]},
       {has_unviewed_forum_posts,
        [{title, <<"Has Unviewed Posts">>}],
        [{person, zforum_post}]},
       {has_starred_forum_posts,
        [{title, <<"Has Starred Posts">>}],
        [{person, zforum_post}]},
       {has_forum_post,
        [{title, <<"Has Post">>}],
        [{zforum_thread, zforum_post}]},
       {is_fork_of,
        [{title, <<"Is Fork Of">>}],
        [{zforum_thread, zforum_post}]}
      ]
     },
     {resources,
      [
       {zf_category_general,
        zforum_category,
        [{title, <<"General">>},
         {summary, <<"Discuss anything">>}]
       },
       {zf_category_sport,
        zforum_category,
        [{title, <<"Sport">>},
         {summary, <<"Discuss Sport">>}]
       },
       {zf_category_politics,
        zforum_category,
        [{title, <<"Politics">>},
         {summary, <<"Discuss Politics">>}]
       },
       {zf_category_entertainment,
        zforum_category,
        [{title, <<"Entertainment">>},
         {summary, <<"Hollywood Gossip">>}]
       }
      ]
     }
    ].

