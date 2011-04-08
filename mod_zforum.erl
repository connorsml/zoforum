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

-include_lib("zotonic.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-export([event/2]).

-record(state, {context}).

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_datamodel:manage(?MODULE, datamodel(), Context),
    {ok, #state{context=z_context:new(Context)}}.


handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%Handle adding a new forum post
event({submit, {addpost, [{thread_id, ThreadId}]}, _TriggerId, _TargetId}, Context) ->
    UserId = z_acl:user(Context),
    Body = z_context:get_q_validated("body", Context),
    {ok, PostId} = m_zforum:create_post(Body, ThreadId, UserId, Context),
    PostTemplate = z_template:render("_zforum_post.tpl", [{post_id, PostId}], Context),
    z_render:insert_bottom("zforum_posts", PostTemplate, Context);

%Handle adding a new forum thread & post
event({submit, {addpost, [{cat_id, CatId}]}, _TriggerId, _TargetId}, Context) ->
    UserId = z_acl:user(Context),
    Title = z_context:get_q_validated("title", Context),
    Summary = z_context:get_q_validated("summary", Context),
    Body = z_context:get_q_validated("body", Context),
    case m_zforum:create_thread(Title, Summary, CatId, UserId, Context) of
        {ok, ThreadId} -> 
            {ok, _PostId} = m_zforum:create_post(Body, ThreadId, UserId, Context),
            ThreadSummaryTemplate = z_template:render("_zforum_thread_summary.tpl", [{thread_id, ThreadId}], Context),
            z_render:insert_top("zf_forum_threads", ThreadSummaryTemplate, Context);
    	{error, _Message} -> Context
    end;


%Handle forking a thread
%% This is so that if a user wants to reply directly to a post rather than a thread, they can break this away from the current thread
%% it will be come a stand-alone thread in the category, which may or may not be the same as the category if the last thread
%% it could be tagged in the ui with some like """ "New Thread about Chelsea" was "Old Thread about Soccer" """
event({submit, {addpost, [{cat_id, CatId}, {post_id, PostId}]}, _TriggerId, _TargetId}, Context) ->
    UserId = z_acl:user(Context),
    Title = z_context:get_q_validated("title", Context),
    Summary = z_context:get_q_validated("summary", Context),
    Body = z_context:get_q_validated("body", Context),
    case m_zforum:create_thread(Title, Summary, CatId, UserId, Context) of
        {ok, ThreadId} -> 
            {ok, _} = m_edge:insert(ThreadId, is_fork_of, PostId, Context),
            m_zforum:create_post(Body, ThreadId, UserId, Context);
    	{error, _Message} -> Context
    end;

%Handle changing a forum post
event({submit, {editpost, _Args}, _TriggerId, _TargetId}, Context) ->
    %Title = z_context:get_q_validated("title", Context),
    %Body = z_context:get_q_validated("body", Context),
    %Id = z_context:get_q_validated("post_id", Context),
    Context;

%Handle deleting a forum post
event({submit, {deletepost, _Args}, _TriggerId, _TargetId}, Context) ->
    %Id = z_context:get_q_validated("post_id", Context),
    Context.

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

