%% @author Michael Connors <michael@bring42.net>, Dmitrii Dimandt <dmitri@dmitriid.com>
%% @copyright 2011 Michael Connors
%% @doc ZForum resource

%% Copyright 2011 Michael Connors
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

-module(m_zforum).
-author("Michael Connors <michael@bring42.net>, Dmitrii Dimandt <dmitri@dmitriid.com>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    get/2,

    create_forum/4,
    create_thread/5,
    create_post/4
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, z_context:get_reqdata(Context)).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    values(z_context:get_reqdata(Context)).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    values(z_context:get_reqdata(Context)).


%% @doc Fetch threads, posts, etc.
get(What, #context{} = Context) -> get(What, z_context:get_reqdata(Context));
get(_What, undefined) -> undefined;
get(method, RD) -> wrq:method(RD);
get(version, RD) -> wrq:version(RD);
get(peer, RD) -> wrq:peer(RD);
get(is_ssl, RD) ->
    case wrq:port(RD) of
        {ssl, _} -> true;
        _ -> false
    end; 
get(host, RD) -> wrq:get_req_header_lc("host", RD);
get(raw_path, RD) -> wrq:raw_path(RD);
get(path, RD) -> wrq:path(RD);
get(qs, RD) -> wrq:req_qs(RD);
get(headers, RD) -> wrq:req_headers(RD);
get(user_agent, RD) -> proplists:get_value("user-agent", wrq:req_headers(RD));
get(_Key, _RD) -> undefined.


values(RD) ->
    [ {K, get(K,RD)} || K <- [
            method, version, peer, is_ssl, host, raw_path, path, qs, headers
        ]
    ].


create_forum(Title, Summary, UserId, Context) ->
    %% TODO: support multiple forums
    ok.


create_thread(Title, Summary, ForumCategoryId, UserId, Context) ->
    CategoryId = m_category:name_to_id_check(zforum_thread, Context),
    Props = [
          {category_id, CategoryId},
          {title, Title},
          {summary, Summary},
          {is_published, true}],
    case m_rsc:insert(Props, Context) of
        {ok, ThreadId} ->
            {ok, _} = m_edge:insert(ThreadId, has_forum_category, ForumCategoryId, Context),
            {ok, _} = m_edge:insert(ThreadId, author, UserId, Context),
            {ok, ThreadId};
        {error, _} ->
	    {error, <<"Thread not created!">>}
    end.

create_post(Body, ThreadId, UserId, Context) ->
    Title = m_rsc:p(ThreadId, title, Context),
    Summary = m_rsc:p(ThreadId, summary, Context),
    CategoryId = m_category:name_to_id_check(zforum_post, Context),
    Props = [
          {category_id, CategoryId},
          {title, Title},
          {summary, Summary},
          {body, Body},
          {is_published, true}],
    {ok, PostId} = m_rsc:insert(Props, Context),
    {ok, _} = m_edge:insert(ThreadId, has_forum_post, PostId, Context),
    {ok, _} = m_edge:insert(PostId, author, UserId, Context),
    z_notifier:notify({post_added, ThreadId, PostId}, Context),
    case m_edge:get_id(ThreadId, zf_has_followers, UserId, Context) of
        undefined -> 
            {ok, _} = m_edge:insert(ThreadId, zf_has_followers, UserId, Context);
        _Defined ->
            ok
    end,
    {ok, PostId}.
