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

-module(resource_zforum).
-author("Michael Connors <michael@bring42.net>, Dmitrii Dimandt <dmitri@dmitriid.com>").

-export([
    event/2
]).

-include_lib("resource_html.hrl").

html(Context) ->
    resource_page:html(Context).

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

