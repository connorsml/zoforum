{% extends "base.tpl" %}
{% block html_head_extra %}
    {% lib "css/zp-zforum.css" %}
{% endblock %}
{% block content%}
    {% with m.rsc[id] as thread %}
        <nav id="zforum-menu">
            <ul>
                <li><a href="/forum">{_ Forum Home _}</a></li>
                <li><a href="{{thread.has_forum_category.page_url}}">{_ Back to _}&nbsp;{{thread.has_forum_category.title}}&nbsp;</a></li>
                <li>Categories
                    <ul id="category-menu">
                    {% with m.search[{query cat='zforum_category' sort='-publication_start'}] as result %}
                        {% for cat_id in result %}
                            <li>
                                <a href="{{ m.rsc[cat_id].page_url}}">{{ m.rsc[cat_id].title}}</a>
                            </li>
                        {% endfor %}
                    {% endwith %}
                    </ul>
                  </li>
            </ul>
        </nav>
    
        <h1>{{ thread.title }}</h1>
        <h2>{{ thread.summary }}</h2>
        <h3>{_ started by _}&nbsp;{{thread.author.name}}&mdash;{{thread.created|date:"d m y H:i"}}</h3>
        <div id="forum">
            <section id="forum_posts">
                <ul id="zforum_posts">
                {% for post in thread.has_forum_post %}
                    {% include "_zforum_post.tpl" post_id=post.id %}
                {% endfor %}
                </ul>
            </section>
            {% include "_zforum_form.tpl" thread_id=thread.id %} 
        </div>
    {% endwith %}
{% endblock %}