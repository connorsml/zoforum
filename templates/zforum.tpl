{% extends "base.tpl" %}
{% block html_head_extra %}
    {% lib "css/zp-zforum.css" %}
{% endblock %}
{% block content%}
    <h1>{_ Forum  _}</h1>
    <nav id="zforum-menu">
        <ul>
            <li class="zf-first"><a href="/forum" class="{% if not q.id|is_defined %} current{% endif %}">{_ Forum Home _}</a></li>
                {% with m.search[{query cat='zforum_category' sort='-publication_start'}] as result %}
                    {% for cat_id in result %}
                        <li class="{% if forloop.last %}zf-last{% endif %}">
                                <a href="{{ m.rsc[cat_id].page_url}}" class="{% ifequal q.id cat_id %} current {% endifequal %}">{{ m.rsc[cat_id].title}}</a>

                        </li>
                    {% endfor %}
                {% endwith %}
        </ul>
    </nav>
    <div id="forum">
        {% if q.id %}

            <section id="current_category">
                 <h2>{{ m.rsc[q.id].title }}</h2>
                 <ul id="zf_forum_threads">
                 {% with m.search[{query cat='zforum_thread' hasobject=[q.id,'has_forum_category'] sort='-publication_start'}] as result %}
                     {% for thread_id in result %}
                         {% include "_zforum_thread_summary.tpl"  thread_id=thread_id %}
                     {% endfor %}
                 {% endwith %}
                 </ul>
                 {% include "_zforum_form.tpl" cat_id=q.id %}
            </section>
        {% endif %}

        <section id="latest_posts" class="clearfix">
            <header>
                <h2>Recent Posts</h2>
            </header>
            <ul>
            {% with m.search[{query cat='zforum_post' sort='-publication_start' pagelen=5}] as result %}
                {% for post_id in result %}
                    <li>
                        <a href="{{ m.rsc[post_id].s.has_forum_post.page_url}}">{{ m.rsc[post_id].title}}</a>
                    </li>
                {% endfor %}
            {% endwith %}
            </ul>
        </section>
    </div>
{% endblock %}