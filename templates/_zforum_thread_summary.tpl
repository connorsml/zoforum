{% with m.rsc[thread_id] as thread %}
    <li>
        <a href="{{ thread.page_url}}">{{ thread.title}}</a>
    </li>
{% endwith %}