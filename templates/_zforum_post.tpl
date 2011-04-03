{% with m.rsc[post_id] as post %}
    <li>
        <h3>{{post.author.name}}&mdash;{{post.created|date:"d m y H:i"}}</h3>
        <div>
            {{post.body}}
        </div>
    </li>
{% endwith %}