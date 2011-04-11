
{% with m.rsc[post_id] as post %}
    <li>
        
        {% if post.id|member:user.has_unviewed_forum_posts %}
            <h3><a href="#" id="view_{{post.id}}">{{post.author.name}}&mdash;{{post.created|date:"d m y H:i"}}</a></h3>
            {% wire id="view_"|append:post.id postback={view post_id=post_id } delegate="mod_zforum" action={show target="post_body_"|append:post.id} %}
            <div id="post_body_{{post.id}}" style="display: none;">
                {{post.body}}
            </div>
        {% else %}
            <h3>{{post.author.name}}&mdash;{{post.created|date:"d m y H:i"}}</h3>
            <div>
                {{post.body}}
            </div>
        {% endif %}
    </li>
{% endwith %}