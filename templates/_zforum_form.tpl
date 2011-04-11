    {% if user_id %}
        {% if thread_id %}
            {% wire id="addpost" type="submit" postback={addpost thread_id=thread_id } delegate="mod_zforum" %}
            <form id="addpost" method="post" action="postback">
                <div>
                    <div class="form-item">
                    	<label for="body">{_ Body _}</label>
                    	<textarea name="body" id="body" cols="60" rows="8"></textarea>
                    	{% validate id="body" type={presence} %}
                    </div>
                    <div class="form-item button-wrapper">
                    	<button type="submit">{_ Post _}</button>
                    </div>
                </div>
            </form>
        {% else %}
            {% wire id="addpost" type="submit" postback={addpost cat_id=cat_id } delegate="mod_zforum" %}
            <form id="addpost" method="post" action="postback">
                <div>
            	    <div class="form-item">
            	        <label for="title">Title</label>
                    	    <input type="text" name="title" id="title" />
                    	    {% validate id="title" type={presence} %}
            	    </div>
            	    <div class="form-item">
            	        <label for="summary">Summary</label>
                	    <input type="text" name="summary" id="summary" />
                	    {% validate id="summary" type={presence} %}	
                    </div>
                    <div class="form-item">
                    	<label for="body">{_ Body _}</label>
                    	<textarea name="body" id="body" cols="60" rows="8"></textarea>
                    	{% validate id="body" type={presence} %}
                    </div>
                    <div class="form-item button-wrapper">
                    	<button type="submit">{_ Post _}</button>
                    </div>
                </div>
            </form>
        {% endif %}
    {% else %}
        <p>{_ Login or Signup if you would like to post in this forum. _}</p>
    {% endif %}

