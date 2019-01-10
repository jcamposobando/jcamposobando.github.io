---
layout: default
---

<div class="posts">
  {% for post in site.posts %}
    <article class="post">

      <h1><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a></h1>
      
  		<small>{{ post.date | date: "%-d %B %Y" }}</small><br>
      
		{% if post.tags %}
			<small>
				{% for tag in post.tags %}
					<a href="/tags/{{tag}}">{{tag}}</a> 
				{% endfor %}
			</small> <br>
		{% endif %}
		<br>
		

      <div class="entry">
        {{ post.excerpt }}
      </div>

      <a href="{{ site.baseurl }}{{ post.url }}" class="read-more">Read More</a>
    </article>
    <br>
    <br>
  {% endfor %}
</div>
