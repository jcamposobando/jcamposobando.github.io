---
layout: post
title: Continuous Calendar
tags: processing generative design java
gist: true
excerpt_separator: <!--more-->
---

There's a space in my room where I wanted to put a calendar in, so I created one that just fitted in. I also wanted a calendar in which every day is displayed continuously, so each column represents a day of the week (starting on Monday) and each row is a week.

[ ![_config.yml]({{ site.baseurl }}/images/calendar/g14.png)]({{ site.baseurl }}/images/calendar/3907.pdf)

<!--more-->

For it, I wanted a pattern that looked simple. I chose a pattern of lines of varying angle and width. In order to make the variations look "rhythmic", I used the Perlin noise function provided by Processing. That means that different seeds for the noise create different patterns.

[ ![_config.yml]({{ site.baseurl }}/images/calendar/0085.png)] ({{ site.baseurl }}/images/calendar/0085.pdf)

Originally the length was also variable, but I liked it better when it was constant. I also tried some variants where the width of the lines was bigger so they contained the numbers. It looked like a sea of [meneitos](https://www.google.com/search?q=meneitos&source=lnms&tbm=isch&sa=X&ved=0ahUKEwjnjcWixOLfAhUBJt8KHexKAVcQ_AUIDigB&biw=1366&bih=677) though.

[ ![ _config.yml]({{ site.baseurl }}/images/calendar/19427.png)] ({{ site.baseurl }}/images/calendar/19427.pdf)

Using Processing's tweak mode was incredibly helpful as it made possible to see how changing the parameters affected the results on the fly instead of having to run the sketch again.

<script src="https://gist.github.com/jcamposobando/2937fb8d0b8848b093c58d2ac04806d8.js"></script>

The font is [Space Mono](https://fonts.google.com/specimen/Space+Mono), which is the same font used for accent on this website.

These are the links for the calendars in the screenshots in pdf so you can edit them:
[Firs One]({{ site.baseurl }}/images/calendar/3907.pdf),
[Second One]({{ site.baseurl }}/images/calendar/0085.pdf),
[Third One]({{ site.baseurl }}/images/calendar/19427.pdf). You can also download them clicking the images.



