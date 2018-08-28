require 'yaml'
tags = []

dd = Dir.glob(File.join('_posts','*.md'))

puts dd
print dd

dd.each do |file|
	yaml_s = File.read(file).split(/^---$/)[1]
	yaml_h = YAML.load(yaml_s)
	puts yaml_h['tags']
	aa = yaml_h['tags'].split(" ")
	print aa
	tags += yaml_h['tags'].split(" ")
end

tags.map(&:downcase).uniq.each do |tag|
	File.write "#{tag}.html" , <<-EOF
---
layout: tagpage
title: #{tag}
permalink: /tags/#{tag}
---
	EOF
end
