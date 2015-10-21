---
layout: post
title: "Updating Octopress post-Mavericks upgrade."
date: 2014-03-13 06:18
comments: true
categories: octopress osx
---

I decided this morning to write a blog post related to Rust.  I have
not posted to this blog in months, and in the meantime I had upgraded
this computer at home to Mac OS X Mavericks (10.9.2).

So of course my existing set of commands for Octopress workflow did
not work.

<!-- more -->

At first there were dependencies like `chunky_png-1.2.7` that had to
be satisfied (re-installed, I assume; I am pretty sure I blew away my
previous Homebrew setup during the upgrade; I do not know how much
that overlaps with Ruby's package management system).

The few step was just blind following of the suggestions made by my
tools: `rake` suggests to run `bundle install`, and I comply.  And the results
seem promising:
``` text
% rake generate
Could not find chunky_png-1.2.7 in any of the sources
Run \`bundle install\` to install missing gems.
% bundle install
Fetching gem metadata from http://rubygems.org/.......
Fetching gem metadata from http://rubygems.org/..
Using rake (0.9.6) 
Using RedCloth (4.2.9) 
Installing chunky_png (1.2.7) 
Using fast-stemmer (1.0.2) 
Using classifier (1.3.3) 
Using fssm (0.2.10) 
Installing sass (3.2.5) 
Using compass (0.12.2) 
Using directory_watcher (1.4.1) 
Installing haml (3.1.8) 
Installing kramdown (0.13.8) 
Installing liquid (2.3.0) 
Using syntax (1.0.0) 
Using maruku (0.6.1) 
Using posix-spawn (0.3.6) 
Using yajl-ruby (1.1.0) 
Installing pygments.rb (0.3.7) 
Installing jekyll (0.12.0) 
Installing rack (1.4.5) 
Installing rack-protection (1.3.2) 
Using rb-fsevent (0.9.3) 
Using rdiscount (1.6.8) 
Using redcarpet (2.2.2) 
Using rubypants (0.2.0) 
Installing tilt (1.3.3) 
Installing sinatra (1.3.4) 
Installing stringex (1.4.0) 
Using bundler (1.3.5) 
Your bundle is complete!
Use \`bundle show [gemname]\` to see where a bundled gem is installed.
```

But I
balked on the second step:
```
% rake generate
rake aborted!
You have already activated rake 10.1.0, but your Gemfile requires rake 0.9.6. Using bundle exec may solve this.
/Users/pnkfelix/Dev/Sites/pnkfx-blog/Rakefile:2:in \`<top (required)>'
(See full trace by running task with --trace)
% bundle exec
bundler: exec needs a command to run
```

I did not understand what `bundle exec` meant here, so I did not do the "obvious thing", which apparently is to re-run generate but within bundle, like so:
`% bundle exec rake generate`

Instead I fumbled around trying to figure out what my situation was
with respect to `rake:` do I need to downgrade to a previous version?
Or do I need to upgraade its subcomponents, and/or my whole site
configuration?

The first things I learned from a couple web interactions:

From [stackoverflow](http://stackoverflow.com/questions/17474969/you-have-already-activated-rake-0-9-6-but-your-gemfile-requires-rake-10-1-0-us)
I learned:

* You can find out what version(s) of a gem you have install, with
the relatively obvious `gem list` command:
```
% gem list rake

*** LOCAL GEMS ***

rake (10.1.0, 0.9.6)
```

* You can also remove particular versions of a gem, with the `gem
uninstall` command:
```
% gem uninstall rake

Select gem to uninstall:
 1. rake-0.9.6
 2. rake-10.1.0
 3. All versions
> 1
Successfully uninstalled rake-0.9.6
```

But these facts and this process did not actually help, because I
still needed `rake-0.9.6` for my site configuration, for some reason I
have not yet determined (mostly due to lack of trying).

I then did some more guessing and followed some false paths, like
reinstalling the `bundler` gem, uninstalling and reinstalling rake
(which effectively led to me replacing rake-10.1.0 with rake-10.1.1).

At some point I ran this:
```
% bundle update rake
Fetching gem metadata from http://rubygems.org/........
Fetching gem metadata from http://rubygems.org/..
Resolving dependencies...
Installing rake (0.9.6) 
Using RedCloth (4.2.9) 
Using chunky_png (1.2.7) 
Using fast-stemmer (1.0.2) 
Using classifier (1.3.3) 
Using fssm (0.2.10) 
Using sass (3.2.5) 
Using compass (0.12.2) 
Using directory_watcher (1.4.1) 
Using haml (3.1.8) 
Using kramdown (0.13.8) 
Using liquid (2.3.0) 
Using syntax (1.0.0) 
Using maruku (0.6.1) 
Using posix-spawn (0.3.6) 
Using yajl-ruby (1.1.0) 
Using pygments.rb (0.3.7) 
Using jekyll (0.12.0) 
Using rack (1.4.5) 
Using rack-protection (1.3.2) 
Using rb-fsevent (0.9.3) 
Using rdiscount (1.6.8) 
Using redcarpet (2.2.2) 
Using rubypants (0.2.0) 
Using tilt (1.3.3) 
Using sinatra (1.3.4) 
Using stringex (1.4.0) 
Using bundler (1.3.5) 
Your bundle is updated!
```
but I still got the error:
```
% rake generate
rake aborted!
You have already activated rake 10.1.1, but your Gemfile requires rake 0.9.6. Using bundle exec may solve this.
/Users/pnkfelix/Dev/Sites/pnkfx-blog/Rakefile:2:in \`<top (required)>'
(See full trace by running task with --trace)
```
and this is when I finally saw that I had to do:
```
% bundle exec rake generate
```

Except that this did not solve everything:
``` text
% bundle exec rake generate
## Generating Site with Jekyll
unchanged sass/screen.scss
Configuration from /Users/pnkfelix/Dev/Sites/pnkfx-blog/_config.yml
Building site: source -> public
YAML Exception reading 2013-04-12-better-command-completion-in-bash-aka-resolving-zsh-envy.markdown: invalid byte sequence in US-ASCII
/Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/backtick_code_block.rb:13:in \`gsub': invalid byte sequence in US-ASCII (ArgumentError)
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/backtick_code_block.rb:13:in \`render_code_block\'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/octopress_filters.rb:12:in \`pre_filter'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/octopress_filters.rb:28:in \`pre_render'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/post_filters.rb:112:in \`block in pre_render'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/post_filters.rb:111:in \`each'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/post_filters.rb:111:in \`pre_render'
	from /Users/pnkfelix/Dev/Sites/pnkfx-blog/plugins/post_filters.rb:166:in \`do_layout'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/lib/jekyll/post.rb:195:in \`render'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/lib/jekyll/site.rb:200:in \`block in render'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/lib/jekyll/site.rb:199:in \`each'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/lib/jekyll/site.rb:199:in \`render'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/lib/jekyll/site.rb:41:in \`process'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/lib/ruby/gems/1.9.1/gems/jekyll-0.12.0/bin/jekyll:264:in \`<top (required)>'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/bin/jekyll:23:in \`load'
	from /Users/pnkfelix/.rbenv/versions/1.9.3-p194/bin/jekyll:23:in \`<main>'
```

Another web search brought me to a [post by a fellow Mavericks user](http://otfusion.org/blog/2013/10/27/os-x-mavericks/) who seems to have a similar attitude to my own about ruby development.
And from that I found the full command I needed
```
% LANG=en_US.utf-8 bundle exec rake generate
## Generating Site with Jekyll
unchanged sass/screen.scss
Configuration from /Users/pnkfelix/Dev/Sites/pnkfx-blog/_config.yml
Building site: source -> public
Successfully generated site: source -> public
```

And here we are!
