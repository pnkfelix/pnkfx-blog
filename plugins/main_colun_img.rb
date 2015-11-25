## (Adapted from [tufte-jekyll][])
##
## Liquid tag 'maincolumn-figure' used to add image data that fits within the main column
## area of the layout
## Usage {% maincolumn /path/to/image 'This is the caption' %}
##
## [tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
#
module Jekyll
  class RenderMainColumnTag < Liquid::Tag

  	require "shellwords"

    def initialize(tag_name, text, tokens)
      super
      @text = text.shellsplit
    end

    def render(context)
      "<figure><figcaption>#{@text[1]}</figcaption><img src='#{@text[0]}'/></figure>"
    end
  end
end

Liquid::Template.register_tag('maincolumn', Jekyll::RenderMainColumnTag)
