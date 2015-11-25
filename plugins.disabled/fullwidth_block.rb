## (Adapted from `fullwidth` plugin in [tufte-jekyll][])
##
## This has a fairly harmless hack that wraps the content in a div to prevent it from being
## wrapped in a paragraph tag instead.
## Usage
## {% fullwidthblock %}
## {% endfullwidthblock %}
##
## [tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
#
module Jekyll
  class RenderFullWidthFigure < Liquid::Block
    require "shellwords"

    def initialize(tag_name, text, tokens)
      super
      @text = text.shellsplit
    end

    def render(context)
      content = super
      "<figure class='fullwidth'>" + content + "</figure>"
    end
  end
end

Liquid::Template.register_tag('fullwidthfigure', Jekyll::RenderFullWidthFigure)
