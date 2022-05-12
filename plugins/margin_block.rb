## (Adapted from `margin_figure` plugin of [tufte-jekyll][])
##
## Liquid tag 'maincolumn' used to add image data that fits within the main column
## area of the layout
## Syntax:
## {% marginblock %}
## Phrasing content (i.e. paragraph nestable stuff)
## {% endmarginblock %}
##
## [tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
#
module Jekyll
  class RenderMarginBlockTag < Liquid::Block

    require "shellwords"

    def initialize(tag_name, text, tokens)
      super
      @text = text.shellsplit
    end

    def render(context)
      content = super
      "<label for='#{@text[0]}' class='margin-toggle'>&#8853;</label>"+
        "<input type='checkbox' id='#{@text[0]}' class='margin-toggle'/>"+
        "<span class='marginnote'>" + content + "</span>"
    end
  end
end

Liquid::Template.register_tag('marginblock', Jekyll::RenderMarginBlockTag)
