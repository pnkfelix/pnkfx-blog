## (Adapted from [tufte-jekyll][])
##
## Usage: {% marginnote 'mn-id-whatever' 'This is a margin note *without* a superscript' %}
##
## [tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
module Jekyll
  class RenderMarginNoteTag < Liquid::Tag

    require "shellwords"

    def initialize(tag_name, text, tokens)
      super
      @text = text.shellsplit
    end

    def render(context)
      "<label for='#{@text[0]}' class='margin-toggle'> &#8853;</label><input type='checkbox' id='#{@text[0]}' class='margin-toggle'/><span class='marginnote'>#{@text[1]} </span>"
    end
  end
end

Liquid::Template.register_tag('marginnote', Jekyll::RenderMarginNoteTag)

