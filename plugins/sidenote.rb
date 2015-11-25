## (Adapted from [tufte-jekyll][])
##
## Usage {% sidenote 'sn-id-whatever' 'This is a sidenote and *displays a superscript*'%}
##
## [tufte-jekyll]: http://clayh53.github.io/tufte-jekyll/articles/15/tufte-style-jekyll-blog
#
module Jekyll
  class RenderSideNoteTag < Liquid::Tag

    require "shellwords"

    def initialize(tag_name, text, tokens)
      super
      @text = text.shellsplit
    end

    def render(context)
      "<label for='#{@text[0]}' class='margin-toggle sidenote-number'></label><input type='checkbox' id='#{@text[0]}' class='margin-toggle'/><span class='sidenote'>#{@text[1]} </span>"
    end
  end
end

Liquid::Template.register_tag('sidenote', Jekyll::RenderSideNoteTag)
