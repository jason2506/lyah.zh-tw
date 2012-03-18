module Jekyll

  class CodeBlock < Liquid::Block
    def initialize(tag_name, markup, tokens)
      super
      @type = markup
    end

    def render(context)
      content = super.join
      %Q{<pre name="code" class="#{@type}">#{content}</pre>}
    end
  end

end

Liquid::Template.register_tag('code', Jekyll::CodeBlock)