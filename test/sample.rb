module A
  class B
    module C
      class D
        def m
          :hey
        end
      end
    end
  end
end

module E
  m do
    begin
      :block_without_args
    rescue
      whatever
    end
  end

  m do |arg|
    :block_with_args
  end
end

if a?
  'a'
elsif b?
  unless c?
    'c'
  else
    'b'
  end
end

case hey
when 1
  :foo
when 2
  :bar
else
  :default
end

def oneliner; 1; end
