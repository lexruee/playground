#!/usr/bin/env ruby

module MixinNoIfs
    def if_true(&block)
        if self
            block.call
        end
        self
    end

    def if_false(&block)
        if !self
            block.call
        end
        self
    end
end

class TrueClass
    include MixinNoIfs
end

class FalseClass
    include MixinNoIfs
end


(1 == 1)
    .if_false { puts "1 == 1 is false" }
    .if_true { puts "1 == 1 is true" }

(1 == 2)
    .if_true { puts "1 == 2 is true" }
    .if_false { puts "1 == 2 is false" }
