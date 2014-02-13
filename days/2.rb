#!/usr/bin/env ruby
# encoding: utf-8

# FIND
# -- how to access files with and without code blocks
# without block
file = File.open("../data/test.txt", "w+")
file.puts "Hi there - no block used!"
file.close

# with block - really similar behavior than "with" in Python
File.open("../data/test_block.txt", "w+") do |file|
    file.puts "Hi there - with block!"
end

puts IO.read("../data/test.txt")
puts IO.read("../data/test_block.txt")


# -- how to translate a hash to an array
# this notation actually creates symbols!
h = {a: 1, b: 2, c: 3}
a = h.to_a
p a
p a.length
a = h.to_a.flatten
p a
p a.length

# -- and back to hash
p Hash[h.to_a]
p Hash[*h.to_a.flatten]
# or fancy with incject
a = h.to_a
a.inject(Hash.new) do |h, pair|
    h[pair.first] = pair.last
    h
end
p a
# know the difference between puts, p, and print
# puts: calls .to_s
# p: calls .inspect
# print: same as puts but without new line appended


# -- can you iterate through a hash
h.each { |k, v| puts "key: #{k}, value: #{v}" }


# -- other common data structures than stacks which can be represented by lists
# queues
a = [1, 2, 3]
a.unshift(0)
a.insert(0, -1)
a.insert(-1, 4)
p a.pop

# or
a.push(5)
p a.shift

# list
a.insert(2, 'c')
a.delete('c')
a.delete_at(0)

# others are bag/set (uniq, & operator) and matrix (transpose)



# DO
# first
a = [*1..16]
a.each_with_index do |n, i|
    p a[((i - 3)..i)] if (i + 1) % 4 == 0
end
# way easier!
a.each_slice(4) { |slice| p slice }

# second
# know about .respond_to?('method_name') --> helpful for duck typing
# know about .map which applies a block to each element of an array and returns
#   a new array (or not if .map!)

# third
def grep(file, phrase)
    regex = Regexp.new(phrase)
    File.foreach(file).with_index { |line, n| puts "#{n}: #{line}" if line =~ regex }
end
grep('../data/test', "there")
