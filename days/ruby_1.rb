#!/usr/bin/env ruby
# encoding: utf-8

puts "Hello, world."

s = "Hello, Ruby"
puts s.index('Ruby')

for i in 1..10
    puts 'Florian Hartl'
end

for i in 1..10
    puts "This is sentence number #{i}"
end
# or
1.upto(10) { |i| puts "This is sentence number #{i}" }

# extra
print "Please input your guess: "
n = rand(10)
guess = gets
if guess.to_i < n
    puts "too low"
elsif guess.to_i > n
    puts "too high"
else
    puts "you made it!"
end
