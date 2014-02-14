#!/usr/bin/env ruby
# encoding: utf-8

module ActsAsCsv
    def self.included(base)
        base.extend ClassMethods
    end

    module ClassMethods
        def acts_as_csv
            include InstanceMethods
        end
    end

    module InstanceMethods

        def read
            @csv_contents = []
            filename = '../data/ruby.csv'
            file = File.new(filename)
            @header = file.gets.chomp.split(', ')

            file.each do |row|
                csv_row = row.chomp.split(', ')
                @csv_contents << CsvRow.new(@header, csv_row)
            end
        end

        attr_accessor :header, :csv_contents

        def initialize
            read
        end

        def each &block
            @csv_contents.each &block
        end
    end

    class CsvRow
        attr_accessor :header_row, :content_row

        def initialize(header_row, content_row)
            @header_row = header_row
            @content_row = content_row
        end

        def method_missing name, *args
            i = @header_row.index(name.to_s)
            @content_row[i]
        end
    end
end

class RubyCsv # no inheritance! You can mix it in
    include ActsAsCsv
    acts_as_csv
end

csv = RubyCsv.new
csv.each { |row| puts row.one }
