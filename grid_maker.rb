#!/usr/env ruby

infile = ARGV[0]
outfile = infile.split(".").first + ".pl"

lines = File.open(infile).readlines

# Parse width and height of grid
width, height = lines[0].split(" ")
width, height = width.to_i, height.to_i
grid_size = "width(#{width}).\nheight(#{height})."
lines = lines.drop 1

# Parse rows
grid = []
lines[0...height].each_with_index do |row, row_index|
  row.split("")[0...width].each_with_index do |tile, col_index|
    grid << [[col_index, row_index], tile]
  end
end 

def number?(label)
  begin
    Integer(label)
    true
  rescue
    false
  end
end

def render_item(item)
  coords = item[0]
  label = item[1]
  if number?(label)
    "island(#{coords}).\nbridgeLimit(#{coords}, #{label})."
  else
    nil
  end
end

# Write to file
File.open(outfile, "w") do |f|
  f.puts grid_size
  f.puts
  f.puts grid.map { |i| render_item i }.select{ |i| not i.nil? }.join("\n")
end