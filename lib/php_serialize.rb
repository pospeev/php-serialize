#!/usr/bin/env ruby
# Copyright (c) 2003-2009 Thomas Hurst <tom@hur.st>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

require 'stringio'
require 'securerandom'

# PHP serialize() and unserialize() workalikes
#
# Release History:
#  1.0.0 - 2003-06-02 - First release.
#  1.0.1 - 2003-06-16 - Minor bugfixes.
#  1.0.2 - 2004-09-17 - Switch all {}'s to explicit Hash.new's.
#  1.1.0 - 2009-04-01 - Pass assoc to recursive calls (thanks to Edward Speyer).
#                     - Serialize Symbol like String.
#                     - Add testsuite.
#                     - Instantiate auto-generated Structs properly (thanks
#                       to Philip Hallstrom).
#                     - Unserialize arrays properly in assoc mode.
#                     - Add PHP session support (thanks to TJ Vanderpoel).
#                     - Release as tarball and gem.
#
# See http://www.php.net/serialize and http://www.php.net/unserialize for
# details on the PHP side of all this.
module PHP
  ARRAY_TYPE = 'a'.freeze
  OBJECT_TYPE = 'O'.freeze
  STRING_TYPE = 's'.freeze
  INTEGER_TYPE = 'i'.freeze
  DOUBLE_TYPE = 'd'.freeze
  NULL_TYPE = 'N'.freeze
  BOOLEAN_TYPE = 'b'.freeze
  OPEN_CURLY_BRACKET = '{'.freeze
  COLON = ':'.freeze
  EQUALS = '='.freeze
  SEMICOLON = ';'.freeze

	class StringIOReader < StringIO
		# Reads data from the buffer until +char+ is found. The
		# returned string will include +char+.
		def read_until(char)
			val, cpos = nil, pos
			if idx = string.index(char, cpos)
				val = read(idx - cpos + 1)
			end
			val
		end
	end

# mixed = PHP.unserialize(string serialized, [hash classmap, [bool assoc]])
#
# Returns an object containing the reconstituted data from serialized.
#
# If a PHP array (associative; like an ordered hash) is encountered, it
# scans the keys; if they're all incrementing integers counting from 0,
# it's unserialized as an Array, otherwise it's unserialized as a Hash.
# Note: this will lose ordering.  To avoid this, specify assoc=true,
# and it will be unserialized as an associative array: [[key,value],...]
#
# If a serialized object is encountered, the hash 'classmap' is searched for
# the class name (as a symbol).  Since PHP classnames are not case-preserving,
# this *must* be a .capitalize()d representation.  The value is expected
# to be the class itself; i.e. something you could call .new on.
#
# If it's not found in 'classmap', the current constant namespace is searched,
# and failing that, a new Struct(classname) is generated, with the arguments
# for .new specified in the same order PHP provided; since PHP uses hashes
# to represent attributes, this should be the same order they're specified
# in PHP, but this is untested.
#
# each serialized attribute is sent to the new object using the respective
# {attribute}=() method; you'll get a NameError if the method doesn't exist.
#
# Array, Hash, Fixnum, Float, True/FalseClass, NilClass and String should
# be returned identically (i.e. foo == PHP.unserialize(PHP.serialize(foo))
# for these types); Struct should be too, provided it's in the namespace
# Module.const_get within unserialize() can see, or you gave it the same
# name in the Struct.new(<structname>), otherwise you should provide it in
# classmap.
#
# Note: StringIO is required for unserialize(); it's loaded as needed
	def PHP.unserialize(string, classmap = nil, assoc = false) # {{{
		if classmap == true or classmap == false
			assoc = classmap
			classmap = {}
		end
		classmap ||= {}

		ret = nil
		original_encoding = string.encoding if string.respond_to?(:encoding)
		string = StringIOReader.new(string.respond_to?(:force_encoding) ? string.force_encoding('BINARY') : string)
		while string.string[string.pos, 32] =~ /^(\w+)\|/ # session_name|serialized_data
			ret ||= {}
			string.pos += $&.size
			ret[$1] = PHP.do_unserialize(string, classmap, assoc, original_encoding)
		end

		ret ? ret : PHP.do_unserialize(string, classmap, assoc, original_encoding)
	end

private
	def PHP.do_unserialize(string, classmap, assoc, original_encoding)
		val = nil
		# determine a type
		type = string.read(2)[0,1]
		case type
			when ARRAY_TYPE # associative array, a:length:{[index][value]...}
				count = string.read_until(OPEN_CURLY_BRACKET).to_i
				val = vals = Array.new
				count.times do |i|
					vals << [do_unserialize(string, classmap, assoc, original_encoding), do_unserialize(string, classmap, assoc, original_encoding)]
				end
				string.read(1) # skip the ending }

				# now, we have an associative array, let's clean it up a bit...
				# arrays have all numeric indexes, in order; otherwise we assume a hash
				array = true
				i = 0
				vals.each do |key,value|
					if key != i # wrong index -> assume hash
						array = false
						break
					end
					i += 1
				end

				if array
					vals.collect! do |key,value|
						if value.respond_to? :force_encoding
							value.kind_of?(String) ? value.force_encoding(original_encoding) : value
						else
							value
						end
					end
				else
					if assoc
						val = vals.map {|v| v }
					else
						val = Hash.new
						vals.each do |key,value|

							if value.respond_to? :force_encoding
								key = key.force_encoding(original_encoding) if key.kind_of?(String)
								value = value.force_encoding(original_encoding) if value.kind_of?(String)
							end

							val[key] = value
						end
					end
				end

			when OBJECT_TYPE # object, O:length:"class":length:{[attribute][value]...}
				# class name (lowercase in PHP, grr)
				len = string.read_until(COLON).to_i + 3 # quotes, seperator
				klass = string.read(len)[1...-2].capitalize.intern # read it, kill useless quotes

        # generate random name for PHP stdClass
        klass = "#{klass}#{SecureRandom.hex(10)}".to_sym if klass == :Stdclass

				# read the attributes
				attrs = []
				len = string.read_until(OPEN_CURLY_BRACKET).to_i

				len.times do
					attr = (do_unserialize(string, classmap, assoc, original_encoding))
					attrs << [attr.intern, (attr << EQUALS).intern, do_unserialize(string, classmap, assoc, original_encoding)]
				end
				string.read(1)

				val = nil
				# See if we need to map to a particular object
				if classmap.has_key?(klass)
					val = classmap[klass].new
				elsif Struct.const_defined?(klass) # Nope; see if there's a Struct
					classmap[klass] = val = Struct.const_get(klass)
					val = val.new
				else # Nope; see if there's a Constant
					begin
						classmap[klass] = val = Module.const_get(klass)

						val = val.new
					rescue NameError # Nope; make a new Struct
						classmap[klass] = val = Struct.new(klass.to_s, *attrs.collect { |v| v[0].to_s })
						val = val.new
					end
				end

				attrs.each do |attr,attrassign,v|
					val.__send__(attrassign, v)
				end

			when STRING_TYPE # string, s:length:"data";
				len = string.read_until(COLON).to_i + 3 # quotes, separator
				val = string.read(len)[1...-2] # read it, kill useless quotes
				val = val.force_encoding(original_encoding) if val.respond_to?(:force_encoding)

			when INTEGER_TYPE # integer, i:123
				val = string.read_until(SEMICOLON).to_i

			when DOUBLE_TYPE # double (float), d:1.23
				val = string.read_until(SEMICOLON).to_f

			when NULL_TYPE # NULL, N;
				val = nil

			when BOOLEAN_TYPE # bool, b:0 or 1
				val = (string.read(2)[0] == ?1 ? true : false)

			else
				raise TypeError, "Unable to unserialize type '#{type}'"
		end

		val
	end # }}}
end

