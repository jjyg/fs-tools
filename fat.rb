
class String
def byte_at(off) self[off, 1].unpack('C').first end
def word_at(off) self[off, 2].unpack('v').first end
def dword_at(off) self[off, 4].unpack('V').first end
def trimendspc() sub(/ +$/, '') end
end
class Integer
def to_size
	o = []
	x = self>>30
	o << x << 'G' if x > 0
	x = (self>>20)&0x3ff
	o << x << 'M' if x > 0
	x = (self>>10)&0x3ff
	o << x << 'k' if x > 0
	x = self&0x3ff
	o << x if x > 0 or o.empty?
	o.join
end
end

module Fat
class Header
	attr_accessor :type, :creator, :byte_per_sector, :sector_per_cluster, :reserved_sector_count, :number_of_fat,
			:max_root_entry, :total_sectors, :sector_per_fat, :root_dir_cluster, :volume_label, :fat_type

	def initialize(vol, type=nil)
		hdr = vol.read(0, 512)
		if not type
			fat16_type = hdr[0x36, 8].trimendspc
			fat32_type = hdr[0x52, 8].trimendspc
			type = :fat12 if fat16_type == 'FAT12'
			type = :fat16 if fat16_type == 'FAT16'
			type = :fat32 if fat32_type == 'FAT32'
			puts "Fat type autodetect #{type.inspect}" if $VERBOSE
		end
		@type = type
		@creator = hdr[3, 8].trimendspc
		@byte_per_sector = hdr.word_at 0xb
		@sector_per_cluster = hdr.byte_at 0xd
		@reserved_sector_count = hdr.word_at 0xe
		@number_of_fat = hdr.byte_at 0x10
		@max_root_entry = hdr.word_at 0x11
		@total_sectors = hdr.dword_at 0x20
		@sector_per_fat = (type == :fat32 ? hdr.dword_at(0x24) : hdr.word_at(0x16))
		@root_dir_cluster = hdr.dword_at 0x2c
		@volume_label = hdr[(type == :fat32 ? 0x47 : 0x2b), 11].trimendspc
		@fat_type = hdr[(type == :fat32 ? 0x52 : 0x36), 8].trimendspc
	end
end

class File
	attr_accessor :shortname, :attr, :cluster, :size, :longname, :dir	# @dir = [dir_cluster, dir_index]
	def initialize(raw, longname=nil, dir=nil)
		@shortname = raw[0, 8].trimendspc
		ext  = raw[8, 3].trimendspc
		@shortname << '.' << ext if not ext.empty?
		@attr = raw.byte_at(0xb)
		@cluster = raw.word_at(0x1a) | (raw.word_at(0x14) << 16)
		@size = raw.dword_at(0x1c)
		return if not longname
		longname = longname[0, 2*ext] if ext = longname.unpack('v*').index(0)
		longname = longname.unpack('v*').pack('C*') if longname.unpack('v*').all? { |c| c < 256 }
		@longname = longname if not longname.empty?
	end

	def name; longname || @shortname end
	def dir?; (@attr & 0x10) > 0 end
	def deleted?; @shortname[0] == ?\xe5 end

	def to_s
		s = ''
		s << '(del) ' if deleted?
		s << name.inspect
		%w[RO HID SYS LABEL DIR ARV DEV ERR].each_with_index { |a, i| s << ' ' << a if (@attr & (1 << i)) > 0 }
		s << " #{@size.to_size}  cs=#{'%x' % @cluster}"
	end
end

class Directory
	attr_accessor :list
	def initialize(vol, clust)
		@list = []
		longname = ''
		idx = -1
		chn = vol.read_chain(clust)
		chn = [] if chn.length > 1000
		vol.read_fat_data(clust, chn).scan(/.{32}/m) { |d|
			idx += 1
p d if $DEBUG
			f = File.new(d, longname, [clust, idx])
			f.cluster &= 0xffff if vol.header.type != :fat32
			break if f.shortname[0] == ?\0
			if f.attr == 0xf
				_, n = decode_long_filename(d)
				# i should be a sequence number, with 0x40 set for last entry (eg 0x43..2..1)
				# but deleted have all i = 0xe5
				longname = n+longname
				next
			end
			longname = ''
			@list << f
		}
	end

	def decode_long_filename(d)
		[d.byte_at(0), d[1, 10] + d[0xe, 12] + d[0x1c, 4]]
	end
end

class Fat
	attr_accessor :fat
	def initialize(vol)
		@fat = vol.read_sect(vol.header.reserved_sector_count, vol.header.sector_per_fat)
	end

	def read_ptr(nr)
		@fat.dword_at(nr*4) & 0x0fff_ffff
	end

	# read a chain of ptrs upto termination tag (included)
	def read_chain(clust, limit=-1)
		chain = [clust]
		while clust >= 2 and clust <= 0x0fff_ffef and (limit < 0 or limit > chain.length) and clust < @fat.length/4+2
			clust = read_ptr(clust)
			chain << clust
		end
		chain
	end
end

class Volume
	attr_accessor :fd, :header, :fat
	attr_accessor :data_sector_offset, :rootdir_sector_offset, :byte_per_cluster
	def initialize(file)
		@fd = ::File.open(file, 'rb+')
		@header = Header.new(self)
p @header if $VERBOSE
		@fat = Fat.new(self)

		@byte_per_cluster = @header.byte_per_sector*@header.sector_per_cluster
		@rootdir_sector_offset = @header.reserved_sector_count + @header.number_of_fat*@header.sector_per_fat
		@data_sector_offset = @rootdir_sector_offset + @header.max_root_entry*32/@header.byte_per_sector
puts "FAT tracks #{cluster_count} clusters",
 "metadata size: #{(@data_sector_offset*@header.byte_per_sector).to_size}",
 "data size: #{data_size.to_size}" if $VERBOSE
	end

	def data_size
		cluster_count * @byte_per_cluster
	end

	# number of data clusters referenced by the fat
	def cluster_count
		case @header.type
		when :fat32; @fat.fat.length/4
		when :fat16; @fat.fat.length/2
		when :fat12; @fat.fat.length*2/3
		end - 2
	end

	def read(pos, count)
		@fd.pos = pos
		@fd.read(count)
	end

	def read_sect(sectnr, count=1)
		read(sectnr*@header.byte_per_sector, count*@header.byte_per_sector)
	end

	def read_clust(clustnr)
		if clustnr == :root
			if @header.type != :fat32
				return read(@rootdir_sector_offset*@header.byte_per_sector, @header.max_root_entry*32)
			end
			clustnr = @header.root_dir_cluster
		end
		read_sect(@data_sector_offset + (clustnr-2)*@header.sector_per_cluster, @header.sector_per_cluster)
	end

	# returns the array of clusters containing data for the file starting at clust
	# handles :root for :fat32, does not include end-of-chain cluster index
	def read_chain(clust)
		if clust == :root
			return [:root] if @header.type != :fat32
			clust = @header.root_dir_cluster
		end
		@fat.read_chain(clust)[0..-2]
	end

	# returns the content of the fat file starting at clust
	def read_fat_data(clust, chn=read_chain(clust))
		chn.inject('') { |data, cs| data << read_clust(cs) }
	end

	def read_file_data(file)
		read_fat_data(file.cluster)[0, file.size]
	end

	# reads the directory whose content starts at clust
	# use :root for root directory
	def read_directory(clust)
		Directory.new(self, clust)
	end

	# map an offset inside a file starting at clust to an offset into the raw fs
	# return nil if out of file
	def fileoff_to_devoff(clust, off=0)
		return if off < 0
		if clust == :root and @header.type != :fat32
			return if off >= @header.max_root_entry*32
			return @rootdir_sector_offset*@header.byte_per_sector + off
		end
		return if not clust = read_chain(clust)[off/@byte_per_cluster]
		@data_sector_offset + (clust-2) * @byte_per_cluster + off % @byte_per_cluster
	end
end
end

if __FILE__ == $0
v = Fat::Volume.new(ARGV.shift)

path = ARGV.shift.to_s
if not path
	# ls -lR
	dump_dir = lambda { |cluster, curpath|
		v.read_directory(cluster).list.each { |e|
			p = "#{curpath}/#{e.name}"
			if e.dir?
				puts "#{p}/"
				dump_dir[e.cluster, "#{p}/"]
			end
		}
	}
	dump_dir[:root, '/']
else
dir = :root
File.dirname(path).split('/').each { |name|
	puts name
	abort "#{name} not found" unless sd = v.read_directory(dir).list.find { |f| f.name.downcase == name.downcase }
	dir = sd.cluster
}

# find recoverable files in path & subdirs (ignore deleted dirs)
totalsize = 0
recover = lambda { |subdir, cs|
	puts "recovery in #{File.join(path, subdir).inspect}" if $VERBOSE
	v.read_directory(cs).list.each { |f|
		if f.dir?
			next if f.deleted?
			next if f.name == '.' or f.name == '..'
			next if subdir.count('/') > 20
			recover[File.join(subdir, f.name), f.cluster]
		else
			next if not f.deleted?
			chn = v.read_chain(f.cluster)
			if f.size > (chn.length-1)*v.byte_per_cluster and f.size <= chn.length*v.byte_per_cluster
				puts "recoverable #{File.join(subdir, f.name).inspect}"
				# v.read_file_data(f)
				totalsize += chn.length*v.byte_per_cluster
			else
				#puts "missing data in #{f.name.inspect}"
			end
		end
	}
}
recover['', dir]
puts "total recoverable size: #{totalsize.to_size}"
end
end
