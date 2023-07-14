require 'json'
require 'pathname'

(1..90).each { |i|
  File.open("submissions/p#{'%02d' % i}.json") { |f|
    dir = Pathname("submissions/p#{'%02d' % i}")
    dir.mkpath

    json = JSON.load(f)
    json["Success"].each { |sub|
      id = sub["_id"]
      path = dir / "submission-p#{'%02d' % i}-#{sub['submitted_at'].gsub(/[:]/, '_')}.json"
      if path.exist?
        File.open(path) { |f2|
          JSON.load(f2)
        }
      else
        system "api-sh/get-submission.sh #{id} > #{path.to_s}"
        sleep 1
      end
    }
  }
}
