(1..90).each { |i|
  system("api-sh/get-submissions.sh #{i} 100 > submissions/p#{'%02d' % i}.json")
}
