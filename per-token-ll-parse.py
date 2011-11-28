#! /usr/bin/env python


import sys
import re

LL_REGEX = re.compile('^\<([0-9]+)\>\sLL\/token:\s(-[0-9\.]+)\s*$') 


def process_stream(stream, outstream): 
  for l in stream.xreadlines(): 
    m = LL_REGEX.search(l) 
    if m: 
      i, ll = m.groups()
      outstream.write(','.join(m.groups())) 
      outstream.write('\n')


def main(argv=None):
  if argv is None: 
    argv = sys.argv 
    
    if not sys.stdin.isatty(): 
      process_stream(sys.stdin, sys.stdout) 
    else: 
      #print USAGE % (argv[0],argv[0]) 
      with open(argv[1], 'r') as f: 
        process_stream(f, sys.stdout)
    
if __name__ == '__main__':
    sys.exit(main())


