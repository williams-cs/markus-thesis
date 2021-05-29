command = "killall shard.exe; rm -f /tmp/shard/v0001/shard.exe; rm -f /tmp/shard/crash.txt; rm -f /tmp/shard/throttle.txt; rm -f /tmp/shard/packetloss.txt"

all: setup

setup: local
	- ssh 21myf1@devon.cs.williams.edu $(command)
	- ssh 21myf1@brava.cs.williams.edu $(command)
	- ssh 21myf1@sykia.cs.williams.edu $(command)
	- ssh 21myf1@guernsey.cs.williams.edu $(command)

local:
	rm -f /tmp/shard/crash.txt
	- (cd ../..; make; killall shard.exe)

.PHONY: setup local