import os
import sys
import time
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import logreader

start_timestamp = 0

word_count_tasks = "1620537060074021903"

def test():
    run = ""
    if len(sys.argv) < 2:
        run = logreader.runs()[-1]
    else:
        run = sys.argv[1]
    lognames = logreader.lognames(run)
    latest = lognames[-1]
    times = logreader.load(run, latest, 'time', 'No forced failures')
    times.plot('time', ['No forced failures'])
    # plt.title('Tasks completed over time for word_count.shard')
    plt.xlabel('Time passed (seconds)')
    plt.ylabel('Tasks completed')
    plt.savefig('out/{id}.png'.format(id=run))

def ensure_cluster_setup():
    os.system("../../shard.exe ./setup.shard")

def run_shard(experiment, plot_latest, x, legend, arg=None, run=None, floatify=True):
    if run is None:
        argstr = ""
        if arg is not None:
            argstr = " " + arg
        os.system("../../shard.exe ./" + experiment + ".shard" + argstr)
        run = logreader.runs()[-1]
    logname = ""
    if plot_latest:
        logname = logreader.lognames(run)[-1]
    else:
        logname = "_internal.log"
    return (run, logreader.load(run, logname, x, legend, floatify))

def write_meta(name, ts, pairs):
    with open('out/{ts}/{name}_{ts}.txt'.format(ts=ts, name=name), 'w') as file:
        for (subname, run) in pairs:
            file.write("{subname}: {run}\n".format(subname=subname, run=run))

# Memory usage, 100 trials, different clusters
def e1():
    pass

# Word count, 20 trials, different clusters
def e2():
    pass

# Word count slow, 20 trials, different clusters
def e3():
    pass

# Word length, single unreachable host in cluster, task complete time
def e4(e4a=None, e4b=None):
    re4a, e4a = run_shard("e4a", True, "time", "No unreachable host", run=e4a)
    re4b, e4b = run_shard("e4b", True, "time", "One unreachable host", run=e4b)
    plt.plot("time", "No unreachable host", data=e4a)
    plt.plot("time", "One unreachable host", data=e4b)
    plt.xlabel('Time passed (seconds)')
    plt.ylabel('Tasks completed')
    plt.legend()
    plt.savefig('out/{ts}/e4_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e4", start_timestamp,
        [("e4a", re4a), ("e4b", re4b)])

# Random hash, fixed crash count, 1-5
def e5(e5=None):
    re5, e5 = run_shard("e5", False, "numcrashes", "Time taken", run=e5, floatify=False)
    plt.bar("numcrashes", "Time taken", data=e5)
    plt.xlabel("Number of crashes")
    plt.ylabel("Time taken to complete (seconds)")
    plt.savefig("out/{ts}/e5_{ts}.png".format(ts=start_timestamp))
    plt.clf()
    write_meta("e5", start_timestamp,
        [("e5", re5)])

# Word length, fixed crash chance, 0-3%
def e6(e6=None):
    chances = [0,1,2,3]
    res = []
    es = []
    for i in range(len(chances)):
        chance = chances[i]
        run = None
        if e6 is not None:
            run = e6[i]
        rei, ei = run_shard("e6", True, "time", "{chance}%".format(chance=chance), run=run, arg=str(i))
        res.append(rei)
        es.append(ei)
    for i in range(len(chances)):
        chance = chances[i]
        plt.plot("time", "{chance}%".format(chance=chance), data=es[i])
    plt.xlabel("Time passed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend(title="Crash chance")
    plt.savefig('out/{ts}/e6_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e6", start_timestamp,
        [("e6{i}".format(i=i), res[i]) for i in range(len(chances))])

# Random hash, varying fail chance, 0-75
def e7():
    pass

# Word length, fixed fail chance, 0-80 (9 times)
def e8(e8=None):
    chances = [0,10,20,30,40,50,60,70,80]
    res = []
    es = []
    for i in range(len(chances)):
        chance = chances[i]
        run = None
        if e8 is not None:
            run = e8[i]
        rei, ei = run_shard("e8", True, "time", "{chance}%".format(chance=chance), run=run, arg=str(i))
        res.append(rei)
        es.append(ei)
    for i in range(len(chances)):
        chance = chances[i]
        plt.plot("time", "{chance}%".format(chance=chance), data=es[i])
    plt.xlabel("Time passed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend(title="Failure chance")
    plt.savefig('out/{ts}/e8_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e8", start_timestamp,
        [("e8{i}".format(i=i), res[i]) for i in range(len(chances))])

# Random hash, 10 trials, 0-75% data loss rate
def e9():
    pass

# Word length, task complete time, 0-0-0-0, 25-25-25-25, 0-0-50-50
def e10():
    pass

# Word length, 2 fast and 2 slow (10x)
def e11():
    pass

# Line length, shard vs ssh
def e12():
    pass

if __name__ == "__main__":
    start_timestamp = round(time.time() * 1000)
    os.mkdir("out/{ts}".format(ts=start_timestamp))
    ensure_cluster_setup()
    # test()
    e4(e4=[1620888152545757822,1620888156710099568])
    e5(e5=1620868998158979719)
    e6(e6=[1620887387746814696, 1620887392243372193, 1620887401990941484, 1620887442563669042])
    e8(e8=[1620887210730862524, 1620887215114942116, 1620887220080365563, 1620887225712660849,
       1620887231396474703, 1620887236869738531, 1620887242457871647, 1620887248425840888, 1620887254350673881])

# times = pd.read_csv('error_rate.csv.output')
# times.plot('error_rate', ['time'])
# plt.xlabel('Error rate')
# plt.ylabel('Time taken')
# plt.savefig('out2.png')

# times = pd.read_csv('times.csv.output')
# times.plot('time', ['amount'])
# plt.xlabel('Time passed (seconds)')
# plt.ylabel('Tasks completed')
# plt.savefig('out.png')