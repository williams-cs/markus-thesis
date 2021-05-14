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

def run_shard(experiment, plot_latest, x, legend, arg=None, run=None, floatify=True, plot_index=1, multiplot=0):
    if run is None:
        argstr = ""
        if arg is not None:
            argstr = " " + arg
        os.system("../../shard.exe ./" + experiment + ".shard" + argstr)
        run = logreader.runs()[-1]
    logname = ""
    if plot_latest:
        if multiplot > 0:
            lognames = [s for s in logreader.lognames(run) if s[0] == '0']
            lognames = lognames[(len(lognames) - plot_index * multiplot):(len(lognames) - plot_index * multiplot + multiplot)]
            return (run, [logreader.load(run, lognames[i], x, legend[i], floatify) for
                i in range(multiplot)])
        lognames = [s for s in logreader.lognames(run) if s[0] != '0']
        logname = lognames[-plot_index]
    else:
        logname = "_internal.log"
    return (run, logreader.load(run, logname, x, legend, floatify))

def write_meta(name, ts, pairs):
    with open('out/{ts}/{name}_{ts}.txt'.format(ts=ts, name=name), 'w') as file:
        for (subname, run) in pairs:
            file.write("{subname}: {run}\n".format(subname=subname, run=run))

# Memory usage, 100 trials, different clusters
def e1(e1a=None, e1b=None):
    re1a, e1a = run_shard("e1a", True, "time", "Localhost", run=e1a)
    re1b, e1b = run_shard("e1b", True, "time", "Williams CS cluster", run=e1b)
    plt.plot("time", "Localhost", data=e1a)
    plt.plot("time", "Williams CS cluster", data=e1b)
    plt.xlabel('Time elapsed (seconds)')
    plt.ylabel('Iterations completed')
    plt.legend()
    plt.savefig('out/{ts}/e1_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e1", start_timestamp,
        [("e1a", re1a), ("e1b", re1b)])

# Word count, 20 trials, different clusters
def e2(e2a=None, e2b=None):
    re2a, e2a = run_shard("e2a", True, "time", "Localhost", run=e2a)
    re2b, e2b = run_shard("e2b", True, "time", "Williams CS cluster", run=e2b)
    plt.plot("time", "Localhost", data=e2a)
    plt.plot("time", "Williams CS cluster", data=e2b)
    plt.xlabel('Time elapsed (seconds)')
    plt.ylabel('Iterations completed')
    plt.legend()
    plt.savefig('out/{ts}/e2_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e2", start_timestamp,
        [("e2a", re2a), ("e2b", re2b)])

# Word count slow, 20 trials, different clusters
def e3(e3a=None, e3b=None):
    re3a, e3a = run_shard("e3a", True, "time", "Localhost", run=e3a)
    re3b, e3b = run_shard("e3b", True, "time", "Williams CS cluster", run=e3b)
    plt.plot("time", "Localhost", data=e3a)
    plt.plot("time", "Williams CS cluster", data=e3b)
    plt.xlabel('Time elapsed (seconds)')
    plt.ylabel('Iterations completed')
    plt.legend()
    plt.savefig('out/{ts}/e3_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e3", start_timestamp,
        [("e3a", re3a), ("e3b", re3b)])

# Word length, single unreachable host in cluster, task complete time
def e4(e4a=None, e4b=None):
    re4a, e4a = run_shard("e4a", True, "time", "No unreachable host", run=e4a)
    re4b, e4b = run_shard("e4b", True, "time", "One unreachable host", run=e4b)
    plt.plot("time", "No unreachable host", data=e4a)
    plt.plot("time", "One unreachable host", data=e4b)
    plt.xlabel('Time elapsed (seconds)')
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
    plt.ylabel("Completion time (seconds)")
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
    plt.xlabel("Time elapsed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend(title="Crash probability")
    plt.savefig('out/{ts}/e6_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e6", start_timestamp,
        [("e6{i}".format(i=i), res[i]) for i in range(len(chances))])

# Random hash, fixed fail chance, 0-80 (9 times)
def e7(e7=None):
    re7, e7 = run_shard("e7", False, "failchance", "Time taken", run=e7, floatify=False)
    plt.bar("failchance", "Time taken", data=e7)
    plt.xlabel("Failure probability (percent)")
    plt.ylabel("Completion time (seconds)")
    plt.savefig("out/{ts}/e7_{ts}.png".format(ts=start_timestamp))
    plt.clf()
    write_meta("e7", start_timestamp,
        [("e7", re7)])

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
    plt.xlabel("Time elapsed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend(title="Failure probability")
    plt.savefig('out/{ts}/e8_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e8", start_timestamp,
        [("e8{i}".format(i=i), res[i]) for i in range(len(chances))])

# Random hash, 10 trials, 0-50% data loss rate
def e9(e9=None):
    re9, e9 = run_shard("e9", False, "numcrashes", "Time taken", run=e9, floatify=False)
    plt.bar("numcrashes", "Time taken", data=e9)
    plt.xlabel("Packet loss probability")
    plt.ylabel("Completion time (seconds)")
    plt.savefig("out/{ts}/e9_{ts}.png".format(ts=start_timestamp))
    plt.clf()
    write_meta("e9", start_timestamp,
        [("e9", re9)])

# Word length, task complete time, cluster setup 0-0-50-50
def e10(e10=None):
    clustersize = 4
    vals = [0, 1, 5, 50]
    legends = ["{val}%".format(val=val) for val in vals]
    re10, e10s = run_shard("e10", True, "time", legends, run=e10,
        multiplot=clustersize, plot_index=2, arg=" ".join([str(val) for val in vals]))
    for i in range(clustersize):
        # print(mt)
        # oamt = e11s[i].tail(1)[legends[i]]
        # e11s[i].append({"time": mt, legends[i]: oamt}, ignore_index=True)
        plt.plot("time", legends[i], data=e10s[i])
    plt.xlabel("Time passed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend(title="Packet loss probability")
    plt.savefig('out/{ts}/e10_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e10", start_timestamp, [("e10", re10)])

# Word length, 2 fast and 2 throttled (2 ops / s)
def e11(e11=None):
    clustersize = 4
    legends = ["Not throttled (1)", "Not throttled (2)", "Throttled (1)", "Throttled (2)"]
    re11, e11s = run_shard("e11", True, "time", legends, run=e11, multiplot=clustersize, plot_index=2)
    for i in range(clustersize):
        # print(mt)
        # oamt = e11s[i].tail(1)[legends[i]]
        # e11s[i].append({"time": mt, legends[i]: oamt}, ignore_index=True)
        plt.plot("time", legends[i], data=e11s[i])
    plt.xlabel("Time passed (seconds)")
    plt.ylabel("Tasks completed")
    plt.legend()
    plt.savefig('out/{ts}/e11_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e11", start_timestamp, [("e11", re11)])

# Line length, shard vs ssh
def e12(e12a=None, e12b=None):
    re12a, e12a = run_shard("e12", True, "time", "Shard script", run=e12a)
    def run_shard_ssh():
        if e12b is None:
            os.system("cd tmp; bash ./e12b.sh")
        fpath = "./tmp/log.txt"
        data = pd.read_csv(fpath, delimiter=",", names=["time", "Shell script with SSH"])
        return "true", data
    re12b, e12b = run_shard_ssh()
    plt.plot("time", "Shard script", data=e12a)
    plt.plot("time", "Shell script with SSH", data=e12b)
    plt.xlabel('Time elapsed (seconds)')
    plt.ylabel('Tasks completed')
    plt.legend()
    plt.savefig('out/{ts}/e12_{ts}.png'.format(ts=start_timestamp))
    plt.clf()
    write_meta("e12", start_timestamp,
        [("e12a", re12a), ("e12b", re12b)])

if __name__ == "__main__":
    start_timestamp = round(time.time() * 1000)
    os.mkdir("out/{ts}".format(ts=start_timestamp))
    ensure_cluster_setup()
    # test()
    e1(e1a=1620956251960841308, e1b=1620956261041915473)
    e2(e2a=1620956288546195659, e2b=1620956316161957438)
    e3(e3a=1620956329450936696, e3b=1620956402203034470)
    e4(e4a=1620960222925117370, e4b=1620960227020405998)
    e5(e5=1620868998158979719)
    e6(e6=[1620959697707394987, 1620959702241451245, 1620959716821576751,
        1620959735530775973])
    # Remove e7()
    e8(e8=[1620959303745098225, 1620959308376605637, 1620959313416609637,
        1620959318693010215, 1620959323857935428, 1620959329323987592,
        1620959334520438280, 1620959339633052881, 1620959345126921240])
    e9(e9=1620956765048516288)
    e10(e10=1620959834725407126)
    e11(e11=1620959869052132943)
    e12(e12a=1620960543995083444, e12b=True)


    # Bad runs:
    # e4(e4a=1620888152545757822,e4b=1620888156710099568)
    # e6(e6=[1620887387746814696, 1620887392243372193, 1620887401990941484, 1620887442563669042])
    # e8(e8=[1620887210730862524, 1620887215114942116, 1620887220080365563, 1620887225712660849,
    #    1620887231396474703, 1620887236869738531, 1620887242457871647, 1620887248425840888, 1620887254350673881])
    # e11(e11=1620931913873809096)
    # e10(e10=1620955782711934973)
    # e11(e11=1620957869681825107)

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