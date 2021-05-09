import os
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

def load(run, logname, xaxis, yaxis):
    converters = {0: lambda s: float(" ".join(s.split(" ")[2:]))}
    fpath = "/tmp/shard/logs/{id}/{name}".format(id = run, name = logname)
    data = pd.read_csv(fpath, delimiter=",", names=[xaxis, yaxis], converters=converters)
    return data

def runs():
    dirpath = "/tmp/shard/logs/"
    names = [run for run in os.listdir(dirpath)]
    names.sort()
    return names

def lognames(run):
    dirpath = "/tmp/shard/logs/{id}/".format(id = run)
    names = [logname for logname in os.listdir(dirpath)]
    names.sort()
    return names
