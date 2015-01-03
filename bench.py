#!/usr/bin/env python

from __future__ import print_function

import toml
import json
import os
import sys
import resource
from difflib import SequenceMatcher, ndiff
from string import Template
from subprocess import Popen, PIPE
import time

rusage_names = """ru_utime
ru_stime
ru_maxrss
ru_ixrss
ru_idrss
ru_isrss
ru_minflt
ru_majflt
ru_nswap
ru_inblock
ru_oublock
ru_msgsnd
ru_msgrcv
ru_nsignals
ru_nvcsw
ru_nivcsw""".split('\n')


def die(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


def run_profile(cmd, dir, time_limit, times, **kwargs):
    start_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    out, err = '', ''
    returncode = 0
    start_time = time.time()
    proc = Popen(['/usr/bin/timeout', str(time_limit)] + cmd,
                 stdout=PIPE, stderr=PIPE,
                 cwd=dir, shell=False, universal_newlines=True)
    out, err = proc.communicate()
    returncode = proc.returncode
    end_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    end_time = time.time()
    usage = dict(zip(rusage_names,
                     map(lambda x: x[1]-x[0], zip(start_usage, end_usage))))
    usage['elapsed'] = end_time - start_time
    return out, err, returncode, usage


def bench_profile(profile, reference):
    out, err, ret, usage = run_profile(**profile)
    valid = int(SequenceMatcher(None,
                                out.split('\n'),
                                reference.split('\n')).ratio()*100)
    color = 31
    if ret == 0:
        if 100 - valid < profile['valid_percent']:
            color = 32
        else:
            color = 33
    else:
        color = 31
    profile['valid'] = valid
    print('\033[1;%dm%24s\033[m(%3d%%): %.2f %s' %
          (color, profile['name'], valid, usage[profile['time_field']], err[:-1]))
    if color == 33:
        print('\n'.join(ndiff(out.split('\n'), reference.split('\n'))))
    return out, err[:-1], ret, usage


def load_config():
    with open('bench.toml') as configfile:
        config = toml.loads(configfile.read())
    items = config['item']
    default = items['default']
    reference = config['reference']
    del items['default']
    for name, item in zip(items, items.values()):
        profile = dict(default)
        profile.update(item)
        profile['name'] = name
        items[name] = profile
        for k, v in zip(profile, profile.values()):
            if type(v) is str:
                profile[k] = Template(v).safe_substitute(**profile)
    return items, reference


def main():
    items, reference = load_config()

    reference_result, err, ret, _ = run_profile(**items[reference["result"]])
    if ret != 0:
        die("reference implementation failed")

    valids, invalids, fails = [], [], []
    for item in items.values():
        out, err, ret, usage = bench_profile(item, reference_result)
        item['time'] = usage[item['time_field']]
        if ret == 0:
            for i in range(item['times'] - 1):
                _, err, _, usage = bench_profile(item, reference_result)
                item['time'] += usage[item['time_field']]
            if 100-item['valid'] < item['valid_percent']:
                valids.append(item)
            else:
                invalids.append(item)
        else:
            item['error'] = err
            fails.append(item)

    print("------------Results-------------TIME")

    valids.sort(key=lambda x: x['time'])
    for item in valids:
        print('\033[1;32m%24s\033[m(%3d%%): %.2f' %
              (item['name'], item['valid'], item['time']))

    print("------------Invalids----------------")
    for item in invalids:
        print('\033[1;33m%24s\033[m(%3d%%): %.2f' %
              (item['name'], item['valid'], item['time']))

    print("------------Fails-------------------")
    for item in fails:
        print('\033[1;31m%24s\033[m       : %s' %
              (item['name'], item['error']))

main()
