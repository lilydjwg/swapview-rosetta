#!/usr/bin/env python

from __future__ import print_function

import json
import os
import sys
import resource
import time
from difflib import (SequenceMatcher, unified_diff)
from string import Template
from subprocess import Popen, PIPE
from statistics import (mean, stdev)

import toml

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
                     map(lambda x: x[1] - x[0], zip(start_usage, end_usage))))
    usage['elapsed'] = end_time - start_time
    return out, err, returncode, usage


def bench_profile(profile, ref_result):
    out, err, ret, usage = run_profile(**profile)
    ratio = SequenceMatcher(None,
                            out.split('\n'),
                            ref_result.split('\n')).ratio()
    profile['ratio'] = ratio
    if profile['verbose']:
        print('\033[1;%dm%24s\033[m(%3d%%): %8.2f %s' %
              (32 if ret == 0 else 31,
               profile['name'], int(ratio*100),
               usage[profile['time_field']]*1000, err[:-1]))
        if ret == 0 and ratio < profile['show_diff_below']:
            print('\n'.join(unified_diff(ref_result.split('\n'), out.split('\n'),
                                         fromfile='reference result',
                                         tofile=profile['name'],
                                         n=0, lineterm='')))
    return out, err[:-1], ret, usage


def load_config():
    filename = sys.argv[1] if len(sys.argv) > 1 else 'bench.toml'
    with open(filename) as configfile:
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


def times2str(item):
    time = item['time']
    valid = sorted(time)[:item['valid_times']]
    avgv = mean(valid)
    return ("%s %s [%s]" % (
        '%8.2f' % (avgv*1000),
        ' '.join(('%8.2f' % (x(time)*1000))
            for x in (min, mean, max, stdev)),
        ' '.join(('%8.2f' % (x*1000))
            for x in valid)))


def main():
    items, reference = load_config()
    succ, fails = [], []
    for item in sorted(items.values(), key=lambda x:x['name']):
        ref_result, err, ret, _ = run_profile(**items[reference["result"]])
        if ret != 0:
            die("reference implementation failed")
        out, err, ret, usage = bench_profile(item, ref_result)
        item['time'] = [usage[item['time_field']]]
        if ret == 0:
            for i in range(item['times'] - 1):
                _, err, _, usage = bench_profile(item, ref_result)
                item['time'].append(usage[item['time_field']])
            succ.append(item)
        else:
            item['error'] = err
            fails.append(item)

    print(("---------Results--------(Diff):" +
           "  KMinAvg      Min      Avg      Max    Stdev [K(%d)-samples raw data]") %
          items[reference["result"]]['valid_times'])

    succ.sort(key=lambda x: sum(sorted(x['time'])[:x['valid_times']]))
    for item in succ:
        print('\033[1;%dm%24s\033[m(%3d%%): %s' %
              (33 if item['ratio'] < item['show_diff_below'] else 32,
               item['name'], int(item['ratio']*100), times2str(item)))

    print("------------Fails-------------------")
    for item in fails:
        print('\033[1;31m%24s\033[m       : %s' %
              (item['name'], item['error']))

if __name__=='__main__':
    main()
