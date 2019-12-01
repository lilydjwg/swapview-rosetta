#!/usr/bin/env python

from __future__ import print_function

import sys
import resource
import time
from difflib import (SequenceMatcher, unified_diff)
from string import Template
from subprocess import Popen, PIPE
from statistics import (mean, stdev)

import pytoml

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


def run_profile(cmd, dir, time_limit, **kwargs):
    start_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    out, err = '', ''
    returncode = 0
    start_time = time.time()
    proc = Popen(cmd, stdout=PIPE, stderr=PIPE,
                 cwd=dir, shell=False, universal_newlines=True)
    out, err = proc.communicate(timeout=time_limit)
    returncode = proc.returncode
    end_usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    end_time = time.time()
    usage = dict(zip(rusage_names,
                     map(lambda x: x[1] - x[0], zip(start_usage, end_usage))))
    usage['elapsed'] = end_time - start_time
    return out, err, returncode, usage


def _key(l):
    pid, data, cmd = l.split(None, 2)
    return data, cmd, pid


def stable_sort(lines):
    if len(lines) < 2:
        return
    lines[1:-2] = sorted(lines[1:-2], key=_key)


def bench_profile(profile, ref_result_out, verbose,
                  time_field, show_diff_below, **kwargs):
    try:
        out, err, ret, usage = run_profile(**profile)
    except KeyboardInterrupt:
        die("Keyboard Interrupt from user")
    except:
        out, err, ret, usage = (
            "", ('Error cmd: %s\n' % " ".join(profile['cmd'])), 1, dict(elapsed=0))
    outlines = out.split('\n')
    stable_sort(outlines)
    ref_outlines = ref_result_out.split('\n')
    stable_sort(ref_outlines)
    ratio = SequenceMatcher(None, outlines, ref_outlines).ratio()
    profile['ratio'] = ratio
    if verbose:
        print('\033[1;%dm%-24s\033[m(%3d%%): %8.2f %s' %
              (32 if ret == 0 else 31,
               profile['name'], int(ratio * 100),
               usage[time_field] * 1000, err[: -1]))
        if ret == 0 and ratio < show_diff_below:
            print('$\n'.join(unified_diff(
                ref_outlines, outlines, fromfile='reference result',
                tofile=profile['name'], n=0, lineterm='')),
                end = '$\n',
            )
    return out, err[:-1], ret, usage


def load_config():
    filename = sys.argv[1] if len(sys.argv) > 1 else 'benchmark.toml'
    with open(filename) as configfile:
        config = pytoml.loads(configfile.read())
    items = config['item']
    default = items['default']
    if 'options' in config:
        options = config['options']
    else:
        options = dict(ref_result="C",
                       time_field="elapsed",
                       show_diff_below=0.9,
                       verbose=True)
    ret_items = {}
    for name, item in zip(items, items.values()):
        if name == 'default':
            continue
        if name.startswith('"') and name.endswith('"'):
            import ast
            name = ast.literal_eval(name)
        profile = dict(default)
        profile.update(item)
        profile['name'] = name
        for k, v in zip(profile, profile.values()):
            if type(v) is str:
                profile[k] = Template(v).safe_substitute(
                    **profile, name=name.split(':', 1)[0])
        ret_items[name] = profile
    return ret_items, options


def times2str(item):
    time = item['time']
    valid = sorted(time)[:int(item['valid_percent'] * len(time) / 100)]
    avgv = mean(valid)
    return "%s %s %4d" % ('%8.2f' % (avgv * 1000),
                          ' '.join(('%8.2f' % (x(time) * 1000))
                                   for x in (min, mean, max, stdev)),
                          len(time))


def main():
    items, options = load_config()
    succ, fails = [], []
    for item in sorted(items.values(), key=lambda x: x['name']):
        ref_out, err, ret, _ = run_profile(**items[options["ref_result"]])
        if ret != 0:
            die("reference implementation failed")
        out, err, ret, usage = bench_profile(item, ref_out, **options)
        item['time'] = [usage[options['time_field']]]
        if ret == 0:
            for i in range(item['count_limit'] - 1):
                _, err, _, usage = bench_profile(item, ref_out, **options)
                item['time'].append(usage[options['time_field']])
            succ.append(item)
        else:
            item['error'] = err
            fails.append(item)

    print(("---------Results--------(Diff):" +
           "  KMinAvg      Min      Avg      Max    Stdev  Cnt"))

    succ.sort(key=lambda x:
              sum(sorted(x['time'])
                  [:int(x['valid_percent'] * x['count_limit'] / 100)]))
    for item in succ:
        print('\033[1;%dm%-24s\033[m(%3d%%): %s' %
              (33 if item['ratio'] < options['show_diff_below'] else 32,
               item['name'], int(item['ratio'] * 100), times2str(item)))

    print("------------Fails-------------------")
    for item in fails:
        print('\033[1;31m%-24s\033[m       : %s' %
              (item['name'], item['error']))

if __name__ == '__main__':
    main()
