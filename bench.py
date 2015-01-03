#!/usr/bin/env python

from __future__ import print_function

import toml
import json
import os
from subprocess import Popen, PIPE
import sys
import curses
from difflib import SequenceMatcher, ndiff
from string import Template


def run_profile(cmd, dir, time_limit, time_format, **kwargs):
    proc = Popen(['/usr/bin/timeout', str(time_limit),
                  '/usr/bin/time', '-f', time_format] + cmd,
                 stdout=PIPE, stderr=PIPE,
                 cwd=dir, shell=False, universal_newlines=True)
    out, err = proc.communicate()
    return out, err, proc.returncode


def bench_profile(profile, reference):
    out, err, ret = run_profile(**profile)
    valid = int(SequenceMatcher(None,
                                out.split("\n"),
                                reference.split("\n")).ratio()*100)
    color = 31
    if ret == 0:
        if 100 - valid < profile['valid_percent']:
            color = 32
        else:
            color = 33
    else:
        color = 31
    print('\033[1;%dm%24s\033[m(%3d%%): %s' %
          (color, profile['name'], valid, err[:-1]))
    if color == 33:
        print("\n".join(ndiff(out.split("\n"), reference.split("\n"))))
    return out, err[:-1], ret, valid


def die(msg):
    print(msg, file=sys.stderr)
    sys.exit(1)


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

    out, err, ret = run_profile(**items[reference["result"]])
    if ret != 0:
        die("reference implementation failed")

    valids, invalids, fails = [], [], []
    for item in items.values():
        out, err, ret, val = bench_profile(item, out)
        if ret == 0:
            item['time'] = float(err)
            for i in range(item['times'] - 1):
                _, err, _, val = bench_profile(item, out)
                item['time'] += float(err)
            item['valid'] = val
            if 100-val < item['valid_percent']:
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

main()
